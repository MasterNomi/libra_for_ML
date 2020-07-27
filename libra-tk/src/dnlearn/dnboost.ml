(******************************************************************************)
(* The Libra Toolkit                                                          *)
(*                                                                            *)
(* Copyright (C) 2015 by Daniel Lowd and Amirmohammad Rooshenas               *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(* Redistribution and use in source and binary forms, with or without         *)
(* modification, are permitted provided that the following conditions are     *)
(* met:                                                                       *)
(*                                                                            *)
(* 1. Redistributions of source code must retain the above copyright          *)
(* notice, this list of conditions and the following disclaimer.              *)
(*                                                                            *)
(* 2. Redistributions in binary form must reproduce the above copyright       *)
(* notice, this list of conditions and the following disclaimer in the        *)
(* documentation and/or other materials provided with the distribution.       *)
(*                                                                            *)
(* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS        *)
(* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          *)
(* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR      *)
(* A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT       *)
(* OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,      *)
(* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT           *)
(* LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,      *)
(* DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY      *)
(* THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT        *)
(* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE      *)
(* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.       *)
(******************************************************************************)

open Printf
open Ext

let big_int = 1000000000 (* = 1 billion *)

(* Globals used for command line parameters *)

let datafile = ref ""
let validfile = ref ""
let schemafile = ref ""
let outfile = ref ""
let shrinkage = ref 0.0
let mincount = ref 10

(* The weight the score function assigns to each additional split created,
   i.e., the penalty for each additional parameter. *)
let num_trees = ref 100 
let num_leaves = ref 8

let usage = "Usage: dnboost -i <input> -o <output> [...]"
let args = Arg.align
 ([("-i", Arg.Set_string datafile, " Training data file") ;
   ("-valid", Arg.Set_string validfile, " Validation data") ;
   ("-s", Arg.Set_string schemafile, " Data schema (optional)") ;
   ("-o", Arg.Set_string outfile, " Output dependency network") ;
   ("-numtrees", Arg.Set_int num_trees, " Number of trees per CPD") ;
   ("-numleaves", Arg.Set_int num_leaves, " Number of leaves per tree") ;
   ("-nu", Arg.Set_float shrinkage, " Shrinkage [0-1] (0 => line search)") ;
   ("-mincount", Arg.Set_int mincount, 
     " Minimum number of examples at each decision tree leaf [10]")]
   @ common_arguments)


module F = Mn.Factor

let learn_one schema data vdata target_var =
  vlogf "\nVARIABLE %d:\n" target_var;
  dlogf "schema.(%d) = %d\n" target_var schema.(target_var);

  (* Set up data for training *)
  let y = Array.map (fun x -> x.(target_var)) data in
  let x = data in
  Array.iter (fun x_i -> x_i.(target_var) <- 0) data;
  let vy = Array.map (fun x -> x.(target_var)) vdata in
  let vx = vdata in

  (* Train model *)
  Timer.clear "learn";
  Timer.start "learn";
  let trees = LogitboostImpl.boost_trees !num_trees !num_leaves
    !shrinkage !mincount schema.(target_var) schema x y vx vy in
  vlogf "Learning time: %f seconds\n" (Timer.elapsed "learn");

  (* Restore original data *)
  Array.iter2 (fun x_i y_i -> x_i.(target_var) <- y_i) x y;
  trees


let model_to_features target_var model =
  let trees_to_features value tree_l =
    let fl = List.flatten (List.map (F.tree_to_features []) tree_l) in
    let add_target f = 
      let condl = Array.to_list f.F.cond in
      {f with F.cond=Array.of_list ((true, target_var, value)::condl)} in
    List.map add_target fl in
  let features = Array.mapi trees_to_features model in
  Array.fold_left ( @ ) [] features


let pseudo_pll models x =
  let total = ref 0. in
  for j = 0 to Array.length x - 1 do
    total := !total +. (LogitboostImpl.model_prob models.(j) x).(x.(j))
  done;
  !total


let do_learn () =
  (* Read in data and determine schema (number of values for each var) *)
  let data = Data.input_example_list (open_in !datafile) in
  let vdata = 
    if !validfile <> "" then 
      Array.of_list (Data.input_example_list (open_in !validfile))
    else [||] in
  vlogf "Loaded data.\n";

  let schema = 
    if !schemafile <> "" then begin
      let schemain = open_in !schemafile in
      let s = Data.input_example schemain in
      close_in schemain ; s
    end
    else Data.schema data in 
  Timer.clear "learnall";
  Timer.start "learnall";
  let data = Array.of_list data in
  let models = 
    Array.init (Array.length schema) (learn_one schema data vdata) in
  vlogf "Total learning time: %f\n" (Timer.elapsed "learnall");
  let score = Array.sumf_map (pseudo_pll models) data in
  vlogf "Total log score: %f\n" score;

  if !validfile <> "" then begin
    let vscore = Array.sumf_map (pseudo_pll models) vdata in
    vlogf "Validation set logscore: %f\n"vscore
  end;

  (* Save to disk *)
  let o = open_out !outfile in
  let features = Array.mapi model_to_features models in
  Data.output_schema o schema;
  fprintf o "DN {\n";
  let output_varfeatures v fl = 
    fprintf o "v%d {\n" v;
    List.iter (Mn.Factor.output_feature o) fl;
    output_string o "}\n" in
  Array.iteri output_varfeatures features;
  output_string o "}\n"


let main () = 
  Arg.parse args ignore usage;
  if !datafile = "" || !outfile = "" then
    Arg.usage args usage
  else begin
    common_log_init ();
    do_learn ()
  end

let _ = main ()
