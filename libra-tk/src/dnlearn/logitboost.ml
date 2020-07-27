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
let schemafile = ref ""
let outfile = ref ""
let target_var = ref 0 
let shrinkage = ref 0.1
let mincount = ref 10

(* The weight the score function assigns to each additional split created,
   i.e., the penalty for each additional parameter. *)
let num_trees = ref 100 
let num_leaves = ref 8

let usage = "Usage: logitboost -i <input> -o <output> [...]"
let args = Arg.align
 ([("-i", Arg.Set_string datafile, " Training data file") ;
   ("-s", Arg.Set_string schemafile, " Data schema (optional)") ;
   ("-o", Arg.Set_string outfile, " Output model") ;
   ("-target", Arg.Set_int target_var, " Index of target variable (0-based)") ;
   ("-numtrees", Arg.Set_int num_trees, " Number of trees") ;
   ("-numleaves", Arg.Set_int num_leaves, " Number of leaves per tree") ;
   ("-nu", Arg.Set_float shrinkage, " Shrinkage (0-1]");
   ("-mincount", Arg.Set_int mincount, 
     " Minimum number of examples at each leaf [10]")]
   @ common_arguments)

  
  (*
(* Squared loss of var on data, according to distribution dist *)
let dist_sql dist counts = 
  let sql p q = (float_of_int p) *. (-. q *. q) in 
  Array.sumf (Array.map2 sql counts dist) 

(* Log loss of var on data, according to distribution dist *)
let dist_ll dist counts = 
  let ll p q = if p > 0 then (float_of_int p) *. (log q) else 0. in
  Array.sumf (Array.map2 ll counts dist) 
*)

module F = Mn.Factor

let do_learn target_var =
  (* Read in data and determine schema (number of values for each var) *)
  let data = Data.input_example_list (open_in !datafile) in
  vlogf "Loaded data.\n";
  let schema = 
    if !schemafile <> "" then begin
      let schemain = open_in !schemafile in
      let s = Data.input_example schemain in
      close_in schemain ; s
    end
    else Data.schema data in 
  let data = Array.of_list data in

  (* Set up data for training *)
  let y = Array.map (fun x -> x.(target_var)) data in
  let x = data in
  Array.iter (fun x -> x.(target_var) <- 0) data;

  Timer.start "learn";
  dlogf "schema.(%d) = %d\n" target_var schema.(target_var);
  let trees = LogitboostImpl.boost_trees !num_trees !num_leaves
    !shrinkage !mincount schema.(target_var) schema x y in
  let rec setleaves i = function
  | F.Leaf w ->
    F.Vertex (target_var, i, (F.Leaf w), (F.Leaf 0.))
  | F.Vertex (var, value, l, r) -> 
    F.Vertex (var, value, (setleaves i l), (setleaves i r)) in
  let trees = Array.mapi (fun i l -> List.map (setleaves i) l) trees in

  (* For each variable, build a decision tree and set it as the CPD *)
  (* 
  let numvars = Array.length schema in
  let bn = Bn.create_empty_network schema in
  for i = 0 to numvars - 1 do
    let root = learn_dt schema data i infinity in
    Bn.set_cptree bn i root
  done;
  *)
  vlogf "Learning time: %f seconds\n" (Timer.elapsed "learn");

  (* Save to disk *)
  let o = open_out !outfile in
  Array.iter (List.iter (Mn.Factor.output_tree o "")) trees


let main () = 
  Arg.parse args ignore usage;
  if !datafile = "" || !outfile = "" then
    Arg.usage args usage
  else begin
    common_log_init ();
    do_learn !target_var
  end

let _ = main ()
