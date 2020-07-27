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

(* Globals used for command line parameters *)
let mfile = ref ""
let depnet = ref false
let datafile = ref ""
let pll = ref false
let clib = ref true
let per_var = ref false
 
let usage = "Usage: mscore -m <model> -i <data>"
let args = Arg.align
 ([("-m", Arg.Set_string mfile, " Input model (BN, MN, DN, or AC)") ;
   ("-depnet", Arg.Set depnet, " Score model as a DN, using pseudo-log-likelihood");
   ("-i", Arg.Set_string datafile, " Test data file");
   ("-pll", Arg.Set pll, " Compute pseudo-log-likelihood");
   ("-pervar", Arg.Set per_var, " Give MN PLL or BN LL for each variable separately.");
   ("-clib", Arg.Set clib, " Use C library for computing PLL");
   ("-noclib", Arg.Clear clib, " Use only OCaml code for computing PLL")]
   @ common_arguments)

(* Distinguish between two return types:
   A single value with the total result, and a vector with
   per-variable results. *)
type score_result = Value of float | Vector of float array
let as_value f x = Value (f x) 
let as_vector f x = Vector (f x) 

let score_mn mn =
  if !pll && not !depnet then 
    let fa = Array.of_list (Mn.to_features mn) in
    if !per_var then
      as_vector
      (fun x ->
          let score_var x i = (Mn.mb_logdist mn x i).(x.(i)) in
          Array.init (Mn.numvars mn) (score_var x))
    else if !clib then
      let cmn = Pll.create_mn mn.Mn.schema fa in
      as_value (Pll.pll_mn cmn)
    else
      as_value (Pll.fa_pll mn.Mn.schema fa)
  else begin
    if !per_var then begin
      nlogf "ERROR: -pervar not supported with MN log score. Try -pll.\n";
      exit (-1)
    end else
      as_value (Mn.raw_logprob mn)
  end


let main () = 
  Arg.parse args ignore usage;
  if !mfile = "" || !datafile = "" then
    (Arg.usage args usage; exit 0);

  common_log_init();

  let vs = ref None in

  let score_one =
    match filetype !mfile with
    | ACFile ->
      if !pll then 
        let (circ, fl) = Circuit.load_with_features (open_in !mfile) in
        (* Convert to an MN. (copied from mconvert.ml) *)
        let f_to_factor f =
          Mn.Factor.Feature
            {Mn.Factor.cond = Array.of_list f.Circuit.cond;
             Mn.Factor.weight_id = (Circuit.feature_node f).Circuit.id;
             Mn.Factor.weight = log f.Circuit.weight} in
        let factors = Array.of_list (List.map f_to_factor fl) in
        let mn = Mn.create (circ.Circuit.schema) factors in
        score_mn mn
      else
        let circ = Circuit.load (open_in !mfile) in
        let scratch = Circuit.create_scratch circ in
        let z = Circuit.compute_z scratch circ in
        as_value (fun x -> Circuit.logprob_x scratch circ x -. z)

    | BNFile ->
      let bn = Bn.load_auto !mfile in
      if !per_var then 
        as_vector
        (fun x ->
            let score_var x i =
              if !pll && not !depnet then
                (Bn.mb_logprob bn x i).(x.(i))
              else
                Bn.node_logscore bn x i in
            Array.init (Bn.numvars bn) (score_var x))
      else if !pll && not !depnet then 
        as_value (Bn.pll bn)
      else 
        as_value (Bn.loglikelihood bn)
    | MNFile ->
      let mn = Mn.load_auto !mfile in
      score_mn mn
    | _ -> printf "ERROR: Could not determine filetype from extension.\n";
           exit (-1) in
  let datastream = open_in !datafile in 
  let s = stats_make () in
  try 
    while true do
      let (w, x) = Data.input_wexample datastream in
      match score_one x with
      | Value ll ->
        vlogf "%f\n" ll ; 
        stats_wadd s w ll
      | Vector pervar_ll ->
        let varstats = match !vs with 
        | None -> Array.map (fun _ -> stats_make ()) pervar_ll 
        | Some x -> x in 
        vs := Some varstats;
        for i = 0 to Array.length pervar_ll - 1 do
          vlogf "%d: %f\n" i pervar_ll.(i);
          stats_wadd varstats.(i) w pervar_ll.(i)
        done;
        let ll = Array.sumf pervar_ll in
        vlogf "%f\n" ll;
        stats_wadd s w ll
    done
  with Data.Eof -> 
    begin match !vs with 
    | None -> ()
    | Some varstats ->
      Array.iteri (fun i s -> nlogf "%d: avg = %f +/- %f\n" i 
        (stats_mean s) (stats_stderr s)) varstats
    end;
    nlogf "avg = %f +/- %f\n" (stats_mean s) (stats_stderr s);

;;
main ()
