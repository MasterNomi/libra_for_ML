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

let qfile = ref ""
let evfile = ref ""
let modelfile = ref ""
let marg_outfile = ref ""
let sameev = ref false
let max_iter = ref 50
let threshold = ref 0.0001
let mpe = ref false

let bp_usage = "Usage: bp -m <model> -ev <evidence> [...]"
let mp_usage = "Usage: maxprod -m <model> -ev <evidence> [...]"

let shared_args1 = [("-m", Arg.Set_string modelfile, " Model file (BN or MN)")]
let shared_args2 = 
  [("-ev", Arg.Set_string evfile, " Evidence file");
   ("-sameev", Arg.Set sameev, " Use the same evidence for all queries") ;
   ("-q",  Arg.Set_string qfile,  " Query file");
   ("-maxiter", Arg.Set_int max_iter, " Maximum number of iterations [50]") ;
   ("-thresh", Arg.Set_float threshold, " Convergence threshold [0.0001]")]

let mp_args = Arg.align (shared_args1 @
  [("-mo", Arg.Set_string marg_outfile, " Output file for MPE states")] 
  @ shared_args2 @ common_arguments)

let bp_args = Arg.align (shared_args1 @
  [("-mo", Arg.Set_string marg_outfile, " Output file for marginals")] 
  @ shared_args2 @ common_arguments)

let load_mn file =
  match filetype file with
  | MNFile -> Mn.load_auto file
  | BNFile -> Bn.to_mn (Bn.load_auto file)
  | _ -> nlogf "ERROR: Unsupported model file type.\n"; exit(-1)

module F = Mn.Factor

let is_exhaustive schema fl =
  let vars = F.vars (F.FeatureSet fl) in
  let procstate x = List.exists (fun f -> F.fmatch x f.F.cond) fl in
  List.for_all (( = ) true) (Mn.Varstate.map_state schema vars procstate)

let fix_factor schema = function
  | F.FeatureSet fl ->
      if is_exhaustive schema fl then F.FeatureSet fl
      else F.to_table schema (F.FeatureSet fl)
  | F.Feature f -> F.to_table schema (F.Feature f)
  | f -> f

let complete_feature_sets mn =
  {mn with Mn.factors=Array.map (fix_factor mn.Mn.schema) mn.Mn.factors}

let get_marg () =
  let mn = load_mn !modelfile in
  let schema = Mn.schema mn in
  let mn = if !mpe then complete_feature_sets mn else mn in

  (* Process arguments, open streams *)
  let sumormax = if !mpe then max else logsumexp2 in
  let f ev  =
    BpImpl.run_bp sumormax !max_iter !threshold mn ev in
  (f, schema)

let main maxproduct = 
  mpe := maxproduct;
  let usage = if maxproduct then mp_usage else bp_usage in
  let args = if maxproduct then mp_args else bp_args in
  Arg.parse args ignore usage;
  if !modelfile = "" then
    (Arg.usage args usage; exit (-1));
  common_log_init ();
  let (f, schema) = get_marg() in
  InfShared.run_marg maxproduct f schema !marg_outfile 
    !evfile !qfile !sameev
