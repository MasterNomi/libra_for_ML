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
let qfile = ref ""
let evfile = ref ""
let modelfile = ref ""
let mfile = ref ""
let marg_outfile = ref ""
let sameev = ref false
let max_iter = ref 50
let threshold = ref 0.0001
let depnet = ref false
let roundrobin = ref false

(* Currently disabled. *)
let sameinit = ref false
let smooth_init = ref 0.0001 

let usage = "Usage: mf -m <model> -ev <evidence> [...]"
let args = Arg.align
 ([("-m", Arg.Set_string modelfile, " Model file (BN, MN, or DN)") ;
   ("-mo", Arg.Set_string marg_outfile, " Output file for marginals") ;
   ("-ev", Arg.Set_string evfile, " Evidence file");
   ("-sameev", Arg.Set sameev, " Use the same evidence for all queries");
   ("-q",  Arg.Set_string qfile,  " Query file");
   ("-maxiter", Arg.Set_int max_iter, " Maximum number of iterations [50]") ;
   ("-thresh", Arg.Set_float threshold, " Convergence threshold [0.0001]");
   ("-depnet", Arg.Set depnet, 
     " Interpret factors as CPDs in a cyclical dependency network");
   ("-roundrobin", Arg.Set roundrobin,
     " Update marginals in order instead of skipping unchanged vars")]
 (* Reenable sometime in the future: 
   ("-init", Arg.Set_string mfile, " Initial variable marginals");
   ("-sameinit", Arg.Set sameinit, " Use the same initialization for all queries");
   ("-smoothinit", Arg.Set_float smooth_init, " Prior counts of uniform distribution for smoothing the initial marginal") *) 
   @ common_arguments)


let read_marginals schema mstream =
  let marg = Data.input_marginals mstream in
  let norm_weight = !smooth_init in
  let smooth dim x = 
    (x +. (norm_weight/.(float_of_int dim))) /. (1. +. norm_weight) in
  Array.map (Array.map2 smooth schema) marg 

let get_marg () =
  (* Read in file *)
  let mn = match filetype !modelfile with
    | BNFile -> Bn.to_mn (Bn.load_auto !modelfile)
    | MNFile -> Mn.load_auto !modelfile
    | _ -> nlogf "ERROR: Unsupported file type.\n"; exit (-1) in
  let schema = Mn.schema mn in

  let has_mfile = !mfile <> "" in
  let mstream = if has_mfile then (open_in !mfile) else stdin in
  if not has_mfile then
    sameinit := true;
  if !evfile = "" then sameev := true;
  if !sameev then sameinit := true;

  (* Read in shared initialization for all queries *)
  let sharedinit =
    if !sameinit then
      if has_mfile then read_marginals schema mstream
      else [||] (* Empty array signifies "use the uniform distribution" *)
    else [||] in

  (* Actual function to get marginals *)
  let f ev=
    let init = 
      if !sameinit then sharedinit else (read_marginals schema mstream) in
    let mn = Mn.simplify mn ev in
    MfImpl.meanfield !depnet !roundrobin !max_iter 
      !threshold schema mn init in
  (f, schema)

let main () = 
  Arg.parse args ignore usage;
  if !modelfile = "" then
    (Arg.usage args usage; exit (-1));
  common_log_init();
  let (f, schema) = get_marg() in
  InfShared.run_marg false f schema !marg_outfile 
    !evfile !qfile !sameev

let _ = main ()
