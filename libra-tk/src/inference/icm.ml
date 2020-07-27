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

let _ = Random.self_init ()

(* Globals used for command line parameters *)
let qfile = ref ""
let evfile = ref ""
let modelfile = ref ""
let depnet = ref false 
let marg_outfile = ref ""
let sameev = ref false
let restarts = ref 1
let max_iters = ref 100

let usage = "Usage: icm -m <model> [...]"
let args = Arg.align
 ([("-m", Arg.Set_string modelfile, " Model file (BN, MN, or DN)") ;
   ("-mo", Arg.Set_string marg_outfile, " Output file for MPE states");
   ("-depnet", Arg.Set depnet, " Treat BN as a DN when computing conditional probabilities");
   ("-ev", Arg.Set_string evfile, " Evidence file");
   ("-sameev", Arg.Set sameev, " Use the same evidence for all queries");
   ("-q",  Arg.Set_string qfile,  " Query file");
   ("-restarts", Arg.Set_int restarts, " Number of random restarts [1]");
   ("-maxiter", Arg.Set_int max_iters, " Maximum number of iterations [-1]");
   (* Add parameters for max time, num chains, num burn-in, num samples *)
   ("-seed", Arg.Int Random.init, " Random seed")]
   @ common_arguments)

open GibbsImpl

(* Returns difference in log-likelihood between two instances *)
let relative_ll model x x' =
  let state = Array.copy x in
  let total = ref 0.0 in
  for i = 0 to model.numvars - 1 do
    if x.(i) <> x'.(i) then begin
      let dist = model.mb_dist state i in
      total := !total +. log dist.(x.(i)) -. log dist.(x'.(i));
      state.(i) <- x'.(i)
    end
  done;
  !total

(* Convert state to a set of marginals *)
let state_to_marg schema x =
  let marg = Array.map (fun dim -> Array.make dim 0.) schema in
  Array.iteri (fun i value -> marg.(i).(value) <- 1.0) x;
  marg

(* Run ICM once, starting from the given state *)
let run_icm_once model ev init =
  (* Start with initial state *)
  let x = 
    if init = [||] then Array.make model.numvars 0
    else Array.copy init in
  (* Add evidence *)
  for i = 0 to model.numvars - 1 do 
    if ev.(i) >= 0 then
      x.(i) <- ev.(i)
  done;
  (* Update each non-evidence var in turn until there is no change *)
  let stop = ref false in
  let iters = ref 0 in
  while (not !stop && !iters != !max_iters) do
    stop := true;
    incr iters;
    for i = 0 to model.numvars - 1 do 
      if ev.(i) < 0 then begin
        let dist = model.mb_dist x i in
        let newval = Array.argmax dist in
        if newval != x.(i) then begin
          x.(i) <- newval;
          stop := false
        end
      end
    done
  done;
  if !iters = !max_iters then
    vlogf "Reached maximum number of iterations.\n";
  x

let run_icm model ev =
  let bestx = ref (Array.make model.numvars 0) in
  for i = 1 to !restarts do
    let init = Array.map Random.int model.schema in
    let x = run_icm_once model ev init in
    if relative_ll model x !bestx > 0. then
      bestx := x;
 (* dlogf "init=%s  x=%s\n" (string_of_iarray init)
      (string_of_iarray x); *)
  done;
  state_to_marg model.schema!bestx


let load_model () =
  (* Load a model (copied from gibbs.ml) *)
  if Bn.filename_is_xmod !modelfile || 
      Bn.filename_is_bif !modelfile || 
      Bn.filename_is_cn !modelfile then 
    let bn = Bn.load_auto !modelfile in
    if !depnet then GibbsImpl.dn_model bn
    else GibbsImpl.bn_model bn
  else
    let mn = Mn.load_auto !modelfile in
    GibbsImpl.mn_model mn
  

let main () = 
  Timer.start "total";
  Arg.parse args ignore usage;
  if !modelfile = "" then 
    (Arg.usage args usage; exit (-1));
  common_log_init();
  let model = load_model () in
  (* Run MPE inference *)
  InfShared.run_marg true (run_icm model) model.schema 
      !marg_outfile !evfile !qfile !sameev;
  vlogf "Total time: %fs\n" (Timer.elapsed "total")

let _ = main ()
