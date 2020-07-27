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
let modelfile = ref ""
let datafile = ref ""
let outfile = ref ""
let maxiter = ref 100
let sd = ref 1.0
let clib = ref true
let usecache = ref true
let lambda = ref 0.0 

let usage = "Usage: mnlearnw -m <model> -i <data> -o <output> [...]"
let args = Arg.align
 ([("-m",  Arg.Set_string modelfile, " Input Markov network structure") ;
   ("-i",  Arg.Set_string datafile, " Training data") ;
   ("-o",  Arg.Set_string outfile, " Output model") ;
   ("-maxiter", Arg.Set_int maxiter, " Maximum number of optimization iterations [100]") ;
   ("-sd", Arg.Set_float sd, " Standard deviation of Gaussian weight prior [1.0] (< 0 = inf)") ;
   ("-l1", Arg.Set_float lambda, " Weight of L1 norm [0.0]") ;
   ("-clib", Arg.Set clib, " Use C implementation for speed [true] "); 
   ("-noclib", Arg.Clear clib, " Use OCaml implementation");
   ("-cache", Arg.Set usecache, " Enable feature cache (much faster, but more memory) [true]");
   ("-nocache", Arg.Clear usecache, " Disable feature cache")]
   @ common_arguments)

let sqrt2pi = sqrt(2.0 *. 3.14159265) 

let lognormal mu sd x =
  if sd < 0.0 then 0.0 else
  -.(x -. mu) *. (x -. mu) /. (2.0 *. sd *. sd) -. log(sd *. sqrt2pi)

let deriv_lognormal mu sd x =
  if sd < 0.0 then 0.0 else
  -.(x -. mu)/.(sd *. sd)

(* Optimize the weights of the mn.  Warning: This modifies mn. *)
let pll_optimize data sd maxiter mn =
  let w = Array.make (Mn.numweights mn) 0.0 in
  let val_and_grad =
    if !clib && !usecache then
      (* Cached, written in C *)
      let cache = Pll.build_pll_cache_c mn data in
      Pll.pll_val_and_grad_cached_c cache
    else if !clib then
      (* Uncached, written in C *)
      let cache = Pll.build_pll_minicache_c mn data in
      Pll.pll_val_and_grad_c cache
    else if !usecache then
      (* Cached, written in OCaml *)
      let cache = Pll.build_pll_cache mn data in
      Pll.pll_val_and_grad_cached cache
    else
      (* Uncached, written in OCaml *)
      let cache = Pll.build_pll_minicache mn data in
      Pll.pll_val_and_grad cache in

  vlogf "Num weights = %d\n" (Array.length w);

  (* Construct a function that computes the regularized PLL *)
  let f x g =
    (* Compute value and gradient of PLL *)
    let v = val_and_grad x g in
    if log_exists log_debug then begin
      dlogf "x = %s\n" (string_of_farray x);
      dlogf "g = %s\n" (string_of_farray g);
    end;
    (* Negate (since we're minimizing), and add in L2 gradient *)
    for i = 0 to Array.length g - 1 do
      g.(i) <- -.g.(i) -. deriv_lognormal 0.0 sd x.(i)
    done;
    let v' = -.v -. (Array.sumf_map (lognormal 0.0 sd) x) in
    (* Double-check PLL computation *)
    if log_exists log_debug then begin
      Mn.set_weights mn x;
      let pll' = Array.sumf_map (fun (wt,x) -> wt *. (Mn.pll mn x)) data in
      if abs_float(pll' -. v) > 0.001 then
        dlogf "ERROR -- PLL computations disagree: %f vs. %f\n" v pll'
    end;
    let n = Array.sumf_map fst data in
    vlogf "pll = %f (reg: %f) (%f s)\n" (v/.n) (-.v'/.n) 
      (Timer.elapsed "pll_val_and_grad");
    Timer.clear "pll_val_and_grad";
    v' in 
  let (errcode, pll) = Lbfgs.minimize_l1 !lambda f w 1.0e-3 maxiter in
  vlogf "LBFGS errcode = %s\n" (Lbfgs.errstring errcode);
  (* This modifies the mn in place to use the new weights. *)
  Mn.set_weights mn w;
  (mn, pll)

let do_optimize_weights () =
  (* Read in model and data *) 
  let mn = 
    match filetype !modelfile with
    | BNFile -> Bn.to_mn (Bn.load_auto !modelfile)
    | _ -> Mn.load_auto !modelfile in
  vlogf "Num variables = %d\n" (Array.length mn.Mn.schema) ; 
  let data = Array.of_list (Data.input_wexample_list (open_in !datafile)) in
  let (mn', pll) = pll_optimize data !sd !maxiter mn in
  Mn.write_auto !outfile mn';
  nlogf "PLL = %f\n" pll ;
  nlogf "Total time: %f seconds\n" (Sys.time ())

let main () = 
  Arg.parse args ignore usage;
  if !modelfile = "" || !datafile = "" || !outfile = "" then
    Arg.usage args usage
  else begin
    common_log_init ();
    do_optimize_weights ()
  end

let _ = main ()
