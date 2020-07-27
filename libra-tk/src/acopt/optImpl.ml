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

open Circuit
open Ext
open Printf

(* NOTE: See Circuit and Mn for type definitions of their
 * respective features. *)

(* ************************************** *)

type all_probs = {pf: float array;
                  pg: float array;
                  px: float array;
                  pff: float array array;
                  pfx: float array array;
                  pgx: float array array;
                  pfg: float array array;
                  mutable logz: float}

let create_probs numf numg numx =
  let pf  = Array.make numf 0.0 in
  let pg  = Array.make numg 0.0 in
  let px  = Array.make numx 0.0 in
  let pff = Array.make_matrix numf numf 0.0 in
  let pfx = Array.make_matrix numf numx 0.0 in
  let pgx = Array.make_matrix numg numx 0.0 in
  let pfg = Array.make_matrix numf numg 0.0 in
  let logz = 0.0 in
  {pf=pf; pg=pg; px=px; pff=pff; pfx=pfx; pgx=pgx; pfg=pfg; logz=logz}


(* Reset everything to zero.  Shouldn't need to use this. 
let clear_probs probs =
  let zero_array a = Array.fill a 0 (Array.length a) 0.0 in
  zero_array probs.pf;
  zero_array probs.pg;
  zero_array probs.px;
  Array.iter zero_array probs.pff;
  Array.iter zero_array probs.pfx;
  Array.iter zero_array probs.pgx;
  Array.iter zero_array probs.pfg;
  probs.logz <- 0.0
 *)

(* Get all marginal and pairwise probabilities *)
let infer_all circ ds probs bfa fa fweights =
  dlogf "Entering infer_all..."; 
  let numg = Array.length bfa in
  let numf = Array.length fa in
  let numx = Array.length circ.flat_vnodes in
  let fa_ids = Array.map (fun f -> (feature_node f).id) fa in
  let x_ids  = Array.map id circ.flat_vnodes in

  (* Create local aliases for the arrays in the probs struct. 
   * Since arrays are mutable, modifying these will fill in the
   * correct values in probs. *)
  let gprobs = probs.pg in
  let xprobs = probs.px in
  let fgprobs = probs.pfg in 
  let gxprobs = probs.pgx in 
  let dr = ds.dr in

  if Array.min fa_ids < 0 then begin
    let i = Array.argmin fa_ids in 
    nlogf "ERROR: Feature %d has no parameter node in the circuit.\n" i;
    assert(false)
  end;

  (* Compute logz and single variable marginals by differentiating 
   * conditioned on no evidence. *)
  let ze = conditions_to_ev circ.schema [||] in
  get_derivatives_raw circ ds ze ;
  (* debug "Got derivatives...\n"; *)
  let logz = ds.vr.(0) in
  let id_to_prob id = if id >= 0 then exp(dr.(id) -. logz) else log_zero in
  let id_to_wprob id w = exp(dr.(id) +. w -. logz) in
  for i = 0 to numx - 1 do
    xprobs.(i) <- id_to_prob x_ids.(i)
  done;
  (* Compute probabilities P(g_k), P(g_k ^ f_i), and P(g_k ^ x)
   * by differentiating conditioning on each g_k in turn. *)
  for k = 0 to numg - 1 do
    (* printf "P(g_%d) ...\n" k; flush stdout; *)
    let bf = bfa.(k) in
    let bev = conditions_to_ev circ.schema bf.Mn.Factor.cond in
    get_derivatives_raw circ ds bev ;
    gprobs.(k) <- exp(ds.vr.(0) -. logz); (* Update probability *)
    for i = 0 to numf - 1 do
      fgprobs.(i).(k) <- id_to_wprob fa_ids.(i) fweights.(i)
    done;
    for i = 0 to numx - 1 do
      gxprobs.(k).(i) <- id_to_prob x_ids.(i) ;
    done;
    (* Derivatives ignore evidence on x, so we must reincorporate it
     * manually. *)
    let xidx = ref 0 in
    for xi = 0 to Array.length bev - 1 do
      for xj = 0 to Array.length bev.(xi) - 1 do
        if bev.(xi).(xj) = log_zero then
          gxprobs.(k).(!xidx) <- 0.0 ;
        incr xidx
      done
    done;
  done ;

  (* Compute all P(f_i ^ f_j) *)
  let ffprobs = probs.pff in
  let fxprobs = probs.pfx in 
  let fprobs = probs.pf in
  for j = 0 to numf - 1 do
    (* printf "P(f_%d) ...\n" j; flush stdout; *)
    let f = fa.(j) in
    get_derivatives_raw circ ds f.ev ;
    fprobs.(j) <- exp(ds.vr.(0) -. logz);
    for i = 0 to numf - 1 do
      let p = id_to_wprob fa_ids.(i) fweights.(i) in
      ffprobs.(j).(i) <- p ;
      (* TODO SLOW (and unnecessary)... *)
      ffprobs.(i).(j) <- p 
    done;
    for i = 0 to numx - 1 do
      fxprobs.(j).(i) <- id_to_prob x_ids.(i)
    done;
    (* Derivatives ignore evidence on x, so we must reincorporate it
     * manually. *)
    let xidx = ref 0 in
    for xi = 0 to Array.length f.ev - 1 do
      for xj = 0 to Array.length f.ev.(xi) - 1 do
        if f.ev.(xi).(xj) = log_zero then
          fxprobs.(j).(!xidx) <- 0.0 ;
        incr xidx
      done
    done 
  done ;
  (* debug "All done!\n"; *)
  probs.logz <- logz;
  probs


(* Select pjf and pjg from a probs struct. *)
let make_jprobs probs j =
  (probs.pf, probs.pg, probs.pff.(j), probs.pfg.(j), probs.logz) 

  
let weighted_sum weights values =
  Array.sumf (Array.map2 ( *. ) weights values)

exception UnexpectedNo

(* d KLD/d w_j = sum_i w_i * (P(f_i f_j) - P(f_i) P(f_j))
               - sum_k w_k * (P(f_j g_k) - P(g_k) P(f_j)) *)
let kld_deriv fweights gweights (p_f, p_g, p_jf, p_jg, logz) j =
  let numf = Array.length p_f and numg = Array.length p_g in
  let p_j = p_f.(j) in 
  let idiff i = p_jf.(i) -. p_f.(i) *. p_j in
  let i_total = weighted_sum fweights (Array.init numf idiff) in
  let kdiff k = p_jg.(k) -. p_g.(k) *. p_j in
  let k_total = weighted_sum gweights (Array.init numg kdiff) in
  let deriv = i_total -. k_total in
  deriv



(* KLD = sum_i w_i P(f_i) - log Z - sum_k w_k P(g_k) *)
let kld_value fweights gweights p_f p_g logz =
  let i_total = weighted_sum fweights p_f in
  let k_total = weighted_sum gweights p_g in
  i_total -. k_total -. logz


(* Run inference and get KLD. Easier interface, useful for debugging. *)
let infer_kld_value circ bfa fa =
  let fweights = Array.map (fun f -> Circuit.const_value (feature_node f)) fa in
  let gweights = Array.map (fun bf -> bf.Mn.Factor.weight) bfa in
  let ds = Circuit.create_deriv_scratch circ in
  let probs = create_probs (Array.length fa) (Array.length bfa) 
    (Array.length circ.flat_vnodes) in
  let probs = infer_all circ ds probs bfa fa fweights in
  kld_value fweights gweights probs.pf probs.pg probs.logz


(* Callback for L-BFGS to get K-L divergence and gradient.
 * Runs inference to get all probabilities.
 *)
let kl_val_and_grad circ ds probs bfa fa x g =
  let begtime = Sys.time() in
  (* *** Set weights *** *)
  assert(Array.length x = Array.length g);
  Array.iter2 (fun f wt -> set_feature_value f wt) fa x; 
  (* *** Run inference *** *)
  let probs = infer_all circ ds probs bfa fa x in
  let fweights = x in
  let gweights = Array.map (fun bf -> bf.Mn.Factor.weight) bfa in

  (* DEBUG: Print out all inferred stuff *)
  if log_exists log_debug then begin
    let print_array a = Array.iter (dlogf "%f ") a; dlogf "\n" in
    dlogf "logz = %f\n" probs.logz;
    dlogf "fprobs:\n"; print_array probs.pf;
    dlogf "gprobs:\n"; print_array probs.pg;
    dlogf "ffprobs:\n"; Array.iter print_array probs.pff;
    dlogf "fgprobs:\n"; Array.iter print_array probs.pfg;
    dlogf "gweights:"; print_array gweights;
    dlogf "fweights:"; print_array fweights;
  end;

  (* *** Compute gradient *** *)
  for j = 0 to Array.length fa - 1 do
    g.(j) <- kld_deriv fweights gweights (make_jprobs probs j) j;
    dlogf "d f/d w_%d = %f\n" j g.(j) 
  done ;

  (* *** Compute value *** *)
  let fx = kld_value fweights gweights probs.pf probs.pg probs.logz in
  if log_exists log_debug then 
    Array.iteri (dlogf "%d: %f\n") x;
  let endtime = Sys.time() in
  nlogf "fx = %f (%f sec)\n" fx (endtime -. begtime); 
  fx


(* Callback for L-BFGS to get standard inclusive K-L divergence and gradient
 * according to an empirical distribution (samples).
 * Runs inference to get all probabilities.
 *)
let datakl_val_and_grad circ ds true_fprobs ev fa x g =
  let begtime = Sys.time() in

  (* *** Set weights *** *)
  let fweights = x in 
  assert(Array.length fweights = Array.length g);
  Array.iter2 (fun f wt -> set_feature_value f wt) fa x; 

  (* *** Get derivatives *** *)
  (* let ze = conditions_to_ev circ.schema [] in *)
  let e = example_to_ev circ.schema ev in
  get_derivatives_raw circ ds e ;
  let logz = ds.vr.(0) in
  let fprobs = 
    Array.mapi (fun i f -> exp(ds.dr.((feature_node f).id) +. fweights.(i) -. logz)) fa in

  if log_exists log_debug then
    for i = 0 to Array.length fa - 1 do
      dlogf "%d. Id: %d  Weight: %f  Expected: %f  True: %f\n" 
        i (feature_node fa.(i)).id fweights.(i) fprobs.(i) true_fprobs.(i)
    done;

  (* *** Compute (negative) gradient *** *)
  for i = 0 to Array.length fa - 1 do
    g.(i) <- fprobs.(i) -. true_fprobs.(i);
  done ;
  let fx = Array.sumf (Array.map2 ( *. ) true_fprobs fweights) -. logz in

  let endtime = Sys.time() in
  nlogf "fx = %f (%f sec)\n" fx (endtime -. begtime);
  (-.fx)

let datakl_optimize circ true_counts ev fa maxiter =
  let x = Array.map (fun f -> feature_value f) fa in 
  let ds = create_deriv_scratch circ in
  let klfunc = datakl_val_and_grad circ ds true_counts ev fa in
  (* let (errcode,kld) = Lbfgs.minimizep klfunc x 1.0e-5 100 in *)
  let (errcode,kld) = Lbfgs.minimize klfunc x 1.0e-5 maxiter in 
  nlogf "Errcode = %s; KLD = %f\n" (Lbfgs.errstring errcode) kld ;
  kld


let noise () = 0.0 (* Random.float 0.10 -. 0.05 *)

(* Run L-BFGS to optimize all circuit weights *)
let optimize circ bfa fa maxiter =
  let x = Array.map (fun f -> feature_value f +. noise ()) fa in 
  let ds = create_deriv_scratch circ in
  let probs = create_probs (Array.length fa) (Array.length bfa) 
    (Array.length circ.flat_vnodes) in
  let klfunc = kl_val_and_grad circ ds probs bfa fa in
  (* let (errcode,kld) = Lbfgs.minimize klfunc x 1.0e-5 100 in *)
  let (errcode,kld) = Lbfgs.minimize klfunc x 1.0e-5 maxiter in 
  nlogf "Errcode = %s; KLD = %f\n" (Lbfgs.errstring errcode) kld ;
  kld
