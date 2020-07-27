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

let pi = 4. *. atan 1.

(* log of a normal distribution with given stddev and zero mean *)
(* UNUSED, since the log(pi sigma^2)/2 term acts as an ad hoc
   per-split penalty of 0.572.  It's best to handle this explicitly.
let lognormal stddev x = 
  if stddev > 0. then
    -.(x *. x) /. (2. *. stddev) -. log (pi *. stddev) /. 2.
  else 0.
 *)

let lognormal stddev x = 
  if stddev > 0. then
    -.(x *. x) /. (2. *. stddev) 
  else 0.

let dlognormal stddev x =
  if stddev > 0. then
    -. x /. stddev
  else 0.

(* NOTE: See Circuit and Mn for type definitions of their
 * respective features. *)

(* Callback for L-BFGS to get standard inclusive K-L divergence and gradient
 * according to an empirical distribution (samples).
 * Runs inference to get all probabilities.
 *)
let datall_val_and_grad prior_stddev circ ds true_fprobs n ev fa x g =
  let begtime = Sys.time() in

  (* *** Set weights *** *)
  let fweights = x in 
  assert(Array.length fweights = Array.length g);
  Array.iter2 set_feature_value fa x; 

  (* *** Get derivatives *** *)
  (* let ze = conditions_to_ev circ.schema [] in *)
  let e = example_to_ev circ.schema ev in
  get_derivatives_raw circ ds e ;
  let logz = ds.vr.(0) in
  let get_prob i f =
    if log_exists log_debug then begin
      Circuit.output_feature (log_stream log_debug) f;
      Circuit.output_node (log_stream log_debug) (feature_node f);
      dlogf "\ni = %d; id = %d; wt = %f\n" i (feature_node f).id fweights.(i)
    end;
    exp(ds.dr.((feature_node f).id) +. fweights.(i) -. logz) in
  let fprobs = Array.mapi get_prob fa in

  if log_exists log_debug then
    for i = 0 to Array.length fa - 1 do
      dlogf "%d. Id: %d  Weight: %f  Expected: %f  True: %f\n" 
        i (feature_node fa.(i)).id fweights.(i) fprobs.(i) true_fprobs.(i)
    done;

  (* *** Compute (negative) gradient *** *)
  for i = 0 to Array.length fa - 1 do
    let dprior = dlognormal prior_stddev fweights.(i) in
    g.(i) <- -.(n *. (true_fprobs.(i) -. fprobs.(i)) +. dprior)
  done ;
  let prior = Array.sumf_map (lognormal prior_stddev) fweights in
  let fx = Array.sumf (Array.map2 ( *. ) true_fprobs fweights) -. logz in

  let endtime = Sys.time() in
  vlogf "fx = %f (%f sec)\n" fx (endtime -. begtime);
  (-.fx) *. n -. prior


let data_ll prior_stddev circ ds true_fprobs fa n =
  let x = Array.map feature_value fa in
  datall_val_and_grad prior_stddev circ ds true_fprobs n [||] fa x x

let datall_optimize c prior_stddev circ true_counts n ev fa maxiter =
  let x = Array.map feature_value fa in 
  let ds = create_deriv_scratch circ in
  let llfunc = datall_val_and_grad prior_stddev circ ds true_counts n ev fa in
  let (errcode,ll) = Lbfgs.minimize_l1 c llfunc x 1.0e-5 maxiter in 
  nlogf "Errcode = %s; KLD = %f\n" (Lbfgs.errstring errcode) ll ;
  (-.ll)

(* *** *** *** *** *** *** *** *** *)

let empirical_fx_probs circ f data vars =
  (* Get counts *)
  let counts = Array.map (fun dim -> Array.make dim 0) circ.schema in
  let fcond = Array.of_list f.cond in
  let add_counts x =
    if Mn.Factor.fmatch x fcond then 
      for i = 0 to Array.length x - 1 do
        counts.(i).(x.(i)) <- counts.(i).(x.(i)) + 1
      done in
  Array.iter add_counts data;
  (* Normalize into probabilities *)
  let total = float_of_int (Array.length data) in
  let vnode_prob n =
    let (var, value) = var_details n in
    (float_of_int counts.(var).(value)) /. total in
  let ftotal = Array.count (fun x -> Mn.Factor.fmatch x fcond) data in
  let true_probs = List.map vnode_prob vars in
  let p_f = (float_of_int ftotal) /. total in
  let all_probs = List.map (fun p -> (p, p_f -. p)) true_probs in
  all_probs


let expected_fx_probs circ ds logz f vars = 
  (* Compute derivatives, conditioned on feature *)
  let fcond = Array.of_list f.cond in
  let ev = conditions_to_ev circ.schema fcond in
  get_derivatives_raw circ ds ev;
  (* Manually incorporate evidence on x *)
  for i = 0 to Array.length ev - 1 do
    for j = 0 to Array.length ev.(i) - 1 do
      if ev.(i).(j) = log_zero then
        ds.dr.(circ.vnodes.(i).(j).id) <- log_zero
    done
  done;
  (* Get probability P(f ^ x_i) for each variable x_i *)
  let node_to_prob n = exp(ds.dr.(n.id) -. logz) in
  let true_probs = List.map node_to_prob vars in
  (* P(f) = exp(value at root) /. Z *)
  let fprob = exp(ds.vr.(0) -. logz) in
  (* P(f ^ !x_i) = P(f) - P(f ^ x_i) *)
  let all_probs = List.map (fun p -> (p, fprob -. p)) true_probs in
  all_probs


(* Compute change in log likelihood when only the weights w_j and
   w_k of two mutually exclusive features f_j and f_k change *)
let fscore_2d c prior_stddev n (pt_data, pf_data) (pt_w, pf_w) =
  (* dlogf "true: %f %f  exp: %f %f\n" pt_data pf_data pt_w pf_w; *)
  (*print_string "l1: "; print_float c; print_string "\n";
  *)
  let f delta gradient = 
    (* Compute value *)
    let pt_w' = pt_w *. exp delta.(0) in
    let pf_w' = pf_w *. exp delta.(1) in
    let z' = pt_w' +. pf_w' +. 1. -. pt_w -. pf_w in
    let prior = lognormal prior_stddev delta.(0)
        +. lognormal prior_stddev delta.(1) in
    let raw_value = 
        delta.(0) *. pt_data +. delta.(1) *. pf_data -. log z' in
    let value = -.(raw_value *. n +. prior) -. gradient.(0) in

    (* Compute gradient *)
    let dprior_t = dlognormal prior_stddev delta.(0) in
    let dprior_f = dlognormal prior_stddev delta.(1) in
    let dft_dx = pt_data -. pt_w' /. z' in
    let dff_dx = pf_data -. pf_w' /. z' in
    gradient.(0) <- -.(n *. dft_dx +. dprior_t);
    gradient.(1) <- -.(n *. dff_dx +. dprior_f);
    value in

  (* Maximize log likelihood gain by minimizing its negative with LBFGS *)
  let x = [|0.;0.|] in
  let (errcode,loss) = Lbfgs.minimize_l1 c f x 1.0e-10 100 in
  (*let (errcode,loss) = Lbfgs.minimize_l1 0.0 f x 1.0e-10 100 in*)
  (* DEBUG *)
  if log_exists log_debug then
    dlogf "gain = %f  (%s)\n" (-.loss) (Lbfgs.errstring errcode);
  (-.loss, (x.(0), x.(1)))
  

(* 
(* Simpler scoring method that only changes the weight of the first feature. *)
let fscore_1d prior_stddev n (p_data, pf_data) (p_w, pf_w) =
  (* Compute change in log likelihood when only the weight w_j of
     feature f_j changes, so w'_j = w_j + delta_j: 

     log P_w(X) - log P_w'(X) = 
       delta_j f_j(X) - log (P_w(f_j) exp(delta_j) + (1-P_w(f_j))) 
     d/d delta_j log P_w(X) - log P_w'(X) = 
       f_j(X) - (P_w(f_j) exp(delta_j))/(P_w(f_j) exp(delta_j) + (1-P_w(f_j))) *)
  dlogf "true: %f  exp: %f\n" p_data p_w;
  let f delta gradient = 
    let p_w' = p_w *. exp delta.(0) in
    let dprior = dlognormal prior_stddev delta.(0) in
    let dfdx = p_data -. p_w' /. (p_w' +. 1. -. p_w) in
    gradient.(0) <- -.(n *. dfdx +. dprior);
    let value = -.(delta.(0) *. p_data -. log (p_w' +. 1. -. p_w)) *. n
        -. lognormal prior_stddev delta.(0) in
    (* dlogf "x=%f  f(x)=%f  df/dx=%f\n" delta.(0) value gradient.(0); *)
    value in

  (* Maximize log likelihood gain by minimizing its negative with LBFGS *)
  let x = [|0.|] in
  let (errcode,loss) = Lbfgs.minimizep f x 1.0e-10 100 in
  (* DEBUG *)
  dlogf "gain = %f  (%d)\n" (-.loss) errcode;
  (-.loss, (x.(0), 0.))
  *)

let score_feature_extensions c prior_stddev circ ds logz data f vars =
  Timer.start "emp_probs";
  let data_probs = empirical_fx_probs circ f data vars in
  Timer.stop "emp_probs";
  Timer.start "exp_probs";
  let model_probs = expected_fx_probs circ ds logz f vars in
  Timer.stop "exp_probs";
  let n = float_of_int (Array.length data) in
  Timer.start "score_2d";
  let ret = (List.map2 (fscore_2d c prior_stddev n) data_probs model_probs, 
      data_probs, model_probs) in
  Timer.stop "score_2d";
  ret
