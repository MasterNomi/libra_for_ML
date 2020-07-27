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

open Ext
open Printf

module F = Mn.Factor

(*
 * Queue for a fixed number of integers, 0 through n.  When adding an
 * integer to the queue, it will only be added if it is not already a
 * member.  All operations except creation are constant time.
 *
 * Implementation consists of two fixed-size arrays, capable
 * of holding the entire set of integers.  The first (circ_data)
 * represents the queue order as a circular array, with phead and
 * ptail indexing the front and back, respectively.  The second
 * (mem_data) keeps track of which elements are in the queue.
 *)
type int_array_queue = 
  {circ_data: int array; 
   mem_data: bool array;
   mutable num_elems: int;
   mutable phead: int;
   mutable ptail: int}

let q_create dim =
  {circ_data = Array.make dim 0;
   mem_data = Array.make dim false;
   num_elems = 0;
   phead = 0;
   ptail = 0}

let q_size q = q.num_elems

let q_add q i =
  if q.mem_data.(i) = false then begin
    q.mem_data.(i) <- true;
    q.circ_data.(q.ptail) <- i;
    q.ptail <- (q.ptail + 1) mod (Array.length q.circ_data);
    q.num_elems <- q.num_elems + 1
  end
  
let q_get q =
  if q.num_elems <= 0 then 
    raise Not_found
  else begin
    let ret = q.circ_data.(q.phead) in
    q.mem_data.(ret) <- false;
    q.phead <- (q.phead + 1) mod (Array.length q.circ_data);
    q.num_elems <- q.num_elems - 1;
    ret
  end


(* Mean squared difference of the exponentiated values from two arrays.
 * Used for comparing probability distributions that are specified as
 * arrays of log probabilities. *)
let rmse x y =
  let sqdiff la lb = 
    let diff = exp la -. exp lb in 
    diff *. diff in
  let diffa = Array.map2 sqdiff x y in
  sqrt((Array.sumf diffa)/.(float_of_int (Array.length diffa)))

(* Minimum probability, in order to prevent underflow and nan *)
let neg_dist_epsilon = 1.0E-10

let neg_dist a =
  let total = Array.sumf_map exp a in
  Array.map (fun p -> log (max (total -. exp p) neg_dist_epsilon)) a


(*
 * Helper functions for mean field inference with FEATURES
 *)

(* Create hashes that map variable/value pairs to the features in which 
 * they appear. *)
let make_fhash pfa =
  let no_hash = Hashtbl.create 100 in
  let yes_hash = Hashtbl.create 100 in
  let add_to_hash (lp, f) (sense, var, value) = 
    Hashtbl.add (if sense then yes_hash else no_hash) (var, value) (lp, f) in
  let hashf (lp, f) = 
    Array.iter (add_to_hash (lp, f)) f.F.cond in
  Array.iter hashf pfa;
  (yes_hash, no_hash)

(* Update marginal for v using features *)
let update_marg_feature yh nh marginals neg_marginals v =
  (* Compute new marginal for v *)
  let vmarg = Array.make (Array.length marginals.(v)) 0. in
  for i = 0 to Array.length vmarg - 1 do
    let y_bfl = Hashtbl.find_all yh (v,i) in
    let n_bfl = Hashtbl.find_all nh (v,i) in
    let wparam_y (lp, f) = 
      f.F.weight *. (exp (!lp -. marginals.(v).(i))) in
    let wparam_n (lp, f) = 
      f.F.weight *. (exp (!lp -. neg_marginals.(v).(i))) in
    vmarg.(i) <- List.sumf_map wparam_y y_bfl -. List.sumf_map wparam_n n_bfl
  done;
  normalize_inplace_log vmarg;
  let neg_vmarg = neg_dist vmarg in
  let change = rmse vmarg marginals.(v) in
  (* Update cached feature probs *)
  for i = 0 to Array.length vmarg - 1 do
    let y_bfl = Hashtbl.find_all yh (v,i) in
    let minus_plus (lp, f) = 
      lp := !lp -. marginals.(v).(i) +. vmarg.(i) in
    List.iter minus_plus y_bfl ;
    let n_bfl = Hashtbl.find_all nh (v,i) in
    let plus_minus (lp, f) = 
      lp := !lp -. neg_marginals.(v).(i) +. neg_vmarg.(i) in
    List.iter plus_minus n_bfl 
  done;
  marginals.(v) <- vmarg;
  neg_marginals.(v) <- neg_vmarg;
  change


let factor_neighbors numvars factors =
  let factor_vars = Array.map Mn.Factor.vars factors in
  let var_neighbors = Array.init numvars (fun _ -> Hashset.create 10) in
  (* Add all neighbor combos within a clique *)
  let add_clique vl =
    List.iter (fun i -> List.iter (Hashset.add var_neighbors.(i)) vl) vl in
  Array.iter add_clique factor_vars;
  (* Convert to lists and remove self-neighborship *)
  Array.mapi (fun i h -> List.filter ((!=) i) (Hashset.to_list h)) var_neighbors


(*
 * Helper functions for mean field inference with CPDs
 * (needed for dependency networks)
 *)

let cpd_neighbors cpds =
  let cpd_vars = Array.map Mn.Factor.vars cpds in
  (* Given a list of inputs to each CPD... *)
  let numvars = Array.length cpds in
  let var_neighbors = Array.init numvars (fun _ -> Hashset.create 10) in
  (* ...Generate a list of CPDs for each input *)
  let add_neighbors cpd_i cpd_vars =
    let addone v = if v != cpd_i then Hashset.add var_neighbors.(v) cpd_i in
    List.iter addone cpd_vars in
  Array.iteri add_neighbors cpd_vars;
  Array.map Hashset.to_list var_neighbors


(* Update the marginal distribution of a single variable *)
(* Set target var to specific value in the marginals *)
let set_var marginals neg_marginals var value =
  Array.fill marginals.(var) 0 (Array.length marginals.(var)) log_zero;
  Array.fill neg_marginals.(var) 0 (Array.length neg_marginals.(var)) log_one;
  marginals.(var).(value) <- log_one;
  neg_marginals.(var).(value) <- log_zero 

(* Update marginal for v using CPDs *)
let update_marg_cpd cpds marginals neg_marginals v =
  let old_vmarg = Array.copy marginals.(v) in
  let vmarg = Array.make (Array.length marginals.(v)) 0. in
  for i = 0 to Array.length vmarg - 1 do
    set_var marginals neg_marginals v i; 
    vmarg.(i) <- F.expected_log_value cpds.(v) marginals neg_marginals 
  done;
  normalize_inplace_log vmarg;
  let neg_vmarg = neg_dist vmarg in
  let change = rmse old_vmarg vmarg in
  marginals.(v) <- vmarg;
  neg_marginals.(v) <- neg_vmarg;
  change

let build_features mn marginals neg_marginals =
  let get_bprob f = 
    let cond_to_wt (sense, var, value) =
      if sense then marginals.(var).(value)
      else neg_marginals.(var).(value) in
    let logprob_f = Array.sumf_map cond_to_wt f.F.cond in
    let lp = ref logprob_f in
    (lp, f) in
  let fa = Array.of_list (Mn.to_features mn) in
  Array.map get_bprob fa


(* Meanfield inference: Main loop *)
let rec meanfield_rec
    update_marg q neighbors max_iter threshold marginals neg_marginals i =

  (* Select and update next variable in queue *)
  if q_size q = 0 then nlogf "ERROR: Queue size is zero!\n";
  let v = q_get q in

  if log_exists log_debug then
    dlogf "Old marginal for %d: %s\n" v (string_of_farray marginals.(v));

  let change = update_marg marginals neg_marginals v in 

  if log_exists log_debug then
    dlogf "New marginal for %d: %s\nRMSE: %f\n" 
      v (string_of_farray marginals.(v)) change;

  (* If marginal changed, add neighbors *)
  if change > threshold then
    (* TRICKY: The queue already contains the full list of variables, in
    order, since they're never written over.  By manually setting the number
    of elements, we force it to cycle through that full set of variables one 
    more time.  We indicate the round-robin approach by an empty
    neighbor look-up table. *)
    if neighbors = [||] then q.num_elems <- Array.length marginals
    else List.iter (q_add q) neighbors.(v);

  if q_size q == 0 then begin
    vlogf "Converged after %d variable updates.\n" i;
    vlogf "(Equal to approximately %d complete iterations.)\n"
      (i / Array.length marginals)
  end else if i >= max_iter * Array.length marginals then
    vlogf "Reached the maximum number of variable updates.  Stopping.\n";

  (* Continue until convergence *)
  if q_size q > 0 && i < max_iter * Array.length marginals then
    meanfield_rec update_marg q neighbors max_iter threshold 
      marginals neg_marginals (i+1)

(* Mean field inference *)
let meanfield depnet roundrobin max_iter threshold schema mn init_marginals =
  Timer.clear "meanfield";
  Timer.start "meanfield";
  (* Copy initial marginals if present; 
   * otherwise, create a uniform initial distribution *)
  let marginals = 
    if Array.length init_marginals > 0 then
      Array.map (Array.map log) init_marginals 
    else
      let uniform_dist dim = 
        Array.make dim (log (1.0 /. float_of_int dim)) in
      Array.map uniform_dist schema in
  (* Compute probabilities of v_i != value_j for each i, j *)
  let neg_marginals = Array.map neg_dist marginals in
  let q = q_create (Array.length schema) in
  for i = 0 to Array.length schema - 1 do
    q_add q i
  done;
  (* Set neighbors, for intelligent updates *)
  let neighbors = 
    if roundrobin then [||] 
    else if depnet then cpd_neighbors mn.Mn.factors 
    else factor_neighbors (Array.length schema) mn.Mn.factors in
  (* Build update function *)
  let update_marg = 
    (* For DNs, assume factors are CPDs (and are in order!) *)
    if depnet then update_marg_cpd mn.Mn.factors 
    else 
      (* Otherwise, build features, compute initial probs, and make hashes *)
      let pfa = build_features mn marginals neg_marginals in
      let (yh, nh) = make_fhash pfa in
      update_marg_feature yh nh in
  meanfield_rec update_marg q neighbors max_iter threshold 
        marginals neg_marginals 1;
  dlogf "Opt time: %f\n" (Timer.delta "meanfield"); 
  marginals
