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

(* Explanation of common variables:
 *
 * neighbors -- mapping from neighbor index to variable number
 *   e.g., if a potential has neighbors 5, 1, 2, 3, then
 *   this might be the array: [|5;1;2;3|]
 *
 * nmap -- mapping from variables to neighbor indices.
 *   e.g., if a potential has neighbors 5, 1, 2, 3, then
 *   this might be the array: [|-1;1;2;3;-1;0|], where
 *   "-1" denotes "not a neighbor".
 *
 * imsg -- array of incoming messages to the factor, 
 *   indexed by neighbor index.  Each is stored in log
 *   space and normalized so that:
 *     forall_i (sum_j exp imsg.(i).(j) = 1).
 *
 * omsg -- Same as imsg, but outgoing messages
 *)

(* Set a variable in conditions array to either value
   or !value, depending on sense (which is true or false).
   Returns a function that resets this change. *)
let set_condition conditions (sense, var, value) =
  if sense then begin
    (* Save original states *)
    let old = Array.copy conditions.(var) in
    (* Set conflicting values to false *)
    for i = 0 to Array.length conditions.(var) - 1 do
      if i <> value then
        conditions.(var).(i) <- false
    done;
    (* Construct reset function to restore states *)
    let reset () =
      for i = 0 to Array.length conditions.(var) - 1 do
        conditions.(var).(i) <- old.(i)
      done in
    reset
  end else begin
    (* Save original state *)
    let orig = conditions.(var).(value) in
    (* Set specified value to false *)
    conditions.(var).(value) <- false;
    (* Construct reset function *)
    let reset () = conditions.(var).(value) <- orig in
    reset
  end

module F = Mn.Factor

let rec tree_probs sumormax schema imsg omsg neighbors nmap conditions = function 
(* Explanation of parameters:
   neighbors: list of neighbors (0-based variable indices), stored as
       an array
   nmap: map from neighbor to its index in the imsg and omsg arrays.
       Represented as an array with a value for each variable.  If
       nmap.(i) < 0, then it is not a neighbor of this factor.
   imsg: incoming messages to this factor from neighboring variables.
       Order is described by nmap.
   omsg: outgoing messages from this factor to neighboring variables
   accu: log probability of the path from the root to the current node,
         according to the incoming messages.
   conditions: restrictions on variable assignments specified by the
       path from the root to this node in the decision tree.  Consists
       of an array with an array for each variable in the overall
       schema.  value of conditions.(i).(j) is 1 if the ith variable
       can take on its jth value and 0 if that value is excluded.  *)

(* If it's a leaf, add appropriate probability to all compatible slots *)
| F.Leaf wt ->
    (* Get sum (or max) of valid probabilities for input variable *)
    let total_prob i var =
      let result = ref log_zero in
      for value = 0 to schema.(var) - 1 do
        if conditions.(var).(value) then
          result := sumormax !result imsg.(i).(value)
      done;
      !result in
    (* Compute total or max probability for all valid assignments 
     * to variables at this leaf. *)
    let total_probs = Array.mapi total_prob neighbors in
    let baseprob = wt +. Array.sumf total_probs in

    if baseprob <> log_zero then 
      (* Add the corresponding probabilities to all compatible marginals *)
      (* var is the variable; i is its index in the arrays of
         incoming and outgoing messages. *)
      let set_marginals i var =
        (* Add actual probabilities *)
        for value = 0 to schema.(var) - 1 do
          if conditions.(var).(value) then
            omsg.(i).(value) <- sumormax omsg.(i).(value) 
              (baseprob +. imsg.(i).(value) -. total_probs.(i))
        done in
      Array.iteri set_marginals neighbors

(* Otherwise, condition on the splitting variable and recurse *) 
| F.Vertex (var, value, left, right) ->
    (* Recurse when var = value *)
    let reset = set_condition conditions (true, var, value) in
    tree_probs sumormax schema imsg omsg neighbors nmap conditions left;
    reset ();
    (* Recurse when var != value *)
    let reset = set_condition conditions (false, var, value) in
    tree_probs sumormax schema imsg omsg neighbors nmap conditions right;
    reset ()


(* Generate an array representing which values of each variable are legal *)
let condarray schema neighbors nmap f =
  let conditions = 
    Array.map (fun n -> Array.make schema.(n) true) neighbors in
  let apply_cond (sense,var,value) =
    for i = 0 to schema.(var) - 1 do
      if (i = value) <> sense then
        conditions.(nmap.(var)).(i) <- false
    done in
  Array.iter apply_cond f.F.cond;
  conditions

(* Compute the probability of a set of conditions, represented
 * in an array form generated by the above function. *)
let condprob sumormax c msg =
(* The following optimization only works for sum, not max, so it 
   has been disabled.
  (* If all values are legal, return log 1 (which is zero) *)
  if Array.for_all identity c then 0.
  (* Otherwise, add up probs of all legal values *)
  else *) begin
    let total = ref log_zero in
    for i = 0 to Array.length c - 1 do
      if c.(i) then
        total := sumormax !total msg.(i)
    done;
    !total
  end

(* Helper function for fs_probs, that works on a single feature. *)
let f_probs sumormax schema imsg omsg psum neighbors nmap f = 
  let wt = f.F.weight in
  let conditions = condarray schema neighbors nmap f in
  (* Compute log probability of the feature, given the input messages,
   * OR the log probability of the most likely state satisfying the
   * input messages. *)
  let p = Array.sumf (Array.map2 (condprob sumormax) conditions imsg) in

  (* For each involved variable... *) 
  if p <> log_zero then
  for i = 0 to Array.length omsg - 1 do
    (* Divide prob by total marginal of satisfying states of this var,
     * OR the prob of the most likely satisfying state, in order
     * to get the probability of just the other variables. *)
    let total = ref log_zero in
    for value = 0 to Array.length imsg.(i) - 1 do
      if conditions.(i).(value) then 
        total := sumormax !total imsg.(i).(value)
    done;
    let p = p -. !total in

    (* For each value that satisfies feature,
       add/max wt*prob with marginal, and prob with total. *)
    for value = 0 to Array.length imsg.(i) - 1 do
      if conditions.(i).(value) then begin
        omsg.(i).(value) <- 
          sumormax omsg.(i).(value) (wt +. p +. imsg.(i).(value));
        psum.(i).(value) <- sumormax psum.(i).(value) p;
      end
    done
  done

(*
let set_other_probs sumormax schema imsg omsg fl neighbors x logp i =
  if i = Array.length neighbors then begin
    (* If all variables have been set and the resulting state does not
       match any features in the list, then add probabilities. *)
    if Mn.F.fweight fl x = 0. then
      for v = 0 to Array.length neighbors - 1 do
        omsg.(v).(x.(v)) <- sumormax omsg.(v).(x.(v)) logp
      done 
  end else
    (* Set this variable and recurse on the rest *)
    for j = 0 to schema.(neighbors.(i)) - 1 do
      x.(i) <- j;
      let logp = logp +. imsg.(i).(j) in
      set_other_probs schema imsg omsg neighbors x logp (i+1)
    done

  let procstate varstate =
    (* Compute product of potential and all incoming messages *)
    let p = (F.log_value f varstate) +. 
      Array.sumf (Array.map2 (fun i dist -> dist.(varstate.(i))) neighbors imsg) in
    (* Add probability to appropriate element of matching outputs *)
    let var_prob i dist =
      dist.(varstate.(i)) <- sumormax dist.(varstate.(i)) p in
    Array.iter2 var_prob neighbors omsg in
  Mn.Varstate.iter_state schema (Array.to_list neighbors) procstate
  *)


(* Update messages for a potential that represents a set of
 * mutually exclusive features. *)
let fs_probs sumormax schema imsg omsg neighbors nmap fl =
  let psum = Array.map Array.copy omsg in
  List.iter (f_probs sumormax schema imsg omsg psum neighbors nmap) fl;
  if sumormax == logsumexp2 then
    for i = 0 to Array.length omsg - 1 do
      for value = 0 to Array.length omsg.(i) - 1 do
        (* If the probabilities of all relevant rules added up to
           less than 1, then add 1 times the remaining probability,
           to fill in the missing cases.  This is equivalent to
           saying that the value of the potential is 1 when none of
           the features is satisfied. *)
        if psum.(i).(value) < 0. then 
          let not_p = log(1. -. exp psum.(i).(value)) in
          omsg.(i).(value) <- 
            sumormax omsg.(i).(value) (not_p +. imsg.(i).(value))
      done
    done
  else ()
  (* HACK: If we're doing max-product, then we assume/require that 
   * every feature set is either exhaustive or has been converted to
   * a table by the caller. *)


(* Update the belief propagation messages in and out of a 
 * single potential 
 *)
let update_potential_messages sumormax schema imsg f =

  (* Create bidirectional mapping between variable and neighbor indices. *)
  let neighbors_l = F.vars f in
  let neighbors = Array.of_list neighbors_l in 
  let nmap = Array.make (Array.length schema) (-1) in
  Array.iteri (fun i n -> nmap.(n) <- i) neighbors;
  let omsg = Array.map (fun i -> Array.make schema.(i) log_zero) neighbors in

  begin match f with
  | F.Tree root ->
    (* TODO: create conditions array only once and share it, for efficiency? *)
    let conditions = Array.map (fun dim -> Array.make dim true) schema in
    tree_probs sumormax schema imsg omsg neighbors nmap conditions root

  | F.Table (vars,ranges,table) ->
    let procstate varstate =
      (* Compute product of potential and all incoming messages *)
      let p = (F.log_value varstate f) +. 
        Array.sumf (Array.map2 (fun i dist -> dist.(varstate.(i))) neighbors imsg) in
      (* Add probability to appropriate element of matching outputs *)
      let var_prob i dist =
        dist.(varstate.(i)) <- sumormax dist.(varstate.(i)) p in
      Array.iter2 var_prob neighbors omsg in
    Mn.Varstate.iter_state schema neighbors_l procstate
  | F.FeatureSet fl -> 
    fs_probs sumormax schema imsg omsg neighbors nmap fl
  | F.Feature feat -> 
    fs_probs sumormax schema imsg omsg neighbors nmap [feat]
  | F.Const _ -> ()
  end;
  for i = 0 to Array.length omsg - 1 do
    (* Factor out the incoming message *)
    for j = 0 to Array.length omsg.(i) - 1 do 
      if imsg.(i).(j) <> log_zero then 
        omsg.(i).(j) <- omsg.(i).(j) -. imsg.(i).(j)
    done;
    normalize_inplace_log omsg.(i)
  done;
  omsg

let pprod a1 a2 = Array.map2 ( +. ) a1 a2

let dist_divide a1 a2 =
  let a = Array.map2 (fun x1 x2 -> if x2 <> log_zero then x1 -. x2 else log_zero) a1 a2 in
  normalize_inplace_log a;
  a

let kld d1 d2 =
  let d1 = Array.map exp d1 in
  let d2 = Array.map exp d2 in
  Array.sumf (Array.map2 (fun a b -> a *. log(a /. b)) d1 d2)

let check_convergence threshold old_marg new_marg =
  let klds = Array.map2 kld old_marg new_marg in
  let maxkl = Array.max klds in
  dlogf "max kld = %f\n" maxkl;
  if maxkl < threshold then true else false

exception Converged

let run_bp sumormax max_iter threshold mn evidence = 
  let schema = Mn.schema mn in
  let numfact = Array.length (Mn.factors mn) in
  let evidence = Data.check_evidence schema evidence in
  let mn = Mn.simplify mn evidence in
  let allneighbors = 
    Array.map (fun f -> Array.of_list (F.vars f)) (Mn.factors mn) in
  let uniform dim = Array.make dim (log (1. /. (float_of_int dim))) in

  (* Create potential messages *)
  let gen_potential_msgs f  =
    let neighbors = Array.of_list (Mn.Factor.vars f) in
    Array.map (fun i -> uniform schema.(i)) neighbors in
  let potentials = Mn.factors mn in
  let potential_msgs = Array.map gen_potential_msgs potentials in
  
  (* Update repeatedly until convergence *)
  let marginals = ref (Array.map uniform schema) in
  begin try 
    for i = 1 to max_iter do

      (* Update messages using potentials *)
      dlogf "ITERATION %d:\n" i;
      let new_potentials = Array.map2 
        (update_potential_messages sumormax schema) potential_msgs potentials in
      Array.iter2 (fun m1 m2 -> Array.blit m1 0 m2 0 (Array.length m1)) 
        new_potentials potential_msgs;

      dlogf "Outgoing messages:\n";
      if log_exists log_debug then
        Array.iter (Data.output_marginals (log_stream log_debug)) 
          new_potentials;

      (* Update marginals *)
      let new_marginals = Array.map uniform schema in
      for i = 0 to numfact - 1 do 
        for j = 0 to Array.length allneighbors.(i) - 1 do
          new_marginals.(allneighbors.(i).(j)) <- 
            pprod new_marginals.(allneighbors.(i).(j)) potential_msgs.(i).(j)
        done
      done;
      Array.iter normalize_inplace_log new_marginals;

      dlogf "Marginals:\n";
      if log_exists log_debug then
        Data.output_marginals (log_stream log_debug) new_marginals;

      (* Check for convergence *)
      if check_convergence threshold !marginals new_marginals then 
        raise Converged
      else 
        marginals := new_marginals;

      (* Send messages back to the potentials if we haven't converged yet. *)
      for i = 0 to numfact - 1 do 
        for j = 0 to Array.length allneighbors.(i) - 1 do
          (* Incoming message = variable marginal / outgoing message *)
          potential_msgs.(i).(j) <- 
            dist_divide !marginals.(allneighbors.(i).(j)) potential_msgs.(i).(j)
        done
      done;
    done
  with Converged -> () end;
  dlogf "Marginals before returning:\n";
  if log_exists log_debug then
    Data.output_marginals (log_stream log_debug) !marginals;
  !marginals
