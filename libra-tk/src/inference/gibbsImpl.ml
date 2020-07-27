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

(* Abstract interface for sampling from a Bayesian network, dependency
   network, or Markov network. *)
type model =
  {schema: int array;
   numvars: int;
   mb_dist: int array -> int -> float array;
   simplify: int array -> unit}

let bn_model bn =
  let b = ref bn in
  {schema = Bn.schema bn;
   numvars = Array.length (Bn.schema bn);
   mb_dist = (fun x v -> Bn.mb_prob !b x v);
   simplify = fun ev -> b := Bn.simplify bn ev}

let dn_model bn =
  let b = ref bn in
  {schema = Bn.schema bn;
   numvars = Array.length (Bn.schema bn);
   mb_dist = (fun x v -> Bn.cond_prob !b x v);
   simplify = fun ev -> b := Bn.simplify bn ev}

let mn_model mn =
  let m = ref mn in
  {schema = Mn.schema mn;
   numvars = Array.length (Mn.schema mn);
   mb_dist = (fun x v -> Array.map exp (Mn.mb_logdist !m x v));
   simplify = fun ev -> m := Mn.simplify mn ev}

type gibbs_params =
  {burn_iter: int;
   sample_iter: int;
   chains: int;
   rao_blackwellized: bool;
   prior: float}

let parse_gibbs_params burn_iter sample_iter chains speed rb prior = 

  (* Assign default values, if missing *)
  let sample_iter = if sample_iter < 0 then 1000 else sample_iter in
  let burn_iter =   if burn_iter < 0   then sample_iter/10 else burn_iter in
  let chains =      if chains < 0      then 1 else chains in

  (* Alternately, the speed string lets you choose one of several 
     predefined options *)
  let (burn_iter, sample_iter, chains) =
    match speed with
      "slowest" -> (100000, 1000000, 10)
    | "slower"  ->  (10000,  100000, 10)
    | "slow"    ->   (1000,   10000, 10)
    | "medium"  ->    (100,    1000, 10)
    | "fast"    ->    (100,    1000,  1)
    | "" ->  (burn_iter, sample_iter, chains)
    | _ -> (nlogf "ERROR: Unrecognized speed \"%s\".\n" speed; (0,0,0)) in

  {burn_iter=burn_iter; sample_iter=sample_iter; chains=chains;
   rao_blackwellized=rb; prior=prior} 


let dummy_on_init state = ()
let dummy_on_sample state i oldval newval probs = ()


(* Resample a single variable. *)
let resample model state i on_sample =
  (*assert(Array.for_all (fun xi -> xi >= 0) state);
  *)
  let oldval = state.(i) in
  let probs = model.mb_dist state i in
  let newval = Bn.sample_array probs in
  state.(i) <- newval;
  on_sample state i oldval newval probs
  
let burn_in_chain params model ev =
  (* Create initial state *)
  let schema = model.schema in 
  let numvars = Array.length schema in

  (* Interpret [||] as meaning "no evidence" *)
  let ev =
    if ev = [||] then
      Array.make numvars (-1)
    else
      ev in

  Data.check_point schema ev;

  let randstate i = 
    if ev.(i) < 0 then
      Random.int schema.(i)
    else
      ev.(i) in
  let state = Array.init numvars randstate in

  (* Burn-in *)
  for iter = 0 to params.burn_iter - 1 do
    for i = 0 to model.numvars - 1 do
      if ev.(i) < 0 then
        resample model state i dummy_on_sample
    done 
  done;
  state


(* Run Gibbs sampling.
 * on_init callback is used to set up any data structures necessary,
 *   given the initial state.
 * on_sample callback is called every time a variable is resampled.
 *)
let run_gibbs params model ev on_init on_sample =

  let ev = 
    if Array.length ev = 0 then
      Array.make model.numvars (-1)
    else
      ev in
    
  Data.check_point model.schema ev;

  (* Simplify model by conditioning on evidence *)
  model.simplify ev;

  (* Burn-in and sample from each chain *)
  for i = 0 to params.chains - 1 do 
    let state = burn_in_chain params model ev in
    on_init state;
    for iter = 0 to params.sample_iter - 1 do
      for i = 0 to model.numvars - 1 do 
        if ev.(i) < 0 then
          resample model state i on_sample
      done
    done
  done


(* Use Gibbs sampling to compute the probability of each query
 * configuration according to the joint distribution induced by the
 * samples. 
 *)
let run_gibbs_joint params out model ev queries =

  let ev = 
    if Array.length ev = 0 then
      Array.make model.numvars (-1)
    else
      ev in

  Timer.start "sampling";
  Data.check_point model.schema ev;
  Array.iter (Data.check_point model.schema) queries;

  (* Create counts, initialized with a uniform prior *)
  let num_qvar q = 
    let is_qvar qv evv = evv < 0 && qv >= 0 in
    Array.sum (Array.map2 (fun qv evv -> if is_qvar qv evv then 1 else 0) q ev) in
  let total_prior = params.prior in
  let prior_count q = total_prior /. (2. ** (float_of_int (num_qvar q))) in
  let counts = Array.map prior_count queries in 
  
  (* Keep track of number of variables differing between current state
     and each query.  Caching this lets us update the counts for each
     query in O(1) instead of O(n). *) 
  let diffvars = Array.make (Array.length queries) 0 in

  (* Define initialization callback:
   * Count number of incompatibilities between initial state and each query.
   *)
  let set_num_incompatible state qi q =
    let n = ref 0 in
    for i = 0 to Array.length q - 1 do
      (* Add one for each query variable that is incompatible with the state. *)
      if q.(i) >= 0 && state.(i) <> q.(i) then
        incr n
    done; 
    diffvars.(qi) <- !n in
  let on_init state =
    Array.iteri (set_num_incompatible state) queries in

  (* Define sampling callback: Increase counts for each matched state. *)
  let check_query probs i oldval newval qi q  =
    for j = 0 to Array.length probs - 1 do
      if probs.(j) = 0. then begin
        nlogf "WARNING: Zero probability of X_%d = %d\n" i j
      end
    done;
    (* Increase difference count if we used to match on this var. *)
    if q.(i) = oldval then 
      diffvars.(qi) <- diffvars.(qi) + 1;
    (* If all but one variables match, then the current var must be the
     * only non-matching var.  Add Rao-Blackwellized counts. *)
    if params.rao_blackwellized && diffvars.(qi) = 1 && q.(i) >= 0 then
      counts.(qi) <- counts.(qi) +. probs.(q.(i));
    (* Reduce difference count if we now match on this var. *)
    if q.(i) = newval then
      diffvars.(qi) <- diffvars.(qi) - 1;
    (* Increase count, if we're not Rao-Blackwellizing *)
    if (not params.rao_blackwellized) && diffvars.(qi) = 0 && q.(i) >= 0 then
      counts.(qi) <- counts.(qi) +. 1. in
  let on_sample state i oldval newval probs =
    out state i oldval newval probs;
    Array.iteri (check_query probs i oldval newval) queries in

  (* Run sampling *)
  run_gibbs params model ev on_init on_sample;

  (* Normalize *)
  let max_count q =  (float_of_int params.sample_iter) 
                  *. (float_of_int params.chains)
                  *. (float_of_int (num_qvar q)) +. total_prior in
  let ll = Array.map2 (fun q c -> log(c /. (max_count q))) queries counts in
  (ll, Timer.delta "sampling")



let run_gibbs_marg params process_marginals out model ev queries =

  let ev = 
    if Array.length ev = 0 then
      Array.make model.numvars (-1)
    else
      ev in

  Timer.start "sampling";

  (* Initialize prior *)
  let uniform dim = Array.make dim (params.prior /. (float_of_int dim)) in
  let marg_counts = Array.map uniform model.schema in

  (* Add counts *)
  let add_marg_count i j p =
    marg_counts.(i).(j) <- marg_counts.(i).(j) +. p in
  let on_sample state i oldval newval probs =
    out state i oldval newval probs;
    if params.rao_blackwellized then
      Array.iteri (add_marg_count i) probs 
    else 
      marg_counts.(i).(newval) <- marg_counts.(i).(newval) +. 1. in

  (* Run sampling *)
  run_gibbs params model ev dummy_on_init on_sample;

  (* Normalize, and maybe print out marginals *) 
  Array.iter normalize_inplace_raw marg_counts;

  (* Print out marginals, or do whatever the caller wants with them *)
  process_marginals marg_counts;

  (* Score all the queries *)
  let score_query q =
    let score_i i qval =
      if ev.(i) >= 0 || qval < 0 then 0.
      else log marg_counts.(i).(qval) in
    Array.sumf (Array.mapi score_i q) in
  let ll = Array.map score_query queries in
  (ll, Timer.delta "sampling")
