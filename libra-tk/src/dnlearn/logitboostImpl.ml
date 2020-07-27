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

(*
 * Representation and functions on weighted regression data. 
 *)

type weighted_regression_data = 
  {x: int array array;   (* Feature values *)
   w: float array;       (* Real-valued weights: p_i * (1 - p_i) *)
   z: float array}       (* Target values: r_i - p_i *)

(* Partition data into two sets: those matching the new constraint
   and those not. *)
let split_data data var value =
  let x = data.x and w = data.w and z = data.z in
  let n = ref 0 in
  for i = 0 to Array.length x - 1 do
    if data.x.(i).(var) = value then
      incr n
  done;
  let n_t = !n and n_f = Array.length x - !n in
  let x_t = Array.make n_t [||] in
  let w_t = Array.make n_t 0. in
  let z_t = Array.make n_t 0. in
  let x_f = Array.make n_f [||] in
  let w_f = Array.make n_f 0. in
  let z_f = Array.make n_f 0. in
  let t_i = ref 0 and f_i = ref 0 in
  for i = 0 to Array.length x - 1 do
    if x.(i).(var) = value then begin
      x_t.(!t_i) <- x.(i);
      w_t.(!t_i) <- w.(i);
      z_t.(!t_i) <- z.(i);
      incr t_i
    end else begin
      x_f.(!f_i) <- x.(i);
      w_f.(!f_i) <- w.(i);
      z_f.(!f_i) <- z.(i);
      incr f_i
    end
  done;
  let data_t = {x=x_t; w=w_t; z=z_t} in
  let data_f = {x=x_f; w=w_f; z=z_f} in
  (data_t, data_f)


(* Remove low-weight instances.  Doesn't seem to speed things up significantly. *)
let prune_data frac data =
  let x = data.x and w = data.w and z = data.z in
  let total = Array.sumf w in
  let thresh = frac *. total /. (float_of_int (Array.length x)) in

  (* Compute new array sizes *)
  let n = ref 0 in
  for i = 0 to Array.length x - 1 do
    if w.(i) >= thresh then
      incr n
  done;

  (* Fill new arrays with instances of acceptable weight *)
  let x' = Array.make !n x.(0) in
  let w' = Array.make !n w.(0) in
  let z' = Array.make !n z.(0) in
  let i' = ref 0 in
  for i = 0 to Array.length x - 1 do
    if w.(i) >= thresh then begin
      x'.(!i') <- x.(i);
      w'.(!i') <- w.(i);
      z'.(!i') <- z.(i);
      incr i'
    end
  done;
  {x=x'; w=w'; z=z'}



(*
 * Representation and functions on decision trees.
 *)

module F = Mn.Factor
(* type tree = Leaf of float | Vertex of (int * int * tree * tree) *)

let rec dtval x = function
| F.Leaf wt -> wt
| F.Vertex (var, value, l, r) ->
  if x.(var) = value then dtval x l else dtval x r

(* Compute P(Y|X=x) using the specified model *)
let model_prob model x =
  let logprobs = Array.map (List.sumf_map (dtval x)) model in
  normalize_inplace_log logprobs;
  logprobs

let model_pred model x =
  let logprobs = Array.map (List.sumf_map (dtval x)) model in
  Array.argmax logprobs
  
(* Compute log loss of the model on data set *)
let logscore x y model =
  let total = ref 0. in
  for i = 0 to Array.length x - 1 do
    total := !total +. (model_prob model x.(i)).(y.(i))
  done;
  !total

(* Same as above, but accept probabilities as input *)
let logscore_p p y =
  let total = ref 0. in
  for i = 0 to Array.length y - 1 do
    total := !total +. log p.(y.(i)).(i)
  done;
  !total

(* Compute squared loss of the model on data set *)
let sqscore x y model =
  let total = ref 0. in
  for i = 0 to Array.length x - 1 do
    let p = (model_prob model x.(i)).(y.(i)) in
    total := !total +. (1. -. p) *. (1. -. p)
  done;
  !total

let sqscore_p p y =
  let total = ref 0. in
  for i = 0 to Array.length y - 1 do
    let p = p.(y.(i)).(i) in
    total := !total +. (1. -. p) *. (1. -. p)
  done;
  !total

let accuracy x y model =
  let total = ref 0. in
  for i = 0 to Array.length x - 1 do
    if model_pred model x.(i) = y.(i) then
      total := !total +. 1.
  done;
  !total /. (float_of_int (Array.length x))

let accuracy_p p y =
  let total = ref 0. in
  for i = 0 to Array.length y - 1 do
    let best_j = ref 0 in
    for j = 1 to Array.length p - 1 do
      if p.(j).(i) > p.(!best_j).(i) then
        best_j := j
    done;
    if !best_j = y.(i) then
      total := !total +. 1.
  done;
  !total /. (float_of_int (Array.length y))

let logscore2 x y model = logscore x y [|model; []|]
let sqscore2 x y model = sqscore x y [|model; []|]
let accuracy2 x y model = accuracy x y [|model; []|]


(*
 * Functions for learning a single decision tree from data
 *)

(* 
Data structure for representing a decision tree while we're
learning it.  In each iteration, we greedily add the best split, which
could be a modification to any leaf.  This is easiest to represent
using mutation, so we use lots of references.
*)
type temp_node = TLeaf of weighted_regression_data
               | TVertex of (int * int * (temp_node ref) * (temp_node ref))

type temp_dtree = temp_node ref

(* Computes reduction in squared loss according to the specified split 
let score_split data var value =
  let x = data.x and w = data.w and z = data.z in
  let t_sum = ref 0. in
  let t_n = ref 0. in
  let f_sum = ref 0. in
  let f_n = ref 0. in
  for i = 0 to Array.length x - 1 do
    if x.(i).(var) = value then begin
      t_sum := !t_sum +. z.(i) *. w.(i);
      t_n   := !t_n +. w.(i)
    end else begin
      f_sum := !f_sum +. z.(i) *. w.(i);
      f_n   := !f_n +. w.(i)
    end
  done;
  let t_mean = !t_sum /. !t_n in
  let f_mean = !f_sum /. !f_n in
  let base_mean = (!t_sum +. !f_sum) /. (!t_n +. !f_n) in
  (* dlogf "t_mean = %f; f_mean = %f; base_mean = %f\n" t_mean f_mean base_mean; *)
  let sqloss = ref 0. in
  let baseloss = ref 0. in
  for i = 0 to Array.length x - 1 do
    let mean = if x.(i).(var) = value then t_mean else f_mean in
    let delta = z.(i) -. mean in
    sqloss := !sqloss +. w.(i) *. delta *. delta;
    let base_delta = z.(i) -. base_mean in
    baseloss := !baseloss +. w.(i) *. base_delta *. base_delta
  done;
  let ret = !baseloss -. !sqloss in
  (* DEBUG *)
  dlogf "Split (%d, %d): %f\n" var value ret; 
  ret
 *)

(* Computes reduction in squared loss according to the specified split *)
let score_split_robust mincount base_num base_denom data var value =
  let x = data.x and w = data.w and z = data.z in
  let t_num = ref 0. in
  let t_denom = ref 0. in
  let n = ref 0 in
  for i = 0 to Array.length x - 1 do
    if x.(i).(var) = value then begin
      t_num   := !t_num +. z.(i);
      t_denom := !t_denom +. w.(i);
      incr n
    end
  done;
  if !n < mincount || Array.length x - !n < mincount then
    0.
  else
    let se num denom = if denom = 0. then 0. else num *. num /. denom in
    let l_se = se !t_num !t_denom in
    let r_se = se (base_num -. !t_num) (base_denom -. !t_denom) in 
    let base_se = se base_num base_denom in
    let ret = l_se +. r_se -. base_se in
    (* DEBUG dlogf "Split (%d, %d): %f\n" var value ret; *)
    ret


let best_split_in_list = function
| [] -> invalid_arg "empty list in best_split_in_list"
| x :: l ->
  List.fold_left 
    (fun (l,var,value, score) (l',var',value', score') -> 
      if score > score' then (l,var,value,score) 
                        else (l',var',value',score')) x l

let best_split mincount schema dtleaf =
  let data = match !dtleaf with TLeaf data -> data
             | TVertex(_,_,_,_) -> failwith "Error: Vertex should be leaf" in
  (* IDEA: Keep list of invalid splits, and pass it down to leaves? *)

  (* 
  let counts = Array.map (fun dim -> Array.make dim 0) schema in
  let x = data.x in
  let n = Array.length x in
  let numvars = Array.length schema in
  for i = 0 to n - 1 do
    let xi = x.(i) in
    for v = 0 to numvars - 1 do 
      let xiv = xi.(v) in
      counts.(v).(xiv) <- counts.(v).(xiv) + 1
    done
  done; 
  *)
  let gen_splits var dim =
    (* Don't put in two tests for binary variables; it's redundant *)
    if dim = 2 then [(var, 0)]
    else Array.to_list (Array.init dim (fun i -> (var, i))) in
  let splits = List.flatten (Array.to_list (Array.mapi gen_splits schema)) in 
  (*
  let nonzero (var, value) =
    counts.(var).(value) <> 0 && counts.(var).(value) <> n in 
  let splits = List.filter nonzero splits in 
  *)
  let base_num = Array.sumf data.z in
  let base_denom = Array.sumf data.w in
  let score_one (var,value) =
    let s = score_split_robust mincount base_num base_denom data var value in
    (dtleaf, var, value, s) in
  let scored = List.map score_one splits in
  if scored = [] then
    (dtleaf, 0, 0, 0.0)
  else
    best_split_in_list scored 


let leafmax = 100.

(* Robust leaf value (Li, 2010) *)
let leaf_val_robust numclasses data =
  let w = data.w and z = data.z in
  let num = Array.sumf z in
  let denom = Array.sumf w in
  (* dlogf "leaf: %d/%d * %f/%f = %f\n" (numclasses-1) numclasses 
    !num !denom ((j -. 1.) /. j *. !num /. !denom); *)
  if denom = 0. then 0.
  else 
    let ret = num /. denom in
    if ret > leafmax then leafmax
    else if ret < -.leafmax then -.leafmax
    else ret
 
exception Converged

(* Learn a single decision tree *)
let rec learn_dt_depth maxdepth mincount numclasses schema data =
  if maxdepth = 0 then
    F.Leaf (leaf_val_robust numclasses data)
  else begin
  (* HACK TODO -- this ref stuff is ugly... *)
  let split = best_split mincount schema (ref (TLeaf data)) in
  let (l, var, value, splitscore) = split in
  if splitscore <= 0.0 then 
    F.Leaf (leaf_val_robust numclasses data)
  else 
    let (t_data, f_data) = split_data data var value in
    let t_node = 
      learn_dt_depth (maxdepth-1) mincount numclasses schema t_data in
    let f_node = 
      learn_dt_depth (maxdepth-1) mincount numclasses schema f_data in
    F.Vertex (var, value, t_node, f_node)
  end

(* Learn a single decision tree *)
let learn_dt maxleaves mincount numclasses schema data =
  let dt = ref (TLeaf data) in
  let splits = ref [(best_split mincount schema dt)] in
  begin try
  for i = 2 to maxleaves do
    dlogf "...... Selecting split %d\n" i;
    (* Select and remove best split *)
    let s = best_split_in_list !splits in
    let (l, var, value, splitscore) = s in
    if splitscore <= 0.0 then raise Converged;
    dlogf "   chose (%d,%d): %f\n" var value splitscore;
    splits := List.filter (fun (l', _, _, _) -> (l != l')) !splits;
    (* Apply split, turning this leaf into a vertex via mutation. *)
    let data = match !l with TLeaf d -> d 
      | TVertex (_,_,_,_) -> failwith "Error: Vertex should be leaf" in
    let (t_data, f_data) = split_data data var value in
    let t_leaf = ref (TLeaf t_data) in
    let f_leaf = ref (TLeaf f_data) in 
    l := TVertex (var, value, t_leaf, f_leaf);
    (* Get new splits *)
    if i < maxleaves then
      splits := (best_split mincount schema t_leaf) 
             :: (best_split mincount schema f_leaf) :: !splits
  done;
  with Converged -> (); end;
  (* Convert to the more standard decision tree format *)
  let rec dt_to_dt dt =
    match !dt with 
    | TVertex (var, value, l, r) ->
        let dt_l = dt_to_dt l in
        let dt_r = dt_to_dt r in
        F.Vertex (var, value, dt_l, dt_r)
    | TLeaf data -> 
      F.Leaf (leaf_val_robust numclasses data) in
  dt_to_dt dt

let vwindow = 1

let boost maxtrees numclass shrinkage learner schema x y vx vy =
  (* Initialize *)
  let n = Array.length x in
  let vn = Array.length vx in
  let models = Array.make numclass [] in
  let initial_p = 1. /. (float_of_int numclass) in
  let p = Array.make_matrix numclass n initial_p in
  let r = Array.init numclass
    (fun j -> Array.map (fun y_i -> if y_i = j then 1. else 0.) y) in
  let raw_logp = Array.make_matrix numclass n 0.0 in
  let raw_vlogp = Array.make_matrix numclass vn 0.0 in
  let vs = logscore vx vy models in
  let last_vscores = Array.make vwindow vs in
  if log_exists log_verbose then begin
    vlogf "Initial score: %f\n" (logscore x y models);
    if vn > 0 then
      vlogf "Validation score: %f\n" vs
  end;
  try begin
  for m = 1 to maxtrees do
    dlogf "Learning tree %d...\n" m;
    let new_models = Array.make numclass (F.Leaf 0.0) in
    for j = 0 to numclass-1 do
      if numclass == 2 && j == 1 then begin
        models.(j) <- [F.Leaf 0.0];
        new_models.(j) <- F.Leaf 0.0
      end else begin
        dlogf "...class %d...\n" j;
        (* Call base learner to learn a function for class j *)
        let w = Array.map  (fun pi -> pi *. (1. -. pi)) p.(j) in
        let z = Array.map2 (fun ri pi -> ri -. pi) r.(j) p.(j) in
        let data = (* prune_data 0.1 *) {x=x; w=w; z=z} in
        let f_m = learner numclass schema data in 
        new_models.(j) <- f_m
      end
    done;

    let delta_logp = 
      Array.map (fun m -> Array.map (fun xi -> dtval xi m) x) new_models in
    let iters = ref 0 in
    let score_scaling alpha =
      let s = ref 0. in
      let sum_logp = Array.make numclass 0. in
      for i = 0 to n - 1 do
        for j = 0 to numclass - 1 do
          sum_logp.(j) <- raw_logp.(j).(i) +. alpha *. delta_logp.(j).(i)
        done;
        let total = alogsumexp sum_logp in
        s := !s +. sum_logp.(y.(i)) -. total
      done;
      (* dlogf "alpha=%f  score=%f\n" alpha (!s /. (float_of_int n)); *)
      incr iters;
      -.(!s) in
    let alpha =
      if shrinkage > 0. then shrinkage
      else Fminbr.fminbr 0. 10. score_scaling 0.01 in 
    dlogf "alpha = %f; iters = %d\n" alpha !iters;
    let rec rescale alpha = function
      | F.Leaf w -> F.Leaf (alpha *. w)
      | F.Vertex (var, value, l, r) ->
          let l' = rescale alpha l in
          let r' = rescale alpha r in
          F.Vertex (var, value, l', r') in
    let rescaled_models = Array.map (rescale alpha) new_models in
    for j = 0 to numclass - 1 do
      models.(j) <- rescaled_models.(j) :: models.(j)
    done;

    (* Recompute p *)
    for i = 0 to n - 1 do 
      let total = ref log_zero in
      for j = 0 to numclass - 1 do
        raw_logp.(j).(i) <- raw_logp.(j).(i) +.  alpha *. delta_logp.(j).(i);
        total := logsumexp2 !total raw_logp.(j).(i)
      done;
      for j = 0 to numclass - 1 do
        p.(j).(i) <- exp(raw_logp.(j).(i) -. !total)
      done; 
      (* DEBUG: This should be equivalent to the above, but slower. 
      for j = 0 to numclass - 1 do
        p.(j).(i) <- exp (model_prob models x.(i)).(j)
      done; *)
      (* dlogf "p.(%d).(%d) = %f\n" y.(i) i p.(y.(i)).(i) *)
    done; 
    (* TODO -- Test validation set here, if it exists! *)
    if log_exists log_verbose then begin
      let s1 = logscore_p p y in
      let s2 = sqscore_p p y in
      let s3 = 1. -. accuracy_p p y in 
      vlogf "%4d. Losses: log=%f  sq.=%f  0/1=%f\n" m s1 s2 s3;
    end;

    (* Score validation set, if it exists *)
    let vs = ref 0. in
    for i = 0 to vn - 1 do
      let total = ref log_zero in
      for j = 0 to numclass - 1 do
        let delta = dtval vx.(i) rescaled_models.(j) in
        raw_vlogp.(j).(i) <- raw_logp.(j).(i) +. delta;
        total := logsumexp2 !total raw_vlogp.(j).(i)
      done;
      vs := !vs +. raw_vlogp.(vy.(i)).(i) -. !total
    done;
    vlogf "              validation=%f\n" !vs;
    if vn > 0 && !vs <= last_vscores.(0) then
      raise Converged;
    for i = 0 to Array.length last_vscores - 2 do
      last_vscores.(i) <- last_vscores.(i+1)
    done;
    last_vscores.(Array.length last_vscores - 1) <- !vs
  done;
  models
  end with Converged -> Array.map List.tl models


let boost_trees maxtrees maxleaves shrinkage mincount numclass schema x y vx vy = 
  (* let learner = learn_dt_depth maxleaves in *)
  let learner = learn_dt maxleaves mincount in 
  (* TODO -- special case for 2 classes? *)
  boost maxtrees numclass shrinkage learner schema x y vx vy
