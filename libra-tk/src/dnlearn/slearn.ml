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

type dtree = Leaf of float | Vertex of int * int * dtree * dtree

type condition = bool * int * int 

type distribution = {dvar: int ; 
                     leaves: dtree list ;
                     cond: condition list;
                     ll: float}

type split = {d: distribution ; 
              v: node ; 
              vnodes: node list ; 
              maxscore: float;
              mutable maxl: float; 
              mutable l: float option; 
              hashid: int}

let split_cmp d1 d2 = compare d2.maxscore d1.maxscore
let split_eq s1 s2 = s1.hashid == s2.hashid

module SplitSet = Set.Make(struct 
                           type t = split
                           let compare = split_cmp
                         end)

let split_hash split = split.hashid

module SplitHash = Hashtbl.Make(struct
                                type t = split
                                let equal = split_eq
                                let hash = split_hash
                              end)
module SplitHSet =
struct
  include SplitHash

  let add ns x = add ns x ()
  let iter f ns = iter (fun x () -> f x) ns
  let fold f ns = fold (fun x () l -> f x l) ns
  let to_list ns = fold (fun x l -> x :: l) ns []
  let sum_map f ns = fold (fun x accu -> f x + accu) ns 0
  let sumf_map f ns = fold (fun x accu -> f x +. accu) ns 0.0
  let filter f ns = iter (fun n -> if not (f n) then remove ns n) ns
end

let global_hashid = ref 0

let gen_hashid () =
  let id = !global_hashid in
  incr global_hashid ;
  assert (!global_hashid > 0) ;
  id

let ll dist = dist.ll
let dvar dist = dist.dvar
let conditions dist = dist.cond

module F = Mn.Factor

let const_value = function
| Leaf w -> w
| Vertex(_,_,_,_) -> invalid_arg "const_value is undefined for Vertex"

let dist_to_features d =
  let mkfeature i n =
    let cond = (true, d.dvar, i) :: d.cond in
    {F.cond=cond; 
     F.weight=const_value n;
     F.weight_id=(-1)} in
  List.mapi mkfeature d.leaves

(*
 * Debugging
 *)

(* Print out a single candidate split. *)
let output_split out s =
  fprintf out "l=%f; e=%d; id=%d\n" s.maxl 
    (match s.e with None -> (-1) | Some n -> n) s.hashid

(* Print out a single distribution.  Lists the child var, its conditional
 * distribution given the parent configuration, and the parent configuration.
 *)
let output_dist out d =
  fprintf out "dvar: %d ; vals: " d.dvar ;
  List.iter (fun n -> fprintf out " %f" (exp (const_value n))) d.leaves ;
  fprintf out " ; cond:" ;
  List.iter (fun (sense, var, value) ->
    fprintf out " %cv%d_%d" (if sense then '+' else '-') var value) d.cond;
  fprintf out "\n"

let print_dist = output_dist stdout

(*
 * Structure Learning
 *)

  (*
let rec count_var_rec var counts = function
  | x :: l -> 
    let i = x.(var) in counts.(i) <- counts.(i) + 1 ;
    count_var_rec var counts l
  | [] -> ()

(* Number of times var takes on each value in the data *)
let count_var var numvalues data =
  let counts = Array.make numvalues 0 in
  count_var_rec var counts data ;
  (*
  let add_count x = counts.(x.(var)) <- counts.(x.(var)) + 1 in
  List.iter add_count data ; *)
  counts
  *)
  (*
let count_var var numvalues data = Array.map (List.length) data
*)

let cond_match condl x = 
  List.for_all (fun (sense,var,value) -> (x.(var) = value) = sense) condl
  
let count_var_c cond var numvalues data =
  let counts = Array.make numvalues 0 in 
  for i = 0 to Array.length data - 1 do
    let x = data.(i) in
    if cond_match cond x then
      counts.(x.(var)) <- counts.(x.(var)) + 1
  done;
  counts

let ml_estimate counts =
  let numvalues = Array.length counts in
  let prior_counts = 1.0 in
  let prior = prior_counts /. (float_of_int numvalues) in
  let total = prior_counts +. float_of_int (Array.sum counts) in
  let normalize c = log ((prior +. float_of_int c) /. total) in
  Array.map normalize counts

(* Log loss of var on data, according to distribution dist *)
let dist_ll logdist counts = 
  let kl p logq = (float_of_int p) *. logq in
  Array.sumf (Array.map2 kl counts logdist) 

let node_dist_ll nodes counts =
  let logp = Array.map const_value nodes in
  dist_ll logp counts

let get_consts n =
  assert (is_sot_dist n) ;
  let a = Array.make (num_children n) null_node in
  let cvalue c = var_value (List.find is_var c.children) in
  let cconst c = List.find is_const c.children in
  List.iter (fun c -> a.(cvalue c) <- cconst c) n.children ;
  a


(* Log loss of the given variable on the given data,
 * using a distribution estimated from that data (plus a prior). *)
let ml_ll counts =  
  let dist = ml_estimate counts in
  dist_ll dist counts


let split_counts_l cond var numvals data vl =
  let va = Array.of_list vl in
  let v_var = Array.map fst va in
  let v_val = Array.map snd va in
  let pro_a = Array.init (List.length vl) (fun _ -> Array.make numvals 0) in
  let con_a = Array.init (List.length vl) (fun _ -> Array.make numvals 0) in
  for i = 0 to Array.length data - 1 do
    let x = data.(i) in
    let xval = x.(var) in 
    if cond_match cond x then
      for j = 0 to Array.length va - 1 do
        if x.(v_var.(j)) = v_val.(j) then
          pro_a.(j).(xval) <- pro_a.(j).(xval) + 1
        else
          con_a.(j).(xval) <- con_a.(j).(xval) + 1
      done
  done;
  (pro_a, con_a)


(* Change in log likelihood due to splitting the given node on
   the given variable/value combo. *)
let delta_loglikelihood_l dist vl data =
  let numvalues = List.length dist.nodes in
  let (pro, con) = split_counts_l dist.cond dist.dvar numvalues data vl in
  Array.to_list (Array.map2 (fun p c -> ml_ll p +. ml_ll c -. dist.ll) pro con)

(* Initialize random number generator to a fixed seed, for
   reproducible results. *)
let _ = Random.init 1337

(* TODO -- this interface is icky... need better way to filter... *)
let gen_split circ data f dist =
  let f v = f dist.dvar (var_var v) in
  let vars = List.filter f (Array.to_list circ.flat_vnodes) in
  (* For Boolean variables, only consider splitting on value=0,
     since splitting on value=1 is equivalent. *)
  let multival v = 
    not ((var_value v == 1) && circ.schema.(var_var v) == 2) in
  let vars = List.filter multival vars in
  let vl = List.map var_details vars in
  let ll_l = delta_loglikelihood_l dist vl data in

  (* HACK: We subtract a small random float from each likelihood in order
   * to make them unique.  This is necessary because we're using
   * delta likelihood as a key in a Set data structure, which
   * removes duplicates. *)
  let mksplit v ll = 
    let vnodes = sibling_vars circ v in
    let ll = ll -. (Random.float 0.00001) in
    {d=dist ; v=v ; l=Some ll; maxl=ll;  maxscore=ll; e=None; mine=0;
     vnodes=vnodes ; hashid = gen_hashid ()} in
  let splits = List.map2 mksplit vars ll_l in
  List.filter (fun s -> s.maxl > 0.0) splits

(* *** Initialization *** *)

let var_marginals var dim data =
  ml_estimate (count_var_c [] var dim data)

let initial_marginals schema data =
  Array.mapi (fun var dim -> var_marginals var dim data) schema

(* Only works when no constants are shared. *)
let create_dists circ data =
  let mkdist n = 
    let var = dist_var n in
    let nodes = get_consts n in
    let numvals = Array.length nodes in
    let counts = count_var_c [] var numvals data in
    let ll = node_dist_ll nodes counts in
      {dvar = var; leaves = Leaf; ll = ll; cond = []}  in
  List.map mkdist nl

let init schema data =
  let circ = create_product_of_marginals schema data in
  let dists = create_dists circ data in 
  let splits = List.flatten (List.map (gen_split circ data ( != )) dists) in
  (circ, dists, splits)

(* *** End Initialization *** *)

(* TODO: is there a good way to detect such an event? *)
(* exception Invalid_split *)

let set_dist_ml nodes counts =
  let logp = ml_estimate counts in
  Array.iter2 set_const nodes logp

let split_dist_params dist v data =
  let nodes = Array.of_list dist.nodes in
  let nodes_t = Array.map ncopy nodes in
  let nodes_f = Array.map ncopy nodes in
  let numvalues = Array.length nodes in
  let (svar, svalue) = var_details v in
  let cond_t = (true, svar, svalue) :: dist.cond in
  let cond_f = (false, svar, svalue) :: dist.cond in
  let counts_t = count_var_c cond_t dist.dvar numvalues data in
  let counts_f = count_var_c cond_f dist.dvar numvalues data in
  set_dist_ml nodes_t counts_t ;
  set_dist_ml nodes_f counts_f ;
  let ll_t = node_dist_ll nodes_t counts_t in
  let ll_f = node_dist_ll nodes_f counts_f in
  let dist_t = {dvar = dist.dvar; nodes = Array.to_list nodes_t; 
                cond = cond_t; ll = ll_t} in
  let dist_f = {dvar = dist.dvar; nodes = Array.to_list nodes_f; 
                cond = cond_f; ll = ll_f} in
  (dist_t, dist_f)
