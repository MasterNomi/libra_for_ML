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
module NSet = Circuit.NSet
module NMap = Circuit.NMap
open Circuit
open Ext

module F = Mn.Factor

type split = 
    {acf: Circuit.feature; 
     v: node;
     prob: float * float;
     vnodes: node list ; 
     (* mutable maxscore: float; *)
     mutable mine: int;
     mutable e: int option;
     mutable maxl: float; 
     mutable wt: float * float; 
     hashid: int}

(* This is greater-than instead of less-than so that we have a
   max-heap. *)
(* let split_lt d1 d2 = d1.maxscore > d2.maxscore *)
let split_eq s1 s2 = s1.hashid == s2.hashid

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

(*
 * Debugging
 *)

(* Print out a single candidate split. *)
let output_split out s =
  fprintf out "l=%f; e=%d; id=%d\n" s.maxl 
    (match s.e with None -> (-1) | Some n -> n) s.hashid

(*
 * Structure Learning
 *)

let gen_splits c prior_stddev circ ds logz f data =
  (* ENABLED *)
  (* For Boolean variables, only consider splitting on value=0,
     since splitting on value=1 is equivalent. *)
  let multival v = 
    not ((var_value v == 1) && circ.schema.(var_var v) == 2) in
  let vars = List.filter multival (Array.to_list circ.flat_vnodes) in 
  (* let vars = Array.to_list circ.flat_vnodes in *)
  let (scores, data_counts, model_counts) = 
    OptImpl.score_feature_extensions c prior_stddev circ ds logz data f vars in
  let scores = Array.of_list scores in
  let data_counts = Array.of_list data_counts in
  (* Generate splits *)
  let mksplit i v = 
    let vnodes = sibling_vars circ v in
      {acf=f; 
       prob=data_counts.(i); 
       v=v; 
       wt=snd scores.(i);
       maxl=fst scores.(i); 
       (* maxscore=n *. (fst scores.(i)); *)
       e=None; 
       mine=4; (* HACK: This should be 2 if half_split is true. *)
       vnodes=vnodes; 
       hashid = gen_hashid ()} in
  let splits = List.mapi mksplit vars in

  (* Filter to remove redundant conditions *)
  let ev = conditions_to_ev circ.schema (Array.of_list f.cond) in
  let split_ok s =
    let (var, value) = var_details s.v in
    let ok1 = ref false and ok2 = ref false in
    for i = 0 to circ.schema.(var) - 1 do
      if ev.(var).(i) <> log_zero then
        if i = value then ok1 := true
        else ok2 := true
    done;
    !ok1 && !ok2 in
  let splits = List.filter split_ok splits in
  
  (* Exclude features with small gains *)
  let orig_num = List.length splits in
  let maxl = List.fold_left (fun v s -> max v s.maxl) 0.000001 splits in
  let splits = List.filter (fun s -> s.maxl >= maxl /. 100.) splits in
  let new_num = List.length splits in
  vlogf "Pruned %d out of %d splits.\n" (orig_num - new_num) orig_num;
  splits


(* *** Initialization *** *)

let marginals schema data =
  (* Initialize marginal probabilities to empirical distribution,
       with one count of the uniform distribution added in to avoid
          zero probabilities (and thus weights of -inf). *)
  let u d = 1.0 /. (float_of_int d) in 
  let probs = Array.map (fun d -> Array.make d (u d)) schema in
  for i = 0 to Array.length data - 1 do
    let x = data.(i) in
    for j = 0 to Array.length data.(i) - 1 do
      probs.(j).(x.(j)) <- probs.(j).(x.(j)) +. 1.0
    done
  done;
  Array.iter normalize_inplace_raw probs;
  probs

(*let marginals schema data =
  (* Initialize marginal probabilities to empirical distribution,
       with one count of the uniform distribution added in to avoid
          zero probabilities (and thus weights of -inf). *)
  let u d = 1.0 /. float_of_int d in
  let probs = Array.map (fun d -> Array.make d (u d)) schema in
  let frac_count = 1. /. (float_of_int (Array.length data + 1)) in
  for i = 0 to Array.length data - 1 do
    let x = data.(i) in
    for j = 0 to Array.length data.(i) - 1 do
      probs.(j).(x.(j)) <- probs.(j).(x.(j)) +. frac_count
    done
  done;
  probs
*)
(*
let marginals schema data =
  let probs = Array.map (fun d -> Array.make d 0.) schema in
  let fraccount = 1. /. (float_of_int (Array.length data)) in
  for i = 0 to Array.length data - 1 do
    let x = data.(i) in
    for j = 0 to Array.length data.(i) - 1 do
      probs.(j).(x.(j)) <- probs.(j).(x.(j)) +. fraccount
    done
  done;
  probs
*)
let create_product_of_marginals schema probs =
  let fl = ref [] in
  let fp = ref [] in
  let r = create_times [] in
  let vnodes = make_vnodes schema in
  let vnodes_l = Array.map (Array.to_list) vnodes in
  for var = 0 to Array.length schema - 1 do 
    let p = create_plus [] in
    for i = 0 to schema.(var) - 1 do
      (* Add nodes for this value *)
      let logp = log probs.(var).(i) in
      let t = create_times [] in
      let theta = create_const logp in
      add_child t theta ;
      add_child t vnodes.(var).(i) ;
      add_child p t ;
      (* Construct feature *)
      let f = {acnode=theta;
               cond = [(true, var, i)]; 
               weight = logp;
               ev=[||]} in  (* TODO: Set this? *)
      fl := f :: !fl;
      fp := probs.(var).(i) :: !fp
    done ;
    add_child r p ;
  done ;
  let circ = of_graph schema vnodes vnodes_l r in
  (circ, !fl, !fp)

let init c prior_stddev schema data =
  let probs = marginals schema data in
  let (circ, fl, fp) = create_product_of_marginals schema probs in
  let ds = Circuit.create_deriv_scratch circ in
  let fsplits f = gen_splits c prior_stddev circ ds 0. f data in
  let splits = List.flatten (List.map fsplits fl) in
  (circ, fl, fp, splits)

(* *** End Initialization *** *)

let lazy_ancl size anc nl =
  let key = List.hd nl in
  if not (NMap.mem anc key) then 
    NMap.add anc key (relatedl_a size parents nl) ;
  NMap.find anc key

let lazy_anc size anc n =
  if not (NMap.mem anc n) then 
    NMap.add anc n (relatedl_a size parents [n]) ;
  NMap.find anc n

let build_ancestors circ = 
  let h = NMap.create 100 in
  let hl = NMap.create 100 in
  let size = circ.size in
  function  
    | [x] -> let anc = lazy_anc  size h  x in a_mem anc
    |  l  -> let anc = lazy_ancl size hl l in a_mem anc


(* Generate a new AC feature by splitting the specified AC 
 * feature acf on the specified variable v *)
let gen_feature f sense v = 
  let (svar, svalue) = var_details v in
  let cond' = (sense, svar, svalue) :: f.cond in
  (* Multiply new parameter times old, since they are not
   * mutually exclusive features. *)
  let theta = create_const 0.0 in
  let f' = {acnode = theta; cond = cond'; weight = 0.0; ev=[||]} in
  f'


let split_dist_change ancl visited good_splits n n' =
  if is_times n then begin
    (* If we're an ancestor of just the feature or just the var for
     * any split, that means we may be among the set of nodes that
     * the split depends on.  Invalidate these splits.  
     *)
    let invalidate () =
      let process split =
        let danc = ancl [split.acf.acnode] n in
        let vanc = ancl split.vnodes n in
        if (danc && not vanc) || (vanc && not danc) then begin
          split.e <- None ;
          SplitHSet.remove good_splits split 
        end in
      SplitHSet.iter process good_splits in

    (* If we reduce the number of children or create
     * multiple copies of the node, then any edge estimate that
     * depends on this node could be invalid.
     *)
    if num_children n' < num_children n then 
      invalidate () 
    else if not (NSet.mem visited n) then 
      NSet.add visited n 
    else
      invalidate () 
  end


(*********************************************************
 * Splitting features and computing edge costs.
 * These are the main user interface for this module.
 *********************************************************)
 (* TODO: Document these parameters... *)

(* HACK: Declared as a global to reduce garbage collection *)
let good_splits = SplitHSet.create 100

let split_feature half_split circ features ancl all_splits split = 
  let f = split.acf and v = split.v in
  let d_anc = ancl [f.acnode] in 
  let s_anc = ancl [v] in
  let vnodes = split.vnodes in 
  Hashes.clear_hashes () ;
  Hashes.clear_sets () ;
  let make_vhl v = (v, ancl [v], Hashes.get_hash ()) in
  let vhl = List.map make_vhl vnodes in

  let f' = gen_feature f true v in
  let h_ma = Hashes.get_hash () in
  let h_f = Hashes.get_hash () in
  let h_t = Hashes.get_hash () in 
  (* Replace orig. parameter node with a product with new feature's 
     parameter node *)
  let n' = create_node TimesNode [f'.acnode; f.acnode] in
  NMap.add h_t f.acnode n';
  let g' = if half_split then f' else gen_feature f false v in
  if not half_split then
    (let m' = create_node TimesNode [g'.acnode; f.acnode] in
    NMap.add h_f f.acnode m');
  let visited = Hashes.get_set () in
  (* HACK: Declared as a global to reduce garbage collection *)
  SplitHSet.clear good_splits;
  List.iter (fun s -> if s.e <> None then 
                      SplitHSet.add good_splits s) all_splits ;

  let changed n n' = 
    if SplitHSet.length good_splits = 0 then ()
    else split_dist_change ancl visited good_splits n n' in
  let ncreate r children = create_node r.details children in
  (* let cl = remove_single_children details [] children in 
     create_node details cl in *)
  let r = Graphclone.copy_ma ncreate ignore ignore changed 
           (d_anc, s_anc, h_t, h_f, vhl, h_ma) circ.root in
  flush stdout;
  circ.root <- r;
  rebuild circ ;
  if half_split then [f'] else [f'; g']

(* Alternate version -- less efficient, but always right. 
let delta_edges1 circ ancl max_edges split =
  let c = Circuit.copy circ in
  let root = c.root in
  let old_edges = List.sum (node_map num_children root) in
  let (_,_) = split_dist c ancl [] split [||] in
  let new_edges = List.sum (node_map num_children r) in
  new_edges - old_edges 
  *)

exception ManyEdges

let delta_edges half_split circ ancl max_edges split =

  Hashes.clear_hashes () ;
  Hashes.clear_sets () ; 
  let acf = split.acf and v = split.v in

  (* Find ancestors *)
  let d_anc = ancl [acf.acnode] in
  let s_anc = ancl [v] in
  let vnodes = split.vnodes in
  let v_anc = ancl vnodes in 

  (* Tricky hack to add edges every time the dist
     splitting code tries to create a ndoe. *)
  (* Initial number of edges is two, since adding a feature always 
   * adds a times node with an edge to the old param node and
   * an edge to a new param node. *)
  let edges = if half_split then ref 2 else ref 4 in
  let rem_edges e = 
    edges := !edges - e in
  let add_edges e = 
    edges := !edges + e; 
    if !edges > max_edges then raise ManyEdges in 

  (* Set of nodes we need to keep *)
  let marked  = Hashes.get_set () in 
  (* Set of nodes already removed *)
  let removed = Hashes.get_set () in
  let visit n =
    if not (NSet.mem marked n) && not (NSet.mem removed n) then 
      (* printf "Removing: "; print_endline n ; *)
      (rem_edges (num_children n) ;
       NSet.add removed n) in

  let rec mark_tree n =
    if not (NSet.mem marked n) && v_anc n then begin
      NSet.add marked n ;
      if NSet.mem removed n then 
        (* printf "Readding: "; print_endline n ; *)
        add_edges (num_children n) ;
      List.iter mark_tree n.children 
    end in

  let ncreate r children = 
    add_edges (List.length children) ; null_node in

  (* Allocate hashes *)
  let hashes = ref [] in
  let make_vhl v = 
    let h = Hashes.get_hash() in
    hashes := h :: !hashes;
    (v, ancl [v], h) in
  let vhl = List.map make_vhl vnodes in
  let h_ma = Hashes.get_hash () in
  let h_t = Hashes.get_hash () in 
  let h_f = Hashes.get_hash () in
  let changed n n' = () in
  let num_edges = 
    try 
      ignore(Graphclone.copy_ma ncreate mark_tree visit changed 
            (d_anc, s_anc, h_t, h_f, vhl, h_ma) circ.root);
      !edges 
    with ManyEdges -> max_edges + 1 in
  (* Return hashes and sets to the pool *)
  (*
  List.iter Hashes.release_hash [h_ma, h_t, h_f];
  List.iter Hashes.release_hash !hashes;
  List.iter Hashes.release_set [marked, removed]; *)
  num_edges
