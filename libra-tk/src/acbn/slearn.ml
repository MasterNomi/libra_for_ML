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
open Circuit
open Ext


type condition = bool * int * int 

type distribution = {dvar: int ; 
                     nodes: node list ;
                     cond: condition list;
                     ll: float}

type split = {d: distribution ; 
              v: node ; 
              vnodes: node list ; 
              mutable maxscore: float;
              mutable mine: int;
              mutable e: int option;
              mutable maxl: float; 
              mutable l: float option; 
              hashid: int}

(* Use greater-than rather than less-than here so that our
 * min-heap returns the highest-scoring instance first. *)
let split_lt s1 s2 = s1.maxscore > s2.maxscore
let split_cmp s1 s2 = compare s2.maxscore s1.maxscore
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

let ll dist = dist.ll
let dvar dist = dist.dvar
let conditions dist = dist.cond

let dist_to_features d =
  let mkfeature i n =
    let cond = (true, d.dvar, i) :: d.cond in
    {acnode=n; 
     weight=const_value n;
     Circuit.cond=cond;
  (* HACK: we can leave this empty for now, since we're only
     printing out these features, not doing anything with them.
     For any other use, this would be broken. *)
     ev=[||]  } in
  List.mapi mkfeature d.nodes

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
  List.iter (fun n -> fprintf out " %f" (exp (const_value n))) d.nodes ;
  fprintf out " ; cond:" ;
  List.iter (fun (sense, var, value) ->
    fprintf out " %cv%d_%d" (if sense then '+' else '-') var value) d.cond;
  fprintf out "\n"

let print_dist = output_dist stdout

(*
 * Structure Learning
 *)

let cond_match condl x = 
  List.for_all (fun (sense,var,value) -> (x.(var) = value) = sense) condl
  
let count_var_c cond var numvalues data =
  let counts = Array.make numvalues 0. in 
  for i = 0 to Array.length data - 1 do
    let (wt, x) = data.(i) in
    if cond_match cond x then
      counts.(x.(var)) <- counts.(x.(var)) +. wt
  done;
  counts

let ml_estimate prior_counts counts =
  let numvalues = Array.length counts in
  let prior = prior_counts /. (float_of_int numvalues) in
  let total = prior_counts +. Array.sumf counts in
  let normalize c = log ((prior +. c) /. total) in
  Array.map normalize counts

(* Log loss of var on data, according to distribution dist *)
let dist_ll logdist counts = 
  let kl p logq = p *. logq in
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
let ml_ll prior counts =  
  let dist = ml_estimate prior counts in
  dist_ll dist counts


let split_counts_l cond var numvals data vl =
  let va = Array.of_list vl in
  let v_var = Array.map fst va in
  let v_val = Array.map snd va in
  let pro_a = Array.make_matrix (List.length vl) numvals 0. in
  let con_a = Array.make_matrix (List.length vl) numvals 0. in
  for i = 0 to Array.length data - 1 do
    let (wt, x) = data.(i) in
    let xval = x.(var) in 
    if cond_match cond x then
      for j = 0 to Array.length va - 1 do
        if x.(v_var.(j)) = v_val.(j) then
          pro_a.(j).(xval) <- pro_a.(j).(xval) +. wt
        else
          con_a.(j).(xval) <- con_a.(j).(xval) +. wt
      done
  done;
  (pro_a, con_a)


(* Change in log likelihood due to splitting the given node on
   the given variable/value combo. *)
let delta_loglikelihood_l prior dist vl data =
  let numvalues = List.length dist.nodes in
  let (pro, con) = split_counts_l dist.cond dist.dvar numvalues data vl in
  Array.to_list (Array.map2 (fun p c -> ml_ll prior p +. ml_ll prior c -. dist.ll) pro con)


(* TODO -- this interface is icky... need better way to filter... *)
let gen_split prior circ data f dist =
  let f v = f dist.dvar (var_var v) in
  let vars = List.filter f (Array.to_list circ.flat_vnodes) in
  (* For Boolean variables, only consider splitting on value=0,
     since splitting on value=1 is equivalent. *)
  let multival v = 
    not ((var_value v == 1) && circ.schema.(var_var v) == 2) in
  let vars = List.filter multival vars in
  let vl = List.map var_details vars in
  let ll_l = delta_loglikelihood_l prior dist vl data in

  let mksplit v ll = 
    let vnodes = sibling_vars circ v in
    {d=dist ; v=v ; l=Some ll; maxl=ll;  maxscore=ll; e=None; mine=0;
     vnodes=vnodes ; hashid = gen_hashid ()} in
  let splits = List.map2 mksplit vars ll_l in
  List.filter (fun s -> s.maxl > 0.0) splits

(* *** Initialization *** *)

let create_product_of_marginals prior s data =
  let r = create_times [] in
  let vnodes = make_vnodes s in
  let vnodes_l = Array.map (Array.to_list) vnodes in
  for var = 0 to Array.length vnodes - 1 do
    let p = create_plus [] in
    let numvals = s.(var) in 
    let counts = count_var_c [] var numvals data in
    let logp = ml_estimate prior counts in
    for i = 0 to numvals - 1 do
      let t = create_times [] in
      add_child t (create_const logp.(i)) ;
      add_child t vnodes.(var).(i) ;
      add_child p t 
    done ;
    add_child r p ;
  done ;
  of_graph s vnodes vnodes_l r

(* Only works when no constants are shared. *)
let create_dists circ data =
  let nl = all_var_dists circ in
  let mkdist n = 
    let var = dist_var n in
    let nodes = get_consts n in
    let numvals = Array.length nodes in
    let counts = count_var_c [] var numvals data in
    let ll = node_dist_ll nodes counts in
      {dvar = var; nodes = Array.to_list nodes; ll = ll; cond = []}  in
  List.map mkdist nl

let init prior schema data =
  let circ = create_product_of_marginals prior schema data in
  let dists = create_dists circ data in 
  let splits = List.flatten (List.map (gen_split prior circ data ( != )) dists) in
  (circ, dists, splits)

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

(* TODO: is there a good way to detect such an event? *)
(* exception Invalid_split *)

let set_dist_ml prior nodes counts =
  let logp = ml_estimate prior counts in
  Array.iter2 set_const nodes logp

let split_dist_params prior dist v data =
  let nodes = Array.of_list dist.nodes in
  let nodes_t = Array.map ncopy nodes in
  let nodes_f = Array.map ncopy nodes in
  let numvalues = Array.length nodes in
  let (svar, svalue) = var_details v in
  let cond_t = (true, svar, svalue) :: dist.cond in
  let cond_f = (false, svar, svalue) :: dist.cond in
  let counts_t = count_var_c cond_t dist.dvar numvalues data in
  let counts_f = count_var_c cond_f dist.dvar numvalues data in
  set_dist_ml prior nodes_t counts_t ;
  set_dist_ml prior nodes_f counts_f ;
  let ll_t = node_dist_ll nodes_t counts_t in
  let ll_f = node_dist_ll nodes_f counts_f in
  let dist_t = {dvar = dist.dvar; nodes = Array.to_list nodes_t; 
                cond = cond_t; ll = ll_t} in
  let dist_f = {dvar = dist.dvar; nodes = Array.to_list nodes_f; 
                cond = cond_f; ll = ll_f} in
  (dist_t, dist_f)


(* TODO: what do we do when multiple things want to ref. this? *)
let rec remove_single_children details accu = function
  | [] -> accu
  | x :: l -> 
    let accu' = 
      if details = x.details then 
        (printf "Removed single child.\n" ; x.children @ accu)
      else 
        x :: accu in
    remove_single_children details accu' l

(* let remove_single_children enter exit details accu l = l *)

(* Generic function to copy a sub-tree according to some criteria.
   The copied sub-tree is guaranteed to not reference any of the
   original descendants of r. *)
let rec copy_tree ncreate enter changed f_rec f_link h r =
  if NMap.mem h r then NMap.find h r 
  else if not (f_rec r) then r
  else begin
    enter r ;
    let children = List.filter f_link r.children in
    let copy_child = copy_tree ncreate enter changed f_rec f_link h in
    let c_copies = List.map copy_child children in
    let r_copy = 
      match c_copies with 
        [x] -> x    (* Skip nodes with just one child *)
      |  l  -> ncreate r.details c_copies  in
    NMap.add h r r_copy ; 
    if List.length c_copies = 1 then changed r null_node 
                                else changed r r_copy ; 
    r_copy
  end


(* Copy a node that is only an ancestor of dist *)
let copy_dist ncreate enter changed anc h =
  let f_rec n = not (is_const n) && anc n in
  let f_link n = true in
  copy_tree ncreate enter changed f_rec f_link h


(* Copy a node that is only an ancestor of the split *)
let copy_v ncreate enter changed (v,anc,h) vhl =
  (* Copy ancestors of v, omitting redundant literals *)
  let f_rec n = n != v && anc n in 
  (* Omit links to mutually exclusive subtrees *)
  let f_link n =
    let v_parent = List.exists (fun (_,a,_) -> a n) vhl in
    n != v && (anc n || not v_parent) in
  copy_tree ncreate enter changed f_rec f_link h


(* Copy a node that is an ancestor of both split and dist *)
let rec copy_ma ncreate mark_tree enter changed 
      (d_anc, s_anc, h_t, h_f, vhl, h_ma) r =
  enter r ;
  if NMap.mem h_ma r then NMap.find h_ma r
  else begin

  (* Case 1: At least one child is also a mutual ancestor.  Recurse. 
   *)
  let ma_children () = 
    let copy_child c = 
      match (d_anc c, s_anc c) with
        (true, true) -> copy_ma ncreate mark_tree enter changed
            (d_anc, s_anc, h_t, h_f, vhl, h_ma) c
      | (true, false) -> copy_dist ncreate enter changed d_anc h_f c
      | (false, _) -> mark_tree c ; c in
    List.map copy_child r.children in

  (* Case 2: This node is the relevant mutual ancestor, so split on
   *         the variable here and link up copies of the subcircuits.
   *)
  let no_ma_children () =
    let d_child = List.find d_anc r.children in
    let s_child = List.find s_anc r.children in
    let nonanc c = not (d_anc c) && not (s_anc c) in
    let others = List.filter nonanc r.children in
    if is_var s_child then
      let d_copy = copy_dist ncreate enter changed d_anc h_t d_child in
      d_copy :: (s_child :: others)
    else begin
      let d_t = copy_dist ncreate enter changed d_anc h_t d_child in
      let d_f = copy_dist ncreate enter changed d_anc h_f d_child in
      let v_chosen = 
        List.filter (fun (v,anc,h) -> anc s_child) vhl in
      let pchild (v,a,h) = 
        let d_copy = if s_anc v then d_t else d_f in
        let s_copy = copy_v ncreate enter changed (v,a,h) vhl s_child in
        ncreate TimesNode [v; d_copy; s_copy] in
      let pchildren = List.map pchild v_chosen in
      (ncreate PlusNode pchildren) :: others 
    end in

  let is_ma c = s_anc c && d_anc c in
  let has_ma_child = not (is_times r) || List.exists is_ma r.children in
  let children = if has_ma_child then ma_children () 
                                 else no_ma_children () in
  let r_copy = match children with   
                 [x] -> x       (* Skip nodes with just one child *)
               |  l  -> ncreate r.details children in
  NMap.add h_ma r r_copy ;
  (* If we've reached a mutual ancestor, then this node has changed. *)
  (if not has_ma_child then begin
    if List.length children = 1 then (changed r null_node)
                                else (changed r r_copy)
  end) ;
  r_copy
  end 


let split_dist_change ancl visited good_splits n n' =
  if is_times n then begin
    (* If we're an ancestor of just the dist or just the var for
     * any split, that means we may be among the set of nodes that
     * the split depends on.  Invalidate these splits.  
     *)
    let invalidate () =
      let process split =
        let danc = ancl split.d.nodes n in
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

(* Reusable hashes *)
let tmp_hashes: (node NMap.t list ref) = ref [] 
let unused_hashes = ref []
let new_hash () = NMap.create 100

let tmp_sets: (unit NSet.t list ref) = ref []
let unused_sets = ref []
let new_set () = NSet.create 100

let get_hash () =
  match !unused_hashes with
    []   -> let h = new_hash () in
            tmp_hashes := h :: !tmp_hashes ; h
  | h::l -> unused_hashes := l ; h

let get_set () =
  match !unused_sets with
    []   -> let h = new_set () in
            tmp_sets := h :: !tmp_sets ; h
  | h::l -> unused_sets := l ; h

let clear_hashes () =
  unused_hashes := !tmp_hashes ;
  List.iter NMap.clear !tmp_hashes

let clear_sets () =
  unused_sets := !tmp_sets ;
  List.iter NSet.clear !tmp_sets


let split_dist prior circ ancl all_splits split data = 
  let dist = split.d and v = split.v in
  let d_anc = ancl dist.nodes in
  let s_anc = ancl [v] in
  let vnodes = split.vnodes in 
  clear_hashes () ;
  clear_sets () ;
  let make_vhl v = (v, ancl [v], get_hash ()) in
  let vhl = List.map make_vhl vnodes in

  let (dist_t, dist_f) = split_dist_params prior dist v data in
  let h_ma = get_hash () in
  let h_t = get_hash () in 
  let h_f = get_hash () in
  List.iter2 (NMap.add h_t) dist.nodes dist_t.nodes ;
  List.iter2 (NMap.add h_f) dist.nodes dist_f.nodes ;
  let visited = get_set () in
  let good_splits = SplitHSet.create 100 in
  List.iter (fun s -> if s.e <> None then 
                      SplitHSet.add good_splits s) all_splits ;

  let changed n n' = 
    if SplitHSet.length good_splits = 0 then ()
    else split_dist_change ancl visited good_splits n n' in
  let ncreate details children = create_node details children in
  (* let cl = remove_single_children details [] children in 
     create_node details cl in *)
  let r = copy_ma ncreate ignore ignore changed 
           (d_anc, s_anc, h_t, h_f, vhl, h_ma) circ.root in
  circ.root <- r;
  rebuild circ ;
  (dist_t, dist_f)

(* Alternate version -- less efficient, but always right. 
let delta_edges1 prior circ ancl max_edges split =
  let c = Circuit.copy circ in
  let root = c.root in
  let old_edges = List.sum (node_map num_children root) in
  let (_,_) = split_dist prior c ancl [] split [||] in
  let new_edges = List.sum (node_map num_children r) in
  new_edges - old_edges 
  *)

exception ManyEdges

let delta_edges circ ancl max_edges split =

  clear_hashes () ;
  clear_sets () ;
  let dist = split.d and v = split.v in

  (* Find ancestors *)
  let d_anc = ancl dist.nodes in
  let s_anc = ancl [v] in
  let vnodes = split.vnodes in
  let v_anc = ancl vnodes in 

  (* Tricky hack to add edges every time the dist
     splitting code tries to create a ndoe. *)
  let edges = ref 0 in
  let rem_edges e = 
    edges := !edges - e in
  let add_edges e = 
    edges := !edges + e; 
    if !edges > max_edges then raise ManyEdges in 

  let marked  = get_set () in 
  let removed = get_set () in
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

  let ncreate details children = 
    add_edges (List.length children) ; null_node in

  (* Allocate hashes *)
  let make_vhl v = (v, ancl [v], get_hash ()) in
  let vhl = List.map make_vhl vnodes in
  let h_ma = get_hash () in
  let h_t = get_hash () in 
  let h_f = get_hash () in
  let changed n n' = () in
  try 
    ignore(copy_ma ncreate mark_tree visit changed 
          (d_anc, s_anc, h_t, h_f, vhl, h_ma) circ.root);
    !edges 
  with ManyEdges -> max_edges + 1
