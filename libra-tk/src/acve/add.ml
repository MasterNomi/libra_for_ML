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

(*
 * Type Declarations
 *)
type sym_add_node = Leaf of Circuit.node
                  | Split of int * sym_add_id array 
                and
     sym_add_id = int * sym_add_node

type sym_add = {root: sym_add_id;
                vars: bool array; (* true if ith var is present in add *)
                avo: int array;   (* map from var indices to their order *)
                lvo: int list;    (* var indices in elimination order *)
                  (* true if ADD affects the size of the multiplied ADD
                   * in the ith step of variable elimination *)
                in_table: bool array;
                  (* size of ADD after removing variable i 
                   * (and previous vars according to elim order) *)
                pruned_sizes: int array} 

(* type sym_add_root = (int, unit) Hashtbl.t  * sym_add *)

exception NonLeafException
exception LeafException

(* Global numbering for nodes, so that they can be hashed *)
let zero_hashid = ref (-1)
let one_hashid = ref (-1)
let global_hashid = ref 0

(* Variable ordering: Map ADD nodes to their rank in the list *)
(* TODO -- is this backwards?  If not, why not? *)
let pos avo = function
    Leaf _ -> 100
  | Split (var, _) -> -avo.(var)

let is_zero (id, x) = id = !zero_hashid 
let is_one (id, x) = id = !one_hashid
let is_nonzero (id, x) = id <> !zero_hashid 
let is_nonone (id, x) = id <> !one_hashid

let gen_hashid () =
  let id = !global_hashid in
  incr global_hashid ;
  assert (!global_hashid > 0) ;
  id

let make_split var children =
  let id = gen_hashid () in
  (id, Split (var, children)) 


let create_split_node splithash var children = 
  let cid = fst children.(0) in
  if Array.for_all (fun c -> cid = fst c) children then
    children.(0)
  else 
    let lchildren = List.map fst (Array.to_list children) in
    if Hashtbl.mem splithash (var :: lchildren) then
      Hashtbl.find splithash (var :: lchildren)
    else
      let split = make_split var children in
      (Hashtbl.add splithash (var :: lchildren) split ; split)


let create_constleaf_node wt =
  let id = gen_hashid () in
  let l = Leaf (Circuit.create_const wt) in
  if wt = neg_infinity then 
   (assert (!zero_hashid < 0); zero_hashid := id)
  else if wt = 0.0 then 
   (assert (!one_hashid < 0); one_hashid := id);
  (id, l) 

let create_constleaf_node_hashed leafhash wt =
  if Hashtbl.mem leafhash wt then
    Hashtbl.find leafhash wt
  else
    let leaf = create_constleaf_node wt in
    (Hashtbl.add leafhash wt leaf ; leaf)

let node_size root =
  let h = Hashtbl.create 100 in
  let rec s (id_x, x) = 
    if (Hashtbl.mem h id_x) then 0
    else begin
      Hashtbl.add h id_x () ;
      match x with 
          Leaf n -> 1
        | Split (var, children) -> 1 + Array.sum_map s children
    end in
  s root

let size add = node_size add.root

let get_leaf_value = function
  (id, Leaf n) -> Circuit.const_value n
| (id, Split(v, children)) -> raise NonLeafException

let get_split_var = function
  (id, Leaf n) -> raise LeafException
| (id, Split(v, children)) -> v

let get_split_children = function
  (id, Leaf n) -> raise LeafException
| (id, Split(v, children)) -> children


(*
 * Print out a SymADD
 *)

let rec output_offset out offset (id, x) =
  for i = 1 to offset do
    output_string out "  "
  done ;
  fprintf out "%d: " id;
  let _ = match x with 
    Leaf n -> 
      Circuit.output_node out n ;
      output_string out "\n"
  | Split (v, children) -> 
      fprintf out "split on %d\n" v;
      Array.iter (output_offset out (offset+1)) children in ()

let output out add = output_offset out 0 add.root
let print add = output stdout add

let create_add avo lvo vars root =
  let numvars = Array.length avo in
  {root=root; 
   vars=vars; 
   avo=avo; 
   lvo=lvo; 
   in_table=Array.make numvars false; 
   pruned_sizes=Array.make numvars 0}

(*
 * Generate an ADD for the indicator variables
 *)
let indicators_to_add avo lvo vnodes v = 
  let create_leaf_node n = (gen_hashid (), Leaf n) in
  let children = 
    Array.map create_leaf_node vnodes.(v) in
  (* Array.iter (Node.mini_output stdout) vnodes.(v) ; *)
  let root = make_split v children in 
  let cptvars = Array.make (Array.length vnodes) false in
  cptvars.(v) <- true;
  create_add avo lvo cptvars root

module F = Mn.Factor

let fvars numvars f =
  let fvars = Array.make numvars false in
  List.iter (fun p -> fvars.(p) <- true) (F.vars f);
  fvars

(*
 * Generate an ADD for a CPT
 *)
let table_to_add s_hash l_hash avo lvo schema f =
  (* vars.(i) is true if i'th var is part of this cpt *)
  let numvars = Array.length schema in 
  let vars = fvars numvars f in
  let state = Array.make numvars 0 in

  (* Recursively obtain all configurations of all variables *)
  let rec c2a = function 
    | x :: l when not vars.(x) -> 
        (* If this var is irrelevant, recurse. *)
        c2a l 
    | x :: l ->
        (* Recurse on each value of a relevant variable. *)
        let get_child i =
          state.(x) <- i;
          c2a l in
        let children = Array.init schema.(x) get_child in
        create_split_node s_hash x children
    | [] ->
        (* Base case: return leaf (AC node) for this configuration. *)
        let wt = F.log_value state f in
        (* HACK TODO -- is this safe to delete??? *)
        (* if wt = neg_infinity then
          vars.(Array.length vars - 1) <- true; *)
        create_constleaf_node_hashed l_hash wt in

  let root = c2a (List.rev lvo) in
  let add = create_add avo lvo vars root in
  if log_exists log_debug then
    output (log_stream log_debug) add;
  add


(* Take an out-of-order ADD and convert it into an ordered one.
 * This could incur a blow-up of up to O(n^2) 
 * (or more? I haven't proven this.)
 *)
let rec shallow_reorder cache s_hash avo (id, n) =
  if Hashtbl.mem cache id then Hashtbl.find cache id
  else let result =

  match n with Leaf _ -> (id, n) | Split(nv, children) ->

  (* Check for an ordering violation *) 
  let c_vorder = Array.map (fun (id_c, c) -> pos avo c) children in
  if Array.min c_vorder > pos avo n then
    (id, n)
  else begin
    (* Select first child to appear *)
    let c = children.(Array.argmin c_vorder) in
    let cv = get_split_var c in
    let cchildren = get_split_children c in

    (* Transpose grandchildren to create new children *)
    let create_ith_child i =
      let matching_gchild c' = match c' with 
          (id, Leaf _) -> c' 
        | (id, Split(v', gchildren)) -> 
            if v' = cv then gchildren.(i) else c' in
      let new_grandchildren = Array.map matching_gchild children in
      (* Children split on n's variable, nv *)
      let new_child = create_split_node s_hash nv new_grandchildren in
      shallow_reorder cache s_hash avo new_child in 
    let new_children = Array.init (Array.length cchildren) create_ith_child in

    (* Replacement node splits on child's variable, cv *)
    create_split_node s_hash cv new_children 
  end

  in Hashtbl.add cache id result ; result


let rec deep_reorder cache s_hash order (id, n) =
  match n with Leaf _ -> (id, n) | Split(v, children) ->
  (* Reorder descendants, then ourselves *)
  (* dlogf "Reordering split node (%d)\n" id; *)
  let children' = Array.map (deep_reorder cache s_hash order) children in
  let (id', n') = create_split_node s_hash v children' in
  shallow_reorder cache s_hash order (id', n')

(* Fix cases where two we have two splits on the same variable. 
   In that case, we can collapse the two splits into one.
   Perform this on the entire ADD. *)
let rec remove_redundant cache s_hash (id, n) =
  if Hashtbl.mem cache id then Hashtbl.find cache id
  else let result = 

  match n with Leaf _ -> (id, n) | Split(v, children) ->

  let fix_child i c =
    (* Remove redundancies recursively *)
    let c' = remove_redundant cache s_hash c in
    match c' with
      (id, Leaf _) -> c'
    | (id, Split(v', gchildren)) ->
      (* When we have a match, jump directly to the appropriate child
         of the split; no need to test the variable twice. *)
      if v' = v then gchildren.(i) else c' in
  let new_children = Array.mapi fix_child children in
  create_split_node s_hash v new_children 

  in Hashtbl.add cache id result ; result
  
let reorder_add s_hash avo lvo vars root =
  let cache = Hashtbl.create 100 in
  let root = deep_reorder cache s_hash avo root in
  Hashtbl.clear cache;
  let root = remove_redundant cache s_hash root in
  create_add avo lvo vars root 


(*
 * Generate an unordered ADD for a tree-structured factor
 *)
let rec ftree_to_add s_hash l_hash schema = function
  | F.Leaf w -> 
      create_constleaf_node_hashed l_hash w
  | F.Vertex (svar, value, tchild, fchild) ->
      let tadd = ftree_to_add s_hash l_hash schema tchild in 
      let fadd = ftree_to_add s_hash l_hash schema fchild in 
      let children = Array.make (schema.(svar)) fadd in
      children.(value) <- tadd ;
      create_split_node s_hash svar children 

(*
 * Generate a factor for a single feature.
 *)
let sort_conditions avo condl = 
  let cmp_cond (sense1, var1, val1) (sense2, var2, val2) =
    avo.(var2) - avo.(var1) in
  List.sort cmp_cond condl

let feat_to_add_split s_hash l_hash var okvalues child =
  if var >= 0 then 
    let zero = create_constleaf_node_hashed l_hash 0. in
    let children = 
      Array.map (fun b -> if b then child else zero) okvalues in
    create_split_node s_hash var children
  else
    child

let rec rec_feat_to_add s_hash l_hash schema wt currvar currvals condl =
  match condl with
  | [] -> 
    let child = create_constleaf_node_hashed l_hash wt in
    feat_to_add_split s_hash l_hash currvar currvals child
  | (sense,var,value) :: l when var <> currvar ->
    let newvals = Array.make schema.(var) true in
    let child = rec_feat_to_add s_hash l_hash schema wt var newvals condl in
    feat_to_add_split s_hash l_hash currvar currvals child
  | (sense,var,value) :: l ->
    for i = 0 to Array.length currvals - 1 do
      if (i=value) <> sense then
        currvals.(i) <- false
    done;
    rec_feat_to_add s_hash l_hash schema wt var currvals l

let feat_to_add s_hash l_hash avo lvo schema f = 
  let root = rec_feat_to_add s_hash l_hash 
    schema f.F.weight (-1) [||] (sort_conditions avo (Array.to_list f.F.cond)) in
  create_add avo lvo (fvars (Array.length schema) (F.Feature f)) root


(*
 * Convert an MN factor into an ADD
 *)
let factor_to_adds s_hash l_hash avo lvo schema = function
| F.Table (vars, ranges, t) -> 
  [table_to_add s_hash l_hash avo lvo schema (F.Table (vars, ranges, t))]
| F.Tree root -> 
  let vars = fvars (Array.length schema) (F.Tree root) in
  let addroot = ftree_to_add s_hash l_hash schema root in
  [reorder_add s_hash avo lvo vars addroot]
| F.Feature feat ->
  [feat_to_add s_hash l_hash avo lvo schema feat]
| F.FeatureSet fl ->
  List.map (feat_to_add s_hash l_hash avo lvo schema) fl
| F.Const wt -> 
  let vars = Array.make (Array.length schema) false in
  [create_add avo lvo vars (create_constleaf_node_hashed l_hash wt)]


let is_zero_leaf = function
    Split _ -> false
  | Leaf n -> Circuit.is_const n && (Circuit.const_value n = neg_infinity)

let is_one_leaf = function
    Split _ -> false
  | Leaf n -> Circuit.is_const n && (Circuit.const_value n = 0.0)


(* Create circuit nodes using a unique list.  (DISABLED)

(* HACK: These are globals, for now... *)
let times_hash = Hashtbl.create 1000
let plus_hash = Hashtbl.create 1000

let create_times n_x n_y =
  let ix = Circuit.hashid n_x in
  let iy = Circuit.hashid n_y in
  let key = if ix < iy then (ix, iy) else (iy, ix) in
  if Hashtbl.mem times_hash key then
    Hashtbl.find times_hash key
  else 
    let n = Circuit.create_times [n_x; n_y] in
    (Hashtbl.add times_hash key n; n)

let create_plus children =
  let key = List.sort compare (List.map Circuit.hashid children) in
  if Hashtbl.mem plus_hash key then
    Hashtbl.find plus_hash key
  else
    let n = Circuit.create_plus children in
    (Hashtbl.add plus_hash key n; n)
 *)

(* Create circuit nodes without the unique list:
   Much faster and almost as good! *)
let create_times n_x n_y = Circuit.create_times [n_x; n_y]
let create_plus nl = Circuit.create_plus nl

(* Optimized multiplication *)
let rec rec_mult leaf_product avo cache (id_x, x) (id_y, y) = 

  (* Enforce ordering *)
  let (id_x, x, id_y, y) = 
    if pos avo x > pos avo y then 
      (id_y, y, id_x, x)
    else
      (id_x, x, id_y, y) in

  (* Check cache first *)
  if Hashtbl.mem cache (id_x, id_y) then 
    Hashtbl.find cache (id_x, id_y)
  else if Hashtbl.mem cache (id_y, id_x) then
    Hashtbl.find cache (id_y, id_x)

  else if id_x = !one_hashid || id_y = !zero_hashid then (id_y, y)
  else if id_y = !one_hashid || id_x = !zero_hashid then (id_x, x)

  (* Actual application... *)
  else 
    let id = gen_hashid() in 
    let result = 
      match x, y with
      (* Apply to two leaves (easy) *)
        Leaf n_x, Leaf n_y -> 
          leaf_product id n_x n_y

      (* When split variable is the same, recurse on the children *)
      | Split (x_var, x_children), Split (y_var, y_children) 
               when x_var == y_var -> 
          let var = x_var in
          let children = 
            Array.map2 (rec_mult leaf_product avo cache) x_children y_children in
          (id, Split (var, children))

      (* General case: recurse with each combo *)
      | Split (x_var, x_children), _ -> 
          let var = x_var in
          let children = 
            Array.map ((rec_mult leaf_product avo cache) (id_y, y)) x_children in
          (id, Split (var, children))

      (* The ordering should prevent the Leaf+Split case, always. *)
      | Leaf _, _ -> assert false in 
    (Hashtbl.add cache (id_x, id_y) result ; result) 

let multiplyc leaf_product cache x y =
  let product = rec_mult leaf_product x.avo cache x.root y.root in
  let product_vars = Array.map2 ( || ) x.vars y.vars in
  create_add x.avo x.lvo product_vars product 

let multiply leaf_product x y = 
  let cache = Hashtbl.create 100 in
  multiplyc leaf_product cache x y


(*
 * Sum out a variable at the bottom of a SymADD
 *)

let ac (id_x, x) = 
  match x with Leaf n -> n 
             | _ -> raise NonLeafException

let sumcache = Hashtbl.create 100

let sum_out leaf_sum var x = 
  let vars = Array.copy x.vars in
  vars.(var) <- false;
  let rec so (id_y, y) =
    if Hashtbl.mem sumcache id_y then
      Hashtbl.find sumcache id_y
    else begin
      let ret = 
        match y with 
          Split (y_var, y_children) when y_var == var ->
            (* Remove children that have the constant value zero *)
            if Array.exists is_zero y_children then 
              let cl' = List.filter is_nonzero (Array.to_list y_children) in 
              match cl' with
                [] -> y_children.(0)
              | [x] -> x
              | l -> 
                  let children = List.map ac cl' in
                  leaf_sum id_y children
            else
              let children = Array.to_list (Array.map ac y_children) in
              leaf_sum id_y children
        | Split (y_var, y_children) ->
            (id_y, Split (y_var, Array.map so y_children))
        | Leaf n -> 
            (id_y, Leaf n) in
      Hashtbl.add sumcache id_y ret ; ret
    end in
  Hashtbl.clear sumcache;
  create_add x.avo x.lvo vars (so x.root)


(*
 * Generate features from the ADDs, in the format used by
 * the Circuit module.  Assumes the circuit has already been
 * output, so the Circuit node ids are set.
 *)

let rec feature_conds_rec (id, n) accu =
  if is_one (id, n) then [] else 
  match n with
    Leaf l -> 
      if Circuit.id l >= 0 then
        [{Circuit.acnode=l; 
          Circuit.weight=Circuit.const_value l;
          Circuit.cond=accu;
          Circuit.ev=[||]}]
      else []
  | Split (v,children) ->
      let cfeatures = Array.mapi (fun i c -> 
        feature_conds_rec c ((true,v,i) :: accu)) children in
      List.flatten (Array.to_list cfeatures)

let circuit_features addl = 
  List.flatten (List.map (fun add -> feature_conds_rec add.root []) addl) 
