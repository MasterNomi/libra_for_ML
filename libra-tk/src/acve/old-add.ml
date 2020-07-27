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
    zero_hashid := id
  else if wt = 0.0 then 
    one_hashid := id;
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


(*
 * Generate an ADD for a CPT
 *)
let cpt_to_add s_hash l_hash avo lvo bn v =
  (* cptvars.(i) is true if i'th var is part of this cpt *)
  let cptvars = Array.make (Bn.numvars bn) false in
  List.iter (fun p -> cptvars.(p) <- true) (Bn.parents bn v) ;
  cptvars.(v) <- true ;
  let state = Array.make (Bn.numvars bn) 0 in

  (* Recursively obtain all configurations of all variables *)
  let rec c2a = function 
    | x :: l when not cptvars.(x) -> 
        (* If this var is irrelevant, recurse. *)
        c2a l 
    | x :: l ->
        (* Recurse on each value of a relevant variable. *)
        let get_child i =
          state.(x) <- i;
          c2a l in
        let children = Array.init (Bn.get_range bn x) get_child in
        create_split_node s_hash x children
    | [] ->
        (* Base case: return leaf (AC node) for this configuration. *)
        let wt = Bn.node_logscore bn state v in
        (* HACK TODO -- is this safe to delete??? *)
        (* if wt = neg_infinity then
          cptvars.(Array.length cptvars - 1) <- true; *)
        create_constleaf_node_hashed l_hash wt in

  let root = c2a (List.rev lvo) in
  let add = create_add avo lvo cptvars root in
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
    let new_children = Array.init (Array.length children) create_ith_child in

    (* Replacement node splits on child's variable, cv *)
    create_split_node s_hash cv new_children 
  end

  in Hashtbl.add cache id result ; result


let rec deep_reorder cache s_hash order (id, n) =
  match n with Leaf _ -> (id, n) | Split(v, children) ->
  (* Reorder descendants, then ourselves *)
  let children' = Array.map (deep_reorder cache s_hash order) children in
  let (id', n') = create_split_node s_hash v children' in
  shallow_reorder cache s_hash order (id', n')
  
let reorder_add s_hash avo lvo vars root =
  let cache = Hashtbl.create 100 in
  let root = deep_reorder cache s_hash avo root in
  create_add avo lvo vars root 


(*
 * Generate an unordered ADD for a tree-structured CPD
 *)
let rec rec_tree_to_add schema s_hash l_hash v = function
  | Bn.Leaf probs -> 
      let lprobs = Array.map log probs in 
      let children = Array.map (create_constleaf_node_hashed l_hash) lprobs in
      create_split_node s_hash v children
  | Bn.Vertex (svar, value, tchild, fchild) ->
      let tadd = rec_tree_to_add schema s_hash l_hash v tchild in 
      let fadd = rec_tree_to_add schema s_hash l_hash v fchild in 
      let children = Array.make (schema.(svar)) fadd in
      children.(value) <- tadd ;
      create_split_node s_hash svar children 

exception NonTreeDist
  
let tree_to_add s_hash l_hash bn v root =
  (* cptvars.(i) is true if i'th var is part of this cpt *)
  let cptvars = Array.make (Bn.numvars bn) false in
  List.iter (fun p -> cptvars.(p) <- true) (Bn.parents bn v);
  cptvars.(v) <- true ;
  let schema = Bn.schema bn in
  let add = rec_tree_to_add schema s_hash l_hash v root in
  dlogf "Converted tree for var %d.\n" v;
  (cptvars, add)


let cpd_to_add s_hash l_hash avo lvo bn v =
  match bn.Bn.dists.(v) with
  | Bn.Table t -> 
    cpt_to_add s_hash l_hash avo lvo bn v 
  | Bn.Tree root -> 
    let (vars, root) = tree_to_add s_hash l_hash bn v root in
    reorder_add s_hash avo lvo vars root 


let is_zero_leaf = function
    Split _ -> false
  | Leaf n -> Circuit.is_const n && (Circuit.const_value n = neg_infinity)

let is_one_leaf = function
    Split _ -> false
  | Leaf n -> Circuit.is_const n && (Circuit.const_value n = 0.0)


(* TODO -- circuit cache! *)

(* Optimized multiplication *)
let rec rec_mult avo cache (id_x, x) (id_y, y) = 

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
    let result = 
      match x, y with
      (* Apply to two leaves (easy) *)
        Leaf n_x, Leaf n_y -> 
          Leaf (Circuit.create_times [n_x; n_y]) 

      (* When split variable is the same, recurse on the children *)
      | Split (x_var, x_children), Split (y_var, y_children) 
               when x_var == y_var -> 
          let var = x_var in
          let children = 
            Array.map2 (rec_mult avo cache) x_children y_children in
          Split (var, children) 

      (* General case: recurse with each combo *)
      | Split (x_var, x_children), _ -> 
          let var = x_var in
          let children = 
            Array.map ((rec_mult avo cache) (id_y, y)) x_children in
          Split (var, children) 

      (* The ordering should prevent the Leaf+Split case, always. *)
      | Leaf _, _ -> assert false in 
    let id = gen_hashid () in
    (Hashtbl.add cache (id_x, id_y) (id, result) ; (id, result)) 


exception OutOfMemory of int

let max_node_size = 10000000

let multiply x y = 
  (* (x_vars, x) (y_vars, y) = *)
  let cache = Hashtbl.create 100 in
  let product = rec_mult x.avo cache x.root y.root in
  let product_vars = Array.map2 ( || ) x.vars y.vars in
  create_add x.avo x.lvo product_vars product 


(*
 * Sum out a variable at the bottom of a SymADD
 *)

let ac (id_x, x) = 
  match x with Leaf n -> n 
             | _ -> raise NonLeafException

let sum_out var x = 
  let vars = Array.copy x.vars in
  vars.(var) <- false;
  let hash = Hashtbl.create 100 in
  let rec so (id_y, y) =
    if Hashtbl.mem hash id_y then
      Hashtbl.find hash id_y
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
                  (id_y, Leaf (Circuit.create_plus children))
            else
              let children = Array.to_list (Array.map ac y_children) in
              (id_y, Leaf (Circuit.create_plus children))
        | Split (y_var, y_children) ->
            (id_y, Split (y_var, Array.map so y_children))
        | Leaf n -> 
            (id_y, Leaf n) in
      Hashtbl.add hash id_y ret ; ret
    end in
  create_add x.avo x.lvo vars (so x.root)


exception EmptyList


(****************************
 * Minfill variable ordering
 ****************************)

let fill_edges adj elim v =
  let edge_list = ref [] in
  for i = 0 to Array.length elim - 1 do
    if not elim.(i) && adj.(v).(i) then
      for j = 0 to i - 1 do
        if not elim.(j) && adj.(v).(j) && not adj.(i).(j) then
          edge_list := (i,j) :: !edge_list 
      done
  done ;
  !edge_list

let fill_in adj elim v =
  let edges = fill_edges adj elim v in
  List.iter (fun (i,j) -> adj.(i).(j) <- true; adj.(j).(i) <- true) edges 

let minfill_var adj elim =
  let best_num = ref (max_int) in
  let best = ref (-1) in
  for i = 0 to Array.length elim - 1 do
    if not elim.(i) then
      let num_edges = List.length (fill_edges adj elim i) in
      if num_edges < !best_num then 
        (best := i ; best_num := num_edges)
  done ;
  !best


let minfill_next_var adj elim =
  let v = minfill_var adj elim in
  elim.(v) <- true ;
  fill_in adj elim v ;
  v


let minfill_order (numvars, clusters) =

  (* Construct adjacency table *)
  let adj = Array.make numvars [||] in
  for i = 0 to numvars - 1 do
    adj.(i) <- Array.make numvars false
  done ;

  (* Fill adjacency table *)
  let rec add_pairs = function
    | x::yl -> let add_y y = 
                 adj.(x).(y) <- true ;
                 adj.(y).(x) <- true in
               List.iter add_y yl ;
               add_pairs yl
    | [] -> () in
  List.iter add_pairs clusters ;

  let elim = Array.make numvars false in
  Array.to_list (Array.init numvars (fun _ -> minfill_next_var adj elim))

(* Get clusters used to compute the minfill ordering *)
let bn_to_clusters bn =
  let numvars = Bn.numvars bn in 
  let family i = i :: (Bn.parents bn i) in
  (numvars, Array.to_list (Array.init numvars family))

;;


(****************************
 * ADD Variable Elimination
 ****************************)


(* Efficient caching for ADD sizes 
 * Used for multiplying smallest ADDs first in varelim *)
let size_hash = Hashtbl.create 100 
let hsize add = 
  if Hashtbl.mem size_hash add.root then
    Hashtbl.find size_hash add.root
  else begin
    let s = size add in
    Hashtbl.add size_hash add.root s ;
    s
  end 
let cmpsize a b = hsize a - hsize b 


let rec rec_eliminate maxsize avo addset = function
  | [] -> 
      if log_exists log_debug then begin
        dlogf "Final add set:\n";
        List.iter (output (log_stream log_debug)) addset
      end;
      addset
  | v :: l ->
      (* For each variable in the elimination ordering, *)
      (* Select all ADDs with the variable *)
      start_time "elim";
      let has_v add = add.vars.(v) in
      let (relevant, irrelevant) = List.partition has_v addset in

      (* Multiply smallest ones first  -- WORKS!!! *)
      let multiply x y =
        let product = multiply x y in
        if maxsize > 0 && node_size product.root > maxsize then
          raise (printf "VARIABLE %d FAILED!\n\n" v; OutOfMemory v)
        else
          product in
        (* else (printf "size %d < maxsize %d\n" (node_size product.root) maxsize; product) *) 
      let orelevant = List.sort cmpsize relevant in
      let product = List.fold_left multiply  
        (List.hd orelevant) (List.tl orelevant) in

      (* Pick the factor with fewest vars that aren't in current factor *
      let rec multall rel =
        match (List.sort cmpsize rel) with
          a :: [] -> a
        | a :: b :: l -> multall ((multiply avo a b) :: l)
        | [] -> raise EmptyList in
      let product = multall (indicator_adds.(v) :: relevant) in *)

      (* Always multiply smallest pair of factors next -- Similar
       * to previous method.
      let rec multall rel =
        match (List.sort cmpsize rel) with
          a :: [] -> a
        | a :: b :: l -> multall ((multiply avo a b) :: l)
        | [] -> raise EmptyList in
      let product = multall (indicator_adds.(v) :: relevant) in
       *)
      vlogf "Eliminating var %d: %fs\n" v (elapsed "elim") ;

      (* ...and sum out the eliminated variable *)
      let summed_out = sum_out v product in 
      vlogf "Summing out: %fs\n" (elapsed "elim") ;
      vlogf "Final size: %d\n" (hsize summed_out); 
      let addset' = summed_out :: irrelevant in
      vlogf "%d factors remaining.\n" (List.length addset');
      if log_exists log_debug then
        List.iter (output (log_stream log_debug)) addset';
      rec_eliminate maxsize avo addset' l 

let eliminate maxsize avo addset lvo =
  rec_eliminate maxsize avo addset lvo

let bn_to_adds bn avo lvo = 
  let numvars = Bn.numvars bn in
  let schema = Bn.schema bn in
  let vn = Circuit.make_vnodes schema in
  (* let vnl = Array.map (Array.to_list) vn in *)

  (* Create ADDs for each variable *)
  let s_hash = Hashtbl.create 100 in
  let l_hash = Hashtbl.create 100 in
  let cpd_adds = Array.init numvars (cpd_to_add s_hash l_hash avo lvo bn) in
  let indicator_adds = Array.init numvars (indicators_to_add avo lvo vn) in
  (Array.to_list cpd_adds, Array.to_list indicator_adds, s_hash, l_hash)


(*************************
 * Dynamic simplification 
 ************************)

(* Recursively compute the maximum and minimum P(x | pa(x))
 * over the subgraph rooted at n.
 *)
let rec minmax_logprob mmh (id, n) = 
  if Hashtbl.mem mmh id then
    Hashtbl.find mmh id
  else begin
    let (minval, maxval) = 
      match n with
        Leaf l -> 
          let wt = Circuit.const_value l in 
          (wt, wt)
      | Split (v', children) -> 
          let aminmax = Array.map (minmax_logprob mmh) children in
          let minval = Array.min (Array.map fst aminmax) in
          let maxval = Array.max (Array.map snd aminmax) in
          (minval, maxval) in

    Hashtbl.add mmh id (minval, maxval) ;
    (minval, maxval)
  end

let compute_llratios_prune root = 
  let mmh = Hashtbl.create 100 in
  ignore (minmax_logprob mmh root) ;
  mmh


(* Prune ADD according to some threshold *)
let rec prune_add_rec l_hash s_hash mmh cache thresh avo maxv (id, n) = 
  if Hashtbl.mem cache id then
    Hashtbl.find cache id
  else begin
    let result = 
      if not (Hashtbl.mem mmh id) then (id, n)
      else 
        match n with
          Leaf l -> (id, n)
        | Split(v, children) ->
            if maxv >= 0 && avo.(v) < avo.(maxv) then (id, n)
            else 
              let (minval, maxval) = Hashtbl.find mmh id in
              if minval = maxval || 
                (minval > neg_infinity && minval /. maxval < thresh) then begin
                (* Close enough -- Approximate with geometric mean! *)
                vlogf "minval = %f; maxval = %f\n" minval maxval ;
                create_constleaf_node_hashed l_hash (-. sqrt (minval *. maxval))
              end
              else 
                (* Can't approximate yet; recurse instead *)
                let c' = Array.map 
                  (prune_add_rec l_hash s_hash mmh cache thresh avo maxv) children in
                create_split_node s_hash v c' in
    Hashtbl.add cache id result;
    result
  end

let prune_add l_hash s_hash mmh thresh add =
  let cache = Hashtbl.create 100 in
  prune_add_rec l_hash s_hash mmh cache thresh add.avo 101 add.root

let prune_add_v l_hash s_hash mmh thresh v add =
  let cache = Hashtbl.create 100 in
  prune_add_rec l_hash s_hash mmh cache thresh add.avo v add.root




(* NEW APPROACH (5/13/09)
 *
 * Pick one child to replace node with.  Do matching between all node
 * pairs and compute log likelihoods.  If we need the speed, we can
 * optionally do branch-and-bound types of things.
 *)

(* TODO -- branch and bound!  It'll be faster! *)

let rec max_llratio avo cache (id_x, x) (id_y, y) =

  (* Enforce ordering *)
  let (id_x, x, id_y, y) = 
    (* First sort by variable order *)
    if pos avo x > pos avo y then 
      (id_y, y, id_x, x)
    else 
      (id_x, x, id_y, y) in

  let key = (min id_x id_y, max id_x id_y) in
  if Hashtbl.mem cache key then
    Hashtbl.find cache key
  else
    let result =
      match x, y with 
      (* Apply to two leaves (easy) *)
        Leaf n_x, Leaf n_y -> 
          let valx = Circuit.const_value n_x in
          let valy = Circuit.const_value n_y in
          maxf (abs_float (valx /. valy)) (abs_float (valy /. valx))

      (* When split variable is the same, recurse on the children *)
      | Split (x_var, x_children), Split (y_var, y_children) 
               when x_var == y_var -> 
          let child_maxlls = 
            Array.map2 (max_llratio avo cache) x_children y_children in
          Array.max child_maxlls 

      (* General case: recurse with each combo *)
      | Split (x_var, x_children), _ -> 
          let child_maxlls = 
            Array.map ((max_llratio avo cache) (id_y, y)) x_children in
          Array.max child_maxlls 

      (* The ordering should prevent the Leaf+Split case, always. *)
      | Leaf _, _ -> assert false in 
    (Hashtbl.add cache key result ; result)


let rec compute_llratios_among_children avo visited cache (id, n) =
  if not (Hashtbl.mem visited id) then begin
    Hashtbl.add visited id () ;
    match n with
      Leaf _ -> ()
    | Split (var, children) ->
        (* Compute cost of each pair *)
        let numchildren = Array.length children in
        for i = 0 to numchildren - 1 do
          for j = i+1 to numchildren - 1 do
            ignore (max_llratio avo cache children.(i) children.(j)) ;
            Array.iter (compute_llratios_among_children avo visited cache) 
                children
          done
        done
  end

(* Compute all likelihood ratios between pairs of children *)
let compute_llratios_choose add =
  let visited = Hashtbl.create 100 in
  let cache = Hashtbl.create 100 in
  compute_llratios_among_children add.avo visited cache add.root ;
  cache


let rec simplify_add_rec avo l_hash s_hash mmh prh cache thresh maxv (id, n) = 
  if Hashtbl.mem cache id then
    Hashtbl.find cache id
  else begin
    let result = 
      match n with
        Leaf l -> (1.0, (id, n))
      | Split(v, children) ->
          dlogf "Split on %d...\n" v; 
          if maxv >= 0 && avo.(v) < avo.(maxv) then begin
            dlogf "Stopping: avo.(%d) = %d < %d = avo.(%d)\n"
              v avo.(v) avo.(maxv) maxv; 
            (1.0, (id, n))
          end
          else begin
          (* Simplify children, obtaining cost so far and updated children *)
          let recf = 
            simplify_add_rec avo l_hash s_hash mmh prh cache thresh maxv in
          let recresults = Array.map recf children in
          let base_ratio = Array.map fst recresults in
          let children'  = Array.map snd recresults in

          (* Figure out the best simplification locally. Do this based
           * on the original ADDs, not the simplified ones, so that
           * our approximation cost is accurate. *)
          let numchildren = Array.length children in
          let approx_ratio = Array.make numchildren 1.0 in
          for i = 0 to numchildren - 1 do
            (* let id_i  = fst children.(i) in *)
            for j = 0 to numchildren - 1 do
              (* let id_j = fst children.(j) in
                 let ratio = try Hashtbl.find mmh (id_i, id_j) 
                          with Not_found -> Hashtbl.find mmh (id_j, id_i) in
              approx_ratio.(i) <- maxf approx_ratio.(i) ratio ; *)
              let ratio = max_llratio avo mmh children'.(i) children.(j) in
              approx_ratio.(i) <- maxf approx_ratio.(i) ratio
            done 
          done ;
          let total_ratio = Array.map2 ( maxf ) base_ratio approx_ratio in
          (* let total_ratio = Array.map2 ( *. ) base_ratio approx_ratio in *)
          let bestchild = Array.argmin total_ratio in
          (*
          let (minval, maxval) = Hashtbl.find prh id in 
          dlogf "prune ratio =     %f\n" (minval /. maxval);
          dlogf "max base_ratio =  %f\n" (Array.max base_ratio);
          dlogf "min total_ratio = %f\n" (Array.min total_ratio);
          *)
          if Array.min total_ratio < thresh then
            (Array.min total_ratio, children'.(bestchild))
          else 
            let newsplit = create_split_node s_hash v children' in
            (Array.max base_ratio, newsplit) 
          end
      in
    Hashtbl.add cache id result;
    result
  end

let simplify_add l_hash s_hash mmh prh thresh add =
  let cache = Hashtbl.create 100 in
  if log_exists log_debug then begin
    dlogf "BEFORE SIMPLIFY:\n";
    output_offset (log_stream log_debug) 0 add.root;
  end;
  let result = 
    simplify_add_rec add.avo l_hash s_hash mmh prh cache thresh (-1) add.root in
  if log_exists log_debug then begin
    dlogf "AFTER SIMPLIFY:\n";
    output_offset (log_stream log_debug) 0 (snd result);
  end;
  result

let simplify_add_v l_hash s_hash mmh prh thresh v add =
  let original_size = size add in
  let cache = Hashtbl.create 100 in
  let result = snd (simplify_add_rec add.avo 
                    l_hash s_hash mmh prh cache thresh v add.root) in
  vlogf "SIMPLIFY: %d -> %d (t:%f)\n" original_size (node_size result) thresh;
  result


(* Determine which steps of variable elimination include which ADDs
 * (or pieces of ADDs, as variables get summed out).
 *)
let rec compute_membership avo clusters = function
  [] -> ()
| x :: l ->
    (* Select clusters with x *)
    let has_x (vars, _) = vars.(x) in
    let (with_x, without_x) = List.partition has_x clusters in
    (* Merge them *)
    let multiply (v1, l1) (v2, l2) = 
      let v = Array.map2 ( || ) v1 v2 in
      let l = l1 @ l2 in
      (v, l) in
    let new_clust = List.fold_left multiply (List.hd with_x) (List.tl with_x) in
    (* Mark all factors used in this cluster as members *)
    List.iter (fun add -> add.in_table.(avo.(x)) <- true) (snd new_clust) ;
    (* Remove factors with no remaining variables *)
    let sum_out (v, l) =
      let final_order a = 
        Array.max (Array.map2 (fun x o -> if x then o else (-1)) a avo) in
      let l' = List.filter (fun add -> final_order add.vars > avo.(x)) l in
      (v, l') in
    (* Recurse to the next variable in the ordering *)
    compute_membership avo ((sum_out new_clust) :: without_x) l


let rec compute_pruned_sizes_r visited add vl (id, n) =
  (* Count this as a leaf for the parent and all in-between vars *)
  let rec inc_count = function
    v :: l ->
      if -add.avo.(v) <= pos add.avo n then begin
        add.pruned_sizes.(v) <- add.pruned_sizes.(v) + 1;
        inc_count l
      end else (v :: l)
  | [] -> [] in
  let l = inc_count vl in

  (* Recurse on children if we haven't already *)
  if not (Hashtbl.mem visited id) then begin
    Hashtbl.add visited id () ;
    match n with 
      Split(v, children) -> 
        Array.iter (compute_pruned_sizes_r visited add l) children
    | Leaf _ -> ()
  end
    

(* Compute the number of leaves the ADD will have after summing out
 * each variable.
 *)
let compute_pruned_sizes add =
  for i = 0 to Array.length add.pruned_sizes - 1 do
    add.pruned_sizes.(i) <- 0
  done ;
  let visited = Hashtbl.create 100 in
  compute_pruned_sizes_r visited add (List.rev add.lvo) add.root;
  let sizestr = String.concat " " 
    (Array.to_list (Array.map string_of_int add.pruned_sizes)) in
  dlogf "PRUNED SIZES: %s\n" sizestr;

;;


let adds_in_ve_steps adds avo lvo =
  (* Determine which ADD will participate in which products *)
  let make_cluster add =
    for i = 0 to Array.length add.in_table - 1 do
      add.in_table.(i) <- false
    done ;
    (add.vars, [add]) in
  let clusters = List.map make_cluster adds in
  compute_membership avo clusters lvo 

;;

let max_table_sizes avo lvo adds =
  adds_in_ve_steps adds avo lvo;
  let numvars = Array.length avo in
  let sizes = Array.make numvars 1. in
  let mult_sizes add =
    for i = 0 to numvars - 1 do
      if add.in_table.(i) then 
        sizes.(i) <- sizes.(i) *. (float_of_int add.pruned_sizes.(i))
    done in
  List.iter mult_sizes adds ;
  let sizestr = 
    String.concat " " (Array.to_list (Array.map string_of_float sizes)) in
  vlogf "Sizes: %s\n" sizestr;
  sizes

let prune_v s_hash l_hash v thresh add =
  if thresh = 1.0 then
    add
  else
    let llratios = compute_llratios_prune add.root in
    let root = prune_add_v l_hash s_hash llratios thresh v add in
    create_add add.avo add.lvo add.vars root 

let simplify_v s_hash l_hash v thresh add =
  let mmh = compute_llratios_choose add in 
  let prh = compute_llratios_prune add.root in
  let root = simplify_add_v l_hash s_hash mmh prh thresh v add in
  create_add add.avo add.lvo add.vars root 


let prune_table_to_size prune_v adds (maxsize:float) v =
  let (vadds, others) = List.partition (fun add -> add.in_table.(v)) adds in
  let rec prune_thresh thresh =
    let vadds' = List.map (prune_v v thresh) vadds in
    List.iter compute_pruned_sizes vadds';
    let s = List.fold_left 
      (fun s add -> s *. (float_of_int add.pruned_sizes.(v))) 1. vadds' in
    vlogf "At threshold %f, final size s = %f\n" thresh s;
    if s <= maxsize then 
      vadds' @ adds
    else
      prune_thresh (thresh *. 2.) in
  prune_thresh 2.0


let rec prune_to_size (maxsize:float) prune_v adds =
  (* Recompute sizes/ordering/etc. *)
  let a = List.hd adds in
  let avo = a.avo and lvo = a.lvo in
  let sizes = max_table_sizes avo lvo adds in
  (* Prune until the largest factor is smaller than the required size *)
  if Array.max sizes > maxsize then begin
    let v = Array.argmax sizes in
    vlogf "Size of %d is %f\n" v sizes.(v);
    let adds' = prune_table_to_size prune_v adds maxsize v in
    (* Move on to check other factors... *)
    prune_to_size maxsize prune_v adds'
  end else
    adds


let rec prune_one_to_size (maxsize:float) prune_v thresh add =
  let add' = prune_v (-1) thresh add in
  let s = size add' in
  if (float_of_int s) > maxsize then
    (vlogf "Size %d is too big!\n" s; 
    prune_one_to_size maxsize prune_v (thresh *. 2.0) add)
  else
    (vlogf "Size %d is ok!\n" s; add')

    (*
let rec compile_until_small avo lvo prune_v (maxsize:float) adds ind_adds =
  vlogf "cus maxsize = %f\n" maxsize;
  List.iter compute_pruned_sizes adds;
  let adds' = prune_to_size maxsize prune_v adds in
  try
    eliminate 1000000 avo (adds' @ ind_adds) lvo 
  with OutOfMemory -> 
    compile_until_small avo lvo prune_v (maxsize /. 2.) adds ind_adds
    *)

let rec compile_until_small avo lvo prune_v (maxsize:float) adds ind_adds =
  vlogf "cus maxsize = %f\n" maxsize;
  try
    (eliminate 1000000 avo (adds @ ind_adds) lvo, adds)
  with OutOfMemory v -> 
    (* Recompute sizes/ordering/etc. *)
    List.iter compute_pruned_sizes adds;
    let a = List.hd adds in
    let avo = a.avo and lvo = a.lvo in
    (* Half the worst-case size of the table with v. *)
    let sizes = max_table_sizes avo lvo adds in
    let adds' = prune_table_to_size prune_v adds (sizes.(v) /. 2.) v in
    compile_until_small avo lvo prune_v maxsize adds' ind_adds


let finish_ac add_roots =
  (* Extract AC roots and multiply them together to get a single root *)
  let extract_ac add =
    let (id, x) = add.root in 
    match x with
      Leaf n -> n
    | _ -> 
      if log_exists log_debug then begin
        output (log_stream log_debug) add
      end;
      raise NonLeafException  in
  let ac_roots = List.map extract_ac add_roots in
  match ac_roots with
     [r] -> r
   | _   -> Circuit.create_times ac_roots 


let build_avo lvo =
  let numvars = List.length lvo in
  let avo = Array.make numvars 0 in
  let rec build_order i = function
    | [] -> ()
    | x :: l -> avo.(x) <- i ; build_order (i+1) l in
  build_order 0 lvo ;
  avo


let varelim prune_one thresh bn = 
  dlogf "Entering varelim...\n";
  start_time "ve";
  let lvo = minfill_order (bn_to_clusters bn) in
  let avo = build_avo lvo in
  nlogf "Minfill ordering: %fs\n" (elapsed "ve") ;

  let (cpd_adds, ind_adds, s_hash, l_hash) = bn_to_adds bn avo lvo in
  nlogf "ADD creation: %fs\n" (elapsed "ve") ;

  let cpd_adds = List.map (prune_one s_hash l_hash) cpd_adds in
  nlogf "Pruning: %fs\n" (elapsed "ve") ;
        
  let add_roots = eliminate 0 avo (cpd_adds @ ind_adds) lvo in
  nlogf "Variable elimination: %fs\n" (elapsed "ve");
  (finish_ac add_roots, cpd_adds)


let varelim_mem prune_v maxsize bn = 
  dlogf "Entering varelim_mem...\n";
  start_time "ve";
  let lvo = minfill_order (bn_to_clusters bn) in
  let avo = build_avo lvo in
  nlogf "Minfill ordering: %fs\n" (elapsed "ve") ;

  let (cpd_adds, ind_adds, s_hash, l_hash) = bn_to_adds bn avo lvo in
  nlogf "ADD creation: %fs\n" (elapsed "ve") ;

  let p = prune_v s_hash l_hash in
  let cpd_adds = List.map (prune_one_to_size maxsize p 1.0) cpd_adds in
  let (add_roots, cpd_adds) = 
    compile_until_small avo lvo p maxsize cpd_adds ind_adds in
  nlogf "Variable elimination: %fs\n" (elapsed "ve");
  (finish_ac add_roots, cpd_adds)

;;


let varelim_choose thresh bn = 
  let prune_one s_hash l_hash (add: sym_add) = 
    let llratios_choose = compute_llratios_choose add in 
    let prh = compute_llratios_prune add.root in
    let root = 
      snd (simplify_add l_hash s_hash llratios_choose prh thresh add) in
    create_add add.avo add.lvo add.vars root in
  varelim prune_one thresh bn

;;

let varelim_prune thresh bn =
  let prune_one s_hash l_hash (add: sym_add) = 
    let llratios = compute_llratios_prune add.root in
    let root = prune_add l_hash s_hash llratios thresh add in
    create_add add.avo add.lvo add.vars root in
  varelim prune_one thresh bn

let varelim_prune_m maxsize bn =
  varelim_mem prune_v maxsize bn

let varelim_choose_m maxsize bn =
  varelim_mem simplify_v maxsize bn

;;

(*
(* Make all leaves unique, so they can be more easily merged and stuff. *)
let rec make_leaves_unique cache (id, x) =
  if Hashtbl.mem cache id then
    Hashtbl.find id 
  else begin
    let result = 
      match x with 
        Leaf ac -> 
          (gen_hashid(), ncopy ac)
      | Split (v, children) -> 
          let children' = Array.map (make_leaves_unique seen) children in
          create_split_node shash v children' in
    (Hashtbl.add cache id result ; result)
  end

let make_leaves_unique add =
  let root = make_leaves_unique_rec add.root
  create_add add.avo add.lvo add.vars root 
*)


let rec feature_conds_rec (id, n) accu = 
  match n with
    Leaf l -> 
      [{Circuit.acnode=l; 
        Circuit.weight=Circuit.const_value l;
        Circuit.cond=accu;
        Circuit.ev=[||]}]
  | Split (v,children) ->
      let cfeatures = Array.mapi (fun i c -> 
        feature_conds_rec c ((true,v,i) :: accu)) children in
      List.flatten (Array.to_list cfeatures)

let circuit_features addl = 
  List.flatten (List.map (fun add -> feature_conds_rec add.root []) addl) 

  
(*
 * Do binary search on threshold, trying to compile at each value.
 *)



(*
 * Computing the K-L divergence resulting from approximating a set of
 * parameters with a single parameter.

(* n = numerator for q; h = numerator for entropy; d = denominator *)
let update_kld_stats (h_n, h_h, h_d) id logprob wt =

  (* Get totals so far... *)
  let base_n = Hashtbl.dfind h_n id neg_infinity in
  let base_h = Hashtbl.dfind h_h id neg_infinity in
  let base_d = Hashtbl.dfind h_d id neg_infinity in
  
  (* n = weighted avg of potential values 
       = log sum_i P(x_i) phi(x_i) *)
  let n = logsumexp2 (logprob +. wt) base_n in
  (* h = weighted avg of log potential values 
       = log sum_i P(x_i) -log phi(x_i) 
     We work with -log phi(x_i) instead, since we can only take logs of
     positive numbers, and phi(x_i) is usually <= 1. *)
  let h = logsumexp2 (logprob +. log(-.wt)) base_h in
  (* d = sum of probability weights 
       = sum_i P(x_i) *)
  let d = logsumexp2 logprob base_d in

  (* Save the updated values *)
  Hashtbl.add h_n id n ;
  Hashtbl.add h_h id h ;
  Hashtbl.add h_d id d 
 *)

(*
let compute_kld (h_n, h_h, h_d) id =
  let n = Hashtbl.find h_n id in
  let h = Hashtbl.find h_h id in
  let d = Hashtbl.find h_d id in
  (* log sum_i P(x_i) phi(x_i) = log E[phi(x)] *)
  let log_avg_phi = n -. d in
  (* 1. log[sum_i P(x_i) -log phi(x_i)] - log[sum_i P(x_i)] = 
        log( E[-log phi(x_i)] )
     2. -exp [log( E[-log phi(x_i)] )] = 
        E[log phi(x_i)] 
   *)
  let avg_log_phi = -exp(h -. d) in
  let kld = logq -. logh
  (* Here, KLD = E[log phi(x)] - log E[phi(x)] *)

let create_kld_hashes () =
  (Hashtbl.create 100, Hashtbl.create 100, Hashtbl.create 100)


let rec kld_q marg nodeorder qhash khash lastvar probs nodes =
  (* Caching *)
  let nkey = List.map fst nodes in
  if Hashtbl.mem khash nhkey then
    Hashtbl.find khash nkey
  else 
  (* Sort nodes to enforce ADD order *)
  let vo (p1,n1) (p2,n2) = nodeorder n1 n2 in
  let probs_and_nodes = List.sort nodeorder (List.combine probs nodes) in
  let (probs, nodes) = List.split probs_and_nodes in 
  let result = 
  match nodes with
    (* Base case: all leaves *)
    (id, Leaf a) :: l ->
      let leaf_vals  = List.map get_leaf_value nodes in
      let leaf_probs = List.map exp leaf_vals in
      (* Construct approximation as weighted average of probabilities *)
      let qval = log (dotprod probs leaf_probs) in
      (dotprod probs leaf_vals) -. qval

    (* When split variable is the same, recurse on the children *)
  | (id, Split (var, children)) :: l ->
      let samevar = function 
        | (id, Split (v, cl)) -> v = var
        | _ -> false in
      let (varl, nonvarl) = List.partition samevar nodes in
      let get_childrenl = function 
        | (id, Split (v, ca)) -> Array.to_list ca 
        | _ -> [] in
      let varl_children = List.transpose (List.map get_childrenl varl) in
      let recchildren = List.map (fun l -> l @ nonvarl) varl_children in
      let child_klds = 
        List.map (kld_q marg nodeorder khash lastvar probs) recchildren in

      (* If this ADD represents the conditional probability of a particular
       * variable, then the marginal probabilities are redundant with
       * the leaf probabilities (and less accurate). *)
      if var = lastvar then
        List.sumf child_klds
      (* Otherwise, weight these children by their respective probabilities *)
      else
        (dotproduct (Array.to_list marg.(var)) child_klds)
   | [] -> raise EmptyList in
  Hashtbl.add khash nkey result ; result



let rec kld marg nodeorder khash lastvar probs nodes =
  let nkey = List.map fst nodes in
  if Hashtbl.mem khash nhkey then
    Hashtbl.find khash nkey
  else 
  let vo (p1,n1) (p2,n2) = nodeorder n1 n2 in
  let probs_and_nodes = List.sort nodeorder (List.combine probs nodes) in
  let (probs, nodes) = List.split probs_and_nodes in 
  let result = 
  match nodes with
    (* Base case: all leaves *)
    (id, Leaf a) :: l ->
      let leaf_vals  = List.map get_leaf_value nodes in
      let leaf_probs = List.map exp leaf_vals in
      (* Construct approximation as weighted average of probabilities *)
      let qval = log (dotproduct probs leaf_probs) in
      (dotproduct probs leaf_vals) -. qval

    (* When split variable is the same, recurse on the children *)
  | (id, Split (var, children)) :: l ->
      let samevar = function 
        | (id, Split (v, cl)) -> v = var
        | _ -> false in
      let (varl, nonvarl) = List.partition samevar nodes in
      let get_childrenl = function 
        | (id, Split (v, ca)) -> Array.to_list ca 
        | _ -> [] in
      let varl_children = List.transpose (List.map get_childrenl varl) in
      let recchildren = List.map (fun l -> l @ nonvarl) varl_children in
      let child_klds = 
        List.map (kld marg nodeorder khash lastvar probs) recchildren in

      (* If this ADD represents the conditional probability of a particular
       * variable, then the marginal probabilities are redundant with
       * the leaf probabilities (and less accurate). *)
      if var = lastvar then
        List.sumf child_klds
      (* Otherwise, weight these children by their respective probabilities *)
      else
        (dotproduct (Array.to_list marg.(var)) child_klds)
   | [] -> raise EmptyList in
  Hashtbl.add khash nkey result ; result


  (* TODO -- make sense of this... hmmm... *)
let rec kld_all marg avo khash p root =
  match root with
    (id, Split (var, children)) ->
      let probs = Array.to_list marg.(var) in
      let base = (try Hashtbl.find id with Not_found -> 0.0) in
      (* Get KLD of removing this split *)
      let new_kld = kld marg avo khash lastvar (Array.to_list children) in
      Hashtbl.add id (base +. p *. new_kld) ;
      (* Recurse *)
      let kld_child margp c = kld_all marg avo khash (margp *. p) c in
      Array.iter2 kld_child marg.(var) children 
  | _ -> ()


(* Compute an expectation recursively for each node in the graph *)
let rec node_expectation cache marg varx f (id, n) =
  if Hashtbl.mem cache id then
    Hashtbl.find cache id
  else begin
    let e = 
      match n with 
        Leaf w -> (exp w) *. (f w)
      | Split (var, children) ->
          let cvals = 
            Array.map (node_expectation cache marg varx f) children in
          if var = varx then
            Array.sumf child_h
          else
            adotprod child_h marg.var in
    Hashtbl.add cache id e ; e
  end

(* Compute the entropy of each node and store it in a cache *)
let rec node_entropy cache marg varx (id, n) =
  let entropy_f logw = logw in
  node_expectation cache marg varx entropy_f (id, n)

(* Compute an expectation recursively for each node in the graph *)
let rec node_distexpectation cache marg varx f (id, n) =
  if Hashtbl.mem cache id then
    Hashtbl.find cache id
  else begin
    let e = 
      match n with 
        Leaf w -> LeafException
      | Split (var, children) ->
          if var = varx then
            Array.map (compose f get_leaf_value) children in
          else
            let cdists = Array.map 
              (node_distexpectation cache marg varx f) children in
            cdists
            Array.sumf child_h
            adotprod child_h marg.var in
    Hashtbl.add cache id e ; e
  end



(* Compute the log coverage of each node and stores it in the cache.
 * log coverage = log probability node will be reached according to marginal
let rec node_coverage cache marg varx currcov (id, n) = 
  let cov = 
    match n with 
      Leaf w -> currcov +. w
    | Split (var, children) ->
        if var = varx then
          Array.iter (node_coverage cache marg varx currcov) children 
        else
          Array.iter2 
            (fun p c -> node_coverage cache marg varx (log p +. currcov) c) 
          marg.(var) children in
  Hashtbl.add cache id (cov +. Hashtbl.dfind cache id neg_infinity)
 *)
*)


(*
(*
 * SECTION: 
 *
 *
 * Code for computing the structural savings of pruning a node.  In a
 * tree, this is trivial.  In a graph, it requires a few tricks to
 * reach near-linear efficiency (including the union-find algorithm).
 *)


(* Compute the number of parents for each node and return the mapping *)
let node_num_parents (rid, r) =

  (* Create set of visited nodes and number of parents hash *)
  let visited = Hashtbl.create 100 in
  let nump = Hashtbl.create 100 in
  let incp (id, n) = Hashtbl.add nump id (1 + Hashtbl.dfind nump id 0) in

  (* f increases the number of parents for each child, then
   * recursively visits all children that we haven't seen before. *) 
  let rec f (id, n) =
    if not (Hashtbl.mem visited id) then begin
      Hashtbl.add visited id () ;
      match n with 
        Leaf w -> ()
      | Split (_, children) ->
          Array.iter incp children;
          Array.iter f children
    end in

  (* Set the root (special case) then begin the recursion *)
  Hashtbl.add nump rid 0;
  f (rid, r) ;
  nump
  

(* 
 * Union find implementation 
 *)
type uf = {mutable rank: int; 
           mutable label: int;
           mutable parent: uf option}

(* MakeSet operation *)
let uf_make () = {rank = 0; label = -1; parent = None}

(* Find operation, with path compression *)
let rec uf_find_a accu x =
  match x.parent with 
    None -> 
      List.iter (fun n -> n.parent <- Some x) accu; x
  | Some p -> 
      uf_find_a (x :: accu) p

let uf_find x = uf_find_a [] x

(* Union operation, with rank updating *)
let uf_union x y =
  let x_root = uf_find x in
  let y_root = uf_find y in
  if y_root.rank < x_root.rank then
    y_root.parent <- Some x_root
  else if x_root.rank < y_root.rank then
    x_root.parent <- Some y_root
  else if x_root != y_root then begin
    y_root.parent <- Some x_root ;
    x_root.rank <- x_root.rank + 1
  end


(*
 * Counting the dependents of each node 
 *)

(* Recursive helper function for turning a node graph into a 
 * dependency tree. *)
let rec rec_dependents c_el num_p final_p (id, n) =
 match n with 
    Leaf _ -> uf_make ()
  | Split (_, children) ->
      (* ufp collects all of the nodes that depend on n and its ancestors. *)
      let ufp = uf_make () in
      let attach_child c = uf_union ufp c; (uf_find ufp).label <- id in

      let do_child (cid, c) = 
        (* Link the child here if we haven't seen it before *)
        if not (Hashtbl.mem c_el cid) then begin
          let ufc = uf_make () in
          Hashtbl.add c_el cid ufc ;
          attach_child ufc
        end ;

        (* Decrement the number of parents *)
        let rem = (Hashtbl.find num_p cid) - 1 in
        Hashtbl.add num_p cid rem ;

        (* If we've seen all parents, set the final parent and recurse *)
        if rem = 0 then begin
          (* If this child has an earlier parent set, use that.
           * Otherwise, use this node id. *)
          let parent_id = if Hashtbl.mem c_el cid then
                            (uf_find (Hashtbl.find c_el cid)).label
                          else
                            id in
          Hashtbl.add final_p cid parent_id;
          let ufc = rec_dependents c_el num_p final_p (cid, c) in
          attach_child ufc
        end in
      Array.iter do_child children ;
      ufp


(* Build a map from node id to the number of descendant nodes that would 
 * be deleted if the node was deleted. *)
let node_dependents r =
  let final_parent = Hashtbl.create 100 in
  let child_el = Hashtbl.create 100 in
  let num_parents = node_num_parents r in
  (* TODO -- is this right? *)
  ignore (rec_dependents child_el num_parents final_parent r) ;
  let total_dep = Hashtbl.create 100 in
  let visited = Hashtbl.create 100 in
  let rec sum_dependents (id, n) = 
    if not (Hashtbl.mem visited id) then begin
      (* First, recurse to children *)
      Hashtbl.add visited id ();
      match n with
        Leaf _ -> ()
      | Split (_, children) ->
          Array.iter sum_dependents children; (* TODO -- is this right? *)
      
      (* Add our total dependents + 1 (for ourself) to our
       * parent's total dependents. *)
      let pid = Hashtbl.dfind final_parent id (-1) in
      let n_dep = Hashtbl.dfind total_dep id 0 in
      let p_dep = Hashtbl.dfind total_dep pid 0 in
      Hashtbl.add total_dep pid (1 + n_dep + p_dep)
    end in
  sum_dependents r ;
  total_dep

  *)
