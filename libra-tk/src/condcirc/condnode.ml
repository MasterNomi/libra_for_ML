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
			
(*
 * Type declarations 
 *) 

type ndetails = TimesNode
              | PlusNode
              | VarNode of int * int (* (variable, value) *)
              | ConstNode of float
							| InverseNode
							| EvNode of int 
							| ExpNode 
              | NullNode ;;

type node = {hashid: int ;
             mutable id: int ; 
             mutable parents: node list ; 
             mutable children: node list ;
             mutable details: ndetails}

let hashid n = n.hashid

(* 
 * Hashing data structures
 *)
module NHashMap = Hashtbl.Make (struct 
                               type t = node
                               let equal a b = a.hashid == b.hashid
                               let hash a = a.hashid 
                             end)

module NHashSet =
struct
  include NHashMap

  let add ns x = add ns x ()
  let iter f ns = iter (fun x () -> f x) ns
  let fold f ns = fold (fun x () l -> f x l) ns
  let to_list ns = fold (fun x l -> x :: l) ns []
  let sum_map f ns = fold (fun x accu -> f x + accu) ns 0
  let sumf_map f ns = fold (fun x accu -> f x +. accu) ns 0.0
  let filter f ns = iter (fun n -> if not (f n) then remove ns n) ns
end

(*
module NArrayMap =
struct
  include Array 
  let create size = create size None
  let mem a k = match a.(k.id) with Some _ -> true | None -> false
  let find a k = match a.(k.id) with 
                   Some v -> v
                 | None -> raise Not_found
  let add a k v = a.(k.id) <- Some v
  let remove a k = a.(k.id) <- None 
end

module NArraySet =
struct
  include Array 
  let create size = create size false
  let mem a k = a.(k.id)
  let add a k = a.(k.id) <- true
  let remove a k = a.(k.id) <- false
end
*)

module NSet = NHashSet
module NMap = NHashMap

(* Assign hash ids sequentially with the help of a global mutable var *)
let global_hashid = ref 0

let set_hashid_ref refid = 
	global_hashid := refid;
	ignore()

let gen_hashid () =
  let id = !global_hashid in
  incr global_hashid ;
  assert (!global_hashid > 0) ;
  id

let null_node = {id = -1; 
                 hashid = gen_hashid ();
                 parents = []; 
                 children = []; 
                 details = NullNode}

(* Having a single hashtable that we clear and reuse is more efficient
 * than creating them whenever necessary.  
 *
 * WARNING: This hack also makes the code very non-threadsafe.
 *)
let tmp_hash = NSet.create 1000 


(*
 * SECTION 0: Constructors
 *)

let create_node d c =
  let i = gen_hashid () in
  {id = -1; 
   hashid = i;
   parents = []; 
   children = c; 
   details = d}

(* Copy constructor *)
let ncopy n = create_node n.details []

let create_var var value = create_node (VarNode (var, value)) []
let create_const wt = create_node (ConstNode wt) []
let create_times = create_node TimesNode
let create_plus = create_node PlusNode
let create_inverse ch = create_node InverseNode [ch]
let create_evnode var = create_node(EvNode var) [] 
let create_exp ch = create_node ExpNode [ch]

(* 
 * SECTION 1: Operations involving nodes only 
 *)

let is_times n = match n.details with TimesNode      -> true | _ -> false
let is_plus n  = match n.details with PlusNode       -> true | _ -> false
let is_var n   = match n.details with VarNode(_,_)   -> true | _ -> false
let is_const n = match n.details with ConstNode(_)   -> true | _ -> false
let is_null  n = match n.details with NullNode       -> true | _ -> false
let is_inverse n = match n.details with InverseNode  -> true | _ -> false
let is_ev  n = match n.details with EvNode(_) -> true | _ -> false
let is_exp  n = match n.details with ExpNode -> true | _ -> false

let const_value n = match n.details with ConstNode wt -> wt 
                                       | _ -> assert false

let var_details n = match n.details with VarNode(vr,vl) -> (vr,vl)
                                       | _ -> assert false

let var_var n   = match n.details with VarNode(vr,vl) -> vr
																		 | _ -> assert false


let ev_var n   = match n.details with EvNode(vr) -> vr
                                     | _ -> assert false

let var_value n = match n.details with VarNode(vr,vl) -> vl
                                  | _ -> assert false


let is_var_product n = match n.details with
    TimesNode -> List.exists is_const n.children &&
                 List.exists is_var n.children 
  | _ -> false

let is_sot_dist n = match n.details with
    PlusNode -> List.for_all is_var_product n.children
  | _ -> false

let set_const n wt = n.details <- ConstNode wt 

let __add_parent p n    = n.parents <- p :: n.parents
let __remove_parent p n = n.parents <- List.remove_fast p n.parents 

let add_child p c = 
  __add_parent p c ;
  match p.details with
		ExpNode
  | InverseNode
  | TimesNode 
  | PlusNode -> p.children <- c :: p.children 
    (* No other type of node should have children, but we'll cut them
       some slack and assume they'll fix it later. *)
  | _ -> p.children <- c :: p.children

let remove_child p c =
  __remove_parent p c ;
  match p.details with 
  	ExpNode
	| InverseNode
  | TimesNode
  | PlusNode -> p.children <- List.remove_fast c p.children
  | _ -> assert false

(* OPT: These are n^2 and could be sped up. *)
let remove_all_children p = 
  List.iter (__remove_parent p) p.children ; p.children <- [] 

let remove_all_parents c = 
  List.iter (fun x -> remove_child x c) c.parents 
  (* ; printf "Parents remaining: %d\n" (List.length c.parents) *)

let unlink n = remove_all_parents n ; remove_all_children n

(* Accessors, for convenience in function passing *)
let id n = n.id
let parents n = n.parents
let children n = n.children
let num_children n = List.length n.children

(*
 * Debug functions 
 *)
let node_name n = "n" ^ (string_of_int n.id)
let node_name_hack n = match n.details with
    ConstNode w -> sprintf "%.6g" (exp w)
  | _ -> node_name n

let string_of_node n = 
  let s = (node_name n) ^ " = " in
  (* let children = List.map get_name n.children in *)
  let children = List.map node_name_hack n.children in 
  match n.details with 
    TimesNode -> s ^ String.concat " * " children 
  | PlusNode -> s ^ String.concat " + " children
  | ConstNode f -> s ^ (sprintf "%.6g" (exp f))
  | VarNode(var, value) -> s ^ (sprintf "v%d_%d" var value)
  | EvNode var -> s ^ (sprintf "e%d" var)
  | InverseNode -> s ^ String.concat " / " children
	| ExpNode -> s ^ String.concat " # " children
  | NullNode -> "null"

let output_node channel n = 
  output_string channel (string_of_node n)

let print_node = output_node stdout

let print_node_endline n = print_node n ; print_string "\n"

(* 
 * Functions for traversing node networks 
 *)
let rec add_related f h n =
  if not (NSet.mem h n) then begin
    NSet.add h n ;
    List.iter (add_related f h) (f n)
  end

let relatedl f ns = 
  let rel = NSet.create 100 in
  List.iter (add_related f rel) ns ; rel

(* let a_mem a n = n.id < Array.length a && a.(n.id) *)
let a_mem a n = a.(n.id) 

let rec add_related_a f a n =
  if not (a_mem a n) then begin
    a.(n.id) <- true ;
    List.iter (add_related_a f a) (f n)
  end

let relatedl_a size f ns = 
  let rel = Array.make size false in
  List.iter (add_related_a f rel) ns ; rel

let related f n = relatedl f [n]

let ancestors n = related parents n 
let descendants n = related children n

let ancestorsl ns = relatedl parents ns 
let descendantsl ns = relatedl children ns

(* UNUSED 
let rec add_prelated f h l n =
  if not (NMap.mem h n) then
    NMap.add h n l ;
    let l' = n::l in
    List.iter (add_prelated f h l') (f n)

let prelated f n = 
  let rel = NMap.create 100 in
  add_prelated f rel [] n ; rel

let pancestors n = prelated get_parents n
let pdescendants n = prelated get_children n
 *)

let move_parent_refs n_old n_new =
  let parents = n_old.parents in
  remove_all_parents n_old ;
  List.iter (fun x -> add_child x n_new) parents 
  (* ; remove_all_children n_old *)

(* Iterate through n and all its children (depth-first) *)
let node_iter f n = 
  (* let h = NSet.create 1000 in *)
  let h = tmp_hash in NSet.clear tmp_hash ;
  let rec reciter n = 
    if not (NSet.mem h n) then 
      (NSet.add h n ; f n; List.iter reciter n.children) in
  reciter n

(* Map applied to n and all its children (depth-first) *)
let node_map f n = 
  (* let h = NSet.create 1000 in *)
  let h = tmp_hash in NSet.clear tmp_hash ; 
  let rec recmap accu n = 
    if NSet.mem h n then accu (* List.rev accu *)
    else begin
      let accu' = (f n) :: accu in
      NSet.add h n ;
      List.fold_left recmap accu' n.children
    end in
  let result = recmap [] n in
  result

let root_to_list root = node_map identity root

let prune_orphans root = 
  node_iter (fun n -> n.parents <- []) root ;
  let notify_children p = List.iter (__add_parent p) p.children in
  node_iter notify_children root

  (* May leave orphans *)
let prune_single_parents root =
  let prune_if_single p = 
    match p.children with 
      [c] -> move_parent_refs p c 
    |  _  -> () in
  node_iter prune_if_single root

  (* Will not leave orphans *)
let prune_single_children root =
  let prune_if_single c =
    if (* is_plus c || *) is_times c then
      match c.parents with
        [p] -> if p.details = c.details then begin
                 remove_child p c ;
                 List.rev_iter (add_child p) c.children ; 
                 unlink c 
               end
      |  _  -> () in
  node_iter prune_if_single root

