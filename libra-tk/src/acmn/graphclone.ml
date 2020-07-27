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

open Circuit
open Ext

(* Dummy nodes with the right details *)
let dummy_times = create_node TimesNode []
let dummy_plus = create_node PlusNode []

(*********************************************************
 * Core code for copying and relinking subgraphs
 *********************************************************)

(* ...UNUSED...
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
*)

(* Generic function to copy a sub-tree according to some criteria.
   The copied sub-tree is guaranteed to not reference any of the
   original descendants of r. *)
let rec copy_tree ncreate enter changed f_rec f_link h r =
  if NMap.mem h r then NMap.find h r 
  else if not (f_rec r) then r
  else begin
    enter r;
    let children = List.filter f_link r.children in
    let copy_child = copy_tree ncreate enter changed f_rec f_link h in
    let c_copies = List.map copy_child children in
    let r_copy = 
      match c_copies with 
        [x] -> x    (* Skip nodes with just one child *)
      |  l  -> ncreate r c_copies  in
    NMap.add h r r_copy ; 
    if List.length c_copies = 1 then changed r null_node 
                                else changed r r_copy ; 
    r_copy
  end


(* Copy a node that is only an ancestor of dist *)
let copy_dist ncreate enter changed anc h =
  (* Recurse on all ancestors of the target node that are not
     themselves feature nodes. *)
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
        ncreate dummy_times [v; d_copy; s_copy] in
      let pchildren = List.map pchild v_chosen in
      (ncreate dummy_plus pchildren) :: others 
    end in

  let is_ma c = s_anc c && d_anc c in
  let has_ma_child = not (is_times r) || List.exists is_ma r.children in
  let children = if has_ma_child then ma_children () 
                                 else no_ma_children () in
  let r_copy = match children with   
                 [x] -> x       (* Skip nodes with just one child *)
               |  l  -> ncreate r children in
  NMap.add h_ma r r_copy ;
  (* If we've reached a mutual ancestor, then this node has changed. *)
  (if not has_ma_child then begin
    if List.length children = 1 then (changed r null_node)
                                else (changed r r_copy)
  end) ;
  r_copy
  end 
