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
open Add

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

exception OutOfMemory of int

(* Cache defined globally to save time. *)
let multcache = Hashtbl.create 100 

(* Sums and products for building ACs *)
let leaf_sum_sym id children =
  (id, Leaf (Circuit.create_plus children))

let leaf_product_sym id l1 l2 =
  (id, Leaf (Circuit.create_times [l1;l2]))

let const_leaf_value l =
  if Circuit.is_const l then 
    Circuit.const_value l
  else 
    0.0

(* Sums and products for running VE *)
let leaf_sum_const lcache id children =
  let v = logsumexp (List.map const_leaf_value children) in
  Add.create_constleaf_node_hashed lcache v

let leaf_product_const lcache id l1 l2 =
  let v = (const_leaf_value l1) +. (const_leaf_value l2) in
  Add.create_constleaf_node_hashed lcache v


let rec rec_eliminate (leaf_sum, leaf_product) maxsize avo addset = function
  | [] -> 
      if log_exists log_debug then begin
        dlogf "Final add set:\n";
        List.iter (output (log_stream log_debug)) addset
      end;
      addset
  | v :: l ->
      (* For each variable in the elimination ordering, *)
      (* Select all ADDs with the variable *)
      Timer.start "elim";
      dlogf "Eliminating variable %d.\n" v;
      let has_v add = add.vars.(v) in
      let (relevant, irrelevant) = List.partition has_v addset in
      dlogf "Variable %d appears in %d factors.\n" v (List.length relevant);

      (* Multiply smallest ones first -- WORKS!!! *)
      let multiply x y =
        let product = multiplyc leaf_product multcache x y in
        if maxsize > 0. && float_of_int(node_size product.root) > maxsize then
          (nlogf "VARIABLE %d FAILED!\n\n" v; raise (OutOfMemory v))
        else
          product in
        (* else (printf "size %d < maxsize %d\n" (node_size product.root) maxsize; product) *) 
      let orelevant = List.sort cmpsize relevant in
      let product = List.fold_left multiply  
        (List.hd orelevant) (List.tl orelevant) in
      Hashtbl.clear multcache;
      dlogf "Multiplied relevant factors.\n"; 
      if log_exists log_debug then
        (output (log_stream log_debug)) product;

      (* Pick the factor with fewest vars that aren't in current factor *
      let rec multall rel =
        match (List.sort cmpsize rel) with
          a :: [] -> a
        | a :: b :: l -> multall ((multiply a b) :: l)
        | [] -> assert false in
      let product = multall (indicator_adds.(v) :: relevant) in *)

      (* Always multiply smallest pair of factors next -- Similar
       * to previous method.
      let rec multall rel =
        match (List.sort cmpsize rel) with
          a :: [] -> a
        | a :: b :: l -> multall ((multiply a b) :: l)
        | [] -> assert false in
      let product = multall (indicator_adds.(v) :: relevant) in
      vlogf "Eliminating var %d: %fs\n" v (Timer.delta "elim") ;
       *) 
       
      (* ...and sum out the eliminated variable *)
      let summed_out = sum_out leaf_sum v product in 
      vlogf "Summing out %d: %fs\n" v (Timer.delta "elim") ;
      vlogf "Final size: %d\n" (hsize summed_out); 
      let addset' = summed_out :: irrelevant in
      vlogf "%d factors remaining.\n" (List.length addset');
      if log_exists log_debug then
        List.iter (output (log_stream log_debug)) addset';
      rec_eliminate (leaf_sum, leaf_product) maxsize avo addset' l 

;;

let eliminate sumprod maxsize avo addset lvo =
  rec_eliminate sumprod maxsize avo addset lvo

let mn_to_adds mn avo lvo = 
  let numvars = Mn.numvars mn in
  let schema = Mn.schema mn in
  let vn = Circuit.make_vnodes schema in

  (* Create ADDs for each variable *)
  let s_hash = Hashtbl.create 100 in
  let l_hash = Hashtbl.create 100 in
  let indicator_adds = Array.init numvars (indicators_to_add avo lvo vn) in
  dlogf "Created indicator ADDs.\n";
  let schema = Mn.schema mn in
  let factor_adds = 
    Array.map (factor_to_adds s_hash l_hash avo lvo schema) (Mn.factors mn) in
  (List.flatten (Array.to_list factor_adds), 
   Array.to_list indicator_adds, s_hash, l_hash)

let bn_to_adds bn avo lvo =
  mn_to_adds (Bn.to_mn bn) avo lvo


exception OutOfMemory of int

let rec compile_until_small sumprod avo lvo prune_v maxsize adds ind_adds =
  vlogf "cus maxsize = %f\n" maxsize;
  try
    (eliminate sumprod maxsize avo (adds @ ind_adds) lvo, adds)
  with OutOfMemory v -> 
    (* Recompute sizes/ordering/etc. *)
    List.iter AddPrune.compute_pruned_sizes adds;
    let a = List.hd adds in
    let avo = a.avo and lvo = a.lvo in
    (* Half the worst-case size of the table with v. *)
    let sizes = AddPrune.max_table_sizes avo lvo adds in
    let adds' = AddPrune.prune_table_to_size prune_v adds (sizes.(v) /. 2.) v in
    compile_until_small sumprod avo lvo prune_v maxsize adds' ind_adds


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


let varelim buildac prune_one thresh mn = 
  dlogf "Entering varelim...\n";
  Timer.start "ve";
  let lvo = Minfill.minfill_order (Minfill.mn_to_clusters mn) in
  let avo = build_avo lvo in
  nlogf "Minfill ordering: %fs\n" (Timer.delta "ve") ;

  let (factor_adds, ind_adds, s_hash, l_hash) = mn_to_adds mn avo lvo in
  nlogf "ADD creation: %fs\n" (Timer.delta "ve") ;

  let factor_adds = List.map (prune_one s_hash l_hash) factor_adds in
  nlogf "Pruning: %fs\n" (Timer.delta "ve") ;
        
  let sumprod = 
    if buildac then (leaf_sum_sym, leaf_product_sym)
    else (leaf_sum_const l_hash, leaf_product_const l_hash) in

  let add_roots = eliminate sumprod 0. avo (factor_adds @ ind_adds) lvo in
  nlogf "Variable elimination: %fs\n" (Timer.delta "ve");
  (finish_ac add_roots, factor_adds)


let varelim_mem buildac prune_v maxsize mn = 
  dlogf "Entering varelim_mem...\n";
  Timer.start "ve";
  let lvo = Minfill.minfill_order (Minfill.mn_to_clusters mn) in
  let avo = build_avo lvo in
  nlogf "Minfill ordering: %fs\n" (Timer.delta "ve") ;

  let (factor_adds, ind_adds, s_hash, l_hash) = mn_to_adds mn avo lvo in
  nlogf "ADD creation: %fs\n" (Timer.delta "ve") ;

  let p = prune_v s_hash l_hash in
  let factor_adds = List.map (AddPrune.prune_one_to_size maxsize p 1.0) factor_adds in
  let sumprod = 
    if buildac then (leaf_sum_sym, leaf_product_sym)
    else (leaf_sum_const l_hash, leaf_product_const l_hash) in
  let (add_roots, factor_adds) = 
    compile_until_small sumprod avo lvo p maxsize factor_adds ind_adds in
  nlogf "Variable elimination: %fs\n" (Timer.delta "ve");
  (finish_ac add_roots, factor_adds)


let varelim_choose buildac thresh mn = 
  let prune_one s_hash l_hash (add: sym_add) = 
    let llratios_choose = AddPrune.compute_llratios_choose add in 
    let prh = AddPrune.compute_llratios_prune add.root in
    let root = 
      snd (AddPrune.simplify_add l_hash s_hash llratios_choose prh thresh add) in
    create_add add.avo add.lvo add.vars root in
  varelim buildac prune_one thresh mn


let varelim_prune buildac thresh mn =
  let prune_one s_hash l_hash (add: sym_add) = 
    let llratios = AddPrune.compute_llratios_prune add.root in
    let root = AddPrune.prune_add l_hash s_hash llratios thresh add in
    create_add add.avo add.lvo add.vars root in
  varelim buildac prune_one thresh mn

let varelim_prune_m buildac maxsize mn =
  varelim_mem buildac AddPrune.prune_v maxsize mn

let varelim_choose_m buildac maxsize mn =
  varelim_mem buildac AddPrune.simplify_v maxsize mn
