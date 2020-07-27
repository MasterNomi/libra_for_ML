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
open Add

(*********************************
 * Dynamic simplification of ADDs
 *********************************)

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




(*
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
  dlogf "SIMPLIFY: %d -> %d (t:%f)\n" original_size (node_size result) thresh;
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
    if maxsize > 0. && s > maxsize then
      prune_thresh (thresh *. 2.) 
    else
      vadds' @ adds in
  prune_thresh 2.0


let rec prune_to_size maxsize prune_v adds =
  (* Recompute sizes/ordering/etc. *)
  let a = List.hd adds in
  let avo = a.avo and lvo = a.lvo in
  let sizes = max_table_sizes avo lvo adds in
  (* Prune until the largest factor is smaller than the required size *)
  if maxsize > 0. && Array.max sizes > maxsize then begin
    let v = Array.argmax sizes in
    vlogf "Size of %d is %f\n" v sizes.(v);
    let adds' = prune_table_to_size prune_v adds maxsize v in
    (* Move on to check other factors... *)
    prune_to_size maxsize prune_v adds'
  end else
    adds


let rec prune_one_to_size maxsize prune_v thresh add =
  let add' = prune_v (-1) thresh add in
  let s = size add' in
  if maxsize > 0. && (float_of_int s) > maxsize then
    (dlogf "Size %d is too big!\n" s; 
    prune_one_to_size maxsize prune_v (thresh *. 2.0) add)
  else
    (dlogf "Size %d is ok!\n" s; add')
