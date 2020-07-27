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

let mn_to_clusters mn =
  let numvars = Mn.numvars mn in 
  (numvars, Array.to_list (Array.map Mn.Factor.vars (Mn.factors mn)))
