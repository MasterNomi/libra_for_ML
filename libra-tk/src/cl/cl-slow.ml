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

(* OCaml implementation of Chow-Liu algorithm *)

open Ext
open Data
open Printf

(* DEBUG *)
let vlog = fprintf stdout


let mutual_information schema examples i j =
  let counts = 
    Array.init schema.(i) (fun _ -> Array.make schema.(j) 0.) in
  let icounts = Array.make schema.(i) 0. in
  let jcounts = Array.make schema.(j) 0. in
  let add_counts x =
    let xi = x.(i) and xj = x.(j) in
    icounts.(xi) <- icounts.(xi) +. 1.;
    jcounts.(xj) <- jcounts.(xj) +. 1.;
    counts.(xi).(xj) <- counts.(xi).(xj) +. 1. in
  Array.iter add_counts examples;
  let total = float_of_int (Array.length examples) in
  let mi = ref 0.0 in
  for ival = 0 to schema.(i) - 1 do
    for jval = 0 to schema.(j) - 1 do
      let p_ij = counts.(ival).(jval) /. total in
      let p_i  = icounts.(ival) /. total in
      let p_j  = jcounts.(jval) /. total in
      (* printf "P(x_%d = %d, x_%d = %d) = %f\n" i ival j jval p_ij; *)
      if p_ij > 0. then
        mi := !mi +. p_ij *. log (p_ij /. (p_i *. p_j))
    done;
  done;
  vlog "MI (%d, %d, %f)\n" i j !mi; 
  (i, j, !mi)


let rec add_edge schema examples visited edges chosen_edges i j =

  (* Add new edge *)
  let chosen_edges = (i, j) :: chosen_edges in

  (* Add linked edges... *)
  let n = if visited.(i) then j else i in
  let numvars = Array.length schema in
  let new_edges = Array.to_list
    (Array.init numvars (fun n' -> mutual_information schema examples n n')) in
  let edges = new_edges @ edges in
  visited.(n) <- true;

  (* Remove candidate edges that would create a cycle *)
  let edges =
    List.filter (fun (i, j, v) -> not visited.(i) || not visited.(j)) edges in

  (* Check to see if we're done... *)
  if List.length chosen_edges = numvars - 1 then
    chosen_edges
  else begin
    (* Select the best edge and recurse! *)
    (* TODO -- we can do this in O(n^2) instead of O(n^2 log n) if we care. *)
    let sort_edges = 
      List.sort (fun (_, _, mi1) (_, _, mi2) -> compare mi2 mi1) edges in
    let (i, j, mi) = List.hd sort_edges in
    vlog "CHOSEN (%d, %d, %f)\n" i j mi; 
    add_edge schema examples visited sort_edges chosen_edges i j
  end


let chow_liu schema examples =
  let numvars = Array.length schema in
  let visited = Array.make numvars false in
  add_edge schema examples visited [] [] 0 0


let main () =
  let exstream = open_in Sys.argv.(1) in
  let examples = Data.input_example_list exstream in
  let schema = Data.schema examples in
  let a_examples = Array.of_list examples in
  let edges = chow_liu schema a_examples in 
  let print_edge (i, j) =
    printf "(%d, %d)\n" i j in
  List.iter print_edge edges
;;

let _ = main ()
