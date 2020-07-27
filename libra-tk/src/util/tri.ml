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

(* Returns the var with the most neighbors already selected. *)
let max_cardinality_var adj ordered =
  let best_num = ref (-1) in
  let best = ref (-1) in
  for i = 0 to Array.length ordered - 1 do
    if not ordered.(i) then
      let num_neighbors = ref 0 in
      for j = 0 to Array.length ordered - 1 do
        if ordered.(j) && adj.(i).(j) then 
          incr num_neighbors
      done ;
      if !num_neighbors > !best_num then 
        (best := i ; best_num := !num_neighbors)
  done ;
  !best

(* Adds edges between previously selected variables that are both
 * adjacent to v.
 *)
let fill_in adj ordered v =
  let changes = ref false in
  for i = 0 to Array.length ordered - 1 do
    if ordered.(i) && adj.(v).(i) then
      for j = 0 to i - 1 do
        if ordered.(i) && adj.(v).(j) && not adj.(i).(j) then
          (adj.(i).(j) <- true ; adj.(j).(i) <- true ; changes := true)
      done
  done ;
  !changes

exception GraphIsChordal
exception GraphNotChordal

let main () =
  (* Read basic adjacencies in the form of clusters *)
  let clusters = read_clusters () in

  (* Construct var-to-name and name-to-var mapping *)
  let n2v = Hashtbl.create 100 and v2n = Hashtbl.create 100 in
  let add_name n =
    if not (Hashtbl.mem n2v n) then begin
      let v = Hashtbl.length n2v in
      Hashtbl.add n2v n v ;
      Hashtbl.add v2n v n 
    end in
  List.iter (fun c -> List.iter add_name c) clusters ; 
  let vclusters = List.map (List.map (Hashtbl.find n2v)) clusters in

  (* Construct adjacency table *)
  let numvars = Hashtbl.length n2v in
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
  List.iter add_pairs vclusters ;

  try while true do 
    let is_ordered = Array.make numvars false in
    try 
      for idx = 0 to numvars - 1 do
        let v = max_cardinality_var adj is_ordered in
        is_ordered.(v) <- true ;
        if fill_in adj is_ordered v then 
          raise GraphNotChordal   (* Added adjacencies; reset loop *)
      done ; raise GraphIsChordal (* Triangulation done; exit loop *)
    with GraphNotChordal -> ()
  done with GraphIsChordal -> () ;

  (* Print cliques to stdout *)
  let is_ordered = Array.make numvars false in
  for idx = 0 to numvars - 1 do 
    let v = max_cardinality_var adj is_ordered in
    is_ordered.(v) <- true ;
    printf "%s" (Hashtbl.find v2n v) ;
    for j = 0 to numvars - 1 do
      if adj.(v).(j) && is_ordered.(j) then
        printf " %s" (Hashtbl.find v2n j)
    done ;
    printf "\n"
  done ;

;;

let _ = main ()
