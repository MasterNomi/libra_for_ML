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

let read_clusters () = 
  let clusters = ref [] in
  (try 
    while true do
      let vars = Str.split (Str.regexp "[ \t]+") (read_line ()) in
      clusters := vars :: !clusters 
    done
  with End_of_file -> ()) ;
  List.rev !clusters

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
  List.iter (fun (i,j) -> adj.(i).(j) <- true; adj.(j).(i) <- true) edges ;
  (List.length edges > 0)

let minfill_var adj elim =
  let best_num = ref (max_int) in
  let best = ref (-1) in
  for i = 0 to Array.length elim - 1 do
    if not elim.(i) then
      let num_edges = List.length (fill_edges adj elim i) in
      if num_edges < !best_num then 
        (printf "New best %d (%d edges)\n" i num_edges; best := i ; best_num := num_edges)
      else if num_edges == !best_num then
        printf "Tie best %d (%d edges)\n" i num_edges
  done ;
  printf "Best is %d (%d edges)\n" !best !best_num ;
  !best


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

  let elim = Array.make numvars false in
  for idx = 0 to numvars - 1 do
    let v = minfill_var adj elim in
    elim.(v) <- true ;
    printf "Next var: %s\n" (Hashtbl.find v2n v) ;
    let _ = fill_in adj elim v in ()
  done ; ()

  (* Print cliques to stdout 
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
  *)

;;

let _ = main ()
