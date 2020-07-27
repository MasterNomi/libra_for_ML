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

(* Globals used for command line parameters *)
let outbnfile = ref ""
let datafile = ref ""
let schemafile = ref ""
let prior = ref 1.

let usage = "Usage: cl -i <data> [-o <bn out>] [...]"
let args = Arg.align
 ([("-i", Arg.Set_string datafile, " Training data file") ;
   ("-s", Arg.Set_string schemafile, " Data schema (optional)") ;
   ("-o", Arg.Set_string outbnfile, " Output Bayesian network") ;
   ("-prior", Arg.Set_float prior, " Prior counts of uniform distribution")]
   @ common_arguments)

let collect_counts schema exstream =

  (* Collect all pairwise counts *)
  let numvars = Array.length schema in
  let marg_counts = Array.map (fun d -> Array.make d 0) schema in
  let create_counts i j = Array.make_matrix schema.(i) schema.(j) 0 in
  let joint_counts = 
    Array.init numvars (fun i -> Array.init i (create_counts i)) in
  let add_counts x =
    for i = 0 to numvars - 1 do
      let xi = x.(i) in
      marg_counts.(i).(xi) <- marg_counts.(i).(xi) + 1;
      for j = 0 to i - 1 do
        let xj = x.(j) in
        joint_counts.(i).(j).(xi).(xj) <- joint_counts.(i).(j).(xi).(xj) + 1 
      done;
    done in
  let n = ref 0 in
  (try while true do
    let ex = Data.input_example exstream in
    incr n;
    add_counts ex
  done with Eof -> ());
  (marg_counts, joint_counts, !n)


let compute_mi schema num_examples marg_counts joint_counts =

  (* Compute a single mutual information score *)
  let total = !prior +. float_of_int num_examples in
  let calc_mi i j =
    let mi = ref 0.0 in
    let ip = !prior /. (float_of_int schema.(i)) in
    let jp = !prior /. (float_of_int schema.(j)) in
    let ijp = !prior /. (float_of_int (schema.(i) * schema.(j))) in
    for ival = 0 to schema.(i) - 1 do
      for jval = 0 to schema.(j) - 1 do
        let p_ij = 
          (ijp +. float_of_int joint_counts.(i).(j).(ival).(jval)) /. total in
        let p_i  = (ip +. float_of_int marg_counts.(i).(ival)) /. total in
        let p_j  = (jp +. float_of_int marg_counts.(j).(jval)) /. total in
        dlogf "P(x_%d = %d, x_%d = %d) = %f\n" i ival j jval p_ij; 
        if p_ij > 0. then
          mi := !mi +. p_ij *. log (p_ij /. (p_i *. p_j))
      done;
    done;
    !mi in

  (* Calculate all mutual informations *)
  let numvars = Array.length schema in
  let all_mi = Array.init numvars (fun i -> Array.init i (calc_mi i)) in
  all_mi


let rec add_edge all_mi visited edges chosen_edges i j =

  let numvars = Array.length visited in

  (* Add new edge *)
  let chosen_edges = (i, j) :: chosen_edges in

  (* Add linked edges... *)
  let n = if visited.(i) then j else i in

  let get_mi n' =
    if n = n' then (n, n', 0.) 
    else begin
      let n1 = min n n' and n2 = max n n' in
      let mi = all_mi.(n2).(n1) in
      (* dlogf "I(%d ; %d) = %f\n" n n' mi; *)
      (n, n', mi) 
    end in
  let new_edges = Array.init numvars get_mi in
  Array.iter (Heap.add edges) new_edges;
  visited.(n) <- true;

  (* Get next best edge that won't create a cycle. *)
  let rec get_next () =
    let (i, j, mi) = Heap.min edges in
    Heap.remove_min edges;
    if visited.(i) && visited.(j) then
      get_next ()
    else
      (i, j, mi) in

  (* Check to see if we're done.  (We'll end up with numvars-1 edges in
     the end, but for now, we have a dummy edge at the beginning.) *)
  if List.length chosen_edges = numvars then
    chosen_edges
  else begin
    (* Select the best edge and recurse! *)
    let (i, j, mi) = get_next () in
    (* dlogf "CHOSEN: %d -> %d (%f)\n" i j mi; *)
    add_edge all_mi visited edges chosen_edges i j
  end


let chow_liu schema all_mi =
  let numvars = Array.length schema in
  let visited = Array.make numvars false in
  let edge_comp (_, _, mi1) (_, _, mi2) = mi1 > mi2 in
  let edges = Heap.create edge_comp 100 in
  let all_edges = add_edge all_mi visited edges [] 0 0 in
  (* Remove the first dummy edge *)
  List.tl (List.rev all_edges)


let create_bn schema edges marg_counts joint_counts =

  (* Create a BN from the edge set and the mutual informations *)
  let bn = Bn.create_empty_network schema in

  (* i = parent; j = child *)
  let set_cpd (i, j) =
    let counts = 
      if i < j then 
        Array.transpose joint_counts.(j).(i)
      else
        joint_counts.(i).(j) in
    let table = Array.make_matrix schema.(i) schema.(j) 0. in
    let ip = !prior /. (float_of_int schema.(i)) in
    let ijp = !prior /. (float_of_int (schema.(i) * schema.(j))) in
    for ii = 0 to schema.(i) - 1 do
      for jj = 0 to schema.(j) - 1 do
        table.(ii).(jj) <- log (((ijp +. float_of_int counts.(ii).(jj))) /. 
                                (ip +. float_of_int marg_counts.(i).(ii)));
      done
    done;
    Bn.set_cpt bn j [i] table in 
  List.iter set_cpd edges; 

  (* Set marginal distributions for all nodes with no parents *)
  for i = 0 to Array.length schema - 1 do
    if Bn.parents bn i = [] then begin
      let ip = !prior /. (float_of_int schema.(i)) in
      let denom = !prior +. float_of_int (Array.sum marg_counts.(i)) in
      let count_to_weight c = log ((ip +. float_of_int c) /. denom) in
      let dist = Array.map count_to_weight marg_counts.(i) in
      Bn.set_cpt bn i [] [|dist|]
    end 
  done;
  bn


let main () =
  Arg.parse args ignore usage;
  if !outbnfile = "" || !datafile = "" then
    (Arg.usage args usage; exit 0);
  common_log_init ();
  let schema = 
    (* Scan the data file *)
    if !schemafile = "" then begin
      Timer.start "schema";
      let s = Data.stream_schema (open_in !datafile) in
      nlogf "Inferring schema from data: %f s\n" (Timer.elapsed "schema"); s
    (* Read the schema from a separate file *)
    end else 
      Data.input_example (open_in !schemafile) in
  nlogf "Schema:\n%s" (Data.to_string_schema schema);
    let exstream = open_in !datafile in
  flush stdout;
  Timer.start "counts";
  let (marg_counts, joint_counts, n) = collect_counts schema exstream in 
  nlogf "Collecting counts: %f s\n" (Timer.elapsed "counts");
  Timer.start "mi";
  let all_mi = compute_mi schema n marg_counts joint_counts in
  nlogf "Computing MI: %f s\n" (Timer.elapsed "mi");
  Timer.start "edges";
  let edges = chow_liu schema all_mi in 
  nlogf "Picking edges: %f s\n" (Timer.elapsed "edges");
  Timer.start "bn";
  let bn = create_bn schema edges marg_counts joint_counts in
  nlogf "Creating BN: %f s\n" (Timer.elapsed "bn");

  vlogf "\nEdge list:\n";
  List.iter (fun (i,j) -> vlogf "%d -> %d\n" i j) edges;

  (* Print out BN *)
  if !outbnfile <> "" then
    Bn.write_auto !outbnfile bn
;;

let _ = main ()
