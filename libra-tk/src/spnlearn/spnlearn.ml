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

open Filename
open Ext
open Printf
open Circuit
open Spn
open Unix
open Random


let acfile = ref ""
let qfile = ref ""
let vfile = ref ""
let evfile = ref ""
let datafile = ref ""
let l1param = ref 5.0
let stddev = ref 1.0
let maxe = ref 100000
let split_penalty = ref 10.0
let network = ref "spac"
let ratio = ref 0.000000001
let cut_thresh = ref 0.00001
let schemafile = ref ""
let num_part = ref 5
let lambda = ref 0.2 
let outputmodel = ref ""
let concurrent_thr = ref 4
let outputinference = ref ""
let max_depth = ref 0
let max_extension = ref 5
let extension_num = ref 0
let start_time = ref 0.0
let output_dir = ref ""
let minl1 = ref 1.0
let minedge = ref 200000.0
let minps = ref 2.0
let seed = ref 0
let override = ref false
let indicators = ref [||]

let usage = "Usage: spnlearn -i <data> -s <schema> -o <output> [...]"
let args = Arg.align
  ([

    ("-i", Arg.Set_string datafile, " Training data file");
    ("-s",  Arg.Set_string schemafile,  " Schema file");
    ("-o", Arg.Set_string output_dir, " Output ac file");
    ("-l",  Arg.Set_float lambda,  " EM clustering penalty");
    ("-k",  Arg.Set_int num_part,  " Max sum nodes' cardinalities");
    ("-cp",  Arg.Set_int concurrent_thr,  " Number of concurrent process");
    ("-vth",  Arg.Set_float cut_thresh,  " Vertical cut thresh");
    ("-seed", Arg.Set_int seed, " Seed value for random generator");
    ("-f",  Arg.Set override,  " Force to override model directory")
    ] @ common_arguments)
  

exception NodeTypeError


let append_string path s =
    let chan = open_out_gen [Open_wronly; Open_creat] 0o666 path
    in let len = out_channel_length chan
    in
        begin
        seek_out chan len;
        output_string chan s;
        close_out chan;
        end

let learn_leaves node = 
  let numf = (float_of_int (Array.length node.data)) +. 2.00 in
  let ones = (float_of_int (Array.fold_left (fun s x-> s + x.(0) ) 0 node.data)) +. 1.00 in 
  let p1 = ones  /. numf in
  let zeors = numf -. ones in
  let p0 = zeors /. numf in
  let p1node = Circuit.create_const (log p1) in
  let p0node = Circuit.create_const (log p0) in
  let v1 = !indicators.(snd node.schema.(0)).(1) in
  let v0 = !indicators.(snd node.schema.(0)).(0) in
  let times1 = Circuit.create_times [v1; p1node] in
  let times0 = Circuit.create_times [v0; p0node] in
  let plus = Circuit.create_plus [times0; times1] in
  plus

let rec build_spn node cut_thr depth=
  let varnum = Array.length node.schema in
  let samplesize = Array.length node.data in 

  if varnum > 1 then begin
    let hNodeList  = Spn.h_split node !num_part  !lambda !concurrent_thr !ratio  in
    vlogf "size of h node list: %d\n" (List.length hNodeList);
    let s = List.length hNodeList in
    if s > 1 then begin
      let sum_children = ref [] in
      for i=0 to (List.length hNodeList) - 1 do
        try 
          let ndepth = depth - 1 in
          let n = List.nth hNodeList i in
          let nsamplesize = Array.length n.data in 
          let vNodeList = Spn.v_split_gtest (List.nth hNodeList i) cut_thr in
          let circ_nodes = List.map (fun n->build_spn n cut_thr (ndepth - 1)) vNodeList in
          let times = Circuit.create_times circ_nodes in

          let weight = (float_of_int nsamplesize) /. (float_of_int samplesize) in
          (*vlogf "%f " weight; *)
          let param_node = create_const (log weight) in
          let subtree = Circuit.create_times [param_node; times] in
          sum_children := subtree::!sum_children

        with VSplitNotPossible -> begin
          vlogf "V-split not possible\n"; 
            
          let n = List.nth hNodeList i in
          let nsamplesize = Array.length n.data in 
            
          let weight = (float_of_int nsamplesize) /. (float_of_int samplesize) in
          let param_node = Circuit.create_const (log weight) in
          let plus = build_spn (List.nth hNodeList i) cut_thr (depth-1) in
            
          let subtree = Circuit.create_times [param_node; plus] in
          sum_children := subtree::!sum_children
        end 
      done;
      Circuit.create_plus !sum_children
    end
    else begin 
      let vNodeList = Spn.v_split_gtest node 1000.0 in
      let circ_nodes = List.map (fun n->learn_leaves n) vNodeList in
      let times = Circuit.create_times circ_nodes in
      times
    end
  end
  else begin
    learn_leaves node
  end


let learn_spac_structure data schema_ar = 
  (*let schema_ar = Array.of_list schema in*)
  let i_schema = Array.augment schema_ar in
  indicators := Circuit.make_vnodes schema_ar;
  let indicators_l = Array.map Array.to_list !indicators in
  let root = Spn.create_leaf data i_schema in
  let acroot = build_spn root !cut_thresh !max_extension in
  Spn.print_spn root;
  Circuit.of_graph schema_ar !indicators indicators_l acroot 



exception InsufficientArguments



let endswith s1 s2 =
  let re = Str.regexp (Str.quote s2 ^ "$")
  in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false


let main() =
  seed := ((int_of_float (Unix.time()) )  mod  1000); 
  Arg.parse args ignore usage;
  start_time := Unix.gettimeofday();
  common_log_init();

  Random.init !seed;
  if !datafile = "" ||  !schemafile = "" || !output_dir = "" then (Arg.usage args usage; exit(-1));
  if not (check_suffix !output_dir ".ac") then begin nlogf "Error: output model should end with .ac \n"; exit 1 end;


  
  Timer.start "spnlearn";  

  let data = Data.load_data_ar !datafile in

  let schema = Data.load_schema !schemafile in

  
  vlogf "learning the spac structure .... \n";
  let circ = learn_spac_structure data schema in

  let scratch = Circuit.create_scratch circ in
  max_depth := 0;

  if !output_dir <> "" then begin
    let out_ch = open_out !output_dir in
    Circuit.output out_ch circ;
    close_out out_ch 
  end;


  vlogf "Calculating data llg\n"; flush_all(); 
  let logprobs = Array.map (fun x-> Circuit.logprob_x scratch circ x) data in

  let llg_data = Array.sumf logprobs in
  let n = float_of_int ( Array.length data) in
  let avg_data = llg_data /. n in

  
  vlogf "Calculating valid llg\n"; flush_all();  
  
  Timer.stop "spnlearn";

  
  nlogf "Time: %f\n" (Timer.elapsed "spnlearn");

  nlogf "Average data likelihood: %f\n" avg_data;
  vlogf "Time spent on clustering: %f\n" (Timer.elapsed "clustering_time");
  vlogf "Time spent on merging data parts: %f\n" (Timer.elapsed "data_parts_merge");
  let filelist = Sys.readdir "." in
  Array.iter ( fun f -> if endswith f ".cluster" || endswith f ".data" || endswith f ".schema" || endswith f ".log" then Sys.remove f   ) filelist; 
  ignore()


let extend() = ignore()


let _=
   main()

  
