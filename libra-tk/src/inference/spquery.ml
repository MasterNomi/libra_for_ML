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
open Printf
open Circuit
open Str
open Ext
open Spn

let qfile = ref ""
let evfile = ref ""
let network = ref ""
let schemafile = ref ""
let modelfile = ref ""
let outfile = ref ""
let usage = "Usage: spquery -i <query> -m <model_directory> [...]"
let args = Arg.align
  ([("-m",  Arg.Set_string modelfile,  " Input SPN (SPAC) model directory"); 
    ("-ev",  Arg.Set_string evfile,  " Evidence file");
    ("-q",  Arg.Set_string qfile,  " Query file")]
(* This option allows just the inference results to be redirected to a
   file, separately from all other output.  This isn't standard
   throughout Libra, so I'm disabling it for now.  Haven't entirely
   decided if we should make this option standard for inference
   algorithms or not.
    ("-o",  Arg.Set_string outfile,  " Inference esult file"); *)
    @ Ext.common_arguments)



let comps = ref [||] 
let logpTemp = ref [||]
let logeTemp = ref [||]
let feature_lists = ref [||]
let scratches = ref [||] 
let logzs = ref [||] 
(*let data_parts = ref [||]
*)

exception CircuitLoadException


let load_circuit filename =
  try Circuit.load_with_features (open_in filename) 
  with Circuit.Unknown_child(n,c) -> printf "Error: node %s references unknown child node %s.\n" n c; raise CircuitLoadException

let load_comps () =
  (*printf "load comps..%d\n"(Array.length !node_array) ; flush_all();*)
  feature_lists := [||];
  comps := [||];
  for i=1 to Spn.get_spn_size() - 1  do
  (*  printf "node id:%d type:%s\n" !node_array.(i).id (match !node_array.(i).nodetype with ACNode->"ac" | PlusNode-> "+"| TimesNode ->"*");
  *)
    let node = Spn.get_node_by_id i in
    if (node.nodetype == LeafNode) then (* || nodetype == PlusNode || nodetype == TimesNode) then *)
    begin
      let comp_name = node.acname in
      let full_comp_name = String.concat dir_sep [!modelfile; comp_name] in
      let (component, fl) = load_circuit full_comp_name in
      let n = Array.length !comps in 
      comps := Array.append !comps [|component|];
      feature_lists := Array.append !feature_lists [|fl|];
      node.ac_index <- n;
    end
  done;
  ignore()



let answer_acnode_cached node_index i  =
  let ac_index = (Spn.get_node_by_id node_index).ac_index in
  !logpTemp.(ac_index).(i)


let answer_acnode_cached_ev node_index i  =
 
  let ac_index = (Spn.get_node_by_id node_index).ac_index in
  let p = !logeTemp.(ac_index).(i) in
  p

let answer_acnode_query node_index q = 
  let node = Spn.get_node_by_id node_index in
  let ac_index = node.ac_index in
  let circ = !comps.(ac_index) in
  let scratch = !scratches.(ac_index) in 
  let logz = !logzs.(ac_index) in
  let schema_indexes = Array.map (snd) (node.schema ) in
  let c_q = Array.take q schema_indexes in
  let logp = Circuit.logprob_x scratch circ c_q in
  let final = logp -. logz in
  final



let normalize a =
  let sum = Array.sumf (Array.map (exp) a) in 
  let n_a = Array.map (fun x->log ((exp x) /. sum) ) a in
  n_a



let rec answer_spac_qurey node q =  
  let children = node.children in
  let n = Array.length children in 
  if node.nodetype == PlusNode then
  begin
    (*let n_params = Array.normalizelog node.params in*)
    let n = Array.length children in 
    let logprobs = Array.make n 0.0 in
    for i=0 to n - 1 do
      logprobs.(i) <- answer_spac_qurey children.(i) q
    done;
    let final_probs = Array.map2 (fun logp logw-> logp +. logw ) logprobs node.params in 

    (*let total = float_of_int(Array.sum node.counts) in*)
    let final_result = (Ext.alogsumexp final_probs) in 
    final_result
  end
  else if node.nodetype == TimesNode then
  begin
    let logprobs = Array.make n 0.0 in
    for i=0 to (Array.length children) - 1 do
      logprobs.(i) <- answer_spac_qurey children.(i) q
    done; 
    (Array.sumf logprobs) 
  end 
  else if node.nodetype == LeafNode then
    answer_acnode_query node.id q 

  else raise NodeTypeError  

let rec answer_spac_cached_ev node q_i =
  (*printf "node.id %d\n" node.id;*)
  let children = node.children in
  let n = Array.length children in 
  if node.nodetype == PlusNode then
  begin
    let n = Array.length children in 
    let logprobs = Array.make n 0.0 in
    for i=0 to n - 1 do
      logprobs.(i) <- answer_spac_cached_ev children.(i) q_i
    done;
    let final_probs = Array.map2 (fun logp logw-> logp +. logw ) logprobs node.params in 
    (*let total = float_of_int(Array.sum node.counts) in*)
    let final_result = (Ext.alogsumexp final_probs) in
    final_result
  end
  else if node.nodetype == TimesNode then
  begin
    let logprobs = Array.make n 0.0 in
    for i=0 to (Array.length children) - 1 do
      logprobs.(i) <- answer_spac_cached_ev children.(i) q_i
    done; 
    let final_result = (Array.sumf logprobs) in
    final_result
  end 
  else if node.nodetype == LeafNode then
    answer_acnode_cached_ev node.id q_i

  else raise NodeTypeError  


let rec answer_spac_cached node q_i = 
  let children = node.children in
  let n = Array.length children in 
  (*printf "id: %d answer query: %d\n" node.id q_i;
  flush_all();*)
  if node.nodetype == PlusNode then
  begin
    (*let n_params = Array.normalizelog node.params in*)
    let n = Array.length children in 
    let logprobs = Array.make n 0.0 in
    for i=0 to n - 1 do
      logprobs.(i) <- answer_spac_cached children.(i) q_i
    done;
    let final_probs = Array.map2 (fun logp logw-> logp +. logw ) logprobs node.params in 
    (*let total = float_of_int(Array.sum node.counts) in*)
    let final_result = (Ext.alogsumexp final_probs) in

    (*printf "node: %d p = %f\n" node.id (exp final_result);
  *)
  final_result
  end
  else if node.nodetype == TimesNode then
  begin
    let logprobs = Array.make n 0.0 in
    for i=0 to (Array.length children) - 1 do
      logprobs.(i) <- answer_spac_cached children.(i) q_i
    done; 
    let final_result = (Array.sumf logprobs) in
       (*printf "node: %d p = %f\n" node.id (exp final_result);
       *)
     final_result
  end 
  else if node.nodetype == LeafNode then begin
  let final_result = answer_acnode_cached node.id q_i in
    
    (*printf "node: %d q_i: %d p = %f\n" node.id q_i (exp final_result);
  *)
  final_result
  end
  else raise NodeTypeError  




let answer_query root q =
  let logp = answer_spac_qurey root q in
  logp


let answer_query_cached root q_i =
  let logp = answer_spac_cached root q_i in
  logp

let answer_query_cached_ev root q_i =
  let logp = answer_spac_cached_ev root q_i in
  logp

exception InsufficientArguments

let answer_joint_queries circ scratch logz queries evidence =
  let n = Array.length queries in
 
  let logps = Array.make n 0.0 in
  for k=0 to n - 1 do
      (* By definition of conditional probability:
           log P(q|e) = log P(q) - log P(e) *)
      let q = queries.(k) in

    (* Input validation *)
      Data.check_point circ.Circuit.schema q;

      let e = evidence.(k) in
      (* Check for compatibility between evidence and query. *)
      if Array.length e > 0 then
        Data.check_point circ.Circuit.schema e;
      let mismatch = ref false in
      for i = 0 to Array.length e - 1 do
        if e.(i) >= 0 then begin
          if q.(i) < 0 then 
            q.(i) <- e.(i)
          else if q.(i) != e.(i) then
            mismatch := true
        end
      done;

      (* Compute probability of query and evidence *)
      let logp_q = if !mismatch then log 0. 
                   else Circuit.logprob_x scratch circ q in
      let logp_ev = Circuit.logprob_x scratch circ e in

      let l = logp_q -. logp_ev in
      logps.(k) <- l 
  done;
  logps

let preload_query_results queries = 
  let n = Array.length queries in
  logpTemp := Array.make (Array.length !comps) [||];  
  for i = 0 to (Array.length !node_array) - 1 do
    let node = !node_array.(i) in 
    if node.nodetype == LeafNode (* || node.nodetype == PlusNode || node.nodetype == TimesNode*) then 
    begin
      let result = Array.make n 0.0 in
      let indeces = Array.map (snd) node.schema in
      let myqueries = Array.takeCol queries indeces in
  
      let ac_index = node.ac_index in
      let circ = !comps.(ac_index) in
      let scratch = !scratches.(ac_index) in 
      let logz = !logzs.(ac_index) in

      for k=0 to n - 1 do
        let logp = Circuit.logprob_x scratch circ myqueries.(k) in
        result.(k) <- (logp -. logz) 
      done;
      !logpTemp.(ac_index) <- result
    end
  done


let preload_ev_results queries = 
  let n = Array.length queries in
  logeTemp := Array.make (Array.length !comps) [||];  
  for i = 0 to (Array.length !node_array) - 1 do
    let node = !node_array.(i) in 
    if node.nodetype == LeafNode (* || node.nodetype == PlusNode || node.nodetype == TimesNode*) then 
    begin
      let result = Array.make n 0.0 in
      let indeces = Array.map (snd) node.schema in
      let myqueries = Array.takeCol queries indeces in
  
      let ac_index = node.ac_index in
      let circ = !comps.(ac_index) in
      let scratch = !scratches.(ac_index) in 
      let logz = !logzs.(ac_index) in

      for k=0 to n - 1 do
        let logp = Circuit.logprob_x scratch circ myqueries.(k) in
        result.(k) <- (logp -. logz)  
      done;
      !logeTemp.(ac_index) <- result
    end
  done

let preload_with_ev queries evidence = 
  logpTemp := Array.make (Array.length !comps) [||];  
  for i = 0 to (Array.length !node_array) - 1 do
    let node = !node_array.(i) in 
    if node.nodetype == LeafNode (* || node.nodetype == PlusNode || node.nodetype == TimesNode*) then 
    begin
      let indeces = Array.map (snd) node.schema in
      let myqueries = Array.takeCol queries indeces in
      let myevidence = Array.takeCol evidence indeces in
  
      let ac_index = node.ac_index in
      let circ = !comps.(ac_index) in
      let scratch = !scratches.(ac_index) in 
      let logz = !logzs.(ac_index) in
  
      let result = answer_joint_queries circ scratch logz  myqueries myevidence in
      Array.iter (printf "%f\n") result;
      !logpTemp.(ac_index) <- result
    end
  done
(* main main *) 


let endswith s1 s2 =
  let re = Str.regexp (Str.quote s2 ^ "$") in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false

let _ =
  Timer.start "total";
  Arg.parse args ignore usage;
  if  !qfile = "" || !modelfile = "" then (Arg.usage args usage; exit (-1));
  Ext.common_log_init ();
  
  if endswith !modelfile dir_sep then
    modelfile := String.sub !modelfile 0 ((String.length !modelfile) - 1);

  if not (check_suffix !modelfile ".spn") then begin printf "Only accepts .spn as the input model file"; exit 1 end;
  

  let test = Data.load_data_ar !qfile in
  (*let schema = Data.load_schema !schemafile in*)
  let t1 = Sys.time() in
  ignore(t1);
  (*printf "loading the model .... \n"; *)
  Spn.load_model (String.concat dir_sep [!modelfile; "spac.m"]);  
 
  load_comps();
  
  
  scratches := Array.map (Circuit.create_scratch) !comps;
  logzs := Array.map2 (fun s c->Circuit.compute_z s c ) !scratches !comps;
  
  (*printf "preload test data into ACs..\n";
  flush_all();
  *)
  let root = Spn.get_node_by_id 0 in 
  let finallogprobs = ref [||] in
  if !evfile <> "" then begin
    let ev = Data.load_data_ar !evfile in
    (*preload_with_ev test ev; *)

    preload_ev_results ev;
    preload_query_results test;
    
    (*printf "Answering test queries..\n"; *)
    
    let testlogprobs = Array.mapi (fun i x-> answer_query_cached root i) test in
    let evlogprobs = Array.mapi (fun i x-> answer_query_cached_ev root i) ev in
    finallogprobs := Array.map2 (fun q e -> q-.e) testlogprobs evlogprobs;
  end else begin
    preload_query_results test;
    (*printf "Answering test queries..\n"; *)
    finallogprobs := Array.mapi (fun i x-> answer_query_cached root i) test
  end;
  let llgstats = Ext.stats_make() in
  Array.iter (fun x -> Ext.stats_add llgstats x) !finallogprobs;
  

  (*print_spn root nt; *)

  (*printf "result avg llg: %.7f\n" avg_test;*) 

  
  if stats_n llgstats > 0. then begin
    if !outfile <> "" then begin
      let inference_channel = open_out !outfile in
      Array.iter (fprintf inference_channel "%.7f\n" ) !finallogprobs;
      fprintf inference_channel "avg = %f +/- %f\n" (stats_mean llgstats) (stats_stderr llgstats);
      nlogf "avg = %f +/- %f\n" (stats_mean llgstats) (stats_stderr llgstats);
      close_out_noerr inference_channel
    end
    else begin
      Array.iter (nlogf "%.7f\n" ) !finallogprobs;
      nlogf "avg = %f +/- %f\n" (stats_mean llgstats) (stats_stderr llgstats);
    end;
    vlogf "Total time: %fs\n" (Timer.elapsed "total")
    
  end


