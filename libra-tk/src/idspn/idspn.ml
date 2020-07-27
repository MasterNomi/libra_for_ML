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
open Unix
open Circuit
open Random
open Spn

exception CircuitLoadException
exception NodeTypeError

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
let ratio = ref 0.0000001
let cut_thresh = ref 0.001
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

let usage = "Usage: idspn -i <data> -o <output> [...]"
let args = Arg.align
  ([

    ("-i", Arg.Set_string datafile, " Training data file");
    ("-s",  Arg.Set_string schemafile,  " Data schema (optional)");
    ("-o", Arg.Set_string output_dir, " Output SPN directory");
    ("-l1",  Arg.Set_float l1param,  " Weight of L1 norm [5.0]");
    ("-l",  Arg.Set_float lambda,  " EM clustering penalty [0.2]");
    ("-ps",  Arg.Set_float split_penalty,  " Per-split penalty [10.0]");
    ("-k",  Arg.Set_int num_part,  " Max sum nodes' cardinalities [5]");
    ("-sd",  Arg.Set_float stddev,  " Standard deviation of Gaussian weight prior [1.0]");
    ("-cp",  Arg.Set_int concurrent_thr,  " Number of concurrent processes [4]");
    ("-vth",  Arg.Set_float cut_thresh,  " Vertical cut thresh [0.001]");
    ("-ext", Arg.Set_int max_extension, " Maximum number of node extensions [5]");  
    ("-minl1", Arg.Set_float minl1, " Minimum value for components' L1 priors [1.0]");
    ("-minedge", Arg.Set_float minedge, " Minimum edge budget for components [200000]");
    ("-minps", Arg.Set_float minps, " Minimum split penalty for components [2.0]");
    ("-seed", Arg.Set_int seed, " Random seed");
    ("-f",  Arg.Set override,  " Force to override output SPN directory")
(* options used for research code *)
(*  ("-q",  Arg.Set_string qfile,  " Query file (optional)"); 
    ("-va", Arg.Set_string vfile, " Validation file(optional)"); 
    ("-om",  Arg.Set_string outputmodel,  " Output model file");
    ("-of",  Arg.Set_string outputinference,  " Output inference file"); *)
  ] @ common_arguments)
  


(********  SPN data structure and functions for manupulating the data structre *********)
(* Move to spn *)
(*
type ndetails = TimesNode
        | PlusNode
        | NullNode 
  | ACNode;;

type spnode = {
  mutable id: int;
  mutable parents: spnode array;
  mutable children: spnode array;
  mutable params: float array;
  mutable nodetype: ndetails;
  mutable schema: (int * int) array;
  mutable data: int array array;
  mutable acname: string;
  mutable ac_index: int ;
  mutable counts: int array;
  mutable activated_link: int;
  mutable thresh : float;
  mutable final : bool;
  mutable infcached: bool;
  mutable logpTemp : float array;
}
*)



(*let root = ref spnode
*)
(* Move to spn *)
(*
let node_array = ref [||]
*)

let comps = ref [||] 
let feature_lists = ref [||]
let scratches = ref [||] 
let logzs = ref [||]
 
(*let data_parts = ref [||]
*)
let get_next_id () = 
  Array.length !node_array


(* Move to spn *)
(*
let create_node t pt c pm d s=
  let n = {id = get_next_id(); 
  parents = pt;
  children = c; 
  params = pm;
  nodetype = t;
  schema = s; 
  data = d;
  acname = "";
  ac_index = -1;
  counts = [||];
  activated_link = 0;
  thresh = 0.0;
  final = false;
  infcached = false;
  logpTemp = [||] } in
  node_array := Array.append !node_array [|n|];
  n

let create_acnode = create_node ACNode [||] [||] [||]  
 
let create_plus = create_node PlusNode [||] [||] [||] 

let create_times = create_node TimesNode [||] [||] [||] 

*)

let len a = Array.length a 

let node_as_array n = [|n|]

let node_as_list n = [n]

(* Move to spn *)
(*
let get_type_name nodetype = 
  if nodetype == PlusNode then "+" else if nodetype == TimesNode then "*" else if nodetype == ACNode then "ac" else "null"


let add_child node child = 
  node.children <- Array.append node.children  [|child|];
  node.params <- Array.append node.params [|log(float_of_int(Array.length child.data)) -. log( float_of_int(Array.length node.data))|];
  node.counts <- Array.append node.counts [|(Array.length child.data)|];
  ignore()

let set_parent node parent =
  add_child parent node;
  node.parents <- Array.append node.parents [|parent|];
  ignore()

*)
(*************************************************************)

(** function that prints the information of a single SPN node into the output file **)
(* Move to spn *)
(*
let print_node out node =
  let n = (Array.length node.children) in
  let plen = Array.length node.parents in
  let parentid = if plen > 0 then node.parents.(0).id else -1 in
  if false then ignore() (*node.id > 0 && node.parents.(0).nodetype == ACNode then ignore()*)
  else begin  
    fprintf out "n %d %s %d \n" node.id  (match node.nodetype with ACNode-> "ac" | TimesNode-> "*" | PlusNode-> "+" | NullNode ->  "null") parentid;
    match node.nodetype with ACNode->
        begin
        let indeces = Array.map (snd) node.schema in
        let st_li = Array.to_list (Array.map (string_of_int) indeces) in
        let line = String.concat " " st_li in
        fprintf out "%s\n" line
        
        end;
      | TimesNode ->
        begin
        fprintf out "%f\n" node.thresh;
        for i=0 to n-1 do
          fprintf out "%d " node.children.(i).id
        done;
        fprintf out "\n"
        end;
        let indeces = Array.map (snd) node.schema in
        let st_li = Array.to_list (Array.map (string_of_int) indeces) in
        let line = String.concat " " st_li in
        fprintf out "%s\n" line
      | PlusNode ->
        begin

        for i=0 to n-1 do
          fprintf out "%d " node.children.(i).id
        done;
        fprintf out "\n";
        for i=0 to n-1 do
          fprintf out "%f " node.params.(i)
        done;
        fprintf out "\n"
        end;
        
        let indeces = Array.map (snd) node.schema in
        let st_li = Array.to_list (Array.map (string_of_int) indeces) in
        let line = String.concat " " st_li in
        fprintf out "%s\n" line
      | _ -> ignore()
  end

(** prints the SPN structure into the output file **)
let print_model out root =
  fprintf out "%s\n" !network;
  fprintf out "%d\n" (Array.length !node_array);
  Array.iter (print_node out) !node_array

*)

(*** loads an arithmetic circuit from the given file ***)
let load_circuit filename =
  try Circuit.load_with_features (open_in filename) 
  with Circuit.Unknown_child(n,c) -> nlogf "Error: node %s references unknown child node %s.\n" n c; raise CircuitLoadException



(****** Learns a sum node using sample clustering ***********)
(* Move to spn *)
(*
let h_split node num_part =
  let v = len node.data.(0) in
  dlogf "h-split node:%d varsize:%d \n" node.id v;
  (*let m = create_adj_matrix node.data sch 0.000001 in 
  ignore(m);*)
  let finish = ref false in
  let it = ref num_part in
  let data_parts = ref [||] in
  let totalVar = float_of_int (Array.length !node_array.(0).schema) in
  let nodeVar = float_of_int ( Array.length node.schema ) in
  let varRatio = nodeVar /. totalVar in
  while !it >= 2 && (not !finish) do
    data_parts := Partition.horizontal_cluster node.id node.data [||] node.schema !it (!lambda *. varRatio) !concurrent_thr;  
  let sizes = Array.map (fun d->let l = Array.length d in nlogf "%d " l; l ) !data_parts in
    nlogf "\n";
    let total = Array.sum sizes in
    let ratios = Array.map (fun s-> float_of_int (s) /. float_of_int(total)) sizes in
    let ok = ref true in
    Array.iter ( fun r -> if (r < !ratio ) then ok := false ) ratios;
    if !ok then 
    begin
      finish := true;
    end else 
    begin 
      dlogf "Partitioning with %d parts unsuccessful\n" !it;
      it := !it - 1;
    end
  done;

  if !it < 2 then
    raise HSplitNotPossible;

  let nodeList = ref [] in
  for i = 0 to (Array.length !data_parts) - 1 do
    let node_i = create_acnode !data_parts.(i) node.schema in
    set_parent node_i node;
    nodeList := node_i::!nodeList
  done;
  node.nodetype <- PlusNode; 
  !nodeList

(******* Learns a product node using variable clustering ************)
let v_split node cut_thr=
  let v = len node.data.(0) in
  dlogf "v-split node:%d varsize:%d\n" node.id v;
  
  (*let l = len node.data in*)
  let nodeList = ref [] in
  try
    let one_var_cc = ref [] in
    let ccs = Partition.find_ccs node.data node.schema cut_thr in
    let ccs_size = Array.length ccs in
    for i = 0 to ccs_size - 1 do 
      let nodesnum_i = Array.of_list ccs.(i) in
      let nodesnum_i_size = Array.length nodesnum_i in
      if nodesnum_i_size > 1 then 
      (*nlogf "nodesnum_i_size = %d\n" nodesnum_i_size;
      flush_all();*)
      begin
        let data_i = Array.takeCol node.data nodesnum_i in
        let schema_i = Array.take node.schema nodesnum_i in
        let node_i = create_acnode data_i schema_i in
        set_parent node_i node;
        nodeList := node_i::!nodeList
      end
      else begin
        one_var_cc := nodesnum_i.(0)::!one_var_cc
      end
    done;
    let s = List.length !one_var_cc in
    let len = List.length !nodeList in
    if len > 0 && s > 0 then begin
        let one_var_cc_ar = Array.of_list !one_var_cc in
        let data_i = Array.takeCol node.data one_var_cc_ar in
        let schema_i = Array.take node.schema one_var_cc_ar in
        let node_i = create_acnode data_i schema_i in
        node_i.final <- true;
        set_parent node_i node;
        nodeList := node_i::!nodeList;

        node.nodetype <- TimesNode;
        node.thresh <- cut_thr;
        !nodeList 
    end else if s > 0 then 
    begin
      node.final <- true;
      raise VSplitNotPossible
    end
    else begin
      node.nodetype <- TimesNode;
      node.thresh <- cut_thr;
      !nodeList 
    end
  with CutNotPossible-> raise VSplitNotPossible
*)

(* Used PIC clustering *)
(*
let v_split node cut_thr=
  nlogf "v-split node:%d\n" node.id;
  
  (*let l = len node.data in
  let v = len node.data.(0) in *)
  try
    let (nodesnum1, nodesnum2, bestCut) = Partition.vertical_partition node.data node.schema cut_thr in
    let l1 = len nodesnum1 in
    let l2 = len nodesnum2 in
    nlogf "l1: %d l2: %d\n" l1 l2;
    flush_all();
    if (l1 > 0 && l2 > 0 ) then begin
      let data1 = Array.takeCol node.data nodesnum1 in
      let schema1 = Array.take node.schema nodesnum1 in
      let node1 = create_acnode data1 schema1 in
      set_parent node1 node;
      let data2 = Array.takeCol node.data nodesnum2 in
    
      let schema2 = Array.take node.schema nodesnum2 in

      let node2 = create_acnode data2 schema2 in
      set_parent node2 node;
  
      node.nodetype <- TimesNode;
      let nodeList = [node2; node1] in
      node.thresh <- bestCut;
      nodeList

    end else 
      raise VSplitNotPossible 
  with CutNotPossible-> raise VSplitNotPossible
*)




(***** Learns an AC node using ACMN algorithm *********)
let learn_acnode network comp_id data schema var_ratio=
  flush_all();
  let pid = Unix.fork() in
    match pid with
    0 -> begin
      let data_file = Printf.sprintf "%s-%d.data" network comp_id in
      let schema_file = Printf.sprintf "%s-%d.schema" network comp_id in
      let output = Printf.sprintf "%s-%d.ac" network comp_id  in
      let logfile = Printf.sprintf "%s-%d.log" network comp_id in
      Data.dump_data data data_file;
      let sch = Array.map (fst) schema in 
      Data.dump_schema sch schema_file;
      let ps = !split_penalty *. var_ratio in
      let me = (float_of_int !maxe) *.var_ratio in
      let l1 = !l1param *. var_ratio in
      let cmd = Printf.sprintf "libra acmn -s %s -shrink -o %s -i %s -l1 %f -sd %f -ps %f -maxe %d -log %s" schema_file output data_file (if l1 < !minl1 then !minl1 else l1) !stddev (if ps < !minps then !minps else ps) (int_of_float (if me < !minedge then !minedge else me)) logfile in 
      let t = Unix.localtime (Unix.gettimeofday() ) in 

      vlogf "%s time: %d:%d:%d\n" cmd t.tm_hour t.tm_min t.tm_sec;
      
      flush_all();
      ignore(Unix.system cmd);

      let t2 = Unix.localtime (Unix.gettimeofday() ) in 
      vlogf "finish %s time: %d:%d:%d pid %d\n" cmd t2.tm_hour t2.tm_min t2.tm_sec (Unix.getpid());
      flush_all();
    
      exit 1
    end 
    | _ -> begin
      ignore();
    end;
  pid


let answer_acnode_cached node_index i  =
  (*let ac_index = !node_array.(node_index).ac_index in
  !logpTemp.(ac_index).(i)*)
  !node_array.(node_index).logpTemp.(i)


let rec answer_spac_cached node q_i =
  (*nlogf "answer node:%d q_i:%d\n" node.id q_i;*)
  flush_all();  
  let children = node.children in
  let n = Array.length children in 
  if node.nodetype == PlusNode  then
  begin
    (*let n_params = Array.normalizelog node.params in*)
    let n = Array.length children in 
    let logprobs = Array.make n 0.0 in
    for i=0 to n - 1 do
      logprobs.(i) <- answer_spac_cached children.(i) q_i
    done;
    let final_probs = Array.map2 (fun logp logw-> logp +. logw ) logprobs node.params in 
    (*let total = float_of_int(Array.sum node.counts) in*)
    (Ext.alogsumexp final_probs) 
  end
  else if node.nodetype == TimesNode then
  begin
    let logprobs = Array.make n 0.0 in
    for i=0 to (Array.length children) - 1 do
      logprobs.(i) <- answer_spac_cached children.(i) q_i
    done; 
    (Array.sumf logprobs) 
  end 
  else if node.nodetype == LeafNode then
    answer_acnode_cached node.id q_i
  else begin nlogf "Ooops, you are not welcome here at node:%d\n" node.id; flush_all(); raise NodeTypeError end  


let answer_acnode_query node_index q = 
  let ac_index = !node_array.(node_index).ac_index in
  let circ = !comps.(ac_index) in
  let scratch = !scratches.(ac_index) in 
  let logz = !logzs.(ac_index) in
  let schema_indexes = Array.map (snd) (!node_array.(node_index).schema ) in
  let c_q = Array.take q schema_indexes in
  let logp = Circuit.logprob_x scratch circ c_q in
  let final = logp -. logz in
  final

(* Move to ext *)
(*
let rem_first l = 
  match l with            
    | [] -> []
  | h::t -> t 

let rem_item l item = 
  let newl = ref [] in
  let la = Array.of_list l in
  for i = (List.length l) - 1 downto 0 do 
    if la.(i) <> item then newl := la.(i)::!newl
  done;
  !newl
*)
(*
let preload node data =
  nlogf "preloading node: %d with index:%d\n" node.id node.ac_index; 
  let n = Array.length data in
  let result = Array.make n 0.0 in
  
  let ac_index = node.ac_index in
  let circ = !comps.(ac_index) in
  let scratch = !scratches.(ac_index) in 
  let logz = !logzs.(ac_index) in

  for k=0 to n - 1 do
    let logp = Circuit.logprob_x scratch circ data.(k) in
      result.(k) <- (logp -. logz) 
  done;
  logpTemp := Array.append !logpTemp [|result|]
*)

let learn_acs nodeList =
  dlogf "Learn acs for "; List.iter(fun n-> vlogf "%d " n.id) nodeList;
  dlogf "\n";
  let child_list = ref [] in
  (*let now = int_of_float (Unix.gettimeofday()) in*)
  let allvar = float_of_int (Array.length !node_array.(0).schema) in
  (*Array.iter (fun n-> if n.nodetype == ACNode then
      let nodevar = float_of_int (Array.length n.schema) in
      let var_ratio = nodevar /. allvar in
      let pid = learn_acnode !network now n.id n.data n.schema var_ratio in
      let output = Printf.sprintf "%s-%d-%d.ac" !network now n.id  in
      n.acname<-output; child_list := pid::!child_list ) !node_array;
  *)
  (*let nodenum = Array.length !node_array in
  for i=1 to nodenum - 1 do
    let n = !node_array.(i) in *)
  let learn n = 
    if n.nodetype == LeafNode || n.nodetype == PlusNode || n.nodetype == TimesNode then
    begin
      let nconcurrents = ref (List.length !child_list) in
      while !nconcurrents >= !concurrent_thr do
        let (pid, status) = Unix.wait() in
        child_list := List.rem_item !child_list pid;
        nconcurrents := !nconcurrents - 1
        
      done;
      let nodevar = float_of_int (Array.length n.schema) in
      let var_ratio = nodevar /. allvar in
      let data_ratio = float_of_int (Array.length n.data) /. float_of_int (Array.length !node_array.(0).data ) in
      let pid = learn_acnode !network n.id n.data n.schema (var_ratio *. data_ratio) in
      let output = sprintf "%s-%d.ac" !network n.id  in
      n.acname<-output; 
      child_list := pid::!child_list 
    end in
  List.iter (learn) nodeList;
  
  (*
  (* Thread implementation *)
  let ids = ref [] in 
  Array.iter (fun n-> if n.nodetype == ACNode then 
    let tid = Thread.create learn_acnode_thread (!network, now, n.id, n.data, n.schema) in
    let output = Printf.sprintf "%s-%d-%d.ac" !network now n.id  in
      n.acname<-output; ids:=tid::!ids) !node_array;
  
  List.iter (Thread.join) !ids;
  *)

  while (List.length !child_list) > 0 do
    let p = List.hd !child_list in 
    child_list := List.rem_first !child_list;
    ignore (Unix.waitpid [] p) 
  done; 


   (* let (pid, status) = Unit.wait();
    child_list := rem_item !child_list pid;
  done; *)

  vlogf "end of learning %d acs -----------------\n" (List.length nodeList);
  flush_all();
  ignore()

let node_lt n1 n2 = (Array.length n1.data ) > (Array.length n2.data)  

  (*nlogf "logpTemp size: %d\n" (Array.length !logpTemp);*)
(*
let load_node_comp node data=
  nlogf "load node comp %d\n" node.id;
  let comp_name = node.acname in
  let (component, fl) = load_circuit comp_name in
  let n = Array.length !comps in 
  comps := Array.append !comps [|component|];
  feature_lists := Array.append !feature_lists [|fl|];
  !node_array.(node.id).ac_index <- n;
  let scratch = Circuit.create_scratch component in
  let logz = Circuit.compute_z scratch component in
  logzs := Array.append !logzs [|logz|];
  scratches := Array.append !scratches [|scratch|];
  
  le datasize = Array.length data in
  let result = Array.make datasize 0.0 in
  
  for k=0 to datasize - 1 do
      let logp = Circuit.logprob_x scratch component data.(k) in
        result.(k) <- (logp -. logz) 
  done;
  logpTemp := Array.append !logpTemp [|result|]
*)


(** Add nodes to heap **)
let add_to_heap node_heap node = 
  let varnum = Array.length node.schema in
  let samplesize = Array.length node.data in 
  if varnum > 10 && (not node.final) && samplesize > 50 && node.nodetype == LeafNode then Heap.add node_heap node else ignore()

(** NOT USED **)
let append_string path s =
    let chan = open_out_gen [Open_wronly; Open_creat] 0o666 path
    in let len = out_channel_length chan
    in
        begin
        seek_out chan len;
        output_string chan s;
        close_out chan;
        end

(** The heap comparing function **)
let compare n1 n2 =
  let l1 = Array.length n1.data.(0) in
  let l2 = Array.length n2.data.(0) in
  if l1 > l2 then -1 else if l2 > l1 then 1 
  else begin
    let sl1 = Array.length n1.data in
    let sl2 = Array.length n2.data in
    if sl1 > sl2 then -1 else if sl2 > sl1 then 1 else 0
  end


(** Cache the results of queries for every single AC node. This speedup the process of computing loglikelihood over the whole SPN structure **)
let preload_all_nodes queries =
  vlogf "preload all nodes\n"; flush_all();
  let n = Array.length queries in
  (*logpTemp := Array.make (Array.length !comps) [||]; *) 
  for i = 1 to (Array.length !node_array) - 1 do
    if !node_array.(i).nodetype == LeafNode then begin    
      let node = !node_array.(i) in 
      let result = Array.make n 0.0 in
      let indeces = Array.map (snd) node.schema in
      (*nlogf "node:%d indeces: " node.id;
      Array.iter (nlogf "%d ") indeces;
      nlogf "\n";*)
      let myqueries = Array.takeCol queries indeces in
  
      let ac_index = node.ac_index  in
      let circ = !comps.(ac_index) in
      let scratch = !scratches.(ac_index) in 
      let logz = !logzs.(ac_index) in
      if not node.infcached then begin
        for k=0 to n - 1 do
          let logp = Circuit.logprob_x scratch circ myqueries.(k) in
          result.(k) <- (logp -. logz);
        done;
        node.logpTemp <- result;
        node.infcached <- true;
      end
    end
  done;

  vlogf "preload all nodes.end\n"; flush_all()


(** Load the corresponding AC of all AC nodes, used for computing the train or test data loglikelihood **)  
let load_all_comps () =
  feature_lists := [||];
  comps := [||];
  scratches := [||];
  logzs := [||];
  for i=1 to (Array.length !node_array) - 1  do
    if !node_array.(i).nodetype == LeafNode then begin 
      let comp_name = !node_array.(i).acname in
      let (component, fl) = load_circuit comp_name in
      let n = Array.length !comps in 
      comps := Array.append !comps [|component|];
      feature_lists := Array.append !feature_lists [|fl|];
      !node_array.(i).ac_index <- n;
    end;
  done;
  scratches := Array.map (Circuit.create_scratch) !comps;
  logzs := Array.map2 (fun s c->Circuit.compute_z s c ) !scratches !comps;
  ignore()

let clear_all_caches () = 
  vlogf "clear caches\n"; flush_all();
  for i=0 to (Array.length !node_array) - 1 do
    !node_array.(i).infcached <- false;
    !node_array.(i).logpTemp <- [||]
  done


(** The main part in the learning SPN structure is extends_spn, which extends an AC node with SPN subtree **)
let extends_spn data node cut_thr depth =
  vlogf "Extending node:%d\n" node.id;
  incr extension_num; 
  let newNodeList = ref [] in
  let doHSplit vnode =
    if not vnode.final then 
    begin
      try
        let hNodeList = h_split vnode !num_part !lambda !concurrent_thr !ratio in
        newNodeList := List.append !newNodeList hNodeList;
      with Spn.HSplitNotPossible -> ignore() 
    end 
    else if node.id != vnode.id then begin
      newNodeList := List.append !newNodeList [vnode]
    end in  
  begin
  try 
    let vNodeList = v_split  node cut_thr in
    List.iter (doHSplit) vNodeList;

    newNodeList := List.append !newNodeList vNodeList;
  
  with Spn.VSplitNotPossible -> begin vlogf "V-split not possible\n"; 
    doHSplit node end
  end;
  ignore();
  let len  = List.length !newNodeList in
  if len > 1 then
  begin
    let sortedNodeList = List.sort compare !newNodeList in
    learn_acs sortedNodeList;
      
    let root = !node_array.(0) in

    (** some useful debuging statements **)
    (*nlogf "loading new nodes\n"; flush_all();  *)
    (*List.iter (fun n->load_node_comp n data) sortedNodeList;*)
    (*nlogf "Size: %d %d %d\n" (Array.length !logpTemp)(Array.length !comps) (Array.length !node_array); 
    nlogf "after loading new nodes\n"; flush_all();  *)
    (*Array.iter (fun n->nlogf "node id:%d type:%s acindex:%d acname:%s \n" n.id (get_type_name n.nodetype) n.ac_index n.acname; Array.iter (fun s->nlogf "%d " (snd s)) n.schema; nlogf "\n"; ) !node_array;*)
    
    let nf = float_of_int (Array.length data) in
    
    
    (** some useful debuging statements **)
    (*let subtree_llg = Array.mapi (fun i d->answer_spac_cached root i) data in
    nlogf "Subtree llg: %f\n" ((Array.sumf subtree_llg) /. nf);
    
    logpTemp := [||];
    
    for i=1  to ((Array.length !node_array)-1 ) do

    nlogf "here %d\n" i; flush_all();  
    let comp_name = node.acname in
      let (component, fl) = load_circuit comp_name in
      !comps.(!node_array.(i).ac_index) <- component
    done;*)
    
    let oldtype = node.nodetype in  
    node.nodetype <- LeafNode;
    let start = Unix.gettimeofday() in
    vlogf "load nodes....\n"; flush_all(); 
    comps := [||];
    load_all_comps();
    preload_all_nodes data;

    let finish = Unix.gettimeofday() in
    vlogf "finish loading and preloading...(%f s)\n" (finish -. start);  
    node.nodetype <- oldtype;

    let subtree_llg2 = Array.mapi (fun i d->answer_spac_cached root i) data in
    vlogf "Subtree2 llg: %f\n" ((Array.sumf subtree_llg2) /. nf);

    if node.id > 0 then
    begin
      node.nodetype <- LeafNode;
      let plus_llg = Array.mapi (fun i d->answer_spac_cached root i) data in  
      let avg = (Array.sumf plus_llg) /. nf in
      vlogf "llg without extension: %f\n" avg;
      let diff = ref 0.0 in

      Array.iter2 (fun p s -> diff := !diff +. (p -. s))  plus_llg subtree_llg2;
      let kl = (!diff /. nf) in
      vlogf "KL: %f\n" kl;
      flush_all();

      (** prints the intermediate log-likelihood into the output file **)
      (*let partial_model = sprintf "%s-llg.log" !network in
      
      let llg = (Array.sumf subtree_llg2) /. nf in
      let now = Unix.gettimeofday() in
      let llg_log =  sprintf "%d %f %f\n" node.id llg (now -. !start_time) in   
      append_string partial_model llg_log;
      *)

      if kl > 0.0 then begin 
        node.nodetype <- LeafNode; 
        node.final <- true; 
        vlogf "Reject extending node: %d\n" node.id;
 
        vlogf "Removing nodes: ";
        List.iter (fun p->vlogf "%d " p.id; p.nodetype <- NullNode) !newNodeList;
        vlogf "\n";
        [] end 
      else begin 
        vlogf "Accept extending node: %d\n" node.id;
        node.nodetype <- oldtype; 
       
        if !outputmodel <> "" then 
        begin
          let partial_model = sprintf "%s.%d" !outputmodel !extension_num in
          let model_channel = open_out partial_model in
          print_model model_channel root;
          close_out_noerr model_channel
        end;

        !newNodeList end
    end else begin
      !newNodeList 
    end
  end else begin


    node.final <- true; 
    vlogf "Reject extending node: %d\n" node.id;
    vlogf "Removing nodes: ";
    List.iter (fun p->vlogf "%d " p.id; p.nodetype <- NullNode) !newNodeList;
    vlogf "\n";
    node.nodetype <- LeafNode; 
    []
  end
  

let rec build_spn data node_heap cut_thr depth=
  let nodeList = ref [] in
  let now = Unix.gettimeofday() in
  let spent_time = now -. !start_time in
     
  if Heap.size node_heap > 0 && depth > 0 && spent_time < 68400.0 then
  begin
    let next = Heap.min node_heap in
    Heap.remove_min node_heap;
    
      
    nodeList := extends_spn data next cut_thr depth;
    List.iter (fun n->add_to_heap node_heap n) !nodeList; 
    build_spn data node_heap cut_thr (depth - 1)
  end
  else 
    ignore()
(*

let rec build_spn node cut_thr depth=
  try
    let varnum = Array.length node.schema in
    let samplesize = Array.length node.data in 
    if depth > 0 && varnum > 10 && (not node.final) && samplesize > 50 then 
    begin
      let hNodeList  = h_split node !num_part in
      nlogf "size of h node list: %d\n" (List.length hNodeList);
      flush_all();

      for i=0 to (List.length hNodeList) - 1 do
        try 
          let ndepth = depth - 1 in
          let n = List.nth hNodeList i in
          let nsamplesize = Array.length n.data in 
          if ndepth > 0 && (not n.final) && nsamplesize > 50 then 
          begin
          let vNodeList = v_split_gtest (List.nth hNodeList i) cut_thr in
          List.iter (fun n->build_spn n cut_thr (ndepth - 1)) vNodeList
          end;
        with VSplitNotPossible -> nlogf "V-split not possible\n"; build_spn (List.nth hNodeList i) cut_thr (depth-1);  ignore(); 
        
      done
    end
    else begin ignore() end;
  with HSplitNotPossible ->nlogf "H-split not possible\n"; ignore()

*)

let print_params node =
  Array.iter (fun p->vlogf "%f " (exp p)) node.params 




let rec print_tree node depth = 
  if depth > !max_depth then max_depth := depth;
  for i=0 to depth do
    vlogf "  "
  done;
  vlogf "id: %d sample: %d vars: %d type: " node.id (Array.length node.data) (Array.length node.schema);
  (match node.nodetype with LeafNode-> vlogf "ac\n" | TimesNode-> vlogf "*\n"  | PlusNode-> vlogf "+ "; print_params node; vlogf "\n" | NullNode -> vlogf "null\n"; ignore());
  
  if node.nodetype == PlusNode || node.nodetype == TimesNode then
  begin
    let n = len node.children in
    for i=0 to n - 1 do
      print_tree node.children.(i) (depth+1)
    done;
  end;
  ignore()


let print_spn node =
  print_tree node 0

(*
let learn_acnode_thread (network, now, comp_id, data, schema )=
  let data_file = Printf.sprintf "%s-%d-%d.data" network now comp_id in
  let schema_file = Printf.sprintf "%s-%d-%d.schema" network now comp_id in
  let output = Printf.sprintf "%s-%d-%d.ac" network now comp_id  in
  let logfile = Printf.sprintf "%s-%d-%d.log" network now comp_id in
  Datafile.dump_data data data_file;
  let sch = Array.map (fst) schema in 
  Datafile.dump_schema sch schema_file;
      
  let cmd = Printf.sprintf "acmn -s %s -shrink -o %s -i %s -c %f -stddev %f -ps %f -maxe %d -v -log %s" schema_file output data_file (!l1param) !stddev !split_penalty !maxe logfile in
  let t = Unix.localtime (Unix.gettimeofday() ) in 

  nlogf "%s time: %d:%d:%d\n" cmd t.tm_hour t.tm_min t.tm_sec;
      
  flush_all();
  ignore(Unix.system cmd);

  let t2 = Unix.localtime (Unix.gettimeofday() ) in 
  nlogf "finish %s time: %d:%d:%d pid %d\n" cmd t2.tm_hour t2.tm_min t2.tm_sec (Unix.getpid());
  flush_all()
)
*)

  

let minisleep (sec: float) =
    ignore (Unix.select [] [] [] sec)


let load_comps () =
  feature_lists := [||];
  comps := [||];
  for i=0 to (Array.length !node_array) - 1  do
    if (!node_array.(i).nodetype == LeafNode ) then 
    begin
      let comp_name = !node_array.(i).acname in
      let (component, fl) = load_circuit comp_name in
      let n = Array.length !comps in 
      comps := Array.append !comps [|component|];
      feature_lists := Array.append !feature_lists [|fl|];
      !node_array.(i).ac_index <- n;
    end
  done;
  scratches := Array.map (Circuit.create_scratch) !comps;
  logzs := Array.map2 (fun s c->Circuit.compute_z s c ) !scratches !comps;
  ignore()




(** Learns the structure of SPN by trying to extends its AC nodes **)
let learn_spac_structure data schema = 
  let schema_ar = Array.of_list schema in
  let i_schema = Array.augment schema_ar in
  let root = Spn.create_leaf data i_schema in
  let node_heap = Heap.create node_lt 100 in
  Heap.add node_heap root;
  build_spn data node_heap !cut_thresh !max_extension;
  root



(** Utility function for normalizing a vector to a probability distribution **)
let normalize a =
  let sum = Array.sumf (Array.map (exp) a) in 
  let n_a = Array.map (fun x->log ((exp x) /. sum) ) a in
  n_a

(** Used inside the procedure of optimazing params **)
(*
let rec update_spac_params node q = 
  let children = node.children in
  let n = Array.length children in 
  if node.nodetype == PlusNode  then
  begin
    (*let total = float_of_int(Array.sum node.counts) in
    node.params <- Array.map (fun x -> log (float_of_int x) -. log total)  node.counts;
    let n_params = Array.normalizelog node.params in*)
    let n = Array.length children in 
    let plogprobs = Array.make n 0.0 in
    for i=0 to n - 1 do
      plogprobs.(i) <- update_spac_params children.(i) q;
    done;

    let final_probs = Array.map2 (fun logp logw-> logp +. logw ) plogprobs node.params in 
    let k = Array.argmax final_probs in
    node.activated_link <- k;
    (*node.counts.(k) <- node.counts.(k) + 1;
    let total = float_of_int(Array.sum node.counts) in
    node.params <- Array.map (fun x -> log (float_of_int x) -. log total)  node.counts;*)
    let pvalue = (Ext.alogsumexp final_probs) in
    pvalue
  end
  else if node.nodetype == TimesNode then
  begin
    let tlogprobs = Array.make n 0.0 in
    for i=0 to (Array.length children) - 1 do
      tlogprobs.(i) <- update_spac_params children.(i) q
    done; 
    let tvalue = (Array.sumf tlogprobs) in
    (*nlogf "log p from node : %d %f\n" node.id tvalue;*)
    tvalue
  end 
  else if node.nodetype == ACNode then 
  begin
    let logp = answer_acnode_query node.id q in
    logp
  end else if node.nodetype == NullNode then
  begin
    nlogf "Should not be here at all\n";
    flush_all();
    -.10000.0
  end
  else raise NodeTypeError  
*)

(******* NOT USED **********)
let rec answer_spac_query node q =  
  let children = node.children in
  let n = Array.length children in 
  if node.nodetype == PlusNode then
  begin
    (*let n_params = Array.normalizelog node.params in*)
    let n = Array.length children in 
    let logprobs = Array.make n 0.0 in
    for i=0 to n - 1 do
      logprobs.(i) <- answer_spac_query children.(i) q
    done;
    let final_probs = Array.map2 (fun logp logw-> logp +. logw ) logprobs node.params in 
    (*let total = float_of_int(Array.sum node.counts) in*)
    (Ext.alogsumexp final_probs) 
  end
  else if node.nodetype == TimesNode then
  begin
    let logprobs = Array.make n 0.0 in
    for i=0 to (Array.length children) - 1 do
      logprobs.(i) <- answer_spac_query children.(i) q
    done; 
    (Array.sumf logprobs) 
  end 
  else if node.nodetype == LeafNode then
    answer_acnode_query node.id q 

  else raise NodeTypeError  



(** Used in the procedure of optimizing the parameters **)
(*
let rec update_params node = 
  if node.nodetype == PlusNode  then
  begin
    for i = 0 to (Array.length node.children) - 1 do
      node.params.(i) <- log (float_of_int (Array.length node.children.(i).data)) -. log (float_of_int(Array.length node.data));
      node.counts.(i) <- Array.length node.children.(i).data;
      update_params node.children.(i)
    done
  end else if node.nodetype == TimesNode then
  begin
    for i = 0 to (Array.length node.children) - 1 do
      update_params node.children.(i)
    done
  end else if node.nodetype == ACNode then 
  begin
    ignore()
  end else raise NodeTypeError
*)


(** Can be used to re-assign samples to cluster, useful for updating the parameters **)
(*
let rec assign_sample q node  =
  
  let indexes = Array.map (snd) node.schema in
  let subq = Array.take q indexes in 
  node.data <- Array.append node.data  [|subq|];
  if node.nodetype == PlusNode  then
  begin
    let activated_node = node.children.(node.activated_link) in
    assign_sample q activated_node
  end
  else if node.nodetype == TimesNode then 
  begin
    Array.iter (assign_sample  q) node.children
  end 
  else if node.nodetype == ACNode then 
  begin
    (*let index = node.ac_index in
    let indexes = Array.map (snd) node.schema in
    let subq = Array.take q indexes in 
    node.data <- Array.append node.data  [|subq|] *)
  end
  else 
    raise NodeTypeError
*)

(** NOT USED **)
let compute_empirical_prob f data =
  let fcond = Array.of_list f.cond in
  let total = float_of_int (Array.length data) in
  let ftotal = float_of_int (Array.count(fun x -> Mn.Factor.fmatch x fcond) data) in
  ftotal /. total 

(*
let optimize_ac node =
  flush_all();

  let pid = Unix.fork() in
    match pid with
    0 -> begin
      nlogf "Optimizing AC node: %d in process: %d\n" node.id (Unix.getpid());
      flush_all();
      let channel = open_out node.acname in 
      let data = node.data in
      flush_all();
      let i = node.ac_index in
      let fl = !feature_lists.(i) in
      let n = float_of_int (Array.length data) in
      let true_probs = ref [||] in
      List.iter (fun f -> let fprob = compute_empirical_prob f data in true_probs := Array.append !true_probs [|fprob|] ) fl; 
        let fa = Array.of_list fl in
      let opt_l1param = 1.0 in
      let v = OptImpl.datall_optimize opt_l1param !stddev !comps.(i) !true_probs n [||] fa 100 in
      ignore(v);
      Circuit.output_with_features channel !comps.(i) !feature_lists.(i);
      exit 1
    end 
    | _ -> begin
      ignore();
    end;
  pid

*)



(******* NOT USED ***************)
let rec prune node =
  nlogf "pruning node.id = %d\n" node.id;
  flush_all();
  if node.nodetype == LeafNode then 
  begin
    node.nodetype <- NullNode;
    let parent = node.parents.(0) in 
    let sibling_and_me = parent.children in
    if parent.nodetype == TimesNode then 
    begin
      let sibling = ref [||] in
      Array.iter (fun n-> if n.id <> node.id then sibling := Array.append !sibling [|n|] ) sibling_and_me;
      parent.children <- !sibling;
      node.parents.(0) <- parent;
      if (Array.length !sibling) < 2 then prune parent
    end else if parent.nodetype == PlusNode then
    begin
      let sibling = ref [||] in 
      let newparams = ref [||] in 
      (*let newcounts = ref [||] in *) 
      Array.iteri (fun i n-> if n.id != node.id then 
          begin
          sibling := Array.append !sibling [|n|];
          (*newcounts := Array.append !newcounts [|parent.counts.(i)|]; *)
          newparams := Array.append !newparams [|parent.params.(i)|]
          end ) sibling_and_me;
    
      parent.params <- !newparams;
      (*parent.counts <- !newcounts; *)
      parent.children <- !sibling;
      node.parents.(0) <- parent
    end
  end 
  else if node.nodetype == TimesNode then 
  begin
    let children = node.children in 
    let childsize = Array.length children in
    let parent = node.parents.(0) in
    let sibling_and_me = parent.children in
    if childsize == 1 then 
    begin
      node.nodetype <- NullNode;
      Array.iteri (fun i n-> if n.id == node.id then parent.children.(i) <- children.(0)) sibling_and_me;
    end
    else if childsize == 0 then 
    begin 
      node.nodetype <- NullNode;
      let sibling = ref [||] in 
      let newparams = ref [||] in 
      (*let newcounts = ref [||] in *) 
      Array.iteri (fun i n-> if n.id != node.id then 
                      begin
                        sibling := Array.append !sibling [|n|];
                        (*newcounts := Array.append !newcounts [|parent.counts.(i)|]; *)
                        newparams := Array.append !newparams [|parent.params.(i)|]
                      end ) sibling_and_me;
    
      parent.params <- !newparams;
      (* parent.counts <- !newcounts; *)
      parent.children <- !sibling;
      node.parents.(0) <- parent
    end
  end else if node.nodetype == PlusNode then
  begin
    nlogf "Should node be here\n";
  end 





(** Can be used for optimizing the parameters after learning the structure. **)
(*
let optimize_params root data =
  let it = ref 10 in
  let change = ref 100.0 in
  let prev_llg = ref (-.1000.0) in

  scratches := Array.map (Circuit.create_scratch) !comps;
  logzs := Array.map2 (fun s c->Circuit.compute_z s c) !scratches !comps;
  
  while (!it < 10) && !change > 0.01 do
    incr it;
    nlogf "Optimizing round %d previous change : %f\n" !it !change;
    Array.iter (fun n-> n.data <- [||]) !node_array;
    let probs = Array.map ( fun d-> let logp = update_spac_params root d in assign_sample d root; logp) data in
    update_params root;
    let llg_data = Array.sumf probs in
    let llg_avg = llg_data /. float_of_int(Array.length probs) in
    change := absf(llg_avg -. !prev_llg);
    nlogf "new change: %f\n" !change;
    prev_llg := llg_avg;
    nlogf "llg_avg on data -- round %d: %f\n" !it llg_avg; 
    
    let child_list = ref [] in
    for i=0 to (Array.length !node_array) - 1  do
      let node = !node_array.(i) in
      if node.nodetype == ACNode then
      begin
        let resize = Array.length node.data in 
        nlogf "Size of reassigned data = %d\n" resize;
        if resize > 0 then
        begin
          let pid = optimize_ac node in
          child_list := pid::!child_list
        end
        else begin
          prune node;
          print_spn !node_array.(0)
        end
      end
    done;
    
    while (List.length !child_list) > 0 do
      let p = List.hd !child_list in 
      child_list := rem_first !child_list;
      ignore (Unix.waitpid [] p)
    done;
    nlogf "Loading acnodes ...... \n";
    print_spn !node_array.(0); 
    flush_all();
    load_comps();
  
    nlogf "end of optimizing acnodes -----------------\n";
    flush_all();
    ignore()
  done;
  ignore()

*)

(*
let rec compute_logz node = 
  let children = node.children in
  let n = Array.length children in 
  if node.nodetype == PlusNode then
  begin
    let n_params = normalize node.params in
    let n = Array.length children in 
    let logprobs = Array.make n 0.0 in
    for i=0 to n - 1 do
      logprobs.(i) <- answer_spac_qurey children.(i) q
    done;
    let final_probs = Array.map2 (fun logp logw-> logp +. logw ) logprobs n_params in 
    let total = float_of_int(Array.sum node.counts) in
    (Ext.alogsumexp final_probs) 
  end
  else if node.nodetype == TimesNode then
  begin
    let logprobs = Array.make n 0.0 in
    for i=0 to (Array.length children) - 1 do
      logprobs.(i) <- answer_spac_qurey children.(i) q
    done; 
    (Array.sumf logprobs) 
  end 
  else if node.nodetype == ACNode then
    !logzs.(node.id)
  else raise NodeTypeError  

*)


(** NOT USED **)
let answer_query root q =
  let logp = answer_spac_query root q in
  logp

(** answer the queries from the cache **)
let answer_query_cached root q_i =
  let logp = answer_spac_cached root q_i in
  logp



(** NOT USED **)
let rec prune_spac node root valid=
  (match node.nodetype with LeafNode->ignore()
      | TimesNode-> Array.iter (fun n->prune_spac n root valid )node.children
      | PlusNode-> Array.iter (fun n->prune_spac n root valid )node.children | NullNode-> ignore() );
  if node.nodetype == LeafNode then
  begin
    ignore()
  end else
  begin
    if node.id == root.id then ignore()
    else begin
      let validlogprobs = Array.mapi (fun i x-> answer_query_cached root i) valid in
      let llg_valid = Array.sumf validlogprobs in
      let nv = float_of_int (Array.length valid) in
      let avg_valid = llg_valid /. nv in
      let oldtype = node.nodetype in
      node.nodetype <- LeafNode;
      let validlogprobs = Array.mapi (fun i x-> answer_query_cached root i) valid in
      let llg_valid = Array.sumf validlogprobs in
      let avg_valid_new = llg_valid /. nv in
      if avg_valid_new >= avg_valid then
      begin
        nlogf "prune node id: %d  old llg:%f new llg:%f\n" node.id avg_valid avg_valid_new;
        ignore()
      end else begin
        node.nodetype <- oldtype;
        nlogf "prune node id: %d  old llg:%f new llg:%f not possible\n" node.id avg_valid avg_valid_new
      end
    end
  end
    





(* test for clustering *)
(*
let _=
  Arg.parse args ignore usage;
  if !datafile = "" || !network = "" then (Arg.usage args usage; raise InsufficientArguments);
    
  let data = Datafile.load_data_ar !datafile in
  let schema = Datafile.load_schema !schemafile in
  let valid = Datafile.load_data_ar !vfile in
  let t1 = Sys.time() in
  nlogf "learning the spac structure .... \n";
  let i_schema = Array.mapi (fun i x->(x,i)) (Array.of_list schema) in
  let data_parts = Partition.horizontal_cluster 0 data valid i_schema !num_part !lambda 1 in  
  ignore(data_parts)
*)




let endswith s1 s2 =
  let re = Str.regexp (Str.quote s2 ^ "$")
  in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false


let remove_nonempty_dir dirname = 
  let filelist = Sys.readdir dirname in 
  Array.iter (fun f->Sys.remove (String.concat dir_sep [dirname; f]) )filelist;
  ignore()


(* main main *) 
let main() =
  seed := ((int_of_float (Unix.time()) )  mod  1000); 
  Arg.parse args ignore usage;
  start_time := Unix.gettimeofday();
  common_log_init();

  Random.init !seed;
  if !datafile = "" || !output_dir = "" then (Arg.usage args usage; exit(-1));
  if not (check_suffix !output_dir ".spn") then begin nlogf "Error: output directory model should end with .spn \n"; exit 1 end;



  let odir = sprintf "%s" !output_dir in
    if Sys.file_exists odir then 
    begin
      if not !override then begin
        nlogf "Error: directory %s exists.\nUse -f to override the existing directory.\n" !output_dir; exit 1 end
      else begin
        remove_nonempty_dir !output_dir
      end
    end
    else Unix.mkdir odir 0o755;


  
  let t1 = Unix.gettimeofday() in
    

  let data = Data.load_data_ar !datafile in

  let schema = 
    if !schemafile <> "" then 
      Data.load_schema !schemafile 
    else 
      Data.stream_schema (open_in !datafile) in

  
  Unix.chdir odir;
  vlogf "learning the spac structure .... \n";
  let root = learn_spac_structure data (Array.to_list schema) in

    
  vlogf "Printing spn...\n";
  flush_all();
  max_depth := 0;
  print_spn root;
  (*nlogf "learning acs....\n";
  learn_acs();*)
  
  let om = sprintf "%s.m" !network in
  (*if !outputmodel <> "" then *)
  begin
    let model_channel = open_out om in
    print_model model_channel root;
    close_out_noerr model_channel
  end;



  vlogf "Loading acs ...\n";
  load_all_comps();
  
  
  (** used to compute test likelihood in the same phase as learning. Useful for reducing the experiment complexity. 
      now you can use spquery to find the test likelihood **)
  (*  
  let logprobs2 = Array.mapi (fun i x-> answer_query_cached root i) data in
  
  let llg_data = Array.sumf logprobs2 in
  let n = float_of_int ( Array.length data) in
  let avg_data = llg_data /. n in

  nlogf "data llg: %f\n" avg_data;
  *)
  (*nlogf "Pruning spac ...\n";*)


  (*let max_depth_before_prune = !max_depth in*)
  (*preload_all_nodes data;
  prune_spac root root data;*)
  (*max_depth := 0;*)
  (*print_spn root*)

  (*let data_parts = ref (Array.make (Array.length !comps) [||]) in
  *)
  (*optimize_params root data;*)
  
  (*Array.iter (nlogf "%f\n" ) probs;


  
  scratches := Array.map (Circuit.create_scratch) !comps;
  logzs := Array.map2 (fun s c->Circuit.compute_z s c ) !scratches !comps;
  *)


  let max_depth_after_prune = !max_depth in
  let t2 = Unix.gettimeofday() in
  dlogf "Calculating data llg\n"; flush_all(); 
  clear_all_caches();
  preload_all_nodes data;
  let logprobs = Array.mapi (fun i x-> answer_query_cached root i) data in
  (*Array.iter (nlogf "%f\n" ) logprobs;*)

  let llg_data = Array.sumf logprobs in
  let n = float_of_int ( Array.length data) in
  let avg_data = llg_data /. n in

  
  (*nlogf "Calculating valid llg\n"; flush_all();  
  clear_all_caches();
  let avg_valid = ref 0.0 in
  if !vfile <> "" then begin

    let valid = Data.load_data_ar !vfile in
    preload_all_nodes valid;
    let validlogprobs = Array.mapi (fun i x-> answer_query_cached root i) valid in
    let llg_valid = Array.sumf validlogprobs in
  
    let nv = float_of_int ( Array.length valid) in
    avg_valid := llg_valid /. nv
  end;
  *)
  let t3 = Unix.gettimeofday() in
	(*
  let avg_test = ref 0.0 in 
  if !qfile <> "" then begin
    nlogf "Calculating test llg\n"; flush_all(); 
  
    let test = Data.load_data_ar !qfile in
    clear_all_caches();
  
    preload_all_nodes test;
    let testlogprobs = Array.mapi (fun i x-> answer_query_cached root i) test in

    let llg_test = Array.sumf testlogprobs in
    let nt = float_of_int ( Array.length test) in
    avg_test := llg_test /. nt;

    if !outputinference <> "" then
    begin
      let inference_channel = open_out !outputinference in
      Array.iter (fprintf inference_channel "%f\n" ) testlogprobs;
      close_out_noerr inference_channel
    end


  end;
	*)
  let t4 = Unix.gettimeofday() in


  

  ignore(max_depth_after_prune);
  ignore(t1);
  ignore(t2);
  ignore(t3);
  ignore(t4);

  (*nlogf "result %s %d %f %f %f %f %f %f %d %f %f %f %f %f %d %d\n" !network !num_part !split_penalty !l1param !stddev !ratio !cut_thresh !lambda !maxe avg_data !avg_valid !avg_test (t2 -. t1) (t4 -. t3) !max_extension max_depth_after_prune;
  *)
  nlogf "Average data likelihood: %f\n" avg_data;
  
  let filelist = Sys.readdir "." in
  let endswith = String.suffix in
  Array.iter ( fun f -> if endswith f ".cluster" || endswith f ".data" || endswith f ".schema" || endswith f ".log" then Sys.remove f   ) filelist; 
  ignore()


let extend() = ignore()


let _=
   main()  
