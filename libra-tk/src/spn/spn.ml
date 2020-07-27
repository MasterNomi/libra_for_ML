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
open Str


type data_t = int array array
type local_schema_t = (int*int) array
type schema_t = int array

exception CircuitLoadException
exception HSplitNotPossible
exception VSplitNotPossible
exception NodeTypeError

type spn_node_type = TimesNode
        | PlusNode
        | NullNode 
  | LeafNode;;

type spnode = {
  id: int;
  mutable parents: spnode array;
  mutable children: spnode array;
  mutable params: float array;
  mutable nodetype: spn_node_type;
  mutable schema: (int * int) array;
  mutable data: int array array;
  mutable acname: string;
  mutable ac_index: int ;
  mutable final : bool;
  mutable infcached: bool;
  mutable logpTemp : float array;

}

(*type spnode_light = {
  mutable id: int;
  mutable parents: int array;
  mutable children: int array;
  mutable params: float array;
  mutable nodetype: spn_node_type;
  mutable schema: (int * int) array;
  mutable acname: string;
  mutable ac_index: int ;
}
  
*)

(********  SPN data structure and functions for manipulating the data structure *********)


(*let root = ref spnode
*)
let node_array = ref [||]
let comps = ref [||] 
let feature_lists = ref [||]
let scratches = ref [||] 
let logzs = ref [||]
 

(*let data_parts = ref [||]
*)
let get_next_id () = 
  Array.length !node_array

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
  final = false;
  infcached = false;
  logpTemp = [||]; 
  } in
  node_array := Array.append !node_array [|n|];
  n

let create_leaf = create_node LeafNode [||] [||] [||]  
 
let create_plus = create_node PlusNode [||] [||] [||] 

let create_times = create_node TimesNode [||] [||] [||] 

let create_null() = create_node NullNode [||] [||] [||] [||] [||]  

let get_spn_size() = Array.length !node_array 

let len a = Array.length a 

let node_as_array n = [|n|]

let node_as_list n = [n]



let get_node_by_id id = !node_array.(id)


(** Returns a string value corresponding to each type of nodes in spn *)
let get_type_name nodetype = 
  if nodetype == PlusNode then "+" else if nodetype == TimesNode then "*" else if nodetype == LeafNode then "ac" else "null"

(** Adds a child to the node, and update the parameters based on the number of samples in the child node *)
let add_child node child = 
  node.children <- Array.append node.children  [|child|];
  node.params <- Array.append node.params [|log(float_of_int(Array.length child.data)) -. log( float_of_int(Array.length node.data))|];
  ignore()

(** Sets the parent of a node, and also updates the children of the parent node *)
let set_parent node parent =
  add_child parent node;
  node.parents <- Array.append node.parents [|parent|];
  ignore()


(** Prints the information of a single SPN node into an output file *)
let print_node out node =
  let n = (Array.length node.children) in
  let plen = Array.length node.parents in
  let parentid = if plen > 0 then node.parents.(0).id else -1 in
  if false then ignore() (*node.id > 0 && node.parents.(0).nodetype == LeafNode then ignore()*)
  else begin  
    fprintf out "n %d %s %d \n" node.id  (match node.nodetype with LeafNode-> "ac" | TimesNode-> "*" | PlusNode-> "+" | NullNode ->  "null") parentid;
    match node.nodetype with LeafNode->
        begin
        let indeces = Array.map (snd) node.schema in
        let st_li = Array.to_list (Array.map (string_of_int) indeces) in
        let line = String.concat " " st_li in
        fprintf out "%s\n" line
        
        end;
      | TimesNode ->
        begin
        (*fprintf out "%f\n" node.thresh;*)
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

(** Prints the SPN structure into the output file *)
let print_model out root =
  fprintf out "%s\n" "spac";
  fprintf out "%d\n" (Array.length !node_array);
  Array.iter (print_node out) !node_array



let space = regexp " " 
let read_line_tokens model = split space (input_line model) 



exception ParseError of string

let parse_node_first_pass model  = 
  let tokens = split space (input_line model) in
  let i = match (List.nth tokens 0) with "n" -> 0 | _-> raise (ParseError "not a node") in
  ignore(i);
  let nodeid = int_of_string (List.nth tokens 1) in
  let nodetype = match (List.nth tokens 2) with "+" -> PlusNode | "ac" -> LeafNode | "*" -> TimesNode | "null"->NullNode | _-> raise(ParseError "node type incorrect") in
  (match nodetype with PlusNode -> 
    begin
			let tokens = read_line_tokens model in
			ignore(tokens);
			let tokens = read_line_tokens model in
			ignore(tokens);
			let tokens = read_line_tokens model in
			ignore(tokens);
			let n = create_plus [||] [||] in
			ignore(n)
		end
    | TimesNode ->
    begin

			let tokens = read_line_tokens model in
			ignore(tokens);
			let tokens = read_line_tokens model in
			ignore(tokens);
			let n = create_times  [||] [||] in
			ignore(n)
    end
    | LeafNode ->
    begin


  	  let tokens = read_line_tokens model in
 
			ignore(tokens);
 			let ac_node = create_leaf [||] [||]  in
   
   		ignore(ac_node);
    	!node_array.(nodeid).acname <- sprintf "spac-%d.ac"  nodeid;

    end
    | NullNode-> begin let nullnode= create_null() in ignore(nullnode) end
   );
  
  ignore()




let parse_node_second_pass model  =
  let tokens = split space (input_line model) in
  (*printf "line: %s %s %s\n" (List.nth tokens 0) (List.nth tokens 1) (List.nth tokens 2); 
  *)
  let i = match (List.nth tokens 0) with "n" -> 0 | _-> raise (ParseError "not a node") in
  ignore(i);
  let nodeid = int_of_string (List.nth tokens 1) in
	let node = get_node_by_id nodeid in
  let parentid = int_of_string (List.nth tokens 3) in
  let parentIds = if parentid < 0 then [||] else [|parentid|] in
	let parents = Array.map (get_node_by_id) parentIds in
  (match node.nodetype with PlusNode -> 
    begin
			let tokens = read_line_tokens model in
			let childrenIds = Array.of_list (List.map (int_of_string) tokens) in
			
			let children =  Array.map (get_node_by_id) childrenIds in
			
			let tokens = read_line_tokens model in
			let params = Array.of_list (List.map (float_of_string) tokens) in
			
			let tokens = read_line_tokens model in
			let schema = Array.of_list (List.map (int_of_string) tokens ) in
			let i_schema = Array.map (fun s->(2,s))schema in
			

			node.children <- children;
			node.parents <- parents;
			node.params <- params;
			node.schema <- i_schema;
			ignore(node)
    end
    | TimesNode ->
    begin
			let tokens = read_line_tokens model in
			let childrenIds = Array.of_list (List.map (int_of_string) tokens) in
			let children =  Array.map (get_node_by_id) childrenIds in
			let tokens = read_line_tokens model in
			let schema = Array.of_list (List.map (int_of_string) tokens ) in
			let i_schema = Array.map (fun s->(2,s))schema in
			
			
			node.children <- children;
			node.parents <- parents;
			node.schema <- i_schema;
			ignore(node)
    end
    | LeafNode ->
    begin
			let tokens = read_line_tokens model in
			let schema = Array.of_list (List.map (int_of_string) tokens ) in
			let i_schema = Array.map (fun s->(2,s))schema in

			node.parents <- parents;
			node.schema <- i_schema;
   

			node.acname <- sprintf "spac-%d.ac" nodeid;

			ignore(node);
    end
    | NullNode-> ignore()
   );
  
  ignore()
  (*!node_array.(nodeid).nodetype <- nodetype;*)
  (*!node_array.(nodeid).acname <- sprintf "%s-%d.ac" !network nodeid*)




let load_model modelfile =
		node_array := [||];
    let model = open_in modelfile in
    let network = input_line model in
    ignore(network);
		let num_nodes = int_of_string (input_line model) in
    for i=1 to num_nodes do
      parse_node_first_pass model     
    done;
		close_in_noerr model;


    let model = open_in modelfile in
    let network = input_line model in
		ignore(network);
    let num_nodes = int_of_string (input_line model) in
    (*printf "Num of nodes: %d\n" num_nodes; *)  
    for i=1 to num_nodes do
      parse_node_second_pass model     
    done;
		close_in_noerr model;
		ignore()
	
	(*raise (ParseError "SPN Parse Error") *)







(** Learns a sum node using sample clustering *)
let h_split node num_part lambda concurrent_thr ratio =
  let v = len node.data.(0) in
  vlogf "h-split node:%d varsize:%d \n" node.id v;
  (*let m = create_adj_matrix node.data sch 0.000001 in 
  ignore(m);*)
  let finish = ref false in
  let it = ref num_part in
  let data_parts = ref [||] in
  let totalVar = float_of_int (Array.length !node_array.(0).schema) in
  let nodeVar = float_of_int ( Array.length node.schema ) in
  let varRatio = nodeVar /. totalVar in
  while !it >= 2 && (not !finish) do
    data_parts := Partition.horizontal_cluster node.id node.data node.schema !it (lambda *. varRatio) concurrent_thr; 
    vlogf "sizes of clusters: "; 
    let sizes = Array.map (fun d->let l = Array.length d in vlogf "%d " l; l ) !data_parts in
    vlogf "\n";
    let total = Array.sum sizes in
    let ratios = Array.map (fun s-> float_of_int (s) /. float_of_int(total)) sizes in
    let ok = ref true in
    Array.iter ( fun r -> if (r < ratio ) then ok := false ) ratios;
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
    let node_i = create_leaf !data_parts.(i) node.schema in
    set_parent node_i node;
    nodeList := node_i::!nodeList
  done;
  node.nodetype <- PlusNode; 
  !nodeList

(** Learns a sum node using sample clustering for conditional distribution *)
let h_split_cond node num_part lambda concurrent_thr ratio =
  let v = len node.data.(0) in
  vlogf "h-split node:%d varsize:%d \n" node.id v;
  (*let m = create_adj_matrix node.data sch 0.000001 in 
  ignore(m);*)
  let finish = ref false in
  let it = ref num_part in
  let data_parts = ref [||] in
  let totalVar = float_of_int (Array.length !node_array.(0).schema) in
  let nodeVar = float_of_int ( Array.length node.schema ) in
  let varRatio = nodeVar /. totalVar in
  while !it >= 2 && (not !finish) do
    data_parts := Partition.horizontal_cluster node.id node.data node.schema !it (lambda *. varRatio) concurrent_thr;  
  let sizes = Array.map (fun d->let l = Array.length d in vlogf "%d " l; l ) !data_parts in
    vlogf "\n";
    let total = Array.sum sizes in
    let ratios = Array.map (fun s-> float_of_int (s) /. float_of_int(total)) sizes in
    let ok = ref true in
    Array.iter ( fun r -> if (r < ratio ) then ok := false ) ratios;
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
    let node_i = create_leaf !data_parts.(i) node.schema in
    set_parent node_i node;
    nodeList := node_i::!nodeList
  done;
  node.nodetype <- PlusNode; 
  !nodeList

(** Learns a product node using variable clustering 
@param node spnode to split
@param cut_thr we suppose no edge if the mutual information of two nodes is less than cut_thr 
*)
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
        let node_i = create_leaf data_i schema_i in
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
        let node_i = create_leaf data_i schema_i in
        node_i.final <- true;
        set_parent node_i node;
        nodeList := node_i::!nodeList;

        node.nodetype <- TimesNode;
        (*node.thresh <- cut_thr; *)
        !nodeList 
    end else if s > 0 then 
    begin
      node.final <- true;
      raise VSplitNotPossible
    end
    else begin
      node.nodetype <- TimesNode;
      (*node.thresh <- cut_thr; *)
      !nodeList 
    end
  with Partition.CutNotPossible-> raise VSplitNotPossible


let v_split_gtest node cut_thr=
  let v = len node.data.(0) in
  dlogf "v-split node:%d varsize:%d\n" node.id v;
  
  (*let l = len node.data in*)
  let nodeList = ref [] in
  try
    let ccs = Partition.find_ccs node.data node.schema cut_thr in
    let ccs_size = Array.length ccs in
    for i = 0 to ccs_size - 1 do 
      let nodesnum_i = Array.of_list ccs.(i) in
      let data_i = Array.takeCol node.data nodesnum_i in
      let schema_i = Array.take node.schema nodesnum_i in
      let node_i = create_leaf data_i schema_i in
      set_parent node_i node;
      nodeList := node_i::!nodeList
    done;
    node.nodetype <- TimesNode;
    (*node.thresh <- cut_thr;*)
    !nodeList 
  with Partition.CutNotPossible-> raise VSplitNotPossible

let h_split_force node num_part lambda concurrent_thr ratio =
  let v = len node.data.(0) in
  vlogf "h-split node:%d varsize:%d \n" node.id v;
  (*let m = create_adj_matrix node.data sch 0.000001 in 
  ignore(m);*)
  let num = ref 0 in
  let data_parts = ref [||] in
  let it = ref 1.0 in
  while !num <= 1  do
    data_parts := Partition.horizontal_cluster node.id node.data node.schema num_part (lambda /. !it) concurrent_thr;  
    vlogf "Number of samples in each cluster: ";
    let sizes = Array.map (fun d->let l = Array.length d in vlogf "%d " l; l ) !data_parts in
    vlogf "\n";
    vlogf "number of clusters: %d\n" (Array.length sizes); 

    it := !it +. 1.0;
    num := Array.length !data_parts
  done;

  let nodeList = ref [] in
  for i = 0 to (Array.length !data_parts) - 1 do
    let node_i = create_leaf !data_parts.(i) node.schema in
    set_parent node_i node;
    nodeList := node_i::!nodeList
  done;
  node.nodetype <- PlusNode; 
  !nodeList








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
  with Partition.CutNotPossible-> raise VSplitNotPossible
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


let node_lt n1 n2 = (Array.length n1.data ) > (Array.length n2.data)  

 
(** Add nodes to heap *)
let add_to_heap node_heap node = 
  let varnum = Array.length node.schema in
  let samplesize = Array.length node.data in 
  if varnum > 10 && (not node.final) && samplesize > 50 && node.nodetype == LeafNode then Heap.add node_heap node else ignore()

(* NOT USED *)
let append_string path s =
    let chan = open_out_gen [Open_wronly; Open_creat] 0o666 path
    in let len = out_channel_length chan
    in
        begin
        seek_out chan len;
        output_string chan s;
        close_out chan;
        end

(** The heap comparing function *)
let compare n1 n2 =
  let l1 = Array.length n1.data.(0) in
  let l2 = Array.length n2.data.(0) in
  if l1 > l2 then -1 else if l2 > l1 then 1 
  else begin
    let sl1 = Array.length n1.data in
    let sl2 = Array.length n2.data in
    if sl1 > sl2 then -1 else if sl2 > sl1 then 1 else 0
  end



let print_params node =
  Array.iter (fun p->vlogf "%f " (exp p)) node.params 




let rec print_tree node depth =
  (*if depth > max_depth then max_d := depth; *)
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





(** NOT USED **)
let compute_empirical_prob f data =
  let fcond = Array.of_list f.cond in
  let total = float_of_int (Array.length data) in
  let ftotal = float_of_int (Array.count(fun x -> Mn.Factor.fmatch x fcond) data) in
  ftotal /. total 


