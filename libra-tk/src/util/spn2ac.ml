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
open Unix
open Circuit
open Str
open Ext
open Spn

let network = ref ""
let modelfile = ref ""
let outfile = ref ""
let outfile_js = ref ""
let outfile_dot = ref ""
let usage = "Usage: spn2ac -m <model_directory> -o <ac_output>"
let args = Arg.align
  ([
    ("-m",  Arg.Set_string modelfile,  " Input SPN (SPAC) model directory");
    ("-o",  Arg.Set_string outfile,  " Output arithmetic circuit")]
    @ common_arguments)


let comps = ref [||] 
let comps2 = ref [||] 
let feature_lists = ref [||]
let scratches = ref [||] 
let logzs = ref [||] 

exception CircuitLoadException

let load_circuit filename =
  try Circuit.load_with_features (open_in filename) 
  with Circuit.Unknown_child(n,c) -> printf "Error: node %s references unknown child node %s.\n" n c; raise CircuitLoadException

let load_comps () =
  feature_lists := [||];
  comps := [||];
  for i=1 to Spn.get_spn_size() - 1  do
    let node = Spn.get_node_by_id i in
    if (node.nodetype == LeafNode) then (* || nodetype == PlusNode || nodetype == TimesNode) then *)
    begin
      let comp_name = node.acname in
    (*  printf "%s\n" comp_name; flush_all();*)
    let full_comp_name = String.concat dir_sep [!modelfile; comp_name] in
      let (component, fl) = load_circuit full_comp_name in
      let n = Array.length !comps in 
      comps := Array.append !comps [|component|];
      feature_lists := Array.append !feature_lists [|fl|];
      node.ac_index <- n;
    end
  done;

  scratches := Array.map (Circuit.create_scratch) !comps;
  logzs := Array.map2 (fun s c->Circuit.compute_z s c ) !scratches !comps;
  ignore()


let rec rebuild_ac node =
  (match node.nodetype 
    with LeafNode-> begin
        let circ = !comps.(node.ac_index) in
        let logz = !logzs.(node.ac_index) in
        (* let coeff = -.logz in *)
        let coeff_nodes = ref [] in
        let remaining = ref logz in
        let log2 = log 2.0 in
        while !remaining > log2 do
          let node = Circuit.create_const (-.log2) in
          coeff_nodes := node::!coeff_nodes;
          remaining := !remaining -. log2;
        done;

        let node = Circuit.create_const (-. !remaining) in
        coeff_nodes := node::!coeff_nodes;
        (*printf "nid: %d logz:%f 1/z: %f\n" node.id logz (exp coeff);*)
        let children = circ.root::!coeff_nodes in
        
        (* OLD: let coeff_node = Circuit.create_const coeff in *)
        let new_root = Circuit.create_times children in
        new_root
     end
       | TimesNode-> begin
        let num = Array.length node.children in
        let children_roots = ref [] in 
        for i=0 to num - 1 do
          let circ_root = rebuild_ac node.children.(i) in
          children_roots := circ_root::!children_roots
        done;
        Circuit.create_times (List.rev !children_roots)
     end
       | PlusNode-> begin
        let num = Array.length node.children in
        let children_roots = ref [] in 
        for i=0 to num - 1 do
          let circ_root = rebuild_ac node.children.(i) in
          let param_node = Circuit.create_const node.params.(i) in
          let p_node = Circuit.create_times [param_node; circ_root] in
          children_roots := p_node::!children_roots
        done;
        Circuit.create_plus (List.rev !children_roots)
      end 
       | NullNode -> raise Circuit.UnsupportedNodeType ) 

let of_graph4 s r =
  let vn = Circuit.make_vnodes s in
  let vnl = Array.map Array.to_list vn in
  Circuit.of_graph s vn vnl r
           

let rebuild_spn spnroot =
  let schema = Array.map fst spnroot.schema in
  
  let vn = Circuit.make_vnodes  schema in
  let vnl = Array.map Array.to_list vn in
    
  for i=0 to (Array.length !node_array)  -1 do
		let node = Spn.get_node_by_id i in
    if node.nodetype == LeafNode then 
    begin
      let localmapping = Array.map snd node.schema in
      let circ = !comps.(node.ac_index) in
      let fixchild c =
        if is_var c then
          let (var, value) = var_details c in
          (*printf "n %d: %d->%d\n" i var localmapping.(var);
          *)
          vn.(localmapping.(var)).(value)
        else c in
      let relink_vars m =
        m.Circuit.children <- List.map fixchild m.Circuit.children in
      Circuit.node_iter relink_vars circ.root ;
      let newcirc = Circuit.of_graph (Array.map fst node.schema) vn vnl circ.root in
      (* let newindex = Array.length !comps2 in *)
      comps2 := Array.append !comps2 [|newcirc|];
      (* acnode.ac_index <- newindex; *)
      !comps.(node.ac_index) <- newcirc
    end
  done;
  ignore()



let main() =
  Arg.parse args ignore usage;
  common_log_init();

  if (!modelfile = "" || !outfile = "") then (Arg.usage args usage; exit(-1));
  
  if String.suffix !modelfile dir_sep then
    modelfile := String.sub !modelfile 0 ((String.length !modelfile) - 1);
  
  if not (check_suffix !modelfile ".spn") then begin 
    printf "Only accepts .spn as the input model file"; exit 1 
  end;
  
  if !outfile_js <> "" && not (check_suffix !outfile_js ".js") then begin 
    printf "Javascript output file should end with .js\n"; 
    exit 1 
  end;

  Spn.load_model (String.concat dir_sep [!modelfile; "spac.m"]);  
  
  load_comps();
  
	let root = (Spn.get_node_by_id 0) in
  rebuild_spn root;
  let newAcRoot = rebuild_ac root in
  
  let newCircuit = of_graph4 (Array.map fst root.schema)  newAcRoot in
  
  
  if !outfile <> "" then begin
    let out_ch = open_out !outfile in
    Circuit.output out_ch newCircuit;
    close_out out_ch 
  end;

  
  if !outfile_js <> "" then begin
    let out_ch = open_out !outfile_js in
    Circuit.output_js out_ch newCircuit;
    close_out out_ch
  end;
 
  if !outfile_dot <> "" then begin
    let out_ch = open_out !outfile_dot in
    Circuit.output_dot out_ch newCircuit;
    close_out out_ch
  end;
 
  printf "Done!\n";
  ignore()

;;

main()
