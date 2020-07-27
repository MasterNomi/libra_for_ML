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
open AcParseType
open Ext

include Node

(*
      Circuit data structure representing an arithmetic circuit.
 *)

type scratch_t = float array * int array array
type evidence_t = float array array
type schema_t = int array
type example_t = int array

type circuit = {schema: int array ;
                vnodes:   node array array ;
                vnodes_l: node list array ;
                flat_vnodes: node array ;
                mutable nodes: node array ; 
                mutable root: node ;
                mutable size: int}

(* Imports from Node *)

(* 
 * Debug functions 
 *)

exception UnsupportedNodeType

(* Convert between root-first node indexing and root-last node
   indexing.  Node id (n.id) is assumed to be in root-first order,
   but we print it out in root-last order, so this requires that we
   change the ids around.  Kind of a mess. *)
let node_output_id size n = size - n.id - 1 
let node_input_index size i = size - i - 1 


let string_of_schema schema= 
  "("^(String.concat " " (List.map string_of_int (Array.to_list schema)) )^")\n"

(* Output circuit *)
let output channel circ =  
  output_string channel (string_of_schema circ.schema);
  let rev_id = node_output_id circ.size in 
  let output_children n = 
    List.iter (fun c -> fprintf channel " %d" (rev_id c)) n.children in
  for i = circ.size - 1 downto 0 do
    let n = circ.nodes.(i) in
    (match n.details with 
        PlusNode -> 
          output_string channel "+" ;
          output_children n
      | TimesNode -> 
          output_string channel "*" ;
          output_children n
      | VarNode (var, value) -> 
          fprintf channel "v %d %d" var value
      | ConstNode wt ->
          fprintf channel "n %.6g" (exp wt)
      | _ -> raise UnsupportedNodeType) ;
    output_string channel "\n"
  done;
  fprintf channel "EOF\n"


let output_js channel circ = 
  output_string channel "var g = new dagreD3.Digraph();\n";

  let rev_id = node_output_id circ.size in 
  let output_children_js n p = 
    List.iter (fun c -> fprintf channel "g.addEdge(null, \"n%d\",   \"n%d\", { label: \"\" });\n" p (rev_id c)) n.children in
  for i = circ.size - 1 downto 0 do
    let n = circ.nodes.(i) in
    (match n.details with 
        PlusNode -> 
          fprintf channel "g.addNode(\"n%d\",    { label: \"+\" });\n" i
      | TimesNode -> 
          fprintf channel "g.addNode(\"n%d\",    { label: \"*\" });\n" i
      | VarNode (var, value) -> 
          fprintf channel "g.addNode(\"n%d\",    { label: \"V%d,%d\" });\n" i var value
      | ConstNode wt ->
          fprintf channel "g.addNode(\"n%d\",    { label: \"%.4f\" });\n" i (exp wt)
      | _ -> raise UnsupportedNodeType) 

  done;

  output_string channel "\n\n";
  for i = circ.size - 1 downto 0 do
    let n = circ.nodes.(i) in
    (match n.details with 
        PlusNode ->
          output_children_js n i
      | TimesNode -> 
          output_children_js n i
      | VarNode (var, value) -> 
        ignore();
      | ConstNode wt ->
        ignore();
      | _ -> raise UnsupportedNodeType) ;
    output_string channel "\n"
  done


let output_dot channel circ = 
  output_string channel "digraph g {\n";

  let rev_id = node_output_id circ.size in 
  let output_children_dot n  = 
    List.iter (fun c -> fprintf channel "\"n%d\" -> \"n%d\";\n" (rev_id n) (rev_id c)) n.children in
  for i = circ.size - 1 downto 0 do
    let n = circ.nodes.(i) in
    (match n.details with 
        PlusNode -> 
          fprintf channel "n%d [label = \"+%d\"];\n" (rev_id n) (rev_id n)
      | TimesNode -> 
          fprintf channel "n%d [label = \"*%d\"];\n" (rev_id n) (rev_id n)
      | VarNode (var, value) -> 
          fprintf channel "n%d [label = \"V%d,%d\" ];\n" (rev_id n) var value
      | ConstNode wt ->
          fprintf channel "n%d [label = \"%.4f\"];\n " (rev_id n)  (exp wt)
      | _ -> raise UnsupportedNodeType) 

  done;

  output_string channel "\n\n";
  for i = circ.size - 1 downto 0 do
    let n = circ.nodes.(i) in
    (match n.details with 
        PlusNode ->
          output_children_dot n 
      | TimesNode -> 
          output_children_dot n 
      | VarNode (var, value) -> 
        ignore();
      | ConstNode wt ->
        ignore();
      | _ -> raise UnsupportedNodeType) ;
    output_string channel "\n"
  done;
  output_string channel "}\n"



let print = output stdout


(*
 * Print out a circuit in optimized form starting with just the root.
 * Recursively prints out all children first.  Sets child indices 
 * to be in reverse order (to be consisted with array representation, 
 * which lists the root first), but printed indices are in sequential
 * order (to be consistent with output representation, which lists the
 * root last).
 *)
let rec output_node_rec chan nhash size index n = 
  if not (NHashSet.mem nhash n) then begin
    NHashSet.add nhash n;
    let output_children i cl = 
      let ir = ref i in
      List.iter (fun c -> ir := output_node_rec chan nhash size !ir c) cl;
      !ir in
    let new_index = output_children index n.children in
    let rev_id = node_output_id size in 
    let output_child_list n = 
      List.iter (fun c -> fprintf chan " %d" (rev_id c)) n.children in
    n.id <- new_index;
    (match n.details with 
        PlusNode ->
          output_string chan "+";
          output_child_list n
      | TimesNode ->
          output_string chan "*";
          output_child_list n
      | VarNode (var, value) -> 
          fprintf chan "v %d %d" var value
      | ConstNode wt ->
          fprintf chan "n %.6g" (exp wt)
      | _ -> raise UnsupportedNodeType) ;
    output_string chan "\n" ;
    new_index - 1
  end
  else
    index

let output_root channel schema r =

  output_string channel (string_of_schema schema);
  (*output_string channel (Data.to_string_schema schema) ; *)
  let nhash = NHashSet.create 100 in
  (* Need to pass in total number of nodes and initial index,
     which is the total number of nodes - 1.  (Since we're
     zero-based.)  output_node_rec returns the next index, which
     is one less than the last index used.  After outputting all
     nodes, the last node we output should have had index 0. *)
  let numnodes = List.length (root_to_list r) in
  let final_index = 
    output_node_rec channel nhash numnodes (numnodes-1) r + 1 in
  assert(r.id = 0);
  assert(final_index = 0);
  fprintf channel "EOF\n"

let print_root = output_root stdout


(*
 * General circuit functions
 *)

let schema circ = circ.schema

let numvars circ = Array.length circ.schema

let node circ i = circ.nodes.(i)

let all_var_dists circ =
  let l = Array.to_list circ.nodes in
  List.filter is_sot_dist l

let dist_var dist =
  assert (is_sot_dist dist) ;
  let child = List.hd dist.children in
  let gchild = List.find is_var child.children in
  var_var gchild 

let all_vars circ = circ.vnodes

let sibling_vars circ v = circ.vnodes_l.(var_var v)

let var circ varnum = circ.vnodes.(varnum)

let flatten_vars vnodes = Array.concat (Array.to_list vnodes)
let flat_vars circ = circ.flat_vnodes


(* Put all nodes in the circuit into a topological partial order. *)
let order circ =
  let nump = Array.map (compose List.length parents) circ.nodes in
  let newnodes = Array.copy circ.nodes in
  let i = ref 0 in
  let rec order_rec n =
    nump.(n.id) <- nump.(n.id) - 1 ; 
    if nump.(n.id) <= 0 then begin 
      n.id <- !i ;
      newnodes.(n.id) <- n ;
      incr i ;  
      List.iter order_rec n.children
    end in
  order_rec circ.root ;
  circ.nodes <- newnodes 
  (*
  assert (Array.for_all (fun x -> x <= 0) nump)
  *)
let make_vnodes schema =
  let numvars = (Array.length schema) in
  let a = Array.make numvars [||] in
  for i = 0 to numvars - 1 do
    a.(i) <- Array.make schema.(i) null_node ;
    for j = 0 to schema.(i) - 1 do
      a.(i).(j) <- create_var i j
    done
  done ;
  a

let __make_node_array l =
  let a = Array.of_list l in
  let update_node i n =
    n.id <- i in
  Array.iteri update_node a ;
  a

let rebuild circ =
  let r = circ.root in
  Node.prune_orphans r ;
  (* TODO: can we make this more purely functional? *)
  if false then ( Node.prune_single_parents r ; Node.prune_orphans r ) ;
  if false then ( Node.prune_single_children r ; Node.prune_orphans r ) ;
  Array.iter (fun n -> n.id <- -1) circ.nodes ;
  let l = root_to_list r in
  circ.nodes <- __make_node_array l ;
  circ.size <- Array.length circ.nodes ;
  order circ

(* Construct a new circuit from a schema and root node 
   and places it in topological order. *)
let of_graph s vn vnl r =
  let c = {schema = s ; 
           vnodes   = vn ; 
           vnodes_l = vnl ; 
           flat_vnodes = (flatten_vars vn); 
           nodes = [||] ; 
           root = r ; 
           size = 0} in
  rebuild c ; c
   
(* UNUSED *)
let of_graph2 s s2 r =
  let vn = make_vnodes s in
  let vnl = Array.map Array.to_list vn in
  let fixchild c =
    if is_var c then
      let (var, value) = var_details c in
      vn.(1).(value)
    else c in
  let relink_vars n =
    n.children <- List.map fixchild n.children in
  node_iter relink_vars r ;
  of_graph s vn vnl r

let depth circ =
  let a = Array.make circ.size 0 in
  for i = circ.size - 1 downto 0 do
    let max_depth d n = max d a.(n.id) in
    a.(i) <- 1 + List.fold_left max_depth 0 circ.nodes.(i).children 
  done ;
  a.(circ.root.id)


(*
 * Circuit construction functions
 *)

(*
let print_objsize x = 
  let t = Objsize.objsize x in
  printf "Data: %d;  Headers: %d;  Depth: %d\n" 
    t.Objsize.data t.Objsize.headers t.Objsize.depth ;
  printf "Bytes with headers: %d\n" (Objsize.size_with_headers t) ;
  printf "Bytes without headers: %d\n" (Objsize.size_without_headers t)
  *)


exception Unknown_child of string * string 

let update_parents circ =
  (* Remove all parents *)
  Array.iter (fun n -> n.parents <- []) circ.nodes;
  (* Set new parents *)
  Array.iter (fun n -> 
    List.iter (fun c -> 
      c.parents <- n :: c.parents) n.children) circ.nodes

let of_parse (s, pnlist) =
  (* printf "Beginning to construct Circuit from the parse!\n";
  flush stdout; let _ = input_char stdin in  *)
  let schema = Array.of_list s in
  let vn = make_vnodes schema in
  let vn_l = Array.map (Array.to_list) vn in
  let pnarray = Array.of_list pnlist in
  let size = Array.length pnarray in
  let rev_id = node_input_index size in
  let nodes = Array.make size null_node in
  (* Helper function for linking; checks that child nodes already exist. *)
  let get_child i j =
    if i < j then
      raise (Unknown_child ((string_of_int j), (string_of_int i)))
    else 
      nodes.(rev_id j) in
  (* Create and link nodes *)
  for i = 0 to size - 1 do 
    let ri = rev_id i in
    nodes.(ri) <-
      begin
      match pnarray.(i) with 
        AcParseType.PTimesNode children -> 
          create_times (List.map (get_child i) children)
      | AcParseType.PPlusNode children -> 
          create_plus (List.map (get_child i) children)
      | AcParseType.PVarNode (var,value) ->
          (* TODO -- write better error handling. *)
          if var >= Array.length vn || value >= Array.length vn.(var) then
            failwith (sprintf "Circuit parse error: Out-of-bounds variable/value indices, v %d %d" var value);
          vn.(var).(value)
      | AcParseType.PConstNode wt ->
          create_const wt 
      end ; 
  done ;
  (* Set ids *)
  for i = 0 to size - 1 do
    nodes.(i).id <- i
  done ;
  let c = {schema = schema ; 
           vnodes = vn ; 
           vnodes_l = vn_l ; 
           flat_vnodes = (flatten_vars vn); 
           nodes = nodes ; 
           root = nodes.(0) ; 
           size = size} in
  update_parents c;
  c

  
let num_edges circ = List.sum (node_map num_children circ.root)

let num_params circ =
  let n = ref 0 in
  for i = 0 to circ.size - 1 do
    if is_const circ.nodes.(i) then incr n
  done ; !n 



(* 
 * Create and set evidence 
 *)

let create_evidence circ =
  Array.map (fun n -> Array.make n log_one) circ.schema

let create_evidence_s schema =
  Array.map (fun n -> Array.make n log_one) schema

let ev_true e var value =
  for i = 0 to Array.length e.(var) - 1; do
    if i <> value then
      e.(var).(i) <- log_zero
  done

let ev_true2 e var value =
  for i = 0 to Array.length e.(var) - 1; do
    if i <> value then
      e.(var).(i) <- log_zero
    else e.(var).(i) <- log_one
  done

let ev_false e var value = 
  e.(var).(value) <- log_zero

let print_ev e = 
  for i = 0 to Array.length e - 1 do
    for j = 0 to Array.length e.(i) - 1 do 
      printf "%f " e.(i).(j)
    done ;
    printf "| "
  done ;
  printf "\n"

let set_evidence values circ e =
  (* Set literal values using evidence *)
  let set_one n =
    let (var,value) = var_details n in
    if n.id >= 0 then
      values.(n.id) <- e.(var).(value) in
  Array.iter set_one circ.flat_vnodes 

let conditions_to_ev schema cl =
  let e = create_evidence_s schema in
  let set_value (sense, var, value) = 
    (if sense then ev_true else ev_false) e var value in
  Array.iter set_value cl ;
  e

let example_to_ev schema x =
  let e = create_evidence_s schema in
  Array.iteri (fun i v -> if v >= 0 then ev_true e i v) x;
  e



let ev_union circ e1 e2 = 
  let e = create_evidence_s circ.schema in
  for i = 0 to Array.length e1 - 1 do
    for j = 0 to Array.length e1.(i) - 1 do
        if e1.(i).(j) <> e2.(i).(j) then
          e.(i).(j) <- log_zero
        else
          e.(i).(j) <- e1.(i).(j)
    done
  done;
  e


(* True when evidence e1 and e2 are not mutually exclusive. *)
let ev_intersect e1 e2 =
  try 
    for i = 0 to Array.length e1 - 1 do
      let ok = ref false in
      for j = 0 to Array.length e1.(i) - 1 do
        if e1.(i).(j) <> log_zero && e2.(i).(j) <> log_zero then
          ok := true
      done ;
      if not !ok then raise Not_found
    done ;
    true
  with Not_found -> false

let ev_subset e1 e2 = 
  if ev_intersect e1 e2 then
  begin

    let ok = ref true in
    for i = 0 to Array.length e1 - 1 do

      for j = 0 to Array.length e1.(i) - 1 do
        if e1.(i).(j) = log_zero && e2.(i).(j) <> log_zero then ok := false
      done ;
    done ;
    !ok
  end
  else false
let ev_intersect2 e1 e2 =
  try 
    for i = 0 to Array.length e1 - 1 do
      let ok = ref false in
      for j = 0 to Array.length e1.(i) - 1 do
        if ((e1.(i).(j) = log_zero && e2.(i).(j) <> log_zero) || (e1.(i).(j) = log_zero && e2.(i).(j) = log_zero)) then
          ok := true
      done ;
      if not !ok then raise Not_found
    done ;
    true
  with Not_found -> false

(* True when conditions cl1 and cl2 are not mutually exclusive. *)
let cond_intersect schema cl1 cl2 =
  let e1 = conditions_to_ev schema cl1 and e2 = conditions_to_ev schema cl2 in
  ev_intersect e1 e2


(*
 * Circuit features
 *)


(* TODO -- eventually, maybe this should be in a different module. 
 * What's the right way to generalize both BN and AC features?
 *)
type condition = bool * int * int 

type feature = {acnode: node; 
                mutable weight: float;
                cond: condition list;
                ev: float array array}


let feature_contains_var f v = 
  let var = var_var v in
  let c_vars = List.map (fun c-> let (sense,var',value') = c in var') f.cond in
  List.exists (fun v'-> (v' = var) ) c_vars  


let feature_node f = f.acnode
let feature_value f = const_value (feature_node f)
let set_feature_value f wt = set_const (feature_node f) wt

let input_features_lex circ lexbuf =
  let fl = Mn.input_features_lex lexbuf in
  let rev_id = node_input_index circ.size in
  let pf2f pf = 
    (* TODO: Read in multiple nodes...? *)
    let acnode = circ.nodes.(rev_id pf.Mn.Factor.weight_id) in
    let weight = pf.Mn.Factor.weight in
    let cond = pf.Mn.Factor.cond in
    let ev = conditions_to_ev circ.schema cond in
    {acnode=acnode; weight=weight; cond=Array.to_list cond; ev=ev} in
  List.map pf2f fl

let input_features circ chan =
  let lexbuf = Lexing.from_channel chan in
  input_features_lex circ lexbuf
 

let string_of_feature f = 
  let string_of_cond cond = let (sense, var, value) = cond in  sprintf " %cv%d_%d" (if sense then '+' else '-') var value in
  let res = List.fold_left (fun s c -> s ^ (string_of_cond c)  ) "" f.cond in 
  sprintf "%0.5f: %s" f.weight res

let output_feature out f =
  let print_cond (sense, var, value) =
    fprintf out " %cv%d_%d" (if sense then '+' else '-') var value in
  fprintf out "%.6g" (exp (feature_value f));
  List.iter print_cond f.cond;
  fprintf out "\n" 

let output_feature_with_id out rev_id f =
  fprintf out "%d " (rev_id (feature_node f));
  output_feature out f

let output_feature_list out rev_id fl =
  List.iter (output_feature_with_id out rev_id) fl;
  fprintf out "EOF\n"

let output_with_features out circ fl =
  output out circ;
  output_string out "\n";
  output_feature_list out (node_output_id circ.size) fl

let output_root_with_features out schema r fl =
  output_root out schema r;
  output_string out "\n";
  let numnodes = List.length (root_to_list r) in
  output_feature_list out (node_output_id numnodes) fl

let output_features out root fl = 
  let numnodes = List.length (root_to_list root) in
  output_feature_list out (node_output_id numnodes) fl


(*
 * Circuit evaluation functions
 *)

let create_scratch circ = 
  (* order circ ; *)
  let a1 = Array.make circ.size 0.0 in
  let a2 = Array.make circ.size [||] in
  for i = 0 to circ.size - 1 do
    a2.(i) <- Array.of_list (List.map id circ.nodes.(i).children)
  done ;
  (a1, a2)

let node_logvalue tscratch node cids =
  match node.details with
      TimesNode -> 
        let total = ref 0.0 in
        for j = 0 to Array.length cids - 1 do
          total := tscratch.(cids.(j)) +. !total
        done ;
        !total
    | PlusNode -> 
      let cvals = Array.map (Array.get tscratch) cids in 
      Ext.alogsumexp cvals 
    | ConstNode w -> w
    | VarNode (var, value) -> tscratch.(node.id)
    (* OLD: We used to pass in an evidence vector.
    | VarNode (var, value) -> 
        let xval = x.(var) in
        if xval < 0 || xval == value then 0.0 
        else neg_infinity *)
    | NullNode -> assert false

let logvalues scratch circ =
  let (values, all_cids) = scratch in
  assert (Array.length values == circ.size) ;
  let all_nodes = circ.nodes in
  for i = circ.size - 1 downto 0 do
    values.(i) <- node_logvalue values all_nodes.(i) all_cids.(i) 
  done ;
  values

let logprob_ev tscratch circ e =
  let (values, all_cids) = tscratch in
  set_evidence values circ e ;
  let nvals = logvalues (values, all_cids) circ in
  ignore(values.(0));
  nvals.(0)


let logprob_x tscratch circ x =
  (* Set literal values using an example: Each var has one or any value *)
  let (values, all_cids) = tscratch in
  let set_one n =
    if n.id > 0 then begin
      let (var,value) = var_details n in
      if x.(var) = value || x.(var) < 0 then
        values.(n.id) <- log_one
      else
        values.(n.id) <- log_zero 
    end in
  Array.iter set_one circ.flat_vnodes ;
  ignore (logvalues (values, all_cids) circ) ;
  values.(0)

let compute_z tscratch circ = 
  let e = create_evidence circ in
  logprob_ev tscratch circ e



let compute_z_e circ e= 
  let (ev_values, ev_cid) =  create_scratch circ in
  set_evidence ev_values circ e;
  let ev_cs = (ev_values, ev_cid) in
  let ev_values = logvalues ev_cs circ in
  let logz = ev_values.(0) in
  logz



(* Alternate method for evaluating the circuit recursively.
 * The main advantage of this method is that it doesn't require
 * that the circuit be ordered.
 *
 * CURRENTLY UNUSED.  Could be useful for debugging.
 *)

let create_rec_scratch circ = Array.make circ.size None

let rec 
  node_logvalue_rec ar x node =
    let i = node.id in
    match ar.(i) with 
      Some(ll) -> ll
    | None -> let ll = compute_node_logvalue_rec ar x node in
                  ar.(i) <- Some(ll) ; ll
and
  compute_node_logvalue_rec ar x node =
    let getll c = node_logvalue_rec ar x c in
    let cvals () = List.map getll node.children in
    match node.details with
        TimesNode -> List.sumf_map getll node.children
      | PlusNode -> logsumexp (cvals ())
      | ConstNode w -> w
      | VarNode (var, value) -> 
          if x.(var) = value || x.(var) < 0 then 0.0 
          else neg_infinity
      | NullNode -> assert false

let loglikelihood_rec rscratch circ x =
  Array.fill rscratch 0 circ.size None ;
  node_logvalue_rec rscratch x circ.root

let compute_z_rec rscratch circ = 
  let x = Array.make (Array.length circ.schema) (-1) in
  loglikelihood_rec rscratch circ x


(*
 * Prune circuit features and circuits. 
 *)

(* Test to see if an instance satisfies a condition *)
let c_satisfied x (sense, var, value) = 
  x.(var) < 0 || (sense == (x.(var) == value)) 

(* Test to see if an instance satisfies all conditions *)
let f_satisfied x f = List.for_all (c_satisfied x) f.cond 

let prune_features fl ev =
  (* Remove contradictory circuit features *)
  let fl = List.filter (f_satisfied ev) fl in 
  (* In theory, the following code should only speed things up.
   * In practice, it sometimes slows them down!  Why?  Memory locality?
   *)
  (* Remove redundant conditions *)
  let c_unknown (sense, var, value) = ev.(var) < 0 in
  let prune_conditions f = 
    {f with cond=List.filter c_unknown f.cond} in
  let fl = List.map prune_conditions fl in 
  (* Remove empty features (those that are always true, given evidence) *)
  let fl = List.filter (fun f -> f.cond <> []) fl in
  fl


let prune_for_evidence_with_features circ fl ev =
  let zero = create_const log_zero in
  let one  = create_const log_one in
  let new_node = NMap.create 100 in
  let is_zero n = n.hashid == zero.hashid in
  let is_nonzero n = n.hashid != zero.hashid in
  let is_nonone n  = n.hashid != one.hashid in 

  (* Prune features first *)
  let fl = prune_features fl ev in

  (* Keep parameter nodes associated with remaining features. *)
  let fset = NSet.create 100 in
  List.iter (fun f -> NSet.add fset f.acnode) fl;
  let is_nonparam_const n = 
    is_const n && not (NSet.mem fset n) in

  let prune_one n =
    (* printf "Old node: "; Node.print_endline n; flush stdout; *)
    let n' = begin
    (* Get new children first. *)
    let children = List.map (NMap.find new_node) n.children in
    match n.details with
      TimesNode -> 
        let children = List.filter is_nonone children in 
        if List.exists is_zero children then
          zero
        else if List.length children = 0 then
          one
        else if List.length children = 1 then
          List.hd children 
        else begin
          (* Merge all non-parameter constant nodes into a single constant. *)
          let (const_ch, nonconst_ch) = 
            List.partition is_nonparam_const children in
          if List.length const_ch < 2 then
            create_times children
          else
            let n = create_const (List.sumf_map const_value const_ch) in
            if List.length nonconst_ch = 0 then
              n
            else
              create_times (n :: nonconst_ch)
        end
    | PlusNode ->
        let children = List.filter is_nonzero children in 
        if List.length children = 0 then
          zero
        else if List.length children = 1 then
          List.hd children
     (* else if List.for_all is_nonparam_const children then
          create_const (logsumexp (List.map const_value children)) *)
        else
          create_plus children
    | VarNode(var,value) ->
        if ev.(var) < 0 then 
          n
        else if ev.(var) = value then 
          one
        else 
          zero
    | ConstNode w -> n
    | NullNode -> assert false 
    end in
    (* printf "New node: "; Node.print_endline n'; flush stdout; *)

    (* Save in hash *)
    NMap.add new_node n n' in

  Array.rev_iter prune_one circ.nodes;
  circ.root <- NMap.find new_node circ.root;
  rebuild circ;
  fl

(* Prune for evidence, ignoring any circuit features *)
let prune_for_evidence circ ev =
  ignore (prune_for_evidence_with_features circ [] ev)



(*
 * Derivatives
 *)

type deriv_scratch = {dr: float array;
                      vr: float array;
                      vr_nz: float array;
                      bit1: bool array;
                      bit2: bool array;
                      cids: (int array) array }

let create_deriv_scratch circ =
  let (vr, cids) = create_scratch circ in
  {dr = Array.make circ.size 0.0;
   vr = vr;
   vr_nz = Array.make circ.size 0.0;
   bit1 = Array.make circ.size false;
   bit2 = Array.make circ.size false;
   cids = cids}

let node_logvalue_dtopo ds i node =
  let cids = ds.cids.(i) in
  let vr = ds.vr in
  match node.details with
    TimesNode -> 
      let total = ref 0.0 in
      let total_nz = ref 0.0 in
      let numzero = ref 0 in
      for j = 0 to Array.length cids - 1 do
        let id = cids.(j) in
        total := vr.(id) +. !total ;
        if vr.(id) = log_zero then incr numzero 
        else total_nz := !total_nz +. vr.(id)
      done ;
      ds.bit1.(i) <- !numzero > 0;
      ds.bit2.(i) <- !numzero = 1;
      ds.vr_nz.(i) <- !total_nz;
      vr.(i) <- !total
      (* ; if ds.bit2.(i) then 
        printf "vr_nz.(%d) = %f; %f\n" i ds.vr_nz.(i) vr.(i) *)
  | PlusNode -> 
      let cvals = Array.map (Array.get vr) cids in 
      vr.(i) <- alogsumexp cvals
  | ConstNode w -> 
      vr.(i) <- w
  | VarNode (var, value) -> 
      () (* vr.(i) <- vr.(i) *)
  | NullNode -> assert false 

let d_upward_pass circ ds e =
  (* This fills the vr array with the circuit values *) 
  set_evidence ds.vr circ e ;
  for i = circ.size - 1 downto 0 do
    ds.bit1.(i) <- false ;
    ds.bit2.(i) <- false ;
    ds.vr_nz.(i) <- 0.0 ; 
    ds.dr.(i) <- 0.0 ;  
    node_logvalue_dtopo ds i circ.nodes.(i)
  done 

let d_downward_pass circ ds = 
  let dr = ds.dr and vr = ds.vr and bit1 = ds.bit1 and bit2 = ds.bit2 in
  dr.(0) <- log_one ;
  for v = 1 to circ.size - 1 do
    let dr_incr parent =
      let p = parent.id in
      match parent.details with 
        PlusNode  -> dr.(p)
      | TimesNode -> 
          if not bit1.(p) then 
            (if vr.(p) = log_zero then printf "ERROR: bit1 is false!\n";
            dr.(p) +. vr.(p) -. vr.(v))
          else if bit2.(p) && vr.(v) = log_zero then 
            dr.(p) +. ds.vr_nz.(p) 
          else 
            log_zero 
      | _ -> failwith "Unexpected parent node type in d_downward_pass.\n" in
    dr.(v) <- logsumexp (List.map dr_incr circ.nodes.(v).parents)
  done

(* For debugging *)
let __print_ds circ ds = 
  for i = 0 to circ.size - 1 do
    printf "vr: %f; dr: %f\n" ds.vr.(i) ds.dr.(i)
  done


let get_derivatives_raw circ ds e =
  (* DEBUG 
  output stdout circ ; 
  print_ev e ; *)
  d_upward_pass circ ds e ;
  d_downward_pass circ ds 
  (* DEBUG
  ; for i = 0 to circ.size - 1 do
    printf "%d\t%f\t%f\n" i (exp ds.vr.(i)) (exp ds.dr.(i))
  done *)


let get_logmarginals circ ds ev =
  let e = example_to_ev circ.schema ev in
  get_derivatives_raw circ ds e;
  let logz = ds.vr.(0) in
  let dr = ds.dr in
  let node_to_prob n = 
    if n.id >= 0 then dr.(n.id) -. logz else log_one in
  let m = Array.map (Array.map node_to_prob) circ.vnodes in
  (* Apply evidence *)
  for i = 0 to Array.length ev - 1 do
    if ev.(i) >= 0 then
      for j = 0 to Array.length m.(i) - 1 do
        m.(i).(j) <- if j == ev.(i) then log_one else log_zero
      done
  done;
  m

let get_marginals circ ds e =
  let lm = get_logmarginals circ ds e in
  Array.map (Array.map exp) lm

(*
 * MPE Inference 
 *)

let node_logvalue_mpe ds i node =
  let cids = ds.cids.(i) in
  let vr = ds.vr in
  let cvals = Array.map (Array.get vr) cids in
  match node.details with
    TimesNode -> 
      vr.(i) <- Array.sumf cvals
  | PlusNode -> 
      vr.(i) <- Array.max cvals
  | ConstNode w -> 
      vr.(i) <- w
  | VarNode (var, value) -> 
      () (* vr.(i) <- vr.(i) *)
  | NullNode -> assert false 

let mpe_upward_pass circ ds e =
  (* This fills the vr array with the max circuit values *) 
  set_evidence ds.vr circ e ;
  for i = circ.size - 1 downto 0 do
    ds.dr.(i) <- 0.0 ;  
    node_logvalue_mpe ds i circ.nodes.(i)
  done 

let rec mpe_downward_pass circ ds i =
  let cids = ds.cids.(i) in
  ds.dr.(i) <- 1.0;
  let cvals = Array.map (Array.get ds.vr) cids in
  match circ.nodes.(i).details with 
    PlusNode -> 
      let ci = Array.argmax cvals in
      mpe_downward_pass circ ds cids.(ci)
  | TimesNode ->
      Array.iter (mpe_downward_pass circ ds) cids
  | _ -> ()
  
let get_mpe circ ds ev =
  let e = example_to_ev circ.schema ev in
  mpe_upward_pass circ ds e;
  mpe_downward_pass circ ds 0;
  let nodeval n = 
    if n.id >= 0 then ds.dr.(n.id) else 0.0 in
  let varvals = Array.map (Array.map nodeval) circ.vnodes in
  let mpe = Array.map Array.argmax varvals in
  mpe


(*
 * Reading files
 *)

let input_schema chan =
  let s = input_line chan in
  let s' = Str.global_replace (Str.regexp "[()]") "" s in
  let intstrs = Str.split (Str.regexp " ") s' in
  Array.of_list (List.map int_of_string intstrs)
 
let parse channel =
  let lexbuf = Lexing.from_channel channel in
  AcParser.pcircuit AcLexer.lexer lexbuf

let load channel = of_parse (parse channel)

exception NonConstantFeatureWeight of int

let load_with_features channel =
  let lexbuf = Lexing.from_channel channel in
  let circ = of_parse (AcParser.pcircuit AcLexer.lexer lexbuf) in
  let fl = input_features_lex circ lexbuf in

  (* Input validation *)
  let check_for_const f =
    if not (is_const (feature_node f)) then
      raise (NonConstantFeatureWeight (id (feature_node f))) in
  List.iter check_for_const fl;
  (circ, fl)
