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
open Condcirc
open Condspn
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
let ratio = ref 0.001
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
let l1 = ref 0.1
let l2 = ref 1.0
let override = ref false
let indicators = ref [||]
let evnodes = ref [||]

let usage = "Usage: crflearn -i <data> -s <schema> -ev <evidence> -o <output> [...]"
let args = Arg.align
  ([

    ("-i", Arg.Set_string datafile, " Training data file");
    ("-s",  Arg.Set_string schemafile,  " Schema file");
    ("-o", Arg.Set_string output_dir, " Output ac file");
	  ("-ev",  Arg.Set_string evfile,  " Evidence file"); 
		(*
    ("-va", Arg.Set_string vfile, " Validation file(optional)"); *)
	  ("-l1",  Arg.Set_float l1,  " L1 prior constant"); 
	  ("-l2",  Arg.Set_float l2,  " L2 prior constant"); 
    ("-l",  Arg.Set_float lambda,  " EM clustering penalty");
    (* ("-ps",  Arg.Set_float split_penalty,  " Split penalty"); *)
    ("-k",  Arg.Set_int num_part,  " Max sum nodes' cardinalities");
    (*("-sd",  Arg.Set_float stddev,  " Standard Deviation Prior"); *)
    ("-cp",  Arg.Set_int concurrent_thr,  " Number of concurrent process");
(*  ("-net", Arg.Set_string network, " Dataset network name");
    ("-om",  Arg.Set_string outputmodel,  " Output model file");
    ("-of",  Arg.Set_string outputinference,  " Output inference file"); *)
    ("-vth",  Arg.Set_float cut_thresh,  " Vertical cut thresh");
(*  ("-ext", Arg.Set_int max_extension, " Maximum number of node extensions");  
 	  ("-minl1", Arg.Set_float minl1, " Minimum value for components' L1 priors");
  	("-minedge", Arg.Set_float minedge, " Minimum edge budget for components");
  	("-minps", Arg.Set_float minedge, " Minimum split penalty for components"); *)
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


let clear_all_caches () = 
  vlogf "clear caches\n"; flush_all();
  for i=0 to (Array.length !node_array) - 1 do
    !node_array.(i).infcached <- false;
    !node_array.(i).logpTemp <- [||]
  done
exception NegParamException

(** z = \sum_{c'} exp(w_{c'}^T x) *)
let compute_z x w_mat =
	let wn = Array.length w_mat.(0) in
  let z = Array.fold_left (fun s wc-> s +. exp(adotprod_autobias x wc ) ) 0.0 w_mat in
  z 
    
exception IncorretProb

(** Pr(C=c|X=x, \theta) = \frac{exp(\theta_c^T x)}{z} *)
let pr_c_given_x x theta_c z =
  let num = exp ( adotprod_autobias x theta_c ) in
  let value= num /. z in
	if value < 0.0 then raise IncorretProb;
	value





let create_softmax w_mat =

	vlogf "Create softmax\n";
	let prior = 0.001 in
	let evnums = Array.length  !evnodes in
  let k = Array.length w_mat in
	let mins = Array.map (fun wc-> Array.min(wc)) w_mat in
	let global_min = (Array.min mins) -. prior  in 
  (*assert (Array.length w_mat.(0) = (evnums + 1));
  *)

  let expnode_ar = Array.make k null_node in
 
	for c = 0 to k - 1 do

		let w_c = w_mat.(c) in 
		vlogf "c=%d," c; 
    Array.iter (fun w -> vlogf "%f " (w-.global_min)) w_c;

		let prod_list = ref [] in 
    for i = 0 to evnums - 1 do
			let param = (w_c.(i) -. global_min) in 
			(*if (param <= 0.0 ) then exit 1; *)
		
      let wnode = Condcirc.create_const (log param) in
      let prod_node = Condcirc.create_times [wnode; !evnodes.(i)] in
      prod_list := prod_node::!prod_list;
    done;
		

		let param = (w_c.(evnums) -. global_min) in 
		
		vlogf "log param:%f\n" (log param);
		(*if (param <= 0.0 ) then exit 1; *)
		let bias = Condcirc.create_const (log param) in

    let plusnode = Condcirc.create_plus (bias::!prod_list) in
    let expnode = Condcirc.create_exp plusnode in
    expnode_ar.(c)<-expnode 
  done;
 	let partition_node = Condcirc.create_inverse (Condcirc.create_plus (Array.to_list expnode_ar))
	in
  (expnode_ar, partition_node) 



let learn_leaves node =		 	
			let numf = (float_of_int (Array.length node.data)) +. 2.00 in
      let ones = (float_of_int (Array.fold_left (fun s x-> s + x.(0) ) 0 node.data)) +. 1.00 in 
      let p1 = ones  /. numf in
      let zeors = numf -. ones in
      let p0 = zeors /. numf in
			let p1node = Condcirc.create_const (log p1) in
      let p0node = Condcirc.create_const (log p0) in
      let v1 = !indicators.(snd node.schema.(0)).(1) in
      let v0 = !indicators.(snd node.schema.(0)).(0) in
      let times1 = Condcirc.create_times [v1; p1node] in
      let times0 = Condcirc.create_times [v0; p0node] in
      let plus = Condcirc.create_plus [times0; times1] in
      plus
				
			

let learn_leaves2 node = 
			let dim = fst node.schema.(0) in
			let w = Condspn.learn_univariate_softmax_w node !l1 !l2 in
			let (expnodes, partnode) = create_softmax w in

			let prodlist = ref [] in 
			
			let var_id = snd node.schema.(0) in
			vlogf "var_id: %d\n" var_id;
			for i = 0 to dim - 1 do
      	let v = !indicators.(var_id).(i) in
      	let prodnode = Condcirc.create_times [v; expnodes.(i); partnode] in
				prodlist := prodnode::!prodlist
      done;
			let plus = Condcirc.create_plus !prodlist in
      plus
     



let rec build_spn node cut_thr depth= 
	vlogf "build_spn node.id:%d %.4f\n" node.id cut_thr; 
  let varnum = Array.length node.schema in
  let samplesize = Array.length node.data in 
  if !num_part > 1 then begin
    if varnum > 1 then begin
      try 
       let (hNodeList, wmat)  = Condspn.h_split_cond node !num_part  !lambda !concurrent_thr !ratio  !l1 !l2 in
       vlogf "size of h node list: %d\n" (List.length hNodeList);
       let s = List.length hNodeList in
       let ar = Array.range s in

       flush_all();
       let sum_children = ref [] in
       let param_list = ref [] in
       let k = List.length hNodeList in
       let (expnodes, partnode) = create_softmax wmat in
       for i=0 to k - 1 do
         try 
           
           let n = List.nth hNodeList i in
       
       
           let nsamplesize = Array.length n.data in 
           let weight = (float_of_int nsamplesize) /. (float_of_int samplesize) in
           Array.iter (fun e->let z = compute_z e wmat in printf "%d: " i; List.iteri (fun b x-> let p = pr_c_given_x e wmat.(b) z in printf "%0.3f " p) hNodeList; printf "%0.3f\n" weight) n.ev;
          (*
           let ndepth = depth - 1 in

           let nsamplesize = Array.length n.data in 
           *)
           let vNodeList = Condspn.v_split_gtest_cond n cut_thr in
           let circ_nodes = List.map (fun n->build_spn n cut_thr (depth - 1)) vNodeList in
           let times = Condcirc.create_times circ_nodes in
          
           (*
           let nsamplesize = Array.length n.data in 
           let weight = (float_of_int nsamplesize) /. (float_of_int samplesize) in
           (*vlogf "%f " weight*;*)
           let param_node = create_const (log weight) in
           
           
           let subtree = Condcirc.create_times [param_node; times] in 
            *)
           
           let subtree = Condcirc.create_times [expnodes.(i); partnode; times] in 					
           
           

           sum_children := subtree::!sum_children

         with VSplitNotPossible -> begin
           vlogf "V-split not possible\n"; 
             
           let n = List.nth hNodeList i in
           let nsamplesize = Array.length n.data in 
          (*   
           let weight = (float_of_int nsamplesize) /. (float_of_int samplesize) in
           let param_node = Condcirc.create_const (log weight) in
         *)
           let plus = build_spn n cut_thr (depth-1) in
             
           let subtree = Condcirc.create_times [expnodes.(i); partnode; plus] in 
           (*let subtree = Condcirc.create_times [param_node; plus] in *)
           sum_children := subtree::!sum_children
         end 
       done;
       Condcirc.create_plus !sum_children

       with HSplitNotPossible-> begin 
         vlogf "h-split not possible\n"; 
         let n_thr = (cut_thr +. (cut_thr/. 10.0)) in
         try
           let vNodeList = Condspn.v_split_gtest_cond node n_thr in
           let circ_nodes = List.map (fun n->build_spn n n_thr (depth - 1)) vNodeList in
           let times = Condcirc.create_times circ_nodes in
           times
         with VSplitNotPossible-> begin 
           vlogf "v-split not possible. \n";
           build_spn node n_thr (depth-1) 
         end
       end
     end
     else begin
       
       vlogf "Learn univariate softmax leaf\n";	
       learn_leaves node 
     end
   end else begin
     let vNodeList = Condspn.v_split_gtest_cond node 1000.0 in
     let circ_nodes = List.map (fun n->learn_leaves n) vNodeList in
     let times = Condcirc.create_times circ_nodes in
     times 
   end


let minisleep (sec: float) =
    ignore (Unix.select [] [] [] sec)


let learn_cond_spn_structure data schema_ar ev evnum = 
  (*let schema_ar = Array.of_list schema in*)
  let i_schema = Array.augment schema_ar in
  indicators := Condcirc.make_vnodes schema_ar;
	evnodes := Condcirc.make_evnodes evnum;
  let indicators_l = Array.map Array.to_list !indicators in
  
	let root = Condspn.create_leaf data i_schema ev evnum in
	(*
	let w = Condspn.learn_univariate_softmax_w root 0.5 0.5 in
	
	let (expnodes, partnode) = create_softmax w in

  Condcirc.of_graph schema_ar !indicators indicators_l !evnodes partnode 
	*)	
  let acroot = build_spn root !cut_thresh !max_extension in

  (Condcirc.of_graph schema_ar !indicators indicators_l !evnodes acroot, root) 






(*
let rec update_params node = 
  if node.nodetype == PlusNode  then
  begin
    for i = 0 to (Array.length node.children) - 1 do
      node.params.(i) <- log (float_of_int (Array.length node.children.(i).data)) -. log (float_of_int(Array.length node.data));
      (*node.counts.(i) <- Array.length node.children.(i).data;*)
      update_params node.children.(i)
    done
  end else if node.nodetype == TimesNode then
  begin
    for i = 0 to (Array.length node.children) - 1 do
      update_params node.children.(i)
    done
  end else if node.nodetype == LeafNode then 
  begin
    ignore()
  end else raise NodeTypeError

*)


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
      Printf.printf "Optimizing AC node: %d in process: %d\n" node.id (Unix.getpid());
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
      Condcirc.output_with_features channel !comps.(i) !feature_lists.(i);
      exit 1
    end 
    | _ -> begin
      ignore();
    end;
  pid

*)

(*
let rec prune node =
  printf "pruning node.id = %d\n" node.id;
  flush_all();
  if node.nodetype == ACNode then 
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
      let newcounts = ref [||] in 
      Array.iteri (fun i n-> if n.id != node.id then 
          begin
          sibling := Array.append !sibling [|n|];
          newcounts := Array.append !newcounts [|parent.counts.(i)|];
          newparams := Array.append !newparams [|parent.params.(i)|]
          end ) sibling_and_me;
    
      parent.params <- !newparams;
      parent.counts <- !newcounts;
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
      let newcounts = ref [||] in 
      Array.iteri (fun i n-> if n.id != node.id then 
		flush_all();
                      begin
                        sibling := Array.append !sibling [|n|];
                        newcounts := Array.append !newcounts [|parent.counts.(i)|];
                        newparams := Array.append !newparams [|parent.params.(i)|]
                      end ) sibling_and_me;
    
      parent.params <- !newparams;
      parent.counts <- !newcounts;
      parent.children <- !sibling;
      node.parents.(0) <- parent
    end
  end else if node.nodetype == PlusNode then
  begin
    printf "Should node be here\n";
  end 

*)




(*
let optimize_params root data =
  let it = ref 10 in
  let change = ref 100.0 in
  let prev_llg = ref (-.1000.0) in

  scratches := Array.map (Condcirc.create_scratch) !comps;
  logzs := Array.map2 (fun s c->Condcirc.compute_z s c) !scratches !comps;
  
  while (!it < 10) && !change > 0.01 do
    incr it;
    printf "Optimizing round %d previous change : %f\n" !it !change;
    Array.iter (fun n-> n.data <- [||]) !node_array;
    let probs = Array.map ( fun d-> let logp = update_spac_params root d in assign_sample d root; logp) data in
    update_params root;
    let llg_data = Array.sumf probs in
    let llg_avg = llg_data /. float_of_int(Array.length probs) in
    change := absf(llg_avg -. !prev_llg);
    printf "new change: %f\n" !change;
    prev_llg := llg_avg;
    printf "llg_avg on data -- round %d: %f\n" !it llg_avg; 
    
    let child_list = ref [] in
    for i=0 to (Array.length !node_array) - 1  do
      let node = !node_array.(i) in
      if node.nodetype == ACNode then
      begin
        let resize = Array.length node.data in 
        printf "Size of reassigned data = %d\n" resize;
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
    printf "Loading acnodes ...... \n";
    print_spn !node_array.(0); 
    flush_all();
    load_comps();
  
    printf "end of optimizing acnodes -----------------\n";
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

(*
let answer_query_cached root q_i =
  let logp = answer_spac_cached root q_i in
  logp
*)


(*
let rec prune_spac node root valid=
  (match node.nodetype with ACNode->ignore()
      | TimesNode-> Array.iter (fun n->prune_spac n root valid )node.children
      | PlusNode-> Array.iter (fun n->prune_spac n root valid )node.children | NullNode-> ignore() );
  if node.nodetype == ACNode then
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
      node.nodetype <- ACNode;
      let validlogprobs = Array.mapi (fun i x-> answer_query_cached root i) valid in
      let llg_valid = Array.sumf validlogprobs in
      let avg_valid_new = llg_valid /. nv in
      if avg_valid_new >= avg_valid then
      begin
        printf "prune node id: %d  old llg:%f new llg:%f\n" node.id avg_valid avg_valid_new;
        ignore()
      end else begin
        node.nodetype <- oldtype;
        printf "prune node id: %d  old llg:%f new llg:%f not possible\n" node.id avg_valid avg_valid_new
      end
    end
  end
*)   


exception InsufficientArguments


(*
let preload_query_results queries = 
  let n = Array.length queries in
  logpTemp := Array.make (Array.length !comps) [||];  
  for i = 0 to (Array.length !node_array) - 1 do
    let node = !node_array.(i) in 
    if node.nodetype == ACNode then 
    begin
      let result = Array.make n 0.0 in
      let indeces = Array.map (snd) node.schema in
      let myqueries = Array.takeCol queries indeces in
  
      let ac_index = node.ac_index in
      let circ = !comps.(ac_index) in
      let scratch = !scratches.(ac_index) in 
      let logz = !logzs.(ac_index) in

      for k=0 to n - 1 do
        let logp = Condcirc.logprob_x scratch circ myqueries.(k) in
        result.(k) <- (logp -. logz) 
      done;
      !logpTemp.(ac_index) <- result
    end
  done

*)




(* test for clustering *)
(*
let _=
  Arg.parse args ignore usage;
  if !datafile = "" || !network = "" then (Arg.usage args usage; raise InsufficientArguments);
    
  let data = Datafile.load_data_ar !datafile in
  let schema = Datafile.load_schema !schemafile in
  let valid = Datafile.load_data_ar !vfile in
  let t1 = Sys.time() in
  printf "learning the spac structure .... \n";
  let i_schema = Array.mapi (fun i x->(x,i)) (Array.of_list schema) in
  let data_parts = Partition.horizontal_cluster 0 data valid i_schema !num_part !lambda 1 in  
  ignore(data_parts)
*)
(* main main *) 

let endswith s1 s2 =
  let re = Str.regexp (Str.quote s2 ^ "$")
  in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false


let remove_nonempty_dir dirname = 
  let filelist = Sys.readdir dirname in 
  Array.iter (fun f->Sys.remove (String.concat dir_sep [dirname; f]) )filelist;
  ignore()

let main() =
  seed := ((int_of_float (Unix.time()) )  mod  1000); 
  Arg.parse args ignore usage;
  start_time := Unix.gettimeofday();
  common_log_init();

  Random.init !seed;
  if !evfile = ""  || !datafile = "" ||  !schemafile = "" || !output_dir = "" then (Arg.usage args usage; exit(-1));
  if not (check_suffix !output_dir ".ac") then begin nlogf "Error: output model should end with .ac \n"; exit 1 end;


 (*
  let odir = sprintf "%s" !output_dir in
    if Sys.file_exists odir then 
    begin
      if not !override then begin
        printf "Error: directory %s exists\n"!output_dir; exit 1 end
      else begin
        remove_nonempty_dir !output_dir
      end
    end
    else Unix.mkdir odir 0o755;
 *)

  
  let t1 = Unix.gettimeofday() in
    

  let data = Data.load_data_ar !datafile in

  let schema = Data.load_schema !schemafile in
  
  let ev = Data.load_evidence_ar !evfile in

  let evnums = Array.length ev.(0) in 
  
  vlogf "learning the spac structure .... \n";
  let (circ,sproot) = learn_cond_spn_structure data schema ev evnums in
	Condspn.print_spn sproot;
  let scratch = Condcirc.create_scratch circ in
  max_depth := 0;

  if !output_dir <> "" then begin
    let out_ch = open_out !output_dir in
    Condcirc.output out_ch circ;
    close_out out_ch 
  end;





  vlogf "Calculating data llg\n"; flush_all(); 
  let logprobs = Array.map2 (fun y x-> Condcirc.logprob_y scratch circ y x) data ev in

  let llg_data = Array.sumf logprobs in
  let n = float_of_int ( Array.length data) in
  let avg_data = llg_data /. n in

  
  vlogf "Calculating valid llg\n"; flush_all();  
  
  let avg_valid = ref 0.0 in


  let t2 = Unix.gettimeofday() in
  
  nlogf "Time: %f\n" (t2 -. t1);

  nlogf "Average data likelihood: %f\n" avg_data;
	vlogf "Time spent on clustering: %f\n" (Timer.elapsed "clustering_time");
	vlogf "Time spent on merging data parts: %f\n" (Timer.elapsed "data_parts_merge");
  let filelist = Sys.readdir "." in
  Array.iter ( fun f -> if endswith f ".cluster" || endswith f ".data" || endswith f ".schema" || endswith f ".log" then Sys.remove f   ) filelist; 
  ignore()


let extend() = ignore()


let _=
   main()  
