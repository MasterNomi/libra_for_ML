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
open Ext
open Str
open Circuit 
open Unix 
open Random
open Bn
(* Globals used for command line parameters *)

let network = ref ""
let acfile = ref ""
let qfile = ref ""
let vfile = ref ""
let evfile = ref ""
let datafile = ref ""
let prior = ref 0.0001
let num_comps = ref 4
let schemafile = ref ""
let outfile = ref ""
let outmodel = ref "" 
let output_dir = ref ""
let seed = ref 0 
(*((int_of_float Unix.time() )  mod  1000) 
*)
let override = ref false

let usage = "Usage: mtlearn -i <data> -o <output> [...]"
let args = Arg.align
  ([
    ("-i", Arg.Set_string datafile, " Training data file");
    ("-s",  Arg.Set_string schemafile,  " Data schema (optional)");
    ("-o", Arg.Set_string output_dir, " Output SPN directory");
    ("-k",  Arg.Set_int num_comps,  " Number of component [4]");
    ("-seed",  Arg.Set_int seed,  " Random seed");
    ("-f",  Arg.Set override,  " Force to override output SPN directory");
  ]
    @ common_arguments)


let soft_counts schema data comp_probs k=
  (* Collect all pairwise counts *)
  let numvars = Array.length schema in
  let marg_counts = Array.map (fun d -> Array.make d 0.0) schema in
  let create_counts i j = Array.make_matrix schema.(i) schema.(j) 0.0 in
  let joint_counts = 
    Array.init numvars (fun i -> Array.init i (create_counts i)) in

  let add_counts x p =
    let rawp = exp p.(k) in 
    for i = 0 to numvars - 1 do
      let xi = x.(i) in
      marg_counts.(i).(xi) <- marg_counts.(i).(xi) +. rawp;
      for j = 0 to i - 1 do
        let xj = x.(j) in
        joint_counts.(i).(j).(xi).(xj) <- joint_counts.(i).(j).(xi).(xj) +. rawp
      done;
    done in
  Array.iter2 add_counts data comp_probs;
  (marg_counts, joint_counts)

let compute_mi schema marg_counts joint_counts comp_probs k=
  (* Compute a single mutual information score *)
  let tp = Array.fold_left (fun s p -> exp p.(k) +. s ) 0.0 comp_probs in 
  let total = !prior +. tp in
  (*
 let total = !prior +. float_of_int num_examples in
  *)
  let calc_mi i j =
    let mi = ref 0.0 in
    let ip = !prior /. (float_of_int schema.(i)) in
    let jp = !prior /. (float_of_int schema.(j)) in
    let ijp = !prior /. (float_of_int (schema.(i) * schema.(j))) in
    for ival = 0 to schema.(i) - 1 do
      for jval = 0 to schema.(j) - 1 do
        let p_ij = 
          (ijp +. joint_counts.(i).(j).(ival).(jval)) /. total in
        let p_i  = (ip +. marg_counts.(i).(ival)) /. total in
        let p_j  = (jp +. marg_counts.(j).(jval)) /. total in
        (* dlogf "P(x_%d = %d, x_%d = %d) = %f\n" i ival j jval p_ij; *)
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
        table.(ii).(jj) <- log (((ijp +.  counts.(ii).(jj))) /. 
                                (ip +.  marg_counts.(i).(ii)));
      done
    done;
    Bn.set_cpt bn j [i] table in 
  List.iter set_cpd edges; 

  (* Set marginal distributions for all nodes with no parents *)
  for i = 0 to Array.length schema - 1 do
    if Bn.parents bn i = [] then begin
      let ip = !prior /. (float_of_int schema.(i)) in
      let denom = !prior +.  (Array.sumf marg_counts.(i)) in
      let count_to_weight c = log ((ip +. c) /. denom) in
      let dist = Array.map count_to_weight marg_counts.(i) in
      Bn.set_cpt bn i [] [|dist|]
    end 
  done;
  bn


let answer_mixture_query bn_a w_a d =
  Array.map2 (fun bn logw -> logw +. Bn.loglikelihood bn d) bn_a w_a

let get_final_prob bn_a w_a d = 
  let comp_probs = answer_mixture_query bn_a w_a d in
  let p = alogsumexp comp_probs in
  p

let learn_mixture_params schema data num_comps=
  let unnormal_ws = Array.make num_comps 0.0  in
  (*let t = Unix.time() in
  
  let seed = (int_of_float t)  mod  500 in
  Random.init seed;*)
  for k=0 to num_comps - 1 do
    unnormal_ws.(k) <- Random.float 1.0;
  
  done;
  let w_sum = Array.sumf unnormal_ws in 
  let ws = Array.map (fun w->log w -. log w_sum) unnormal_ws in
  
  let round = ref 0 in
  let err = ref 10.0 in
  let n_f = float_of_int (Array.length data) in 

  (* Random soft assignment of data points to clusters. *)
  let sgamma = Array.make_matrix (Array.length data) num_comps 0.0 in
  for i=0 to Array.length sgamma - 1 do
    for j=0 to Array.length sgamma.(i) - 1 do
      sgamma.(i).(j) <- log (Random.float 1.0)
    done
  done;

  Timer.start "mtlearn";

  vlogf "Initial CL trees\n"; flush_all();
  let create_kth_bn k =
      vlogf "Creating tree %d...\n" k;
      let (marg_counts, joint_counts) = soft_counts schema data sgamma k in
      dlogf "Collected counts: %f s\n" (Timer.delta "mtlearn");
      let mi_k = compute_mi schema marg_counts joint_counts sgamma k in
      dlogf "Computed MI: %f s\n" (Timer.delta "mtlearn");
      let edges_k = chow_liu schema mi_k in
      dlogf "Built CL tree: %f s\n" (Timer.delta "mtlearn");
      let bn_k = create_bn schema edges_k marg_counts joint_counts in
      dlogf "Created BN: %f s\n" (Timer.delta "mtlearn"); flush_all();
      bn_k in
  let bns = Array.init num_comps create_kth_bn in

  while !round < 100 && !err > 0.01 do
    (*Printf.nlogf "------------------ Round %d -------------------\n" !round; flush_all(); *)
    incr round;

    let gama =  Array.make num_comps 0.0 in

    (* Soft assignment of examples to clusters *)
    let update_gama d_i d = (* gama is Gamma *)
      (*nlogf "answer_mixture_query %d\n" d_i; flush_all();  *)
      let cs = answer_mixture_query bns ws d in 
      normalize_inplace_log cs;
      (*nlogf "value %d:%f\n" d_i s; flush_all(); *)
      Array.iteri (
        fun k p -> 
          sgamma.(d_i).(k) <- p;
          gama.(k) <- logsumexp2 gama.(k) sgamma.(d_i).(k)) cs in
    Array.iteri update_gama data;

    (* Renormalize by total counts for each cluster. *)
    for i=0 to Array.length sgamma - 1 do
      for k=0 to Array.length gama - 1 do
        sgamma.(i).(k) <- sgamma.(i).(k) -. gama.(k)
      done
    done;
    dlogf "Update assignment: %f s\n" (Timer.delta "mtlearn"); flush_all();

    List.iter Timer.clear ["counts"; "mi"; "cl"; "bn"];
    Timer.start "iters";
    err := 0.0;
    for k=0 to num_comps - 1 do
      Timer.start "counts";
      let (marg_counts, joint_counts) = soft_counts schema data sgamma k in
      Timer.stop "counts";
      Timer.start "mi";
      let mi_k = compute_mi schema marg_counts joint_counts sgamma k in
      Timer.stop "mi";
      Timer.start "cl";
      let edges_k = chow_liu schema mi_k in
      Timer.stop "cl";
      Timer.start "bn";
      let bn_k = create_bn schema edges_k marg_counts joint_counts in
      Timer.stop "bn";
      bns.(k) <- bn_k;
      let w_k = gama.(k) -. log n_f in
      err := !err +. abs_float ( w_k -. ws.(k) );
      ws.(k) <- w_k;
    done;
    dlogf "Create new trees: %f s\n" (Timer.delta "mtlearn"); flush_all();
    dlogf "Counts: %f s; MI: %f s; CL: %f s; BN: %f s\n" 
        (Timer.elapsed "counts") (Timer.elapsed "mi") (Timer.elapsed "cl")
        (Timer.elapsed "bn");
    vlogf "round %d, error = %f (%.3fs)\n" !round !err (Timer.delta "iters"); 
  done;
  (bns, ws)
      

let remove_nonempty_dir dirname = 
  let filelist = Sys.readdir dirname in 
  Array.iter (fun f->Sys.remove (String.concat dir_sep [dirname; f]) )filelist;
  ignore()


;;
let main () = 

  (* Parse arguments *)
  seed := ((int_of_float (Unix.time()) )  mod  1000); 
  Arg.parse args ignore usage;
  if !datafile = "" || !output_dir = "" then
      (Arg.usage args usage; exit 1);

  common_log_init ();
  network := "spac";

  Random.init !seed;
  if not (check_suffix !output_dir ".spn") then begin nlogf "Error: output directory model should end with .spn \n"; exit 1 end;
  
  let odir = sprintf "%s" !output_dir in
    if Sys.file_exists odir then 
      begin
        if not !override then begin
          nlogf "Error: directory %s exists.\nUse -f to override the existing directory.\n" !output_dir; flush_all();  exit 1 end
        else begin
          remove_nonempty_dir !output_dir
        end
      end
    else Unix.mkdir odir 0o755;


  
  let datas = Data.load_data !datafile in 
  let schema = 
    if !schemafile <> "" then
      Data.load_schema !schemafile
    else Data.schema datas in 
  let data_ar = Array.of_list datas in
  
  Unix.chdir odir;
     
  if !qfile <> "" then 
    if !outfile = "" then begin 
      nlogf "Needs output inference file as well\n"; 
          (Arg.usage args usage; exit 0)
  end;

  vlogf "Loading data....\n";
  flush_all();


  let t1 = Sys.time() in

  vlogf "Learning params....\n";
  flush_all();

  let (bn, ws) = learn_mixture_params schema data_ar !num_comps in
  let t2 = Sys.time() in
  flush_all();
  let data_llg = (Array.sumf_map ( fun d->get_final_prob bn ws d ) data_ar) /. float_of_int (Array.length data_ar) in

  let valid_llg = ref 0.0 in
  if ( !vfile <> "" ) then begin
    let val_data = Data.load_data !vfile in  
    valid_llg := (List.sumf_map ( fun d->get_final_prob bn ws d ) val_data) /. float_of_int (List.length val_data)
  end;
  let test_llg = ref 0.0 in 
  if !qfile <> "" && !outfile <> "" then begin
    let queries = Data.load_data !qfile in 
    let output_result = open_out !outfile in
    test_llg := (List.sumf_map ( fun d->let logp = get_final_prob bn ws d in fprintf output_result "%f\n" logp; logp  ) queries) /. float_of_int (List.length queries);
    close_out_noerr output_result
  end;
  outmodel := "spac.m";
  if !outmodel <> "" then
  begin
    for i=1 to !num_comps do
      let name = sprintf "%s-%d.bn" !network i in
      Bn.write_auto name bn.(i-1);

      let acname = sprintf "%s-%d.ac" !network i in
      let cmd = Printf.sprintf "libra acve -m %s -o %s" name acname in 
      vlogf "%s\n" cmd;
    ignore(Unix.system cmd);
    
    ignore();
    done;

    let mout = open_out !outmodel in
    fprintf mout "%s\n%d\n" !network (!num_comps + 1);
    fprintf mout "n 0 + -1\n";

    for i=1 to !num_comps do
      fprintf mout "%d " i
    done;
    fprintf mout "\n";
    for i=0 to !num_comps - 1 do
      fprintf mout "%f " ws.(i)
    done;

    fprintf mout "\n";
    for i=0 to Array.length schema - 1 do 
      fprintf mout "%d " i;
    done;
    fprintf mout "\n";

    for i=0 to !num_comps - 1 do
      fprintf mout "n %d ac 0\n" (i+1);
      for j=0 to Array.length schema - 1 do 
        fprintf mout "%d " j
      done;
      fprintf mout "\n"
    done;
    fprintf mout "\n";
    close_out_noerr mout
  end;
  
  let learning_time = (t2 -. t1) in
  ignore(learning_time);
  (*nlogf "result %s %d %f %f %f %f %f \n" !network !num_comps !prior data_llg !valid_llg !test_llg learning_time; *)
  nlogf "Average data likelihood: %.5f\n" data_llg;
  vlogf "Total time: %f seconds\n" learning_time;
  flush_all()
  
;;
main ()
