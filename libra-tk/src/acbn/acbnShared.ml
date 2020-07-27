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
open Slearn


(* Values of the per-split prior.  If the -psthresh option is used,
 * then we save the current circuit every time we fall below a new
 * threshold.
 *)
let prior_threshes = 
 [1000.0; 500.0; 200.0; 100.0; 50.0; 20.0; 10.0; 5.0; 2.0; 1.0; 
 0.5; 0.2; 0.1; 0.05; 0.02; 0.01; 0.0]

let prior_thresh_names = 
  ["1e3"; "5e2"; "2e2"; "1e2"; "5e1"; "2e1"; "1e1"; "5e0"; "2e0"; "1e0";
  "5em1"; "2em1"; "1em1"; "5em2"; "2em2"; "1em2"; "zero"]
  
let curr_thresh = ref prior_threshes
let curr_tname  = ref prior_thresh_names

let big_int = 1000000000 (* = 1 billion *)

(* Globals used for command line parameters *)

let datafile = ref ""

(* The schema file allows you to specify the dimension of each
   variable.  Its format is a single line containing each dimension,
   separated by spaces, surrounded by parentheses, e.g.:
(2 2 3 2 5)

   For a domain where the first two variables have values 0 and 1, the
   third has values 0, 1, and 2, etc.
 *)
let schemafile = ref ""

(* Output arithmetic circuit *)
let outfile = ref ""

(* Output Bayesian network (xmod format) *)
let bnoutfile = ref ""

(* Prior counts of uniform distribution.  Used to define a symmetric Dirichlet
   prior with parameter (prior)/(range of variable) for each variable. *)
let prior_counts = ref 1.

(* The weight the score function assigns to each additional edge in the AC. *)
let edge_cost = ref 0.1

(* The weight the score function assigns to each additional split created,
   i.e., the penalty for each additional parameter. *)
let split_cost = ref (-1.)

(* Alternate way to define the split cost. *)
let kappa = ref 0.0

(* Maximum number of edges and splits allowed.  Sometimes these are
   exceeded a little bit, due to imprecise coding. *)
let max_edges = ref big_int
let max_splits = ref big_int

(* After every k splits, write the current circuit to disk.  If less
   than 0, skip this. *)
let write_freq = ref (-1)

(* Output models for different per-split thresholds.  This is good for
 * tuning the per-split penalty in a single training pass, rather than
 * n separate training runs.
 *)
let thresh = ref false

(* Use quick heuristic (default is greedy) *)
let quick = ref false

(* After converging with quick heuristic, switch to greedy to see if
   we can further improve the model. *)
let do_greedy = ref false

(* If we converge with one per-edge cost and shrink_edge_cost is true,
 * we will reduce the per-edge cost and keep going.  This is good for
 * when you have a fixed edge limit you're trying to meet.
 *)
let shrink_edge_cost = ref false

(* File containing restrictions on parents. *)
let parent_file = ref ""

(* Skip learning an AC, and just learn a BN (similar to WinMine) *)
let no_ac = ref false

(* Allow cycles -- NO LONGER USED. *)
let depnet = ref false


let usage = "Usage: acbn -i <input> -o <output> [...]"
let args = Arg.align
 ([("-i", Arg.Set_string datafile, " Training data file") ;
   ("-s", Arg.Set_string schemafile, " Data schema (optional)") ;
   ("-o", Arg.Set_string outfile, " Output circuit") ;
   ("-mo", Arg.Set_string bnoutfile, " Output Bayesian network (XMOD or BN format)") ;
   ("-prior", Arg.Set_float prior_counts, " Prior counts of uniform distribution [1]") ;
   ("-parents", Arg.Set_string parent_file, " Restrictions on variable parents") ;
   ("-pe", Arg.Set_float edge_cost, " Per-edge penalty [0.1]") ;
   ("-shrink", Arg.Set shrink_edge_cost, " Shrink edge cost before halting [false]") ;
   ("-ps", Arg.Set_float split_cost, " Per-split penalty [-1.0]") ;
   ("-kappa", Arg.Set_float kappa, " Alternate per-split penalty specification [0.0]") ;
   ("-psthresh", Arg.Set thresh, 
     " Output models for various per-split penalty thresholds [false]");
   ("-maxe", Arg.Set_int max_edges, " Maximum number of edges [1000000000]") ;
   ("-maxs", Arg.Set_int max_splits, " Maximum number of splits [1000000000]") ; 
   ("-quick", Arg.Set quick, " 'Quick and dirty' heuristic [false]") ;
   ("-qgreedy", Arg.Set do_greedy, " Find greedy local optimum after quick heuristic [false]") ;
   ("-freq", Arg.Set_int write_freq, 
     " Number of iterations between saving current circuit [-1]")]
   @ common_arguments)
  
let bnusage = "Usage: bnlearn -i <input> -o <output> [...]"
let bnargs = Arg.align
 ([("-i", Arg.Set_string datafile, " Training data file") ;
   ("-s", Arg.Set_string schemafile, " Data schema (optional)") ;
   ("-o", Arg.Set_string bnoutfile, " Output Bayesian network (XMOD or BN format)") ;
   ("-prior", Arg.Set_float prior_counts, " Prior counts of uniform distribution") ;
   ("-parents", Arg.Set_string parent_file, " Restrictions on variable parents") ;
   ("-ps", Arg.Set_float split_cost, " Per-split penalty [-1.0]") ;
   ("-kappa", Arg.Set_float kappa, " Alternate per-split penalty specification [0.0]") ;
   ("-psthresh", Arg.Set thresh, 
     " Output models for various per-split penalty thresholds [false]");
   ("-maxs", Arg.Set_int max_splits, " Maximum number of splits [1000000000]")]
   @ common_arguments)

let rec update_tree_rec a currnode = function
| [] -> 
  (* Base case: If no conditions remain, construct a leaf. *)
  (match currnode with Bn.Vertex _ -> assert false | Bn.Leaf _ -> ());
  Bn.Leaf a
| (sense,var,value) :: condl ->
  (* Recursive case: Construct a vertex for this condition
     (perhaps based on an existing vertex) *)
  begin
    match currnode with 
      Bn.Vertex(var', value', left, right) ->
        (* The ordering of the conditions *must* match the ordering of
         * the splits in the current tree, or we fail. *)
        assert(var = var');
        if (value = value') = sense then
          let left' = update_tree_rec a left condl in
          Bn.Vertex(var', value', left', right)
        else
          let right' = update_tree_rec a right condl in
          Bn.Vertex(var', value', left, right')
    | Bn.Leaf [||] ->
        let child = update_tree_rec a (Bn.Leaf [||]) condl in
        let other = Bn.Leaf [||] in
        if sense then
          Bn.Vertex(var, value, child, other)
        else
          Bn.Vertex(var, value, other, child)
    | Bn.Leaf _ -> 
       (* We should never get here -- this indicates that a leaf has 
        * appeared twice *)
        assert false
  end


(* Create a BN with tree CPDs from the dists *)
let dists_to_bn schema dists =

  (* Initialize empty BN *)
  let numvars = Array.length schema in
  let bn = Bn.create_empty_network schema in
  let trees = Array.make numvars (Bn.Leaf [||]) in

  (* Build CPDs by turning each dist into a leaf *)
  let update_cpd dist =
    let a = Array.of_list (List.map Circuit.const_value dist.nodes) in
    let v = dist.dvar in
    trees.(v) <- update_tree_rec a trees.(v) (List.rev dist.cond) in
  List.iter update_cpd dists;
  for i = 0 to numvars - 1 do
    bn.Bn.dists.(i) <- Bn.Tree trees.(i)
  done;

  (* Build parent lists for each dist *)
  let rec make_parents pset = function
    | Bn.Leaf _ -> ()
    | Bn.Vertex(var, value, left, right) ->
        if not (Hashset.mem pset var) then
          Hashset.add pset var;
        make_parents pset left;
        make_parents pset right in
  for i = 0 to numvars - 1 do
    let p = Hashset.create 100 in
    make_parents p trees.(i);
    bn.Bn.parents.(i) <- List.rev (Hashset.to_list p)
  done;
  Bn.update_children_and_topo_vars bn;
  bn
  

let write_circuit filename circ dist_list =
  let out = open_out filename in
  let fl = List.flatten (List.map Slearn.dist_to_features dist_list) in
  Circuit.output_with_features out circ fl;
  close_out out 

let write_bn filename circ dist_list =
  let bn = dists_to_bn (Circuit.schema circ) dist_list in
  Bn.write_auto filename bn

(* output_string out "\nDistribution list:\n";
  let dvar_cmp d1 d2 = compare d1.dvar d2.dvar in
  let by_var = List.sort dvar_cmp dist_list in
  List.iter (Slearn.output_dist out) by_var ; 
  *)

let save_circuit circ dist_list = 
  if !outfile <> "" then
    write_circuit !outfile circ dist_list;
  if !bnoutfile <> "" then
    write_bn !bnoutfile circ dist_list

let mkdir () =
  if !outfile <> "" then begin
    try Unix.mkdir (sprintf "%s.d" !outfile) 0o777
    with Unix.Unix_error _ -> ()
  end;
  if !bnoutfile <> "" then begin
    try Unix.mkdir (sprintf "%s.d" !bnoutfile) 0o777
    with Unix.Unix_error _ -> ()
  end

let save_periodic circ dist_list i =
  if !outfile <> "" then begin
    let filename = sprintf "%s.d/%d.ac" !outfile i in
    write_circuit filename circ dist_list 
  end;
  if !bnoutfile <> "" then begin
    let filename = sprintf "%s.d/%d.xmod" !bnoutfile i in
    write_bn filename circ dist_list 
  end


let rawscore l e =
  let norml = l -. !split_cost in
  let norme = float_of_int e in
  norml -. (norme *. !edge_cost)

let score split =
  let e = match split.e with None -> 100_000_000 (* big_int *) | Some e -> e in
  let l = match split.l with None -> 0. | Some l -> l in
  rawscore l e

let mscore split = 
  let e = match split.e with None -> split.mine | Some e -> e in
  let l = match split.l with None -> split.maxl | Some l -> l in
  rawscore l e

let breakeven_edges split =
  let frac_e = (split.maxl -. !split_cost) /. !edge_cost in
  1 + int_of_float frac_e


exception MaxEdges
exception Converged

let add_parent varanc child parent =
  varanc.(child).(parent) <- true;
  (* Transitive closure *)
  (* Every ancestor of the parent is also now an ancestor of the child *)
  for i = 0 to Array.length varanc - 1 do
    if varanc.(parent).(i) then
      varanc.(child).(i) <- true
  done;
  (* Update child's descendants *)
  for i = 0 to Array.length varanc - 1 do
    if varanc.(i).(child) then
      for j = 0 to Array.length varanc - 1 do
        if varanc.(child).(j) then
          varanc.(i).(j) <- true
      done
  done
  
let is_ancestor varanc anc desc =
  anc = desc || varanc.(desc).(anc)

let print_anc varanc =
  if log_exists log_verbose then
    for i = 0 to Array.length varanc - 1 do
      vlogf "%d:" i;
      for j = 0 to Array.length varanc.(i) - 1 do
        if varanc.(i).(j) then
          vlogf " %d" j;
      done;
      vlogf "\n"
    done
  

(* Use a sorted list (Set) to keep track of our maxscore for each
 * split.  When the min number of edges changes, we need to update 
 * our maxscore (and reinsert the split in the split set).
 *)

let rescore_split q x =
  x.mine <-
    if q then (match x.e with None -> x.mine | Some e -> e)
    else 0; 
  x.maxl <- (match x.l with None -> x.maxl | Some l -> l) ;
  x.maxscore <- mscore x

let resort_splits split_set =
  let check_split s =
    if mscore s <> s.maxscore then
      rescore_split !quick s in
  Heap.iter check_split split_set;
  Heap.rebuild split_set


let rec best_split circ anc maxe_remaining max_maxe splitset seen = 

  (* Get best split *)
  let x = Heap.min splitset in

  (* If we've seen this before, then it really is the best!
     (NOTE: We can sometimes skip this for greedy, but at worst 
     it's one extra split evaluation per iteration.) *)
  if !no_ac || SplitHash.mem seen x then (x, max_maxe)
  (* Otherwise, recompute its edge cost, reinsert it with a new
     maxscore, and repeat. *)
  else begin
    (* Compute the change in number of edges, if necessary *)
    (* Stop once our score becomes less than zero *)
    let max_edge_delta = min (breakeven_edges x) maxe_remaining in
    if max_edge_delta < 0 then (x, max_maxe)
    else begin
      let e = Slearn.delta_edges circ anc max_edge_delta x in 
      (* Keep track of our worst case *)
      let max_maxe = max e max_maxe in
      x.e <- Some e;
      (* Place back in the heap in a new location *)
      Heap.remove_min splitset;
      rescore_split !quick x;
      Heap.add splitset x;
      SplitHash.add seen x ();
      best_split circ anc maxe_remaining max_maxe splitset seen
    end
  end


let rec pick_best_split circ anc splitset dist_list =
  if Heap.is_empty splitset then 
    (vlogf "No splits remaining!"; raise Converged)
  else begin
  let maxe_remaining = !max_edges - Circuit.num_edges circ in
  let visited_splits = SplitHash.create 100 in
  let (best, max_maxe) = 
    best_split circ anc maxe_remaining 0 splitset visited_splits in
  nlogf "Splits recomputed: %d ; worst-case edges: %d\n" 
    (SplitHash.length visited_splits) max_maxe;
  vlogf "Best score: %f\n" (score best);
  if score best < 0.0 then 
    if !do_greedy then begin
      nlogf "Quick learning converged; saving circuit and switching to greedy.\n";
      save_circuit circ dist_list;
      (* Change the output filename, so we keep the quick version separate. *)
      if !outfile <> "" then
        outfile := sprintf "%sg" !outfile;
      (* Switch over to fully greedy operation. *)
      quick := false ;
      Heap.iter (fun s -> s.e <- None) splitset ;
      resort_splits splitset;
      pick_best_split circ anc splitset dist_list
    end
    else if !shrink_edge_cost && maxe_remaining > 0 
          && !edge_cost > 1.0e-20
          (* Don't keep going if we have less than 1% of the total edges
             remaining. *)
          && !max_edges / maxe_remaining < 100 then
        begin
          nlogf "Learning converged with edge penalty of %f.\n" !edge_cost;
          edge_cost := !edge_cost /. 2.;
          nlogf "Continuing with new edge cost of %f.\n" !edge_cost;
          save_circuit circ dist_list;
          (* Our split cost estimates might be off, since they were
             upper bounded with a different edge cost. *)
          Heap.iter (fun s -> s.e <- None) splitset ;
          resort_splits splitset;
          pick_best_split circ anc splitset dist_list
        end
    else raise Converged 
  else best
  end


let loglikelihood circ data =
  let scratch = Circuit.create_scratch circ in
  Array.sumf_map (fun (wt, x) -> wt *. Circuit.logprob_x scratch circ x) data 

let do_learn () =
  vlogf "Initializing...\n" ;

  (* Read in data and determine schema (number of values for each var) *)
  let data = Data.input_wexample_list (open_in !datafile) in 
  vlogf "Loaded data.\n";
  let schema = 
    if !schemafile <> "" then
      Data.load_schema !schemafile
    else Data.schema (List.map snd data) in 
  let data = Array.of_list data in

  (* Create initial circuit and set of distributions. (Initial
     distributions will be single-variable marginals.) *)
  let (circ, initial_dists, split_list) = 
    Slearn.init !prior_counts schema data in
  let dist_list = ref initial_dists in

  (* Set up varanc and allowed_parents matrices *)
  let numvars = Array.length schema in 
  let varanc = Array.make_matrix numvars numvars false in
  let allowed_parents = 
    if !parent_file <> "" then 
      ParentParser.parse numvars (open_in !parent_file)
    else
      Array.make_matrix numvars numvars true in


  (* Create a set of candidate splits *)
  let split_set = Heap.create Slearn.split_lt 100 in
  List.iter (Heap.add split_set) split_list;
  let num_splits () = Heap.size split_set in 
  vlogf "Initial splits: %d\n" (num_splits ());

  (* Filter out illegal splits *)
  let invalid s = 
    let dvar = s.d.dvar and vvar = (Circuit.var_var s.v) in
    is_ancestor varanc dvar vvar ||
      not allowed_parents.(dvar).(vvar) in
  Heap.remove_all split_set invalid;
  vlogf "Splits after filtering: %d\n" (num_splits ());

  (* Initial log-likelihood *)
  let init_ll = loglikelihood circ data in
  if !write_freq > 0 || !thresh then mkdir () ;
  nlogf "Initializing took %f seconds.\n" (Sys.time ());
  nlogf "Initial log likelihood: %f\n" init_ll;
  let lasttime = ref (Sys.time()) in

  (* Choose best split, one at a time.  If no split increases the
     score function, then we leave this loop early by raising an
     exception. *)
  (try
  let i = ref 0 in
  while !i < !max_splits do
    incr i ;
    nlogf "\nSPLIT %d:\n" !i;
    (* DEBUG: print out z, to detect some types of circuit corruption. *)
    if log_exists log_debug then begin
      let scratch = Circuit.create_scratch circ in
      dlogf "z = %f\n" (Circuit.compute_z scratch circ) 
    end;
    let beg_time = Sys.time() in
    let anc = Slearn.build_ancestors circ in
    let split = pick_best_split circ anc split_set !dist_list in

    (* Save periodic circuits representing where we'd stop if
     * we had various values of the per-split penalty, -ps
     * Only active if running with -psthresh
     *)
    while !thresh && score split < List.hd !curr_thresh do
      nlogf "Passed -ps threshold %f; saving models.\n" 
        (List.hd !curr_thresh);
      if !outfile <> "" then begin
        let fname = sprintf "%s.d/%s-ps%s" 
          !outfile !outfile (List.hd !curr_tname) in
        write_circuit fname circ !dist_list
      end;
      if !bnoutfile <> "" then begin
        let fname = sprintf "%s.d/%s-ps%s" 
          !bnoutfile !bnoutfile (List.hd !curr_tname) in
        write_bn fname circ !dist_list
      end;
      curr_tname := List.tl !curr_tname ;
      curr_thresh := List.tl !curr_thresh 
    done ;
    let picked_time = Sys.time() in 
    let delta_edge = match split.e with Some e -> e | None -> -1 in
    let curr_edges = Circuit.num_edges circ in
    if delta_edge + curr_edges > !max_edges then
      raise MaxEdges ;

    (* Apply the chosen split. *)
    let all_splits = if !quick || !no_ac then [] 
                     else Heap.to_list split_set in
    let d = split.d and v = split.v in
    let (d1, d2) = 
      if !no_ac then 
        Slearn.split_dist_params !prior_counts d v data 
      else
        Slearn.split_dist !prior_counts circ anc all_splits split data in
    let split_time = Sys.time() in
    let new_edges = Circuit.num_edges circ in

    (* Fix sorting of candidate splits *)
    if all_splits <> [] then
      resort_splits split_set;

    if log_exists log_verbose then begin
       vlogf "Splitting distribution:\n" ;
       Slearn.output_dist (log_stream log_verbose) split.d ;
       vlogf "Splitting on var %s\n" (Circuit.string_of_node split.v)
    end;
    if not !no_ac then begin
      nlogf "Delta edges: %d ; ll: %f\n" delta_edge split.maxl;
      (* Check that the predicted delta_edges is correct. *)
      assert((new_edges - curr_edges) = delta_edge);
      nlogf "Total edges: %d ; Depth: %d\n" 
        new_edges (Circuit.depth circ)
    end;
    nlogf "Elapsed time: %f seconds\n" (Sys.time ());
    nlogf "Delta time: %f seconds\n" (Sys.time () -. !lasttime);
    lasttime := Sys.time();
    Timer.start "update1";

    if !write_freq > 0 && !i mod !write_freq = 0 then 
      save_periodic circ !dist_list !i;

    (* Update distribution list *)
    dist_list := d1 :: d2 :: List.filter (( != ) d) !dist_list ;
    Timer.stop "update1";
    Timer.start "update2";

    (* Remove old splits *)
    Heap.remove_all split_set (fun s-> s.d == d);
    Timer.stop "update2";
    Timer.start "update3";

    (* Update parent list *)
    let dvar = Slearn.dvar d and splitvar = Circuit.var_var v in
    if not !depnet && not varanc.(dvar).(splitvar) then begin
      add_parent varanc dvar splitvar;
      dlogf "Adding parent %d of %d\n" splitvar dvar;
      (* Remove illegal splits *)
      let invalid s = 
        let dvar = s.d.dvar and vvar = (Circuit.var_var s.v) in
        is_ancestor varanc dvar vvar || 
          not allowed_parents.(dvar).(vvar) in
      Heap.remove_all split_set invalid
    end ;
    Timer.stop "update3";
    Timer.start "update4";
    
    (* Add new splits *)
    let split_filter d v = 
      not (is_ancestor varanc d v) && allowed_parents.(d).(v) in
    let dll = Slearn.gen_split !prior_counts circ data split_filter in
    List.iter (Heap.add split_set) (dll d1);
    List.iter (Heap.add split_set) (dll d2);
    Timer.stop "update4";

    vlogf "Total number of potential splits: %d\n" (num_splits ());

    let update_time = Sys.time () in
    nlogf "Times: pick=%.4f split=%.4f update=%.4f\n"
           (picked_time -. beg_time) (split_time -. picked_time)
           (update_time -. split_time)
  done ;
  nlogf "\nMaximum number of splits reached.\n" ;
  with Converged -> nlogf "\nLearning converged.\n" 
     | MaxEdges -> nlogf "\nMaximum number of edges reached.\n" );
  save_circuit circ !dist_list;
  (* Compute the final log likelihood.  Doing it this way will
     always be correct, but could be rather slow... *)
  if not !no_ac && log_exists log_debug then
    dlogf "Final log likelihood: %f\n" (loglikelihood circ data); 
  nlogf "Total time: %f seconds\n" (Sys.time ())


let common_main () = 
  if !no_ac then 
    edge_cost := 0.;
  (* Set default split cost *)
  if !kappa > 0.0 then
    split_cost := -.(log !kappa)
  else if !split_cost < 0. then
    split_cost := if !no_ac then 0.1 else 0.0;
  common_log_init ();
  if (!bnoutfile <> "" && (not (Bn.filename_is_xmod !bnoutfile))
      && (not (Bn.filename_is_cn !bnoutfile))) then begin
    nlogf "Error: Output Bayesian network must be in .xmod or .bn format.\n";
    exit (-1)
  end;
  do_learn ()


let main () = 
  Arg.parse args ignore usage;
  if !datafile = "" 
    || (!no_ac && !bnoutfile = "") 
    || (not !no_ac && !outfile = "") then
    (Arg.usage args usage; exit (-1));
  common_main ()


let bnmain () = 
  Arg.parse bnargs ignore bnusage;
  no_ac := true;
  if !datafile = "" 
    || (!no_ac && !bnoutfile = "") 
    || (not !no_ac && !outfile = "") then
    (Arg.usage bnargs bnusage; exit (-1));
  common_main ()

(* let _= IFDEF BNONLY THEN bnmain () ELSE main () END;; *)
