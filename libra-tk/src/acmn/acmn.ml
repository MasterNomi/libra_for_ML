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
open Circuit

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

(* Output Markov network *)
let mnoutfile = ref ""

(* The weight the score function assigns to each additional edge in the AC. *)
let edge_cost = ref 0.1

(* The weight the score function assigns to each additional split created,
   i.e., the penalty for each additional parameter. *)
let split_cost = ref (-1.)
let half_split = ref false

(* "Sloppiness" factor: Accept a split whose score might be this fraction 
worse than the best, in order to avoid scoring additional splits. *)
let sloppy = ref 0.01

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

(* If we converge with one per-edge cost and shrink_edge_cost is true,
 * we will reduce the per-edge cost and keep going.  This is good for
 * when you have a fixed edge limit you're trying to meet.
 *)
let shrink_edge_cost = ref false

(* Skip learning an AC, and just learn a BN (similar to WinMine) *)
let no_ac = ref false

(* Allow cycles *)
let depnet = ref false

let prior_stddev = ref 1.0 

let l1_param = ref 0.0

let usage = "Usage: acmn -i <input> -o <output> [...]"
let args = Arg.align
 ([("-i", Arg.Set_string datafile, " Training data file") ;
   ("-s", Arg.Set_string schemafile, " Data schema (optional)") ;
   ("-o", Arg.Set_string outfile, " Output circuit") ;
     (* The two options below are currently HACKS *)
     (* This also implies a per-edge cost of 0. *)
   ("-mo", Arg.Set_string mnoutfile, " Output Markov network") ;
   ("-sd", Arg.Set_float prior_stddev, 
       " Standard deviation of Gaussian weight prior [1.0]") ;

   ("-l1", Arg.Set_float l1_param, " Weight of L1 norm [0.0]"); 
   ("-pe", Arg.Set_float edge_cost, " Per-edge penalty [0.1]") ;
   ("-sloppy", Arg.Set_float sloppy, " Score tolerance heuristic [0.01]");
   ("-shrink", Arg.Set shrink_edge_cost, " Shrink edge cost before halting [false]") ;
   ("-ps", Arg.Set_float split_cost, " Per-split penalty [-1.0]") ;
   ("-psthresh", Arg.Set thresh, 
     " Output models for various per-split penalty thresholds [false]");
   ("-maxe", Arg.Set_int max_edges, " Maximum number of edges [1000000000]") ;
   ("-maxs", Arg.Set_int max_splits, " Maximum number of splits [1000000000]") ; 
   ("-quick", Arg.Set quick, " 'Quick and dirty' heuristic [false]") ;
   ("-halfsplit", Arg.Set half_split,  
     " Produce only one child feature in each split [false]") ;
   ("-freq", Arg.Set_int write_freq, 
     " Number of iterations between saving current circuit [-1]")]
   @ common_arguments)
  
let write_circuit filename circ feature_list =
  let out = open_out filename in
  Circuit.output_with_features out circ feature_list;
  close_out out 

module F = Mn.Factor

let write_mn filename circ feature_list =
  let out = open_out filename in
  (* Convert AC features to MN features *)
  let acf_to_mnf f =
    {F.cond = (Array.of_list f.cond); 
     F.weight_id = -1; 
     F.weight = feature_value f} in
  let mnfeatures = Array.map acf_to_mnf (Array.of_list feature_list) in
  (* Build MN *)
  let factors = Array.map (fun f -> Mn.Factor.Feature f) mnfeatures in
  let mn = Mn.create (Circuit.schema circ) factors in
  (* Write MN *)
  Mn.write_auto filename mn;
  close_out out

let save_circuit circ feature_list = 
  if !outfile <> "" then
    write_circuit !outfile circ feature_list;
  if !mnoutfile <> "" then
    write_mn !mnoutfile circ feature_list

let mkdir () =
  if !outfile <> "" then
  try Unix.mkdir (sprintf "%s.d" !outfile) 0o777
  with Unix.Unix_error _ -> ()

let save_periodic circ feature_list i =
  if !outfile <> "" then
    let filename = sprintf "%s.d/%d.ac" !outfile i in
    write_circuit filename circ feature_list 


let rawscore l e =
  let norml = l -. !split_cost in
  let norme = float_of_int e in
  norml -. (norme *. !edge_cost)

let score split =
  let e = match split.e with None -> 100_000_000 (* big_int *) | Some e -> e in
  rawscore split.maxl e

let mscore split = 
  let e = match split.e with None -> split.mine | Some e -> e in
  rawscore split.maxl e

(* Split ordering for the priority queue.  This is greater than
instead of less than so that smaller splits come first. *)
let split_lt s1 s2 =
  mscore s1 > mscore s2

let breakeven_edges split =
  let frac_e = (split.maxl -. !split_cost) /. !edge_cost in
  1 + int_of_float frac_e


exception MaxEdges
exception Converged

(* Use a priority queue (Heap) to keep track of our maxscore for each
 * split.  When the min number of edges changes, we need to update 
 * our maxscore (and reinsert the split in the split set).
 *)

let rescore_split q x =
  x.mine <-
    if q then (match x.e with None -> x.mine | Some e -> e)
    else 0

let resort_splits split_set =
  Heap.rebuild split_set

let compute_fsplit_ll circ ds logz f split_set n =
  Timer.start "efx";
  let vars = Array.to_list circ.flat_vnodes in
  let fprobs_l = OptImpl.expected_fx_probs circ ds logz f vars in
  Timer.stop "efx";
  let fprobs = Hashtbl.create 100 in
  List.iter2 (fun v p -> Hashtbl.add fprobs (var_details v) p) vars fprobs_l;
  fprobs


let rec best_split circ ds logz anc 
    maxe_remaining max_maxe split_set seen fseen n = 

  if Heap.is_empty split_set then raise Converged;

  (* Remove best split *)
  let x = Heap.min split_set in
  Heap.remove_min split_set;

  (* DEBUG *)
  if log_exists log_debug then output_split (log_stream log_debug) x;

  (* If we've seen this before, then it really is the best!
     (NOTE: We can sometimes skip this for greedy, but at worst 
     it's one extra split evaluation per iteration.) *)
  if  SplitHSet.mem seen x then (x, max_maxe)
  (* Otherwise, recompute its edge cost, reinsert it with a new
     maxscore, and repeat. *)
  else begin
    (* Recompute feature expectations if necessary *)
    if not (NMap.mem fseen x.acf.acnode) then begin
      Timer.start "compute_fsplit_ll";
      let fscores = compute_fsplit_ll circ ds logz x.acf split_set n in
      NMap.add fseen x.acf.acnode fscores;
      (* DEBUG *)
      vlogf "Recomputed f\n";
      Timer.stop "compute_fsplit_ll";
    end;

    (* Get ll *)
    (* TODO: We might accidentally compute this twice... *)
    Timer.start "1dsearch";
    let fprobs = NMap.find fseen x.acf.acnode in
    let p = Hashtbl.find fprobs (var_details x.v) in
    let (newl, newwt) = OptImpl.fscore_2d !l1_param !prior_stddev n x.prob p in
    x.maxl <- newl;
    x.wt <- newwt;
    Timer.stop "1dsearch";

    (* Reinsert if our ll is worse than expected *)
    if Heap.size split_set > 0 && mscore (Heap.min split_set) > 0. &&
        mscore x < (1. -. !sloppy) *. mscore (Heap.min split_set) then begin
      Heap.add split_set x;
      (* DEBUG *)
      vlogfz "Score: %f (ll fail)\n" (lazy (mscore x));
      best_split circ ds logz anc maxe_remaining max_maxe 
          split_set seen fseen n
    end else begin
      (* Compute the change in number of edges, if necessary *)
      (* Stop once our score becomes less than zero *)
      let max_edge_delta = min (breakeven_edges x) maxe_remaining in
      if max_edge_delta < 0 then (x, max_maxe)
      else begin
        Timer.start "delta_edges";
        let e = Slearn.delta_edges !half_split circ anc max_edge_delta x in 
        Timer.stop "delta_edges";
        (* Keep track of our worst case *)
        let max_maxe = max e max_maxe in
        x.e <- Some e;
        rescore_split !quick x;
        SplitHSet.add seen x;
        (* DEBUG *)
        vlogf "Recomputed e\n";
        if Heap.size split_set > 0 && 
          mscore x < (1. -. !sloppy) *. mscore (Heap.min split_set) then begin
          (* DEBUG *)
          vlogfz "Score: %f (e fail)\n" (lazy (mscore x));
          Heap.add split_set x;
          best_split circ ds logz anc maxe_remaining max_maxe 
              split_set seen fseen n
        end else
          (x, max_maxe)
      end
    end
  end


let rec pick_best_split circ anc split_set feature_list n =
  if Heap.is_empty split_set then 
    (vlogf "No splits remaining!"; raise Converged);
  let maxe_remaining = !max_edges - Circuit.num_edges circ in
  let visited_splits = SplitHSet.create 100 in
  let visited_fnodes = NMap.create 100 in
  let ds = Circuit.create_deriv_scratch circ in
  let logz = Circuit.compute_z (Circuit.create_scratch circ) circ in
  try begin
    let (best, max_maxe) = 
      best_split circ ds logz anc maxe_remaining 0 split_set 
        visited_splits visited_fnodes n in
    nlogf "Recomputed: %d splits, %d features; worst-case edges: %d\n" 
      (SplitHSet.length visited_splits) (NMap.length visited_fnodes) max_maxe;
    vlogfz "Best score: %f\n" (lazy (score best));
    if score best < 0. then raise Converged;
    best
  end with Converged ->
    (* Don't keep going if we have < 1% of total edges remaining. *)
    if !shrink_edge_cost && maxe_remaining > !max_edges / 100  then begin
      nlogf "Learning converged with edge penalty of %f.\n" !edge_cost;
      edge_cost := !edge_cost /. 2.;
      nlogf "Continuing with new edge cost of %f.\n" !edge_cost;
      save_circuit circ feature_list;
      (* Our split cost estimates might be off, since they were
         upper bounded with a different edge cost. *)
      if not !quick then
        Heap.iter (fun s -> s.e <- None) split_set ;
      resort_splits split_set;
      pick_best_split circ anc split_set feature_list n
    end 
    else 
      raise Converged



let load_data_and_schema filename schemafile =
  let data = Data.input_example_list (open_in filename) in
  let schema = 
    if schemafile <> "" then begin
      let schemain = open_in schemafile in
      let s = Data.input_example schemain in
      close_in schemain ; s
    end
    else Data.schema data in 
  let data = Array.of_list data in
  (data, schema)

let loglikelihood circ data =
  let scratch = Circuit.create_scratch circ in
  let n = float_of_int (Array.length data) in
  Array.sumf_map (Circuit.logprob_x scratch circ) data
    -. (n *. (Circuit.compute_z scratch circ))

let optimize_weights circ feature_list true_counts n =
  let fa = Array.of_list feature_list in
  let fc = Array.of_list true_counts in
  OptImpl.datall_optimize !l1_param !prior_stddev circ fc n [||] fa 100


let do_learn () =
  Timer.start "init";
  vlogf "Initializing...\n" ;

  (* Read in data and determine schema (number of values for each var) *)
  let (data, schema) = load_data_and_schema !datafile !schemafile in 
  vlogf "Loaded data.\n";
  let n = float_of_int (Array.length data) in

  (* Create initial circuit and set of distributions. (Initial
     distributions will be single-variable marginals.) *)
  let (circ, initial_features, initial_counts, split_list) = 
    Slearn.init !l1_param !prior_stddev schema data in
  let feature_list = ref initial_features in
  let feature_counts = ref initial_counts in 

  (* TODO: Load an initial AC structure to start with...? *)

  (* Create a set of candidate splits *)
  let split_set = Heap.create split_lt 100 in
  List.iter (Heap.add split_set) split_list;
  vlogfz "Initial splits: %d\n" (lazy (Heap.size split_set));

  (* Initial log-likelihood *)
  let last_ll = ref (loglikelihood circ data) in
  nlogf "Initial log likelihood: %f\n" !last_ll;
  if !write_freq > 0 || !thresh then mkdir () ;
  nlogf "Initializing took %f seconds.\n" (Timer.elapsed "init");
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
    Timer.start "pick";
    let anc = Slearn.build_ancestors circ in
    let split = pick_best_split circ anc split_set !feature_list n in

    (* Save periodic circuits representing where we'd stop if
     * we had various values of the per-split penalty, -ps
     * Only active if running with -psthresh
     *)
    while !thresh && score split < List.hd !curr_thresh do
      assert(!outfile <> "");
      let fname = sprintf "%s.d/%s-ps%s" 
        !outfile !outfile (List.hd !curr_tname) in
      nlogf "Passed -ps threshold %f; saving circuit \"%s\".\n"
        (List.hd !curr_thresh) fname ;
      write_circuit fname circ !feature_list ;
      curr_tname := List.tl !curr_tname ;
      curr_thresh := List.tl !curr_thresh 
    done ;
    Timer.stop "pick";
    Timer.start "split";
    let delta_edge = match split.e with Some e -> e | None -> -1 in
    let curr_edges = Circuit.num_edges circ in
    if delta_edge + curr_edges > !max_edges then
      raise MaxEdges ;

    (* Apply the chosen split. *)
    let all_splits = if !quick  then [] 
                     else Heap.to_list split_set in
    let f_new = Slearn.split_feature !half_split circ !feature_list 
        anc all_splits split in
    Timer.stop "split";
    Timer.start "update";
    let new_edges = Circuit.num_edges circ in

    if log_exists log_verbose then begin
       vlogf "Splitting feature:\n" ;
       Circuit.output_feature (log_stream log_verbose) split.acf ;
       vlogf "Splitting on var %s\n" (Circuit.string_of_node split.v)
    end;
    nlogf "Delta edges: %d ; ll: %f\n" delta_edge split.maxl;
    (* Check that the predicted delta_edges is correct. *)
    (* assert( (new_edges - curr_edges) = delta_edge); *)
    if not !no_ac && (new_edges - curr_edges) <> delta_edge then
      nlogf "WARNING: Actual delta edges = %d\n" (new_edges - curr_edges);
    nlogf "Total edges: %d ; Depth: %d\n" 
      new_edges (Circuit.depth circ);

    if log_exists log_debug then
      Circuit.output (log_stream log_debug) circ; 

    if !write_freq > 0 && !i mod !write_freq = 0 then 
      save_periodic circ !feature_list !i;

    (* Update feature and feature count lists *)
    feature_list := f_new @ !feature_list;
    if !half_split then
      feature_counts :=  (fst split.prob) :: !feature_counts
    else
      feature_counts :=  (fst split.prob) :: (snd split.prob) :: !feature_counts;

    (* Optimize all weights, starting from initial values *)
    set_feature_value (List.hd !feature_list) (fst split.wt);
    if not !half_split then
      set_feature_value (List.hd (List.tl !feature_list)) (snd split.wt);
    let new_ll = optimize_weights circ !feature_list !feature_counts n in
    nlogf "Actual delta ll: %f\n" (new_ll -. !last_ll);
    last_ll := new_ll;

    (* Add and score new splits *)
    let ds = Circuit.create_deriv_scratch circ in
    let logz = Circuit.compute_z (Circuit.create_scratch circ) circ in
    let fsplits f = Slearn.gen_splits !l1_param !prior_stddev circ ds logz f data in
    let fl = List.map fsplits f_new in
    List.iter (List.iter (Heap.add split_set)) fl;

    vlogfz "Total number of potential splits: %d\n" (lazy (Heap.size split_set));
    
    nlogf "Elapsed time: %f seconds\n" (Sys.time ());
    nlogf "Delta time: %f seconds\n" (Sys.time () -. !lasttime);
    lasttime := Sys.time();
    Timer.stop "update";

    nlogf "Times: pick=%.4f split=%.4f update=%.4f\n"
           (Timer.elapsed "pick") (Timer.elapsed "split")
           (Timer.elapsed "update");
    nlogf "Times: edges=%.4f ll=%.4f efx=%.4f 1dsearch=%.4f\n"
           (Timer.elapsed "delta_edges") (Timer.elapsed "compute_fsplit_ll")
           (Timer.elapsed "efx") (Timer.elapsed "1dsearch");
    nlogf "Times: emp_p=%.4f exp_p=%.4f score2d=%.4f\n"
           (Timer.elapsed "emp_probs") (Timer.elapsed "exp_probs") 
           (Timer.elapsed "score_2d");
           (*
    List.iter Timer.clear ["pick"; "split"; "update"; "efx"; "1dsearch";
      "delta_edges"; "compute_fsplit_ll"; "anc"]; *)
  done ;
  nlogf "\nMaximum number of splits reached.\n" ;
  with Converged -> nlogf "\nLearning converged.\n" 
     | MaxEdges -> nlogf "\nMaximum number of edges reached.\n" );
  save_circuit circ !feature_list;
  (* Compute the final log likelihood.  Doing it this way will
     always be correct, but could be rather slow... *)
  if not !no_ac && log_exists log_debug then
    dlogf "Final log likelihood: %f\n" (loglikelihood circ data); 
  nlogf "Total time: %f seconds\n" (Sys.time ())


let main () = 
  Arg.parse args ignore usage;
  if !datafile = "" 
    || (!no_ac && !mnoutfile = "") 
    || (not !no_ac && !outfile = "") then
    (Arg.usage args usage; exit (-1));
  if !no_ac then begin
    (* Outputting models at different per-split thresholds
       is not (yet) supported for pure BN learning. *)
    thresh := false;
    edge_cost := 0.;
  end;
  (* Set default split cost *)
  if !split_cost < 0. then
    split_cost := if !no_ac then 0.1 else 0.0;
  common_log_init ();
  do_learn ()

let _ = main ()
