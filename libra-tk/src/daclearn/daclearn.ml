open Printf
open Ext
open Slearn
open Circuit
open Gc
(* Values of the per-split prior.  If the -psthresh option is used,
 * then we save the current circuit every time we fall below a new
 * threshold.
 *)



let split_score_valid = ref false 

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

let evfile = ref ""

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
let split_cost = ref (0.1)
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

let l1_param = ref 0.1
let approx_percen = ref 100 
let info_percen = ref 100 
let initac = ref "" 
let feature_comp (_, s1) (_, s2) = s1 > s2 

let timelimit = ref 86400.

let support_thresh = ref 0.0001
let batch_size = ref 2
let mu_thresh = ref 0.01
let max_ev = ref 10
let initedges = ref ""

let usage = "Usage: acmn -i <input> -o <output> [...]"
let args = Arg.align
 ([
   ("-i", Arg.Set_string datafile, " Training data file") ;
   ("-s", Arg.Set_string schemafile, " Data schema (optional)") ;
   ("-o", Arg.Set_string outfile, " Output circuit") ;
     (* The two options below are currently HACKS *)
     (* This also implies a per-edge cost of 0. *)
   ("-mo", Arg.Set_string mnoutfile, " Output Markov network") ;
   ("-init", Arg.Set_string initac, " Start with an initial arithmetic circuit") ;
   ("-ccl", Arg.Set_string initedges, " Start with an initial conditional tree CRF") ;
   ("-sd", Arg.Set_float prior_stddev, 
       " Standard deviation of Gaussian weight prior [1.0]") ;
   ("-ev", Arg.Set_string evfile, " Training evidence file") ;
   ("-l1", Arg.Set_float l1_param, " Weight of L1 norm [0.0]"); 
   ("-pe", Arg.Set_float edge_cost, " Per-edge penalty [0.1]") ;
   ("-sloppy", Arg.Set_float sloppy, " Score tolerance heuristic [0.01]");
   ("-shrink", Arg.Set shrink_edge_cost, " Shrink edge cost before halting [false]") ;
   ("-ps", Arg.Set_float split_cost, " Per-split penalty [-1.0]") ;
   ("-psthresh", Arg.Set thresh, 
     " Output models for various per-split penalty thresholds [false]");
   ("-maxe", Arg.Set_int max_edges, " Maximum number of edges [1000000000]");
   ("-time", Arg.Set_float timelimit, " Maximum time budget (second) [86400]");
   ("-maxs", Arg.Set_int max_splits, " Maximum number of splits [1000000000]"); 
   ("-quick", Arg.Set quick, " 'Quick and dirty' heuristic [false]");
   ("-info", Arg.Set_int info_percen, " Use top given percentage of more informative evidence variables [100]");
   ("-bs", Arg.Set_int batch_size, " Batch size for feature extension [1]");
   ("-fmin", Arg.Set_float support_thresh, " Minumum support for new features [0.0001]");
   ("-approx", Arg.Set_int approx_percen, " Use top given percentange of feature extension [100]");
   ("-minmu", Arg.Set_float mu_thresh, " Mutual information threshold [0.0005]");
   ("-maxev", Arg.Set_int max_ev, " Mutual information threshold [0.0005]");

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

let compute_fsplit_ll circ ev_logzs f split_set n =
  dlogf "compute_fsplit: %s \n" (string_of_feature f);
  Timer.start "efx";

  (*let vars = Array.to_list circ.flat_vnodes in 
  *)
  let multival v = 
    not ((var_value v == 1) && circ.schema.(var_var v) == 2) in
    
  let vars = List.filter multival (Array.to_list circ.flat_vnodes) in 
  
  let fprobs_l = OptImpl.expected_fx_probs circ f vars ev_logzs in
     
  (*List.iteri (fun i p-> printf "i=%d: " i; Array.iter (fun m-> (printf "%0.3f %0.3f," m.(0) m.(1)) ) p; printf "\n") fprobs_l;
  printf "\n";
  flush_all(); *)
  (*let logfprobs_l = List.map ( fun p_ev -> Array.fold_left (fun s fx ->  (s +. log fx.(0) ) ) 0.0 p_ev ) fprobs_evs in
  let fprobs_l = List.map (exp) logfprobs_l in *)
  Timer.stop "efx";
  vlogf "timing: compute_fsplit %f\n" (Timer.last_elapsed "efx"); 
  let fprobs = Hashtbl.create 100 in
  List.iter2 (fun v p -> Hashtbl.add fprobs (var_details v) p) vars fprobs_l;
  
  fprobs



let expand_feature circ feature_list feature_heap split_set data schema ev ev_schema extra_evs_schema ev_matched_data_list evlogz fseen = 
    Timer.start "gensplit";
    if Heap.is_empty feature_heap then (vlogf "No feautre to extend\n"; raise Converged)   
    else begin
    let fsplits f = Slearn.gen_splits feature_list !l1_param !prior_stddev circ f data schema ev ev_schema extra_evs_schema ev_matched_data_list !approx_percen !split_cost !edge_cost evlogz fseen in
      (*let fl = List.map fsplits f_new in
      List.iter (List.iter (Heap.add split_set)) fl;
      *)
    let nf = float_of_int (Array.length data) in 
    let k = !batch_size in
    for i = 0 to k - 1 do  
      let (f,su) = Heap.min feature_heap in
      Heap.remove_min feature_heap;
      if (su /. nf) < !support_thresh then (vlogf "No more feature with enough support\n";raise Converged); 
      vlogf "Extending feature %s with support : %f\n" (string_of_feature f) su;
      let fl = fsplits f in
      
      List.iter (Heap.add split_set) fl
    done;
    Timer.stop "gensplit";
    end;

    split_score_valid := true 


let rec best_split circ anc 
    maxe_remaining max_maxe split_set seen fseen n ev extra_evs_schema it feature_heap feature_list data schema ev_schema ev_matched_data_list evlogz discard_set = 

  while Heap.is_empty split_set do
     vlogf "No splits remaining!\n";
     try
        expand_feature circ feature_list feature_heap split_set data schema ev ev_schema extra_evs_schema ev_matched_data_list evlogz fseen;

     with Converged -> raise Converged;
  done;

  (* Remove best split *)
  let x = Heap.min split_set in
  Heap.remove_min split_set;

  (* DEBUG *)
  if log_exists log_debug then output_split (log_stream log_debug) x;

  (* If we've seen this before, then it really is the best!
     (NOTE: We can sometimes skip this for greedy, but at worst 
     it's one extra split evaluation per iteration.) *)
  
  (*if  SplitHSet.mem seen x then begin (x, max_maxe)
  end
  (* Otherwise, recompute its edge cost, reinsert it with a new
     maxscore, and repeat. *)
  else begin
   *) 
   (* Recompute feature expectations if necessary *)
    (*PEDRAM need revisit*)
     
    let (mvar, mvalue) = var_details x.v in   
     
    if not (NMap.mem fseen x.acf.acnode) then begin
      let (mvar, mvalue) = var_details x.v in   
      Timer.start "compute_fsplit_ll" ;
      vlogf "Recompute f:%s, var:%d value:%d\n" (string_of_feature x.acf) mvar mvalue;
      let fscores = compute_fsplit_ll circ evlogz x.acf split_set n in
      NMap.add fseen x.acf.acnode fscores;
      (* DEBUG *)
      vlogf "Recomputed f\n";
      Timer.stop "compute_fsplit_ll";
    end
    else dlogf "Seen f:%s, var:%d value:%d already.\n" (string_of_feature x.acf) mvar mvalue;
    
   
    
    (* Get ll *)
    (* TODO: We might accidentally compute this twice... *)
    Timer.start "1dsearch";
    
    let fprobs = NMap.find fseen x.acf.acnode in
    let p = Hashtbl.find fprobs (var_details x.v) in
    
    let (mvar, mvalue) = var_details x.v in 
    (*Array.iter (fun m-> (printf "%f %f," m.(0) m.(1)) ) p;
    printf "\n";
    flush_all(); *)
    let multival v = 
       not ((var_value v == 1) && circ.schema.(var_var v) == 2 ) in 
    

    
    let vars = List.filter multival (Array.to_list circ.flat_vnodes) in 
  
  (*
    let vars = List.filter (fun v'-> not (feature_contains_var x.acf v')) mvars in
    *)
    (*let fprobs_l = OptImpl.expected_fx_probs circ x.acf vars ev_logzs in
    
    let (newl, newwt) = OptImpl.fscore_2d !l1_param !prior_stddev n ev x.acf x.prob (List.nth fprobs_l mvar) x.v in
    *)
    (*let (newl, newwt) = OptImpl.fscore_2d !l1_param !prior_stddev n ev extra_evs_schema x.acf x.prob p x.v in
    *)
   
    (*let (tt,ff) = x.ounts in
    if tt >= 0.0 && ff >= 0.0 then begin
      let (newl, newwt) = OptImpl.fscore_2d !l1_param !prior_stddev n ev extra_evs_schema x.acf x.counts p x.v in
      vlogf "rescoring feature f: %s var:%d value:%d, newl: %0.3f oldl:%0.3f\n" (string_of_feature x.acf) mvar mvalue newl x.maxl;
      x.maxl <- newl;
      x.wt <- newwt;
    end 
    else begin *)

    (*if not x.final then begin *)

    let deltalogz = Array.make (Array.length ev) 0.0 in
    (*if  not !split_score_valid then begin
    *)
      let (newl, newwt) = OptImpl.fscore_2d2 !l1_param !prior_stddev n ev extra_evs_schema x.acf [||] deltalogz x.prob p x.v in
      
        vlogf "rescoring feature f: %s var:%d value:%d, newl: %0.3f oldl:%0.3f\n" (string_of_feature x.acf) mvar mvalue newl x.maxl;

        x.maxl <- newl;
        x.wt <- newwt;
    (*end;*)
    (*

    end;
    *)


    Timer.stop "1dsearch";
    if (it mod 20 ) = 0 then begin
       if log_exists log_verbose  then Gc.print_stat (log_stream (log_verbose) );
       Gc.major();
    end;
    if x.maxl < !split_cost then 
       best_split circ anc maxe_remaining max_maxe 
              split_set seen fseen n ev extra_evs_schema (it+1) feature_heap feature_list data schema ev_schema ev_matched_data_list evlogz discard_set 
    else
      (*
    end;*)

    
    (*if x.maxl < 0.0 then exit 0; *)
    (* Reinsert if our ll is worse than expected *)
    (*if Heap.size split_set > 0 && mscore (Heap.min split_set) > 0. &&
        mscore x < (1. -. !sloppy) *. mscore (Heap.min split_set) then begin
      Heap.add split_set x;
      (* DEBUG *)
      vlogfz "Score: %f (ll fail)\n" (lazy (mscore x));
      best_split circ anc maxe_remaining max_maxe 
          split_set seen fseen n ev extra_evs_schema ev_logzs (it+1)
    end else begin
    *)
    begin
      (* Compute the change in number of edges, if necessary *)
      (* Stop once our score becomes less than zero *)
      (*let max_edge_delta = min (breakeven_edges x) maxe_remaining in
      if max_edge_delta < 0 then (x, max_maxe, deltalogz)
      else *) begin
        Timer.start "delta_edges";
        
        if x.on_ev then begin
          nlogf "Spliting on evidence\n";
          if !half_split then x.e <- Some 5 else x.e <- Some 7;
        end else begin
          
          let max_edge_delta = min (breakeven_edges x) maxe_remaining in
          let e = Slearn.delta_edges !half_split circ anc max_edge_delta x in 

          (* Keep track of our worst case *)

          x.e <- Some e;
        end;

        Timer.stop "delta_edges";
        let e = match x.e with Some a->a in
        let max_maxe = max e max_maxe in
        rescore_split !quick x;
        SplitHSet.add seen x;
        (* DEBUG *)
        
        if score x > 0.0 then ( printf "first best score: %f\n"; score x;
          (x, max_maxe, deltalogz))
        else

           (vlogf "Recomputed e, edges: %d x.maxl: %f\n" e x.maxl;
           Heap.add discard_set x;
           best_split circ anc maxe_remaining max_maxe 
              split_set seen fseen n ev extra_evs_schema (it+1) feature_heap feature_list data schema ev_schema ev_matched_data_list evlogz discard_set 
          )
        (*        
        if Heap.size split_set > 0 && 
          mscore x < (1. -. !sloppy) *. mscore (Heap.min split_set) then begin
          (* DEBUG *)
          vlogfz "Score: %f (e fail)\n" (lazy (mscore x));
          Heap.add split_set x;
          best_split circ anc maxe_remaining max_maxe 
              split_set seen fseen n ev extra_evs_schema ev_logzs (it+1)
        end else
          (x, max_maxe)
          *)
      end
    end


let rec pick_best_split circ anc split_set feature_list n ev extra_evs_schema feature_heap ev_matched_data_list ev_schema schema data evlogz fseen discard_set =
  

  let maxe_remaining = !max_edges - Circuit.num_edges circ in
  let visited_splits = SplitHSet.create 100 in
  
  try begin
  while Heap.is_empty split_set do
     vlogf "No splits remaining!\n"; 
     expand_feature circ feature_list feature_heap split_set data schema ev ev_schema extra_evs_schema ev_matched_data_list evlogz fseen 
  done;


  (*let ds = Circuit.create_deriv_scratch circ in
  let logz = Circuit.compute_z (Circuit.create_scratch circ) circ in*)

    let (best, max_maxe,delta_logz) = 
      best_split circ anc maxe_remaining 0 split_set 
        visited_splits fseen n ev extra_evs_schema 1 feature_heap  feature_list data schema ev_schema ev_matched_data_list evlogz discard_set in

    nlogf "Recomputed: %d splits, %d features; worst-case edges: %d\n" 
      (SplitHSet.length visited_splits) (NMap.length fseen) max_maxe;
    nlogf "Best score: %f\n" (score best);
    if score best < 0. then 
      pick_best_split circ anc split_set feature_list n ev extra_evs_schema feature_heap ev_matched_data_list ev_schema schema data evlogz fseen discard_set
    else
      (best,delta_logz)
  end with Converged ->
    (* Don't keep going if we have < 1% of total edges remaining. *)
    
    if !shrink_edge_cost && maxe_remaining > !max_edges / 100  then begin
      if !edge_cost < 0.001 then raise Converged;  
      edge_cost := !edge_cost /. 2.;
      printf "Shrink the edge cost to %f.\n" !edge_cost;
      printf "Discarded split set size: %d\n" (Heap.size discard_set);
      edge_cost := !edge_cost /. 2.;
      for i=1 to Heap.size discard_set  do
        let s = Heap.min discard_set in
        Heap.remove_min discard_set;
        Heap.add split_set s;
      done;
      resort_splits split_set;
      pick_best_split circ anc split_set feature_list n ev extra_evs_schema feature_heap ev_matched_data_list ev_schema schema data evlogz fseen discard_set
    end
    (*
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
      pick_best_split circ anc split_set feature_list n ev extra_evs_schema feature_heap ev_matched_data_list ev_schema schema data evlogz fseen discard_set
    end 
      *)
    else 
      raise Converged


(* TODO revisit for multi-dimensions *)
let entropy varnums var_indeces data=
  let count = Array.init varnums (fun i->Array.make 2 0.0001) in  
  List.iter (fun x->Array.iteri (fun i v->count.(i).(v) <- count.(i).(v) +. 1.0) x ) data;
  let n = float_of_int (List.length data) in

  (* H_D(v) *)
  let h_d v = -.Array.fold_left (fun s c->let p = c /. n in (*printf "p=%f c=%f " p c;*) s +. p *. (log p)) 0.0 count.(v) in
  let total_h_d  = (Array.sumf_map (h_d ) var_indeces) /. (float_of_int (Array.length var_indeces)) in
  total_h_d 

(* TODO revisit for multi-dimensions *)
let info_gain h_d data varnums var_indeces var= 
  let n = float_of_int (List.length data) in
  let (d_t,d_f) = List.partition (fun x->x.(var) = 1) data in
  let parts =[|d_t; d_f|] in
  let entropies = Array.map (entropy varnums var_indeces ) parts in
  let sizes = Array.map (fun d-> float_of_int (List.length d)) parts in
  let scaled_entropies = Array.map2 (fun len h-> h *. len /. n) sizes entropies in
  let gain = h_d -.  (Array.sumf scaled_entropies) in
  gain 



let load_data_ev_schema filename evfilename schemafile =
  vlogf "load_data_ev_schema\n";
  
  flush_all();
  let data = Data.input_example_list (open_in filename) in
  (*let ev = Data.input_wexample_list (open_in evfilename) in
  *)
   
  let schema = 
    if schemafile <> "" then begin
      let schemain = open_in schemafile in
      let s = Data.input_example schemain in
      close_in schemain ; s
    end
    else Data.schema data in 
  
  
  let no_wev = Data.input_example_list (open_in evfilename) in
  let ev_schema = Array.map (fun i->i) (List.hd no_wev) in
  
  let orig_no_wev = List.map (Array.copy) no_wev in


  let extra_evs_schema = Array.map (fun i->i) (List.hd no_wev) in
  
  let ev_vars= Array.mapi (fun i x -> if x >= 0 then i else -1) ev_schema in
  let ev_vars = Array.of_list (List.filter (fun x-> x >= 0) (Array.to_list ev_vars)) in
  let ev_size = Array.length ev_vars in
  let varnums = Array.length (List.hd data) in
  if !info_percen < 100 then begin
    vlogf "Compute entropy\n";
    flush_all();
    let h_d = entropy varnums ev_vars data in
    vlogf "Compute information gain\n";
    
    flush_all();
    let gains = Array.map (fun v->(v, info_gain h_d data varnums ev_vars v)) ev_vars in
    Array.sort (fun (v1, g1) (v2, g2)-> if not (g1 = g1) then -1 else if not (g2 = g2) then 1 else if g1 > g2 then 1 else if g1 = g2 then 0 else -1)  gains;
    

    Array.iter (fun (v,g) -> vlogf "v: %d g:%f " v g ) gains;
    vlogf "\n";
    
    vlogf "sizes %d\n" (List.length no_wev);

    let len = (100 - !info_percen) * ev_size / 100 in
    let subset = Array.init len (fun i -> extra_evs_schema.(fst gains.(i)) <- -1; fst gains.(i) ) in

    vlogf "Original evidence set:\n";
    Array.iteri (fun i x-> if x >= 0 then vlogf "%d " i ) ev_schema; 
    vlogf "\n";


    (*let ev = Data.create_weighted_example no_wev in
      *)

(*  
    let e_data = Array.to_list (Array.map (example_to_ev circ.schema) data) in
    *)
    (*let ev_matched_data = List.filter (ev_intersect e) e_data in
    *)
    List.iter (fun e-> Array.iter (fun s-> e.(s) <- -1 ) subset ) no_wev;
    vlogf "Create weighted example\n";
    
    flush_all();
  end;
  let (ev, matched_data_list) = Data.create_weighted_example2 no_wev data in
  let ev_matached_data_list = List.map (fun g->List.map (example_to_ev schema) g ) matched_data_list in
  
  
  vlogf "\nev size:%d\n" (List.length ev);
  
  flush_all();
  let (w, ev0) = List.hd ev in
  
  let ev_schema = Array.map (fun i->i) ev0 in
  vlogf "Reduced evidence set:\n";
  Array.iteri (fun i x-> if x >= 0 then vlogf "%d " i ) ev0; 
  vlogf "\n";

  (*
  List.iter (fun wev-> let (w, e) = wev in printf "w: %f\n" w) ev; 
  *)


  let data = Array.of_list data in
  let ev = Array.of_list ev in
  let e_ev = Array.map (fun wev -> let (w,x) = wev in
      let e = example_to_ev schema x in
      (w,x,e) ) ev in

  if !info_percen < 100 then begin 
    let orig_wev = Data.create_weighted_example orig_no_wev in

    let orig_e_ev = Array.map (fun wev -> let (w,x) = wev in
      let e = example_to_ev schema x in
      (w,x,e) ) (Array.of_list orig_wev) in
    (data, e_ev, schema, ev_schema, extra_evs_schema, ev_matached_data_list, orig_e_ev, orig_no_wev )
  end else 
    (data, e_ev, schema, ev_schema, extra_evs_schema, ev_matached_data_list, e_ev, orig_no_wev )

let loglikelihood circ data =
  let scratch = Circuit.create_scratch circ in
  let n = float_of_int (Array.length data) in
  Array.sumf_map (Circuit.logprob_x scratch circ) data
    -. (n *. (Circuit.compute_z scratch circ))

let optimize_weights_stochastic circ feature_list true_counts n ev =
  let fa = Array.of_list feature_list in
  OptImpl.datall_optimize_stochastic !l1_param !prior_stddev circ true_counts n ev fa 100


let optimize_weights circ feature_list true_counts n ev maxiter =
  let fa = Array.of_list feature_list in
  OptImpl.datall_optimize  !l1_param !prior_stddev circ true_counts n ev fa maxiter
 
let one_step_optimize_weights circ feature_list true_counts n ev step_size =
  let fa = Array.of_list feature_list in
  OptImpl.one_step_optimize  !l1_param !prior_stddev circ true_counts n ev fa step_size

exception OutdatedLogz

let do_learn () =
  Timer.start "init";
  vlogf "Initializing...\n" ;

  (* Read in data and determine schema (number of values for each var) *)
  let (data, ev, schema, ev_schema, extra_evs_schema, ev_matched_data_list, orig_ev, orig_no_wev) = load_data_ev_schema !datafile !evfile !schemafile in 
  vlogf "Loaded data.\n";
  let n = (Array.length data) in
  
  (* Create initial circuit and set of distributions. (Initial
     distributions will be single-variable marginals.) *)
  
  

  flush_all();
  (* Initial log-likelihood *)
  (*let last_ll = ref (loglikelihood circ data) in
  nlogf "Initial log likelihood: %f\n" !last_ll;
  
  if !write_freq > 0 || !thresh then mkdir () ;
  nlogf "Initializing took %f seconds.\n" (Timer.elapsed "init");
  *)

  
 
  let feature_list = ref [] in
  let feature_counts = ref [] in
  let split_set = Heap.create split_lt 100 in
  let discard_set = Heap.create split_lt 500 in
  
  let circ  = if !initac = "" then begin 
      if !write_freq > 0 then mkdir();
      let (circ, initial_features, initial_counts, split_list, initial_feature_counts) = 
        Slearn.init !l1_param !prior_stddev schema data ev ev_schema extra_evs_schema ev_matched_data_list !split_cost !mu_thresh !max_ev !initedges in
      feature_list := initial_features;
      feature_counts := (Array.to_list (Array.transpose (Array.of_list initial_feature_counts)));
      List.iter (Heap.add split_set) split_list; 
      circ
    end else if !initac = "cl" then begin
      mkdir();
      let learn_cl = sprintf "libra condcl -i %s -ev %s -o %s.d/0.bn" !datafile !evfile !outfile in
      vlogf "learning cl using %s\n" learn_cl;
      ignore(Unix.system learn_cl);
      let acve = sprintf "libra acve -m %s.d/0.bn -o %s.d/0.ac" !outfile !outfile in
      ignore(Unix.system acve);
      let init_circ_file = sprintf "%s.d/0.ac" !outfile in
      let chan = open_in init_circ_file in
      let (circ, fl) = load_with_features chan in
      feature_list := fl;

      let counts = (OptImpl.empirical_all_features_probs circ (Array.of_list !feature_list) data ev ev_matched_data_list) in  
      feature_counts := (Array.to_list (Array.transpose (Array.of_list counts) ));
      circ
    end else begin
      if !write_freq > 0 then mkdir();
      let chan = open_in !initac in
      let (circ, fl) = load_with_features chan in
      feature_list := fl;
    
      let counts = (OptImpl.empirical_all_features_probs circ (Array.of_list !feature_list) data ev ev_matched_data_list) in  
      feature_counts := (Array.to_list (Array.transpose (Array.of_list counts) ));
      Timer.start "optimize"; 
      let all_f_counts = Array.transpose (Array.of_list !feature_counts) in
      ignore(optimize_weights circ !feature_list all_f_counts (float_of_int n) ev 100); 
      Timer.stop "optimize";
      if !write_freq > 0 then save_periodic circ !feature_list 0;
      circ
    end in
  vlogf "Initial lens: %d %d\n" (List.length !feature_counts) (List.length !feature_list);


  
  let lasttime = ref (Sys.time()) in
  
  
  let ev_logzs = ref [||] in  
  flush_all();

  ev_logzs := Array.map (fun wev -> let (w,x,e) = wev in
      let nlogz = compute_z_e circ e in
      (w,x,e,nlogz) ) ev;

  let feature_heap = Heap.create feature_comp 200 in

  let update_feature_heap f fcounts = 
      let f_support = ref 0.0 in
      for j = 0 to Array.length ev - 1 do
        let (w,_,_) = ev.(j) in
        f_support := !f_support +. w *. fcounts.(j);
      done;
      Heap.add feature_heap (f, !f_support) in
  
  let alpha = ref (1.0 /. ( float_of_int (Array.length data)) ) in 
  let step_size = ref (Array.make (List.length !feature_list) alpha)  in
  List.iter2 (fun f c-> if (List.length f.cond) > 1 then update_feature_heap f c ) !feature_list !feature_counts;

  (* Choose best split, one at a time.  If no split increases the
     score function, then we leave this loop early by raising an
     exception. *)
  let lastll = ref (-.100000000.0) in

  let i = ref 0 in
  (try
  
  let initsize = Heap.size split_set in
  
  (*let initsize = 0 in 
  *)
  while !i < !max_splits && !lasttime < !timelimit do
    incr i ;
    nlogf "\nSPLIT %d:\n" !i;
    (* DEBUG: print out z, to detect some types of circuit corruption. *)
    if log_exists log_debug then begin
      let scratch = Circuit.create_scratch circ in
      dlogf "z = %f\n" (Circuit.compute_z scratch circ) 
    end;
    Timer.start "pick";
    let anc = Slearn.build_ancestors circ in
    

    let fseen = NMap.create 100 in

    let split = if !i <= initsize then begin 
      let s = Heap.min split_set in
      Heap.remove_min split_set;
      
      let varId = var_var s.v in
      let (_,featureVarId,_) = List.hd s.acf.cond in
      Heap.iter ( fun sp ->         
        let svarId = var_var sp.v in
        let (_,sfeatureVarId,_) = List.hd sp.acf.cond in
        if svarId = varId || svarId = featureVarId ||  sfeatureVarId = varId || sfeatureVarId = featureVarId then begin
          sp.maxl <- sp.maxl +. 0.0;
        end
       ) split_set;
       Heap.rebuild split_set;
      
      s
    end
    else begin
      let (s, delta_logzs) = pick_best_split circ anc split_set !feature_list n ev extra_evs_schema  feature_heap ev_matched_data_list ev_schema schema data !ev_logzs fseen discard_set in
      s
    end  in 
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

    
    
    
    
    (*printf "selected split maxl = %f\n" split.maxl;    
    *)
    (* Apply the chosen split. *)
    let all_splits = if !quick  then [] 
                     else Heap.to_list split_set in
    let f_new = Slearn.split_feature !half_split circ !feature_list 
        anc all_splits split in

    (*prune_circuit circ.root; *)
    let new_step_size = Array.make (List.length f_new) alpha in
    Array.iteri (fun i s -> !step_size.(i) <- alpha  ) !step_size;

    
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


    (* Update feature and feature count lists *)

    
    feature_list := f_new @ !feature_list;
    step_size := Array.of_list ( (Array.to_list new_step_size) @ (Array.to_list !step_size));
    split_score_valid := false; 
    let dim = circ.schema.(var_var split.v)  in 
    let dimCounts = Array.make dim 0.0 in 
    (*printf "len: %d\n" dim;*)
    for j = 0 to Array.length ev - 1 do
      let (w,_,_) = ev.(j) in
      Array.iteri (fun d c-> dimCounts.(d) <- dimCounts.(d) +. w *. c ) split.prob.(j);
    done;
    for k= dim - 1 downto 0 do
      (*printf "dim:%d counts: %f\n" k dimCounts.(k); *)
      let fcounts = Array.map (fun r-> r.(k)) split.prob in
      feature_counts := fcounts :: !feature_counts; 
      Heap.add feature_heap (List.nth f_new (k), dimCounts.(k));
    done;
    


    if !write_freq > 0 && !i mod !write_freq = 0 then begin
      save_periodic circ !feature_list !i;
    end else begin
      Timer.start "evlogz";
      (*List.iter (fun f-> printf "f: %s\n" (string_of_feature f) ) !feature_list;
      *)

      (*let n = Array.length ev in
      (try 
        for t = 0 to 10 do
          let j = Random.int (n-1) in
          let (w,x,e,logz) = !ev_logzs.(j) in
          let new_logz = logz +. delta_logzs.(j) in
          let true_logz = compute_z_e circ e in
          if abs_float(  true_logz -. new_logz ) > 0.00001 then raise OutdatedLogz;
        done;
      
        for j = 0 to Array.length ev - 1 do
          let (w,x,e,logz) = !ev_logzs.(j) in
          !ev_logzs.(j) <- (w,x,e,logz+.delta_logzs.(j));
        done;
      
      with OutdatedLogz -> ( printf "Outdated logz!\n";
        List.iter (fun f->printf "feature cause outdated feature %s\n" (string_of_feature f) ) f_new;

      save_periodic circ !feature_list !i;

      ev_logzs := Array.map (fun wev -> let (w,x,e) = wev in
       let nlogz = compute_z_e circ e in
      (w,x,e,nlogz) ) ev;)
      );
      *)



      Timer.stop "evlogz";
    end;
   
    if !i = initsize then begin
      Timer.start "optimize"; 
      let all_f_counts = Array.transpose (Array.of_list !feature_counts) in
 
      nlogf "Joint weight optimization.\n";  
      List.iter (fun f-> dlogf "f: %s\n" (string_of_feature f) ) !feature_list ;
      ignore(optimize_weights circ !feature_list all_f_counts (float_of_int n) ev 100); 
    
      Timer.stop "optimize";
      Timer.start "evlogz";
      
      ev_logzs := Array.map (fun wev -> let (w,x,e) = wev in
        let nlogz = compute_z_e circ e in
        (w,x,e,nlogz) ) ev;

      Timer.stop "evlogz";
      if !write_freq > 0 then 
         save_periodic circ !feature_list 0;

    end
    else if !i > initsize then begin
      Timer.start "optimize"; 
      let all_f_counts = Array.transpose (Array.of_list !feature_counts) in
            
      (*

      ignore(optimize_weights circ !feature_list all_f_counts (float_of_int n) ev 3); 
      *) 
      
      let old_feature_value = List.map (feature_value) !feature_list in
      let llg = one_step_optimize_weights circ !feature_list all_f_counts (float_of_int n) ev !alpha in

      if llg < !lastll then begin
        alpha := !alpha /. 2.0;
        List.iter2 (fun f v->set_feature_value f v) !feature_list old_feature_value;
      end else lastll := llg;
     
      
      Timer.stop "optimize";


      Timer.start "evlogz";
      
      ev_logzs := Array.map (fun wev -> let (w,x,e) = wev in
       let nlogz = compute_z_e circ e in
      (w,x,e,nlogz) ) ev;

      Timer.stop "evlogz";
   end;
    (* Optimize all weights, starting from initial values *)
    
    (*set_feature_value (List.hd !feature_list) (fst split.wt);
    if not !half_split then
      set_feature_value (List.hd (List.tl !feature_list)) (snd split.wt);
   
    *)


    (*if !i = !max_splits then begin (* mod 2 = 0 then begin*)
      
      let new_ll = optimize_weights circ !feature_list !feature_counts n ev in
      nlogf "Actual delta ll: %f\n" (new_ll -. !last_ll);
    
      last_ll := new_ll;
   (* end;*)
    *)
    
    

   
  
  
     
    vlogf "Timing compute evlogz: %f\n" (Timer.last_elapsed "evlogz");
    (* Add and score new splits *)
    (*
    let ds = Circuit.create_deriv_scratch circ in
    *)
    (*let logz = Circuit.compute_z (Circuit.create_scratch circ) circ in *)
    (*
    if Heap.is_empty split_set then begin  
      if Heap.is_empty feature_heap then raise Converged;
      
      let fsplits f = Slearn.gen_splits !feature_list !l1_param !prior_stddev circ f data schema ev ev_schema extra_evs_schema ev_matched_data_list !approx_percen in
      (*let fl = List.map fsplits f_new in
      List.iter (List.iter (Heap.add split_set)) fl;
      *)
      let (f,su) = Heap.min feature_heap in
      vlogf "Extending feature %s with support : %f\n" (string_of_feature f) su;
      let fl = fsplits f in
      List.iter (Heap.add split_set) fl;

      vlogfz "Total number of potential splits: %d\n" (lazy (Heap.size split_set));
    end;
    *)
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
    nlogf "Times: gensplit: %0.4f evlogz:%0.4f deriv:%0.4f union:%0.4f\n" (Timer.elapsed "gensplit") (Timer.elapsed "evlogz") (Timer.elapsed "derivative") (Timer.elapsed "union");
           (*
    List.iter Timer.clear ["pick"; "split"; "update"; "efx"; "1dsearch";
      "delta_edges"; "compute_fsplit_ll"; "anc"]; *)
  done ;
  if !i >= !max_splits then nlogf "\nMaximum number of splits reached.\n" else nlogf "\nTime limit reached.\n";
  with Converged -> nlogf "\nLearning converged.\n" 
     | MaxEdges -> nlogf "\nMaximum number of edges reached.\n" );
  

  (*
  let nfeature_counts = OptImpl.empirical_all_features_probs circ (Array.of_list !feature_list) data ev ev_matched_data_list in  
 

  List.iter (fun c -> Array.iter (printf "%0.2f " )  c; printf "\n" ) (nfeature_counts);
  printf "\n";
  *)
  (*printf "number of counts: %d\n" (List.length !feature_counts);
  printf "number of feature: %d\n" (List.length !feature_list);
  *)
  if !i > 0 then begin (*!i mod !write_freq > 0 then begin *) 
    Timer.start "optimize"; 
    let all_f_counts = Array.transpose (Array.of_list !feature_counts) in
 
    vlogf "Final weight optimization:\n";  
    List.iter (fun f-> dlogf "f: %s\n" (string_of_feature f) ) !feature_list ;
    ignore(optimize_weights circ !feature_list all_f_counts (float_of_int n) ev 50); 
    
    Timer.stop "optimize";
    (*Array.iter (fun c -> Array.iter (printf "%0.2f " ) c; printf "\n" ) all_f_counts;
    printf "\n"; *)
  end;
  let t = Timer.elapsed "optimize" in
  save_circuit circ !feature_list;
  (* Compute the final log likelihood.  Doing it this way will
     always be correct, but could be rather slow... *)
  if not !no_ac then
    nlogf "Final log likelihood: %f\n" (((loglikelihood circ data) -. (loglikelihood circ (Array.of_list orig_no_wev))) /. (float_of_int (Array.length data))); 
  nlogf "Total time: %f seconds\n" (Sys.time ())


let main () = 
  Arg.parse args ignore usage;
  if !datafile = ""
    || !evfile = ""
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

  common_log_init ();
  do_learn ()

let _ = main ()
