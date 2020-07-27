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
open OptImpl 

let _ = Random.self_init ()

(* Globals used for command line parameters *)
let evfile = ref ""
let acfile = ref ""
let modelfile = ref ""
let datafile = ref ""
let mfile = ref ""
let schemafile = ref ""
let outfile = ref ""
let write_freq = ref (-1)
let gibbs = ref false
let gspeed = ref ""
let num_chains = ref (-1) 
let num_burn_iter = ref (-1) 
let num_sample_iter = ref (-1)
let depnet = ref false
let rao_blackwellized = ref true
(* TODO...
let unlimited_time = ref true
let maxh = ref (-1)
let maxm = ref (-1)
let maxs = ref (-1)
*)
let maxiter = ref 100

let usage = "Usage: acopt -m <circuit> -o <output> [...]"
let args = Arg.align
 ([("-m",  Arg.Set_string acfile, " Arithmetic circuit to optimize") ;
   ("-o",  Arg.Set_string outfile, " Output circuit") ;
   ("-ma", Arg.Set_string modelfile, " Model to approximate (BN or MN)") ;
   ("-i",  Arg.Set_string datafile, " Empirical distribution to approximate") ;
   ("-gibbs", Arg.Set gibbs, " Approximate sampled empirical distribution [false]") ;
   ("-gspeed", Arg.Set_string gspeed, " Number of samples (fast/medium/slow/slower/slowest)");
   ("-gc", Arg.Set_int num_chains, " Number of MCMC chains ");
   ("-gb", Arg.Set_int num_burn_iter, " Number of burn iterations");
   ("-gs", Arg.Set_int num_sample_iter, " Number sampling iterations");
   ("-gdn", Arg.Set depnet, " Sample from CPD instead of Markov blanket");
   ("-norb", Arg.Clear rao_blackwellized, " Disable Rao-Blackwellization");
(* ("-gprior", Arg.Set_float gibbs_prior, " Prior counts for smoothing Gibbs estimates"); *)
   ("-seed", Arg.Int Random.init, " Random seed");
   ("-ev", Arg.Set_string evfile, " Evidence file");
   ("-init", Arg.Set_string mfile, " Initial variable marginals");
   (* TODO...
   ("-maxh", Arg.Set_int maxh, " Maximum number of hours to run");
   ("-maxm", Arg.Set_int maxm, " Maximum number of minutes to run");
   ("-maxs", Arg.Set_int maxs, " Maximum number of seconds to run"); *)
   ("-maxiter", Arg.Set_int maxiter, " Maximum number of optimization iterations [100]")]
   @ common_arguments)


(* Use Gibbs sampling to compute the probability of each feature. *)
let gibbs_feature_probs params model ev fa =

  (* Initialization *)
  Timer.start "sampling";
  let counts = Array.make (Array.length fa) 0 in 
  let n = ref 0 in
  let first_var = ref (-1) in 

  (* Define sampling callback: Increase counts for each matched state. *)
  let on_sample state i oldval newval probs =
    (* Keep track of first var we see.  When we see it again, we've
       resampled all the vars once. *)
    if !first_var < 0 then first_var := i;
    (* Only add counts after resampling all vars (this is faster) *)
    if !first_var = i then begin
      incr n;
      for fi = 0 to Array.length fa - 1 do
        if Circuit.f_satisfied state fa.(fi) then
          counts.(fi) <- counts.(fi) + 1
      done 
    end in
  GibbsImpl.run_gibbs params model ev GibbsImpl.dummy_on_init on_sample;

  (* Normalize *)
  let n = float_of_int !n in
  let fprobs = Array.map (fun c -> (float_of_int c) /. n) counts in
  nlogf "Sampling time: %fs\n" (Timer.delta "sampling");
  fprobs


(* 
NOTES ON AN ALTERNATE GIBBS SAMPLING APPROACH:

Suppose that we make a list of features that depend on each variable,
or even each variable value.  We then keep track of the number of
unsatisifed conditions for each feature and update it whenever we
resample a variable that it depends on.  Therefore, in a single Gibbs
sampling iteration (that is, resampling all variables once), a feature
with k conditions would be considered k times.

In contrast, if we test all features once per sampling iteration (say,
at the very end), then we'll test at most k conditions for a feature
with k conditions, but more commonly, we'll test an average of 2
(assuming mutually exclusive and exhaustive features in our CPDs).

Therefore, we can compute the relative slow-down of the alternate
approach.  For Audio, the average feature length is 7.7 tests,
suggesting a factor of 4 slow-down (or more, due to constant factors).
The advantage of the alternate approach is that it gives us a slightly
finer-grained sampling: a single feature could become true, then
false, then true again through the course of a single sampling
iteration.  Second, it allows us to do Rao-Blackwellisation, so that
we add fractional counts whenever we resample a variable that has the
potential to make the feature satisfied.

At this time, we do not deem the extra overhead and complexity to be
worth it.
*)

let save_circuit circ feature_list =
  let out = open_out !outfile in
  Circuit.output_with_features out circ feature_list 

(* exception Converged *)

(* UNUSED
let get_all_probs circ mfa fa =
  let fweights = Array.map (fun f -> Node.const_value f.Circuit.acnode) fa in
  let ds = Circuit.create_deriv_scratch circ in
  let probs = create_probs (Array.length fa) (Array.length mfa) 
    (Array.length circ.flat_vnodes) in
  infer_all circ ds mfa fa fweights 
  *)

let do_optimize_weights () =
  vlogf "Initializing...\n" ;

  (* 
   * Read in circuit and condition on evidence 
   *) 
  let (circ, fl) = Circuit.load_with_features (open_in !acfile) in 
  nlogf "Num variables = %d\n" (Array.length circ.Circuit.schema) ; 

  (* Read in one piece of evidence to condition *)
  let evidence = 
    if !evfile = "" then 
      Array.make (Circuit.numvars circ) (-1) 
    else 
      Data.input_example (open_in !evfile) in

  if !mfile <> "" then begin
    let marg = Data.input_marginals (open_in !mfile) in
    if Array.length marg < Circuit.numvars circ then begin
      nlogf "ERROR: Read %d marginals; expected %d\n" 
        (Array.length marg) (Circuit.numvars circ);
      exit (-1);
    end;

    (* Set feature probability to the marginal probability of the
     * first variable in the condition list.  By convention, this
     * is the ``child'' variable for the CPD.
     *)
    let set_prob f = 
      let (sense, var, value) = List.hd f.Circuit.cond in
      let p = 
        if sense then 
          marg.(var).(value)
        else 
          1. -. marg.(var).(value) in
      Circuit.set_feature_value f (log p) in
    List.iter set_prob fl
    (* DEBUG 
    ; Circuit.output stdout circ;
    exit 0 *)
  end;


  (* Condition circuit and features on evidence *)
  let orig_num_features = List.length fl in

  if (Array.length evidence) != Circuit.numvars circ then
    (nlogf "Error: Evidence is wrong size or format!\n"; exit (-1));
  if log_exists log_verbose then begin
    vlogf "Circuit before pruning:\n";
    Circuit.output (log_stream log_verbose) circ
  end;
  let fl = Circuit.prune_for_evidence_with_features circ fl evidence in
  Circuit.update_parents circ;

  if log_exists log_verbose then begin
    vlogf "Circuit after pruning:\n";
    Circuit.output (log_stream log_verbose) circ ; 
  end;

  nlogf "Number of circuit features = %d\n" orig_num_features;
  nlogf "Number of pruned circuit features = %d\n" (List.length fl); 

  (* Input validation *)
  let check_for_const f =
    let n = Circuit.feature_node f in
    if not (Circuit.is_const n) then
      nlogf "ERROR: Feature references to non-constant node %d!\n" 
        (Circuit.id n) in
  List.iter check_for_const fl;

  let fa = Array.of_list fl in 
  if !datafile <> "" then begin
    let truecounts = Array.make (Array.length fa) 0.0 in
    let numex = ref 0 in
    let inc_counts a x =
      for i = 0 to Array.length fa - 1 do
        if Circuit.f_satisfied x fa.(i) then
          a.(i) <- a.(i) +. 1.
      done in
    let datastream = open_in !datafile in 
    try while true do
      let x = Data.input_example datastream in
      inc_counts truecounts x; 
      incr numex
    done with Data.Eof -> () ;
    let numex = float_of_int !numex in
    let normcounts = Array.map (fun x -> x /. numex) truecounts in
    let kld = datakl_optimize circ normcounts evidence fa !maxiter in
    nlogf "Final K-L divergence is: %f\n" kld ; 
    nlogf "Optimization took %f seconds.\n" (Sys.time ())
  end
  (* 
   * Match feature expectations computed with Gibbs sampling
   *)
  else if !gibbs then begin
    let params = GibbsImpl.parse_gibbs_params 
      !num_burn_iter !num_sample_iter !num_chains !gspeed 
      !rao_blackwellized 0.0 in
    (* Load model *)
    let model = 
      match filetype !modelfile with
      | BNFile ->
        let bn = Bn.load_auto !modelfile in
        if !depnet then GibbsImpl.dn_model bn 
                   else GibbsImpl.bn_model bn 
      | MNFile ->
        let mn = Mn.load_auto !modelfile in
        GibbsImpl.mn_model mn 
      | _ ->
        nlogf "ERROR: Unsupported model type.\n";
        exit (-1) in
    (* Run sampling *)
    let fprobs = gibbs_feature_probs params model evidence fa in
    let kld = datakl_optimize circ fprobs evidence fa !maxiter in
    nlogf "Final K-L divergence is: %f\n" kld ; 
    nlogf "Optimization took %f seconds.\n" (Sys.time ())
  end
  else begin

  (* 
   * Load and condition model 
   *)
  let mn = match filetype !modelfile with
    | BNFile -> Bn.to_mn (Bn.load_auto !modelfile)
    | MNFile -> Mn.load_auto !modelfile
    | _ -> nlogf "ERROR: Unsupported file type.\n"; exit (-1) in
  if Mn.numvars mn != Circuit.numvars circ then
    (printf "Error: BN has different number of variables than circuit!\n";
    exit (-1));
  let mn = Mn.simplify mn evidence in
  let mfa = Array.of_list (Mn.to_features mn) in
  nlogf "Num MN features = %d\n" (Array.length mfa);

  (* Optimize weights using simple gradient methods *)
  let kld = optimize circ mfa fa !maxiter in
  nlogf "Final K-L divergence is: %f\n" kld ; 
  nlogf "Optimization took %f seconds.\n" (Sys.time ());

  (* Final weight optimization *)

  (* Option 2: Optimize the weights associated with each pruned CPT
   * as a set.  Iterate through the CPTs until we are no longer
   * improving.
   *)
  (*
  let fweights = Array.map (fun f -> Node.const_value f.acnode) !fa in
  let gweights = Array.map (fun bf -> bf.bparam) mfa in 
  let lastkl = ref (infer_kld_value circ mfa !fa) in
  let currkl = ref 0.0 in
  let continue = ref true in
  while !continue do
    for i = 0 to Array.length circ.Circuit.schema - 1 do
      let fa_id = Array.mapi (fun i f -> (i,f)) !fa in
      let fa_idl = Array.to_list fa_id in
      let ja_idl = List.filter (fun (j,f) -> f.var=i) fa_idl in
      let ja = Array.of_list (List.map fst ja_idl) in
      let p = get_all_probs circ mfa !fa in
      let new_kl = optimize_n_simple fweights gweights p ja in
      Array.iter (fun j -> !fa.(j).acnode.Node.details 
                             <- Node.ConstNode fweights.(j)) ja;
      currkl := new_kl
    done ;
    if !lastkl -. !currkl < 0.00001 then 
      continue := false ;
    lastkl := !currkl
  done ; 
  let kld = infer_kld_value circ mfa !fa in
  *)

  (* Option 3: Optimize weight sets approximately, using correlation
   * propagation.
   *)
  (* TODO -- this will require creating a new version of
   * infer_and_optimize_one that handles n mutually exclusive features
   * instead.
   *)

  (* Option 4: Create weight sets where features are, empirically,
   * mutually exclusive.  Potentially much faster and more general
   * than Option 3, but also could be less accurate. 
   *)
  (* TODO -- this might end up being the fastest and best method overall. *)
  end ;

  save_circuit circ fl;
  nlogf "Total time: %f seconds\n" (Sys.time ())

let main () = 
  Arg.parse args ignore usage;
  if !acfile = "" || (!modelfile = "" && !datafile = "") || !outfile = "" then
    Arg.usage args usage
  else begin
  (* TODO...
    (* Process maximum time limit stuff *)
    if !maxs >= 0 || !maxh >= 0 || !maxm >= 0 then 
      unlimited_time := false;
    if !maxs < 0 then maxs := 0;
    if !maxm < 0 then maxm := 0;
    if !maxh < 0 then maxh := 0;
    maxs := !maxs + 60 * (!maxm + 60 * !maxh); *)
    common_log_init ();
    do_optimize_weights ()
  end

let _ = main ()
