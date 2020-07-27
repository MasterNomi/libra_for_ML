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

let _ = Random.self_init ()

(* Globals used for command line parameters *)
let qfile = ref ""
let evfile = ref ""
let modelfile = ref ""
let depnet = ref false 
let rao_blackwellized = ref true 
let prior = ref 1.0 
let compute_marg = ref false
let marg_outfile = ref ""
let sample_outfile = ref ""
let write_freq = ref (-1)
let sameev = ref false
let num_chains = ref (-1)
let num_burn_iter = ref (-1)
let num_sample_iter = ref (-1)
let speed = ref ""
(* let samplequeries = ref (-1) *)

let usage = "Usage: gibbs -m <model> [...]"
let args = Arg.align
 ([("-m", Arg.Set_string modelfile, " Model file (BN, MN, or DN)") ;
   ("-depnet", Arg.Set depnet, " Sample from CPD only, not Markov blanket");
   ("-mo", Arg.Set_string marg_outfile, " Output file for marginals") ;
   ("-so", Arg.Set_string sample_outfile, " Output file for samples") ;
   (* ("-sskip", Arg.Set_int sample_skip, " Skip samples") ; *)
   ("-marg", Arg.Set compute_marg, " Compute marginals") ;
   ("-ev", Arg.Set_string evfile, " Evidence file");
   ("-sameev", Arg.Set sameev, " Use the same evidence for all queries");
   ("-q",  Arg.Set_string qfile,  " Query file");
   ("-speed", Arg.Set_string speed, " Number of samples (fast/medium/slow/slower/slowest)");
(* ("-samplequeries", Arg.Set_int samplequeries, " Sample queries with replacement to obtain set number of queries"); *)
   ("-chains", Arg.Set_int num_chains, " Number of MCMC chains");
   ("-burnin", Arg.Set_int num_burn_iter, " Number of burn-in iterations");
   ("-sampling", Arg.Set_int num_sample_iter, " Number sampling iterations");
   ("-norb", Arg.Clear rao_blackwellized, " Disable Rao-Blackwellization");
   ("-prior", Arg.Set_float prior, " Total prior counts");
   (* Add parameters for max time, num chains, num burn-in, num samples *)
   ("-seed", Arg.Int Random.init, " Random seed")]
   @ common_arguments)

(* exception Converged *)


(* Select n queries at random, with replacement 
let sample_with_replacement queries n =
  let rand_q _ = queries.(Random.int (Array.length queries)) in
  Array.init n rand_q *)

let do_gibbs () =
  (* Read in files *)
  Timer.start "reading";
  let params = GibbsImpl.parse_gibbs_params !num_burn_iter 
    !num_sample_iter !num_chains !speed !rao_blackwellized !prior in

  (* GibbsImpl uses an abstract interface rather than the raw BN
   * or MN structure, and provides conversion methods to generate that
   * interface on-demand.
   *)
  let model =
    if Bn.filename_is_xmod !modelfile || 
        Bn.filename_is_bif !modelfile ||
        Bn.filename_is_cn !modelfile then 
      let bn = Bn.load_auto !modelfile in
      if !depnet then GibbsImpl.dn_model bn
      else GibbsImpl.bn_model bn
      (* else GibbsImpl.mn_model (Bn.to_mn bn) (* GibbsImpl.bn_model bn *) *)
    else
      let mn = Mn.load_auto !modelfile in
      GibbsImpl.mn_model mn in

  let schema = model.GibbsImpl.schema in

  if !evfile = "" then sameev := true;
  let evidence = 
    if !evfile = "" then [| [||] |]
    else begin
      let wel = Data.input_wexample_list (open_in !evfile) in
      let el = List.map snd wel in
      List.iter (Data.check_point schema) el;
      Array.of_list el 
    end in
  let (qweights, queries) =
    if !qfile = "" then ([], [||])
    else begin
      let wql = Data.input_wexample_list (open_in !qfile) in
      let wl = List.map fst wql in
      let ql = List.map snd wql in
      List.iter (Data.check_point schema) ql;
      (wl, Array.of_list ql)
    end in
  (* Shorten the evidence and queries so that they match *)
  let (evidence, queries) =
    if !sameev then
      ([|evidence.(0)|], queries)
    else if !qfile = "" then
      (evidence, queries)
    else
      let minlen = 
        min (Array.length evidence) (Array.length queries) in
      (Array.sub evidence 0 minlen, Array.sub queries 0 minlen) in
  (* let queries =
    if !samplequeries > 0 then
      sample_with_replacement queries !samplequeries
    else queries in *)
  vlogf "Reading time: %fs\n" (Timer.elapsed "reading");

  (* Open file for writing samples.  Write a sample out once
   * we've resampled all variables once. *)
  let first_var = ref (-1) in
  let out =
    if !sample_outfile <> "" then begin
      let o = open_out !sample_outfile in
      fun state i oldval newval probs ->
        if !first_var < 0 then first_var := i;
        if i = !first_var then
          Data.output_example o state
    end else
      GibbsImpl.dummy_on_sample in

  (* Continuation that prints out marginals, if desired *)
  let marg_testid = ref 1 in
  let mout = if !marg_outfile <> "" then open_out !marg_outfile 
             else log_stream log_normal in
  let process_marginals marg =
    if !marg_outfile <> "" || (!qfile = "" && !sample_outfile = "") then begin
      fprintf mout "Marginals for evidence %d:\n" !marg_testid;
      incr marg_testid;
      Data.output_marginals mout marg 
    end in
    
  (* Run inference for each piece of evidence *)
  let lprobs = ref [] in
  let times = ref [] in
  for i = 0 to Array.length evidence - 1 do
    (* Select evidence and queries.  If sameev is true, use all
     * queries in a single batch. *)
    let ev = evidence.(i) in
    let q =
      if !sameev then queries
      else if Array.length queries = 0 then [||]
      else [|queries.(i)|] in
    (* Run sampling, save results *)
    let (newprobs, time) =
      if !compute_marg then
        GibbsImpl.run_gibbs_marg params process_marginals out model ev q
      else
        GibbsImpl.run_gibbs_joint params out model ev q in
    lprobs := newprobs :: !lprobs;

    (* Add correct time to list, just once *)
    times := time :: !times;
    (* Add dummy times for all queries done at the same time *)
    for i = 1 to Array.length newprobs - 1 do
      times := 0.0 :: !times
    done
  done;

  (* Print results *)
  let lprobs = List.concat (List.rev (List.map Array.to_list !lprobs)) in
  let times = List.rev !times in 
  if Array.length queries > 0 then begin
    if log_exists log_verbose then
      List.iter2 (nlogf "%f %f\n") lprobs times
    else
      List.iter (nlogf "%f\n") lprobs;
    let s = stats_make () in
    List.iter2 (stats_wadd s) qweights lprobs;
    nlogf "avg = %f +/- %f\n" (stats_mean s) (stats_stderr s)
  end


let main () = 
  Timer.start "total";
  Arg.parse args ignore usage;
  if !modelfile = "" then 
    (Arg.usage args usage; exit (-1));
  common_log_init();
  (* By default, compute and print out the marginals *)
  if !marg_outfile <> "" || (!sample_outfile = "" && !qfile = "") 
    then compute_marg := true;
  do_gibbs();
  vlogf "Total time: %fs\n" (Timer.elapsed "total")

let _ = main ()
