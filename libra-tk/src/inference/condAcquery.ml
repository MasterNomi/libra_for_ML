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

(* Globals used for command line parameters *)
let acfile = ref ""
let qfile = ref ""
let evfile = ref ""
let xfile = ref ""
let marg_outfile = ref ""
let sameev = ref false
let marg = ref false
let marg_outfile = ref ""
let preprune = ref false
let mpe = ref false
let outfile = ref ""

let load_circuit filename =
  try Condcirc.load (open_in filename) 
  with Condcirc.Unknown_child(n,c) -> 
         nlogf "Error: node %s references unknown child node %s.\n" n c; 
         exit 0 

let usage = "Usage: acquery -m <ac file> -q <query> -ev <evidence> [...]"
let args = Arg.align
  ([("-m", Arg.Set_string acfile, " Input arithmetic circuit");
    ("-ev", Arg.Set_string evfile, " Evidence file");
    ("-q",  Arg.Set_string qfile,  " Query file");
    ("-x",  Arg.Set_string xfile,  " Fix evidence file");

 	  ("-o",  Arg.Set_string outfile,  " Inference result file");
    ("-sameev", Arg.Set sameev, " Use the same evidence for all queries");
    ("-preprune", Arg.Set preprune, " Prune circuit for the evidence");
    ("-marg", Arg.Set marg, " Use conditional marginals instead of joint distribution");
    ("-mpe", Arg.Set mpe, " Compute most probable explanation (MPE) state");
    ("-mo", Arg.Set_string marg_outfile, " Output file for marginals or MPE")]
    @ common_arguments)

let answer_joint_queries circ query_buf evstream xstream =
  let scratch = Condcirc.create_scratch circ in

  (* Handle shared evidence: 
     Read just one piece of evidence and compute its probability only once. *)
  if !evfile = "" then sameev := true;
  let (_, sharedev) =
    if !sameev && !evfile <> "" then 
      Data.input_wexample evstream
    else
      (* Create "dummy" evidence if none specified. *)
      (1.0, Array.make (Array.length circ.Condcirc.schema) (-1)) in 
  let logp_shared_ev x= 
    if !sameev then begin
      (* Pruning the circuit for the evidence may speed up resulting
       * queries significantly, by shrinking the size of the circuit. *)
      if !preprune then
        Condcirc.prune_for_evidence circ sharedev; 
      Condcirc.logprob_y scratch circ sharedev x 
    end else
      0.0 in

  (* Compute each log conditional probability... *)
  let sl = stats_make () in
  let st = stats_make () in
  begin try 
    while true do 
      (* By definition of conditional probability:
           log P(q|e) = log P(q) - log P(e) *)
      Timer.clear "query";
      Timer.start "query";
      let (qweight, q) = Data.input_wexample query_buf in
      (* Input validation *)
      Data.check_point circ.Condcirc.schema q;
      let e = if !sameev then sharedev 
              else Data.input_example evstream in
			let x = Data.input_evidence xstream in
      (* Check for compatibility between evidence and query. *)
      if Array.length e > 0 then
        Data.check_point circ.Condcirc.schema e;
      let mismatch = ref false in
      for i = 0 to Array.length e - 1 do
        if e.(i) >= 0 then begin
          if q.(i) < 0 then 
            q.(i) <- e.(i)
          else if q.(i) != e.(i) then
            mismatch := true
        end
      done;

      (* Compute probability of query and evidence *)
      let logp_q = if !mismatch then log 0. 
                   else Condcirc.logprob_y scratch circ q x in
      let logp_ev = if !sameev then logp_shared_ev x
                    else Condcirc.logprob_y scratch circ e x in
      let l = logp_q -. logp_ev in
      let dt = Timer.elapsed "query" in
      if log_exists "outinf" then
        logf "outinf" "%f %f\n" l dt
      else
        nlogf "%f\n" l;
      stats_wadd sl qweight l;
      stats_add st dt
    done 
  with Data.Eof -> () end;
  (sl, st)

(* Construct an artificial marginal, matching the given complete
   instance x. *)
let fake_logmarginals schema x =
  let m = Array.map (fun dim -> Array.make dim log_zero) schema in
  Array.iteri (fun i v -> m.(i).(v) <- log_one) x;
  m

let main () = 
  Timer.start "total";

  (* Parse arguments *)
  Arg.parse args ignore usage;
  if !marg_outfile <> "" then marg := true;
  if !acfile = "" || !qfile = ""  then
    (Arg.usage args usage; exit 0);

  common_log_init ();
  let circ = load_circuit !acfile in
	if !outfile <> "" then
	begin
		let inference_channel = open_out !outfile in
		Ext.register_log "outinf" inference_channel
	end;

  (* Handle marginals and MPE using shared code. *)
  if !marg || !mpe then begin
    let ds = Condcirc.create_deriv_scratch circ in
    let schema = circ.Condcirc.schema in
    (if !mpe then 
       let f ev x = fake_logmarginals schema (Condcirc.get_mpe circ ds ev x) in
       InfShared.run_marg_cond true f schema !marg_outfile 
         !evfile !qfile !sameev !xfile
     else
       let f ev x = Condcirc.get_logmarginals circ ds ev x in
       InfShared.run_marg_cond false f schema !marg_outfile 
         !evfile !qfile !sameev !xfile)
  end else begin
    (* Circuit.update_parents circ; *)
    let evstream = if !evfile <> "" then open_in !evfile else stdin in 
    let qstream = if !qfile <> "" then open_in !qfile else stdin in
		let xstream = if !xfile <> "" then open_in !xfile else stdin in

    (* Run inference *)
    let (sl, st) = answer_joint_queries circ qstream evstream xstream in

    (* Print stats *)
    if stats_n sl > 0. then begin
      if classify_float (stats_stderr st) = FP_nan then
        vlogf "time = %f +/- 0\n" (stats_mean st)
      else
        vlogf "time = %f +/- %f\n" (stats_mean st) (stats_stderr st);
      nlogf "avg = %f +/- %f\n" (stats_mean sl) (stats_stderr sl);
      logf "outinf" "avg = %f +/- %f\n" (stats_mean sl) (stats_stderr sl);
    end;
    vlogf "Total time: %fs\n" (Timer.elapsed "total")
  end

;;
main ()
