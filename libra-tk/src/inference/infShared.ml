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

let condition_marginals ev marginals =
  if Array.length ev = 0 then marginals
  else begin
    let cond_one e m = 
      if e < 0 then m
      else begin
        let m' = Array.make (Array.length m) log_zero in
        m'.(e) <- log_one;
        m'
      end in
    Array.map2 cond_one ev marginals
  end

let __run_marg_cond mpe get_marg schema outfile evfile qfile sameev xfile = 
  (* Process arguments, open streams *)
  let evstream = if evfile <> "" then open_in evfile else stdin in 
  let qstream = if qfile <> "" then open_in qfile else stdin in 
  let xstream = if qfile <> "" then open_in xfile else stdin in 
  let sameev = if evfile = "" then true else sameev in
  let sharedmarg = 
    if sameev then begin
      Timer.start "inf";
      let (_, ev) = if evfile = "" then (1.0, [||])
                    else Data.input_wexample evstream in
      let ev = Data.check_evidence schema ev in
      let x = Data.input_evidence xstream in 
      let sm = get_marg ev x in
      vlogf "Inference time: %fs\n" (Timer.elapsed "inf");
      (ev, sm)
    end else ([||], [||]) in
  let mout = if outfile <> "" then open_out outfile 
             else log_stream log_normal in

  let first_marginal = ref true in
  let sl = stats_make () in
  let st = stats_make () in
  begin
  try while true do
    Timer.start "query";
    let (ev, marg) = 
      if sameev then
        sharedmarg 
      else begin
        let (_, ev) = Data.input_wexample evstream in
        let ev = Data.check_evidence schema ev in
                let x = Data.input_evidence xstream in 
        (ev, get_marg ev x) 
      end in
    dlogf "Marginals before conditioning on evidence:\n";
    if log_exists log_debug then
      Data.output_marginals (log_stream log_debug) marg;
    let marg = condition_marginals ev marg in
    dlogf "Marginals after conditioning on evidence:\n";
    if log_exists log_debug then
      Data.output_marginals (log_stream log_debug) marg;

    (* Write out marginals or MPE states *)
    if outfile <> "" || qfile = "" then begin
      (* Separate two marginal sets with a blank line *)
      if !first_marginal then
        first_marginal := false
      else if not mpe then
        output_string mout "\n";
      if mpe then begin
        (* Print out MPE state *)
        let mpe_state = Array.map Array.argmax marg in
        Data.output_example mout mpe_state
      end else
        (* Print out marginal *)
        let marg = Array.map (Array.map exp) marg in
        Data.output_marginals mout marg
    end;

    if qfile <> "" then begin
      let (qw, q) = Data.input_wexample qstream in
      Data.check_point schema q;
      let loss =
        (* Compute Hamming loss over non-evidence query variables *)
        if mpe then begin
          let mpe_state = Array.map Array.argmax marg in 
          let numq = ref 0 in
          let numdiff = ref 0 in
          for i = 0 to Array.length q - 1 do
            if q.(i) >= 0 && ev.(i) < 0 then begin
              incr numq;
              if q.(i) != mpe_state.(i) then 
                incr numdiff
           end
          done;
          if !numq > 0 then
            (float_of_int !numdiff) /. (float_of_int !numq)
          else
            0.0
        end else
          let llvar var vmarg =
            if q.(var) >= 0 && ev.(var) < 0 then vmarg.(q.(var)) else 0. in
          Array.sumf (Array.mapi llvar marg) in
      let delta_t = Timer.delta "query" in
      if log_exists log_verbose then
        vlogf "%f %f\n" loss delta_t
      else
        nlogf "%f\n" loss;
      stats_wadd sl qw loss;
      stats_add st delta_t
    end;
    (* Stop immediately if we only have a single piece of 
     * evidence and no queries. *)
    if sameev && qfile = "" then raise Data.Eof
  done with Data.Eof -> ()
  end;
  (sl, st)

let __run_marg mpe get_marg schema outfile evfile qfile sameev= 
  (* Process arguments, open streams *)
  let evstream = if evfile <> "" then open_in evfile else stdin in 
  let qstream = if qfile <> "" then open_in qfile else stdin in 
  let sameev = if evfile = "" then true else sameev in
  let sharedmarg = 
    if sameev then begin
      Timer.start "inf";
      let (_, ev) = if evfile = "" then (1.0, [||])
                    else Data.input_wexample evstream in
      let ev = Data.check_evidence schema ev in
      let sm = get_marg ev in
      vlogf "Inference time: %fs\n" (Timer.elapsed "inf");
      (ev, sm)
    end else ([||], [||]) in
  let mout = if outfile <> "" then open_out outfile 
             else log_stream log_normal in

  let first_marginal = ref true in
  let sl = stats_make () in
  let st = stats_make () in
  begin
  try while true do
    Timer.start "query";
    let (ev, marg) = 
      if sameev then
        sharedmarg 
      else begin
        let (_, ev) = Data.input_wexample evstream in
        let ev = Data.check_evidence schema ev in
        (ev, get_marg ev) 
      end in
    dlogf "Marginals before conditioning on evidence:\n";
    if log_exists log_debug then
      Data.output_marginals (log_stream log_debug) marg;
    let marg = condition_marginals ev marg in
    dlogf "Marginals after conditioning on evidence:\n";
    if log_exists log_debug then
      Data.output_marginals (log_stream log_debug) marg;

    (* Write out marginals or MPE states *)
    if outfile <> "" || qfile = "" then begin
      (* Separate two marginal sets with a blank line *)
      if !first_marginal then
        first_marginal := false
      else if not mpe then
        output_string mout "\n";
      if mpe then begin
        (* Print out MPE state *)
        let mpe_state = Array.map Array.argmax marg in
        Data.output_example mout mpe_state
      end else
        (* Print out marginal *)
        let marg = Array.map (Array.map exp) marg in
        Data.output_marginals mout marg
    end;

    if qfile <> "" then begin
      let (qw, q) = Data.input_wexample qstream in
      Data.check_point schema q;
      let loss =
        (* Compute Hamming loss over non-evidence query variables *)
        if mpe then begin
          let mpe_state = Array.map Array.argmax marg in 
          let numq = ref 0 in
          let numdiff = ref 0 in
          for i = 0 to Array.length q - 1 do
            if q.(i) >= 0 && ev.(i) < 0 then begin
              incr numq;
              if q.(i) != mpe_state.(i) then 
                incr numdiff
           end
          done;
          if !numq > 0 then
            (float_of_int !numdiff) /. (float_of_int !numq)
          else
            0.0
        end else
          let llvar var vmarg =
            if q.(var) >= 0 && ev.(var) < 0 then vmarg.(q.(var)) else 0. in
          Array.sumf (Array.mapi llvar marg) in
      let delta_t = Timer.delta "query" in
      if log_exists log_verbose then
        vlogf "%f %f\n" loss delta_t
      else
        nlogf "%f\n" loss;
      stats_wadd sl qw loss;
      stats_add st delta_t
    end;
    (* Stop immediately if we only have a single piece of 
     * evidence and no queries. *)
    if sameev && qfile = "" then raise Data.Eof
  done with Data.Eof -> ()
  end;
  (sl, st)

let run_marg mpe get_marg schema outfile evfile qfile sameev  = 
  Timer.start "total";

  let (sl, st) = __run_marg mpe get_marg schema outfile evfile qfile sameev in

  (* Print stats *)
  if stats_n sl > 0. then begin
    if classify_float (stats_stderr st) = FP_nan then
      vlogf "time = %f +/- 0\n" (stats_mean st)
    else
      vlogf "time = %f +/- %f\n" (stats_mean st) (stats_stderr st);
    nlogf "avg = %f +/- %f\n" (stats_mean sl) (stats_stderr sl);
  end;
  vlogf "Total time: %fs\n" (Timer.elapsed "total")


let run_marg_cond mpe get_marg schema outfile evfile qfile sameev xfile = 
  Timer.start "total";

  let (sl, st) = __run_marg_cond mpe get_marg schema outfile evfile qfile sameev xfile in

  (* Print stats *)
  if stats_n sl > 0. then begin
    if classify_float (stats_stderr st) = FP_nan then
      vlogf "time = %f +/- 0\n" (stats_mean st)
    else
      vlogf "time = %f +/- %f\n" (stats_mean st) (stats_stderr st);
    nlogf "avg = %f +/- %f\n" (stats_mean sl) (stats_stderr sl);
  end;
  vlogf "Total time: %fs\n" (Timer.elapsed "total")
