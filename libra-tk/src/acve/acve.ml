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
let modelfile = ref ""
let outfile = ref ""
let threshold = ref 0.0
let onlyprune = ref false
let prethresh = ref false
let maxsize = ref 0.0

let usage = "Usage: acve -m <model> [-o <circuit>] [...]"
let args = Arg.align
 ([("-m", Arg.Set_string modelfile, " Model file (BN or MN)") ;
   ("-o", Arg.Set_string outfile, " Output circuit")] 
(* EXPERIMENTAL AC pruning strategies -- DISABLED FOR NOW.
   ("-t", Arg.Set_float threshold, " Approximation threshold") ;
   ("-pt", Arg.Set prethresh, " Apply threshold in advance") ;
   ("-ms", Arg.Set_float maxsize, " Maximum ADD size") ;
   ("-p", Arg.Set onlyprune, " Simple pruning method") *)
   @ common_arguments)

let main () =
  Arg.parse args ignore usage;
  if !modelfile = "" (* || !outfile = "" *) then
    Arg.usage args usage
  else begin
    common_log_init ();
    let mn =
      match filetype !modelfile with
      | BNFile -> Bn.to_mn (Bn.load_auto !modelfile)
      | MNFile -> Mn.load_auto !modelfile
      | _ -> nlogf "ERROR: Unsupported file type.\n"; exit (-1) in
    let schema = Mn.schema mn in
    let buildac = !outfile <> "" in
    let (acroot, adds) = 
      if !prethresh then
        if !onlyprune then AcveImpl.varelim_prune buildac !threshold mn
                      else AcveImpl.varelim_choose buildac !threshold mn
      else
        if !onlyprune then AcveImpl.varelim_prune_m buildac !maxsize mn
                      else AcveImpl.varelim_choose_m buildac !maxsize mn in
      
    let before = Sys.time() in
    if buildac then begin
      (* Inputs: outfile/ostream schema acroot fl (size?) *)
      let ostream = open_out !outfile in
      Circuit.output_root ostream schema acroot;
      let fl = Add.circuit_features adds in
      output_string ostream "\n";
      Circuit.output_features ostream acroot fl;
      
      (*Circuit.output_root_with_features ostream schema acroot fl; *)
      let after = Sys.time() in
      nlogf "Creating circuit: %fs\n" (after -. before) 
    end else begin
      let v = 
        if Circuit.is_times acroot then begin
          dlogf "DEBUG 1\n";
          List.sumf_map Circuit.const_value (Circuit.children acroot);
        end else
          Circuit.const_value acroot in
      nlogf "%f\n" v
    end
  end

let _ = main ()
