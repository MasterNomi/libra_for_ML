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
let modelfile = ref ""
let weightfile = ref ""
let outfile = ref ""

let usage = "Usage: mnsetw -m <model> -w <weights> -o <output> [...]"
let args = Arg.align
 ([("-m", Arg.Set_string modelfile, " Input Markov network") ;
   ("-w", Arg.Set_string weightfile, " Weight file") ;
   ("-o", Arg.Set_string outfile, " Output model")]
   @ common_arguments)

(* Read array of weights from a file *)
let read_weights file =
  let channel = open_in file in
  let l = ref [] in
  try
    while true do
      let w = float_of_string (input_line channel) in
      l := w :: !l
    done; [||] 
  with End_of_file -> 
    Array.of_list (List.rev !l)

let do_set_weights () =
  (* Read in model and data *) 
  let mn = 
    match filetype !modelfile with
    | BNFile -> Bn.to_mn (Bn.load_auto !modelfile)
    | _ -> Mn.load_auto !modelfile in
  Mn.set_weights mn (read_weights !weightfile);
  Mn.write_auto !outfile mn;
  vlogf "Total time: %f seconds\n" (Sys.time ())

let main () = 
  Arg.parse args ignore usage;
  if !modelfile = "" || !weightfile = "" || !outfile = "" then
    Arg.usage args usage
  else begin
    common_log_init ();
    do_set_weights ()
  end

let _ = main ()
