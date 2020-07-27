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
let file = ref ""

let usage = "Usage: fstats -i <file>"
let args = Arg.align
 ([("-i", Arg.Set_string file, " Input file (BN, MN, DN, AC, or data)")]
   @ common_arguments)

let main () = 
  Arg.parse args ignore usage;
  if !file = "" then
    (Arg.usage args usage; exit 0);

  common_log_init();

  nlogf "Filename: %s\n" !file;
  match filetype !file with
  | ACFile ->
    nlogf "Filetype: AC\n";
    let (circ, fl) = Circuit.load_with_features (open_in !file) in
    nlogf "Variables: %d\n" (Circuit.numvars circ);
    nlogf "Schema: %s\n" (Data.to_string_schema (Circuit.schema circ));
    nlogf "Nodes: %d\n" circ.Circuit.size;
    let ntimes = ref 0 in
    let nplus = ref 0 in
    let nvar = ref 0 in
    let nconst = ref 0 in
    let inc_one n = match n.Circuit.details with
      | Circuit.TimesNode    -> incr ntimes
      | Circuit.PlusNode     -> incr nplus
      | Circuit.VarNode(_,_) -> incr nvar
      | Circuit.ConstNode _  -> incr nconst 
      | Circuit.NullNode     -> () in
    Array.iter inc_one circ.Circuit.nodes;
    nlogf "  %d times\n" !ntimes;
    nlogf "  %d plus\n" !nplus;
    nlogf "  %d var\n" !nvar;
    nlogf "  %d const\n" !nconst;
    nlogf "Edges: %d\n" (Circuit.num_edges circ);
    nlogf "Features: %d\n" (List.length fl);
    let scratch = Circuit.create_scratch circ in
    nlogf "Z = %f\n" (Circuit.compute_z scratch circ) 

  | BNFile ->
    nlogf "Filetype: BN (%s)\n"
      (if Bn.filename_is_xmod !file then "XMOD" 
       else if Bn.filename_is_bif !file then "BIF"
       else if Bn.filename_is_dn !file then "DN"
       else "BN");
    let bn = Bn.load_auto !file in
    let numvars = Bn.numvars bn in
    nlogf "Variables: %d\n" numvars;
    nlogf "Schema: %s\n" (Data.to_string_schema (Bn.schema bn));
    let total_parents = 
      Array.sum (Array.init numvars (Bn.numparents bn)) in
    let total_params =
      Array.sum (Array.init numvars (Bn.numparams bn)) in
    nlogf "Parameters: %d\n" total_params;
    nlogf "Average parents: %f\n" 
      ((float_of_int total_parents) /. (float_of_int numvars));
  | MNFile ->
    nlogf "Filetype: MN\n";
    let mn = Mn.load_auto !file in
    let numvars = Mn.numvars mn in 
    nlogf "Variables: %d\n" numvars;
    nlogf "Schema: %s\n" (Data.to_string_schema (Mn.schema mn));
    let factors = Mn.factors mn in
    nlogf "Factors: %d\n" (Array.length factors);
    nlogf "Parameters: %d\n" 
      (Array.sum_map Mn.Factor.numparams factors);
    let total_arity = 
      Array.sum_map (fun f -> List.length (Mn.Factor.vars f)) factors in
    nlogf "Average arity: %f\n" 
      ((float_of_int total_arity) /.  (float_of_int numvars))
  | SPNFile ->
    nlogf "Filetype: SPN\n";
    let modelfile = (String.concat Filename.dir_sep [!file; "spac.m"]) in  
    let model = open_in modelfile in
    let network = input_line model in
    let num_nodes = int_of_string (input_line model) in
    nlogf "Network: %s\n" network;
    nlogf "Number of nodes: %d\n" num_nodes 
  | _ -> 
    try
      let exl = Data.input_wexample_list (open_in !file) in
      let exl = List.map snd exl in
      nlogf "Filetype: Data\n";
      if exl = [] then 
        nlogf "Empty.\n"
      else begin
        let n = List.length exl in
        let num_vars = Array.length (List.hd exl) in
        let num_missing = List.sum_map (Array.count (( > ) 0)) exl in
        nlogf "Points: %d\n" n;
        if List.for_all (fun x -> Array.length x = num_vars) exl then begin
          nlogf "Variables: %d\n" num_vars;
          nlogf "Schema: %s\n" (Data.to_string_schema (Data.schema exl))
        end else
          nlogf "Variables: inconsistent!\n";
        let num_entries = List.sum_map Array.length exl in
        nlogf "Fraction missing: %f\n" ((float_of_int num_missing) /.
          (float_of_int num_entries))
      end
    with Data.Parse_error _ ->
      nlogf "Filetype: Unknown\n"

;;
main ()
