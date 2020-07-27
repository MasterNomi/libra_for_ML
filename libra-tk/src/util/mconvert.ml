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
let evfile = ref ""
let tofeatures = ref false
let todn = ref false

let usage = "Usage: mconvert -m <model> -o <output>"
let args = Arg.align
 ([("-m", Arg.Set_string modelfile, 
     " Input model file (BN, MN, or AC)");
   ("-o", Arg.Set_string outfile, " Output model file");
   ("-ev", Arg.Set_string evfile, " Evidence file");
   ("-feat", Arg.Set tofeatures, " Convert MN factors to features");
   ("-dn", Arg.Set todn, 
     " Convert factors to conditional distributions, producing a DN")]
   @ common_arguments)


let make_dn mn =
  let featurelists = Array.make (Array.length mn.Mn.schema) [] in
  let add_to_list f i =
    featurelists.(i) <- f :: featurelists.(i) in
  let add_feature f =
    List.iter (add_to_list f) (Mn.Factor.feature_set_vars [f]) in
  let features = Mn.to_features mn in
  List.iter add_feature features;
  let factors = Array.map (fun f -> Mn.Factor.FeatureSet f) featurelists in
  Mn.create mn.Mn.schema factors
  
let type_to_str = function
  | ACFile -> "circuit"
  | BNFile -> "Bayesian network"
  | MNFile -> "Markov network"
  | SPNFile -> "spn"
  | UnknownFile -> "unknown"
  | DataFile -> "data"

let main () = 
  Arg.parse args ignore usage;
  if !modelfile = "" || !outfile = "" then
    (Arg.usage args usage; exit 0);

  common_log_init();

  let has_ev = !evfile <> "" in
  let ev = if has_ev then Data.input_example (open_in !evfile)
           else [||] in

  (* Convert to a DN if target filename ends with foo.dn *)
  if Bn.filename_is_dn !outfile && not (Bn.filename_is_dn !modelfile) then
    todn := true;

  if has_ev then 
    dlogf "Evidence: %s\n" (Data.to_string_example ev);

  let proc_mn mn =
    let mn = if has_ev then Mn.simplify mn ev else mn in
    let mn = 
      if !todn then make_dn mn 
      else if not !tofeatures then mn 
      else
        let factor_to_fset f = 
          Mn.Factor.FeatureSet (Mn.Factor.to_features f) in
        let factors = Array.map factor_to_fset (Mn.factors mn) in
        Mn.create mn.Mn.schema factors in
    (* Use DN file format, which requires using the BN module. *)
    if Bn.filename_is_cn !outfile then begin
      let bn = Bn.create_empty_network mn.Mn.schema in
      bn.Bn.acyclic <- false;
      let factors = Mn.factors mn in
      for i = 0 to Array.length factors - 1 do
        Bn.set_factorset bn i [factors.(i)]
      done;
      Bn.write_auto !outfile bn
    end else
      Mn.write_auto !outfile mn in

  if !todn then
    if filetype !outfile = MNFile || Bn.filename_is_cn !outfile then 
      tofeatures := true
    else
      nlogf "Ignoring -dn option, which requires a .mn/.uai/.dn/.bn/.cn output file.";

  match (filetype !modelfile, filetype !outfile) with
  | (BNFile, BNFile) ->
    let bn = Bn.load_auto !modelfile in
    (* TODO: Convert trees to tables if necessary... *)
    let bn = if has_ev then Bn.simplify bn ev else bn in
    if !todn || !tofeatures then
      (* First convert to an MN so that the CPDs are all factors.
       * Then proc_mn will eventually convert it back to a DN
       * and write it to disk. *)
      proc_mn (Bn.to_mn bn)
    else if Bn.filename_is_cn !modelfile && Bn.filename_is_xmod !outfile then
      nlogf "ERROR: Converting from BN to XMOD not supported.\n"
    else if Bn.filename_is_cn !modelfile && Bn.filename_is_bif !outfile then
      nlogf "ERROR: Converting from BN to BIF not supported.\n" 
    else
      Bn.write_auto !outfile bn
  | (MNFile, BNFile) ->
    if not !todn then
      nlogf "ERROR: Converting from %s to %s not supported.\n"
          (type_to_str MNFile) (type_to_str BNFile)
    else
      proc_mn (Mn.load_auto !modelfile)
  | (BNFile, MNFile) ->
    let bn = Bn.load_auto !modelfile in
    proc_mn (Bn.to_mn bn)
  | (MNFile, MNFile) ->
    proc_mn (Mn.load_auto !modelfile)
  | (ACFile, MNFile) -> 
    let (ac, fl) = Circuit.load_with_features (open_in !modelfile) in
    let f_to_factor f =
      Mn.Factor.Feature
        {Mn.Factor.cond = Array.of_list f.Circuit.cond;
         Mn.Factor.weight_id = (Circuit.feature_node f).Circuit.id;
         Mn.Factor.weight = log f.Circuit.weight} in
    let factors = Array.of_list (List.map f_to_factor fl) in
    let mn = Mn.create (ac.Circuit.schema) factors in
    proc_mn mn
  | (ACFile, ACFile) -> 
    let (ac, fl) = Circuit.load_with_features (open_in !modelfile) in
    let fl = 
      if has_ev then 
        Circuit.prune_for_evidence_with_features ac fl ev
      else fl in
    Circuit.output_with_features (open_out !outfile) ac fl
  | (t1, t2) -> nlogf "ERROR: Converting from %s to %s not supported.\n"
          (type_to_str t1) (type_to_str t2)

;;
main ()
