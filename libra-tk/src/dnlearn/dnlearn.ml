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

let big_int = 1000000000 (* = 1 billion *)

(* Globals used for command line parameters *)

let datafile = ref ""
let schemafile = ref ""
let validfile = ref ""
let dnoutfile = ref ""
let mincount = ref 10
let l1prior = ref 0.1
let logistic = ref false

(* The weight the score function assigns to each additional split created,
   i.e., the penalty for each additional parameter. *)
let split_cost = ref 0.0
let kappa = ref 0.0
let cost_inc =ref 0.5
let kappa_inc = ref 0.0

(* Prior counts *)
let prior = ref 1.0


let usage = "Usage: dnlearn -i <input> -o <output> [...]"
let args = Arg.align
 ([("-i", Arg.Set_string datafile, " Training data file") ;
   ("-s", Arg.Set_string schemafile, " Data schema (optional)") ;
   ("-o", Arg.Set_string dnoutfile, " Output dependency network") ;
   ("-ps", Arg.Set_float split_cost, " Per-split penalty [0.0]") ;
   ("-kappa", Arg.Set_float kappa, " Alternate per-split penalty specification [0.0]") ;
(* Tune each conditional distribution separately -- not recommended.
   ("-valid", Arg.Set_string validfile, " Validation data file, for early stopping") ;
   ("-psi", Arg.Set_float cost_inc, " Additive increment in split penalty") ;
   ("-kappai", Arg.Set_float kappa_inc, " Mutiplicative increment in kappa") ;
   *)
   ("-prior", Arg.Set_float prior, " Prior counts of uniform distribution [1.0]");
   ("-tree", Arg.Clear logistic, " Use decision tree CPDs [default]");
   ("-mincount", Arg.Set_int mincount, 
     " Minimum number of examples at each decision tree leaf [10]");
   ("-logistic", Arg.Set logistic, " Use logistic regression CPDs [false]");
   ("-l1", Arg.Set_float l1prior, " Weight of L1 norm for logistic regression [0.1]")]
   @ common_arguments)


let counts_to_probs prior counts =
  let total = prior +. float_of_int (Array.sum counts) in
  let prior_count = prior /. (float_of_int (Array.length counts)) in
  Array.map (fun x -> ((float_of_int x +. prior_count) /. total)) counts
  
(* Squared loss of var on data, according to distribution dist *)
let dist_sql dist counts = 
  let sql p q = (float_of_int p) *. (-. q *. q) in 
  Array.sumf (Array.map2 sql counts dist) 

(* Log loss of var on data, according to distribution dist *)
let dist_ll dist counts = 
  let ll p q = if p > 0 then (float_of_int p) *. (log q) else 0. in
  Array.sumf (Array.map2 ll counts dist) 


(* Split score is change in log likelihood *)
let split_score pro_c con_c =
  let total_c = Array.map2 ( + ) pro_c con_c in
  (* Must have at least mincount examples at each leaf. *)
  if Array.sum pro_c < !mincount || Array.sum con_c < !mincount then 0.0
  else
    let total_p = counts_to_probs !prior total_c in
    let pro_p = counts_to_probs !prior pro_c in
    let con_p = counts_to_probs !prior con_c in
    (dist_ll pro_p pro_c +. dist_ll con_p con_c) -. dist_ll total_p total_c
 

let split_counts_l var numvals data va =
  let v_var = Array.map fst va in
  let v_val = Array.map snd va in
  (* TODO: Reuse matrices instead of re-allocating them each time? *)
  let pro_a = Array.make_matrix numvals (Array.length va) 0 in
  let con_a = Array.make_matrix numvals (Array.length va) 0 in
  for i = 0 to Array.length data - 1 do
    let x = data.(i) in
    let xval = x.(var) in 
    for j = 0 to Array.length va - 1 do
      if x.(v_var.(j)) = v_val.(j) then
        pro_a.(xval).(j) <- pro_a.(xval).(j) + 1
      else
        con_a.(xval).(j) <- con_a.(xval).(j) + 1
    done
  done;
  let pro_a' = Array.transpose pro_a in
  let con_a' = Array.transpose con_a in
  (pro_a', con_a')


let best_split schema data var va =
  let (pro, con) = split_counts_l var schema.(var) data va in
  let scores = Array.map2 split_score pro con in
  if log_exists log_debug then begin
    let print_score i score =
      let (var, value) = va.(i) in 
      dlogf "Split %d (v%d=%d): %f\n" i var value score in
    Array.iteri print_score scores
  end;
  let i = max 0 (Array.argmax scores) in
  let (var, value) = va.(i) in
  (var, value, scores.(i), pro.(i), con.(i))


let rec learn_dt_rec schema data v split_cost parentscore =
  (* Get the best split for the current node *)
  let gen_splits var dim =
    if var = v then []
    else if dim = 2 then [(var, 0)]
    else Array.to_list (Array.init dim (fun i -> (var, i))) in
  let vl = List.flatten (Array.to_list (Array.mapi gen_splits schema)) in
  let va = Array.of_list vl in
  let (var, value, score, pro_c, con_c) = best_split schema data v va in

  (* If the best split isn't good enough, return a leaf.
     Otherwise, recurse on the two subtrees. *)
  if score <= split_cost || classify_float score = FP_nan then
    let total_c = Array.map2 ( + ) pro_c con_c in
    Bn.Leaf (Array.map log (counts_to_probs !prior total_c))
  else begin
    vlogf "Var %d: Splitting on v%d=%d (score=%f).\n" v var value score;
    if score > parentscore then
      dlogf
"Score of %f is better than parent score of %f. (Not submodular.)\n" 
        score parentscore;
    let (pro_data, con_data) = 
      Array.partition (fun x -> x.(var) = value) data in
    dlogf "Pro: %d; Con: %d\n" (Array.length pro_data) (Array.length
      con_data) ;
    let left = learn_dt_rec schema pro_data v split_cost score in
    let right = learn_dt_rec schema con_data v split_cost score in
    Bn.Vertex(var, value, left, right)
  end

let dt_score dt vdata v =
  List.sumf_map (fun (w,x) -> w *. (Bn.tree_logprob x dt).(x.(v))) vdata

let rec learn_dt_valid schema data vdata v (best_score, best_dt, i) split_cost = 
  let dt = learn_dt_rec schema data v split_cost infinity in
  let score = dt_score dt vdata v in
  vlogf "VALIDATION SCORE: %f\n" score;
  (* Compare to previous best.  i counts number of iterations since our 
   * model improved. *)
  let (best_dt, best_score, i) = 
    if score > best_score then (dt, score, 0) 
    else (best_dt, best_score, i+1) in 
  if i > 2 || split_cost < !cost_inc then
    best_dt
  else begin
    vlogf "Learning with split cost = %f\n" (split_cost -. !cost_inc);
    learn_dt_valid schema data vdata v (best_score, best_dt, i) 
      (split_cost -. !cost_inc)
  end

let learn_dt schema data vdata v =
  let dt = learn_dt_rec schema data v !split_cost infinity in
  if vdata = [] then 
    dt
  else
    let score = dt_score dt vdata v in
    let split_cost = !split_cost -. !cost_inc in
    learn_dt_valid schema data vdata v (score, dt, 0) split_cost
  

(*
 * L1-regularized logistic regression CPDs 
 *)


(* TODO -- add support for multi-valued vars, eventually... *)
let lr_prob v w x =
  (* Use v'th weight to represent baseline probability w_0. *)
  let subtotal = ref w.(v) in
  for i = 0 to Array.length x - 1 do
    if x.(i) = 1 && i <> v then
      subtotal := !subtotal +. w.(i);
  done;
  if x.(v) = 1 then
    1.0 /. (1.0 +. exp(-.(!subtotal)))
  else
    1.0 /. (1.0 +. exp(!subtotal))
  

(* TODO -- add support for multi-valued vars, eventually... *)
let lr_val_and_grad schema data v w g =
  (* Initialize value and gradient to zero *)
  let total = ref 0.0 in
  for i = 0 to Array.length schema - 1 do 
    g.(i) <- 0.0
  done;
  for i = 0 to Array.length data - 1 do
    let x = data.(i) in
    let p = lr_prob v w x in
    total := !total +. log p;
    for j = 0 to Array.length schema - 1 do
      (* The 'v = j' test ensures that the v'th feature acts as one
       * that is always true, for representing a baseline prob. *)
      if x.(j) = 1 || v = j then begin
        if x.(v) = 1 then
          g.(j) <- g.(j) +. (1. -. p)
        else
          g.(j) <- g.(j) -. (1. -. p)
      end
    done
  done;
  for i = 0 to Array.length g - 1 do
    g.(i) <- -. g.(i)
  done;
  vlogf "var=%d  ll=%f\n" v !total;
  -.(!total)


module F = Mn.Factor

let learn_lr schema data v =
  let w = Array.make (Array.length schema) 0.0 in
  let (errcode, _) = Lbfgs.minimize_l1 !l1prior 
                     (lr_val_and_grad schema data v) w 0.001 1000 in
  vlogf "LBFGS errcode = %s\n" (Lbfgs.errstring errcode);
  let fl = ref [] in
  for i = 0 to Array.length w - 1 do
    if w.(i) <> 0. then begin
      let f = 
        if i = v then 
          F.Feature {F.cond=[|(true,v,1)|]; 
                     F.weight_id=(-1); F.weight=w.(i)}
        else
          F.Feature {F.cond=[|(true,v,1);(true,i,1)|]; 
                     F.weight_id=(-1); F.weight=w.(i)} in
      fl := f :: !fl 
    end
  done;
  !fl


let do_learn () =
  (* Read in data and determine schema (number of values for each var) *)
  (* TODO: Eventually support weighted examples here. *)
  let data = Data.input_example_list (open_in !datafile) in
  let vdata = 
    if !validfile = "" then [] 
    else Data.input_wexample_list (open_in !validfile) in
  vlogf "Loaded data.\n";
  let schema = 
    if !schemafile <> "" then begin
      let schemain = open_in !schemafile in
      let s = Data.input_example schemain in
      close_in schemain ; s
    end
    else Data.schema (data @ (List.map snd vdata)) in 
  let data = Array.of_list data in

  (* For each variable, build a decision tree and set it as the CPD *)
  Timer.start "dnlearn";
  let numvars = Array.length schema in
  let bn = Bn.create_empty_network schema in
  bn.Bn.acyclic <- false;
  for i = 0 to numvars - 1 do
    if !logistic then
      Bn.set_factorset bn i (learn_lr schema data i)
    else
      Bn.set_cptree bn i (learn_dt schema data vdata i)
  done;
  vlogf "Learning time: %f seconds\n" (Timer.elapsed "dnlearn");

  (* Save to disk *)
  Bn.write_auto !dnoutfile bn


let main () = 
  Arg.parse args ignore usage;
  if !datafile = "" || !dnoutfile = "" then
    Arg.usage args usage
  else begin
    if !kappa > 0.0 then
      split_cost := -.(log !kappa);
    if !kappa_inc > 0.0 then
      cost_inc := (log !kappa_inc);
    common_log_init ();
    if Bn.filename_is_xmod !dnoutfile && !logistic then begin
      nlogf "ERROR: XMOD files do not support logistic regression CPDs.\n";
      exit (-1)
    end;
    do_learn ()
  end

let _ = main ()
