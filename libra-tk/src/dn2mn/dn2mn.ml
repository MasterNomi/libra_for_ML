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

(* Command line arguments *)
let modelfile = ref ""
let datafile = ref ""
let outfile = ref ""

let use_base = ref false
let use_marg = ref false
let use_fcounts = ref false

let orderfile = ref ""
let symorder = ref true
let single_orders = ref false
let linear_orders = ref false
let all_orders = ref false
let maxlen = ref (-1)

let use_hash = ref false

let usage = "Usage: dn2mn -m <model> -o <output> [...]"
let args = Arg.align
 ([("-m", Arg.Set_string modelfile, " Model file (DN)") ;
   ("-o", Arg.Set_string outfile, " Output Markov network") ;

   ("-i", Arg.Set_string datafile, " Input data");
   ("-base", Arg.Set use_base, " Average over base instances from data");
   ("-bcounts", Arg.Set use_fcounts, 
       " Average over base instances from data, using cached counts");
   ("-marg", Arg.Set use_marg, " Average over all instances, weighted by data marginals [default]"); 
   ("-order", Arg.Set_string orderfile, " Variable order file");
   ("-rev", Arg.Set symorder, " Add reverse orders, for symmetry [default]");
   ("-norev", Arg.Clear symorder, " Do not add reverse orders");
   ("-single", Arg.Set single_orders, " Average over given orders");
   ("-linear", Arg.Set linear_orders, " Average over all rotations of all orders [default]");
   ("-all", Arg.Set all_orders, " Average over all possible orderings");
   ("-maxlen", Arg.Set_int maxlen, 
     " Max feature length for -linear or -all [default: unlimited]");
   ("-uniq", Arg.Set use_hash, " Combine weights for duplicate features (may be slower)")]
   @ common_arguments)

module F = Mn.Factor

let simple_cond_cmp = F.simple_cond_cmp 


(* Keep track of final weights using global variables (bad hack) *)
let weight_hash = Hashtbl.create 100

let add_weight (condl: (bool*int*int) list) (w: float) =
  let condl = List.sort simple_cond_cmp condl in
  if Hashtbl.mem weight_hash condl then begin
    let w' = w +. Hashtbl.find weight_hash condl in
    Hashtbl.replace weight_hash condl w'
  end else
    Hashtbl.add weight_hash condl w

let get_weight condl =
  let condl = List.sort simple_cond_cmp condl in
  Hashtbl.dfind weight_hash condl 0.

let get_features () =
  let fl = ref [] in
  let add_f condl w =
    let f = F.Feature 
      {F.cond=Array.of_list condl; F.weight=w; F.weight_id=(-1)} in
    fl := f :: !fl in
  Hashtbl.iter add_f weight_hash;
  !fl


let dn2mn_basic schema cpds order xbase = 
  (* To turn a consistent DN into an equivalent MN, we can change
     log P(X_i|X_-i) into the factor phi_i(X_1, ..., X_i) =
       log P(X_i|X_1..i-1, X_i+1..n=x_i+1..n) -
         log P(X_i=x'_i|X_1..i-1, X_i+1..n=x_i+1..n) 
     where x' is any fixed "base" instance.

     For an inconsistent DN, the results may depend on the ordering of 
     the factors and the base instance x'. *)
  let numvars = Array.length schema in
  let ev = Array.copy xbase in
  let newfactors = Array.make (2*numvars) [] in
  for i = 0 to numvars - 1 do
    (* Subtract log P(x'_i|x_i+1..n) *)
    let v = order.(i) in
    newfactors.(2*v+1) <- 
      (let fl = List.map (F.simplify ev) cpds.(v) in
       List.map (F.rescale (-1.)) fl);
    ev.(v) <- (-1);
    (* Add log P(x'_i|x_i+1..n) *)
    newfactors.(2*v) <- List.map (F.simplify ev) cpds.(v)
  done;
  if !use_hash then begin
    let add_feature_weight f =
      add_weight (Array.to_list f.F.cond) f.F.weight in
    let add_factor_weight factor =
      List.iter add_feature_weight (F.to_features factor) in
    Array.iter (List.iter add_factor_weight) newfactors;
    [||]
  end else
    Array.flattenl newfactors


(*
 * Use fixed order, but average over all base instances.
 *)
let single_subfeature order pcondl denom cvar f =
  let condl = Array.to_list f.F.cond in
  let (nonev, ev) = List.partition (fun (sense,var,value) -> 
      order.(var) < order.(cvar) + if denom then 0 else 1) condl in
  let p = pcondl ev in
  if p = 0.0 then []
  else
    let p' = if denom then (-.p) else p in
    let w = p' *. f.F.weight in
    if !use_hash then 
      (add_weight nonev w; [])
    else 
      [F.Feature {F.cond=Array.of_list nonev; F.weight=w; F.weight_id=(-1)}]

(*
 * More efficient -- only consider linear set of orderings.
 *)


(* Count number of distinct conditions, excluding the child var.
 * Assumes that conditions are sorted by variable number. *)
let num_other_conds cvar f =
  snd (Array.fold_right 
    (fun (sense,var,value) (last_var, total) -> 
      if var = cvar || var = last_var then (var, total)
      else (var, total+1)) f.F.cond (-1,0)) 

let num_conds f = num_other_conds (-1) f


let cond_cmp order (_,var,_) (_,var',_) =
  order.(var) - order.(var')

(* Generate all trailing suffixes, e.g.:
   [x1; x2; x3] -> [[x1; x2; x3]; [x2; x3]; [x3]] 
   Assign weight based on the difference in order between
   adjacent variables.  Save removed conditions (prefixes) 
   for later reference. *)
let rec condl_suffixes order lastorder accu = function
| c :: l -> 
  let (_,var,_) = c in
  let w = order.(var) - lastorder in
  (w, c::l, accu) :: condl_suffixes order order.(var) (c::accu) l
| [] -> []

(* When one variable has multiple conditions, the suffixes function
   above will assign a weight of zero to the later conditions on the
   same variable (since the difference in variable order is 0).  
   This will only condition on the first test of each variable, 
   rather than testing on all of them.
   
   This function keeps the last set of conditions that ends on
   each variable, which includes all of the conditions instead of
   just one of them. *)
let rec rem_duplicate_conds last_zero = function
  | (0, cl, rem_cl) :: l ->
    rem_duplicate_conds (Some (0, cl, rem_cl)) l
  | c :: l ->
    (match last_zero with 
    | None -> c :: rem_duplicate_conds None l
    | Some c' -> c' :: c :: rem_duplicate_conds None l)
  | [] -> []


(* Construct a reordering where child var comes last *)
let reorder order cvar =
  let numvars = Array.length order in
  let neworder = Array.make numvars (-1) in
  let offset = (numvars - 1) - order.(cvar) in
  for i = 0 to numvars - 1 do
    neworder.(i) <- (order.(i) + offset) mod numvars
  done;
  neworder

let linear_subfeatures order pcondl denom cvar f =
  let maxvar = Array.length order in 
  (* Generate new ordering where child var comes last.  Sort accordingly. *)
  let order = reorder order cvar in
  let condl = List.sort (cond_cmp order) (Array.to_list f.F.cond) in
  if List.length condl = 0 then [] else begin

  (* Generate condition lists, weighted by number of compatible orderings.
     In the denominator, the child var is always removed. *)
  let (condl, removed) = 
    if denom then List.partition (fun (_,var,_) -> var != cvar) condl
    else (condl, []) in
  let w_condls = condl_suffixes order (-1) removed condl in
  let w_condls = rem_duplicate_conds None w_condls in

  (* Reweight, taking into the probability of matching the 
     removed conditions *)
  let sign = if denom then (-.1.) else 1. in
  let fweight w rem_cl = 
    let w' = f.F.weight *. float_of_int w /. float_of_int maxvar in
    w' *. pcondl rem_cl *. sign in

  (* Build feature or add feature weight to global hash *)
  if !use_hash then begin
    let add_feature_weight (w, cl, rem_cl) =
      add_weight cl (fweight w rem_cl) in
    List.iter add_feature_weight w_condls; []
  end else
    let make_feature (w, cl, rem_cl) =
      let w' = fweight w rem_cl in
      F.Feature {F.cond=Array.of_list cl; F.weight=w'; F.weight_id=(-1)} in
    List.map make_feature w_condls
  end

(* Simplify one feature using linear orderings or a single order. *)
let adaptive_linear_subfeatures maxlen order pcondl denom cvar f =
  if num_conds f <= maxlen then
    linear_subfeatures order pcondl denom cvar f
  else
    single_subfeature order pcondl denom cvar f

let average_simplify_linear maxlen order pcondl cvar cpdfl =
  let gen_features = adaptive_linear_subfeatures maxlen order pcondl in
  let num = List.flatten (List.map (gen_features false cvar) cpdfl) in
  let denom = List.flatten (List.map (gen_features true cvar) cpdfl) in
  num @ denom 

let dn2mn_linear maxlen orders pcondl cpds =
  let numvars = Array.length cpds in
  let c = 1.0 /. (float_of_int (List.length orders)) in
  let fl = ref [] in
  for i = 0 to numvars - 1 do 
    let cpdfl = List.flatten (List.map F.to_features cpds.(i)) in
    let cpdfl = List.map (fun f -> {f with F.weight=f.F.weight *. c}) cpdfl in
    let simplify_one o = average_simplify_linear maxlen o pcondl i cpdfl in
    fl := (List.flatten (List.map simplify_one orders)) @ !fl 
  done;
  !fl


(*
 * Average over all exponentially many orderings.
 *)

(* Functions for incrementing a k-bit binary counter 
   Required for iterating over all states. *)
exception EndState

let rec incstate_rec s i =
  if i >= Array.length s then 
    raise EndState
  else if s.(i) then begin
    s.(i) <- false;
    incstate_rec s (i+1)
  end else
    s.(i) <- true

let incstate s = incstate_rec s 0

let rec select_cond denom cvar (last_var,last_s) (accu_in, accu_out) 
                                                      condl selectedl =
  match condl with
  | [] -> (accu_in, accu_out)
  | (sense,var,value)::cl ->
    (* Get first selection flag from list.  If list is empty, then 
       the value should be irrelevant, so we use a default. *)
    let (s,sl) = match selectedl with 
    | s::sl -> (s,sl) 
    | [] -> assert (var = cvar || var = last_var); (false,[]) in

    (* Handle special cases of child var or repeated vars. *)
    let (s,sl) = 
      (* Always include the child (target) variable in the numerator,
         and never in the denominator (always condition on it) *)
      if var = cvar then ((not denom), s::sl)
      (* If var is repeated, use last selection *)
      else if var = last_var then (last_s, s::sl)
      (* Default: use selection flag and remove it from the list *)
      else (s, sl) in

    (* Include condition if selected; otherwise, omit. *)
    let accu' = 
      if s then ((sense,var,value)::accu_in, accu_out)
      else (accu_in, (sense,var,value)::accu_out) in
    select_cond denom cvar (var,s) accu' cl sl

(* Basic combinatorics *)
let rec factorial = function
  | 0 -> 1
  | k -> k * (factorial (k-1))

let rec reweighting k l =
  (* 1. Pick an ordering of target var and k others: (k+1)!
   * 2. Divide by equivalent orderings of l chosen vars and 
   *    k-l non-chosen vars: l! * (k-l)! *)
  let orders = factorial (k+1) / (factorial l * factorial (k-l)) in
  1. /. float_of_int orders

let subfeature pcondl denom cvar selected f =
  let condl = Array.to_list f.F.cond in
  let selectedl = Array.to_list selected in
  let (condl', excondl') = 
    select_cond denom cvar (-1,false) ([],[]) condl selectedl in
  let p = pcondl excondl' in
  let p' = if denom then (-.p) else p in
  (* k = # of conditions that were considered.  Each subfeature
   * represents a subset of these conditions with probability 2^-k. *)
  let k = Array.length selected in
  (* l = # of conditions that were selected for inclusion.
   * Hence, the remaining k-l were conditioned away using the base
   * instance, of which the fraction 2^-(k-l) matched. *)
  let l = Array.sum_map (function true -> 1 | false -> 0) selected in
  (* Compute final weight *)
  (* dlogf "DB reweighting %d %d = %f\n" k l (reweighting k l);
  dlogf "DB p = %f\n" p; *)
  let weight' = f.F.weight *. p' *. (reweighting k l) in
  (condl', weight')

let average_simplify_f pcondl denom cvar f =
  let fl = ref [] in
  (* Iterate through all 2^k combinations of each other var
     coming before or after the child var. *)
  (* dlogf "DB cvar=%d\n" cvar;
  dlogf "DB num_other_conds=%d\n" (num_other_conds cvar f); *)
  let s = Array.make (num_other_conds cvar f) false in
  try while true do
    (* For each combination, add the simplified feature with
       the appropriate weight *)
    let (condl, w) = subfeature pcondl denom cvar s f in
    if !use_hash then add_weight condl w
    else
      let f' = F.Feature 
        {F.cond=Array.of_list condl; F.weight=w; F.weight_id=(-1)} in
      fl := f' :: !fl;
    incstate s
  done; [] with EndState -> !fl

let average_simplify_all maxlen orders pcondl cvar cpdfl =
  (* Exclude long features from the exponential expansion *)
  let (shortf, longf) = 
    List.partition (fun f -> num_conds f <= maxlen) cpdfl in
  let num = 
    List.flatten (List.map (average_simplify_f pcondl false cvar) shortf) in
  let denom = 
    List.flatten (List.map (average_simplify_f pcondl true cvar) shortf) in
  (* If there are long features, then perform linear conversion on them. *)
  if longf <> [] then begin
    let c = 1. /. (float_of_int (List.length orders)) in
    let longf = List.map (fun f -> {f with F.weight=f.F.weight *. c}) longf in
    let longlen = 1 + Array.length (List.hd orders) in
    let a_s_l o = average_simplify_linear longlen o pcondl cvar longf in 
    let linearfl = List.flatten (List.map a_s_l orders) in
    num @ denom @ linearfl
  end else
    num @ denom 

let dn2mn_all maxlen orders pcondl cpds = 
  let numvars = Array.length cpds in
  let fl = ref [] in
  for i = 0 to numvars - 1 do 
    let cpdfl = List.flatten (List.map F.to_features cpds.(i)) in
    fl := (average_simplify_all maxlen orders pcondl i cpdfl) @ !fl
  done;
  !fl


(*
 * Functions for computing instance probilities from data.
 *)

let read_data datafile =
  if datafile = "" then ([||], 0.0)
  else begin
    let fin = open_in datafile in
    let data = Array.of_list (Data.input_wexample_list fin) in
    let num_data = Array.sumf_map fst data in
    close_in fin;
    (data, num_data)
  end

(* Compute marginal distribution from data *) 
let build_marg data num_data schema =
  (* Get marginal distribution over each variable to start with *)
  let uniform dim = Array.make dim (1.0 /. float_of_int dim) in
  let marg = Array.map uniform schema in
  (* Update with data, if specified.  Data should already be loaded. *)
  if num_data > 0.0 then begin
    for i = 0 to Array.length data - 1 do
      let (w,x) = data.(i) in
      for i = 0 to Array.length x - 1 do
        marg.(i).(x.(i)) <- marg.(i).(x.(i)) +. w
      done
    done;
    Array.iter normalize_inplace_raw marg
  end;
  marg

(* Old version that only works with binary vars:
(* Compute feature probability using univariate marginals *)
let marg_feature_prob marg cl =
  let cprob (sense,var,value) = 
    if sense then marg.(var).(value)
    else 1.0 -. marg.(var).(value) in
  List.fold_left ( *. ) 1.0 (List.map cprob cl)
  *)

(* Properly handle multi-valued vars with multiple conditions, as long
   as the conditions are grouped together in the list. *)
let rec marg_feature_prob_rec marg (lastvar, varmarg) = function
  | [] -> Array.sumf varmarg
  | (sense,var,value) :: l ->
    (* Create new varcond array, if necessary.
       Save the result of the old one as p0. *)
    let (p0, varmarg) = 
      if var = lastvar then (1., varmarg) 
      else (Array.sumf varmarg, Array.copy marg.(var)) in
    (* Apply this condition to the array. *)
    for i = 0 to Array.length varmarg - 1 do
      if (i = value) != sense then
        varmarg.(i) <- 0.
    done;
    p0 *. (marg_feature_prob_rec marg (var, varmarg) l)

let marg_feature_prob marg cl =
  let cl = List.sort simple_cond_cmp cl in
  marg_feature_prob_rec marg (-1, [|1.|]) cl


(* Compute feature probability using data and cached counts *)
let feature_prob data num_data fhash cl =
  let cl = List.sort simple_cond_cmp cl in 
  if not (Hashtbl.mem fhash cl) then begin
    let cond = Array.of_list cl in
    let num = ref 0.0 in
    for i = 0 to Array.length data - 1 do
      let (w,x) = data.(i) in
      if Mn.Factor.fmatch x cond then
        num := !num +. w
    done;
    Hashtbl.add fhash cl (!num /. num_data)
  end;
  Hashtbl.find fhash cl


(* Add in the reverse of orders *) 
let rec revorders = function
  | o :: l -> o :: (Array.rev o) :: (revorders l)
  | [] -> []

(* Build set of orderings to use *)
let build_orders numvars =
  let ol = if !orderfile = "" then [Array.init numvars (fun i -> i)]
           else Data.input_example_list (open_in !orderfile) in
  let ol = if !symorder then revorders ol else ol in
  ol

(* Convert to lookup vectors instead of the original permutation *)
let build_order_lookups o =
  let o' = Array.make (Array.length o) (-1) in
  for i = 0 to Array.length o - 1 do
      assert(o'.(o.(i)) = -1);
      o'.(o.(i)) <- i
  done;
  o'

let main () = 
  Arg.parse args ignore usage;
  if !modelfile = "" || !outfile = "" then
    (Arg.usage args usage; exit 0);
  common_log_init();

  (* Validate arguments for averaging over base instances and orderings *)
  if (!use_base && !use_marg) || (!use_base && !use_fcounts) ||
       (!use_marg && !use_fcounts) then
    (nlogf "ERROR: -base, -marg, and -fcounts are mutually exclusive."; exit (-1));
  if not !use_base && not !use_marg && not !use_fcounts then
    use_marg := true;

  if (!single_orders && !linear_orders) || (!single_orders && !all_orders) ||
       (!linear_orders && !all_orders) then
    (nlogf "ERROR: -single, -linear, and -all are mutually exclusive."; exit (-1));
  if not !single_orders && not !linear_orders && not !all_orders then
    linear_orders := true;

  (* For linear or all orders, use -fcounts in place of -base.  (They're
     theoretically equivalent, but -base is faster for a fixed order.) *)
  if (!all_orders || !linear_orders) && !use_base then begin
    use_base := false;
    use_fcounts := true
  end;

  (* Load model *)
  Timer.start "dn2mn";
  let (schema, cpds) = match filetype !modelfile with
    | BNFile -> 
      let bn = Bn.load_auto !modelfile in
      (Bn.schema bn, Array.mapi (Bn.cpd_to_factors bn) bn.Bn.dists)
    | MNFile -> 
      let mn = Mn.load_auto !modelfile in
      (Mn.schema mn, Array.map (fun f -> [f]) mn.Mn.factors)
    | _ -> nlogf "ERROR: Unsupported file type.\n"; exit (-1) in
  vlogf "Loading model: %f s\n" (Timer.delta "dn2mn");

  if !single_orders then
    (* Linear order with a maxlen of zero means always reverting 
       to the basic order. *)
    maxlen := 0
  else if !maxlen < 0 then 
    (* A maxlen of n+1 means never resorting to the basic order. *)
    maxlen := Array.sum schema + 1;

  let orders = build_orders (Array.length schema) in
  let (data, num_data) = read_data !datafile in
  let pcondl =
    if !use_marg then
      marg_feature_prob (build_marg data num_data schema)
    else
      feature_prob data num_data (Hashtbl.create 100) in
  vlogf "Reading examples and orders: %f s\n" (Timer.elapsed "dn2mn");

  let features = 
    if !all_orders then 
      let orders = List.map build_order_lookups orders in
      Array.of_list (dn2mn_all !maxlen orders pcondl cpds)
    else if not !use_base then 
      let orders = List.map build_order_lookups orders in
      Array.of_list (dn2mn_linear !maxlen orders pcondl cpds) 
    else begin
      (* Translate the DN multiple times with different bases and orders.
       * Uses all combinations of bases and orders. *)
      let xbases = 
        if num_data = 0.0 then [Array.init (Array.length schema) (fun i -> i)]
        else Array.to_list (Array.map snd data) in
      assert(List.length xbases > 0);
      let factors = List.flatten
        (List.map (fun o -> List.map (dn2mn_basic schema cpds o) xbases) orders) in
      let factors = 
        Array.of_list (List.flatten (List.map Array.to_list factors)) in
      (* Average over all translations *)
      let combos = float_of_int (List.length orders * List.length xbases) in
      Array.map (F.rescale (1./.combos)) factors
    end in
  (* If hashing weights, then features are stored in hash table
     instead and we need to retrieve them from there. *) 
  let features = 
    if !use_hash then 
      Array.of_list (get_features ())
    else features in
  let mn = Mn.create schema features in
  vlogf "Building MN: %f s\n" (Timer.delta "dn2mn");
  Mn.write_auto !outfile mn;
  vlogf "Writing MN: %f s\n" (Timer.delta "dn2mn");
  vlogf "Total time: %f s\n" (Timer.elapsed "dn2mn")

let _ = main ()
