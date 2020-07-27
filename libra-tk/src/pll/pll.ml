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

(*
 * PLL OPTIMIZATION CODE
 *
 * Example usage: Read in model and data, optimize PLL, and write out
 * optimized MN file.
 *
 * let opt modelfile datafile outfile = 
 *   let mn = Mn.load_auto modelfile in
 *   let data = Array.of_list (Data.input_example_list (open_in datafile)) in
 *   let w = Array.make (Mn.numweights mn) 0.0 in
 *   let cache = Mn.build_pll_cache mn data in
 *   let (errcode, pll) = Lbfgs.minimize (pll_val_and_grad cache) w in
 *   let mn' = pll_cache_to_mn cache w in
 *   write_auto outfile mn'
 *)
open Ext
open Printf

(* Keep track of how many violations of a conjunctive feature are present --
   zero, one, or more than one. *)
type satisfaction = Sat | Unsat of (int * bool array) | NeverSat 

type wexample_t = Data.wexample_t
type example_t = Data.example_t
type schema_t = Data.schema_t 

type valgrad_callback = float array -> float array -> float 

type pll_cache_t = schema_t * Mn.Factor.feature array *
  wexample_t array * int array array array array

(* Go through conditions, one by one.  If all match, return Sat.
 * If one variable doesn't match, return UnSat.  If two or more 
 * don't match, return NeverSat (and terminate early).
 *)
let rec compute_sat_rec x sat cond i = 
  if i >= Array.length cond then 
    sat
  else
    let (sense, var, value) = cond.(i) in 
    if (x.(var) = value) <> sense then 
      let sat = match sat with
      | Sat -> Unsat(var, [||])
      | Unsat(v, a) when v = var -> Unsat(var, [||])
      | _ -> NeverSat in
      if sat = NeverSat then NeverSat
      else compute_sat_rec x sat cond (i+1)
    else
      compute_sat_rec x sat cond (i+1)

(* Go through conditions, one by one.  If all match, return Sat.
   If one variable doesn't match, return Unsat.  If two or more 
   don't match, return NeverSat (and terminate early). *)
let compute_sat schema x f =
  (* Do a first pass through the conditions. *)
  let result = compute_sat_rec x Sat f.Mn.Factor.cond 0 in
  (* When only one variable doesn't match, get list of allowed values. *)
  match result with
  | Unsat(v, a) ->
    let values = Array.make schema.(v) true in
    for i = 0 to Array.length f.Mn.Factor.cond - 1 do
      let (sense, var, value) = f.Mn.Factor.cond.(i) in
      if var = v then
        for j = 0 to schema.(v) - 1 do
          if sense <> (j = value) then
            values.(j) <- false
        done
    done;
    Unsat(v, values)
  | Sat -> Sat
  | NeverSat -> NeverSat

(* Alternate implementation -- much slower.
let compute_sat2 schema x f = 
  let condl = Array.to_list f.Mn.Factor.cond in
  let valsetl = Mn.Factor.condl_to_valsetl schema condl in
  let do_one sat (var, values) =
    if sat = NeverSat then 
      NeverSat
    else if values.(x.(var)) then
      sat
    else if sat = Sat then
      Unsat(var, values)
    else
      NeverSat in
  List.fold_left do_one Sat valsetl
 *)

(* Convert array of allowed values into a list *)
let sat_array_to_list (var, values) =
  let l = ref [] in
  for i = 0 to Array.length values - 1 do
    if values.(i) then
      l := (var,i) :: !l
  done;
  !l

let cond_sat_set schema cond =
  let condl = Array.to_list cond in
  let valsetl = Mn.Factor.condl_to_valsetl schema condl in
  List.flatten (List.map sat_array_to_list valsetl)

(* Generate set of var/value assignments that satisfy this feature,
   assuming all other variables are held constant.  Excludes variables
   that do not appear in the feature. *)
let generate_sat_states schema x f =  
  match compute_sat schema x f with
  | Sat -> 
    (* dlogf "Sat\n"; *)
    (* List.flatten (List.map (sat_set schema) (Array.to_list f.Mn.Factor.cond)) *)
    cond_sat_set schema f.Mn.Factor.cond
  | Unsat (var, values) -> 
    (* dlogf "Unsat: %d, %s\n" var, string_of_ilist values); *)
    sat_array_to_list (var, values)
  | NeverSat -> 
    (* dlogf "Never\n"; *) []

(* Helper function for computing the gradient *)
let add_counts_to_gradient schema featurelists (wt, x) w g =
  let value = ref 0.0 in
  for j = 0 to Array.length schema - 1 do 
    let jfeatures = featurelists.(j) in
    let logdist = Array.make schema.(j) 0.0 in 
    (* Compute distribution over P(X_j) *)
    for k = 0 to schema.(j) - 1 do
      logdist.(k) <- 0.0;
      for l = 0 to Array.length jfeatures.(k) - 1 do
        logdist.(k) <- logdist.(k) +. w.(jfeatures.(k).(l))
      done;
    done;
    normalize_inplace_log logdist;
    (* Add to value of PLL *)
    value := !value +. wt *. logdist.(x.(j));
    (* For each relevant feature's gradient, we want to add the 
     * difference between the true counts and the expectation.
     * We do this by subtracting one from the true state probability
     * (and zero from all others). *)
    (* Reuse the logdist array, to save memory *)
    let dist = logdist in
    for k = 0 to schema.(j) - 1 do
      dist.(k) <- exp logdist.(k)
    done;
    (* let dist = Array.map exp logdist in *)
    dist.(x.(j)) <- dist.(x.(j)) -. 1.;
    (* Add to gradient of PLL *)
    for k = 0 to schema.(j) - 1 do
      for l = 0 to Array.length jfeatures.(k) - 1 do
        let wi = jfeatures.(k).(l) in
        g.(wi) <- g.(wi) -. wt *. dist.(k)
      done
    done
  done;
  !value

let add_counts_to_gradient_l schema featurelists (wt, x) w g =
  let value = ref 0.0 in
  for j = 0 to Array.length schema - 1 do 
    let jfeatures = featurelists.(j) in
    (* Compute distribution over P(X_j) *)
    let logdist = Array.make schema.(j) 0.0 in
    for k = 0 to schema.(j) - 1 do
      logdist.(k) <- List.sumf_map (fun wi -> w.(wi)) jfeatures.(k)
    done;
    normalize_inplace_log logdist;
    (* Add to value of PLL *)
    value := !value +. wt *. logdist.(x.(j));
    (* For each relevant feature's gradient, we want to add the 
     * difference between the true counts and the expectation.
     * We do this by subtracting one from the true state probability
     * (and zero from all others). *)
    let dist = Array.map exp logdist in
    dist.(x.(j)) <- dist.(x.(j)) -. 1.;
    (* Add to gradient of PLL *)
    for k = 0 to schema.(j) - 1 do
      List.iter (fun wi -> g.(wi) <- g.(wi) -. wt *. dist.(k)) jfeatures.(k)
    done
  done;
  !value
      
(*
 * PLL Gradient Method 1: Build up a full cache first.  Works well. 
 *)

(* Construct key statistics for PLL value and gradient computations *)
let build_pll_cache mn data = 
  Timer.start "build_pll_cache";
  let schema = mn.Mn.schema in
  let fa = Array.of_list (Mn.to_features mn) in
  let datafeatures = Array.make (Array.length data) [||] in
  for i = 0 to Array.length data - 1 do
    let (wt, x) = data.(i) in
    let featurelists = Array.map (fun d -> Array.make d []) schema in
    let append_f fi (var,value) =
      featurelists.(var).(value) <- fi :: featurelists.(var).(value) in
    let add_f fi f =
      let sl = generate_sat_states schema x f in
      List.iter (append_f fi) sl in
    Array.iteri add_f fa;
    datafeatures.(i) <- Array.map (Array.map Array.of_list) featurelists
  done;
  Timer.stop "build_pll_cache";
  let cache = (schema, fa, data, datafeatures) in
  cache


(* Compute gradient of the PLL. *)
let pll_val_and_grad_cached cache w g =
  (* Clear gradient array *)
  Timer.start "pll_val_and_grad";
  assert(Array.length w = Array.length g);
  for l = 0 to Array.length g - 1 do
    g.(l) <- 0.0
  done;
  let (schema, fa, data, datafeatures) = cache in
  let value = ref 0.0 in
  for i = 0 to Array.length data - 1 do
    value := !value +. add_counts_to_gradient schema datafeatures.(i) 
      data.(i) w g
  done;
  Timer.stop "pll_val_and_grad";
  !value


(*
 * PLL Gradient Method 2: No cache at all
 *)
let build_pll_minicache mn (data:wexample_t array) = 
  let schema = mn.Mn.schema in
  let fa = Array.of_list (Mn.to_features mn) in
  (schema, fa, data, [||])

let pll_val_and_grad cache w g =
  let (schema, fa, data, _) = cache in
  (* Clear gradient array *)
  Timer.start "pll_val_and_grad";
  assert(Array.length w = Array.length g);
  for l = 0 to Array.length g - 1 do
    g.(l) <- 0.0
  done;
  let value = ref 0.0 in
  for i = 0 to Array.length data - 1 do
    (* Compute features satisfied by each variable *)
    let featurelists = Array.map (fun d -> Array.make d []) schema in
    let append_f fi (var,value) =
      featurelists.(var).(value) <- fi :: featurelists.(var).(value) in
    let (weight, x) = data.(i) in
    let add_f fi f =
      let sl = generate_sat_states schema x f in
      List.iter (append_f fi) sl in
    Array.iteri add_f fa;
    (* Update value and gradient *)
    let featurelists = Array.map (Array.map Array.of_list) featurelists in
    value := !value +. add_counts_to_gradient schema featurelists
      data.(i) w g
  done;
  Timer.stop "pll_val_and_grad";
  !value


(* Construct an MN from the PLL cache data and the weight vector. *)
let pll_cache_to_mn cache w =
  let (schema, fa, data, _) = cache in
  Array.iter2 (fun f wi -> f.Mn.Factor.weight <- wi) fa w;
  let factors = Array.map (fun f -> Mn.Factor.Feature f) fa in
  let mn = Mn.create schema factors in
  mn

let fa_pll schema fa x =
  let score = ref 0.0 in
  let logdist = Array.map (fun d -> Array.make d 0.0) schema in
  let add_weight wi (var,value) =
    (* dlogf "value=%d w=%f" value wi; *)
    logdist.(var).(value) <- logdist.(var).(value) +. wi in
  let add_f f =
    let sl = generate_sat_states schema x f in
    List.iter (add_weight f.Mn.Factor.weight) sl in
  Array.iter add_f fa;
  Array.iter normalize_inplace_log logdist;
  for var = 0 to Array.length schema - 1 do
    (* dlogf "logdist.(%d).(%d) = %f\n" var x.(var) logdist.(var).(x.(var)); *)
    score := !score +. logdist.(var).(x.(var))
  done;
  !score

(* Support for a C encoding of MNs. *)
type mn_t

external create_mn_helper : 
    int array -> (bool * int * int) array array -> float array -> mn_t 
      = "ocaml_create_mn"

let create_mn schema fa =
  flush stdout;
  let w = Array.map (fun f -> f.Mn.Factor.weight) fa in
  let conds = Array.map (fun f -> f.Mn.Factor.cond) fa in
  create_mn_helper schema conds w

(* Support for a C encoding of a dataset. *)
type data_t

external create_data :
    (float * int array) array -> data_t = "ocaml_create_data"

(* External libraries for computing PLL and its gradient slightly faster. *)
external pll_mn :
    mn_t -> int array -> float = "ocaml_c_pll_mn"
    
external pll_val_and_grad_ext :
    mn_t -> data_t -> valgrad_callback = "ocaml_c_pll_val_and_grad"

type minicache_c_t = schema_t * Mn.Factor.feature array *
    wexample_t array * (mn_t * data_t)

let build_pll_minicache_c mn data =
  let schema = mn.Mn.schema in
  let fa = Array.of_list (Mn.to_features mn) in
  let cmn = create_mn schema fa in
  let cdata = create_data data in
  let cache = (schema, fa, data, (cmn, cdata)) in
  cache

let pll_val_and_grad_c cache w g =
  flush stdout;
  Timer.start "pll_val_and_grad";
  let (_, _, _, (mn, data)) = cache in
  let pll = pll_val_and_grad_ext mn data w g in
  Timer.stop "pll_val_and_grad";
  pll

(* Support for C encoding of external cache info *)
type cache_ext_t

external create_cache_ext :
  mn_t -> data_t -> cache_ext_t = "ocaml_create_cache"

(* external check_cache_ext :
  cache_t -> cache_t = "ocaml_c_check_cache" *)

type cache_c_t = schema_t * Mn.Factor.feature array * wexample_t array *
  (mn_t * data_t * cache_ext_t)

let build_pll_cache_c mn data =
  let schema = mn.Mn.schema in
  let fa = Array.of_list (Mn.to_features mn) in
  let cmn = create_mn schema fa in
  let cdata = create_data data in
  let ccache = create_cache_ext cmn cdata in
  (* ignore(check_cache_ext ccache); *)
  let cache = (schema, fa, data, (cmn, cdata, ccache)) in
  cache

external pll_val_and_grad_cache_ext :
    mn_t -> data_t -> cache_ext_t -> float array -> float array -> float 
      = "ocaml_c_pll_val_and_grad_cached"

let pll_val_and_grad_cached_c cache w g =
  flush stdout;
  Timer.start "pll_val_and_grad";
  let (_, _, _, (cmn, cdata, ccache)) = cache in
  (* ignore(check_cache_ext ccache); *)
  let pll = pll_val_and_grad_cache_ext cmn cdata ccache w g in
  Timer.stop "pll_val_and_grad";
  pll
