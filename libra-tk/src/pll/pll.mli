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

(** The Pll module performs efficient computation of pseudo-likelihood
    and its gradient, given a Markov network represented as a set of
    weighted conjunctive features.  It includes implementations in
    both C and OCaml, with and without a cache of feature statistics. *)

(** Keep track of how many violations of a conjunctive feature are
    present -- zero, one, or more than one. *)
type satisfaction = Sat | Unsat of (int * bool array) | NeverSat

type wexample_t = Data.wexample_t
type example_t = Data.example_t
type schema_t = Data.schema_t 

type valgrad_callback = float array -> float array -> float 

type pll_cache_t

(** Construct key statistics for PLL value and gradient computations *)
val build_pll_cache : Mn.network -> wexample_t array -> pll_cache_t

(** Compute value and gradient of PLL using cache *)
val pll_val_and_grad_cached : pll_cache_t -> valgrad_callback 

(** Construct small cache data structure, not precomputing feature counts *) 
val build_pll_minicache : Mn.network -> wexample_t array -> pll_cache_t

(** Compute value and gradient of PLL without using full cache *)
val pll_val_and_grad : pll_cache_t -> valgrad_callback

(** Construct an MN from the PLL cache data and the weight vector. *)
val pll_cache_to_mn : pll_cache_t -> float array -> Mn.network

(** Compute PLL of a single example from a feature array. *)
val fa_pll : schema_t -> Mn.Factor.feature array -> example_t -> float


(** {6 Computing PLL and its gradient with external C implementation} *)

type mn_t

(** Build MN structure for use with external C PLL implementation. *)
val create_mn : int array -> Mn.Factor.feature array -> mn_t

(** Compute PLL of MN. External C implementation. *)
external pll_mn : mn_t -> int array -> float = "ocaml_c_pll_mn"

type minicache_c_t

(** Construct small cache data structure, not precomputing feature counts,
    for external C implementation. *)
val build_pll_minicache_c : Mn.network -> wexample_t array -> minicache_c_t

(** Compute value and gradient PLL without using full cache. 
    External C implementation. *)
val pll_val_and_grad_c : minicache_c_t -> valgrad_callback

type cache_c_t

(** Construct key statistics for PLL value and gradient computations,
    for use with external C implementation. *)
val build_pll_cache_c : Mn.network -> wexample_t array -> cache_c_t

(** Compute PLL and its gradient using cache. External C implementation. *)
val pll_val_and_grad_cached_c : cache_c_t -> valgrad_callback 
