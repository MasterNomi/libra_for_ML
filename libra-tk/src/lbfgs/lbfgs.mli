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

(** The {b lbfgs} library acts as a simple wrapper to the C
    LBFGS library written by Naoaki Okazaki. *)

(** Type of error messages produced by LBFGS minimizer. *)
type lbfgs_err =
    Success
  | Stop
  | AlreadyMinimized
  | UnknownError
  | LogicError
  | OutOfMemory
  | Canceled
  | InvalidN
  | InvalidNSSE
  | InvalidXSSE
  | InvalidEpsilon
  | InvalidTestPeriod
  | InvalidDelta
  | InvalidLinesearch
  | InvalidMinstep
  | InvalidMaxstep
  | InvalidFtol
  | InvalidWolfe
  | InvalidGtol
  | InvalidXtol
  | InvalidMaxLinesearch
  | InvalidOrthantwise
  | InvalidOrthantwiseStart
  | InvalidOrthantwiseEnd
  | OutOfInterval
  | IncorrectTminmax
  | RoundingError
  | MinimumStep
  | MaximumStep
  | MaximumLinesearch
  | MaximumIteration
  | WidthTooSmall
  | InvalidParameters
  | IncreaseGradient

(** Convert an error type into a human-readable error string, mainly for
    debugging purposes. *)
val errstring : lbfgs_err -> string

(** Type of function callback used by the LBFGS minimizer.  This
    function must computing the value and gradient of a function at a
    given point. *)
type valgrad_func = float array -> float array -> float

(** [minimize_l1 c f x decrease maxiter] finds the minimum of function [f]
    with L1 penalty weight [c], starting its search at point [x].  
    Halts when the rate of decrease is less than [decrease] or after 
    [maxiter] iterations.  Returns an error code and minimum function value.  
    [x] is modified to contain the minimizing point. *) 
val minimize_l1 :
  float -> valgrad_func -> float array -> float -> int -> lbfgs_err * float

(** [minimize_l1_simple c f x] finds the minimum of function [f]
    with L1 penalty weight [c], starting its search at point [x].  
    Uses default parameters for its convergence criteria.
    Returns an error code and minimum function value.  
    [x] is modified to contain the minimizing point. *) 
val minimize_l1_simple :
  float -> valgrad_func -> float array -> lbfgs_err * float

(** [minimize f x decrease maxiter] finds the minimum of function [f],
    starting its search at point [x].  Halts when minimum rate of decrease
    is less than [decrease] or after [maxiter] iterations.  Returns an
    error code and minimum function value.  [x] is modified to contain the
    minimizing point.  *) 
val minimize :
  valgrad_func -> float array -> float -> int -> lbfgs_err * float

(** [minimize_simple f x] finds the minimum of function [f],
    starting its search at point [x].  Uses default parameters for
    its convergence criteria.  Returns an error code and minimum 
    function value.  [x] is modified to contain the minimizing point.  *) 
val minimize_simple : valgrad_func -> float array -> lbfgs_err * float
