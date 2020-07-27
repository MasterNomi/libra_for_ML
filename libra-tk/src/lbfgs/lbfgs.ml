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
 * OCaml interface for LBFGS library. 
 * LBFGS library written by Naoaki Okazaki.
 * Stubs written by Daniel Lowd.
 *)

type lbfgs_err =
      (* Successful execution *)
      Success     
    | Stop
    | AlreadyMinimized
    | UnknownError
    (* Logic error. *)
    | LogicError 
    (* Insufficient memory. *)
    | OutOfMemory 
    (* The minimization process has been canceled. *)
    | Canceled   
    (* Invalid number of variables specified. *)
    | InvalidN   
    (* Invalid number of variables (for SSE) specified. *)
    | InvalidNSSE
    (* The array x must be aligned to 16 (for SSE). *)
    | InvalidXSSE
    (* Invalid parameter lbfgs_parameter_t::epsilon specified. *)
    | InvalidEpsilon
    (* Invalid parameter lbfgs_parameter_t::past specified. *)
    | InvalidTestPeriod
    (* Invalid parameter lbfgs_parameter_t::delta specified. *)
    | InvalidDelta 
    (* Invalid parameter lbfgs_parameter_t::linesearch specified. *)
    | InvalidLinesearch 
    (* Invalid parameter max_step specified. *)
    | InvalidMinstep  
    (* Invalid parameter max_step specified. *)
    | InvalidMaxstep  
    (* Invalid parameter ftol specified. *)
    | InvalidFtol 
    (* Invalid parameter lbfgs_parameter_t::wolfe specified. *)
    | InvalidWolfe
    (* Invalid parameter gtol specified. *)
    | InvalidGtol 
    (* Invalid parameter xtol specified. *)
    | InvalidXtol 
    (* Invalid parameter max_linesearch specified. *)
    | InvalidMaxLinesearch 
    (* Invalid parameter orthantwise_c specified. *)
    | InvalidOrthantwise   
    (* Invalid parameter lbfgs_parameter_t::orthantwise_start specified. *)
    | InvalidOrthantwiseStart
    (* Invalid parameter lbfgs_parameter_t::orthantwise_end specified. *)
    | InvalidOrthantwiseEnd
    (* The line-search step went out of the interval of uncertainty. *)
    | OutOfInterval 
    (* A logic error occurred; alternatively, the interval of uncertainty 
       became too small. *)
    | IncorrectTminmax  
    (* A rounding error occurred; alternatively, no line-search step
       satisfies the sufficient decrease and curvature conditions. *)
    | RoundingError
    (* The line-search step became smaller than lbfgs_parameter_t::min_step. *)
    | MinimumStep
    (* The line-search step became larger than lbfgs_parameter_t::max_step. *)
    | MaximumStep
    (* The line-search routine reaches the maximum number of evaluations. *)
    | MaximumLinesearch 
    (* The algorithm routine reaches the maximum number of iterations. *)
    | MaximumIteration 
    (* Relative width of the interval of uncertainty is at most xtol. *)
    | WidthTooSmall
    (* A logic error (negative line-search step) occurred. *)
    | InvalidParameters
    (* The current search direction increases the objective function value. *)
    | IncreaseGradient

let errstring = function
| Success -> "Success"
| Stop -> "Stop"
| AlreadyMinimized -> "AlreadyMinimized"
| UnknownError -> "UnknownError"
| LogicError  -> "LogicError"
| OutOfMemory -> "OutOfMemory"
| Canceled    -> "Canceled" 
| InvalidN    -> "InvalidN"
| InvalidNSSE -> "InvalidNSSE"
| InvalidXSSE -> "InvalidXSSE"
| InvalidEpsilon -> "InvalidEpsilon"
| InvalidTestPeriod -> "InvalidTestPeriod"
| InvalidDelta -> "InvalidDelta"
| InvalidLinesearch -> "InvalidLinesearch" 
| InvalidMinstep -> "InvalidMinstep"
| InvalidMaxstep -> "InvalidMaxstep"
| InvalidFtol -> "InvalidFtol"
| InvalidWolfe -> "InvalidWolfe"
| InvalidGtol -> "InvalidGtol"
| InvalidXtol -> "InvalidXtol"
| InvalidMaxLinesearch -> "InvalidMaxLinesearch"
| InvalidOrthantwise -> "InvalidOrthantwise"
| InvalidOrthantwiseStart -> "InvalidOrthantwiseStart"
| InvalidOrthantwiseEnd -> "InvalidOrthantwiseEnd"
| OutOfInterval -> "OutOfInterval"
| IncorrectTminmax -> "IncorrectTminmax"
| RoundingError -> "RoundingError"
| MinimumStep -> "MinimumStep"
| MaximumStep -> "MaximumStep"
| MaximumLinesearch -> "MaximumLinesearch"
| MaximumIteration -> "MaximumIteration"
| WidthTooSmall -> "WidthTooSmall"
| InvalidParameters -> "InvalidParameters"
| IncreaseGradient -> "IncreaseGradient"

let all_errors = [|Success;
                   Stop;
                   AlreadyMinimized;
                   UnknownError;
                   LogicError;
                   OutOfMemory;
                   Canceled;
                   InvalidN;
                   InvalidNSSE;
                   InvalidXSSE;
                   InvalidEpsilon;
                   InvalidTestPeriod;
                   InvalidDelta;
                   InvalidLinesearch;
                   InvalidMinstep;
                   InvalidMaxstep;
                   InvalidFtol;
                   InvalidWolfe;
                   InvalidGtol;
                   InvalidXtol;
                   InvalidMaxLinesearch;
                   InvalidOrthantwise;
                   InvalidOrthantwiseStart;
                   InvalidOrthantwiseEnd;
                   OutOfInterval;
                   IncorrectTminmax;
                   RoundingError;
                   MinimumStep;
                   MaximumStep;
                   MaximumLinesearch;
                   MaximumIteration;
                   WidthTooSmall;
                   InvalidParameters;
                   IncreaseGradient|]

let err_of_int i =
  if i >= 0 then
    all_errors.(i)
  else
    all_errors.(i + 1027)

type valgrad_func = float array -> float array -> float
external c_lbfgs : 
    valgrad_func -> float array -> float -> float -> int -> int*float = "c_lbfgs"

let minimize_l1 c f x decrease maxiter =
  flush stdout; 
  let (result, value) = c_lbfgs f x c decrease maxiter in
  (err_of_int result, value)

let minimize_l1_simple c f x =
  minimize_l1 c f x 1.0e-5 100

let minimize f x decrease maxiter = 
  minimize_l1 0.0 f x decrease maxiter

let minimize_simple f x =
  minimize f x 1.0e-5 100
