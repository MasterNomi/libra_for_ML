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

type parsenode = PTimesNode of int list
               | PPlusNode of int list
               | PVarNode of int * int
               | PConstNode of float
							 | PEvNode of int 
							 | PExpNode of int 
               | PInverseNode of int list
;;

(*
let parsenode_to_string = function
  | PTimesNode(s, l) -> s ^ " = " ^ String.concat " * " l
  | PPlusNode(s, l) -> s ^ " = " ^ String.concat " + " l
  | PWPlusNode(s, l) -> 
    let conv (w,c) = Printf.sprintf "%g %s" w c in 
    s ^ " = " ^ String.concat " + " (List.map conv l)
  | PSplitNode(s, l) ->
    let conv (w,v,c) = Printf.sprintf "%g v%d_0 %s" w v c in 
    s ^ " = " ^ String.concat " + " (List.map conv l)
  | PDistNode(s, l) -> 
    let conv (w,v) = Printf.sprintf "%g v%d_0" w v in 
    s ^ " = " ^ String.concat " + " (List.map conv l)
    *)
