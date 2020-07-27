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

open Ext

(* regexs *)
let colon = Str.regexp ":"
let whitespace = Str.regexp "[ \t]+" 
let comment = Str.regexp "#" 
let blankline = Str.regexp "^[ \t]*$" 

(* error handling *)
exception Parse_error of string

let parse_error i s =
  let msg = Printf.sprintf "Parse error on line %d: %s\n" i s in
  raise (Parse_error msg)


(* Read in parent variable constraints from input channel, one line at
   a time.  Restrictions can limit the parents to a specific set of
   parents ("none except 1 2 8 10") or to any parent not in a list
   ("all except 3 5").  Returns matrix of allowed parents for each
   child variable. 

   Example file format:
   
   # This is a comment
   0: all except 1 3   # only vars 1 and 3 may be parents of 0
   1: none except 5 2  # var 1 may only have var 5 or 2 as a parent
   2: none             # var 2 may have no parents
   5: all              # var 5 may have any parents

   *)
let parse numvars input =
  let allowed_parents = Array.make_matrix numvars numvars true in
  let linenum = ref 0 in
  (* read one line at a time until EOF *)
  try while true do 
    incr linenum;
    let s = input_line input in
    (* Remove comments and skip blank lines.
       Must add blank space, since it ignores delimiters at the
       beginning of the string otherwise. *)
    let s = List.hd (Str.split comment (" " ^ s)) in 
    let s = Str.replace_first whitespace "" s in
    if not (Str.string_match blankline s 0) then begin 
      (* get child variable *)
      let child, rule = match Str.split colon s with
      | c :: r :: [] -> int_of_string c, r
      | l -> nlogf "Wrong number of colons: %d\n" (List.length l); (0, "all") in (* parse_error !linenum "invalid number of colons" *) 
      match Str.split whitespace rule with 
      (* allow all parents except for a certain list *)
      | "all" :: "except" :: rest ->
        for i = 0 to numvars - 1 do 
          allowed_parents.(child).(i) <- true
        done;
        let pl = List.map int_of_string rest in
        List.iter (fun p -> allowed_parents.(child).(p) <- false) pl
      (* prohibit all parents except for a certain list *)
      | "none" :: "except" :: rest ->
        for i = 0 to numvars - 1 do 
          allowed_parents.(child).(i) <- false
        done;
        let pl = List.map int_of_string rest in
        List.iter (fun p -> allowed_parents.(child).(p) <- true) pl
      (* allow all or none *)
      | ["all"] -> ()
      | ["none"] ->
        for i = 0 to numvars - 1 do 
          allowed_parents.(child).(i) <- false
        done
      | _ -> parse_error !linenum rule
    end;
  done; allowed_parents
  with End_of_file -> allowed_parents
