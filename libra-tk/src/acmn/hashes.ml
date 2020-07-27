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

(*********************************************************
 * Reusable hashes.  By maintaining a pool of hashes that
 * can be used over and over again, we avoid a lot of
 * garbage collection and speed things up significantly.
 *********************************************************)

open Circuit
open Ext

let tmp_hashes: (node NMap.t list ref) = ref [] 
let unused_hashes = ref []
let new_hash () = NMap.create 100

let tmp_sets: (unit NSet.t list ref) = ref []
let unused_sets = ref []
let new_set () = NSet.create 100

let get_hash () =
  match !unused_hashes with
    []   -> let h = new_hash () in
            tmp_hashes := h :: !tmp_hashes ; h
  | h::l -> unused_hashes := l ; h

let get_set () =
  match !unused_sets with
    []   -> let h = new_set () in
            tmp_sets := h :: !tmp_sets ; h
  | h::l -> unused_sets := l ; h

let release_hash h =
  NMap.clear h;
  unused_hashes := h :: !unused_hashes

let release_set h =
  NSet.clear h;
  unused_sets := h :: !unused_sets

let clear_hashes () =
  unused_hashes := !tmp_hashes ;
  List.iter NMap.clear !tmp_hashes

let clear_sets () =
  unused_sets := !tmp_sets ;
  List.iter NSet.clear !tmp_sets
