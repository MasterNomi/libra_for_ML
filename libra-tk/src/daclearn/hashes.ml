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
