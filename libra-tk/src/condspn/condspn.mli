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

(** SPN library provides data structure and methods for learning sum-product networks. *)



(** {6 Data structures} *)

type data_t = int array array
type local_schema_t = (int*int) array
type schema_t = int array

(** Different node types in SPNs *)
type spn_node_type = TimesNode
        | PlusNode
        | NullNode (** Used for dummy nodes. *) 
  | LeafNode;;

type spnode_cond = {
  id: int;
  mutable parents: spnode_cond array;
  mutable children: spnode_cond array;
  mutable params: float array;
  mutable nodetype: spn_node_type;
  mutable schema: (int * int) array;
  mutable data: int array array;
	mutable evnums : int;
  mutable ev: float array array;
  mutable final : bool;
  mutable infcached: bool;
  mutable logpTemp : float array;

}
(*
(** Each node in an SPN is a [spnode]. *)
type spnode_cond = {
  id: int; (** Each node of an SPN has a unique auto_increment ID. *)
  mutable parents: spnode array; (** Array of parents nodes in the SPN. *)
  mutable children: spnode array; (** Array of children nodes in the SPN. *)
  mutable params: float array; (** if the spnode is PlusNode, then params holds the edge weights of each child in the list of children. *)  
  mutable nodetype: spn_node_type; 
  mutable schema: local_schema_t; (** The first items show the relative schema of the current node. The second items show the index of variables in the schema of the network. *) 
  mutable data: data_t; (** Each node carry some portion of training data. Required for learning.*)
  mutable acname: string; (** If the node is LeafNode, then [acname] is the filename of a tractable graphical model that represent the leaf distribution. *)
  mutable ac_index: int; (** If the node is LeafNode, the [Comps\[ac_index\]] holds the leaf distribution after loading them from file. *) 
  mutable final : bool; (** Used in the learning to show that a node is final and cannot be modified. *)
  mutable infcached: bool; (** Used for caching inference result to speedup. *)
  mutable logpTemp : float array; (** Used for caching inference result to speedup. *)
}*)


(** Raised when data clustering is not possible. *)
exception HSplitNotPossible

(** Raised when variable clustering is not possible. *)
exception VSplitNotPossible

exception NodeTypeError


(** Stores the nodes of an SPN network. *)
val node_array : spnode_cond array ref
(*val comps : spnode list ref
*)


(** {6 Operations} *)

(** [create_node t pt c pm d s] creates a new spn node and adds ot to {!node_array}. [t] is the node type, [pt] is the list of parents, [c] is the children list, [pm] is the list of parameters, [d] is data, and [s] is the schema. *)
val create_node: spn_node_type -> spnode_cond array -> spnode_cond array -> float array -> data_t -> local_schema_t -> float array array -> int-> spnode_cond

(** Creates a plus node for the given data and schema. *)
val create_plus: data_t -> local_schema_t -> float array array -> int -> spnode_cond


(** Creates a times node for the given data and schema. *)
val create_times:  data_t -> local_schema_t -> float array array -> int  -> spnode_cond


(** Create a leaf node for the given data and schema. *)
val create_leaf:  data_t -> local_schema_t -> float array array -> int -> spnode_cond

(** Returns a string value corresponding to the node types. *)
val get_type_name: spn_node_type -> string

(** Adds a child to the node, and update the parameters based on the number of samples in the child node. *)
val add_child: spnode_cond -> spnode_cond -> unit


(** Sets the parent of a node, and also updates the children of the parent node. *)
val set_parent: spnode_cond -> spnode_cond -> unit

(** Writes the information of a single SPN node into output file. *)
val print_node: out_channel -> spnode_cond -> unit

(** Writes the SPN structure into the output file *)
val print_model: out_channel -> spnode_cond -> unit



(*
val print_params: spnode-> unit
val print_tree: spnode-> int -> unit
*)


(** Prints the information of an SPN. Used for dubugging. *)
val print_spn: spnode_cond -> unit


(** [h_split node num_part lambda  concurrent_thr ratio] learns a sum node using sample clustering for conditional distribution. [num_part] is the maximum number of clusters. [lambda] is a penalty for the number of clusters. [concurrent_thr] determinied the number of concurrent processes for clustering. [ratio] is no longer in used. 
@raise HSplitNotPossible if the number of found clusters is one.  *)
val h_split_cond: spnode_cond-> int -> float -> int -> float-> float -> float -> (spnode_cond list * float array array)


(** Similar to [h_split], but never raises HSplitNotPossible, so it always find a split. (Useful for the spnlearn algorithm). *)
val h_split_force_cond: spnode_cond-> int -> float -> int -> float-> float -> float ->(spnode_cond list * float array array)




(** [v_split node cut_thr] learns a product node using variable clustering. [node] is a spnode to split. We suppose that there is no edge between two nodes if the mutual information of the nodes is less than [cut_thr]. 
*)
val v_split_cond: spnode_cond -> float -> spnode_cond list


(** [v_split_gtest node cut_thr] learns a product node using variable clustering. [node] is a spnode to split. It suppose no edge between two nodes if g-test is less than [cut_thr]. 
*)
val v_split_gtest_cond: spnode_cond -> float -> spnode_cond list


val learn_univariate_softmax_w: spnode_cond -> float -> float -> float array array

