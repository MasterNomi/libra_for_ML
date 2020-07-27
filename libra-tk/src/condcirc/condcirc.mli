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

(**
The circuit library provides an interface to arithmetic circuits, which helps developing algorithms for learning and inference using arithmetic circuits.
*)

(** {5 Node } *)

(** Diffrent types of nodes in arithmetic circuits *)
type ndetails =
  Condnode.ndetails =
    TimesNode (** Represents products in the circuits. *)
  | PlusNode (**  Represents sums in the circuits. *)
  | VarNode of int * int (** Represents indicators which is defined for each state of every variables. *)
  | ConstNode of float (**  Represents parameters. *)
  | InverseNode (** Represent an inverse node, which computes the inverse of its only child. *)
	| EvNode of int 
	| ExpNode 
  | NullNode (** Represents dummy nodes *)


(**	
Each element in an arithmetic circuit is node (including parameters, plus, times, and indicators), which has unique hashid.
	Node maintains a list of its parent and its children nodes in the circuit graph. 
*)	
type node =
  Condnode.node = {
  hashid : int; (** Unique auto-increment id *)  
  mutable id : int; (** Shows the index of node in the [citcuit.nodes]. *)
  mutable parents : node list; (** List of parent nodes in the circuit graph. *)
  mutable children : node list; (** List of children nodes in the circuit graph. *) 
  mutable details : ndetails; (** Type of the node. *) 
}


(** {6 Node operations } *)


(** Creates a node whose type NullNode, and its parent and children lists are empty. *)
val null_node : node


(*val tmp_hash : unit NSet.t
*)

(** Creates an arithmetic circuit node *) 
val create_node : ndetails -> node list -> node

(** Copy constructor. *)
val ncopy : node -> node

(** Creates an indicator node. *)
val create_var : int -> int -> node

(** Creates a constant node (parameter node). *)
val create_const : float -> node

(** Creates a times (product) node. *)
val create_times : node list -> node

val create_exp : node -> node

val create_inverse: node -> node

(** Creates a plus (sum) node. *)
val create_plus : node list -> node

(** Checks whether the node is a times (product) node. *) 
val is_times : node -> bool

(** Checks whether the node is a plus (sum) node. *) 
val is_plus : node -> bool


(** Checks whether the node is an indicator node. *) 
val is_var : node -> bool

(** Checks whether the node is a constant (parameter) node. *) 
val is_const : node -> bool

(** Checks whether the node is a null node. *) 
val is_null : node -> bool

(** Gets the parameter value stored in the constant node *) 
val const_value : node -> float


(** [var_details node] returns the variable-value pair if [node] is a variable node *) 
val var_details : node -> int * int


(** [var_var node] returns the variable if [node] is a variable node *) 
val var_var : node -> int


(** [var_value node] returns the value if [node] is a variable node *) 
val var_value : node -> int

(** [is_var_product node] returns true if [node] is a times node and has at least one const node and one variable node in its children. *) 
val is_var_product : node -> bool

(** [is_sot_dist node] returns [true] if [node] is a plus node and  [is_var_product] is true for all of its children. *)
val is_sot_dist : node -> bool

(** [set_const node wt] creates a const node with the value [wt] and assign it to [node]. *)
val set_const : node -> float -> unit

(*val __add_parent : node -> node -> unit
val __remove_parent : node -> node -> unit
*)

(** [add_child parent child] appends [child] to the children of [parent] and updates the parent of [child] accordingly. *)
val add_child : node -> node -> unit

(** [remove_child parent child] removes child from the children of [parent] and updates [child] accordingly. *)
val remove_child : node -> node -> unit

(** [remove_all_children parent] removes all children of [parent]. *) 
val remove_all_children : node -> unit

(** [remove_all_parents child] removes all parent of [child]. *)
val remove_all_parents : node -> unit

(** [unlink node] removes all parents and children of [node]. *)
val unlink : node -> unit

(** [id node] returns the id of [node]. *)
val id : node -> int

(** [parents node] returns the parents of [node]. *)
val parents : node -> node list

(** [children node] returns the children of [node]. *)
val children : node -> node list

(** [num_children node] returns the number of children of [node]. *)
val num_children : node -> int


(*val node_name_hack : node -> string*)

(** {6 Printing and debuging operations} *)

val node_name : node -> string
val string_of_node : node -> string
val output_node : out_channel -> node -> unit
val output_root : out_channel -> int array -> node -> unit
val print_node : node -> unit
val print_node_endline : node -> unit

(** {5 Arithmetic circuits} *)

exception Unknown_child of string * string
exception UnsupportedNodeType


(** {6 Auxilary types} *) 

type scratch_t = float array * int array array
type evidence_t = float array array
type schema_t = int array
type example_t = int array


(** {6 Circuit type} *)

(** Data structure for arithmetic circuits *)
type circuit = {
  schema : schema_t; (** The schema of variables in the circuit. *) 
  vnodes : node array array; (** [vnodes.(i)] is coresponding to the variable [i] in [schema]. [vnodes.(i).(j)] is the variable indictator for state [j] of variable [i]. *)  
  vnodes_l : node list array; (** The same as [vnodes], but using list of indicator variables (instead of array) for represenging different dimensions of variables. *)
	evnodes: node array;
  flat_vnodes : node array; (** Array of all indictator variables. *)
  mutable nodes : node array; (** All nodes in the circuit. *)
  mutable root : node; (** The circuit root node. *)
  mutable size : int; (** The size of the circuit. *)
}

(** {6 Circuit operations} *)

(** Reads an arithmetic circuits from a file description *)
val load : in_channel -> circuit

(** Converts the given schema to a string *)
val string_of_schema : schema_t -> string

val node_output_id : int -> node -> int


(*

val node_input_index : int -> int -> int
*)

(** Stores an arithmetic circuit in the regular plain text format. *) 
val output : out_channel -> circuit -> unit


(** Stores an arithmetic circuit in the java script format for visualizing using Digraph. *)
val output_js : out_channel -> circuit -> unit

(** Stores an arithmetic circuit in the dot format (for visualizing). *)
val output_dot : out_channel -> circuit -> unit

(** Prints an arithmetic circuit in the terminal. *)
val print : circuit -> unit

(** Returns the schema of an arithmetic circuit. *)
val schema : circuit -> schema_t

(** Returns the number of variables in an arithmetic circuit. *)
val numvars : circuit -> int

(** Returns the node at a specific index. *)
val node : circuit -> int -> node

(** [all_var_dists circ] returns all plus nodes in the circuit [circ] that represent distributions. *)
val all_var_dists : circuit -> node list

(** [dist_var n] returns the index of the random variable being modeled given the node [n] representing a multinomial distribution. *) 
val dist_var : node -> int

(** Returns all indicator nodes (variable nodes). *)
val all_vars : circuit -> node array array

(** [sibling_vars circ n] returns a list of all indicator nodes for all values of the same variable given the indicator node [n] for a particular variable/value combination. For example, given the indicator node for [x_5=1], it returns indicators for [x_5=0, x_5=1, ..., x_5=k]. *)
val sibling_vars : circuit -> node -> node list

(** [var circ i] returns the array of indicator nodes for the given random variable index [i] and the circuit [circ]. *)
val var : circuit -> int -> node array

(** Converts an array of indicator node arrays, one per random variable, into a flat array of all indicator nodes. *)
val flatten_vars : 'a array array -> 'a array

(**  Returns a flat array of all indicator nodes for all random variables. *)
val flat_vars : circuit -> node array

(** Puts all nodes in the circuit into a topological partial order. *)
val order : circuit -> unit

(** Creates an indicator variable for each state of every variable based on the given schema *)
val make_vnodes : schema_t -> node array array


(** Creates evidence variables. *) 
val make_evnodes : int -> node array 

(*
val __make_node_array : node list -> node array
*)

(** Rebuilds the circuit data structure from the root node. Fixes inconsistent parent/child relationships and removes orphaned nodes, leading to a compact and consistent node array. *)
val rebuild : circuit -> unit

(** [of_graph schema vn vnl root] constructs a new circuit from [schema] and [root] node and places it in topological order. [vn] is node  *)
val of_graph : schema_t -> node array array -> node list array -> node array-> node -> circuit

(*val of_graph2 : schema_t -> schema_t -> node -> circuit
*)

(** Returns the depth of an arithmetic circuit *)
val depth : circuit -> int


val update_parents : circuit -> unit

(*val of_parse : int list * AcParseType.parsenode list -> circuit
*)

(** Return the number of edges in an arithmetic circuit. *)
val num_edges : circuit -> int

(** Return the number of parameters in an arithmetic circuit. *)
val num_params : circuit -> int


(** {6 Create, set evidence} *)


val create_evidence : circuit -> evidence_t
val create_evidence_s : int array -> evidence_t
val ev_true : evidence_t -> int -> int -> unit
val ev_false : evidence_t -> int -> int -> unit

(*val set_evidence : 'a array -> circuit -> 'a array array -> unit
*)

(** {6 Other evidence related operations} *)

val example_to_ev : schema_t -> example_t -> evidence_t

(** [ev_intersect ev1 ev2] returns [true] when evidence [e1] and [e2] are not mutually exclusive. *)
val ev_intersect : evidence_t -> evidence_t -> bool


val print_ev : evidence_t -> unit


(** {5 Arithmetic circuits for Markov networks} *)


(** [condition] represents a variable value combination in features. For example, [(true,5,1)] means that variable 5 has state 1, has been shown using [+v5_1]. On the other hand, [(false, 5, 1)] says that variable 5 does not have state 1 ([-v5_1]). *)
type condition = bool * int * int


(** Represents feature and its weight in the coresponding log-linear model of the arithmetic circuit. *)
type feature = {
  acnode : node;
  mutable weight : float; (** The feature weight *)
  cond : condition list; (** The set of conditions that creates a feature, e.g. [+v3_1 +v4_2 +v6_0] *)
  ev : evidence_t;
}

(** [feature_node f] returns the acnode related to the feature [f]. *)
val feature_node : feature -> node


val feature_value : feature -> float

val set_feature_value : feature -> float -> unit

val create_feature : schema_t -> node -> condition array -> feature


(** Prunes a circuit conditioned on evidence. *)
val prune_for_evidence : circuit -> example_t -> unit


(** Checks to see if an example satisfies a condition *)
val c_satisfied : example_t -> condition -> bool

(** Checks to see if an instance satisfies all conditions *)
val f_satisfied : example_t -> feature -> bool

(** Prunes features by removing empty, redundant, and contradictory circuit features *)
val prune_features : feature list -> example_t -> feature list

(** Converts a condition to evidence. *)
val conditions_to_ev : schema_t -> (condition) array -> evidence_t

(* Returns true when two given conditions are not mutually exclusive. *)
val cond_intersect : schema_t -> (condition) array -> (condition) array -> bool


(** {6 Read/Write features} *)

(*
val input_features_lex : circuit -> Lexing.lexbuf -> feature list
*)

val input_features : circuit -> in_channel -> feature list

val output_feature : out_channel -> (node -> int) -> feature -> unit

val output_feature_list :
  out_channel -> (node -> int) -> feature list -> unit

(** Stores the given arithmetic circuit and feature list in [.ac] format. *)
val output_with_features : out_channel -> circuit -> feature list -> unit



(** Reads an arithmetic circuits and log-linear features from [.ac] file (used for arithmetic circuts that represent Markov networks) *)
val load_with_features : in_channel -> circuit * feature list




(** {6 Circuit derivatives } *)
  
(** Data structure for computing the derivatives of circuits with respect to features.
 *)
type deriv_scratch = {
  dr : float array;
  vr : float array;
  vr_nz : float array;
  bit1 : bool array;
  bit2 : bool array;
  cids : int array array;
}


(** Initializes the data structure for computing the derivatives *)
val create_deriv_scratch : circuit -> deriv_scratch

(** Computes derivatives of an arithmetic circuit with respect to all features conditioned on evidence *)
val get_derivatives_raw : circuit -> deriv_scratch -> evidence_t -> float array -> unit



(** {5 Inference using arithmetic circuits} *)

(*
val node_logvalue : float array -> node -> int array -> float
*)

val create_scratch : circuit -> scratch_t

(** Maps {!circuit.nodes} to their logvalues after the evaluation of the [circuit]. *)
val logvalues : scratch_t -> circuit -> float array

(*val loglikelihood_ev : scratch_t -> circuit -> evidence_t -> float
val loglikelihood_x : scratch_t -> circuit -> example_t -> float
*)

(** [logprob_x scratch circuit evidence] @return the log probability of [evidence] given [circuit]. For arithmetic circuits that represent Bayesian networks the log probalitity is normalized, but for circuits representing Markov networks it is unnormalized.  
[scratch] can be computed using {!create_scratch}.  *)
val logprob_ev_x : scratch_t -> circuit -> evidence_t -> float array-> float

(** [logprob_x scratch circ s] @return the log probability of the example [s] given the circuit [circ].
 For arithmetic circuits that represent Bayesian networks the log probalitity is normalized, but for circuits representing Markov networks it is unnormalized.  
[scratch] can be computed using {!create_scratch}. *)
val logprob_y : scratch_t -> circuit -> example_t -> float array-> float

(** [get_logmarginals circ ds s] @return the log-marginals of the example [s] given the circuit [circ] and its derivative scratch [ds]. *) 
val get_logmarginals : circuit -> deriv_scratch -> example_t -> float array -> float array array

(** [get_marginals circ ds s] @return the marginals of the example [s] given the circuit [circ] and its derivative scratch [ds]. *) 
val get_marginals : circuit -> deriv_scratch -> example_t -> float array -> float array array

(** Computes the log partition function *)
val compute_z : scratch_t -> circuit -> float array-> float



(** [get_mpe circ scratch e] computes the MAP state of variables given the arithmetic circuit [circ], its empty derivative data structure [scratch], and the evidence [e]. *)
val get_mpe : circuit -> deriv_scratch -> int array -> float array-> int array


(** {5 Hashing modules for Node} *)

(*val hashid : node -> int *)
module NHashMap :
  sig
    type key = node
    type 'a t = 'a Condnode.NHashMap.t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
  end
module NHashSet :
  sig
    type key = node
    type 'a t = 'a NHashMap.t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val copy : 'a t -> 'a t
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val length : 'a t -> int
    val add : unit t -> key -> unit
    val iter : (key -> unit) -> unit t -> unit
    val fold : (key -> 'a -> 'a) -> unit t -> 'a -> 'a
    val to_list : unit t -> key list
    val sum_map : (key -> int) -> unit t -> int
    val sumf_map : (key -> float) -> unit t -> float
    val filter : (key -> bool) -> unit t -> unit
  end

module NSet :
  sig
    type key = node
    type 'a t = 'a NHashMap.t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val copy : 'a t -> 'a t
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val length : 'a t -> int
    val add : unit t -> key -> unit
    val iter : (key -> unit) -> unit t -> unit
    val fold : (key -> 'a -> 'a) -> unit t -> 'a -> 'a
    val to_list : unit t -> key list
    val sum_map : (key -> int) -> unit t -> int
    val sumf_map : (key -> float) -> unit t -> float
    val filter : (key -> bool) -> unit t -> unit
  end
module NMap :
  sig
    type key = node
    type 'a t = 'a NHashMap.t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
  end

(*  
val global_hashid : int ref
val gen_hashid : unit -> int
*)

(*
val add_related :
  (NSet.key -> NSet.key list) -> unit NSet.t -> NSet.key -> unit
val relatedl : (NSet.key -> NSet.key list) -> NSet.key list -> unit NSet.t

val add_related_a : (node -> node list) -> bool array -> node -> unit
val related : (NSet.key -> NSet.key list) -> NSet.key -> unit NSet.t
val ancestors : NSet.key -> unit NSet.t
val descendants : NSet.key -> unit NSet.t
val ancestorsl : NSet.key list -> unit NSet.t
val descendantsl : NSet.key list -> unit NSet.t
val move_parent_refs : node -> node -> unit


val prune_orphans : NSet.key -> unit
val prune_single_parents : NSet.key -> unit
val prune_single_children : NSet.key -> unit

*)

(** [node_iter f root] {b Applies} function [f] on every nodes of the circuit whose root node is determined by [root]. *)
val node_iter : (NSet.key -> 'a) -> NSet.key -> unit


val node_map : (NSet.key -> 'a) -> NSet.key -> 'a list

val root_to_list : NSet.key -> NSet.key list



val relatedl_a : int -> (node -> node list) -> node list -> bool array
val a_mem : 'a array -> node -> 'a

(*
val maxf : float -> float -> float
val logsumexp : float list -> float
val logsumexp2 : float -> float -> float
*)
(*
val output_node_rec :
  out_channel -> unit NHashSet.t -> int -> int -> NHashSet.key -> int


val print_root : int array -> node -> unit
*)


(*val create_rec_scratch : circuit -> 'a option array*)


(*val node_logvalue_rec : float option array -> int array -> node -> float
*)


(*
val compute_node_logvalue_rec : float option array -> int array -> node -> float
*)


(*
val loglikelihood_rec : float option array -> circuit -> int array -> float
val compute_z_rec : float option array -> circuit -> float
*)

(*
val node_logvalue_dtopo : deriv_scratch -> int -> node -> unit
*)
(*
val d_upward_pass : circuit -> deriv_scratch -> float array array -> unit
val d_downward_pass : circuit -> deriv_scratch -> unit


val __print_ds : circuit -> deriv_scratch -> unit
*)



(*  
val node_logvalue_mpe : deriv_scratch -> int -> node -> unit
val mpe_upward_pass : circuit -> deriv_scratch -> float array array -> unit
val mpe_downward_pass : circuit -> deriv_scratch -> int -> unit
*)

(*
val input_schema : in_channel -> int array
val parse : in_channel -> int list * AcParseType.parsenode list
*)



(*exception NonConstantFeatureWeight of int*)

  
