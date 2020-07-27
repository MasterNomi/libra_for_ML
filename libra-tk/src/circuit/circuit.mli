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

(** The Circuit module provides an interface for working with 
    arithmetic circuits, so that learning and inference algorithms
    can be built on top of this data structure/inference
    representation. *)

(** {5 Node } *)

(** Different types of nodes in arithmetic circuits *)
type ndetails =
  Node.ndetails =
    TimesNode (** Represents products in the circuits *)
  | PlusNode (** Represents sums in the circuits *)
  | VarNode of int * int (** Indicator variable for (var,value) pair *)
  | ConstNode of float (** Represents parameters (log-space) *)
  | NullNode (** Represents dummy nodes *)


(** Each element in an arithmetic circuit is a node (including
    parameters, plus, times, and indicators), which has a unique hashid.
    Node maintains a list of its parent and its children nodes in the
    circuit graph. *)
type node =
  Node.node = {
  hashid : int; (** Unique auto-increment id *)  
  mutable id : int; (** Shows the index of node in the [citcuit.nodes]. *)
  mutable parents : node list; (** List of parent nodes in the circuit graph. *)
  mutable children : node list; (** List of children nodes in the circuit graph. *) 
  mutable details : ndetails; (** Type of the node. *) 
}


(** {6 Node operations } *)

(** Creates a node with type NullNode, and its parent and children
    lists are empty. *)
val null_node : node

(** Creates a node with the given node specification and children. *) 
val create_node : ndetails -> node list -> node

(** Copy constructor. *)
val ncopy : node -> node


(** Creates an indicator node. *)
val create_var : int -> int -> node

(** Creates a constant node (parameter node). *)
val create_const : float -> node

(** Creates a times (product) node. *)
val create_times : node list -> node

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


(** [var_details node] returns the variable-value pair if [node] is an
    indicator variable node *) 
val var_details : node -> int * int

(** [var_var node] returns the variable if [node] is a variable node *) 
val var_var : node -> int

(** [var_value node] returns the value if [node] is a variable node *) 
val var_value : node -> int

(** [is_var_product node] returns true if [node] is a product of
    a constant and an indicator variable. *) 
val is_var_product : node -> bool

(** [is_sot_dist node] returns true if [node] is a sum of variable
    products (constants times indicator variables). Nodes with this
    structure are used to represent marginal distributions over
    individual variables. *)
val is_sot_dist : node -> bool

(** [set_const node wt] sets the value of const node [node] to [wt]. *)
val set_const : node -> float -> unit

(** [add_child parent child] appends [child] to the children of
    [parent] and updates the parent of [child] accordingly. *)
val add_child : node -> node -> unit

(** [remove_child parent child] removes [child] from the children of
    [parent] and updates [child] accordingly. *)
val remove_child : node -> node -> unit

(** [remove_all_children parent] removes all children of [parent]. *) 
val remove_all_children : node -> unit

(** [remove_all_parents child] removes all parents of [child]. *)
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


(** {6 Printing and debugging operations} *)

(** [node_name] returns the name of node [node], e.g., "n5". *)
val node_name : node -> string

(** [string_of_node node] returns a string representation of node [node]. *)
val string_of_node : node -> string

(** [output_node out node] writes node [node] to the output channel [out]. *)
val output_node : out_channel -> node -> unit

(** [output_root out schema root] recursively writes circuit to output
    channel [out], starting at the root. *)
val output_root : out_channel -> int array -> node -> unit

(** Prints a single node to stdout. *)
val print_node : node -> unit

(** Prints a single node to stdout, including a newline at the end. *)
val print_node_endline : node -> unit


(** {5 Arithmetic circuits} *)

(** Parse error: Node references a child that has not yet been defined. *)
exception Unknown_child of string * string

(** Parse error: Node used for feature weight is not a const node. *)
exception NonConstantFeatureWeight of int

(** Type error: Given node type is not supported by this function. *)
exception UnsupportedNodeType


(** {6 Auxilary types} *) 

(** Evidence data structure that specifies value of each indicator
    variable. *)
type evidence_t = float array array 

(** Data types from Data module. *)
type schema_t = Data.schema_t
type example_t = Data.example_t


(** {6 Circuit type} *)

(** Data structure for arithmetic circuits. Fields are mutable so that
    the circuit can be updated in place. *)
type circuit = {
  schema : schema_t; (** Data schema specifies the range of each variable. *)
  vnodes : node array array; 
  (** Array of arrays of indicator nodes.  [vnodes.(i).(j)] is the
      indicator variable node for state [j] of variable [i]. *)  
  vnodes_l : node list array; 
  (** Array of lists of indicator nodes.  
      Redundant with vnodes, but maintained for convenience. *)
  flat_vnodes : node array; 
  (** Array of all indictator variables.  
      Redundant with vnodes, but maintained for convenience. *)
  mutable nodes : node array; (** All nodes in the circuit. *)
  mutable root : node; (** The circuit root node. *)
  mutable size : int; (** The number of nodes in the circuit. *)
}

(** {6 Circuit operations} *)

(** Reads an arithmetic circuit from a file description. *)
val load : in_channel -> circuit

(** Stores an arithmetic circuit in the regular plain text format. *) 
val output : out_channel -> circuit -> unit


(** Stores an arithmetic circuit in the javascript format for
    visualizing using Digraph. *)
val output_js : out_channel -> circuit -> unit

(** Stores an arithmetic circuit in the dot format (for visualizing). *)
val output_dot : out_channel -> circuit -> unit

(** Prints an arithmetic circuit to stdout. *)
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

(**  Returns a flat array of all indicator nodes for all random variables. *)
val flat_vars : circuit -> node array

(** Puts all nodes in the circuit into a topological partial order. *)
val order : circuit -> unit

(** Creates an indicator variable for each state of every variable
    based on the given schema *)
val make_vnodes : schema_t -> node array array

(** Rebuilds the circuit data structure from the root node. Fixes
    inconsistent parent/child relationships and removes orphaned nodes,
    leading to a compact and consistent node array. *)
val rebuild : circuit -> unit

(** [of_graph schema vn vnl root] constructs a new circuit from [schema] and [root] node and places it in topological order. [vn] is node  *)
val of_graph : schema_t -> node array array -> node list array -> node -> circuit

(** Returns the depth of an arithmetic circuit *)
val depth : circuit -> int

(** Update parent references for all nodes to be consistent. *)
val update_parents : circuit -> unit

(** Return the number of edges in an arithmetic circuit. *)
val num_edges : circuit -> int

(** Return the number of parameters in an arithmetic circuit. *)
val num_params : circuit -> int


(** {6 Create, set evidence} *)




(** [create_evidence schema] creates an matrix of ones as the values of indicator variables. Each column corresponds to a variable in the schema. *) 
val create_evidence_s : schema_t -> evidence_t

(** Returns [create_evidence_s] of the [circuit.schema]. *) 
val create_evidence : circuit -> evidence_t
val ev_true : evidence_t -> int -> int -> unit
val ev_false : evidence_t -> int -> int -> unit
val set_evidence : 'a array -> circuit -> 'a array array -> unit

(** {6 Other evidence related operations} *)

val example_to_ev : schema_t -> example_t -> evidence_t

(** [ev_intersect e1 e2] returns [true] when evidence [e1] and [e2] are not mutually exclusive. *)
val ev_intersect : evidence_t -> evidence_t -> bool

(** [ev_subset e1 e2] returns [true] if evidence [e1] is subset of evidence [e2] (i.e. pr(e1|e2) = 1.0 ) *)
val ev_subset : evidence_t -> evidence_t -> bool

val ev_intersect2 : evidence_t -> evidence_t -> bool

(** [ev_union circuit ev1 ev2] creates another evidence [e] such that [e] is the union of [ev1] and [ev2]. [ev_union] suppose that [ev1] and [ev2] are not mutualy exclusive. *)
val ev_union : circuit -> evidence_t -> evidence_t -> evidence_t

(** Print evidence data structure to stdout. *)
val print_ev : evidence_t -> unit


(** {5 Arithmetic circuits for Markov networks} *)

(** [condition] represents a variable value combination in features.
    For example, [(true,5,1)] means that variable 5 has state 1, and
    [(false, 5, 1)] means that variable 5 does not have state 1. *)
type condition = bool * int * int

(** Represents feature and its weight in the coresponding log-linear
    model of the arithmetic circuit. *)
type feature = {
  acnode : node;  (** Node containing the feature weight *)
  mutable weight : float; (** The feature weight *)
  cond : condition list; (** The set of conditions that creates a feature, e.g. [+v3_1 +v4_2 +v6_0] *)
  ev : evidence_t;
}

(** [feature_node f] returns the acnode related to the feature [f]. *)
val feature_node : feature -> node

(** [feature_value f] returns the value of feature [f] when its condition 
    is satisfied. *)
val feature_value : feature -> float

(** [set_feature_value f wt] sets the value of feature [f] to [wt], 
    modifying the associated constant node. *)
val set_feature_value : feature -> float -> unit

(** [feature_contains_var f v] returns true if feature [f] has a condition of variable [v]. *)
val feature_contains_var : feature -> node -> bool

(** [c_satisfied x cond] returns true if [x] satisfies [cond]. *)
val c_satisfied : example_t -> condition -> bool

(** [f_satisfied x f] returns true if an instance [x] satisfies all 
    conditions of feature [f]. *)
val f_satisfied : example_t -> feature -> bool

(** Prunes features by removing empty, redundant, and contradictory
    circuit features. Returns shortened list of simplified features. *)
val prune_features : feature list -> example_t -> feature list

(** Prunes a circuit and its features, given evidence. 
    After conditioning on evidence, parameter nodes for remaining
    features will remain unchanged; other constant nodes may be
    merged, depending on circuit structure.  Returns shortened list of
    simplified features.  May leave the parent references for some
    nodes in an inconsistent state - use [update_parents] to fix. *)
val prune_for_evidence_with_features : 
      circuit -> feature list -> example_t -> feature list

(** Prunes a circuit conditioned on evidence. Constant nodes may be
    merged, depending on circuit structure. May leave the parent
    references for some nodes in an inconsistent state - use
    [update_parents] to fix. *)
val prune_for_evidence : circuit -> example_t -> unit

(** Converts a condition to evidence. *)
val conditions_to_ev : schema_t -> (condition) array -> evidence_t

(** Returns true when two given conditions are not mutually exclusive. *)
val cond_intersect : schema_t -> (condition) array -> (condition) array -> bool


(** {6 Read/Write features} *)

(** Converts the given feature to a string *)
val string_of_feature : feature -> string

(** [output_feature out f] writes feature [f] to the output channel [out]. *)
val output_feature : out_channel -> feature -> unit

(** Stores the given arithmetic circuit and feature list in [.ac] format. *)
val output_with_features : out_channel -> circuit -> feature list -> unit

(** Stores the given arithmetic circuit and feature list in [.ac] format,
    given the circuit schema, root node, and feature list. *)
val output_root_with_features : 
    out_channel -> schema_t -> node -> feature list -> unit


(** Stores the feature list given the circuit root. *)
val output_features :		
		  out_channel -> node -> feature list -> unit


(** Reads an arithmetic circuits and log-linear features from [.ac]
    file. Used for arithmetic circuts that represent log-linear models. *)
val load_with_features : in_channel -> circuit * feature list


(** {6 Circuit derivatives } *)
  
(** Data structure for computing the derivatives of circuits with
    respect to features. *)
type deriv_scratch = {
  dr : float array;  (** Derivative of root with respect to each node. *)
  vr : float array;  (** Log value of each node. *)
  vr_nz : float array; 
  bit1 : bool array;
  bit2 : bool array;
  cids : int array array;
}

(** Initializes the data structure for computing the derivatives.
    Reusing this data structure is much more efficient than creating
    it for every single query. *)
val create_deriv_scratch : circuit -> deriv_scratch

(** Computes derivatives of an arithmetic circuit with respect to all
    features conditioned on evidence. Updates the deriv_scratch
    data structure in place. *)
val get_derivatives_raw : circuit -> deriv_scratch -> evidence_t -> unit



(** {5 Inference using arithmetic circuits} *)

(** Helper data structure for circuit inference.  Caches node values
    and child ids for each node. Reusing it over multiple queries is
    much faster than recreating it each time. *)
type scratch_t = float array * int array array

(** Create helper data structure. *)
val create_scratch : circuit -> scratch_t

(** Maps {!circuit.nodes} to their logvalues after the evaluation of
    the [circuit]. *)
val logvalues : scratch_t -> circuit -> float array


(** [logprob_ev scratch circ ev] returns the log probability of
    evidence structure [ev] in circuit [circ]. For arithmetic circuits that
    represent Bayesian networks the log probability is normalized, but for
    circuits representing Markov networks it is unnormalized.  [scratch]
    can be constructed using {!create_scratch}. *)
val logprob_ev : scratch_t -> circuit -> evidence_t -> float

(** [logprob_x scratch circ x] returns the log probability of 
    example [x] in circuit [circ].  For arithmetic circuits that
    represent Bayesian networks the log probability is normalized, but for
    circuits representing Markov networks it is unnormalized.  [scratch]
    can be created using {!create_scratch}. *)
val logprob_x : scratch_t -> circuit -> example_t -> float

(** [get_logmarginals circ ds ev] returns the log-marginals of all
    variables in circuit [circ] given evidence [ev]. 
    [ds] can be created using {!create_deriv_scratch}. *)
val get_logmarginals : circuit -> deriv_scratch -> example_t -> float array array

(** [get_marginals circ ds ev] return the marginals of all variables
    in circuit [circ] given evidence [ev].
    [ds] can be created using {!create_deriv_scratch}. *)
val get_marginals : circuit -> deriv_scratch -> example_t -> float array array

(** Returns the log partition function. *)
val compute_z : scratch_t -> circuit -> float


val compute_z_e : circuit -> evidence_t -> float

(** [get_mpe circ ds e] computes the MPE state of variables given
    the circuit [circ], its empty derivative data structure
    [ds], and the evidence [ev]. *)
val get_mpe : circuit -> deriv_scratch -> example_t -> int array


(** {5 Map and Set modules for Node (based on Hashtbl)} *)

module NSet :
  sig
    type key = node
    type 'a t = 'a Node.NSet.t
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
    type 'a t = 'a Node.NMap.t
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

(** [node_iter f root] applies function [f] to every node in the
    circuit rooted at [root]. *)
val node_iter : (node -> 'a) -> node -> unit

(** [node_map f root] applies function [f] to every node in the
    circuit rooted at [root] and creates a list of the results. *)
val node_map : (node -> 'a) -> node -> 'a list

(** [root_to_list root] generates a list of all nodes reachable from
    [root]. *)
val root_to_list : node -> node list

(** [relatedl_a size rel nl] returns an array in which the [i]th
    element is true if the node with id [i] is reachable from some
    node in [nl] through relation [rel]. Useful for generating lists
    of ancestor or descendant nodes, represented as bit vectors. *)
val relatedl_a : int -> (node -> node list) -> node list -> bool array

(** [a_mem a n] returns the [i]th element of array [a], where 
    [i] is the id of node [n]. Similar to NMap.find but faster. *)
val a_mem : 'a array -> node -> 'a

