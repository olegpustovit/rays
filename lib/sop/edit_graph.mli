(** Immutable editable representation of a procedural network.

    [Node.t] remains the compiled, cookable DAG. This document can also hold disconnected input
    slots and nodes outside the current display path, then compile any valid node back to an
    ordinary [Node.t]. It owns topology only; editor positions, selection, menus, and cooking belong
    to UI layers. *)

type t

type input_requirement =
  | Required
  | Optional
  | Rest
  | Optional_rest
      (** [Rest] is only valid last: the slot repeats, so a node holds any number of inputs at least
          the slot count. The first rest input is required, the others optional; extras are named
          [name_2], [name_3], ... (see {!node_slot_names}). [Optional_rest] also repeats only at the
          end, but permits zero connected elements. Fixed optional slots keep their positions before
          it. [connect] one past the last input appends one. *)

type factory

type node_info = {
  node : Node.t;
  id : int;
  label : string;
  operation : string;
  version : int;
  parameters : string;
  facts : Node.facts;
  dependencies : Context.Dependencies.t;
  inputs : int option array;
  has_parameters : bool;
}

val of_graph : Node.t -> t

val empty : t
(** No nodes; build it up with [add_node] so every node keeps its factory. *)

val root : t -> int option
val set_root : int -> t -> (t, string) result
val inspect : t -> node_info list
val find : t -> node_id:int -> Node.t option
val inputs : t -> node_id:int -> int option array option

val node_slot_names : t -> node_id:int -> string list option
(** Input names retained by the entry's factory, or canonical [in0], [in1], … for a graph imported
    from code. *)

val node_factory_key : t -> node_id:int -> string option
val node_factory_fields : t -> node_id:int -> Param.field_view list

val compile : t -> (Node.t, string) result
(** Compile the document root, or a specific display node. Disconnected slots, missing references,
    and cycles are reported without changing the document. *)

val compile_node : t -> node_id:int -> (Node.t, string) result

type compiled
(** Every node of the document compiled once. Pass the previous result to reuse each node whose
    entry and compiled inputs are physically unchanged, so a parameter edit rebuilds only the edited
    node and its consumers. *)

val compile_all : ?previous:compiled -> t -> compiled
val compiled_node : compiled -> node_id:int -> (Node.t, string) result

val replace_node : Node.t -> t -> (t, string) result
(** Replace only the node payload. Its logical id and input arity must match. *)

(* Retarget a factory-backed node to an optional-input factory with the same
   operation, preserving its id and label. By default arity is unchanged and
   wiring stays at each index. [preserve_wires_by_name] follows slot names,
   allowing optional slots to be added or disconnected slots to be removed.
   Callers update path-keyed metadata on rename. *)
val apply_parameters :
  t -> node_id:int -> (string * Param.value) list -> (t * Param.effects, string) result

val add_node : ?inputs:int option array -> ?factory:factory -> Node.t -> t -> (t, string) result
(** Add a node. By default its existing node inputs are adopted when those ids already belong to the
    document; other slots start disconnected. *)

val remove_nodes : int list -> t -> t

(* Retain selected entries with their logical ids and internal connections;
   disconnect inputs from outside the selection. Used when moving a selection
   into a compound definition. *)

val connect : source:int -> consumer:int -> input_index:int -> t -> (t, string) result
val disconnect : consumer:int -> input_index:int -> t -> (t, string) result

val factory :
  ?operation:string ->
  ?slots:string list ->
  ?input_types:string list ->
  ?keyword_inputs:string list ->
  ?fields:Param.field_view list ->
  key:string ->
  label:string ->
  category:string list ->
  arity:int ->
  (Node.t list -> (Node.t, string) result) ->
  factory

val factory_slots :
  ?operation:string ->
  ?slots:string list ->
  ?input_types:string list ->
  ?keyword_inputs:string list ->
  ?fields:Param.field_view list ->
  key:string ->
  label:string ->
  category:string list ->
  inputs:input_requirement list ->
  (Node.t option list -> (Node.t, string) result) ->
  factory

val factory_key : factory -> string
val factory_operation : factory -> string
val factory_label : factory -> string
val factory_category : factory -> string list
val factory_fields : factory -> Param.field_view list

val factory_facts : factory -> Node.facts
(** Declaration at default parameters, computed once without cooking. Instance facts are rebuilt
    with the node's parameters and may select other names. *)

val factory_arity : factory -> int
val factory_inputs : factory -> input_requirement list

val factory_slot_names : factory -> string list
(** Named inputs, in slot order; defaults to [in0], [in1], … . *)

val factory_input_types : factory -> string list
(** Serialized input type names, defaulting to [geometry]. The Flow catalog validates them against
    the registered types. *)

val factory_keyword_inputs : factory -> string list
(** Required physical slots exposed as typed keyword parameters by Flow. Names must be distinct and
    must not conflict with scalar schema fields. *)

val instantiate : factory -> Node.t list -> (Node.t, string) result
val instantiate_optional : factory -> Node.t option list -> (Node.t, string) result
