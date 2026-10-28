(** Immutable procedural DAG nodes. Shared node values represent shared graph
    subgraphs. *)

type cook_mode =
  | Generator
  | Duplicate_input of int
  | In_place of int
  | Instance_input of int
  | Passthrough of int
  | Generic

type elementwise = Points | Primitives | None
type topology = Preserved | Changed
type facts = {
  cook_mode : cook_mode;
  elementwise : elementwise;
  reads : string list;
  writes : string list;
  topology : topology;
  exact : bool;
}
(** Kernel legality and component dependencies. ["P"] names canonical positions;
    other names select all matching attribute owners. ["*"] means opaque access
    to the whole input/output. Topology and groups remain cache dependencies.
    Unannotated nodes are opaque, topology-changing and exact. *)

type t

val facts : t -> facts

val id : t -> int
val label : t -> string
val operation : t -> string
val version : t -> int
val parameters : t -> string
(** The hand-written cache text of an unparameterized node, or, for a
    parameterized node that has none, [Param.cook_text] of its schema
    values ([name=value;...]). *)

(** [Param.cook_key] of the attached schema values, computed once when
    the node is parameterized, or [""] for an unparameterized node. Session
    cache identity includes it next to intrinsic operator parameters, so every
    cook-impact schema field participates in the key. Generated inspection text
    does not add a second copy to that identity. *)
val parameter_key : t -> string
val dependencies : t -> Context.Dependencies.t
val inputs : t -> t list
val trace : t -> Diagnostic.trace

(** Concrete, type-erased inspector fields owned by this node instance. Empty
    when the node exposes no parameter fields. *)
val parameter_fields : t -> Param.field_view list
val has_parameters : t -> bool

val relabel : string -> t -> t
(** The same node (id, parameters, cook identity) under a new display label;
    a blank label is ignored. *)

(** Attach a typed immutable parameter record and its pure reconstruction
    function to a node. This is the public extension point for custom SOPs.
    Standard catalog SOPs use the same mechanism. The reconstruction receives
    the stable node label and current graph inputs. *)
val parameterize :
  schema:'parameters Param.schema ->
  values:'parameters ->
  rebuild:(label:string -> inputs:t list -> 'parameters -> (t, string) result) ->
  t ->
  (t, string) result

(** Apply inspector writes to this node only. Cook-affecting changes rebuild
    its operator closure while preserving the logical node id; view/export
    changes update metadata without invalidating cooked geometry. Constructor
    validation failures return [Error] and leave the existing node intact. *)
val apply_parameters :
  t ->
  (string * Param.value) list ->
  (t * Param.effects, string) result

module Private : sig
  val fresh_id : unit -> int

  val with_facts : facts -> t -> t
  (** Refine the conservative declaration without changing the cook mode. *)

  val restore_id : int -> t -> (t, string) result
  (** Restore a saved logical id and reserve it in the shared allocator. *)

  type input_policy = All | Only of int
  type cooked = {
    payload : Payload.t;
    instances : Rays_math.Mat4.t array option;
  }
  type geometry_cooked = {
    geometry : Rdk.Geometry.t;
    instances : Rays_math.Mat4.t array option;
  }

  val make :
    ?label:string ->
    operation:string ->
    version:int ->
    parameters:string ->
    cook_mode:cook_mode ->
    dependencies:Context.Dependencies.t ->
    ?input_policy:input_policy ->
    ?expand:(Context.t -> t array -> Payload.t array -> (t array, Diagnostic.t) result) ->
    inputs:t array ->
    (node_id:int -> Context.t -> Payload.t array ->
     (cooked, Diagnostic.t) result) ->
    t

  val make_geometry :
    ?label:string -> operation:string -> version:int -> parameters:string ->
    cook_mode:cook_mode -> dependencies:Context.Dependencies.t ->
    ?input_policy:input_policy ->
    ?expand:(Context.t -> t array -> Rdk.Geometry.t array -> (t array, Diagnostic.t) result) ->
    inputs:t array ->
    (node_id:int -> Context.t -> Rdk.Geometry.t array ->
      (geometry_cooked, Diagnostic.t) result) -> t
  (** Geometry SOP adapter. An Image or Kernel input returns [E_PAYLOAD] before calling
      either the expansion or cook callback. *)

  val parameterize_with_key :
    ?parameter_key:string -> schema:'parameters Param.schema -> values:'parameters ->
    rebuild:(label:string -> inputs:t list -> 'parameters -> (t, string) result) ->
    t -> (t, string) result
  (** Attach typed parameter values while supplying their exact opaque key.
      Use when a stable binary key avoids serializing an already-typed payload. *)

  val cache_parameters : t -> string
  (** Intrinsic operator identity before schema display text is attached. *)
  val cache_facts : t -> string
  (** Process-local binary declaration identity, encoded once at construction. *)
  val input_policy : t -> input_policy
  val input_array : t -> t array
  (* Rebuild a parameterized node against new inputs so input-dependent
     schemas (for example Switch choices) stay current. The logical id is
     retained. Unparameterized nodes only swap their inputs. *)
  val rebuild_with_inputs : t -> t array -> t
  (* Clone a node with a fresh logical id. Parameter values and operator
     metadata are preserved and input-dependent schemas are rebuilt. *)
  (* Retain a document node's logical identity and label on a freshly rebuilt
     operator whose physical input list may differ because optional slots were
     connected or disconnected. *)
  val adopt_identity : source:t -> t -> t
  val cook :
    t -> Context.t -> Payload.t array ->
    (cooked, Diagnostic.t) result

  val expand :
    t -> (Context.t -> t array -> Payload.t array -> (t array, Diagnostic.t) result) option
  (** A zone node: given its current input nodes and their cooked outputs it returns the roots of the
      sub-graphs to cook, one per element; the session cooks them (through its
      cache) and passes their outputs, in order, to [cook] in place of the
      inputs. *)
end

type info = {
  id : int;
  label : string;
  operation : string;
  version : int;
  parameters : string;
  facts : facts;
  dependencies : Context.Dependencies.t;
  input_ids : int list;
  has_parameters : bool;
}

(** Deterministic input-before-consumer order. Shared nodes appear once.
    [Session.inspect] caches repeated queries when a session owns the work. *)
val inspect : t -> info list
val find : t -> node_id:int -> t option

(** Union of context facts consumed anywhere in the reachable DAG. Sketch
    runtimes use this to schedule external-effect recooks. *)
val graph_dependencies : t -> Context.Dependencies.t
