(** The evaluator of the workspace language: values outside geometry.

    Numbers, vectors, text, lists, records, functions, loops, [if]/[cond]/[case],
    higher-order forms, [str], list operations, [ref] with overrides and
    graph inputs run here, sequentially, on IEEE doubles, with a step budget.
    Packed operations budget each 1,024-element block separately; native
    storage bounds limit their counts, while expensive bodies remain bounded.
    Geometry is not built: a catalog call (and the workspace operator
    [sop/curve]) becomes a plan node, lowered into one network per graph
    instance.

    {2 The geometry plan}

    A node is keyed by [(site, iter)]:
    - [site] is the lexical path of the call: the enclosing binding, result,
      zone or [fn] path (as in [Workspace.path]) extended by the route down
      through argument names ([#n] when a name repeats, as in [sop/merge]),
      list/record/[if]/[cond] positions.  A [defn] body is prefixed by the
      site of its call, so every call site of a [defn] is a distinct subtree;
    - [iter] is the iteration tuple: the running index of each enclosing zone
      (outermost first, counted over the whole product) and
      of each enclosing [fn] call (a closure counts its own calls; a [defn] or
      operator passed to [map] counts one per element).
    Two nodes of one graph instance never share a key.

    [args] holds the fully evaluated arguments in written order.  A geometry
    input is [Deferred (Ty.geometry, id)] (an earlier node, possibly in another instance) or
    [No_geo] (for [nil]); the inputs of [sop/merge] are spliced (lists
    flattened, [No_geo] dropped) and all named [input].  Numbers keep their
    dynamic [Int]/[Float] kind: lowering coerces them to the catalog
    parameter type.  A catalog kind used as a function value ([(map
    sop/box xs)]) has positional arguments named [$0], [$1], ...; kinds are
    named as written.  Scene, world, settings and panel values ([scene/object],
    [ui/split], ...) are [Struct] values, not nodes: their arguments are
    checked here ([ui/tile] holds 1-16 panels, split axis and ratio, settings
    ranges) and geometry inside them is a [Deferred (Geometry, id)] reference.

    {2 Split evaluation}

    {!static} evaluates every term that does not depend on frame facts or a fold. A term that
    does becomes a {!residual}: the term with the environment it was
    evaluated in, so every static capture is folded in.  A plan node whose
    argument depends on the frame carries a [Residual] there (a [Vec3] with a live
    component is one residual for the whole vector); {!residual_eval} and
    {!force} evaluate it for a complete frame without touching the plan, and never
    create geometry.  Structure cannot depend on [t] (the checker rejects
    it), so one static pass serves every frame.  {!run} is the convenience
    that does both for a given time.

    [residual] environments hold only lexically free bindings, including names
    captured by nested function and zone bodies. The checked-term walk is
    memoized with a bounded weak cache. *)

type ('f, 'r) payload = ('f, 'r) Value.t =
  | Int of int
  | Float of float
  | Bool of bool
  | Text of string
  | Vec2 of float * float
  | Vec3 of float * float * float
  | Vec4 of float * float * float * float
  | List of ('f, 'r) payload array
  | Int_array of int array
  | Text_array of string array
  | Float_array of float array
  | Vec2_array of float array
  | Vec3_array of float array
  | Vec4_array of float array
  | Record of (string * ('f, 'r) payload) list  (** fields in written order *)
  | Deferred of Ty.t * int  (** a plan node *)
  | No_geo  (** [nil] *)
  | Struct of string * Ty.t * (string * ('f, 'r) payload) list
  | Source of string * Ty.t * (string * ('f, 'r) payload) list
      (** a scene, world, settings or panel value: the head, its resolved type and its
          arguments; [scene/merge] and [ui/tile] splice their lists *)
  | Fn of 'f
  | Residual of 'r

type value = (fn, residual) payload
and fn
and residual
and state
and node = {
  id : int;  (** index in [plan.nodes] *)
  inst : int;  (** index in [plan.instances] *)
  site : Workspace.path;
  iter : int list;
  kind : string;  (** catalog kind or deferred operator, including [sop/curve] and [draw/*] *)
  ty : Ty.t;  (** the declared result type of the deferred node *)
  args : (string * value) list;
}

module Residual : sig
  (** The compiler-facing residual boundary: inspect immutable checked terms,
      bind captures and state to an execution, and invoke the reference walker
      only as a leaf when compiled execution declines or fails. *)
  type _ Effect.t +=
    | Observe_kernel : residual -> unit Effect.t
    | Kernel_value : residual -> value option Effect.t
    | Bulk_static : residual -> value option Effect.t
    | Bulk_approx : residual -> value option Effect.t

  val handle : ?observe:(residual -> unit) -> ?kernel:(residual -> value option) ->
    ?bulk:(residual -> value option) ->
    ?bulk_approx:(residual -> value option) -> (unit -> 'a) -> 'a
  (** Handle evaluator yield points for residual observation and large static
      maps. The handler resumes the evaluator with a decision; no dispatcher is
      stored in evaluator state. *)

  val free_names : Workspace.term -> string list
  (** Lexically free binding names, including nested function captures. *)
  val with_bindings : residual -> term:Workspace.term -> (string * value) list -> residual
  val bind_live : ?state:state -> ?elems:(string * value) list ->
    ?resolve:(value -> (value, Diagnostic.t) result) -> residual -> live:Input.t ->
    (unit, Diagnostic.t) result

  val function_bindings : fn -> (string * value) list
  val compiled_function : residual -> instance:int -> prefix:Workspace.path ->
    zone:Workspace.path -> params:(Workspace.pattern * Ty.t option) list ->
    body:Workspace.term -> bindings:(string * value) list -> fn
  val function_id : fn -> int
  val function_body : fn -> ((Workspace.pattern * Ty.t option) list * Workspace.term) option
  val function_origin : fn -> (int * Workspace.path * Workspace.path) option
  val function_name : fn -> string option

  val residual_definition : residual -> string -> Workspace.graph option
  val residual_graph : residual -> string -> Workspace.graph option
  val graph_instance : residual -> string -> int option
  type residual_view = {
    term : Workspace.term;
    bindings : (string * value) list;
    site : Workspace.path;
    owner : Workspace.path;
    iter : int list;
    instance : int;
    prefix : Workspace.path;
    previous : bool;
    host : bool;
    geometry : bool;
  }
  val residual_view : residual -> residual_view
  val residual_id : residual -> int
  val checkpoint : residual -> (unit -> unit)
  val rerun : ?on_walk:(unit -> unit) -> residual -> (value, Diagnostic.t) result
  (** Run the reference walker for this residual only, without offering it to
      the compiled dispatcher again. *)
  val state_read : residual -> instance:int -> path:Workspace.path -> iter:int list -> host:bool ->
    init:(unit -> value) -> value
  val state_peek : residual -> instance:int -> path:Workspace.path -> iter:int list -> host:bool -> value option
  val state_write : residual -> instance:int -> path:Workspace.path -> iter:int list -> host:bool -> value -> value
  val residual_ops : residual -> Op.t list
  val eval_term : ?state:state -> ?elems:(string * value) list ->
    ?resolve:(value -> (value, Diagnostic.t) result) -> ?on_walk:(unit -> unit) ->
    ?kernel:(residual -> value option) -> residual -> Workspace.term ->
    live:Input.t -> (value, Diagnostic.t) result
end

type live = Input.t

type instance = {
  graph : string;
  default : bool;  (** the graph evaluated with its own defaults: the one an editor shows, and the one that records *)
  inputs : (string * value) list;  (** every input, defaults filled in *)
  result : value;
}
(** One evaluation of a graph: the top-level graphs with their inputs, and one
    per distinct [(ref g :k v)] override tuple (cached by value, L14). *)

type plan = { instances : instance array; nodes : node array }

type t = {
  plan : plan;
  authored : int array;
      (** Originating syntax form ID by plan node ID, shared by copies and
          independent of the evaluation route. IDs belong to this checked
          source and can change on reparse; they are not plan identities.
          Zero denotes a synthetic node or a kind invoked as a function value. *)
  results : (string * value) list;  (** each graph, evaluated with its default (or given) inputs *)
  states : value list;  (** frame folds to advance, including unused bindings *)
  records : (Workspace.path * (int list * value) list) list;
      (** with [~record:true]: the values seen at each path, per iteration tuple,
          in evaluation order, at most 4,096 per path (bindings, results,
          zone variables [":x"], [fn] parameters, graph inputs).  Geometry is a
          [Deferred (Geometry, id)] reference (its counts come from the cook, {!Flow_graph.Probe}); a
          live value is a [Residual] until forced ({!run}, {!Flow_graph.Probe}).
          Packed local maps retain [fn-path/~calls] templates for reference
          inspection beyond the recording cap. *)
}

val static :
  ?record:bool -> ?inputs:(string * (string * value) list) list -> Workspace.t ->
  (t, Diagnostic.t) result
(** Evaluate every graph once.  [inputs] overrides graph inputs by graph name
    (the OCaml-visible interface).  The first error stops the
    run; its code is one of [E_ITER_BOUND] (a driven [range], [linspace],
    [concat] or zone over 4,096), [E_EVAL_BUDGET], [E_LIST_RANGE]
    ([first]/[last]/[nth]), [E_PATTERN] (a destructuring pattern longer than a
    driven list), [E_NONFINITE], [E_RANGE] ([ui/tile], [ui/split],
    [settings/config] with computed arguments), [E_DEPTH], [E_LIVE_GEOMETRY]. *)

val create_state : unit -> state
val reset_state : ?host_state:bool -> state -> unit
(** Reset on reload or a new export. [host_state=false] resets animation
    playback while retaining host interaction state. *)

val fork_state : state -> state
(** Copy the fold snapshot for pure worker reads; data is shared immutably. *)

val state_stamp : state -> string
(** Opaque cache epoch for a fold snapshot. Forks retain the epoch; writes and
    resets get a fresh one, while rollback restores the prior epoch. *)

val transaction : state -> (unit -> ('a, Diagnostic.t) result) -> ('a, Diagnostic.t) result
(** Keep the previous fold snapshot when a group of evaluations fails. *)

val residual_eval : residual -> live:live -> (value, Diagnostic.t) result
(** A live term's value at a time (never a [Residual] at the top). *)

val force : ?state:state -> ?elems:(string * value) list ->
  value -> live:live -> (value, Diagnostic.t) result
(** Every residual inside a value replaced by its value at the time.  [elems] binds
    the element of each geometry zone ({!element_key}) to its value (a point
    is a [Vec3]); a residual that reads an unbound element is an error.
    Private forcing with [resolve] materializes host-owned [Source] values and
    packed [Struct] sources at the cook boundary. Without a resolver these
    report [E_DATA_SOURCE]. Resolved values must have their declared type and
    valid storage. *)

val element_key : Workspace.path -> string
(** The name under which {!force} binds the element of the zone at this path. *)

val is_live : value -> bool
(** A residual anywhere inside. *)

val frame_dependent : value -> bool
(** Reads a frame fact; element-only residuals return false. *)

val state_dependent : value -> bool
(** Reads an environment-owned frame fold. *)

val run :
  ?record:bool -> ?inputs:(string * (string * value) list) list -> ?state:state -> ?live:live -> time:float -> Workspace.t ->
  (t, Diagnostic.t) result
(** {!static} then {!force} of every argument, input, result and record at
    [time]: no [Residual] is left. *)

val hash : float list -> float
(** [value/rand]: a pure hash. Each key [x] becomes
    [k = int32(floor(x * 1000003))]; from [h = 0x9e3779b9] each key does
    [h = imul(h xor k, 0x85ebca6b); h = h xor (h >>> 13); h = imul(h, 0xc2b2ae35);
    h = h xor (h >>> 16)] (32-bit unsigned, implemented by
    {!Rays_math.Hash.float_sequence32}), and the result is
    [(h mod 1000000) / 1000000], greater than or equal to zero and less than one. *)

val show : value -> string
(** [str] formatting; a residual shows as [?]. *)

module Private : sig
  val static_with_requests :
    ?budget:int ->
    ?record:bool -> ?inputs:(string * (string * value) list) list ->
    Workspace.t -> (t, Diagnostic.t) result
  (** Run the static evaluator with residual requests unhandled. Callers that
      need kernel observation or bulk map specialization must install a
      {!Residual.handle}; ordinary {!static} handles requests with the reference
      walker. [budget] overrides the ordinary AST-step guard. *)

  val map_function : ?site:(int * Workspace.path * int list) -> ?path:Workspace.path ->
    signature:Ty.fn_signature -> fn -> value list -> (value, Diagnostic.t) result
  (** Bind immutable packed columns to an instantiated function, retaining its
      body and captures as a genuine residual map for downstream compilation.
      [site] identifies the runtime instance; [path] identifies its authored producer. *)

  val free_name_walks : unit -> int
  (** Number of memo misses, for the checked-term reuse regression. *)

  val map_function_bindings : (value -> value) -> fn -> fn
  (** Replace immutable closure captures, preserving callable identity and scope. *)

  val remap_function_geometry : sources:(int * int) list -> fn -> fn
  (** Remap geometry IDs through closure and residual captures when a host moves
      a callable between plans. Function identity and authored scope stay intact. *)

  val state_values : state -> value list
  (** Values retained by a forked snapshot, for host payload accounting. *)

  val function_scope : fn -> (Workspace.path * int list) option
  (** Authored zone and enclosing iteration tuple of an instantiated local
      function, for a host supplying selected bulk-call probes. *)

  val force_reference : ?state:state -> ?elems:(string * value) list ->
    ?resolve:(value -> (value, Diagnostic.t) result) -> value -> live:live ->
    (value, Diagnostic.t) result
  (** Force through the tree walker, including captured residuals, with no executor.
      Suitable for probes and parity checks. *)

  val force_reference_observed : ?state:state -> ?elems:(string * value) list ->
    ?resolve:(value -> (value, Diagnostic.t) result) -> on_walk:(unit -> unit) -> value -> live:live ->
    (value, Diagnostic.t) result

  val map_probe : ?state:state -> ?resolve:(value -> (value, Diagnostic.t) result) ->
    ?offset:int -> residual -> live:live ->
    (int * (int -> ((Workspace.path * (int list * value) list) list, Diagnostic.t) result), Diagnostic.t) result
  (** Materialize map inputs once, optionally through [execute], then reference-record only
      the selected function call. This admits indices beyond the preview cap;
      [offset] preserves call numbering when a function serves several maps. *)
end
