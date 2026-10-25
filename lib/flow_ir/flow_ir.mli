(** Specialized, renderer-independent dataflow. The reference evaluator remains
    in [flow]; downstream hosts own catalog cooking and display sinks. *)
module Operators = Operators
module Packed = Packed
module Core = Core
module Core_region = Core_region
module Core_fuse = Core_fuse
module Core_lower = Core_lower
module Core_exec = Core_exec
module Core_draw = Core_draw

val static :
  ?record:bool -> ?inputs:(string * (string * Flow.Eval.value) list) list ->
  Flow.Workspace.t -> (Flow.Eval.t, Flow.Diagnostic.t) result
(** {!Flow.Eval.static} with the same result, in which a static packed [map] of more than
    4,096 elements runs as its CPU kernel after the walker has made the first 4,096 elements
    ({!Flow.Eval.Private.static_with_kernels}, [bulk]). The walker finishes the map itself when
    the kernel refuses the form, fails, or differs from the walked elements. {!qualify_workspace}
    evaluates the same way. *)

val qualify_workspace :
  ?record:bool -> ?inputs:(string * (string * Flow.Eval.value) list) list ->
  ?observe:(Flow.Workspace.path option -> Flow.Eval.residual -> unit) ->
  Flow.Workspace.t -> (Flow.Workspace.t * Flow.Eval.t, Flow.Diagnostic.t) result
(** Evaluate once with actual captures and qualify immutable checker candidates
    through production packed compilation and the shared GPU form checks. Any
    observed refusal dominates successes across instances/tuples. Unobserved or
    ambiguous producers stay explicitly pending. Every call rebuilds conclusions;
    no residual/program proofs are retained and live forcing never requalifies.
    The optional audit callback receives the associated authored producer, or
    None for an ambiguous/missing association, and the actual observed handle.
    For image/map, successful adaptation supplies its synthetic pixel map with
    the original captures, authored producer and runtime instance/tuple. *)

type id = Flow.Workspace.path * int list
(** The path includes a graph-instance namespace; provenance retains the authored
    path, so overrides have distinct cardinality origins without moving cards. *)

module Count : sig
  type t = Static of int | Data of id | Unknown
  (** [Data origin] ties a runtime count to its source. Distinct sources and
      unknown counts cannot be fused merely because both are dynamic. *)
end

type rate = Static | Frame | Event
type precision = Exact | Approx
type tier = Interp | Closure | Cpu_kernel | Gpu | Gpu_compile | Gpu_readback | Cooked
module Cost : sig
  type tier_cost = {fixed : float; per_element : float}
  val estimate : tier -> count:int -> float
  val cheapest : legal:tier list -> count:int -> tier
  val packed : count:int -> tier
  (** The same measured CPU decision used at placement and force time. *)

end
module Gpu : sig
  type value={identity:int;count:int;width:int;stamp:int64;gpu_seconds:float option}
  (** Borrowed output metadata; [gpu_seconds] is the completed device dispatch
      duration when the backend supports timestamps. *)

  type kernel={run:Packed.Private.inputs -> (value,Flow.Diagnostic.t)result;
    readback:value -> (Flow.Eval.value,Flow.Diagnostic.t)result}
  type backend={cost:Packed.t -> count:int -> float option;
    prepare:Packed.t -> (kernel,Flow.Diagnostic.t)result}
  type policy=Measured | Qualification
  val policy : unit -> policy
  val with_backend : ?policy:policy -> backend -> (unit -> 'a) -> 'a
  (** Initial-domain host scope. [None] cost means unmeasured and keeps CPU placement.
      Qualification explicitly forces the native test path; it is not production calibration. *)

  val set_service : backend option -> unit
  (** Publish the backend that cook workers may use through {!pump}, or withdraw it. Only a host that
      calls {!pump} on the initial domain may publish one: a worker waits until the request is served. *)

  val pump : unit -> unit
  (** Serve the device requests the cook workers have queued. Initial domain only. *)

  val available : unit -> backend option
  (** The backend of this domain's scope, else the published service, reached through the mailbox
      from a worker. *)
end
type source =
  | Constant of Flow.Eval.value
  | Frame_field of string
  | Input of string
  | State_previous of Flow.Eval.residual
type body =
  | Operation of string
  | Vector
  | Field of string
  | List_value
  | Record_value of string list
  | Struct_value of string * Flow.Ty.t * string list
  | Reference of Flow.Eval.residual
  | Packed_map of Packed.t
  | Readback
type kernel = { body : body; elementwise : bool; requires_exact : bool }
type sink = Display of string | Export | Sop_input | State_seed | Cache_key
(* A mark set is the draw operator ([draw/circles], [draw/rects], [draw/lines],
   [draw/points]) and its attributes. The node's [id] and [provenance] are the drawing
   call's path; an attribute's [edge] names the argument that carries it, so every pass
   carries it. [output] picks one of the several outputs of a fused region node (else 0). *)
type role = Position | Target | Size | Fill | Stroke | Width
type attribute = { role : role; edge : string; output : int }
type marks = { set : string; attributes : attribute list }
type kind =
  | Source of source
  | Kernel of kernel
  | Opaque of string * Flow.Check.kernel_facts option
  | Sink of sink
  | Marks of marks
type edge = { name : string; node : int }
type node = {
  id : id;
  ty : Flow.Ty.t;
  count : Count.t;
  rate : rate;
  precision : precision;
  kind : kind;
  args : edge list;
  scope : int list;
  invariant : bool;
  provenance : id list;
  tier : tier;
}
type t = { nodes : node array; roots : int array; groups : int array array }

type execution = { owner : id; sites : (int * Flow.Workspace.path * int list) list;
  tier : tier; seconds : float }
module Profile : sig
  type t
  val create : clock:(unit -> float) -> t
  val executions : t -> execution list
  (** Immutable snapshot of at most 512 recent groups. Each packed group has
      one timing, excluding input materialization and nested groups. *)
end

val of_evaluation : Flow.Workspace.t -> Flow.Check.catalog -> Flow.Eval.t -> t
(** Build from the specialized plan, residual captures, graph results and folds. *)

val attribute_edges : t -> node -> (attribute * int) list
(** The attributes of a [Marks] node with the node each reads, in attribute order; its
    [rate] is that attribute's rate. Empty for any other node. *)

val share : t -> t
val hoist : t -> t
val fuse_attributes : t -> t
(** Fuse the region attributes of each mark set that agree on count or origin, rate, scope
    and precision into one region node with an output each ([attribute.output]). Declines
    at the register limit; never fails. *)

val fuse : t -> t
val prune : t -> t
type target = { legal : node -> tier -> bool; cost : tier -> count:int -> float }
(* What a back end can run. [legal node tier] says whether the node may be placed on the tier
   (the CPU tiers are normally always legal; [Gpu] needs a region it can emit, or a mark set it
   can draw; [Gpu_readback] is the copy of a GPU value back to the CPU). [cost] estimates seconds. *)
val native : target
(** Metal: a region needs a static count of at least 1,024 and no GPU refusal; readback is
    legal; [cost] is {!Cost.estimate}. [place] without a target is exactly this. *)

val place : ?approx:Flow.Workspace.Paths.t ->
  ?gpu_cost:(Packed.t -> count:int -> float option) -> ?target:target -> t -> (t, Flow.Diagnostic.t) result
(** A [Marks] node is placed like a kernel: [Gpu] when at least one attribute is a GPU region
    and every other attribute is a GPU region or a constant, [Cpu_kernel] when an attribute is
    a CPU region, else [Interp]. A target whose [legal _ Gpu_readback] is false never places a
    region on the GPU if its value would have to come back: it feeds an exact read, or a mark
    set that is not wholly on the GPU. *)

val optimize : ?target:target -> t -> (t, Flow.Diagnostic.t) result
(** Each pass retains authored provenance; precision legality precedes placement. *)

module Executor : sig
  type program
  val compile : ?profile:Profile.t -> ?strict:bool -> ?approx:Flow.Workspace.Paths.t -> ?sink:sink ->
    ?count_source:Packed.count_source -> Flow.Eval.value -> (program, Flow.Diagnostic.t) result
  val graph : program -> t

  val packed_root : program -> Packed.t option
  (** Packed root, through transparent [exact] readback boundaries. *)

  val force : ?state:Flow.Eval.state -> ?elems:(string * Flow.Eval.value) list ->
    ?resolve:(Flow.Eval.value -> (Flow.Eval.value, Flow.Diagnostic.t) result) ->
    ?reference:bool -> ?strict:bool -> program -> live:Flow.Eval.live ->
    (Flow.Eval.value, Flow.Diagnostic.t) result
  (** Selected/probed cones use [reference:true]. Unsupported terms keep the
      reference tree walker; supported scalar dataflow uses its operator records.
      Failures rerun the reference for its precise diagnostic. [strict:true]
      requires packed numeric bodies at compilation and execution, without
      cost-based interpreter placement or error fallback. [reference:true]
      retains the explicit reference oracle. *)

  val force_into : ?state:Flow.Eval.state -> ?elems:(string * Flow.Eval.value) list ->
    ?resolve:(Flow.Eval.value -> (Flow.Eval.value, Flow.Diagnostic.t) result) ->
    ?strict:bool ->
    column_source:(Flow.Eval.residual -> Flow.Workspace.term ->
      (Packed.column array option, Flow.Diagnostic.t) result) ->
    count:int -> outputs:float array option array array -> program -> live:Flow.Eval.live ->
    (unit option, Flow.Diagnostic.t) result
  (** Execute a collecting packed root into disjoint owned component planes.
      [None] leaves a scalar or unsupported root on its ordinary execution path.
      Borrowed inputs remain immutable; omitted output planes are still checked. *)

  type displayed=Cpu of Flow.Eval.value | Gpu of Gpu.value
  val try_display : ?state:Flow.Eval.state -> ?resolve:(Flow.Eval.value -> (Flow.Eval.value,Flow.Diagnostic.t)result) ->
    ?reference:bool -> ?policy:Gpu.policy -> program -> live:Frame_input.t ->
    (displayed option,Flow.Diagnostic.t)result
  (** Select a GPU producer without executing the CPU fallback. [None] leaves
      ordinary worker cooking intact; explicit readbacks return owned CPU data. *)

  val region_display : ?profile:Profile.t -> ?approx:Flow.Workspace.Paths.t -> ?policy:Gpu.policy -> unit ->
    ?state:Flow.Eval.state -> ?resolve:(Flow.Eval.value -> (Flow.Eval.value,Flow.Diagnostic.t)result) ->
    Packed.t -> live:Frame_input.t -> (Gpu.value option,Flow.Diagnostic.t)result
  (** [region_display ()] is the placement a draw program's region gets as a mark attribute: the
      GPU output when the current backend takes the region (its site is in [approx], it has no
      GPU refusal, the static or run-time count is at least 1,024 and the cost model or
      [Qualification] says so), else [None] for the CPU. It keeps the kernels of the last 64
      regions it saw. *)

  val force_display : ?state:Flow.Eval.state -> ?reference:bool -> ?policy:Gpu.policy -> program -> live:Frame_input.t ->
    (displayed,Flow.Diagnostic.t)result

  type marks
  val compile_marks : ?profile:Profile.t -> ?approx:Flow.Workspace.Paths.t ->
    ?count_source:Packed.count_source -> ?id:id -> string -> (string * Flow.Eval.value) list ->
    (marks, Flow.Diagnostic.t) result
  (** A mark set ([draw/circles], [rects], [lines], [points]) from its operator inputs by name.
      The IR is optimized as a whole: attribute regions fuse and the set is placed
      ([marks_graph]). *)

  val marks_graph : marks -> t
  val force_marks : ?state:Flow.Eval.state -> marks -> live:Frame_input.t ->
    ((string * Flow.Eval.value) list, Flow.Diagnostic.t) result
  (** Every attribute's value by name. An attribute whose rate is [Static] runs once and its
      value is kept; a fused region runs once per frame for all its outputs; the others run
      every call. *)
end
