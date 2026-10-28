module Packed = Packed
(** Specialized, renderer-independent dataflow. The reference evaluator remains in [flow];
    downstream hosts own catalog cooking and display sinks. *)

module Core = Core
module Core_region = Core_region
module Core_fuse = Core_fuse
module Core_lower = Core_lower
module Core_exec = Core_exec
module Core_draw = Core_draw
module Kernel_layout = Kernel_layout

module Gpu : sig
  type value = {
    identity : int;
    count : int;
    width : int;
    output : int;
    stamp : int64;
    gpu_seconds : float option;
  }
  (** Borrowed output metadata; [output] selects a fused producer's buffer. [gpu_seconds] is the
      completed device dispatch duration when the backend supports timestamps. *)

  type planes = { count : int; width : int; materialise : unit -> float array array }
  (** An output kept where it was computed: [materialise] (once, from any domain, never failing: the
      dispatch's completion and status were checked when it was kept) reads it as [width] owned
      planes of [count] floats, for a planar store. Until then the device owns it. *)

  type kernel = {
    run : Packed.Host.inputs -> (value, Diagnostic.t) result;
    readback : value -> (Flow.Eval.value, Diagnostic.t) result;
    planes : value -> (planes, Diagnostic.t) result;
        (** Keeps the output for a lazy planar store: nothing is read until it materialises. *)
  }

  val planes_of :
    (value -> (Flow.Eval.value, Diagnostic.t) result) -> value -> (planes, Diagnostic.t) result
  (** [planes] from a [readback], deinterleaving its array. *)

  type backend = {
    cost : fast:bool -> Packed.t -> count:int -> float option;
    prepare : fast:bool -> Packed.t -> (kernel, Diagnostic.t) result;
  }

  type policy = Measured | Qualification
  type config = { backend : backend option; fast : bool; policy : policy }

  val default : config
  (** Exact CPU execution with no device backend. *)
end

val static :
  ?record:bool ->
  ?inputs:(string * (string * Flow.Eval.value) list) list ->
  Flow.Workspace.t ->
  (Flow.Eval.t, Diagnostic.t) result
(** {!Flow.Eval.static} with the same result, in which a static packed [map] of more than 4,096
    elements runs as its CPU kernel after the walker has made the first 4,096 elements
    ({!Flow.Eval.Residual}, [Bulk_static]). The evaluator resumes after the scoped handler supplies
    a value or declines. The walker finishes the map itself when the kernel refuses the form, fails,
    or differs from the walked elements. {!qualify_workspace} evaluates the same way. *)

val qualify_workspace :
  ?record:bool ->
  ?inputs:(string * (string * Flow.Eval.value) list) list ->
  ?observe:(Flow.Workspace.path option -> Flow.Eval.residual -> unit) ->
  ?gpu:Gpu.config ->
  Flow.Workspace.t ->
  (Flow.Workspace.t * Flow.Eval.t, Diagnostic.t) result
(** Evaluate once with actual captures and qualify immutable checker candidates through production
    packed compilation and the shared GPU form checks. Any observed refusal dominates successes
    across instances/tuples. Unobserved or ambiguous producers stay explicitly pending. Every call
    rebuilds conclusions; no residual/program proofs are retained and live forcing never
    requalifies. The optional audit callback receives the associated authored producer, or None for
    an ambiguous/missing association, and the actual observed handle. For image/map, successful
    adaptation supplies its synthetic pixel map with the original captures, authored producer and
    runtime instance/tuple. *)

type id = Flow.Workspace.path * int list
(** A profile owner: authored path and iteration. *)

type tier = Interp | Core | Cpu_kernel | Gpu | Gpu_compile | Gpu_readback

module Cost : sig
  type tier_cost = { fixed : float; per_element : float }

  val estimate : tier -> count:int -> float
  val cheapest : legal:tier list -> count:int -> tier

  val packed : count:int -> tier
  (** The same measured CPU decision used at placement and force time. *)

  val sop_device : Gpu.policy -> Core.Region.t -> count:int -> bool
  (** A fused SOP write's region goes to the device: [Qualification], or [count] of at least 2^18
      elements and noise in its code. *)
end

type sink = Display of string | Sop_input

type execution = {
  owner : id;
  sites : (int * Flow.Workspace.path * int list) list;
  tier : tier;
  seconds : float;
}

module Profile : sig
  type t

  val create : clock:(unit -> float) -> t

  val executions : t -> execution list
  (** Immutable snapshot of at most 512 recent groups. Each packed group has one timing, excluding
      input materialization and nested groups. *)
end

module Executor : sig
  type program

  val compile :
    ?trace:((Packed.t, Diagnostic.t list) result -> unit) ->
    ?profile:Profile.t ->
    ?strict:bool ->
    ?approx:Flow.Workspace.Paths.t ->
    ?sink:sink ->
    ?count_source:Packed.count_source ->
    Flow.Eval.value ->
    (program, Diagnostic.t) result

  val packed_region : program -> Packed.t option
  (** The root's packed region, when the compiled value is one. *)

  val root_is_packed_or_readback : program -> bool
  (** Whether the root directly runs a packed region or its exact readback. *)

  val fresh_csr_fields : program -> string list
  (** Record fields whose arrays are freshly allocated by an elementwise packed root. *)

  val packed_root : program -> Packed.t option
  (** Packed root, through transparent [exact] readback boundaries. *)

  val force :
    ?state:Flow.Eval.state ->
    ?elems:(string * Flow.Eval.value) list ->
    ?resolve:(Flow.Eval.value -> (Flow.Eval.value, Diagnostic.t) result) ->
    ?reference:bool ->
    ?strict:bool ->
    program ->
    live:Flow.Eval.live ->
    (Flow.Eval.value, Diagnostic.t) result
  (** Selected/probed cones use [reference:true]. Unsupported terms keep the reference tree walker;
      supported scalar dataflow uses its operator records. Failures rerun the reference for its
      precise diagnostic. [strict:true] requires packed numeric bodies at compilation and execution,
      without cost-based interpreter placement or error fallback. [reference:true] retains the
      explicit reference oracle. *)

  (** Narrow test counters for lowering and reference-walker attempts in one force operation. *)
  module Private : sig
    val force_with_lower_attempts :
      ?state:Flow.Eval.state ->
      ?elems:(string * Flow.Eval.value) list ->
      ?resolve:(Flow.Eval.value -> (Flow.Eval.value, Diagnostic.t) result) ->
      ?reference:bool ->
      ?strict:bool ->
      program ->
      live:Flow.Eval.live ->
      (Flow.Eval.value, Diagnostic.t) result * int

    val force_with_walker_counts :
      ?state:Flow.Eval.state ->
      ?elems:(string * Flow.Eval.value) list ->
      ?resolve:(Flow.Eval.value -> (Flow.Eval.value, Diagnostic.t) result) ->
      ?reference:bool ->
      ?strict:bool ->
      program ->
      live:Flow.Eval.live ->
      (Flow.Eval.value, Diagnostic.t) result * int * int
  end

  val record_on_device :
    ?state:Flow.Eval.state ->
    ?resolve:(Flow.Eval.value -> (Flow.Eval.value, Diagnostic.t) result) ->
    ?gpu:Gpu.config ->
    program ->
    live:Flow.Eval.live ->
    ((string * Gpu.planes) list option, Diagnostic.t) result
  (** The elementwise fields of a record program (a fused SOP write's) as one region on the device,
      in fast precision when {!Cost.sop_device} places it: one dispatch, each output kept on the
      device as its planes (a field of width [w]: [w] arrays of the count), by field name, read only
      when they materialise. [None] keeps the record on the CPU, also when the device fails. An
      input array the device already holds (the same array as the last dispatch's) is not uploaded
      again. *)

  val force_into :
    ?state:Flow.Eval.state ->
    ?elems:(string * Flow.Eval.value) list ->
    ?resolve:(Flow.Eval.value -> (Flow.Eval.value, Diagnostic.t) result) ->
    ?gpu:Gpu.config ->
    ?strict:bool ->
    column_source:
      (Flow.Eval.residual ->
      Flow.Workspace.term ->
      (Packed.column array option, Diagnostic.t) result) ->
    count:int ->
    outputs:float array option array array ->
    program ->
    live:Flow.Eval.live ->
    (unit option, Diagnostic.t) result
  (** Execute a collecting packed root into disjoint owned component planes. [None] leaves a scalar
      or unsupported root on its ordinary execution path. Borrowed inputs remain immutable; omitted
      output planes are still checked. *)

  type displayed = Cpu of Flow.Eval.value | Gpu of Gpu.value

  val try_display :
    ?state:Flow.Eval.state ->
    ?resolve:(Flow.Eval.value -> (Flow.Eval.value, Diagnostic.t) result) ->
    ?reference:bool ->
    ?gpu:Gpu.config ->
    program ->
    live:Input.t ->
    (displayed option, Diagnostic.t) result
  (** Select a GPU producer without executing the CPU fallback. [None] leaves ordinary worker
      cooking intact; explicit readbacks return owned CPU data. *)

  val region_display :
    ?gpu:Gpu.config ->
    ?profile:Profile.t ->
    ?approx:Flow.Workspace.Paths.t ->
    unit ->
    ?state:Flow.Eval.state ->
    ?resolve:(Flow.Eval.value -> (Flow.Eval.value, Diagnostic.t) result) ->
    Packed.t ->
    live:Input.t ->
    (Gpu.value option, Diagnostic.t) result
  (** [region_display ()] is the placement a draw program's region gets as a mark attribute: the GPU
      output when the current backend takes the region (its site is in [approx], it has no GPU
      refusal, the static or run-time count is at least 1,024 and the cost model or [Qualification]
      says so), else [None] for the CPU. It keeps the kernels of the last 64 regions it saw. *)

  val region_readback :
    ?gpu:Gpu.config ->
    unit ->
    Packed.t ->
    live:Input.t ->
    (Flow.Eval.value option, Diagnostic.t) result
  (** In fast precision a region of 4,096 elements or more that a drawing computes with (not a mark
      attribute) runs on the current backend and is read back as a 32-bit array; [None] leaves it to
      the CPU. Kernels are kept by the region's form, at most 16. *)

  val force_display :
    ?state:Flow.Eval.state ->
    ?reference:bool ->
    ?gpu:Gpu.config ->
    program ->
    live:Input.t ->
    (displayed, Diagnostic.t) result
end
