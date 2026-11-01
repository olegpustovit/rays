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

  type planes = {
    count : int;
    width : int;
    materialise : unit -> float array array;
    resident : Flow.Eval.value option;
    settle : unit -> (unit, Diagnostic.t) result;
    release : held:bool -> unit;
    bounds : unit -> float array option;
  }
  (** An output kept where it was computed, possibly still in flight. [settle] (on the initial
      domain, once per dispatch; every output of one dispatch settles it) waits for the dispatch and
      checks its status: an [Error] means the values are not to be used. [materialise] (once, from
      any domain once settled, never failing) reads it as [width] owned planes of [count] floats,
      for a planar store. Until then the device owns it, and [resident] is a placeholder array (of
      one element) the same backend binds to that buffer when a later kernel's source is it, without
      reading it. [release ~held] says its owner dropped the result: the backend may reclaim the
      buffer. [held]: the owner handed it out, so it stays readable (read first if it is still
      reachable when the buffer is needed) and reads never fail; otherwise the buffer is reused at
      once and nothing may read it again. [bounds] (a vec3 output, settled and unread) reduces it
      where it is to its per-component minima then maxima, six floats, equal to a scan of its
      planes; [None] when the backend cannot (the reader then scans the planes). *)

  type kernel = {
    run : Packed.Host.inputs -> (value, Diagnostic.t) result;
    readback : value -> (Flow.Eval.value, Diagnostic.t) result;
    planes : value -> (planes, Diagnostic.t) result;
        (** Keeps the output for a lazy planar store: nothing is read until it materialises. *)
  }

  val planes_of :
    (value -> (Flow.Eval.value, Diagnostic.t) result) -> value -> (planes, Diagnostic.t) result
  (** [planes] from a [readback], deinterleaving its array. *)

  type integer_storage = ..
  (** Backend-owned integer storage token; no native handle crosses this boundary. *)

  type integers = {
    count : int;
    storage : integer_storage option;
    materialise : unit -> int array;
    release : held:bool -> unit;
  }

  type residency = {
    executor : Core_exec.resident;
    retain_uint32 : Flow.Eval.value -> integers option;
    retain_float : Flow.Eval.value -> planes option;
    validate_topology :
      point_count:int ->
      vertices:Flow.Eval.value ->
      offsets:Flow.Eval.value ->
      kinds:Flow.Eval.value ->
      max_polygon_size:int ->
      (unit, Diagnostic.t) result option;
  }
  (** A backend-neutral resident value scope. Validate topology before retaining its fields; retain
      final streams before closing the executor. [None] declines an unsupported stream. *)

  type backend = {
    residency : fast:bool -> residency option;
    cost : ?inputs:Packed.Host.inputs -> fast:bool -> Packed.t -> count:int -> float option;
        (** With prepared inputs, measure their actual layout, uniforms and frame values. Without
            them the backend may offer a shape estimate; callers recheck after preparation. *)
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
  ?replay:bool ->
  ?verify:bool ->
  Flow.Workspace.t ->
  (Flow.Eval.t, Diagnostic.t) result
(** Exact static kernels run before the walker for maps above 4,096 elements. Refusal or failure
    restarts reference evaluation from zero. [record] keeps eager checked heads; [replay] instead
    uses closure call selectors for body records (named-function recording keeps its head). [verify]
    compares every successful kernel element with the walker bit for bit and aborts on any
    difference. It is intended for qualification, not production evaluation. {!qualify_workspace}
    uses the same execution and recording contract. *)

val qualify_workspace :
  ?record:bool ->
  ?inputs:(string * (string * Flow.Eval.value) list) list ->
  ?observe:(Flow.Workspace.path option -> Flow.Eval.residual -> unit) ->
  ?replay:bool ->
  ?verify:bool ->
  ?defer:bool ->
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
    runtime instance/tuple. [defer:true] keeps qualifying large static maps in SOP graphs as
    residuals for their cook. Host graphs stay eager; the default is false for CLI checking and
    independent evaluation. *)

type id = Flow.Workspace.path * int list
(** A profile owner: authored path and iteration. *)

type tier = Interp | Core | Cpu_kernel | Gpu | Gpu_compile | Gpu_readback

type placement_reason =
  | Unsupported_form
  | Missing_consumer
  | Validation_read
  | Cpu_consumer
  | No_cost_evidence
  | Cpu_measured_cheaper
  | Device_measured_cheaper
  | Held_on_device

type placement = {
  owner : id;
  count : int option;
  reason : placement_reason;
  cpu_seconds : float option;
  device_seconds : float option;
  cpu_consumers : int option;
}

module Cost : sig
  type tier_cost = { fixed : float; per_element : float }

  val estimate : tier -> count:int -> float
  val cheapest : legal:tier list -> count:int -> tier

  val packed : count:int -> tier
  (** The same measured CPU decision used at placement and force time. *)

  val choose :
    cpu_seconds:float option ->
    device_seconds:float option ->
    resident:bool ->
    cpu_consumers:int ->
    placement_reason
  (** Compare total time to the known consumers, including transfers, validation and sinks. Missing,
      negative or nonfinite evidence keeps CPU. Ties keep CPU. *)

  val readback : count:int -> width:int -> float
  (** Measured seconds to move [count] elements of [width] floats between host and device. *)

  val on_device : placement_reason -> bool
  val reason_name : placement_reason -> string
end

type sink = Display of string | Sop_input

type execution = {
  owner : id;
  sites : (int * Flow.Workspace.path * int list) list;
  tier : tier;
  seconds : float;
}

module Profile : sig
  type static_map = {
    site : Flow.Workspace.path;
    count : int;
    tier : tier;
    deferred : bool;
    refusal : Diagnostic.t list;
    head_elements : int;
    tail_elements : int;
    seconds : float;
    head_seconds : float;
    kernel_seconds : float;
    tail_seconds : float;
  }

  type t

  val create : clock:(unit -> float) -> t

  val sample : t -> (unit -> 'a) -> 'a
  (** Scope static-map observations to this domain, restoring the previous scope on failure. Timings
      are inclusive of nested maps and use only the supplied clock. *)

  val static_maps : t -> static_map list
  (** At most 512 recent static maps, oldest first. Head/tail counts are actual walker calls; the
      compilation/force attempt and any refusal are measured separately. *)

  val executions : t -> execution list
  (** Immutable snapshot of at most 512 recent groups. Each packed group has one timing, excluding
      input materialization and nested groups. *)

  val place : t option -> placement -> unit
  (** Record one decision for its owner, replacing the owner's previous one; [None] uses the scoped
      profile of this domain. *)

  val placements : t -> placement list
  (** At most 512 recent placement decisions, newest first, one per owner. A declined program with
      unknown cardinality has [count = None], never a fabricated zero. Unknown downstream CPU-reader
      counts likewise remain [cpu_consumers = None]. *)
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
    ?resident:Core_exec.resident ->
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
      explicit reference oracle. [resident] offers compiled value regions to a device scope,
      including cached root programs. Successful placeholders must be retained or materialised
      before that scope closes; explicit reference evaluation does not use it. A failed resident
      attempt closes the scope and retries the whole value on CPU under the requested [strict]
      policy, discarding all partial placeholders rather than mixing fields from both attempts.
      Resident records, including frame-dependent fields, share a cached whole-value Core program so
      capacity replay reevaluates the complete producer/consumer chain. *)

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
    ?resident:(Flow.Eval.value -> (Flow.Eval.value * int) option) ->
    ?expected_count:int ->
    ?cpu_consumers:int ->
    ?cpu_penalty:float ->
    ?cpu_cost:(owner:string list * int list -> count:int -> float option) ->
    ?gpu:Gpu.config ->
    program ->
    live:Flow.Eval.live ->
    ((string * Gpu.planes) list option, Diagnostic.t) result
  (** The elementwise fields of a record program (a fused SOP write's) as one region on the device,
      in fast precision when the shared measured comparison places it: one dispatch, each output
      kept on the device as its planes (a field of width [w]: [w] arrays of the count), by field
      name, read only when they materialise. [None] keeps the record on the CPU, also when the
      device fails. [cpu_cost] prices the CPU route of this site from its measured cooks; [None] from
      it is {!No_cost_evidence} and the CPU; without it the route is priced from the cost table. An input array the device already holds (the same array as the last dispatch's)
      is not uploaded again. [resident v] names a source bound as the value [v] (an attribute read)
      that the device already holds: its placeholder ({!Gpu.planes}) and count, passed instead of
      evaluating it.

      Measured placement needs a known [cpu_consumers] count. Without it the record stays on CPU
      with [Missing_consumer]. CPU readers share one materialisation per output; resident input
      readback is charged to the CPU alternative. Zero promises a known resident consumer, not an
      unknown sink. Qualification bypasses cost. Partial output borrow failure releases earlier
      outputs immediately. With [expected_count], scalar uniforms and no resident-input resolver,
      compare before preparing inputs: a definite CPU refusal does no source evaluation. Missing
      early evidence permits preparation. A device candidate validates and compares actual inputs
      again, including their table layout and resident bindings. The expected
      count is the consuming writer's target size, not a substitute for input/count validation.
      Negative expected counts are rejected. [cpu_penalty] seconds are added to the CPU alternative: what
      its result costs downstream that the device one does not (a mesh kept whole on the device is
      displayed without being read back). *)

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
