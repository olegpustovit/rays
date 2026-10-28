type t
type output
type scene = {accel:Ogpu.Backend.accel; vertices:Ogpu.Backend.buffer; spheres:Ogpu.Backend.buffer; sphere_count:int;
  instances:Ogpu.Backend.buffer option}
(** [scene/trace] bindings: the structure, its triangle vertices (instanced: all prototypes' concatenated), the
    procedural (centre, radius) float4s, and for an instanced scene the 16-float-per-instance table
    [base primitive; world-to-object 3x4 row-major; 3 spare] (a moving one: 32 floats, [base primitive; start
    object-to-world 3x4; end object-to-world 3x4; 7 spare]). *)
type extras = {vertices_end:Ogpu.Backend.buffer option; strands:Ogpu.Backend.buffer option; strand_count:int;
  curves:Ogpu.Backend.accel option}
(** Bindings of a moving or stranded scene, after the instance table: a flat moving scene's shutter-end
    triangle vertices, then the strand segments (two float4 each: end a, radius; end b, radius) and, for
    native curves, their curve structure. *)
val no_extras : extras
val create : Renderer.gpu -> Pipelines.t -> Emit.msl -> t
val dispatch : ?wait:bool -> ?reset_status:bool -> ?commands:Ogpu.Backend.commands ->
  ?count:(Ogpu.Backend.buffer * int * int) -> ?into:(Ogpu.Backend.buffer * Ogpu.Backend.buffer) -> ?bound:(int -> Ogpu.Backend.buffer option) ->
  ?bound_table:(int -> Ogpu.Backend.buffer option) ->
  ?scene:scene -> ?extras:extras -> t -> Flow_ir.Packed.Private.inputs ->
  (output, Diagnostic.t) result
(** [~wait:false] leaves the dispatch in flight, without its status read; [~commands] only encodes it
    into the caller's command buffer (epoch 0 until the caller commits it); [~reset_status:false]
    keeps ORed error flags from earlier encodings on this runner. [~count:(cell, mul, div)]
    bounds a fast kernel's count by [cell * mul / div] on the device; [~into:(output, cell)] writes a
    one-output fast kernel's output and count into the caller's buffers; [bound i] and
    [bound_table u] supply device buffers already holding input [i] or uniform [u]'s table.
    Input arrays are borrowed immutable storage. Replace an array when its
    contents change; mutating an uploaded array is unsupported. Each input
    slot retains only its last successfully uploaded array and covered length,
    until buffer replacement or close. Frame/uniform/status writes remain live. *)

val select_output : index:int -> output -> output option
(** Select a fused producer's output; an invalid index returns [None]. The
    selected view shares the producer's lifetime and dispatch generation. *)

(** [index] overrides the selected output (initially the first). *)
val readback : ?index:int -> output -> (Flow.Eval.value, Diagnostic.t) result
val buffer : ?index:int -> output -> Ogpu.Backend.buffer option
val count_record : output -> Ogpu.Backend.buffer option
(* A fast kernel's 24-byte count record {n, 6n, 1, 0, 0, 0} (see {!Emit.msl}); [None] for an exact
   kernel or a superseded output. *)
val count : output -> int
val epoch : output -> int64
val width : output -> int
val gpu_seconds : output -> float option
val max_total_threads : t -> (int, Diagnostic.t) result
(* The kernel pipeline's most threads per threadgroup: Metal lowers it when the kernel needs more registers. *)
val close : t -> unit
val compact : Ogpu.Backend.commands -> Pipelines.t -> mask:Ogpu.Backend.buffer -> count:int ->
  count_in:Ogpu.Backend.buffer option -> kept:Ogpu.Backend.buffer -> total:Ogpu.Backend.buffer ->
  (unit, Diagnostic.t) result
(* Encodes into [commands] the indices of [mask]'s first elements that are nonzero, as floats into
   [kept], and their count into the cell [total]. The mask's count is [count], or at most the uint
   in [count_in]. Nothing is committed or read. *)
val read_count : Ogpu.Backend.queue -> Ogpu.Backend.buffer -> epoch:int64 -> (int, Diagnostic.t) result
val read_floats : Ogpu.Backend.queue -> Ogpu.Backend.buffer -> epoch:int64 -> length:int ->
  (float array, Diagnostic.t) result
(* [length] floats of a buffer, once the queue has completed [epoch]. *)
type instance = {first:int; triangles:int; transform:float array; transform_end:float array option}
(** An instance of the [triangles] triangles from [first] (in {!description}'s [triangles]), with its row-major
    3x4 object-to-world transform and, for instance motion, its shutter-end transform. *)
type description = {triangles:bytes; triangles_end:bytes option; instances:instance array; sphere_data:bytes;
  strand_data:bytes}
(** A scene for {!build_scene}: packed float3 triangle corners (nine float32 per triangle, in primitive order;
    instanced, every prototype's), the same corners at the shutter's end (vertex motion; flat scenes only),
    the instances ([[||]]: a flat scene), float4 (centre, radius) spheres, and strand segments, eight float32
    each (end a, radius, end b, radius; world space, both radii equal). *)
type built = {scene:scene; extras:extras; instanced:bool; motion:bool; curves:[`Loop | `Native] option;
  release:unit -> unit}
(** [instanced], [motion] and [curves] are the static facts the scene's kernel is emitted with ({!Emit.kernel}):
    strands are native curves where [caps] has [Ray_tracing_curves], else an in-loop capsule test. *)
val build_scene : ?caps:Ogpu.Caps.t -> Renderer.gpu -> description -> (built, Diagnostic.t) result
(** Builds and waits for the scene's structures. Motion builds motion structures (keyframes at times 0 and 1,
    clamped) when [caps] (default: the device's) has [Ray_tracing_motion]; otherwise the scene is built at
    the shutter midpoint and [motion] is false. [release] destroys every structure and buffer. *)
module Private : sig
  val diagnostic : Ogpu.Error.t -> Diagnostic.t
  val buffer_creations : t -> int
  val input_uploads : t -> int
  val input_uploaded_bytes : t -> int
  val status_reads : t -> int
  val check_status : t -> epoch:int64 -> (unit, Diagnostic.t) result
  val readback_bytes : t -> int
  val create_owned : Ogpu.Backend.device -> Ogpu.Backend.queue -> Pipelines.t -> Emit.msl -> t
end
