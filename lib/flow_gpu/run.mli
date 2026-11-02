type t
type output

type pool
(** Kept outputs' buffers that runners share, recycled by size class (256 MiB free at most). Each
    holder tells the GC its buffer's size, so a dead one is collected and its buffer returns without
    a read; past a quarter of the device's working set of retired holders still unread, the oldest is
    read into owned planes and its buffer reused. *)

val pool : unit -> pool

val close_pool : pool -> unit

val recycled :
  pool -> device:Ogpu.Backend.device -> int -> (Ogpu.Backend.buffer * Ogpu.Backend.floats * int) option
(** A free buffer of at least that many bytes and at most twice it, after giving back what collected
    holders released (and asking for a collection past the retired budget). *)

val created : pool -> int -> unit
(** Tell the pool's pacing of bytes made outside it. *)
(** On the initial domain, after its runners are closed: destroys what it still holds. *)

type scene = {
  accel : Ogpu.Backend.accel;
  vertices : Ogpu.Backend.buffer;
  spheres : Ogpu.Backend.buffer;
  sphere_count : int;
  instances : Ogpu.Backend.buffer option;
}
(** [scene/trace] bindings: the structure, its triangle vertices (instanced: all prototypes'
    concatenated), the procedural (centre, radius) float4s, and for an instanced scene the
    16-float-per-instance table [base primitive; world-to-object 3x4 row-major; 3 spare] (a moving
    one: 32 floats, [base primitive; start object-to-world 3x4; end object-to-world 3x4; 7 spare]).
*)

type extras = {
  vertices_end : Ogpu.Backend.buffer option;
  strands : Ogpu.Backend.buffer option;
  strand_count : int;
  curves : Ogpu.Backend.accel option;
}
(** Bindings of a moving or stranded scene, after the instance table: a flat moving scene's
    shutter-end triangle vertices, then the strand segments (two float4 each: end a, radius; end b,
    radius) and, for native curves, their curve structure. *)

val no_extras : extras

val create : ?pool:pool -> Renderer.gpu -> Pipelines.t -> Emit.msl -> t
(** [pool] (default: its own, closed with it) is where its kept outputs' buffers come from and go.
*)

val dispatch :
  ?wait:bool ->
  ?reset_status:bool ->
  ?uint32_cpu:bool ->
  ?commands:Ogpu.Backend.commands ->
  ?encoder:Ogpu.Backend.compute_encoder ->
  ?count:Ogpu.Backend.buffer * int * int ->
  ?into:Ogpu.Backend.buffer * Ogpu.Backend.buffer ->
  ?bound:(int -> Ogpu.Backend.buffer option) ->
  ?bound_table:(int -> (Ogpu.Backend.buffer * int) option) ->
  ?scene:scene ->
  ?extras:extras ->
  ?resident:Ogpu.Backend.accel list ->
  t ->
  Flow_ir.Packed.Host.inputs ->
  (output, Diagnostic.t) result
(** [~wait:false] leaves the dispatch in flight, without its status read; [~commands] only encodes
    it into the caller's command buffer (epoch 0 until the caller commits it), into its open compute
    [encoder] when given (the caller ends it; a serial encoder orders the dispatches);
    [~reset_status:false] keeps ORed error flags from earlier encodings on this runner.
    [~count:(cell, mul, div)] bounds a fast kernel's count by [cell * mul / div] on the device, and
    its grid too: an indirect dispatch of groups of 256 to that count, written on the device
    (Kernels.dispatch_arguments), not to the capacity [inputs.count]; [~into:(output, cell)] writes
    a one-output fast kernel's output and count into the caller's buffers; [bound i] and
    [bound_table u] supply device buffers already holding input [i] or uniform [u]'s table (with the
    table's length in floats, which a gather clamps to); a bound input's array is not read.
    [resident] lists the bottom-level structures an instanced [scene] references, declared read so
    they stay resident. Input arrays are borrowed immutable storage. Replace an array when its
    contents change; mutating an uploaded array is unsupported. Each input slot retains only its
    last successfully uploaded array and covered length, until buffer replacement or close.
    Frame/uniform/status writes remain live. Emit's uint32 source/table buffers require explicit
    [bound]/[bound_table] callbacks; missing bindings refuse with [E_GPU_UINT32_BINDING] before
    upload. [uint32_cpu:true] instead validates and uploads ordinary host arrays as exact uint32
    words; implicit float32 holders still refuse. Selected uint32 scalar uniforms and frame words
    are likewise validated before upload. *)

val select_output : index:int -> output -> output option
(** Select a fused producer's output; an invalid index returns [None]. The selected view shares the
    producer's lifetime and dispatch generation. *)

val readback : ?index:int -> output -> (Flow.Eval.value, Diagnostic.t) result
(** [index] overrides the selected output (initially the first). Uint32 storage returns an exact
    [Int_array], including values above 2^24 and through 2^32-1. *)

val planes : ?index:int -> output -> (float array array, Diagnostic.t) result
(** The output as [width] owned planes of [count] doubles (component [c] of element [i] at
    [planes.(c).(i)]), converted once from the mapped float32 contents in parallel blocks, with the
    finite check folded in. *)

val keep : ?index:int -> output -> (Flow_ir.Gpu.planes, Diagnostic.t) result
(** Hand an output, possibly still in flight, to a lazy planar store without waiting. [settle] (on
    the initial domain) waits for the dispatch and reads its status once; [materialise] reads
    {!planes} once, from any domain after [settle] succeeded (on the initial domain it waits
    itself), with no queue work. [release ~held:true] retires it (its owner dropped a result it
    handed out): it stays readable, and its buffer comes back to the pool once it is read or
    collected, or when the pool reads it to bound memory. [release ~held:false] (never handed out,
    and not retired before) frees it: its buffer comes back at once and a later read raises. The
    runner takes a recycled or fresh buffer for its next dispatch; {!close} first materialises every
    holder still alive (unless [~discard:true], at a host's shutdown: nobody reads them, a later
    read raises). [resident] is the output's placeholder, an array of [width] zeros: a later
    {!dispatch} (of any runner) whose input is that array binds the held buffer instead, while it is
    held and unread. Uint32 outputs refuse with [E_GPU_UINT32_KEEP] until adopted by a typed
    topology holder; they cannot be borrowed as float32 buffers. *)

val buffer : ?index:int -> output -> Ogpu.Backend.buffer option

type uint32_holder = {
  count : int;
  materialise : unit -> int array;
  settle : unit -> (unit, Diagnostic.t) result;
  release : held:bool -> unit;
  pin : unit -> (Ogpu.Backend.buffer * int * (unit -> bool) * (unit -> unit)) option;
}

val keep_uint32 : ?index:int -> output -> (uint32_holder, Diagnostic.t) result
(** Transfers an unsigned output into an exact integer holder, using the same bounded pool and
    release lifecycle as [keep], without registering a float input binding. Settle on the creating
    domain before a worker reads. [materialise] caches exact integers and reads payload once. [pin]
    borrows an unread settled stream until unpin; it declines once materialised or freed. A runner
    close materialises live holders unless discard was requested. *)

val integers : uint32_holder -> Flow_ir.Gpu.integers
(** The same holder through the backend-neutral integer carrier, with shared ownership. *)

val pin_integers :
  Flow_ir.Gpu.integers -> (Ogpu.Backend.buffer * int * (unit -> bool) * (unit -> unit)) option
(** Borrow native UInt32 storage without materialising it. Other backend tokens decline. The native
    holder's creating-domain, settlement, view-count and lifetime rules apply. *)

val adopt_uint32 :
  t -> buffer:Ogpu.Backend.buffer -> count:int -> (uint32_holder, Diagnostic.t) result

val adopt_float :
  t ->
  buffer:Ogpu.Backend.buffer ->
  count:int ->
  width:int ->
  (Flow_ir.Gpu.planes, Diagnostic.t) result
(** Transfer a caller-owned, completed and status-validated buffer without copying or reading it.
    Success gives this runner exclusive ownership; failure leaves ownership with the caller. Buffers
    must be mapped, and float buffers must contain validated finite float32 values. The owned mapped
    view supports worker reads after invalidation; released buffers rejoin the pool. *)

val pin : float array -> (Ogpu.Backend.buffer * int * (unit -> bool) * (unit -> unit)) option
(** A display's borrow of the held buffer a settled output's placeholder ([keep]'s [resident])
    stands for, while it is held: the buffer (packed float32, [width] per element, from offset 0),
    its element count, [valid] (the buffer still exists: {!close} destroys it, read first) and
    [unpin]. Until [unpin] the pool neither reuses nor reads back the buffer, also after its holder
    is read or collected. [None] for an unsettled, freed or unknown output. Initial domain. *)

val count_record : output -> Ogpu.Backend.buffer option

(* A fast kernel's 24-byte count record {n, 6n, 1, 0, 0, 0} (see {!Emit.msl}); [None] for an exact
   kernel or a superseded output. *)
val count : output -> int
val epoch : output -> int64
val width : output -> int
val gpu_seconds : output -> float option
val max_total_threads : t -> (int, Diagnostic.t) result

(* The kernel pipeline's most threads per threadgroup: Metal lowers it when the kernel needs more registers. *)
val close : ?discard:bool -> t -> unit

val compact :
  ?uint32:bool ->
  ?encoder:Ogpu.Backend.compute_encoder ->
  Ogpu.Backend.commands ->
  Pipelines.t ->
  mask:Ogpu.Backend.buffer ->
  count:int ->
  count_in:Ogpu.Backend.buffer option ->
  kept:Ogpu.Backend.buffer ->
  total:Ogpu.Backend.buffer ->
  blocks:Ogpu.Backend.buffer ->
  groups:Ogpu.Backend.buffer ->
  (unit, Diagnostic.t) result

(* Encodes into [commands] the indices of [mask]'s first elements that are nonzero, as floats into
   [kept], and their count into the cell [total]. The mask's count is [count], or at most the uint
   in [count_in]; [blocks] is scratch of [compact_blocks count] bytes. With [count_in] the count and
   scatter passes take their group count from it on the device, through [groups] (12 bytes).
   Nothing is committed or read. [uint32] instead reads a uint32 mask and writes exact uint32
   indices; count cells and scratch have the same layout. *)

val compact_blocks : int -> int

val validate_topology :
  ?on_read:(int -> unit) ->
  Ogpu.Backend.queue ->
  Pipelines.t ->
  vertices:Ogpu.Backend.buffer * int ->
  offsets:Ogpu.Backend.buffer * int ->
  kinds:Ogpu.Backend.buffer * int ->
  status:Ogpu.Backend.buffer ->
  point_count:int ->
  max_polygon_size:int ->
  (unit, Diagnostic.t) result
(** One workgroup checks exact uint32 topology, reading only a 36-byte status record. Counts are
    actual element counts within buffer capacity; status needs 36 bytes. No input ownership
    transfers. Polygon sizes above [max_polygon_size] refuse. [on_read] records successful status
    bytes even when validation rejects the mesh. *)

val triangle_indices :
  ?on_read:(int -> unit) ->
  Ogpu.Backend.queue ->
  Pipelines.t ->
  positions:Ogpu.Backend.buffer * int ->
  vertices:Ogpu.Backend.buffer * int ->
  offsets:Ogpu.Backend.buffer * int ->
  kinds:Ogpu.Backend.buffer * int ->
  indices:Ogpu.Backend.buffer ->
  blocks:Ogpu.Backend.buffer ->
  status:Ogpu.Backend.buffer ->
  (int, Diagnostic.t) result
(** Generate UInt32 triangle indices for resident polygons with three/four corners, using the host's
    first-ear rule and positions. Returns the settled index count. Numerically uncertain/degenerate
    quads and other primitives refuse with [E_GPU_DISPLAY_TRIANGLES], so callers can take the host
    path. [blocks] needs one word per 256 primitives; [status] needs one word. Only four status
    bytes are read, reported by [on_read]; no input/output ownership transfers. *)

val fill_indices :
  ?value:int ->
  Ogpu.Backend.queue ->
  Pipelines.t ->
  Ogpu.Backend.buffer ->
  count:int ->
  (unit, Diagnostic.t) result
(** Writes [0..count-1], or the supplied constant [value], as exact uint32 words on the device and
    waits for completion. No size-dependent CPU staging. *)

val read_count :
  Ogpu.Backend.queue -> Ogpu.Backend.buffer -> epoch:int64 -> (int, Diagnostic.t) result

val read_floats :
  Ogpu.Backend.queue ->
  Ogpu.Backend.buffer ->
  epoch:int64 ->
  length:int ->
  (float array, Diagnostic.t) result

val read_uint32 :
  Ogpu.Backend.queue ->
  Ogpu.Backend.buffer ->
  epoch:int64 ->
  length:int ->
  (int array, Diagnostic.t) result

(* [read_floats]/[read_uint32] read [length] words after the queue completes [epoch]. *)
type instance = {
  first : int;
  triangles : int;
  transform : float array;
  transform_end : float array option;
}
(** An instance of the [triangles] triangles from [first] (in {!description}'s [triangles]), with
    its row-major 3x4 object-to-world transform and, for instance motion, its shutter-end transform.
*)

type description = {
  triangles : bytes;
  triangles_end : bytes option;
  instances : instance array;
  sphere_data : bytes;
  strand_data : bytes;
}
(** A scene for {!build_scene}: packed float3 triangle corners (nine float32 per triangle, in
    primitive order; instanced, every prototype's), the same corners at the shutter's end (vertex
    motion; flat scenes only), the instances ([[||]]: a flat scene), float4 (centre, radius)
    spheres, and strand segments, eight float32 each (end a, radius, end b, radius; world space,
    both radii equal). *)

type built = {
  scene : scene;
  extras : extras;
  instanced : bool;
  motion : bool;
  curves : [ `Loop | `Native ] option;
  release : unit -> unit;
}
(** [instanced], [motion] and [curves] are the static facts the scene's kernel is emitted with
    ({!Emit.kernel}): strands are native curves where [caps] has [Ray_tracing_curves], else an
    in-loop capsule test. *)

val build_scene : ?caps:Ogpu.Caps.t -> Renderer.gpu -> description -> (built, Diagnostic.t) result
(** Builds and waits for the scene's structures. Motion builds motion structures (keyframes at times
    0 and 1, clamped) when [caps] (default: the device's) has [Ray_tracing_motion]; otherwise the
    scene is built at the shutter midpoint and [motion] is false. [release] destroys every structure
    and buffer. *)

module Private : sig
  val diagnostic : Ogpu.Error.t -> Diagnostic.t
  val buffer_creations : t -> int
  val input_uploads : t -> int
  val input_uploaded_bytes : t -> int
  val status_reads : t -> int
  val check_status : t -> epoch:int64 -> (unit, Diagnostic.t) result
  val readback_bytes : t -> int
  val bounds_readback_bytes : t -> int

  val materialisations : t -> int
  (** Kept outputs read so far (from {!keep}'s thunks or at close). *)

  val held_buffers : t -> int
  (** Buffers kept outputs hold, after destroying those of collected holders. *)

  val create_owned :
    ?pool:pool -> Ogpu.Backend.device -> Ogpu.Backend.queue -> Pipelines.t -> Emit.msl -> t

  val pool_free_floats : pool -> Ogpu.Backend.floats list
  val pool_drain : pool -> unit
  val slot_shrink_after : int
  val pool_freed : pool -> int
  (** Holders freed unread ([release ~held:false]). *)

  val pool_held : pool -> int
  (** Buffers its kept outputs hold now, after recycling what collected or read ones gave back. *)
end

val free_budget : int
(** Idle bytes kept for reuse: the pool's free list, and a host's display storage. *)
