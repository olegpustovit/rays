type t

val create : clock:(unit -> float) -> Renderer.gpu -> t
(** Owns at most 64 runners and 64 pipelines. An absent cost keeps production CPU placement until
    native measurements supply a model. The caller owns the GPU lease. *)

val backend : t -> Flow_ir.Gpu.backend

val output : t -> Flow_ir.Gpu.value -> (Run.output option, Diagnostic.t) result
(** Borrow the current output after completion and status validation. Closed, evicted or superseded
    generations return [Ok None]; kernel status errors remain typed. A successful status is read
    once per dispatch, so later borrows do not add status readbacks. *)

val resident : ?uint32:bool -> fast:bool -> t -> Flow_ir.Core_exec.resident
(** A drawing's resident fold on this host, in fast precision only: keeps a loop's region arrays on
    the device and reads them back as the loop ends. One per drawing. Each region site's calls keep
    their buffers, placeholders and counts on the host between drawings (64 calls a site, 32 sites)
    and size its arrays by them; a count that outgrew one reruns the loop exactly. [uint32:true]
    uses exact unsigned words for integer arrays and captures, with device-generated integer ranges.
    Float and uint32 site caches are separate; float-only display borrows decline. Stateless value
    execution must materialise or adopt results before closing this scope. *)

type residency = {
  executor : Flow_ir.Core_exec.resident;
  retain_uint32 : Flow.Eval.value -> Run.uint32_holder option;
  retain_float : Flow.Eval.value -> Flow_ir.Gpu.planes option;
  uint32_stream : Flow.Eval.value -> (Ogpu.Backend.buffer * int) option;
}

val resident_scope : ?uint32:bool -> fast:bool -> t -> residency
(** A resident execution whose settled result streams can transfer to the host's holder pool. Retain
    each final stream before closing [executor]; aliases return the same holder. Retained
    placeholders remain readable executor inputs until scope close: a pin prevents the holder pool
    from recycling their buffers, and CPU reads use the holder's materialisation. The holders
    outlive this scope; host shutdown discards unread holders. *)

val validate_topology :
  t ->
  vertices:Ogpu.Backend.buffer * int ->
  offsets:Ogpu.Backend.buffer * int ->
  kinds:Ogpu.Backend.buffer * int ->
  point_count:int ->
  max_polygon_size:int ->
  (unit, Diagnostic.t) result
(** Validates exact uint32 streams without reading payload, with one cached status buffer.
    Successful status reads, including topology rejections, count as validation bytes. *)

val show : t -> Flow_ir.Packed.t -> live:Input.t -> Flow_ir.Gpu.value option option
(** A drawing's mark attribute over the arrays its resident fold kept after the loop
    ({!Flow_ir.Core_exec.resident}'s [settle]), dispatched over their buffers with the device count:
    its output, which {!output} resolves and which carries a count record. [None] when it reads no
    kept array (the ordinary display path), [Some None] when it reads one but cannot run on this
    host (the CPU, after the arrays are read back). *)

val normals :
  t ->
  positions:Ogpu.Backend.buffer ->
  count:int ->
  corners:int array ->
  (Ogpu.Backend.buffer * (unit -> unit)) option
(** A triangle mesh's display normals (packed float3: [Rays.Mesh.recalculate_normals] in float32)
    from the [count] packed float3 [positions] the device holds and the triangles' [corners],
    computed on the device and waited for (initial domain). The buffer is the caller's until the
    release. The corner tables are kept by the [corners] array's identity (8). [None] when the
    device cannot, or a point has more than 4,096 corners. *)

val triangle_indices :
  t ->
  positions:Ogpu.Backend.buffer * int ->
  vertices:Ogpu.Backend.buffer * int ->
  offsets:Ogpu.Backend.buffer * int ->
  kinds:Ogpu.Backend.buffer * int ->
  (Ogpu.Backend.buffer * int * (unit -> unit), Diagnostic.t) result
(** The host-rule triangle index buffer for resident triangle/quad streams, its settled count and
    idempotent release (creating domain). Borrows all inputs, reads only four validation bytes, and
    reuses the display storage pool (four free buffers per size, 64 sizes). Uncertain or unsupported
    polygons return [E_GPU_DISPLAY_TRIANGLES] for a caller's host fallback. Release the output
    before releasing the host's GPU lease. *)

val close : t -> unit
(** Close before releasing the caller's GPU lease. *)

type stats = {
  runners_created : int;
  runners_released : int;
  pipeline_compilations : int;
  pipeline_releases : int;
  buffer_creations : int;
  input_uploads : int;
  input_uploaded_bytes : int;
  status_reads : int;
  readback_bytes : int;
  payload_readback_bytes : int;
  status_readback_bytes : int;
  count_readback_bytes : int;
  validation_readback_bytes : int;
  bounds_readback_bytes : int;
      (** Successful readbacks by purpose, including the resident executor's direct reads.
          [readback_bytes] remains payload plus bounds; status, counts and validation are separate.
      *)
  materialisations : int;  (** kept outputs a lazy store read ({!Flow_gpu.Run.keep}) *)
  held_buffers : int;  (** buffers kept outputs hold now *)
  freed : int;  (** kept outputs whose owner freed them unread, never handed out *)
}

val zero : stats

val stats : t -> stats
(** Cumulative successful operations, including evicted runners and closed owners. Inspect on the
    creating domain, outside measured frame work. *)

val forget_producer : t -> int -> unit
(** Closes the runner of a prepared kernel, by its output's identity: its upload buffers and the
    inputs they were compared to go with it. A later run of that kernel makes a new one. *)
