type t

val create : clock:(unit -> float) -> Renderer.gpu -> t
(** Owns at most 64 runners and 64 pipelines. An absent cost keeps production CPU placement until
    native measurements supply a model. The caller owns the GPU lease. *)

val backend : t -> Flow_ir.Gpu.backend

val output : t -> Flow_ir.Gpu.value -> (Run.output option, Diagnostic.t) result
(** Borrow the current output after completion and status validation. Closed, evicted or superseded
    generations return [Ok None]; kernel status errors remain typed. A successful status is read
    once per dispatch, so later borrows do not add status readbacks. *)

val resident : fast:bool -> t -> Flow_ir.Core_exec.resident
(** A drawing's resident fold on this host, in fast precision only: keeps a loop's region arrays on
    the device and reads them back as the loop ends. One per drawing. *)

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
  materialisations : int;  (** kept outputs a lazy store read ({!Flow_gpu.Run.keep}) *)
  held_buffers : int;  (** buffers kept outputs hold now *)
  freed : int;  (** kept outputs whose owner freed them unread, never handed out *)
}

val zero : stats

val stats : t -> stats
(** Cumulative successful operations, including evicted runners and closed owners. Inspect on the
    creating domain, outside measured frame work. *)
