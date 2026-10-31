type policy = Flow_ir.Gpu.config
(** The workspace owner's immutable execution policy. *)

val default_policy : policy

type t

val create : ?clock:(unit -> float) -> unit -> t
val config : ?fast:bool -> t -> Flow_ir.Gpu.config
val with_backend : ?fast:bool -> t -> (Flow_ir.Gpu.config -> 'a) -> 'a

val pump : ?follow_up:bool -> t -> unit
(** Serve this workspace's queued GPU requests on the initial domain. [follow_up] (default true)
    spins 0.5 ms after a request for the worker's next link of the chain; a caller that {!wait}s
    passes false. *)

val wait : t -> timeout:float -> unit
(** Sleep on the initial domain until a request is queued or [timeout] seconds pass. *)

val resident :
  Flow_ir.Gpu.config ->
  t ->
  (Flow_ir.Core_exec.resident
  * (Flow_ir.Packed.t -> live:Input.t -> Flow_ir.Gpu.value option option))
  option

(* Inside [with_backend ~fast:true], a resident fold for one drawing on this workspace's host, and
   its mark attributes over the arrays it keeps after its loop ({!Flow_gpu.Host.show}). *)
val qualification : t -> unit

val image :
  t ->
  key:string ->
  width:int ->
  height:int ->
  publish:(Flow_gpu.Image_sink.output -> ('a, Diagnostic.t) result) ->
  Flow_ir.Gpu.value ->
  ('a, Diagnostic.t) result

val marks : t -> Marks_scene.gpu
(* The [Marks]-family sources of the GPU mark sets of a frame, one per slot (at most 64), each
   owning the uploads of its CPU attributes and borrowing its GPU ones. *)

val display : t -> Rdk.Geometry.t -> (string * bool * (unit -> unit)) option
(** {!Rdk_rays.Surface.scene3}'s [device]: a point cloud's P (and N) this workspace's device holds,
    settled and unread, pinned in its pool and registered as the renderer's device positions (the
    key, whether N is bound, and what the mesh keeps). A triangle mesh without N binds the normals
    the host display would compute, made from P on the device ({!Flow_gpu.Host.normals}). It is
    released two {!frame}s after it is dropped: its key left out of [keep], or its mesh collected.
    [None] (draw the host copy) otherwise. Initial domain. *)

val frame : ?keep:string list -> t -> unit
(** One presented frame (initial domain): [keep] is every display key the frame's scenes still draw,
    when they changed; a bound display left out is dropped. Displays dropped two frames ago are
    unregistered and unpinned, so their buffers return to the pool. *)

val held_displays : t -> int
(** Displays bound or dropped and not yet released. *)

val displays : t -> int
(** Device meshes {!display} has bound. *)

val close : t -> unit

val image_stats : t -> Flow_gpu.Host.stats * int * int * int * int
(** Host totals, sinks created/closed and sink buffer/texture creations, including failed
    uncommitted sinks and totals retained after close. *)

module Private : sig
  val install_test_backend : t -> Flow_ir.Gpu.backend -> unit

  val on_owner_for_test : ?waits:bool -> t -> (unit -> 'a) -> 'a
  (** [waits]: the request blocks on the device, as a settle does. *)
end
