type t
val create : unit -> t
val config : ?fast:bool -> t -> Flow_ir.Gpu.config
val with_backend : ?fast:bool -> t -> (Flow_ir.Gpu.config -> 'a) -> 'a
val pump : t -> unit
(** Serve this workspace's queued GPU requests on the initial domain. *)
val resident : Flow_ir.Gpu.config -> t -> Flow_ir.Core_exec.resident option
(* Inside [with_backend ~fast:true], a resident fold for one drawing on this workspace's host. *)
val qualification : t -> unit
val image : t -> key:string -> width:int -> height:int ->
  publish:(Flow_gpu.Image_sink.output -> ('a,Diagnostic.t) result) ->
  Flow_ir.Gpu.value -> ('a,Diagnostic.t) result
val marks : t -> Marks_scene.gpu
(* The [Marks]-family sources of the GPU mark sets of a frame, one per slot (at most 64), each
   owning the uploads of its CPU attributes and borrowing its GPU ones. *)

val close : t -> unit
val image_stats : t -> Flow_gpu.Host.stats * int * int * int * int
(** Host totals, sinks created/closed and sink buffer/texture creations, including
    failed uncommitted sinks and totals retained after close. *)
module Private : sig
  val install_test_backend : t -> Flow_ir.Gpu.backend -> unit
  val on_owner_for_test : t -> (unit -> 'a) -> 'a
end
