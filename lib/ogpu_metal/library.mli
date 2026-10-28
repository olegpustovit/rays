type t
(** A compiled Metal library shared by many pipelines. Runtime MSL and precompiled metallib
    artifacts both load here; pipelines are then created per entry point with function constants. *)

val create : Device.t -> Ogpu_core.Shader.t -> (t, Ogpu_core.Error.t) result

val compile_native :
  string -> Device.t -> Ogpu_core.Shader.t -> (Metal.Library.t, Ogpu_core.Error.t) result

val validate : Device.t -> t -> (unit, Ogpu_core.Error.t) result
val destroy : t -> (unit, Ogpu_core.Error.t) result

module Private : sig
  val metal : t -> Metal.Library.t
  val attach_pipeline : t -> unit
  val detach_pipeline : t -> unit
end
