type t

(** A compute pipeline for one entry point of a shared library, specialized
    with function constants; [interface] is checked exactly against Metal
    reflection. The caller owns it (no cache). *)
val create_compute_from_library : Device.t -> Library.t -> entry:string ->
  constants:(string * Ogpu_core.Shader.constant_value) list ->
  interface:Ogpu_core.Shader.binding list -> (t,Ogpu_core.Error.t) result
val create_render_owned : ?indirect:bool -> ?primitive_topology:Metal.Render_pipeline.primitive_topology -> ?blend:Ogpu_core.Pipeline.blend -> Device.t -> Ogpu_core.Pipeline.render_descriptor -> (t,Ogpu_core.Error.t) result
val destroy : t -> (unit,Ogpu_core.Error.t) result

module Private : sig
  type native = Compute of Metal.Compute_pipeline.t | Render of Metal.Render_pipeline.t
  val native : t -> native
  val argument_encoder : t -> buffer_index:int64 -> (Metal.Shader_argument_encoder.t,Ogpu_core.Error.t) result
  val retain_submission : t -> (unit,Ogpu_core.Error.t) result
  val release_submission : t -> unit
end
