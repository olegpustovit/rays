type color_format = Rgba8_unorm | Bgra8_unorm
type depth_format = No_depth | Depth32_float
type blend = Replace | Alpha | Add | Multiply | Screen | Subtract

(** [depth_format] states the depth attachment the pipeline is drawn with. Metal 4 pipelines are not specialized on it, so the Metal driver does
    not read it; it is part of the portable description because a backend with
    render-pass compatibility rules needs it. *)
type render_descriptor =
  { backend : string; label : string option; layout : Binding.pipeline_layout
  ; vertex : Shader.t; vertex_entry : string
  ; fragment : Shader.t option; fragment_entry : string option
  ; color_format : color_format; depth_format : depth_format; sample_count : int }

(** Checks a render pipeline description against the shaders' reflected
    bindings and the adapter limits; drivers create the native pipeline. *)
val create_render : Caps.t -> render_descriptor -> (unit, Error.t) result
