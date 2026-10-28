type stage = Vertex | Fragment | Compute
type kind = Buffer | Texture | Sampler | Acceleration_structure
type layout_entry = { binding : int; kind : kind; visibility : stage list }
type layout
type pipeline_layout

val create_layout : layout_entry list -> (layout, Error.t) result

val create_pipeline_layout :
  device:Handle.device ->
  capabilities:Caps.t ->
  (int * layout) list ->
  (pipeline_layout, Error.t) result

val pipeline_layouts : pipeline_layout -> (int * layout_entry list) list
