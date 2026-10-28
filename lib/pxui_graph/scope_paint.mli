(* Private graph-pane interface; only the pane's shared operations. *)
val ty_color : Scope_state.t -> Flow.Ty.t option -> Rays_math.Color.t

val frame_in :
  Pxui.Ui.Paint.t ->
  x:float ->
  y:float ->
  w:float ->
  h:float ->
  ?fill:Rays_math.Color.t ->
  Rays_math.Color.t ->
  unit

val head_pad : float
val label_x : float
val field_w : float
val vec_w : float
val field_h : float
val switch_w : float
val switch_h : float
val value_x : float -> float
val port_y : float
val shown_glyph : string -> string
val all_numbers : Flow.Syntax.t list -> bool
val wired_name : Flow_graph.Projection.row -> string -> string

val wired_geo :
  measure:('a -> string -> float) ->
  Scope_state.t ->
  z:float ->
  fs:'a ->
  x:float ->
  w:float ->
  fold:bool ->
  string ->
  float * float * float * string option * float

val ref_chip_geo : Scope_state.t -> z:float -> fs:int -> w:float -> string -> float * float

val selector_geo :
  measure:('a -> string -> float) ->
  z:float ->
  fs:'a ->
  w:float ->
  count:int ->
  probe:int ->
  float * float * float * float * string

val lens_button_box : len:int -> int -> (float * float) * (float * float)
val lens_replace_box : float -> (float * float) * (float * float)
val lens_top : float -> float -> float

val paint_point :
  Pxui.Ui.Paint.t ->
  Pxui.Ui.t ->
  Scope_state.t ->
  z:float ->
  fs:int ->
  Flow_graph.Projection.placed ->
  ?failed:'a ->
  selected:bool ->
  float * float * 'b * 'c ->
  unit

val paint_node :
  Pxui.Ui.Paint.t ->
  Pxui.Ui.t ->
  Scope_state.t ->
  z:float ->
  fs:int ->
  ?footer:Flow_graph.Probe.footer ->
  ?lens_step:int ->
  ?hovered:bool ->
  ?carry:bool ->
  ?out_wired:bool ->
  shown:Flow_graph.Projection.level ->
  ?failed:string ->
  Flow_graph.Projection.placed ->
  Flow_graph.Projection.node ->
  selected:bool ->
  row_hover:int option ->
  float * float * float * float ->
  unit

val zone_uses : Flow_graph.Projection.zone -> string list -> bool

val paint_zone_frame :
  Pxui.Ui.Paint.t ->
  Pxui.Ui.t ->
  Scope_state.t ->
  z:float ->
  fs:int ->
  ?footer:Flow_graph.Probe.footer ->
  Flow_graph.Projection.node ->
  Flow_graph.Projection.zone ->
  selected:bool ->
  out_wired:bool ->
  float * float * float * float ->
  probe:int ->
  count:int ->
  unit

val paint_input :
  Pxui.Ui.Paint.t ->
  Pxui.Ui.t ->
  Scope_state.t ->
  z:float ->
  fs:int ->
  Flow_graph.Projection.input ->
  selected:bool ->
  float * float * float * float ->
  unit

val paint_return :
  Pxui.Ui.Paint.t ->
  Pxui.Ui.t ->
  Scope_state.t ->
  z:float ->
  fs:int ->
  Flow_graph.Projection.scope ->
  selected:bool ->
  float * float * float * float ->
  unit

val paint_polyline :
  clip:float * float * float * float ->
  Pxui.Ui.Paint.t ->
  width:float ->
  Rays_math.Color.t ->
  (float * float) list ->
  unit

val paint_background :
  Pxui.Ui.Paint.t ->
  Scope_state.t ->
  viewport:float * float * float * float ->
  wires:int list ->
  (Flow_graph.Projection.node
  * Flow_graph.Projection.zone
  * Flow_graph.Projection.placed
  * float
  * float)
  list ->
  unit
