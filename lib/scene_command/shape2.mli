(** Shared renderer-neutral tessellation for ordinary Scene2 and retained display-list producers.
    Coordinates are logical points. *)

val line :
  from_:float * float -> to_:float * float -> width:float -> color:int32 -> Render_ir.geometry

val polygon :
  (float * float) list -> fill:int32 option -> stroke:int32 option -> Render_ir.geometry array

val polyline : (float * float) list -> color:int32 -> Render_ir.geometry
val path : fill:int32 option -> stroke:int32 option -> Path.t -> Render_ir.geometry array

val rounded_rect :
  width:float ->
  height:float ->
  radius:float ->
  fill:int32 option ->
  stroke:int32 option ->
  Render_ir.geometry array

val ellipse :
  center:float * float ->
  rx:float ->
  ry:float ->
  fill:int32 option ->
  stroke:int32 option ->
  Render_ir.geometry array
