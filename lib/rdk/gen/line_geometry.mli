(** Packed point clouds and straight/open/closed polylines. *)

type kind = Line_curve | Line_points

val points : (float * float * float) array -> Geometry.t

val polyline :
  ?closed:bool -> (float * float * float) array ->
  (Geometry.t, Error.t) result
