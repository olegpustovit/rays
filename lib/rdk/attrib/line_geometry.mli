(** Packed point clouds and straight/open/closed polylines. *)

val points : (float * float * float) array -> Geometry.t

val polyline :
  ?closed:bool -> (float * float * float) array ->
  (Geometry.t, Error.t) result
