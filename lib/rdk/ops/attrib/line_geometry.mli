(** Packed point clouds and straight/open/closed polylines. *)

val points : (float * float * float) array -> Geometry.t

type params = {
  closed : bool;
}

val polyline : params -> (float * float * float) array -> (Geometry.t, Error.t) result