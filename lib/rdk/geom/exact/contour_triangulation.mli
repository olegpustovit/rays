(** Deterministic triangulation of one simple outer contour with simple holes. *)

val triangulate :
  ?cancel:Cancel.t ->
  point:(int -> float * float) ->
  outer:int array ->
  holes:int array array ->
  unit ->
  (int array, string) result
(** Return flattened token triples. Contours may have either winding; the
    implementation normalizes the outer contour counter-clockwise and holes
    clockwise, inserts deterministic non-crossing visibility bridges, and
    ear-clips the resulting weakly simple ring. *)
