(** Packed instance materialization, duplication, and copy-to-points. *)


val materialize_instances :
  transforms:Rays_math.Mat4.t array -> Geometry.t ->
  (Geometry.t, Error.t) result
(** Materialize transform-major copies with exact topology and group ancestry. *)
