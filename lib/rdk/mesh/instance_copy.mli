(** Packed instance materialization, duplication, and copy-to-points. *)

type copy_target_owner = Copy_target_points | Copy_target_vertices
  | Copy_target_primitives
type copy_target_operation = Copy_target_nothing | Copy_target_copy
  | Copy_target_add | Copy_target_subtract | Copy_target_multiply
type copy_target_attribute_rule = {
  copy_target_pattern : string;
  copy_target_owner : copy_target_owner;
  copy_target_operation : copy_target_operation;
}

val materialize_instances :
  transforms:Rays_math.Mat4.t array -> Geometry.t ->
  (Geometry.t, Error.t) result
(** Materialize transform-major copies with exact topology and group ancestry. *)

