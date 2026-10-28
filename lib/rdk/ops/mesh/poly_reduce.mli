type plan = {
  edges : Edge_group.t;
  removed_primitives : int;
}

type scratch

type target =
  | Reduce_ratio of float
  | Reduce_primitive_count of int

val run :
  ?cancel:Cancel.t -> ?grain:int -> ?target:target ->
  ?primitives:Group.t -> ?hard_points:Group.t -> ?hard_edges:Edge_group.t ->
  ?preserve_boundary:bool -> ?only_original_positions:bool ->
  ?equalize_lengths:float -> ?max_normal_deviation:float ->
  ?output_group:string -> ?recompute_point_normals:bool ->
  Geometry.t -> (Geometry.t, Error.t) result
