type plan = {
  edges : Edge_group.t;
  removed_primitives : int;
}

type scratch

type target =
  | Reduce_ratio of float
  | Reduce_primitive_count of int

type params = {
  cancel : Cancel.t option;
  grain : int option;
  target : target;
  primitives : Group.t option;
  hard_points : Group.t option;
  hard_edges : Edge_group.t option;
  preserve_boundary : bool;
  only_original_positions : bool;
  equalize_lengths : float;
  max_normal_deviation : float option;
  output_group : string option;
  recompute_point_normals : bool;
}

val run : params -> Geometry.t -> (Geometry.t, Error.t) result