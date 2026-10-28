(** Geometry adapter for the shared packed planar Delaunay kernel. *)

type projection =
  | Best_fit
  | Plane_xy
  | Plane_yz
  | Plane_zx
  | Plane of { origin : Rays_math.Vec3.t; normal : Rays_math.Vec3.t }
  | Point_attribute of string

(** Every field is required: the SOP declaration owns the defaults and the
    tests build values with [Rdk_test_support.triangulate2d]. *)
type params = {
  grain : int;
  selection : Element_selection.t option;
  constraint_edges : Edge_group.t option;
  constraint_primitives : Group.t option;
  projection : projection;
  seed : int64;
  split_crossing_constraints : bool;
  flood_from_hull_boundary : bool;
  remove_outside_constraint_polygons : bool;
  silhouette_constraints : bool;
  remove_outside_silhouette : bool;
  ignore_non_constraint_points : bool;
  remove_duplicate_points : bool;
  refine : bool;
  allow_constraint_splitting : bool;
  minimum_angle : float;
  maximum_area : float option;
  target_edge_length : float option;
  minimum_edge_length : float;
  maximum_new_points : int;
  regularization_steps : int;
  allow_movement_of_interior_input_points : bool;
  preserve_point_payload : bool;
  restore_original_point_positions : bool;
  keep_primitives : bool;
  split_point_group : string option;
  refinement_point_group : string option;
  triangle_group : string option;
  constraint_group : string option;
  remove_unused_points : bool;
  recompute_point_normals : bool;
}

val run : ?cancel:Cancel.t -> params -> Geometry.t -> (Geometry.t, Error.t) result
