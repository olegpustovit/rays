type mode = Fill_single_polygon | Fill_triangles | Fill_triangle_fan

type boundary_plan = {
  source_index : Rdk_core.Topology_index.t;
  source_index_view : Rdk_core.Topology_index.View.view;
  loop_offsets : int array;
  loop_vertices : int array;
  loop_points : int array;
}

type center_map =
  | No_centers
  | Point_centers of int
  | Vertex_centers of { vertex_points : int array; first_point : int }

type params = {
  cancel : Rdk_core.Cancel.t option;
  grain : int option;
  boundary : Rdk_core.Edge_group.t option;
  mode : mode;
  reverse_patches : bool;
  unique_points : bool;
  update_point_normals : bool;
  patch_group : string option;
}

val run : params -> Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
