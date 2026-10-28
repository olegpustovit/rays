type mode = Fill_single_polygon | Fill_triangles | Fill_triangle_fan
type boundary_plan = {
  source_index : Rdk_core.Topology_index.t;
  source_index_view : Rdk_core.Topology_index.View.view;
  loop_offsets : int array;
  loop_vertices : int array;
  loop_points : int array;
}
type center_map =
    No_centers
  | Point_centers of int
  | Vertex_centers of { vertex_points : int array; first_point : int; }
val run :
  ?cancel:Rdk_core.Cancel.t ->
  ?grain:int ->
  ?boundary:Rdk_core.Edge_group.t ->
  ?mode:mode ->
  ?reverse_patches:bool ->
  ?unique_points:bool ->
  ?update_point_normals:bool ->
  ?patch_group:string ->
  Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
