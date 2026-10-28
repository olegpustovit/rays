type scheme = Catmull_clark | Loop | Bilinear
type boundary_interpolation =
    Subdivide_boundary_none
  | Subdivide_boundary_edge_only
  | Subdivide_boundary_edge_and_corner
type face_varying_interpolation =
    Subdivide_fvar_none
  | Subdivide_fvar_corners_only
  | Subdivide_fvar_corners_plus1
  | Subdivide_fvar_corners_plus2
  | Subdivide_fvar_boundaries
  | Subdivide_fvar_all
type triangle_policy =
    Subdivide_triangles_catmull_clark
  | Subdivide_triangles_smooth
type creasing_method =
    Subdivide_creasing_uniform
  | Subdivide_creasing_chaikin
type crack_policy =
    Subdivide_do_not_close
  | Subdivide_pull_no_edge_division
  | Subdivide_pull_divide_edges of float
  | Subdivide_pull_triangulate of float
  | Subdivide_stitch_no_edge_division
  | Subdivide_stitch_divide_edges
  | Subdivide_stitch_triangulate
type plan = {
  scheme : scheme;
  source : Rdk_core.Geometry.t;
  source_topology : Rdk_core.Topology.View.view;
  index : Rdk_core.Topology_index.View.view;
  source_points : int;
  source_vertices : int;
  source_primitives : int;
  edge_count : int;
  face_offset : int;
  output_points : int;
  point_offsets : int array;
  point_sources : int array;
  point_weights : float array;
  point_representative : int array;
  edge_ancestry_attribute : string option;
  boundary_interpolation : boundary_interpolation;
  triangle_subdivision : triangle_policy;
  creasing_method : creasing_method;

  creases : crease_plan option;
  vertex_kind : bytes;
  vertex_a : int array;
  vertex_b : int array;
  vertex_primitive : int array;
  vertex_edge_source : int array;
  primitive_source : int array;
  topology : Rdk_core.Topology.t;
}
and crease_plan = {
  method_ : creasing_method;
  edge_sharpness : float array;
  child_edge_sharpness : float array;
  has_edge_creases : bool;

  vertex_masks : vertex_mask_plan;
}
and vertex_mask_plan = {
  parent_kind : bytes;
  child_kind : bytes;
  transition_factor : float array;
  parent_a : int array;
  parent_b : int array;
  child_a : int array;
  child_b : int array;
}
type fvar_point_plan = {
  fvar_scheme : scheme;
  fvar_source_points : int;
  fvar_edge_count : int;
  fvar_output_points : int;
  fvar_point_offsets : int array;
  fvar_point_sources : int array;
  fvar_point_weights : float array;
  fvar_index : Rdk_core.Topology_index.View.view;
  fvar_creases : crease_plan option;
}
type fvar_refinement =
    Linear_fvar of plan
  | Isolated_fvar of { geometry_refinement : plan;
      corner_sharpness : float array;
    }
  | Continuous_fvar of { geometry_refinement : plan;
      source_vertex_of_point : int array; pinned_points : bytes;
      output_point_of_vertex : int array;
    }
  | Split_fvar of { refinement : fvar_point_plan;
      source_vertex_of_point : int array; output_point_of_vertex : int array;
    }
type output_edge_plan = { child_edges : int array; output_edge_count : int; }
type triangle_append = {
  triangle_points : int array;
  triangle_vertex_sources : int array;
  triangle_primitive_sources : int array;
}
type point_weld = {
  old_to_output : int array;
  output_source : int array;
  merge_offsets : int array;
  merge_members : int array;
  bias : float;
}
type pull_plan = {
  ancestry_attribute : string;
  source_topology : Rdk_core.Topology.t;
  source_index : Rdk_core.Topology_index.t;
  source_positions : Rdk_core.Packed.Float3.t;
  selected_edge_start : int array;
  unselected_primitive_of_edge : int array;
  unselected_point_of_source : int array;
  unselected_primitive_of_source : int array;
}
type interface_chains = {
  edge_offsets : int array;
  points : int array;
  vertices : int array;
}
val interpolate_owned_attribute :
  ?cancel:Rdk_core.Cancel.t ->
  ?grain:int ->
  point_left:int array ->
  point_right:int array ->
  point_weight:float array ->
  vertex_left:int array ->
  vertex_right:int array ->
  vertex_weight:float array ->
  primitive_source:int array -> Rdk_core.Attribute.t -> Rdk_core.Attribute.t
type divided_boundary = {
  geometry : Rdk_core.Geometry.t;

  points : int array;
  vertices : int array;
  source_primitive_to_output : int array;
}
type detail_overrides = {
  osd_scheme : Rdk_core.Attribute.t option;
  osd_vtxboundaryinterpolation : Rdk_core.Attribute.t option;
  osd_fvarlinearinterpolation : Rdk_core.Attribute.t option;
  osd_creasingmethod : Rdk_core.Attribute.t option;
  osd_trianglesubdiv : Rdk_core.Attribute.t option;
}
type params = {
  cancel : Rdk_core.Cancel.t option;
  grain : int option;
  scheme : scheme;
  iterations : int;
  primitives : Rdk_core.Group.t option;
  cracks : crack_policy;
  consistent_topology : bool;
  creases : Rdk_core.Geometry.t option;
  crease_primitives : Rdk_core.Group.t option;
  crease_weight : float option;
  generate_resulting_creases : bool;
  resulting_crease_group : String.t option;
  hole_primitives : Rdk_core.Group.t option;
  remove_holes : bool;
  boundary_interpolation : boundary_interpolation;
  face_varying_interpolation : face_varying_interpolation;
  triangle_policy : triangle_policy;
  creasing_method : creasing_method;
  treat_curves_as_independent : bool;
  recompute_point_normals : bool;
}

val subdivide : params -> Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
