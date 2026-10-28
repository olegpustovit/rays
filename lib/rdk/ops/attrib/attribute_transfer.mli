type mode =
  | Nearest
  | Inverse_distance of { neighbors : int; power : float }
  | Kernel of { neighbors : int; radius : float; kernel : kernel }

and kernel = Links | RenderMan | Hart

type unmatched = Keep_target | Default_value
type falloff = Linear | Smoothstep | Uniform of float
type surface_falloff = falloff

type surface_attribute = {
  source_owner : Attribute.owner;
  source_name : string;
  target_name : string;
}

type spatial_params = {
  cancel : Cancel.t option;
  grain : int option;
  names : string list option;
  pattern : string option;
  mode : mode;
  max_distance : float option;
  blend_width : float;
  falloff : falloff;
  unmatched : unmatched;
  source_elements : Group.t option;
  target_elements : Group.t option;
  source : Geometry.t;
  target : Geometry.t;
}

type detail_params = {
  names : string list option;
  pattern : string option;
  source : Geometry.t;
  target : Geometry.t;
}

type surface_params = {
  cancel : Cancel.t option;
  grain : int option;
  max_distance : float option;
  blend_width : float;
  falloff : surface_falloff;
  unmatched : unmatched;
  target_owner : Attribute.owner;
  distance_attribute : string option;
  source_primitives : Group.t option;
  source_vertices : Group.t option;
  source_vertex_selection : Surface_index.vertex_selection;
  target_elements : Group.t option;
  attributes : surface_attribute list;
  source : Geometry.t;
  target : Geometry.t;
}

type vertices_params = {
  cancel : Cancel.t option;
  grain : int option;
  names : string list option;
  pattern : string option;
  max_distance : float option;
  blend_width : float;
  falloff : falloff;
  unmatched : unmatched;
  source_primitives : Group.t option;
  source_vertices : Group.t option;
  source_vertex_selection : Surface_index.vertex_selection;
  target_vertices : Group.t option;
  source : Geometry.t;
  target : Geometry.t;
}

type all_params = {
  cancel : Cancel.t option;
  grain : int option;
  point_pattern : string option;
  vertex_pattern : string option;
  primitive_pattern : string option;
  detail_pattern : string option;
  mode : mode;
  max_distance : float option;
  blend_width : float;
  falloff : falloff;
  unmatched : unmatched;
  source : Geometry.t;
  target : Geometry.t;
}

val transfer_points : spatial_params -> (Geometry.t, Error.t) result
val transfer_primitives : spatial_params -> (Geometry.t, Error.t) result
val transfer_detail : detail_params -> (Geometry.t, Error.t) result
val transfer_surface : surface_params -> (Geometry.t, Error.t) result
val transfer_vertices : vertices_params -> (Geometry.t, Error.t) result
val transfer_all : all_params -> (Geometry.t, Error.t) result
