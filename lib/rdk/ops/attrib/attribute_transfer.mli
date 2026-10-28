type mode =
  | Nearest
  | Inverse_distance of { neighbors : int; power : float }
  | Kernel of { neighbors : int; radius : float; kernel : kernel }
and kernel = Links | RenderMan | Hart

type unmatched = Keep_target | Default_value
type falloff = Linear | Smoothstep | Uniform of float
type surface_falloff = falloff
type surface_vertex_selection = Surface_index.vertex_selection =
  | All_triangle_vertices | Any_triangle_vertex

type surface_attribute = {
  source_owner : Attribute.owner;
  source_name : string;
  target_name : string;
}

val transfer_points :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?names:string list ->
  ?pattern:string ->
  ?mode:mode ->
  ?max_distance:float ->
  ?blend_width:float ->
  ?falloff:falloff ->
  ?unmatched:unmatched ->
  ?source_points:Group.t ->
  ?target_points:Group.t ->
  source:Geometry.t ->
  target:Geometry.t ->
  unit ->
  (Geometry.t, Error.t) result

val transfer_primitives :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?names:string list ->
  ?pattern:string ->
  ?mode:mode ->
  ?max_distance:float ->
  ?blend_width:float ->
  ?falloff:falloff ->
  ?unmatched:unmatched ->
  ?source_primitives:Group.t ->
  ?target_primitives:Group.t ->
  source:Geometry.t ->
  target:Geometry.t ->
  unit ->
  (Geometry.t, Error.t) result

val transfer_detail :
  ?names:string list ->
  ?pattern:string ->
  source:Geometry.t ->
  target:Geometry.t ->
  unit ->
  (Geometry.t, Error.t) result

val transfer_surface :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?max_distance:float ->
  ?blend_width:float ->
  ?falloff:surface_falloff ->
  ?unmatched:unmatched ->
  ?target_owner:Attribute.owner ->
  ?distance_attribute:string ->
  ?source_primitives:Group.t ->
  ?source_vertices:Group.t ->
  ?source_vertex_selection:surface_vertex_selection ->
  ?target_elements:Group.t ->
  attributes:surface_attribute list ->
  source:Geometry.t ->
  target:Geometry.t ->
  unit ->
  (Geometry.t, Error.t) result

val transfer_vertices :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?names:string list ->
  ?pattern:string ->
  ?max_distance:float ->
  ?blend_width:float ->
  ?falloff:falloff ->
  ?unmatched:unmatched ->
  ?source_primitives:Group.t ->
  ?source_vertices:Group.t ->
  ?source_vertex_selection:surface_vertex_selection ->
  ?target_vertices:Group.t ->
  source:Geometry.t ->
  target:Geometry.t ->
  unit ->
  (Geometry.t, Error.t) result

val transfer_all :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?point_pattern:string ->
  ?vertex_pattern:string ->
  ?primitive_pattern:string ->
  ?detail_pattern:string ->
  ?mode:mode ->
  ?max_distance:float ->
  ?blend_width:float ->
  ?falloff:falloff ->
  ?unmatched:unmatched ->
  source:Geometry.t ->
  target:Geometry.t ->
  unit ->
  (Geometry.t, Error.t) result
