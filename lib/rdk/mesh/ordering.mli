type owner = Points | Primitives
type key =
    X
  | Y
  | Z
  | Distance_to of Rays_math.Vec3.t
  | Along_vector of Rays_math.Vec3.t
  | Attribute_component of { name : string; component : int; }
  | By_vertex_order
  | By_primitive_index
  | Spatial_locality
  | Random of int64
  | Index_attribute of string
  | Reverse
  | Shift of int
exception Sort_error of string
val sort :
  ?cancel:Rdk_core.Cancel.t ->
  ?grain:int ->
  ?selection:Rdk_core.Group.t ->
  ?descending:bool ->
  ?output_indices:string ->
  ?combine_indices:bool ->
  owner:owner ->
  key:key -> Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
