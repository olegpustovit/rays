type selection =
  Rdk_core.Element_selection.t =
    Selected_points of Rdk_core.Group.t
  | Selected_vertices of Rdk_core.Group.t
  | Selected_primitives of Rdk_core.Group.t
  | Selected_edges of Rdk_core.Edge_group.t
type planes = { x : float array; y : float array; z : float array; }
val validate_selection :
  Rdk_core.Topology.t -> selection option -> (unit, string) result
val selection_needs_index : selection option -> bool
val point_selected :
  selection option -> Rdk_core.Topology_index.t option -> int -> bool
val point_vector_attribute :
  string -> String.t -> Rdk_core.Geometry.t -> (planes, string) result
val resolve_directions :
  ?cancel:Rdk_core.Cancel.t ->
  grain:int ->
  ?direction_attribute:String.t ->
  Rdk_core.Geometry.t -> (planes, string) result
val normals :
  ?cancel:Rdk_core.Cancel.t ->
  grain:int ->
  ?primitives:Rdk_core.Group.t ->
  Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, string) result
val peak :
  ?cancel:Cancel.t -> ?grain:int -> ?selection:selection ->
  ?direction_attribute:string -> ?normalize_direction:bool ->
  ?mask_attribute:string -> distance:float -> ?recompute_normals:bool ->
  Geometry.t -> (Geometry.t, Error.t) result

val mountain :
  ?cancel:Cancel.t -> ?grain:int -> ?selection:selection ->
  ?direction_attribute:string -> ?mask_attribute:string -> ?seed:int -> height:float ->
  ?frequency:Rays_math.Vec3.t -> ?offset:Rays_math.Vec3.t ->
  ?octaves:int -> ?lacunarity:float -> ?roughness:float ->
  ?height_attribute:string -> ?recompute_normals:bool ->
  Geometry.t -> (Geometry.t, Error.t) result

type noise_displace_mode = Height_2d | Normal_3d
val noise_displace :
  ?cancel:Cancel.t -> ?grain:int -> ?mode:noise_displace_mode -> amplitude:float -> frequency:float ->
  seed:int -> Geometry.t -> (Geometry.t, Error.t) result
(** Default [Height_2d] adds signed X/Z noise to Y. [Normal_3d] adds point
    [N * (amplitude * Rays_math.Noise.sample3(P * frequency))], without normalizing N.
    Both preserve topology and invalidate point/vertex N. O(n) time and
    O(n) output/scratch memory; stable ranges are identical across domains.
    Height displacement shares immutable X/Z planes and owns its Y output;
    each mode uses an owned output plane for temporary noise samples. *)
