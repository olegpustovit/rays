
type group_use = Mirror_group_as_source | Mirror_group_as_destination

type method_ =
  | Mirror_by_plane of {
      origin : Rays_math.Vec3.t;
      normal : Rays_math.Vec3.t;
      distance : float;
      tolerance : float;
    }
  | Mirror_by_mapping of {
      mapping_attribute : string;
      destination_group : Group.t;
    }

type transform =
  | Mirror_copy
  | Mirror_uv of {
      origin_u : float;
      origin_v : float;
      direction_u : float;
      direction_v : float;
    }
  | Mirror_vector
  | Mirror_point

val run :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?group:Group.t ->
  ?group_use:group_use ->
  ?attributes:string ->
  ?transform:transform ->
  ?string_replace:(string * string) ->
  ?output_mapping:string ->
  ?source_group:string ->
  ?destination_group:string ->
  owner:Attribute.owner ->
  method_:method_ ->
  Geometry.t ->
  (Geometry.t, Error.t) result
