type group_use = Mirror_group_as_source | Mirror_group_as_destination

type method_ =
  | Mirror_by_plane of {
      origin : Rays_math.Vec3.t;
      normal : Rays_math.Vec3.t;
      distance : float;
      tolerance : float;
    }
  | Mirror_by_mapping of { mapping_attribute : string; destination_group : Group.t }

type transform =
  | Mirror_copy
  | Mirror_uv of { origin_u : float; origin_v : float; direction_u : float; direction_v : float }
  | Mirror_vector
  | Mirror_point

type params = {
  cancel : Cancel.t option;
  grain : int option;
  group : Group.t option;
  group_use : group_use;
  attributes : string;
  transform : transform;
  string_replace : (string * string) option;
  output_mapping : string option;
  source_group : string option;
  destination_group : string option;
  owner : Attribute.owner;
  method_ : method_;
}

val run : params -> Geometry.t -> (Geometry.t, Error.t) result
