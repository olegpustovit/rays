type projection =
  | Planar of {
      origin : Rays_math.Vec3.t;
      u_axis : Rays_math.Vec3.t;
      v_axis : Rays_math.Vec3.t;
    }
  | Cylindrical of {
      origin : Rays_math.Vec3.t;
      axis : Rays_math.Vec3.t;
      seam : Rays_math.Vec3.t;
      height : float;
    }
  | Spherical of {
      origin : Rays_math.Vec3.t;
      axis : Rays_math.Vec3.t;
      seam : Rays_math.Vec3.t;
    }

val flatten :
  ?cancel:Cancel.t -> ?grain:int -> ?name:string -> ?seams:Group.t ->
  ?edge_seams:Edge_group.t -> ?iterations:int -> ?tolerance:float ->
  Geometry.t -> (Geometry.t, Error.t) result

val relax :
  ?cancel:Cancel.t -> ?grain:int -> ?name:string -> ?seams:Group.t ->
  ?edge_seams:Edge_group.t -> ?uv_tolerance:float -> ?iterations:int ->
  ?tolerance:float -> Geometry.t -> (Geometry.t, Error.t) result
