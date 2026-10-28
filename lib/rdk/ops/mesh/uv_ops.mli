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

type params = Uv_parameterize.params = {
  cancel : Cancel.t option;
  grain : int option;
  name : string;
  seams : Group.t option;
  edge_seams : Edge_group.t option;
  uv_tolerance : float;
  iterations : int;
  tolerance : float;
}

val flatten : params -> Geometry.t -> (Geometry.t, Error.t) result
val relax : params -> Geometry.t -> (Geometry.t, Error.t) result
