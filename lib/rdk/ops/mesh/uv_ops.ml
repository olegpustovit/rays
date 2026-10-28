
type projection =
  | Planar of { origin : Rays_math.Vec3.t; u_axis : Rays_math.Vec3.t; v_axis : Rays_math.Vec3.t }
  | Cylindrical of {
      origin : Rays_math.Vec3.t; axis : Rays_math.Vec3.t; seam : Rays_math.Vec3.t; height : float;
    }
  | Spherical of { origin : Rays_math.Vec3.t; axis : Rays_math.Vec3.t; seam : Rays_math.Vec3.t }

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

let flatten params geometry =
  Error.guard ~operation:"uv_flatten" ~code:"invalid_uv" @@ fun () ->
  Uv_parameterize.solve params Uv_parameterize.Circle geometry

let relax params geometry =
  Error.guard ~operation:"uv_relax" ~code:"invalid_uv" @@ fun () ->
  Uv_parameterize.solve params Uv_parameterize.Preserve geometry
