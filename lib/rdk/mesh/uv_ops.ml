
type projection =
  | Planar of { origin : Rays_math.Vec3.t; u_axis : Rays_math.Vec3.t; v_axis : Rays_math.Vec3.t }
  | Cylindrical of {
      origin : Rays_math.Vec3.t; axis : Rays_math.Vec3.t; seam : Rays_math.Vec3.t; height : float;
    }
  | Spherical of { origin : Rays_math.Vec3.t; axis : Rays_math.Vec3.t; seam : Rays_math.Vec3.t }

let flatten ?cancel ?grain ?name ?seams ?edge_seams ?iterations ?tolerance
    geometry =
  Error.guard ~operation:"uv_flatten" ~code:"invalid_uv" @@ fun () ->
  Uv_parameterize.solve ?cancel ?grain ?name ?seams ?edge_seams ?iterations
    ?tolerance Uv_parameterize.Circle geometry

let relax ?cancel ?grain ?name ?seams ?edge_seams ?uv_tolerance ?iterations
    ?tolerance geometry =
  Error.guard ~operation:"uv_relax" ~code:"invalid_uv" @@ fun () ->
  Uv_parameterize.solve ?cancel ?grain ?name ?seams ?edge_seams ?uv_tolerance
    ?iterations ?tolerance Uv_parameterize.Preserve geometry

