(** Surface properties for 3D lighting. *)

type t = {
  diffuse : Rays_math.Color.t;
  ambient : Rays_math.Color.t;
  specular : Rays_math.Color.t;
  emissive : Rays_math.Color.t;
  shininess : float;
  round : float;
      (** Round-corners radius in scene units for the path tracer (0 = off): edges shade as a bevel
          of this radius without changing geometry. Raster ignores it. *)
}

val create :
  ?diffuse:Rays_math.Color.t ->
  ?ambient:Rays_math.Color.t ->
  ?specular:Rays_math.Color.t ->
  ?emissive:Rays_math.Color.t ->
  ?shininess:float ->
  ?round:float ->
  unit ->
  t

val default : t
val matte : Rays_math.Color.t -> t
val unlit : Rays_math.Color.t -> t
