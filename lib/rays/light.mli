(** Immutable light sources for [Scene3]. *)

type attenuation = {
  constant : float;
  quadratic : float;
}

type kind =
  | Directional of { direction : Rays_math.Vec3.t }
  | Point of { position : Rays_math.Vec3.t; attenuation : attenuation }
  | Spot of {
      position : Rays_math.Vec3.t;
      direction : Rays_math.Vec3.t;
      cutoff : float;
      concentration : float;
      attenuation : attenuation;
    }
  | Area of {
      position : Rays_math.Vec3.t;
      direction : Rays_math.Vec3.t;
      width : float;
      height : float;
      samples : int;
      attenuation : attenuation;
    }

type t = {
  kind : kind;
  diffuse : Rays_math.Color.t;
  intensity : float;
}
(** [diffuse] tints both the diffuse and the specular response. *)

val attenuation :
  ?constant:float -> ?quadratic:float -> unit -> attenuation
(** Falloff divisor [constant + quadratic * distance²]. Both coefficients
    must be finite and non-negative, with at least one non-zero. *)

val directional :
  ?diffuse:Rays_math.Color.t ->
  ?intensity:float ->
  direction:Rays_math.Vec3.t ->
  unit ->
  t
val point :
  ?diffuse:Rays_math.Color.t ->
  ?intensity:float ->
  ?attenuation:attenuation ->
  at:Rays_math.Vec3.t ->
  unit ->
  t

val spot :
  ?diffuse:Rays_math.Color.t ->
  ?intensity:float ->
  ?attenuation:attenuation ->
  at:Rays_math.Vec3.t ->
  direction:Rays_math.Vec3.t ->
  cutoff:float ->
  concentration:float ->
  unit ->
  t
(** [cutoff] is the cone half-angle in radians. *)

val area :
  ?diffuse:Rays_math.Color.t ->
  ?intensity:float ->
  ?attenuation:attenuation ->
  ?samples:int ->
  at:Rays_math.Vec3.t ->
  direction:Rays_math.Vec3.t ->
  width:float ->
  height:float ->
  unit ->
  t
(* [samples] is [1], [4], [9], or [16] deterministic surface samples. *)
