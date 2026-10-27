(** Surface properties for 3D lighting. *)

type t = {
  diffuse : Color.t;
  ambient : Color.t;
  specular : Color.t;
  emissive : Color.t;
  shininess : float;
  round : float;
      (** Round-corners radius in scene units for the path tracer (0 = off): edges shade as a
          bevel of this radius without changing geometry. Raster ignores it. *)
}

val create :
  ?diffuse:Color.t ->
  ?ambient:Color.t ->
  ?specular:Color.t ->
  ?emissive:Color.t ->
  ?shininess:float ->
  ?round:float ->
  unit ->
  t

val default : t
val matte : Color.t -> t
val unlit : Color.t -> t
