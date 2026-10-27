open Rays_math

type mode = Separate_pieces_separate | Separate_pieces_move_back

val run :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?owner:Attribute.owner ->
  ?translation_attribute:string ->
  ?axis:Rays_math.Vec3.t ->
  ?gap:float ->
  mode:mode ->
  piece_attribute:string ->
  Geometry.t -> (Geometry.t, Error.t) result
(** Deterministically pack integer- or text-identified pieces along [axis], or
    reverse a previous pack from its stored float3 translation attribute. *)
