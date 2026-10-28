(** Prepared SOP surfaces. Material batches share one converted mesh, and
    explosion moves complete pieces before any material split. *)
type t
val of_geometry : ?instances:Rays_math.Mat4.t array -> Rdk_core.Geometry.t -> (t, string) result
val scene3 : ?explosion:Packed_pieces.explosion -> t -> Rays.Scene.D3.t
