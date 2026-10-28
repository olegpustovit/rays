(** Terminal render representation for disconnected SOP pieces.

    The source remains one immutable cooked RDK geometry. Piece membership and centers are compiled
    once into compact arrays, after which explosion controls only rebuild render positions; they
    never recook topology. This is the Rays counterpart of Connectivity -> Pack by Name -> Transform
    Pieces/Exploded View, without inventing editable packed primitives inside [Rdk.Geometry.t]. *)

type t

val of_geometry : piece_attribute:string -> Rdk_core.Geometry.t -> (t, string) result
val piece_count : t -> int

type explosion = {
  amount : float;
  scale : Rays_math.Vec3.t;
  piece_attribute : string;
  noise_amount : float;
  noise_frequency : float;
  noise_seed : int;
}

val explosion : operation:string -> fields:Param.field_view list -> explosion option
val mesh_for_explosion : explosion option -> t -> Rays.Mesh.t
