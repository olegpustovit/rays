type t
(** Prepared SOP surfaces. Material batches share one converted mesh, and explosion moves complete
    pieces before any material split. *)

val of_geometry : ?instances:Rays_math.Mat4.t array -> Rdk_core.Geometry.t -> (t, string) result

val scene3 :
  ?explosion:Packed_pieces.explosion ->
  ?device:(Rdk_core.Geometry.t -> (string * bool * (unit -> unit)) option) ->
  t ->
  Rays.Scene.D3.t
(** On the initial domain. A point cloud whose P (and N) a device holds, or a triangle mesh whose P
    and point N it holds, with nothing else to show, is drawn from the device: [device] registers
    its buffers with the renderer ({!Rays.Mesh.View.create_device}: the key, whether N is bound,
    what the mesh keeps). Without [device], or when it returns [None], the host mesh is built
    (reading P back). *)
