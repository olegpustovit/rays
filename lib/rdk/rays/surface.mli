type t
(** Prepared SOP surfaces. Material batches share one converted mesh, and explosion moves complete
    pieces before any material split. *)

exception Conversion_error of string

val of_geometry : ?instances:Rays_math.Mat4.t array -> Rdk_core.Geometry.t -> (t, string) result

val host_reason : t -> string option
(** Why preparation selected the host bridge; [None] for a device-eligible surface. A later rejected
    device borrow is reported by the callback's owner. *)

val scene3_result :
  ?explosion:Packed_pieces.explosion ->
  ?device:(Rdk_core.Geometry.t -> (string * bool * int option * int * (unit -> unit)) option) ->
  t ->
  (Rays.Scene.D3.t, string) result
(** On the initial domain. Resident point P/N/Cd and triangle/quad topology can be drawn from the
    device. Other attributes/layouts use the host bridge. For resident polygon topology, the device
    callback validates and generates display indices before accepting the borrow: [device] registers
    its buffers with the renderer ({!Rays.Mesh.View.create_device}: the key, whether N is bound, the
    device triangle index count if any, the colour component width (0, 3 or 4), and what the mesh
    keeps). Metadata comes from the successful borrow, never inferred from a deferred stream which
    another CPU reader may have forced. Without [device], or when it returns [None], the host mesh
    is built (reading P back). *)

val scene3 :
  ?explosion:Packed_pieces.explosion ->
  ?device:(Rdk_core.Geometry.t -> (string * bool * int option * int * (unit -> unit)) option) ->
  t ->
  Rays.Scene.D3.t
(** Convenience form of {!scene3_result}. Raises {!Conversion_error} on a deferred host conversion
    failure; a device mesh's later host-copy request raises the same error. *)
