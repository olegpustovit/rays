(** Pure descriptions of a depth-tested 3D scene. *)

type render_mode = Faces | Wireframe | Vertices
type cull = Cull_none | Cull_back | Cull_front
type shading = Smooth | Flat
type comparison = Never | Less | Equal | Less_equal | Greater | Not_equal | Greater_equal | Always
type depth_state = private { comparison : comparison; write : bool }
type blend = Replace | Alpha | Add | Multiply | Screen | Subtract

type texture = private {
  value : Texture.t;
  filter : Texture.filter;
  wrap_u : Texture.wrap;
  wrap_v : Texture.wrap;
}

type node
type t

val empty : t

(* [samples] is [1], [4], [9], or [16] coverage samples per pixel. The native
   renderer shades at most 64 lights; rendering a scene with more fails with a
   typed lowering error instead of dropping lights. *)
val create :
  ?lights:Light.t list ->
  ?ambient:Rays_math.Color.t ->
  ?separate_specular:bool ->
  ?samples:int ->
  node list ->
  t

val depth_state : ?write:bool -> unit -> depth_state
val textured : ?filter:Texture.filter -> Texture.t -> texture

val mesh :
  ?material:Material.t ->
  ?texture:texture ->
  ?mode:render_mode ->
  ?cull:cull ->
  ?shading:shading ->
  Mesh.t ->
  node

(* Draw immutable geometry at many independent transforms. *)
val instances_array :
  ?material:Material.t ->
  ?mode:render_mode ->
  ?cull:cull ->
  ?shading:shading ->
  Mesh.t ->
  Rays_math.Mat4.t array ->
  node
(** Array-native instance submission. The array is copied once; meshes remain shared and instance
    transforms stream through rendering without first materializing a drawing list. *)

val nodes : t -> node list
(** The scene's drawings without its lights and render settings, e.g. to place a whole scene under a
    [transform] inside another. *)

val group : node list -> node
val transform : Rays_math.Mat4.t -> node list -> node
val translate : Rays_math.Vec3.t -> node list -> node
val with_depth : depth_state -> node list -> node
val with_blend : blend -> node list -> node
val plane : ?cull:cull -> width:float -> height:float -> unit -> node

val with_world : World.baked -> t -> t
(** Light the scene with a baked {!World} instead of Blinn-Phong: the camera map (or the World's
    background color) behind the geometry, image-based diffuse (SH9) and specular (prefiltered
    mips), the sun and the extracted rect lights beside the scene's own lights, all through the path
    tracer's GGX BRDF and ACES tone map. [specification/environment.md] documents the material
    mapping and the light cap. *)

(** Internal renderer boundary. *)
module Walk : sig
  val with_texture : texture -> t -> t

  type drawing = {
    mesh : Mesh.t;
    material : Material.t;
    texture : texture option;
    mode : render_mode;
    cull : cull;
    depth : depth_state;
    blend : blend;
    transform : Rays_math.Mat4.t;
  }

  val drawings : t -> drawing list
  val cacheable : t -> bool

  (* Iterate one descriptor per mesh node. [Some transforms] is a borrowed
     instance batch composed after the descriptor's parent transform. *)
  val iter_batches : (drawing -> Rays_math.Mat4.t array option -> unit) -> t -> unit
  val lights : t -> Light.t list
  val ambient : t -> Rays_math.Color.t
  val separate_specular : t -> bool
  val depth_clear : t -> float
  val samples : t -> int
  val world : t -> World.baked option
end
