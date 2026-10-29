(** Immutable indexed 3D geometry. *)

type mode = Points | Lines | Line_strip | Line_loop | Triangles | Triangle_strip | Triangle_fan
type t

val mode : t -> mode
val vertices : t -> Rays_math.Vec3.t list
val vertex_count : t -> int
val index_count : t -> int
val centroid : t -> Rays_math.Vec3.t option
val vertex : int -> t -> Rays_math.Vec3.t option
val with_mode : mode -> t -> t

(* [remove_vertex] preserves valid indexing and therefore returns an error
   while the vertex is still referenced. *)
val recalculate_normals : t -> t
(* Duplicate triangle vertices so every face has one constant normal. *)

(* Expand triangle, strip, and fan modes to indexed triangles. *)

val plane : width:float -> height:float -> unit -> t

(** [packed_view] is the borrowed zero-copy renderer boundary. [view] materializes boxed vectors for
    compatibility with sibling algorithms. Callers must not mutate arrays returned by either view.
*)
module View : sig
  type vec3_view = { x : float array; y : float array; z : float array }

  type view = {
    mode : mode;
    vertices : Rays_math.Vec3.t array;
    normals : Rays_math.Vec3.t array option;
    colors : Rays_math.Color.t array option;
    tex_coords : Rays_math.Vec2.t array option;
  }

  val view : t -> view

  type packed_view = {
    mode : mode;
    vertices : vec3_view;
    indices : int array;
    normals : vec3_view option;
    colors : Rays_math.Color.t array option;
    tex_coords : Rays_math.Vec2.t array option;
  }

  val packed_view : t -> packed_view

  val create_owned :
    ?mode:mode -> ?indices:int array -> Rays_math.Vec3.t array -> (t, string) result
  (** Ownership-transfer construction for sibling-library generators. Position and normal records
      are packed into retained coordinate planes; the other supplied arrays become owned by the
      returned immutable mesh. *)

  val create_packed_owned :
    ?mode:mode ->
    ?indices:int array ->
    ?normals:vec3_view ->
    ?colors:Rays_math.Color.t array ->
    ?tex_coords:Rays_math.Vec2.t array ->
    vec3_view ->
    (t, string) result
  (** Zero-copy construction from packed coordinate planes. Every supplied array becomes owned by
      the returned immutable mesh and must never be mutated or aliased afterward. *)

  val create_packed_shared :
    ?mode:mode ->
    ?indices:int array ->
    ?normals:vec3_view ->
    ?colors:Rays_math.Color.t array ->
    ?tex_coords:Rays_math.Vec2.t array ->
    vec3_view ->
    (t, string) result
  (** Zero-copy construction from already-published immutable packed storage. The returned mesh
      retains the supplied arrays and may share them with another immutable value. No holder may
      mutate the arrays afterward. *)

  val create_device :
    ?mode:mode ->
    ?indices:int array ->
    ?colors:Rays_math.Color.t array ->
    ?tex_coords:Rays_math.Vec2.t array ->
    key:string ->
    normals:bool ->
    keep:(unit -> unit) ->
    int ->
    (t, string) result
  (** A mesh of [count] vertices whose packed float3 positions (and, with [normals], normals) a
      device holds, registered under [key] ({!Renderer.register_gpu_positions}); colours, texture
      coordinates and indices stay host arrays. Its {!vertices} are empty: nothing reads the
      positions on the host. Without normals a vertex's normal is (0, 0, 1), as a host mesh's
      without them, so a lit triangle mesh needs [normals]. [keep] is held by the mesh: its producer
      releases the buffers when it is collected. *)

  val device : t -> (string * bool) option
  (** A device mesh's key and whether it carries normals. *)

  val triangle_count : t -> int
end
