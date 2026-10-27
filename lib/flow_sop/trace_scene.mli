(** The CPU host for [scene/trace]: a polygon geometry's {!Rdk.Surface_index} answers the ray queries of a
    [Flow.Ray_host] scene id. Distances are world units along the normalised direction; [normal] is the
    geometric triangle normal; [u] and [v] are the barycentric weights of triangle corners 1 and 2. *)
val sphere_prim : int -> int
(** A sphere hit reports [prim = -2 - k] for the k-th sphere; a miss reports -1. *)
val world_to_object : float array -> float array
(** The inverse of a row-major 3x4 affine transform. *)
val register : ?spheres:(float * float * float * float) array -> int -> Rdk.Geometry.t -> (unit, string) result
(** [spheres] are procedural (centre, radius) primitives tested analytically beside the triangles; the nearer
    hit wins. [normal] of a sphere hit is the outward unit normal; [u] and [v] are 0. *)
val unregister : int -> unit
val register_instances : ?spheres:(float * float * float * float) array -> int ->
  (Rdk.Geometry.t * float array) array -> (unit, string) result
(** Instances of prototypes with row-major 3x4 object-to-world transforms. [instance] is the array index, [prim]
    the primitive within its prototype, [normal] the world-space unit normal, [t] the world distance. *)
