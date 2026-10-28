(** Metal ray-tracing path tracer.

    RDK meshes carry metallic-roughness materials and are lit by a procedural
    dome: a sky/ground gradient plus soft rectangular emitter panels, which
    together stand in for a studio HDRI, or by a baked [Rays.World]
    ({!set_world}). Rendering is progressive: every
    [render] call adds [spp] samples per pixel into an accumulation buffer and
    refreshes the borrowed [image]; a camera change restarts accumulation. *)

(** Linear floating-point RGB for materials and light. Channels are not
    quantized or clamped, so dim and HDR values survive GPU upload. *)
module Linear_color = Rays_math.Color.Linear

type material =
  { albedo : Linear_color.t
  ; roughness : float  (** 0 = mirror, 1 = fully rough *)
  ; metallic : float
  ; emission : Linear_color.t
  ; round : float
    (** Round-corners radius in scene units (0 = off). A render-time shading
        bevel: the normal rolls over onto adjacent faces within this distance
        of an edge, like Redshift's Round Corners, without changing geometry. *) }

val material :
  ?roughness:float -> ?metallic:float -> ?emission:Linear_color.t -> ?round:float ->
  Linear_color.t -> material

(** A soft rectangle of light painted on the dome around [direction]. Extents
    are angular half-sizes in radians; [softness] is the edge falloff. *)
type panel =
  { direction : Rays_math.Vec3.t
  ; width : float
  ; height : float
  ; softness : float
  ; color : Linear_color.t
  ; intensity : float }

val panel :
  ?softness:float -> intensity:float -> width:float ->
  height:float -> Rays_math.Vec3.t -> panel

type environment = { sky : Linear_color.t; ground : Linear_color.t; panels : panel list }

(** Rectangle area light of [size] (width, height) centred at [at], facing
    [target], two-sided. Sampled with shadow rays (next-event estimation);
    lights are analytic and, without a World ({!set_world}), never appear as
    visible geometry. *)
type light =
  { at : Rays_math.Vec3.t; target : Rays_math.Vec3.t; size : float * float; color : Linear_color.t; intensity : float }

val rect_light :
  intensity:float -> size:float * float -> target:Rays_math.Vec3.t ->
  Rays_math.Vec3.t -> light

val light_of : Rays.Light.t -> light
(** The rect that lights like a raster [Light.t] with physical 1/d²
    attenuation: a white matte face at distance [d] receives irradiance
    [pi * intensity * color / d²] (the raster World path's inverse: a rect of
    radiance [L] and area [A] is a [Light.t] of color [L * A / pi]). [color]
    is [diffuse / 255] as linear RGB; the rect's radiance is
    [pi * intensity / (width * height)].
    - [Area]: same size, centred at its position, facing [direction].
    - [Point]: a 0.1 x 0.1 rect facing down (-Y).
    - [Spot]: a 0.1 x 0.1 rect facing [direction].
    - [Directional]: a 20 x 20 rect 100 units against [direction], aimed at
      the origin, with radiance [pi * intensity * 100² / 400].
    ponytail: rects are two-sided and cosine-weighted, so a point light is
    dark sideways, a spot ignores its cone, and attenuation coefficients other
    than the quadratic term are ignored. *)

(** An analytic sphere, traced through a bounding-box intersection function
    rather than tessellated. *)
type sphere = { center : Rays_math.Vec3.t; radius : float; sphere_material : material }

(** A round polyline of constant [thickness] (diameter), traced as linear
    curve segments. Needs [Ogpu.Caps.Ray_tracing_curves]; [create] and
    [replace_mesh] fail with a typed message otherwise. *)
type strand = { points : Rays_math.Vec3.t array; thickness : float; strand_material : material }

(** Scene mesh inputs are already triangle-ready [Rays.Mesh.t] values with
    per-vertex normals. Geometry triangulation and normal generation happen
    before entering the path tracer. *)
type scene =
  { objects : (Rays.Mesh.t * material) list
  ; spheres : sphere list
  ; strands : strand list
  ; environment : environment
  ; lights : light list }

type t

(** Pure prepared geometry: a flat triangle mesh or a prototype plus compact
    instance transforms. [mesh] packs existing triangle buffers and materials;
    it does not triangulate geometry or generate normals. A SOP cook worker may
    prepare it off the initial domain. *)
type mesh

val mesh :
  ?spheres:sphere list -> ?strands:strand list -> (Rays.Mesh.t * material) list ->
  (mesh, string) result
(** Triangles, spheres, and strands together become one primitive structure
    per kind under an instance structure (Metal holds one kind per structure). *)

val mesh_instanced :
  prototype:mesh -> ?materials:material array ->
  ?motion:Rays_math.Mat4.t array -> Rays_math.Mat4.t array -> (mesh, string) result
(** One prototype and its instance transforms, without duplicated triangles.
    For [n] instances, preparation takes O(n) time and O(n) auxiliary memory;
    the prototype's topology is stored once. [materials] (one per instance)
    overrides the prototype material per instance through the instance user
    id. [motion] gives each instance a second transform at the end of the
    shutter; the instance structure carries both keyframes and every sample
    picks a shutter time, so instances blur along their motion. *)

val scene_mesh : (Rays_math.Mat4.t * mesh) list -> (mesh, string) result
(** Several prepared meshes placed at their world transforms in one
    traceable scene: an instance structure over one primitive structure per
    input. A flat input is one instance at its transform; an instanced input
    is one instance per copy, each copy's transform premultiplied by its
    object's. Materials stay per object. Pure, like {!mesh}. Inputs must be
    triangle meshes without motion (spheres, strands, or motion are an
    [Error]); a [scene_mesh] result may itself be an input. *)

(** [Some reason] when [caps] cannot path-trace (no hardware ray tracing). *)
val unavailable : Ogpu.Caps.t -> string option

(** {!unavailable} for the shared GPU: the editor asks before it creates a tracer. *)
val available : unit -> string option

(** Builds the primitive acceleration structure, compiles the kernels, and
    allocates accumulation and output storage. Fails with a typed message
    when no ray-tracing Metal device is available. *)
val create :
  ?spp:int -> ?bounces:int -> ?exposure:float -> ?round_samples:int ->
  ?bsdf_only:bool -> ?profile:bool ->
  width:int -> height:int -> scene -> (t, string) result
(** [round_samples] (default 4) is the number of probe rays per camera-visible
    shading point for round-corner materials; secondary bounces use a quarter.
    [bsdf_only] (default false) disables all light sampling; [profile] logs each GPU band. *)

(** Accepts a perspective camera with no lens offset, forced aspect, or
    vertical flip. Other projections return an error. The camera's
    [Rays.Camera.lens] is thin-lens depth of field: every accumulated
    sample starts from a random point of the lens disk and aims at the
    focus plane, so bokeh converges with the rest of the image at no extra
    per-sample cost; preview frames stay pinhole. Publishes the previous
    frame's pixels to [image], then submits a new frame
    without waiting for it (one frame of latency), as row bands the
    window's own drawing interleaves with. A frame whose camera
    differs from the previous call, or the first frame over newly installed
    geometry ({!replace_mesh}, {!queue_mesh}, {!move}), lights, or World, is
    an interactive preview: full-resolution
    primary visibility, direct lighting, one round-corner probe, temporal
    reprojection with disocclusion rejection, and an edge-aware spatial
    resolve. Progressive accumulation restarts as soon as the camera rests and edits
    stop. *)
val render : t -> Rays.Camera.t -> (unit, string) result

val replace_mesh : t -> mesh -> (unit, string) result
(** Synchronously builds and swaps scene geometry, then restarts accumulation. *)

val queue_mesh : t -> mesh -> (unit, string) result
(** Starts a replacement build without waiting for the GPU. [render] continues
    showing the old scene until the new structure is complete. A newer queued
    mesh supersedes one still building; at most one build and one queued mesh
    are retained. [flush] waits for the latest replacement. *)

val move : t -> Rays_math.Mat4.t list -> (unit, string) result
(** Re-places the objects of the newest mesh given to the tracer, which must
    be a {!scene_mesh} of as many objects as matrices, at new world
    transforms (in input order), without re-preparing or re-uploading
    geometry: only the instance records and the instance structure are
    rebuilt, over the existing primitive structures. Like {!queue_mesh} it
    does not wait: the moved scene shows once its build completes, then
    accumulation restarts; a newer [move] supersedes a pending one. *)

val flush : t -> (unit, string) result
(** Publishes the in-flight frame and waits for queued geometry;
    [render] then [flush] is a
    synchronous render. [render] itself never blocks: while the GPU is still
    busy with the previous frame it returns without submitting, so the
    caller's loop keeps its own frame rate. *)

val set_world : t -> Rays.World.baked option -> (unit, string) result
(** Light and frame the scene with a baked World instead of the procedural
    [environment] (whose sky, ground and panels are then ignored); [None]
    restores it. Uploads the maps and CDF only when the value changes
    (physical equality), then restarts accumulation. With a World:
    camera rays that miss show the camera map, or [Color c] as radiance [c],
    or for [Transparent] nothing, with the film's alpha holding coverage
    (straight alpha; the RGBA8 film always has alpha). Later bounces see the
    lighting map, sampled through its CDF and the sun cone with multiple
    importance sampling. [baked.lights] join the {!set_lights} lights as
    one-sided rects, and every rect light becomes visible to rays (camera,
    mirrors) instead of shadow rays only. The effective exposure is
    [2 ** baked.exposure] times [create]'s [exposure]. *)

val set_lights : t -> light list -> (unit, string) result
(** Replaces the scene's rect lights and restarts accumulation. Rect only:
    approximate a point or spot light with a small rect facing its target. *)

val reset : t -> unit
(** Restarts accumulation; the next frame renders at full quality even
    right after a scene edit. *)

val samples : t -> int
(** Accumulated samples per pixel. *)

val resize : t -> width:int -> height:int -> (unit, string) result
(** Match a new viewport: waits for the in-flight frame, reallocates the film,
    and restarts accumulation. Scene geometry is kept. No-op at the same size. *)

val size : t -> int * int
val image : t -> Rays.Image.t
(** Borrowed; destroyed by [destroy]. In a native Sketch, Scene samples its
    completed GPU film directly. *)

val destroy : t -> unit

module Private : sig
  val max_total_threads : t -> (int, string) result
  (** The current mesh shape's pathtrace pipeline thread limit (the Lisp kernel's register-pressure reference). *)
end
