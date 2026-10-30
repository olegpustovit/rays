type t
type gpu_vertices_owner

val device : t -> Ogpu.Backend.device
val queue : t -> Ogpu.Backend.queue

val target : t -> Ogpu.Backend.texture
(** The owned RGBA8 texture every submission renders into; a shared-device renderer's callers may
    sample it directly between completed submissions. *)

type mesh_key =
  | Host of string
  | Gpu_vertices of string
      (** [Gpu_vertices name]: a stream borrowed through [register_gpu_vertices ~key:name]. *)

val mesh_key_string : mesh_key -> string

type mesh = {
  key : mesh_key;
  vertices : bytes;
  vertex_count : int;
  indices : bytes;
  index_count : int;
  primitive : Ogpu.Render_pass.primitive;
}

type state = {
  viewport : int * int * int * int;
  scissor : int * int * int * int;
  cull : Ogpu.Render_pass.cull;
  depth_compare : Ogpu.Render_pass.comparison;
  depth_write : bool;
  depth_load : Ogpu.Render_pass.load;
  depth_clear : float;
  transform_uniforms : bytes option;
}

type geometry_draw = { mesh : mesh; state : state }

type pipeline_family =
  | Scene2
  | Scene2_textured
  | Scene3
  | Scene3_points
  | Scene3_textured
  | Scene3_world
  | Ui
  | Marks
      (** Exact number of family/blend variants required for each supported sample count. Cache
          owners use this value so adding a family cannot silently evict a still-live pipeline
          during renderer construction. *)

(* Every level holds [width * height] texels of 4, 8 or 16 bytes (the size
    level 0 implies): [Rgba8_unorm], [Rgba16_float] or [Rgba32_float]. *)
type texture_level = { width : int; height : int; bytes : bytes }

type sampled_texture = {
  key : string;
  levels : texture_level array;
  sampler : Ogpu.Types.sampler_descriptor;
  gpu : Ogpu.Backend.texture option;
}

type auxiliary_resource = {
  key : string;
  buffer : bytes;
  texture : sampled_texture;
  environment : sampled_texture option;
      (** A [Scene3_world] draw's camera map (texture 9, sampler 10). *)
  sun_shadow : string option;
      (** [Some key]: this World block (sun view-projection at words 40..55) samples the renderer's
          sun map at texture 8, rendered from the frame's depth-writing [Replace]/[Alpha] World
          triangle draws with the same key whenever the key differs from the last rendered one. *)
}

type sampled_draw = {
  family : pipeline_family;
  blend : Ogpu.Pipeline.blend;
  texture : sampled_texture option;
  auxiliary : auxiliary_resource option;
  vertex_attributes : (string * bytes) option;
      (** Independently retained Scene3 color/UV stream (12 bytes per vertex), bound at vertex
          buffer 11. Required (and only valid) for the Scene3 families; 2D, UI and Marks draws carry
          [None]. *)
  samples : int;
  draw : geometry_draw;
}

type scene3_entry = sampled_draw
type prepared_scene3 = { entries : scene3_entry array }

val prepare_scene3 :
  clear:float * float * float * float ->
  clear_depth:float ->
  scene3_entry array ->
  (prepared_scene3, Ogpu.Error.t) result
(** Validates and copies a native Scene3 submission description. Nested byte resources remain
    borrowed and must be retained by the staging owner. *)

val create :
  ?device:Ogpu.Backend.device ->
  ?gpu_vertices:gpu_vertices_owner ->
  ?sun_depth:(Ogpu.Backend.device -> (Ogpu.Backend.pipeline, Ogpu.Error.t) result) ->
  offscreen:bool ->
  Ogpu.Backend.driver ->
  Ogpu.Surface.configuration ->
  (Ogpu.Backend.device ->
  pipeline_family ->
  Ogpu.Pipeline.blend ->
  int ->
  (Ogpu.Backend.pipeline, Ogpu.Error.t) result) ->
  (t, Ogpu.Error.t) result
(** Creates a renderer with one pipeline per family, blend, and supported sample count. The factory
    receives the renderer's device and returns owned OGPU render pipelines (see
    [Ogpu.Backend.create_render_pipeline]); the renderer destroys them. Scene2 draws run
    marks.rays' geometry pipeline (a vertex a float2 position and colour word) and Scene2_textured
    its quad pipeline (a quad's 80 bytes of constants its vertices), which binds its texture and
    sampler through a fragment argument buffer at index 7; both are created with [~indirect:true].

    With [~offscreen:true] the renderer owns a texture target without creating, acquiring, or
    presenting a platform surface. Submissions complete before the call returns, so exact readback
    and explicit destruction have the same contract as surface-backed execution. With [?device], the
    renderer borrows that device (the driver is unused) and never destroys it, so a Canvas can share
    the presenting window's GPU. [?sun_depth] makes the pipeline of the World sun map pass (RGBA8
    color, Depth32 depth, one sample), created on first use; without it a sun-shadowed World draw is
    [Unsupported]. *)

val submit :
  ?prepared:string * int64 ->
  ?after_prepare:(unit -> unit) ->
  ?clear:float * float * float * float ->
  t ->
  sampled_draw list ->
  (bool, Ogpu.Error.t) result

val replay_prepared_sampled_resources :
  ?clear:float * float * float * float ->
  identity:string ->
  version:int64 ->
  t ->
  ((bool * int) option, Ogpu.Error.t) result

val resize : t -> Ogpu.Surface.configuration -> (unit, Ogpu.Error.t) result
val upload_bytes : t -> int64

val sun_shadow_passes : t -> int64
(** Sun map passes rendered so far (see [auxiliary_resource.sun_shadow]). *)

val sun_map_size : int
(** Width and height of the sun map. *)

module Private : sig
  type gpu_vertices
  type nonrec gpu_vertices_owner = gpu_vertices_owner

  val create_gpu_vertices_owner : unit -> gpu_vertices_owner
  val gpu_vertices_owner : t -> gpu_vertices_owner

  val register_gpu_vertices :
    gpu_vertices_owner ->
    ?extra:Ogpu.Backend.buffer list ->
    ?stride:int ->
    ?indirect:Ogpu.Backend.buffer * int64 ->
    key:string ->
    device:Ogpu.Backend.device ->
    buffer:Ogpu.Backend.buffer ->
    vertex_count:int ->
    valid:(unit -> bool) ->
    unit ->
    (gpu_vertices, Ogpu.Error.t) result
  (** Borrow a producer-owned 64-byte UI instance stream under a [Gpu_vertices] mesh key.
      [vertex_count] is four per instance. A source with [extra] buffers (a Marks mark set: [buffer]
      at binding 0, [extra] at 1, 2, ...; exactly five for [Marks], none for [Ui]) is not
      size-checked against the 64-byte layout. The producer must close registrations before
      destroying/reusing buffers; [valid] also guards every retained replay. At most 128 are live.
      With [indirect] [(buffer, offset)] the shape is drawn by [draw_indexed_indirect] from the five
      uints the producer's device wrote there, an index count of at most the registration's; such a
      draw is never batched or recorded. A [Scene3] or [Scene3_points] draw of a device's packed
      float3 positions (empty [vertices], host indices and attribute record, uniform word 79 set)
      registers them as [buffer] with [stride] 12 and its normals as the one [extra] buffer, or none
      (word 80 clear: the normal is (0, 0, 1)). *)

  val unregister_gpu_vertices : gpu_vertices -> unit

  val find_gpu_vertices :
    gpu_vertices_owner -> string -> (Ogpu.Backend.buffer * Ogpu.Backend.buffer list * int) option
  (** A live, valid registration's buffer, extra buffers and vertex count (initial domain). *)

  val gpu_vertex_count_for_test : gpu_vertices_owner -> int
  val cache_count_for_report : t -> int
  val coalesced_draws_for_test : sampled_draw list -> int
  (* The number of draws the renderer encodes for these, after merging neighbours; -1 on error. *)
end

type retained_stats = { plan_hits : int64; plan_misses : int64 }
(** Indirect-command replay plans: one automatic plan admitted after two identical frames and one
    identity-keyed prepared plan. *)

val retained_stats : t -> retained_stats
val pipeline_count : t -> int
val read_pixels : t -> bytes_per_row:int -> (bytes, Ogpu.Error.t) result
val destroy : t -> (unit, Ogpu.Error.t) result
