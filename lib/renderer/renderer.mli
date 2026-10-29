(** Isolated native Metal frame coordinator. The public values deliberately contain no SDL or native
    handles. *)

module Scene_execution = Scene_execution

type error = Ogpu.Error.t

val pp_error : Format.formatter -> error -> unit

type configuration = {
  logical_width : int;
  logical_height : int;
  drawable_width : int;
  drawable_height : int;
  title : string;
  vsync : bool;
  high_density : bool;  (** a drawable at the display's native density (false: the logical size) *)
}

val default_configuration : configuration

type family = Scene_execution.pipeline_family =
  | Scene2
  | Scene2_textured
  | Scene3
  | Scene3_points
  | Scene3_textured
  | Scene3_world
  | Ui
  | Marks

type blend = Ogpu.Pipeline.blend = Replace | Alpha | Add | Multiply | Screen | Subtract
type draw = Scene_execution.sampled_draw

type resource =
  | Image of Runtime_resources.Image.t
  | Text of Runtime_resources.Text.t
  | Canvas of Runtime_resources.Canvas.t

val prepared_draw :
  family:family ->
  ?blend:blend ->
  ?texture:Scene_execution.sampled_texture ->
  ?auxiliary:Scene_execution.auxiliary_resource ->
  ?vertex_attributes:string * bytes ->
  ?samples:int ->
  Scene_execution.geometry_draw ->
  draw
(** Adopt an already prepared draw without exposing it again. Scene3 values are retained as a
    distinct family and never misrouted through a Scene2 pipeline. *)

type t

val create : configuration -> (t, error) result

(* A true layerless native Metal target. It owns one long-lived device and
   pipeline coordinator until [destroy], without acquiring/presenting a window. *)
val create_offscreen : configuration -> (t, error) result
val draw : ?clear:float * float * float * float -> t -> draw list -> (bool, error) result

val lower_scene2 :
  t ->
  density:int ->
  resource:(int -> resource option) ->
  Scene_command.Render_ir.t ->
  (draw list, error) result

type stats = {
  logical_draws : int64;
  logical_passes : int64;
  pipeline_cache_entries : int;
  uploaded_bytes : int64;
  gpu_duration_seconds : float;
  retained_plan_hits : int64;
  retained_plan_misses : int64;
  sun_shadow_passes : int64;
}

val zero_stats : stats
val stats : t -> (stats, error) result

type presentation_facts = Runtime.presentation_facts

val capture : t -> (bytes, error) result
val read_pixels : t -> bytes_per_row:int -> (bytes, error) result

val offscreen_target : t -> (Ogpu.Backend.texture, error) result
(** An offscreen execution's completed frame texture, on the device it leased (the presenting
    window's when one exists). A window execution samples a Canvas published from it directly;
    readback happens only for CPU access. *)

val destroy : t -> (unit, error) result
(** A window execution fails with [Invalid_argument], destroying nothing, while GPU leases or
    offscreen executions still share its device. *)

type gpu
(** A borrowed GPU: the active window's OGPU device (so Scene samples GPU film textures directly)
    or, without a window, a lazily created headless device shared by all leases. Every lease owns
    its own queue. Release it while SDL is alive; the headless device goes away with the last lease.
*)

val acquire_gpu : unit -> (gpu, error) result
val gpu_device : gpu -> Ogpu.Backend.device
val gpu_queue : gpu -> Ogpu.Backend.queue

val gpu_shared : gpu -> bool
(** True when the device is the presenting window's. *)

val release_gpu : gpu -> unit

type gpu_marks
type marks_source = Host of bytes * int | Device of (unit -> Ogpu.Backend.buffer option) * int

val create_gpu_marks : gpu -> (gpu_marks, error) result
(** A [Marks]-family source for one mark set (specification/marks.md): [gpu_marks] makes a token for
    a [Shapes] command drawing [count] instances through the five attribute buffers (position,
    extent, fill, stroke, width; [kind] is the [Ui_batch] kind word). Each is
    [Host (bytes, stride)], borrowed and unchanged, or [Device (source, stride)], another producer's
    buffer. The token stays valid while every device source returns the same buffer; any change
    gives a new stamp. With [indirect (source, offset)] the set is drawn by an indexed indirect draw
    from five uints at [offset]. Close the source before releasing its GPU lease. *)

val gpu_marks :
  ?indirect:(unit -> Ogpu.Backend.buffer option) * int64 ->
  gpu_marks ->
  kind:int ->
  count:int ->
  marks_source array ->
  (Scene_command.Marks.gpu_token, error) result

val close_gpu_marks : gpu_marks -> unit

val register_gpu_positions :
  gpu ->
  positions:Ogpu.Backend.buffer ->
  ?normals:Ogpu.Backend.buffer ->
  count:int ->
  valid:(unit -> bool) ->
  unit ->
  (string * (unit -> unit), error) result
(** Borrows another producer's [count] packed float3 positions (and normals) for a Scene3 mesh drawn
    from the device: the key a device mesh names ({!Rays.Mesh.View.create_device}) and its
    unregistration (initial domain). The producer keeps the buffers while [valid]; a draw of a
    registration no longer valid is refused as stale. At most 128 borrowed sources are live. *)

module Native : sig
  val runtime : t -> Runtime.t
  (** The borrowed SDL/runtime shell; its lifetime remains owned by [t]. *)

  type scaled_cache

  val new_scaled_cache : unit -> scaled_cache

  val scale_draws :
    Runtime.frame_facts -> Scene_execution.geometry_draw list -> Scene_execution.geometry_draw list

  val scale_sampled_resources :
    Runtime.frame_facts -> Scene_execution.sampled_draw list -> Scene_execution.sampled_draw list

  val scale_sampled_cached :
    scaled_cache ->
    Runtime.frame_facts ->
    Scene_execution.sampled_draw list ->
    Scene_execution.sampled_draw list

  val gpu_vertices_owner : t -> Scene_execution.Private.gpu_vertices_owner
  val create_offscreen_with_gpu : gpu -> configuration -> (t, error) result
  val snapshot_count_for_test : t -> int
  val window_handle : t -> Sdl3.Window.t option

  type submission
  type batch

  val encoded_draws_for_test : batch -> int
  (* Draws the renderer encodes for the batch after neighbours merge ([-1] on error). *)

  (* Starts an isolated zero-copy Scene2 lowering transaction.  A later
      lowering or step failure releases every image snapshot owned by this
      submission without affecting another active submission. *)
  val begin_submission : t -> (submission, error) result

  val lower_scene2 :
    submission ->
    density:int ->
    resource:(int -> resource option) ->
    Scene_command.Render_ir.t ->
    (batch, error) result

  val lower_scene2_segment :
    submission ->
    identity:int64 ->
    version:int64 ->
    cacheable:bool ->
    density:int ->
    resource:(int -> resource option) ->
    Scene_command.Render_ir.t ->
    (batch, error) result

  (* Lowers PXUI instances into one [Ui] draw per batch. Texture ids resolve
     through [resource]; id 0 samples a white texel. *)
  val lower_ui :
    submission ->
    density:int ->
    resource:(int -> resource option) ->
    Scene_command.Ui_batch.t ->
    (batch, error) result

  (* Adopts already prepared non-Scene2 draws into this submission. *)
  val adopt_draws : submission -> draw list -> (batch, error) result

  (* Adopts a 3D layer's draws, retaining the layer's render once it keeps the same [layer]
     identity on consecutive frames (see the retained views in the implementation).  A
     window execution only; offscreen ones adopt the draws directly. *)
  val adopt_retained_view :
    submission ->
    density:int ->
    layer:Scene_execution.prepared_scene3 ->
    draw list ->
    (batch, error) result

  (* Consumes the submission on either success or failure. *)
  val step :
    ?clear:float * float * float * float ->
    ?identity:string ->
    ?version:int64 ->
    submission ->
    batch list ->
    (unit, error) result

  val replay :
    ?clear:float * float * float * float ->
    identity:string ->
    version:int64 ->
    t ->
    (unit option, error) result

  (* Idempotently releases an unsubmitted transaction. *)
  val cancel : submission -> unit
  val retained_scene2_segment_stats : t -> int * int64 * int64
end
