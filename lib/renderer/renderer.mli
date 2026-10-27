(** Isolated native Metal frame coordinator.  The public values deliberately
    contain no SDL or native handles. *)

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

type family = Scene_execution.pipeline_family = Scene2 | Scene2_textured | Scene3 | Scene3_points | Scene3_textured | Scene3_world | Ui | Marks
type blend = Ogpu.Pipeline.blend = Replace | Alpha | Add | Multiply | Screen | Subtract
type draw = Scene_execution.sampled_draw
type resource = Image of Runtime_resources.Image.t |
  Text of Runtime_resources.Text.t | Canvas of Runtime_resources.Canvas.t

(** Adopt an already prepared draw without exposing it again. Scene3 values are
    retained as a distinct family and never misrouted through a Scene2 pipeline. *)
val prepared_draw : family:family -> ?blend:blend ->
  ?texture:Scene_execution.sampled_texture ->
  ?auxiliary:Scene_execution.auxiliary_resource -> ?vertex_attributes:(string * bytes) -> ?samples:int ->
  Scene_execution.geometry_draw -> draw

type t
val create : configuration -> (t,error) result
(* A true layerless native Metal target. It owns one long-lived device and
   pipeline coordinator until [destroy], without acquiring/presenting a window. *)
val create_offscreen : configuration -> (t,error) result
val draw : ?clear:(float * float * float * float) -> t -> draw list ->
  (bool,error) result
val lower_scene2 : t -> density:int -> resource:(int -> resource option) ->
  Scene_command.Render_ir.t -> (draw list,error) result
type stats = { logical_draws:int64; logical_passes:int64; pipeline_cache_entries:int;
  uploaded_bytes:int64; gpu_duration_seconds:float; retained_plan_hits:int64;
  retained_plan_misses:int64; sun_shadow_passes:int64 }
val zero_stats : stats
val stats : t -> (stats,error) result
type presentation_facts = Runtime.presentation_facts
val capture : t -> (bytes,error) result
val read_pixels : t -> bytes_per_row:int -> (bytes,error) result

(** An offscreen execution's completed frame texture, on the device it leased
    (the presenting window's when one exists). A window execution samples a
    Canvas published from it directly; readback happens only for CPU access. *)
val offscreen_target : t -> (Ogpu.Backend.texture,error) result

(** A window execution fails with [Invalid_argument], destroying nothing,
    while GPU leases or offscreen executions still share its device. *)
val destroy : t -> (unit,error) result

(** A borrowed GPU: the active window's OGPU device (so Scene samples GPU
    film textures directly) or, without a window, a lazily created headless
    device shared by all leases. Every lease owns its own queue. Release it
    while SDL is alive; the headless device goes away with the last lease. *)
type gpu
val acquire_gpu : unit -> (gpu,error) result
val gpu_device : gpu -> Ogpu.Backend.device
val gpu_queue : gpu -> Ogpu.Backend.queue

(** True when the device is the presenting window's. *)
val gpu_shared : gpu -> bool
val release_gpu : gpu -> unit
module Private : sig
  (** The borrowed SDL/runtime shell; its lifetime remains owned by [t]. *)
  val runtime : t -> Runtime.t
  type scaled_cache
  val new_scaled_cache : unit -> scaled_cache
  val scale_draws : Runtime.frame_facts -> Scene_execution.geometry_draw list -> Scene_execution.geometry_draw list
  val scale_sampled_resources : Runtime.frame_facts -> Scene_execution.sampled_draw list -> Scene_execution.sampled_draw list
  val scale_sampled_cached : scaled_cache -> Runtime.frame_facts -> Scene_execution.sampled_draw list -> Scene_execution.sampled_draw list
  (** A [Marks]-family source for one mark set ([specification/marks.md]): a token for a
      [Shapes] command that draws [count] instances through the six attribute buffers. Each
      attribute is [Host (bytes, stride)], uploaded to a buffer the source owns (float32
      positions, extents and widths, [0xRRGGBBAA] words for fill and stroke; stride 0 is one
      constant), or [Device (source, stride)], another producer's buffer bound directly. The
      attributes, in order, are position, extent, fill, stroke and width; [kind] is the
      [Ui_batch] kind word (rect 0, circle 4, line 5). Host bytes are borrowed and must not
      change. The token stays valid while every device [source] returns the buffer it returned
      here, and the same token comes back while nothing changed; any change gives a new stamp
      that invalidates the previous token. Close the source before releasing its GPU lease.
      With [indirect (source, offset)] the set is drawn by an indexed indirect draw from the
      five uints the producer's device wrote at [offset] of [source]'s buffer (index count
      six per drawn instance, at most [6 * count]); [source] joins the device inputs the
      token's validity follows. *)
  type gpu_marks
  val gpu_vertices_owner : t -> Scene_execution.Private.gpu_vertices_owner
  val create_offscreen_with_gpu : gpu -> configuration -> (t,error) result
  type marks_source=Host of bytes*int|Device of (unit->Ogpu.Backend.buffer option)*int
  val create_gpu_marks : gpu -> (gpu_marks,error) result
  val gpu_marks : ?indirect:((unit->Ogpu.Backend.buffer option)*int64) -> gpu_marks -> kind:int -> count:int -> marks_source array ->
    (Scene_command.Marks.gpu_token,error) result
  val close_gpu_marks : gpu_marks -> unit
  val snapshot_count_for_test : t -> int
  val window_handle : t -> Sdl3.Window.t option
  type submission
  type batch
  val encoded_draws_for_test : batch -> int
  (* Draws the renderer encodes for the batch after neighbours merge ([-1] on error). *)

  (* Starts an isolated zero-copy Scene2 lowering transaction.  A later
      lowering or step failure releases every image snapshot owned by this
      submission without affecting another active submission. *)
  val begin_submission : t -> (submission,error) result
  val lower_scene2 : submission -> density:int ->
    resource:(int -> resource option) -> Scene_command.Render_ir.t ->
    (batch,error) result
  val lower_scene2_segment : submission -> identity:int64 -> version:int64 ->
    cacheable:bool -> density:int -> resource:(int -> resource option) ->
    Scene_command.Render_ir.t -> (batch,error) result
  (* Lowers PXUI instances into one [Ui] draw per batch. Texture ids resolve
     through [resource]; id 0 samples a white texel. *)
  val lower_ui : submission -> density:int ->
    resource:(int -> resource option) -> Scene_command.Ui_batch.t ->
    (batch,error) result
  (* Adopts already prepared non-Scene2 draws into this submission. *)
  val adopt_draws : submission -> draw list -> (batch,error) result
  (* Adopts a 3D layer's draws, retaining the layer's render once it keeps the same [layer]
     identity on consecutive frames (see the retained views in the implementation).  A
     window execution only; offscreen ones adopt the draws directly. *)
  val adopt_retained_view : submission -> density:int -> layer:Scene_execution.prepared_scene3 ->
    draw list -> (batch,error) result
  (* Consumes the submission on either success or failure. *)
  val step : ?clear:(float * float * float * float) -> ?identity:string ->
    ?version:int64 -> submission ->
    batch list -> (unit,error) result
  val replay : ?clear:(float * float * float * float) -> identity:string ->
    version:int64 -> t -> (unit option,error) result
  (* Idempotently releases an unsubmitted transaction. *)
  val cancel : submission -> unit
  val retained_scene2_segment_stats : t -> int * int64 * int64
end
