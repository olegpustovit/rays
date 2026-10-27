type token = int64
type receipt = { epoch:int64 }
type driver_resource = { token:token; write:int64 -> bytes -> (unit,Error.t) result;
  read:int64 -> int -> (bytes,Error.t) result;
  read_into:int64 -> bytes -> int -> int -> (unit,Error.t) result;
  destroy:unit -> (unit,Error.t) result }

(** An intersection function table created from a pipeline with linked
    functions: the functions and their argument buffers. *)
type driver_table =
  { table_token:token
  ; table_set_function:index:int -> string -> (unit,Error.t) result
  ; table_set_buffer:index:int -> token -> offset:int64 -> (unit,Error.t) result
  ; destroy_table:unit -> (unit,Error.t) result }
type driver_pipeline =
  { pipeline_token:token
  ; create_table:capacity:int -> (driver_table,Error.t) result
  ; destroy_pipeline:unit -> (unit,Error.t) result }
type driver_frame = { frame_token:token }

(** Accumulated GPU execution time reported by the driver for this queue's device. *)
type gpu_timing = {  gpu_seconds:float }
type driver_surface =
  { configure:Surface.configuration -> (unit,Error.t) result
  ; acquire_sync:unit -> ([ `Acquired of driver_frame | `Timeout | `Occluded | `Device_lost ],Error.t) result
  ; discard:driver_frame -> (unit,Error.t) result
  ; destroy_surface:unit -> (unit,Error.t) result }
type driver_library =
  { create_compute_pipeline_in:entry:string -> constants:(string * Shader.constant_value) list ->
      interface:Shader.binding list -> linked:string list ->
      (driver_pipeline,Error.t) result
  ; destroy_library:unit -> (unit,Error.t) result }
type accel_sizes = { structure_size:int64; build_scratch_size:int64 }
type driver_keyframe = token * int64
type driver_geometry =
  | Driver_triangles of { vertices:token; offset:int64; length:int64; vertex_stride:int; vertex_count:int }
  | Driver_motion_triangles of { keyframes:driver_keyframe array; vertex_stride:int; vertex_count:int }
  | Driver_boxes of { boxes:driver_keyframe array; stride:int; count:int; opaque:bool; duplicate:bool; table_offset:int }
  | Driver_curves of { control:driver_keyframe array; control_stride:int; control_count:int
                     ; radii:driver_keyframe array; radius_stride:int; indices:token; index_offset:int64
                     ; segment_count:int; per_segment:int; curve_type:int; basis:int; caps:int }
type driver_motion =
  { motion_keyframes:int; motion_start:float; motion_end:float; motion_start_border:int; motion_end_border:int }
type instance_kind = Acceleration.instance_kind = User_id_instances | Motion_instances
type driver_accel_descriptor =
  | Driver_blas of { geometries:driver_geometry array; motion:driver_motion option }
  | Driver_tlas of { instances:token; offset:int64; instance_count:int; kind:instance_kind
                   ; structures:token array; motion_transforms:(token * int64 * int) option }
  | Driver_sized of { size:int64; template:token }
type driver_accel = { accel_token:token; accel_sizes:accel_sizes; destroy_accel:unit -> (unit,Error.t) result }

(** One TLAS instance: a row-major 3×4 affine transform (12 floats), a
    visibility [mask] (32 bits), and the index of its bottom-level structure
    in the TLAS descriptor's [structures]. Packed by the driver into its own
    native layout through [pack_instances]. *)
type instance = { transform:float array; mask:int; structure_index:int }
type driver_compute_encoder =
  { set_pipeline:token -> (unit,Error.t) result
  ; set_buffer:index:int -> offset:int64 -> token -> (unit,Error.t) result
  ; set_bytes:index:int -> bytes -> (unit,Error.t) result
  ; set_texture:index:int -> token -> (unit,Error.t) result
  ; set_accel:index:int -> token -> (unit,Error.t) result
  ; set_table:index:int -> token -> (unit,Error.t) result
  ; dispatch_threads:threads:int * int * int -> threadgroup:int * int * int -> (unit,Error.t) result
  ; compute_use_accels:token list -> (unit,Error.t) result
  ; end_compute:unit -> (unit,Error.t) result }
type driver_accel_encoder =
  { build:token -> scratch:token -> scratch_offset:int64 -> (unit,Error.t) result
  ; compact:src:token -> dst:token -> (unit,Error.t) result
  ; write_compacted_size:token -> dst:token -> offset:int64 -> (unit,Error.t) result
  ; end_accel:unit -> (unit,Error.t) result }
type driver_blit_encoder =
  { buffer_to_texture:src:token -> offset:int64 -> bytes_per_row:int64 -> bytes_per_image:int64 -> dst:token -> mip:int -> origin:Types.origin -> extent:Types.extent -> (unit,Error.t) result
  ; end_blit:unit -> (unit,Error.t) result }
type shader_stage = Vertex | Fragment
type winding = Clockwise | Counter_clockwise
type depth_state = { depth_compare:Render_pass.comparison; depth_write:bool; stencil:Render_pass.stencil_state option }
type driver_color_attachment = { color:token; color_resolve:token option; color_load:Render_pass.load; color_store:Render_pass.store; color_clear:float * float * float * float }
type driver_depth_attachment = { depth:token; depth_load:Render_pass.load; depth_store:Render_pass.store; depth_clear:float }
type driver_stencil_attachment = { stencil:token; stencil_load:Render_pass.load; stencil_store:Render_pass.store; stencil_clear:int }
type driver_render_target = { target_colors:driver_color_attachment array; target_depth:driver_depth_attachment option; target_stencil:driver_stencil_attachment option; target_width:int; target_height:int; target_samples:int }
type driver_batch_draw = { batch_pipeline:token; batch_buffers:(shader_stage * int * token * int64) array; batch_primitive:Render_pass.primitive; batch_index:(Render_pass.index_type * token * int64 * int64) option; batch_vertex_start:int; batch_vertex_count:int; batch_instances:int }
type driver_render_encoder =
  { render_set_pipeline:token -> (unit,Error.t) result
  ; set_stage_buffer:shader_stage -> index:int -> offset:int64 -> token -> (unit,Error.t) result
  ; set_stage_texture:shader_stage -> index:int -> token -> (unit,Error.t) result
  ; set_stage_sampler:shader_stage -> index:int -> token -> (unit,Error.t) result
  ; set_viewport:Render_pass.rect -> (unit,Error.t) result
  ; set_scissor:Render_pass.rect -> (unit,Error.t) result
  ; set_cull:Render_pass.cull -> (unit,Error.t) result
  ; set_winding:winding -> (unit,Error.t) result
  ; set_depth_state:depth_state option -> (unit,Error.t) result
  ; set_stencil_reference:front:int32 -> back:int32 -> (unit,Error.t) result
  ; draw:primitive:Render_pass.primitive -> first:int -> count:int -> instances:int -> (unit,Error.t) result
  ; draw_indexed:primitive:Render_pass.primitive -> index_type:Render_pass.index_type -> token -> offset:int64 -> count:int64 -> instances:int -> (unit,Error.t) result
  ; draw_indexed_indirect:primitive:Render_pass.primitive -> index_type:Render_pass.index_type -> token -> offset:int64 -> token -> arguments_offset:int64 -> (unit,Error.t) result
  ; draw_batch:driver_batch_draw array -> (unit,Error.t) result
  ; use_resources:token list -> (unit,Error.t) result
  ; execute_icb:token -> location:int -> length:int -> (unit,Error.t) result
  ; end_render:unit -> (unit,Error.t) result }
type driver_sampler = { sampler_token:token; destroy_sampler:unit -> (unit,Error.t) result }
type driver_icb =
  { icb_token:token
  ; icb_set_pipeline:index:int -> token -> (unit,Error.t) result
  ; icb_set_buffer:index:int -> shader_stage -> slot:int -> offset:int64 -> token -> (unit,Error.t) result
  ; icb_draw:index:int -> primitive:Render_pass.primitive -> first:int -> count:int -> instances:int -> (unit,Error.t) result
  ; icb_draw_indexed:index:int -> primitive:Render_pass.primitive -> index_type:Render_pass.index_type -> token -> offset:int64 -> count:int64 -> instances:int -> (unit,Error.t) result
  ; destroy_icb:unit -> (unit,Error.t) result }
type driver_argument =
  { argument_length:int; argument_alignment:int
  ; argument_texture:buffer:token -> offset:int64 -> slot:int -> token -> (unit,Error.t) result
  ; argument_sampler:buffer:token -> offset:int64 -> slot:int -> token -> (unit,Error.t) result
  ; destroy_argument:unit -> (unit,Error.t) result }
type render_pipeline_options = { blend:Pipeline.blend; topology:Render_pass.primitive; indirect:bool }
type driver_commands =
  { compute_encoder:unit -> (driver_compute_encoder,Error.t) result
  ; accel_encoder:unit -> (driver_accel_encoder,Error.t) result
  ; blit_encoder:unit -> (driver_blit_encoder,Error.t) result
  ; render_encoder:driver_render_target -> (driver_render_encoder,Error.t) result
  ; commit:unit -> (receipt,Error.t) result
  ; commit_present:source:token -> driver_frame -> (receipt,Error.t) result
  ; abandon:unit -> (unit,Error.t) result }

type driver_queue =
  { complete_through:int64 -> (unit,Error.t) result
  ; poll_through:int64 -> (bool,Error.t) result
  ; completed_epoch:unit -> int64
  ; begin_commands:unit -> (driver_commands,Error.t) result
  ; gpu_duration:int64 -> float option
  ; gpu_timing:unit -> gpu_timing
  ; destroy_queue:unit -> (unit,Error.t) result }
type driver_device =
  { device_handle:Handle.device; capabilities:Caps.t
  ; create_buffer:Types.memory -> Types.buffer_descriptor -> (driver_resource,Error.t) result
  ; create_texture:Types.texture_descriptor -> (driver_resource,Error.t) result
  ; create_depth_texture:Types.texture_descriptor -> (driver_resource,Error.t) result
  ; create_stencil_texture:Types.texture_descriptor -> (driver_resource,Error.t) result
  ; create_library:Shader.t -> (driver_library,Error.t) result
  ; create_accel:driver_accel_descriptor -> (driver_accel,Error.t) result
  ; instance_layout:instance_kind -> Acceleration.instance_layout
  ; create_sampler:Types.sampler_descriptor -> (driver_sampler,Error.t) result
  ; create_render_pipeline:render_pipeline_options -> Pipeline.render_descriptor -> (driver_pipeline,Error.t) result
  ; create_icb:max_commands:int -> (driver_icb,Error.t) result
  ; create_argument:pipeline:token -> shader_stage -> index:int -> (driver_argument,Error.t) result
  ; create_queue:unit -> (driver_queue,Error.t) result
  ; create_surface:Surface.configuration -> (driver_surface,Error.t) result
  ; destroy_device:unit -> (unit,Error.t) result }
type driver = { create_device:unit -> (driver_device,Error.t) result }

type device
type buffer
type texture
type pipeline
type queue
type surface
type frame

val create_device : driver -> (device,Error.t) result
val capabilities : device -> Caps.t
val device_handle : device -> Handle.device
val create_buffer : ?memory:Types.memory -> device -> Types.buffer_descriptor -> (buffer,Error.t) result
val create_texture : device -> Types.texture_descriptor -> (texture,Error.t) result
val create_depth_texture : device -> Types.texture_descriptor -> (texture,Error.t) result
val create_stencil_texture : device -> Types.texture_descriptor -> (texture,Error.t) result
val create_queue : device -> (queue,Error.t) result
val create_surface : device -> Surface.configuration -> (surface,Error.t) result
val buffer_id : buffer -> int64
val texture_id : texture -> int64
val write_buffer : buffer -> offset:int64 -> bytes -> (unit,Error.t) result
val read_buffer : buffer -> offset:int64 -> length:int -> (bytes,Error.t) result
val read_texture : texture -> bytes_per_row:int -> (bytes,Error.t) result
val read_texture_into : texture -> bytes_per_row:int -> destination:bytes ->
  (unit,Error.t) result
val complete_through : queue -> int64 -> (unit,Error.t) result
val poll_through : queue -> int64 -> (bool,Error.t) result
val completed_epoch : queue -> int64
val configure : surface -> Surface.configuration -> (unit,Error.t) result
val acquire_sync : surface -> ([ `Acquired of frame | `Timeout | `Occluded | `Device_lost ],Error.t) result
val discard : frame -> (unit,Error.t) result
val destroy_buffer : buffer -> (unit,Error.t) result
val destroy_texture : texture -> (unit,Error.t) result
val destroy_pipeline : pipeline -> (unit,Error.t) result
val destroy_queue : queue -> (unit,Error.t) result
val destroy_surface : surface -> (unit,Error.t) result
val destroy_device : device -> (unit,Error.t) result

module Private : sig
  val texture_driver_token : texture -> int64 * int64
  val texture_descriptor : texture -> Types.texture_descriptor
  val texture_destroyed : texture -> bool
end

(** {1 Reusable libraries, acceleration structures, and encoders}

    Immediate-mode command recording modelled on Metal: a [commands] value is
    recorded through one open encoder at a time and committed without
    blocking; [Command_buffer.status] polls its receipt. Drivers translate
    tokens; every state, range, capability, and device check lives here. *)
type library
type accel
type commands
type compute_encoder
type accel_encoder
type blit_encoder

(** Geometry for bottom-level structures. A keyframe list holds one entry for
    a static structure and exactly the structure's motion keyframes otherwise.
    One structure holds one geometry kind (triangles, boxes, or curves).
    Bounding boxes are 24-byte min/max float triples that an intersection
    function resolves; curves are float3 control points with float radii and
    uint32 segment indices. *)
type keyframe = { buffer:buffer; offset:int64 }
type curve_type = Round_curve | Flat_curve
type curve_basis = Bspline | Catmull_rom | Linear_basis | Bezier
type curve_caps = No_caps | Disk_caps | Sphere_caps
type geometry_options = { opaque:bool; duplicate_intersection:bool; table_offset:int }
val default_geometry_options : geometry_options
type geometry =
  | Triangles of { vertices:buffer; offset:int64; length:int64; vertex_stride:int; vertex_count:int }
  | Motion_triangles of { keyframes:keyframe list; vertex_stride:int; vertex_count:int }
  | Bounding_boxes of { boxes:keyframe list; stride:int; count:int; options:geometry_options }
  | Curves of { control_points:keyframe list; control_stride:int; control_point_count:int
              ; radii:keyframe list; radius_stride:int; indices:buffer; index_offset:int64
              ; segment_count:int; control_points_per_segment:int
              ; curve_type:curve_type; basis:curve_basis; caps:curve_caps }
type border = Clamp | Vanish
type motion = { keyframes:int; start_time:float; end_time:float; start_border:border; end_border:border }

(** [Tlas_of] selects the instance record kind (user ids, or motion instances
    whose [motion_transforms] buffer holds packed 4x3 keyframe transforms).
    [Sized] allocates an empty structure of [size] bytes shaped like
    [template], to be filled by [compact_accel]. *)
type accel_descriptor =
  | Blas of { geometries:geometry list }
  | Motion_blas of { geometries:geometry list; motion:motion }
  | Tlas_of of { instances:buffer; offset:int64; instance_count:int; kind:instance_kind
               ; structures:accel list; motion_transforms:(buffer * int64 * int) option }
  | Sized of { size:int64; template:accel }
type instance_record = { instance:instance; user_id:int; table_offset:int }
type motion_instance =
  { record:instance_record; transforms_start:int; transforms_count:int
  ; start_time:float; end_time:float; start_border:border; end_border:border }
type function_table

val buffer_size : buffer -> int64

val create_library : device -> Shader.t -> (library,Error.t) result

(** Creates a compute pipeline for [entry] with optional function constants.
    [interface] is the entry point's exact binding set, checked against the
    driver's reflection. *)

(** [linked] names intersection or visible functions of the library that the
    pipeline's function tables may reference; it requires [Caps.Function_tables]. *)
val create_compute_pipeline_from : library -> entry:string ->
  ?constants:(string * Shader.constant_value) list -> interface:Shader.binding list ->
  ?linked:string list -> unit -> (pipeline,Error.t) result
val create_intersection_table : pipeline -> capacity:int -> (function_table,Error.t) result
val table_set_function : function_table -> index:int -> string -> (unit,Error.t) result
val table_set_buffer : function_table -> index:int -> buffer -> (unit,Error.t) result
val destroy_table : function_table -> (unit,Error.t) result
val destroy_library : library -> (unit,Error.t) result
val pack_instance_records : device -> instance_record array -> (bytes,Error.t) result
val pack_motion_instances : device -> motion_instance array -> (bytes,Error.t) result

(** Row-major 3×4 transforms packed into 48-byte keyframe records. *)
val pack_transforms : float array array -> (bytes,Error.t) result
val create_accel : device -> accel_descriptor -> (accel,Error.t) result
val accel_sizes : accel -> accel_sizes
val destroy_accel : accel -> (unit,Error.t) result
val begin_commands : queue -> (commands,Error.t) result

val compute_encoder : commands -> (compute_encoder,Error.t) result
val set_pipeline : compute_encoder -> pipeline -> (unit,Error.t) result
val set_buffer : compute_encoder -> index:int -> buffer -> (unit,Error.t) result
val set_bytes : compute_encoder -> index:int -> bytes -> (unit,Error.t) result
val set_texture : compute_encoder -> index:int -> texture -> (unit,Error.t) result
val set_accel : compute_encoder -> index:int -> accel -> (unit,Error.t) result
val set_table : compute_encoder -> index:int -> function_table -> (unit,Error.t) result
val dispatch_threads : compute_encoder -> threads:int * int * int -> threadgroup:int * int * int -> (unit,Error.t) result
val end_compute : compute_encoder -> (unit,Error.t) result
val accel_encoder : commands -> (accel_encoder,Error.t) result
val build_accel : accel_encoder -> accel -> scratch:buffer -> ?scratch_offset:int64 -> unit -> (unit,Error.t) result

val compact_accel : accel_encoder -> src:accel -> dst:accel -> (unit,Error.t) result
val write_compacted_size : accel_encoder -> accel -> dst:buffer -> ?offset:int64 -> unit -> (unit,Error.t) result
val end_accel : accel_encoder -> (unit,Error.t) result
val blit_encoder : commands -> (blit_encoder,Error.t) result
val end_blit : blit_encoder -> (unit,Error.t) result
val commit : commands -> (receipt,Error.t) result
val abandon : commands -> (unit,Error.t) result

(** GPU execution time of a completed receipt when the driver reports one. *)
val gpu_duration : queue -> receipt -> float option
val gpu_timing : queue -> gpu_timing

(** {1 Render path}

    Samplers, portable render pipelines, argument buffers, indirect command
    buffers, and the render encoder recorded into [commands]. *)
type sampler
type icb
type argument
type render_encoder

(** [store]: [Store] keeps the attachment's contents (and, with [resolve], also
    writes the resolve texture), [Resolve] writes only the resolve texture, and
    [Discard] keeps nothing and cannot be combined with [resolve]. *)
type color_attachment = { texture:texture; resolve:texture option; load:Render_pass.load; store:Render_pass.store; clear:float * float * float * float }
type depth_attachment = { depth_texture:texture; depth_load:Render_pass.load; depth_store:Render_pass.store; depth_clear:float }
type stencil_attachment = { stencil_texture:texture; stencil_load:Render_pass.load; stencil_store:Render_pass.store; stencil_clear:int }
type render_target = { colors:color_attachment list; depth:depth_attachment option; stencil:stencil_attachment option }
type batch_draw =
  { pipeline:pipeline; buffers:(shader_stage * int * buffer * int64) array; primitive:Render_pass.primitive
  ; index:(Render_pass.index_type * buffer * int64 * int64) option; vertex_start:int; vertex_count:int; instances:int }

val create_sampler : device -> Types.sampler_descriptor -> (sampler,Error.t) result
val destroy_sampler : sampler -> (unit,Error.t) result
val create_render_pipeline : ?blend:Pipeline.blend -> ?topology:Render_pass.primitive -> ?indirect:bool ->
  device -> Pipeline.render_descriptor -> (pipeline,Error.t) result
val pipeline_indirect : pipeline -> bool
val create_icb : device -> max_commands:int -> (icb,Error.t) result
val icb_set_pipeline : icb -> index:int -> pipeline -> (unit,Error.t) result
val icb_set_buffer : icb -> index:int -> shader_stage -> slot:int -> ?offset:int64 -> buffer -> (unit,Error.t) result
val icb_draw : icb -> index:int -> primitive:Render_pass.primitive -> first:int -> count:int -> ?instances:int -> unit -> (unit,Error.t) result
val icb_draw_indexed : icb -> index:int -> primitive:Render_pass.primitive -> index_type:Render_pass.index_type -> buffer -> offset:int64 -> count:int64 -> ?instances:int -> unit -> (unit,Error.t) result
val destroy_icb : icb -> (unit,Error.t) result
val create_argument : pipeline -> shader_stage -> index:int -> (argument,Error.t) result
val argument_length : argument -> int
val argument_alignment : argument -> int
val argument_texture : argument -> buffer -> offset:int64 -> slot:int -> texture -> (unit,Error.t) result
val argument_sampler : argument -> buffer -> offset:int64 -> slot:int -> sampler -> (unit,Error.t) result
val destroy_argument : argument -> (unit,Error.t) result
val render_encoder : commands -> render_target -> (render_encoder,Error.t) result
val set_render_pipeline : render_encoder -> pipeline -> (unit,Error.t) result
val set_stage_buffer : render_encoder -> shader_stage -> index:int -> ?offset:int64 -> buffer -> (unit,Error.t) result
val set_stage_texture : render_encoder -> shader_stage -> index:int -> texture -> (unit,Error.t) result
val set_stage_sampler : render_encoder -> shader_stage -> index:int -> sampler -> (unit,Error.t) result
val set_viewport : render_encoder -> Render_pass.rect -> (unit,Error.t) result
val set_scissor : render_encoder -> Render_pass.rect -> (unit,Error.t) result
val set_cull : render_encoder -> Render_pass.cull -> (unit,Error.t) result
val set_winding : render_encoder -> winding -> (unit,Error.t) result
val set_depth_state : render_encoder -> depth_state option -> (unit,Error.t) result
val set_stencil_reference : render_encoder -> front:int32 -> back:int32 -> (unit,Error.t) result
val draw : render_encoder -> primitive:Render_pass.primitive -> first:int -> count:int -> ?instances:int -> unit -> (unit,Error.t) result
val draw_indexed : render_encoder -> primitive:Render_pass.primitive -> index_type:Render_pass.index_type -> buffer -> offset:int64 -> count:int64 -> ?instances:int -> unit -> (unit,Error.t) result
val draw_indexed_indirect : render_encoder -> primitive:Render_pass.primitive -> index_type:Render_pass.index_type ->
  buffer -> offset:int64 -> buffer -> arguments_offset:int64 -> (unit,Error.t) result
(** An indexed draw whose counts the device wrote: five uints at [arguments_offset] of the
    second buffer (index count, instance count, first index, base vertex, base instance). *)

(** Validates every draw once and hands the whole batch to the driver, which
    may execute it in one native call. *)
val draw_batch : render_encoder -> batch_draw array -> (unit,Error.t) result
val use_resources : render_encoder -> [ `Buffer of buffer | `Texture of texture ] list -> (unit,Error.t) result

(** Executes recorded indirect commands. A render pipeline must be set on the
    encoder first, as Metal validation requires. *)
val execute_icb : render_encoder -> icb -> location:int -> length:int -> (unit,Error.t) result
val end_render : render_encoder -> (unit,Error.t) result
val buffer_to_texture : blit_encoder -> src:buffer -> ?offset:int64 -> bytes_per_row:int64 -> bytes_per_image:int64 -> dst:texture -> ?mip:int -> ?origin:Types.origin -> extent:Types.extent -> unit -> (unit,Error.t) result

(** Commits the recorded commands and presents [source] into the acquired
    frame with the same command buffer; the frame is consumed on success. *)
val commit_present : commands -> source:texture -> frame -> (receipt,Error.t) result

(** Declares built structures an instance structure bound with [set_accel]
    references (its bottom-level structures) as read by the dispatch, so the
    device keeps them resident; without it they may be evicted over time and
    rays silently miss. Every structure must be built and of the encoder's
    device. An empty list is a no-op. *)
val compute_use_accels : compute_encoder -> accel list -> (unit,Error.t) result
