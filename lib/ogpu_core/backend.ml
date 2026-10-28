type token = int64
type receipt = { epoch : int64 }
type floats = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

type driver_resource = {
  token : token;
  write : int64 -> bytes -> (unit, Error.t) result;
  read : int64 -> int -> (bytes, Error.t) result;
  read_into : int64 -> bytes -> int -> int -> (unit, Error.t) result;
  destroy : unit -> (unit, Error.t) result;
}

(* A host-visible buffer and its host view. [flush n] makes the view's first [n] bytes the device
   copy's; [invalidate n] the reverse. A unified-memory backend aliases the two (both are
   bookkeeping); a discrete one copies through staging. *)
type driver_mapped = {
  mapped_resource : driver_resource;
  view : floats;
  flush : int -> (unit, Error.t) result;
  invalidate : int -> (unit, Error.t) result;
}

type driver_pipeline = {
  pipeline_token : token;
  max_threads : int;
  destroy_pipeline : unit -> (unit, Error.t) result;
}

type driver_frame = { frame_token : token }
type gpu_timing = { gpu_seconds : float }

type driver_library = {
  create_compute_pipeline_in :
    entry:string ->
    constants:(string * Shader.constant_value) list ->
    interface:Shader.binding list ->
    (driver_pipeline, Error.t) result;
  destroy_library : unit -> (unit, Error.t) result;
}

type accel_sizes = { structure_size : int64; build_scratch_size : int64 }
type driver_keyframe = token * int64

type driver_geometry =
  | Driver_triangles of {
      vertices : token;
      offset : int64;
      length : int64;
      vertex_stride : int;
      vertex_count : int;
    }
  | Driver_motion_triangles of {
      keyframes : driver_keyframe array;
      vertex_stride : int;
      vertex_count : int;
    }
  | Driver_boxes of {
      boxes : driver_keyframe array;
      stride : int;
      count : int;
      opaque : bool;
      duplicate : bool;
    }
  | Driver_curves of {
      control : driver_keyframe array;
      control_stride : int;
      control_count : int;
      radii : driver_keyframe array;
      radius_stride : int;
      indices : token;
      index_offset : int64;
      segment_count : int;
      per_segment : int;
      curve_type : int;
      basis : int;
      caps : int;
    }

type driver_motion = {
  motion_keyframes : int;
  motion_start : float;
  motion_end : float;
  motion_start_border : int;
  motion_end_border : int;
}

type instance_kind = Acceleration.instance_kind = User_id_instances | Motion_instances

type driver_accel_descriptor =
  | Driver_blas of { geometries : driver_geometry array; motion : driver_motion option }
  | Driver_tlas of {
      instances : token;
      offset : int64;
      instance_count : int;
      kind : instance_kind;
      structures : token array;
      motion_transforms : (token * int64 * int) option;
    }
  | Driver_sized of { size : int64; template : token }

type driver_accel = {
  accel_token : token;
  accel_sizes : accel_sizes;
  destroy_accel : unit -> (unit, Error.t) result;
}

type instance = { transform : float array; mask : int; structure_index : int }

type driver_compute_encoder = {
  set_pipeline : token -> (unit, Error.t) result;
  set_buffer : index:int -> offset:int64 -> token -> (unit, Error.t) result;
  set_bytes : index:int -> bytes -> (unit, Error.t) result;
  set_texture : index:int -> token -> (unit, Error.t) result;
  set_accel : index:int -> token -> (unit, Error.t) result;
  dispatch_threads :
    threads:int * int * int -> threadgroup:int * int * int -> (unit, Error.t) result;
  compute_use_accels : token list -> (unit, Error.t) result;
  end_compute : unit -> (unit, Error.t) result;
}

type driver_accel_encoder = {
  build : token -> scratch:token -> scratch_offset:int64 -> (unit, Error.t) result;
  compact : src:token -> dst:token -> (unit, Error.t) result;
  write_compacted_size : token -> dst:token -> offset:int64 -> (unit, Error.t) result;
  end_accel : unit -> (unit, Error.t) result;
}

type driver_blit_encoder = {
  buffer_to_texture :
    src:token ->
    offset:int64 ->
    bytes_per_row:int64 ->
    bytes_per_image:int64 ->
    dst:token ->
    mip:int ->
    origin:Types.origin ->
    extent:Types.extent ->
    (unit, Error.t) result;
  end_blit : unit -> (unit, Error.t) result;
}

type shader_stage = Vertex | Fragment
type winding = Clockwise | Counter_clockwise
type depth_state = { depth_compare : Render_pass.comparison; depth_write : bool }

type driver_color_attachment = {
  color : token;
  color_resolve : token option;
  color_load : Render_pass.load;
  color_store : Render_pass.store;
  color_clear : float * float * float * float;
}

type driver_depth_attachment = {
  depth : token;
  depth_load : Render_pass.load;
  depth_store : Render_pass.store;
  depth_clear : float;
}

type driver_render_target = {
  target_colors : driver_color_attachment array;
  target_depth : driver_depth_attachment option;
  target_width : int;
  target_height : int;
  target_samples : int;
}

type driver_batch_draw = {
  batch_pipeline : token;
  batch_buffers : (shader_stage * int * token * int64) array;
  batch_primitive : Render_pass.primitive;
  batch_index : (Render_pass.index_type * token * int64 * int64) option;
  batch_vertex_start : int;
  batch_vertex_count : int;
  batch_instances : int;
}

type driver_render_encoder = {
  render_set_pipeline : token -> (unit, Error.t) result;
  set_stage_buffer : shader_stage -> index:int -> offset:int64 -> token -> (unit, Error.t) result;
  set_stage_texture : shader_stage -> index:int -> token -> (unit, Error.t) result;
  set_stage_sampler : shader_stage -> index:int -> token -> (unit, Error.t) result;
  set_viewport : Render_pass.rect -> (unit, Error.t) result;
  set_scissor : Render_pass.rect -> (unit, Error.t) result;
  set_cull : Render_pass.cull -> (unit, Error.t) result;
  set_winding : winding -> (unit, Error.t) result;
  set_depth_state : depth_state option -> (unit, Error.t) result;
  draw :
    primitive:Render_pass.primitive ->
    first:int ->
    count:int ->
    instances:int ->
    (unit, Error.t) result;
  draw_indexed :
    primitive:Render_pass.primitive ->
    index_type:Render_pass.index_type ->
    token ->
    offset:int64 ->
    count:int64 ->
    instances:int ->
    (unit, Error.t) result;
  draw_indexed_indirect :
    primitive:Render_pass.primitive ->
    index_type:Render_pass.index_type ->
    token ->
    offset:int64 ->
    token ->
    arguments_offset:int64 ->
    (unit, Error.t) result;
  draw_batch : driver_batch_draw array -> (unit, Error.t) result;
  use_resources : token list -> (unit, Error.t) result;
  execute_icb : token -> location:int -> length:int -> (unit, Error.t) result;
  end_render : unit -> (unit, Error.t) result;
}

type driver_sampler = { sampler_token : token; destroy_sampler : unit -> (unit, Error.t) result }

type driver_icb = {
  icb_token : token;
  icb_set_pipeline : index:int -> token -> (unit, Error.t) result;
  icb_set_buffer :
    index:int -> shader_stage -> slot:int -> offset:int64 -> token -> (unit, Error.t) result;
  icb_draw :
    index:int ->
    primitive:Render_pass.primitive ->
    first:int ->
    count:int ->
    instances:int ->
    (unit, Error.t) result;
  icb_draw_indexed :
    index:int ->
    primitive:Render_pass.primitive ->
    index_type:Render_pass.index_type ->
    token ->
    offset:int64 ->
    count:int64 ->
    instances:int ->
    (unit, Error.t) result;
  destroy_icb : unit -> (unit, Error.t) result;
}

type driver_argument = {
  argument_length : int;
  argument_alignment : int;
  argument_texture : buffer:token -> offset:int64 -> slot:int -> token -> (unit, Error.t) result;
  argument_sampler : buffer:token -> offset:int64 -> slot:int -> token -> (unit, Error.t) result;
  destroy_argument : unit -> (unit, Error.t) result;
}

type render_pipeline_options = {
  blend : Pipeline.blend;
  topology : Render_pass.primitive;
  indirect : bool;
}

type driver_commands = {
  compute_encoder : unit -> (driver_compute_encoder, Error.t) result;
  accel_encoder : unit -> (driver_accel_encoder, Error.t) result;
  blit_encoder : unit -> (driver_blit_encoder, Error.t) result;
  render_encoder : driver_render_target -> (driver_render_encoder, Error.t) result;
  commit : unit -> (receipt, Error.t) result;
  commit_present : source:token -> driver_frame -> (receipt, Error.t) result;
  abandon : unit -> (unit, Error.t) result;
}

type driver_surface = {
  configure : Surface.configuration -> (unit, Error.t) result;
  acquire_sync :
    unit -> ([ `Acquired of driver_frame | `Timeout | `Occluded | `Device_lost ], Error.t) result;
  discard : driver_frame -> (unit, Error.t) result;
  destroy_surface : unit -> (unit, Error.t) result;
}

type driver_queue = {
  complete_through : int64 -> (unit, Error.t) result;
  poll_through : int64 -> (bool, Error.t) result;
  completed_epoch : unit -> int64;
  begin_commands : unit -> (driver_commands, Error.t) result;
  gpu_duration : int64 -> float option;
  gpu_timing : unit -> gpu_timing;
  destroy_queue : unit -> (unit, Error.t) result;
}

type driver_device = {
  device_handle : Handle.device;
  capabilities : Caps.t;
  create_buffer : Types.memory -> Types.buffer_descriptor -> (driver_resource, Error.t) result;
  create_mapped_buffer : Types.buffer_descriptor -> (driver_mapped, Error.t) result;
  create_texture : Types.texture_descriptor -> (driver_resource, Error.t) result;
  create_depth_texture : Types.texture_descriptor -> (driver_resource, Error.t) result;
  create_library : Shader.t -> (driver_library, Error.t) result;
  create_accel : driver_accel_descriptor -> (driver_accel, Error.t) result;
  instance_layout : instance_kind -> Acceleration.instance_layout;
  create_sampler : Types.sampler_descriptor -> (driver_sampler, Error.t) result;
  create_render_pipeline :
    render_pipeline_options -> Pipeline.render_descriptor -> (driver_pipeline, Error.t) result;
  create_icb : max_commands:int -> (driver_icb, Error.t) result;
  create_argument :
    pipeline:token -> shader_stage -> index:int -> (driver_argument, Error.t) result;
  create_queue : unit -> (driver_queue, Error.t) result;
  create_surface : Surface.configuration -> (driver_surface, Error.t) result;
  destroy_device : unit -> (unit, Error.t) result;
}

type driver = { create_device : unit -> (driver_device, Error.t) result }

type device = {
  raw : driver_device;
  handle : Handle.device;
  mutable children : int;
  mutable dead : bool;
  mutable queues : queue list;
}

and resource = {
  raw : driver_resource;
  handle : unit Handle.t;
  device : device;
  mutable dead : bool;
}

and buffer = {
  resource : resource;
  buffer_descriptor : Types.buffer_descriptor;
  memory : Types.memory;
  mapping : driver_mapped option;
}

and texture = { resource : resource; texture_descriptor : Types.texture_descriptor }

and pipeline = {
  pipeline_driver : driver_pipeline;
  device : device;
  pipeline_library : library option;
  pipeline_indirect : bool;
  pipeline_shape : pipeline_shape;
  mutable dead : bool;
}

and pipeline_shape = Vertex_shape | Compute_shape

and library = {
  library_raw : driver_library;
  library_device : device;
  mutable library_dead : bool;
  mutable library_pipelines : int;
}

and queue = { raw : driver_queue; device : device; mutable dead : bool }

type surface = {
  raw : driver_surface;
  device : device;
  mutable dead : bool;
  mutable frames : int;
  mutable configuration : Surface.configuration;
}

type frame = { raw : driver_frame; surface : surface; mutable consumed : bool }

let error op kind text = Error (Error.make op kind text)
let mark_submitted (_ : queue) (_ : resource) = ()

let create_device driver =
  match driver.create_device () with
  | Error _ as e -> e
  | Ok raw -> (
      match Caps.validate raw.capabilities with
      | Error _ as e -> e
      | Ok () -> Ok { raw; handle = raw.device_handle; children = 0; dead = false; queues = [] })

let capabilities (value : device) = value.raw.capabilities
let device_handle (value : device) = value.handle

let live op (device : device) =
  if device.dead then error op Error.Stale_handle "device is destroyed" else Ok ()

let make_resource (device : device) raw =
  { raw; handle = Handle.create ~device:device.handle; device; dead = false }

let create_buffer ?(memory = Types.Shared) device descriptor =
  match live "Backend.create_buffer" device with
  | Error _ as e -> e
  | Ok () -> (
      match Validation.validate_buffer device.raw.capabilities descriptor with
      | Error _ as e -> e
      | Ok () -> (
          match device.raw.create_buffer memory descriptor with
          | Error _ as e -> e
          | Ok raw ->
              device.children <- device.children + 1;
              Ok
                {
                  resource = make_resource device raw;
                  buffer_descriptor = descriptor;
                  memory;
                  mapping = None;
                }))

let create_mapped_buffer device (descriptor : Types.buffer_descriptor) =
  match live "Backend.create_mapped_buffer" device with
  | Error _ as e -> e
  | Ok () -> (
      match Validation.validate_buffer device.raw.capabilities descriptor with
      | Error _ as e -> e
      | Ok () when Int64.rem descriptor.size 4L <> 0L ->
          error "Backend.create_mapped_buffer" Error.Invalid_argument
            "mapped buffer size must be a multiple of 4"
      | Ok () -> (
          match device.raw.create_mapped_buffer descriptor with
          | Error _ as e -> e
          | Ok mapped ->
              device.children <- device.children + 1;
              Ok
                ( {
                    resource = make_resource device mapped.mapped_resource;
                    buffer_descriptor = descriptor;
                    memory = Types.Shared;
                    mapping = Some mapped;
                  },
                  mapped.view )))

let sync operation f (value : buffer) ~length =
  match value.mapping with
  | _ when value.resource.dead -> error operation Error.Stale_handle "buffer is destroyed"
  | None -> error operation Error.Invalid_argument "buffer is not mapped"
  | Some _ when length < 0 || Int64.of_int length > value.buffer_descriptor.size ->
      error operation Error.Invalid_argument "range exceeds the buffer"
  | Some mapped -> f mapped length

let flush_mapped = sync "Backend.flush_mapped" (fun m -> m.flush)
let invalidate_mapped = sync "Backend.invalidate_mapped" (fun m -> m.invalidate)
let buffer_size (value : buffer) = value.buffer_descriptor.size

let create_texture device descriptor =
  match live "Backend.create_texture" device with
  | Error _ as e -> e
  | Ok () -> (
      match Validation.validate_texture device.raw.capabilities descriptor with
      | Error _ as e -> e
      | Ok () -> (
          match device.raw.create_texture descriptor with
          | Error _ as e -> e
          | Ok raw ->
              device.children <- device.children + 1;
              Ok { resource = make_resource device raw; texture_descriptor = descriptor }))

let create_depth_texture device descriptor =
  match live "Backend.create_depth_texture" device with
  | Error _ as e -> e
  | Ok () -> (
      match Validation.validate_texture device.raw.capabilities descriptor with
      | Error _ as e -> e
      | Ok ()
        when (not (List.mem Types.Render_attachment descriptor.usage))
             || List.exists (fun usage -> usage <> Types.Render_attachment) descriptor.usage ->
          error "Backend.create_depth_texture" Error.Invalid_argument
            "depth textures are render-attachment only"
      | Ok () -> (
          match device.raw.create_depth_texture descriptor with
          | Error _ as e -> e
          | Ok raw ->
              device.children <- device.children + 1;
              Ok { resource = make_resource device raw; texture_descriptor = descriptor }))

let create_queue device =
  let operation = "Backend.create_queue" in
  match live operation device with
  | Error _ as e -> e
  | Ok () -> (
      match device.raw.create_queue () with
      | Error _ as e -> e
      | Ok raw ->
          device.children <- device.children + 1;
          let queue = { raw; device; dead = false } in
          device.queues <- queue :: device.queues;
          Ok queue)

let create_surface device configuration =
  match live "Backend.create_surface" device with
  | Error _ as e -> e
  | Ok () -> (
      match Surface.create device.handle configuration with
      | Error _ as e -> e
      | Ok portable -> (
          Surface.destroy portable;
          match device.raw.create_surface configuration with
          | Error _ as e -> e
          | Ok raw ->
              device.children <- device.children + 1;
              Ok { raw; device; dead = false; frames = 0; configuration }))

let buffer_id (value : buffer) = Handle.id value.resource.handle
let texture_id (value : texture) = Handle.id value.resource.handle

let write_buffer (value : buffer) ~offset bytes =
  if value.resource.dead then error "Backend.write_buffer" Error.Stale_handle "buffer is destroyed"
  else if value.memory = Types.Device_local then
    error "Backend.write_buffer" Error.Unsupported "device-local buffers are not host writable"
  else value.resource.raw.write offset bytes

let read_buffer (value : buffer) ~offset ~length =
  if value.resource.dead then error "Backend.read_buffer" Error.Stale_handle "buffer is destroyed"
  else if value.memory = Types.Device_local then
    error "Backend.read_buffer" Error.Unsupported "device-local buffers are not host readable"
  else value.resource.raw.read offset length

let read_texture (value : texture) ~bytes_per_row =
  if value.resource.dead then error "Backend.read_texture" Error.Stale_handle "texture is destroyed"
  else if bytes_per_row <= 0 || value.texture_descriptor.height > max_int / bytes_per_row then
    error "Backend.read_texture" Error.Invalid_argument "row pitch is invalid"
  else value.resource.raw.read 0L (bytes_per_row * value.texture_descriptor.height)

let read_texture_into (value : texture) ~bytes_per_row ~destination =
  let operation = "Backend.read_texture_into" in
  if value.resource.dead then error operation Error.Stale_handle "texture is destroyed"
  else if bytes_per_row <= 0 || value.texture_descriptor.height > max_int / bytes_per_row then
    error operation Error.Invalid_argument "row pitch is invalid"
  else
    let length = bytes_per_row * value.texture_descriptor.height in
    if Bytes.length destination <> length then
      error operation Error.Invalid_argument "destination length does not match texture extent"
    else value.resource.raw.read_into 0L destination 0 length

let complete_through (queue : queue) epoch =
  if queue.dead then error "Backend.complete_through" Error.Stale_handle "queue is destroyed"
  else queue.raw.complete_through epoch

let poll_through (queue : queue) epoch =
  if queue.dead then error "Backend.poll_through" Error.Stale_handle "queue is destroyed"
  else queue.raw.poll_through epoch

let completed_epoch (queue : queue) = queue.raw.completed_epoch ()

type status = Pending | Completed

let status queue (receipt : receipt) =
  Result.map (function true -> Completed | false -> Pending) (poll_through queue receipt.epoch)

let configure (surface : surface) value =
  if surface.dead then error "Backend.configure" Error.Stale_handle "surface is destroyed"
  else if surface.frames <> 0 then
    error "Backend.configure" Error.Invalid_state "surface has acquired frames"
  else
    match surface.raw.configure value with
    | Error _ as e -> e
    | Ok () ->
        surface.configuration <- value;
        Ok ()

let acquire_sync (surface : surface) =
  if surface.dead then error "Backend.acquire_sync" Error.Stale_handle "surface is destroyed"
  else
    match surface.raw.acquire_sync () with
    | Error _ as e -> e
    | Ok (`Acquired raw) ->
        surface.frames <- surface.frames + 1;
        Ok (`Acquired { raw; surface; consumed = false })
    | Ok `Timeout -> Ok `Timeout
    | Ok `Occluded -> Ok `Occluded
    | Ok `Device_lost -> Ok `Device_lost

let consume op action (frame : frame) =
  if frame.consumed then error op Error.Invalid_state "frame is already consumed"
  else if frame.surface.dead then error op Error.Stale_handle "surface is destroyed"
  else
    match action frame.raw with
    | Error _ as e -> e
    | Ok () ->
        frame.consumed <- true;
        frame.surface.frames <- frame.surface.frames - 1;
        Ok ()

let validate_present op (queue : queue) (source : texture) (frame : frame) =
  if frame.consumed then error op Error.Invalid_state "frame is already consumed"
  else if frame.surface.dead then error op Error.Stale_handle "surface is destroyed"
  else if queue.dead then error op Error.Stale_handle "queue is destroyed"
  else if source.resource.dead then error op Error.Stale_handle "source texture is destroyed"
  else if queue.device != frame.surface.device then
    error op Error.Cross_device "queue belongs to a different device"
  else if source.resource.device != frame.surface.device then
    error op Error.Cross_device "source texture belongs to a different device"
  else
    let descriptor = source.texture_descriptor and configuration = frame.surface.configuration in
    if
      descriptor.width <> configuration.physical_width
      || descriptor.height <> configuration.physical_height
    then
      error op Error.Invalid_argument
        "source texture extent does not match the configured physical surface extent"
    else if descriptor.sample_count <> 1 then
      error op Error.Invalid_argument "source texture must have sample_count=1"
    else if descriptor.format <> Types.Rgba8_unorm then
      error op Error.Invalid_argument "source texture must be Rgba8_unorm"
    else if not (List.mem Types.Texture_binding descriptor.usage) then
      error op Error.Invalid_argument "source texture must have Texture_binding usage"
    else Ok ()

let consume_present frame =
  frame.consumed <- true;
  frame.surface.frames <- frame.surface.frames - 1

let discard frame = consume "Backend.discard" frame.surface.raw.discard frame

let destroy_resource (resource : resource) =
  if resource.dead then Ok ()
  else
    match resource.raw.destroy () with
    | Error _ as e -> e
    | Ok () ->
        let id = Handle.id resource.handle in
        resource.dead <- true;
        Handle.destroy resource.handle;
        ignore id;
        resource.device.children <- resource.device.children - 1;
        Ok ()

let destroy_buffer (value : buffer) = destroy_resource value.resource
let destroy_texture (value : texture) = destroy_resource value.resource

let destroy_pipeline (value : pipeline) =
  if value.dead then Ok ()
  else
    match value.pipeline_driver.destroy_pipeline () with
    | Error _ as e -> e
    | Ok () ->
        value.dead <- true;
        value.device.children <- value.device.children - 1;
        Option.iter
          (fun library -> library.library_pipelines <- library.library_pipelines - 1)
          value.pipeline_library;
        Ok ()

let destroy_queue (value : queue) =
  if value.dead then Ok ()
  else
    match value.raw.destroy_queue () with
    | Error _ as e -> e
    | Ok () ->
        value.dead <- true;
        value.device.queues <- List.filter (fun queue -> queue != value) value.device.queues;
        value.device.children <- value.device.children - 1;
        Ok ()

let destroy_surface (value : surface) =
  if value.dead then Ok ()
  else if value.frames <> 0 then
    error "Backend.destroy_surface" Error.Invalid_state "surface has acquired frames"
  else
    match value.raw.destroy_surface () with
    | Error _ as e -> e
    | Ok () ->
        value.dead <- true;
        value.device.children <- value.device.children - 1;
        Ok ()

let destroy_device (value : device) =
  if value.dead then Ok ()
  else if value.children <> 0 then
    error "Backend.destroy_device" Error.Invalid_state "device has live children"
  else
    match value.raw.destroy_device () with
    | Error _ as e -> e
    | Ok () ->
        value.dead <- true;
        Ok ()

(* Reusable shader libraries, acceleration structures, and immediate-mode
   command encoders. Validation lives here; drivers only translate tokens. *)
type accel = {
  accel_raw : driver_accel;
  accel_device : device;
  portable : Acceleration.t;
  accel_structures : accel list;
  mutable accel_dead : bool;
}

type commands_state = Recording | Encoding | Committed | Abandoned

type commands = {
  commands_raw : driver_commands;
  commands_queue : queue;
  mutable state : commands_state;
}

type compute_encoder = {
  compute_raw : driver_compute_encoder;
  compute_commands : commands;
  mutable compute_open : bool;
  mutable pipeline_set : bool;
}

type accel_encoder = {
  accel_encoder_raw : driver_accel_encoder;
  accel_commands : commands;
  mutable accel_open : bool;
}

type blit_encoder = {
  blit_raw : driver_blit_encoder;
  blit_commands : commands;
  mutable blit_open : bool;
}

type keyframe = { buffer : buffer; offset : int64 }
type curve_type = Round_curve | Flat_curve
type curve_basis = Bspline | Catmull_rom | Linear_basis | Bezier
type curve_caps = No_caps | Disk_caps | Sphere_caps
type geometry_options = { opaque : bool; duplicate_intersection : bool }

let default_geometry_options = { opaque = false; duplicate_intersection = false }

type geometry =
  | Triangles of {
      vertices : buffer;
      offset : int64;
      length : int64;
      vertex_stride : int;
      vertex_count : int;
    }
  | Motion_triangles of { keyframes : keyframe list; vertex_stride : int; vertex_count : int }
  | Bounding_boxes of {
      boxes : keyframe list;
      stride : int;
      count : int;
      options : geometry_options;
    }
  | Curves of {
      control_points : keyframe list;
      control_stride : int;
      control_point_count : int;
      radii : keyframe list;
      radius_stride : int;
      indices : buffer;
      index_offset : int64;
      segment_count : int;
      control_points_per_segment : int;
      curve_type : curve_type;
      basis : curve_basis;
      caps : curve_caps;
    }

type border = Clamp | Vanish

type motion = {
  keyframes : int;
  start_time : float;
  end_time : float;
  start_border : border;
  end_border : border;
}

type accel_descriptor =
  | Blas of { geometries : geometry list }
  | Motion_blas of { geometries : geometry list; motion : motion }
  | Tlas_of of {
      instances : buffer;
      offset : int64;
      instance_count : int;
      kind : instance_kind;
      structures : accel list;
      motion_transforms : (buffer * int64 * int) option;
    }
  | Sized of { size : int64; template : accel }

type instance_record = { instance : instance; user_id : int }

type motion_instance = {
  record : instance_record;
  transforms_start : int;
  transforms_count : int;
  start_time : float;
  end_time : float;
  start_border : border;
  end_border : border;
}

let valid_name value = value <> "" && not (String.contains value '\000')

let create_library device shader =
  let op = "Backend.create_library" in
  match live op device with
  | Error _ as e -> e
  | Ok () -> (
      match device.raw.create_library shader with
      | Error _ as e -> e
      | Ok raw ->
          device.children <- device.children + 1;
          Ok
            {
              library_raw = raw;
              library_device = device;
              library_dead = false;
              library_pipelines = 0;
            })

let create_compute_pipeline_from (library : library) ~entry ?(constants = []) ~interface () =
  let op = "Backend.create_compute_pipeline_from" in
  if library.library_dead then error op Error.Stale_handle "library is destroyed"
  else
    match live op library.library_device with
    | Error _ as e -> e
    | Ok () -> (
        let device = library.library_device in
        match Caps.require ~operation:op device.raw.capabilities Caps.Compute_pipeline with
        | Error _ as e -> e
        | Ok () -> (
            if not (valid_name entry) then
              error op Error.Invalid_argument "entry name is empty or contains NUL"
            else
              match Shader.validate_bindings interface with
              | Error _ as e -> e
              | Ok () -> (
                  match Shader.validate_constants constants with
                  | Error _ as e -> e
                  | Ok () -> (
                      match
                        library.library_raw.create_compute_pipeline_in ~entry ~constants ~interface
                      with
                      | Error _ as e -> e
                      | Ok pipeline_driver ->
                          device.children <- device.children + 1;
                          library.library_pipelines <- library.library_pipelines + 1;
                          Ok
                            {
                              pipeline_driver;
                              device;
                              pipeline_library = Some library;
                              pipeline_indirect = false;
                              pipeline_shape = Compute_shape;
                              dead = false;
                            }))))

let destroy_library (library : library) =
  let op = "Backend.destroy_library" in
  if library.library_dead then Ok ()
  else if library.library_pipelines > 0 then
    error op Error.Invalid_state "library has live pipelines"
  else
    match library.library_raw.destroy_library () with
    | Error _ as e -> e
    | Ok () ->
        library.library_dead <- true;
        library.library_device.children <- library.library_device.children - 1;
        Ok ()

let accel_sizes (value : accel) = value.accel_raw.accel_sizes
let max_total_threads (pipeline : pipeline) = pipeline.pipeline_driver.max_threads
let instance_stride_of (device : device) kind = (device.raw.instance_layout kind).(0)
let finite value = Float.is_finite value && Float.abs value <= 3.402823466e38

let valid_instance (i : instance) =
  Array.length i.transform = 12
  && Array.for_all finite i.transform && i.mask >= 0 && i.mask <= 0xFFFF_FFFF
  && i.structure_index >= 0

let valid_record (r : instance_record) =
  valid_instance r.instance && r.user_id >= 0 && r.user_id <= 0xFFFF_FFFF

let border_code = function Clamp -> 0 | Vanish -> 1

let pack_instance_records (device : device) records =
  let op = "Backend.pack_instance_records" in
  if Array.length records = 0 then error op Error.Invalid_argument "instance array is empty"
  else if not (Array.for_all valid_record records) then
    error op Error.Invalid_argument "instance transform, mask, index, or user id is invalid"
  else
    Ok
      (Acceleration.pack_instances
         (device.raw.instance_layout User_id_instances)
         (Array.map
            (fun (r : instance_record) ->
              (r.instance.transform, r.instance.mask, r.instance.structure_index, r.user_id))
            records))

let pack_motion_instances (device : device) instances =
  let op = "Backend.pack_motion_instances" in
  if Array.length instances = 0 then error op Error.Invalid_argument "instance array is empty"
  else if
    not
      (Array.for_all
         (fun (m : motion_instance) ->
           valid_record m.record && m.transforms_start >= 0 && m.transforms_count >= 2
           && Float.is_finite m.start_time && Float.is_finite m.end_time
           && m.start_time < m.end_time)
         instances)
  then
    error op Error.Invalid_argument
      "motion instance record, transform range, or time range is invalid"
  else
    Ok
      (Acceleration.pack_motion_instances
         (device.raw.instance_layout Motion_instances)
         (Array.map
            (fun (m : motion_instance) ->
              ( (m.record.instance.mask, m.record.instance.structure_index, m.record.user_id),
                (m.transforms_start, m.transforms_count),
                (border_code m.start_border, border_code m.end_border),
                (m.start_time, m.end_time) ))
            instances))

let pack_transforms transforms =
  let op = "Backend.pack_transforms" in
  if Array.length transforms = 0 then error op Error.Invalid_argument "transform array is empty"
  else if not (Array.for_all (fun t -> Array.length t = 12 && Array.for_all finite t) transforms)
  then error op Error.Invalid_argument "transforms must be finite 3x4 rows"
  else Ok (Acceleration.pack_transforms transforms)

let check_resource op (device : device) (resource : resource) =
  if resource.dead then error op Error.Stale_handle "resource is destroyed"
  else if resource.device != device then
    error op Error.Cross_device "resource belongs to another device"
  else Ok ()

let check_accel op (device : device) (value : accel) =
  if value.accel_dead then error op Error.Stale_handle "acceleration structure is destroyed"
  else if value.accel_device != device then
    error op Error.Cross_device "acceleration structure belongs to another device"
  else Ok ()

let rec check_all op device = function
  | [] -> Ok ()
  | (buffer : buffer) :: rest -> (
      match check_resource op device buffer.resource with
      | Error _ as e -> e
      | Ok () -> check_all op device rest)

let geometry_buffers = function
  | Triangles { vertices; _ } -> [ vertices ]
  | Motion_triangles { keyframes; _ } -> List.map (fun (k : keyframe) -> k.buffer) keyframes
  | Bounding_boxes { boxes; _ } -> List.map (fun (k : keyframe) -> k.buffer) boxes
  | Curves { control_points; radii; indices; _ } ->
      List.map (fun (k : keyframe) -> k.buffer) (control_points @ radii) @ [ indices ]

let descriptor_buffers = function
  | Blas { geometries; _ } | Motion_blas { geometries; _ } ->
      List.concat_map geometry_buffers geometries
  | Tlas_of { instances; motion_transforms; _ } ->
      instances :: Option.fold ~none:[] ~some:(fun (b, _, _) -> [ b ]) motion_transforms
  | Sized _ -> []

let descriptor_structures = function
  | Blas _ | Motion_blas _ -> []
  | Tlas_of { structures; _ } -> structures
  | Sized { template; _ } -> [ template ]

let create_accel device descriptor =
  let op = "Backend.create_accel" in
  match live op device with
  | Error _ as e -> e
  | Ok () -> (
      match Caps.require ~operation:op device.raw.capabilities Caps.Ray_tracing with
      | Error _ as e -> e
      | Ok () -> (
          let uses_curves =
            match descriptor with
            | Blas { geometries; _ } | Motion_blas { geometries; _ } ->
                List.exists (function Curves _ -> true | _ -> false) geometries
            | _ -> false
          in
          let uses_motion =
            match descriptor with
            | Motion_blas _ -> true
            | Tlas_of { kind = Motion_instances; _ } -> true
            | _ -> false
          in
          match
            if uses_motion then
              Caps.require ~operation:op device.raw.capabilities Caps.Ray_tracing_motion
            else Ok ()
          with
          | Error _ as e -> e
          | Ok () -> (
              match
                if uses_curves then
                  Caps.require ~operation:op device.raw.capabilities Caps.Ray_tracing_curves
                else Ok ()
              with
              | Error _ as e -> e
              | Ok () -> (
                  match check_all op device (descriptor_buffers descriptor) with
                  | Error _ as e -> e
                  | Ok () -> (
                      let structures = descriptor_structures descriptor in
                      let rec check_structures = function
                        | [] -> Ok ()
                        | (value : accel) :: rest -> (
                            match check_accel op device value with
                            | Error _ as e -> e
                            | Ok () -> (
                                match (descriptor, Acceleration.descriptor value.portable) with
                                | Tlas_of _, Acceleration.Tlas _ ->
                                    error op Error.Invalid_argument
                                      "TLAS instances must reference bottom-level structures"
                                | _ -> check_structures rest))
                      in
                      match check_structures structures with
                      | Error _ as e -> e
                      | Ok () -> (
                          let range (b : buffer) offset length =
                            {
                              Acceleration.buffer = b.resource.handle;
                              buffer_size = b.buffer_descriptor.size;
                              offset;
                              length;
                            }
                          in
                          let span stride count =
                            if count <= 0 || stride <= 0 || count > max_int / stride then 0L
                            else Int64.of_int (count * stride)
                          in
                          let keyframe_ranges stride count keyframes =
                            List.map
                              (fun (k : keyframe) -> range k.buffer k.offset (span stride count))
                              keyframes
                          in
                          let portable_geometry = function
                            | Triangles { vertices; offset; length; vertex_stride; vertex_count } ->
                                Acceleration.Triangles
                                  {
                                    vertices = range vertices offset length;
                                    vertex_stride;
                                    vertex_count;
                                  }
                            | Motion_triangles { keyframes; vertex_stride; vertex_count } ->
                                Acceleration.Motion_triangles
                                  {
                                    keyframes = keyframe_ranges vertex_stride vertex_count keyframes;
                                    vertex_stride;
                                    vertex_count;
                                  }
                            | Bounding_boxes { boxes; stride; count; _ } ->
                                Acceleration.Bounding_boxes
                                  { boxes = keyframe_ranges stride count boxes; stride; count }
                            | Curves
                                {
                                  control_points;
                                  control_stride;
                                  control_point_count;
                                  radii;
                                  radius_stride;
                                  indices;
                                  index_offset;
                                  segment_count;
                                  control_points_per_segment;
                                  _;
                                } ->
                                Acceleration.Curves
                                  {
                                    control_points =
                                      keyframe_ranges control_stride control_point_count
                                        control_points;
                                    control_stride;
                                    control_point_count;
                                    radii = keyframe_ranges radius_stride control_point_count radii;
                                    radius_stride;
                                    indices = range indices index_offset (span 4 segment_count);
                                    segment_count;
                                    control_points_per_segment;
                                  }
                          in
                          let motion_option = function
                            | Blas _ -> None
                            | Motion_blas { motion; _ } -> Some motion.keyframes
                            | _ -> None
                          in
                          let portable_descriptor =
                            match descriptor with
                            | Blas { geometries } | Motion_blas { geometries; _ } ->
                                Acceleration.Blas
                                  {
                                    geometries =
                                      Array.of_list (List.map portable_geometry geometries);
                                    motion_keyframes = motion_option descriptor;
                                  }
                            | Tlas_of { instances; offset; instance_count; kind; structures; _ } ->
                                let stride = instance_stride_of device kind in
                                Acceleration.Tlas
                                  {
                                    instances = range instances offset (span stride instance_count);
                                    instance_stride = stride;
                                    instance_count;
                                    instance_kind = kind;
                                    structures =
                                      List.map
                                        (fun (a : accel) -> Acceleration.handle a.portable)
                                        structures;
                                  }
                            | Sized { size; template } ->
                                Acceleration.Sized
                                  { size; template = Acceleration.handle template.portable }
                          in
                          let motion_valid =
                            match descriptor with
                            | Motion_blas { motion; _ } ->
                                motion.keyframes >= 2 && Float.is_finite motion.start_time
                                && Float.is_finite motion.end_time
                                && motion.start_time < motion.end_time
                            | Tlas_of { kind = Motion_instances; motion_transforms = None; _ } ->
                                false
                            | Tlas_of { motion_transforms = Some (_, offset, count); _ } ->
                                offset >= 0L && count > 0
                            | _ -> true
                          in
                          if not motion_valid then
                            error op Error.Invalid_argument
                              "motion keyframes, time range, or transform range is invalid"
                          else
                            match
                              Acceleration.create device.handle ~ray_tracing:true
                                portable_descriptor
                            with
                            | Error _ as e -> e
                            | Ok portable -> (
                                let tok (b : buffer) = b.resource.raw.token in
                                let keyframes keyframes =
                                  Array.of_list
                                    (List.map
                                       (fun (k : keyframe) -> (tok k.buffer, k.offset))
                                       keyframes)
                                in
                                let driver_geometry = function
                                  | Triangles
                                      { vertices; offset; length; vertex_stride; vertex_count } ->
                                      Driver_triangles
                                        {
                                          vertices = tok vertices;
                                          offset;
                                          length;
                                          vertex_stride;
                                          vertex_count;
                                        }
                                  | Motion_triangles { keyframes = k; vertex_stride; vertex_count }
                                    ->
                                      Driver_motion_triangles
                                        { keyframes = keyframes k; vertex_stride; vertex_count }
                                  | Bounding_boxes { boxes; stride; count; options } ->
                                      Driver_boxes
                                        {
                                          boxes = keyframes boxes;
                                          stride;
                                          count;
                                          opaque = options.opaque;
                                          duplicate = options.duplicate_intersection;
                                        }
                                  | Curves
                                      {
                                        control_points;
                                        control_stride;
                                        control_point_count;
                                        radii;
                                        radius_stride;
                                        indices;
                                        index_offset;
                                        segment_count;
                                        control_points_per_segment;
                                        curve_type;
                                        basis;
                                        caps;
                                      } ->
                                      Driver_curves
                                        {
                                          control = keyframes control_points;
                                          control_stride;
                                          control_count = control_point_count;
                                          radii = keyframes radii;
                                          radius_stride;
                                          indices = tok indices;
                                          index_offset;
                                          segment_count;
                                          per_segment = control_points_per_segment;
                                          curve_type =
                                            (match curve_type with
                                            | Round_curve -> 0
                                            | Flat_curve -> 1);
                                          basis =
                                            (match basis with
                                            | Bspline -> 0
                                            | Catmull_rom -> 1
                                            | Linear_basis -> 2
                                            | Bezier -> 3);
                                          caps =
                                            (match caps with
                                            | No_caps -> 0
                                            | Disk_caps -> 1
                                            | Sphere_caps -> 2);
                                        }
                                in
                                let driver_descriptor =
                                  match descriptor with
                                  | Blas { geometries } ->
                                      Driver_blas
                                        {
                                          geometries =
                                            Array.of_list (List.map driver_geometry geometries);
                                          motion = None;
                                        }
                                  | Motion_blas { geometries; motion } ->
                                      Driver_blas
                                        {
                                          geometries =
                                            Array.of_list (List.map driver_geometry geometries);
                                          motion =
                                            Some
                                              {
                                                motion_keyframes = motion.keyframes;
                                                motion_start = motion.start_time;
                                                motion_end = motion.end_time;
                                                motion_start_border =
                                                  border_code motion.start_border;
                                                motion_end_border = border_code motion.end_border;
                                              };
                                        }
                                  | Tlas_of
                                      {
                                        instances;
                                        offset;
                                        instance_count;
                                        kind;
                                        structures;
                                        motion_transforms;
                                      } ->
                                      Driver_tlas
                                        {
                                          instances = tok instances;
                                          offset;
                                          instance_count;
                                          kind;
                                          structures =
                                            Array.of_list
                                              (List.map
                                                 (fun (a : accel) -> a.accel_raw.accel_token)
                                                 structures);
                                          motion_transforms =
                                            Option.map
                                              (fun (b, o, c) -> (tok b, o, c))
                                              motion_transforms;
                                        }
                                  | Sized { size; template } ->
                                      Driver_sized
                                        { size; template = template.accel_raw.accel_token }
                                in
                                match device.raw.create_accel driver_descriptor with
                                | Error _ as e ->
                                    Acceleration.destroy portable;
                                    e
                                | Ok raw ->
                                    device.children <- device.children + 1;
                                    Ok
                                      {
                                        accel_raw = raw;
                                        accel_device = device;
                                        portable;
                                        accel_structures = structures;
                                        accel_dead = false;
                                      })))))))

let destroy_accel (value : accel) =
  if value.accel_dead then Ok ()
  else
    match value.accel_raw.destroy_accel () with
    | Error _ as e -> e
    | Ok () ->
        value.accel_dead <- true;
        Acceleration.destroy value.portable;
        value.accel_device.children <- value.accel_device.children - 1;
        Ok ()

let begin_commands (queue : queue) =
  let op = "Backend.begin_commands" in
  if queue.device.dead then error op Error.Stale_handle "device is destroyed"
  else if queue.dead then error op Error.Stale_handle "queue is destroyed"
  else
    match queue.raw.begin_commands () with
    | Error _ as e -> e
    | Ok raw -> Ok { commands_raw = raw; commands_queue = queue; state = Recording }

let recording op (commands : commands) =
  match commands.state with
  | Recording -> Ok ()
  | Encoding -> error op Error.Invalid_state "an encoder is still open"
  | Committed -> error op Error.Invalid_state "commands are already committed"
  | Abandoned -> error op Error.Invalid_state "commands were abandoned"

let commands_device (commands : commands) = commands.commands_queue.device
let max_slot = 31

let valid_slot op index =
  if index < 0 || index > max_slot then
    error op Error.Invalid_argument "binding slot is out of range"
  else Ok ()

let positive3 (x, y, z) = x > 0 && y > 0 && z > 0

let compute_encoder commands =
  let op = "Backend.compute_encoder" in
  match recording op commands with
  | Error _ as e -> e
  | Ok () -> (
      match
        Caps.require ~operation:op (commands_device commands).raw.capabilities Caps.Compute_pipeline
      with
      | Error _ as e -> e
      | Ok () -> (
          match commands.commands_raw.compute_encoder () with
          | Error _ as e -> e
          | Ok raw ->
              commands.state <- Encoding;
              Ok
                {
                  compute_raw = raw;
                  compute_commands = commands;
                  compute_open = true;
                  pipeline_set = false;
                }))

let open_compute op (encoder : compute_encoder) =
  if encoder.compute_open then Ok () else error op Error.Invalid_state "compute encoder is ended"

let set_pipeline (encoder : compute_encoder) (pipeline : pipeline) =
  let op = "Backend.set_pipeline" in
  match open_compute op encoder with
  | Error _ as e -> e
  | Ok () -> (
      if pipeline.dead then error op Error.Stale_handle "pipeline is destroyed"
      else if pipeline.device != commands_device encoder.compute_commands then
        error op Error.Cross_device "pipeline belongs to another device"
      else
        match encoder.compute_raw.set_pipeline pipeline.pipeline_driver.pipeline_token with
        | Error _ as e -> e
        | Ok () ->
            encoder.pipeline_set <- true;
            Ok ())

let set_buffer (encoder : compute_encoder) ~index (buffer : buffer) =
  let op = "Backend.set_buffer" in
  match open_compute op encoder with
  | Error _ as e -> e
  | Ok () -> (
      match valid_slot op index with
      | Error _ as e -> e
      | Ok () -> (
          match check_resource op (commands_device encoder.compute_commands) buffer.resource with
          | Error _ as e -> e
          | Ok () -> (
              if buffer.buffer_descriptor.size <= 0L then
                error op Error.Invalid_argument "buffer is empty"
              else
                match
                  encoder.compute_raw.set_buffer ~index ~offset:0L buffer.resource.raw.token
                with
                | Error _ as e -> e
                | Ok () ->
                    mark_submitted encoder.compute_commands.commands_queue buffer.resource;
                    Ok ())))

let max_inline_bytes = 4096

let set_bytes (encoder : compute_encoder) ~index bytes =
  let op = "Backend.set_bytes" in
  match open_compute op encoder with
  | Error _ as e -> e
  | Ok () -> (
      match valid_slot op index with
      | Error _ as e -> e
      | Ok () ->
          if Bytes.length bytes = 0 || Bytes.length bytes > max_inline_bytes then
            error op Error.Invalid_argument "inline bytes must be 1..4096 bytes"
          else encoder.compute_raw.set_bytes ~index bytes)

let set_texture (encoder : compute_encoder) ~index (texture : texture) =
  let op = "Backend.set_texture" in
  match open_compute op encoder with
  | Error _ as e -> e
  | Ok () -> (
      match valid_slot op index with
      | Error _ as e -> e
      | Ok () -> (
          match check_resource op (commands_device encoder.compute_commands) texture.resource with
          | Error _ as e -> e
          | Ok () -> (
              if
                not
                  (List.exists
                     (fun usage -> usage = Types.Texture_binding || usage = Types.Storage_binding)
                     texture.texture_descriptor.usage)
              then error op Error.Invalid_argument "texture lacks shader binding usage"
              else
                match encoder.compute_raw.set_texture ~index texture.resource.raw.token with
                | Error _ as e -> e
                | Ok () ->
                    mark_submitted encoder.compute_commands.commands_queue texture.resource;
                    Ok ())))

let set_accel (encoder : compute_encoder) ~index (accel : accel) =
  let op = "Backend.set_accel" in
  match open_compute op encoder with
  | Error _ as e -> e
  | Ok () -> (
      match valid_slot op index with
      | Error _ as e -> e
      | Ok () -> (
          match check_accel op (commands_device encoder.compute_commands) accel with
          | Error _ as e -> e
          | Ok () ->
              if not (Acceleration.built accel.portable) then
                error op Error.Invalid_state "acceleration structure has no encoded build"
              else encoder.compute_raw.set_accel ~index accel.accel_raw.accel_token))

let dispatch op (encoder : compute_encoder) grid threadgroup dispatch =
  match open_compute op encoder with
  | Error _ as e -> e
  | Ok () ->
      if not encoder.pipeline_set then error op Error.Invalid_state "no compute pipeline is set"
      else if not (positive3 grid && positive3 threadgroup) then
        error op Error.Invalid_argument "dispatch dimensions must be positive"
      else dispatch ()

let dispatch_threads encoder ~threads ~threadgroup =
  dispatch "Backend.dispatch_threads" encoder threads threadgroup (fun () ->
      encoder.compute_raw.dispatch_threads ~threads ~threadgroup)

let end_compute (encoder : compute_encoder) =
  let op = "Backend.end_compute" in
  match open_compute op encoder with
  | Error _ as e -> e
  | Ok () -> (
      match encoder.compute_raw.end_compute () with
      | Error _ as e -> e
      | Ok () ->
          encoder.compute_open <- false;
          encoder.compute_commands.state <- Recording;
          Ok ())

let accel_encoder commands =
  let op = "Backend.accel_encoder" in
  match recording op commands with
  | Error _ as e -> e
  | Ok () -> (
      match
        Caps.require ~operation:op (commands_device commands).raw.capabilities Caps.Ray_tracing
      with
      | Error _ as e -> e
      | Ok () -> (
          match commands.commands_raw.accel_encoder () with
          | Error _ as e -> e
          | Ok raw ->
              commands.state <- Encoding;
              Ok { accel_encoder_raw = raw; accel_commands = commands; accel_open = true }))

let open_accel op (encoder : accel_encoder) =
  if encoder.accel_open then Ok () else error op Error.Invalid_state "acceleration encoder is ended"

let scratch_alignment = 256L

let check_scratch op device (scratch : buffer) offset required =
  match check_resource op device scratch.resource with
  | Error _ as e -> e
  | Ok () ->
      if
        offset < 0L
        || Int64.rem offset scratch_alignment <> 0L
        || offset > Int64.sub scratch.buffer_descriptor.size required
      then error op Error.Invalid_argument "scratch range is invalid, unaligned, or insufficient"
      else Ok ()

let accel_operation op (encoder : accel_encoder) (accel : accel) scratch scratch_offset required
    transition encode =
  match open_accel op encoder with
  | Error _ as e -> e
  | Ok () -> (
      let device = commands_device encoder.accel_commands in
      match check_accel op device accel with
      | Error _ as e -> e
      | Ok () -> (
          match check_scratch op device scratch scratch_offset required with
          | Error _ as e -> e
          | Ok () -> ( match transition () with Error _ as e -> e | Ok _ -> encode ())))

let build_accel encoder accel ~scratch ?(scratch_offset = 0L) () =
  let op = "Backend.build_accel" in
  let device = commands_device encoder.accel_commands in
  if
    List.exists
      (fun (structure : accel) ->
        structure.accel_dead || not (Acceleration.built structure.portable))
      accel.accel_structures
  then error op Error.Invalid_state "TLAS references a destroyed or unbuilt structure"
  else
    accel_operation op encoder accel scratch scratch_offset (accel_sizes accel).build_scratch_size
      (fun () -> Acceleration.build device.handle accel.portable)
      (fun () ->
        encoder.accel_encoder_raw.build accel.accel_raw.accel_token
          ~scratch:scratch.resource.raw.token ~scratch_offset)

let accel_pair op (encoder : accel_encoder) ~(src : accel) ~(dst : accel) transition encode =
  match open_accel op encoder with
  | Error _ as e -> e
  | Ok () -> (
      let device = commands_device encoder.accel_commands in
      match check_accel op device src with
      | Error _ as e -> e
      | Ok () -> (
          match check_accel op device dst with
          | Error _ as e -> e
          | Ok () -> (
              if (accel_sizes dst).structure_size <= 0L then
                error op Error.Invalid_argument "destination structure is empty"
              else match transition device with Error _ as e -> e | Ok () -> encode ())))

let compact_accel encoder ~src ~dst =
  let op = "Backend.compact_accel" in
  accel_pair op encoder ~src ~dst
    (fun device ->
      Acceleration.compact_into device.handle ~source:src.portable ~destination:dst.portable)
    (fun () ->
      encoder.accel_encoder_raw.compact ~src:src.accel_raw.accel_token
        ~dst:dst.accel_raw.accel_token)

let write_compacted_size encoder (accel : accel) ~(dst : buffer) ?(offset = 0L) () =
  let op = "Backend.write_compacted_size" in
  match open_accel op encoder with
  | Error _ as e -> e
  | Ok () -> (
      let device = commands_device encoder.accel_commands in
      match check_accel op device accel with
      | Error _ as e -> e
      | Ok () -> (
          match check_resource op device dst.resource with
          | Error _ as e -> e
          | Ok () -> (
              if
                offset < 0L
                || Int64.rem offset 8L <> 0L
                || offset > Int64.sub dst.buffer_descriptor.size 8L
              then
                error op Error.Invalid_argument
                  "compacted size destination needs eight aligned bytes"
              else
                match Acceleration.compacted_size device.handle accel.portable with
                | Error _ as e -> e
                | Ok () -> (
                    match
                      encoder.accel_encoder_raw.write_compacted_size accel.accel_raw.accel_token
                        ~dst:dst.resource.raw.token ~offset
                    with
                    | Error _ as e -> e
                    | Ok () ->
                        mark_submitted encoder.accel_commands.commands_queue dst.resource;
                        Ok ()))))

let end_accel (encoder : accel_encoder) =
  let op = "Backend.end_accel" in
  match open_accel op encoder with
  | Error _ as e -> e
  | Ok () -> (
      match encoder.accel_encoder_raw.end_accel () with
      | Error _ as e -> e
      | Ok () ->
          encoder.accel_open <- false;
          encoder.accel_commands.state <- Recording;
          Ok ())

let blit_encoder commands =
  let op = "Backend.blit_encoder" in
  match recording op commands with
  | Error _ as e -> e
  | Ok () -> (
      match commands.commands_raw.blit_encoder () with
      | Error _ as e -> e
      | Ok raw ->
          commands.state <- Encoding;
          Ok { blit_raw = raw; blit_commands = commands; blit_open = true })

let open_blit op (encoder : blit_encoder) =
  if encoder.blit_open then Ok () else error op Error.Invalid_state "blit encoder is ended"

let end_blit (encoder : blit_encoder) =
  let op = "Backend.end_blit" in
  match open_blit op encoder with
  | Error _ as e -> e
  | Ok () -> (
      match encoder.blit_raw.end_blit () with
      | Error _ as e -> e
      | Ok () ->
          encoder.blit_open <- false;
          encoder.blit_commands.state <- Recording;
          Ok ())

let commit (commands : commands) =
  match recording "Backend.commit" commands with
  | Error _ as e -> e
  | Ok () -> (
      match commands.commands_raw.commit () with
      | Error _ as e -> e
      | Ok receipt ->
          commands.state <- Committed;
          Ok receipt)

let abandon (commands : commands) =
  match commands.state with
  | Committed -> error "Backend.abandon" Error.Invalid_state "commands are already committed"
  | Abandoned -> Ok ()
  | Recording | Encoding -> (
      match commands.commands_raw.abandon () with
      | Error _ as e -> e
      | Ok () ->
          commands.state <- Abandoned;
          Ok ())

let gpu_duration (queue : queue) (receipt : receipt) =
  if queue.dead then None else queue.raw.gpu_duration receipt.epoch

let gpu_timing (queue : queue) = queue.raw.gpu_timing ()

(* Samplers, render pipelines, argument buffers, indirect command buffers,
   and the render encoder. *)
type sampler = {
  sampler_raw : driver_sampler;
  sampler_device : device;
  mutable sampler_dead : bool;
}

type icb = {
  icb_raw : driver_icb;
  icb_device : device;
  icb_capacity : int;
  mutable icb_dead : bool;
}

type argument = {
  argument_raw : driver_argument;
  argument_device : device;
  mutable argument_dead : bool;
}

type render_encoder = {
  render_raw : driver_render_encoder;
  render_commands : commands;
  mutable render_open : bool;
  mutable render_pipeline_set : bool;
  mutable render_shape : pipeline_shape;
}

type color_attachment = {
  texture : texture;
  resolve : texture option;
  load : Render_pass.load;
  store : Render_pass.store;
  clear : float * float * float * float;
}

type depth_attachment = {
  depth_texture : texture;
  depth_load : Render_pass.load;
  depth_store : Render_pass.store;
  depth_clear : float;
}

type render_target = { colors : color_attachment list; depth : depth_attachment option }

type batch_draw = {
  pipeline : pipeline;
  buffers : (shader_stage * int * buffer * int64) array;
  primitive : Render_pass.primitive;
  index : (Render_pass.index_type * buffer * int64 * int64) option;
  vertex_start : int;
  vertex_count : int;
  instances : int;
}

let create_sampler device descriptor =
  let op = "Backend.create_sampler" in
  match live op device with
  | Error _ as e -> e
  | Ok () -> (
      match Validation.validate_sampler descriptor with
      | Error _ as e -> e
      | Ok () -> (
          match device.raw.create_sampler descriptor with
          | Error _ as e -> e
          | Ok raw ->
              device.children <- device.children + 1;
              Ok { sampler_raw = raw; sampler_device = device; sampler_dead = false }))

let destroy_sampler (value : sampler) =
  if value.sampler_dead then Ok ()
  else
    match value.sampler_raw.destroy_sampler () with
    | Error _ as e -> e
    | Ok () ->
        value.sampler_dead <- true;
        value.sampler_device.children <- value.sampler_device.children - 1;
        Ok ()

let create_render_pipeline ?(blend = Pipeline.Replace) ?(topology = Render_pass.Triangle_list)
    ?(indirect = false) device descriptor =
  let op = "Backend.create_render_pipeline" in
  match live op device with
  | Error _ as e -> e
  | Ok () -> (
      match Caps.require ~operation:op device.raw.capabilities Caps.Render_pipeline with
      | Error _ as e -> e
      | Ok () -> (
          match Pipeline.create_render device.raw.capabilities descriptor with
          | Error _ as e -> e
          | Ok () -> (
              if topology <> Render_pass.Triangle_list && topology <> Render_pass.Point_list then
                error op Error.Invalid_argument
                  "pipeline topology class must be triangles or points"
              else
                match
                  device.raw.create_render_pipeline { blend; topology; indirect } descriptor
                with
                | Error _ as e -> e
                | Ok pipeline_driver ->
                    device.children <- device.children + 1;
                    Ok
                      {
                        pipeline_driver;
                        device;
                        pipeline_library = None;
                        pipeline_indirect = indirect;
                        pipeline_shape = Vertex_shape;
                        dead = false;
                      })))

let pipeline_indirect (value : pipeline) = value.pipeline_indirect

let create_icb device ~max_commands =
  let op = "Backend.create_icb" in
  match live op device with
  | Error _ as e -> e
  | Ok () -> (
      if max_commands <= 0 || max_commands > 65_536 then
        error op Error.Invalid_argument "indirect command capacity must be in 1..65536"
      else
        match device.raw.create_icb ~max_commands with
        | Error _ as e -> e
        | Ok raw ->
            device.children <- device.children + 1;
            Ok { icb_raw = raw; icb_device = device; icb_capacity = max_commands; icb_dead = false }
      )

let check_icb op (value : icb) index =
  if value.icb_dead then error op Error.Stale_handle "indirect command buffer is destroyed"
  else if index < 0 || index >= value.icb_capacity then
    error op Error.Invalid_argument "indirect command index is out of range"
  else Ok ()

let icb_set_pipeline (value : icb) ~index (pipeline : pipeline) =
  let op = "Backend.icb_set_pipeline" in
  match check_icb op value index with
  | Error _ as e -> e
  | Ok () ->
      if pipeline.dead then error op Error.Stale_handle "pipeline is destroyed"
      else if pipeline.device != value.icb_device then
        error op Error.Cross_device "pipeline belongs to another device"
      else value.icb_raw.icb_set_pipeline ~index pipeline.pipeline_driver.pipeline_token

let icb_set_buffer (value : icb) ~index stage ~slot ?(offset = 0L) (buffer : buffer) =
  let op = "Backend.icb_set_buffer" in
  match check_icb op value index with
  | Error _ as e -> e
  | Ok () -> (
      match valid_slot op slot with
      | Error _ as e -> e
      | Ok () -> (
          match check_resource op value.icb_device buffer.resource with
          | Error _ as e -> e
          | Ok () ->
              if offset < 0L || offset >= buffer.buffer_descriptor.size then
                error op Error.Invalid_argument "buffer offset is out of range"
              else value.icb_raw.icb_set_buffer ~index stage ~slot ~offset buffer.resource.raw.token
          ))

let valid_draw op primitive ~first ~count ~instances =
  if first < 0 || count <= 0 || instances <= 0 then
    error op Error.Invalid_argument "draw range is invalid"
  else if primitive = Render_pass.Triangle_list && count mod 3 <> 0 then
    error op Error.Invalid_argument "triangle-list vertex count is not divisible by three"
  else if primitive = Render_pass.Line_list && count mod 2 <> 0 then
    error op Error.Invalid_argument "line-list vertex count is not divisible by two"
  else if primitive = Render_pass.Triangle_strip && count < 3 then
    error op Error.Invalid_argument "triangle strip requires at least three vertices"
  else Ok ()

let valid_indexed op device primitive index_type (buffer : buffer) ~offset ~count ~instances =
  match check_resource op device buffer.resource with
  | Error _ as e -> e
  | Ok () ->
      let stride = match index_type with Render_pass.Uint16 -> 2L | Uint32 -> 4L in
      if
        count <= 0L || offset < 0L
        || Int64.rem offset stride <> 0L
        || count > Int64.div (Int64.sub buffer.buffer_descriptor.size offset) stride
        || instances <= 0
      then error op Error.Invalid_argument "index range is invalid"
      else if primitive = Render_pass.Triangle_list && Int64.rem count 3L <> 0L then
        error op Error.Invalid_argument "triangle-list index count is not divisible by three"
      else if primitive = Render_pass.Line_list && Int64.rem count 2L <> 0L then
        error op Error.Invalid_argument "line-list index count is not divisible by two"
      else Ok ()

let icb_draw (value : icb) ~index ~primitive ~first ~count ?(instances = 1) () =
  let op = "Backend.icb_draw" in
  match check_icb op value index with
  | Error _ as e -> e
  | Ok () -> (
      match valid_draw op primitive ~first ~count ~instances with
      | Error _ as e -> e
      | Ok () -> value.icb_raw.icb_draw ~index ~primitive ~first ~count ~instances)

let icb_draw_indexed (value : icb) ~index ~primitive ~index_type (buffer : buffer) ~offset ~count
    ?(instances = 1) () =
  let op = "Backend.icb_draw_indexed" in
  match check_icb op value index with
  | Error _ as e -> e
  | Ok () -> (
      match
        valid_indexed op value.icb_device primitive index_type buffer ~offset ~count ~instances
      with
      | Error _ as e -> e
      | Ok () ->
          value.icb_raw.icb_draw_indexed ~index ~primitive ~index_type buffer.resource.raw.token
            ~offset ~count ~instances)

let destroy_icb (value : icb) =
  if value.icb_dead then Ok ()
  else
    match value.icb_raw.destroy_icb () with
    | Error _ as e -> e
    | Ok () ->
        value.icb_dead <- true;
        value.icb_device.children <- value.icb_device.children - 1;
        Ok ()

let create_argument (pipeline : pipeline) stage ~index =
  let op = "Backend.create_argument" in
  if pipeline.dead then error op Error.Stale_handle "pipeline is destroyed"
  else
    match live op pipeline.device with
    | Error _ as e -> e
    | Ok () -> (
        match valid_slot op index with
        | Error _ as e -> e
        | Ok () -> (
            match
              pipeline.device.raw.create_argument ~pipeline:pipeline.pipeline_driver.pipeline_token
                stage ~index
            with
            | Error _ as e -> e
            | Ok raw ->
                pipeline.device.children <- pipeline.device.children + 1;
                Ok { argument_raw = raw; argument_device = pipeline.device; argument_dead = false })
        )

let argument_length (value : argument) = value.argument_raw.argument_length
let argument_alignment (value : argument) = value.argument_raw.argument_alignment

let argument_target op (value : argument) (buffer : buffer) offset =
  if value.argument_dead then error op Error.Stale_handle "argument encoder is destroyed"
  else
    match check_resource op value.argument_device buffer.resource with
    | Error _ as e -> e
    | Ok () ->
        if
          offset < 0L
          || Int64.rem offset (Int64.of_int (max 1 value.argument_raw.argument_alignment)) <> 0L
          || offset
             > Int64.sub buffer.buffer_descriptor.size
                 (Int64.of_int value.argument_raw.argument_length)
        then error op Error.Invalid_argument "argument buffer range is invalid or unaligned"
        else Ok ()

let argument_texture (value : argument) (buffer : buffer) ~offset ~slot (texture : texture) =
  let op = "Backend.argument_texture" in
  match argument_target op value buffer offset with
  | Error _ as e -> e
  | Ok () -> (
      match check_resource op value.argument_device texture.resource with
      | Error _ as e -> e
      | Ok () ->
          value.argument_raw.argument_texture ~buffer:buffer.resource.raw.token ~offset ~slot
            texture.resource.raw.token)

let argument_sampler (value : argument) (buffer : buffer) ~offset ~slot (sampler : sampler) =
  let op = "Backend.argument_sampler" in
  match argument_target op value buffer offset with
  | Error _ as e -> e
  | Ok () ->
      if sampler.sampler_dead then error op Error.Stale_handle "sampler is destroyed"
      else if sampler.sampler_device != value.argument_device then
        error op Error.Cross_device "sampler belongs to another device"
      else
        value.argument_raw.argument_sampler ~buffer:buffer.resource.raw.token ~offset ~slot
          sampler.sampler_raw.sampler_token

let destroy_argument (value : argument) =
  if value.argument_dead then Ok ()
  else
    match value.argument_raw.destroy_argument () with
    | Error _ as e -> e
    | Ok () ->
        value.argument_dead <- true;
        value.argument_device.children <- value.argument_device.children - 1;
        Ok ()

let render_encoder commands (target : render_target) =
  let op = "Backend.render_encoder" in
  match recording op commands with
  | Error _ as e -> e
  | Ok () -> (
      let device = commands_device commands in
      match Caps.require ~operation:op device.raw.capabilities Caps.Render_pipeline with
      | Error _ as e -> e
      | Ok () -> (
          match target.colors with
          | [] -> error op Error.Invalid_argument "render target needs one color attachment"
          | _ :: _ :: _ -> error op Error.Unsupported "multiple color attachments are not exposed"
          | [ color ] -> (
              let attachment_check (t : texture) =
                match check_resource op device t.resource with
                | Error _ as e -> e
                | Ok () ->
                    if not (List.mem Types.Render_attachment t.texture_descriptor.usage) then
                      error op Error.Invalid_argument
                        "attachment texture lacks Render_attachment usage"
                    else Ok ()
              in
              match attachment_check color.texture with
              | Error _ as e -> e
              | Ok () -> (
                  let width = color.texture.texture_descriptor.width
                  and height = color.texture.texture_descriptor.height
                  and samples = color.texture.texture_descriptor.sample_count in
                  let same_extent (t : texture) =
                    t.texture_descriptor.width = width && t.texture_descriptor.height = height
                  in
                  let resolve_ok =
                    match color.resolve with
                    | None ->
                        if color.store = Render_pass.Resolve then
                          error op Error.Invalid_argument "resolve store needs a resolve texture"
                        else Ok ()
                    | Some resolve -> (
                        if samples = 1 then
                          error op Error.Invalid_argument "single-sample attachment cannot resolve"
                        else if color.store = Render_pass.Discard then
                          error op Error.Invalid_argument
                            "a resolve texture needs a Store or Resolve store action"
                        else
                          match attachment_check resolve with
                          | Error _ as e -> e
                          | Ok () ->
                              if
                                (not (same_extent resolve))
                                || resolve.texture_descriptor.sample_count <> 1
                              then
                                error op Error.Invalid_argument
                                  "resolve texture extent or sample count differs"
                              else Ok ())
                  in
                  match resolve_ok with
                  | Error _ as e -> e
                  | Ok () -> (
                      let depth_ok =
                        match target.depth with
                        | None -> Ok ()
                        | Some d -> (
                            match attachment_check d.depth_texture with
                            | Error _ as e -> e
                            | Ok () ->
                                if
                                  (not (same_extent d.depth_texture))
                                  || d.depth_texture.texture_descriptor.sample_count <> samples
                                then
                                  error op Error.Invalid_argument
                                    "depth attachment extent or sample count differs"
                                else if d.depth_store = Render_pass.Resolve then
                                  error op Error.Invalid_argument "depth attachments cannot resolve"
                                else if
                                  (not (Float.is_finite d.depth_clear))
                                  || d.depth_clear < 0. || d.depth_clear > 1.
                                then error op Error.Invalid_argument "depth clear is outside [0,1]"
                                else Ok ())
                      in
                      match depth_ok with
                      | Error _ as e -> e
                      | Ok () -> (
                          let r, g, b, a = color.clear in
                          if not (List.for_all Float.is_finite [ r; g; b; a ]) then
                            error op Error.Invalid_argument "clear color is not finite"
                          else
                            let token (t : texture) = t.resource.raw.token in
                            let driver_target =
                              {
                                target_colors =
                                  [|
                                    {
                                      color = token color.texture;
                                      color_resolve = Option.map token color.resolve;
                                      color_load = color.load;
                                      color_store = color.store;
                                      color_clear = color.clear;
                                    };
                                  |];
                                target_depth =
                                  Option.map
                                    (fun d ->
                                      {
                                        depth = token d.depth_texture;
                                        depth_load = d.depth_load;
                                        depth_store = d.depth_store;
                                        depth_clear = d.depth_clear;
                                      })
                                    target.depth;
                                target_width = width;
                                target_height = height;
                                target_samples = samples;
                              }
                            in
                            match commands.commands_raw.render_encoder driver_target with
                            | Error _ as e -> e
                            | Ok raw ->
                                let queue = commands.commands_queue in
                                mark_submitted queue color.texture.resource;
                                Option.iter
                                  (fun (t : texture) -> mark_submitted queue t.resource)
                                  color.resolve;
                                Option.iter
                                  (fun d -> mark_submitted queue d.depth_texture.resource)
                                  target.depth;
                                commands.state <- Encoding;
                                Ok
                                  {
                                    render_raw = raw;
                                    render_commands = commands;
                                    render_open = true;
                                    render_pipeline_set = false;
                                    render_shape = Vertex_shape;
                                  }))))))

let open_render op (encoder : render_encoder) =
  if encoder.render_open then Ok () else error op Error.Invalid_state "render encoder is ended"

let render_device (encoder : render_encoder) = commands_device encoder.render_commands

let set_render_pipeline (encoder : render_encoder) (pipeline : pipeline) =
  let op = "Backend.set_render_pipeline" in
  match open_render op encoder with
  | Error _ as e -> e
  | Ok () -> (
      if pipeline.dead then error op Error.Stale_handle "pipeline is destroyed"
      else if pipeline.device != render_device encoder then
        error op Error.Cross_device "pipeline belongs to another device"
      else if pipeline.pipeline_shape = Compute_shape then
        error op Error.Invalid_argument "compute pipelines cannot be set on a render encoder"
      else
        match encoder.render_raw.render_set_pipeline pipeline.pipeline_driver.pipeline_token with
        | Error _ as e -> e
        | Ok () ->
            encoder.render_pipeline_set <- true;
            encoder.render_shape <- pipeline.pipeline_shape;
            Ok ())

let set_stage_buffer (encoder : render_encoder) stage ~index ?(offset = 0L) (buffer : buffer) =
  let op = "Backend.set_stage_buffer" in
  match open_render op encoder with
  | Error _ as e -> e
  | Ok () -> (
      match valid_slot op index with
      | Error _ as e -> e
      | Ok () -> (
          match check_resource op (render_device encoder) buffer.resource with
          | Error _ as e -> e
          | Ok () -> (
              if offset < 0L || offset >= buffer.buffer_descriptor.size then
                error op Error.Invalid_argument "buffer offset is out of range"
              else
                match
                  encoder.render_raw.set_stage_buffer stage ~index ~offset buffer.resource.raw.token
                with
                | Error _ as e -> e
                | Ok () ->
                    mark_submitted encoder.render_commands.commands_queue buffer.resource;
                    Ok ())))

let set_stage_texture (encoder : render_encoder) stage ~index (texture : texture) =
  let op = "Backend.set_stage_texture" in
  match open_render op encoder with
  | Error _ as e -> e
  | Ok () -> (
      match valid_slot op index with
      | Error _ as e -> e
      | Ok () -> (
          match check_resource op (render_device encoder) texture.resource with
          | Error _ as e -> e
          | Ok () -> (
              if not (List.mem Types.Texture_binding texture.texture_descriptor.usage) then
                error op Error.Invalid_argument "texture lacks Texture_binding usage"
              else
                match
                  encoder.render_raw.set_stage_texture stage ~index texture.resource.raw.token
                with
                | Error _ as e -> e
                | Ok () ->
                    mark_submitted encoder.render_commands.commands_queue texture.resource;
                    Ok ())))

let set_stage_sampler (encoder : render_encoder) stage ~index (sampler : sampler) =
  let op = "Backend.set_stage_sampler" in
  match open_render op encoder with
  | Error _ as e -> e
  | Ok () -> (
      match valid_slot op index with
      | Error _ as e -> e
      | Ok () ->
          if sampler.sampler_dead then error op Error.Stale_handle "sampler is destroyed"
          else if sampler.sampler_device != render_device encoder then
            error op Error.Cross_device "sampler belongs to another device"
          else encoder.render_raw.set_stage_sampler stage ~index sampler.sampler_raw.sampler_token)

let valid_rect op (rect : Render_pass.rect) =
  if rect.x < 0 || rect.y < 0 || rect.width <= 0 || rect.height <= 0 then
    error op Error.Invalid_argument "rectangle is empty or negative"
  else Ok ()

let set_viewport (encoder : render_encoder) (rect : Render_pass.rect) =
  let op = "Backend.set_viewport" in
  (* A viewport maps clip space and may hang over the target; only its size is checked. *)
  match open_render op encoder with
  | Error _ as e -> e
  | Ok () ->
      if rect.width <= 0 || rect.height <= 0 then
        error op Error.Invalid_argument "viewport is empty"
      else encoder.render_raw.set_viewport rect

let set_scissor (encoder : render_encoder) rect =
  let op = "Backend.set_scissor" in
  match open_render op encoder with
  | Error _ as e -> e
  | Ok () -> (
      match valid_rect op rect with
      | Error _ as e -> e
      | Ok () -> encoder.render_raw.set_scissor rect)

let set_cull (encoder : render_encoder) cull =
  match open_render "Backend.set_cull" encoder with
  | Error _ as e -> e
  | Ok () -> encoder.render_raw.set_cull cull

let set_winding (encoder : render_encoder) winding =
  match open_render "Backend.set_winding" encoder with
  | Error _ as e -> e
  | Ok () -> encoder.render_raw.set_winding winding

let set_depth_state (encoder : render_encoder) state =
  match open_render "Backend.set_depth_state" encoder with
  | Error _ as e -> e
  | Ok () -> encoder.render_raw.set_depth_state state

let vertex_pipeline op (encoder : render_encoder) =
  if not encoder.render_pipeline_set then error op Error.Invalid_state "no render pipeline is set"
  else if encoder.render_shape <> Vertex_shape then
    error op Error.Invalid_state "vertex draws need a vertex/fragment pipeline"
  else Ok ()

let draw (encoder : render_encoder) ~primitive ~first ~count ?(instances = 1) () =
  let op = "Backend.draw" in
  match open_render op encoder with
  | Error _ as e -> e
  | Ok () -> (
      match vertex_pipeline op encoder with
      | Error _ as e -> e
      | Ok () -> (
          if false then error op Error.Invalid_state "no render pipeline is set"
          else
            match valid_draw op primitive ~first ~count ~instances with
            | Error _ as e -> e
            | Ok () -> encoder.render_raw.draw ~primitive ~first ~count ~instances))

let draw_indexed (encoder : render_encoder) ~primitive ~index_type (buffer : buffer) ~offset ~count
    ?(instances = 1) () =
  let op = "Backend.draw_indexed" in
  match open_render op encoder with
  | Error _ as e -> e
  | Ok () -> (
      if not encoder.render_pipeline_set then
        error op Error.Invalid_state "no render pipeline is set"
      else
        match
          valid_indexed op (render_device encoder) primitive index_type buffer ~offset ~count
            ~instances
        with
        | Error _ as e -> e
        | Ok () -> (
            match
              encoder.render_raw.draw_indexed ~primitive ~index_type buffer.resource.raw.token
                ~offset ~count ~instances
            with
            | Error _ as e -> e
            | Ok () ->
                mark_submitted encoder.render_commands.commands_queue buffer.resource;
                Ok ()))

(* The index and instance counts are five uints the device wrote at [arguments_offset] (index
   count, instance count, first index, base vertex, base instance), read when the draw runs. *)
let draw_indexed_indirect (encoder : render_encoder) ~primitive ~index_type (buffer : buffer)
    ~offset (arguments : buffer) ~arguments_offset =
  let op = "Backend.draw_indexed_indirect" in
  match open_render op encoder with
  | Error _ as e -> e
  | Ok () -> (
      if not encoder.render_pipeline_set then
        error op Error.Invalid_state "no render pipeline is set"
      else
        let device = render_device encoder in
        match check_resource op device buffer.resource with
        | Error _ as e -> e
        | Ok () -> (
            match check_resource op device arguments.resource with
            | Error _ as e -> e
            | Ok () -> (
                let stride = match index_type with Render_pass.Uint16 -> 2L | Uint32 -> 4L in
                if
                  offset < 0L
                  || Int64.rem offset stride <> 0L
                  || offset >= buffer.buffer_descriptor.size
                then error op Error.Invalid_argument "index offset is invalid"
                else if
                  arguments_offset < 0L
                  || Int64.rem arguments_offset 4L <> 0L
                  || Int64.add arguments_offset 20L > arguments.buffer_descriptor.size
                then error op Error.Invalid_argument "indirect arguments range is invalid"
                else
                  match
                    encoder.render_raw.draw_indexed_indirect ~primitive ~index_type
                      buffer.resource.raw.token ~offset arguments.resource.raw.token
                      ~arguments_offset
                  with
                  | Error _ as e -> e
                  | Ok () ->
                      mark_submitted encoder.render_commands.commands_queue buffer.resource;
                      mark_submitted encoder.render_commands.commands_queue arguments.resource;
                      Ok ())))

let draw_batch (encoder : render_encoder) (draws : batch_draw array) =
  let op = "Backend.draw_batch" in
  match open_render op encoder with
  | Error _ as e -> e
  | Ok () -> (
      let device = render_device encoder in
      if Array.length draws = 0 then error op Error.Invalid_argument "draw batch is empty"
      else
        (* The one validation of every draw; backends trust the lowered batch. *)
        let rec check_buffers (buffers : (shader_stage * int * buffer * int64) array) j =
          if j = Array.length buffers then Ok ()
          else
            let _, index, (b : buffer), offset = Array.unsafe_get buffers j in
            match valid_slot op index with
            | Error _ as e -> e
            | Ok () -> (
                match check_resource op device b.resource with
                | Error _ as e -> e
                | Ok () ->
                    if offset < 0L || offset >= b.buffer_descriptor.size then
                      error op Error.Invalid_argument "buffer offset is out of range"
                    else check_buffers buffers (j + 1))
        in
        let rec check i =
          if i = Array.length draws then Ok ()
          else
            let d = draws.(i) in
            if d.pipeline.dead then error op Error.Stale_handle "pipeline is destroyed"
            else if d.pipeline.device != device then
              error op Error.Cross_device "pipeline belongs to another device"
            else
              match check_buffers d.buffers 0 with
              | Error _ as e -> e
              | Ok () -> (
                  match d.index with
                  | None -> (
                      match
                        valid_draw op d.primitive ~first:d.vertex_start ~count:d.vertex_count
                          ~instances:d.instances
                      with
                      | Error _ as e -> e
                      | Ok () -> check (i + 1))
                  | Some (index_type, b, offset, count) -> (
                      match
                        valid_indexed op device d.primitive index_type b ~offset ~count
                          ~instances:d.instances
                      with
                      | Error _ as e -> e
                      | Ok () -> check (i + 1)))
        in
        match check 0 with
        | Error _ as e -> e
        | Ok () -> (
            let lowered =
              Array.map
                (fun d ->
                  {
                    batch_pipeline = d.pipeline.pipeline_driver.pipeline_token;
                    batch_buffers =
                      Array.map
                        (fun (stage, index, (b : buffer), offset) ->
                          (stage, index, b.resource.raw.token, offset))
                        d.buffers;
                    batch_primitive = d.primitive;
                    batch_index =
                      Option.map
                        (fun (t, (b : buffer), o, c) -> (t, b.resource.raw.token, o, c))
                        d.index;
                    batch_vertex_start = d.vertex_start;
                    batch_vertex_count = d.vertex_count;
                    batch_instances = d.instances;
                  })
                draws
            in
            match encoder.render_raw.draw_batch lowered with
            | Error _ as e -> e
            | Ok () ->
                encoder.render_pipeline_set <- true;
                Ok ()))

let use_resources (encoder : render_encoder) resources =
  let op = "Backend.use_resources" in
  match open_render op encoder with
  | Error _ as e -> e
  | Ok () -> (
      let device = render_device encoder and queue = encoder.render_commands.commands_queue in
      let rec tokens acc = function
        | [] -> Ok (List.rev acc)
        | `Buffer (b : buffer) :: rest -> (
            match check_resource op device b.resource with
            | Error _ as e -> e
            | Ok () ->
                mark_submitted queue b.resource;
                tokens (b.resource.raw.token :: acc) rest)
        | `Texture (t : texture) :: rest -> (
            match check_resource op device t.resource with
            | Error _ as e -> e
            | Ok () ->
                mark_submitted queue t.resource;
                tokens (t.resource.raw.token :: acc) rest)
      in
      match tokens [] resources with
      | Error _ as e -> e
      | Ok tokens -> encoder.render_raw.use_resources tokens)

let execute_icb (encoder : render_encoder) (icb : icb) ~location ~length =
  let op = "Backend.execute_icb" in
  match open_render op encoder with
  | Error _ as e -> e
  | Ok () ->
      if not encoder.render_pipeline_set then
        error op Error.Invalid_state "no render pipeline is set"
      else if icb.icb_dead then error op Error.Stale_handle "indirect command buffer is destroyed"
      else if icb.icb_device != render_device encoder then
        error op Error.Cross_device "indirect command buffer belongs to another device"
      else if location < 0 || length <= 0 || location > icb.icb_capacity - length then
        error op Error.Invalid_argument "indirect command range is out of range"
      else encoder.render_raw.execute_icb icb.icb_raw.icb_token ~location ~length

let end_render (encoder : render_encoder) =
  let op = "Backend.end_render" in
  match open_render op encoder with
  | Error _ as e -> e
  | Ok () -> (
      match encoder.render_raw.end_render () with
      | Error _ as e -> e
      | Ok () ->
          encoder.render_open <- false;
          encoder.render_commands.state <- Recording;
          Ok ())

(* Blit region validation, formerly in the description-based transfer pass. *)
let blit_range size offset length =
  offset >= 0L && length > 0L && offset <= size && length <= Int64.sub size offset

let texrange (d : Types.texture_descriptor) mip (o : Types.origin) (e : Types.extent) =
  mip >= 0 && mip < d.mip_levels && o.x >= 0 && o.y >= 0 && o.z >= 0 && e.width > 0 && e.height > 0
  && e.depth > 0
  && o.x <= max 1 (d.width lsr mip) - e.width
  && o.y <= max 1 (d.height lsr mip) - e.height
  && o.z <= max 1 (d.depth lsr mip) - e.depth

let pitches texel (e : Types.extent) row image =
  let height = Int64.of_int e.height in
  row >= Int64.mul (Int64.of_int e.width) (Int64.of_int texel)
  && Int64.rem row 256L = 0L
  && height > 0L
  && row <= Int64.div Int64.max_int height
  && image >= Int64.mul row height

let footprint image depth =
  let depth = Int64.of_int depth in
  if depth <= 0L || image <= 0L || image > Int64.div Int64.max_int depth then None
  else Some (Int64.mul image depth)

let buffer_texture_region op ~buffer_usage ~texture_usage (b : buffer) offset row image
    (x : texture) mip origin (extent : Types.extent) =
  if
    not
      (List.mem buffer_usage b.buffer_descriptor.usage
      && List.mem texture_usage x.texture_descriptor.usage)
  then error op Error.Invalid_argument "buffer or texture usage does not permit the copy"
  else if
    not
      (texrange x.texture_descriptor mip origin extent
      && pitches (Types.texel_bytes x.texture_descriptor.format) extent row image)
  then error op Error.Invalid_argument "texture region or row pitch is invalid"
  else
    match footprint image extent.depth with
    | Some size when blit_range b.buffer_descriptor.size offset size -> Ok ()
    | _ -> error op Error.Invalid_argument "buffer range does not cover the texture region"

let buffer_to_texture (encoder : blit_encoder) ~(src : buffer) ?(offset = 0L) ~bytes_per_row
    ~bytes_per_image ~(dst : texture) ?(mip = 0) ?(origin = { Types.x = 0; y = 0; z = 0 }) ~extent
    () =
  let op = "Backend.buffer_to_texture" in
  match open_blit op encoder with
  | Error _ as e -> e
  | Ok () -> (
      let device = commands_device encoder.blit_commands in
      match check_resource op device src.resource with
      | Error _ as e -> e
      | Ok () -> (
          match check_resource op device dst.resource with
          | Error _ as e -> e
          | Ok () -> (
              match
                buffer_texture_region op ~buffer_usage:Types.Copy_src
                  ~texture_usage:Types.Texture_copy_dst src offset bytes_per_row bytes_per_image dst
                  mip origin extent
              with
              | Error _ as e -> e
              | Ok () -> (
                  match
                    encoder.blit_raw.buffer_to_texture ~src:src.resource.raw.token ~offset
                      ~bytes_per_row ~bytes_per_image ~dst:dst.resource.raw.token ~mip ~origin
                      ~extent
                  with
                  | Error _ as e -> e
                  | Ok () ->
                      mark_submitted encoder.blit_commands.commands_queue src.resource;
                      mark_submitted encoder.blit_commands.commands_queue dst.resource;
                      Ok ()))))

let commit_present (commands : commands) ~(source : texture) (frame : frame) =
  let op = "Backend.commit_present" in
  match recording op commands with
  | Error _ as e -> e
  | Ok () -> (
      let queue = commands.commands_queue in
      match validate_present op queue source frame with
      | Error _ as e -> e
      | Ok () -> (
          match
            commands.commands_raw.commit_present ~source:source.resource.raw.token frame.raw
          with
          | Error _ as e -> e
          | Ok receipt ->
              commands.state <- Committed;
              consume_present frame;
              Ok receipt))

let compute_use_accels (encoder : compute_encoder) accels =
  let op = "Backend.compute_use_accels" in
  match open_compute op encoder with
  | Error _ as e -> e
  | Ok () -> (
      let device = commands_device encoder.compute_commands in
      let rec check = function
        | [] -> Ok ()
        | (accel : accel) :: rest -> (
            match check_accel op device accel with
            | Error _ as e -> e
            | Ok () ->
                if not (Acceleration.built accel.portable) then
                  error op Error.Invalid_state "acceleration structure has no encoded build"
                else check rest)
      in
      match check accels with
      | Error _ as e -> e
      | Ok () ->
          if accels = [] then Ok ()
          else
            encoder.compute_raw.compute_use_accels
              (List.map (fun (a : accel) -> a.accel_raw.accel_token) accels))

let texture_driver_token (texture : texture) =
  (Handle.device_id texture.resource.device.handle, texture.resource.raw.token)

let texture_descriptor (texture : texture) = texture.texture_descriptor
let texture_destroyed (texture : texture) = texture.resource.dead
