include Metal_raw_types
module Registry = (Metal_gen.Make)(Metal_raw_types)
include Registry
let get = function | Ok value -> value | Error message -> failwith message
let buffer_info raw =
  ((get (Registry.buffer_info_length raw)),
    (Int64.to_int (get (Registry.buffer_info_storageMode raw))),
    (Int64.to_int (get (Registry.buffer_info_cpuCacheMode raw))),
    (Int64.to_int (get (Registry.buffer_info_hazardTrackingMode raw))),
    (get (Registry.buffer_info_heapOffset raw)))
let buffer_set_label = Registry.set_buffer_label
let texture_is_shareable raw = get (Registry.texture_is_shareable raw)
let indirect_render_command = Registry.indirect_render_command
let indirect_render_command_set_pipeline =
  Registry.indirect_render_command_set_pipeline
let function_kind raw = Int64.to_int (get (Registry.function_kind raw))
let compute_pipeline_create = Registry.compute_pipeline_create
let compute_pipeline_max_total_threads raw =
  Int64.to_int (get (Registry.compute_pipeline_max_total_threads raw))
let acceleration_encoder_copy_and_compact =
  Registry.acceleration_encoder_copy_and_compact
let layer_next_drawable = Registry.layer_next_drawable
let compute_encoder_set_pipeline = Registry.compute_encoder_set_pipeline
let compute_encoder_set_buffer = Registry.compute_encoder_set_buffer
let compute_encoder_set_texture = Registry.compute_encoder_set_texture
let compute_encoder_dispatch = Registry.compute_encoder_dispatch
let compute35_acceleration = Registry.compute35_acceleration
let shader_function_argument_encoder raw index =
  if index < 0L
  then Error "argument buffer index must be non-negative"
  else Registry.shader_function_argument_encoder raw index
let ( let* ) = Result.bind
let buffer_create device length options =
  if length <= 0L
  then Error "buffer length must be positive"
  else
    if
      ((options land (lnot 0x3ff)) <> 0) ||
        (((options land 0xf) > 1) ||
           ((((options lsr 4) land 0xf) > 2) ||
              (((options lsr 8) land 3) > 2)))
    then invalid_arg "invalid Metal resource options"
    else Registry.buffer_create device length (Int64.of_int options)
let indirect_render_command_draw_primitives raw primitive start count
  instances base =
  if (start < 0L) || ((count < 0L) || ((instances < 0L) || (base < 0L)))
  then Error "invalid indirect draw range"
  else
    Registry.indirect_render_command_draw_primitives raw
      (Int64.of_int primitive) start count instances base
let acceleration_structure_create = Registry.acceleration_structure_create
let command_buffer_present_drawable raw drawable mode time =
  match mode with
  | 0 -> Registry.command_buffer_present raw drawable
  | 1 -> Registry.command_buffer_present_at_time raw drawable time
  | 2 -> Registry.command_buffer_present_after_duration raw drawable time
  | _ -> Error "invalid presentation mode"
let render_pass_descriptor_create = Registry.render_pass_descriptor_create
let pipeline_render_indirect =
  Registry.render_pipeline_state_support_indirect_command_buffers
let drawable10_snapshot raw =
  let* drawableID = Registry.drawable10_snapshot_drawableID raw
   in
  let* presentedTime = Registry.drawable10_snapshot_presentedTime raw
   in Ok (drawableID, presentedTime)
let indirect_render_command_set_vertex_buffer raw buffer offset index =
  if (offset < 0L) || (index < 0)
  then Error "invalid indirect buffer binding"
  else
    Registry.indirect_render_command_set_vertex_buffer raw buffer offset
      index
let indirect_render_command_set_fragment_buffer raw buffer offset index =
  if (offset < 0L) || (index < 0)
  then Error "invalid indirect buffer binding"
  else
    Registry.indirect_render_command_set_fragment_buffer raw buffer offset
      index
