type handle

type texture_descriptor = {
  texture_type : int;
  pixel_format : int;
  width : int;
  height : int;
  depth : int;
  mip_levels : int;
  sample_count : int;
  array_length : int;
  storage_mode : int;
  cpu_cache_mode : int;
  hazard_tracking_mode : int;
  usage : int;
  allow_gpu_optimized_contents : bool;
  compression_type : int;
  swizzle_red : int;
  swizzle_green : int;
  swizzle_blue : int;
  swizzle_alpha : int;
}
[@@ocaml.doc
  " Positional native ABI record. Keep field order synchronized with\n\
  \    [texture_descriptor] in [metal_bridge.mm]. "]

type function_constant_value = string * int * int64 * float
[@@ocaml.doc " Positional native ABI value for one function-constant specialization. "]

type pipeline_binding_info =
  string
  * int
  * int
  * int64
  * bool
  * bool
  * int64
  * int64
  * int
  * int
  * int
  * bool
  * int64
  * int64
  * int64
  * int64
  * int64
  * Metal_argument_reflection_snapshot.reflected_type option
[@@ocaml.doc
  " Positional native ABI value for one reflected pipeline binding. Keep this\n\
  \    synchronized with [copy_bindings] in [metal_bridge.mm]. "]

type compute_pipeline_descriptor = {
  label : string option;
  reflection : bool;
  preloaded_libraries : handle array;
  binary_archives : handle array;
  fail_on_binary_archive_miss : bool;
  support_indirect_command_buffers : bool;
  buffer_mutabilities : int array;
}

type metal4_function_reference = handle * string
[@@ocaml.doc " Library handle and function name used by Metal 4 static linking. "]

type metal4_static_linking_descriptor = {
  functions : metal4_function_reference array;
  private_functions : metal4_function_reference array;
  groups : (string * metal4_function_reference array) array;
}

type metal4_stage_dynamic_linking_descriptor = {
  max_call_stack_depth : int64;
  binary_linked_functions : handle array;
  preloaded_libraries : handle array;
}

type render_pipeline_reflection = {
  vertex_bindings : pipeline_binding_info array;
  fragment_bindings : pipeline_binding_info array;
  tile_bindings : pipeline_binding_info array;
  object_bindings : pipeline_binding_info array;
  mesh_bindings : pipeline_binding_info array;
}

type metal4_render_color_attachment_descriptor = {
  pixel_format : int;
  blending_state : int;
  source_rgb_blend_factor : int;
  destination_rgb_blend_factor : int;
  rgb_blend_operation : int;
  source_alpha_blend_factor : int;
  destination_alpha_blend_factor : int;
  alpha_blend_operation : int;
  write_mask : int;
}

type metal4_vertex_attribute_descriptor = {
  attribute_index : int;
  vertex_format : int;
  offset : int64;
  buffer_index : int;
}

type metal4_vertex_layout_descriptor = {
  buffer_index : int;
  stride : int64 option;
  step_function : int;
  step_rate : int64;
}

type metal4_vertex_descriptor = {
  attributes : metal4_vertex_attribute_descriptor array;
  layouts : metal4_vertex_layout_descriptor array;
}

type metal4_render_descriptor = {
  label : string option;
  library : handle;
  vertex_function : string;
  fragment_function : string option;
  reflection : bool;
  raster_sample_count : int64;
  color_attachments : metal4_render_color_attachment_descriptor array;
  rasterization_enabled : bool;
  primitive_topology : int;
  support_indirect_commands : bool;
  lookup_archives : handle array;
  vertex_descriptor : metal4_vertex_descriptor option;
  support_vertex_binary_linking : bool;
  support_fragment_binary_linking : bool;
  vertex_dynamic_linking : metal4_stage_dynamic_linking_descriptor option;
  fragment_dynamic_linking : metal4_stage_dynamic_linking_descriptor option;
  vertex_static_linking : metal4_static_linking_descriptor option;
  fragment_static_linking : metal4_static_linking_descriptor option;
  alpha_to_coverage : bool;
  alpha_to_one : bool;
  max_vertex_amplification_count : int64;
  color_attachment_mapping : int;
}

type depth_stencil_face_descriptor = {
  compare_function : int;
  stencil_failure_operation : int;
  depth_failure_operation : int;
  pass_operation : int;
  read_mask : int32;
  write_mask : int32;
}
[@@ocaml.doc " Positional native ABI record for one immutable stencil face. "]

type depth_stencil_descriptor = {
  depth_compare_function : int;
  depth_write_enabled : bool;
  front_face_stencil : depth_stencil_face_descriptor option;
  back_face_stencil : depth_stencil_face_descriptor option;
  label : string option;
}
[@@ocaml.doc " Positional native ABI record for immutable depth/stencil state. "]

type indirect_command_buffer_descriptor = {
  command_types : int64;
  inherit_buffers : bool;
  inherit_pipeline_state : bool;
  max_vertex_buffer_bind_count : int64;
  max_fragment_buffer_bind_count : int64;
  max_kernel_buffer_bind_count : int64;
  support_ray_tracing : bool;
  support_dynamic_attribute_stride : bool;
  max_kernel_threadgroup_memory_bind_count : int64;
  max_object_buffer_bind_count : int64;
  max_mesh_buffer_bind_count : int64;
  max_object_threadgroup_memory_bind_count : int64;
  inherit_depth_stencil_state : bool;
  inherit_depth_bias : bool;
  inherit_depth_clip_mode : bool;
  inherit_cull_mode : bool;
  inherit_front_facing_winding : bool;
  inherit_triangle_fill_mode : bool;
  support_color_attachment_mapping : bool;
}

type accel_keyframe = { keyframe_buffer : handle; keyframe_offset : int64 }

type accel_geometry_raw =
  | Raw_triangles of {
      vertex : handle;
      vertex_offset : int64;
      vertex_stride : int64;
      triangle_count : int64;
      index : handle option;
      index_offset : int64;
      index_uint16 : bool;
      keyframes : accel_keyframe array;
      opaque : bool;
      allow_duplicate : bool;
    }
  | Raw_boxes of {
      boxes : handle;
      box_offset : int64;
      box_stride : int64;
      box_count : int64;
      box_keyframes : accel_keyframe array;
      box_opaque : bool;
      box_allow_duplicate : bool;
    }
  | Raw_curves of {
      control : handle;
      control_offset : int64;
      control_stride : int64;
      control_count : int64;
      radius : handle;
      radius_offset : int64;
      radius_stride : int64;
      curve_index : handle;
      curve_index_offset : int64;
      curve_index_uint16 : bool;
      segment_count : int64;
      segment_control_points : int64;
      curve_type : int;
      curve_basis : int;
      end_caps : int;
      control_keyframes : accel_keyframe array;
      radius_keyframes : accel_keyframe array;
      curve_opaque : bool;
      curve_allow_duplicate : bool;
    }

type accel_motion_raw = {
  keyframe_count : int64;
  start_time : float;
  end_time : float;
  start_border : int;
  end_border : int;
}

type accel_primitive_raw = {
  geometries : accel_geometry_raw array;
  motion : accel_motion_raw option;
  primitive_refit : bool;
  fast_build : bool;
}

type accel_instances_raw = {
  instances_buffer : handle;
  instances_offset : int64;
  instances_stride : int64;
  instances_count : int64;
  instance_kind : int;
  instanced : handle array;
  motion_transforms : handle option;
  motion_transform_offset : int64;
  motion_transform_count : int64;
  instances_refit : bool;
}

type presentation_command_snapshot = int64 * int64 * int64 * float * float * float * float * bool
