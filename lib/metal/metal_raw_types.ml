type handle
type device_region = (int64 * int64 * int64 * int64 * int64 * int64)
type texture_descriptor =
  {
  texture_type: int ;
  pixel_format: int ;
  width: int ;
  height: int ;
  depth: int ;
  mip_levels: int ;
  sample_count: int ;
  array_length: int ;
  storage_mode: int ;
  cpu_cache_mode: int ;
  hazard_tracking_mode: int ;
  usage: int ;
  allow_gpu_optimized_contents: bool ;
  compression_type: int ;
  swizzle_red: int ;
  swizzle_green: int ;
  swizzle_blue: int ;
  swizzle_alpha: int }[@@ocaml.doc
                        " Positional native ABI record. Keep field order synchronized with\n    [texture_descriptor] in [metal_bridge.mm]. "]
type texture_view_descriptor =
  {
  pixel_format: int ;
  texture_type: int ;
  base_mip: int ;
  mip_count: int ;
  base_slice: int ;
  slice_count: int ;
  requested_swizzle_red: int ;
  requested_swizzle_green: int ;
  requested_swizzle_blue: int ;
  requested_swizzle_alpha: int ;
  effective_swizzle_red: int ;
  effective_swizzle_green: int ;
  effective_swizzle_blue: int ;
  effective_swizzle_alpha: int }[@@ocaml.doc
                                  " Positional native ABI record for texture-view construction. "]
type function_constant_value = (string * int * int64 * float)[@@ocaml.doc
                                                               " Positional native ABI value for one function-constant specialization. "]
type function_constant_info = (string * int * int64 * bool)[@@ocaml.doc
                                                             " Name, Metal data-type code, index, and required flag. "]
type acceleration_triangle_descriptor =
  {
  vertex_buffer: handle ;
  vertex_offset: int64 ;
  vertex_stride: int64 ;
  triangle_count: int64 ;
  index_buffer: handle option ;
  index_offset: int64 }
type acceleration_instance_descriptor =
  {
  instance_buffer: handle ;
  instance_count: int64 ;
  primitives: handle array ;
  instance_offset: int64 }
type pipeline_binding_info =
  (string * int * int * int64 * bool * bool * int64 * int64 * int * int * int
    * bool * int64 * int64 * int64 * int64 * int64 *
    Metal_argument_reflection_snapshot.reflected_type option)[@@ocaml.doc
                                                               " Positional native ABI value for one reflected pipeline binding. Keep this\n    synchronized with [copy_bindings] in [metal_bridge.mm]. "]
type library_compile_descriptor =
  {
  label: string option ;
  library_type: int ;
  install_name: string option ;
  linked_libraries: handle array }
type compute_pipeline_descriptor =
  {
  label: string option ;
  reflection: bool ;
  linked_functions: handle array ;
  preloaded_libraries: handle array ;
  binary_archives: handle array ;
  fail_on_binary_archive_miss: bool ;
  support_indirect_command_buffers: bool ;
  buffer_mutabilities: int array }
type metal4_function_reference = (handle * string)[@@ocaml.doc
                                                    " Library handle and function name used by Metal 4 static linking. "]
type metal4_static_linking_descriptor =
  {
  functions: metal4_function_reference array ;
  private_functions: metal4_function_reference array ;
  groups: (string * metal4_function_reference array) array }
type metal4_stage_dynamic_linking_descriptor =
  {
  max_call_stack_depth: int64 ;
  binary_linked_functions: handle array ;
  preloaded_libraries: handle array }
type metal4_compute_descriptor =
  {
  label: string option ;
  library: handle ;
  function_name: string ;
  reflection: bool ;
  threadgroup_size_multiple: bool ;
  max_total_threads: int64 ;
  required_threads_width: int64 ;
  required_threads_height: int64 ;
  required_threads_depth: int64 ;
  support_binary_linking: bool ;
  support_indirect_commands: bool ;
  preloaded_libraries: handle array ;
  max_call_stack_depth: int64 ;
  lookup_archives: handle array ;
  binary_linked_functions: handle array ;
  static_linking: metal4_static_linking_descriptor option }[@@ocaml.doc
                                                             " Positional native ABI record for synchronous Metal 4 compute compilation. "]
type render_pipeline_reflection =
  {
  vertex_bindings: pipeline_binding_info array ;
  fragment_bindings: pipeline_binding_info array ;
  tile_bindings: pipeline_binding_info array ;
  object_bindings: pipeline_binding_info array ;
  mesh_bindings: pipeline_binding_info array }
type metal4_render_color_attachment_descriptor =
  {
  pixel_format: int ;
  blending_state: int ;
  source_rgb_blend_factor: int ;
  destination_rgb_blend_factor: int ;
  rgb_blend_operation: int ;
  source_alpha_blend_factor: int ;
  destination_alpha_blend_factor: int ;
  alpha_blend_operation: int ;
  write_mask: int }
type metal4_vertex_attribute_descriptor =
  {
  attribute_index: int ;
  vertex_format: int ;
  offset: int64 ;
  buffer_index: int }
type metal4_vertex_layout_descriptor =
  {
  buffer_index: int ;
  stride: int64 option ;
  step_function: int ;
  step_rate: int64 }
type metal4_vertex_descriptor =
  {
  attributes: metal4_vertex_attribute_descriptor array ;
  layouts: metal4_vertex_layout_descriptor array }
type metal4_render_descriptor =
  {
  label: string option ;
  library: handle ;
  vertex_function: string ;
  fragment_function: string option ;
  reflection: bool ;
  raster_sample_count: int64 ;
  color_attachments: metal4_render_color_attachment_descriptor array ;
  rasterization_enabled: bool ;
  primitive_topology: int ;
  support_indirect_commands: bool ;
  lookup_archives: handle array ;
  vertex_descriptor: metal4_vertex_descriptor option ;
  support_vertex_binary_linking: bool ;
  support_fragment_binary_linking: bool ;
  vertex_dynamic_linking: metal4_stage_dynamic_linking_descriptor option ;
  fragment_dynamic_linking: metal4_stage_dynamic_linking_descriptor option ;
  vertex_static_linking: metal4_static_linking_descriptor option ;
  fragment_static_linking: metal4_static_linking_descriptor option ;
  alpha_to_coverage: bool ;
  alpha_to_one: bool ;
  max_vertex_amplification_count: int64 ;
  color_attachment_mapping: int }
type metal4_mesh_descriptor =
  {
  label: string option ;
  library: handle ;
  object_function: string option ;
  mesh_function: string ;
  fragment_function: string option ;
  reflection: bool ;
  max_total_object_threads: int64 ;
  max_total_mesh_threads: int64 ;
  required_object_width: int64 ;
  required_object_height: int64 ;
  required_object_depth: int64 ;
  required_mesh_width: int64 ;
  required_mesh_height: int64 ;
  required_mesh_depth: int64 ;
  object_threadgroup_size_multiple: bool ;
  mesh_threadgroup_size_multiple: bool ;
  payload_memory_length: int64 ;
  max_total_threadgroups_per_mesh_grid: int64 ;
  raster_sample_count: int64 ;
  color_attachments: metal4_render_color_attachment_descriptor array ;
  rasterization_enabled: bool ;
  support_indirect_commands: bool ;
  lookup_archives: handle array ;
  support_object_binary_linking: bool ;
  support_mesh_binary_linking: bool ;
  support_fragment_binary_linking: bool ;
  object_dynamic_linking: metal4_stage_dynamic_linking_descriptor option ;
  mesh_dynamic_linking: metal4_stage_dynamic_linking_descriptor option ;
  fragment_dynamic_linking: metal4_stage_dynamic_linking_descriptor option ;
  object_static_linking: metal4_static_linking_descriptor option ;
  mesh_static_linking: metal4_static_linking_descriptor option ;
  fragment_static_linking: metal4_static_linking_descriptor option ;
  alpha_to_coverage: bool ;
  alpha_to_one: bool ;
  max_vertex_amplification_count: int64 ;
  color_attachment_mapping: int }
type metal4_tile_descriptor =
  {
  label: string option ;
  library: handle ;
  tile_function: string ;
  reflection: bool ;
  raster_sample_count: int64 ;
  color_formats: int array ;
  threadgroup_size_matches_tile_size: bool ;
  max_total_threads: int64 ;
  required_threads_width: int64 ;
  required_threads_height: int64 ;
  required_threads_depth: int64 ;
  support_binary_linking: bool ;
  static_linking: metal4_static_linking_descriptor option ;
  lookup_archives: handle array ;
  dynamic_linking: metal4_stage_dynamic_linking_descriptor option }
type metal4_render_attachment =
  {
  texture: handle ;
  resolve_texture: handle option ;
  load_action: int ;
  store_action: int ;
  clear_red: float ;
  clear_green: float ;
  clear_blue: float ;
  clear_alpha: float }[@@ocaml.doc
                        " Positional native ABI record for one base-level Metal 4 render-pass color\n    attachment and its optional single-sample resolve target. "]
type metal4_render_depth_attachment =
  {
  texture: handle ;
  load_action: int ;
  store_action: int ;
  clear_depth: float }[@@ocaml.doc
                        " Positional native ABI record for one base-level Metal 4 render-pass depth\n    attachment. "]
type metal4_render_stencil_attachment =
  {
  texture: handle ;
  load_action: int ;
  store_action: int ;
  clear_stencil: int32 }[@@ocaml.doc
                          " Positional native ABI record for one base-level Metal 4 render-pass stencil\n    attachment. "]
type metal4_render_pass_descriptor =
  {
  color_attachments: metal4_render_attachment array ;
  depth_attachment: metal4_render_depth_attachment option ;
  stencil_attachment: metal4_render_stencil_attachment option ;
  width: int ;
  height: int ;
  sample_count: int ;
  label: string option ;
  support_color_attachment_mapping: bool ;
  visibility_result_buffer: handle option ;
  visibility_result_type: int }[@@ocaml.doc
                                 " Positional native ABI record for a Metal 4 render pass. "]
type metal4_vertex_amplification_view_mapping =
  {
  viewport_array_index_offset: int64 ;
  render_target_array_index_offset: int64 }[@@ocaml.doc
                                             " Positional native ABI record for one amplified vertex view. "]
type metal4_scissor_rect = {
  x: int64 ;
  y: int64 ;
  width: int64 ;
  height: int64 }[@@ocaml.doc
                   " Positional native ABI record for one Metal scissor rectangle. "]
type depth_stencil_face_descriptor =
  {
  compare_function: int ;
  stencil_failure_operation: int ;
  depth_failure_operation: int ;
  pass_operation: int ;
  read_mask: int32 ;
  write_mask: int32 }[@@ocaml.doc
                       " Positional native ABI record for one immutable stencil face. "]
type depth_stencil_descriptor =
  {
  depth_compare_function: int ;
  depth_write_enabled: bool ;
  front_face_stencil: depth_stencil_face_descriptor option ;
  back_face_stencil: depth_stencil_face_descriptor option ;
  label: string option }[@@ocaml.doc
                          " Positional native ABI record for immutable depth/stencil state. "]
type indirect_command_buffer_descriptor =
  {
  command_types: int64 ;
  inherit_buffers: bool ;
  inherit_pipeline_state: bool ;
  max_vertex_buffer_bind_count: int64 ;
  max_fragment_buffer_bind_count: int64 ;
  max_kernel_buffer_bind_count: int64 ;
  support_ray_tracing: bool ;
  support_dynamic_attribute_stride: bool ;
  max_kernel_threadgroup_memory_bind_count: int64 ;
  max_object_buffer_bind_count: int64 ;
  max_mesh_buffer_bind_count: int64 ;
  max_object_threadgroup_memory_bind_count: int64 ;
  inherit_depth_stencil_state: bool ;
  inherit_depth_bias: bool ;
  inherit_depth_clip_mode: bool ;
  inherit_cull_mode: bool ;
  inherit_front_facing_winding: bool ;
  inherit_triangle_fill_mode: bool ;
  support_color_attachment_mapping: bool }
type metal4_argument_table_descriptor =
  {
  max_buffers: int ;
  max_textures: int ;
  max_samplers: int ;
  initialize_bindings: bool ;
  support_attribute_strides: bool ;
  label: string option }[@@ocaml.doc
                          " Positional native ABI record for a Metal 4 argument-table descriptor. "]
type metal4_binary_function_descriptor =
  {
  library: handle ;
  source_function: handle ;
  binary_name: string ;
  pipeline_independent: bool ;
  lookup_archives: handle array }[@@ocaml.doc
                                   " Positional native ABI record for a Metal 4 binary-function lookup or\n    compilation. "]
type accel_keyframe = {
  keyframe_buffer: handle ;
  keyframe_offset: int64 }
type accel_geometry_raw =
  | Raw_triangles of
  {
  vertex: handle ;
  vertex_offset: int64 ;
  vertex_stride: int64 ;
  triangle_count: int64 ;
  index: handle option ;
  index_offset: int64 ;
  index_uint16: bool ;
  keyframes: accel_keyframe array ;
  opaque: bool ;
  allow_duplicate: bool ;
  table_offset: int64 }
  | Raw_boxes of
  {
  boxes: handle ;
  box_offset: int64 ;
  box_stride: int64 ;
  box_count: int64 ;
  box_keyframes: accel_keyframe array ;
  box_opaque: bool ;
  box_allow_duplicate: bool ;
  box_table_offset: int64 }
  | Raw_curves of
  {
  control: handle ;
  control_offset: int64 ;
  control_stride: int64 ;
  control_count: int64 ;
  radius: handle ;
  radius_offset: int64 ;
  radius_stride: int64 ;
  curve_index: handle ;
  curve_index_offset: int64 ;
  curve_index_uint16: bool ;
  segment_count: int64 ;
  segment_control_points: int64 ;
  curve_type: int ;
  curve_basis: int ;
  end_caps: int ;
  control_keyframes: accel_keyframe array ;
  radius_keyframes: accel_keyframe array ;
  curve_opaque: bool ;
  curve_allow_duplicate: bool ;
  curve_table_offset: int64 }
type accel_motion_raw =
  {
  keyframe_count: int64 ;
  start_time: float ;
  end_time: float ;
  start_border: int ;
  end_border: int }
type accel_primitive_raw =
  {
  geometries: accel_geometry_raw array ;
  motion: accel_motion_raw option ;
  primitive_refit: bool ;
  fast_build: bool }
type accel_instances_raw =
  {
  instances_buffer: handle ;
  instances_offset: int64 ;
  instances_stride: int64 ;
  instances_count: int64 ;
  instance_kind: int ;
  instanced: handle array ;
  motion_transforms: handle option ;
  motion_transform_offset: int64 ;
  motion_transform_count: int64 ;
  instances_refit: bool }
type presentation_layer_snapshot =
  (int64 * float * float * int64 * bool * int64 * bool * bool * bool * bool)
type presentation_command_snapshot =
  (int64 * int64 * int64 * float * float * float * float * bool)
type presentation_render_pass_advanced =
  (int64 * int64 * int64 * int64 * int64 * bool * (float * float) array)
type prepared_render_pass_state =
  (int * handle option * (int32 * int32) option * (float * float * float *
    float * float * float) * (int * int * int * int))
type render_command_scissor = (int64 * int64 * int64 * int64)
type render_command_viewport =
  (float * float * float * float * float * float)
type render_command_view_mapping = (int64 * int64)
type resource_buffer_snapshot = (handle * int64 * int * int * int)[@@ocaml.doc
                                                                    " Resource100 generated raw integration. Internal handles only. "]
type resource_texture_snapshot =
  (handle * int64 * int64 * int64 * int64 * int64 * int64 * int * int * int *
    int * int * int64)
type resource_root_snapshot =
  | Resource_buffer_root of resource_buffer_snapshot
  | Resource_texture_root of resource_texture_snapshot
type metal4_size = (int64 * int64 * int64)
type metal4_origin = (int64 * int64 * int64)
type metal4_range = (int64 * int64)
type metal4_buffer_texture_copy =
  (handle * int64 * int64 * int64 * metal4_size * handle * int64 * int64 *
    metal4_origin)
type metal4_texture_buffer_copy =
  (handle * int64 * int64 * metal4_origin * metal4_size * handle * int64 *
    int64 * int64)
type metal4_owned_buffer_range = (handle * int64 * int64)
type mesh_tile_threadgroup_size = (int64 * int64 * int64)
type mesh_color_attachment_mechanical =
  {
  pixel_format: int64 ;
  source_rgb_blend_factor: int ;
  destination_rgb_blend_factor: int ;
  rgb_blend_operation: int ;
  source_alpha_blend_factor: int ;
  destination_alpha_blend_factor: int ;
  alpha_blend_operation: int ;
  write_mask: int64 }
type mesh_descriptor_mechanical =
  {
  label: string option ;
  depth_attachment_pixel_format: int64 ;
  stencil_attachment_pixel_format: int64 ;
  required_threads_per_mesh_threadgroup: mesh_tile_threadgroup_size ;
  required_threads_per_object_threadgroup: mesh_tile_threadgroup_size }
type tile_descriptor_mechanical =
  {
  label: string option ;
  required_threads_per_threadgroup: mesh_tile_threadgroup_size }
type blit_copy_spec =
  | Blit_buffer_to_texture of int64 * int64 * int64 * int64 * int64 * int64 *
  int64 * int64 * int64 * int64 * int64 * int64
  | Blit_buffer_to_buffer of int64 * int64 * int64
  | Blit_tensor_to_tensor of handle * handle * handle * handle
  | Blit_texture_to_buffer of int64 * int64 * int64 * int64 * int64 * int64 *
  int64 * int64 * int64 * int64 * int64 * int64
  | Blit_texture_region of int64 * int64 * int64 * int64 * int64 * int64 *
  int64 * int64 * int64 * int64 * int64 * int64 * int64
  | Blit_texture_levels of int64 * int64 * int64 * int64 * int64 * int64
  | Blit_texture_whole
  | Blit_indirect_commands of int64 * int64 * int64
