use metal_sys::Handle;

pub(crate) type TextureDescriptor = TextureDescriptorData;

#[derive(Clone, Debug)]
pub(crate) struct TextureDescriptorData {
    pub texture_type: i32,
    pub pixel_format: i32,
    pub width: usize,
    pub height: usize,
    pub depth: usize,
    pub mip_levels: usize,
    pub sample_count: usize,
    pub array_length: usize,
    pub storage_mode: i32,
    pub cpu_cache_mode: i32,
    pub hazard_tracking_mode: i32,
    pub usage: i32,
    pub allow_gpu_optimized_contents: bool,
    pub compression_type: i32,
    pub swizzle_red: i32,
    pub swizzle_green: i32,
    pub swizzle_blue: i32,
    pub swizzle_alpha: i32,
}

#[derive(Clone, Debug)]
pub(crate) struct FunctionConstantValue {
    pub name: String,
    pub data_type: i32,
    pub bits: i64,
    pub floating: f64,
}

pub(crate) type PipelineBindingInfo = (
    String,
    i32,
    i32,
    i64,
    bool,
    bool,
    i64,
    i64,
    i32,
    i32,
    i32,
    bool,
    i64,
    i64,
    i64,
    i64,
    i64,
    Option<crate::metal_argument_reflection_snapshot::ReflectedType>,
);

#[derive(Clone, Debug)]
pub(crate) struct ComputePipelineDescriptor {
    pub label: Option<String>,
    pub reflection: bool,
    pub preloaded_libraries: Vec<Handle>,
    pub binary_archives: Vec<Handle>,
    pub fail_on_binary_archive_miss: bool,
    pub support_indirect_command_buffers: bool,
    pub buffer_mutabilities: Vec<i32>,
}

#[derive(Clone, Debug)]
pub(crate) struct Metal4StaticLinkingDescriptor {
    pub functions: Vec<(Handle, String)>,
    pub private_functions: Vec<(Handle, String)>,
    pub groups: Vec<(String, Vec<(Handle, String)>)>,
}

#[derive(Clone, Debug)]
pub(crate) struct Metal4StageDynamicLinkingDescriptor {
    pub max_call_stack_depth: i64,
    pub binary_linked_functions: Vec<Handle>,
    pub preloaded_libraries: Vec<Handle>,
}

// ponytail: no Debug, PipelineBindingInfo is an 18-tuple and std implements Debug only up to 12. A named struct is the upgrade if Debug is needed.
#[derive(Clone, Default)]
pub(crate) struct RenderPipelineReflection {
    pub vertex_bindings: Vec<PipelineBindingInfo>,
    pub fragment_bindings: Vec<PipelineBindingInfo>,
    pub tile_bindings: Vec<PipelineBindingInfo>,
    pub object_bindings: Vec<PipelineBindingInfo>,
    pub mesh_bindings: Vec<PipelineBindingInfo>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Metal4RenderColorAttachmentDescriptor {
    pub pixel_format: i32,
    pub blending_state: i32,
    pub source_rgb_blend_factor: i32,
    pub destination_rgb_blend_factor: i32,
    pub rgb_blend_operation: i32,
    pub source_alpha_blend_factor: i32,
    pub destination_alpha_blend_factor: i32,
    pub alpha_blend_operation: i32,
    pub write_mask: i32,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Metal4VertexAttributeDescriptor {
    pub attribute_index: i32,
    pub vertex_format: i32,
    pub offset: i64,
    pub buffer_index: i32,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Metal4VertexLayoutDescriptor {
    pub buffer_index: i32,
    pub stride: Option<i64>,
    pub step_function: i32,
    pub step_rate: i64,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct Metal4VertexDescriptor {
    pub attributes: Vec<Metal4VertexAttributeDescriptor>,
    pub layouts: Vec<Metal4VertexLayoutDescriptor>,
}

#[derive(Clone, Debug)]
pub(crate) struct Metal4RenderDescriptor {
    pub label: Option<String>,
    pub library: Handle,
    pub vertex_function: String,
    pub fragment_function: Option<String>,
    pub reflection: bool,
    pub raster_sample_count: i64,
    pub color_attachments: Vec<Metal4RenderColorAttachmentDescriptor>,
    pub rasterization_enabled: bool,
    pub primitive_topology: i32,
    pub support_indirect_commands: bool,
    pub lookup_archives: Vec<Handle>,
    pub vertex_descriptor: Option<Metal4VertexDescriptor>,
    pub support_vertex_binary_linking: bool,
    pub support_fragment_binary_linking: bool,
    pub vertex_dynamic_linking: Option<Metal4StageDynamicLinkingDescriptor>,
    pub fragment_dynamic_linking: Option<Metal4StageDynamicLinkingDescriptor>,
    pub vertex_static_linking: Option<Metal4StaticLinkingDescriptor>,
    pub fragment_static_linking: Option<Metal4StaticLinkingDescriptor>,
    pub alpha_to_coverage: bool,
    pub alpha_to_one: bool,
    pub max_vertex_amplification_count: i64,
    pub color_attachment_mapping: i32,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct DepthStencilFaceDescriptor {
    pub compare_function: i32,
    pub stencil_failure_operation: i32,
    pub depth_failure_operation: i32,
    pub pass_operation: i32,
    pub read_mask: i32,
    pub write_mask: i32,
}

#[derive(Clone, Debug)]
pub(crate) struct DepthStencilDescriptor {
    pub depth_compare_function: i32,
    pub depth_write_enabled: bool,
    pub front_face_stencil: Option<DepthStencilFaceDescriptor>,
    pub back_face_stencil: Option<DepthStencilFaceDescriptor>,
    pub label: Option<String>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct IndirectCommandBufferDescriptor {
    pub command_types: i64,
    pub inherit_buffers: bool,
    pub inherit_pipeline_state: bool,
    pub max_vertex_buffer_bind_count: i64,
    pub max_fragment_buffer_bind_count: i64,
    pub max_kernel_buffer_bind_count: i64,
    pub support_ray_tracing: bool,
    pub support_dynamic_attribute_stride: bool,
    pub max_kernel_threadgroup_memory_bind_count: i64,
    pub max_object_buffer_bind_count: i64,
    pub max_mesh_buffer_bind_count: i64,
    pub max_object_threadgroup_memory_bind_count: i64,
    pub inherit_depth_stencil_state: bool,
    pub inherit_depth_bias: bool,
    pub inherit_depth_clip_mode: bool,
    pub inherit_cull_mode: bool,
    pub inherit_front_facing_winding: bool,
    pub inherit_triangle_fill_mode: bool,
    pub support_color_attachment_mapping: bool,
}

#[derive(Clone, Debug)]
pub(crate) struct AccelKeyframe { pub buffer: Handle, pub offset: i64 }

#[derive(Clone, Debug)]
pub(crate) enum AccelGeometryRaw {
    Triangles {
        vertex: Handle,
        vertex_offset: i64,
        vertex_stride: i64,
        triangle_count: i64,
        index: Option<Handle>,
        index_offset: i64,
        index_uint16: bool,
        keyframes: Vec<AccelKeyframe>,
        opaque: bool,
        allow_duplicate: bool,
    },
    Boxes {
        boxes: Handle,
        box_offset: i64,
        box_stride: i64,
        box_count: i64,
        keyframes: Vec<AccelKeyframe>,
        opaque: bool,
        allow_duplicate: bool,
    },
    Curves {
        control: Handle,
        control_offset: i64,
        control_stride: i64,
        control_count: i64,
        radius: Handle,
        radius_offset: i64,
        radius_stride: i64,
        curve_index: Handle,
        curve_index_offset: i64,
        curve_index_uint16: bool,
        segment_count: i64,
        segment_control_points: i64,
        curve_type: i32,
        curve_basis: i32,
        end_caps: i32,
        control_keyframes: Vec<AccelKeyframe>,
        radius_keyframes: Vec<AccelKeyframe>,
        opaque: bool,
        allow_duplicate: bool,
    },
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct AccelMotionRaw {
    pub keyframe_count: i64,
    pub start_time: f64,
    pub end_time: f64,
    pub start_border: i32,
    pub end_border: i32,
}

#[derive(Clone, Debug)]
pub(crate) struct AccelPrimitiveRaw {
    pub geometries: Vec<AccelGeometryRaw>,
    pub motion: Option<AccelMotionRaw>,
    pub refit: bool,
    pub fast_build: bool,
}

#[derive(Clone, Debug)]
pub(crate) struct AccelInstancesRaw {
    pub buffer: Handle,
    pub offset: i64,
    pub stride: i64,
    pub count: i64,
    pub kind: i32,
    pub instanced: Vec<Handle>,
    pub motion_transforms: Option<Handle>,
    pub motion_transform_offset: i64,
    pub motion_transform_count: i64,
    pub refit: bool,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct PresentationCommandSnapshot {
    pub drawable_id: i64,
    pub target_id: i64,
    pub flags: i64,
    pub presented_time: f64,
    pub command_start_time: f64,
    pub command_end_time: f64,
    pub scheduled_time: f64,
    pub retained_references: bool,
}
