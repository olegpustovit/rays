// Rust Metal bindings and ABI metadata. Keep in sync with native/generated_metal.mm.
use super::{Error, Handle};
use std::ffi::{CStr, CString};
use std::mem::MaybeUninit;

#[allow(non_snake_case)] pub mod record {
    #[repr(C)]
    #[derive(Clone, Copy, Debug, Default, PartialEq)]
    pub struct Mtl_viewport {
        pub originX: f64,
        pub originY: f64,
        pub width: f64,
        pub height: f64,
        pub znear: f64,
        pub zfar: f64,
    }
    #[repr(C)]
    #[derive(Clone, Copy, Debug, Default, PartialEq)]
    pub struct Mtl_scissor_rect {
        pub x: u64,
        pub y: u64,
        pub width: u64,
        pub height: u64,
    }
    #[repr(C)]
    #[derive(Clone, Copy, Debug, Default, PartialEq)]
    pub struct Mtl_size_int {
        pub width: usize,
        pub height: usize,
        pub depth: usize,
    }
}
#[derive(Clone, Copy, Debug, Eq, PartialEq)] pub enum Feature { Buffer, Texture, Sampler, ComputePipeline, RenderPipeline, Queue, Surface, RayTracing, RayTracingCurves, RayTracingMotion, RayTracingRefit, MemorylessAttachments }
#[derive(Clone, Copy, Debug)] pub struct BindingMetadata { pub name: &'static str, pub receiver: &'static str, pub selector: &'static str, pub feature: Feature, pub since: Option<(u16,u16)>, pub autorelease_pool: bool }
pub const BINDINGS: &[BindingMetadata] = &[
BindingMetadata { name: "device_supports_family", receiver: "Device", selector: "supportsFamily:", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "device_max_threadgroup_memory_length", receiver: "Device", selector: "maxThreadgroupMemoryLength", feature: Feature::ComputePipeline, since: Some((10,13)), autorelease_pool: true },
BindingMetadata { name: "render_pipeline_state_support_indirect_command_buffers", receiver: "Render_pipeline", selector: "supportIndirectCommandBuffers", feature: Feature::RenderPipeline, since: Some((10,14)), autorelease_pool: true },
BindingMetadata { name: "device_name", receiver: "Device", selector: "name", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "device_registry_id", receiver: "Device", selector: "registryID", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "device_is_low_power", receiver: "Device", selector: "lowPower", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "device_is_removable", receiver: "Device", selector: "removable", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "device_is_headless", receiver: "Device", selector: "headless", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "device_has_unified_memory", receiver: "Device", selector: "hasUnifiedMemory", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "device_recommended_max_working_set_size", receiver: "Device", selector: "recommendedMaxWorkingSetSize", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "device_current_allocated_size", receiver: "Device", selector: "currentAllocatedSize", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "device_max_buffer_length", receiver: "Device", selector: "maxBufferLength", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "device_supports_raytracing", receiver: "Device", selector: "supportsRaytracing", feature: Feature::RayTracing, since: None, autorelease_pool: true },
BindingMetadata { name: "device_supports_raytracing_from_render", receiver: "Device", selector: "supportsRaytracingFromRender", feature: Feature::RayTracing, since: None, autorelease_pool: true },
BindingMetadata { name: "device_supports_dynamic_libraries", receiver: "Device", selector: "supportsDynamicLibraries", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "device_supports_texture_sample_count", receiver: "Device", selector: "supportsTextureSampleCount:", feature: Feature::Texture, since: None, autorelease_pool: true },
BindingMetadata { name: "command_queue_create", receiver: "Device", selector: "newCommandQueue", feature: Feature::Queue, since: None, autorelease_pool: true },
BindingMetadata { name: "command_buffer_create", receiver: "Command_queue", selector: "commandBuffer", feature: Feature::Queue, since: None, autorelease_pool: true },
BindingMetadata { name: "set_command_buffer_label", receiver: "Command_buffer", selector: "label=", feature: Feature::Queue, since: None, autorelease_pool: true },
BindingMetadata { name: "command_buffer_status", receiver: "Command_buffer", selector: "status", feature: Feature::Queue, since: None, autorelease_pool: true },
BindingMetadata { name: "command_buffer_commit", receiver: "Command_buffer", selector: "commit", feature: Feature::Queue, since: None, autorelease_pool: true },
BindingMetadata { name: "command_buffer_compute_encoder", receiver: "Command_buffer", selector: "computeCommandEncoder", feature: Feature::ComputePipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "command_buffer_blit_encoder", receiver: "Command_buffer", selector: "blitCommandEncoder", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "command_buffer_acceleration_encoder", receiver: "Command_buffer", selector: "accelerationStructureCommandEncoder", feature: Feature::RayTracing, since: None, autorelease_pool: true },
BindingMetadata { name: "command_buffer_render_encoder_from_pass", receiver: "Command_buffer", selector: "renderCommandEncoderWithDescriptor:", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "compute_encoder_end", receiver: "Compute_encoder", selector: "endEncoding", feature: Feature::ComputePipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "render_encoder_end", receiver: "Render_encoder", selector: "endEncoding", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "blit_encoder_end", receiver: "Blit_encoder", selector: "endEncoding", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "acceleration_encoder_end", receiver: "Acceleration_encoder", selector: "endEncoding", feature: Feature::RayTracing, since: None, autorelease_pool: true },
BindingMetadata { name: "render_encoder_set_pipeline", receiver: "Render_encoder", selector: "setRenderPipelineState:", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "render_encoder_set_vertex_buffer", receiver: "Render_encoder", selector: "setVertexBuffer:offset:atIndex:", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "render_encoder_set_fragment_buffer", receiver: "Render_encoder", selector: "setFragmentBuffer:offset:atIndex:", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "render_encoder_set_vertex_texture", receiver: "Render_encoder", selector: "setVertexTexture:atIndex:", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "render_encoder_set_fragment_texture", receiver: "Render_encoder", selector: "setFragmentTexture:atIndex:", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "render_encoder_set_vertex_sampler", receiver: "Render_encoder", selector: "setVertexSamplerState:atIndex:", feature: Feature::Sampler, since: None, autorelease_pool: true },
BindingMetadata { name: "render_encoder_set_fragment_sampler", receiver: "Render_encoder", selector: "setFragmentSamplerState:atIndex:", feature: Feature::Sampler, since: None, autorelease_pool: true },
BindingMetadata { name: "render_depth_stencil", receiver: "Render_encoder", selector: "setDepthStencilState:", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "render_encoder_set_viewport", receiver: "Render_encoder", selector: "setViewport:", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "render_encoder_set_scissor", receiver: "Render_encoder", selector: "setScissorRect:", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "render_encoder_set_cull_mode", receiver: "Render_encoder", selector: "setCullMode:", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "render_encoder_set_winding", receiver: "Render_encoder", selector: "setFrontFacingWinding:", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "render_encoder_draw_primitives", receiver: "Render_encoder", selector: "drawPrimitives:vertexStart:vertexCount:instanceCount:", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "render_draw_indexed_basic", receiver: "Render_encoder", selector: "drawIndexedPrimitives:indexCount:indexType:indexBuffer:indexBufferOffset:", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "render_draw_indexed_instances", receiver: "Render_encoder", selector: "drawIndexedPrimitives:indexCount:indexType:indexBuffer:indexBufferOffset:instanceCount:", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "render_draw_indexed_indirect", receiver: "Render_encoder", selector: "drawIndexedPrimitives:indexType:indexBuffer:indexBufferOffset:indirectBuffer:indirectBufferOffset:", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "buffer_create", receiver: "Device", selector: "newBufferWithLength:options:", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "buffer_info_length", receiver: "Buffer", selector: "length", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "buffer_info_storageMode", receiver: "Buffer", selector: "storageMode", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "buffer_info_cpuCacheMode", receiver: "Buffer", selector: "cpuCacheMode", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "buffer_info_hazardTrackingMode", receiver: "Buffer", selector: "hazardTrackingMode", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "buffer_info_heapOffset", receiver: "Buffer", selector: "heapOffset", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "set_buffer_label", receiver: "Buffer", selector: "label=", feature: Feature::Buffer, since: None, autorelease_pool: true },
BindingMetadata { name: "texture_is_shareable", receiver: "Texture", selector: "shareable", feature: Feature::Texture, since: None, autorelease_pool: true },
BindingMetadata { name: "indirect_render_command", receiver: "Indirect_command_buffer", selector: "indirectRenderCommandAtIndex:", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "indirect_render_command_set_pipeline", receiver: "Indirect_render_command", selector: "setRenderPipelineState:", feature: Feature::RenderPipeline, since: None, autorelease_pool: false },
BindingMetadata { name: "indirect_render_command_set_vertex_buffer", receiver: "Indirect_render_command", selector: "setVertexBuffer:offset:atIndex:", feature: Feature::RenderPipeline, since: None, autorelease_pool: false },
BindingMetadata { name: "indirect_render_command_set_fragment_buffer", receiver: "Indirect_render_command", selector: "setFragmentBuffer:offset:atIndex:", feature: Feature::RenderPipeline, since: None, autorelease_pool: false },
BindingMetadata { name: "indirect_render_command_draw_primitives", receiver: "Indirect_render_command", selector: "drawPrimitives:vertexStart:vertexCount:instanceCount:baseInstance:", feature: Feature::RenderPipeline, since: None, autorelease_pool: false },
BindingMetadata { name: "function_kind", receiver: "Function", selector: "functionType", feature: Feature::ComputePipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "compute_pipeline_create", receiver: "Device", selector: "newComputePipelineStateWithFunction:error:", feature: Feature::ComputePipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "compute_pipeline_max_total_threads", receiver: "Compute_pipeline", selector: "maxTotalThreadsPerThreadgroup", feature: Feature::ComputePipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "acceleration_structure_create", receiver: "Device", selector: "newAccelerationStructureWithSize:", feature: Feature::RayTracing, since: None, autorelease_pool: true },
BindingMetadata { name: "acceleration_encoder_copy_and_compact", receiver: "Acceleration_encoder", selector: "copyAndCompactAccelerationStructure:toAccelerationStructure:", feature: Feature::RayTracing, since: None, autorelease_pool: true },
BindingMetadata { name: "layer_next_drawable", receiver: "Metal_layer", selector: "nextDrawable", feature: Feature::Surface, since: None, autorelease_pool: true },
BindingMetadata { name: "command_buffer_present", receiver: "Command_buffer", selector: "presentDrawable:", feature: Feature::Surface, since: None, autorelease_pool: true },
BindingMetadata { name: "command_buffer_present_at_time", receiver: "Command_buffer", selector: "presentDrawable:atTime:", feature: Feature::Surface, since: None, autorelease_pool: true },
BindingMetadata { name: "command_buffer_present_after_duration", receiver: "Command_buffer", selector: "presentDrawable:afterMinimumDuration:", feature: Feature::Surface, since: None, autorelease_pool: true },
BindingMetadata { name: "render_pass_descriptor_create", receiver: "Class", selector: "renderPassDescriptor", feature: Feature::RenderPipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "compute_encoder_set_pipeline", receiver: "Compute_encoder", selector: "setComputePipelineState:", feature: Feature::ComputePipeline, since: None, autorelease_pool: false },
BindingMetadata { name: "compute_encoder_set_buffer", receiver: "Compute_encoder", selector: "setBuffer:offset:atIndex:", feature: Feature::ComputePipeline, since: None, autorelease_pool: false },
BindingMetadata { name: "compute_encoder_set_texture", receiver: "Compute_encoder", selector: "setTexture:atIndex:", feature: Feature::ComputePipeline, since: None, autorelease_pool: false },
BindingMetadata { name: "compute_encoder_dispatch", receiver: "Compute_encoder", selector: "dispatchThreads:threadsPerThreadgroup:", feature: Feature::ComputePipeline, since: None, autorelease_pool: false },
BindingMetadata { name: "compute_encoder_dispatch_indirect", receiver: "Compute_encoder", selector: "dispatchThreadgroupsWithIndirectBuffer:indirectBufferOffset:threadsPerThreadgroup:", feature: Feature::ComputePipeline, since: None, autorelease_pool: false },
BindingMetadata { name: "compute35_acceleration", receiver: "Compute_encoder", selector: "setAccelerationStructure:atBufferIndex:", feature: Feature::RayTracing, since: None, autorelease_pool: false },
BindingMetadata { name: "shader_function_argument_encoder", receiver: "Function", selector: "newArgumentEncoderWithBufferIndex:", feature: Feature::ComputePipeline, since: None, autorelease_pool: true },
BindingMetadata { name: "drawable10_snapshot_drawableID", receiver: "Metal_drawable", selector: "drawableID", feature: Feature::Surface, since: None, autorelease_pool: true },
BindingMetadata { name: "drawable10_snapshot_presentedTime", receiver: "Metal_drawable", selector: "presentedTime", feature: Feature::Surface, since: None, autorelease_pool: true },
];
#[derive(Clone, Copy, Debug)] pub struct EnumCase { pub name: &'static str, pub symbol: &'static str, pub value: i64, pub since: (u32,u32,u32) }
#[derive(Clone, Copy, Debug)] pub struct EnumMetadata { pub name: &'static str, pub feature: Feature, pub cases: &'static [EnumCase] }
#[allow(non_upper_case_globals)]
pub mod mtl_data_type { pub type Value = i64;
pub const mtl_data_type_array: Value = 2i64;
pub const mtl_data_type_b_float: Value = 121i64;
pub const mtl_data_type_b_float2: Value = 122i64;
pub const mtl_data_type_b_float3: Value = 123i64;
pub const mtl_data_type_b_float4: Value = 124i64;
pub const mtl_data_type_bool: Value = 53i64;
pub const mtl_data_type_bool2: Value = 54i64;
pub const mtl_data_type_bool3: Value = 55i64;
pub const mtl_data_type_bool4: Value = 56i64;
pub const mtl_data_type_char: Value = 45i64;
pub const mtl_data_type_char2: Value = 46i64;
pub const mtl_data_type_char3: Value = 47i64;
pub const mtl_data_type_char4: Value = 48i64;
pub const mtl_data_type_compute_pipeline: Value = 79i64;
pub const mtl_data_type_depth_stencil_state: Value = 139i64;
pub const mtl_data_type_float: Value = 3i64;
pub const mtl_data_type_float2: Value = 4i64;
pub const mtl_data_type_float2x2: Value = 7i64;
pub const mtl_data_type_float2x3: Value = 8i64;
pub const mtl_data_type_float2x4: Value = 9i64;
pub const mtl_data_type_float3: Value = 5i64;
pub const mtl_data_type_float3x2: Value = 10i64;
pub const mtl_data_type_float3x3: Value = 11i64;
pub const mtl_data_type_float3x4: Value = 12i64;
pub const mtl_data_type_float4: Value = 6i64;
pub const mtl_data_type_float4x2: Value = 13i64;
pub const mtl_data_type_float4x3: Value = 14i64;
pub const mtl_data_type_float4x4: Value = 15i64;
pub const mtl_data_type_half: Value = 16i64;
pub const mtl_data_type_half2: Value = 17i64;
pub const mtl_data_type_half2x2: Value = 20i64;
pub const mtl_data_type_half2x3: Value = 21i64;
pub const mtl_data_type_half2x4: Value = 22i64;
pub const mtl_data_type_half3: Value = 18i64;
pub const mtl_data_type_half3x2: Value = 23i64;
pub const mtl_data_type_half3x3: Value = 24i64;
pub const mtl_data_type_half3x4: Value = 25i64;
pub const mtl_data_type_half4: Value = 19i64;
pub const mtl_data_type_half4x2: Value = 26i64;
pub const mtl_data_type_half4x3: Value = 27i64;
pub const mtl_data_type_half4x4: Value = 28i64;
pub const mtl_data_type_indirect_command_buffer: Value = 80i64;
pub const mtl_data_type_instance_acceleration_structure: Value = 118i64;
pub const mtl_data_type_int: Value = 29i64;
pub const mtl_data_type_int2: Value = 30i64;
pub const mtl_data_type_int3: Value = 31i64;
pub const mtl_data_type_int4: Value = 32i64;
pub const mtl_data_type_intersection_function_table: Value = 116i64;
pub const mtl_data_type_long: Value = 81i64;
pub const mtl_data_type_long2: Value = 82i64;
pub const mtl_data_type_long3: Value = 83i64;
pub const mtl_data_type_long4: Value = 84i64;
pub const mtl_data_type_none: Value = 0i64;
pub const mtl_data_type_pointer: Value = 60i64;
pub const mtl_data_type_primitive_acceleration_structure: Value = 117i64;
pub const mtl_data_type_r16_snorm: Value = 65i64;
pub const mtl_data_type_r16_unorm: Value = 64i64;
pub const mtl_data_type_r8_snorm: Value = 63i64;
pub const mtl_data_type_r8_unorm: Value = 62i64;
pub const mtl_data_type_rg11_b10_float: Value = 76i64;
pub const mtl_data_type_rg16_snorm: Value = 69i64;
pub const mtl_data_type_rg16_unorm: Value = 68i64;
pub const mtl_data_type_rg8_snorm: Value = 67i64;
pub const mtl_data_type_rg8_unorm: Value = 66i64;
pub const mtl_data_type_rgb10_a2_unorm: Value = 75i64;
pub const mtl_data_type_rgb9_e5_float: Value = 77i64;
pub const mtl_data_type_rgba16_snorm: Value = 74i64;
pub const mtl_data_type_rgba16_unorm: Value = 73i64;
pub const mtl_data_type_rgba8_snorm: Value = 72i64;
pub const mtl_data_type_rgba8_unorm: Value = 70i64;
pub const mtl_data_type_rgba8_unorm_s_rgb: Value = 71i64;
pub const mtl_data_type_render_pipeline: Value = 78i64;
pub const mtl_data_type_sampler: Value = 59i64;
pub const mtl_data_type_short: Value = 37i64;
pub const mtl_data_type_short2: Value = 38i64;
pub const mtl_data_type_short3: Value = 39i64;
pub const mtl_data_type_short4: Value = 40i64;
pub const mtl_data_type_struct: Value = 1i64;
pub const mtl_data_type_tensor: Value = 140i64;
pub const mtl_data_type_texture: Value = 58i64;
pub const mtl_data_type_u_char: Value = 49i64;
pub const mtl_data_type_u_char2: Value = 50i64;
pub const mtl_data_type_u_char3: Value = 51i64;
pub const mtl_data_type_u_char4: Value = 52i64;
pub const mtl_data_type_u_int: Value = 33i64;
pub const mtl_data_type_u_int2: Value = 34i64;
pub const mtl_data_type_u_int3: Value = 35i64;
pub const mtl_data_type_u_int4: Value = 36i64;
pub const mtl_data_type_u_long: Value = 85i64;
pub const mtl_data_type_u_long2: Value = 86i64;
pub const mtl_data_type_u_long3: Value = 87i64;
pub const mtl_data_type_u_long4: Value = 88i64;
pub const mtl_data_type_u_short: Value = 41i64;
pub const mtl_data_type_u_short2: Value = 42i64;
pub const mtl_data_type_u_short3: Value = 43i64;
pub const mtl_data_type_u_short4: Value = 44i64;
pub const mtl_data_type_visible_function_table: Value = 115i64;
pub fn to_int64(value: Value) -> i64 { value }
pub fn of_int64(value: i64) -> Option<Value> { match value {
0 => Some(value),
1 => Some(value),
2 => Some(value),
3 => Some(value),
4 => Some(value),
5 => Some(value),
6 => Some(value),
7 => Some(value),
8 => Some(value),
9 => Some(value),
10 => Some(value),
11 => Some(value),
12 => Some(value),
13 => Some(value),
14 => Some(value),
15 => Some(value),
16 => Some(value),
17 => Some(value),
18 => Some(value),
19 => Some(value),
20 => Some(value),
21 => Some(value),
22 => Some(value),
23 => Some(value),
24 => Some(value),
25 => Some(value),
26 => Some(value),
27 => Some(value),
28 => Some(value),
29 => Some(value),
30 => Some(value),
31 => Some(value),
32 => Some(value),
33 => Some(value),
34 => Some(value),
35 => Some(value),
36 => Some(value),
37 => Some(value),
38 => Some(value),
39 => Some(value),
40 => Some(value),
41 => Some(value),
42 => Some(value),
43 => Some(value),
44 => Some(value),
45 => Some(value),
46 => Some(value),
47 => Some(value),
48 => Some(value),
49 => Some(value),
50 => Some(value),
51 => Some(value),
52 => Some(value),
53 => Some(value),
54 => Some(value),
55 => Some(value),
56 => Some(value),
58 => Some(value),
59 => Some(value),
60 => Some(value),
62 => Some(value),
63 => Some(value),
64 => Some(value),
65 => Some(value),
66 => Some(value),
67 => Some(value),
68 => Some(value),
69 => Some(value),
70 => Some(value),
71 => Some(value),
72 => Some(value),
73 => Some(value),
74 => Some(value),
75 => Some(value),
76 => Some(value),
77 => Some(value),
78 => Some(value),
79 => Some(value),
80 => Some(value),
81 => Some(value),
82 => Some(value),
83 => Some(value),
84 => Some(value),
85 => Some(value),
86 => Some(value),
87 => Some(value),
88 => Some(value),
115 => Some(value),
116 => Some(value),
117 => Some(value),
118 => Some(value),
121 => Some(value),
122 => Some(value),
123 => Some(value),
124 => Some(value),
139 => Some(value),
140 => Some(value),
_ => None, } }
}
pub mod mtl_feature_set { pub type Value = i64;
pub const mtl_feature_set_osx_gpu_family1_v1: Value = 10000i64;
pub const mtl_feature_set_osx_gpu_family1_v2: Value = 10001i64;
pub const mtl_feature_set_osx_read_write_texture_tier2: Value = 10002i64;
pub const mtl_feature_set_mac_os_gpu_family1_v1: Value = 10000i64;
pub const mtl_feature_set_mac_os_gpu_family1_v2: Value = 10001i64;
pub const mtl_feature_set_mac_os_gpu_family1_v3: Value = 10003i64;
pub const mtl_feature_set_mac_os_gpu_family1_v4: Value = 10004i64;
pub const mtl_feature_set_mac_os_gpu_family2_v1: Value = 10005i64;
pub const mtl_feature_set_mac_os_read_write_texture_tier2: Value = 10002i64;
pub fn to_int64(value: Value) -> i64 { value }
pub fn of_int64(value: i64) -> Option<Value> { match value {
10000 => Some(value),
10001 => Some(value),
10002 => Some(value),
10003 => Some(value),
10004 => Some(value),
10005 => Some(value),
_ => None, } }
}
pub const ENUMS: &[EnumMetadata] = &[
EnumMetadata { name: "Mtl_data_type", feature: Feature::ComputePipeline, cases: &[
EnumCase { name: "mtl_data_type_array", symbol: "MTLDataTypeArray", value: 2i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_b_float", symbol: "MTLDataTypeBFloat", value: 121i64, since: (14,0,0) },
EnumCase { name: "mtl_data_type_b_float2", symbol: "MTLDataTypeBFloat2", value: 122i64, since: (14,0,0) },
EnumCase { name: "mtl_data_type_b_float3", symbol: "MTLDataTypeBFloat3", value: 123i64, since: (14,0,0) },
EnumCase { name: "mtl_data_type_b_float4", symbol: "MTLDataTypeBFloat4", value: 124i64, since: (14,0,0) },
EnumCase { name: "mtl_data_type_bool", symbol: "MTLDataTypeBool", value: 53i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_bool2", symbol: "MTLDataTypeBool2", value: 54i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_bool3", symbol: "MTLDataTypeBool3", value: 55i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_bool4", symbol: "MTLDataTypeBool4", value: 56i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_char", symbol: "MTLDataTypeChar", value: 45i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_char2", symbol: "MTLDataTypeChar2", value: 46i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_char3", symbol: "MTLDataTypeChar3", value: 47i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_char4", symbol: "MTLDataTypeChar4", value: 48i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_compute_pipeline", symbol: "MTLDataTypeComputePipeline", value: 79i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_depth_stencil_state", symbol: "MTLDataTypeDepthStencilState", value: 139i64, since: (26,0,0) },
EnumCase { name: "mtl_data_type_float", symbol: "MTLDataTypeFloat", value: 3i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_float2", symbol: "MTLDataTypeFloat2", value: 4i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_float2x2", symbol: "MTLDataTypeFloat2x2", value: 7i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_float2x3", symbol: "MTLDataTypeFloat2x3", value: 8i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_float2x4", symbol: "MTLDataTypeFloat2x4", value: 9i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_float3", symbol: "MTLDataTypeFloat3", value: 5i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_float3x2", symbol: "MTLDataTypeFloat3x2", value: 10i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_float3x3", symbol: "MTLDataTypeFloat3x3", value: 11i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_float3x4", symbol: "MTLDataTypeFloat3x4", value: 12i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_float4", symbol: "MTLDataTypeFloat4", value: 6i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_float4x2", symbol: "MTLDataTypeFloat4x2", value: 13i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_float4x3", symbol: "MTLDataTypeFloat4x3", value: 14i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_float4x4", symbol: "MTLDataTypeFloat4x4", value: 15i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_half", symbol: "MTLDataTypeHalf", value: 16i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_half2", symbol: "MTLDataTypeHalf2", value: 17i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_half2x2", symbol: "MTLDataTypeHalf2x2", value: 20i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_half2x3", symbol: "MTLDataTypeHalf2x3", value: 21i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_half2x4", symbol: "MTLDataTypeHalf2x4", value: 22i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_half3", symbol: "MTLDataTypeHalf3", value: 18i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_half3x2", symbol: "MTLDataTypeHalf3x2", value: 23i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_half3x3", symbol: "MTLDataTypeHalf3x3", value: 24i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_half3x4", symbol: "MTLDataTypeHalf3x4", value: 25i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_half4", symbol: "MTLDataTypeHalf4", value: 19i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_half4x2", symbol: "MTLDataTypeHalf4x2", value: 26i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_half4x3", symbol: "MTLDataTypeHalf4x3", value: 27i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_half4x4", symbol: "MTLDataTypeHalf4x4", value: 28i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_indirect_command_buffer", symbol: "MTLDataTypeIndirectCommandBuffer", value: 80i64, since: (10,14,0) },
EnumCase { name: "mtl_data_type_instance_acceleration_structure", symbol: "MTLDataTypeInstanceAccelerationStructure", value: 118i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_int", symbol: "MTLDataTypeInt", value: 29i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_int2", symbol: "MTLDataTypeInt2", value: 30i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_int3", symbol: "MTLDataTypeInt3", value: 31i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_int4", symbol: "MTLDataTypeInt4", value: 32i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_intersection_function_table", symbol: "MTLDataTypeIntersectionFunctionTable", value: 116i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_long", symbol: "MTLDataTypeLong", value: 81i64, since: (12,0,0) },
EnumCase { name: "mtl_data_type_long2", symbol: "MTLDataTypeLong2", value: 82i64, since: (12,0,0) },
EnumCase { name: "mtl_data_type_long3", symbol: "MTLDataTypeLong3", value: 83i64, since: (12,0,0) },
EnumCase { name: "mtl_data_type_long4", symbol: "MTLDataTypeLong4", value: 84i64, since: (12,0,0) },
EnumCase { name: "mtl_data_type_none", symbol: "MTLDataTypeNone", value: 0i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_pointer", symbol: "MTLDataTypePointer", value: 60i64, since: (10,13,0) },
EnumCase { name: "mtl_data_type_primitive_acceleration_structure", symbol: "MTLDataTypePrimitiveAccelerationStructure", value: 117i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_r16_snorm", symbol: "MTLDataTypeR16Snorm", value: 65i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_r16_unorm", symbol: "MTLDataTypeR16Unorm", value: 64i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_r8_snorm", symbol: "MTLDataTypeR8Snorm", value: 63i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_r8_unorm", symbol: "MTLDataTypeR8Unorm", value: 62i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_rg11_b10_float", symbol: "MTLDataTypeRG11B10Float", value: 76i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_rg16_snorm", symbol: "MTLDataTypeRG16Snorm", value: 69i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_rg16_unorm", symbol: "MTLDataTypeRG16Unorm", value: 68i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_rg8_snorm", symbol: "MTLDataTypeRG8Snorm", value: 67i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_rg8_unorm", symbol: "MTLDataTypeRG8Unorm", value: 66i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_rgb10_a2_unorm", symbol: "MTLDataTypeRGB10A2Unorm", value: 75i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_rgb9_e5_float", symbol: "MTLDataTypeRGB9E5Float", value: 77i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_rgba16_snorm", symbol: "MTLDataTypeRGBA16Snorm", value: 74i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_rgba16_unorm", symbol: "MTLDataTypeRGBA16Unorm", value: 73i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_rgba8_snorm", symbol: "MTLDataTypeRGBA8Snorm", value: 72i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_rgba8_unorm", symbol: "MTLDataTypeRGBA8Unorm", value: 70i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_rgba8_unorm_s_rgb", symbol: "MTLDataTypeRGBA8Unorm_sRGB", value: 71i64, since: (11,0,0) },
EnumCase { name: "mtl_data_type_render_pipeline", symbol: "MTLDataTypeRenderPipeline", value: 78i64, since: (10,14,0) },
EnumCase { name: "mtl_data_type_sampler", symbol: "MTLDataTypeSampler", value: 59i64, since: (10,13,0) },
EnumCase { name: "mtl_data_type_short", symbol: "MTLDataTypeShort", value: 37i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_short2", symbol: "MTLDataTypeShort2", value: 38i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_short3", symbol: "MTLDataTypeShort3", value: 39i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_short4", symbol: "MTLDataTypeShort4", value: 40i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_struct", symbol: "MTLDataTypeStruct", value: 1i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_tensor", symbol: "MTLDataTypeTensor", value: 140i64, since: (26,0,0) },
EnumCase { name: "mtl_data_type_texture", symbol: "MTLDataTypeTexture", value: 58i64, since: (10,13,0) },
EnumCase { name: "mtl_data_type_u_char", symbol: "MTLDataTypeUChar", value: 49i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_u_char2", symbol: "MTLDataTypeUChar2", value: 50i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_u_char3", symbol: "MTLDataTypeUChar3", value: 51i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_u_char4", symbol: "MTLDataTypeUChar4", value: 52i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_u_int", symbol: "MTLDataTypeUInt", value: 33i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_u_int2", symbol: "MTLDataTypeUInt2", value: 34i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_u_int3", symbol: "MTLDataTypeUInt3", value: 35i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_u_int4", symbol: "MTLDataTypeUInt4", value: 36i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_u_long", symbol: "MTLDataTypeULong", value: 85i64, since: (12,0,0) },
EnumCase { name: "mtl_data_type_u_long2", symbol: "MTLDataTypeULong2", value: 86i64, since: (12,0,0) },
EnumCase { name: "mtl_data_type_u_long3", symbol: "MTLDataTypeULong3", value: 87i64, since: (12,0,0) },
EnumCase { name: "mtl_data_type_u_long4", symbol: "MTLDataTypeULong4", value: 88i64, since: (12,0,0) },
EnumCase { name: "mtl_data_type_u_short", symbol: "MTLDataTypeUShort", value: 41i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_u_short2", symbol: "MTLDataTypeUShort2", value: 42i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_u_short3", symbol: "MTLDataTypeUShort3", value: 43i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_u_short4", symbol: "MTLDataTypeUShort4", value: 44i64, since: (10,11,0) },
EnumCase { name: "mtl_data_type_visible_function_table", symbol: "MTLDataTypeVisibleFunctionTable", value: 115i64, since: (11,0,0) },
] },
EnumMetadata { name: "Mtl_feature_set", feature: Feature::Buffer, cases: &[
EnumCase { name: "mtl_feature_set_osx_gpu_family1_v1", symbol: "MTLFeatureSet_OSX_GPUFamily1_v1", value: 10000i64, since: (10,11,0) },
EnumCase { name: "mtl_feature_set_osx_gpu_family1_v2", symbol: "MTLFeatureSet_OSX_GPUFamily1_v2", value: 10001i64, since: (10,12,0) },
EnumCase { name: "mtl_feature_set_osx_read_write_texture_tier2", symbol: "MTLFeatureSet_OSX_ReadWriteTextureTier2", value: 10002i64, since: (10,12,0) },
EnumCase { name: "mtl_feature_set_mac_os_gpu_family1_v1", symbol: "MTLFeatureSet_macOS_GPUFamily1_v1", value: 10000i64, since: (10,11,0) },
EnumCase { name: "mtl_feature_set_mac_os_gpu_family1_v2", symbol: "MTLFeatureSet_macOS_GPUFamily1_v2", value: 10001i64, since: (10,12,0) },
EnumCase { name: "mtl_feature_set_mac_os_gpu_family1_v3", symbol: "MTLFeatureSet_macOS_GPUFamily1_v3", value: 10003i64, since: (10,13,0) },
EnumCase { name: "mtl_feature_set_mac_os_gpu_family1_v4", symbol: "MTLFeatureSet_macOS_GPUFamily1_v4", value: 10004i64, since: (10,14,0) },
EnumCase { name: "mtl_feature_set_mac_os_gpu_family2_v1", symbol: "MTLFeatureSet_macOS_GPUFamily2_v1", value: 10005i64, since: (10,14,0) },
EnumCase { name: "mtl_feature_set_mac_os_read_write_texture_tier2", symbol: "MTLFeatureSet_macOS_ReadWriteTextureTier2", value: 10002i64, since: (10,12,0) },
] },
];
#[derive(Clone, Copy, Debug)] pub struct RecordMetadata { pub name: &'static str, pub sdk: &'static str, pub feature: Feature }
pub const RECORDS: &[RecordMetadata] = &[
RecordMetadata { name: "Mtl_viewport", sdk: "MTLViewport", feature: Feature::RenderPipeline },
RecordMetadata { name: "Mtl_scissor_rect", sdk: "MTLScissorRect", feature: Feature::RenderPipeline },
RecordMetadata { name: "Mtl_size_int", sdk: "MTLSize", feature: Feature::ComputePipeline },
];
pub const HANDLE_KINDS: &[(&str,u32)] = &[
("Acceleration4_bbox_descriptor", 1),
("Acceleration4_curve_descriptor", 2),
("Acceleration4_indirect_instance_descriptor", 3),
("Acceleration4_instance_descriptor", 4),
("Acceleration4_motion_bbox_descriptor", 5),
("Acceleration4_motion_curve_descriptor", 6),
("Acceleration4_motion_triangle_descriptor", 7),
("Acceleration4_primitive_descriptor", 8),
("Acceleration4_triangle_descriptor", 9),
("Acceleration_bbox_descriptor", 10),
("Acceleration_curve_descriptor", 11),
("Acceleration_descriptor", 12),
("Acceleration_descriptor4", 13),
("Acceleration_encoder", 14),
("Acceleration_indirect_instance_descriptor", 15),
("Acceleration_instance_descriptor", 16),
("Acceleration_motion_bbox_descriptor", 17),
("Acceleration_motion_curve_descriptor", 18),
("Acceleration_motion_keyframe", 19),
("Acceleration_motion_triangle_descriptor", 20),
("Acceleration_pass_descriptor", 21),
("Acceleration_primitive_descriptor", 22),
("Acceleration_sample_attachment", 23),
("Acceleration_sample_attachment_array", 24),
("Acceleration_structure", 25),
("Acceleration_triangle_owned_descriptor", 26),
("Argument_table4", 27),
("Binary_archive", 28),
("Binary_function", 29),
("Binary_functions_descriptor4", 30),
("Blit_encoder", 31),
("Blit_pass_descriptor", 32),
("Blit_sample_attachment", 33),
("Blit_sample_attachment_array", 34),
("Buffer", 35),
("Buffer_layout_descriptor", 36),
("Buffer_layout_descriptor_array", 37),
("Color_attachment_array4", 38),
("Color_attachment_descriptor", 39),
("Color_attachment_descriptor4", 40),
("Command_allocator4", 41),
("Command_buffer", 42),
("Command_buffer4", 43),
("Command_buffer_options4", 44),
("Command_queue", 45),
("Command_queue4", 46),
("Command_queue_descriptor", 47),
("Compile_options", 48),
("Compiler", 49),
("Compute_encoder", 50),
("Compute_encoder4", 51),
("Compute_pass_descriptor", 52),
("Compute_pipeline", 53),
("Compute_pipeline_descriptor", 54),
("Compute_sample_attachment", 55),
("Compute_sample_attachment_array", 56),
("Counter", 57),
("Counter_descriptor", 58),
("Counter_heap4", 59),
("Counter_sample_buffer", 60),
("Counter_set", 61),
("Depth_stencil", 62),
("Device", 63),
("Dynamic_library", 64),
("External_memory", 65),
("Fence", 66),
("Function", 67),
("Function_constant_values", 68),
("Function_descriptor", 69),
("Function_reflection", 70),
("Fx_spatial_scaler", 71),
("Heap", 72),
("Indirect_command_buffer", 73),
("Indirect_compute_command", 74),
("Indirect_render_command", 75),
("Io_surface", 76),
("Library", 77),
("Linked_functions", 78),
("Log_state", 79),
("Log_state_descriptor", 80),
("Logical_to_physical_color_attachment_map", 81),
("Mesh_pipeline_descriptor", 82),
("Metal_drawable", 83),
("Metal_layer", 84),
("Pipeline_archive", 85),
("Pipeline_buffer_descriptor", 86),
("Pipeline_dataset", 87),
("Pipeline_descriptor4", 88),
("Placement_mapping_queue", 89),
("Render_encoder", 90),
("Render_encoder4", 91),
("Render_pass_descriptor", 92),
("Render_pass_descriptor4", 93),
("Render_pipeline", 94),
("Render_pipeline_descriptor", 95),
("Render_pipeline_functions_descriptor", 96),
("Render_pipeline_reflection", 97),
("Render_sample_attachment_array", 98),
("Render_sample_attachment_descriptor", 99),
("Residency_set", 100),
("Resource_state_encoder", 101),
("Resource_state_pass_descriptor", 102),
("Resource_state_sample_attachment_array", 103),
("Resource_state_sample_attachment_descriptor", 104),
("Resource_view_pool_descriptor", 105),
("Sampler", 106),
("Shader_argument_encoder", 107),
("Shader_attribute", 108),
("Shader_attribute_descriptor", 109),
("Shader_attribute_descriptor_array", 110),
("Shader_stage_descriptor", 111),
("Shader_vertex_attribute", 112),
("Shared_event", 113),
("Shared_texture_handle", 114),
("Submission4", 115),
("Tensor", 116),
("Tensor_descriptor", 117),
("Tensor_extents", 118),
("Texture", 119),
("Texture_descriptor", 120),
("Texture_view_descriptor", 121),
("Texture_view_pool", 122),
("Tile_pipeline_descriptor", 123),
("Vertex_descriptor", 124),
("Xpc_connection", 125),
("Xpc_request", 126),
("Xpc_service", 127),
];
pub const KIND_ACCELERATION4_BBOX_DESCRIPTOR: u32 = 1;
pub const KIND_ACCELERATION4_CURVE_DESCRIPTOR: u32 = 2;
pub const KIND_ACCELERATION4_INDIRECT_INSTANCE_DESCRIPTOR: u32 = 3;
pub const KIND_ACCELERATION4_INSTANCE_DESCRIPTOR: u32 = 4;
pub const KIND_ACCELERATION4_MOTION_BBOX_DESCRIPTOR: u32 = 5;
pub const KIND_ACCELERATION4_MOTION_CURVE_DESCRIPTOR: u32 = 6;
pub const KIND_ACCELERATION4_MOTION_TRIANGLE_DESCRIPTOR: u32 = 7;
pub const KIND_ACCELERATION4_PRIMITIVE_DESCRIPTOR: u32 = 8;
pub const KIND_ACCELERATION4_TRIANGLE_DESCRIPTOR: u32 = 9;
pub const KIND_ACCELERATION_BBOX_DESCRIPTOR: u32 = 10;
pub const KIND_ACCELERATION_CURVE_DESCRIPTOR: u32 = 11;
pub const KIND_ACCELERATION_DESCRIPTOR: u32 = 12;
pub const KIND_ACCELERATION_DESCRIPTOR4: u32 = 13;
pub const KIND_ACCELERATION_ENCODER: u32 = 14;
pub const KIND_ACCELERATION_INDIRECT_INSTANCE_DESCRIPTOR: u32 = 15;
pub const KIND_ACCELERATION_INSTANCE_DESCRIPTOR: u32 = 16;
pub const KIND_ACCELERATION_MOTION_BBOX_DESCRIPTOR: u32 = 17;
pub const KIND_ACCELERATION_MOTION_CURVE_DESCRIPTOR: u32 = 18;
pub const KIND_ACCELERATION_MOTION_KEYFRAME: u32 = 19;
pub const KIND_ACCELERATION_MOTION_TRIANGLE_DESCRIPTOR: u32 = 20;
pub const KIND_ACCELERATION_PASS_DESCRIPTOR: u32 = 21;
pub const KIND_ACCELERATION_PRIMITIVE_DESCRIPTOR: u32 = 22;
pub const KIND_ACCELERATION_SAMPLE_ATTACHMENT: u32 = 23;
pub const KIND_ACCELERATION_SAMPLE_ATTACHMENT_ARRAY: u32 = 24;
pub const KIND_ACCELERATION_STRUCTURE: u32 = 25;
pub const KIND_ACCELERATION_TRIANGLE_OWNED_DESCRIPTOR: u32 = 26;
pub const KIND_ARGUMENT_TABLE4: u32 = 27;
pub const KIND_BINARY_ARCHIVE: u32 = 28;
pub const KIND_BINARY_FUNCTION: u32 = 29;
pub const KIND_BINARY_FUNCTIONS_DESCRIPTOR4: u32 = 30;
pub const KIND_BLIT_ENCODER: u32 = 31;
pub const KIND_BLIT_PASS_DESCRIPTOR: u32 = 32;
pub const KIND_BLIT_SAMPLE_ATTACHMENT: u32 = 33;
pub const KIND_BLIT_SAMPLE_ATTACHMENT_ARRAY: u32 = 34;
pub const KIND_BUFFER: u32 = 35;
pub const KIND_BUFFER_LAYOUT_DESCRIPTOR: u32 = 36;
pub const KIND_BUFFER_LAYOUT_DESCRIPTOR_ARRAY: u32 = 37;
pub const KIND_COLOR_ATTACHMENT_ARRAY4: u32 = 38;
pub const KIND_COLOR_ATTACHMENT_DESCRIPTOR: u32 = 39;
pub const KIND_COLOR_ATTACHMENT_DESCRIPTOR4: u32 = 40;
pub const KIND_COMMAND_ALLOCATOR4: u32 = 41;
pub const KIND_COMMAND_BUFFER: u32 = 42;
pub const KIND_COMMAND_BUFFER4: u32 = 43;
pub const KIND_COMMAND_BUFFER_OPTIONS4: u32 = 44;
pub const KIND_COMMAND_QUEUE: u32 = 45;
pub const KIND_COMMAND_QUEUE4: u32 = 46;
pub const KIND_COMMAND_QUEUE_DESCRIPTOR: u32 = 47;
pub const KIND_COMPILE_OPTIONS: u32 = 48;
pub const KIND_COMPILER: u32 = 49;
pub const KIND_COMPUTE_ENCODER: u32 = 50;
pub const KIND_COMPUTE_ENCODER4: u32 = 51;
pub const KIND_COMPUTE_PASS_DESCRIPTOR: u32 = 52;
pub const KIND_COMPUTE_PIPELINE: u32 = 53;
pub const KIND_COMPUTE_PIPELINE_DESCRIPTOR: u32 = 54;
pub const KIND_COMPUTE_SAMPLE_ATTACHMENT: u32 = 55;
pub const KIND_COMPUTE_SAMPLE_ATTACHMENT_ARRAY: u32 = 56;
pub const KIND_COUNTER: u32 = 57;
pub const KIND_COUNTER_DESCRIPTOR: u32 = 58;
pub const KIND_COUNTER_HEAP4: u32 = 59;
pub const KIND_COUNTER_SAMPLE_BUFFER: u32 = 60;
pub const KIND_COUNTER_SET: u32 = 61;
pub const KIND_DEPTH_STENCIL: u32 = 62;
pub const KIND_DEVICE: u32 = 63;
pub const KIND_DYNAMIC_LIBRARY: u32 = 64;
pub const KIND_EXTERNAL_MEMORY: u32 = 65;
pub const KIND_FENCE: u32 = 66;
pub const KIND_FUNCTION: u32 = 67;
pub const KIND_FUNCTION_CONSTANT_VALUES: u32 = 68;
pub const KIND_FUNCTION_DESCRIPTOR: u32 = 69;
pub const KIND_FUNCTION_REFLECTION: u32 = 70;
pub const KIND_FX_SPATIAL_SCALER: u32 = 71;
pub const KIND_HEAP: u32 = 72;
pub const KIND_INDIRECT_COMMAND_BUFFER: u32 = 73;
pub const KIND_INDIRECT_COMPUTE_COMMAND: u32 = 74;
pub const KIND_INDIRECT_RENDER_COMMAND: u32 = 75;
pub const KIND_IO_SURFACE: u32 = 76;
pub const KIND_LIBRARY: u32 = 77;
pub const KIND_LINKED_FUNCTIONS: u32 = 78;
pub const KIND_LOG_STATE: u32 = 79;
pub const KIND_LOG_STATE_DESCRIPTOR: u32 = 80;
pub const KIND_LOGICAL_TO_PHYSICAL_COLOR_ATTACHMENT_MAP: u32 = 81;
pub const KIND_MESH_PIPELINE_DESCRIPTOR: u32 = 82;
pub const KIND_METAL_DRAWABLE: u32 = 83;
pub const KIND_METAL_LAYER: u32 = 84;
pub const KIND_PIPELINE_ARCHIVE: u32 = 85;
pub const KIND_PIPELINE_BUFFER_DESCRIPTOR: u32 = 86;
pub const KIND_PIPELINE_DATASET: u32 = 87;
pub const KIND_PIPELINE_DESCRIPTOR4: u32 = 88;
pub const KIND_PLACEMENT_MAPPING_QUEUE: u32 = 89;
pub const KIND_RENDER_ENCODER: u32 = 90;
pub const KIND_RENDER_ENCODER4: u32 = 91;
pub const KIND_RENDER_PASS_DESCRIPTOR: u32 = 92;
pub const KIND_RENDER_PASS_DESCRIPTOR4: u32 = 93;
pub const KIND_RENDER_PIPELINE: u32 = 94;
pub const KIND_RENDER_PIPELINE_DESCRIPTOR: u32 = 95;
pub const KIND_RENDER_PIPELINE_FUNCTIONS_DESCRIPTOR: u32 = 96;
pub const KIND_RENDER_PIPELINE_REFLECTION: u32 = 97;
pub const KIND_RENDER_SAMPLE_ATTACHMENT_ARRAY: u32 = 98;
pub const KIND_RENDER_SAMPLE_ATTACHMENT_DESCRIPTOR: u32 = 99;
pub const KIND_RESIDENCY_SET: u32 = 100;
pub const KIND_RESOURCE_STATE_ENCODER: u32 = 101;
pub const KIND_RESOURCE_STATE_PASS_DESCRIPTOR: u32 = 102;
pub const KIND_RESOURCE_STATE_SAMPLE_ATTACHMENT_ARRAY: u32 = 103;
pub const KIND_RESOURCE_STATE_SAMPLE_ATTACHMENT_DESCRIPTOR: u32 = 104;
pub const KIND_RESOURCE_VIEW_POOL_DESCRIPTOR: u32 = 105;
pub const KIND_SAMPLER: u32 = 106;
pub const KIND_SHADER_ARGUMENT_ENCODER: u32 = 107;
pub const KIND_SHADER_ATTRIBUTE: u32 = 108;
pub const KIND_SHADER_ATTRIBUTE_DESCRIPTOR: u32 = 109;
pub const KIND_SHADER_ATTRIBUTE_DESCRIPTOR_ARRAY: u32 = 110;
pub const KIND_SHADER_STAGE_DESCRIPTOR: u32 = 111;
pub const KIND_SHADER_VERTEX_ATTRIBUTE: u32 = 112;
pub const KIND_SHARED_EVENT: u32 = 113;
pub const KIND_SHARED_TEXTURE_HANDLE: u32 = 114;
pub const KIND_SUBMISSION4: u32 = 115;
pub const KIND_TENSOR: u32 = 116;
pub const KIND_TENSOR_DESCRIPTOR: u32 = 117;
pub const KIND_TENSOR_EXTENTS: u32 = 118;
pub const KIND_TEXTURE: u32 = 119;
pub const KIND_TEXTURE_DESCRIPTOR: u32 = 120;
pub const KIND_TEXTURE_VIEW_DESCRIPTOR: u32 = 121;
pub const KIND_TEXTURE_VIEW_POOL: u32 = 122;
pub const KIND_TILE_PIPELINE_DESCRIPTOR: u32 = 123;
pub const KIND_VERTEX_DESCRIPTOR: u32 = 124;
pub const KIND_XPC_CONNECTION: u32 = 125;
pub const KIND_XPC_REQUEST: u32 = 126;
pub const KIND_XPC_SERVICE: u32 = 127;
pub fn device_supports_family(receiver: &Handle, arg0: i64) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_supports_family(receiver_ptr, arg0, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_supports_family"] fn ffi_rays_metal_device_supports_family(receiver: *mut super::NativeHandle, arg0: i64, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_max_threadgroup_memory_length(receiver: &Handle) -> Result<u64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_max_threadgroup_memory_length(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_max_threadgroup_memory_length"] fn ffi_rays_metal_device_max_threadgroup_memory_length(receiver: *mut super::NativeHandle, output: *mut u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_pipeline_state_support_indirect_command_buffers(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_pipeline_state_support_indirect_command_buffers(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_pipeline_state_support_indirect_command_buffers"] fn ffi_rays_metal_render_pipeline_state_support_indirect_command_buffers(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_name(receiver: &Handle) -> Result<String, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*const std::ffi::c_char> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_name(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { CStr::from_ptr(output.assume_init()).to_string_lossy().into_owned() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_name"] fn ffi_rays_metal_device_name(receiver: *mut super::NativeHandle, output: *mut *const std::ffi::c_char, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_registry_id(receiver: &Handle) -> Result<u64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_registry_id(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_registry_id"] fn ffi_rays_metal_device_registry_id(receiver: *mut super::NativeHandle, output: *mut u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_is_low_power(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_is_low_power(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_is_low_power"] fn ffi_rays_metal_device_is_low_power(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_is_removable(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_is_removable(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_is_removable"] fn ffi_rays_metal_device_is_removable(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_is_headless(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_is_headless(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_is_headless"] fn ffi_rays_metal_device_is_headless(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_has_unified_memory(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_has_unified_memory(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_has_unified_memory"] fn ffi_rays_metal_device_has_unified_memory(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_recommended_max_working_set_size(receiver: &Handle) -> Result<u64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_recommended_max_working_set_size(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_recommended_max_working_set_size"] fn ffi_rays_metal_device_recommended_max_working_set_size(receiver: *mut super::NativeHandle, output: *mut u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_current_allocated_size(receiver: &Handle) -> Result<u64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_current_allocated_size(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_current_allocated_size"] fn ffi_rays_metal_device_current_allocated_size(receiver: *mut super::NativeHandle, output: *mut u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_max_buffer_length(receiver: &Handle) -> Result<u64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_max_buffer_length(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_max_buffer_length"] fn ffi_rays_metal_device_max_buffer_length(receiver: *mut super::NativeHandle, output: *mut u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_supports_raytracing(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_supports_raytracing(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_supports_raytracing"] fn ffi_rays_metal_device_supports_raytracing(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_supports_raytracing_from_render(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_supports_raytracing_from_render(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_supports_raytracing_from_render"] fn ffi_rays_metal_device_supports_raytracing_from_render(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_supports_dynamic_libraries(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_supports_dynamic_libraries(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_supports_dynamic_libraries"] fn ffi_rays_metal_device_supports_dynamic_libraries(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_supports_texture_sample_count(receiver: &Handle, arg0: u64) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_supports_texture_sample_count(receiver_ptr, arg0, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_supports_texture_sample_count"] fn ffi_rays_metal_device_supports_texture_sample_count(receiver: *mut super::NativeHandle, arg0: u64, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_queue_create(receiver: &Handle) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_queue_create(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_queue_create"] fn ffi_rays_metal_command_queue_create(receiver: *mut super::NativeHandle, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_create(receiver: &Handle) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_create(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_create"] fn ffi_rays_metal_command_buffer_create(receiver: *mut super::NativeHandle, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn set_command_buffer_label(receiver: &Handle, arg0: &str) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let arg0_cstring = CString::new(arg0).map_err(|_| Error::InvalidArgument("Metal strings must not contain NUL"))?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_set_command_buffer_label(receiver_ptr, arg0_cstring.as_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_set_command_buffer_label"] fn ffi_rays_metal_set_command_buffer_label(receiver: *mut super::NativeHandle, arg0: *const std::ffi::c_char, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_status(receiver: &Handle) -> Result<i64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<i64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_status(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_status"] fn ffi_rays_metal_command_buffer_status(receiver: *mut super::NativeHandle, output: *mut i64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_commit(receiver: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_commit(receiver_ptr, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_commit"] fn ffi_rays_metal_command_buffer_commit(receiver: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_compute_encoder(receiver: &Handle) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_compute_encoder(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_compute_encoder"] fn ffi_rays_metal_command_buffer_compute_encoder(receiver: *mut super::NativeHandle, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_blit_encoder(receiver: &Handle) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_blit_encoder(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_blit_encoder"] fn ffi_rays_metal_command_buffer_blit_encoder(receiver: *mut super::NativeHandle, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_acceleration_encoder(receiver: &Handle) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_acceleration_encoder(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_acceleration_encoder"] fn ffi_rays_metal_command_buffer_acceleration_encoder(receiver: *mut super::NativeHandle, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_render_encoder_from_pass(receiver: &Handle, arg0: &Handle) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_render_encoder_from_pass(receiver_ptr, arg0.as_native_ptr()?, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_render_encoder_from_pass"] fn ffi_rays_metal_command_buffer_render_encoder_from_pass(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn compute_encoder_end(receiver: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_compute_encoder_end(receiver_ptr, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_compute_encoder_end"] fn ffi_rays_metal_compute_encoder_end(receiver: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_end(receiver: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_end(receiver_ptr, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_end"] fn ffi_rays_metal_render_encoder_end(receiver: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn blit_encoder_end(receiver: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_blit_encoder_end(receiver_ptr, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_blit_encoder_end"] fn ffi_rays_metal_blit_encoder_end(receiver: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn acceleration_encoder_end(receiver: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_acceleration_encoder_end(receiver_ptr, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_acceleration_encoder_end"] fn ffi_rays_metal_acceleration_encoder_end(receiver: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_pipeline(receiver: &Handle, arg0: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_pipeline(receiver_ptr, arg0.as_native_ptr()?, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_pipeline"] fn ffi_rays_metal_render_encoder_set_pipeline(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_vertex_buffer(receiver: &Handle, arg0: &Handle, arg1: u64, arg2: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_vertex_buffer(receiver_ptr, arg0.as_native_ptr()?, arg1, arg2, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_vertex_buffer"] fn ffi_rays_metal_render_encoder_set_vertex_buffer(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, arg2: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_fragment_buffer(receiver: &Handle, arg0: &Handle, arg1: u64, arg2: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_fragment_buffer(receiver_ptr, arg0.as_native_ptr()?, arg1, arg2, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_fragment_buffer"] fn ffi_rays_metal_render_encoder_set_fragment_buffer(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, arg2: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_vertex_texture(receiver: &Handle, arg0: &Handle, arg1: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_vertex_texture(receiver_ptr, arg0.as_native_ptr()?, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_vertex_texture"] fn ffi_rays_metal_render_encoder_set_vertex_texture(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_fragment_texture(receiver: &Handle, arg0: &Handle, arg1: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_fragment_texture(receiver_ptr, arg0.as_native_ptr()?, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_fragment_texture"] fn ffi_rays_metal_render_encoder_set_fragment_texture(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_vertex_sampler(receiver: &Handle, arg0: &Handle, arg1: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_vertex_sampler(receiver_ptr, arg0.as_native_ptr()?, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_vertex_sampler"] fn ffi_rays_metal_render_encoder_set_vertex_sampler(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_fragment_sampler(receiver: &Handle, arg0: &Handle, arg1: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_fragment_sampler(receiver_ptr, arg0.as_native_ptr()?, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_fragment_sampler"] fn ffi_rays_metal_render_encoder_set_fragment_sampler(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_depth_stencil(receiver: &Handle, arg0: Option<&Handle>) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_depth_stencil(receiver_ptr, match arg0 { Some(value) => value.as_native_ptr()?, None => std::ptr::null_mut() }, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_depth_stencil"] fn ffi_rays_metal_render_depth_stencil(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_viewport(receiver: &Handle, arg0: record::Mtl_viewport) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_viewport(receiver_ptr, arg0, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_viewport"] fn ffi_rays_metal_render_encoder_set_viewport(receiver: *mut super::NativeHandle, arg0: record::Mtl_viewport, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_scissor(receiver: &Handle, arg0: record::Mtl_scissor_rect) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_scissor(receiver_ptr, arg0, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_scissor"] fn ffi_rays_metal_render_encoder_set_scissor(receiver: *mut super::NativeHandle, arg0: record::Mtl_scissor_rect, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_cull_mode(receiver: &Handle, arg0: i64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_cull_mode(receiver_ptr, arg0, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_cull_mode"] fn ffi_rays_metal_render_encoder_set_cull_mode(receiver: *mut super::NativeHandle, arg0: i64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_winding(receiver: &Handle, arg0: i64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_winding(receiver_ptr, arg0, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_winding"] fn ffi_rays_metal_render_encoder_set_winding(receiver: *mut super::NativeHandle, arg0: i64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_draw_primitives(receiver: &Handle, arg0: i64, arg1: u64, arg2: u64, arg3: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_draw_primitives(receiver_ptr, arg0, arg1, arg2, arg3, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_draw_primitives"] fn ffi_rays_metal_render_encoder_draw_primitives(receiver: *mut super::NativeHandle, arg0: i64, arg1: u64, arg2: u64, arg3: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_draw_indexed_basic(receiver: &Handle, arg0: i64, arg1: u64, arg2: i64, arg3: &Handle, arg4: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_draw_indexed_basic(receiver_ptr, arg0, arg1, arg2, arg3.as_native_ptr()?, arg4, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_draw_indexed_basic"] fn ffi_rays_metal_render_draw_indexed_basic(receiver: *mut super::NativeHandle, arg0: i64, arg1: u64, arg2: i64, arg3: *mut super::NativeHandle, arg4: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_draw_indexed_instances(receiver: &Handle, arg0: i64, arg1: u64, arg2: i64, arg3: &Handle, arg4: u64, arg5: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_draw_indexed_instances(receiver_ptr, arg0, arg1, arg2, arg3.as_native_ptr()?, arg4, arg5, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_draw_indexed_instances"] fn ffi_rays_metal_render_draw_indexed_instances(receiver: *mut super::NativeHandle, arg0: i64, arg1: u64, arg2: i64, arg3: *mut super::NativeHandle, arg4: u64, arg5: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_draw_indexed_indirect(receiver: &Handle, arg0: i64, arg1: i64, arg2: &Handle, arg3: u64, arg4: &Handle, arg5: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_draw_indexed_indirect(receiver_ptr, arg0, arg1, arg2.as_native_ptr()?, arg3, arg4.as_native_ptr()?, arg5, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_draw_indexed_indirect"] fn ffi_rays_metal_render_draw_indexed_indirect(receiver: *mut super::NativeHandle, arg0: i64, arg1: i64, arg2: *mut super::NativeHandle, arg3: u64, arg4: *mut super::NativeHandle, arg5: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn buffer_create(receiver: &Handle, arg0: u64, arg1: i64) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_buffer_create(receiver_ptr, arg0, arg1, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_buffer_create"] fn ffi_rays_metal_buffer_create(receiver: *mut super::NativeHandle, arg0: u64, arg1: i64, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn buffer_info_length(receiver: &Handle) -> Result<u64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_buffer_info_length(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_buffer_info_length"] fn ffi_rays_metal_buffer_info_length(receiver: *mut super::NativeHandle, output: *mut u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn buffer_info_storageMode(receiver: &Handle) -> Result<i64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<i64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_buffer_info_storageMode(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_buffer_info_storageMode"] fn ffi_rays_metal_buffer_info_storageMode(receiver: *mut super::NativeHandle, output: *mut i64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn buffer_info_cpuCacheMode(receiver: &Handle) -> Result<i64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<i64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_buffer_info_cpuCacheMode(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_buffer_info_cpuCacheMode"] fn ffi_rays_metal_buffer_info_cpuCacheMode(receiver: *mut super::NativeHandle, output: *mut i64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn buffer_info_hazardTrackingMode(receiver: &Handle) -> Result<i64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<i64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_buffer_info_hazardTrackingMode(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_buffer_info_hazardTrackingMode"] fn ffi_rays_metal_buffer_info_hazardTrackingMode(receiver: *mut super::NativeHandle, output: *mut i64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn buffer_info_heapOffset(receiver: &Handle) -> Result<u64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_buffer_info_heapOffset(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_buffer_info_heapOffset"] fn ffi_rays_metal_buffer_info_heapOffset(receiver: *mut super::NativeHandle, output: *mut u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn set_buffer_label(receiver: &Handle, arg0: &str) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let arg0_cstring = CString::new(arg0).map_err(|_| Error::InvalidArgument("Metal strings must not contain NUL"))?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_set_buffer_label(receiver_ptr, arg0_cstring.as_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_set_buffer_label"] fn ffi_rays_metal_set_buffer_label(receiver: *mut super::NativeHandle, arg0: *const std::ffi::c_char, message: *mut *const std::ffi::c_char) -> i32; }
pub fn texture_is_shareable(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_texture_is_shareable(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_texture_is_shareable"] fn ffi_rays_metal_texture_is_shareable(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn indirect_render_command(receiver: &Handle, arg0: u64) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_indirect_render_command(receiver_ptr, arg0, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_indirect_render_command"] fn ffi_rays_metal_indirect_render_command(receiver: *mut super::NativeHandle, arg0: u64, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn indirect_render_command_set_pipeline(receiver: &Handle, arg0: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_indirect_render_command_set_pipeline(receiver_ptr, arg0.as_native_ptr()?, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_indirect_render_command_set_pipeline"] fn ffi_rays_metal_indirect_render_command_set_pipeline(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn indirect_render_command_set_vertex_buffer(receiver: &Handle, arg0: &Handle, arg1: u64, arg2: usize) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_indirect_render_command_set_vertex_buffer(receiver_ptr, arg0.as_native_ptr()?, arg1, arg2, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_indirect_render_command_set_vertex_buffer"] fn ffi_rays_metal_indirect_render_command_set_vertex_buffer(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, arg2: usize, message: *mut *const std::ffi::c_char) -> i32; }
pub fn indirect_render_command_set_fragment_buffer(receiver: &Handle, arg0: &Handle, arg1: u64, arg2: usize) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_indirect_render_command_set_fragment_buffer(receiver_ptr, arg0.as_native_ptr()?, arg1, arg2, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_indirect_render_command_set_fragment_buffer"] fn ffi_rays_metal_indirect_render_command_set_fragment_buffer(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, arg2: usize, message: *mut *const std::ffi::c_char) -> i32; }
pub fn indirect_render_command_draw_primitives(receiver: &Handle, arg0: i64, arg1: u64, arg2: u64, arg3: u64, arg4: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_indirect_render_command_draw_primitives(receiver_ptr, arg0, arg1, arg2, arg3, arg4, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_indirect_render_command_draw_primitives"] fn ffi_rays_metal_indirect_render_command_draw_primitives(receiver: *mut super::NativeHandle, arg0: i64, arg1: u64, arg2: u64, arg3: u64, arg4: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn function_kind(receiver: &Handle) -> Result<i64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<i64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_function_kind(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_function_kind"] fn ffi_rays_metal_function_kind(receiver: *mut super::NativeHandle, output: *mut i64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn compute_pipeline_create(receiver: &Handle, arg0: &Handle) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_compute_pipeline_create(receiver_ptr, arg0.as_native_ptr()?, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_compute_pipeline_create"] fn ffi_rays_metal_compute_pipeline_create(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn compute_pipeline_max_total_threads(receiver: &Handle) -> Result<u64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_compute_pipeline_max_total_threads(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_compute_pipeline_max_total_threads"] fn ffi_rays_metal_compute_pipeline_max_total_threads(receiver: *mut super::NativeHandle, output: *mut u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn acceleration_structure_create(receiver: &Handle, arg0: u64) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_acceleration_structure_create(receiver_ptr, arg0, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_acceleration_structure_create"] fn ffi_rays_metal_acceleration_structure_create(receiver: *mut super::NativeHandle, arg0: u64, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn acceleration_encoder_copy_and_compact(receiver: &Handle, arg0: &Handle, arg1: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_acceleration_encoder_copy_and_compact(receiver_ptr, arg0.as_native_ptr()?, arg1.as_native_ptr()?, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_acceleration_encoder_copy_and_compact"] fn ffi_rays_metal_acceleration_encoder_copy_and_compact(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn layer_next_drawable(receiver: &Handle) -> Result<Option<Handle>, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_layer_next_drawable(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { let pointer = output.assume_init(); if pointer.is_null() { None } else { Some(Handle::from_native(pointer)?) } })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_layer_next_drawable"] fn ffi_rays_metal_layer_next_drawable(receiver: *mut super::NativeHandle, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_present(receiver: &Handle, arg0: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_present(receiver_ptr, arg0.as_native_ptr()?, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_present"] fn ffi_rays_metal_command_buffer_present(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_present_at_time(receiver: &Handle, arg0: &Handle, arg1: f64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_present_at_time(receiver_ptr, arg0.as_native_ptr()?, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_present_at_time"] fn ffi_rays_metal_command_buffer_present_at_time(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: f64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_present_after_duration(receiver: &Handle, arg0: &Handle, arg1: f64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_present_after_duration(receiver_ptr, arg0.as_native_ptr()?, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_present_after_duration"] fn ffi_rays_metal_command_buffer_present_after_duration(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: f64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_pass_descriptor_create() -> Result<Handle, Error> {
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_pass_descriptor_create(output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_pass_descriptor_create"] fn ffi_rays_metal_render_pass_descriptor_create(output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn compute_encoder_set_pipeline(receiver: &Handle, arg0: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_compute_encoder_set_pipeline(receiver_ptr, arg0.as_native_ptr()?, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_compute_encoder_set_pipeline"] fn ffi_rays_metal_compute_encoder_set_pipeline(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn compute_encoder_set_buffer(receiver: &Handle, arg0: &Handle, arg1: u64, arg2: usize) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_compute_encoder_set_buffer(receiver_ptr, arg0.as_native_ptr()?, arg1, arg2, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_compute_encoder_set_buffer"] fn ffi_rays_metal_compute_encoder_set_buffer(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, arg2: usize, message: *mut *const std::ffi::c_char) -> i32; }
pub fn compute_encoder_set_texture(receiver: &Handle, arg0: &Handle, arg1: usize) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_compute_encoder_set_texture(receiver_ptr, arg0.as_native_ptr()?, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_compute_encoder_set_texture"] fn ffi_rays_metal_compute_encoder_set_texture(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: usize, message: *mut *const std::ffi::c_char) -> i32; }
pub fn compute_encoder_dispatch(receiver: &Handle, arg0: record::Mtl_size_int, arg1: record::Mtl_size_int) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_compute_encoder_dispatch(receiver_ptr, arg0, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_compute_encoder_dispatch"] fn ffi_rays_metal_compute_encoder_dispatch(receiver: *mut super::NativeHandle, arg0: record::Mtl_size_int, arg1: record::Mtl_size_int, message: *mut *const std::ffi::c_char) -> i32; }
pub fn compute_encoder_dispatch_indirect(receiver: &Handle, arg0: &Handle, arg1: u64, arg2: record::Mtl_size_int) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_compute_encoder_dispatch_indirect(receiver_ptr, arg0.as_native_ptr()?, arg1, arg2, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_compute_encoder_dispatch_indirect"] fn ffi_rays_metal_compute_encoder_dispatch_indirect(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, arg2: record::Mtl_size_int, message: *mut *const std::ffi::c_char) -> i32; }
pub fn compute35_acceleration(receiver: &Handle, arg0: Option<&Handle>, arg1: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_compute35_acceleration(receiver_ptr, match arg0 { Some(value) => value.as_native_ptr()?, None => std::ptr::null_mut() }, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_compute35_acceleration"] fn ffi_rays_metal_compute35_acceleration(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn shader_function_argument_encoder(receiver: &Handle, arg0: u64) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_shader_function_argument_encoder(receiver_ptr, arg0, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_shader_function_argument_encoder"] fn ffi_rays_metal_shader_function_argument_encoder(receiver: *mut super::NativeHandle, arg0: u64, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn drawable10_snapshot_drawableID(receiver: &Handle) -> Result<u64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_drawable10_snapshot_drawableID(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_drawable10_snapshot_drawableID"] fn ffi_rays_metal_drawable10_snapshot_drawableID(receiver: *mut super::NativeHandle, output: *mut u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn drawable10_snapshot_presentedTime(receiver: &Handle) -> Result<f64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<f64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_drawable10_snapshot_presentedTime(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_drawable10_snapshot_presentedTime"] fn ffi_rays_metal_drawable10_snapshot_presentedTime(receiver: *mut super::NativeHandle, output: *mut f64, message: *mut *const std::ffi::c_char) -> i32; }
