#include "metal_handle.h"
#include "metal_kinds.h"
#include <Metal/Metal.h>
#include <cstdint>

struct RaysIndirectCommandBufferDescriptor {
    int64_t command_types;
    uint8_t inherit_buffers;
    uint8_t inherit_pipeline_state;
    int64_t max_vertex_buffer_bind_count;
    int64_t max_fragment_buffer_bind_count;
    int64_t max_kernel_buffer_bind_count;
    uint8_t support_ray_tracing;
    uint8_t support_dynamic_attribute_stride;
    int64_t max_kernel_threadgroup_memory_bind_count;
    int64_t max_object_buffer_bind_count;
    int64_t max_mesh_buffer_bind_count;
    int64_t max_object_threadgroup_memory_bind_count;
    uint8_t inherit_depth_stencil_state;
    uint8_t inherit_depth_bias;
    uint8_t inherit_depth_clip_mode;
    uint8_t inherit_cull_mode;
    uint8_t inherit_front_facing_winding;
    uint8_t inherit_triangle_fill_mode;
    uint8_t support_color_attachment_mapping;
};

extern "C" int32_t rays_metal_indirect_command_buffer_create(
    MetalHandle *device_handle, const RaysIndirectCommandBufferDescriptor *source,
    uint64_t raw_count, int64_t raw_options, MetalHandle **out, const char **message) {
    if (@available(macOS 10.14, *)) { } else return rays_metal_set_unavailable(message, "indirect command buffers require macOS 10.14 or newer");
    if (source == nullptr || out == nullptr)
        return rays_metal_set_error(message, "invalid indirect command buffer size or options");
    @try {
        @autoreleasepool {
            id<MTLDevice> device = (__bridge id<MTLDevice>)rays_metal_handle_object(device_handle, KIND_DEVICE);
            MTLIndirectCommandBufferDescriptor *descriptor = [[MTLIndirectCommandBufferDescriptor alloc] init];
            descriptor.commandTypes = static_cast<MTLIndirectCommandType>(source->command_types);
            descriptor.inheritBuffers = source->inherit_buffers != 0;
            descriptor.inheritPipelineState = source->inherit_pipeline_state != 0;
            descriptor.maxVertexBufferBindCount = static_cast<NSUInteger>(source->max_vertex_buffer_bind_count);
            descriptor.maxFragmentBufferBindCount = static_cast<NSUInteger>(source->max_fragment_buffer_bind_count);
            if (@available(macOS 11.0, *)) descriptor.maxKernelBufferBindCount = static_cast<NSUInteger>(source->max_kernel_buffer_bind_count);
            else if (source->max_kernel_buffer_bind_count != 0) return rays_metal_set_unavailable(message, "indirect compute commands require macOS 11 or newer");
            if (@available(macOS 13.0, *)) descriptor.supportRayTracing = source->support_ray_tracing != 0;
            else if (source->support_ray_tracing) return rays_metal_set_unavailable(message, "indirect ray tracing requires macOS 13 or newer");
            if (@available(macOS 14.0, *)) {
                descriptor.supportDynamicAttributeStride = source->support_dynamic_attribute_stride != 0;
                descriptor.maxKernelThreadgroupMemoryBindCount = static_cast<NSUInteger>(source->max_kernel_threadgroup_memory_bind_count);
                descriptor.maxObjectBufferBindCount = static_cast<NSUInteger>(source->max_object_buffer_bind_count);
                descriptor.maxMeshBufferBindCount = static_cast<NSUInteger>(source->max_mesh_buffer_bind_count);
                descriptor.maxObjectThreadgroupMemoryBindCount = static_cast<NSUInteger>(source->max_object_threadgroup_memory_bind_count);
            } else if (source->support_dynamic_attribute_stride || source->max_kernel_threadgroup_memory_bind_count != 0 || source->max_object_buffer_bind_count != 0 || source->max_mesh_buffer_bind_count != 0 || source->max_object_threadgroup_memory_bind_count != 0) {
                return rays_metal_set_unavailable(message, "advanced indirect commands require macOS 14 or newer");
            }
            if (@available(macOS 26.0, *)) {
                descriptor.inheritDepthStencilState = source->inherit_depth_stencil_state != 0;
                descriptor.inheritDepthBias = source->inherit_depth_bias != 0;
                descriptor.inheritDepthClipMode = source->inherit_depth_clip_mode != 0;
                descriptor.inheritCullMode = source->inherit_cull_mode != 0;
                descriptor.inheritFrontFacingWinding = source->inherit_front_facing_winding != 0;
                descriptor.inheritTriangleFillMode = source->inherit_triangle_fill_mode != 0;
                descriptor.supportColorAttachmentMapping = source->support_color_attachment_mapping != 0;
            } else if (source->inherit_depth_stencil_state || source->inherit_depth_bias || source->inherit_depth_clip_mode || source->inherit_cull_mode || source->inherit_front_facing_winding || source->inherit_triangle_fill_mode || source->support_color_attachment_mapping) {
                return rays_metal_set_unavailable(message, "Metal 4 indirect inheritance requires macOS 26 or newer");
            }
            id<MTLIndirectCommandBuffer> buffer = [device newIndirectCommandBufferWithDescriptor:descriptor maxCommandCount:static_cast<NSUInteger>(raw_count) options:static_cast<MTLResourceOptions>(raw_options)];
            if (buffer == nil || buffer.device.registryID != device.registryID || buffer.size == 0)
                return rays_metal_set_error(message, "Metal rejected the indirect command buffer descriptor");
            *out = rays_metal_icb_handle_new((__bridge void *)buffer, static_cast<uint32_t>(source->command_types));
            return *out ? 0 : rays_metal_set_error(message, "unable to allocate Metal handle");
        }
    } @catch (NSException *exception) {
        return rays_metal_set_error(message, exception.reason.UTF8String);
    }
}

extern "C" int32_t rays_metal_indirect_render_draw_indexed(
    MetalHandle *command_handle, MetalHandle *icb_handle, uint64_t device_id, int32_t primitive, uint64_t index_count, int32_t index_type,
    MetalHandle *index_buffer_handle, uint64_t index_offset, uint64_t instance_count,
    int64_t base_vertex, uint64_t base_instance, const char **message) {
    if (message) *message = nullptr;
    @try {
        @autoreleasepool {
            id<MTLIndirectCommandBuffer> icb = (__bridge id<MTLIndirectCommandBuffer>)rays_metal_handle_object(icb_handle, KIND_INDIRECT_COMMAND_BUFFER);
            id<MTLBuffer> index_buffer = (__bridge id<MTLBuffer>)rays_metal_handle_object(index_buffer_handle, KIND_BUFFER);
            if (icb == nil || index_buffer == nil)
                return rays_metal_set_error(message, "invalid indirect command buffer or index buffer handle");
            if ((rays_metal_icb_command_types(icb_handle) & MTLIndirectCommandTypeDrawIndexed) == 0)
                return rays_metal_set_invalid_argument(message, "native indirect command buffer does not enable indexed draws");
            const uint64_t element_size = index_type == 0 ? 2 : 4;
            if (index_offset > index_buffer.length ||
                index_count > (index_buffer.length - index_offset) / element_size)
                return rays_metal_set_error(message, "indexed indirect draw range is invalid");
            if (index_buffer.device.registryID != device_id)
                return rays_metal_set_error(message, "index buffer belongs to another device");
            if (@available(macOS 10.14, *)) {
                id<MTLIndirectRenderCommand> command = (__bridge id<MTLIndirectRenderCommand>)rays_metal_handle_object(command_handle, KIND_INDIRECT_RENDER_COMMAND);
                if (command == nil) return rays_metal_set_error(message, "invalid indirect render command handle");
                [command drawIndexedPrimitives:static_cast<MTLPrimitiveType>(primitive)
                                    indexCount:static_cast<NSUInteger>(index_count)
                                     indexType:static_cast<MTLIndexType>(index_type)
                                   indexBuffer:index_buffer
                             indexBufferOffset:static_cast<NSUInteger>(index_offset)
                                 instanceCount:static_cast<NSUInteger>(instance_count)
                                    baseVertex:static_cast<NSInteger>(base_vertex)
                                  baseInstance:static_cast<NSUInteger>(base_instance)];
                return 0;
            } else {
                return rays_metal_set_unavailable(message, "indirect render commands require macOS 10.14 or newer");
            }
        }
    } @catch (NSException *exception) {
        return rays_metal_set_error(message, exception.reason.UTF8String);
    }
}

// ogpu_metal: executeCommandsInBuffer:withRange: on a render encoder.
extern "C" int32_t rays_metal_render_encoder_execute_icb_range(MetalHandle *encoder_handle, MetalHandle *icb_handle, uint64_t location, uint64_t length, const char **message) {
  if (message) *message = nullptr;
  @try { @autoreleasepool {
    id<MTLRenderCommandEncoder> encoder = (__bridge id<MTLRenderCommandEncoder>)rays_metal_handle_object(encoder_handle, KIND_RENDER_ENCODER);
    id<MTLIndirectCommandBuffer> icb = (__bridge id<MTLIndirectCommandBuffer>)rays_metal_handle_object(icb_handle, KIND_INDIRECT_COMMAND_BUFFER);
    if (!encoder || !icb) return rays_metal_set_error(message, "invalid render encoder or indirect command buffer handle");
    [encoder executeCommandsInBuffer:icb withRange:NSMakeRange(static_cast<NSUInteger>(location), static_cast<NSUInteger>(length))];
    return 0;
  }} @catch (NSException *exception) { return rays_metal_set_error(message, exception.reason.UTF8String); }
}
