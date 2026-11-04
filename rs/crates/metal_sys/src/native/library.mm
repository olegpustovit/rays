#include "metal_handle.h"
#include "metal_kinds.h"
#include <Metal/Metal.h>
#include <cstring>
#include <new>

extern "C" void caml_rays_metal_free_bytes(char *bytes);

static uint64_t argument_resource_device(id object, int32_t tag) {
    switch (tag) {
        case 0: return ((__bridge id<MTLBuffer>)object).device.registryID;
        case 1: return ((__bridge id<MTLTexture>)object).device.registryID;
        case 2: return ((__bridge id<MTLSamplerState>)object).device.registryID;
        case 3: return ((__bridge id<MTLAccelerationStructure>)object).device.registryID;
        case 4: return ((__bridge id<MTLIndirectCommandBuffer>)object).device.registryID;
        case 7: return ((__bridge id<MTLRenderPipelineState>)object).device.registryID;
        case 8: return ((__bridge id<MTLComputePipelineState>)object).device.registryID;
        case 9: return ((__bridge id<MTLDepthStencilState>)object).device.registryID;
        default: return 0;
    }
}

extern "C" int32_t caml_rays_metal_argument_encoder_snapshot(
    MetalHandle *handle, char **label, uint64_t *encoded_length, uint64_t *alignment,
    uint64_t *device_registry_id, const char **message) {
    if (message) *message = nullptr;
    if (label) *label = nullptr;
    @try {
        @autoreleasepool {
            id<MTLArgumentEncoder> encoder = (__bridge id<MTLArgumentEncoder>)rays_metal_handle_object(handle, KIND_SHADER_ARGUMENT_ENCODER);
            NSString *value = encoder.label;
            if (value != nil && label != nullptr) {
                const char *utf8 = value.UTF8String;
                if (utf8 == nullptr) return rays_metal_set_error(message, "argument encoder label is not valid UTF-8");
                size_t size = std::strlen(utf8) + 1;
                char *copy = new (std::nothrow) char[size];
                if (copy == nullptr) return rays_metal_set_error(message, "unable to allocate argument encoder label");
                std::memcpy(copy, utf8, size);
                *label = copy;
            }
            *encoded_length = static_cast<uint64_t>(encoder.encodedLength);
            *alignment = static_cast<uint64_t>(encoder.alignment);
            *device_registry_id = encoder.device.registryID;
            return 0;
        }
    } @catch (NSException *exception) {
        return rays_metal_set_error(message, exception.reason.UTF8String);
    }
}

extern "C" int32_t caml_rays_metal_argument_encoder_set_buffer(
    MetalHandle *encoder_handle, MetalHandle *buffer_handle, int64_t offset, int64_t start_offset,
    int64_t array_element, const char **message) {
    if (message) *message = nullptr;
    if (offset < 0 || start_offset < 0 || array_element < 0)
        return rays_metal_set_error(message, "negative argument buffer offset or element");
    @try {
        @autoreleasepool {
            id<MTLArgumentEncoder> encoder = (__bridge id<MTLArgumentEncoder>)rays_metal_handle_object(encoder_handle, KIND_SHADER_ARGUMENT_ENCODER);
            id<MTLBuffer> buffer = (__bridge id<MTLBuffer>)rays_metal_handle_object(buffer_handle, KIND_BUFFER);
            if (encoder == nil || buffer == nil)
                return rays_metal_set_error(message, "invalid argument encoder or buffer handle");
            if (static_cast<uint64_t>(offset) > buffer.length || argument_resource_device(buffer, 0) != encoder.device.registryID)
                return rays_metal_set_error(message, "argument buffer range or device mismatch");
            if (array_element == 0) [encoder setArgumentBuffer:buffer offset:static_cast<NSUInteger>(offset)];
            else [encoder setArgumentBuffer:buffer startOffset:static_cast<NSUInteger>(start_offset) arrayElement:static_cast<NSUInteger>(array_element)];
            return 0;
        }
    } @catch (NSException *exception) {
        return rays_metal_set_error(message, exception.reason.UTF8String);
    }
}

extern "C" int32_t caml_rays_metal_argument_encoder_single(
    MetalHandle *encoder_handle, int32_t tag, MetalHandle *resource_handle, int64_t offset,
    int64_t index, const char **message) {
    if (message) *message = nullptr;
    if (offset < 0 || index < 0) return rays_metal_set_error(message, "negative argument offset or index");
    @try {
        @autoreleasepool {
            id<MTLArgumentEncoder> encoder = (__bridge id<MTLArgumentEncoder>)rays_metal_handle_object(encoder_handle, KIND_SHADER_ARGUMENT_ENCODER);
            id resource = nil;
            switch (tag) {
                case 0: resource = (__bridge id<MTLBuffer>)rays_metal_handle_object(resource_handle, KIND_BUFFER); break;
                case 1: resource = (__bridge id<MTLTexture>)rays_metal_handle_object(resource_handle, KIND_TEXTURE); break;
                case 2: resource = (__bridge id<MTLSamplerState>)rays_metal_handle_object(resource_handle, KIND_SAMPLER); break;
                case 3: resource = (__bridge id<MTLAccelerationStructure>)rays_metal_handle_object(resource_handle, KIND_ACCELERATION_STRUCTURE); break;
                case 4: resource = (__bridge id<MTLIndirectCommandBuffer>)rays_metal_handle_object(resource_handle, KIND_INDIRECT_COMMAND_BUFFER); break;
                case 7: resource = (__bridge id<MTLRenderPipelineState>)rays_metal_handle_object(resource_handle, KIND_RENDER_PIPELINE); break;
                case 8: resource = (__bridge id<MTLComputePipelineState>)rays_metal_handle_object(resource_handle, KIND_COMPUTE_PIPELINE); break;
                case 9: resource = (__bridge id<MTLDepthStencilState>)rays_metal_handle_object(resource_handle, KIND_DEPTH_STENCIL); break;
                default: return rays_metal_set_error(message, "unknown argument binding kind");
            }
            if (encoder.device.registryID != argument_resource_device(resource, tag)) return rays_metal_set_error(message, "argument binding device mismatch");
            switch (tag) {
                case 0: [encoder setBuffer:resource offset:static_cast<NSUInteger>(offset) atIndex:static_cast<NSUInteger>(index)]; break;
                case 1: [encoder setTexture:resource atIndex:static_cast<NSUInteger>(index)]; break;
                case 2: [encoder setSamplerState:resource atIndex:static_cast<NSUInteger>(index)]; break;
                case 3:
                    if (@available(macOS 11.0, *)) [encoder setAccelerationStructure:resource atIndex:static_cast<NSUInteger>(index)];
                    else return rays_metal_set_unavailable(message, "argument encoder acceleration structures require macOS 11");
                    break;
                case 4: [encoder setIndirectCommandBuffer:resource atIndex:static_cast<NSUInteger>(index)]; break;
                case 7: [encoder setRenderPipelineState:resource atIndex:static_cast<NSUInteger>(index)]; break;
                case 8: [encoder setComputePipelineState:resource atIndex:static_cast<NSUInteger>(index)]; break;
                case 9: [encoder setDepthStencilState:resource atIndex:static_cast<NSUInteger>(index)]; break;
            }
            return 0;
        }
    } @catch (NSException *exception) {
        return rays_metal_set_error(message, exception.reason.UTF8String);
    }
}
