#include "metal_handle.h"
#include "metal_kinds.h"
#include <Metal/Metal.h>
#include <cstdint>
#include <cmath>

using RaysSamplerDescriptor = struct {
  int32_t min_filter, mag_filter, mip_filter, max_anisotropy;
  int32_t address_s, address_t, address_r, border_color, reduction_mode;
  uint8_t normalized_coordinates;
  float lod_min, lod_max;
  uint8_t lod_average;
  float lod_bias;
  int32_t compare_function;
  uint8_t support_argument_buffers;
};
struct RaysStencilFaceDescriptor {
  int32_t compare_function, stencil_failure_operation, depth_failure_operation, pass_operation;
  uint32_t read_mask, write_mask;
};
struct RaysDepthStencilDescriptor {
  int32_t depth_compare_function;
  uint8_t depth_write_enabled;
  RaysStencilFaceDescriptor front_face_stencil;
  uint8_t has_front_face_stencil;
  RaysStencilFaceDescriptor back_face_stencil;
  uint8_t has_back_face_stencil;
};

extern "C" int32_t rays_metal_sampler_create(MetalHandle *device_handle,
    const RaysSamplerDescriptor *s, const char *raw_label, MetalHandle **output,
    const char **message) {
  if (message) *message = nullptr;
  if (output) *output = nullptr;
  if (s == nullptr || output == nullptr)
    return rays_metal_set_invalid_argument(message, "invalid sampler descriptor");
  @try { @autoreleasepool {
    id<MTLDevice> device = (__bridge id<MTLDevice>)rays_metal_handle_object(device_handle, KIND_DEVICE);
    if (device == nil) return rays_metal_set_error(message, "invalid Metal device handle");
    if (s->reduction_mode != 0 || s->lod_bias != 0) {
      uint8_t supported = 0;
      const int32_t status = rays_metal_device_supports_sampler_reduction(device_handle, &supported, message);
      if (status != 0) return status;
      if (!supported) return rays_metal_set_unsupported(message, "sampler reduction modes and LOD bias require macOS 26 and Apple GPU family 10");
    }
    MTLSamplerDescriptor *descriptor = [[MTLSamplerDescriptor alloc] init];
    descriptor.minFilter = static_cast<MTLSamplerMinMagFilter>(s->min_filter);
    descriptor.magFilter = static_cast<MTLSamplerMinMagFilter>(s->mag_filter);
    descriptor.mipFilter = static_cast<MTLSamplerMipFilter>(s->mip_filter);
    descriptor.maxAnisotropy = static_cast<NSUInteger>(s->max_anisotropy);
    descriptor.sAddressMode = static_cast<MTLSamplerAddressMode>(s->address_s);
    descriptor.tAddressMode = static_cast<MTLSamplerAddressMode>(s->address_t);
    descriptor.rAddressMode = static_cast<MTLSamplerAddressMode>(s->address_r);
    descriptor.borderColor = static_cast<MTLSamplerBorderColor>(s->border_color);
    descriptor.normalizedCoordinates = s->normalized_coordinates != 0;
    descriptor.lodMinClamp = s->lod_min; descriptor.lodMaxClamp = s->lod_max;
    descriptor.lodAverage = s->lod_average != 0;
    descriptor.compareFunction = static_cast<MTLCompareFunction>(s->compare_function);
    if (@available(macOS 26.0, *)) { descriptor.reductionMode = static_cast<MTLSamplerReductionMode>(s->reduction_mode); descriptor.lodBias = s->lod_bias; }
    if (@available(macOS 13.0, *)) descriptor.supportArgumentBuffers = s->support_argument_buffers != 0;
    else if (s->support_argument_buffers) return rays_metal_set_unavailable(message, "sampler argument-buffer support requires macOS 13 or newer");
    NSString *label = raw_label ? [NSString stringWithUTF8String:raw_label] : nil;
    if (raw_label && label == nil) return rays_metal_set_error(message, "sampler label is not valid UTF-8");
    if (label != nil) descriptor.label = label;
    id<MTLSamplerState> sampler = [device newSamplerStateWithDescriptor:descriptor];
    if (sampler == nil || sampler.device.registryID != device.registryID) return rays_metal_set_error(message, "Metal rejected the sampler descriptor");
    *output = rays_metal_handle_new((__bridge void *)sampler, KIND_SAMPLER);
    return *output ? 0 : rays_metal_set_error(message, "unable to retain Metal sampler");
  }} @catch (NSException *exception) { return rays_metal_set_error(message, exception.reason.UTF8String); }
}

extern "C" int32_t rays_metal_depth_stencil_create(MetalHandle *device_handle,
    const RaysDepthStencilDescriptor *s, const char *raw_label, MetalHandle **output,
    const char **message) {
  if (message) *message = nullptr; if (output) *output = nullptr;
  if (s == nullptr || output == nullptr || s->depth_compare_function < 0 || s->depth_compare_function > 7)
    return rays_metal_set_error(message, "invalid depth/stencil descriptor");
  @try { @autoreleasepool {
    id<MTLDevice> device = (__bridge id<MTLDevice>)rays_metal_handle_object(device_handle, KIND_DEVICE);
    if (device == nil) return rays_metal_set_error(message, "invalid Metal device handle");
    MTLDepthStencilDescriptor *descriptor = [[MTLDepthStencilDescriptor alloc] init];
    descriptor.depthCompareFunction = static_cast<MTLCompareFunction>(s->depth_compare_function);
    descriptor.depthWriteEnabled = s->depth_write_enabled != 0;
    auto make_face = [](const RaysStencilFaceDescriptor &f) -> MTLStencilDescriptor * {
      if (f.compare_function < 0 || f.compare_function > 7 || f.stencil_failure_operation < 0 || f.stencil_failure_operation > 7 || f.depth_failure_operation < 0 || f.depth_failure_operation > 7 || f.pass_operation < 0 || f.pass_operation > 7) return nil;
      MTLStencilDescriptor *v = [[MTLStencilDescriptor alloc] init];
      v.stencilCompareFunction = static_cast<MTLCompareFunction>(f.compare_function);
      v.stencilFailureOperation = static_cast<MTLStencilOperation>(f.stencil_failure_operation);
      v.depthFailureOperation = static_cast<MTLStencilOperation>(f.depth_failure_operation);
      v.depthStencilPassOperation = static_cast<MTLStencilOperation>(f.pass_operation);
      v.readMask = f.read_mask; v.writeMask = f.write_mask; return v;
    };
    if (s->has_front_face_stencil) { descriptor.frontFaceStencil = make_face(s->front_face_stencil); if (descriptor.frontFaceStencil == nil) return rays_metal_set_error(message, "invalid front stencil descriptor"); }
    if (s->has_back_face_stencil) { descriptor.backFaceStencil = make_face(s->back_face_stencil); if (descriptor.backFaceStencil == nil) return rays_metal_set_error(message, "invalid back stencil descriptor"); }
    NSString *label = raw_label ? [NSString stringWithUTF8String:raw_label] : nil;
    if (raw_label && label == nil) return rays_metal_set_error(message, "depth/stencil label is not valid UTF-8");
    if (label != nil) descriptor.label = label;
    id<MTLDepthStencilState> state = [device newDepthStencilStateWithDescriptor:descriptor];
    if (state == nil || state.device.registryID != device.registryID) return rays_metal_set_error(message, "Metal rejected the depth/stencil descriptor");
    *output = rays_metal_handle_new((__bridge void *)state, KIND_DEPTH_STENCIL);
    return *output ? 0 : rays_metal_set_error(message, "unable to retain Metal depth/stencil state");
  }} @catch (NSException *exception) { return rays_metal_set_error(message, exception.reason.UTF8String); }
}

extern "C" int32_t rays_metal_render_pass_descriptor_set_sizes(MetalHandle *pass_handle,
    int64_t width, int64_t height, int64_t array_length, int64_t sample_count,
    const char **message) {
  if (message) *message = nullptr;
  @try { @autoreleasepool {
    MTLRenderPassDescriptor *pass = (__bridge MTLRenderPassDescriptor *)rays_metal_handle_object(pass_handle, KIND_RENDER_PASS_DESCRIPTOR);
    if (!pass) return rays_metal_set_error(message, "invalid render-pass handle");
    pass.renderTargetWidth = static_cast<NSUInteger>(width); pass.renderTargetHeight = static_cast<NSUInteger>(height);
    pass.renderTargetArrayLength = static_cast<NSUInteger>(array_length); pass.defaultRasterSampleCount = static_cast<NSUInteger>(sample_count);
    return 0;
  }} @catch (NSException *exception) { return rays_metal_set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_pass_descriptor_set_attachments(MetalHandle *pass_handle, MetalHandle *color_handle,
    MetalHandle *depth_handle, MetalHandle *stencil_handle, MetalHandle *visibility_handle,
    const double *clear, const char **message) {
  if (message) *message = nullptr;
  if (!clear) return rays_metal_set_error(message, "missing render-pass clear color");
  @try { @autoreleasepool {
    MTLRenderPassDescriptor *pass = (__bridge MTLRenderPassDescriptor *)rays_metal_handle_object(pass_handle, KIND_RENDER_PASS_DESCRIPTOR);
    id<MTLTexture> color = (__bridge id<MTLTexture>)rays_metal_handle_object(color_handle, KIND_TEXTURE);
    id<MTLTexture> depth = depth_handle ? (__bridge id<MTLTexture>)rays_metal_handle_object(depth_handle, KIND_TEXTURE) : nil;
    id<MTLTexture> stencil = stencil_handle ? (__bridge id<MTLTexture>)rays_metal_handle_object(stencil_handle, KIND_TEXTURE) : nil;
    id<MTLBuffer> visibility = visibility_handle ? (__bridge id<MTLBuffer>)rays_metal_handle_object(visibility_handle, KIND_BUFFER) : nil;
    if (!pass || !color || (depth_handle && !depth) || (stencil_handle && !stencil) ||
        (visibility_handle && !visibility) || (depth && depth.device.registryID != color.device.registryID) ||
        (stencil && stencil.device.registryID != color.device.registryID) ||
        (visibility && visibility.device.registryID != color.device.registryID))
      return rays_metal_set_error(message, "invalid or incompatible render-pass attachment");
    pass.colorAttachments[0].texture = color; pass.colorAttachments[0].clearColor = MTLClearColorMake(clear[0],clear[1],clear[2],clear[3]);
    if (depth) pass.depthAttachment.texture = depth;
    if (stencil) pass.stencilAttachment.texture = stencil;
    if (visibility) pass.visibilityResultBuffer = visibility;
    return 0;
  }} @catch (NSException *exception) { return rays_metal_set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_pass_set_actions(MetalHandle *pass_handle, int32_t color_load,
    int32_t color_store, int32_t depth_load, int32_t depth_store, double clear_depth,
    int32_t stencil_load, int32_t stencil_store, uint32_t clear_stencil, const char **message) {
  if (message) *message = nullptr;
  @try { @autoreleasepool {
    MTLRenderPassDescriptor *pass = (__bridge MTLRenderPassDescriptor *)rays_metal_handle_object(pass_handle, KIND_RENDER_PASS_DESCRIPTOR);
    if (!pass) return rays_metal_set_error(message, "invalid render-pass handle");
    pass.colorAttachments[0].loadAction = static_cast<MTLLoadAction>(color_load);
    pass.colorAttachments[0].storeAction = static_cast<MTLStoreAction>(color_store);
    pass.depthAttachment.loadAction = static_cast<MTLLoadAction>(depth_load);
    pass.depthAttachment.storeAction = static_cast<MTLStoreAction>(depth_store);
    pass.depthAttachment.clearDepth = clear_depth;
    pass.stencilAttachment.loadAction = static_cast<MTLLoadAction>(stencil_load);
    pass.stencilAttachment.storeAction = static_cast<MTLStoreAction>(stencil_store);
    pass.stencilAttachment.clearStencil = clear_stencil;
    return 0;
  }} @catch (NSException *exception) { return rays_metal_set_error(message, exception.reason.UTF8String); }
}
// ogpu_metal: the multisample resolve target of colour attachment 0 (nullptr clears it).
extern "C" int32_t rays_metal_render_pass_set_resolve(MetalHandle *pass_handle, MetalHandle *texture_handle, const char **message) {
  if (message) *message = nullptr;
  @try { @autoreleasepool {
    MTLRenderPassDescriptor *pass = (__bridge MTLRenderPassDescriptor *)rays_metal_handle_object(pass_handle, KIND_RENDER_PASS_DESCRIPTOR);
    if (!pass) return rays_metal_set_error(message, "invalid render-pass handle");
    id<MTLTexture> texture = nil;
    if (texture_handle) {
      texture = (__bridge id<MTLTexture>)rays_metal_handle_object(texture_handle, KIND_TEXTURE);
      if (!texture) return rays_metal_set_error(message, "invalid resolve texture handle");
    }
    pass.colorAttachments[0].resolveTexture = texture;
    return 0;
  }} @catch (NSException *exception) { return rays_metal_set_error(message, exception.reason.UTF8String); }
}
