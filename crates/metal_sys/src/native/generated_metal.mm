#include <Foundation/Foundation.h>
#include <Metal/Metal.h>
#include <QuartzCore/CAMetalLayer.h>
#include <cstddef>
#include <cstdint>
#include <cmath>
#include <string>
#include <new>
#include <limits>
#include <type_traits>
#include "metal_handle.h"
#include "metal_kinds.h"
struct MetalHandle { void *object; uint32_t kind; };
static thread_local std::string error_text;
static thread_local std::string text_buffer;
extern "C" int32_t rays_metal_set_error(const char **out, const char *message) { error_text = message ? message : "Metal operation failed"; if (out) *out = error_text.c_str(); return 1; }
extern "C" int32_t rays_metal_set_unavailable(const char **out, const char *message) { error_text = message ? message : "Metal API is unavailable on this macOS version"; if (out) *out = error_text.c_str(); return 2; }
extern "C" void *rays_metal_handle_object(MetalHandle *handle, uint32_t expected_kind) { if (handle == nullptr || handle->object == nullptr || handle->kind != expected_kind) return nullptr; return handle->object; }
extern "C" MetalHandle *rays_metal_handle_new(void *pointer, uint32_t kind) { if (pointer == nullptr) return nullptr; id object = (__bridge id)pointer; return new (std::nothrow) MetalHandle{(__bridge_retained void *)object, kind}; }
static int32_t set_error(const char **out, const char *message) { return rays_metal_set_error(out, message); }
static int32_t set_unavailable(const char **out) { return rays_metal_set_unavailable(out, nullptr); }
static MetalHandle *make_handle(id object, uint32_t kind) { return object == nil ? nullptr : rays_metal_handle_new((__bridge void *)object, kind); }
static id checked_object(MetalHandle *handle, uint32_t kind) { void *pointer = rays_metal_handle_object(handle, kind); if (pointer == nullptr) @throw [NSException exceptionWithName:@"MetalHandleError" reason:@"invalid Metal handle" userInfo:nil]; return (__bridge id)pointer; }
extern "C" void rays_metal_handle_destroy(MetalHandle *handle) { @autoreleasepool { if (handle != nullptr) { if (handle->object != nullptr) (void)CFBridgingRelease(handle->object); delete handle; } } }
extern "C" int32_t rays_metal_default_device(MetalHandle **out, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLDevice> device = MTLCreateSystemDefaultDevice(); if (device == nil) return set_error(message, "no default Metal device"); *out = make_handle(device, KIND_DEVICE); return *out ? 0 : set_error(message, "unable to allocate Metal handle"); } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }
extern "C" int32_t rays_metal_device_supports_sampler_reduction(MetalHandle *handle, uint8_t *output, const char **message) { if (message) *message = nullptr; if (output == nullptr) return set_error(message, "invalid sampler capability output"); @try { @autoreleasepool { id<MTLDevice> device = checked_object(handle, KIND_DEVICE); BOOL supported = NO; if (@available(macOS 26.0, *)) { supported = [device supportsFamily:MTLGPUFamilyApple10] && [MTLSamplerDescriptor instancesRespondToSelector:@selector(reductionMode)] && [MTLSamplerDescriptor instancesRespondToSelector:@selector(setReductionMode:)] && [MTLSamplerDescriptor instancesRespondToSelector:@selector(lodBias)] && [MTLSamplerDescriptor instancesRespondToSelector:@selector(setLodBias:)]; } *output = supported ? 1 : 0; return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }
extern "C" int pthread_main_np(void);
extern "C" int32_t rays_metal_is_main_thread(void) { return pthread_main_np(); }
extern "C" int32_t rays_metal_buffer_copy(MetalHandle *handle, uint64_t offset, uint8_t *bytes, uintptr_t length, uint8_t write, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLBuffer> buffer = checked_object(handle, KIND_BUFFER); if (offset > buffer.length || length > buffer.length - offset) return set_error(message, "buffer range is out of bounds"); void *contents = buffer.contents; if (contents == nullptr) return set_error(message, "buffer has no CPU mapping"); if (length != 0) { if (write) __builtin_memcpy(static_cast<uint8_t *>(contents) + offset, bytes, length); else __builtin_memcpy(bytes, static_cast<uint8_t *>(contents) + offset, length); } if (write && length != 0 && buffer.storageMode == MTLStorageModeManaged) [buffer didModifyRange:NSMakeRange(static_cast<NSUInteger>(offset), static_cast<NSUInteger>(length))]; return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }
extern "C" int32_t rays_metal_buffer_readback(MetalHandle *handle, uint8_t **contents, uint64_t *length, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLBuffer> buffer = checked_object(handle, KIND_BUFFER); if (buffer.storageMode != MTLStorageModeShared) return set_error(message, "readback requires shared buffer storage"); if (contents == nullptr || length == nullptr) return set_error(message, "invalid readback output"); *contents = static_cast<uint8_t *>(buffer.contents); *length = buffer.length; if (*contents == nullptr) return set_error(message, "buffer has no CPU mapping"); return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }
#include <unistd.h>
extern "C" int32_t rays_metal_buffer_create_mapped(MetalHandle *device_handle, uint64_t length, int64_t options, MetalHandle **out, float **mapping, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLDevice> device = checked_object(device_handle, KIND_DEVICE); const uint64_t page = static_cast<uint64_t>(getpagesize()); if (page == 0 || length > UINT64_MAX - (page - 1)) return set_error(message, "mapped buffer rounded length overflows"); const uint64_t rounded = ((length + page - 1) / page) * page; if (rounded > std::numeric_limits<NSUInteger>::max()) return set_error(message, "mapped buffer exceeds Metal length bounds"); id<MTLBuffer> buffer = [device newBufferWithLength:static_cast<NSUInteger>(rounded) options:static_cast<MTLResourceOptions>(options)]; if (buffer == nil || buffer.contents == nullptr) return set_error(message, "unable to allocate mapped Metal buffer"); __builtin_memset(buffer.contents, 0, static_cast<size_t>(rounded)); *out = make_handle(buffer, KIND_BUFFER); if (*out == nullptr) return set_error(message, "unable to allocate Metal handle"); *mapping = static_cast<float *>(buffer.contents); return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }
struct RaysTextureDescriptor { int32_t texture_type; int32_t pixel_format; uint64_t width; uint64_t height; uint64_t depth; uint64_t mip_levels; uint64_t sample_count; uint64_t array_length; int32_t storage_mode; int32_t cpu_cache_mode; int32_t hazard_tracking_mode; int32_t usage; uint8_t allow_gpu_optimized_contents; int32_t compression_type; int32_t swizzle_red; int32_t swizzle_green; int32_t swizzle_blue; int32_t swizzle_alpha; };
extern "C" int32_t rays_metal_texture_create(MetalHandle *device_handle, const RaysTextureDescriptor *source, const char *label, MetalHandle **out, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLDevice> device = checked_object(device_handle, KIND_DEVICE); MTLTextureDescriptor *descriptor = [[MTLTextureDescriptor alloc] init]; descriptor.textureType = static_cast<MTLTextureType>(source->texture_type); descriptor.pixelFormat = static_cast<MTLPixelFormat>(source->pixel_format); descriptor.width = static_cast<NSUInteger>(source->width); descriptor.height = static_cast<NSUInteger>(source->height); descriptor.depth = static_cast<NSUInteger>(source->depth); descriptor.mipmapLevelCount = static_cast<NSUInteger>(source->mip_levels); descriptor.sampleCount = static_cast<NSUInteger>(source->sample_count); descriptor.arrayLength = static_cast<NSUInteger>(source->array_length); descriptor.storageMode = static_cast<MTLStorageMode>(source->storage_mode); descriptor.cpuCacheMode = static_cast<MTLCPUCacheMode>(source->cpu_cache_mode); descriptor.hazardTrackingMode = static_cast<MTLHazardTrackingMode>(source->hazard_tracking_mode); descriptor.usage = static_cast<MTLTextureUsage>(source->usage); descriptor.allowGPUOptimizedContents = source->allow_gpu_optimized_contents != 0; if (@available(macOS 15.0, *)) descriptor.compressionType = static_cast<MTLTextureCompressionType>(source->compression_type); else if (source->compression_type != 0) return set_unavailable(message); if (@available(macOS 13.0, *)) descriptor.swizzle = MTLTextureSwizzleChannelsMake(static_cast<MTLTextureSwizzle>(source->swizzle_red), static_cast<MTLTextureSwizzle>(source->swizzle_green), static_cast<MTLTextureSwizzle>(source->swizzle_blue), static_cast<MTLTextureSwizzle>(source->swizzle_alpha)); else if (source->swizzle_red != 2 || source->swizzle_green != 3 || source->swizzle_blue != 4 || source->swizzle_alpha != 5) return set_unavailable(message); NSString *text = nil; if (label != nullptr) { text = [NSString stringWithUTF8String:label]; if (text == nil) return set_error(message, "invalid UTF-8 texture label"); } id<MTLTexture> texture = [device newTextureWithDescriptor:descriptor]; if (texture == nil) return set_error(message, "Metal returned no texture"); if (text != nil) texture.label = text; *out = make_handle(texture, KIND_TEXTURE); return *out ? 0 : set_error(message, "unable to allocate Metal handle"); } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }
extern "C" int32_t rays_metal_texture_read(MetalHandle *handle, uint8_t *destination, uintptr_t destination_length, uintptr_t bytes_per_row, uintptr_t bytes_per_image, uint64_t x, uint64_t y, uint64_t z, uint64_t width, uint64_t height, uint64_t depth, uint64_t mip_level, uint64_t slice, const char **message) {
  if (message) *message = nullptr;
  @try { @autoreleasepool {
    id<MTLTexture> texture = checked_object(handle, KIND_TEXTURE);
    // This safe Rust ABI accepts a borrowed slice. Bound the actual native write, including
    // standalone sys callers that have no Core admission value; never trust a raw pointer alone.
    if (texture.storageMode == MTLStorageModePrivate || texture.storageMode == MTLStorageModeMemoryless || texture.sampleCount != 1)
      return rays_metal_set_unsupported(message, "texture has no CPU transfer mapping");
    if (destination == nullptr || mip_level >= texture.mipmapLevelCount || width == 0 || height == 0 || depth == 0)
      return rays_metal_set_invalid_argument(message, "invalid texture region or mip level");
    const NSUInteger shift = static_cast<NSUInteger>(mip_level > 63 ? 63 : mip_level);
    const NSUInteger mw = MAX(NSUInteger(1), texture.width >> shift);
    const NSUInteger mh = MAX(NSUInteger(1), texture.height >> shift);
    const NSUInteger md = MAX(NSUInteger(1), texture.depth >> shift);
    if (x > mw || width > mw - x || y > mh || height > mh - y || z > md || depth > md - z)
      return rays_metal_set_invalid_argument(message, "texture region exceeds the native allocation");
    NSUInteger slices = 1;
    switch (texture.textureType) {
      case MTLTextureTypeCube: slices = 6; break;
      case MTLTextureTypeCubeArray: slices = texture.arrayLength * 6; break;
      case MTLTextureType1DArray: case MTLTextureType2DArray: slices = texture.arrayLength; break;
      default: break;
    }
    if (slice >= slices) return rays_metal_set_invalid_argument(message, "texture slice exceeds the native allocation");
    NSUInteger texel = 0;
    switch (texture.pixelFormat) {
      case MTLPixelFormatR8Unorm: texel = 1; break;
      case MTLPixelFormatRGBA8Unorm: case MTLPixelFormatBGRA8Unorm:
      case MTLPixelFormatBGRA8Unorm_sRGB: case MTLPixelFormatDepth32Float: texel = 4; break;
      case MTLPixelFormatRGBA16Float: texel = 8; break;
      case MTLPixelFormatRGBA32Float: texel = 16; break;
      default: return rays_metal_set_unsupported(message, "native CPU transfer pixel format is unsupported");
    }
    const NSUInteger max = std::numeric_limits<NSUInteger>::max();
    if (width > max / texel) return rays_metal_set_invalid_argument(message, "texture row size overflow");
    const NSUInteger row = static_cast<NSUInteger>(width) * texel;
    if (bytes_per_row < row || bytes_per_row % texel != 0 || height > max / bytes_per_row)
      return rays_metal_set_invalid_argument(message, "texture row pitch is invalid");
    const NSUInteger image = static_cast<NSUInteger>(height) * bytes_per_row;
    if (depth > 1 && bytes_per_image < image)
      return rays_metal_set_invalid_argument(message, "texture image pitch is invalid");
    const NSUInteger rows = static_cast<NSUInteger>(height - 1) * bytes_per_row;
    if (depth > 1 && depth - 1 > max / bytes_per_image)
      return rays_metal_set_invalid_argument(message, "texture transfer size overflow");
    const NSUInteger images = static_cast<NSUInteger>(depth - 1) * bytes_per_image;
    if (row > max - rows || rows + row > max - images || images + rows + row > destination_length)
      return rays_metal_set_invalid_argument(message, "texture destination is too short");
    MTLRegion region = MTLRegionMake3D(static_cast<NSUInteger>(x), static_cast<NSUInteger>(y), static_cast<NSUInteger>(z), static_cast<NSUInteger>(width), static_cast<NSUInteger>(height), static_cast<NSUInteger>(depth));
    [texture getBytes:destination bytesPerRow:bytes_per_row bytesPerImage:bytes_per_image fromRegion:region mipmapLevel:static_cast<NSUInteger>(mip_level) slice:static_cast<NSUInteger>(slice)];
    return 0;
  } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_command_buffer_wait(MetalHandle *handle, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLCommandBuffer> buffer = checked_object(handle, KIND_COMMAND_BUFFER); [buffer waitUntilCompleted]; return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }
extern "C" int32_t rays_metal_command_buffer_error(MetalHandle *handle, const char **output) { @try { @autoreleasepool { id<MTLCommandBuffer> buffer = checked_object(handle, KIND_COMMAND_BUFFER); NSError *error = buffer.error; if (error == nil) { *output = nullptr; return 0; } text_buffer = error.localizedDescription.UTF8String ?: "Metal command buffer failed"; *output = text_buffer.c_str(); return 1; } } @catch (NSException *exception) { text_buffer = exception.reason.UTF8String ?: "Metal command buffer failed"; *output = text_buffer.c_str(); return 1; } }
extern "C" int32_t rays_metal_compute_encoder_set_bytes(MetalHandle *handle, const uint8_t *bytes, uintptr_t length, uint64_t index, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLComputeCommandEncoder> encoder = checked_object(handle, KIND_COMPUTE_ENCODER); [encoder setBytes:bytes length:length atIndex:static_cast<NSUInteger>(index)]; return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }
extern "C" int32_t rays_metal_compute_encoder_use_accelerations(MetalHandle *handle, MetalHandle **structures, uintptr_t count, const char **message) { if (message) *message = nullptr; if (count == 0) return set_error(message, "acceleration structure list must not be empty"); @try { @autoreleasepool { id<MTLComputeCommandEncoder> encoder = checked_object(handle, KIND_COMPUTE_ENCODER); for (uintptr_t index = 0; index < count; ++index) { id<MTLAccelerationStructure> structure = checked_object(structures[index], KIND_ACCELERATION_STRUCTURE); [encoder useResource:structure usage:MTLResourceUsageRead]; } return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }
struct RaysPresentationCommandSnapshot { uint64_t queue_device_id; uint64_t device_id; uint64_t error_options; double gpu_start; double gpu_end; double kernel_start; double kernel_end; uint8_t retained_references; };
extern "C" int32_t rays_metal_presentation_command_snapshot(MetalHandle *handle, RaysPresentationCommandSnapshot *out, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLCommandBuffer> buffer = checked_object(handle, KIND_COMMAND_BUFFER); out->queue_device_id = buffer.commandQueue.device.registryID; out->device_id = buffer.device.registryID; out->error_options = static_cast<uint64_t>(buffer.errorOptions); out->gpu_start = buffer.GPUStartTime; out->gpu_end = buffer.GPUEndTime; out->kernel_start = buffer.kernelStartTime; out->kernel_end = buffer.kernelEndTime; out->retained_references = buffer.retainedReferences ? 1 : 0; return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }
extern "C" int32_t rays_metal_texture_info(MetalHandle *handle, int64_t *values, const char **message) { if (message) *message = nullptr; if (values == nullptr) return set_error(message, "invalid texture-info output"); @try { @autoreleasepool { id<MTLTexture> texture = checked_object(handle, KIND_TEXTURE); MTLTextureSwizzleChannels swizzle = texture.swizzle; values[0] = static_cast<int64_t>(texture.textureType); values[1] = static_cast<int64_t>(texture.pixelFormat); values[2] = static_cast<int64_t>(texture.width); values[3] = static_cast<int64_t>(texture.height); values[4] = static_cast<int64_t>(texture.depth); values[5] = static_cast<int64_t>(texture.mipmapLevelCount); values[6] = static_cast<int64_t>(texture.sampleCount); values[7] = static_cast<int64_t>(texture.arrayLength); values[8] = static_cast<int64_t>(texture.usage); values[9] = static_cast<int64_t>(texture.storageMode); values[10] = static_cast<int64_t>(texture.cpuCacheMode); values[11] = static_cast<int64_t>(texture.hazardTrackingMode); values[12] = texture.allowGPUOptimizedContents ? 1 : 0; values[13] = static_cast<int64_t>(texture.compressionType); values[14] = static_cast<int64_t>(swizzle.red); values[15] = static_cast<int64_t>(swizzle.green); values[16] = static_cast<int64_t>(swizzle.blue); values[17] = static_cast<int64_t>(swizzle.alpha); return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }
#include <cstring>
extern "C" int32_t rays_metal_library_compile(MetalHandle *device_handle, const char *source_bytes, const char *label_bytes, MetalHandle **out, const char **message) { if (message) *message = nullptr; if (source_bytes == nullptr) return set_error(message, "null Metal source"); @try { @autoreleasepool { id<MTLDevice> device = checked_object(device_handle, KIND_DEVICE); NSString *source = [NSString stringWithUTF8String:source_bytes]; if (source == nil) return set_error(message, "Metal source is not valid UTF-8"); NSString *label = label_bytes ? [NSString stringWithUTF8String:label_bytes] : nil; if (label_bytes && label == nil) return set_error(message, "Metal library label is not valid UTF-8"); MTLCompileOptions *options = [[MTLCompileOptions alloc] init]; options.fastMathEnabled = NO; NSError *error = nil; id<MTLLibrary> library = [device newLibraryWithSource:source options:options error:&error]; if (library == nil) { NSString *description = error.localizedDescription ?: @"Metal source compilation failed without NSError"; if (label) description = [NSString stringWithFormat:@"%@: %@", label, description]; return set_error(message, description.UTF8String); } library.label = label; if (library.device.registryID != device.registryID || ((label == nil) != (library.label == nil)) || (label && ![library.label isEqualToString:label])) return set_error(message, "Metal changed checked library properties"); *out = make_handle(library, KIND_LIBRARY); return *out ? 0 : set_error(message, "unable to allocate Metal handle"); } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }
extern "C" int32_t rays_metal_library_function_names(MetalHandle *handle, char **output, uintptr_t *length, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLLibrary> library = checked_object(handle, KIND_LIBRARY); std::string packed; for (NSString *name in library.functionNames) { const char *utf8 = name.UTF8String ?: ""; packed.append(utf8); packed.push_back('\0'); } if (packed.empty()) packed.push_back('\0'); char *bytes = new (std::nothrow) char[packed.size()]; if (bytes == nullptr) return set_error(message, "unable to allocate function-name buffer"); std::memcpy(bytes, packed.data(), packed.size()); *output = bytes; *length = packed.size(); return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }
extern "C" void rays_metal_free_bytes(char *bytes) { delete[] bytes; }
extern "C" int32_t rays_metal_function_find(MetalHandle *library_handle, const char *name_bytes, MetalHandle **out, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLLibrary> library = checked_object(library_handle, KIND_LIBRARY); NSString *name = name_bytes ? [NSString stringWithUTF8String:name_bytes] : nil; if (name == nil) return set_error(message, "Metal function name is not valid UTF-8"); id<MTLFunction> function = [library newFunctionWithName:name]; if (function == nil) return set_error(message, "Metal library has no function with the requested name"); if (function.device.registryID != library.device.registryID || ![function.name isEqualToString:name]) return set_error(message, "Metal changed checked function lookup properties"); *out = make_handle(function, KIND_FUNCTION); return *out ? 0 : set_error(message, "unable to allocate Metal handle"); } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }
struct RaysFunctionConstantValue { const char *name; int32_t tag; int64_t integral; double floating; };
extern "C" int32_t rays_metal_function_specialize(MetalHandle *library_handle, const char *name_bytes, const RaysFunctionConstantValue *constants, uintptr_t count, const char *label_bytes, MetalHandle **out, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLLibrary> library = checked_object(library_handle, KIND_LIBRARY); NSString *name = name_bytes ? [NSString stringWithUTF8String:name_bytes] : nil; NSString *label = label_bytes ? [NSString stringWithUTF8String:label_bytes] : nil; if (name == nil || (label_bytes && label == nil)) return set_error(message, "Metal function name or label is not valid UTF-8"); MTLFunctionConstantValues *values = [[MTLFunctionConstantValues alloc] init]; NSMutableSet<NSString *> *names = [NSMutableSet set]; for (uintptr_t index = 0; index < count; ++index) { const RaysFunctionConstantValue &constant = constants[index]; NSString *constant_name = constant.name ? [NSString stringWithUTF8String:constant.name] : nil; if (constant_name == nil || [names containsObject:constant_name]) return set_error(message, "function-constant names must be valid and unique"); [names addObject:constant_name]; switch (constant.tag) { case 0: { const bool value = constant.integral != 0; [values setConstantValue:&value type:MTLDataTypeBool withName:constant_name]; break; } case 1: { const int8_t value = static_cast<int8_t>(constant.integral); [values setConstantValue:&value type:MTLDataTypeChar withName:constant_name]; break; } case 2: { const uint8_t value = static_cast<uint8_t>(constant.integral); [values setConstantValue:&value type:MTLDataTypeUChar withName:constant_name]; break; } case 3: { const int16_t value = static_cast<int16_t>(constant.integral); [values setConstantValue:&value type:MTLDataTypeShort withName:constant_name]; break; } case 4: { const uint16_t value = static_cast<uint16_t>(constant.integral); [values setConstantValue:&value type:MTLDataTypeUShort withName:constant_name]; break; } case 5: { const int32_t value = static_cast<int32_t>(constant.integral); [values setConstantValue:&value type:MTLDataTypeInt withName:constant_name]; break; } case 6: { const uint32_t value = static_cast<uint32_t>(constant.integral); [values setConstantValue:&value type:MTLDataTypeUInt withName:constant_name]; break; } case 7: { const int64_t value = constant.integral; [values setConstantValue:&value type:MTLDataTypeLong withName:constant_name]; break; } case 8: { const uint64_t value = static_cast<uint64_t>(constant.integral); [values setConstantValue:&value type:MTLDataTypeULong withName:constant_name]; break; } case 9: { const _Float16 value = static_cast<_Float16>(constant.floating); [values setConstantValue:&value type:MTLDataTypeHalf withName:constant_name]; break; } case 10: { const float value = static_cast<float>(constant.floating); [values setConstantValue:&value type:MTLDataTypeFloat withName:constant_name]; break; } default: return set_error(message, "invalid function-constant type tag"); } } NSError *error = nil; id<MTLFunction> function = [library newFunctionWithName:name constantValues:values error:&error]; if (function == nil) return set_error(message, error.localizedDescription.UTF8String ?: "Metal function specialization failed without NSError"); function.label = label; if (function.device.registryID != library.device.registryID || ![function.name isEqualToString:name] || ((label == nil) != (function.label == nil)) || (label && ![function.label isEqualToString:label])) return set_error(message, "Metal changed checked function specialization properties"); *out = make_handle(function, KIND_FUNCTION); return *out ? 0 : set_error(message, "unable to allocate Metal handle"); } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }
static_assert(std::is_standard_layout_v<MTLViewport>);
static_assert(std::is_standard_layout_v<MTLScissorRect>);
static_assert(std::is_standard_layout_v<MTLSize>);
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeArray) == 0x0000000000000002ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 140000
static_assert(static_cast<unsigned long long>(MTLDataTypeBFloat) == 0x0000000000000079ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 140000
static_assert(static_cast<unsigned long long>(MTLDataTypeBFloat2) == 0x000000000000007aULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 140000
static_assert(static_cast<unsigned long long>(MTLDataTypeBFloat3) == 0x000000000000007bULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 140000
static_assert(static_cast<unsigned long long>(MTLDataTypeBFloat4) == 0x000000000000007cULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeBool) == 0x0000000000000035ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeBool2) == 0x0000000000000036ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeBool3) == 0x0000000000000037ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeBool4) == 0x0000000000000038ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeChar) == 0x000000000000002dULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeChar2) == 0x000000000000002eULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeChar3) == 0x000000000000002fULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeChar4) == 0x0000000000000030ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeComputePipeline) == 0x000000000000004fULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 260000
static_assert(static_cast<unsigned long long>(MTLDataTypeDepthStencilState) == 0x000000000000008bULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeFloat) == 0x0000000000000003ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeFloat2) == 0x0000000000000004ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeFloat2x2) == 0x0000000000000007ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeFloat2x3) == 0x0000000000000008ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeFloat2x4) == 0x0000000000000009ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeFloat3) == 0x0000000000000005ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeFloat3x2) == 0x000000000000000aULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeFloat3x3) == 0x000000000000000bULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeFloat3x4) == 0x000000000000000cULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeFloat4) == 0x0000000000000006ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeFloat4x2) == 0x000000000000000dULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeFloat4x3) == 0x000000000000000eULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeFloat4x4) == 0x000000000000000fULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeHalf) == 0x0000000000000010ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeHalf2) == 0x0000000000000011ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeHalf2x2) == 0x0000000000000014ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeHalf2x3) == 0x0000000000000015ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeHalf2x4) == 0x0000000000000016ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeHalf3) == 0x0000000000000012ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeHalf3x2) == 0x0000000000000017ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeHalf3x3) == 0x0000000000000018ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeHalf3x4) == 0x0000000000000019ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeHalf4) == 0x0000000000000013ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeHalf4x2) == 0x000000000000001aULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeHalf4x3) == 0x000000000000001bULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeHalf4x4) == 0x000000000000001cULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101400
static_assert(static_cast<unsigned long long>(MTLDataTypeIndirectCommandBuffer) == 0x0000000000000050ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeInstanceAccelerationStructure) == 0x0000000000000076ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeInt) == 0x000000000000001dULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeInt2) == 0x000000000000001eULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeInt3) == 0x000000000000001fULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeInt4) == 0x0000000000000020ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeIntersectionFunctionTable) == 0x0000000000000074ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 120000
static_assert(static_cast<unsigned long long>(MTLDataTypeLong) == 0x0000000000000051ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 120000
static_assert(static_cast<unsigned long long>(MTLDataTypeLong2) == 0x0000000000000052ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 120000
static_assert(static_cast<unsigned long long>(MTLDataTypeLong3) == 0x0000000000000053ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 120000
static_assert(static_cast<unsigned long long>(MTLDataTypeLong4) == 0x0000000000000054ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeNone) == 0x0000000000000000ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101300
static_assert(static_cast<unsigned long long>(MTLDataTypePointer) == 0x000000000000003cULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypePrimitiveAccelerationStructure) == 0x0000000000000075ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeR16Snorm) == 0x0000000000000041ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeR16Unorm) == 0x0000000000000040ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeR8Snorm) == 0x000000000000003fULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeR8Unorm) == 0x000000000000003eULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeRG11B10Float) == 0x000000000000004cULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeRG16Snorm) == 0x0000000000000045ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeRG16Unorm) == 0x0000000000000044ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeRG8Snorm) == 0x0000000000000043ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeRG8Unorm) == 0x0000000000000042ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeRGB10A2Unorm) == 0x000000000000004bULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeRGB9E5Float) == 0x000000000000004dULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeRGBA16Snorm) == 0x000000000000004aULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeRGBA16Unorm) == 0x0000000000000049ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeRGBA8Snorm) == 0x0000000000000048ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeRGBA8Unorm) == 0x0000000000000046ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeRGBA8Unorm_sRGB) == 0x0000000000000047ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101400
static_assert(static_cast<unsigned long long>(MTLDataTypeRenderPipeline) == 0x000000000000004eULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101300
static_assert(static_cast<unsigned long long>(MTLDataTypeSampler) == 0x000000000000003bULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeShort) == 0x0000000000000025ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeShort2) == 0x0000000000000026ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeShort3) == 0x0000000000000027ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeShort4) == 0x0000000000000028ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeStruct) == 0x0000000000000001ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 260000
static_assert(static_cast<unsigned long long>(MTLDataTypeTensor) == 0x000000000000008cULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101300
static_assert(static_cast<unsigned long long>(MTLDataTypeTexture) == 0x000000000000003aULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeUChar) == 0x0000000000000031ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeUChar2) == 0x0000000000000032ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeUChar3) == 0x0000000000000033ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeUChar4) == 0x0000000000000034ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeUInt) == 0x0000000000000021ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeUInt2) == 0x0000000000000022ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeUInt3) == 0x0000000000000023ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeUInt4) == 0x0000000000000024ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 120000
static_assert(static_cast<unsigned long long>(MTLDataTypeULong) == 0x0000000000000055ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 120000
static_assert(static_cast<unsigned long long>(MTLDataTypeULong2) == 0x0000000000000056ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 120000
static_assert(static_cast<unsigned long long>(MTLDataTypeULong3) == 0x0000000000000057ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 120000
static_assert(static_cast<unsigned long long>(MTLDataTypeULong4) == 0x0000000000000058ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeUShort) == 0x0000000000000029ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeUShort2) == 0x000000000000002aULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeUShort3) == 0x000000000000002bULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLDataTypeUShort4) == 0x000000000000002cULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 110000
static_assert(static_cast<unsigned long long>(MTLDataTypeVisibleFunctionTable) == 0x0000000000000073ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLFeatureSet_OSX_GPUFamily1_v1) == 0x0000000000002710ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101200
static_assert(static_cast<unsigned long long>(MTLFeatureSet_OSX_GPUFamily1_v2) == 0x0000000000002711ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101200
static_assert(static_cast<unsigned long long>(MTLFeatureSet_OSX_ReadWriteTextureTier2) == 0x0000000000002712ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
static_assert(static_cast<unsigned long long>(MTLFeatureSet_macOS_GPUFamily1_v1) == 0x0000000000002710ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101200
static_assert(static_cast<unsigned long long>(MTLFeatureSet_macOS_GPUFamily1_v2) == 0x0000000000002711ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101300
static_assert(static_cast<unsigned long long>(MTLFeatureSet_macOS_GPUFamily1_v3) == 0x0000000000002713ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101400
static_assert(static_cast<unsigned long long>(MTLFeatureSet_macOS_GPUFamily1_v4) == 0x0000000000002714ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101400
static_assert(static_cast<unsigned long long>(MTLFeatureSet_macOS_GPUFamily2_v1) == 0x0000000000002715ULL);
#endif
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101200
static_assert(static_cast<unsigned long long>(MTLFeatureSet_macOS_ReadWriteTextureTier2) == 0x0000000000002712ULL);
#endif
extern "C" int32_t rays_metal_device_supports_family(MetalHandle *receiver, int64_t arg0, uint8_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      const BOOL native_result = [object supportsFamily:static_cast<MTLGPUFamily>(arg0)];
      *out = native_result ? 1 : 0;
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_device_max_threadgroup_memory_length(MetalHandle *receiver, uint64_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      if (@available(macOS 10.13, *)) {
        const NSUInteger native_result = object.maxThreadgroupMemoryLength;
        *out = static_cast<uint64_t>(native_result);
        return 0;
      } else { return set_unavailable(message); }
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_pipeline_state_support_indirect_command_buffers(MetalHandle *receiver, uint8_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderPipelineState> object = checked_object(receiver, KIND_RENDER_PIPELINE);
      if (@available(macOS 10.14, *)) {
        const BOOL native_result = object.supportIndirectCommandBuffers;
        *out = native_result ? 1 : 0;
        return 0;
      } else { return set_unavailable(message); }
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_device_name(MetalHandle *receiver, const char * *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      const NSString * native_result = object.name;
      text_buffer = native_result.UTF8String ?: ""; *out = text_buffer.c_str();
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_device_registry_id(MetalHandle *receiver, uint64_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      const NSUInteger native_result = object.registryID;
      *out = static_cast<uint64_t>(native_result);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_device_is_low_power(MetalHandle *receiver, uint8_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      const BOOL native_result = object.lowPower;
      *out = native_result ? 1 : 0;
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_device_is_removable(MetalHandle *receiver, uint8_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      const BOOL native_result = object.removable;
      *out = native_result ? 1 : 0;
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_device_is_headless(MetalHandle *receiver, uint8_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      const BOOL native_result = object.headless;
      *out = native_result ? 1 : 0;
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_device_has_unified_memory(MetalHandle *receiver, uint8_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      const BOOL native_result = object.hasUnifiedMemory;
      *out = native_result ? 1 : 0;
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_device_recommended_max_working_set_size(MetalHandle *receiver, uint64_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      const NSUInteger native_result = object.recommendedMaxWorkingSetSize;
      *out = static_cast<uint64_t>(native_result);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_device_current_allocated_size(MetalHandle *receiver, uint64_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      const NSUInteger native_result = object.currentAllocatedSize;
      *out = static_cast<uint64_t>(native_result);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_device_max_buffer_length(MetalHandle *receiver, uint64_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      const NSUInteger native_result = object.maxBufferLength;
      *out = static_cast<uint64_t>(native_result);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_device_supports_raytracing(MetalHandle *receiver, uint8_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      const BOOL native_result = object.supportsRaytracing;
      *out = native_result ? 1 : 0;
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_device_supports_raytracing_from_render(MetalHandle *receiver, uint8_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      const BOOL native_result = object.supportsRaytracingFromRender;
      *out = native_result ? 1 : 0;
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_device_supports_dynamic_libraries(MetalHandle *receiver, uint8_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      const BOOL native_result = object.supportsDynamicLibraries;
      *out = native_result ? 1 : 0;
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_device_supports_texture_sample_count(MetalHandle *receiver, uint64_t arg0, uint8_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      const BOOL native_result = [object supportsTextureSampleCount:static_cast<NSUInteger>(arg0)];
      *out = native_result ? 1 : 0;
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_command_queue_create(MetalHandle *receiver, MetalHandle * *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      const id native_result = [object newCommandQueue];
      if (native_result == nil) return set_error(message, "Metal returned nil"); *out = make_handle(native_result, KIND_COMMAND_QUEUE);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_command_buffer_create(MetalHandle *receiver, MetalHandle * *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLCommandQueue> object = checked_object(receiver, KIND_COMMAND_QUEUE);
      const id native_result = [object commandBuffer];
      if (native_result == nil) return set_error(message, "Metal returned nil"); *out = make_handle(native_result, KIND_COMMAND_BUFFER);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_command_buffer_status(MetalHandle *receiver, int64_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLCommandBuffer> object = checked_object(receiver, KIND_COMMAND_BUFFER);
      const MTLCommandBufferStatus native_result = object.status;
      *out = static_cast<int64_t>(native_result);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_command_buffer_commit(MetalHandle *receiver, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLCommandBuffer> object = checked_object(receiver, KIND_COMMAND_BUFFER);
      [object commit];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_command_buffer_compute_encoder(MetalHandle *receiver, MetalHandle * *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLCommandBuffer> object = checked_object(receiver, KIND_COMMAND_BUFFER);
      const id native_result = [object computeCommandEncoder];
      if (native_result == nil) return set_error(message, "Metal returned nil"); *out = make_handle(native_result, KIND_COMPUTE_ENCODER);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_command_buffer_blit_encoder(MetalHandle *receiver, MetalHandle * *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLCommandBuffer> object = checked_object(receiver, KIND_COMMAND_BUFFER);
      const id native_result = [object blitCommandEncoder];
      if (native_result == nil) return set_error(message, "Metal returned nil"); *out = make_handle(native_result, KIND_BLIT_ENCODER);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_command_buffer_acceleration_encoder(MetalHandle *receiver, MetalHandle * *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLCommandBuffer> object = checked_object(receiver, KIND_COMMAND_BUFFER);
      const id native_result = [object accelerationStructureCommandEncoder];
      if (native_result == nil) return set_error(message, "Metal returned nil"); *out = make_handle(native_result, KIND_ACCELERATION_ENCODER);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_command_buffer_render_encoder_from_pass(MetalHandle *receiver, MetalHandle * arg0, MetalHandle * *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLCommandBuffer> object = checked_object(receiver, KIND_COMMAND_BUFFER);
      const id native_result = [object renderCommandEncoderWithDescriptor:checked_object(arg0, KIND_RENDER_PASS_DESCRIPTOR)];
      if (native_result == nil) return set_error(message, "Metal returned nil"); *out = make_handle(native_result, KIND_RENDER_ENCODER);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_compute_encoder_end(MetalHandle *receiver, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLComputeCommandEncoder> object = checked_object(receiver, KIND_COMPUTE_ENCODER);
      [object endEncoding];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_encoder_end(MetalHandle *receiver, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> object = checked_object(receiver, KIND_RENDER_ENCODER);
      [object endEncoding];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_blit_encoder_end(MetalHandle *receiver, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLBlitCommandEncoder> object = checked_object(receiver, KIND_BLIT_ENCODER);
      [object endEncoding];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_acceleration_encoder_end(MetalHandle *receiver, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLAccelerationStructureCommandEncoder> object = checked_object(receiver, KIND_ACCELERATION_ENCODER);
      [object endEncoding];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_encoder_set_pipeline(MetalHandle *receiver, MetalHandle * arg0, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> object = checked_object(receiver, KIND_RENDER_ENCODER);
      [object setRenderPipelineState:checked_object(arg0, KIND_RENDER_PIPELINE)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_encoder_set_vertex_buffer(MetalHandle *receiver, MetalHandle * arg0, uint64_t arg1, uint64_t arg2, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> object = checked_object(receiver, KIND_RENDER_ENCODER);
      [object setVertexBuffer:checked_object(arg0, KIND_BUFFER) offset:static_cast<NSUInteger>(arg1) atIndex:static_cast<NSUInteger>(arg2)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_encoder_set_fragment_buffer(MetalHandle *receiver, MetalHandle * arg0, uint64_t arg1, uint64_t arg2, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> object = checked_object(receiver, KIND_RENDER_ENCODER);
      [object setFragmentBuffer:checked_object(arg0, KIND_BUFFER) offset:static_cast<NSUInteger>(arg1) atIndex:static_cast<NSUInteger>(arg2)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_encoder_set_vertex_texture(MetalHandle *receiver, MetalHandle * arg0, uint64_t arg1, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> object = checked_object(receiver, KIND_RENDER_ENCODER);
      [object setVertexTexture:checked_object(arg0, KIND_TEXTURE) atIndex:static_cast<NSUInteger>(arg1)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_encoder_set_fragment_texture(MetalHandle *receiver, MetalHandle * arg0, uint64_t arg1, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> object = checked_object(receiver, KIND_RENDER_ENCODER);
      [object setFragmentTexture:checked_object(arg0, KIND_TEXTURE) atIndex:static_cast<NSUInteger>(arg1)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_encoder_set_vertex_sampler(MetalHandle *receiver, MetalHandle * arg0, uint64_t arg1, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> object = checked_object(receiver, KIND_RENDER_ENCODER);
      [object setVertexSamplerState:checked_object(arg0, KIND_SAMPLER) atIndex:static_cast<NSUInteger>(arg1)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_encoder_set_fragment_sampler(MetalHandle *receiver, MetalHandle * arg0, uint64_t arg1, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> object = checked_object(receiver, KIND_RENDER_ENCODER);
      [object setFragmentSamplerState:checked_object(arg0, KIND_SAMPLER) atIndex:static_cast<NSUInteger>(arg1)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_depth_stencil(MetalHandle *receiver, MetalHandle * arg0, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> object = checked_object(receiver, KIND_RENDER_ENCODER);
      [object setDepthStencilState:(arg0 == nullptr ? nil : checked_object(arg0, KIND_DEPTH_STENCIL))];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_encoder_set_viewport(MetalHandle *receiver, MTLViewport arg0, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> object = checked_object(receiver, KIND_RENDER_ENCODER);
      [object setViewport:MTLViewport{arg0.originX, arg0.originY, arg0.width, arg0.height, arg0.znear, arg0.zfar}];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_encoder_set_scissor(MetalHandle *receiver, MTLScissorRect arg0, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> object = checked_object(receiver, KIND_RENDER_ENCODER);
      [object setScissorRect:MTLScissorRect{arg0.x, arg0.y, arg0.width, arg0.height}];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_encoder_set_cull_mode(MetalHandle *receiver, int64_t arg0, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> object = checked_object(receiver, KIND_RENDER_ENCODER);
      [object setCullMode:static_cast<MTLCullMode>(arg0)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_encoder_set_winding(MetalHandle *receiver, int64_t arg0, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> object = checked_object(receiver, KIND_RENDER_ENCODER);
      [object setFrontFacingWinding:static_cast<MTLWinding>(arg0)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_encoder_draw_primitives(MetalHandle *receiver, int64_t arg0, uint64_t arg1, uint64_t arg2, uint64_t arg3, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> object = checked_object(receiver, KIND_RENDER_ENCODER);
      [object drawPrimitives:static_cast<MTLPrimitiveType>(arg0) vertexStart:static_cast<NSUInteger>(arg1) vertexCount:static_cast<NSUInteger>(arg2) instanceCount:static_cast<NSUInteger>(arg3)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_draw_indexed_basic(MetalHandle *receiver, int64_t arg0, uint64_t arg1, int64_t arg2, MetalHandle * arg3, uint64_t arg4, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> object = checked_object(receiver, KIND_RENDER_ENCODER);
      [object drawIndexedPrimitives:static_cast<MTLPrimitiveType>(arg0) indexCount:static_cast<NSUInteger>(arg1) indexType:static_cast<MTLIndexType>(arg2) indexBuffer:checked_object(arg3, KIND_BUFFER) indexBufferOffset:static_cast<NSUInteger>(arg4)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_draw_indexed_instances(MetalHandle *receiver, int64_t arg0, uint64_t arg1, int64_t arg2, MetalHandle * arg3, uint64_t arg4, uint64_t arg5, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> object = checked_object(receiver, KIND_RENDER_ENCODER);
      [object drawIndexedPrimitives:static_cast<MTLPrimitiveType>(arg0) indexCount:static_cast<NSUInteger>(arg1) indexType:static_cast<MTLIndexType>(arg2) indexBuffer:checked_object(arg3, KIND_BUFFER) indexBufferOffset:static_cast<NSUInteger>(arg4) instanceCount:static_cast<NSUInteger>(arg5)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_draw_indexed_indirect(MetalHandle *receiver, int64_t arg0, int64_t arg1, MetalHandle * arg2, uint64_t arg3, MetalHandle * arg4, uint64_t arg5, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> object = checked_object(receiver, KIND_RENDER_ENCODER);
      [object drawIndexedPrimitives:static_cast<MTLPrimitiveType>(arg0) indexType:static_cast<MTLIndexType>(arg1) indexBuffer:checked_object(arg2, KIND_BUFFER) indexBufferOffset:static_cast<NSUInteger>(arg3) indirectBuffer:checked_object(arg4, KIND_BUFFER) indirectBufferOffset:static_cast<NSUInteger>(arg5)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_buffer_create(MetalHandle *receiver, uint64_t arg0, int64_t arg1, MetalHandle * *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      const id native_result = [object newBufferWithLength:static_cast<NSUInteger>(arg0) options:static_cast<MTLResourceOptions>(arg1)];
      if (native_result == nil) return set_error(message, "Metal returned nil"); *out = make_handle(native_result, KIND_BUFFER);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_buffer_info_length(MetalHandle *receiver, uint64_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLBuffer> object = checked_object(receiver, KIND_BUFFER);
      const NSUInteger native_result = object.length;
      *out = static_cast<uint64_t>(native_result);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_buffer_info_storageMode(MetalHandle *receiver, int64_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLBuffer> object = checked_object(receiver, KIND_BUFFER);
      const MTLStorageMode native_result = object.storageMode;
      *out = static_cast<int64_t>(native_result);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_buffer_info_cpuCacheMode(MetalHandle *receiver, int64_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLBuffer> object = checked_object(receiver, KIND_BUFFER);
      const MTLCPUCacheMode native_result = object.cpuCacheMode;
      *out = static_cast<int64_t>(native_result);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_buffer_info_hazardTrackingMode(MetalHandle *receiver, int64_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLBuffer> object = checked_object(receiver, KIND_BUFFER);
      const MTLHazardTrackingMode native_result = object.hazardTrackingMode;
      *out = static_cast<int64_t>(native_result);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_set_buffer_label(MetalHandle *receiver, const char * arg0, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      if (arg0 == nullptr) return set_error(message, "null UTF-8 input"); NSString *text0 = [NSString stringWithUTF8String:arg0]; if (text0 == nil) return set_error(message, "invalid UTF-8 input");
      id<MTLBuffer> object = checked_object(receiver, KIND_BUFFER);
      object.label = text0;
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_texture_is_shareable(MetalHandle *receiver, uint8_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLTexture> object = checked_object(receiver, KIND_TEXTURE);
      const BOOL native_result = object.shareable;
      *out = native_result ? 1 : 0;
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_indirect_render_command(MetalHandle *receiver, uint64_t arg0, MetalHandle * *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLIndirectCommandBuffer> object = checked_object(receiver, KIND_INDIRECT_COMMAND_BUFFER);
      const id native_result = [object indirectRenderCommandAtIndex:static_cast<NSUInteger>(arg0)];
      if (native_result == nil) return set_error(message, "Metal returned nil"); *out = make_handle(native_result, KIND_INDIRECT_RENDER_COMMAND);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_indirect_render_command_set_pipeline(MetalHandle *receiver, MetalHandle * arg0, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLIndirectRenderCommand> object = checked_object(receiver, KIND_INDIRECT_RENDER_COMMAND);
      [object setRenderPipelineState:checked_object(arg0, KIND_RENDER_PIPELINE)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_indirect_render_command_set_vertex_buffer(MetalHandle *receiver, MetalHandle * arg0, uint64_t arg1, uintptr_t arg2, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLIndirectRenderCommand> object = checked_object(receiver, KIND_INDIRECT_RENDER_COMMAND);
      [object setVertexBuffer:checked_object(arg0, KIND_BUFFER) offset:static_cast<NSUInteger>(arg1) atIndex:static_cast<NSUInteger>(arg2)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_indirect_render_command_set_fragment_buffer(MetalHandle *receiver, MetalHandle * arg0, uint64_t arg1, uintptr_t arg2, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLIndirectRenderCommand> object = checked_object(receiver, KIND_INDIRECT_RENDER_COMMAND);
      [object setFragmentBuffer:checked_object(arg0, KIND_BUFFER) offset:static_cast<NSUInteger>(arg1) atIndex:static_cast<NSUInteger>(arg2)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_indirect_render_command_draw_primitives(MetalHandle *receiver, int64_t arg0, uint64_t arg1, uint64_t arg2, uint64_t arg3, uint64_t arg4, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLIndirectRenderCommand> object = checked_object(receiver, KIND_INDIRECT_RENDER_COMMAND);
      [object drawPrimitives:static_cast<MTLPrimitiveType>(arg0) vertexStart:static_cast<NSUInteger>(arg1) vertexCount:static_cast<NSUInteger>(arg2) instanceCount:static_cast<NSUInteger>(arg3) baseInstance:static_cast<NSUInteger>(arg4)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_function_kind(MetalHandle *receiver, int64_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLFunction> object = checked_object(receiver, KIND_FUNCTION);
      const MTLFunctionType native_result = object.functionType;
      *out = static_cast<int64_t>(native_result);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_compute_pipeline_max_total_threads(MetalHandle *receiver, uint64_t *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLComputePipelineState> object = checked_object(receiver, KIND_COMPUTE_PIPELINE);
      const NSUInteger native_result = object.maxTotalThreadsPerThreadgroup;
      *out = static_cast<uint64_t>(native_result);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_acceleration_structure_create(MetalHandle *receiver, uint64_t arg0, MetalHandle * *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLDevice> object = checked_object(receiver, KIND_DEVICE);
      const id native_result = [object newAccelerationStructureWithSize:static_cast<NSUInteger>(arg0)];
      if (native_result == nil) return set_error(message, "Metal returned nil"); *out = make_handle(native_result, KIND_ACCELERATION_STRUCTURE);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_acceleration_encoder_copy_and_compact(MetalHandle *receiver, MetalHandle * arg0, MetalHandle * arg1, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLAccelerationStructureCommandEncoder> object = checked_object(receiver, KIND_ACCELERATION_ENCODER);
      [object copyAndCompactAccelerationStructure:checked_object(arg0, KIND_ACCELERATION_STRUCTURE) toAccelerationStructure:checked_object(arg1, KIND_ACCELERATION_STRUCTURE)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_layer_next_drawable(MetalHandle *receiver, MetalHandle * *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      CAMetalLayer * object = checked_object(receiver, KIND_METAL_LAYER);
      const id native_result = [object nextDrawable];
      *out = native_result == nil ? nullptr : make_handle(native_result, KIND_METAL_DRAWABLE);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_command_buffer_present(MetalHandle *receiver, MetalHandle * arg0, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLCommandBuffer> object = checked_object(receiver, KIND_COMMAND_BUFFER);
      [object presentDrawable:checked_object(arg0, KIND_METAL_DRAWABLE)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_command_buffer_present_at_time(MetalHandle *receiver, MetalHandle * arg0, double arg1, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLCommandBuffer> object = checked_object(receiver, KIND_COMMAND_BUFFER);
      [object presentDrawable:checked_object(arg0, KIND_METAL_DRAWABLE) atTime:arg1];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_command_buffer_present_after_duration(MetalHandle *receiver, MetalHandle * arg0, double arg1, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLCommandBuffer> object = checked_object(receiver, KIND_COMMAND_BUFFER);
      [object presentDrawable:checked_object(arg0, KIND_METAL_DRAWABLE) afterMinimumDuration:arg1];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_render_pass_descriptor_create(MetalHandle * *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      const id native_result = [MTLRenderPassDescriptor renderPassDescriptor];
      if (native_result == nil) return set_error(message, "Metal returned nil"); *out = make_handle(native_result, KIND_RENDER_PASS_DESCRIPTOR);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_compute_encoder_set_pipeline(MetalHandle *receiver, MetalHandle * arg0, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLComputeCommandEncoder> object = checked_object(receiver, KIND_COMPUTE_ENCODER);
      [object setComputePipelineState:checked_object(arg0, KIND_COMPUTE_PIPELINE)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_compute_encoder_set_buffer(MetalHandle *receiver, MetalHandle * arg0, uint64_t arg1, uintptr_t arg2, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLComputeCommandEncoder> object = checked_object(receiver, KIND_COMPUTE_ENCODER);
      [object setBuffer:checked_object(arg0, KIND_BUFFER) offset:static_cast<NSUInteger>(arg1) atIndex:static_cast<NSUInteger>(arg2)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_compute_encoder_set_texture(MetalHandle *receiver, MetalHandle * arg0, uintptr_t arg1, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLComputeCommandEncoder> object = checked_object(receiver, KIND_COMPUTE_ENCODER);
      [object setTexture:checked_object(arg0, KIND_TEXTURE) atIndex:static_cast<NSUInteger>(arg1)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_compute_encoder_dispatch(MetalHandle *receiver, MTLSize arg0, MTLSize arg1, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLComputeCommandEncoder> object = checked_object(receiver, KIND_COMPUTE_ENCODER);
      [object dispatchThreads:MTLSize{arg0.width, arg0.height, arg0.depth} threadsPerThreadgroup:MTLSize{arg1.width, arg1.height, arg1.depth}];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_compute_encoder_dispatch_indirect(MetalHandle *receiver, MetalHandle * arg0, uint64_t arg1, MTLSize arg2, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLComputeCommandEncoder> object = checked_object(receiver, KIND_COMPUTE_ENCODER);
      [object dispatchThreadgroupsWithIndirectBuffer:checked_object(arg0, KIND_BUFFER) indirectBufferOffset:static_cast<NSUInteger>(arg1) threadsPerThreadgroup:MTLSize{arg2.width, arg2.height, arg2.depth}];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_compute35_acceleration(MetalHandle *receiver, MetalHandle * arg0, uint64_t arg1, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLComputeCommandEncoder> object = checked_object(receiver, KIND_COMPUTE_ENCODER);
      [object setAccelerationStructure:(arg0 == nullptr ? nil : checked_object(arg0, KIND_ACCELERATION_STRUCTURE)) atBufferIndex:static_cast<NSUInteger>(arg1)];
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
extern "C" int32_t rays_metal_shader_function_argument_encoder(MetalHandle *receiver, uint64_t arg0, MetalHandle * *out, const char **message) {
  if (message) *message = nullptr;
  @try {
    @autoreleasepool {
      id<MTLFunction> object = checked_object(receiver, KIND_FUNCTION);
      const id native_result = [object newArgumentEncoderWithBufferIndex:static_cast<NSUInteger>(arg0)];
      if (native_result == nil) return set_error(message, "Metal returned nil"); *out = make_handle(native_result, KIND_SHADER_ARGUMENT_ENCODER);
      return 0;
    }
  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }
}
