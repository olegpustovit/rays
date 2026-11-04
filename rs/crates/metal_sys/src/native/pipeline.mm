#include "metal_handle.h"
#include "metal_kinds.h"
#include <Metal/Metal.h>
#include <algorithm>
#include <cstdint>
#include <cstring>
#include <limits>
#include <string>
#include <new>

struct RaysMetalColorAttachment {
  int32_t pixel_format;
  int32_t blending_state;
  int32_t source_rgb_blend_factor;
  int32_t destination_rgb_blend_factor;
  int32_t rgb_blend_operation;
  int32_t source_alpha_blend_factor;
  int32_t destination_alpha_blend_factor;
  int32_t alpha_blend_operation;
  int32_t write_mask;
};
struct RaysMetalRenderResource {
  MetalHandle *handle;
  int32_t kind;
};

struct RaysMetalPipelineBinding {
  char *name;
  int32_t binding_type;
  int32_t access;
  uint64_t index;
  uint8_t used;
  uint8_t argument;
  int64_t buffer_alignment;
  int64_t buffer_data_size;
  int32_t buffer_data_type;
  int32_t texture_type;
  int32_t texture_data_type;
  uint8_t depth;
  int64_t array_length;
  int64_t threadgroup_alignment;
  int64_t threadgroup_data_size;
  int64_t object_alignment;
  int64_t object_data_size;
  void *reflected_type;
};
static void free_pipeline_bindings(RaysMetalPipelineBinding *bindings, size_t count) {
  if (bindings == nullptr) return;
  for (size_t index = 0; index < count; ++index) {
    delete[] bindings[index].name;
    if (bindings[index].reflected_type != nullptr)
      (void)CFBridgingRelease(bindings[index].reflected_type);
  }
  delete[] bindings;
}

static uint64_t texture_slice_count(id<MTLTexture> texture) {
  switch (texture.textureType) {
  case MTLTextureType1DArray:
  case MTLTextureType2DArray:
  case MTLTextureType2DMultisampleArray:
    return texture.arrayLength;
  case MTLTextureTypeCube:
    return 6;
  case MTLTextureTypeCubeArray:
    return texture.arrayLength * 6;
  default:
    return 1;
  }
}

static id<MTLFunction> checked_render_function(id<MTLLibrary> library,
                                               NSString *name,
                                               MTLFunctionType expected,
                                               const char *stage,
                                               const char **message) {
  if (name == nil || name.length == 0 ||
      ![library.functionNames containsObject:name]) {
    rays_metal_set_error(message, (std::string("Metal render ") + stage +
                                   " function is absent from its library").c_str());
    return nil;
  }
  id<MTLFunction> function = [library newFunctionWithName:name];
  if (function == nil || function.functionType != expected ||
      function.device.registryID != library.device.registryID) {
    rays_metal_set_error(message, (std::string("Metal render ") + stage +
                                   " function has an incompatible stage or device").c_str());
    return nil;
  }
  return function;
}

API_AVAILABLE(macos(11.0))
static bool copy_bindings(NSArray<id<MTLBinding>> *source,
                          RaysMetalPipelineBinding **output,
                          size_t *output_count,
                          const char **message) {
  *output = nullptr;
  *output_count = 0;
  if (source.count > std::numeric_limits<size_t>::max() /
                          sizeof(RaysMetalPipelineBinding)) {
    rays_metal_set_error(message, "Metal reflection has too many bindings");
    return false;
  }
  auto *bindings = source.count == 0
                       ? nullptr
                       : new (std::nothrow) RaysMetalPipelineBinding[source.count]{};
  if (source.count != 0 && bindings == nullptr) {
    rays_metal_set_error(message, "unable to allocate Metal reflection bindings");
    return false;
  }
  for (NSUInteger index = 0; index < source.count; ++index) {
    id<MTLBinding> binding = source[index];
    if (binding.name == nil || binding.name.UTF8String == nullptr) {
      free_pipeline_bindings(bindings, index);
      return rays_metal_set_error(message, "Metal returned an invalid reflected binding");
    }
    const char *utf8 = binding.name.UTF8String;
    const size_t name_length = std::strlen(utf8);
    bindings[index].name = new (std::nothrow) char[name_length + 1];
    if (bindings[index].name == nullptr) {
      free_pipeline_bindings(bindings, index);
      return rays_metal_set_error(message, "unable to copy Metal reflection binding name");
    }
    std::memcpy(bindings[index].name, utf8, name_length + 1);
    bindings[index].binding_type = static_cast<int32_t>(binding.type);
    bindings[index].access = static_cast<int32_t>(binding.access);
    bindings[index].index = static_cast<uint64_t>(binding.index);
    bindings[index].used = binding.used ? 1 : 0;
    bindings[index].argument = binding.argument ? 1 : 0;
    switch (binding.type) {
    case MTLBindingTypeBuffer: {
      id<MTLBufferBinding> value = (id<MTLBufferBinding>)binding;
      if (value.bufferAlignment > static_cast<NSUInteger>(std::numeric_limits<int64_t>::max()) ||
          value.bufferDataSize > static_cast<NSUInteger>(std::numeric_limits<int64_t>::max())) {
        free_pipeline_bindings(bindings, index + 1);
        return rays_metal_set_error(message, "Metal buffer reflection exceeds signed bounds");
      }
      bindings[index].buffer_alignment = static_cast<int64_t>(value.bufferAlignment);
      bindings[index].buffer_data_size = static_cast<int64_t>(value.bufferDataSize);
      bindings[index].buffer_data_type = static_cast<int32_t>(value.bufferDataType);
      break;
    }
    case MTLBindingTypeThreadgroupMemory: {
      id<MTLThreadgroupBinding> value = (id<MTLThreadgroupBinding>)binding;
      if (value.threadgroupMemoryAlignment > static_cast<NSUInteger>(std::numeric_limits<int64_t>::max()) ||
          value.threadgroupMemoryDataSize > static_cast<NSUInteger>(std::numeric_limits<int64_t>::max())) {
        free_pipeline_bindings(bindings, index + 1);
        return rays_metal_set_error(message, "Metal threadgroup reflection exceeds signed bounds");
      }
      bindings[index].threadgroup_alignment = static_cast<int64_t>(value.threadgroupMemoryAlignment);
      bindings[index].threadgroup_data_size = static_cast<int64_t>(value.threadgroupMemoryDataSize);
      break;
    }
    case MTLBindingTypeTexture: {
      id<MTLTextureBinding> value = (id<MTLTextureBinding>)binding;
      if (value.arrayLength > static_cast<NSUInteger>(std::numeric_limits<int64_t>::max())) {
        free_pipeline_bindings(bindings, index + 1);
        return rays_metal_set_error(message, "Metal texture reflection exceeds signed bounds");
      }
      bindings[index].texture_type = static_cast<int32_t>(value.textureType);
      bindings[index].texture_data_type = static_cast<int32_t>(value.textureDataType);
      bindings[index].depth = value.depthTexture ? 1 : 0;
      bindings[index].array_length = static_cast<int64_t>(value.arrayLength);
      break;
    }
    default:
      break;
    }
    if (@available(macOS 14.0, *)) {
      if (binding.type == MTLBindingTypeObjectPayload) {
        id<MTLObjectPayloadBinding> value = (id<MTLObjectPayloadBinding>)binding;
        if (value.objectPayloadAlignment > static_cast<NSUInteger>(std::numeric_limits<int64_t>::max()) ||
            value.objectPayloadDataSize > static_cast<NSUInteger>(std::numeric_limits<int64_t>::max())) {
          free_pipeline_bindings(bindings, index + 1);
          return rays_metal_set_error(message, "Metal object-payload reflection exceeds signed bounds");
        }
        bindings[index].object_alignment = static_cast<int64_t>(value.objectPayloadAlignment);
        bindings[index].object_data_size = static_cast<int64_t>(value.objectPayloadDataSize);
      }
    }
    MTLType *root_type = nil;
    if (binding.type == MTLBindingTypeBuffer) {
      id<MTLBufferBinding> buffer = (id<MTLBufferBinding>)binding;
      root_type = buffer.bufferPointerType;
      if (root_type == nil) root_type = buffer.bufferStructType;
    }
    if (root_type != nil) bindings[index].reflected_type = (__bridge_retained void *)root_type;
  }
  *output = bindings;
  *output_count = static_cast<size_t>(source.count);
  return true;
}

extern "C" int32_t caml_rays_metal_compute_pipeline_create_descriptor(
    MetalHandle *device_handle, MetalHandle *function_handle, const char *raw_label,
    uint8_t wants_reflection, MetalHandle **output, RaysMetalPipelineBinding **bindings,
    size_t *binding_count, const char **message) {
  if (message) *message = nullptr;
  if (output) *output = nullptr;
  if (bindings) *bindings = nullptr;
  if (binding_count) *binding_count = 0;
  if (device_handle == nullptr || function_handle == nullptr || output == nullptr ||
      bindings == nullptr || binding_count == nullptr)
    return rays_metal_set_error(message, "invalid compute-pipeline arguments");
  @try {
    @autoreleasepool {
      id<MTLDevice> device = (__bridge id<MTLDevice>)
          rays_metal_handle_object(device_handle, KIND_DEVICE);
      id<MTLFunction> function = (__bridge id<MTLFunction>)
          rays_metal_handle_object(function_handle, KIND_FUNCTION);
      if (device == nil || function == nil ||
          device.registryID != function.device.registryID ||
          function.functionType != MTLFunctionTypeKernel)
        return rays_metal_set_error(message, "compute function and device are incompatible");
      NSString *label = raw_label == nullptr ? nil : [NSString stringWithUTF8String:raw_label];
      if (raw_label != nullptr && label == nil)
        return rays_metal_set_error(message, "compute-pipeline label is not valid UTF-8");
      MTLComputePipelineDescriptor *descriptor = [[MTLComputePipelineDescriptor alloc] init];
      descriptor.computeFunction = function;
      descriptor.label = label;
      MTLAutoreleasedComputePipelineReflection reflection = nil;
      NSError *error = nil;
      MTLPipelineOption options = wants_reflection
          ? (MTLPipelineOptionBindingInfo | MTLPipelineOptionBufferTypeInfo)
          : MTLPipelineOptionNone;
      id<MTLComputePipelineState> pipeline =
          [device newComputePipelineStateWithDescriptor:descriptor
                                                options:options
                                             reflection:&reflection
                                                  error:&error];
      if (pipeline == nil)
        return rays_metal_set_error(message, error.localizedDescription.UTF8String
            ?: "Metal compute-pipeline creation failed without NSError");
      if (pipeline.device.registryID != device.registryID ||
          pipeline.maxTotalThreadsPerThreadgroup == 0 ||
          ((label == nil) != (pipeline.label == nil)) ||
          (label != nil && ![label isEqualToString:pipeline.label]))
        return rays_metal_set_error(message, "Metal changed checked compute-pipeline properties");
      if (wants_reflection) {
        if (@available(macOS 11.0, *)) {
          if (reflection == nil)
            return rays_metal_set_error(message, "Metal returned no compute-pipeline reflection");
          if (!copy_bindings(reflection.bindings, bindings, binding_count, message)) return 1;
        } else {
          return rays_metal_set_unavailable(message, "compute-pipeline reflection requires macOS 11");
        }
      }
      *output = rays_metal_handle_new((__bridge void *)pipeline, KIND_COMPUTE_PIPELINE);
      if (*output == nullptr) {
        free_pipeline_bindings(*bindings, *binding_count);
        *bindings = nullptr;
        *binding_count = 0;
        return rays_metal_set_error(message, "unable to retain compute pipeline");
      }
      return 0;
    }
  } @catch (NSException *exception) {
    free_pipeline_bindings(*bindings, *binding_count);
    *bindings = nullptr;
    *binding_count = 0;
    return rays_metal_set_error(message, exception.reason.UTF8String);
  }
}

extern "C" int32_t caml_rays_metal_compiler_create(
    MetalHandle *device_handle, MetalHandle **output, const char **message) {
  if (message) *message = nullptr;
  if (output) *output = nullptr;
  if (device_handle == nullptr || output == nullptr)
    return rays_metal_set_error(message, "invalid Metal 4 compiler arguments");
  @try {
    @autoreleasepool {
      id<MTLDevice> device = (__bridge id<MTLDevice>)
          rays_metal_handle_object(device_handle, KIND_DEVICE);
      if (device == nil) return rays_metal_set_error(message, "invalid Metal device handle");
      if (@available(macOS 26.0, *)) {
        if (![device supportsFamily:MTLGPUFamilyMetal4])
          return rays_metal_set_unavailable(message, "Metal 4 compilers are unsupported by this device");
        MTL4CompilerDescriptor *descriptor = [[MTL4CompilerDescriptor alloc] init];
        NSError *error = nil;
        id<MTL4Compiler> compiler = [device newCompilerWithDescriptor:descriptor error:&error];
        if (compiler == nil)
          return rays_metal_set_error(message, error.localizedDescription.UTF8String ?: "Metal compiler creation failed without NSError");
        if (compiler.device.registryID != device.registryID)
          return rays_metal_set_error(message, "Metal changed the compiler device");
        *output = rays_metal_handle_new((__bridge void *)compiler, KIND_COMPILER);
        return *output ? 0 : rays_metal_set_error(message, "unable to retain Metal compiler");
      }
      return rays_metal_set_unavailable(message, "Metal 4 compilers require macOS 26 or newer");
    }
  } @catch (NSException *exception) {
    return rays_metal_set_error(message, exception.reason.UTF8String);
  }
}

extern "C" int32_t caml_rays_metal_compiler_create_render_pipeline(
    MetalHandle *compiler_handle, MetalHandle *library_handle,
    const char *raw_label, const char *raw_vertex, const char *raw_fragment,
    uint8_t reflection, uint64_t raw_sample_count,
    const RaysMetalColorAttachment *attachments, size_t attachment_count,
    int32_t raw_topology, uint8_t support_indirect_commands,
    MetalHandle **output, const char **message) {
  if (message) *message = nullptr;
  if (output) *output = nullptr;
  if (compiler_handle == nullptr || library_handle == nullptr || raw_vertex == nullptr ||
      output == nullptr || (attachment_count != 0 && attachments == nullptr) ||
      attachment_count > 8 || raw_sample_count == 0 || raw_sample_count > std::numeric_limits<NSUInteger>::max() ||
      raw_topology < 1 || raw_topology > 3)
    return rays_metal_set_error(message, "invalid Metal 4 render-pipeline arguments");
  @try {
    @autoreleasepool {
      id<MTL4Compiler> compiler = (__bridge id<MTL4Compiler>)
          rays_metal_handle_object(compiler_handle, KIND_COMPILER);
      id<MTLLibrary> library = (__bridge id<MTLLibrary>)
          rays_metal_handle_object(library_handle, KIND_LIBRARY);
      if (compiler == nil || library == nil ||
          compiler.device.registryID != library.device.registryID)
        return rays_metal_set_error(message, "invalid or incompatible Metal compiler/library");
      if (@available(macOS 26.0, *)) {
        NSString *label = raw_label == nullptr ? nil : [NSString stringWithUTF8String:raw_label];
        NSString *vertex_name = [NSString stringWithUTF8String:raw_vertex];
        NSString *fragment_name = raw_fragment == nullptr ? nil : [NSString stringWithUTF8String:raw_fragment];
        if ((raw_label != nullptr && label == nil) || vertex_name == nil ||
            (raw_fragment != nullptr && fragment_name == nil))
          return rays_metal_set_error(message, "Metal render-pipeline strings are not valid UTF-8");
        id<MTLFunction> vertex_function = checked_render_function(library, vertex_name, MTLFunctionTypeVertex, "vertex", message);
        if (vertex_function == nil) return 1;
        id<MTLFunction> fragment_function = nil;
        if (fragment_name != nil) {
          fragment_function = checked_render_function(library, fragment_name, MTLFunctionTypeFragment, "fragment", message);
          if (fragment_function == nil) return 1;
        }
        if ((fragment_function != nil) != (attachment_count != 0))
          return rays_metal_set_error(message, "render rasterization requires a fragment function and color attachments");
        if (raw_sample_count > static_cast<uint64_t>(std::numeric_limits<NSUInteger>::max()) ||
            ![compiler.device supportsTextureSampleCount:static_cast<NSUInteger>(raw_sample_count)])
          return rays_metal_set_error(message, "Metal render sample count is unsupported");
        MTL4RenderPipelineDescriptor *descriptor = [[MTL4RenderPipelineDescriptor alloc] init];
        descriptor.label = label;
        MTL4LibraryFunctionDescriptor *vertex = [[MTL4LibraryFunctionDescriptor alloc] init];
        vertex.library = library;
        vertex.name = vertex_name;
        descriptor.vertexFunctionDescriptor = vertex;
        MTL4LibraryFunctionDescriptor *fragment = nil;
        if (fragment_function != nil) {
          fragment = [[MTL4LibraryFunctionDescriptor alloc] init];
          fragment.library = library;
          fragment.name = fragment_name;
          descriptor.fragmentFunctionDescriptor = fragment;
        }
        descriptor.rasterSampleCount = static_cast<NSUInteger>(raw_sample_count);
        descriptor.rasterizationEnabled = true;
        descriptor.inputPrimitiveTopology = static_cast<MTLPrimitiveTopologyClass>(raw_topology);
        descriptor.supportIndirectCommandBuffers = support_indirect_commands
            ? MTL4IndirectCommandBufferSupportStateEnabled
            : MTL4IndirectCommandBufferSupportStateDisabled;
        if (reflection) {
          MTL4PipelineOptions *options = [[MTL4PipelineOptions alloc] init];
          options.shaderReflection = MTL4ShaderReflectionBindingInfo | MTL4ShaderReflectionBufferTypeInfo;
          descriptor.options = options;
        }
        for (NSUInteger index = 0; index < attachment_count; ++index) {
          const RaysMetalColorAttachment &source = attachments[index];
          if (source.pixel_format == MTLPixelFormatInvalid || source.pixel_format == MTLPixelFormatDepth32Float ||
              source.blending_state < 0 || source.blending_state > 1 || source.source_rgb_blend_factor < 0 || source.source_rgb_blend_factor > 18 ||
              source.destination_rgb_blend_factor < 0 || source.destination_rgb_blend_factor > 18 || source.rgb_blend_operation < 0 || source.rgb_blend_operation > 4 ||
              source.source_alpha_blend_factor < 0 || source.source_alpha_blend_factor > 18 || source.destination_alpha_blend_factor < 0 || source.destination_alpha_blend_factor > 18 ||
              source.alpha_blend_operation < 0 || source.alpha_blend_operation > 4 || source.write_mask < 0 || (source.write_mask & ~15) != 0)
            return rays_metal_set_error(message, "invalid Metal 4 color attachment descriptor");
          MTL4RenderPipelineColorAttachmentDescriptor *attachment = [[MTL4RenderPipelineColorAttachmentDescriptor alloc] init];
          attachment.pixelFormat = static_cast<MTLPixelFormat>(source.pixel_format);
          attachment.blendingState = static_cast<MTL4BlendState>(source.blending_state);
          attachment.sourceRGBBlendFactor = static_cast<MTLBlendFactor>(source.source_rgb_blend_factor);
          attachment.destinationRGBBlendFactor = static_cast<MTLBlendFactor>(source.destination_rgb_blend_factor);
          attachment.rgbBlendOperation = static_cast<MTLBlendOperation>(source.rgb_blend_operation);
          attachment.sourceAlphaBlendFactor = static_cast<MTLBlendFactor>(source.source_alpha_blend_factor);
          attachment.destinationAlphaBlendFactor = static_cast<MTLBlendFactor>(source.destination_alpha_blend_factor);
          attachment.alphaBlendOperation = static_cast<MTLBlendOperation>(source.alpha_blend_operation);
          attachment.writeMask = static_cast<MTLColorWriteMask>(source.write_mask);
          descriptor.colorAttachments[index] = attachment;
        }
        NSError *error = nil;
        id<MTLRenderPipelineState> pipeline = [compiler newRenderPipelineStateWithDescriptor:descriptor compilerTaskOptions:nil error:&error];
        if (pipeline == nil)
          return rays_metal_set_error(message, error.localizedDescription.UTF8String ?: "Metal render-pipeline compilation failed without NSError");
        if (pipeline.device.registryID != compiler.device.registryID ||
            ((label == nil) != (pipeline.label == nil)) || (label != nil && ![pipeline.label isEqualToString:label]) ||
            (reflection && pipeline.reflection == nil))
          return rays_metal_set_error(message, "Metal changed checked render-pipeline properties");
        *output = rays_metal_handle_new((__bridge void *)pipeline, KIND_RENDER_PIPELINE);
        return *output ? 0 : rays_metal_set_error(message, "unable to retain Metal render pipeline");
      }
      return rays_metal_set_unavailable(message, "Metal 4 render pipelines require macOS 26 or newer");
    }
  } @catch (NSException *exception) {
    return rays_metal_set_error(message, exception.reason.UTF8String);
  }
}

extern "C" int32_t caml_rays_metal_render_pipeline_reflection(
    MetalHandle *pipeline_handle, RaysMetalPipelineBinding **vertex,
    size_t *vertex_count, RaysMetalPipelineBinding **fragment,
    size_t *fragment_count, const char **message) {
  if (message) *message = nullptr;
  if (vertex) *vertex = nullptr;
  if (vertex_count) *vertex_count = 0;
  if (fragment) *fragment = nullptr;
  if (fragment_count) *fragment_count = 0;
  if (pipeline_handle == nullptr || vertex == nullptr || vertex_count == nullptr ||
      fragment == nullptr || fragment_count == nullptr)
    return rays_metal_set_error(message, "invalid render-pipeline reflection arguments");
  @try {
    @autoreleasepool {
      id<MTLRenderPipelineState> pipeline = (__bridge id<MTLRenderPipelineState>)
          rays_metal_handle_object(pipeline_handle, KIND_RENDER_PIPELINE);
      if (pipeline == nil || pipeline.reflection == nil)
        return rays_metal_set_error(message, "render pipeline has no requested reflection");
      if (@available(macOS 26.0, *)) {
        if (!copy_bindings(pipeline.reflection.vertexBindings, vertex, vertex_count, message)) return 1;
        if (!copy_bindings(pipeline.reflection.fragmentBindings, fragment, fragment_count, message)) {
          free_pipeline_bindings(*vertex, *vertex_count);
          *vertex = nullptr;
          *vertex_count = 0;
          return 1;
        }
        return 0;
      }
      return rays_metal_set_unavailable(message, "Metal render-pipeline reflection requires macOS 26 or newer");
    }
  } @catch (NSException *exception) {
    if (vertex && *vertex) {
      free_pipeline_bindings(*vertex, *vertex_count);
      *vertex = nullptr;
      *vertex_count = 0;
    }
    if (fragment && *fragment) {
      free_pipeline_bindings(*fragment, *fragment_count);
      *fragment = nullptr;
      *fragment_count = 0;
    }
    return rays_metal_set_error(message, exception.reason.UTF8String);
  }
}

extern "C" void caml_rays_metal_pipeline_reflection_free(
    RaysMetalPipelineBinding *bindings, size_t count) {
  if (bindings == nullptr) return;
  free_pipeline_bindings(bindings, count);
}

extern "C" int32_t caml_rays_metal_blit_encoder_copy_buffer_to_texture(
    MetalHandle *encoder_handle, MetalHandle *source_handle,
    uint64_t source_offset, uint64_t source_bytes_per_row,
    uint64_t source_bytes_per_image, MetalHandle *destination_handle,
    uint64_t destination_slice, uint64_t destination_level,
    uint64_t x, uint64_t y, uint64_t z, uint64_t width, uint64_t height,
    uint64_t depth, const char **message) {
  if (message) *message = nullptr;
  if (source_bytes_per_row == 0 || source_bytes_per_image == 0 || width == 0 ||
      height == 0 || depth == 0)
    return rays_metal_set_error(message, "invalid buffer-to-texture blit dimensions or pitch");
  @try {
    @autoreleasepool {
      id<MTLBlitCommandEncoder> encoder = (__bridge id<MTLBlitCommandEncoder>)
          rays_metal_handle_object(encoder_handle, KIND_BLIT_ENCODER);
      id<MTLBuffer> source = (__bridge id<MTLBuffer>)
          rays_metal_handle_object(source_handle, KIND_BUFFER);
      id<MTLTexture> destination = (__bridge id<MTLTexture>)
          rays_metal_handle_object(destination_handle, KIND_TEXTURE);
      if (encoder == nil || source == nil || destination == nil ||
          source.device.registryID != destination.device.registryID ||
          source_offset > source.length || destination.sampleCount != 1 ||
          destination.storageMode == MTLStorageModeMemoryless ||
          destination_slice >= texture_slice_count(destination) ||
          destination_level >= destination.mipmapLevelCount ||
          x > std::numeric_limits<NSUInteger>::max() || y > std::numeric_limits<NSUInteger>::max() ||
          z > std::numeric_limits<NSUInteger>::max() || width > std::numeric_limits<NSUInteger>::max() ||
          height > std::numeric_limits<NSUInteger>::max() || depth > std::numeric_limits<NSUInteger>::max() ||
          source_bytes_per_row > std::numeric_limits<NSUInteger>::max() ||
          source_bytes_per_image > std::numeric_limits<NSUInteger>::max())
        return rays_metal_set_error(message, "buffer-to-texture blit handles or range are invalid");
      const NSUInteger copy_width = static_cast<NSUInteger>(width);
      const NSUInteger copy_height = static_cast<NSUInteger>(height);
      const NSUInteger copy_depth = static_cast<NSUInteger>(depth);
      const NSUInteger destination_x = static_cast<NSUInteger>(x);
      const NSUInteger destination_y = static_cast<NSUInteger>(y);
      const NSUInteger destination_z = static_cast<NSUInteger>(z);
      const NSUInteger row_pitch = static_cast<NSUInteger>(source_bytes_per_row);
      const NSUInteger image_pitch = static_cast<NSUInteger>(source_bytes_per_image);
      const NSUInteger level = static_cast<NSUInteger>(destination_level);
      const NSUInteger mip_width = std::max<NSUInteger>(1, destination.width >> level);
      const NSUInteger mip_height = std::max<NSUInteger>(1, destination.height >> level);
      const NSUInteger mip_depth = std::max<NSUInteger>(1, destination.depth >> level);
      NSUInteger block_width = 1;
      NSUInteger block_height = 1;
      NSUInteger bytes_per_block = 0;
      switch (destination.pixelFormat) {
      case MTLPixelFormatR8Unorm: bytes_per_block = 1; break;
      case MTLPixelFormatRGBA8Unorm:
      case MTLPixelFormatBGRA8Unorm:
      case MTLPixelFormatBGRA8Unorm_sRGB:
      case MTLPixelFormatDepth32Float: bytes_per_block = 4; break;
      case MTLPixelFormatRGBA16Float: bytes_per_block = 8; break;
      case MTLPixelFormatRGBA32Float: bytes_per_block = 16; break;
      default: return rays_metal_set_error(message, "buffer-to-texture blit format is unsupported by the binding");
      }
      const NSUInteger row_blocks = copy_width / block_width + (copy_width % block_width != 0);
      const NSUInteger image_rows = copy_height / block_height + (copy_height % block_height != 0);
      if (destination_x % block_width != 0 || destination_y % block_height != 0 ||
          destination_x > mip_width || copy_width > mip_width - destination_x ||
          destination_y > mip_height || copy_height > mip_height - destination_y ||
          destination_z > mip_depth || copy_depth > mip_depth - destination_z ||
          row_blocks > std::numeric_limits<NSUInteger>::max() / bytes_per_block ||
          row_pitch < row_blocks * bytes_per_block || row_pitch % bytes_per_block != 0 ||
          image_rows != 0 && row_pitch > std::numeric_limits<NSUInteger>::max() / image_rows ||
          image_pitch < row_pitch * image_rows ||
          copy_depth != 0 && image_pitch > std::numeric_limits<NSUInteger>::max() / copy_depth)
        return rays_metal_set_error(message, "buffer-to-texture blit range or row pitch is invalid");
      const NSUInteger required = image_pitch * copy_depth;
      if (required > source.length - source_offset)
        return rays_metal_set_error(message, "buffer-to-texture blit exceeds the source buffer");
      [encoder copyFromBuffer:source
                 sourceOffset:static_cast<NSUInteger>(source_offset)
            sourceBytesPerRow:row_pitch
          sourceBytesPerImage:image_pitch
                   sourceSize:MTLSizeMake(copy_width, copy_height, copy_depth)
                    toTexture:destination
             destinationSlice:static_cast<NSUInteger>(destination_slice)
             destinationLevel:level
            destinationOrigin:MTLOriginMake(destination_x, destination_y, destination_z)];
      return 0;
    }
  } @catch (NSException *exception) {
    return rays_metal_set_error(message, exception.reason.UTF8String);
  }
}
extern "C" int32_t caml_rays_metal_render_encoder_use_resources(
    MetalHandle *encoder_handle, const RaysMetalRenderResource *resources,
    size_t count, int32_t raw_usage, int32_t raw_stages,
    const char **message) {
  if (message) *message = nullptr;
  if (encoder_handle == nullptr || resources == nullptr || count == 0 ||
      raw_usage <= 0 || (raw_usage & ~7) != 0 ||
      raw_stages <= 0 || (raw_stages & ~3) != 0)
    return rays_metal_set_error(message, "invalid render resource-use arguments");
  @try {
    @autoreleasepool {
      id<MTLRenderCommandEncoder> encoder = (__bridge id<MTLRenderCommandEncoder>)
          rays_metal_handle_object(encoder_handle, KIND_RENDER_ENCODER);
      if (encoder == nil)
        return rays_metal_set_error(message, "invalid Metal render encoder handle");
      for (size_t index = 0; index < count; ++index) {
        id<MTLResource> resource = nil;
        switch (resources[index].kind) {
        case 0:
          resource = (__bridge id<MTLBuffer>)rays_metal_handle_object(resources[index].handle, KIND_BUFFER);
          break;
        case 1:
          resource = (__bridge id<MTLTexture>)rays_metal_handle_object(resources[index].handle, KIND_TEXTURE);
          break;
        default:
          return rays_metal_set_error(message, "unknown render resource kind");
        }
        if (resource == nil || resource.device.registryID != encoder.device.registryID)
          return rays_metal_set_error(message, "render resource is invalid or belongs to another device");
        if (@available(macOS 10.15, *)) {
          [encoder useResource:resource
                         usage:static_cast<MTLResourceUsage>(raw_usage)
                        stages:static_cast<MTLRenderStages>(raw_stages)];
        } else {
          return rays_metal_set_unavailable(message, "render resource use requires macOS 10.15 or newer");
        }
      }
      return 0;
    }
  } @catch (NSException *exception) {
    return rays_metal_set_error(message, exception.reason.UTF8String);
  }
}
