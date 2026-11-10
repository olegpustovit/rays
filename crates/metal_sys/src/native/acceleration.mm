#import <Foundation/Foundation.h>
#import <Metal/Metal.h>

#include "metal_handle.h"
#include "metal_kinds.h"
#include <cstdint>
#include <limits>

struct RaysAccelKeyframe {
  MetalHandle *buffer;
  uint64_t offset;
};

struct RaysAccelGeometry {
  int32_t kind;
  MetalHandle *buffer;
  uint64_t offset;
  uint64_t stride;
  uint64_t count;
  MetalHandle *secondary_buffer;
  uint64_t secondary_offset;
  uint64_t secondary_stride;
  MetalHandle *index_buffer;
  uint64_t index_offset;
  uint8_t index_uint16;
  uint64_t segment_count;
  uint32_t segment_control_points;
  int32_t curve_type;
  int32_t basis;
  int32_t end_caps;
  const RaysAccelKeyframe *keyframes;
  uintptr_t keyframe_count;
  const RaysAccelKeyframe *secondary_keyframes;
  uintptr_t secondary_keyframe_count;
  uint8_t opaque;
  uint8_t allow_duplicate_intersection;
};

struct RaysAccelMotion {
  uint64_t keyframe_count;
  double start_time;
  double end_time;
  int32_t start_border;
  int32_t end_border;
};

struct RaysAccelPrimitiveDescriptor {
  const RaysAccelGeometry *geometries;
  uintptr_t geometry_count;
  uint8_t has_motion;
  RaysAccelMotion motion;
  uint8_t refit;
  uint8_t fast_build;
};

struct RaysAccelInstancesDescriptor {
  MetalHandle *buffer;
  uint64_t offset;
  uint64_t stride;
  uint64_t count;
  int32_t instance_kind;
  MetalHandle *const *structures;
  uintptr_t structure_count;
  uint8_t has_motion_transforms;
  MetalHandle *motion_transform_buffer;
  uint64_t motion_transform_offset;
  uint64_t motion_transform_count;
  uint8_t refit;
};

struct RaysAccelBuildDescriptor {
  int32_t kind;
  const RaysAccelPrimitiveDescriptor *primitive;
  const RaysAccelInstancesDescriptor *instances;
};

struct RaysAccelSizes {
  uint64_t acceleration_structure_size;
  uint64_t build_scratch_buffer_size;
  uint64_t refit_scratch_buffer_size;
};

struct RaysAccelInstanceLayout {
  int64_t values[12];
};

static id<MTLBuffer> rays_accel_buffer(MetalHandle *handle) {
  return (__bridge id<MTLBuffer>)rays_metal_handle_object(handle, KIND_BUFFER);
}

static NSArray<MTLMotionKeyframeData *> *rays_accel_keyframes(
    const RaysAccelKeyframe *values, uintptr_t count) API_AVAILABLE(macos(12.0)) {
  if (count == 0 || values == nullptr) return nil;
  NSMutableArray<MTLMotionKeyframeData *> *frames =
      [NSMutableArray arrayWithCapacity:count];
  for (uintptr_t i = 0; i < count; ++i) {
    id<MTLBuffer> buffer = rays_accel_buffer(values[i].buffer);
    if (buffer == nil) return nil;
    MTLMotionKeyframeData *frame = [MTLMotionKeyframeData data];
    frame.buffer = buffer;
    frame.offset = static_cast<NSUInteger>(values[i].offset);
    [frames addObject:frame];
  }
  return frames;
}

static void rays_accel_set_common(
    MTLAccelerationStructureGeometryDescriptor *geometry,
    const RaysAccelGeometry *source) API_AVAILABLE(macos(11.0)) {
  geometry.opaque = source->opaque != 0;
  geometry.allowDuplicateIntersectionFunctionInvocation =
      source->allow_duplicate_intersection != 0;
}

static MTLAccelerationStructureGeometryDescriptor *rays_accel_geometry(
    const RaysAccelGeometry *source, const char **message, bool *unavailable)
    API_AVAILABLE(macos(11.0)) {
  if (source == nullptr) {
    rays_metal_set_error(message, "null acceleration geometry");
    return nil;
  }
  id<MTLBuffer> buffer = rays_accel_buffer(source->buffer);
  if (buffer == nil) {
    rays_metal_set_error(message, "invalid acceleration geometry buffer");
    return nil;
  }

  switch (source->kind) {
  case 0: {
    if (source->keyframe_count == 0) {
      MTLAccelerationStructureTriangleGeometryDescriptor *geometry =
          [MTLAccelerationStructureTriangleGeometryDescriptor descriptor];
      geometry.vertexBuffer = buffer;
      geometry.vertexBufferOffset = static_cast<NSUInteger>(source->offset);
      geometry.vertexStride = static_cast<NSUInteger>(source->stride);
      geometry.triangleCount = static_cast<NSUInteger>(source->count);
      if (source->index_buffer != nullptr) {
        id<MTLBuffer> index = rays_accel_buffer(source->index_buffer);
        if (index == nil) {
          rays_metal_set_error(message, "invalid triangle index buffer");
          return nil;
        }
        geometry.indexBuffer = index;
        geometry.indexBufferOffset = static_cast<NSUInteger>(source->index_offset);
        geometry.indexType = source->index_uint16 ? MTLIndexTypeUInt16 : MTLIndexTypeUInt32;
      }
      rays_accel_set_common(geometry, source);
      return geometry;
    }
    if (@available(macOS 12.0, *)) {
      NSArray<MTLMotionKeyframeData *> *frames =
          rays_accel_keyframes(source->keyframes, source->keyframe_count);
      if (frames == nil) {
        rays_metal_set_error(message, "invalid triangle motion keyframes");
        return nil;
      }
      MTLAccelerationStructureMotionTriangleGeometryDescriptor *geometry =
          [MTLAccelerationStructureMotionTriangleGeometryDescriptor descriptor];
      geometry.vertexBuffers = frames;
      geometry.vertexStride = static_cast<NSUInteger>(source->stride);
      geometry.triangleCount = static_cast<NSUInteger>(source->count);
      if (source->index_buffer != nullptr) {
        id<MTLBuffer> index = rays_accel_buffer(source->index_buffer);
        if (index == nil) {
          rays_metal_set_error(message, "invalid triangle index buffer");
          return nil;
        }
        geometry.indexBuffer = index;
        geometry.indexBufferOffset = static_cast<NSUInteger>(source->index_offset);
        geometry.indexType = source->index_uint16 ? MTLIndexTypeUInt16 : MTLIndexTypeUInt32;
      }
      rays_accel_set_common(geometry, source);
      return geometry;
    }
    *unavailable = true;
    rays_metal_set_unavailable(message, "motion acceleration structures require macOS 12");
    return nil;
  }
  case 1: {
    if (source->keyframe_count == 0) {
      MTLAccelerationStructureBoundingBoxGeometryDescriptor *geometry =
          [MTLAccelerationStructureBoundingBoxGeometryDescriptor descriptor];
      geometry.boundingBoxBuffer = buffer;
      geometry.boundingBoxBufferOffset = static_cast<NSUInteger>(source->offset);
      geometry.boundingBoxStride = static_cast<NSUInteger>(source->stride);
      geometry.boundingBoxCount = static_cast<NSUInteger>(source->count);
      rays_accel_set_common(geometry, source);
      return geometry;
    }
    if (@available(macOS 12.0, *)) {
      NSArray<MTLMotionKeyframeData *> *frames =
          rays_accel_keyframes(source->keyframes, source->keyframe_count);
      if (frames == nil) {
        rays_metal_set_error(message, "invalid bounding-box motion keyframes");
        return nil;
      }
      MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor *geometry =
          [MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor descriptor];
      geometry.boundingBoxBuffers = frames;
      geometry.boundingBoxStride = static_cast<NSUInteger>(source->stride);
      geometry.boundingBoxCount = static_cast<NSUInteger>(source->count);
      rays_accel_set_common(geometry, source);
      return geometry;
    }
    *unavailable = true;
    rays_metal_set_unavailable(message, "motion acceleration structures require macOS 12");
    return nil;
  }
  case 2: {
    if (@available(macOS 14.0, *)) {
      id<MTLBuffer> radius = rays_accel_buffer(source->secondary_buffer);
      id<MTLBuffer> index = rays_accel_buffer(source->index_buffer);
      if (radius == nil || index == nil) {
        rays_metal_set_error(message, "invalid curve radius or index buffer");
        return nil;
      }
      MTLCurveType curve_type = source->curve_type == 0 ? MTLCurveTypeRound : MTLCurveTypeFlat;
      MTLCurveBasis curve_basis = MTLCurveBasisBSpline;
      switch (source->basis) {
      case 1: curve_basis = MTLCurveBasisCatmullRom; break;
      case 2: curve_basis = MTLCurveBasisLinear; break;
      case 3: curve_basis = MTLCurveBasisBezier; break;
      default: break;
      }
      MTLCurveEndCaps end_caps = MTLCurveEndCapsNone;
      if (source->end_caps == 1) end_caps = MTLCurveEndCapsDisk;
      if (source->end_caps == 2) end_caps = MTLCurveEndCapsSphere;
      if (source->keyframe_count == 0) {
        MTLAccelerationStructureCurveGeometryDescriptor *geometry =
            [MTLAccelerationStructureCurveGeometryDescriptor descriptor];
        geometry.controlPointBuffer = buffer;
        geometry.controlPointBufferOffset = static_cast<NSUInteger>(source->offset);
        geometry.controlPointStride = static_cast<NSUInteger>(source->stride);
        geometry.controlPointCount = static_cast<NSUInteger>(source->count);
        geometry.controlPointFormat = MTLAttributeFormatFloat3;
        geometry.radiusBuffer = radius;
        geometry.radiusBufferOffset = static_cast<NSUInteger>(source->secondary_offset);
        geometry.radiusStride = static_cast<NSUInteger>(source->secondary_stride);
        geometry.radiusFormat = MTLAttributeFormatFloat;
        geometry.indexBuffer = index;
        geometry.indexBufferOffset = static_cast<NSUInteger>(source->index_offset);
        geometry.indexType = source->index_uint16 ? MTLIndexTypeUInt16 : MTLIndexTypeUInt32;
        geometry.segmentCount = static_cast<NSUInteger>(source->segment_count);
        geometry.segmentControlPointCount = source->segment_control_points;
        geometry.curveType = curve_type;
        geometry.curveBasis = curve_basis;
        geometry.curveEndCaps = end_caps;
        rays_accel_set_common(geometry, source);
        return geometry;
      }
      NSArray<MTLMotionKeyframeData *> *control_frames =
          rays_accel_keyframes(source->keyframes, source->keyframe_count);
      NSArray<MTLMotionKeyframeData *> *radius_frames =
          rays_accel_keyframes(source->secondary_keyframes, source->secondary_keyframe_count);
      if (control_frames == nil || radius_frames == nil) {
        rays_metal_set_error(message, "invalid curve motion keyframes");
        return nil;
      }
      MTLAccelerationStructureMotionCurveGeometryDescriptor *geometry =
          [MTLAccelerationStructureMotionCurveGeometryDescriptor descriptor];
      geometry.controlPointBuffers = control_frames;
      geometry.controlPointStride = static_cast<NSUInteger>(source->stride);
      geometry.controlPointCount = static_cast<NSUInteger>(source->count);
      geometry.controlPointFormat = MTLAttributeFormatFloat3;
      geometry.radiusBuffers = radius_frames;
      geometry.radiusStride = static_cast<NSUInteger>(source->secondary_stride);
      geometry.radiusFormat = MTLAttributeFormatFloat;
      geometry.indexBuffer = index;
      geometry.indexBufferOffset = static_cast<NSUInteger>(source->index_offset);
      geometry.indexType = source->index_uint16 ? MTLIndexTypeUInt16 : MTLIndexTypeUInt32;
      geometry.segmentCount = static_cast<NSUInteger>(source->segment_count);
      geometry.segmentControlPointCount = source->segment_control_points;
      geometry.curveType = curve_type;
      geometry.curveBasis = curve_basis;
      geometry.curveEndCaps = end_caps;
      rays_accel_set_common(geometry, source);
      return geometry;
    }
    *unavailable = true;
    rays_metal_set_unavailable(message, "curve acceleration structures require macOS 14");
    return nil;
  }
  default:
    rays_metal_set_error(message, "unknown acceleration geometry type");
    return nil;
  }
}

static MTLAccelerationStructureDescriptor *rays_accel_descriptor(
    const RaysAccelBuildDescriptor *source, const char **message, bool *unavailable)
    API_AVAILABLE(macos(11.0)) {
  if (source == nullptr) {
    rays_metal_set_error(message, "null acceleration build descriptor");
    return nil;
  }
  if (source->kind == 0) {
    const RaysAccelPrimitiveDescriptor *primitive = source->primitive;
    if (primitive == nullptr || primitive->geometries == nullptr) {
      rays_metal_set_error(message, "primitive descriptor requires geometry");
      return nil;
    }
    MTLPrimitiveAccelerationStructureDescriptor *descriptor =
        [MTLPrimitiveAccelerationStructureDescriptor descriptor];
    NSMutableArray<MTLAccelerationStructureGeometryDescriptor *> *geometries =
        [NSMutableArray arrayWithCapacity:primitive->geometry_count];
    for (uintptr_t i = 0; i < primitive->geometry_count; ++i) {
      MTLAccelerationStructureGeometryDescriptor *geometry =
          rays_accel_geometry(&primitive->geometries[i], message, unavailable);
      if (geometry == nil) return nil;
      [geometries addObject:geometry];
    }
    descriptor.geometryDescriptors = geometries;
    if (primitive->has_motion != 0) {
      if (@available(macOS 12.0, *)) {
        descriptor.motionKeyframeCount = static_cast<NSUInteger>(primitive->motion.keyframe_count);
        descriptor.motionStartTime = static_cast<float>(primitive->motion.start_time);
        descriptor.motionEndTime = static_cast<float>(primitive->motion.end_time);
        descriptor.motionStartBorderMode = primitive->motion.start_border == 0
            ? MTLMotionBorderModeClamp : MTLMotionBorderModeVanish;
        descriptor.motionEndBorderMode = primitive->motion.end_border == 0
            ? MTLMotionBorderModeClamp : MTLMotionBorderModeVanish;
      } else {
        *unavailable = true;
        rays_metal_set_unavailable(message, "motion acceleration structures require macOS 12");
        return nil;
      }
    }
    MTLAccelerationStructureUsage usage = MTLAccelerationStructureUsageNone;
    if (primitive->refit != 0) usage |= MTLAccelerationStructureUsageRefit;
    if (primitive->fast_build != 0) usage |= MTLAccelerationStructureUsagePreferFastBuild;
    descriptor.usage = usage;
    return descriptor;
  }
  if (source->kind == 1) {
    const RaysAccelInstancesDescriptor *instances = source->instances;
    if (instances == nullptr || instances->buffer == nullptr ||
        instances->structures == nullptr) {
      rays_metal_set_error(message, "instance descriptor requires a buffer and structures");
      return nil;
    }
    id<MTLBuffer> instance_buffer = rays_accel_buffer(instances->buffer);
    if (instance_buffer == nil) {
      rays_metal_set_error(message, "invalid instance descriptor buffer");
      return nil;
    }
    MTLInstanceAccelerationStructureDescriptor *descriptor =
        [MTLInstanceAccelerationStructureDescriptor descriptor];
    descriptor.instanceDescriptorBuffer = instance_buffer;
    descriptor.instanceDescriptorBufferOffset = static_cast<NSUInteger>(instances->offset);
    descriptor.instanceDescriptorStride = static_cast<NSUInteger>(instances->stride);
    descriptor.instanceCount = static_cast<NSUInteger>(instances->count);
    switch (instances->instance_kind) {
    case 1:
      descriptor.instanceDescriptorType = MTLAccelerationStructureInstanceDescriptorTypeUserID;
      break;
    case 2:
      if (@available(macOS 12.0, *)) {
        descriptor.instanceDescriptorType = MTLAccelerationStructureInstanceDescriptorTypeMotion;
      } else {
        *unavailable = true;
        rays_metal_set_unavailable(message, "motion instances require macOS 12");
        return nil;
      }
      break;
    default:
      rays_metal_set_error(message, "unknown instance descriptor type");
      return nil;
    }
    NSMutableArray<id<MTLAccelerationStructure>> *structures =
        [NSMutableArray arrayWithCapacity:instances->structure_count];
    for (uintptr_t i = 0; i < instances->structure_count; ++i) {
      id<MTLAccelerationStructure> structure =
          (__bridge id<MTLAccelerationStructure>)rays_metal_handle_object(
              instances->structures[i], KIND_ACCELERATION_STRUCTURE);
      if (structure == nil) {
        rays_metal_set_error(message, "invalid instanced acceleration structure");
        return nil;
      }
      [structures addObject:structure];
    }
    descriptor.instancedAccelerationStructures = structures;
    if (instances->has_motion_transforms != 0) {
      if (@available(macOS 12.0, *)) {
        id<MTLBuffer> transforms = rays_accel_buffer(instances->motion_transform_buffer);
        if (transforms == nil) {
          rays_metal_set_error(message, "invalid motion transform buffer");
          return nil;
        }
        descriptor.motionTransformBuffer = transforms;
        descriptor.motionTransformBufferOffset = static_cast<NSUInteger>(instances->motion_transform_offset);
        descriptor.motionTransformCount = static_cast<NSUInteger>(instances->motion_transform_count);
      } else {
        *unavailable = true;
        rays_metal_set_unavailable(message, "motion instances require macOS 12");
        return nil;
      }
    }
    descriptor.usage = instances->refit ? MTLAccelerationStructureUsageRefit
                                        : MTLAccelerationStructureUsageNone;
    return descriptor;
  }
  rays_metal_set_error(message, "unknown acceleration build descriptor type");
  return nil;
}

// current objects once when the immutable prepared descriptor takes its owners.
extern "C" int32_t rays_metal_acceleration_validate_bindings(
    MetalHandle *device_handle, MetalHandle *const *buffers,
    const uint64_t *declared_lengths, uintptr_t buffer_count,
    MetalHandle *const *structures, uintptr_t structure_count,
    const char **message) {
  if (message) *message = nullptr;
  if ((buffer_count && (!buffers || !declared_lengths)) ||
      (structure_count && !structures))
    return rays_metal_set_invalid_argument(message, "null acceleration binding array");
  @try {
    @autoreleasepool {
      id<MTLDevice> device = (__bridge id<MTLDevice>)
          rays_metal_handle_object(device_handle, KIND_DEVICE);
      if (!device)
        return rays_metal_set_invalid_argument(message, "invalid acceleration device");
      for (uintptr_t i = 0; i < buffer_count; ++i) {
        id<MTLBuffer> buffer = rays_accel_buffer(buffers[i]);
        if (!buffer || buffer.device != device || buffer.length < declared_lengths[i])
          return rays_metal_set_invalid_argument(message,
              "native acceleration buffer device or allocation does not match admission");
      }
      if (@available(macOS 11.0, *)) {
        for (uintptr_t i = 0; i < structure_count; ++i) {
          id<MTLAccelerationStructure> structure = (__bridge id<MTLAccelerationStructure>)
              rays_metal_handle_object(structures[i], KIND_ACCELERATION_STRUCTURE);
          if (!structure || structure.device != device)
            return rays_metal_set_invalid_argument(message,
                "native acceleration structure device does not match admission");
        }
        return 0;
      }
      return rays_metal_set_unavailable(message, "acceleration structures require macOS 11");
    }
  } @catch (NSException *exception) {
    return rays_metal_set_error(message, exception.reason.UTF8String);
  }
}

extern "C" int32_t rays_metal_acceleration_descriptor_sizes(
    MetalHandle *device_handle, const RaysAccelBuildDescriptor *raw_descriptor,
    RaysAccelSizes *output, const char **message) {
  if (message != nullptr) *message = nullptr;
  if (output != nullptr) *output = {};
  if (device_handle == nullptr || raw_descriptor == nullptr || output == nullptr)
    return rays_metal_set_error(message, "invalid acceleration size arguments");
  if (@available(macOS 11.0, *)) {
    @try {
      @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)
            rays_metal_handle_object(device_handle, KIND_DEVICE);
        if (device == nil) return rays_metal_set_error(message, "invalid Metal device handle");
        bool unavailable = false;
        MTLAccelerationStructureDescriptor *descriptor =
            rays_accel_descriptor(raw_descriptor, message, &unavailable);
        if (descriptor == nil) {
          if (unavailable)
            return rays_metal_set_unavailable(message, "requested acceleration feature requires macOS 12");
          return message != nullptr && *message != nullptr ? 1 :
              rays_metal_set_error(message, "unable to create acceleration descriptor");
        }
        MTLAccelerationStructureSizes sizes =
            [device accelerationStructureSizesWithDescriptor:descriptor];
        output->acceleration_structure_size = static_cast<uint64_t>(sizes.accelerationStructureSize);
        output->build_scratch_buffer_size = static_cast<uint64_t>(sizes.buildScratchBufferSize);
        output->refit_scratch_buffer_size = static_cast<uint64_t>(sizes.refitScratchBufferSize);
        return 0;
      }
    } @catch (NSException *exception) {
      return rays_metal_set_error(message, exception.reason.UTF8String);
    }
  }
  return rays_metal_set_unavailable(message, "acceleration structures require macOS 11");
}

extern "C" int32_t rays_metal_acceleration_encoder_build(
    MetalHandle *encoder_handle, MetalHandle *destination_handle,
    const RaysAccelBuildDescriptor *raw_descriptor, MetalHandle *scratch_handle,
    uint64_t scratch_offset, uint8_t refit, const char **message) {
  if (message != nullptr) *message = nullptr;
  if (encoder_handle == nullptr || destination_handle == nullptr ||
      raw_descriptor == nullptr || scratch_handle == nullptr ||
      scratch_offset > static_cast<uint64_t>(std::numeric_limits<NSUInteger>::max()))
    return rays_metal_set_error(message, "invalid acceleration build arguments");
  if (@available(macOS 11.0, *)) {
    @try {
      @autoreleasepool {
        id<MTLAccelerationStructureCommandEncoder> encoder =
            (__bridge id<MTLAccelerationStructureCommandEncoder>)
                rays_metal_handle_object(encoder_handle, KIND_ACCELERATION_ENCODER);
        id<MTLAccelerationStructure> destination =
            (__bridge id<MTLAccelerationStructure>)rays_metal_handle_object(
                destination_handle, KIND_ACCELERATION_STRUCTURE);
        id<MTLBuffer> scratch = rays_accel_buffer(scratch_handle);
        if (encoder == nil || destination == nil || scratch == nil)
          return rays_metal_set_error(message, "invalid acceleration encoder, structure, or scratch buffer");
        bool unavailable = false;
        MTLAccelerationStructureDescriptor *descriptor =
            rays_accel_descriptor(raw_descriptor, message, &unavailable);
        if (descriptor == nil) {
          if (unavailable)
            return rays_metal_set_unavailable(message, "requested acceleration feature requires macOS 12");
          return message != nullptr && *message != nullptr ? 1 :
              rays_metal_set_error(message, "unable to create acceleration descriptor");
        }
        if (refit != 0) {
          [encoder refitAccelerationStructure:destination
                                   descriptor:descriptor
                                  destination:nil
                                scratchBuffer:scratch
                          scratchBufferOffset:static_cast<NSUInteger>(scratch_offset)];
        } else {
          [encoder buildAccelerationStructure:destination
                                   descriptor:descriptor
                                scratchBuffer:scratch
                          scratchBufferOffset:static_cast<NSUInteger>(scratch_offset)];
        }
        return 0;
      }
    } @catch (NSException *exception) {
      return rays_metal_set_error(message, exception.reason.UTF8String);
    }
  }
  return rays_metal_set_unavailable(message, "acceleration structures require macOS 11");
}

extern "C" int32_t rays_metal_acceleration_encoder_write_size(
    MetalHandle *encoder_handle, MetalHandle *structure_handle,
    MetalHandle *buffer_handle, uint64_t offset, int64_t size_data_type,
    const char **message) {
  if (message != nullptr) *message = nullptr;
  uint64_t bytes = size_data_type == 0x55 ? sizeof(uint64_t) : sizeof(uint32_t);
  if (encoder_handle == nullptr || structure_handle == nullptr || buffer_handle == nullptr ||
      (size_data_type != 0x21 && size_data_type != 0x55) || offset % bytes != 0)
    return rays_metal_set_error(message, "invalid compacted-size arguments");
  if (@available(macOS 11.0, *)) {
    @try {
      @autoreleasepool {
        id<MTLAccelerationStructureCommandEncoder> encoder =
            (__bridge id<MTLAccelerationStructureCommandEncoder>)
                rays_metal_handle_object(encoder_handle, KIND_ACCELERATION_ENCODER);
        id<MTLAccelerationStructure> structure =
            (__bridge id<MTLAccelerationStructure>)rays_metal_handle_object(
                structure_handle, KIND_ACCELERATION_STRUCTURE);
        id<MTLBuffer> buffer = rays_accel_buffer(buffer_handle);
        if (encoder == nil || structure == nil || buffer == nil ||
            offset > buffer.length || bytes > buffer.length - offset)
          return rays_metal_set_error(message, "compacted-size destination range out of bounds");
        [encoder writeCompactedAccelerationStructureSize:structure
                                                toBuffer:buffer
                                                  offset:static_cast<NSUInteger>(offset)
                                           sizeDataType:static_cast<MTLDataType>(size_data_type)];
        return 0;
      }
    } @catch (NSException *exception) {
      return rays_metal_set_error(message, exception.reason.UTF8String);
    }
  }
  return rays_metal_set_unavailable(message, "acceleration structures require macOS 11");
}

extern "C" int32_t rays_metal_acceleration_instance_layout(
    int32_t kind, RaysAccelInstanceLayout *output, const char **message) {
  if (message != nullptr) *message = nullptr;
  if (output == nullptr) return rays_metal_set_error(message, "null instance layout output");
  for (int i = 0; i < 12; ++i) output->values[i] = -1;
  if (@available(macOS 11.0, *)) {
    switch (kind) {
    case 1:
      output->values[0] = sizeof(MTLAccelerationStructureUserIDInstanceDescriptor);
      output->values[1] = offsetof(MTLAccelerationStructureUserIDInstanceDescriptor, transformationMatrix);
      output->values[2] = offsetof(MTLAccelerationStructureUserIDInstanceDescriptor, options);
      output->values[3] = offsetof(MTLAccelerationStructureUserIDInstanceDescriptor, mask);
      output->values[4] = offsetof(MTLAccelerationStructureUserIDInstanceDescriptor, accelerationStructureIndex);
      output->values[5] = offsetof(MTLAccelerationStructureUserIDInstanceDescriptor, userID);
      return 0;
    case 2:
      if (@available(macOS 12.0, *)) {
        output->values[0] = sizeof(MTLAccelerationStructureMotionInstanceDescriptor);
        output->values[2] = offsetof(MTLAccelerationStructureMotionInstanceDescriptor, options);
        output->values[3] = offsetof(MTLAccelerationStructureMotionInstanceDescriptor, mask);
        output->values[4] = offsetof(MTLAccelerationStructureMotionInstanceDescriptor, accelerationStructureIndex);
        output->values[5] = offsetof(MTLAccelerationStructureMotionInstanceDescriptor, userID);
        output->values[6] = offsetof(MTLAccelerationStructureMotionInstanceDescriptor, motionTransformsStartIndex);
        output->values[7] = offsetof(MTLAccelerationStructureMotionInstanceDescriptor, motionTransformsCount);
        output->values[8] = offsetof(MTLAccelerationStructureMotionInstanceDescriptor, motionStartBorderMode);
        output->values[9] = offsetof(MTLAccelerationStructureMotionInstanceDescriptor, motionEndBorderMode);
        output->values[10] = offsetof(MTLAccelerationStructureMotionInstanceDescriptor, motionStartTime);
        output->values[11] = offsetof(MTLAccelerationStructureMotionInstanceDescriptor, motionEndTime);
        return 0;
      }
      return rays_metal_set_unavailable(message, "motion instances require macOS 12");
    default:
      return rays_metal_set_error(message, "unknown instance descriptor type");
    }
  }
  return rays_metal_set_unavailable(message, "acceleration structures require macOS 11");
}
