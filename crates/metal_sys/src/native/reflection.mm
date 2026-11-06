#include "metal_handle.h"
#include <Metal/Metal.h>
#include <cstdint>
#include <cstring>
#include <limits>
#include <memory>
#include <new>

struct RaysMetalReflectionNode;
struct RaysMetalReflectionMember {
  char *name;
  int64_t offset;
  int64_t argument_index;
  int64_t data_type;
  RaysMetalReflectionNode *reflected_type;
};
struct RaysMetalReflectionNode {
  int32_t kind;
  int64_t data_type;
  int64_t access;
  int64_t alignment;
  int64_t data_size;
  uint8_t element_is_argument_buffer;
  int64_t length;
  int64_t stride;
  int64_t argument_index_stride;
  int64_t texture_type;
  uint8_t depth;
  int64_t tensor_data_type;
  int64_t index_type;
  uint8_t dimensions_present;
  int64_t *dimensions;
  size_t dimension_count;
  RaysMetalReflectionNode *element;
  RaysMetalReflectionMember *members;
  size_t member_count;
};

static constexpr size_t reflection_max_depth = 32;
static constexpr size_t reflection_max_members = 65'536;

static void free_node(RaysMetalReflectionNode *node) {
  if (node == nullptr) return;
  free_node(node->element);
  if (node->members != nullptr) {
    for (size_t index = 0; index < node->member_count; ++index) {
      delete[] node->members[index].name;
      free_node(node->members[index].reflected_type);
    }
  }
  delete[] node->members;
  delete[] node->dimensions;
  delete node;
}

static bool signed_size(NSUInteger value, int64_t *output) {
  if (value > static_cast<NSUInteger>(std::numeric_limits<int64_t>::max())) return false;
  *output = static_cast<int64_t>(value);
  return true;
}

static MTLType *member_type(MTLStructMember *member) {
  MTLType *type = member.arrayType;
  if (type == nil) type = member.pointerType;
  if (type == nil) type = member.structType;
  if (@available(macOS 26.0, *)) {
    if (type == nil) type = member.tensorReferenceType;
  }
  if (type == nil) type = member.textureReferenceType;
  return type;
}

static MTLType *array_element_type(MTLArrayType *array) {
  MTLType *type = array.elementArrayType;
  if (type == nil) type = array.elementPointerType;
  if (type == nil) type = array.elementStructType;
  if (@available(macOS 26.0, *)) {
    if (type == nil) type = array.elementTensorReferenceType;
  }
  if (type == nil) type = array.elementTextureReferenceType;
  return type;
}

static bool copy_type(MTLType *source, size_t depth, size_t *remaining_members,
                      RaysMetalReflectionNode **output, const char **message) {
  *output = nullptr;
  if (source == nil) return true;
  if (depth > reflection_max_depth) {
    rays_metal_set_error(message, "Metal reflection exceeded maximum depth");
    return false;
  }
  std::unique_ptr<RaysMetalReflectionNode, decltype(&free_node)> node(
      new (std::nothrow) RaysMetalReflectionNode{}, &free_node);
  if (node == nullptr) {
    rays_metal_set_error(message, "unable to allocate Metal reflected type");
    return false;
  }
  node->data_type = static_cast<int64_t>(source.dataType);
  if ([source isKindOfClass:[MTLArrayType class]]) {
    MTLArrayType *array = (MTLArrayType *)source;
    node->kind = 1;
    if (!signed_size(array.arrayLength, &node->length) ||
        !signed_size(array.stride, &node->stride) ||
        !signed_size(array.argumentIndexStride, &node->argument_index_stride)) {
      rays_metal_set_error(message, "Metal array reflection exceeds signed bounds");
      return false;
    }
    if (!copy_type(array_element_type(array), depth + 1, remaining_members,
                   &node->element, message)) return false;
  } else if ([source isKindOfClass:[MTLPointerType class]]) {
    MTLPointerType *pointer = (MTLPointerType *)source;
    node->kind = 2;
    node->access = static_cast<int64_t>(pointer.access);
    if (!signed_size(pointer.alignment, &node->alignment) ||
        !signed_size(pointer.dataSize, &node->data_size)) {
      rays_metal_set_error(message, "Metal pointer reflection exceeds signed bounds");
      return false;
    }
    node->element_is_argument_buffer = pointer.elementIsArgumentBuffer ? 1 : 0;
    MTLType *element = pointer.elementArrayType;
    if (element == nil) element = pointer.elementStructType;
    if (!copy_type(element, depth + 1, remaining_members, &node->element,
                   message)) return false;
  } else if ([source isKindOfClass:[MTLStructType class]]) {
    MTLStructType *structure = (MTLStructType *)source;
    NSArray<MTLStructMember *> *members = structure.members;
    if (members.count > *remaining_members ||
        members.count > std::numeric_limits<size_t>::max() /
                            sizeof(RaysMetalReflectionMember)) {
      rays_metal_set_error(message, "Metal reflection exceeded maximum members");
      return false;
    }
    node->kind = 3;
    node->member_count = static_cast<size_t>(members.count);
    node->members = members.count == 0
                        ? nullptr
                        : new (std::nothrow) RaysMetalReflectionMember[members.count]{};
    if (members.count != 0 && node->members == nullptr) {
      rays_metal_set_error(message, "unable to allocate Metal reflection members");
      return false;
    }
    *remaining_members -= node->member_count;
    for (NSUInteger index = 0; index < members.count; ++index) {
      MTLStructMember *member = members[index];
      if (member.name == nil || member.name.UTF8String == nullptr ||
          [structure memberByName:member.name] != member) {
        rays_metal_set_error(message, "Metal returned an invalid reflected struct member");
        return false;
      }
      const char *name = member.name.UTF8String;
      const size_t name_length = std::strlen(name);
      node->members[index].name = new (std::nothrow) char[name_length + 1];
      if (node->members[index].name == nullptr) {
        rays_metal_set_error(message, "unable to copy reflected struct member name");
        return false;
      }
      std::memcpy(node->members[index].name, name, name_length + 1);
      if (member.offset > static_cast<NSUInteger>(std::numeric_limits<int64_t>::max()) ||
          member.argumentIndex > static_cast<NSUInteger>(std::numeric_limits<int64_t>::max())) {
        rays_metal_set_error(message, "Metal struct member reflection exceeds signed bounds");
        return false;
      }
      node->members[index].offset = static_cast<int64_t>(member.offset);
      node->members[index].argument_index = static_cast<int64_t>(member.argumentIndex);
      node->members[index].data_type = static_cast<int64_t>(member.dataType);
      if (!copy_type(member_type(member), depth + 1, remaining_members,
                     &node->members[index].reflected_type, message)) return false;
    }
  } else if ([source isKindOfClass:[MTLTextureReferenceType class]]) {
    MTLTextureReferenceType *texture = (MTLTextureReferenceType *)source;
    node->kind = 4;
    node->access = static_cast<int64_t>(texture.access);
    node->texture_type = static_cast<int64_t>(texture.textureType);
    node->depth = texture.isDepthTexture ? 1 : 0;
  } else if (@available(macOS 26.0, *)) {
    if ([source isKindOfClass:[MTLTensorReferenceType class]]) {
      MTLTensorReferenceType *tensor = (MTLTensorReferenceType *)source;
      node->kind = 5;
      node->access = static_cast<int64_t>(tensor.access);
      node->tensor_data_type = static_cast<int64_t>(tensor.tensorDataType);
      node->index_type = static_cast<int64_t>(tensor.indexType);
      MTLTensorExtents *extents = tensor.dimensions;
      if (extents != nil) {
        if (extents.rank > reflection_max_members ||
            extents.rank > std::numeric_limits<size_t>::max() / sizeof(int64_t)) {
          rays_metal_set_error(message, "Metal tensor reflection exceeds dimension limits");
          return false;
        }
        node->dimensions_present = 1;
        node->dimension_count = static_cast<size_t>(extents.rank);
        node->dimensions = extents.rank == 0
                               ? nullptr
                               : new (std::nothrow) int64_t[extents.rank];
        if (extents.rank != 0 && node->dimensions == nullptr) {
          rays_metal_set_error(message, "unable to allocate Metal tensor dimensions");
          return false;
        }
        for (NSUInteger index = 0; index < extents.rank; ++index)
          node->dimensions[index] = static_cast<int64_t>([extents extentAtDimensionIndex:index]);
      }
    } else {
      node->kind = 0;
    }
  } else {
    node->kind = 0;
  }
  *output = node.release();
  return true;
}

extern "C" int32_t caml_rays_metal_reflection_snapshot(
    void *raw_type, void **output, const char **message) {
  if (message) *message = nullptr;
  if (output) *output = nullptr;
  if (output == nullptr)
    return rays_metal_set_error(message, "invalid Metal reflection snapshot output");
  if (raw_type == nullptr) return 0;
  @try {
    @autoreleasepool {
      auto *type = (__bridge MTLType *)raw_type;
      size_t remaining = reflection_max_members;
      RaysMetalReflectionNode *node = nullptr;
      if (!copy_type(type, 0, &remaining, &node, message)) {
        free_node(node);
        return 1;
      }
      *output = node;
      return 0;
    }
  } @catch (NSException *exception) {
    return rays_metal_set_error(message, exception.reason.UTF8String);
  }
}

extern "C" void caml_rays_metal_reflection_free(void *raw_node) {
  free_node(static_cast<RaysMetalReflectionNode *>(raw_node));
}
