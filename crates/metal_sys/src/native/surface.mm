#import <Foundation/Foundation.h>
#import <Metal/Metal.h>
#import <QuartzCore/CAMetalLayer.h>

#include "metal_handle.h"
#include "metal_kinds.h"
#include <cstdint>
#include <limits>

extern "C" int32_t rays_metal_layer_adopt_borrowed(
    MetalHandle *device_handle, void *borrowed_layer, MetalHandle **output,
    const char **message) {
  if (message != nullptr) *message = nullptr;
  if (output != nullptr) *output = nullptr;
  @try {
    @autoreleasepool {
      if (device_handle == nullptr || borrowed_layer == nullptr || output == nullptr)
        return rays_metal_set_error(message, "invalid borrowed Metal layer arguments");
      id object = (__bridge id)borrowed_layer;
      if (![object isKindOfClass:[CAMetalLayer class]])
        return rays_metal_set_error(message, "native layer token does not contain CAMetalLayer");
      id<MTLDevice> device = (__bridge id<MTLDevice>)
          rays_metal_handle_object(device_handle, KIND_DEVICE);
      if (device == nil)
        return rays_metal_set_error(message, "invalid Metal device handle");
      CAMetalLayer *layer = (CAMetalLayer *)object;
      if (layer.device != nil && layer.device.registryID != device.registryID)
        return rays_metal_set_error(message, "CAMetalLayer belongs to another Metal device");
      layer.device = device;
      *output = rays_metal_handle_new((__bridge void *)layer, KIND_METAL_LAYER);
      if (*output == nullptr)
        return rays_metal_set_error(message, "unable to retain CAMetalLayer");
      return 0;
    }
  } @catch (NSException *exception) {
    return rays_metal_set_error(message, exception.reason.UTF8String);
  }
}

extern "C" int32_t rays_metal_layer_configure(
    MetalHandle *layer_handle, uint64_t width, uint64_t height, int64_t format,
    uint8_t framebuffer_only, uint64_t maximum_drawables, uint8_t allows_timeout,
    uint8_t display_sync, uint8_t presents_with_transaction,
    const char **message) {
  if (message != nullptr) *message = nullptr;
  @try {
    @autoreleasepool {
      if (layer_handle == nullptr || width == 0 || height == 0 ||
          width > static_cast<uint64_t>(std::numeric_limits<NSUInteger>::max()) ||
          height > static_cast<uint64_t>(std::numeric_limits<NSUInteger>::max()) ||
          maximum_drawables < 2 || maximum_drawables > 3 ||
          (format != 80 && format != 81 && format != 115))
        return rays_metal_set_error(message, "invalid Metal layer configuration");
      CAMetalLayer *layer = (__bridge CAMetalLayer *)
          rays_metal_handle_object(layer_handle, KIND_METAL_LAYER);
      if (layer == nil)
        return rays_metal_set_error(message, "invalid CAMetalLayer handle");
      if (@available(macOS 10.13.2, *)) {
        layer.drawableSize = CGSizeMake(static_cast<CGFloat>(width),
                                       static_cast<CGFloat>(height));
        layer.pixelFormat = static_cast<MTLPixelFormat>(format);
        layer.framebufferOnly = framebuffer_only != 0;
        layer.maximumDrawableCount = static_cast<NSUInteger>(maximum_drawables);
        layer.allowsNextDrawableTimeout = allows_timeout != 0;
        layer.displaySyncEnabled = display_sync != 0;
        layer.presentsWithTransaction = presents_with_transaction != 0;
      } else {
        return rays_metal_set_unavailable(
            message, "CAMetalLayer drawable configuration is unavailable");
      }
      return 0;
    }
  } @catch (NSException *exception) {
    return rays_metal_set_error(message, exception.reason.UTF8String);
  }
}

extern "C" int32_t rays_metal_drawable_texture(
    MetalHandle *drawable_handle, MetalHandle **output, uint64_t *width,
    uint64_t *height, int64_t *format, const char **message) {
  if (message != nullptr) *message = nullptr;
  if (output != nullptr) *output = nullptr;
  @try {
    @autoreleasepool {
      if (drawable_handle == nullptr || output == nullptr || width == nullptr ||
          height == nullptr || format == nullptr)
        return rays_metal_set_error(message, "invalid Metal drawable arguments");
      id<CAMetalDrawable> drawable = (__bridge id<CAMetalDrawable>)
          rays_metal_handle_object(drawable_handle, KIND_METAL_DRAWABLE);
      if (drawable == nil)
        return rays_metal_set_error(message, "invalid Metal drawable handle");
      id<MTLTexture> texture = drawable.texture;
      if (texture == nil)
        return rays_metal_set_error(message, "drawable has no texture");
      *output = rays_metal_handle_new((__bridge void *)texture, KIND_TEXTURE);
      if (*output == nullptr)
        return rays_metal_set_error(message, "unable to retain drawable texture");
      *width = static_cast<uint64_t>(texture.width);
      *height = static_cast<uint64_t>(texture.height);
      *format = static_cast<int64_t>(texture.pixelFormat);
      return 0;
    }
  } @catch (NSException *exception) {
    return rays_metal_set_error(message, exception.reason.UTF8String);
  }
}
