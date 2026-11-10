#pragma once

#include <cstdint>

struct MetalHandle;

extern "C" {
void *rays_metal_handle_object(MetalHandle *handle, uint32_t expected_kind);
MetalHandle *rays_metal_handle_new(void *object, uint32_t kind);
MetalHandle *rays_metal_icb_handle_new(void *object, uint32_t command_types);
uint32_t rays_metal_icb_command_types(MetalHandle *handle);
int32_t rays_metal_set_error(const char **message, const char *text);
int32_t rays_metal_set_unavailable(const char **message, const char *text);
int32_t rays_metal_device_supports_sampler_reduction(MetalHandle *device, uint8_t *output, const char **message);
}

// Status3 is rejected caller input, distinct from native/device failures and unavailable2.
inline int32_t rays_metal_set_invalid_argument(const char **message, const char *text) {
  rays_metal_set_error(message, text);
  return 3;
}

inline int32_t rays_metal_set_unsupported(const char **message, const char *text) {
  rays_metal_set_error(message, text);
  return 4;
}
