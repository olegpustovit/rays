#pragma once

#include <cstdint>

struct MetalHandle;

extern "C" {
void *rays_metal_handle_object(MetalHandle *handle, uint32_t expected_kind);
MetalHandle *rays_metal_handle_new(void *object, uint32_t kind);
int32_t rays_metal_set_error(const char **message, const char *text);
int32_t rays_metal_set_unavailable(const char **message, const char *text);
}
