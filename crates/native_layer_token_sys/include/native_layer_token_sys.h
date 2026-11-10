#ifndef RAYS_NATIVE_LAYER_TOKEN_SYS_H
#define RAYS_NATIVE_LAYER_TOKEN_SYS_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct rays_native_layer_token rays_native_layer_token;
/* Linux: a token's layer pointer names this pair (wl_display*, wl_surface*). */
typedef struct rays_wayland_layer { void *display; void *surface; } rays_wayland_layer;
typedef void (*rays_native_layer_retain_fn)(void *);
typedef void (*rays_native_layer_release_fn)(void *);

/* Returns NULL if retain/release are not both NULL or both non-NULL. */
rays_native_layer_token *rays_native_layer_token_create(
    void *layer, int64_t owner_id, int64_t generation,
    rays_native_layer_retain_fn retain, rays_native_layer_release_fn release);
void rays_native_layer_token_invalidate(const rays_native_layer_token *token);
void *rays_native_layer_token_borrow(const rays_native_layer_token *token,
    int64_t owner_id, int64_t generation);
int64_t rays_native_layer_token_owner_id(const rays_native_layer_token *token);
int64_t rays_native_layer_token_generation(const rays_native_layer_token *token);
bool rays_native_layer_token_alive(const rays_native_layer_token *token);
void rays_native_layer_token_destroy(rays_native_layer_token *token);

#ifdef __cplusplus
}
#endif
#endif
