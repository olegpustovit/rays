#ifndef RAYS_NATIVE_LAYER_TOKEN_H
#define RAYS_NATIVE_LAYER_TOKEN_H
#include <caml/mlvalues.h>
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif
/* The platform supplies paired ownership operations for its opaque presentation
   handle. Two NULL operations describe a borrowed handle invalidated by its owner. */
value rays_native_layer_token_create(void *layer, uint64_t owner, uint64_t generation,
    void (*retain)(void *), void (*release)(void *));
void rays_native_layer_token_invalidate(value token);
void *rays_native_layer_token_borrow(value token, uint64_t owner, uint64_t generation);
#ifdef __cplusplus
}
#endif
#endif
