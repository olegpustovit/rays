#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include <SDL3/SDL.h>
#include <SDL3_image/SDL_image.h>

#include "floor.h"

_Static_assert(SDL_IMAGE_VERSION >= SDL_VERSIONNUM(RAYS_FLOOR_MAJOR, RAYS_FLOOR_MINOR, RAYS_FLOOR_PATCH),
    "SDL3_image headers are below packaging/sdl3.lock floor");

/* Copies a surface into a tightly packed RGBA8 SDL_malloc buffer; always consumes `source`. */
static bool copy_rgba(SDL_Surface *source, const char *what, int *width, int *height, uint8_t **pixels)
{
  SDL_Surface *surface;
  size_t row_bytes, total_bytes;
  uint8_t *copy;
  int row;
  if (source == NULL) return false;
  if (source->format == SDL_PIXELFORMAT_RGBA32) {
    surface = source;
  } else {
    surface = SDL_ConvertSurface(source, SDL_PIXELFORMAT_RGBA32);
    SDL_DestroySurface(source);
    if (surface == NULL) return false;
  }
  if (surface->w <= 0 || surface->h <= 0 || surface->pixels == NULL ||
      (size_t)surface->w > SIZE_MAX / 4) {
    SDL_SetError("%s has invalid dimensions or no CPU pixels", what);
    SDL_DestroySurface(surface);
    return false;
  }
  row_bytes = (size_t)surface->w * 4;
  if ((size_t)surface->h > SIZE_MAX / row_bytes || surface->pitch < 0 ||
      (size_t)surface->pitch < row_bytes) {
    SDL_SetError("%s byte count or pitch is invalid", what);
    SDL_DestroySurface(surface);
    return false;
  }
  total_bytes = row_bytes * (size_t)surface->h;
  copy = (uint8_t *)SDL_malloc(total_bytes);
  if (copy == NULL) {
    SDL_DestroySurface(surface);
    return false;
  }
  if (!SDL_LockSurface(surface)) {
    SDL_free(copy);
    SDL_DestroySurface(surface);
    return false;
  }
  for (row = 0; row < surface->h; row++) {
    memcpy(copy + ((size_t)row * row_bytes),
        (const uint8_t *)surface->pixels + ((size_t)row * (size_t)surface->pitch), row_bytes);
  }
  SDL_UnlockSurface(surface);
  *width = surface->w;
  *height = surface->h;
  *pixels = copy;
  SDL_DestroySurface(surface);
  return true;
}

const char *rays_img_get_error(void) { return SDL_GetError(); }
void rays_img_free(void *pointer) { SDL_free(pointer); }

int rays_image_version(void) { return IMG_Version(); }
int rays_image_compiled_version(void) { return SDL_IMAGE_VERSION; }

bool rays_image_decode(const void *data, size_t length, const char *kind,
    int *width, int *height, uint8_t **pixels)
{
  SDL_IOStream *stream = SDL_IOFromConstMem(data, length);
  SDL_Surface *surface;
  if (stream == NULL) return false;
  surface = kind != NULL ? IMG_LoadTyped_IO(stream, true, kind) : IMG_Load_IO(stream, true);
  return copy_rgba(surface, "decoded image", width, height, pixels);
}
