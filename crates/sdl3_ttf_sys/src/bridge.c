#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include <SDL3/SDL.h>
#include <SDL3_ttf/SDL_ttf.h>

#include "floor.h"

_Static_assert(SDL_TTF_VERSION >= SDL_VERSIONNUM(RAYS_FLOOR_MAJOR, RAYS_FLOOR_MINOR, RAYS_FLOOR_PATCH),
    "SDL3_ttf headers are below packaging/sdl3.lock floor");

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

const char *rays_ttf_get_error(void) { return SDL_GetError(); }
void rays_ttf_free(void *pointer) { SDL_free(pointer); }

int rays_ttf_version(void) { return TTF_Version(); }
int rays_ttf_compiled_version(void) { return SDL_TTF_VERSION; }
bool rays_ttf_init(void) { return TTF_Init(); }
void rays_ttf_quit(void) { TTF_Quit(); }
int rays_ttf_was_init(void) { return TTF_WasInit(); }
void *rays_ttf_open_font(const char *path, double size) { return TTF_OpenFont(path, (float)size); }
void rays_ttf_close_font(void *font) { TTF_CloseFont((TTF_Font *)font); }

void rays_ttf_font_metrics(void *font, int *out)
{
  out[0] = TTF_GetFontHeight((TTF_Font *)font);
  out[1] = TTF_GetFontAscent((TTF_Font *)font);
  out[2] = TTF_GetFontDescent((TTF_Font *)font);
  out[3] = TTF_GetFontLineSkip((TTF_Font *)font);
}

bool rays_ttf_set_font_size_dpi(void *font, double size, int horizontal, int vertical)
{
  return TTF_SetFontSizeDPI((TTF_Font *)font, (float)size, horizontal, vertical);
}

void rays_ttf_set_font_hinting(void *font, int hinting)
{
  TTF_SetFontHinting((TTF_Font *)font, (TTF_HintingFlags)hinting);
}

void rays_ttf_set_font_wrap_alignment(void *font, int code)
{
  static const TTF_HorizontalAlignment alignments[] = {
    TTF_HORIZONTAL_ALIGN_LEFT, TTF_HORIZONTAL_ALIGN_CENTER, TTF_HORIZONTAL_ALIGN_RIGHT
  };
  TTF_SetFontWrapAlignment((TTF_Font *)font, alignments[code]);
}

bool rays_ttf_glyph_metrics(void *font, uint32_t codepoint, int *out)
{
  return TTF_GetGlyphMetrics((TTF_Font *)font, codepoint, &out[0], &out[1], &out[2], &out[3], &out[4]);
}

/* wrap_width < 0 renders unwrapped text. */
bool rays_ttf_render_blended(void *font, const char *text, size_t length, int red, int green,
    int blue, int alpha, int wrap_width, int *width, int *height, uint8_t **pixels)
{
  SDL_Color color;
  SDL_Surface *surface;
  color.r = (Uint8)red;
  color.g = (Uint8)green;
  color.b = (Uint8)blue;
  color.a = (Uint8)alpha;
  surface = wrap_width < 0
      ? TTF_RenderText_Blended((TTF_Font *)font, text, length, color)
      : TTF_RenderText_Blended_Wrapped((TTF_Font *)font, text, length, color, wrap_width);
  return copy_rgba(surface, "rasterized text", width, height, pixels);
}
