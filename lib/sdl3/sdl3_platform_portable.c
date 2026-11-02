#include "sdl3_platform.h"

/* SDL_IsMainThread answers true on every thread until SDL_SetMainReady records one, which an OCaml
   program never calls. The loader runs this on the process's first thread, the OCaml initial one. */
static SDL_ThreadID rays_main_thread;
__attribute__((constructor)) static void rays_note_main_thread(void) { rays_main_thread = SDL_GetCurrentThreadID(); }
bool rays_is_main_thread(void) { return SDL_GetCurrentThreadID() == rays_main_thread; }

value rays_sdl_layer_token(void *layer, uint64_t owner, uint64_t generation)
{
  return rays_native_layer_token_create(layer, owner, generation, NULL, NULL);
}

void rays_scroll_monitor_install(void) {}
bool rays_scroll_take(rays_scroll *out) { (void)out; return false; }
void rays_window_plain_titlebar(SDL_Window *window) { (void)window; }
void rays_app_activate(void) {}
void rays_window_set_background(SDL_Window *window,
    double red, double green, double blue)
{
  (void)window; (void)red; (void)green; (void)blue;
}
