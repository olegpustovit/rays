#ifndef RAYS_SDL3_SYS_BRIDGE_H
#define RAYS_SDL3_SYS_BRIDGE_H

#include <SDL3/SDL.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include <SDL3/SDL_metal.h>
#include "dialog.h"

typedef struct {
  int32_t tag;
  int32_t a, b, c, d, e, f, g, h;
  int32_t integer_x, integer_y;
  int32_t id;
  double x, y, z, w;
  char *text;
  size_t text_len;
} rays_sdl3_event;

int32_t rays_sdl3_linked_version(void);
int32_t rays_sdl3_compiled_version(void);
const char *rays_sdl3_get_error(void);
void rays_sdl3_clear_error(void);
const char *rays_sdl3_platform_name(void);
char *rays_sdl3_pref_path(const char *org, const char *app);
void rays_sdl3_free(void *value);
bool rays_sdl3_is_main_thread(void);
bool rays_sdl3_init_subsystem(uint32_t bits);
void rays_sdl3_quit_subsystem(uint32_t bits);

SDL_Window *rays_sdl3_create_window(const char *title, int width, int height, uint32_t flags);
void rays_sdl3_destroy_window(SDL_Window *window);
bool rays_sdl3_window_size(SDL_Window *window, int *width, int *height);
bool rays_sdl3_window_size_in_pixels(SDL_Window *window, int *width, int *height);
uint32_t rays_sdl3_window_state(SDL_Window *window);
uint32_t rays_sdl3_window_id(SDL_Window *window);
float rays_sdl3_window_pixel_density(SDL_Window *window);
float rays_sdl3_window_display_scale(SDL_Window *window);
bool rays_sdl3_window_position(SDL_Window *window, int *x, int *y);
const char *rays_sdl3_window_title(SDL_Window *window);
bool rays_sdl3_center_window(SDL_Window *window);
bool rays_sdl3_set_window_size(SDL_Window *window, int width, int height);
bool rays_sdl3_set_window_resizable(SDL_Window *window, bool enabled);
bool rays_sdl3_set_window_relative_mouse(SDL_Window *window, bool enabled);
bool rays_sdl3_show_window(SDL_Window *window);
bool rays_sdl3_raise_window(SDL_Window *window);
bool rays_sdl3_hide_window(SDL_Window *window);
bool rays_sdl3_restore_window(SDL_Window *window);
bool rays_sdl3_sync_window(SDL_Window *window);
void rays_sdl3_set_window_background(SDL_Window *window, double red, double green, double blue);

SDL_Cursor *rays_sdl3_create_cursor(int shape);
bool rays_sdl3_set_cursor(SDL_Cursor *cursor);
void rays_sdl3_destroy_cursor(SDL_Cursor *cursor);
bool rays_sdl3_set_control_click_right_click(bool enabled);
bool rays_sdl3_set_mouse_relative_system_scale(bool enabled);
bool rays_sdl3_clipboard_set_text(const char *text);
char *rays_sdl3_clipboard_get_text(void);
bool rays_sdl3_start_text_input(SDL_Window *window);
bool rays_sdl3_set_text_input_area(SDL_Window *window, int x, int y, int width, int height, int cursor);
bool rays_sdl3_stop_text_input(SDL_Window *window);
void rays_sdl3_scroll_monitor_install(void);
void rays_sdl3_window_plain_titlebar(SDL_Window *window);
void rays_sdl3_app_activate(void);
void rays_sdl3_platform_set_window_background(SDL_Window *window, double red, double green, double blue);

SDL_MetalView rays_sdl3_create_metal_view(SDL_Window *window);
void rays_sdl3_destroy_metal_view(SDL_MetalView view);
void *rays_sdl3_metal_layer(SDL_MetalView view);
void rays_sdl3_retain_layer(void *layer);
void rays_sdl3_release_layer(void *layer);

int rays_sdl3_show_dialog(SDL_Window *window, int kind,
    const char *const *names, const char *const *patterns, int count,
    const char *default_location);
bool rays_sdl3_take_dialog(rays_dialog_result *result);
typedef void (*rays_sdl3_live_resize_callback)(void);
void rays_sdl3_set_live_resize_callback(rays_sdl3_live_resize_callback callback);

bool rays_sdl3_test_polling_event_queue(void);
void rays_sdl3_test_queue_resize_during_poll(void);

bool rays_sdl3_poll_event(rays_sdl3_event *event);
bool rays_sdl3_wait_event_timeout(int timeout_ms);
void rays_sdl3_event_free(rays_sdl3_event *event);
bool rays_sdl3_scroll_take(double *x, double *y, int *phase, double *seconds);

#endif
