/* What SDL3 does not carry from AppKit, read through the Objective-C runtime
   so the stubs stay C. Every function is a no-op off the Cocoa video driver
   (the dummy driver has no NSWindow and no NSEvent). Initial domain only. */
#ifndef RAYS_SDL3_COCOA_H
#define RAYS_SDL3_COCOA_H

#include <stdbool.h>
#include <SDL3/SDL.h>

/* Sdl3.Event.scroll_phase: all constants, in this order. */
typedef enum {
  RAYS_SCROLL_BEGAN, RAYS_SCROLL_CHANGED, RAYS_SCROLL_ENDED,
  RAYS_SCROLL_MOMENTUM
} rays_scroll_phase;

/* One phased trackpad scroll NSEvent: its scrollingDeltaX/Y in points with
   SDL's wheel signs (x negated), and its timestamp in seconds. */
typedef struct {
  double x, y, seconds;
  rays_scroll_phase phase;
} rays_scroll;

/* SDL's wheel event multiplies a trackpad's points by 0.1 and drops the
   gesture phases, the lift (a zero delta is not sent) and the difference
   between the fingers and the system's momentum. A local NSEvent monitor
   queues those for phased events; SDL's own wheel events are untouched. The
   monitor's block only writes the fixed ring below, on the thread that pumps
   events. Idempotent. */
void rays_scroll_monitor_install(void);

/* Take the oldest queued scroll, if any. */
bool rays_scroll_take(rays_scroll *out);

/* Keep the traffic lights, hide the title and let the title bar show the
   window's background colour. */
void rays_window_plain_titlebar(SDL_Window *window);

/* Make the process the active app: one started from a terminal is not, so
   its raised window would stay behind the shell. */
void rays_app_activate(void);

/* The window background, which a plain title bar shows (sRGB, 0..1). */
void rays_window_set_background(SDL_Window *window,
    double red, double green, double blue);

#endif
