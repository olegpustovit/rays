#include "sdl3_platform.h"

#include <CoreFoundation/CoreFoundation.h>
#include <pthread.h>
#include <string.h>
#include <objc/message.h>
#include <objc/runtime.h>

/* An OCaml program has no AppKit run loop, so no autorelease pool encloses
   these calls: an object AppKit autoreleases here (the window itself, many
   times over, while its title bar is restyled) would stay in the thread's
   root page and never be released. Every function below that messages AppKit
   brackets itself, as SDL's own Cocoa functions do. */
extern void *objc_autoreleasePoolPush(void);
extern void objc_autoreleasePoolPop(void *pool);

bool rays_is_main_thread(void)
{
  return pthread_main_np() && SDL_IsMainThread();
}

static void retain_layer(void *layer) { CFRetain(layer); }
static void release_layer(void *layer) { CFRelease(layer); }

value rays_sdl_layer_token(void *layer, uint64_t owner, uint64_t generation)
{
  return rays_native_layer_token_create(layer, owner, generation,
      retain_layer, release_layer);
}

#define RAYS_SCROLL_QUEUE 256
static rays_scroll scroll_queue[RAYS_SCROLL_QUEUE];
static int scroll_head, scroll_count;
static bool scroll_installed;
static SEL sel_phase, sel_momentum, sel_precise, sel_dx, sel_dy, sel_timestamp;

static bool cocoa_driver(void)
{
  const char *driver = SDL_GetCurrentVideoDriver();
  return driver != NULL && strcmp(driver, "cocoa") == 0;
}

static unsigned long event_ulong(id event, SEL selector)
{
  return ((unsigned long (*)(id, SEL))objc_msgSend)(event, selector);
}

static double event_double(id event, SEL selector)
{
  return ((double (*)(id, SEL))objc_msgSend)(event, selector);
}

/* NSEventPhase bits. */
enum {
  PHASE_BEGAN = 1, PHASE_CHANGED = 4, PHASE_ENDED = 8, PHASE_CANCELLED = 16,
  PHASE_MAY_BEGIN = 32
};

static void scroll_push(id event)
{
  unsigned long phase = event_ulong(event, sel_phase);
  unsigned long momentum = event_ulong(event, sel_momentum);
  rays_scroll item;
  if (!((signed char (*)(id, SEL))objc_msgSend)(event, sel_precise)) return;
  item.x = -event_double(event, sel_dx);
  item.y = event_double(event, sel_dy);
  item.seconds = event_double(event, sel_timestamp);
  if (momentum != 0) {
    if (item.x == 0. && item.y == 0.) return;
    item.phase = RAYS_SCROLL_MOMENTUM;
  } else if ((phase & (PHASE_BEGAN | PHASE_MAY_BEGIN)) != 0) {
    item.phase = RAYS_SCROLL_BEGAN;
  } else if ((phase & PHASE_CHANGED) != 0) {
    item.phase = RAYS_SCROLL_CHANGED;
  } else if ((phase & (PHASE_ENDED | PHASE_CANCELLED)) != 0) {
    item.phase = RAYS_SCROLL_ENDED;
  } else {
    return; /* an unphased wheel, or fingers resting: SDL's event is enough */
  }
  if (scroll_count == RAYS_SCROLL_QUEUE) { /* drop the oldest */
    scroll_head = (scroll_head + 1) % RAYS_SCROLL_QUEUE;
    scroll_count--;
  }
  scroll_queue[(scroll_head + scroll_count) % RAYS_SCROLL_QUEUE] = item;
  scroll_count++;
}

void rays_scroll_monitor_install(void)
{
  const unsigned long long scroll_wheel_mask = 1ULL << 22;
  Class events = objc_getClass("NSEvent");
  id monitor;
  void *pool;
  if (scroll_installed || !cocoa_driver() || events == Nil) return;
  pool = objc_autoreleasePoolPush();
  sel_phase = sel_getUid("phase");
  sel_momentum = sel_getUid("momentumPhase");
  sel_precise = sel_getUid("hasPreciseScrollingDeltas");
  sel_dx = sel_getUid("scrollingDeltaX");
  sel_dy = sel_getUid("scrollingDeltaY");
  sel_timestamp = sel_getUid("timestamp");
  monitor = ((id (*)(id, SEL, unsigned long long, id (^)(id)))objc_msgSend)(
      (id)events, sel_getUid("addLocalMonitorForEventsMatchingMask:handler:"),
      scroll_wheel_mask, ^id(id event) { scroll_push(event); return event; });
  scroll_installed = monitor != nil; /* AppKit keeps the monitor until removed */
  objc_autoreleasePoolPop(pool);
}

bool rays_scroll_take(rays_scroll *out)
{
  if (scroll_count == 0) return false;
  *out = scroll_queue[scroll_head];
  scroll_head = (scroll_head + 1) % RAYS_SCROLL_QUEUE;
  scroll_count--;
  return true;
}

static id cocoa_window(SDL_Window *window)
{
  if (window == NULL || !cocoa_driver()) return nil;
  return (id)SDL_GetPointerProperty(SDL_GetWindowProperties(window),
      SDL_PROP_WINDOW_COCOA_WINDOW_POINTER, NULL);
}

void rays_window_plain_titlebar(SDL_Window *window)
{
  id native = cocoa_window(window);
  void *pool;
  if (native == nil) return;
  pool = objc_autoreleasePoolPush();
  ((void (*)(id, SEL, signed char))objc_msgSend)(
      native, sel_getUid("setTitlebarAppearsTransparent:"), 1);
  ((void (*)(id, SEL, long))objc_msgSend)(
      native, sel_getUid("setTitleVisibility:"), 1 /* NSWindowTitleHidden */);
  objc_autoreleasePoolPop(pool);
}

void rays_app_activate(void)
{
  Class app_class = objc_getClass("NSApplication");
  id app;
  void *pool;
  if (app_class == Nil || !cocoa_driver()) return;
  pool = objc_autoreleasePoolPush();
  app = ((id (*)(id, SEL))objc_msgSend)((id)app_class, sel_getUid("sharedApplication"));
  ((void (*)(id, SEL, signed char))objc_msgSend)(
      app, sel_getUid("activateIgnoringOtherApps:"), 1);
  objc_autoreleasePoolPop(pool);
}

void rays_window_set_background(SDL_Window *window,
    double red, double green, double blue)
{
  id native = cocoa_window(window);
  Class colors = objc_getClass("NSColor");
  id color;
  void *pool;
  if (native == nil || colors == Nil) return;
  pool = objc_autoreleasePoolPush();
  color = ((id (*)(id, SEL, double, double, double, double))objc_msgSend)(
      (id)colors, sel_getUid("colorWithSRGBRed:green:blue:alpha:"),
      red, green, blue, 1.);
  ((void (*)(id, SEL, id))objc_msgSend)(
      native, sel_getUid("setBackgroundColor:"), color);
  objc_autoreleasePoolPop(pool);
}
