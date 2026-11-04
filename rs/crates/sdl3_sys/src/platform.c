#include "bridge.h"

#if defined(RAYS_SDL3_MACOS)
#include <CoreFoundation/CoreFoundation.h>
#include <pthread.h>
#include <string.h>
#include <objc/message.h>
#include <objc/runtime.h>

extern void *objc_autoreleasePoolPush(void);
extern void objc_autoreleasePoolPop(void *pool);

bool rays_sdl3_is_main_thread(void)
{
  return pthread_main_np() && SDL_IsMainThread();
}

void rays_sdl3_retain_layer(void *layer) { CFRetain(layer); }
void rays_sdl3_release_layer(void *layer) { CFRelease(layer); }

#define SCROLL_QUEUE 256
typedef struct { double x, y, seconds; int phase; } scroll_item;
static scroll_item scroll_queue[SCROLL_QUEUE];
static int scroll_head, scroll_count;
static bool scroll_installed;
static SEL sel_phase, sel_momentum, sel_precise, sel_dx, sel_dy, sel_timestamp;

enum { PHASE_BEGAN = 1, PHASE_CHANGED = 4, PHASE_ENDED = 8,
       PHASE_CANCELLED = 16, PHASE_MAY_BEGIN = 32 };



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

static void scroll_push(id event)
{
  unsigned long phase = event_ulong(event, sel_phase);
  unsigned long momentum = event_ulong(event, sel_momentum);
  scroll_item item;
  if (!((signed char (*)(id, SEL))objc_msgSend)(event, sel_precise)) return;
  item.x = -event_double(event, sel_dx);
  item.y = event_double(event, sel_dy);
  item.seconds = event_double(event, sel_timestamp);
  if (momentum != 0) {
    if (item.x == 0. && item.y == 0.) return;
    item.phase = 3;
  } else if ((phase & (PHASE_BEGAN | PHASE_MAY_BEGIN)) != 0) {
    item.phase = 0;
  } else if ((phase & PHASE_CHANGED) != 0) {
    item.phase = 1;
  } else if ((phase & (PHASE_ENDED | PHASE_CANCELLED)) != 0) {
    item.phase = 2;
  } else {
    return;
  }
  if (scroll_count == SCROLL_QUEUE) {
    scroll_head = (scroll_head + 1) % SCROLL_QUEUE;
    scroll_count--;
  }
  scroll_queue[(scroll_head + scroll_count) % SCROLL_QUEUE] = item;
  scroll_count++;
}

void rays_sdl3_scroll_monitor_install(void)
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
  scroll_installed = monitor != nil;
  objc_autoreleasePoolPop(pool);
}

bool rays_sdl3_scroll_take(double *x, double *y, int *phase, double *seconds)
{
  scroll_item item;
  if (scroll_count == 0) return false;
  item = scroll_queue[scroll_head];
  scroll_head = (scroll_head + 1) % SCROLL_QUEUE;
  scroll_count--;
  *x = item.x;
  *y = item.y;
  *phase = item.phase;
  *seconds = item.seconds;
  return true;
}

static id cocoa_window(SDL_Window *window)
{
  if (window == NULL || !cocoa_driver()) return nil;
  return (id)SDL_GetPointerProperty(SDL_GetWindowProperties(window),
      SDL_PROP_WINDOW_COCOA_WINDOW_POINTER, NULL);
}

void rays_sdl3_window_plain_titlebar(SDL_Window *window)
{
  id native = cocoa_window(window);
  void *pool;
  if (native == nil) return;
  pool = objc_autoreleasePoolPush();
  ((void (*)(id, SEL, signed char))objc_msgSend)(
      native, sel_getUid("setTitlebarAppearsTransparent:"), 1);
  ((void (*)(id, SEL, long))objc_msgSend)(
      native, sel_getUid("setTitleVisibility:"), 1);
  objc_autoreleasePoolPop(pool);
}

void rays_sdl3_app_activate(void)
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

void rays_sdl3_platform_set_window_background(SDL_Window *window,
    double red, double green, double blue)
{
  id native = cocoa_window(window);
  Class colors = objc_getClass("NSColor");
  id color;
  void *pool;
  if (native == nil || colors == Nil) return;
  pool = objc_autoreleasePoolPush();
  color = ((id (*)(id, SEL, double, double, double, double))objc_msgSend)(
      (id)colors, sel_getUid("colorWithSRGBRed:green:blue:alpha:"), red, green, blue, 1.);
  ((void (*)(id, SEL, id))objc_msgSend)(native, sel_getUid("setBackgroundColor:"), color);
  objc_autoreleasePoolPop(pool);
}

#else

static SDL_ThreadID initial_thread;

#if defined(_MSC_VER)
static void __cdecl rays_sdl3_record_initial_thread(void)
{
  initial_thread = SDL_GetCurrentThreadID();
}
#pragma section(".CRT$XCU", read)
__declspec(allocate(".CRT$XCU")) static void (__cdecl *rays_sdl3_initializer)(void) = rays_sdl3_record_initial_thread;
#elif defined(__GNUC__) || defined(__clang__)
__attribute__((constructor)) static void rays_sdl3_record_initial_thread(void)
{
  initial_thread = SDL_GetCurrentThreadID();
}
#endif

bool rays_sdl3_is_main_thread(void)
{
  if (initial_thread == 0) initial_thread = SDL_GetCurrentThreadID();
  return SDL_GetCurrentThreadID() == initial_thread;
}

void rays_sdl3_retain_layer(void *layer) { (void)layer; }
void rays_sdl3_release_layer(void *layer) { (void)layer; }
void rays_sdl3_scroll_monitor_install(void) {}
bool rays_sdl3_scroll_take(double *x, double *y, int *phase, double *seconds)
{
  (void)x; (void)y; (void)phase; (void)seconds;
  return false;
}
void rays_sdl3_window_plain_titlebar(SDL_Window *window) { (void)window; }
void rays_sdl3_app_activate(void) {}
void rays_sdl3_platform_set_window_background(SDL_Window *window,
    double red, double green, double blue)
{
  (void)window; (void)red; (void)green; (void)blue;
}

#endif

SDL_MetalView rays_sdl3_create_metal_view(SDL_Window *window)
{
#ifdef _WIN32
  return (SDL_MetalView)SDL_GetPointerProperty(SDL_GetWindowProperties(window),
      SDL_PROP_WINDOW_WIN32_HWND_POINTER, NULL);
#else
  return SDL_Metal_CreateView(window);
#endif
}

void rays_sdl3_destroy_metal_view(SDL_MetalView view)
{
#ifndef _WIN32
  SDL_Metal_DestroyView(view);
#else
  (void)view;
#endif
}

void *rays_sdl3_metal_layer(SDL_MetalView view)
{
#ifdef _WIN32
  return (void *)view;
#else
  return SDL_Metal_GetLayer(view);
#endif
}
