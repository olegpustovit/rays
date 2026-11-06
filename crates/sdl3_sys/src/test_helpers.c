#include "bridge.h"
#include "sdl3_abi.h"

#include <stdlib.h>
#include <string.h>

static char input_text[] = "\xC5\xBE" "irafa";
static char editing_text[] = "\xC4\x8D";
static char drop_path[] = "/tmp/\xC5\xBE" "aba.png";
static char drop_other[] = "dropped text";

static bool push(SDL_Event *event)
{
  SDL_SetEventEnabled(event->type, true);
  return SDL_PushEvent(event);
}

static bool push_key(Uint32 type, SDL_Keycode key, SDL_Scancode scancode,
    SDL_Keymod mod, bool down, bool repeat)
{
  SDL_Event event;
  SDL_zero(event);
  event.key.type = type;
  event.key.key = key;
  event.key.scancode = scancode;
  event.key.mod = mod;
  event.key.down = down;
  event.key.repeat = repeat;
  return push(&event);
}

static bool push_window(Uint32 type, int data1, int data2)
{
  SDL_Event event;
  SDL_zero(event);
  event.window.type = type;
  event.window.data1 = data1;
  event.window.data2 = data2;
  return push(&event);
}

static bool push_button(Uint32 type, Uint8 button, bool down, float x, float y)
{
  SDL_Event event;
  SDL_zero(event);
  event.button.type = type;
  event.button.button = button;
  event.button.down = down;
  event.button.x = x;
  event.button.y = y;
  return push(&event);
}

static bool push_drop(Uint32 type, const char *data, float x, float y)
{
  SDL_Event event;
  SDL_zero(event);
  event.drop.type = type;
  event.drop.data = type == SDL_EVENT_DROP_FILE && data != NULL ? SDL_strdup(data) : (char *)data;
  event.drop.x = x;
  event.drop.y = y;
  return push(&event);
}

static bool push_pinch(Uint32 type, float scale)
{
  SDL_Event event;
  SDL_zero(event);
  event.pinch.type = type;
  event.pinch.scale = scale;
  return push(&event);
}

bool rays_sdl3_test_push_event_trace(void)
{
  SDL_Event event;
  bool ok = true;
  ok = push_key(SDL_EVENT_KEY_DOWN, SDLK_A, SDL_SCANCODE_A,
      SDL_KMOD_LSHIFT | SDL_KMOD_CAPS, true, false) && ok;
  ok = push_key(SDL_EVENT_KEY_UP, SDLK_A, SDL_SCANCODE_A, 0, false, false) && ok;
  ok = push_key(SDL_EVENT_KEY_DOWN, SDLK_UP, SDL_SCANCODE_UP, 0, true, true) && ok;
  ok = push_key(SDL_EVENT_KEY_DOWN, SDLK_ESCAPE, SDL_SCANCODE_ESCAPE, 0, true, false) && ok;
  ok = push_key(SDL_EVENT_KEY_DOWN, SDLK_F5, SDL_SCANCODE_F5, 0, true, false) && ok;
  ok = push_key(SDL_EVENT_KEY_DOWN, SDLK_RSHIFT, SDL_SCANCODE_RSHIFT, SDL_KMOD_RSHIFT, true, false) && ok;
  ok = push_key(SDL_EVENT_KEY_DOWN, SDLK_LGUI, SDL_SCANCODE_LGUI, SDL_KMOD_LGUI, true, false) && ok;
  ok = push_key(SDL_EVENT_KEY_DOWN, SDLK_DELETE, SDL_SCANCODE_DELETE, 0, true, false) && ok;
  ok = push_key(SDL_EVENT_KEY_DOWN, SDLK_KP_ENTER, SDL_SCANCODE_KP_ENTER, 0, true, false) && ok;
  ok = push_key(SDL_EVENT_KEY_DOWN, SDLK_SPACE, SDL_SCANCODE_SPACE, 0, true, false) && ok;
  ok = push_key(SDL_EVENT_KEY_DOWN, 'Z', SDL_SCANCODE_Z, 0, true, false) && ok;
  ok = push_key(SDL_EVENT_KEY_DOWN, 0x444, SDL_SCANCODE_A, SDL_KMOD_LGUI, true, false) && ok;
  ok = push_key(SDL_EVENT_KEY_DOWN, 0x444, SDL_SCANCODE_7, 0, true, false) && ok;
  ok = push_key(SDL_EVENT_KEY_DOWN, 0x444, SDL_SCANCODE_GRAVE, 0, true, false) && ok;

  SDL_zero(event);
  event.text.type = SDL_EVENT_TEXT_INPUT;
  event.text.text = input_text;
  ok = push(&event) && ok;
  SDL_zero(event);
  event.edit.type = SDL_EVENT_TEXT_EDITING;
  event.edit.text = editing_text;
  event.edit.start = 1;
  event.edit.length = 2;
  ok = push(&event) && ok;

  SDL_zero(event); event.pproximity.type = SDL_EVENT_PEN_PROXIMITY_IN; ok = push(&event) && ok;
  SDL_zero(event); event.tfinger.type = SDL_EVENT_FINGER_DOWN; ok = push(&event) && ok;
  SDL_zero(event); event.gbutton.type = SDL_EVENT_GAMEPAD_BUTTON_DOWN; ok = push(&event) && ok;
  SDL_zero(event); event.adevice.type = SDL_EVENT_AUDIO_DEVICE_ADDED; ok = push(&event) && ok;
  SDL_zero(event); event.display.type = SDL_EVENT_DISPLAY_ADDED; ok = push(&event) && ok;
  SDL_zero(event); event.clipboard.type = SDL_EVENT_CLIPBOARD_UPDATE; ok = push(&event) && ok;
  SDL_zero(event); event.common.type = SDL_EVENT_USER; ok = push(&event) && ok;
  ok = push_window(SDL_EVENT_WINDOW_MOVED, 1, 2) && ok;
  ok = push_window(SDL_EVENT_WINDOW_EXPOSED, 0, 0) && ok;
  ok = push_button(SDL_EVENT_MOUSE_BUTTON_DOWN, 9, true, 1.f, 1.f) && ok;
  ok = push_drop(SDL_EVENT_DROP_TEXT, drop_other, 1.f, 1.f) && ok;
  ok = push_drop(SDL_EVENT_DROP_FILE, NULL, 1.f, 1.f) && ok;

  SDL_zero(event);
  event.motion.type = SDL_EVENT_MOUSE_MOTION;
  event.motion.x = 10.5f; event.motion.y = 20.5f;
  event.motion.xrel = 3.0f; event.motion.yrel = -2.0f;
  ok = push(&event) && ok;
  event.motion.x = 11.5f; event.motion.y = 22.5f;
  event.motion.xrel = 4.0f; event.motion.yrel = 5.0f;
  ok = push(&event) && ok;
  ok = push_button(SDL_EVENT_MOUSE_BUTTON_DOWN, SDL_BUTTON_LEFT, true, 30.f, 40.f) && ok;
  ok = push_button(SDL_EVENT_MOUSE_BUTTON_UP, SDL_BUTTON_MIDDLE, false, 31.f, 41.f) && ok;
  ok = push_button(SDL_EVENT_MOUSE_BUTTON_DOWN, SDL_BUTTON_RIGHT, true, 32.f, 42.f) && ok;
  ok = push_button(SDL_EVENT_MOUSE_BUTTON_DOWN, SDL_BUTTON_X1, true, 33.f, 43.f) && ok;
  ok = push_button(SDL_EVENT_MOUSE_BUTTON_UP, SDL_BUTTON_X2, false, 34.f, 44.f) && ok;
  SDL_zero(event);
  event.wheel.type = SDL_EVENT_MOUSE_WHEEL;
  event.wheel.x = 1.0f; event.wheel.y = -2.0f;
  event.wheel.direction = SDL_MOUSEWHEEL_FLIPPED;
  event.wheel.mouse_x = 50.0f; event.wheel.mouse_y = 60.0f;
  event.wheel.integer_x = 1; event.wheel.integer_y = -2;
  ok = push(&event) && ok;

  ok = push_window(SDL_EVENT_WINDOW_SHOWN, 0, 0) && ok;
  ok = push_window(SDL_EVENT_WINDOW_HIDDEN, 0, 0) && ok;
  ok = push_window(SDL_EVENT_WINDOW_MINIMIZED, 0, 0) && ok;
  ok = push_window(SDL_EVENT_WINDOW_RESTORED, 0, 0) && ok;
  ok = push_window(SDL_EVENT_WINDOW_OCCLUDED, 0, 0) && ok;
  ok = push_window(SDL_EVENT_WINDOW_FOCUS_GAINED, 0, 0) && ok;
  ok = push_window(SDL_EVENT_WINDOW_FOCUS_LOST, 0, 0) && ok;
  ok = push_window(SDL_EVENT_WINDOW_RESIZED, 640, 480) && ok;
  ok = push_window(SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED, 1280, 960) && ok;
  ok = push_window(SDL_EVENT_WINDOW_CLOSE_REQUESTED, 0, 0) && ok;

  ok = push_pinch(SDL_EVENT_PINCH_BEGIN, 1.0f) && ok;
  ok = push_pinch(SDL_EVENT_PINCH_UPDATE, 1.25f) && ok;
  ok = push_pinch(SDL_EVENT_PINCH_END, 1.0f) && ok;
  ok = push_drop(SDL_EVENT_DROP_BEGIN, NULL, 0.f, 0.f) && ok;
  ok = push_drop(SDL_EVENT_DROP_POSITION, NULL, 5.5f, 6.5f) && ok;
  ok = push_drop(SDL_EVENT_DROP_FILE, drop_path, 30.f, 40.f) && ok;
  ok = push_drop(SDL_EVENT_DROP_COMPLETE, NULL, 0.f, 0.f) && ok;
  SDL_zero(event);
  event.quit.type = SDL_EVENT_QUIT;
  ok = push(&event) && ok;
  return ok;
}

void rays_sdl3_test_mutate_event_sources(void)
{
  input_text[0] = 'x';
  editing_text[0] = 'x';
  drop_path[0] = 'x';
}

bool rays_sdl3_test_push_resize_burst(void)
{
  bool ok = true;
  ok = push_window(SDL_EVENT_WINDOW_RESIZED, 640, 480) && ok;
  ok = push_window(SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED, 1280, 960) && ok;
  ok = push_window(SDL_EVENT_WINDOW_RESIZED, 800, 600) && ok;
  return ok;
}

#define TEST_SLOTS 16
static void *test_slots[TEST_SLOTS];
static int test_ids[TEST_SLOTS];

int rays_sdl3_test_dialog_reserve(void)
{
  static const char *names[] = { "Images", "All" };
  static const char *patterns[] = { "png;jpg", "*" };
  int id = 0, index;
  void *slot = rays_dialog_reserve(names, patterns, 2, "/tmp/\xC5\xBE", &id);
  if (slot == NULL) return 0;
  for (index = 0; index < TEST_SLOTS; index++) {
    if (test_slots[index] == NULL) {
      test_slots[index] = slot;
      test_ids[index] = id;
      return id;
    }
  }
  rays_dialog_abandon(slot);
  return 0;
}

static void *take_test_slot(int id)
{
  int index;
  for (index = 0; index < TEST_SLOTS; index++) {
    if (test_slots[index] != NULL && test_ids[index] == id) {
      void *slot = test_slots[index];
      test_slots[index] = NULL;
      return slot;
    }
  }
  return NULL;
}

void rays_sdl3_test_dialog_abandon(int id)
{
  void *slot = take_test_slot(id);
  if (slot != NULL) rays_dialog_abandon(slot);
}

typedef struct {
  void *slot;
  const char *const *paths;
  const char *error;
} finish_request;

static int SDLCALL finish_on_thread(void *data)
{
  finish_request *request = (finish_request *)data;
  if (request->error != NULL) SDL_SetError("%s", request->error);
  rays_dialog_callback(request->slot, request->paths, -1);
  return 0;
}

static bool finish_in_thread(finish_request *request)
{
  SDL_Thread *thread = SDL_CreateThread(finish_on_thread, "dialog-callback", request);
  if (thread == NULL) return false;
  SDL_WaitThread(thread, NULL);
  return true;
}

bool rays_sdl3_test_dialog_finish(int id, const char *const *paths, int count, bool failed)
{
  finish_request request;
  const char *list[8];
  void *slot;
  int index;
  if (count > 7 || count < 0) return false;
  slot = take_test_slot(id);
  if (slot == NULL) return false;
  request.slot = slot;
  request.error = failed ? "boom" : NULL;
  request.paths = NULL;
  if (!failed) {
    for (index = 0; index < count; index++) list[index] = paths[index];
    list[count] = NULL;
    request.paths = list;
  }
  if (!finish_in_thread(&request)) {
    rays_dialog_abandon(slot);
    return false;
  }
  return true;
}

bool rays_sdl3_test_dialog_finish_huge(int id)
{
  enum { HUGE_COUNT = 5000 };
  finish_request request;
  const char **list = (const char **)malloc((HUGE_COUNT + 1) * sizeof(char *));
  void *slot;
  int index;
  bool success;
  if (list == NULL) return false;
  slot = take_test_slot(id);
  if (slot == NULL) { free(list); return false; }
  for (index = 0; index < HUGE_COUNT; index++) list[index] = "/tmp/f";
  list[HUGE_COUNT] = NULL;
  request.slot = slot;
  request.paths = list;
  request.error = NULL;
  success = finish_in_thread(&request);
  free(list);
  if (!success) rays_dialog_abandon(slot);
  return success;
}

#ifdef RAYS_SDL3_MACOS
#include <ApplicationServices/ApplicationServices.h>
#include <objc/message.h>
#include <objc/runtime.h>

void rays_sdl3_test_post_scroll(int points, int phase)
{
  CGEventRef event = CGEventCreateScrollWheelEvent(NULL, kCGScrollEventUnitPixel, 1, (int32_t)points);
  id native, app;
  if (event == NULL) return;
  CGEventSetIntegerValueField(event, kCGScrollWheelEventIsContinuous, 1);
  CGEventSetIntegerValueField(event, kCGScrollWheelEventScrollPhase, phase);
  native = ((id (*)(id, SEL, CGEventRef))objc_msgSend)(
      (id)objc_getClass("NSEvent"), sel_getUid("eventWithCGEvent:"), event);
  app = ((id (*)(id, SEL))objc_msgSend)((id)objc_getClass("NSApplication"), sel_getUid("sharedApplication"));
  ((void (*)(id, SEL, id, signed char))objc_msgSend)(app, sel_getUid("postEvent:atStart:"), native, 0);
  CFRelease(event);
}
#else
void rays_sdl3_test_post_scroll(int points, int phase) { (void)points; (void)phase; }
#endif
