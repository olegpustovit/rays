#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#ifdef __APPLE__
#include <pthread.h>
#endif

#include <SDL3/SDL.h>
#include <SDL3/SDL_metal.h>

#include "generated_abi.h"
#include "sdl3_dialog.h"
#include "sdl3_cocoa.h"
#include "../native_layer_token/native_layer_token.h"

static SDL_Window *window_of_value(value raw)
{
  return (SDL_Window *)(intnat)Nativeint_val(raw);
}

CAMLprim value caml_sdl3_linked_version(value unit)
{
  (void)unit;
  return Val_int(SDL_GetVersion());
}

CAMLprim value caml_sdl3_compiled_version(value unit)
{
  (void)unit;
  return Val_int(SDL_VERSION);
}

CAMLprim value caml_sdl3_get_error(value unit)
{
  const char *message;
  CAMLparam1(unit);
  message = SDL_GetError();
  CAMLreturn(caml_copy_string(message != NULL ? message : ""));
}

CAMLprim value caml_sdl3_clear_error(value unit)
{
  (void)unit;
  SDL_ClearError();
  return Val_unit;
}

CAMLprim value caml_sdl3_is_main_thread(value unit)
{
  (void)unit;
#ifdef __APPLE__
  /* SDL accepts every thread before initialization; headless resources must
     still run on the platform main thread. */
  if (!pthread_main_np()) return Val_false;
#endif
  return Val_bool(SDL_IsMainThread());
}

/* Sdl3.Init.subsystem: OCaml names a set by bits, here are SDL's flags. */
enum { INIT_VIDEO = 1, INIT_EVENTS = 2 };

static SDL_InitFlags init_flags_of_value(value bits)
{
  SDL_InitFlags flags = 0;
  if ((Int_val(bits) & INIT_VIDEO) != 0) flags |= SDL_INIT_VIDEO;
  if ((Int_val(bits) & INIT_EVENTS) != 0) flags |= SDL_INIT_EVENTS;
  return flags;
}

CAMLprim value caml_sdl3_init_subsystem(value bits)
{
  return Val_bool(SDL_InitSubSystem(init_flags_of_value(bits)));
}

CAMLprim value caml_sdl3_quit_subsystem(value bits)
{
  SDL_QuitSubSystem(init_flags_of_value(bits));
  return Val_unit;
}

/* Sdl3.Window.flag bits. */
enum { WINDOW_FLAG_HIDDEN = 1, WINDOW_FLAG_HIGH_PIXEL_DENSITY = 2, WINDOW_FLAG_METAL = 4 };

CAMLprim value caml_sdl3_create_window(
    value title, value width, value height, value flags)
{
  SDL_Window *window;
  SDL_WindowFlags native = 0;
  CAMLparam4(title, width, height, flags);
  if ((Int_val(flags) & WINDOW_FLAG_HIDDEN) != 0) native |= SDL_WINDOW_HIDDEN;
  if ((Int_val(flags) & WINDOW_FLAG_HIGH_PIXEL_DENSITY) != 0) native |= SDL_WINDOW_HIGH_PIXEL_DENSITY;
  if ((Int_val(flags) & WINDOW_FLAG_METAL) != 0) native |= SDL_WINDOW_METAL;
  window = SDL_CreateWindow(
      String_val(title), Int_val(width), Int_val(height), native);
  rays_window_plain_titlebar(window);
  rays_scroll_monitor_install();
  CAMLreturn(caml_copy_nativeint((intnat)window));
}

/* Sdl3.Window.set_background: unboxed floats would need a second entry
   point; this is called when a theme changes, not per frame. */
CAMLprim value caml_sdl3_set_window_background(
    value raw, value red, value green, value blue)
{
  rays_window_set_background(window_of_value(raw),
      Double_val(red), Double_val(green), Double_val(blue));
  return Val_unit;
}

CAMLprim value caml_sdl3_destroy_window(value raw)
{
  SDL_DestroyWindow(window_of_value(raw));
  return Val_unit;
}

static value copy_size_result(bool success, int width, int height)
{
  CAMLparam0();
  CAMLlocal2(pair, some);
  if (!success) {
    CAMLreturn(Val_none);
  }
  pair = caml_alloc_tuple(2);
  Store_field(pair, 0, Val_int(width));
  Store_field(pair, 1, Val_int(height));
  some = caml_alloc(1, 0);
  Store_field(some, 0, pair);
  CAMLreturn(some);
}

CAMLprim value caml_sdl3_window_size(value raw)
{
  int width = 0;
  int height = 0;
  bool success = SDL_GetWindowSize(window_of_value(raw), &width, &height);
  return copy_size_result(success, width, height);
}

CAMLprim value caml_sdl3_window_size_in_pixels(value raw)
{
  int width = 0;
  int height = 0;
  bool success = SDL_GetWindowSizeInPixels(
      window_of_value(raw), &width, &height);
  return copy_size_result(success, width, height);
}

/* Sdl3.Window.state, as bits: hidden, minimized, occluded. */
CAMLprim value caml_sdl3_window_state(value raw)
{
  SDL_WindowFlags flags = SDL_GetWindowFlags(window_of_value(raw));
  return Val_int(((flags & SDL_WINDOW_HIDDEN) != 0 ? 1 : 0)
      | ((flags & SDL_WINDOW_MINIMIZED) != 0 ? 2 : 0)
      | ((flags & SDL_WINDOW_OCCLUDED) != 0 ? 4 : 0));
}

CAMLprim value caml_sdl3_show_window(value raw)
{
  return Val_bool(SDL_ShowWindow(window_of_value(raw)));
}

CAMLprim value caml_sdl3_raise_window(value raw)
{
  rays_app_activate();
  return Val_bool(SDL_RaiseWindow(window_of_value(raw)));
}

CAMLprim value caml_sdl3_hide_window(value raw)
{
  return Val_bool(SDL_HideWindow(window_of_value(raw)));
}

CAMLprim value caml_sdl3_window_id(value raw)
{
  CAMLparam1(raw);
  CAMLreturn(caml_copy_int64(SDL_GetWindowID(window_of_value(raw))));
}

CAMLprim value caml_sdl3_window_display(value raw)
{
  CAMLparam1(raw);
  CAMLreturn(caml_copy_int64(SDL_GetDisplayForWindow(window_of_value(raw))));
}

CAMLprim value caml_sdl3_window_pixel_density(value raw)
{
  CAMLparam1(raw);
  CAMLreturn(caml_copy_double(SDL_GetWindowPixelDensity(window_of_value(raw))));
}

CAMLprim value caml_sdl3_window_display_scale(value raw)
{
  CAMLparam1(raw);
  CAMLreturn(caml_copy_double(SDL_GetWindowDisplayScale(window_of_value(raw))));
}

CAMLprim value caml_sdl3_window_position(value raw)
{
  int x = 0;
  int y = 0;
  bool success = SDL_GetWindowPosition(window_of_value(raw), &x, &y);
  return copy_size_result(success, x, y);
}

CAMLprim value caml_sdl3_window_title(value raw)
{
  const char *title;
  CAMLparam1(raw);
  title = SDL_GetWindowTitle(window_of_value(raw));
  CAMLreturn(caml_copy_string(title != NULL ? title : ""));
}

CAMLprim value caml_sdl3_center_window(value raw)
{
  return Val_bool(SDL_SetWindowPosition(window_of_value(raw),
      SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED));
}

CAMLprim value caml_sdl3_set_window_resizable(value raw, value enabled)
{
  return Val_bool(SDL_SetWindowResizable(window_of_value(raw), Bool_val(enabled)));
}

CAMLprim value caml_sdl3_set_window_relative_mouse(value raw, value enabled)
{
  return Val_bool(SDL_SetWindowRelativeMouseMode(
      window_of_value(raw), Bool_val(enabled)));
}

CAMLprim value caml_sdl3_display_refresh_rate(value raw_id)
{
  const SDL_DisplayMode *mode;
  CAMLparam1(raw_id);
  CAMLlocal2(rate, some);
  mode = SDL_GetCurrentDisplayMode((SDL_DisplayID)Int64_val(raw_id));
  if (mode == NULL || !isfinite(mode->refresh_rate) ||
      mode->refresh_rate <= 0.0f) {
    CAMLreturn(Val_none);
  }
  rate = caml_copy_double(mode->refresh_rate);
  some = caml_alloc(1, 0);
  Store_field(some, 0, rate);
  CAMLreturn(some);
}

/* Sdl3.Cursor.shape, in declaration order. */
CAMLprim value caml_sdl3_create_system_cursor(value shape)
{
  static const SDL_SystemCursor shapes[] = {
    SDL_SYSTEM_CURSOR_DEFAULT, SDL_SYSTEM_CURSOR_TEXT,
    SDL_SYSTEM_CURSOR_EW_RESIZE, SDL_SYSTEM_CURSOR_NS_RESIZE
  };
  CAMLparam1(shape);
  CAMLreturn(caml_copy_nativeint((intnat)SDL_CreateSystemCursor(
      shapes[Int_val(shape)])));
}

CAMLprim value caml_sdl3_set_cursor(value raw)
{
  return Val_bool(SDL_SetCursor((SDL_Cursor *)(intnat)Nativeint_val(raw)));
}

CAMLprim value caml_sdl3_destroy_cursor(value raw)
{
  SDL_DestroyCursor((SDL_Cursor *)(intnat)Nativeint_val(raw));
  return Val_unit;
}

CAMLprim value caml_sdl3_set_window_size(value raw, value width, value height)
{
  return Val_bool(SDL_SetWindowSize(
      window_of_value(raw), Int_val(width), Int_val(height)));
}

CAMLprim value caml_sdl3_restore_window(value raw)
{
  return Val_bool(SDL_RestoreWindow(window_of_value(raw)));
}

CAMLprim value caml_sdl3_sync_window(value raw)
{
  SDL_Window *window;
  bool success;
  CAMLparam1(raw);
  window = window_of_value(raw);
  caml_release_runtime_system();
  success = SDL_SyncWindow(window);
  caml_acquire_runtime_system();
  CAMLreturn(Val_bool(success));
}

/* macOS: SDL turns a Control-click into a right click itself. */
CAMLprim value caml_sdl3_set_control_click_right_click(value enabled)
{
  return Val_bool(SDL_SetHint(SDL_HINT_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK,
      Bool_val(enabled) ? "1" : "0"));
}

CAMLprim value caml_sdl3_clipboard_set_text(value text)
{
  return Val_bool(SDL_SetClipboardText(String_val(text)));
}

CAMLprim value caml_sdl3_clipboard_get_text(value unit)
{
  char *text;
  CAMLparam1(unit);
  CAMLlocal2(copy, some);
  text = SDL_GetClipboardText();
  if (text == NULL) {
    CAMLreturn(Val_none);
  }
  copy = caml_copy_string(text);
  SDL_free(text);
  some = caml_alloc(1, 0);
  Store_field(some, 0, copy);
  CAMLreturn(some);
}

CAMLprim value caml_sdl3_start_text_input(value raw)
{
  return Val_bool(SDL_StartTextInput(window_of_value(raw)));
}

CAMLprim value caml_sdl3_stop_text_input(value raw)
{
  return Val_bool(SDL_StopTextInput(window_of_value(raw)));
}

CAMLprim value caml_sdl3_set_text_input_area(
    value raw, value x, value y, value width, value height, value cursor)
{
  SDL_Rect area;
  area.x = Int_val(x);
  area.y = Int_val(y);
  area.w = Int_val(width);
  area.h = Int_val(height);
  return Val_bool(SDL_SetTextInputArea(
      window_of_value(raw), &area, Int_val(cursor)));
}

/* Six arguments: bytecode passes them in an array. */
CAMLprim value caml_sdl3_set_text_input_area_bytecode(value *argv, int argc)
{
  (void)argc;
  return caml_sdl3_set_text_input_area(argv[0], argv[1], argv[2], argv[3],
      argv[4], argv[5]);
}

CAMLprim value caml_sdl3_create_metal_view(value raw_window)
{
  SDL_MetalView view;
  CAMLparam1(raw_window);
  view = SDL_Metal_CreateView(window_of_value(raw_window));
  CAMLreturn(caml_copy_nativeint((intnat)view));
}

CAMLprim value caml_sdl3_destroy_metal_view(value raw_view)
{
  SDL_Metal_DestroyView((SDL_MetalView)(intnat)Nativeint_val(raw_view));
  return Val_unit;
}

CAMLprim value caml_sdl3_metal_layer_token(value raw_view,value owner,value generation)
{ CAMLparam3(raw_view,owner,generation);void*layer=SDL_Metal_GetLayer((SDL_MetalView)(intnat)Nativeint_val(raw_view));CAMLreturn(rays_native_layer_token_create(layer,Int64_val(owner),Int64_val(generation))); }
CAMLprim value caml_sdl3_invalidate_metal_layer_token(value token)
{ rays_native_layer_token_invalidate(token);return Val_unit; }

/* Sdl3.Dialog.show: the slots and the callback are in sdl3_dialog.c, which
   knows nothing of OCaml. [filters] is an OCaml list of (name, pattern),
   at most 32 (the OCaml side checks); the strings are copied before this
   returns. Returns the dialog's id, or 0 with the SDL error set. */
#define DIALOG_MAX_FILTERS 32

CAMLprim value caml_sdl3_show_dialog(
    value raw_window, value kind, value filters, value default_location)
{
  CAMLparam4(raw_window, kind, filters, default_location);
  const char *names[DIALOG_MAX_FILTERS];
  const char *patterns[DIALOG_MAX_FILTERS];
  value cursor;
  int count = 0;
  for (cursor = filters;
       cursor != Val_emptylist && count < DIALOG_MAX_FILTERS;
       cursor = Field(cursor, 1)) {
    names[count] = String_val(Field(Field(cursor, 0), 0));
    patterns[count] = String_val(Field(Field(cursor, 0), 1));
    count++;
  }
  CAMLreturn(Val_int(rays_dialog_show(
      (rays_dialog_kind)Int_val(kind), window_of_value(raw_window),
      names, patterns, count,
      Is_block(default_location) ? String_val(Field(default_location, 0))
                                 : NULL)));
}

/* Sdl3.Event.dialog_outcome: Cancelled is the constant; Chosen and Failed
   are blocks 0 and 1. */
enum { OUTCOME_BLOCK_CHOSEN = 0, OUTCOME_BLOCK_FAILED = 1 };
enum { OUTCOME_CONSTANT_CANCELLED = 0 };

static value dialog_outcome_value(const rays_dialog_result *finished)
{
  CAMLparam0();
  CAMLlocal4(list, cell, text, outcome);
  switch (finished->outcome) {
  case RAYS_OUTCOME_CHOSEN: {
    /* the payload is NUL-terminated paths back to back: build the list from
       the last path to the first */
    size_t position = finished->payload_length;
    list = Val_emptylist;
    while (position > 0) {
      size_t start = position - 1;
      while (start > 0 && finished->payload[start - 1] != '\0') start--;
      text = caml_copy_string(finished->payload + start);
      cell = caml_alloc(2, 0);
      Store_field(cell, 0, text);
      Store_field(cell, 1, list);
      list = cell;
      position = start;
    }
    outcome = caml_alloc(1, OUTCOME_BLOCK_CHOSEN);
    Store_field(outcome, 0, list);
    break;
  }
  case RAYS_OUTCOME_CANCELLED:
    outcome = Val_int(OUTCOME_CONSTANT_CANCELLED);
    break;
  default:
    text = caml_copy_string(finished->payload != NULL ? finished->payload
        : "the file dialog failed");
    outcome = caml_alloc(1, OUTCOME_BLOCK_FAILED);
    Store_field(outcome, 0, text);
    break;
  }
  CAMLreturn(outcome);
}

/* Events. The stub turns the one SDL_Event union into the OCaml value
   Sdl3.Event.t while SDL still owns any pointer payloads, so no union and no
   raw SDL number crosses into OCaml: every constructor below is chosen with
   SDL's own macros.

   The OCaml and C sides share an order. A constant constructor is the index
   of its place among the constant constructors of its type, a constructor
   with arguments is a block tagged by its place among those. The enums below
   restate that order; test_sdl3_events pushes every kind through a real SDL
   queue and compares what comes back. */

/* Sdl3.Key.t: Char and Unknown carry a value; the rest are constants. */
enum { KEY_CHAR = 0, KEY_UNKNOWN = 1 };
enum {
  KEY_ARROW_UP, KEY_ARROW_DOWN, KEY_ARROW_LEFT, KEY_ARROW_RIGHT,
  KEY_SPACE, KEY_ENTER, KEY_ESCAPE, KEY_BACKSPACE, KEY_TAB,
  KEY_SHIFT, KEY_CONTROL, KEY_ALT, KEY_META,
  KEY_F1, KEY_F2, KEY_F3, KEY_F4, KEY_F5, KEY_F6,
  KEY_F7, KEY_F8, KEY_F9, KEY_F10, KEY_F11, KEY_F12,
  KEY_HOME, KEY_END, KEY_PAGE_UP, KEY_PAGE_DOWN, KEY_INSERT, KEY_DELETE
};

/* Sdl3.Key.modifier: all constants. */
enum {
  MODIFIER_SHIFT, MODIFIER_CONTROL, MODIFIER_ALT, MODIFIER_META,
  MODIFIER_NUM_LOCK, MODIFIER_CAPS_LOCK, MODIFIER_SCROLL_LOCK
};

/* Sdl3.Event.mouse_button, wheel_direction, pinch_phase: all constants. */
enum { BUTTON_LEFT, BUTTON_MIDDLE, BUTTON_RIGHT, BUTTON_X1, BUTTON_X2 };
enum { WHEEL_NORMAL, WHEEL_FLIPPED };
enum { PINCH_BEGAN, PINCH_UPDATED, PINCH_ENDED };

/* Sdl3.Event.window_change: constants first, then Resized and
   Pixel_size_changed as blocks 0 and 1. */
enum {
  WINDOW_SHOWN, WINDOW_HIDDEN, WINDOW_MINIMIZED, WINDOW_RESTORED,
  WINDOW_OCCLUDED, WINDOW_FOCUS_GAINED, WINDOW_FOCUS_LOST,
  WINDOW_CLOSE_REQUESTED
};
enum { WINDOW_RESIZED, WINDOW_PIXEL_SIZE_CHANGED };

/* Sdl3.Event.drop_change: Drop_begin, Drop_position, Drop_complete are
   constants; File is block 0. */
enum { DROP_BEGIN, DROP_POSITION, DROP_COMPLETE };

/* Sdl3.Event.t: Quit is a constant; the rest are blocks in this order. */
enum {
  EVENT_WINDOW, EVENT_KEY, EVENT_TEXT_INPUT, EVENT_TEXT_EDITING,
  EVENT_MOUSE_MOTION, EVENT_MOUSE_BUTTON, EVENT_MOUSE_WHEEL, EVENT_PINCH,
  EVENT_DROP, EVENT_DIALOG, EVENT_SCROLL
};

static value key_of_event(SDL_Keycode keycode, SDL_Scancode scancode)
{
  CAMLparam0();
  CAMLlocal1(result);
  if (keycode >= 33 && keycode <= 126) {
    /* Printable ASCII, lower case. */
    result = caml_alloc(1, KEY_CHAR);
    Store_field(result, 0,
        Val_int(keycode >= 'A' && keycode <= 'Z' ? keycode + ('a' - 'A') : keycode));
    CAMLreturn(result);
  }
  switch (keycode) {
  case SDLK_UP: CAMLreturn(Val_int(KEY_ARROW_UP));
  case SDLK_DOWN: CAMLreturn(Val_int(KEY_ARROW_DOWN));
  case SDLK_LEFT: CAMLreturn(Val_int(KEY_ARROW_LEFT));
  case SDLK_RIGHT: CAMLreturn(Val_int(KEY_ARROW_RIGHT));
  case SDLK_SPACE: CAMLreturn(Val_int(KEY_SPACE));
  case SDLK_RETURN:
  case SDLK_KP_ENTER: CAMLreturn(Val_int(KEY_ENTER));
  case SDLK_ESCAPE: CAMLreturn(Val_int(KEY_ESCAPE));
  case SDLK_BACKSPACE: CAMLreturn(Val_int(KEY_BACKSPACE));
  case SDLK_TAB: CAMLreturn(Val_int(KEY_TAB));
  case SDLK_LSHIFT:
  case SDLK_RSHIFT: CAMLreturn(Val_int(KEY_SHIFT));
  case SDLK_LCTRL:
  case SDLK_RCTRL: CAMLreturn(Val_int(KEY_CONTROL));
  case SDLK_LALT:
  case SDLK_RALT: CAMLreturn(Val_int(KEY_ALT));
  case SDLK_LGUI:
  case SDLK_RGUI: CAMLreturn(Val_int(KEY_META));
  case SDLK_F1: CAMLreturn(Val_int(KEY_F1));
  case SDLK_F2: CAMLreturn(Val_int(KEY_F2));
  case SDLK_F3: CAMLreturn(Val_int(KEY_F3));
  case SDLK_F4: CAMLreturn(Val_int(KEY_F4));
  case SDLK_F5: CAMLreturn(Val_int(KEY_F5));
  case SDLK_F6: CAMLreturn(Val_int(KEY_F6));
  case SDLK_F7: CAMLreturn(Val_int(KEY_F7));
  case SDLK_F8: CAMLreturn(Val_int(KEY_F8));
  case SDLK_F9: CAMLreturn(Val_int(KEY_F9));
  case SDLK_F10: CAMLreturn(Val_int(KEY_F10));
  case SDLK_F11: CAMLreturn(Val_int(KEY_F11));
  case SDLK_F12: CAMLreturn(Val_int(KEY_F12));
  case SDLK_HOME: CAMLreturn(Val_int(KEY_HOME));
  case SDLK_END: CAMLreturn(Val_int(KEY_END));
  case SDLK_PAGEUP: CAMLreturn(Val_int(KEY_PAGE_UP));
  case SDLK_PAGEDOWN: CAMLreturn(Val_int(KEY_PAGE_DOWN));
  case SDLK_INSERT: CAMLreturn(Val_int(KEY_INSERT));
  case SDLK_DELETE: CAMLreturn(Val_int(KEY_DELETE));
  default: break;
  }
  /* A non-Latin layout reports its own letters (a Cyrillic key is no ASCII
     character), so a shortcut is matched by where the key sits: the
     scancode names the US-layout letter or digit of that position. */
  if (scancode >= SDL_SCANCODE_A && scancode <= SDL_SCANCODE_Z) {
    result = caml_alloc(1, KEY_CHAR);
    Store_field(result, 0, Val_int('a' + (scancode - SDL_SCANCODE_A)));
    CAMLreturn(result);
  }
  if (scancode >= SDL_SCANCODE_1 && scancode <= SDL_SCANCODE_9) {
    result = caml_alloc(1, KEY_CHAR);
    Store_field(result, 0, Val_int('1' + (scancode - SDL_SCANCODE_1)));
    CAMLreturn(result);
  }
  if (scancode == SDL_SCANCODE_0) {
    result = caml_alloc(1, KEY_CHAR);
    Store_field(result, 0, Val_int('0'));
    CAMLreturn(result);
  }
  result = caml_alloc(1, KEY_UNKNOWN);
  Store_field(result, 0, Val_int(keycode));
  CAMLreturn(result);
}

static value modifiers_of_event(SDL_Keymod mod)
{
  CAMLparam0();
  CAMLlocal2(list, cell);
  static const struct { SDL_Keymod mask; int tag; } table[] = {
    { SDL_KMOD_SCROLL, MODIFIER_SCROLL_LOCK }, { SDL_KMOD_CAPS, MODIFIER_CAPS_LOCK },
    { SDL_KMOD_NUM, MODIFIER_NUM_LOCK }, { SDL_KMOD_GUI, MODIFIER_META },
    { SDL_KMOD_ALT, MODIFIER_ALT }, { SDL_KMOD_CTRL, MODIFIER_CONTROL },
    { SDL_KMOD_SHIFT, MODIFIER_SHIFT }
  };
  size_t index;
  list = Val_emptylist;
  for (index = 0; index < sizeof(table) / sizeof(table[0]); index++) {
    if ((mod & table[index].mask) != 0) {
      cell = caml_alloc(2, 0);
      Store_field(cell, 0, Val_int(table[index].tag));
      Store_field(cell, 1, list);
      list = cell;
    }
  }
  CAMLreturn(list);
}

static value window_change_of_event(const SDL_Event *event)
{
  CAMLparam0();
  CAMLlocal1(change);
  switch (event->type) {
  case SDL_EVENT_WINDOW_SHOWN: CAMLreturn(Val_int(WINDOW_SHOWN));
  case SDL_EVENT_WINDOW_HIDDEN: CAMLreturn(Val_int(WINDOW_HIDDEN));
  case SDL_EVENT_WINDOW_MINIMIZED: CAMLreturn(Val_int(WINDOW_MINIMIZED));
  case SDL_EVENT_WINDOW_RESTORED: CAMLreturn(Val_int(WINDOW_RESTORED));
  case SDL_EVENT_WINDOW_OCCLUDED: CAMLreturn(Val_int(WINDOW_OCCLUDED));
  case SDL_EVENT_WINDOW_FOCUS_GAINED: CAMLreturn(Val_int(WINDOW_FOCUS_GAINED));
  case SDL_EVENT_WINDOW_FOCUS_LOST: CAMLreturn(Val_int(WINDOW_FOCUS_LOST));
  case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
    CAMLreturn(Val_int(WINDOW_CLOSE_REQUESTED));
  case SDL_EVENT_WINDOW_RESIZED:
  case SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED:
    change = caml_alloc(2, event->type == SDL_EVENT_WINDOW_RESIZED
        ? WINDOW_RESIZED : WINDOW_PIXEL_SIZE_CHANGED);
    Store_field(change, 0, Val_int(event->window.data1));
    Store_field(change, 1, Val_int(event->window.data2));
    CAMLreturn(change);
  default: break;
  }
  CAMLreturn(Val_unit); /* unreachable: the caller filters the type first */
}

/* The OCaml value for an event Rays reads, or false for any other kind
   (touch, pen, gamepad, display, audio and the rest are never copied). */
static bool translate_event(const SDL_Event *event, value *out)
{
  CAMLparam0();
  CAMLlocal3(result, item, payload);

#define ALLOC_EVENT(tag, size) result = caml_alloc((size), (tag))
#define STORE_INT(index, number) Store_field(result, (index), Val_int((number)))
#define STORE_BOOL(index, boolean) Store_field(result, (index), Val_bool((boolean)))
#define STORE_FLOAT(index, number) do { \
    item = caml_copy_double((double)(number)); \
    Store_field(result, (index), item); \
  } while (0)
#define STORE_STRING(index, text) do { \
    item = caml_copy_string((text) != NULL ? (text) : ""); \
    Store_field(result, (index), item); \
  } while (0)

  switch (event->type) {
  case SDL_EVENT_QUIT:
    result = Val_unit;
    break;

  case SDL_EVENT_WINDOW_SHOWN: case SDL_EVENT_WINDOW_HIDDEN:
  case SDL_EVENT_WINDOW_MINIMIZED: case SDL_EVENT_WINDOW_RESTORED:
  case SDL_EVENT_WINDOW_OCCLUDED: case SDL_EVENT_WINDOW_FOCUS_GAINED:
  case SDL_EVENT_WINDOW_FOCUS_LOST: case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
  case SDL_EVENT_WINDOW_RESIZED: case SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED:
    payload = window_change_of_event(event);
    ALLOC_EVENT(EVENT_WINDOW, 1);
    Store_field(result, 0, payload);
    break;

  case SDL_EVENT_KEY_DOWN:
  case SDL_EVENT_KEY_UP:
    ALLOC_EVENT(EVENT_KEY, 4);
    payload = key_of_event(event->key.key, event->key.scancode);
    Store_field(result, 0, payload);
    payload = modifiers_of_event(event->key.mod);
    Store_field(result, 1, payload);
    STORE_BOOL(2, event->key.down);
    STORE_BOOL(3, event->key.repeat);
    break;

  case SDL_EVENT_TEXT_INPUT:
    ALLOC_EVENT(EVENT_TEXT_INPUT, 1);
    STORE_STRING(0, event->text.text);
    break;

  case SDL_EVENT_TEXT_EDITING:
    ALLOC_EVENT(EVENT_TEXT_EDITING, 3);
    STORE_STRING(0, event->edit.text);
    STORE_INT(1, event->edit.start);
    STORE_INT(2, event->edit.length);
    break;

  case SDL_EVENT_MOUSE_MOTION:
    ALLOC_EVENT(EVENT_MOUSE_MOTION, 4);
    STORE_FLOAT(0, event->motion.x);
    STORE_FLOAT(1, event->motion.y);
    STORE_FLOAT(2, event->motion.xrel);
    STORE_FLOAT(3, event->motion.yrel);
    break;

  case SDL_EVENT_MOUSE_BUTTON_DOWN:
  case SDL_EVENT_MOUSE_BUTTON_UP: {
    int button;
    switch (event->button.button) {
    case SDL_BUTTON_LEFT: button = BUTTON_LEFT; break;
    case SDL_BUTTON_MIDDLE: button = BUTTON_MIDDLE; break;
    case SDL_BUTTON_RIGHT: button = BUTTON_RIGHT; break;
    case SDL_BUTTON_X1: button = BUTTON_X1; break;
    case SDL_BUTTON_X2: button = BUTTON_X2; break;
    default: CAMLreturnT(bool, false);
    }
    ALLOC_EVENT(EVENT_MOUSE_BUTTON, 4);
    STORE_INT(0, button);
    STORE_BOOL(1, event->button.down);
    STORE_FLOAT(2, event->button.x);
    STORE_FLOAT(3, event->button.y);
    break;
  }

  case SDL_EVENT_MOUSE_WHEEL:
    ALLOC_EVENT(EVENT_MOUSE_WHEEL, 7);
    STORE_FLOAT(0, event->wheel.x);
    STORE_FLOAT(1, event->wheel.y);
    STORE_INT(2, event->wheel.direction == SDL_MOUSEWHEEL_FLIPPED
        ? WHEEL_FLIPPED : WHEEL_NORMAL);
    STORE_FLOAT(3, event->wheel.mouse_x);
    STORE_FLOAT(4, event->wheel.mouse_y);
    STORE_INT(5, event->wheel.integer_x);
    STORE_INT(6, event->wheel.integer_y);
    break;

  case SDL_EVENT_PINCH_BEGIN:
  case SDL_EVENT_PINCH_UPDATE:
  case SDL_EVENT_PINCH_END:
    ALLOC_EVENT(EVENT_PINCH, 2);
    STORE_INT(0, event->type == SDL_EVENT_PINCH_BEGIN ? PINCH_BEGAN
        : event->type == SDL_EVENT_PINCH_UPDATE ? PINCH_UPDATED : PINCH_ENDED);
    STORE_FLOAT(1, event->pinch.scale);
    break;

  case SDL_EVENT_DROP_BEGIN:
  case SDL_EVENT_DROP_POSITION:
  case SDL_EVENT_DROP_COMPLETE:
  case SDL_EVENT_DROP_FILE:
    if (event->type == SDL_EVENT_DROP_FILE) {
      if (event->drop.data == NULL) CAMLreturnT(bool, false);
      payload = caml_alloc(1, 0);
      item = caml_copy_string(event->drop.data);
      Store_field(payload, 0, item);
    } else {
      payload = Val_int(event->type == SDL_EVENT_DROP_BEGIN ? DROP_BEGIN
          : event->type == SDL_EVENT_DROP_POSITION ? DROP_POSITION
          : DROP_COMPLETE);
    }
    ALLOC_EVENT(EVENT_DROP, 3);
    Store_field(result, 0, payload);
    STORE_FLOAT(1, event->drop.x);
    STORE_FLOAT(2, event->drop.y);
    break;

  default:
    CAMLreturnT(bool, false);
  }

#undef STORE_STRING
#undef STORE_FLOAT
#undef STORE_BOOL
#undef STORE_INT
#undef ALLOC_EVENT
  *out = result;
  CAMLreturnT(bool, true);
}

/* A queued trackpad scroll (sdl3_cocoa.c) as Sdl3.Event.Scroll. */
static bool take_scroll(value *out)
{
  CAMLparam0();
  CAMLlocal2(result, number);
  rays_scroll scroll;
  if (!rays_scroll_take(&scroll)) CAMLreturnT(bool, false);
  result = caml_alloc(4, EVENT_SCROLL);
  number = caml_copy_double(scroll.x);
  Store_field(result, 0, number);
  number = caml_copy_double(scroll.y);
  Store_field(result, 1, number);
  Store_field(result, 2, Val_int(scroll.phase));
  number = caml_copy_double(scroll.seconds);
  Store_field(result, 3, number);
  *out = result;
  CAMLreturnT(bool, true);
}

/* Live resize. While a window edge is dragged, Cocoa runs its own event loop
   inside SDL_PollEvent and the sketch loop gets no turn. SDL calls this watch
   from that loop, on the main thread, with the OCaml runtime lock still held
   (poll is not a blocking section), so it may call the one closure the sketch
   registered as "rays_sdl3_live_redraw". It only acts inside our own poll, once
   at a time; OCaml keeps the exception, the callback never raises. */
static bool in_poll, in_live_redraw, live_watch_installed;

static bool live_resize_watch(void *userdata, SDL_Event *event)
{
  const value *redraw;
  (void)userdata;
  if (!in_poll || in_live_redraw || !SDL_IsMainThread()) return true;
  if (event->type != SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED) return true;
  redraw = caml_named_value("rays_sdl3_live_redraw");
  if (redraw == NULL) return true;
  in_live_redraw = true;
  (void)caml_callback_exn(*redraw, Val_unit);
  in_live_redraw = false;
  return true;
}

static void live_resize_install(void)
{
  if (live_watch_installed) return;
  live_watch_installed = SDL_AddEventWatch(live_resize_watch, NULL);
}

/* Event operations are safe-module main-domain-only, so one reusable native
   union is sufficient. SDL-owned pointer fields are copied before reuse. */
static SDL_Event rays_sdl3_event;

/* An idle sketch blocks here instead of ticking: until SDL has an event or [ms]
   pass. The event stays queued for the next poll. Not inside our poll, the
   live-resize watch returns at once, so no OCaml runs while the lock is free. */
CAMLprim value caml_sdl3_wait_event_timeout(value ms)
{
  bool any;
  int timeout = Int_val(ms);
  caml_release_runtime_system();
  any = SDL_WaitEventTimeout(NULL, timeout);
  caml_acquire_runtime_system();
  return Val_bool(any);
}

CAMLprim value caml_sdl3_poll_event(value unit)
{
  CAMLparam1(unit);
  CAMLlocal3(translated, some, outcome);
  rays_dialog_result finished;
  (void)unit;
  /* a finished file dialog comes first: its slot is the only thing a callback
     wrote */
  if (rays_dialog_take(&finished)) {
    outcome = dialog_outcome_value(&finished);
    translated = caml_alloc(2, EVENT_DIALOG);
    Store_field(translated, 0, Val_int(finished.id));
    Store_field(translated, 1, outcome);
    free(finished.payload);
    some = caml_alloc(1, 0);
    Store_field(some, 0, translated);
    CAMLreturn(some);
  }
  /* A trackpad scroll the pump queued comes before SDL's own events and is
     looked for again after the last pump, so a gesture and the wheel events
     SDL made of it reach the same frame. */
  if (take_scroll(&translated)) {
    some = caml_alloc(1, 0);
    Store_field(some, 0, translated);
    CAMLreturn(some);
  }
  live_resize_install();
  in_poll = true;
  while (SDL_PollEvent(&rays_sdl3_event)) {
    if (translate_event(&rays_sdl3_event, &translated)) {
      some = caml_alloc(1, 0);
      Store_field(some, 0, translated);
      in_poll = false;
      CAMLreturn(some);
    }
  }
  in_poll = false;
  if (take_scroll(&translated)) {
    some = caml_alloc(1, 0);
    Store_field(some, 0, translated);
    CAMLreturn(some);
  }
  CAMLreturn(Val_none);
}
