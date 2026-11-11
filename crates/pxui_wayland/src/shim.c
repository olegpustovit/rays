/* The Wayland client window: registry, xdg-shell toplevel, seat input, text-input-v3, clipboard and scale.
 * Listeners only record raw facts into a queue the Rust side drains; key, scroll and scale policy live in Rust.
 * Every optional protocol is NULL when the compositor does not advertise it, and each use checks first.
 * Needs libwayland-client >= 1.22 (listener structs with preferred_buffer_scale and axis_relative_direction). */
#define _GNU_SOURCE
#include <wayland-client.h>
#include <xkbcommon/xkbcommon.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include "../protocol/xdg-shell.h"
#include "../protocol/xdg-decoration-unstable-v1.h"
#include "../protocol/viewporter.h"
#include "../protocol/fractional-scale-v1.h"
#include "../protocol/cursor-shape-v1.h"
#include "../protocol/text-input-unstable-v3.h"
#include "../protocol/relative-pointer-unstable-v1.h"
#include "../protocol/pointer-constraints-unstable-v1.h"

/* Mirrors translate::Raw on the Rust side. */
enum { RW_KEY = 1, RW_MOTION, RW_BUTTON, RW_AXIS, RW_AXIS_DISCRETE, RW_AXIS_SOURCE, RW_AXIS_STOP, RW_FRAME,
       RW_FOCUS, RW_CLOSE, RW_PREEDIT, RW_COMMIT, RW_PASTE, RW_VISIBLE, RW_RELATIVE };
/* Protocol bits reported by rw_protocols. */
enum { HAS_VIEWPORTER = 1, HAS_FRACTIONAL = 2, HAS_CURSOR_SHAPE = 4, HAS_DECORATION = 8, HAS_TEXT_INPUT = 16,
       HAS_DATA_DEVICE = 32, HAS_SEAT = 64, HAS_POINTER = 128, HAS_KEYBOARD = 256, SERVER_DECORATED = 512 };

typedef struct {
  int32_t kind, a, b, c, d, e;
  uint32_t time;
  double x, y;
  char *text; /* malloc'd, released by rw_event_free */
} rw_event;

#define MAX_OUTPUTS 16
#define MAX_WRITES 8
#define TEXT_MIMES 3
static const char *text_mimes[TEXT_MIMES] = {"text/plain;charset=utf-8", "UTF8_STRING", "text/plain"};

typedef struct rw {
  struct wl_display *display;
  struct wl_registry *registry;
  struct wl_compositor *compositor;
  struct xdg_wm_base *wm;
  struct wl_seat *seat;
  struct wl_pointer *pointer;
  struct wl_keyboard *keyboard;
  struct wl_data_device_manager *data_manager;
  struct wl_data_device *data_device;
  struct wp_viewporter *viewporter;
  struct wp_fractional_scale_manager_v1 *fractional_manager;
  struct wp_cursor_shape_manager_v1 *cursor_manager;
  struct zxdg_decoration_manager_v1 *decoration_manager;
  struct zwp_text_input_manager_v3 *text_manager;
  struct zwp_relative_pointer_manager_v1 *relative_manager;
  struct zwp_pointer_constraints_v1 *constraints;
  uint32_t compositor_version, seat_version;

  struct wl_surface *surface;
  struct xdg_surface *xdg_surface;
  struct xdg_toplevel *toplevel;
  struct wp_viewport *viewport;
  struct wp_fractional_scale_v1 *fractional;
  struct zxdg_toplevel_decoration_v1 *decoration;
  struct wp_cursor_shape_device_v1 *cursor_device;
  struct zwp_text_input_v3 *text_input;
  struct zwp_relative_pointer_v1 *relative_pointer;
  struct zwp_locked_pointer_v1 *locked_pointer;
  int relative;

  struct { struct wl_output *output; uint32_t name; int32_t scale; int entered; } outputs[MAX_OUTPUTS];
  int output_count;

  int32_t width, height, pending_width, pending_height;
  int configured, suspended, pending_suspended, server_decorated;
  int32_t fractional120, preferred_scale, applied_scale120;

  uint32_t serial, pointer_serial;
  int32_t cursor_shape;

  struct xkb_context *xkb;
  struct xkb_keymap *keymap;
  struct xkb_state *xkb_state;
  int32_t repeat_rate, repeat_delay;

  int text_wanted, text_entered, text_enabled;
  int32_t text_rect[4];
  char *preedit_pending, *preedit_shown, *commit_pending;
  int32_t preedit_begin, preedit_end;

  struct wl_data_offer *selection;
  struct wl_data_source *source;
  char *source_text;
  int paste_fd;
  char *paste_buf;
  size_t paste_len;
  struct { int fd; char *text; size_t at, len; } writes[MAX_WRITES];
  int write_count;

  rw_event *queue;
  int queue_head, queue_len, queue_cap;
  int failed;
} rw;

static void push(rw *w, rw_event e) {
  if (w->queue_len == w->queue_cap) {
    int cap = w->queue_cap ? w->queue_cap * 2 : 64;
    rw_event *grown = realloc(w->queue, sizeof(rw_event) * cap);
    if (!grown) { free(e.text); w->failed = 1; return; }
    w->queue = grown;
    w->queue_cap = cap;
  }
  w->queue[w->queue_len++] = e;
}
static void push_kind(rw *w, int32_t kind, int32_t a, int32_t b, uint32_t time, double x, double y) {
  push(w, (rw_event){.kind = kind, .a = a, .b = b, .time = time, .x = x, .y = y});
}
static char *copy(const char *text) { return strdup(text ? text : ""); }

/* ---- scale and geometry ---- */

static int32_t integer_scale(rw *w) {
  if (w->preferred_scale > 0) return w->preferred_scale;
  int32_t scale = 1;
  for (int i = 0; i < w->output_count; ++i)
    if (w->outputs[i].entered && w->outputs[i].scale > scale) scale = w->outputs[i].scale;
  return scale;
}
/* Logical size and scale go to the compositor with the next commit, which the Vulkan present makes. With a
 * viewport the buffer may be any size (fractional or integer scale); without one the integer buffer scale says it. */
static void apply_geometry(rw *w) {
  int32_t scale120 = w->viewporter && w->fractional120 > 0 ? w->fractional120 : 120 * integer_scale(w);
  if (w->viewport) wp_viewport_set_destination(w->viewport, w->width, w->height);
  else if (scale120 != w->applied_scale120 && w->compositor_version >= 3) wl_surface_set_buffer_scale(w->surface, scale120 / 120);
  else if (w->compositor_version < 3) scale120 = 120;
  /* ponytail: without a viewport, a scale change can reach the compositor one frame before the resized swapchain;
   * a buffer not divisible by the new scale is then a protocol error. Every compositor with HiDPI offers wp_viewporter. */
  w->applied_scale120 = scale120;
}

/* ---- xdg-shell ---- */

static void wm_ping(void *data, struct xdg_wm_base *wm, uint32_t serial) { (void)data; xdg_wm_base_pong(wm, serial); }
static const struct xdg_wm_base_listener wm_listener = {.ping = wm_ping};

static void xdg_surface_configure(void *data, struct xdg_surface *surface, uint32_t serial) {
  rw *w = data;
  xdg_surface_ack_configure(surface, serial);
  if (w->pending_width > 0 && w->pending_height > 0) { w->width = w->pending_width; w->height = w->pending_height; }
  if (w->configured && w->pending_suspended != w->suspended) push_kind(w, RW_VISIBLE, !w->pending_suspended, 0, 0, 0, 0);
  w->suspended = w->pending_suspended;
  w->configured = 1;
  apply_geometry(w);
}
static const struct xdg_surface_listener xdg_surface_listener = {.configure = xdg_surface_configure};

static void toplevel_configure(void *data, struct xdg_toplevel *toplevel, int32_t width, int32_t height, struct wl_array *states) {
  rw *w = data;
  (void)toplevel;
  w->pending_width = width;
  w->pending_height = height;
  w->pending_suspended = 0;
  uint32_t *state;
  wl_array_for_each(state, states) if (*state == 9 /* XDG_TOPLEVEL_STATE_SUSPENDED, v6 */) w->pending_suspended = 1;
}
static void toplevel_close(void *data, struct xdg_toplevel *toplevel) { (void)toplevel; push_kind(data, RW_CLOSE, 0, 0, 0, 0, 0); }
static void toplevel_bounds(void *data, struct xdg_toplevel *t, int32_t w, int32_t h) { (void)data; (void)t; (void)w; (void)h; }
static void toplevel_capabilities(void *data, struct xdg_toplevel *t, struct wl_array *c) { (void)data; (void)t; (void)c; }
static const struct xdg_toplevel_listener toplevel_listener = {
  .configure = toplevel_configure, .close = toplevel_close, .configure_bounds = toplevel_bounds, .wm_capabilities = toplevel_capabilities};

static void decoration_configure(void *data, struct zxdg_toplevel_decoration_v1 *d, uint32_t mode) {
  (void)d;
  ((rw *)data)->server_decorated = mode == ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE;
}
static const struct zxdg_toplevel_decoration_v1_listener decoration_listener = {.configure = decoration_configure};

/* ---- scale sources ---- */

static void fractional_preferred(void *data, struct wp_fractional_scale_v1 *f, uint32_t scale) {
  rw *w = data;
  (void)f;
  w->fractional120 = (int32_t)scale;
  apply_geometry(w);
}
static const struct wp_fractional_scale_v1_listener fractional_listener = {.preferred_scale = fractional_preferred};

static void surface_enter(void *data, struct wl_surface *s, struct wl_output *output) {
  rw *w = data;
  (void)s;
  for (int i = 0; i < w->output_count; ++i) if (w->outputs[i].output == output) w->outputs[i].entered = 1;
  apply_geometry(w);
}
static void surface_leave(void *data, struct wl_surface *s, struct wl_output *output) {
  rw *w = data;
  (void)s;
  for (int i = 0; i < w->output_count; ++i) if (w->outputs[i].output == output) w->outputs[i].entered = 0;
  apply_geometry(w);
}
static void surface_preferred_scale(void *data, struct wl_surface *s, int32_t scale) {
  rw *w = data;
  (void)s;
  w->preferred_scale = scale;
  apply_geometry(w);
}
static void surface_preferred_transform(void *data, struct wl_surface *s, uint32_t t) { (void)data; (void)s; (void)t; }
static const struct wl_surface_listener surface_listener = {
  .enter = surface_enter, .leave = surface_leave, .preferred_buffer_scale = surface_preferred_scale,
  .preferred_buffer_transform = surface_preferred_transform};

static void output_geometry(void *d, struct wl_output *o, int32_t x, int32_t y, int32_t pw, int32_t ph, int32_t sp,
                            const char *make, const char *model, int32_t t) {
  (void)d; (void)o; (void)x; (void)y; (void)pw; (void)ph; (void)sp; (void)make; (void)model; (void)t;
}
static void output_mode(void *d, struct wl_output *o, uint32_t f, int32_t w, int32_t h, int32_t r) { (void)d; (void)o; (void)f; (void)w; (void)h; (void)r; }
static void output_done(void *data, struct wl_output *o) { (void)o; rw *w = data; if (w->surface) apply_geometry(w); }
static void output_scale(void *data, struct wl_output *output, int32_t scale) {
  rw *w = data;
  for (int i = 0; i < w->output_count; ++i) if (w->outputs[i].output == output) w->outputs[i].scale = scale;
}
static void output_name(void *d, struct wl_output *o, const char *n) { (void)d; (void)o; (void)n; }
static const struct wl_output_listener output_listener = {
  .geometry = output_geometry, .mode = output_mode, .done = output_done, .scale = output_scale, .name = output_name,
  .description = output_name};

/* ---- pointer ---- */

static void set_cursor_now(rw *w) {
  if (w->relative && w->pointer && w->pointer_serial) { wl_pointer_set_cursor(w->pointer, w->pointer_serial, NULL, 0, 0); return; }
  if (!w->cursor_manager || !w->pointer || !w->pointer_serial) return;
  if (!w->cursor_device) w->cursor_device = wp_cursor_shape_manager_v1_get_pointer(w->cursor_manager, w->pointer);
  wp_cursor_shape_device_v1_set_shape(w->cursor_device, w->pointer_serial, (uint32_t)w->cursor_shape);
}
static void pointer_enter(void *data, struct wl_pointer *p, uint32_t serial, struct wl_surface *s, wl_fixed_t x, wl_fixed_t y) {
  rw *w = data;
  (void)p; (void)s;
  w->pointer_serial = serial;
  set_cursor_now(w); /* a pointer entering a surface has no cursor until the client sets one */
  push_kind(w, RW_MOTION, 0, 0, 0, wl_fixed_to_double(x), wl_fixed_to_double(y));
}
static void pointer_leave(void *data, struct wl_pointer *p, uint32_t serial, struct wl_surface *s) { (void)data; (void)p; (void)serial; (void)s; }
static void pointer_motion(void *data, struct wl_pointer *p, uint32_t time, wl_fixed_t x, wl_fixed_t y) {
  (void)p;
  push_kind(data, RW_MOTION, 0, 0, time, wl_fixed_to_double(x), wl_fixed_to_double(y));
}
static void pointer_button(void *data, struct wl_pointer *p, uint32_t serial, uint32_t time, uint32_t button, uint32_t state) {
  rw *w = data;
  (void)p;
  w->serial = serial;
  push_kind(w, RW_BUTTON, (int32_t)button, state == WL_POINTER_BUTTON_STATE_PRESSED, time, 0, 0);
  if (w->seat_version < 5) push_kind(w, RW_FRAME, 0, 0, time, 0, 0);
}
static void pointer_axis(void *data, struct wl_pointer *p, uint32_t time, uint32_t axis, wl_fixed_t value) {
  rw *w = data;
  (void)p;
  push_kind(w, RW_AXIS, (int32_t)axis, 0, time, wl_fixed_to_double(value), 0);
  if (w->seat_version < 5) push_kind(w, RW_FRAME, 0, 0, time, 0, 0);
}
static void pointer_frame(void *data, struct wl_pointer *p) { (void)p; push_kind(data, RW_FRAME, 0, 0, 0, 0, 0); }
static void pointer_source(void *data, struct wl_pointer *p, uint32_t source) { (void)p; push_kind(data, RW_AXIS_SOURCE, (int32_t)source, 0, 0, 0, 0); }
static void pointer_stop(void *data, struct wl_pointer *p, uint32_t time, uint32_t axis) { (void)p; push_kind(data, RW_AXIS_STOP, (int32_t)axis, 0, time, 0, 0); }
static void pointer_discrete(void *data, struct wl_pointer *p, uint32_t axis, int32_t discrete) {
  (void)p;
  push_kind(data, RW_AXIS_DISCRETE, (int32_t)axis, discrete * 120, 0, 0, 0);
}
static void pointer_value120(void *data, struct wl_pointer *p, uint32_t axis, int32_t value120) {
  (void)p;
  push_kind(data, RW_AXIS_DISCRETE, (int32_t)axis, value120, 0, 0, 0);
}
static void pointer_direction(void *data, struct wl_pointer *p, uint32_t axis, uint32_t direction) { (void)data; (void)p; (void)axis; (void)direction; }
static const struct wl_pointer_listener pointer_listener = {
  .enter = pointer_enter, .leave = pointer_leave, .motion = pointer_motion, .button = pointer_button, .axis = pointer_axis,
  .frame = pointer_frame, .axis_source = pointer_source, .axis_stop = pointer_stop, .axis_discrete = pointer_discrete,
  .axis_value120 = pointer_value120, .axis_relative_direction = pointer_direction};

/* ---- keyboard ---- */

static void keyboard_keymap(void *data, struct wl_keyboard *k, uint32_t format, int32_t fd, uint32_t size) {
  rw *w = data;
  (void)k;
  if (format != WL_KEYBOARD_KEYMAP_FORMAT_XKB_V1) { close(fd); return; }
  char *map = mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);
  close(fd);
  if (map == MAP_FAILED) return;
  struct xkb_keymap *keymap = xkb_keymap_new_from_string(w->xkb, map, XKB_KEYMAP_FORMAT_TEXT_V1, XKB_KEYMAP_COMPILE_NO_FLAGS);
  munmap(map, size);
  if (!keymap) return;
  struct xkb_state *state = xkb_state_new(keymap);
  if (!state) { xkb_keymap_unref(keymap); return; }
  xkb_state_unref(w->xkb_state);
  xkb_keymap_unref(w->keymap);
  w->keymap = keymap;
  w->xkb_state = state;
}
static void keyboard_enter(void *data, struct wl_keyboard *k, uint32_t serial, struct wl_surface *s, struct wl_array *keys) {
  rw *w = data;
  (void)k; (void)s; (void)keys;
  w->serial = serial;
  push_kind(w, RW_FOCUS, 1, 0, 0, 0, 0);
}
static void keyboard_leave(void *data, struct wl_keyboard *k, uint32_t serial, struct wl_surface *s) {
  (void)k; (void)serial; (void)s;
  push_kind(data, RW_FOCUS, 0, 0, 0, 0, 0);
}
static void keyboard_key(void *data, struct wl_keyboard *k, uint32_t serial, uint32_t time, uint32_t key, uint32_t state) {
  rw *w = data;
  (void)k;
  w->serial = serial;
  if (!w->xkb_state) return;
  xkb_keycode_t code = key + 8;
  int down = state == WL_KEYBOARD_KEY_STATE_PRESSED;
  /* The unshifted symbol names the key (as SDL's keycode does); the shifted text is what it types. */
  const xkb_keysym_t *syms = NULL;
  xkb_layout_index_t layout = xkb_state_key_get_layout(w->xkb_state, code);
  int n = layout == XKB_LAYOUT_INVALID ? 0 : xkb_keymap_key_get_syms_by_level(w->keymap, code, layout, 0, &syms);
  xkb_keysym_t sym = n > 0 ? syms[0] : XKB_KEY_NoSymbol;
  char text[64] = "";
  if (down) xkb_state_key_get_utf8(w->xkb_state, code, text, sizeof text);
  int mods = 0;
  if (xkb_state_mod_name_is_active(w->xkb_state, XKB_MOD_NAME_CTRL, XKB_STATE_MODS_EFFECTIVE) > 0) mods |= 2;
  if (xkb_state_mod_name_is_active(w->xkb_state, XKB_MOD_NAME_ALT, XKB_STATE_MODS_EFFECTIVE) > 0) mods |= 4;
  if (xkb_state_mod_name_is_active(w->xkb_state, XKB_MOD_NAME_LOGO, XKB_STATE_MODS_EFFECTIVE) > 0) mods |= 8;
  push(w, (rw_event){.kind = RW_KEY, .a = down, .b = (int32_t)key, .c = (int32_t)sym, .d = xkb_keymap_key_repeats(w->keymap, code),
                     .e = mods, .time = time, .x = (double)xkb_keysym_to_utf32(sym), .text = down ? copy(text) : NULL});
}
static void keyboard_modifiers(void *data, struct wl_keyboard *k, uint32_t serial, uint32_t depressed, uint32_t latched, uint32_t locked, uint32_t group) {
  rw *w = data;
  (void)k; (void)serial;
  if (w->xkb_state) xkb_state_update_mask(w->xkb_state, depressed, latched, locked, 0, 0, group);
}
static void keyboard_repeat(void *data, struct wl_keyboard *k, int32_t rate, int32_t delay) {
  rw *w = data;
  (void)k;
  w->repeat_rate = rate;
  w->repeat_delay = delay;
}
static const struct wl_keyboard_listener keyboard_listener = {
  .keymap = keyboard_keymap, .enter = keyboard_enter, .leave = keyboard_leave, .key = keyboard_key,
  .modifiers = keyboard_modifiers, .repeat_info = keyboard_repeat};

static void seat_capabilities(void *data, struct wl_seat *seat, uint32_t caps) {
  rw *w = data;
  if ((caps & WL_SEAT_CAPABILITY_POINTER) && !w->pointer) {
    w->pointer = wl_seat_get_pointer(seat);
    wl_pointer_add_listener(w->pointer, &pointer_listener, w);
  } else if (!(caps & WL_SEAT_CAPABILITY_POINTER) && w->pointer) {
    if (w->locked_pointer) { zwp_locked_pointer_v1_destroy(w->locked_pointer); w->locked_pointer = NULL; }
    if (w->relative_pointer) { zwp_relative_pointer_v1_destroy(w->relative_pointer); w->relative_pointer = NULL; }
    if (w->cursor_device) { wp_cursor_shape_device_v1_destroy(w->cursor_device); w->cursor_device = NULL; }
    wl_pointer_destroy(w->pointer);
    w->pointer = NULL;
  }
  if ((caps & WL_SEAT_CAPABILITY_KEYBOARD) && !w->keyboard) {
    w->keyboard = wl_seat_get_keyboard(seat);
    wl_keyboard_add_listener(w->keyboard, &keyboard_listener, w);
  } else if (!(caps & WL_SEAT_CAPABILITY_KEYBOARD) && w->keyboard) {
    wl_keyboard_destroy(w->keyboard);
    w->keyboard = NULL;
  }
}
static void seat_name(void *data, struct wl_seat *seat, const char *name) { (void)data; (void)seat; (void)name; }
static const struct wl_seat_listener seat_listener = {.capabilities = seat_capabilities, .name = seat_name};

/* ---- text-input-v3 ---- */

static void text_sync(rw *w) {
  if (!w->text_input || !w->text_entered) return;
  if (w->text_wanted) {
    if (!w->text_enabled) zwp_text_input_v3_enable(w->text_input);
    zwp_text_input_v3_set_cursor_rectangle(w->text_input, w->text_rect[0], w->text_rect[1], w->text_rect[2], w->text_rect[3]);
    w->text_enabled = 1;
  } else if (w->text_enabled) {
    zwp_text_input_v3_disable(w->text_input);
    w->text_enabled = 0;
  } else {
    return;
  }
  zwp_text_input_v3_commit(w->text_input);
}
static void text_enter(void *data, struct zwp_text_input_v3 *t, struct wl_surface *s) {
  rw *w = data;
  (void)t; (void)s;
  w->text_entered = 1;
  w->text_enabled = 0;
  text_sync(w);
}
static void text_leave(void *data, struct zwp_text_input_v3 *t, struct wl_surface *s) {
  rw *w = data;
  (void)s;
  if (w->text_enabled) { zwp_text_input_v3_disable(t); zwp_text_input_v3_commit(t); }
  w->text_entered = w->text_enabled = 0;
}
static void text_preedit(void *data, struct zwp_text_input_v3 *t, const char *text, int32_t begin, int32_t end) {
  rw *w = data;
  (void)t;
  free(w->preedit_pending);
  w->preedit_pending = copy(text);
  w->preedit_begin = begin;
  w->preedit_end = end;
}
static void text_commit(void *data, struct zwp_text_input_v3 *t, const char *text) {
  rw *w = data;
  (void)t;
  free(w->commit_pending);
  w->commit_pending = copy(text);
}
/* ponytail: surrounding text is never sent, so deletion requests are dropped; add set_surrounding_text when an IME needs it. */
static void text_delete(void *data, struct zwp_text_input_v3 *t, uint32_t before, uint32_t after) { (void)data; (void)t; (void)before; (void)after; }
static void text_done(void *data, struct zwp_text_input_v3 *t, uint32_t serial) {
  rw *w = data;
  (void)t; (void)serial;
  if (w->commit_pending && *w->commit_pending) push(w, (rw_event){.kind = RW_COMMIT, .text = w->commit_pending});
  else free(w->commit_pending);
  w->commit_pending = NULL;
  const char *preedit = w->preedit_pending ? w->preedit_pending : "";
  if (strcmp(preedit, w->preedit_shown ? w->preedit_shown : "")) {
    push(w, (rw_event){.kind = RW_PREEDIT, .a = w->preedit_begin, .b = w->preedit_end, .text = copy(preedit)});
    free(w->preedit_shown);
    w->preedit_shown = copy(preedit);
  }
  free(w->preedit_pending);
  w->preedit_pending = NULL; /* each done replaces the preedit: none sent means none shown */
  w->preedit_begin = w->preedit_end = 0;
}
static const struct zwp_text_input_v3_listener text_listener = {
  .enter = text_enter, .leave = text_leave, .preedit_string = text_preedit, .commit_string = text_commit,
  .delete_surrounding_text = text_delete, .done = text_done};

/* ---- clipboard ---- */

static void offer_offer(void *data, struct wl_data_offer *offer, const char *mime) {
  (void)data;
  intptr_t mimes = (intptr_t)wl_data_offer_get_user_data(offer);
  for (int i = 0; i < TEXT_MIMES; ++i) if (!strcmp(mime, text_mimes[i])) mimes |= 1 << i;
  wl_data_offer_set_user_data(offer, (void *)mimes);
}
static void offer_actions(void *data, struct wl_data_offer *o, uint32_t a) { (void)data; (void)o; (void)a; }
static const struct wl_data_offer_listener offer_listener = {.offer = offer_offer, .source_actions = offer_actions, .action = offer_actions};

static void device_offer(void *data, struct wl_data_device *d, struct wl_data_offer *offer) {
  (void)d;
  wl_data_offer_add_listener(offer, &offer_listener, data);
  wl_data_offer_set_user_data(offer, (void *)0);
}
/* ponytail: drag and drop is refused (the offer is destroyed); FileDropped needs text/uri-list handling here. */
static void device_enter(void *data, struct wl_data_device *d, uint32_t serial, struct wl_surface *s, wl_fixed_t x, wl_fixed_t y, struct wl_data_offer *offer) {
  (void)data; (void)d; (void)serial; (void)s; (void)x; (void)y;
  if (offer) wl_data_offer_destroy(offer);
}
static void device_leave(void *data, struct wl_data_device *d) { (void)data; (void)d; }
static void device_motion(void *data, struct wl_data_device *d, uint32_t t, wl_fixed_t x, wl_fixed_t y) { (void)data; (void)d; (void)t; (void)x; (void)y; }
static void device_drop(void *data, struct wl_data_device *d) { (void)data; (void)d; }
static void device_selection(void *data, struct wl_data_device *d, struct wl_data_offer *offer) {
  rw *w = data;
  (void)d;
  if (w->selection) wl_data_offer_destroy(w->selection);
  w->selection = offer;
}
static const struct wl_data_device_listener device_listener = {
  .data_offer = device_offer, .enter = device_enter, .leave = device_leave, .motion = device_motion, .drop = device_drop,
  .selection = device_selection};

static void source_target(void *data, struct wl_data_source *s, const char *mime) { (void)data; (void)s; (void)mime; }
/* The write is queued and finished by service(): this process may be the reader too, and a blocking write would deadlock. */
static void source_send(void *data, struct wl_data_source *s, const char *mime, int32_t fd) {
  rw *w = data;
  (void)s; (void)mime;
  if (w->write_count == MAX_WRITES || !w->source_text) { close(fd); return; }
  fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
  w->writes[w->write_count].fd = fd;
  w->writes[w->write_count].text = copy(w->source_text);
  w->writes[w->write_count].at = 0;
  w->writes[w->write_count].len = strlen(w->source_text);
  w->write_count++;
}
static void source_cancelled(void *data, struct wl_data_source *s) {
  rw *w = data;
  if (w->source == s) { w->source = NULL; free(w->source_text); w->source_text = NULL; }
  wl_data_source_destroy(s);
}
static void source_unused(void *data, struct wl_data_source *s) { (void)data; (void)s; }
static void source_action(void *data, struct wl_data_source *s, uint32_t a) { (void)data; (void)s; (void)a; }
static const struct wl_data_source_listener source_listener = {
  .target = source_target, .send = source_send, .cancelled = source_cancelled, .dnd_drop_performed = source_unused,
  .dnd_finished = source_unused, .action = source_action};

static void finish_paste(rw *w) {
  push(w, (rw_event){.kind = RW_PASTE, .text = w->paste_buf ? w->paste_buf : copy("")});
  w->paste_buf = NULL;
  w->paste_len = 0;
  close(w->paste_fd);
  w->paste_fd = -1;
}
static void service(rw *w) {
  for (int i = 0; i < w->write_count;) {
    ssize_t n = 0;
    while (w->writes[i].at < w->writes[i].len) {
      n = write(w->writes[i].fd, w->writes[i].text + w->writes[i].at, w->writes[i].len - w->writes[i].at);
      if (n <= 0) break;
      w->writes[i].at += (size_t)n;
    }
    if (w->writes[i].at < w->writes[i].len && n < 0 && (errno == EAGAIN || errno == EINTR)) { ++i; continue; }
    close(w->writes[i].fd);
    free(w->writes[i].text);
    w->writes[i] = w->writes[--w->write_count];
  }
  while (w->paste_fd >= 0) {
    char chunk[4096];
    ssize_t n = read(w->paste_fd, chunk, sizeof chunk);
    if (n > 0) {
      char *grown = realloc(w->paste_buf, w->paste_len + (size_t)n + 1);
      if (!grown) { finish_paste(w); break; }
      memcpy(grown + w->paste_len, chunk, (size_t)n);
      w->paste_len += (size_t)n;
      grown[w->paste_len] = 0;
      w->paste_buf = grown;
    } else if (n < 0 && (errno == EAGAIN || errno == EINTR)) {
      break;
    } else {
      finish_paste(w);
    }
  }
}

/* ---- relative pointer ---- */

static void relative_motion(void *data, struct zwp_relative_pointer_v1 *p, uint32_t hi, uint32_t lo, wl_fixed_t dx, wl_fixed_t dy,
                            wl_fixed_t udx, wl_fixed_t udy) {
  (void)p; (void)hi; (void)udx; (void)udy;
  push_kind(data, RW_RELATIVE, 0, 0, lo / 1000, wl_fixed_to_double(dx), wl_fixed_to_double(dy));
}
static const struct zwp_relative_pointer_v1_listener relative_listener = {.relative_motion = relative_motion};

/* ---- registry ---- */

static uint32_t at_most(uint32_t offered, uint32_t wanted) { return offered < wanted ? offered : wanted; }
static void registry_global(void *data, struct wl_registry *r, uint32_t name, const char *interface, uint32_t version) {
  rw *w = data;
  if (!strcmp(interface, wl_compositor_interface.name)) {
    w->compositor_version = at_most(version, 6);
    w->compositor = wl_registry_bind(r, name, &wl_compositor_interface, w->compositor_version);
  } else if (!strcmp(interface, xdg_wm_base_interface.name)) {
    w->wm = wl_registry_bind(r, name, &xdg_wm_base_interface, at_most(version, 6));
    xdg_wm_base_add_listener(w->wm, &wm_listener, w);
  } else if (!strcmp(interface, wl_seat_interface.name) && !w->seat) {
    w->seat_version = at_most(version, 8);
    w->seat = wl_registry_bind(r, name, &wl_seat_interface, w->seat_version);
    wl_seat_add_listener(w->seat, &seat_listener, w);
  } else if (!strcmp(interface, wl_output_interface.name) && w->output_count < MAX_OUTPUTS) {
    int i = w->output_count++;
    w->outputs[i].output = wl_registry_bind(r, name, &wl_output_interface, at_most(version, 2));
    w->outputs[i].name = name;
    w->outputs[i].scale = 1;
    w->outputs[i].entered = 0;
    wl_output_add_listener(w->outputs[i].output, &output_listener, w);
  } else if (!strcmp(interface, wl_data_device_manager_interface.name)) {
    w->data_manager = wl_registry_bind(r, name, &wl_data_device_manager_interface, at_most(version, 3));
  } else if (!strcmp(interface, wp_viewporter_interface.name)) {
    w->viewporter = wl_registry_bind(r, name, &wp_viewporter_interface, 1);
  } else if (!strcmp(interface, wp_fractional_scale_manager_v1_interface.name)) {
    w->fractional_manager = wl_registry_bind(r, name, &wp_fractional_scale_manager_v1_interface, 1);
  } else if (!strcmp(interface, wp_cursor_shape_manager_v1_interface.name)) {
    w->cursor_manager = wl_registry_bind(r, name, &wp_cursor_shape_manager_v1_interface, 1);
  } else if (!strcmp(interface, zxdg_decoration_manager_v1_interface.name)) {
    w->decoration_manager = wl_registry_bind(r, name, &zxdg_decoration_manager_v1_interface, 1);
  } else if (!strcmp(interface, zwp_text_input_manager_v3_interface.name)) {
    w->text_manager = wl_registry_bind(r, name, &zwp_text_input_manager_v3_interface, 1);
  } else if (!strcmp(interface, zwp_relative_pointer_manager_v1_interface.name)) {
    w->relative_manager = wl_registry_bind(r, name, &zwp_relative_pointer_manager_v1_interface, 1);
  } else if (!strcmp(interface, zwp_pointer_constraints_v1_interface.name)) {
    w->constraints = wl_registry_bind(r, name, &zwp_pointer_constraints_v1_interface, 1);
  }
}
static void registry_remove(void *data, struct wl_registry *r, uint32_t name) {
  rw *w = data;
  (void)r;
  for (int i = 0; i < w->output_count; ++i) {
    if (w->outputs[i].name != name) continue;
    wl_output_destroy(w->outputs[i].output);
    w->outputs[i] = w->outputs[--w->output_count];
    if (w->surface) apply_geometry(w);
    break;
  }
}
static const struct wl_registry_listener registry_listener = {.global = registry_global, .global_remove = registry_remove};

/* ---- public ---- */

void rw_close(rw *w);

/* NULL with *error: 1 no compositor connection, 2 no wl_compositor/xdg_wm_base, 3 no first configure, 4 out of memory. */
rw *rw_open(const char *title, int32_t width, int32_t height, int32_t *error) {
  rw *w = calloc(1, sizeof *w);
  if (!w) { *error = 4; return NULL; }
  w->paste_fd = -1;
  w->width = width;
  w->height = height;
  w->cursor_shape = WP_CURSOR_SHAPE_DEVICE_V1_SHAPE_DEFAULT;
  w->repeat_rate = 25;
  w->repeat_delay = 600;
  w->display = wl_display_connect(NULL);
  if (!w->display) { free(w); *error = 1; return NULL; }
  w->xkb = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
  w->registry = wl_display_get_registry(w->display);
  wl_registry_add_listener(w->registry, &registry_listener, w);
  wl_display_roundtrip(w->display);
  if (!w->compositor || !w->wm || !w->xkb) { rw_close(w); *error = 2; return NULL; }
  wl_display_roundtrip(w->display); /* seat capabilities and output scales */

  w->surface = wl_compositor_create_surface(w->compositor);
  wl_surface_add_listener(w->surface, &surface_listener, w);
  if (w->viewporter) w->viewport = wp_viewporter_get_viewport(w->viewporter, w->surface);
  if (w->fractional_manager && w->viewporter) {
    w->fractional = wp_fractional_scale_manager_v1_get_fractional_scale(w->fractional_manager, w->surface);
    wp_fractional_scale_v1_add_listener(w->fractional, &fractional_listener, w);
  }
  w->xdg_surface = xdg_wm_base_get_xdg_surface(w->wm, w->surface);
  xdg_surface_add_listener(w->xdg_surface, &xdg_surface_listener, w);
  w->toplevel = xdg_surface_get_toplevel(w->xdg_surface);
  xdg_toplevel_add_listener(w->toplevel, &toplevel_listener, w);
  xdg_toplevel_set_title(w->toplevel, title);
  xdg_toplevel_set_app_id(w->toplevel, title);
  if (w->decoration_manager) {
    w->decoration = zxdg_decoration_manager_v1_get_toplevel_decoration(w->decoration_manager, w->toplevel);
    zxdg_toplevel_decoration_v1_add_listener(w->decoration, &decoration_listener, w);
    zxdg_toplevel_decoration_v1_set_mode(w->decoration, ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE);
  }
  if (w->data_manager && w->seat) {
    w->data_device = wl_data_device_manager_get_data_device(w->data_manager, w->seat);
    wl_data_device_add_listener(w->data_device, &device_listener, w);
  }
  if (w->text_manager && w->seat) {
    w->text_input = zwp_text_input_manager_v3_get_text_input(w->text_manager, w->seat);
    zwp_text_input_v3_add_listener(w->text_input, &text_listener, w);
  }
  wl_surface_commit(w->surface); /* no buffer: asks for the first configure */
  for (int i = 0; i < 20 && !w->configured; ++i) if (wl_display_roundtrip(w->display) < 0) break;
  if (!w->configured) { rw_close(w); *error = 3; return NULL; }
  return w;
}

void rw_close(rw *w) {
  if (!w) return;
  for (int i = 0; i < w->write_count; ++i) { close(w->writes[i].fd); free(w->writes[i].text); }
  if (w->paste_fd >= 0) close(w->paste_fd);
  free(w->paste_buf);
  if (w->source) wl_data_source_destroy(w->source);
  free(w->source_text);
  if (w->selection) wl_data_offer_destroy(w->selection);
  if (w->data_device) wl_data_device_destroy(w->data_device);
  if (w->text_input) zwp_text_input_v3_destroy(w->text_input);
  if (w->locked_pointer) zwp_locked_pointer_v1_destroy(w->locked_pointer);
  if (w->relative_pointer) zwp_relative_pointer_v1_destroy(w->relative_pointer);
  free(w->preedit_pending); free(w->preedit_shown); free(w->commit_pending);
  if (w->cursor_device) wp_cursor_shape_device_v1_destroy(w->cursor_device);
  if (w->decoration) zxdg_toplevel_decoration_v1_destroy(w->decoration);
  if (w->toplevel) xdg_toplevel_destroy(w->toplevel);
  if (w->xdg_surface) xdg_surface_destroy(w->xdg_surface);
  if (w->fractional) wp_fractional_scale_v1_destroy(w->fractional);
  if (w->viewport) wp_viewport_destroy(w->viewport);
  if (w->surface) wl_surface_destroy(w->surface);
  if (w->pointer) wl_pointer_destroy(w->pointer);
  if (w->keyboard) wl_keyboard_destroy(w->keyboard);
  for (int i = 0; i < w->output_count; ++i) wl_output_destroy(w->outputs[i].output);
  if (w->seat) wl_seat_destroy(w->seat);
  if (w->text_manager) zwp_text_input_manager_v3_destroy(w->text_manager);
  if (w->relative_manager) zwp_relative_pointer_manager_v1_destroy(w->relative_manager);
  if (w->constraints) zwp_pointer_constraints_v1_destroy(w->constraints);
  if (w->decoration_manager) zxdg_decoration_manager_v1_destroy(w->decoration_manager);
  if (w->cursor_manager) wp_cursor_shape_manager_v1_destroy(w->cursor_manager);
  if (w->fractional_manager) wp_fractional_scale_manager_v1_destroy(w->fractional_manager);
  if (w->viewporter) wp_viewporter_destroy(w->viewporter);
  if (w->data_manager) wl_data_device_manager_destroy(w->data_manager);
  if (w->wm) xdg_wm_base_destroy(w->wm);
  if (w->compositor) wl_compositor_destroy(w->compositor);
  if (w->registry) wl_registry_destroy(w->registry);
  if (w->display) { wl_display_flush(w->display); wl_display_disconnect(w->display); }
  xkb_state_unref(w->xkb_state);
  xkb_keymap_unref(w->keymap);
  xkb_context_unref(w->xkb);
  for (int i = w->queue_head; i < w->queue_len; ++i) free(w->queue[i].text);
  free(w->queue);
  free(w);
}

void *rw_display(rw *w) { return w->display; }
void *rw_surface(rw *w) { return w->surface; }
int32_t rw_protocols(rw *w) {
  return (w->viewporter ? HAS_VIEWPORTER : 0) | (w->fractional ? HAS_FRACTIONAL : 0) | (w->cursor_manager ? HAS_CURSOR_SHAPE : 0)
       | (w->decoration_manager ? HAS_DECORATION : 0) | (w->text_input ? HAS_TEXT_INPUT : 0) | (w->data_device ? HAS_DATA_DEVICE : 0)
       | (w->seat ? HAS_SEAT : 0) | (w->pointer ? HAS_POINTER : 0) | (w->keyboard ? HAS_KEYBOARD : 0)
       | (w->server_decorated ? SERVER_DECORATED : 0);
}
/* Logical size, scale in 120ths, key repeat (rate per second, delay ms). */
void rw_state(rw *w, int32_t *width, int32_t *height, int32_t *scale120, int32_t *rate, int32_t *delay) {
  *width = w->width;
  *height = w->height;
  *scale120 = w->applied_scale120 > 0 ? w->applied_scale120 : 120;
  *rate = w->repeat_rate;
  *delay = w->repeat_delay;
}

/* Reads and dispatches what arrived, waiting at most timeout_ms (-1: until something does). 0, or -1 once the
 * connection is lost. Safe beside the Vulkan WSI, which reads its own queue on the same display. */
int32_t rw_dispatch(rw *w, int32_t timeout_ms) {
  struct wl_display *d = w->display;
  if (w->failed || wl_display_dispatch_pending(d) < 0) return -1;
  while (wl_display_prepare_read(d) != 0)
    if (wl_display_dispatch_pending(d) < 0) return -1;
  if (wl_display_flush(d) < 0 && errno != EAGAIN) { wl_display_cancel_read(d); return -1; }
  struct pollfd fds[2 + MAX_WRITES];
  int count = 0;
  fds[count++] = (struct pollfd){.fd = wl_display_get_fd(d), .events = POLLIN};
  if (w->paste_fd >= 0) fds[count++] = (struct pollfd){.fd = w->paste_fd, .events = POLLIN};
  for (int i = 0; i < w->write_count; ++i) fds[count++] = (struct pollfd){.fd = w->writes[i].fd, .events = POLLOUT};
  /* Events queued by an earlier dispatch must not wait for the socket. */
  if (w->queue_len > w->queue_head) timeout_ms = 0;
  int ready = poll(fds, (nfds_t)count, timeout_ms);
  if (ready > 0 && (fds[0].revents & POLLIN)) {
    if (wl_display_read_events(d) < 0) return -1;
  } else {
    wl_display_cancel_read(d);
    if (ready > 0 && (fds[0].revents & (POLLERR | POLLHUP))) return -1;
  }
  if (wl_display_dispatch_pending(d) < 0) return -1;
  service(w);
  wl_display_flush(d);
  return w->failed ? -1 : 0;
}

/* Pops the oldest event into *out; 0 when the queue is empty. */
int32_t rw_next(rw *w, rw_event *out) {
  if (w->queue_head == w->queue_len) { w->queue_head = w->queue_len = 0; return 0; }
  *out = w->queue[w->queue_head++];
  return 1;
}
void rw_event_free(char *text) { free(text); }

void rw_resize(rw *w, int32_t width, int32_t height) {
  w->width = width;
  w->height = height;
  apply_geometry(w);
}
void rw_set_resizable(rw *w, int32_t enabled) {
  xdg_toplevel_set_min_size(w->toplevel, enabled ? 0 : w->width, enabled ? 0 : w->height);
  xdg_toplevel_set_max_size(w->toplevel, enabled ? 0 : w->width, enabled ? 0 : w->height);
  wl_surface_commit(w->surface);
}
/* 0 set (or remembered for the next pointer enter); 1 no cursor-shape protocol. */
int32_t rw_set_cursor(rw *w, int32_t shape) {
  w->cursor_shape = shape;
  set_cursor_now(w);
  return w->cursor_manager ? 0 : 1;
}
/* 0 applied (or applied when the surface gains text focus); 1 no text-input-v3. */
int32_t rw_set_text_input(rw *w, int32_t enabled, int32_t x, int32_t y, int32_t width, int32_t height) {
  w->text_wanted = enabled;
  w->text_rect[0] = x; w->text_rect[1] = y; w->text_rect[2] = width; w->text_rect[3] = height;
  text_sync(w);
  return w->text_input ? 0 : 1;
}
/* 0 offered as the selection; 1 no data device; 4 out of memory. */
int32_t rw_clipboard_set(rw *w, const char *text) {
  if (!w->data_device) return 1;
  char *owned = strdup(text);
  if (!owned) return 4;
  if (w->source) wl_data_source_destroy(w->source);
  free(w->source_text);
  w->source_text = owned;
  w->source = wl_data_device_manager_create_data_source(w->data_manager);
  wl_data_source_add_listener(w->source, &source_listener, w);
  for (int i = 0; i < TEXT_MIMES; ++i) wl_data_source_offer(w->source, text_mimes[i]);
  wl_data_device_set_selection(w->data_device, w->source, w->serial);
  wl_display_flush(w->display);
  return 0;
}
/* Starts reading the selection; an RW_PASTE event follows (empty text when there is none). Returns where the text
 * comes from: 0 the compositor's selection offer, 1 this window's own source (no offer was sent to it), 2 none. */
int32_t rw_request_paste(rw *w) {
  if (w->paste_fd >= 0) return 0;
  intptr_t mimes = w->selection ? (intptr_t)wl_data_offer_get_user_data(w->selection) : 0;
  int fds[2];
  for (int i = 0; i < TEXT_MIMES; ++i) {
    if (!(mimes & (1 << i)) || pipe2(fds, O_CLOEXEC | O_NONBLOCK) != 0) continue;
    wl_data_offer_receive(w->selection, text_mimes[i], fds[1]);
    close(fds[1]);
    w->paste_fd = fds[0];
    wl_display_flush(w->display);
    return 0;
  }
  push(w, (rw_event){.kind = RW_PASTE, .text = copy(w->source_text)});
  return w->source_text ? 1 : 2;
}
/* Synchronous clipboard text: this window's own selection, else the offer read through a pipe until EOF or timeout_ms.
 * NULL when there is no text. The writer is another client, so nothing here needs this connection dispatched. */
char *rw_clipboard_get(rw *w, int32_t timeout_ms) {
  if (w->source_text) return copy(w->source_text);
  intptr_t mimes = w->selection ? (intptr_t)wl_data_offer_get_user_data(w->selection) : 0;
  for (int i = 0; i < TEXT_MIMES; ++i) {
    int fds[2];
    if (!(mimes & (1 << i)) || pipe2(fds, O_CLOEXEC) != 0) continue;
    wl_data_offer_receive(w->selection, text_mimes[i], fds[1]);
    close(fds[1]);
    wl_display_flush(w->display);
    char *text = NULL;
    size_t len = 0;
    struct pollfd fd = {.fd = fds[0], .events = POLLIN};
    while (poll(&fd, 1, timeout_ms) > 0) {
      char chunk[4096];
      ssize_t n = read(fds[0], chunk, sizeof chunk);
      if (n < 0 && errno == EINTR) continue;
      if (n <= 0) break;
      char *grown = realloc(text, len + (size_t)n + 1);
      if (!grown) break;
      memcpy(grown + len, chunk, (size_t)n);
      len += (size_t)n;
      grown[len] = 0;
      text = grown;
    }
    close(fds[0]);
    return text ? text : copy("");
  }
  return NULL;
}
/* Relative pointer mode: hide the cursor, lock the pointer in place and report RW_RELATIVE motion.
 * Returns which halves are missing: 1 no pointer constraints (the pointer is not locked), 2 no relative pointer. */
int32_t rw_set_relative(rw *w, int32_t enabled) {
  w->relative = enabled;
  if (w->locked_pointer) { zwp_locked_pointer_v1_destroy(w->locked_pointer); w->locked_pointer = NULL; }
  if (w->relative_pointer) { zwp_relative_pointer_v1_destroy(w->relative_pointer); w->relative_pointer = NULL; }
  if (enabled && w->pointer && w->constraints)
    w->locked_pointer = zwp_pointer_constraints_v1_lock_pointer(w->constraints, w->surface, w->pointer, NULL,
                                                                ZWP_POINTER_CONSTRAINTS_V1_LIFETIME_PERSISTENT);
  if (enabled && w->pointer && w->relative_manager) {
    w->relative_pointer = zwp_relative_pointer_manager_v1_get_relative_pointer(w->relative_manager, w->pointer);
    zwp_relative_pointer_v1_add_listener(w->relative_pointer, &relative_listener, w);
  }
  set_cursor_now(w);
  wl_display_flush(w->display);
  return (enabled && !w->locked_pointer ? 1 : 0) | (enabled && !w->relative_pointer ? 2 : 0);
}
