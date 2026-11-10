/* A thin libdbus-1 layer: connections and messages with an iterator stack inside, so Rust never lays out
 * DBusMessageIter. Every protocol decision lives in Rust (src/wire.rs is the safe side). Strings come from
 * Rust as (pointer, length) and are copied NUL-terminated; libdbus validates UTF-8 and object paths. */
#include <dbus/dbus.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define AX_DEPTH 16

typedef struct {
  DBusMessage *m;
  DBusMessageIter it[AX_DEPTH];
  int depth; /* index of the current iterator */
  int failed; /* a write failed (out of memory or invalid data): the message is not sent */
  char *buf;
  size_t cap;
} ax_msg;

static void copy_error(DBusError *e, char *out, size_t len) {
  if (len) {
    strncpy(out, e->message ? e->message : (e->name ? e->name : "D-Bus error"), len - 1);
    out[len - 1] = 0;
  }
  dbus_error_free(e);
}

static ax_msg *wrap(DBusMessage *m, int reading) {
  if (!m) return NULL;
  ax_msg *x = calloc(1, sizeof *x);
  if (!x) { dbus_message_unref(m); return NULL; }
  x->m = m;
  if (reading) dbus_message_iter_init(m, &x->it[0]);
  else dbus_message_iter_init_append(m, &x->it[0]);
  return x;
}

DBusConnection *ax_open(const char *address, char *err, size_t errlen) {
  DBusError e;
  dbus_error_init(&e);
  DBusConnection *c;
  if (address) {
    c = dbus_connection_open_private(address, &e);
    if (c && !dbus_bus_register(c, &e)) {
      dbus_connection_close(c);
      dbus_connection_unref(c);
      c = NULL;
    }
  } else {
    c = dbus_bus_get_private(DBUS_BUS_SESSION, &e);
  }
  if (!c) { copy_error(&e, err, errlen); return NULL; }
  dbus_connection_set_exit_on_disconnect(c, FALSE);
  return c;
}

void ax_close(DBusConnection *c) {
  dbus_connection_close(c);
  dbus_connection_unref(c);
}

int ax_fd(DBusConnection *c) {
  int fd = -1;
  return dbus_connection_get_unix_fd(c, &fd) ? fd : -1;
}

const char *ax_unique_name(DBusConnection *c) { return dbus_bus_get_unique_name(c); }

int ax_add_match(DBusConnection *c, const char *rule, char *err, size_t errlen) {
  DBusError e;
  dbus_error_init(&e);
  dbus_bus_add_match(c, rule, &e);
  if (dbus_error_is_set(&e)) { copy_error(&e, err, errlen); return 0; }
  return 1;
}

ax_msg *ax_new_call(const char *dest, const char *path, const char *iface, const char *member) {
  return wrap(dbus_message_new_method_call(dest, path, iface, member), 0);
}
ax_msg *ax_new_signal(const char *path, const char *iface, const char *member) {
  return wrap(dbus_message_new_signal(path, iface, member), 0);
}
ax_msg *ax_new_reply(ax_msg *call) { return wrap(dbus_message_new_method_return(call->m), 0); }
ax_msg *ax_new_error(ax_msg *call, const char *name, const char *text) {
  return wrap(dbus_message_new_error(call->m, name, text), 0);
}

void ax_free(ax_msg *x) {
  if (!x) return;
  dbus_message_unref(x->m);
  free(x->buf);
  free(x);
}

/* A basic value; strings, object paths and signatures as (p, len). */
void ax_put(ax_msg *x, int type, const void *p, size_t len) {
  if (x->failed) return;
  if (type == DBUS_TYPE_STRING || type == DBUS_TYPE_OBJECT_PATH || type == DBUS_TYPE_SIGNATURE) {
    if (len + 1 > x->cap) {
      char *b = realloc(x->buf, len + 1);
      if (!b) { x->failed = 1; return; }
      x->buf = b;
      x->cap = len + 1;
    }
    memcpy(x->buf, p, len);
    x->buf[len] = 0;
    /* a NUL inside the text ends it; invalid UTF-8 or paths are refused below instead of aborting */
    if (type == DBUS_TYPE_STRING && !dbus_validate_utf8(x->buf, NULL)) { x->failed = 1; return; }
    if (type == DBUS_TYPE_OBJECT_PATH && !dbus_validate_path(x->buf, NULL)) { x->failed = 1; return; }
    const char *s = x->buf;
    if (!dbus_message_iter_append_basic(&x->it[x->depth], type, &s)) x->failed = 1;
  } else if (!dbus_message_iter_append_basic(&x->it[x->depth], type, p)) {
    x->failed = 1;
  }
}

/* Open a container: 'a' (sig = element), 'v' (sig = contents), 'r' struct, 'e' dict entry (sig NULL). */
void ax_open_container(ax_msg *x, int type, const char *sig) {
  if (x->failed) return;
  if (x->depth + 1 >= AX_DEPTH) { x->failed = 1; return; }
  if (!dbus_message_iter_open_container(&x->it[x->depth], type, sig, &x->it[x->depth + 1])) { x->failed = 1; return; }
  x->depth++;
}

void ax_close_container(ax_msg *x) {
  if (x->depth == 0) { x->failed = 1; return; }
  if (x->failed) {
    dbus_message_iter_abandon_container_if_open(&x->it[x->depth - 1], &x->it[x->depth]);
  } else if (!dbus_message_iter_close_container(&x->it[x->depth - 1], &x->it[x->depth])) {
    x->failed = 1;
  }
  x->depth--;
}

/* Send and free; the serial, or 0 when the message was malformed or could not be queued. */
uint32_t ax_send(DBusConnection *c, ax_msg *x) {
  dbus_uint32_t serial = 0;
  if (!x->failed && x->depth == 0 && !dbus_connection_send(c, x->m, &serial)) serial = 0;
  ax_free(x);
  return serial;
}

void ax_flush(DBusConnection *c) { dbus_connection_flush(c); }

/* Send `call` (freed) and wait for its reply, positioned at the first argument. */
ax_msg *ax_call_block(DBusConnection *c, ax_msg *call, int timeout_ms, char *err, size_t errlen) {
  if (call->failed) { ax_free(call); strncpy(err, "malformed call", errlen); return NULL; }
  DBusError e;
  dbus_error_init(&e);
  DBusMessage *r = dbus_connection_send_with_reply_and_block(c, call->m, timeout_ms, &e);
  ax_free(call);
  if (!r) { copy_error(&e, err, errlen); return NULL; }
  return wrap(r, 1);
}

/* The next incoming message without blocking (NULL: none), positioned at its first argument. Sets
 * *connected to 0 once the bus has gone. */
ax_msg *ax_pop(DBusConnection *c, int *connected) {
  *connected = dbus_connection_read_write(c, 0) ? 1 : 0;
  return wrap(dbus_connection_pop_message(c), 1);
}

/* Wait up to timeout_ms for input, then pop (for tests and blocking hosts). */
ax_msg *ax_wait(DBusConnection *c, int timeout_ms, int *connected) {
  DBusMessage *m = dbus_connection_pop_message(c);
  if (m) { *connected = 1; return wrap(m, 1); }
  *connected = dbus_connection_read_write(c, timeout_ms) ? 1 : 0;
  return wrap(dbus_connection_pop_message(c), 1);
}

int ax_kind(ax_msg *x) { return dbus_message_get_type(x->m); }
const char *ax_path(ax_msg *x) { return dbus_message_get_path(x->m); }
const char *ax_interface(ax_msg *x) { return dbus_message_get_interface(x->m); }
const char *ax_member(ax_msg *x) { return dbus_message_get_member(x->m); }
const char *ax_error_name(ax_msg *x) { return dbus_message_get_error_name(x->m); }
const char *ax_signature(ax_msg *x) { return dbus_message_get_signature(x->m); }
uint32_t ax_reply_serial(ax_msg *x) { return dbus_message_get_reply_serial(x->m); }
int ax_failed(ax_msg *x) { return x->failed; }

/* Reading: the current argument's type (0 at the end), its basic value, and moving through containers. */
int ax_arg_type(ax_msg *x) { return dbus_message_iter_get_arg_type(&x->it[x->depth]); }
void ax_get(ax_msg *x, void *out) { dbus_message_iter_get_basic(&x->it[x->depth], out); }
int ax_next(ax_msg *x) { return dbus_message_iter_next(&x->it[x->depth]); }
int ax_recurse(ax_msg *x) {
  if (x->depth + 1 >= AX_DEPTH) return 0;
  int t = dbus_message_iter_get_arg_type(&x->it[x->depth]);
  if (t != DBUS_TYPE_ARRAY && t != DBUS_TYPE_STRUCT && t != DBUS_TYPE_VARIANT && t != DBUS_TYPE_DICT_ENTRY) return 0;
  dbus_message_iter_recurse(&x->it[x->depth], &x->it[x->depth + 1]);
  x->depth++;
  return 1;
}
void ax_up(ax_msg *x) {
  if (x->depth > 0) x->depth--;
}
