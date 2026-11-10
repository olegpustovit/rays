/* Test-only: connect to the running Wayland compositor (WSLg, weston, ...) and make a bare wl_surface. */
#include <wayland-client.h>
#include <string.h>

static void global(void *data, struct wl_registry *registry, uint32_t name, const char *interface, uint32_t version) {
  if (!strcmp(interface, "wl_compositor"))
    *(struct wl_compositor **)data = wl_registry_bind(registry, name, &wl_compositor_interface, version < 4 ? version : 4);
}
static void global_remove(void *data, struct wl_registry *registry, uint32_t name) { (void)data; (void)registry; (void)name; }
static const struct wl_registry_listener listener = {global, global_remove};

/* 0 on success; the display and surface then live for the process. */
int rays_wl_probe_open(void **display, void **surface) {
  struct wl_display *d = wl_display_connect(NULL);
  if (!d) return 1;
  struct wl_compositor *compositor = NULL;
  struct wl_registry *registry = wl_display_get_registry(d);
  wl_registry_add_listener(registry, &listener, &compositor);
  wl_display_roundtrip(d);
  if (!compositor) return 2;
  *display = d;
  *surface = wl_compositor_create_surface(compositor);
  return *surface ? 0 : 3;
}
