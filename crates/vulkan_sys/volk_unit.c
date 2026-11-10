/* volk's implementation, vendored as one file. */
#ifdef _WIN32
#define VK_USE_PLATFORM_WIN32_KHR
#else
#define VK_USE_PLATFORM_WAYLAND_KHR
#endif
#include "vendor/volk.c"
