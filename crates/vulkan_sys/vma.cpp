// The one C++ file: Vulkan Memory Allocator's implementation, over volk's function pointers.
#define VK_NO_PROTOTYPES
#include "vendor/volk.h"
#include <cstdio>
#include <cstdint>
#define VMA_STATIC_VULKAN_FUNCTIONS 0
#define VMA_DYNAMIC_VULKAN_FUNCTIONS 1
#define VMA_IMPLEMENTATION
#include "vendor/vk_mem_alloc.h"
