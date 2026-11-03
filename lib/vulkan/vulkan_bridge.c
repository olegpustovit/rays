/* The Vulkan bridge: operations shaped like OGPU's driver records, not one stub per Vulkan call
   (lib/vulkan/AGENTS.md says why). A handle is a C pointer as an OCaml int; a negative int is a
   VkResult. Recording happens on the initial domain; only rv_buffer_invalidate and the staged reads
   may run on another domain, and they share nothing with recording but qlock and xlock. */
#define VK_NO_PROTOTYPES
#ifdef _WIN32
#define VK_USE_PLATFORM_WIN32_KHR
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef interface /* windows.h makes it a macro */
#endif
#include "vendor/volk.h"
#include "vendor/vk_mem_alloc.h"
#include "native_layer_token.h"
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <pthread.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define H(v) ((void *)Long_val(v))
#define Val_h(p) Val_long((intnat)(p))
#define MAX_SLOTS 32 /* descriptors one pipeline may push */
#define MAX_BIND 64  /* binding numbers, which may have gaps */
#define RV_NO_DEVICE (-100000) /* no discrete adapter with what the backend requires */
#define RV_MISSING (-100001)   /* the adapter lacks a required feature */
#define CHUNK_BYTES (1 << 20)
#define DURATIONS 16

typedef struct rv_device rv_device;
typedef struct rv_queue rv_queue;
typedef struct rv_cmd rv_cmd;

typedef struct rv_chunk {
  struct rv_chunk *next;
  VkBuffer buffer;
  VmaAllocation allocation;
  char *map;
  VkDeviceSize size, used;
} rv_chunk;

typedef struct rv_garbage {
  struct rv_garbage *next;
  uint64_t tick;
  void (*destroy)(rv_device *, void *);
  void *payload;
} rv_garbage;

struct rv_device {
  rv_device *next_live;
  VkInstance instance;
  VkPhysicalDevice physical;
  VkDevice device;
  VmaAllocator allocator;
  VkQueue main_queue, transfer_queue;
  uint32_t main_family, transfer_family;
  VkPhysicalDeviceProperties properties;
  bool budget;
  VkPipelineCache pipeline_cache;
  int64_t held; /* bytes of live resources: what allocated_size answers, atomic */
  /* Every submit signals this timeline too: deferred destruction counts on it, across queues. */
  VkSemaphore ticks;
  uint64_t next_tick;
  rv_garbage *garbage;
  rv_chunk *free_chunks;
  pthread_mutex_t qlock; /* recursive: the main queue, the pending uploads, the free chunks */
  rv_queue *upload_queue;
  rv_cmd *upload;
  pthread_mutex_t xlock; /* the transfer queue and its pool */
  VkCommandPool transfer_pool;
  VkCommandBuffer transfer_cb;
  VkFence transfer_fence;
};

typedef struct {
  int kind; /* 0 unbound, 1 buffer, 2 image, 3 sampler, 4 acceleration structure */
  VkBuffer buffer;
  VkDeviceSize offset, range;
  VkImageView view;
  VkSampler sampler;
  VkAccelerationStructureKHR accel;
} rv_slot;

typedef struct {
  uint32_t binding;
  VkDescriptorType type;
} rv_binding;

typedef struct {
  rv_device *d;
  VkPipeline pipeline;
  VkPipelineLayout layout;
  VkDescriptorSetLayout set_layout;
  VkPipelineBindPoint point;
  uint32_t count;
  rv_binding bindings[MAX_SLOTS];
} rv_pipeline;

struct rv_cmd {
  rv_cmd *next;
  rv_cmd *all; /* every command the queue made: one a caller began and dropped goes with the queue */
  rv_queue *q;
  VkCommandBuffer cb;
  VkQueryPool timestamps;
  uint64_t epoch, tick;
  rv_chunk *chunks;
  rv_cmd *attached_upload;
  rv_slot slots[MAX_BIND];
  bool dirty, dispatched;
  rv_pipeline *pipeline;
  VkBuffer index_buffer;
  VkDeviceSize index_offset;
  int rendering;
  uint32_t target_height;
};

struct rv_queue {
  rv_device *d;
  VkSemaphore timeline;
  uint64_t next_epoch;
  VkCommandPool pool;
  rv_cmd *free_list, *flight_head, *flight_tail, *commands;
  double total_seconds;
  uint64_t duration_epochs[DURATIONS];
  double durations[DURATIONS];
  uint32_t duration_at;
};

typedef struct {
  rv_device *d;
  VkBuffer buffer;
  VmaAllocation allocation;
  VkDeviceSize size;
  char *map; /* host-visible memory, NULL for a device-local buffer */
} rv_buffer;

typedef struct {
  rv_device *d;
  VkImage image;
  VmaAllocation allocation;
  VkImageView view;
  VkFormat format;
  VkImageAspectFlags aspect;
  uint32_t width, height, depth, mips, samples;
  VmaAllocationInfo info;
} rv_texture;

static int validation_messages = 0;
static bool validation_fatal = false;
static bool validation_silent = false;
static rv_device *exit_devices = NULL; /* every live device under validation */
/* One instance for the process, whatever devices come and go: destroying the last one unloads the
   driver while its worker threads may still be starting, which faults inside the driver. */
static VkInstance process_instance = VK_NULL_HANDLE;
static VkDebugUtilsMessengerEXT process_messenger = VK_NULL_HANDLE;

static VKAPI_ATTR VkBool32 VKAPI_CALL on_message(VkDebugUtilsMessageSeverityFlagBitsEXT severity,
                                                 VkDebugUtilsMessageTypeFlagsEXT types,
                                                 const VkDebugUtilsMessengerCallbackDataEXT *data,
                                                 void *user) {
  (void)types;
  (void)user;
  if (severity < VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT || validation_silent) return VK_FALSE;
  __atomic_add_fetch(&validation_messages, 1, __ATOMIC_RELAXED);
  fprintf(stderr, "vulkan validation: %s\n", data->pMessage);
  fflush(stderr);
  /* Any message fails the test that caused it. ExitProcess from inside the driver's call detaches
     DLLs the call still holds locks in and hangs, so Windows ends the process outright. */
#ifdef _WIN32
  if (validation_fatal) TerminateProcess(GetCurrentProcess(), 70);
#else
  if (validation_fatal) _exit(70);
#endif
  return VK_FALSE;
}

/* ---- device ---- */

static bool has_extension(VkExtensionProperties *list, uint32_t count, const char *name) {
  for (uint32_t i = 0; i < count; i++)
    if (strcmp(list[i].extensionName, name) == 0) return true;
  return false;
}

static int family_with(rv_device *d, VkQueueFlags want, VkQueueFlags refuse) {
  VkQueueFamilyProperties families[16];
  uint32_t count = 16;
  vkGetPhysicalDeviceQueueFamilyProperties(d->physical, &count, families);
  for (uint32_t i = 0; i < count; i++)
    if ((families[i].queueFlags & want) == want && !(families[i].queueFlags & refuse)) return (int)i;
  return -1;
}

/* A process leaving with a live device (a failing test) must release it here: the validation layer
   joins its threads in DLL shutdown, under the loader lock, and never returns. */
static void leave_device(void) {
  validation_silent = true;
  while (exit_devices) {
    rv_device *d = exit_devices;
    exit_devices = d->next_live;
    vkDeviceWaitIdle(d->device);
    vkDestroyDevice(d->device, NULL);
  }
  if (process_instance) {
    if (process_messenger) vkDestroyDebugUtilsMessengerEXT(process_instance, process_messenger, NULL);
    vkDestroyInstance(process_instance, NULL);
    process_instance = VK_NULL_HANDLE;
  }
}

/* A dedicated copy queue where the device has one; otherwise a second queue of the main family, or
   the same one under qlock's neighbour xlock, which is still externally synchronised per VkQueue. */
static intnat device_create(bool validation) {
  VkResult r = volkInitialize();
  if (r != VK_SUCCESS) return RV_NO_DEVICE;
  if (volkGetInstanceVersion() < VK_API_VERSION_1_3) return RV_NO_DEVICE;
  rv_device *d = calloc(1, sizeof *d);
  VkApplicationInfo app = {.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO, .pApplicationName = "rays",
                           .pEngineName = "rays", .apiVersion = VK_API_VERSION_1_3};
  const char *layers[] = {"VK_LAYER_KHRONOS_validation"};
  const char *extensions[] = {VK_KHR_SURFACE_EXTENSION_NAME,
#ifdef _WIN32
                              VK_KHR_WIN32_SURFACE_EXTENSION_NAME,
#endif
                              VK_EXT_DEBUG_UTILS_EXTENSION_NAME};
  uint32_t extension_count = sizeof extensions / sizeof *extensions;
  VkValidationFeatureEnableEXT enables[] = {VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT};
  VkValidationFeaturesEXT features = {.sType = VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT,
                                      .enabledValidationFeatureCount = 1,
                                      .pEnabledValidationFeatures = enables};
  VkInstanceCreateInfo instance_info = {.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
                                        .pNext = validation ? &features : NULL,
                                        .pApplicationInfo = &app,
                                        .enabledLayerCount = validation ? 1 : 0,
                                        .ppEnabledLayerNames = layers,
                                        .enabledExtensionCount = extension_count - (validation ? 0 : 1),
                                        .ppEnabledExtensionNames = extensions};
  if (!process_instance) {
    r = vkCreateInstance(&instance_info, NULL, &process_instance);
    if (r != VK_SUCCESS) { free(d); return r; }
    volkLoadInstance(process_instance);
  }
  d->instance = process_instance;
  if (validation && !process_messenger) {
    VkDebugUtilsMessengerCreateInfoEXT info = {
        .sType = VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT,
        .messageSeverity = VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
        .messageType = VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT |
                       VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
        .pfnUserCallback = on_message};
    vkCreateDebugUtilsMessengerEXT(d->instance, &info, NULL, &process_messenger);
  }
  VkPhysicalDevice physicals[8];
  uint32_t physical_count = 8;
  r = vkEnumeratePhysicalDevices(d->instance, &physical_count, physicals);
  for (uint32_t i = 0; r >= 0 && i < physical_count; i++) {
    VkPhysicalDeviceProperties p;
    vkGetPhysicalDeviceProperties(physicals[i], &p);
    if (p.deviceType == VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU && p.apiVersion >= VK_API_VERSION_1_3) {
      d->physical = physicals[i];
      d->properties = p;
      break;
    }
  }
  intnat failure = 0;
  if (!d->physical) failure = RV_NO_DEVICE;
  VkExtensionProperties available[512];
  uint32_t available_count = 512;
  if (!failure) vkEnumerateDeviceExtensionProperties(d->physical, NULL, &available_count, available);
  const char *required[] = {VK_KHR_SWAPCHAIN_EXTENSION_NAME, VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME,
                            VK_KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME, VK_KHR_RAY_QUERY_EXTENSION_NAME,
                            VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME};
  const char *enabled[12];
  uint32_t enabled_count = 0;
  for (uint32_t i = 0; !failure && i < sizeof required / sizeof *required; i++) {
    if (!has_extension(available, available_count, required[i])) failure = RV_MISSING;
    enabled[enabled_count++] = required[i];
  }
  if (failure) {
    free(d);
    return failure;
  }
  /* Metal fixes a pipeline to no topology class and lets a draw leave a binding unwritten when it
     never reads it. Both are one extension here; neither is optional for the renderer's pipelines. */
  if (!has_extension(available, available_count, VK_EXT_EXTENDED_DYNAMIC_STATE_3_EXTENSION_NAME) ||
      !has_extension(available, available_count, VK_EXT_ROBUSTNESS_2_EXTENSION_NAME)) {
    free(d);
    return RV_MISSING;
  }
  enabled[enabled_count++] = VK_EXT_EXTENDED_DYNAMIC_STATE_3_EXTENSION_NAME;
  enabled[enabled_count++] = VK_EXT_ROBUSTNESS_2_EXTENSION_NAME;
  if (has_extension(available, available_count, VK_EXT_MEMORY_BUDGET_EXTENSION_NAME)) {
    enabled[enabled_count++] = VK_EXT_MEMORY_BUDGET_EXTENSION_NAME;
    d->budget = true;
  }
  int main_family = family_with(d, VK_QUEUE_GRAPHICS_BIT | VK_QUEUE_COMPUTE_BIT | VK_QUEUE_TRANSFER_BIT, 0);
  int transfer_family = family_with(d, VK_QUEUE_TRANSFER_BIT, VK_QUEUE_GRAPHICS_BIT | VK_QUEUE_COMPUTE_BIT | VK_QUEUE_VIDEO_DECODE_BIT_KHR | VK_QUEUE_VIDEO_ENCODE_BIT_KHR);
  bool shared_family = transfer_family < 0;
  if (shared_family) transfer_family = main_family;
  d->main_family = (uint32_t)main_family;
  d->transfer_family = (uint32_t)transfer_family;
  float priorities[2] = {1.f, 1.f};
  VkDeviceQueueCreateInfo queues[2] = {
      {.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO, .queueFamilyIndex = d->main_family,
       .queueCount = shared_family ? 2 : 1, .pQueuePriorities = priorities},
      {.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO, .queueFamilyIndex = d->transfer_family,
       .queueCount = 1, .pQueuePriorities = priorities}};
  VkPhysicalDeviceRobustness2FeaturesEXT robustness = {.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT, .nullDescriptor = VK_TRUE};
  VkPhysicalDeviceRayQueryFeaturesKHR ray_query = {.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR, .pNext = &robustness, .rayQuery = VK_TRUE};
  VkPhysicalDeviceAccelerationStructureFeaturesKHR accel = {
      .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR, .pNext = &ray_query, .accelerationStructure = VK_TRUE};
  VkPhysicalDeviceVulkan13Features v13 = {.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES, .pNext = &accel,
      .dynamicRendering = VK_TRUE, .synchronization2 = VK_TRUE, .maintenance4 = VK_TRUE};
  VkPhysicalDeviceVulkan12Features v12 = {.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES, .pNext = &v13,
      .timelineSemaphore = VK_TRUE, .bufferDeviceAddress = VK_TRUE, .scalarBlockLayout = VK_TRUE,
      .storageBuffer8BitAccess = VK_TRUE, .uniformAndStorageBuffer8BitAccess = VK_TRUE, .shaderInt8 = VK_TRUE,
      .shaderFloat16 = VK_TRUE, .hostQueryReset = VK_TRUE, .uniformBufferStandardLayout = VK_TRUE,
      .vulkanMemoryModel = VK_TRUE, .vulkanMemoryModelDeviceScope = VK_TRUE};
  VkPhysicalDeviceVulkan11Features v11 = {.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES, .pNext = &v12,
      .storageBuffer16BitAccess = VK_TRUE, .uniformAndStorageBuffer16BitAccess = VK_TRUE, .shaderDrawParameters = VK_TRUE};
  VkPhysicalDeviceFeatures2 features2 = {.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2, .pNext = &v11,
      .features = {.shaderFloat64 = VK_TRUE, .shaderInt64 = VK_TRUE, .shaderInt16 = VK_TRUE, .samplerAnisotropy = VK_TRUE,
                   .fragmentStoresAndAtomics = VK_TRUE, .vertexPipelineStoresAndAtomics = VK_TRUE, .multiDrawIndirect = VK_TRUE,
                   .drawIndirectFirstInstance = VK_TRUE, .imageCubeArray = VK_TRUE, .independentBlend = VK_TRUE,
                   .largePoints = VK_TRUE, .sampleRateShading = VK_TRUE, .shaderStorageImageWriteWithoutFormat = VK_TRUE,
                   .shaderStorageImageReadWithoutFormat = VK_TRUE}};
  VkDeviceCreateInfo device_info = {.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO, .pNext = &features2,
      .queueCreateInfoCount = shared_family ? 1 : 2, .pQueueCreateInfos = queues,
      .enabledExtensionCount = enabled_count, .ppEnabledExtensionNames = enabled};
  r = vkCreateDevice(d->physical, &device_info, NULL, &d->device);
  if (r != VK_SUCCESS) {
    free(d);
    return r == VK_ERROR_FEATURE_NOT_PRESENT || r == VK_ERROR_EXTENSION_NOT_PRESENT ? RV_MISSING : r;
  }
  volkLoadDevice(d->device);
  vkGetDeviceQueue(d->device, d->main_family, 0, &d->main_queue);
  vkGetDeviceQueue(d->device, d->transfer_family, shared_family ? 1 : 0, &d->transfer_queue);
  VmaVulkanFunctions functions = {.vkGetInstanceProcAddr = vkGetInstanceProcAddr, .vkGetDeviceProcAddr = vkGetDeviceProcAddr};
  VmaAllocatorCreateInfo allocator_info = {.flags = VMA_ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT | (d->budget ? VMA_ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT : 0),
      .physicalDevice = d->physical, .device = d->device, .pVulkanFunctions = &functions, .instance = d->instance,
      .vulkanApiVersion = VK_API_VERSION_1_3};
  r = vmaCreateAllocator(&allocator_info, &d->allocator);
  if (r != VK_SUCCESS) return r;
  VkPipelineCacheCreateInfo cache_info = {.sType = VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO};
  vkCreatePipelineCache(d->device, &cache_info, NULL, &d->pipeline_cache);
  VkSemaphoreTypeCreateInfo timeline = {.sType = VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO, .semaphoreType = VK_SEMAPHORE_TYPE_TIMELINE};
  VkSemaphoreCreateInfo semaphore = {.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO, .pNext = &timeline};
  vkCreateSemaphore(d->device, &semaphore, NULL, &d->ticks);
  d->next_tick = 1;
  pthread_mutexattr_t recursive;
  pthread_mutexattr_init(&recursive);
  pthread_mutexattr_settype(&recursive, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(&d->qlock, &recursive);
  pthread_mutexattr_destroy(&recursive);
  pthread_mutex_init(&d->xlock, NULL);
  VkCommandPoolCreateInfo pool = {.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
      .flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT, .queueFamilyIndex = d->transfer_family};
  vkCreateCommandPool(d->device, &pool, NULL, &d->transfer_pool);
  VkCommandBufferAllocateInfo alloc = {.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO, .commandPool = d->transfer_pool,
      .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY, .commandBufferCount = 1};
  vkAllocateCommandBuffers(d->device, &alloc, &d->transfer_cb);
  VkFenceCreateInfo fence = {.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO};
  vkCreateFence(d->device, &fence, NULL, &d->transfer_fence);
  return (intnat)d;
}

CAMLprim value rv_device_create(value validation, value fatal) {
  validation_fatal = Bool_val(fatal);
  intnat created = device_create(Bool_val(validation));
  if (created > 0 && Bool_val(validation)) {
    static bool registered = false;
    if (!registered) atexit(leave_device);
    registered = true;
    ((rv_device *)created)->next_live = exit_devices;
    exit_devices = (rv_device *)created;
  }
  return Val_long(created);
}

CAMLprim value rv_validation_messages(value unit) {
  (void)unit;
  return Val_int(__atomic_load_n(&validation_messages, __ATOMIC_RELAXED));
}

/* [max_buffer; max_texture; max_samples; max_invocations; device_local_bytes; api_version] */
CAMLprim value rv_device_facts(value device) {
  CAMLparam1(device);
  CAMLlocal1(out);
  rv_device *d = H(device);
  VkPhysicalDeviceLimits *l = &d->properties.limits;
  VkPhysicalDeviceMemoryProperties memory;
  vkGetPhysicalDeviceMemoryProperties(d->physical, &memory);
  VkDeviceSize local = 0;
  for (uint32_t i = 0; i < memory.memoryHeapCount; i++)
    if ((memory.memoryHeaps[i].flags & VK_MEMORY_HEAP_DEVICE_LOCAL_BIT) && memory.memoryHeaps[i].size > local)
      local = memory.memoryHeaps[i].size;
  VkSampleCountFlags samples = l->framebufferColorSampleCounts & l->framebufferDepthSampleCounts;
  int max_samples = samples & VK_SAMPLE_COUNT_16_BIT ? 16 : samples & VK_SAMPLE_COUNT_8_BIT ? 8 : samples & VK_SAMPLE_COUNT_4_BIT ? 4 : 1;
  out = caml_alloc_tuple(6);
  Store_field(out, 0, Val_long((intnat)l->maxStorageBufferRange));
  Store_field(out, 1, Val_long((intnat)l->maxImageDimension2D));
  Store_field(out, 2, Val_long(max_samples));
  Store_field(out, 3, Val_long((intnat)l->maxComputeWorkGroupInvocations));
  Store_field(out, 4, Val_long((intnat)local));
  Store_field(out, 5, Val_long((intnat)d->properties.apiVersion));
  CAMLreturn(out);
}

/* [bytes of live resources; the device-local heaps' budget] */
CAMLprim value rv_device_memory(value device) {
  CAMLparam1(device);
  CAMLlocal1(out);
  rv_device *d = H(device);
  VmaBudget budgets[VK_MAX_MEMORY_HEAPS];
  vmaGetHeapBudgets(d->allocator, budgets);
  VkPhysicalDeviceMemoryProperties memory;
  vkGetPhysicalDeviceMemoryProperties(d->physical, &memory);
  uint64_t budget = 0;
  for (uint32_t i = 0; i < memory.memoryHeapCount; i++)
    if (memory.memoryHeaps[i].flags & VK_MEMORY_HEAP_DEVICE_LOCAL_BIT) {
      budget += budgets[i].budget;
    }
  out = caml_alloc_tuple(2);
  Store_field(out, 0, Val_long((intnat)__atomic_load_n(&d->held, __ATOMIC_RELAXED)));
  Store_field(out, 1, Val_long((intnat)budget));
  CAMLreturn(out);
}

/* ---- deferred destruction ---- */

static void defer(rv_device *d, void (*destroy)(rv_device *, void *), void *payload) {
  rv_garbage *g = malloc(sizeof *g);
  g->destroy = destroy;
  g->payload = payload;
  pthread_mutex_lock(&d->qlock);
  g->tick = d->next_tick; /* the next submit may still use it; none after, the caller promises */
  g->next = d->garbage;
  d->garbage = g;
  pthread_mutex_unlock(&d->qlock);
}

static void drain_garbage(rv_device *d) {
  uint64_t done = 0;
  vkGetSemaphoreCounterValue(d->device, d->ticks, &done);
  pthread_mutex_lock(&d->qlock);
  rv_garbage **at = &d->garbage, *ready = NULL;
  while (*at) {
    rv_garbage *g = *at;
    if (g->tick <= done) {
      *at = g->next;
      g->next = ready;
      ready = g;
      continue;
    }
    at = &g->next;
  }
  pthread_mutex_unlock(&d->qlock);
  while (ready) {
    rv_garbage *g = ready;
    ready = g->next;
    g->destroy(d, g->payload);
    free(g);
  }
}

/* ---- host-visible chunks: the set_bytes ring and the upload staging ---- */

static rv_chunk *chunk_new(rv_device *d, VkDeviceSize size) {
  pthread_mutex_lock(&d->qlock);
  rv_chunk **at = &d->free_chunks;
  while (*at && (*at)->size < size) at = &(*at)->next;
  rv_chunk *c = *at;
  if (c) *at = c->next;
  pthread_mutex_unlock(&d->qlock);
  if (c) {
    c->used = 0;
    c->next = NULL;
    return c;
  }
  c = calloc(1, sizeof *c);
  c->size = size > CHUNK_BYTES ? size : CHUNK_BYTES;
  VkBufferCreateInfo info = {.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO, .size = c->size,
      .usage = VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT |
               VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT | VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR};
  VmaAllocationCreateInfo alloc = {.flags = VMA_ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT | VMA_ALLOCATION_CREATE_MAPPED_BIT, .usage = VMA_MEMORY_USAGE_AUTO};
  VmaAllocationInfo out;
  VkResult r = vmaCreateBuffer(d->allocator, &info, &alloc, &c->buffer, &c->allocation, &out);
  if (r != VK_SUCCESS) { free(c); return NULL; }
  c->map = out.pMappedData;
  return c;
}

static void chunks_release(rv_device *d, rv_chunk *c) {
  pthread_mutex_lock(&d->qlock);
  while (c) {
    rv_chunk *next = c->next;
    c->next = d->free_chunks;
    d->free_chunks = c;
    c = next;
  }
  pthread_mutex_unlock(&d->qlock);
}

/* Room for [size] bytes in the command's chunks; NULL when out of memory. */
static char *arena(rv_cmd *c, VkDeviceSize size, VkDeviceSize align, VkBuffer *buffer, VkDeviceSize *offset) {
  rv_chunk *k = c->chunks;
  VkDeviceSize at = k ? (k->used + align - 1) / align * align : 0;
  if (!k || at + size > k->size) {
    rv_chunk *fresh = chunk_new(c->q->d, size);
    if (!fresh) return NULL;
    fresh->next = c->chunks;
    c->chunks = fresh;
    k = fresh;
    at = 0;
  }
  k->used = at + size;
  *buffer = k->buffer;
  *offset = at;
  return k->map + at;
}

/* ---- queues and command buffers ---- */

CAMLprim value rv_queue_create(value device) {
  rv_device *d = H(device);
  rv_queue *q = calloc(1, sizeof *q);
  q->d = d;
  q->next_epoch = 1;
  VkSemaphoreTypeCreateInfo timeline = {.sType = VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO, .semaphoreType = VK_SEMAPHORE_TYPE_TIMELINE};
  VkSemaphoreCreateInfo semaphore = {.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO, .pNext = &timeline};
  VkResult r = vkCreateSemaphore(d->device, &semaphore, NULL, &q->timeline);
  if (r != VK_SUCCESS) { free(q); return Val_long(r); }
  VkCommandPoolCreateInfo pool = {.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
      .flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT, .queueFamilyIndex = d->main_family};
  r = vkCreateCommandPool(d->device, &pool, NULL, &q->pool);
  if (r != VK_SUCCESS) { vkDestroySemaphore(d->device, q->timeline, NULL); free(q); return Val_long(r); }
  return Val_h(q);
}

static rv_cmd *cmd_take(rv_queue *q) {
  rv_device *d = q->d;
  rv_cmd *c = q->free_list;
  if (c) {
    q->free_list = c->next;
    c->next = NULL;
    vkResetCommandBuffer(c->cb, 0);
  } else {
    c = calloc(1, sizeof *c);
    c->q = q;
    c->all = q->commands;
    q->commands = c;
    VkCommandBufferAllocateInfo alloc = {.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO, .commandPool = q->pool,
        .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY, .commandBufferCount = 1};
    VkResult r = vkAllocateCommandBuffers(d->device, &alloc, &c->cb);
    if (r != VK_SUCCESS) { free(c); return NULL; }
    VkQueryPoolCreateInfo pool = {.sType = VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO, .queryType = VK_QUERY_TYPE_TIMESTAMP, .queryCount = 2};
    vkCreateQueryPool(d->device, &pool, NULL, &c->timestamps);
  }
  memset(c->slots, 0, sizeof c->slots);
  c->pipeline = NULL;
  c->dirty = false;
  c->dispatched = false;
  c->rendering = 0;
  c->index_buffer = VK_NULL_HANDLE;
  VkCommandBufferBeginInfo begin = {.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO, .flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT};
  vkBeginCommandBuffer(c->cb, &begin);
  vkCmdResetQueryPool(c->cb, c->timestamps, 0, 2);
  vkCmdWriteTimestamp2(c->cb, VK_PIPELINE_STAGE_2_TOP_OF_PIPE_BIT, c->timestamps, 0);
  return c;
}

static void cmd_recycle(rv_cmd *c) {
  rv_queue *q = c->q;
  chunks_release(q->d, c->chunks);
  c->chunks = NULL;
  if (c->attached_upload) {
    cmd_recycle(c->attached_upload);
    c->attached_upload = NULL;
  }
  c->next = q->free_list;
  q->free_list = c;
}

/* Everything done before or above this barrier is visible to everything below. A global barrier
   covers indirect arguments, vertices, indices, storage, transfer and acceleration structures. */
static void full_barrier(VkCommandBuffer cb) {
  VkMemoryBarrier2 barrier = {.sType = VK_STRUCTURE_TYPE_MEMORY_BARRIER_2,
      .srcStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT, .srcAccessMask = VK_ACCESS_2_MEMORY_WRITE_BIT | VK_ACCESS_2_MEMORY_READ_BIT,
      .dstStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT, .dstAccessMask = VK_ACCESS_2_MEMORY_READ_BIT | VK_ACCESS_2_MEMORY_WRITE_BIT};
  VkDependencyInfo info = {.sType = VK_STRUCTURE_TYPE_DEPENDENCY_INFO, .memoryBarrierCount = 1, .pMemoryBarriers = &barrier};
  vkCmdPipelineBarrier2(cb, &info);
}

static rv_cmd *upload_cmd(rv_device *d) {
  /* under qlock */
  if (!d->upload) {
    if (!d->upload_queue) {
      value handle = rv_queue_create(Val_h(d));
      if (Long_val(handle) < 0) return NULL;
      d->upload_queue = H(handle);
    }
    d->upload = cmd_take(d->upload_queue);
    if (d->upload) full_barrier(d->upload->cb);
  }
  return d->upload;
}

/* Submits [cb] after the pending uploads, on the main queue. qlock held. */
static VkResult submit_locked(rv_device *d, rv_queue *q, rv_cmd *c, uint64_t epoch, VkSemaphore wait, VkSemaphore signal, uint64_t *tick) {
  VkCommandBufferSubmitInfo cbs[2];
  uint32_t cb_count = 0;
  rv_cmd *upload = d->upload;
  if (upload) {
    full_barrier(upload->cb);
    vkEndCommandBuffer(upload->cb);
    cbs[cb_count++] = (VkCommandBufferSubmitInfo){.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO, .commandBuffer = upload->cb};
  }
  if (c) cbs[cb_count++] = (VkCommandBufferSubmitInfo){.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO, .commandBuffer = c->cb};
  VkSemaphoreSubmitInfo signals[3] = {
      {.sType = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO, .semaphore = d->ticks, .value = d->next_tick, .stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT}};
  uint32_t signal_count = 1;
  if (q) signals[signal_count++] = (VkSemaphoreSubmitInfo){.sType = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO, .semaphore = q->timeline, .value = epoch, .stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT};
  if (signal) signals[signal_count++] = (VkSemaphoreSubmitInfo){.sType = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO, .semaphore = signal, .stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT};
  VkSemaphoreSubmitInfo wait_info = {.sType = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO, .semaphore = wait, .stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT};
  VkSubmitInfo2 submit = {.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO_2, .waitSemaphoreInfoCount = wait ? 1 : 0, .pWaitSemaphoreInfos = &wait_info,
      .commandBufferInfoCount = cb_count, .pCommandBufferInfos = cbs, .signalSemaphoreInfoCount = signal_count, .pSignalSemaphoreInfos = signals};
  VkResult r = vkQueueSubmit2(d->main_queue, 1, &submit, VK_NULL_HANDLE);
  if (r == VK_SUCCESS) {
    *tick = d->next_tick++;
    if (upload) {
      d->upload = NULL;
      if (c) c->attached_upload = upload;
      else {
        /* no command to carry it: it is reaped with the tick it signalled */
        upload->tick = *tick;
        upload->next = d->upload_queue->flight_head;
        d->upload_queue->flight_head = upload;
      }
    }
  }
  return r;
}

static void reap_uploads(rv_device *d) {
  if (!d->upload_queue) return;
  uint64_t done = 0;
  vkGetSemaphoreCounterValue(d->device, d->ticks, &done);
  pthread_mutex_lock(&d->qlock);
  rv_cmd **at = &d->upload_queue->flight_head;
  while (*at) {
    rv_cmd *c = *at;
    if (c->tick <= done) {
      *at = c->next;
      cmd_recycle(c);
    } else at = &c->next;
  }
  pthread_mutex_unlock(&d->qlock);
}

/* The pending uploads now, and wait for them: a host read of a just-flushed copy. */
static VkResult flush_uploads_wait(rv_device *d) {
  pthread_mutex_lock(&d->qlock);
  VkResult r = VK_SUCCESS;
  uint64_t tick = 0;
  bool had = d->upload != NULL;
  if (had) r = submit_locked(d, NULL, NULL, 0, VK_NULL_HANDLE, VK_NULL_HANDLE, &tick);
  pthread_mutex_unlock(&d->qlock);
  if (r != VK_SUCCESS || !had) return r;
  VkSemaphoreWaitInfo wait = {.sType = VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO, .semaphoreCount = 1, .pSemaphores = &d->ticks, .pValues = &tick};
  caml_enter_blocking_section();
  r = vkWaitSemaphores(d->device, &wait, UINT64_MAX);
  caml_leave_blocking_section();
  return r;
}

CAMLprim value rv_commands_begin(value queue) {
  rv_queue *q = H(queue);
  drain_garbage(q->d);
  reap_uploads(q->d);
  rv_cmd *c = cmd_take(q);
  return c ? Val_h(c) : Val_long(VK_ERROR_OUT_OF_HOST_MEMORY);
}

/* The epoch this commit will signal; a negative VkResult on failure. */
static intnat commit_cmd(rv_cmd *c, VkSemaphore wait, VkSemaphore signal) {
  rv_queue *q = c->q;
  rv_device *d = q->d;
  full_barrier(c->cb);
  vkCmdWriteTimestamp2(c->cb, VK_PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT, c->timestamps, 1);
  VkResult r = vkEndCommandBuffer(c->cb);
  if (r != VK_SUCCESS) return r;
  uint64_t epoch = q->next_epoch;
  pthread_mutex_lock(&d->qlock);
  r = submit_locked(d, q, c, epoch, wait, signal, &c->tick);
  pthread_mutex_unlock(&d->qlock);
  if (r != VK_SUCCESS) return r;
  q->next_epoch = epoch + 1;
  c->epoch = epoch;
  c->next = NULL;
  if (q->flight_tail) q->flight_tail->next = c;
  else q->flight_head = c;
  q->flight_tail = c;
  return (intnat)epoch;
}

CAMLprim value rv_commands_commit(value commands) { return Val_long(commit_cmd(H(commands), VK_NULL_HANDLE, VK_NULL_HANDLE)); }

CAMLprim value rv_commands_abandon(value commands) {
  rv_cmd *c = H(commands);
  if (c->rendering) vkCmdEndRendering(c->cb);
  vkEndCommandBuffer(c->cb);
  cmd_recycle(c);
  return Val_unit;
}

/* Retires finished submissions, oldest first, recording their GPU time. */
static void reap(rv_queue *q) {
  uint64_t done = 0;
  vkGetSemaphoreCounterValue(q->d->device, q->timeline, &done);
  while (q->flight_head && q->flight_head->epoch <= done) {
    rv_cmd *c = q->flight_head;
    q->flight_head = c->next;
    if (!q->flight_head) q->flight_tail = NULL;
    uint64_t ticks[2];
    VkResult r = vkGetQueryPoolResults(q->d->device, c->timestamps, 0, 2, sizeof ticks, ticks, sizeof ticks[0], VK_QUERY_RESULT_64_BIT);
    if (r == VK_SUCCESS && ticks[1] >= ticks[0]) {
      double seconds = (double)(ticks[1] - ticks[0]) * q->d->properties.limits.timestampPeriod * 1e-9;
      q->total_seconds += seconds;
      q->duration_epochs[q->duration_at] = c->epoch;
      q->durations[q->duration_at] = seconds;
      q->duration_at = (q->duration_at + 1) % DURATIONS;
    }
    cmd_recycle(c);
  }
}

CAMLprim value rv_queue_completed(value queue) {
  rv_queue *q = H(queue);
  uint64_t done = 0;
  VkResult r = vkGetSemaphoreCounterValue(q->d->device, q->timeline, &done);
  return r == VK_SUCCESS ? Val_long((intnat)done) : Val_long(r);
}

CAMLprim value rv_queue_wait(value queue, value epoch) {
  rv_queue *q = H(queue);
  uint64_t target = (uint64_t)Long_val(epoch);
  VkSemaphoreWaitInfo wait = {.sType = VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO, .semaphoreCount = 1, .pSemaphores = &q->timeline, .pValues = &target};
  rv_device *d = q->d;
  VkDevice device = d->device;
  caml_enter_blocking_section();
  VkResult r = vkWaitSemaphores(device, &wait, UINT64_MAX);
  caml_leave_blocking_section();
  if (r == VK_SUCCESS) reap(q);
  return Val_long(r);
}

CAMLprim value rv_queue_poll(value queue) {
  reap(H(queue));
  return Val_unit;
}

CAMLprim value rv_queue_duration(value queue, value epoch) {
  rv_queue *q = H(queue);
  for (int i = 0; i < DURATIONS; i++)
    if (q->duration_epochs[i] == (uint64_t)Long_val(epoch)) return caml_copy_double(q->durations[i]);
  return caml_copy_double(-1.);
}

CAMLprim value rv_queue_total_seconds(value queue) { return caml_copy_double(((rv_queue *)H(queue))->total_seconds); }

static void queue_free(rv_queue *q) {
  VkDevice device = q->d->device;
  while (q->commands) {
    rv_cmd *c = q->commands;
    q->commands = c->all;
    chunks_release(q->d, c->chunks);
    vkDestroyQueryPool(device, c->timestamps, NULL);
    free(c);
  }
  vkDestroyCommandPool(device, q->pool, NULL);
  vkDestroySemaphore(device, q->timeline, NULL);
  free(q);
}

CAMLprim value rv_queue_destroy(value queue) {
  rv_queue *q = H(queue);
  vkQueueWaitIdle(q->d->main_queue);
  reap(q);
  queue_free(q);
  return Val_unit;
}

CAMLprim value rv_device_destroy(value device) {
  rv_device *d = H(device);
  for (rv_device **at = &exit_devices; *at; at = &(*at)->next_live)
    if (*at == d) { *at = d->next_live; break; }
  caml_enter_blocking_section();
  vkDeviceWaitIdle(d->device);
  caml_leave_blocking_section();
  if (d->upload) { vkEndCommandBuffer(d->upload->cb); cmd_recycle(d->upload); d->upload = NULL; }
  reap_uploads(d);
  d->next_tick = 0; /* idle: everything goes */
  while (d->garbage) {
    rv_garbage *g = d->garbage;
    d->garbage = g->next;
    g->destroy(d, g->payload);
    free(g);
  }
  if (d->upload_queue) queue_free(d->upload_queue);
  while (d->free_chunks) {
    rv_chunk *c = d->free_chunks;
    d->free_chunks = c->next;
    vmaDestroyBuffer(d->allocator, c->buffer, c->allocation);
    free(c);
  }
  vkDestroyFence(d->device, d->transfer_fence, NULL);
  vkDestroyCommandPool(d->device, d->transfer_pool, NULL);
  vkDestroySemaphore(d->device, d->ticks, NULL);
  vkDestroyPipelineCache(d->device, d->pipeline_cache, NULL);
  vmaDestroyAllocator(d->allocator);
  vkDestroyDevice(d->device, NULL);
  pthread_mutex_destroy(&d->qlock);
  pthread_mutex_destroy(&d->xlock);
  free(d);
  return Val_unit;
}

/* ---- buffers ---- */

/* kind: 0 host-visible (Shared), 1 device-local */
CAMLprim value rv_buffer_create(value device, value size, value kind) {
  rv_device *d = H(device);
  rv_buffer *b = calloc(1, sizeof *b);
  b->d = d;
  b->size = (VkDeviceSize)Long_val(size);
  uint32_t families[2] = {d->main_family, d->transfer_family};
  bool concurrent = d->main_family != d->transfer_family;
  VkBufferCreateInfo info = {.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO, .size = b->size,
      .usage = VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_STORAGE_BUFFER_BIT |
               VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT | VK_BUFFER_USAGE_VERTEX_BUFFER_BIT | VK_BUFFER_USAGE_INDEX_BUFFER_BIT |
               VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT |
               VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR | VK_BUFFER_USAGE_STORAGE_BUFFER_BIT,
      .sharingMode = concurrent ? VK_SHARING_MODE_CONCURRENT : VK_SHARING_MODE_EXCLUSIVE,
      .queueFamilyIndexCount = concurrent ? 2 : 0, .pQueueFamilyIndices = families};
  VmaAllocationCreateInfo alloc = {.usage = VMA_MEMORY_USAGE_AUTO};
  if (Long_val(kind) == 0)
    alloc.flags = VMA_ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT | VMA_ALLOCATION_CREATE_MAPPED_BIT;
  else
    alloc.usage = VMA_MEMORY_USAGE_AUTO_PREFER_DEVICE;
  VmaAllocationInfo out;
  VkResult r = vmaCreateBuffer(d->allocator, &info, &alloc, &b->buffer, &b->allocation, &out);
  if (r != VK_SUCCESS) { free(b); return Val_long(r); }
  __atomic_add_fetch(&d->held, (int64_t)b->size, __ATOMIC_RELAXED);
  b->map = out.pMappedData;
  if (b->map) memset(b->map, 0, b->size);
  return Val_h(b);
}

static void destroy_buffer_now(rv_device *d, void *payload) {
  rv_buffer *b = payload;
  vmaDestroyBuffer(d->allocator, b->buffer, b->allocation);
  free(b);
}

CAMLprim value rv_buffer_destroy(value buffer) {
  rv_buffer *b = H(buffer);
  __atomic_sub_fetch(&b->d->held, (int64_t)b->size, __ATOMIC_RELAXED);
  defer(b->d, destroy_buffer_now, b);
  return Val_unit;
}

/* A copy on the dedicated queue, waited for with the runtime lock released; xlock held across it. */
static VkResult transfer_copy(rv_device *d, VkBuffer src, VkDeviceSize src_offset, VkBuffer dst, VkDeviceSize dst_offset, VkDeviceSize size) {
  VkDevice device = d->device;
  caml_enter_blocking_section();
  pthread_mutex_lock(&d->xlock);
  VkCommandBufferBeginInfo begin = {.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO, .flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT};
  VkResult r = vkBeginCommandBuffer(d->transfer_cb, &begin);
  if (r == VK_SUCCESS) {
    VkBufferCopy region = {src_offset, dst_offset, size};
    vkCmdCopyBuffer(d->transfer_cb, src, dst, 1, &region);
    VkMemoryBarrier2 host = {.sType = VK_STRUCTURE_TYPE_MEMORY_BARRIER_2, .srcStageMask = VK_PIPELINE_STAGE_2_COPY_BIT,
        .srcAccessMask = VK_ACCESS_2_TRANSFER_WRITE_BIT, .dstStageMask = VK_PIPELINE_STAGE_2_HOST_BIT, .dstAccessMask = VK_ACCESS_2_HOST_READ_BIT};
    VkDependencyInfo info = {.sType = VK_STRUCTURE_TYPE_DEPENDENCY_INFO, .memoryBarrierCount = 1, .pMemoryBarriers = &host};
    vkCmdPipelineBarrier2(d->transfer_cb, &info);
    r = vkEndCommandBuffer(d->transfer_cb);
  }
  if (r == VK_SUCCESS) {
    VkCommandBufferSubmitInfo cb = {.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO, .commandBuffer = d->transfer_cb};
    VkSubmitInfo2 submit = {.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO_2, .commandBufferInfoCount = 1, .pCommandBufferInfos = &cb};
    r = vkQueueSubmit2(d->transfer_queue, 1, &submit, d->transfer_fence);
  }
  if (r == VK_SUCCESS) r = vkWaitForFences(device, 1, &d->transfer_fence, VK_TRUE, UINT64_MAX);
  if (r == VK_SUCCESS) r = vkResetFences(device, 1, &d->transfer_fence);
  vkResetCommandBuffer(d->transfer_cb, 0);
  pthread_mutex_unlock(&d->xlock);
  caml_leave_blocking_section();
  return r;
}

/* A device-to-host read: into [out]. Any domain. */
static VkResult staged_read(rv_buffer *b, VkDeviceSize offset, VkDeviceSize length, void *out) {
  if (length == 0) return VK_SUCCESS;
  rv_device *d = b->d;
  VkResult r = flush_uploads_wait(d);
  if (r != VK_SUCCESS) return r;
  if (b->map) {
    vmaInvalidateAllocation(d->allocator, b->allocation, offset, length);
    memcpy(out, b->map + offset, length);
    return VK_SUCCESS;
  }
  VkBufferCreateInfo info = {.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO, .size = length, .usage = VK_BUFFER_USAGE_TRANSFER_DST_BIT};
  VmaAllocationCreateInfo alloc = {.flags = VMA_ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT | VMA_ALLOCATION_CREATE_MAPPED_BIT, .usage = VMA_MEMORY_USAGE_AUTO, .preferredFlags = VK_MEMORY_PROPERTY_HOST_CACHED_BIT};
  VkBuffer staging;
  VmaAllocation allocation;
  VmaAllocationInfo mapped;
  r = vmaCreateBuffer(d->allocator, &info, &alloc, &staging, &allocation, &mapped);
  if (r != VK_SUCCESS) return r;
  r = transfer_copy(d, b->buffer, offset, staging, 0, length);
  if (r == VK_SUCCESS) {
    vmaInvalidateAllocation(d->allocator, allocation, 0, length);
    memcpy(out, mapped.pMappedData, length);
  }
  vmaDestroyBuffer(d->allocator, staging, allocation);
  return r;
}

static void transfer_barrier(VkCommandBuffer cb) {
  VkMemoryBarrier2 barrier = {.sType = VK_STRUCTURE_TYPE_MEMORY_BARRIER_2,
      .srcStageMask = VK_PIPELINE_STAGE_2_ALL_TRANSFER_BIT, .srcAccessMask = VK_ACCESS_2_TRANSFER_WRITE_BIT,
      .dstStageMask = VK_PIPELINE_STAGE_2_ALL_TRANSFER_BIT, .dstAccessMask = VK_ACCESS_2_TRANSFER_WRITE_BIT | VK_ACCESS_2_TRANSFER_READ_BIT};
  VkDependencyInfo info = {.sType = VK_STRUCTURE_TYPE_DEPENDENCY_INFO, .memoryBarrierCount = 1, .pMemoryBarriers = &barrier};
  vkCmdPipelineBarrier2(cb, &info);
}

/* A device-local buffer reads as zeros, as shared memory does: the next submit fills it. */
CAMLprim value rv_buffer_zero(value buffer) {
  rv_buffer *b = H(buffer);
  rv_device *d = b->d;
  pthread_mutex_lock(&d->qlock);
  rv_cmd *u = upload_cmd(d);
  if (u) {
    transfer_barrier(u->cb);
    vkCmdFillBuffer(u->cb, b->buffer, 0, VK_WHOLE_SIZE, 0);
  }
  pthread_mutex_unlock(&d->qlock);
  return Val_long(u ? VK_SUCCESS : VK_ERROR_OUT_OF_HOST_MEMORY);
}

/* The next submit copies these bytes into the buffer, before its commands. */
static VkResult staged_write(rv_buffer *b, VkDeviceSize offset, const void *from, VkDeviceSize length) {
  if (length == 0) return VK_SUCCESS;
  rv_device *d = b->d;
  VkDeviceSize chunk_offset;
  VkBuffer chunk;
  pthread_mutex_lock(&d->qlock);
  rv_cmd *u = upload_cmd(d);
  char *room = u ? arena(u, length, 16, &chunk, &chunk_offset) : NULL;
  if (room) {
    memcpy(room, from, length);
    VkBufferCopy region = {chunk_offset, offset, length};
    transfer_barrier(u->cb);
    vkCmdCopyBuffer(u->cb, chunk, b->buffer, 1, &region);
  }
  pthread_mutex_unlock(&d->qlock);
  return room ? VK_SUCCESS : VK_ERROR_OUT_OF_DEVICE_MEMORY;
}

CAMLprim value rv_buffer_write(value buffer, value offset, value bytes) {
  rv_buffer *b = H(buffer);
  VkDeviceSize length = caml_string_length(bytes);
  if (b->map) {
    /* ponytail: a host-visible write is immediate, as on shared memory; pending uploads to the same
       range would overwrite it, and a Shared buffer never has any. */
    memcpy(b->map + Long_val(offset), Bytes_val(bytes), length);
    vmaFlushAllocation(b->d->allocator, b->allocation, Long_val(offset), length);
    return Val_long(VK_SUCCESS);
  }
  return Val_long(staged_write(b, Long_val(offset), Bytes_val(bytes), length));
}

CAMLprim value rv_buffer_read(value buffer, value offset, value bytes, value at, value length) {
  CAMLparam3(buffer, offset, bytes);
  rv_buffer *b = H(buffer);
  size_t n = Long_val(length);
  void *scratch = malloc(n ? n : 1);
  VkResult r = staged_read(b, Long_val(offset), n, scratch);
  if (r == VK_SUCCESS) memcpy(Bytes_val(bytes) + Long_val(at), scratch, n);
  free(scratch);
  CAMLreturn(Val_long(r));
}

/* A mapped buffer's view is an OCaml Bigarray: valid after the buffer goes, and fixed in memory. */
CAMLprim value rv_buffer_flush(value buffer, value view, value length) {
  CAMLparam2(buffer, view);
  VkResult r = staged_write(H(buffer), 0, Caml_ba_data_val(view), Long_val(length));
  CAMLreturn(Val_long(r));
}

CAMLprim value rv_buffer_invalidate(value buffer, value view, value length) {
  CAMLparam2(buffer, view);
  VkResult r = staged_read(H(buffer), 0, Long_val(length), Caml_ba_data_val(view));
  CAMLreturn(Val_long(r));
}

/* ---- libraries and pipelines ---- */

CAMLprim value rv_shader_create(value device, value bytes) {
  rv_device *d = H(device);
  VkShaderModuleCreateInfo info = {.sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
      .codeSize = caml_string_length(bytes), .pCode = (const uint32_t *)Bytes_val(bytes)};
  VkShaderModule module;
  VkResult r = vkCreateShaderModule(d->device, &info, NULL, &module);
  return r == VK_SUCCESS ? Val_h(module) : Val_long(r);
}

CAMLprim value rv_shader_destroy(value device, value module) {
  vkDestroyShaderModule(((rv_device *)H(device))->device, H(module), NULL);
  return Val_unit;
}

static VkDescriptorType descriptor_type(int kind) {
  switch (kind) {
  case 0: return VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
  case 1: return VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
  case 2: return VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE;
  case 3: return VK_DESCRIPTOR_TYPE_STORAGE_IMAGE;
  case 4: return VK_DESCRIPTOR_TYPE_SAMPLER;
  default: return VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR;
  }
}

/* interface: an array of (binding, kind). The one set, pushed at draw and dispatch. */
static VkResult pipeline_layout(rv_pipeline *p, value interface) {
  rv_device *d = p->d;
  VkDescriptorSetLayoutBinding bindings[MAX_SLOTS];
  p->count = Wosize_val(interface);
  if (p->count > MAX_SLOTS) return VK_ERROR_TOO_MANY_OBJECTS;
  for (uint32_t i = 0; i < p->count; i++) {
    value entry = Field(interface, i);
    p->bindings[i].binding = Int_val(Field(entry, 0));
    p->bindings[i].type = descriptor_type(Int_val(Field(entry, 1)));
    bindings[i] = (VkDescriptorSetLayoutBinding){.binding = p->bindings[i].binding, .descriptorType = p->bindings[i].type,
        .descriptorCount = 1, .stageFlags = VK_SHADER_STAGE_ALL};
  }
  VkDescriptorSetLayoutCreateInfo set_info = {.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
      .flags = VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR, .bindingCount = p->count, .pBindings = bindings};
  VkResult r = vkCreateDescriptorSetLayout(d->device, &set_info, NULL, &p->set_layout);
  if (r != VK_SUCCESS) return r;
  VkPipelineLayoutCreateInfo info = {.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO, .setLayoutCount = 1, .pSetLayouts = &p->set_layout};
  r = vkCreatePipelineLayout(d->device, &info, NULL, &p->layout);
  if (r != VK_SUCCESS) vkDestroyDescriptorSetLayout(d->device, p->set_layout, NULL);
  return r;
}

CAMLprim value rv_compute_pipeline_create(value device, value module, value entry, value interface) {
  rv_device *d = H(device);
  rv_pipeline *p = calloc(1, sizeof *p);
  p->d = d;
  p->point = VK_PIPELINE_BIND_POINT_COMPUTE;
  VkResult r = pipeline_layout(p, interface);
  if (r != VK_SUCCESS) { free(p); return Val_long(r); }
  VkComputePipelineCreateInfo info = {.sType = VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO, .layout = p->layout,
      .stage = {.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO, .stage = VK_SHADER_STAGE_COMPUTE_BIT,
                .module = H(module), .pName = String_val(entry)}};
  r = vkCreateComputePipelines(d->device, d->pipeline_cache, 1, &info, NULL, &p->pipeline);
  if (r != VK_SUCCESS) {
    vkDestroyPipelineLayout(d->device, p->layout, NULL);
    vkDestroyDescriptorSetLayout(d->device, p->set_layout, NULL);
    free(p);
    return Val_long(r);
  }
  return Val_h(p);
}

static VkBlendFactor blend_table[][4] = {
    /* src color, dst color, src alpha, dst alpha: as ogpu_metal Pipeline.attachment */
    [1] = {VK_BLEND_FACTOR_SRC_ALPHA, VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA, VK_BLEND_FACTOR_ONE, VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA},
    [2] = {VK_BLEND_FACTOR_ONE, VK_BLEND_FACTOR_ONE, VK_BLEND_FACTOR_ONE, VK_BLEND_FACTOR_ONE},
    [3] = {VK_BLEND_FACTOR_DST_COLOR, VK_BLEND_FACTOR_ZERO, VK_BLEND_FACTOR_ONE, VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA},
    [4] = {VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR, VK_BLEND_FACTOR_ONE, VK_BLEND_FACTOR_ONE, VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA},
    [5] = {VK_BLEND_FACTOR_ONE, VK_BLEND_FACTOR_ONE, VK_BLEND_FACTOR_ONE, VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA},
};

static VkFormat color_format(int format) {
  switch (format) {
  case 0: return VK_FORMAT_R8_UNORM;
  case 1: return VK_FORMAT_R8G8B8A8_UNORM;
  case 2: return VK_FORMAT_B8G8R8A8_UNORM;
  case 3: return VK_FORMAT_B8G8R8A8_SRGB;
  case 4: return VK_FORMAT_R16G16B16A16_SFLOAT;
  case 5: return VK_FORMAT_R32G32B32A32_SFLOAT;
  default: return VK_FORMAT_D32_SFLOAT;
  }
}

/* options: [| blend; topology; color_format; has_depth; samples |]; topology 0 point 1 line 2 triangle */
CAMLprim value rv_render_pipeline_create(value device, value vertex, value fragment, value interface, value options) {
  rv_device *d = H(device);
  rv_pipeline *p = calloc(1, sizeof *p);
  p->d = d;
  p->point = VK_PIPELINE_BIND_POINT_GRAPHICS;
  VkResult r = pipeline_layout(p, interface);
  if (r != VK_SUCCESS) { free(p); return Val_long(r); }
  int blend = Int_val(Field(options, 0));
  int topology = Int_val(Field(options, 1));
  VkFormat color = color_format(Int_val(Field(options, 2)));
  bool has_depth = Bool_val(Field(options, 3));
  int samples = Int_val(Field(options, 4));
  VkPipelineShaderStageCreateInfo stages[2] = {
      {.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO, .stage = VK_SHADER_STAGE_VERTEX_BIT, .module = H(vertex), .pName = "main"},
      {.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO, .stage = VK_SHADER_STAGE_FRAGMENT_BIT, .module = (VkShaderModule)H(fragment), .pName = "main"}};
  VkPipelineVertexInputStateCreateInfo input = {.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO};
  VkPipelineInputAssemblyStateCreateInfo assembly = {.sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO,
      .topology = topology == 0 ? VK_PRIMITIVE_TOPOLOGY_POINT_LIST : topology == 1 ? VK_PRIMITIVE_TOPOLOGY_LINE_LIST : VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST};
  VkPipelineViewportStateCreateInfo viewport = {.sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO, .viewportCount = 1, .scissorCount = 1};
  VkPipelineRasterizationStateCreateInfo raster = {.sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO,
      .polygonMode = VK_POLYGON_MODE_FILL, .lineWidth = 1.f};
  VkPipelineMultisampleStateCreateInfo multisample = {.sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO, .rasterizationSamples = (VkSampleCountFlagBits)samples};
  VkPipelineDepthStencilStateCreateInfo depth = {.sType = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO};
  VkPipelineColorBlendAttachmentState attachment = {.colorWriteMask = 0xf};
  if (blend > 0 && blend < 6) {
    attachment.blendEnable = VK_TRUE;
    attachment.srcColorBlendFactor = blend_table[blend][0];
    attachment.dstColorBlendFactor = blend_table[blend][1];
    attachment.srcAlphaBlendFactor = blend_table[blend][2];
    attachment.dstAlphaBlendFactor = blend_table[blend][3];
    attachment.colorBlendOp = VK_BLEND_OP_ADD;
    attachment.alphaBlendOp = VK_BLEND_OP_ADD;
    if (blend == 5) attachment.colorBlendOp = VK_BLEND_OP_REVERSE_SUBTRACT;
  }
  VkPipelineColorBlendStateCreateInfo blending = {.sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO, .attachmentCount = 1, .pAttachments = &attachment};
  VkDynamicState dynamic_states[] = {VK_DYNAMIC_STATE_VIEWPORT, VK_DYNAMIC_STATE_SCISSOR, VK_DYNAMIC_STATE_CULL_MODE, VK_DYNAMIC_STATE_FRONT_FACE,
      VK_DYNAMIC_STATE_DEPTH_TEST_ENABLE, VK_DYNAMIC_STATE_DEPTH_WRITE_ENABLE, VK_DYNAMIC_STATE_DEPTH_COMPARE_OP, VK_DYNAMIC_STATE_PRIMITIVE_TOPOLOGY};
  VkPipelineDynamicStateCreateInfo dynamic = {.sType = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO, .dynamicStateCount = 8, .pDynamicStates = dynamic_states};
  VkPipelineRenderingCreateInfo rendering = {.sType = VK_STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO, .colorAttachmentCount = 1, .pColorAttachmentFormats = &color,
      .depthAttachmentFormat = has_depth ? VK_FORMAT_D32_SFLOAT : VK_FORMAT_UNDEFINED};
  VkGraphicsPipelineCreateInfo info = {.sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO, .pNext = &rendering,
      .stageCount = Long_val(fragment) ? 2 : 1, .pStages = stages, .pVertexInputState = &input, .pInputAssemblyState = &assembly, .pViewportState = &viewport,
      .pRasterizationState = &raster, .pMultisampleState = &multisample, .pDepthStencilState = &depth, .pColorBlendState = &blending,
      .pDynamicState = &dynamic, .layout = p->layout};
  r = vkCreateGraphicsPipelines(d->device, d->pipeline_cache, 1, &info, NULL, &p->pipeline);
  if (r != VK_SUCCESS) {
    vkDestroyPipelineLayout(d->device, p->layout, NULL);
    vkDestroyDescriptorSetLayout(d->device, p->set_layout, NULL);
    free(p);
    return Val_long(r);
  }
  return Val_h(p);
}

static void destroy_pipeline_now(rv_device *d, void *payload) {
  rv_pipeline *p = payload;
  vkDestroyPipeline(d->device, p->pipeline, NULL);
  vkDestroyPipelineLayout(d->device, p->layout, NULL);
  vkDestroyDescriptorSetLayout(d->device, p->set_layout, NULL);
  free(p);
}

CAMLprim value rv_pipeline_destroy(value pipeline) {
  rv_pipeline *p = H(pipeline);
  defer(p->d, destroy_pipeline_now, p);
  return Val_unit;
}

/* ---- binding: all stages share one binding space, written when a draw or dispatch needs it ---- */

CAMLprim value rv_cmd_set_pipeline(value commands, value pipeline) {
  rv_cmd *c = H(commands);
  rv_pipeline *p = H(pipeline);
  vkCmdBindPipeline(c->cb, p->point, p->pipeline);
  c->pipeline = p;
  c->dirty = true;
  return Val_unit;
}

CAMLprim value rv_cmd_set_buffer(value commands, value index, value offset, value buffer) {
  rv_cmd *c = H(commands);
  rv_buffer *b = H(buffer);
  int i = Long_val(index);
  if (i < 0 || i >= MAX_BIND) return Val_long(VK_ERROR_TOO_MANY_OBJECTS);
  c->slots[i] = (rv_slot){.kind = 1, .buffer = b->buffer, .offset = (VkDeviceSize)Long_val(offset), .range = b->size - (VkDeviceSize)Long_val(offset)};
  c->dirty = true;
  return Val_long(VK_SUCCESS);
}

CAMLprim value rv_cmd_set_bytes(value commands, value index, value bytes) {
  rv_cmd *c = H(commands);
  int i = Long_val(index);
  VkDeviceSize length = caml_string_length(bytes);
  if (i < 0 || i >= MAX_BIND) return Val_long(VK_ERROR_TOO_MANY_OBJECTS);
  VkBuffer chunk;
  VkDeviceSize at;
  char *room = arena(c, length ? length : 16, c->q->d->properties.limits.minUniformBufferOffsetAlignment, &chunk, &at);
  if (!room) return Val_long(VK_ERROR_OUT_OF_DEVICE_MEMORY);
  memcpy(room, Bytes_val(bytes), length);
  c->slots[i] = (rv_slot){.kind = 1, .buffer = chunk, .offset = at, .range = length ? length : 16};
  c->dirty = true;
  return Val_long(VK_SUCCESS);
}

CAMLprim value rv_cmd_set_texture(value commands, value index, value texture) {
  rv_cmd *c = H(commands);
  int i = Long_val(index);
  if (i < 0 || i >= MAX_BIND) return Val_long(VK_ERROR_TOO_MANY_OBJECTS);
  c->slots[i] = (rv_slot){.kind = 2, .view = ((rv_texture *)H(texture))->view};
  c->dirty = true;
  return Val_long(VK_SUCCESS);
}

CAMLprim value rv_cmd_set_sampler(value commands, value index, value sampler) {
  rv_cmd *c = H(commands);
  int i = Long_val(index);
  if (i < 0 || i >= MAX_BIND) return Val_long(VK_ERROR_TOO_MANY_OBJECTS);
  c->slots[i] = (rv_slot){.kind = 3, .sampler = H(sampler)};
  c->dirty = true;
  return Val_long(VK_SUCCESS);
}

static void push_descriptors(rv_cmd *c) {
  rv_pipeline *p = c->pipeline;
  if (!p || !c->dirty || p->count == 0) { c->dirty = false; return; }
  VkWriteDescriptorSet writes[MAX_SLOTS];
  VkDescriptorBufferInfo buffers[MAX_SLOTS];
  VkDescriptorImageInfo images[MAX_SLOTS];
  VkWriteDescriptorSetAccelerationStructureKHR accels[MAX_SLOTS];
  uint32_t n = 0;
  for (uint32_t i = 0; i < p->count; i++) {
    uint32_t binding = p->bindings[i].binding;
    VkDescriptorType type = p->bindings[i].type;
    if (binding >= MAX_BIND) continue;
    rv_slot *s = &c->slots[binding];
    if (s->kind == 0 && type == VK_DESCRIPTOR_TYPE_SAMPLER) continue; /* a sampler has no null form */
    VkWriteDescriptorSet *w = &writes[n];
    *w = (VkWriteDescriptorSet){.sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, .dstBinding = binding, .descriptorCount = 1, .descriptorType = type};
    if (s->kind == 0 && (type == VK_DESCRIPTOR_TYPE_STORAGE_BUFFER || type == VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER)) {
      buffers[n] = (VkDescriptorBufferInfo){VK_NULL_HANDLE, 0, VK_WHOLE_SIZE};
      w->pBufferInfo = &buffers[n];
    } else if (s->kind == 0 && (type == VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE || type == VK_DESCRIPTOR_TYPE_STORAGE_IMAGE)) {
      images[n] = (VkDescriptorImageInfo){.imageView = VK_NULL_HANDLE, .imageLayout = VK_IMAGE_LAYOUT_GENERAL};
      w->pImageInfo = &images[n];
    } else if (s->kind == 1) {
      VkDeviceSize limit = type == VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER ? c->q->d->properties.limits.maxUniformBufferRange : s->range;
      buffers[n] = (VkDescriptorBufferInfo){s->buffer, s->offset, s->range < limit ? s->range : limit};
      w->pBufferInfo = &buffers[n];
    } else if (s->kind == 2) {
      images[n] = (VkDescriptorImageInfo){.imageView = s->view, .imageLayout = VK_IMAGE_LAYOUT_GENERAL};
      w->pImageInfo = &images[n];
    } else if (s->kind == 3) {
      images[n] = (VkDescriptorImageInfo){.sampler = s->sampler};
      w->pImageInfo = &images[n];
    } else {
      accels[n] = (VkWriteDescriptorSetAccelerationStructureKHR){.sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR, .accelerationStructureCount = 1, .pAccelerationStructures = &s->accel};
      w->pNext = &accels[n];
    }
    n++;
  }
  if (n) vkCmdPushDescriptorSetKHR(c->cb, p->point, p->layout, 0, n, writes);
  c->dirty = false;
}

/* ---- compute ---- */

CAMLprim value rv_compute_begin(value commands) {
  rv_cmd *c = H(commands);
  c->dispatched = false;
  return Val_unit;
}

/* ponytail: one global barrier between dispatches, whatever they touch; per-buffer tracking in P11
   if a measurement says so. */
static void before_dispatch(rv_cmd *c) {
  if (c->dispatched) full_barrier(c->cb);
  c->dispatched = true;
  push_descriptors(c);
}

CAMLprim value rv_compute_dispatch(value commands, value x, value y, value z) {
  rv_cmd *c = H(commands);
  before_dispatch(c);
  vkCmdDispatch(c->cb, Long_val(x), Long_val(y), Long_val(z));
  return Val_unit;
}

CAMLprim value rv_compute_dispatch_indirect(value commands, value buffer, value offset) {
  rv_cmd *c = H(commands);
  before_dispatch(c);
  vkCmdDispatchIndirect(c->cb, ((rv_buffer *)H(buffer))->buffer, Long_val(offset));
  return Val_unit;
}

CAMLprim value rv_encoder_end(value commands) {
  full_barrier(((rv_cmd *)H(commands))->cb);
  return Val_unit;
}

/* ---- textures and samplers ---- */

/* Every image lives in VK_IMAGE_LAYOUT_GENERAL from its first submit on.
   ponytail: legal everywhere and free on this card (VK_KHR_unified_image_layouts); a GPU that
   compresses per layout pays for it, then track layouts per use here and nowhere else. */
static void image_to_general(rv_device *d, rv_texture *t) {
  pthread_mutex_lock(&d->qlock);
  rv_cmd *u = upload_cmd(d);
  if (u) {
    VkImageMemoryBarrier2 barrier = {.sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2,
        .srcStageMask = VK_PIPELINE_STAGE_2_NONE, .dstStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT,
        .dstAccessMask = VK_ACCESS_2_MEMORY_READ_BIT | VK_ACCESS_2_MEMORY_WRITE_BIT,
        .oldLayout = VK_IMAGE_LAYOUT_UNDEFINED, .newLayout = VK_IMAGE_LAYOUT_GENERAL, .image = t->image,
        .srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED, .dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
        .subresourceRange = {t->aspect, 0, VK_REMAINING_MIP_LEVELS, 0, 1}};
    VkDependencyInfo info = {.sType = VK_STRUCTURE_TYPE_DEPENDENCY_INFO, .imageMemoryBarrierCount = 1, .pImageMemoryBarriers = &barrier};
    vkCmdPipelineBarrier2(u->cb, &info);
  }
  pthread_mutex_unlock(&d->qlock);
}

/* descriptor: [| width; height; depth; mips; samples; format; storage; attachment |] */
CAMLprim value rv_texture_create(value device, value descriptor) {
  rv_device *d = H(device);
  rv_texture *t = calloc(1, sizeof *t);
  t->d = d;
  t->width = Int_val(Field(descriptor, 0));
  t->height = Int_val(Field(descriptor, 1));
  t->depth = Int_val(Field(descriptor, 2));
  t->mips = Int_val(Field(descriptor, 3));
  t->samples = Int_val(Field(descriptor, 4));
  t->format = color_format(Int_val(Field(descriptor, 5)));
  bool storage = Bool_val(Field(descriptor, 6));
  bool attachment = Bool_val(Field(descriptor, 7));
  bool is_depth = t->format == VK_FORMAT_D32_SFLOAT;
  t->aspect = is_depth ? VK_IMAGE_ASPECT_DEPTH_BIT : VK_IMAGE_ASPECT_COLOR_BIT;
  VkImageUsageFlags usage = VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT;
  if (storage && t->samples == 1) usage |= VK_IMAGE_USAGE_STORAGE_BIT;
  if (attachment) usage |= is_depth ? VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT : VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
  VkImageCreateInfo info = {.sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO, .imageType = t->depth > 1 ? VK_IMAGE_TYPE_3D : VK_IMAGE_TYPE_2D,
      .format = t->format, .extent = {t->width, t->height, t->depth}, .mipLevels = t->mips, .arrayLayers = 1,
      .samples = (VkSampleCountFlagBits)t->samples, .tiling = VK_IMAGE_TILING_OPTIMAL, .usage = usage,
      .initialLayout = VK_IMAGE_LAYOUT_UNDEFINED};
  VmaAllocationCreateInfo alloc = {.usage = VMA_MEMORY_USAGE_AUTO_PREFER_DEVICE};
  VkResult r = vmaCreateImage(d->allocator, &info, &alloc, &t->image, &t->allocation, &t->info);
  if (r != VK_SUCCESS) { free(t); return Val_long(r); }
  __atomic_add_fetch(&d->held, (int64_t)t->info.size, __ATOMIC_RELAXED);
  VkImageViewCreateInfo view = {.sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO, .image = t->image,
      .viewType = t->depth > 1 ? VK_IMAGE_VIEW_TYPE_3D : VK_IMAGE_VIEW_TYPE_2D, .format = t->format,
      .subresourceRange = {t->aspect, 0, t->mips, 0, 1}};
  r = vkCreateImageView(d->device, &view, NULL, &t->view);
  if (r != VK_SUCCESS) { vmaDestroyImage(d->allocator, t->image, t->allocation); free(t); return Val_long(r); }
  image_to_general(d, t);
  return Val_h(t);
}

static void destroy_texture_now(rv_device *d, void *payload) {
  rv_texture *t = payload;
  vkDestroyImageView(d->device, t->view, NULL);
  vmaDestroyImage(d->allocator, t->image, t->allocation);
  free(t);
}

CAMLprim value rv_texture_destroy(value texture) {
  rv_texture *t = H(texture);
  __atomic_sub_fetch(&t->d->held, (int64_t)t->info.size, __ATOMIC_RELAXED);
  defer(t->d, destroy_texture_now, t);
  return Val_unit;
}

/* Level 0 of the texture into [bytes], one row per [bytes_per_row]. Initial domain. */
CAMLprim value rv_texture_read(value texture, value bytes, value bytes_per_row, value texel) {
  CAMLparam2(texture, bytes);
  rv_texture *t = H(texture);
  rv_device *d = t->d;
  VkResult r = flush_uploads_wait(d);
  if (r != VK_SUCCESS) CAMLreturn(Val_long(r));
  VkDeviceSize row = Long_val(bytes_per_row), total = row * t->height;
  uint32_t row_texels = (uint32_t)(row / Long_val(texel));
  VkBufferCreateInfo info = {.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO, .size = total, .usage = VK_BUFFER_USAGE_TRANSFER_DST_BIT};
  VmaAllocationCreateInfo alloc = {.flags = VMA_ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT | VMA_ALLOCATION_CREATE_MAPPED_BIT, .usage = VMA_MEMORY_USAGE_AUTO, .preferredFlags = VK_MEMORY_PROPERTY_HOST_CACHED_BIT};
  VkBuffer staging;
  VmaAllocation allocation;
  VmaAllocationInfo mapped;
  r = vmaCreateBuffer(d->allocator, &info, &alloc, &staging, &allocation, &mapped);
  if (r != VK_SUCCESS) CAMLreturn(Val_long(r));
  uint64_t tick = 0;
  pthread_mutex_lock(&d->qlock);
  rv_cmd *u = upload_cmd(d);
  if (u) {
    VkBufferImageCopy region = {.bufferRowLength = row_texels, .imageSubresource = {t->aspect, 0, 0, 1}, .imageExtent = {t->width, t->height, 1}};
    full_barrier(u->cb);
    vkCmdCopyImageToBuffer(u->cb, t->image, VK_IMAGE_LAYOUT_GENERAL, staging, 1, &region);
    r = submit_locked(d, NULL, NULL, 0, VK_NULL_HANDLE, VK_NULL_HANDLE, &tick);
  } else r = VK_ERROR_OUT_OF_HOST_MEMORY;
  pthread_mutex_unlock(&d->qlock);
  if (r == VK_SUCCESS) {
    VkSemaphoreWaitInfo wait = {.sType = VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO, .semaphoreCount = 1, .pSemaphores = &d->ticks, .pValues = &tick};
    caml_enter_blocking_section();
    r = vkWaitSemaphores(d->device, &wait, UINT64_MAX);
    caml_leave_blocking_section();
  }
  if (r == VK_SUCCESS) {
    vmaInvalidateAllocation(d->allocator, allocation, 0, total);
    memcpy(Bytes_val(bytes), mapped.pMappedData, total);
  }
  vmaDestroyBuffer(d->allocator, staging, allocation);
  CAMLreturn(Val_long(r));
}

/* args: [| offset; bytes_per_row; texel; mip; x; y; z; w; h; d |] */
CAMLprim value rv_blit_buffer_to_texture(value commands, value buffer, value texture, value args) {
  rv_cmd *c = H(commands);
  rv_texture *t = H(texture);
#define A(i) Long_val(Field(args, i))
  VkBufferImageCopy region = {.bufferOffset = A(0), .bufferRowLength = (uint32_t)(A(1) / A(2)),
      .imageSubresource = {t->aspect, (uint32_t)A(3), 0, 1}, .imageOffset = {(int32_t)A(4), (int32_t)A(5), (int32_t)A(6)},
      .imageExtent = {(uint32_t)A(7), (uint32_t)A(8), (uint32_t)A(9)}};
#undef A
  full_barrier(c->cb);
  vkCmdCopyBufferToImage(c->cb, ((rv_buffer *)H(buffer))->buffer, t->image, VK_IMAGE_LAYOUT_GENERAL, 1, &region);
  return Val_unit;
}

/* args: [| min; mag; mip; u; v; anisotropy |], lods apart */
CAMLprim value rv_sampler_create(value device, value args, value lod_min, value lod_max) {
  rv_device *d = H(device);
  static const VkSamplerAddressMode modes[] = {VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE, VK_SAMPLER_ADDRESS_MODE_REPEAT, VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT};
  int mip = Int_val(Field(args, 2));
  int anisotropy = Int_val(Field(args, 5));
  VkSamplerCreateInfo info = {.sType = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO,
      .minFilter = Int_val(Field(args, 0)) ? VK_FILTER_LINEAR : VK_FILTER_NEAREST,
      .magFilter = Int_val(Field(args, 1)) ? VK_FILTER_LINEAR : VK_FILTER_NEAREST,
      .mipmapMode = mip == 2 ? VK_SAMPLER_MIPMAP_MODE_LINEAR : VK_SAMPLER_MIPMAP_MODE_NEAREST,
      .addressModeU = modes[Int_val(Field(args, 3))], .addressModeV = modes[Int_val(Field(args, 4))],
      .addressModeW = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE, .anisotropyEnable = anisotropy > 1, .maxAnisotropy = (float)anisotropy,
      .minLod = (float)Double_val(lod_min), .maxLod = mip == 0 ? (float)Double_val(lod_min) + 0.25f : (float)Double_val(lod_max)};
  VkSampler sampler;
  VkResult r = vkCreateSampler(d->device, &info, NULL, &sampler);
  return r == VK_SUCCESS ? Val_h(sampler) : Val_long(r);
}

static void destroy_sampler_now(rv_device *d, void *payload) { vkDestroySampler(d->device, payload, NULL); }

CAMLprim value rv_sampler_destroy(value device, value sampler) {
  defer(H(device), destroy_sampler_now, H(sampler));
  return Val_unit;
}

/* ---- render ---- */

static const VkAttachmentLoadOp load_ops[] = {VK_ATTACHMENT_LOAD_OP_LOAD, VK_ATTACHMENT_LOAD_OP_CLEAR, VK_ATTACHMENT_LOAD_OP_DONT_CARE};

/* Metal converts a clear to 8-bit unorm rounding half up (0.5 is 0x80); this device rounds the tie
   down. Quantize here so the GPU converts an exact multiple of 1/255, the one place a clear is made. */
static float clear_component(const rv_texture *t, double v) {
  bool unorm8 = t->format == VK_FORMAT_R8G8B8A8_UNORM || t->format == VK_FORMAT_B8G8R8A8_UNORM || t->format == VK_FORMAT_R8_UNORM;
  if (!unorm8) return (float)v;
  float c = v < 0 ? 0.f : v > 1 ? 1.f : (float)v;
  return floorf(c * 255.f + 0.5f) / 255.f;
}

/* colors: array of (texture, resolve or 0, load, store, r, g, b, a) as (int array, float array);
   depth: 0, or (texture, load, store, clear) likewise. store: 0 Store, 1 Discard, 2 Resolve */
CAMLprim value rv_render_begin(value commands, value colors, value depth, value size) {
  rv_cmd *c = H(commands);
  uint32_t count = Wosize_val(colors);
  VkRenderingAttachmentInfo attachments[8];
  if (count > 8) return Val_long(VK_ERROR_TOO_MANY_OBJECTS);
  for (uint32_t i = 0; i < count; i++) {
    value entry = Field(colors, i);
    value ints = Field(entry, 0), clear = Field(entry, 1);
    rv_texture *t = H(Field(ints, 0));
    rv_texture *resolve = H(Field(ints, 1));
    int store = Int_val(Field(ints, 3));
    attachments[i] = (VkRenderingAttachmentInfo){.sType = VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO, .imageView = t->view,
        .imageLayout = VK_IMAGE_LAYOUT_GENERAL, .loadOp = load_ops[Int_val(Field(ints, 2))],
        .storeOp = store == 0 ? VK_ATTACHMENT_STORE_OP_STORE : VK_ATTACHMENT_STORE_OP_DONT_CARE,
        .clearValue = {.color = {.float32 = {clear_component(t, Double_flat_field(clear, 0)), clear_component(t, Double_flat_field(clear, 1)), clear_component(t, Double_flat_field(clear, 2)), clear_component(t, Double_flat_field(clear, 3))}}}};
    if (resolve) {
      attachments[i].resolveMode = VK_RESOLVE_MODE_AVERAGE_BIT;
      attachments[i].resolveImageView = resolve->view;
      attachments[i].resolveImageLayout = VK_IMAGE_LAYOUT_GENERAL;
    }
  }
  VkRenderingAttachmentInfo depth_attachment;
  if (Is_block(depth)) {
    value some = Field(depth, 0);
    value ints = Field(some, 0);
    rv_texture *t = H(Field(ints, 0));
    depth_attachment = (VkRenderingAttachmentInfo){.sType = VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO, .imageView = t->view,
        .imageLayout = VK_IMAGE_LAYOUT_GENERAL, .loadOp = load_ops[Int_val(Field(ints, 1))],
        .storeOp = Int_val(Field(ints, 2)) == 0 ? VK_ATTACHMENT_STORE_OP_STORE : VK_ATTACHMENT_STORE_OP_DONT_CARE,
        .clearValue = {.depthStencil = {.depth = (float)Double_val(Field(some, 1))}}};
  }
  uint32_t width = Int_val(Field(size, 0)), height = Int_val(Field(size, 1));
  VkRenderingInfo info = {.sType = VK_STRUCTURE_TYPE_RENDERING_INFO, .renderArea = {{0, 0}, {width, height}}, .layerCount = 1,
      .colorAttachmentCount = count, .pColorAttachments = attachments, .pDepthAttachment = Is_block(depth) ? &depth_attachment : NULL};
  vkCmdBeginRendering(c->cb, &info);
  c->rendering = 1;
  c->target_height = height;
  /* Metal's encoder defaults */
  VkViewport viewport = {0, (float)height, (float)width, -(float)height, 0.f, 1.f}; /* one Y flip, here */
  VkRect2D scissor = {{0, 0}, {width, height}};
  vkCmdSetViewport(c->cb, 0, 1, &viewport);
  vkCmdSetScissor(c->cb, 0, 1, &scissor);
  vkCmdSetCullMode(c->cb, VK_CULL_MODE_NONE);
  vkCmdSetFrontFace(c->cb, VK_FRONT_FACE_CLOCKWISE);
  vkCmdSetDepthTestEnable(c->cb, VK_FALSE);
  vkCmdSetDepthWriteEnable(c->cb, VK_FALSE);
  vkCmdSetDepthCompareOp(c->cb, VK_COMPARE_OP_ALWAYS);
  vkCmdSetPrimitiveTopology(c->cb, VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
  return Val_long(VK_SUCCESS);
}

CAMLprim value rv_render_end(value commands) {
  rv_cmd *c = H(commands);
  vkCmdEndRendering(c->cb);
  c->rendering = 0;
  full_barrier(c->cb);
  return Val_unit;
}

CAMLprim value rv_render_viewport(value commands, value rect) {
  rv_cmd *c = H(commands);
  float x = Long_val(Field(rect, 0)), y = Long_val(Field(rect, 1)), w = Long_val(Field(rect, 2)), h = Long_val(Field(rect, 3));
  VkViewport viewport = {x, y + h, w, -h, 0.f, 1.f};
  vkCmdSetViewport(c->cb, 0, 1, &viewport);
  return Val_unit;
}

CAMLprim value rv_render_scissor(value commands, value rect) {
  rv_cmd *c = H(commands);
  VkRect2D scissor = {{(int32_t)Long_val(Field(rect, 0)), (int32_t)Long_val(Field(rect, 1))}, {(uint32_t)Long_val(Field(rect, 2)), (uint32_t)Long_val(Field(rect, 3))}};
  vkCmdSetScissor(c->cb, 0, 1, &scissor);
  return Val_unit;
}

CAMLprim value rv_render_cull(value commands, value mode) {
  static const VkCullModeFlags modes[] = {VK_CULL_MODE_NONE, VK_CULL_MODE_FRONT_BIT, VK_CULL_MODE_BACK_BIT};
  vkCmdSetCullMode(((rv_cmd *)H(commands))->cb, modes[Int_val(mode)]);
  return Val_unit;
}

/* Metal winding is as seen on screen, Y down; Vulkan's area in framebuffer coordinates has the same sense. */
CAMLprim value rv_render_winding(value commands, value clockwise) {
  vkCmdSetFrontFace(((rv_cmd *)H(commands))->cb, Bool_val(clockwise) ? VK_FRONT_FACE_CLOCKWISE : VK_FRONT_FACE_COUNTER_CLOCKWISE);
  return Val_unit;
}

/* compare: -1 no depth state, else the OGPU comparison in order */
CAMLprim value rv_render_depth(value commands, value compare, value write) {
  static const VkCompareOp ops[] = {VK_COMPARE_OP_NEVER, VK_COMPARE_OP_LESS, VK_COMPARE_OP_EQUAL, VK_COMPARE_OP_LESS_OR_EQUAL,
                                    VK_COMPARE_OP_GREATER, VK_COMPARE_OP_NOT_EQUAL, VK_COMPARE_OP_GREATER_OR_EQUAL, VK_COMPARE_OP_ALWAYS};
  VkCommandBuffer cb = ((rv_cmd *)H(commands))->cb;
  int op = Int_val(compare);
  vkCmdSetDepthTestEnable(cb, op >= 0);
  vkCmdSetDepthWriteEnable(cb, op >= 0 && Bool_val(write));
  if (op >= 0) vkCmdSetDepthCompareOp(cb, ops[op]);
  return Val_unit;
}

static const VkPrimitiveTopology topologies[] = {VK_PRIMITIVE_TOPOLOGY_POINT_LIST, VK_PRIMITIVE_TOPOLOGY_LINE_LIST, VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP};

CAMLprim value rv_render_draw(value commands, value primitive, value first, value count, value instances) {
  rv_cmd *c = H(commands);
  push_descriptors(c);
  vkCmdSetPrimitiveTopology(c->cb, topologies[Int_val(primitive)]);
  vkCmdDraw(c->cb, Long_val(count), Long_val(instances), Long_val(first), 0);
  return Val_unit;
}

static void bind_index(rv_cmd *c, rv_buffer *b, VkDeviceSize offset, int type) {
  vkCmdBindIndexBuffer(c->cb, b->buffer, offset, type ? VK_INDEX_TYPE_UINT32 : VK_INDEX_TYPE_UINT16);
}

/* args: [| primitive; index type (0 u16, 1 u32); offset; count; instances |] */
CAMLprim value rv_render_draw_indexed(value commands, value buffer, value args) {
  rv_cmd *c = H(commands);
  push_descriptors(c);
  vkCmdSetPrimitiveTopology(c->cb, topologies[Int_val(Field(args, 0))]);
  bind_index(c, H(buffer), Long_val(Field(args, 2)), Int_val(Field(args, 1)));
  vkCmdDrawIndexed(c->cb, Long_val(Field(args, 3)), Long_val(Field(args, 4)), 0, 0, 0);
  return Val_unit;
}

/* args: [| primitive; index type; offset; arguments offset |] */
CAMLprim value rv_render_draw_indexed_indirect(value commands, value buffer, value arguments, value args) {
  rv_cmd *c = H(commands);
  push_descriptors(c);
  vkCmdSetPrimitiveTopology(c->cb, topologies[Int_val(Field(args, 0))]);
  bind_index(c, H(buffer), Long_val(Field(args, 2)), Int_val(Field(args, 1)));
  vkCmdDrawIndexedIndirect(c->cb, ((rv_buffer *)H(arguments))->buffer, Long_val(Field(args, 3)), 1, 20);
  return Val_unit;
}

/* ---- presentation ---- */

typedef struct {
  rv_device *d;
  VkSurfaceKHR surface;
  VkSwapchainKHR swapchain;
  VkImage images[16];
  VkSemaphore rendered[16];
  VkSemaphore acquired[17];
  uint32_t image_count, acquire_at;
  VkExtent2D extent;
  VkPresentModeKHR mode;
  int held_image; /* acquired and discarded, handed out again: Vulkan cannot release an image */
  VkSemaphore held_semaphore;
  int current_image;
  VkSemaphore current_semaphore;
  bool stale;
} rv_surface;

/* A swapchain for the surface's current size. 0 extent: the window is minimised, none for now. */
static VkResult swapchain_build(rv_surface *s) {
  rv_device *d = s->d;
  VkSurfaceCapabilitiesKHR caps;
  VkResult r = vkGetPhysicalDeviceSurfaceCapabilitiesKHR(d->physical, s->surface, &caps);
  if (r != VK_SUCCESS) return r;
  caml_enter_blocking_section();
  pthread_mutex_lock(&d->qlock);
  r = vkQueueWaitIdle(d->main_queue);
  pthread_mutex_unlock(&d->qlock);
  caml_leave_blocking_section();
  if (r != VK_SUCCESS) return r;
  VkSwapchainKHR old = s->swapchain;
  s->swapchain = VK_NULL_HANDLE;
  s->extent = caps.currentExtent;
  s->held_image = -1;
  s->current_image = -1;
  s->stale = false;
  if (caps.currentExtent.width == 0 || caps.currentExtent.height == 0) {
    if (old) vkDestroySwapchainKHR(d->device, old, NULL);
    return VK_SUCCESS;
  }
  uint32_t minimum = caps.minImageCount + 1;
  if (minimum < 3) minimum = 3;
  if (caps.maxImageCount && minimum > caps.maxImageCount) minimum = caps.maxImageCount;
  VkSwapchainCreateInfoKHR info = {.sType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR, .surface = s->surface, .minImageCount = minimum,
      .imageFormat = VK_FORMAT_B8G8R8A8_UNORM, .imageColorSpace = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR, .imageExtent = caps.currentExtent,
      .imageArrayLayers = 1, .imageUsage = VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT, .imageSharingMode = VK_SHARING_MODE_EXCLUSIVE,
      .preTransform = caps.currentTransform, .compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR, .presentMode = s->mode, .clipped = VK_TRUE, .oldSwapchain = old};
  r = vkCreateSwapchainKHR(d->device, &info, NULL, &s->swapchain);
  if (old) vkDestroySwapchainKHR(d->device, old, NULL);
  if (r != VK_SUCCESS) return r;
  s->image_count = 16;
  r = vkGetSwapchainImagesKHR(d->device, s->swapchain, &s->image_count, s->images);
  if (r != VK_SUCCESS) return r;
  VkSemaphoreCreateInfo semaphore = {.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO};
  for (uint32_t i = 0; i < 16; i++)
    if (!s->rendered[i]) vkCreateSemaphore(d->device, &semaphore, NULL, &s->rendered[i]);
  /* An acquire semaphore may have been signalled by an out-of-date acquire: fresh ones. */
  for (uint32_t i = 0; i < 17; i++) {
    if (s->acquired[i]) vkDestroySemaphore(d->device, s->acquired[i], NULL);
    vkCreateSemaphore(d->device, &semaphore, NULL, &s->acquired[i]);
  }
  return VK_SUCCESS;
}

/* A borrowed window handle from the token: SDL's HWND on Windows. */
CAMLprim value rv_surface_create(value device, value token, value owner, value generation, value immediate) {
  CAMLparam5(device, token, owner, generation, immediate);
  rv_device *d = H(device);
  void *hwnd = rays_native_layer_token_borrow(token, Int64_val(owner), Int64_val(generation));
  if (!hwnd) CAMLreturn(Val_long(VK_ERROR_SURFACE_LOST_KHR));
  rv_surface *s = calloc(1, sizeof *s);
  s->d = d;
  s->mode = Bool_val(immediate) ? VK_PRESENT_MODE_IMMEDIATE_KHR : VK_PRESENT_MODE_FIFO_KHR;
  s->held_image = -1;
  s->current_image = -1;
#ifdef _WIN32
  VkWin32SurfaceCreateInfoKHR info = {.sType = VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR, .hinstance = GetModuleHandleW(NULL), .hwnd = (HWND)hwnd};
  VkResult r = vkCreateWin32SurfaceKHR(d->instance, &info, NULL, &s->surface);
#else
  VkResult r = VK_ERROR_EXTENSION_NOT_PRESENT;
#endif
  if (r != VK_SUCCESS) { free(s); CAMLreturn(Val_long(r)); }
  VkBool32 supported = VK_FALSE;
  vkGetPhysicalDeviceSurfaceSupportKHR(d->physical, d->main_family, s->surface, &supported);
  r = supported ? swapchain_build(s) : VK_ERROR_INCOMPATIBLE_DISPLAY_KHR;
  if (r != VK_SUCCESS) { vkDestroySurfaceKHR(d->instance, s->surface, NULL); free(s); CAMLreturn(Val_long(r)); }
  CAMLreturn(Val_h(s));
}

CAMLprim value rv_surface_configure(value surface, value immediate) {
  rv_surface *s = H(surface);
  s->mode = Bool_val(immediate) ? VK_PRESENT_MODE_IMMEDIATE_KHR : VK_PRESENT_MODE_FIFO_KHR;
  return Val_long(swapchain_build(s));
}

/* An image index >= 0; -1 timed out; -2 occluded (no size); a VkResult below that as itself. */
CAMLprim value rv_surface_acquire(value surface) {
  rv_surface *s = H(surface);
  if (s->held_image >= 0) {
    s->current_image = s->held_image;
    s->current_semaphore = s->held_semaphore;
    s->held_image = -1;
    return Val_long(s->current_image);
  }
  for (int attempt = 0; attempt < 2; attempt++) {
    if (!s->swapchain || s->stale) {
      VkResult built = swapchain_build(s);
      if (built != VK_SUCCESS) return Val_long(built - 10);
      if (!s->swapchain) return Val_long(-2);
    }
    VkSemaphore semaphore = s->acquired[s->acquire_at];
    s->acquire_at = (s->acquire_at + 1) % 17;
    uint32_t image = 0;
    VkDevice device = s->d->device;
    VkSwapchainKHR swapchain = s->swapchain;
    caml_enter_blocking_section();
    VkResult r = vkAcquireNextImageKHR(device, swapchain, 1000000000ull, semaphore, VK_NULL_HANDLE, &image);
    caml_leave_blocking_section();
    if (r == VK_TIMEOUT || r == VK_NOT_READY) return Val_long(-1);
    if (r == VK_ERROR_OUT_OF_DATE_KHR) { s->stale = true; continue; }
    if (r == VK_SUBOPTIMAL_KHR) s->stale = true;
    else if (r != VK_SUCCESS) return Val_long(r - 10);
    s->current_image = (int)image;
    s->current_semaphore = semaphore;
    return Val_long(image);
  }
  return Val_long(-1);
}

CAMLprim value rv_surface_discard(value surface) {
  rv_surface *s = H(surface);
  s->held_image = s->current_image;
  s->held_semaphore = s->current_semaphore;
  s->current_image = -1;
  return Val_unit;
}

/* Copies [source] to the acquired image, commits and presents. The epoch, or a negative VkResult. */
CAMLprim value rv_commit_present(value commands, value surface, value source) {
  rv_cmd *c = H(commands);
  rv_surface *s = H(surface);
  rv_texture *t = H(source);
  rv_device *d = s->d;
  uint32_t image_index = (uint32_t)s->current_image;
  VkImage image = s->images[image_index];
  VkImageSubresourceRange range = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1};
  VkImageMemoryBarrier2 barrier = {.sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2,
      .srcStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT, .dstStageMask = VK_PIPELINE_STAGE_2_BLIT_BIT, .dstAccessMask = VK_ACCESS_2_TRANSFER_WRITE_BIT,
      .oldLayout = VK_IMAGE_LAYOUT_UNDEFINED, .newLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, .image = image,
      .srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED, .dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED, .subresourceRange = range};
  VkDependencyInfo info = {.sType = VK_STRUCTURE_TYPE_DEPENDENCY_INFO, .imageMemoryBarrierCount = 1, .pImageMemoryBarriers = &barrier};
  full_barrier(c->cb);
  vkCmdPipelineBarrier2(c->cb, &info);
  VkImageBlit blit = {.srcSubresource = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1}, .srcOffsets = {{0, 0, 0}, {(int32_t)t->width, (int32_t)t->height, 1}},
      .dstSubresource = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1}, .dstOffsets = {{0, 0, 0}, {(int32_t)s->extent.width, (int32_t)s->extent.height, 1}}};
  /* ponytail: a blit replaces present.rays, which is this 1:1 RGBA8 to BGRA8 copy on Metal. */
  vkCmdBlitImage(c->cb, t->image, VK_IMAGE_LAYOUT_GENERAL, image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &blit, VK_FILTER_NEAREST);
  barrier.srcStageMask = VK_PIPELINE_STAGE_2_BLIT_BIT;
  barrier.srcAccessMask = VK_ACCESS_2_TRANSFER_WRITE_BIT;
  barrier.dstStageMask = VK_PIPELINE_STAGE_2_NONE;
  barrier.dstAccessMask = 0;
  barrier.oldLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
  barrier.newLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
  vkCmdPipelineBarrier2(c->cb, &info);
  VkSemaphore rendered = s->rendered[image_index];
  intnat epoch = commit_cmd(c, s->current_semaphore, rendered);
  if (epoch < 0) return Val_long(epoch);
  VkPresentInfoKHR present = {.sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR, .waitSemaphoreCount = 1, .pWaitSemaphores = &rendered,
      .swapchainCount = 1, .pSwapchains = &s->swapchain, .pImageIndices = &image_index};
  pthread_mutex_lock(&d->qlock);
  VkResult r = vkQueuePresentKHR(d->main_queue, &present);
  pthread_mutex_unlock(&d->qlock);
  s->current_image = -1;
  if (r == VK_ERROR_OUT_OF_DATE_KHR || r == VK_SUBOPTIMAL_KHR) s->stale = true;
  else if (r != VK_SUCCESS) return Val_long(r - 10);
  return Val_long(epoch);
}

CAMLprim value rv_surface_destroy(value surface) {
  rv_surface *s = H(surface);
  rv_device *d = s->d;
  caml_enter_blocking_section();
  pthread_mutex_lock(&d->qlock);
  vkQueueWaitIdle(d->main_queue);
  pthread_mutex_unlock(&d->qlock);
  caml_leave_blocking_section();
  for (int i = 0; i < 16; i++)
    if (s->rendered[i]) vkDestroySemaphore(d->device, s->rendered[i], NULL);
  for (int i = 0; i < 17; i++)
    if (s->acquired[i]) vkDestroySemaphore(d->device, s->acquired[i], NULL);
  if (s->swapchain) vkDestroySwapchainKHR(d->device, s->swapchain, NULL);
  vkDestroySurfaceKHR(d->instance, s->surface, NULL);
  free(s);
  return Val_unit;
}

/* ---- acceleration structures ---- */

typedef struct rv_accel {
  rv_device *d;
  VkAccelerationStructureKHR handle;
  struct rv_accel *wrap; /* a primitive structure bound to a kernel, as a one-instance top-level structure */
  VkBuffer buffer;
  VmaAllocation allocation;
  VkAccelerationStructureTypeKHR level;
  VkBuildAccelerationStructureFlagsKHR flags;
  uint32_t geometry_count;
  VkAccelerationStructureGeometryKHR *geometries;
  uint32_t *primitives;
  /* an instance structure: the logical records (Backend.instance_layout) and what they index */
  rv_buffer *instances;
  VkDeviceSize instances_offset;
  uint32_t instance_count, structure_count;
  void **structures;
  VkDeviceSize structure_size, build_scratch, update_scratch;
} rv_accel;

static VkDeviceAddress buffer_address(rv_device *d, VkBuffer buffer) {
  VkBufferDeviceAddressInfo info = {.sType = VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO, .buffer = buffer};
  return vkGetBufferDeviceAddress(d->device, &info);
}

static VkDeviceAddress accel_address(rv_accel *a) {
  VkAccelerationStructureDeviceAddressInfoKHR info = {.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR, .accelerationStructure = a->handle};
  return vkGetAccelerationStructureDeviceAddressKHR(a->d->device, &info);
}

/* geometries: arrays [| kind (0 triangles, 1 boxes); buffer; offset; stride; count; opaque; duplicate |] */
static void set_geometries(rv_accel *a, value geometries) {
  for (uint32_t i = 0; i < a->geometry_count; i++) {
    value g = Field(geometries, i);
    rv_buffer *b = H(Field(g, 1));
    VkDeviceAddress at = buffer_address(a->d, b->buffer) + (VkDeviceSize)Long_val(Field(g, 2));
    VkDeviceSize stride = Long_val(Field(g, 3));
    uint32_t count = Long_val(Field(g, 4));
    VkAccelerationStructureGeometryKHR *out = &a->geometries[i];
    *out = (VkAccelerationStructureGeometryKHR){.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR};
    if (Long_val(Field(g, 0)) == 0) {
      out->geometryType = VK_GEOMETRY_TYPE_TRIANGLES_KHR;
      out->flags = VK_GEOMETRY_OPAQUE_BIT_KHR;
      out->geometry.triangles = (VkAccelerationStructureGeometryTrianglesDataKHR){.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR,
          .vertexFormat = VK_FORMAT_R32G32B32_SFLOAT, .vertexData = {.deviceAddress = at}, .vertexStride = stride, .maxVertex = count ? count - 1 : 0, .indexType = VK_INDEX_TYPE_NONE_KHR};
      a->primitives[i] = count / 3;
    } else {
      out->geometryType = VK_GEOMETRY_TYPE_AABBS_KHR;
      out->flags = (Long_val(Field(g, 5)) ? VK_GEOMETRY_OPAQUE_BIT_KHR : 0) | (Long_val(Field(g, 6)) ? 0 : VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR);
      out->geometry.aabbs = (VkAccelerationStructureGeometryAabbsDataKHR){.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR,
          .data = {.deviceAddress = at}, .stride = stride};
      a->primitives[i] = count;
    }
  }
}

static VkResult accel_storage(rv_accel *a) {
  rv_device *d = a->d;
  VkBufferCreateInfo info = {.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO, .size = a->structure_size,
      .usage = VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT};
  VmaAllocationCreateInfo alloc = {.usage = VMA_MEMORY_USAGE_AUTO_PREFER_DEVICE};
  VkResult r = vmaCreateBuffer(d->allocator, &info, &alloc, &a->buffer, &a->allocation, NULL);
  if (r != VK_SUCCESS) return r;
  __atomic_add_fetch(&d->held, (int64_t)a->structure_size, __ATOMIC_RELAXED);
  VkAccelerationStructureCreateInfoKHR create = {.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR,
      .buffer = a->buffer, .size = a->structure_size, .type = a->level};
  r = vkCreateAccelerationStructureKHR(d->device, &create, NULL, &a->handle);
  if (r != VK_SUCCESS) vmaDestroyBuffer(d->allocator, a->buffer, a->allocation);
  return r;
}

static void build_info(rv_accel *a, VkAccelerationStructureBuildGeometryInfoKHR *info, VkAccelerationStructureGeometryKHR *instance_geometry) {
  *info = (VkAccelerationStructureBuildGeometryInfoKHR){.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR, .type = a->level, .flags = a->flags};
  if (a->level == VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR) {
    *instance_geometry = (VkAccelerationStructureGeometryKHR){.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR, .geometryType = VK_GEOMETRY_TYPE_INSTANCES_KHR,
        .geometry = {.instances = {.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR}}};
    info->geometryCount = 1;
    info->pGeometries = instance_geometry;
  } else {
    info->geometryCount = a->geometry_count;
    info->pGeometries = a->geometries;
  }
}

/* The scratch address is aligned here, so the sizes it reports carry the slack. */
static void measure(rv_accel *a) {
  VkAccelerationStructureBuildGeometryInfoKHR info;
  VkAccelerationStructureGeometryKHR instance_geometry;
  build_info(a, &info, &instance_geometry);
  uint32_t instances = a->instance_count;
  VkAccelerationStructureBuildSizesInfoKHR sizes = {.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR};
  vkGetAccelerationStructureBuildSizesKHR(a->d->device, VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR, &info,
      a->level == VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR ? &instances : a->primitives, &sizes);
  a->structure_size = sizes.accelerationStructureSize;
  a->build_scratch = sizes.buildScratchSize + 128;
  a->update_scratch = sizes.updateScratchSize + 128;
}

CAMLprim value rv_accel_create_blas(value device, value geometries, value refit) {
  rv_accel *a = calloc(1, sizeof *a);
  a->d = H(device);
  a->level = VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR;
  a->flags = Bool_val(refit) ? VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR | VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR
                             : VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR | VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR;
  a->geometry_count = Wosize_val(geometries);
  a->geometries = calloc(a->geometry_count + 1, sizeof *a->geometries);
  a->primitives = calloc(a->geometry_count + 1, sizeof *a->primitives);
  set_geometries(a, geometries);
  measure(a);
  VkResult r = accel_storage(a);
  if (r != VK_SUCCESS) { free(a->geometries); free(a->primitives); free(a); return Val_long(r); }
  return Val_h(a);
}

/* structures: the bottom-level handles a record's structure index names */
CAMLprim value rv_accel_create_tlas(value device, value instances, value offset, value count, value structures) {
  rv_accel *a = calloc(1, sizeof *a);
  a->d = H(device);
  a->level = VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR;
  a->flags = VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR;
  a->instances = H(instances);
  a->instances_offset = Long_val(offset);
  a->instance_count = Long_val(count);
  a->structure_count = Wosize_val(structures);
  a->structures = calloc(a->structure_count + 1, sizeof *a->structures);
  for (uint32_t i = 0; i < a->structure_count; i++) a->structures[i] = H(Field(structures, i));
  measure(a);
  VkResult r = accel_storage(a);
  if (r != VK_SUCCESS) { free(a->structures); free(a); return Val_long(r); }
  return Val_h(a);
}

/* An empty structure of [size] bytes shaped like [template], a compaction's destination. */
CAMLprim value rv_accel_create_sized(value device, value size, value template_) {
  rv_accel *t = H(template_);
  rv_accel *a = calloc(1, sizeof *a);
  a->d = H(device);
  a->level = t->level;
  a->structure_size = Long_val(size);
  VkResult r = accel_storage(a);
  if (r != VK_SUCCESS) { free(a); return Val_long(r); }
  return Val_h(a);
}

CAMLprim value rv_accel_sizes(value accel) {
  CAMLparam1(accel);
  CAMLlocal1(out);
  rv_accel *a = H(accel);
  out = caml_alloc_tuple(3);
  Store_field(out, 0, Val_long((intnat)a->structure_size));
  Store_field(out, 1, Val_long((intnat)a->build_scratch));
  Store_field(out, 2, Val_long((intnat)a->update_scratch));
  CAMLreturn(out);
}

static void destroy_accel_now(rv_device *d, void *payload) {
  rv_accel *a = payload;
  if (a->wrap) destroy_accel_now(d, a->wrap);
  vkDestroyAccelerationStructureKHR(d->device, a->handle, NULL);
  vmaDestroyBuffer(d->allocator, a->buffer, a->allocation);
  free(a->geometries);
  free(a->primitives);
  free(a->structures);
  free(a);
}

CAMLprim value rv_accel_destroy(value accel) {
  rv_accel *a = H(accel);
  __atomic_sub_fetch(&a->d->held, (int64_t)a->structure_size, __ATOMIC_RELAXED);
  defer(a->d, destroy_accel_now, a);
  return Val_unit;
}

/* A ray query traverses a top-level structure; Metal's intersector also takes a primitive one. A
   bottom-level structure bound to a kernel is wrapped in a one-instance top-level structure, rebuilt at
   each bind because a refit moves the bounds the wrapper holds. */
static VkResult wrap_primitive(rv_cmd *c, rv_accel *a, VkAccelerationStructureKHR *out) {
  rv_device *d = a->d;
  if (!a->wrap) {
    rv_accel *w = calloc(1, sizeof *w);
    w->d = d;
    w->level = VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR;
    w->flags = VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR;
    w->instance_count = 1;
    measure(w);
    VkResult r = accel_storage(w);
    if (r != VK_SUCCESS) { free(w); return r; }
    a->wrap = w;
  }
  rv_accel *w = a->wrap;
  VkAccelerationStructureInstanceKHR instance = {
      .transform = {.matrix = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}}}, .mask = 0xff, .accelerationStructureReference = accel_address(a)};
  VkBuffer chunk, scratch;
  VkDeviceSize at, scratch_at;
  char *room = arena(c, sizeof instance, 16, &chunk, &at);
  if (!room) return VK_ERROR_OUT_OF_DEVICE_MEMORY;
  memcpy(room, &instance, sizeof instance);
  if (!arena(c, w->build_scratch, 256, &scratch, &scratch_at)) return VK_ERROR_OUT_OF_DEVICE_MEMORY;
  VkAccelerationStructureBuildGeometryInfoKHR info;
  VkAccelerationStructureGeometryKHR geometry;
  build_info(w, &info, &geometry);
  geometry.geometry.instances.data.deviceAddress = buffer_address(d, chunk) + at;
  info.mode = VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR;
  info.dstAccelerationStructure = w->handle;
  info.scratchData.deviceAddress = (buffer_address(d, scratch) + scratch_at + 127) & ~(VkDeviceAddress)127;
  VkAccelerationStructureBuildRangeInfoKHR range = {.primitiveCount = 1};
  const VkAccelerationStructureBuildRangeInfoKHR *pointer = &range;
  full_barrier(c->cb);
  vkCmdBuildAccelerationStructuresKHR(c->cb, 1, &info, &pointer);
  full_barrier(c->cb);
  *out = w->handle;
  return VK_SUCCESS;
}

CAMLprim value rv_cmd_set_accel(value commands, value index, value accel) {
  rv_cmd *c = H(commands);
  rv_accel *a = H(accel);
  int i = Long_val(index);
  if (i < 0 || i >= MAX_BIND) return Val_long(VK_ERROR_TOO_MANY_OBJECTS);
  VkAccelerationStructureKHR handle = a->handle;
  if (a->level == VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR) {
    VkResult r = wrap_primitive(c, a, &handle);
    if (r != VK_SUCCESS) return Val_long(r);
  }
  c->slots[i] = (rv_slot){.kind = 4, .accel = handle};
  c->dirty = true;
  return Val_long(VK_SUCCESS);
}

/* The logical records (68 bytes: column-major transform, user id at 48, mask at 52, structure index
   at 56) become Vulkan's. Their bits are a host fact at build time: read back from the buffer (the
   pending uploads run and are waited for); a device-written instance buffer has no caller.
   ponytail: one wait a build; shadow the host's writes if an animated top level ever shows it. */
static VkResult native_instances(rv_cmd *c, rv_accel *a, VkDeviceAddress *address) {
  VkBuffer chunk;
  VkDeviceSize at;
  VkDeviceSize bytes = (VkDeviceSize)a->instance_count * sizeof(VkAccelerationStructureInstanceKHR);
  char *room = arena(c, bytes ? bytes : 64, 16, &chunk, &at);
  if (!room) return VK_ERROR_OUT_OF_DEVICE_MEMORY;
  VkDeviceSize records = (VkDeviceSize)a->instance_count * 68;
  char *host = malloc(records ? records : 1);
  VkResult read = host ? staged_read(a->instances, a->instances_offset, records, host) : VK_ERROR_OUT_OF_HOST_MEMORY;
  if (read != VK_SUCCESS) { free(host); return read; }
  const char *from = host;
  for (uint32_t i = 0; i < a->instance_count; i++, from += 68) {
    const float *m = (const float *)from;
    uint32_t user_id, mask, index;
    memcpy(&user_id, from + 48, 4);
    memcpy(&mask, from + 52, 4);
    memcpy(&index, from + 56, 4);
    if (index >= a->structure_count) { free(host); return VK_ERROR_VALIDATION_FAILED_EXT; }
    VkAccelerationStructureInstanceKHR out = {
        .transform = {.matrix = {{m[0], m[3], m[6], m[9]}, {m[1], m[4], m[7], m[10]}, {m[2], m[5], m[8], m[11]}}},
        .instanceCustomIndex = user_id & 0xffffff, .mask = mask & 0xff,
        .accelerationStructureReference = accel_address(a->structures[index])};
    memcpy(room + i * sizeof out, &out, sizeof out);
  }
  free(host);
  *address = buffer_address(a->d, chunk) + at;
  return VK_SUCCESS;
}

static VkResult record_build(rv_cmd *c, rv_accel *a, rv_buffer *scratch, VkDeviceSize offset, bool update) {
  VkAccelerationStructureBuildGeometryInfoKHR info;
  VkAccelerationStructureGeometryKHR instance_geometry;
  build_info(a, &info, &instance_geometry);
  VkAccelerationStructureBuildRangeInfoKHR ranges_stack[8];
  VkAccelerationStructureBuildRangeInfoKHR *ranges = info.geometryCount > 8 ? malloc(info.geometryCount * sizeof *ranges) : ranges_stack;
  memset(ranges, 0, info.geometryCount * sizeof *ranges);
  if (a->level == VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR) {
    VkDeviceAddress data = 0;
    VkResult r = native_instances(c, a, &data);
    if (r != VK_SUCCESS) return r;
    instance_geometry.geometry.instances.data.deviceAddress = data;
    ranges[0].primitiveCount = a->instance_count;
  } else {
    for (uint32_t i = 0; i < info.geometryCount; i++) ranges[i].primitiveCount = a->primitives[i];
  }
  VkDeviceAddress scratch_at = buffer_address(a->d, scratch->buffer) + offset;
  info.mode = update ? VK_BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR : VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR;
  info.srcAccelerationStructure = update ? a->handle : VK_NULL_HANDLE;
  info.dstAccelerationStructure = a->handle;
  info.scratchData.deviceAddress = (scratch_at + 127) & ~(VkDeviceAddress)127;
  const VkAccelerationStructureBuildRangeInfoKHR *pointer = ranges;
  vkCmdBuildAccelerationStructuresKHR(c->cb, 1, &info, &pointer);
  full_barrier(c->cb);
  if (ranges != ranges_stack) free(ranges);
  return VK_SUCCESS;
}

CAMLprim value rv_accel_build(value commands, value accel, value scratch, value offset) {
  return Val_long(record_build(H(commands), H(accel), H(scratch), Long_val(offset), false));
}

CAMLprim value rv_accel_refit(value commands, value accel, value geometries, value scratch, value offset) {
  rv_accel *a = H(accel);
  if (Wosize_val(geometries) != a->geometry_count) return Val_long(VK_ERROR_VALIDATION_FAILED_EXT);
  set_geometries(a, geometries);
  return Val_long(record_build(H(commands), a, H(scratch), Long_val(offset), true));
}

CAMLprim value rv_accel_compact(value commands, value source, value destination) {
  rv_cmd *c = H(commands);
  VkCopyAccelerationStructureInfoKHR info = {.sType = VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR,
      .src = ((rv_accel *)H(source))->handle, .dst = ((rv_accel *)H(destination))->handle, .mode = VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR};
  vkCmdCopyAccelerationStructureKHR(c->cb, &info);
  full_barrier(c->cb);
  return Val_unit;
}

static void destroy_query_pool_now(rv_device *d, void *payload) { vkDestroyQueryPool(d->device, payload, NULL); }

/* The compacted size as a uint32 into the buffer, as Metal writes it. */
CAMLprim value rv_accel_write_compacted(value commands, value accel, value buffer, value offset) {
  rv_cmd *c = H(commands);
  rv_accel *a = H(accel);
  rv_device *d = a->d;
  VkQueryPoolCreateInfo info = {.sType = VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO, .queryType = VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR, .queryCount = 1};
  VkQueryPool pool;
  VkResult r = vkCreateQueryPool(d->device, &info, NULL, &pool);
  if (r != VK_SUCCESS) return Val_long(r);
  vkCmdResetQueryPool(c->cb, pool, 0, 1);
  vkCmdWriteAccelerationStructuresPropertiesKHR(c->cb, 1, &a->handle, VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR, pool, 0);
  full_barrier(c->cb);
  vkCmdCopyQueryPoolResults(c->cb, pool, 0, 1, ((rv_buffer *)H(buffer))->buffer, Long_val(offset), 4, VK_QUERY_RESULT_WAIT_BIT);
  full_barrier(c->cb);
  defer(d, destroy_query_pool_now, pool);
  return Val_long(VK_SUCCESS);
}
