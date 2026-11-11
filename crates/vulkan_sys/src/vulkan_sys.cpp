#define VK_NO_PROTOTYPES
#ifdef _WIN32
#define VK_USE_PLATFORM_WIN32_KHR
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef interface
#else
#define VK_USE_PLATFORM_WAYLAND_KHR
#include <unistd.h>
#endif
#ifdef __linux__
#include <cerrno>
#include <cstdio>
#include <fcntl.h>
#include <linux/dma-buf.h>
#include <poll.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#endif
#include "vendor/volk.h"
#include "vendor/vk_mem_alloc.h"
#include <algorithm>
#include <atomic>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <mutex>
#include <new>

namespace {
constexpr int RV_NO_DEVICE = -100000;
constexpr int RV_MISSING = -100001;
constexpr uint32_t MAX_SLOTS = 64;
constexpr uint32_t MAX_PIPELINE_BINDINGS = 32;
constexpr uint32_t DURATIONS = 16;
constexpr VkDeviceSize CHUNK_BYTES = 1u << 20;

struct Device;
struct Queue;
struct Command;
struct Buffer;
struct Texture;
struct Pipeline;
struct Accel;

struct Chunk {
  Chunk *next{};
  VkBuffer buffer{};
  VmaAllocation allocation{};
  char *map{};
  VkDeviceSize size{}, used{};
};
struct Garbage {
  Garbage *next{};
  uint64_t tick{};
  void (*destroy)(Device *, void *){};
  void *payload{};
};
struct Slot {
  int kind{};
  VkBuffer buffer{};
  VkDeviceSize offset{}, range{};
  VkImageView view{};
  VkSampler sampler{};
  VkAccelerationStructureKHR accel{};
};
struct Binding { uint32_t binding{}; VkDescriptorType type{}; };
struct Pipeline {
  Device *device{};
  VkPipeline pipeline{};
  VkPipelineLayout layout{};
  VkDescriptorSetLayout set_layout{};
  VkPipelineBindPoint point{};
  uint32_t count{};
  Binding bindings[MAX_PIPELINE_BINDINGS]{};
};
struct Command {
  Command *next{}, *all{};
  Queue *queue{};
  VkCommandBuffer cb{};
  VkQueryPool timestamps{};
  uint64_t epoch{}, tick{};
  Chunk *chunks{};
  Command *attached_upload{};
  Slot slots[MAX_SLOTS]{};
  bool dirty{}, dispatched{};
  Pipeline *pipeline{};
  VkBuffer index_buffer{};
  VkDeviceSize index_offset{};
  bool rendering{};
  uint32_t target_height{};
  uint64_t external_mark{};
};
struct Queue {
  Device *device{};
  VkSemaphore timeline{};
  uint64_t next_epoch{1};
  VkCommandPool pool{};
  Command *free_list{}, *flight_head{}, *flight_tail{}, *commands{};
  double total_seconds{};
  uint64_t duration_epochs[DURATIONS]{};
  double durations[DURATIONS]{};
  uint32_t duration_at{};
};
struct Device {
  Device *next_live{};
  VkInstance instance{};
  VkPhysicalDevice physical{};
  VkDevice device{};
  VmaAllocator allocator{};
  VkQueue main_queue{}, transfer_queue{};
  uint32_t main_family{}, transfer_family{};
  VkPhysicalDeviceProperties properties{};
  bool budget{}, swapchain{}, ray_tracing{};
  VkPipelineCache pipeline_cache{};
  std::atomic<int64_t> held{};
  VkSemaphore ticks{};
  uint64_t next_tick{1};
  // Compositor hooks (Linux): enabled extensions, imported dmabuf textures, pending acquire fences.
  bool dmabuf{}, modifiers{}, foreign{}, sync_export{}, sync_import{}, dmabuf_sync{};
  Texture *externals{};
  uint64_t external_serial{};
  VkSemaphore waits[8]{};
  uint32_t wait_count{};
  Garbage *garbage{};
  Chunk *free_chunks{};
  std::recursive_mutex qlock;
  Queue *upload_queue{};
  Command *upload{};
  std::mutex xlock;
  VkCommandPool transfer_pool{};
  VkCommandBuffer transfer_cb{};
  VkFence transfer_fence{};
};
struct Buffer {
  Device *device{};
  VkBuffer buffer{};
  VmaAllocation allocation{};
  VkDeviceSize size{};
  char *map{};
};
struct Texture {
  Device *device{};
  VkImage image{};
  VmaAllocation allocation{};
  VkImageView view{};
  VkFormat format{};
  VkImageAspectFlags aspect{};
  uint32_t width{}, height{}, depth{}, mips{}, samples{};
  VmaAllocationInfo info{};
  // An imported dmabuf owns its memory (one per disjoint plane) instead of a VMA allocation.
  VkDeviceMemory memories[4]{};
  uint32_t memory_count{};
  Texture *next_external{};
  uint64_t serial{};
};
struct Surface {
  Device *device{};
  VkSurfaceKHR surface{};
  VkSwapchainKHR swapchain{};
  VkImage images[16]{};
  VkSemaphore rendered[16]{};
  VkSemaphore acquired[17]{};
  uint32_t image_count{}, acquire_at{};
  VkExtent2D extent{}, wanted{};  // wanted: the size a Wayland host asks for (the surface itself reports none)
  VkPresentModeKHR mode{};
  int held_image{-1};
  VkSemaphore held_semaphore{};
  int current_image{-1};
  VkSemaphore current_semaphore{};
  bool stale{};
};
struct Accel {
  Device *device{};
  VkAccelerationStructureKHR handle{};
  Accel *wrap{};
  VkBuffer buffer{};
  VmaAllocation allocation{};
  VkAccelerationStructureTypeKHR level{};
  VkBuildAccelerationStructureFlagsKHR flags{};
  uint32_t geometry_count{};
  VkAccelerationStructureGeometryKHR *geometries{};
  uint32_t *primitives{};
  Buffer *instances{};
  VkDeviceSize instances_offset{};
  uint32_t instance_count{}, structure_count{};
  void **structures{};
  bool build_children{};
  VkDeviceSize structure_size{}, build_scratch{}, update_scratch{}, child_scratch{};
};

std::mutex process_mutex;
VkInstance process_instance{};
VkDebugUtilsMessengerEXT process_messenger{};
std::atomic<int> validation_messages{};
std::atomic<bool> validation_fatal{}, validation_silent{};
Device *exit_devices{};

VkBool32 VKAPI_CALL on_message(VkDebugUtilsMessageSeverityFlagBitsEXT severity,
                               VkDebugUtilsMessageTypeFlagsEXT,
                               const VkDebugUtilsMessengerCallbackDataEXT *data, void *) {
  if (severity < VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT || validation_silent) return VK_FALSE;
  validation_messages.fetch_add(1, std::memory_order_relaxed);
#ifdef _WIN32
  DWORD n;
  HANDLE err = GetStdHandle(STD_ERROR_HANDLE);
  WriteFile(err, "vulkan validation: ", 19, &n, nullptr);
  WriteFile(err, data->pMessage, static_cast<DWORD>(strlen(data->pMessage)), &n, nullptr);
  WriteFile(err, "\n", 1, &n, nullptr);
  if (validation_fatal) TerminateProcess(GetCurrentProcess(), 70);
#else
  (void)!write(2, "vulkan validation: ", 19);
  (void)!write(2, data->pMessage, strlen(data->pMessage));
  (void)!write(2, "\n", 1);
  if (validation_fatal) _exit(70);
#endif
  return VK_FALSE;
}

bool has_extension(const VkExtensionProperties *list, uint32_t count, const char *name) {
  for (uint32_t i = 0; i < count; ++i) if (!strcmp(list[i].extensionName, name)) return true;
  return false;
}
int family_with(Device *d, VkQueueFlags want, VkQueueFlags refuse) {
  VkQueueFamilyProperties families[16];
  uint32_t count = 16;
  vkGetPhysicalDeviceQueueFamilyProperties(d->physical, &count, families);
  for (uint32_t i = 0; i < count; ++i)
    if ((families[i].queueFlags & want) == want && !(families[i].queueFlags & refuse)) return static_cast<int>(i);
  return -1;
}
void leave_device() {
  validation_silent = true;
  while (exit_devices) {
    Device *d = exit_devices;
    exit_devices = d->next_live;
    vkDeviceWaitIdle(d->device);
    vkDestroyDevice(d->device, nullptr);
  }
  if (process_instance) {
    if (process_messenger) vkDestroyDebugUtilsMessengerEXT(process_instance, process_messenger, nullptr);
    vkDestroyInstance(process_instance, nullptr);
    process_instance = VK_NULL_HANDLE;
  }
}
int create_device(bool validation, bool fatal, Device **out) {
  std::lock_guard<std::mutex> global(process_mutex);
  validation_fatal = fatal;
  VkResult r = volkInitialize();
  if (r != VK_SUCCESS || volkGetInstanceVersion() < VK_API_VERSION_1_3) return RV_NO_DEVICE;
  auto *d = new (std::nothrow) Device();
  if (!d) return VK_ERROR_OUT_OF_HOST_MEMORY;
  VkApplicationInfo app{VK_STRUCTURE_TYPE_APPLICATION_INFO};
  app.pApplicationName = "rays"; app.pEngineName = "rays"; app.apiVersion = VK_API_VERSION_1_3;
  const char *layers[] = {"VK_LAYER_KHRONOS_validation"};
#ifdef _WIN32
  const char *platform_surface = VK_KHR_WIN32_SURFACE_EXTENSION_NAME;
#else
  const char *platform_surface = VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME;
#endif
  // Surface extensions are optional: a headless instance (no window system) still creates devices.
  VkExtensionProperties instance_available[256]; uint32_t instance_count = 256;
  vkEnumerateInstanceExtensionProperties(nullptr, &instance_count, instance_available);
  const char *extensions[3]; uint32_t extension_count = 0;
  if (has_extension(instance_available, instance_count, VK_KHR_SURFACE_EXTENSION_NAME) && has_extension(instance_available, instance_count, platform_surface)) {
    extensions[extension_count++] = VK_KHR_SURFACE_EXTENSION_NAME; extensions[extension_count++] = platform_surface;
  }
  if (validation) extensions[extension_count++] = VK_EXT_DEBUG_UTILS_EXTENSION_NAME;
  VkValidationFeatureEnableEXT enables[] = {VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT};
  VkValidationFeaturesEXT features{VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT};
  features.enabledValidationFeatureCount = 1; features.pEnabledValidationFeatures = enables;
  VkInstanceCreateInfo ii{VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO};
  ii.pNext = validation ? &features : nullptr; ii.pApplicationInfo = &app;
  ii.enabledLayerCount = validation ? 1 : 0; ii.ppEnabledLayerNames = layers;
  ii.enabledExtensionCount = extension_count; ii.ppEnabledExtensionNames = extensions;
  if (!process_instance) {
    r = vkCreateInstance(&ii, nullptr, &process_instance);
    if (r != VK_SUCCESS) { delete d; return r; }
    volkLoadInstance(process_instance);
  }
  d->instance = process_instance;
  if (validation && !process_messenger) {
    VkDebugUtilsMessengerCreateInfoEXT mi{VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT};
    mi.messageSeverity = VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT;
    mi.messageType = VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT;
    mi.pfnUserCallback = on_message;
    vkCreateDebugUtilsMessengerEXT(d->instance, &mi, nullptr, &process_messenger);
  }
  VkPhysicalDevice physicals[8]; uint32_t physical_count = 8;
  r = vkEnumeratePhysicalDevices(d->instance, &physical_count, physicals);
  // Prefer discrete, then integrated, then virtual, then CPU (lavapipe).
  const VkPhysicalDeviceType order[] = {VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU, VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU,
      VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU, VK_PHYSICAL_DEVICE_TYPE_CPU};
  for (VkPhysicalDeviceType want : order) {
    for (uint32_t i = 0; !d->physical && r >= 0 && i < physical_count; ++i) {
      VkPhysicalDeviceProperties p; vkGetPhysicalDeviceProperties(physicals[i], &p);
      if (p.deviceType == want && p.apiVersion >= VK_API_VERSION_1_3) { d->physical = physicals[i]; d->properties = p; }
    }
  }
  int failure = d->physical ? 0 : RV_NO_DEVICE;
  VkExtensionProperties available[512]; uint32_t available_count = 512;
  if (!failure) vkEnumerateDeviceExtensionProperties(d->physical, nullptr, &available_count, available);
  const char *required[] = {VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME};
  const char *ray_extensions[] = {VK_KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME, VK_KHR_RAY_QUERY_EXTENSION_NAME, VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME};
  const char *enabled[20]; uint32_t enabled_count = 0;
  for (const char *name : required) {
    if (!failure && !has_extension(available, available_count, name)) failure = RV_MISSING;
    enabled[enabled_count++] = name;
  }
  // Optional features degrade through caps: no swapchain extension = no windows; no ray extensions = no ray tracing.
  if (!failure && has_extension(available, available_count, VK_KHR_SWAPCHAIN_EXTENSION_NAME)) {
    enabled[enabled_count++] = VK_KHR_SWAPCHAIN_EXTENSION_NAME; d->swapchain = true;
  }
  d->ray_tracing = !failure;
  for (const char *name : ray_extensions) d->ray_tracing = d->ray_tracing && has_extension(available, available_count, name);
  if (d->ray_tracing) for (const char *name : ray_extensions) enabled[enabled_count++] = name;
  if (failure || !has_extension(available, available_count, VK_EXT_EXTENDED_DYNAMIC_STATE_3_EXTENSION_NAME) ||
      !has_extension(available, available_count, VK_EXT_ROBUSTNESS_2_EXTENSION_NAME)) {
    delete d; return failure ? failure : RV_MISSING;
  }
  enabled[enabled_count++] = VK_EXT_EXTENDED_DYNAMIC_STATE_3_EXTENSION_NAME;
  enabled[enabled_count++] = VK_EXT_ROBUSTNESS_2_EXTENSION_NAME;
  if (has_extension(available, available_count, VK_EXT_MEMORY_BUDGET_EXTENSION_NAME)) {
    enabled[enabled_count++] = VK_EXT_MEMORY_BUDGET_EXTENSION_NAME; d->budget = true;
  }
#ifdef __linux__
  // Compositor hooks, as wlroots render/vulkan/vulkan.c enables them; each degrades to a false caps flag.
  auto optional = [&](const char *name) { bool found = has_extension(available, available_count, name); if (found) enabled[enabled_count++] = name; return found; };
  if (has_extension(available, available_count, VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME) && has_extension(available, available_count, VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME)) {
    d->dmabuf = optional(VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME) && optional(VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME);
    d->modifiers = optional(VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME);
    d->foreign = optional(VK_EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME);
  }
  if (optional(VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME)) {
    VkPhysicalDeviceExternalSemaphoreInfo si{VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO}; si.handleType = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT;
    VkExternalSemaphoreProperties sp{VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES}; vkGetPhysicalDeviceExternalSemaphoreProperties(d->physical, &si, &sp);
    d->sync_export = sp.externalSemaphoreFeatures & VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT;
    d->sync_import = sp.externalSemaphoreFeatures & VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT;
  }
  // wlroots render/dmabuf_linux.c: the sync_file ioctls exist from Linux 5.20 (6.0); no better probe exists.
  utsname name{};
  if (!uname(&name) && !strcmp(name.sysname, "Linux")) {
    unsigned major = 0, minor = 0; sscanf(name.release, "%u.%u", &major, &minor);
    d->dmabuf_sync = major > 5 || (major == 5 && minor >= 20);
  }
#endif
  int main_family = family_with(d, VK_QUEUE_GRAPHICS_BIT | VK_QUEUE_COMPUTE_BIT | VK_QUEUE_TRANSFER_BIT, 0);
  int transfer_family = family_with(d, VK_QUEUE_TRANSFER_BIT, VK_QUEUE_GRAPHICS_BIT | VK_QUEUE_COMPUTE_BIT |
      VK_QUEUE_VIDEO_DECODE_BIT_KHR | VK_QUEUE_VIDEO_ENCODE_BIT_KHR);
  bool shared = transfer_family < 0;
  if (shared) transfer_family = main_family;
  d->main_family = static_cast<uint32_t>(main_family); d->transfer_family = static_cast<uint32_t>(transfer_family);
  // A family with a single queue (lavapipe) cannot give the transfer path its own: both then share one VkQueue.
  VkQueueFamilyProperties families[16]; uint32_t family_count = 16;
  vkGetPhysicalDeviceQueueFamilyProperties(d->physical, &family_count, families);
  bool one_queue = shared && families[main_family].queueCount < 2;
  float priorities[2] = {1.f, 1.f};
  VkDeviceQueueCreateInfo queues[2]{};
  queues[0] = {VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO}; queues[0].queueFamilyIndex = d->main_family;
  queues[0].queueCount = shared && !one_queue ? 2 : 1; queues[0].pQueuePriorities = priorities;
  queues[1] = {VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO}; queues[1].queueFamilyIndex = d->transfer_family;
  queues[1].queueCount = 1; queues[1].pQueuePriorities = priorities;
  VkPhysicalDeviceRobustness2FeaturesEXT robustness{VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT}; robustness.nullDescriptor = VK_TRUE;
  VkPhysicalDeviceRayQueryFeaturesKHR ray{VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR}; ray.pNext = &robustness; ray.rayQuery = VK_TRUE;
  VkPhysicalDeviceAccelerationStructureFeaturesKHR accel{VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR}; accel.pNext = &ray; accel.accelerationStructure = VK_TRUE;
  VkPhysicalDeviceVulkan13Features v13{VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES}; v13.pNext = d->ray_tracing ? static_cast<void *>(&accel) : static_cast<void *>(&robustness);
  v13.dynamicRendering = VK_TRUE; v13.synchronization2 = VK_TRUE; v13.maintenance4 = VK_TRUE;
  VkPhysicalDeviceVulkan12Features v12{VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES}; v12.pNext = &v13;
  v12.timelineSemaphore = VK_TRUE; v12.bufferDeviceAddress = VK_TRUE; v12.scalarBlockLayout = VK_TRUE;
  v12.storageBuffer8BitAccess = VK_TRUE; v12.uniformAndStorageBuffer8BitAccess = VK_TRUE; v12.shaderInt8 = VK_TRUE;
  v12.shaderFloat16 = VK_TRUE; v12.hostQueryReset = VK_TRUE; v12.uniformBufferStandardLayout = VK_TRUE;
  v12.vulkanMemoryModel = VK_TRUE; v12.vulkanMemoryModelDeviceScope = VK_TRUE;
  VkPhysicalDeviceVulkan11Features v11{VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES}; v11.pNext = &v12;
  v11.storageBuffer16BitAccess = VK_TRUE; v11.uniformAndStorageBuffer16BitAccess = VK_TRUE; v11.shaderDrawParameters = VK_TRUE;
  VkPhysicalDeviceFeatures2 f2{VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2}; f2.pNext = &v11;
  f2.features.shaderFloat64 = VK_TRUE;
  f2.features.shaderInt64 = VK_TRUE;
  f2.features.shaderInt16 = VK_TRUE;
  f2.features.samplerAnisotropy = VK_TRUE;
  f2.features.fragmentStoresAndAtomics = VK_TRUE;
  f2.features.vertexPipelineStoresAndAtomics = VK_TRUE;
  f2.features.multiDrawIndirect = VK_TRUE;
  f2.features.drawIndirectFirstInstance = VK_TRUE;
  f2.features.imageCubeArray = VK_TRUE;
  f2.features.independentBlend = VK_TRUE;
  f2.features.largePoints = VK_TRUE;
  f2.features.sampleRateShading = VK_TRUE;
  f2.features.shaderStorageImageWriteWithoutFormat = VK_TRUE;
  f2.features.shaderStorageImageReadWithoutFormat = VK_TRUE;
  VkDeviceCreateInfo di{VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO}; di.pNext = &f2;
  di.queueCreateInfoCount = shared ? 1 : 2; di.pQueueCreateInfos = queues; di.enabledExtensionCount = enabled_count; di.ppEnabledExtensionNames = enabled;
  r = vkCreateDevice(d->physical, &di, nullptr, &d->device);
  if (r != VK_SUCCESS) { delete d; return r == VK_ERROR_FEATURE_NOT_PRESENT || r == VK_ERROR_EXTENSION_NOT_PRESENT ? RV_MISSING : r; }
  volkLoadDevice(d->device);
  vkGetDeviceQueue(d->device, d->main_family, 0, &d->main_queue);
  if (one_queue) d->transfer_queue = d->main_queue; else vkGetDeviceQueue(d->device, d->transfer_family, shared ? 1 : 0, &d->transfer_queue);
  VmaVulkanFunctions functions{}; functions.vkGetInstanceProcAddr = vkGetInstanceProcAddr; functions.vkGetDeviceProcAddr = vkGetDeviceProcAddr;
  VmaAllocatorCreateInfo ai{}; ai.flags = VMA_ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT | (d->budget ? VMA_ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT : 0);
  ai.physicalDevice = d->physical; ai.device = d->device; ai.pVulkanFunctions = &functions; ai.instance = d->instance; ai.vulkanApiVersion = VK_API_VERSION_1_3;
  r = vmaCreateAllocator(&ai, &d->allocator); if (r != VK_SUCCESS) { vkDestroyDevice(d->device, nullptr); delete d; return r; }
  VkPipelineCacheCreateInfo ci{VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO}; vkCreatePipelineCache(d->device, &ci, nullptr, &d->pipeline_cache);
  VkSemaphoreTypeCreateInfo ti{VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO}; ti.semaphoreType = VK_SEMAPHORE_TYPE_TIMELINE;
  VkSemaphoreCreateInfo si{VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO}; si.pNext = &ti; vkCreateSemaphore(d->device, &si, nullptr, &d->ticks);
  VkCommandPoolCreateInfo pi{VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO}; pi.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT; pi.queueFamilyIndex = d->transfer_family;
  vkCreateCommandPool(d->device, &pi, nullptr, &d->transfer_pool);
  VkCommandBufferAllocateInfo ca{VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO}; ca.commandPool = d->transfer_pool; ca.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY; ca.commandBufferCount = 1;
  vkAllocateCommandBuffers(d->device, &ca, &d->transfer_cb);
  VkFenceCreateInfo fence{VK_STRUCTURE_TYPE_FENCE_CREATE_INFO}; vkCreateFence(d->device, &fence, nullptr, &d->transfer_fence);
  if (validation) { static bool registered = false; if (!registered) { atexit(leave_device); registered = true; } d->next_live = exit_devices; exit_devices = d; }
  *out = d; return VK_SUCCESS;
}
void destroy_semaphore_now(Device *d, void *payload) { vkDestroySemaphore(d->device, reinterpret_cast<VkSemaphore>(payload), nullptr); }
void defer(Device *d, void (*destroy)(Device *, void *), void *payload) {
  auto *g = new (std::nothrow) Garbage{nullptr, 0, destroy, payload};
  if (!g) return; // ponytail: OOM leaks a possibly in-flight resource; preallocate garbage nodes if this occurs.
  std::lock_guard<std::recursive_mutex> lock(d->qlock);
  g->tick = d->next_tick;
  g->next = d->garbage; d->garbage = g;
}
void drain_garbage(Device *d) {
  uint64_t done = 0; vkGetSemaphoreCounterValue(d->device, d->ticks, &done);
  Garbage *ready = nullptr;
  {
    std::lock_guard<std::recursive_mutex> lock(d->qlock);
    Garbage **at = &d->garbage;
    while (*at) { Garbage *g = *at; if (g->tick <= done) { *at = g->next; g->next = ready; ready = g; } else at = &g->next; }
  }
  while (ready) { Garbage *g = ready; ready = g->next; g->destroy(d, g->payload); delete g; }
}
Chunk *chunk_new(Device *d, VkDeviceSize size) {
  {
    std::lock_guard<std::recursive_mutex> lock(d->qlock);
    Chunk **at = &d->free_chunks; while (*at && (*at)->size < size) at = &(*at)->next;
    if (*at) { Chunk *c = *at; *at = c->next; c->used = 0; c->next = nullptr; return c; }
  }
  auto *c = new (std::nothrow) Chunk{}; if (!c) return nullptr;
  c->size = std::max(size, CHUNK_BYTES);
  VkBufferCreateInfo bi{VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO}; bi.size = c->size;
  bi.usage = VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT |
      VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT | VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR;
  VmaAllocationCreateInfo ai{}; ai.flags = VMA_ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT | VMA_ALLOCATION_CREATE_MAPPED_BIT; ai.usage = VMA_MEMORY_USAGE_AUTO;
  VmaAllocationInfo out{}; if (vmaCreateBuffer(d->allocator, &bi, &ai, &c->buffer, &c->allocation, &out) != VK_SUCCESS) { delete c; return nullptr; }
  c->map = static_cast<char *>(out.pMappedData); return c;
}
void chunks_release(Device *d, Chunk *c) {
  std::lock_guard<std::recursive_mutex> lock(d->qlock);
  while (c) { Chunk *next = c->next; c->next = d->free_chunks; d->free_chunks = c; c = next; }
}
char *arena(Command *c, VkDeviceSize size, VkDeviceSize align, VkBuffer *buffer, VkDeviceSize *offset) {
  Chunk *k = c->chunks; VkDeviceSize at = k ? (k->used + align - 1) / align * align : 0;
  if (!k || at + size > k->size) { Chunk *fresh = chunk_new(c->queue->device, size); if (!fresh) return nullptr; fresh->next = c->chunks; c->chunks = fresh; k = fresh; at = 0; }
  k->used = at + size; *buffer = k->buffer; *offset = at; return k->map + at;
}
void full_barrier(VkCommandBuffer cb) {
  VkMemoryBarrier2 b{VK_STRUCTURE_TYPE_MEMORY_BARRIER_2}; b.srcStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;
  b.srcAccessMask = VK_ACCESS_2_MEMORY_WRITE_BIT | VK_ACCESS_2_MEMORY_READ_BIT; b.dstStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;
  b.dstAccessMask = VK_ACCESS_2_MEMORY_READ_BIT | VK_ACCESS_2_MEMORY_WRITE_BIT;
  VkDependencyInfo info{VK_STRUCTURE_TYPE_DEPENDENCY_INFO}; info.memoryBarrierCount = 1; info.pMemoryBarriers = &b; vkCmdPipelineBarrier2(cb, &info);
}
// Imported dmabufs belong to the foreign (kernel/other device) queue family between our command buffers:
// each command buffer acquires them first and releases them last, as wlroots render/vulkan/pass.c does
// per render pass. A texture imported after a command buffer began is left out of it on both ends.
VkImageMemoryBarrier2 external_barrier(Device *d, VkImage image, bool acquire, VkImageLayout old) {
  uint32_t foreign = d->foreign ? VK_QUEUE_FAMILY_FOREIGN_EXT : VK_QUEUE_FAMILY_EXTERNAL;
  VkImageMemoryBarrier2 b{VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2};
  b.srcStageMask = acquire ? VK_PIPELINE_STAGE_2_NONE : VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;
  b.srcAccessMask = acquire ? 0 : VK_ACCESS_2_MEMORY_READ_BIT | VK_ACCESS_2_MEMORY_WRITE_BIT;
  b.dstStageMask = acquire ? VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT : VK_PIPELINE_STAGE_2_NONE;
  b.dstAccessMask = acquire ? VK_ACCESS_2_MEMORY_READ_BIT | VK_ACCESS_2_MEMORY_WRITE_BIT : 0;
  b.oldLayout = old; b.newLayout = VK_IMAGE_LAYOUT_GENERAL;
  b.srcQueueFamilyIndex = acquire ? foreign : d->main_family; b.dstQueueFamilyIndex = acquire ? d->main_family : foreign;
  b.image = image; b.subresourceRange = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1}; return b;
}
void external_barriers(Command *c, bool acquire) {
  Device *d = c->queue->device; if (!d->externals) return;
  std::lock_guard<std::recursive_mutex> lock(d->qlock);
  VkImageMemoryBarrier2 batch[16]{}; uint32_t n = 0;
  auto flush = [&] { VkDependencyInfo info{VK_STRUCTURE_TYPE_DEPENDENCY_INFO}; info.imageMemoryBarrierCount = n; info.pImageMemoryBarriers = batch; if (n) vkCmdPipelineBarrier2(c->cb, &info); n = 0; };
  for (Texture *t = d->externals; t; t = t->next_external) {
    if (t->serial > c->external_mark) continue;
    batch[n++] = external_barrier(d, t->image, acquire, VK_IMAGE_LAYOUT_GENERAL);
    if (n == 16) flush();
  }
  flush();
}
Command *cmd_take(Queue *q) {
  Device *d = q->device; Command *c = q->free_list;
  if (c) { q->free_list = c->next; c->next = nullptr; vkResetCommandBuffer(c->cb, 0); }
  else {
    c = new (std::nothrow) Command{}; if (!c) return nullptr; c->queue = q; c->all = q->commands; q->commands = c;
    VkCommandBufferAllocateInfo ai{VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO}; ai.commandPool = q->pool; ai.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY; ai.commandBufferCount = 1;
    if (vkAllocateCommandBuffers(d->device, &ai, &c->cb) != VK_SUCCESS) { q->commands = c->all; delete c; return nullptr; }
    VkQueryPoolCreateInfo qi{VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO}; qi.queryType = VK_QUERY_TYPE_TIMESTAMP; qi.queryCount = 2;
    if (vkCreateQueryPool(d->device, &qi, nullptr, &c->timestamps) != VK_SUCCESS) { vkFreeCommandBuffers(d->device, q->pool, 1, &c->cb); q->commands = c->all; delete c; return nullptr; }
  }
  memset(c->slots, 0, sizeof c->slots); c->pipeline = nullptr; c->dirty = c->dispatched = false; c->rendering = false; c->index_buffer = VK_NULL_HANDLE;
  VkCommandBufferBeginInfo bi{VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO}; bi.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
  if (vkBeginCommandBuffer(c->cb, &bi) != VK_SUCCESS) return nullptr;
  vkCmdResetQueryPool(c->cb, c->timestamps, 0, 2); vkCmdWriteTimestamp2(c->cb, VK_PIPELINE_STAGE_2_TOP_OF_PIPE_BIT, c->timestamps, 0);
  { std::lock_guard<std::recursive_mutex> lock(d->qlock); c->external_mark = d->external_serial; } external_barriers(c, true); return c;
}
void cmd_recycle(Command *c) {
  Queue *q = c->queue; chunks_release(q->device, c->chunks); c->chunks = nullptr;
  if (c->attached_upload) { cmd_recycle(c->attached_upload); c->attached_upload = nullptr; }
  c->next = q->free_list; q->free_list = c;
}
Queue *queue_new(Device *d, VkResult *error) {
  auto *q = new (std::nothrow) Queue{}; if (!q) { *error = VK_ERROR_OUT_OF_HOST_MEMORY; return nullptr; } q->device = d;
  VkSemaphoreTypeCreateInfo ti{VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO}; ti.semaphoreType = VK_SEMAPHORE_TYPE_TIMELINE;
  VkSemaphoreCreateInfo si{VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO}; si.pNext = &ti; *error = vkCreateSemaphore(d->device, &si, nullptr, &q->timeline);
  if (*error != VK_SUCCESS) { delete q; return nullptr; }
  VkCommandPoolCreateInfo pi{VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO}; pi.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT; pi.queueFamilyIndex = d->main_family;
  *error = vkCreateCommandPool(d->device, &pi, nullptr, &q->pool);
  if (*error != VK_SUCCESS) { vkDestroySemaphore(d->device, q->timeline, nullptr); delete q; return nullptr; } return q;
}
Command *upload_cmd(Device *d) {
  if (!d->upload) { if (!d->upload_queue) { VkResult e{}; d->upload_queue = queue_new(d, &e); if (!d->upload_queue) return nullptr; }
    d->upload = cmd_take(d->upload_queue); if (d->upload) full_barrier(d->upload->cb); }
  return d->upload;
}
VkResult submit_locked(Device *d, Queue *q, Command *c, uint64_t epoch, VkSemaphore wait, VkSemaphore signal, uint64_t *tick) {
  VkCommandBufferSubmitInfo cbs[2]{}; uint32_t cbn = 0; Command *upload = d->upload;
  if (upload) { full_barrier(upload->cb); external_barriers(upload, false); VkResult r = vkEndCommandBuffer(upload->cb); if (r != VK_SUCCESS) return r;
    cbs[cbn] = {VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO}; cbs[cbn++].commandBuffer = upload->cb; }
  if (c) { cbs[cbn] = {VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO}; cbs[cbn++].commandBuffer = c->cb; }
  VkSemaphoreSubmitInfo sigs[3]{};
  sigs[0] = {VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO}; sigs[0].semaphore = d->ticks; sigs[0].value = d->next_tick; sigs[0].stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;
  uint32_t sn = 1;
  if (q) { sigs[sn] = {VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO}; sigs[sn].semaphore = q->timeline; sigs[sn].value = epoch; sigs[sn++].stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT; }
  if (signal) { sigs[sn] = {VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO}; sigs[sn].semaphore = signal; sigs[sn++].stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT; }
  // Acquire fences imported since the last submit gate this one (wlroots render_pass_wait_sync_file).
  VkSemaphoreSubmitInfo wi[9]{}; uint32_t wn = 0;
  if (wait) { wi[wn] = {VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO}; wi[wn].semaphore = wait; wi[wn++].stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT; }
  for (uint32_t i = 0; i < d->wait_count; ++i) { wi[wn] = {VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO}; wi[wn].semaphore = d->waits[i]; wi[wn++].stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT; }
  VkSubmitInfo2 si{VK_STRUCTURE_TYPE_SUBMIT_INFO_2}; si.waitSemaphoreInfoCount = wn; si.pWaitSemaphoreInfos = wi;
  si.commandBufferInfoCount = cbn; si.pCommandBufferInfos = cbs; si.signalSemaphoreInfoCount = sn; si.pSignalSemaphoreInfos = sigs;
  VkResult r = vkQueueSubmit2(d->main_queue, 1, &si, VK_NULL_HANDLE);
  if (r == VK_SUCCESS) { *tick = d->next_tick++; for (uint32_t i = 0; i < d->wait_count; ++i) defer(d, destroy_semaphore_now, reinterpret_cast<void *>(d->waits[i])); d->wait_count = 0; if (upload) { d->upload = nullptr; if (c) c->attached_upload = upload; else { upload->tick = *tick; upload->next = d->upload_queue->flight_head; d->upload_queue->flight_head = upload; } } }
  return r;
}
void queue_free(Queue *q) {
  VkDevice dev = q->device->device;
  while (q->commands) { Command *c = q->commands; q->commands = c->all; chunks_release(q->device, c->chunks); vkDestroyQueryPool(dev, c->timestamps, nullptr); delete c; }
  vkDestroyCommandPool(dev, q->pool, nullptr); vkDestroySemaphore(dev, q->timeline, nullptr); delete q;
}
void reap_uploads(Device *d) {
  if (!d->upload_queue) return; uint64_t done = 0; vkGetSemaphoreCounterValue(d->device, d->ticks, &done);
  std::lock_guard<std::recursive_mutex> lock(d->qlock); Command **at = &d->upload_queue->flight_head;
  while (*at) { Command *c = *at; if (c->tick <= done) { *at = c->next; cmd_recycle(c); } else at = &c->next; }
}
VkResult flush_uploads_wait(Device *d) {
  uint64_t tick = 0; bool had; VkResult r = VK_SUCCESS;
  { std::lock_guard<std::recursive_mutex> lock(d->qlock); had = d->upload != nullptr; if (had) r = submit_locked(d, nullptr, nullptr, 0, VK_NULL_HANDLE, VK_NULL_HANDLE, &tick); }
  if (r != VK_SUCCESS || !had) return r;
  VkSemaphoreWaitInfo wi{VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO}; wi.semaphoreCount = 1; wi.pSemaphores = &d->ticks; wi.pValues = &tick; return vkWaitSemaphores(d->device, &wi, UINT64_MAX);
}
void reap(Queue *q) {
  uint64_t done = 0; vkGetSemaphoreCounterValue(q->device->device, q->timeline, &done);
  while (q->flight_head && q->flight_head->epoch <= done) {
    Command *c = q->flight_head; q->flight_head = c->next; if (!q->flight_head) q->flight_tail = nullptr;
    uint64_t ts[2]{}; VkResult r = vkGetQueryPoolResults(q->device->device, c->timestamps, 0, 2, sizeof ts, ts, sizeof ts[0], VK_QUERY_RESULT_64_BIT);
    if (r == VK_SUCCESS && ts[1] >= ts[0]) { double sec = (ts[1] - ts[0]) * q->device->properties.limits.timestampPeriod * 1e-9;
      q->total_seconds += sec; q->duration_epochs[q->duration_at] = c->epoch; q->durations[q->duration_at] = sec; q->duration_at = (q->duration_at + 1) % DURATIONS; }
    cmd_recycle(c);
  }
}
int64_t commit_cmd(Command *c, VkSemaphore wait, VkSemaphore signal) {
  Queue *q = c->queue; Device *d = q->device; full_barrier(c->cb); vkCmdWriteTimestamp2(c->cb, VK_PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT, c->timestamps, 1);
  external_barriers(c, false); VkResult r = vkEndCommandBuffer(c->cb); if (r != VK_SUCCESS) return r; uint64_t epoch = q->next_epoch;
  { std::lock_guard<std::recursive_mutex> lock(d->qlock); r = submit_locked(d, q, c, epoch, wait, signal, &c->tick); }
  if (r != VK_SUCCESS) return r; q->next_epoch = epoch + 1; c->epoch = epoch; c->next = nullptr;
  if (q->flight_tail) q->flight_tail->next = c; else q->flight_head = c; q->flight_tail = c; return static_cast<int64_t>(epoch);
}
VkResult transfer_copy(Device *d, VkBuffer src, VkDeviceSize so, VkBuffer dst, VkDeviceSize doff, VkDeviceSize size) {
  // Sharing the main queue: take its lock first (same order as everyone else), held across the wait.
  std::unique_lock<std::recursive_mutex> shared_queue(d->qlock, std::defer_lock); if (d->transfer_queue == d->main_queue) shared_queue.lock();
  std::lock_guard<std::mutex> lock(d->xlock); VkCommandBufferBeginInfo bi{VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO}; bi.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
  VkResult r = vkBeginCommandBuffer(d->transfer_cb, &bi);
  if (r == VK_SUCCESS) { VkBufferCopy region{so, doff, size}; vkCmdCopyBuffer(d->transfer_cb, src, dst, 1, &region);
    VkMemoryBarrier2 b{VK_STRUCTURE_TYPE_MEMORY_BARRIER_2}; b.srcStageMask = VK_PIPELINE_STAGE_2_COPY_BIT; b.srcAccessMask = VK_ACCESS_2_TRANSFER_WRITE_BIT;
    b.dstStageMask = VK_PIPELINE_STAGE_2_HOST_BIT; b.dstAccessMask = VK_ACCESS_2_HOST_READ_BIT;
    VkDependencyInfo info{VK_STRUCTURE_TYPE_DEPENDENCY_INFO}; info.memoryBarrierCount = 1; info.pMemoryBarriers = &b; vkCmdPipelineBarrier2(d->transfer_cb, &info); r = vkEndCommandBuffer(d->transfer_cb); }
  if (r == VK_SUCCESS) { VkCommandBufferSubmitInfo cb{VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO}; cb.commandBuffer = d->transfer_cb;
    VkSubmitInfo2 si{VK_STRUCTURE_TYPE_SUBMIT_INFO_2}; si.commandBufferInfoCount = 1; si.pCommandBufferInfos = &cb; r = vkQueueSubmit2(d->transfer_queue, 1, &si, d->transfer_fence); }
  if (r == VK_SUCCESS) r = vkWaitForFences(d->device, 1, &d->transfer_fence, VK_TRUE, UINT64_MAX);
  if (r == VK_SUCCESS) r = vkResetFences(d->device, 1, &d->transfer_fence); vkResetCommandBuffer(d->transfer_cb, 0); return r;
}
VkResult staged_read(Buffer *b, VkDeviceSize offset, VkDeviceSize length, void *out) {
  if (!length) return VK_SUCCESS; Device *d = b->device; VkResult r = flush_uploads_wait(d); if (r != VK_SUCCESS) return r;
  if (b->map) { vmaInvalidateAllocation(d->allocator, b->allocation, offset, length); memcpy(out, b->map + offset, length); return VK_SUCCESS; }
  VkBufferCreateInfo bi{VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO}; bi.size = length; bi.usage = VK_BUFFER_USAGE_TRANSFER_DST_BIT;
  VmaAllocationCreateInfo ai{}; ai.flags = VMA_ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT | VMA_ALLOCATION_CREATE_MAPPED_BIT; ai.usage = VMA_MEMORY_USAGE_AUTO; ai.preferredFlags = VK_MEMORY_PROPERTY_HOST_CACHED_BIT;
  VkBuffer staging{}; VmaAllocation allocation{}; VmaAllocationInfo mapped{}; r = vmaCreateBuffer(d->allocator, &bi, &ai, &staging, &allocation, &mapped); if (r != VK_SUCCESS) return r;
  r = transfer_copy(d, b->buffer, offset, staging, 0, length);
  if (r == VK_SUCCESS) { vmaInvalidateAllocation(d->allocator, allocation, 0, length); memcpy(out, mapped.pMappedData, length); }
  vmaDestroyBuffer(d->allocator, staging, allocation); return r;
}
void transfer_barrier(VkCommandBuffer cb) {
  VkMemoryBarrier2 b{VK_STRUCTURE_TYPE_MEMORY_BARRIER_2}; b.srcStageMask = VK_PIPELINE_STAGE_2_ALL_TRANSFER_BIT; b.srcAccessMask = VK_ACCESS_2_TRANSFER_WRITE_BIT;
  b.dstStageMask = VK_PIPELINE_STAGE_2_ALL_TRANSFER_BIT; b.dstAccessMask = VK_ACCESS_2_TRANSFER_WRITE_BIT | VK_ACCESS_2_TRANSFER_READ_BIT;
  VkDependencyInfo info{VK_STRUCTURE_TYPE_DEPENDENCY_INFO}; info.memoryBarrierCount = 1; info.pMemoryBarriers = &b; vkCmdPipelineBarrier2(cb, &info);
}
VkResult staged_write(Buffer *b, VkDeviceSize offset, const void *src, VkDeviceSize length) {
  if (!length) return VK_SUCCESS; Device *d = b->device; std::lock_guard<std::recursive_mutex> lock(d->qlock);
  Command *u = upload_cmd(d); VkBuffer chunk{}; VkDeviceSize at{}; char *room = u ? arena(u, length, 16, &chunk, &at) : nullptr;
  if (!room) return VK_ERROR_OUT_OF_DEVICE_MEMORY; memcpy(room, src, length); VkBufferCopy r{at, offset, length}; transfer_barrier(u->cb); vkCmdCopyBuffer(u->cb, chunk, b->buffer, 1, &r); return VK_SUCCESS;
}
}

extern "C" {
struct rv_facts { uint64_t max_buffer, max_texture_1d, max_texture_2d, max_texture_3d, max_texture_cube, max_array_layers, device_local_bytes; uint32_t sample_counts, color_sample_counts, depth_sample_counts, max_invocations, api_version, ray_tracing; };
int32_t rv_device_create(bool validation, bool fatal, void **out) { return create_device(validation, fatal, reinterpret_cast<Device **>(out)); }
int32_t rv_validation_messages() { return validation_messages.load(std::memory_order_relaxed); }
int32_t rv_device_facts(void *handle, rv_facts *out) {
  auto *d = static_cast<Device *>(handle); auto *l = &d->properties.limits; VkPhysicalDeviceMemoryProperties m{}; vkGetPhysicalDeviceMemoryProperties(d->physical, &m);
  uint64_t local = 0; for (uint32_t i = 0; i < m.memoryHeapCount; ++i) if ((m.memoryHeaps[i].flags & VK_MEMORY_HEAP_DEVICE_LOCAL_BIT) && m.memoryHeaps[i].size > local) local = m.memoryHeaps[i].size;
  VkSampleCountFlags s = l->framebufferColorSampleCounts & l->framebufferDepthSampleCounts;
  *out = {l->maxStorageBufferRange, l->maxImageDimension1D, l->maxImageDimension2D, l->maxImageDimension3D, l->maxImageDimensionCube, l->maxImageArrayLayers, local, static_cast<uint32_t>(s), static_cast<uint32_t>(l->framebufferColorSampleCounts), static_cast<uint32_t>(l->framebufferDepthSampleCounts), l->maxComputeWorkGroupInvocations, d->properties.apiVersion, d->ray_tracing}; return VK_SUCCESS;
}
int32_t rv_device_memory(void *handle, uint64_t *held, uint64_t *budget) {
  auto *d = static_cast<Device *>(handle); VmaBudget b[VK_MAX_MEMORY_HEAPS]{}; vmaGetHeapBudgets(d->allocator, b); VkPhysicalDeviceMemoryProperties m{}; vkGetPhysicalDeviceMemoryProperties(d->physical, &m);
  *budget = 0; for (uint32_t i = 0; i < m.memoryHeapCount; ++i) if (m.memoryHeaps[i].flags & VK_MEMORY_HEAP_DEVICE_LOCAL_BIT) *budget += b[i].budget;
  *held = static_cast<uint64_t>(d->held.load(std::memory_order_relaxed)); return VK_SUCCESS;
}
void rv_device_destroy(void *handle) {
  auto *d = static_cast<Device *>(handle);
  { std::lock_guard<std::mutex> lock(process_mutex); for (Device **at = &exit_devices; *at; at = &(*at)->next_live) if (*at == d) { *at = d->next_live; break; } }
  vkDeviceWaitIdle(d->device);
  if (d->upload) { vkEndCommandBuffer(d->upload->cb); cmd_recycle(d->upload); d->upload = nullptr; }
  reap_uploads(d);
  while (d->garbage) { Garbage *g = d->garbage; d->garbage = g->next; g->destroy(d, g->payload); delete g; }
  for (uint32_t i = 0; i < d->wait_count; ++i) vkDestroySemaphore(d->device, d->waits[i], nullptr);
  if (d->upload_queue) queue_free(d->upload_queue);
  while (d->free_chunks) { Chunk *c = d->free_chunks; d->free_chunks = c->next; vmaDestroyBuffer(d->allocator, c->buffer, c->allocation); delete c; }
  vkDestroyFence(d->device, d->transfer_fence, nullptr); vkDestroyCommandPool(d->device, d->transfer_pool, nullptr);
  vkDestroySemaphore(d->device, d->ticks, nullptr); vkDestroyPipelineCache(d->device, d->pipeline_cache, nullptr);
  vmaDestroyAllocator(d->allocator); vkDestroyDevice(d->device, nullptr); delete d;
}
int32_t rv_queue_create(void *device, void **out) { VkResult e{}; Queue *q = queue_new(static_cast<Device *>(device), &e); if (!q) return e; *out = q; return VK_SUCCESS; }
int32_t rv_queue_completed(void *queue, uint64_t *done) { auto *q = static_cast<Queue *>(queue); return vkGetSemaphoreCounterValue(q->device->device, q->timeline, done); }
int32_t rv_queue_wait(void *queue, uint64_t epoch) {
  auto *q = static_cast<Queue *>(queue); VkSemaphoreWaitInfo wi{VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO};
  wi.semaphoreCount = 1; wi.pSemaphores = &q->timeline; wi.pValues = &epoch;
  VkResult r = vkWaitSemaphores(q->device->device, &wi, UINT64_MAX); if (r == VK_SUCCESS) reap(q); return r;
}
void rv_queue_poll(void *queue) { reap(static_cast<Queue *>(queue)); }
double rv_queue_duration(void *queue, uint64_t epoch) {
  auto *q = static_cast<Queue *>(queue); for (uint32_t i = 0; i < DURATIONS; ++i) if (q->duration_epochs[i] == epoch) return q->durations[i]; return -1.;
}
double rv_queue_total_seconds(void *queue) { return static_cast<Queue *>(queue)->total_seconds; }
void rv_queue_destroy(void *queue) { auto *q = static_cast<Queue *>(queue); vkQueueWaitIdle(q->device->main_queue); reap(q); queue_free(q); }
int32_t rv_commands_begin(void *queue, void **out) {
  auto *q = static_cast<Queue *>(queue); drain_garbage(q->device); reap_uploads(q->device);
  Command *c = cmd_take(q); if (!c) return VK_ERROR_OUT_OF_HOST_MEMORY; *out = c; return VK_SUCCESS;
}
int32_t rv_commands_commit(void *commands, uint64_t *epoch) {
  int64_t result = commit_cmd(static_cast<Command *>(commands), VK_NULL_HANDLE, VK_NULL_HANDLE);
  if (result < 0) return static_cast<int32_t>(result); *epoch = static_cast<uint64_t>(result); return VK_SUCCESS;
}
void rv_commands_abandon(void *commands) {
  auto *c = static_cast<Command *>(commands); if (c->rendering) vkCmdEndRendering(c->cb); vkEndCommandBuffer(c->cb); cmd_recycle(c);
}
int32_t rv_buffer_create(void *device, uint64_t size, int32_t kind, void **out) {
  auto *d = static_cast<Device *>(device); auto *b = new (std::nothrow) Buffer{}; if (!b) return VK_ERROR_OUT_OF_HOST_MEMORY;
  b->device = d; b->size = size; uint32_t families[2] = {d->main_family, d->transfer_family}; bool concurrent = d->main_family != d->transfer_family;
  VkBufferCreateInfo bi{VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO}; bi.size = size;
  bi.usage = VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_STORAGE_BUFFER_BIT |
      VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT | VK_BUFFER_USAGE_VERTEX_BUFFER_BIT | VK_BUFFER_USAGE_INDEX_BUFFER_BIT |
      VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT | VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR;
  bi.sharingMode = concurrent ? VK_SHARING_MODE_CONCURRENT : VK_SHARING_MODE_EXCLUSIVE; bi.queueFamilyIndexCount = concurrent ? 2 : 0; bi.pQueueFamilyIndices = families;
  VmaAllocationCreateInfo ai{}; ai.usage = VMA_MEMORY_USAGE_AUTO;
  if (kind == 0) ai.flags = VMA_ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT | VMA_ALLOCATION_CREATE_MAPPED_BIT; else ai.usage = VMA_MEMORY_USAGE_AUTO_PREFER_DEVICE;
  VmaAllocationInfo out_info{}; VkResult r = vmaCreateBuffer(d->allocator, &bi, &ai, &b->buffer, &b->allocation, &out_info);
  if (r != VK_SUCCESS) { delete b; return r; }
  d->held.fetch_add(static_cast<int64_t>(size), std::memory_order_relaxed); b->map = static_cast<char *>(out_info.pMappedData);
  if (b->map) memset(b->map, 0, size); *out = b; return VK_SUCCESS;
}
void destroy_buffer_now(Device *d, void *payload) { auto *b = static_cast<Buffer *>(payload); vmaDestroyBuffer(d->allocator, b->buffer, b->allocation); delete b; }
void rv_buffer_destroy(void *buffer) { auto *b = static_cast<Buffer *>(buffer); b->device->held.fetch_sub(static_cast<int64_t>(b->size), std::memory_order_relaxed); defer(b->device, destroy_buffer_now, b); }
int32_t rv_buffer_zero(void *buffer) {
  auto *b = static_cast<Buffer *>(buffer); Device *d = b->device; std::lock_guard<std::recursive_mutex> lock(d->qlock);
  Command *u = upload_cmd(d); if (!u) return VK_ERROR_OUT_OF_HOST_MEMORY; transfer_barrier(u->cb); vkCmdFillBuffer(u->cb, b->buffer, 0, VK_WHOLE_SIZE, 0); return VK_SUCCESS;
}
int32_t rv_buffer_write(void *buffer, uint64_t offset, const void *data, uint64_t length) {
  auto *b = static_cast<Buffer *>(buffer); if (offset > b->size || length > b->size - offset) return VK_ERROR_VALIDATION_FAILED_EXT;
  if (b->map) { if (length) { memcpy(b->map + offset, data, length); vmaFlushAllocation(b->device->allocator, b->allocation, offset, length); } return VK_SUCCESS; }
  return staged_write(b, offset, data, length);
}
int32_t rv_buffer_read(void *buffer, uint64_t offset, void *data, uint64_t length) {
  auto *b = static_cast<Buffer *>(buffer); if (offset > b->size || length > b->size - offset) return VK_ERROR_VALIDATION_FAILED_EXT;
  return staged_read(b, offset, length, data);
}
// Persistent host address of a kind-0 (host-visible, VMA-mapped) buffer; null for a device-local one.
void *rv_buffer_map(void *buffer) { return static_cast<Buffer *>(buffer)->map; }
// Make host writes in [offset, offset+length) visible to the device; a no-op on coherent memory.
int32_t rv_buffer_flush_range(void *buffer, uint64_t offset, uint64_t length) {
  auto *b = static_cast<Buffer *>(buffer); if (!b->map || offset > b->size || length > b->size - offset) return VK_ERROR_VALIDATION_FAILED_EXT;
  return length ? vmaFlushAllocation(b->device->allocator, b->allocation, offset, length) : VK_SUCCESS;
}
int32_t rv_buffer_flush(void *buffer, const void *data, uint64_t length) { return staged_write(static_cast<Buffer *>(buffer), 0, data, length); }
int32_t rv_buffer_invalidate(void *buffer, void *data, uint64_t length) { return staged_read(static_cast<Buffer *>(buffer), 0, length, data); }
struct rv_binding { uint32_t binding; int32_t kind; };
struct rv_pipeline_desc { int32_t blend, topology, color_format; uint8_t has_depth; int32_t samples; };
struct rv_texture_desc { uint32_t width, height, depth, mips, samples; int32_t format; uint8_t storage, attachment, volume; };
static_assert(sizeof(rv_texture_desc) == 28);
struct rv_blit_desc { uint64_t offset; uint32_t bytes_per_row, texel, mip; int32_t x, y, z; uint32_t width, height, depth; };
struct rv_sampler_desc { int32_t min, mag, mip, u, v, anisotropy; double lod_min, lod_max; };

static VkDescriptorType rv_descriptor_type(int kind) {
  switch (kind) { case 0: return VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER; case 1: return VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
    case 2: return VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE; case 3: return VK_DESCRIPTOR_TYPE_STORAGE_IMAGE;
    case 4: return VK_DESCRIPTOR_TYPE_SAMPLER; default: return VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR; }
}
static VkFormat rv_color_format(int format) {
  switch (format) { case 0: return VK_FORMAT_R8_UNORM; case 1: return VK_FORMAT_R8G8B8A8_UNORM;
    case 2: return VK_FORMAT_B8G8R8A8_UNORM; case 3: return VK_FORMAT_B8G8R8A8_SRGB;
    case 4: return VK_FORMAT_R16G16B16A16_SFLOAT; case 5: return VK_FORMAT_R32G32B32A32_SFLOAT;
    default: return VK_FORMAT_D32_SFLOAT; }
}
static VkResult rv_pipeline_layout(Pipeline *p, const rv_binding *interface, uint32_t count) {
  if (count > MAX_PIPELINE_BINDINGS) return VK_ERROR_TOO_MANY_OBJECTS;
  VkDescriptorSetLayoutBinding bindings[MAX_PIPELINE_BINDINGS]{};
  p->count = count;
  for (uint32_t i = 0; i < count; ++i) {
    p->bindings[i] = {interface[i].binding, rv_descriptor_type(interface[i].kind)};
    bindings[i] = {interface[i].binding, p->bindings[i].type, 1, VK_SHADER_STAGE_ALL, nullptr};
  }
  VkDescriptorSetLayoutCreateInfo si{VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO};
  si.flags = VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR; si.bindingCount = count; si.pBindings = bindings;
  VkResult r = vkCreateDescriptorSetLayout(p->device->device, &si, nullptr, &p->set_layout); if (r != VK_SUCCESS) return r;
  VkPipelineLayoutCreateInfo li{VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO}; li.setLayoutCount = 1; li.pSetLayouts = &p->set_layout;
  r = vkCreatePipelineLayout(p->device->device, &li, nullptr, &p->layout);
  if (r != VK_SUCCESS) vkDestroyDescriptorSetLayout(p->device->device, p->set_layout, nullptr); return r;
}
static void destroy_pipeline_now(Device *d, void *payload) {
  auto *p = static_cast<Pipeline *>(payload); vkDestroyPipeline(d->device, p->pipeline, nullptr);
  vkDestroyPipelineLayout(d->device, p->layout, nullptr); vkDestroyDescriptorSetLayout(d->device, p->set_layout, nullptr); delete p;
}
static void push_descriptors(Command *c) {
  Pipeline *p = c->pipeline; if (!p || !c->dirty || !p->count) { c->dirty = false; return; }
  VkWriteDescriptorSet writes[MAX_PIPELINE_BINDINGS]{}; VkDescriptorBufferInfo buffers[MAX_PIPELINE_BINDINGS]{};
  VkDescriptorImageInfo images[MAX_PIPELINE_BINDINGS]{}; VkWriteDescriptorSetAccelerationStructureKHR accels[MAX_PIPELINE_BINDINGS]{};
  uint32_t n = 0;
  for (uint32_t i = 0; i < p->count; ++i) {
    uint32_t binding = p->bindings[i].binding; VkDescriptorType type = p->bindings[i].type;
    if (binding >= MAX_SLOTS) continue; Slot *s = &c->slots[binding];
    if (!s->kind && type == VK_DESCRIPTOR_TYPE_SAMPLER) continue;
    VkWriteDescriptorSet *w = &writes[n]; w->sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET; w->dstBinding = binding; w->descriptorCount = 1; w->descriptorType = type;
    if (!s->kind && (type == VK_DESCRIPTOR_TYPE_STORAGE_BUFFER || type == VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER)) {
      buffers[n] = {VK_NULL_HANDLE, 0, VK_WHOLE_SIZE}; w->pBufferInfo = &buffers[n];
    } else if (!s->kind && (type == VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE || type == VK_DESCRIPTOR_TYPE_STORAGE_IMAGE)) {
      images[n].imageLayout = VK_IMAGE_LAYOUT_GENERAL; w->pImageInfo = &images[n];
    } else if (s->kind == 1) {
      VkDeviceSize lim = type == VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER ? c->queue->device->properties.limits.maxUniformBufferRange : s->range;
      buffers[n] = {s->buffer, s->offset, std::min(s->range, lim)}; w->pBufferInfo = &buffers[n];
    } else if (s->kind == 2) {
      images[n].imageView = s->view; images[n].imageLayout = VK_IMAGE_LAYOUT_GENERAL; w->pImageInfo = &images[n];
    } else if (s->kind == 3) {
      images[n].sampler = s->sampler; w->pImageInfo = &images[n];
    } else {
      accels[n].sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR; accels[n].accelerationStructureCount = 1;
      accels[n].pAccelerationStructures = &s->accel; w->pNext = &accels[n];
    }
    ++n;
  }
  if (n) vkCmdPushDescriptorSetKHR(c->cb, p->point, p->layout, 0, n, writes); c->dirty = false;
}
static void before_dispatch(Command *c) { if (c->dispatched) full_barrier(c->cb); c->dispatched = true; push_descriptors(c); }
static VkResult image_to_general(Device *d, Texture *t) {
  std::lock_guard<std::recursive_mutex> lock(d->qlock); Command *u = upload_cmd(d); if (!u) return VK_ERROR_OUT_OF_HOST_MEMORY;
  VkImageMemoryBarrier2 b{VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2}; b.dstStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;
  b.dstAccessMask = VK_ACCESS_2_MEMORY_READ_BIT | VK_ACCESS_2_MEMORY_WRITE_BIT; b.oldLayout = VK_IMAGE_LAYOUT_UNDEFINED;
  b.newLayout = VK_IMAGE_LAYOUT_GENERAL; b.image = t->image; b.srcQueueFamilyIndex = b.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
  b.subresourceRange = {t->aspect, 0, VK_REMAINING_MIP_LEVELS, 0, 1};
  VkDependencyInfo info{VK_STRUCTURE_TYPE_DEPENDENCY_INFO}; info.imageMemoryBarrierCount = 1; info.pImageMemoryBarriers = &b; vkCmdPipelineBarrier2(u->cb, &info);
  return VK_SUCCESS;
}
int32_t rv_shader_create(void *device, const void *bytes, uint64_t length, void **out) {
  auto *d = static_cast<Device *>(device);
  const uint32_t *code = static_cast<const uint32_t *>(bytes);
  void *aligned = nullptr;
  if (reinterpret_cast<uintptr_t>(bytes) % alignof(uint32_t)) {
    if (length > SIZE_MAX) return VK_ERROR_OUT_OF_HOST_MEMORY;
    aligned = malloc(static_cast<size_t>(length));
    if (!aligned) return VK_ERROR_OUT_OF_HOST_MEMORY;
    memcpy(aligned, bytes, static_cast<size_t>(length));
    code = static_cast<const uint32_t *>(aligned);
  }
  VkShaderModuleCreateInfo si{VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO}; si.codeSize = length; si.pCode = code;
  VkShaderModule module{}; VkResult r = vkCreateShaderModule(d->device, &si, nullptr, &module); free(aligned);
  if (r == VK_SUCCESS) *out = reinterpret_cast<void *>(module); return r;
}
void rv_shader_destroy(void *device, void *module) { vkDestroyShaderModule(static_cast<Device *>(device)->device, reinterpret_cast<VkShaderModule>(module), nullptr); }
int32_t rv_compute_pipeline_create(void *device, void *module, const char *entry, const rv_binding *bindings, uint32_t count, void **out) {
  auto *d = static_cast<Device *>(device); auto *p = new (std::nothrow) Pipeline{}; if (!p) return VK_ERROR_OUT_OF_HOST_MEMORY; p->device = d; p->point = VK_PIPELINE_BIND_POINT_COMPUTE;
  VkResult r = rv_pipeline_layout(p, bindings, count); if (r != VK_SUCCESS) { delete p; return r; }
  VkComputePipelineCreateInfo ci{VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO}; ci.layout = p->layout;
  ci.stage = {VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO, nullptr, 0, VK_SHADER_STAGE_COMPUTE_BIT, reinterpret_cast<VkShaderModule>(module), entry, nullptr};
  r = vkCreateComputePipelines(d->device, d->pipeline_cache, 1, &ci, nullptr, &p->pipeline);
  if (r != VK_SUCCESS) { vkDestroyPipelineLayout(d->device, p->layout, nullptr); vkDestroyDescriptorSetLayout(d->device, p->set_layout, nullptr); delete p; return r; }
  *out = p; return VK_SUCCESS;
}
int32_t rv_render_pipeline_create(void *device, void *vertex, void *fragment, const rv_binding *bindings, uint32_t count, const rv_pipeline_desc *options, void **out) {
  auto *d = static_cast<Device *>(device); auto *p = new (std::nothrow) Pipeline{}; if (!p) return VK_ERROR_OUT_OF_HOST_MEMORY; p->device = d; p->point = VK_PIPELINE_BIND_POINT_GRAPHICS;
  VkResult r = rv_pipeline_layout(p, bindings, count); if (r != VK_SUCCESS) { delete p; return r; }
  static const VkBlendFactor blend[][4] = {{},{VK_BLEND_FACTOR_SRC_ALPHA,VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,VK_BLEND_FACTOR_ONE,VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA},
    {VK_BLEND_FACTOR_ONE,VK_BLEND_FACTOR_ONE,VK_BLEND_FACTOR_ONE,VK_BLEND_FACTOR_ONE},
    {VK_BLEND_FACTOR_DST_COLOR,VK_BLEND_FACTOR_ZERO,VK_BLEND_FACTOR_ONE,VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA},
    {VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR,VK_BLEND_FACTOR_ONE,VK_BLEND_FACTOR_ONE,VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA},
    {VK_BLEND_FACTOR_ONE,VK_BLEND_FACTOR_ONE,VK_BLEND_FACTOR_ONE,VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA},
    {VK_BLEND_FACTOR_ONE,VK_BLEND_FACTOR_ONE,VK_BLEND_FACTOR_ONE,VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA}};
  VkPipelineShaderStageCreateInfo stages[2]{{VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO},{VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO}};
  stages[0].stage = VK_SHADER_STAGE_VERTEX_BIT; stages[0].module = reinterpret_cast<VkShaderModule>(vertex); stages[0].pName = "main";
  stages[1].stage = VK_SHADER_STAGE_FRAGMENT_BIT; stages[1].module = reinterpret_cast<VkShaderModule>(fragment); stages[1].pName = "main";
  VkPipelineVertexInputStateCreateInfo input{VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO};
  VkPrimitiveTopology topo = options->topology == 0 ? VK_PRIMITIVE_TOPOLOGY_POINT_LIST : options->topology == 1 ? VK_PRIMITIVE_TOPOLOGY_LINE_LIST : VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
  VkPipelineInputAssemblyStateCreateInfo assembly{VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO}; assembly.topology = topo;
  VkPipelineViewportStateCreateInfo viewport{VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO}; viewport.viewportCount = viewport.scissorCount = 1;
  VkPipelineRasterizationStateCreateInfo raster{VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO}; raster.polygonMode = VK_POLYGON_MODE_FILL; raster.lineWidth = 1.f;
  VkPipelineMultisampleStateCreateInfo ms{VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO}; ms.rasterizationSamples = static_cast<VkSampleCountFlagBits>(options->samples);
  VkPipelineDepthStencilStateCreateInfo depth{VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO}; VkPipelineColorBlendAttachmentState att{}; att.colorWriteMask = 0xf;
  int b = options->blend; if (b > 0 && b < 6) {
    att.blendEnable = VK_TRUE; att.srcColorBlendFactor = blend[b][0]; att.dstColorBlendFactor = blend[b][1];
    att.srcAlphaBlendFactor = blend[b][2]; att.dstAlphaBlendFactor = blend[b][3];
    att.colorBlendOp = b == 5 ? VK_BLEND_OP_REVERSE_SUBTRACT : VK_BLEND_OP_ADD;
    att.alphaBlendOp = VK_BLEND_OP_ADD;
  }
  VkPipelineColorBlendStateCreateInfo blending{VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO}; blending.attachmentCount = 1; blending.pAttachments = &att;
  VkDynamicState states[] = {VK_DYNAMIC_STATE_VIEWPORT,VK_DYNAMIC_STATE_SCISSOR,VK_DYNAMIC_STATE_CULL_MODE,VK_DYNAMIC_STATE_FRONT_FACE,
    VK_DYNAMIC_STATE_DEPTH_TEST_ENABLE,VK_DYNAMIC_STATE_DEPTH_WRITE_ENABLE,VK_DYNAMIC_STATE_DEPTH_COMPARE_OP,VK_DYNAMIC_STATE_PRIMITIVE_TOPOLOGY};
  VkPipelineDynamicStateCreateInfo dynamic{VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO}; dynamic.dynamicStateCount = 8; dynamic.pDynamicStates = states;
  VkFormat color = rv_color_format(options->color_format); VkPipelineRenderingCreateInfo rendering{VK_STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO};
  rendering.colorAttachmentCount = 1; rendering.pColorAttachmentFormats = &color; rendering.depthAttachmentFormat = options->has_depth ? VK_FORMAT_D32_SFLOAT : VK_FORMAT_UNDEFINED;
  VkGraphicsPipelineCreateInfo ci{VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO}; ci.pNext = &rendering; ci.stageCount = fragment ? 2 : 1; ci.pStages = stages;
  ci.pVertexInputState = &input; ci.pInputAssemblyState = &assembly; ci.pViewportState = &viewport; ci.pRasterizationState = &raster;
  ci.pMultisampleState = &ms; ci.pDepthStencilState = &depth; ci.pColorBlendState = &blending; ci.pDynamicState = &dynamic; ci.layout = p->layout;
  r = vkCreateGraphicsPipelines(d->device, d->pipeline_cache, 1, &ci, nullptr, &p->pipeline);
  if (r != VK_SUCCESS) { vkDestroyPipelineLayout(d->device, p->layout, nullptr); vkDestroyDescriptorSetLayout(d->device, p->set_layout, nullptr); delete p; return r; }
  *out = p; return VK_SUCCESS;
}
void rv_pipeline_destroy(void *pipeline) { auto *p = static_cast<Pipeline *>(pipeline); defer(p->device, destroy_pipeline_now, p); }
void rv_cmd_set_pipeline(void *commands, void *pipeline) { auto *c = static_cast<Command *>(commands); auto *p = static_cast<Pipeline *>(pipeline); vkCmdBindPipeline(c->cb, p->point, p->pipeline); c->pipeline = p; c->dirty = true; }
int32_t rv_cmd_set_buffer(void *commands, int32_t index, uint64_t offset, void *buffer) {
  auto *c = static_cast<Command *>(commands); auto *b = static_cast<Buffer *>(buffer); if (index < 0 || index >= static_cast<int>(MAX_SLOTS) || offset > b->size) return VK_ERROR_TOO_MANY_OBJECTS;
  c->slots[index] = {1, b->buffer, offset, b->size - offset}; c->dirty = true; return VK_SUCCESS;
}
int32_t rv_cmd_set_bytes(void *commands, int32_t index, const void *bytes, uint64_t length) {
  auto *c = static_cast<Command *>(commands); if (index < 0 || index >= static_cast<int>(MAX_SLOTS)) return VK_ERROR_TOO_MANY_OBJECTS;
  VkBuffer chunk{}; VkDeviceSize at{}; VkDeviceSize align = c->queue->device->properties.limits.minUniformBufferOffsetAlignment;
  char *room = arena(c, length ? length : 16, align, &chunk, &at); if (!room) return VK_ERROR_OUT_OF_DEVICE_MEMORY;
  if (length) memcpy(room, bytes, length); c->slots[index] = {1, chunk, at, length ? length : 16}; c->dirty = true; return VK_SUCCESS;
}
int32_t rv_cmd_set_texture(void *commands, int32_t index, void *texture) {
  auto *c = static_cast<Command *>(commands); if (index < 0 || index >= static_cast<int>(MAX_SLOTS)) return VK_ERROR_TOO_MANY_OBJECTS;
  c->slots[index] = {}; c->slots[index].kind = 2; c->slots[index].view = static_cast<Texture *>(texture)->view; c->dirty = true; return VK_SUCCESS;
}
int32_t rv_cmd_set_sampler(void *commands, int32_t index, void *sampler) {
  auto *c = static_cast<Command *>(commands); if (index < 0 || index >= static_cast<int>(MAX_SLOTS)) return VK_ERROR_TOO_MANY_OBJECTS;
  c->slots[index] = {}; c->slots[index].kind = 3; c->slots[index].sampler = reinterpret_cast<VkSampler>(sampler); c->dirty = true; return VK_SUCCESS;
}
void rv_compute_begin(void *commands) { static_cast<Command *>(commands)->dispatched = false; }
void rv_compute_dispatch(void *commands, uint32_t x, uint32_t y, uint32_t z) { auto *c = static_cast<Command *>(commands); before_dispatch(c); vkCmdDispatch(c->cb, x, y, z); }
void rv_compute_dispatch_indirect(void *commands, void *buffer, uint64_t offset) { auto *c = static_cast<Command *>(commands); before_dispatch(c); vkCmdDispatchIndirect(c->cb, static_cast<Buffer *>(buffer)->buffer, offset); }
void rv_encoder_end(void *commands) { full_barrier(static_cast<Command *>(commands)->cb); }
int32_t rv_texture_create(void *device, const rv_texture_desc *desc, void **out) {
  auto *d = static_cast<Device *>(device); auto *t = new (std::nothrow) Texture{}; if (!t) return VK_ERROR_OUT_OF_HOST_MEMORY;
  t->device = d; t->width = desc->width; t->height = desc->height; t->depth = desc->depth; t->mips = desc->mips; t->samples = desc->samples; t->format = rv_color_format(desc->format);
  bool depth = t->format == VK_FORMAT_D32_SFLOAT; t->aspect = depth ? VK_IMAGE_ASPECT_DEPTH_BIT : VK_IMAGE_ASPECT_COLOR_BIT;
  VkImageUsageFlags usage = VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT;
  if (desc->storage && t->samples == 1) usage |= VK_IMAGE_USAGE_STORAGE_BIT; if (desc->attachment) usage |= depth ? VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT : VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
  VkImageCreateInfo ii{VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO}; ii.imageType = desc->volume ? VK_IMAGE_TYPE_3D : VK_IMAGE_TYPE_2D; ii.format = t->format;
  ii.extent = {t->width,t->height,t->depth}; ii.mipLevels = t->mips; ii.arrayLayers = 1; ii.samples = static_cast<VkSampleCountFlagBits>(t->samples);
  ii.tiling = VK_IMAGE_TILING_OPTIMAL; ii.usage = usage; ii.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
  VmaAllocationCreateInfo ai{}; ai.usage = VMA_MEMORY_USAGE_AUTO_PREFER_DEVICE;
  VkResult r = vmaCreateImage(d->allocator, &ii, &ai, &t->image, &t->allocation, &t->info); if (r != VK_SUCCESS) { delete t; return r; }
  d->held.fetch_add(static_cast<int64_t>(t->info.size), std::memory_order_relaxed);
  VkImageViewCreateInfo vi{VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO}; vi.image = t->image; vi.viewType = desc->volume ? VK_IMAGE_VIEW_TYPE_3D : VK_IMAGE_VIEW_TYPE_2D;
  vi.format = t->format; vi.subresourceRange = {t->aspect,0,t->mips,0,1};
  r = vkCreateImageView(d->device, &vi, nullptr, &t->view);
  if (r != VK_SUCCESS) { d->held.fetch_sub(static_cast<int64_t>(t->info.size), std::memory_order_relaxed); vmaDestroyImage(d->allocator,t->image,t->allocation); delete t; return r; }
  r = image_to_general(d,t);
  if (r != VK_SUCCESS) { vkDestroyImageView(d->device,t->view,nullptr); d->held.fetch_sub(static_cast<int64_t>(t->info.size),std::memory_order_relaxed); vmaDestroyImage(d->allocator,t->image,t->allocation); delete t; return r; }
  *out = t; return VK_SUCCESS;
}
void destroy_texture_now(Device *d, void *payload) {
  auto *t = static_cast<Texture *>(payload); vkDestroyImageView(d->device,t->view,nullptr);
  if (t->memory_count) { vkDestroyImage(d->device,t->image,nullptr); for (uint32_t i=0;i<t->memory_count;++i) vkFreeMemory(d->device,t->memories[i],nullptr); }
  else vmaDestroyImage(d->allocator,t->image,t->allocation);
  delete t;
}
void rv_texture_destroy(void *texture) {
  auto *t = static_cast<Texture *>(texture); Device *d = t->device; d->held.fetch_sub(static_cast<int64_t>(t->info.size), std::memory_order_relaxed);
  if (t->memory_count) { std::lock_guard<std::recursive_mutex> lock(d->qlock); for (Texture **at = &d->externals; *at; at = &(*at)->next_external) if (*at == t) { *at = t->next_external; break; } }
  defer(d,destroy_texture_now,t);
}
int32_t rv_texture_read(void *texture, void *bytes, uint64_t row, uint32_t texel) {
  auto *t = static_cast<Texture *>(texture); Device *d = t->device; VkResult r = flush_uploads_wait(d); if (r != VK_SUCCESS) return r;
  VkDeviceSize total = row * t->height; VkBufferCreateInfo bi{VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO}; bi.size = total; bi.usage = VK_BUFFER_USAGE_TRANSFER_DST_BIT;
  VmaAllocationCreateInfo ai{}; ai.flags = VMA_ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT | VMA_ALLOCATION_CREATE_MAPPED_BIT; ai.usage = VMA_MEMORY_USAGE_AUTO; ai.preferredFlags = VK_MEMORY_PROPERTY_HOST_CACHED_BIT;
  VkBuffer staging{}; VmaAllocation allocation{}; VmaAllocationInfo mapped{}; r = vmaCreateBuffer(d->allocator,&bi,&ai,&staging,&allocation,&mapped); if (r != VK_SUCCESS) return r;
  uint64_t tick = 0;
  { std::lock_guard<std::recursive_mutex> lock(d->qlock); Command *u = upload_cmd(d);
    if (u) { VkBufferImageCopy region{}; region.bufferRowLength = static_cast<uint32_t>(row / texel); region.imageSubresource = {t->aspect,0,0,1}; region.imageExtent = {t->width,t->height,1};
      full_barrier(u->cb); vkCmdCopyImageToBuffer(u->cb,t->image,VK_IMAGE_LAYOUT_GENERAL,staging,1,&region); r = submit_locked(d,nullptr,nullptr,0,VK_NULL_HANDLE,VK_NULL_HANDLE,&tick); }
    else r = VK_ERROR_OUT_OF_HOST_MEMORY; }
  if (r == VK_SUCCESS) { VkSemaphoreWaitInfo wi{VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO}; wi.semaphoreCount = 1; wi.pSemaphores = &d->ticks; wi.pValues = &tick; r = vkWaitSemaphores(d->device,&wi,UINT64_MAX); }
  if (r == VK_SUCCESS) { vmaInvalidateAllocation(d->allocator,allocation,0,total); memcpy(bytes,mapped.pMappedData,total); }
  vmaDestroyBuffer(d->allocator,staging,allocation); return r;
}
// Host-to-image rectangle: staged into the pending upload command like staged_write, so it lands before the next submit.
// `bytes` holds the rows at a->bytes_per_row; a->offset is ignored.
int32_t rv_texture_write(void *texture, const void *bytes, const rv_blit_desc *a) {
  auto *t = static_cast<Texture *>(texture); Device *d = t->device; std::lock_guard<std::recursive_mutex> lock(d->qlock);
  VkDeviceSize total = static_cast<VkDeviceSize>(a->bytes_per_row) * (static_cast<VkDeviceSize>(a->height) * a->depth - 1) + static_cast<VkDeviceSize>(a->width) * a->texel;
  Command *u = upload_cmd(d); VkBuffer chunk{}; VkDeviceSize at{}; char *room = u ? arena(u, total, 16, &chunk, &at) : nullptr;
  if (!room) return VK_ERROR_OUT_OF_DEVICE_MEMORY; memcpy(room, bytes, total);
  VkBufferImageCopy r{}; r.bufferOffset = at; r.bufferRowLength = a->bytes_per_row / a->texel; r.imageSubresource = {t->aspect,a->mip,0,1};
  r.imageOffset = {a->x,a->y,a->z}; r.imageExtent = {a->width,a->height,a->depth}; full_barrier(u->cb);
  vkCmdCopyBufferToImage(u->cb,chunk,t->image,VK_IMAGE_LAYOUT_GENERAL,1,&r); return VK_SUCCESS;
}
void rv_blit_buffer_to_texture(void *commands, void *buffer, void *texture, const rv_blit_desc *a) {
  auto *c = static_cast<Command *>(commands); auto *t = static_cast<Texture *>(texture);
  VkBufferImageCopy r{}; r.bufferOffset = a->offset; r.bufferRowLength = a->bytes_per_row / a->texel; r.imageSubresource = {t->aspect,a->mip,0,1};
  r.imageOffset = {a->x,a->y,a->z}; r.imageExtent = {a->width,a->height,a->depth}; full_barrier(c->cb);
  vkCmdCopyBufferToImage(c->cb,static_cast<Buffer *>(buffer)->buffer,t->image,VK_IMAGE_LAYOUT_GENERAL,1,&r);
}
int32_t rv_sampler_create(void *device, const rv_sampler_desc *a, void **out) {
  static const VkSamplerAddressMode modes[] = {VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,VK_SAMPLER_ADDRESS_MODE_REPEAT,VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT};
  if (a->u < 0 || a->u > 2 || a->v < 0 || a->v > 2) return VK_ERROR_VALIDATION_FAILED_EXT;
  VkSamplerCreateInfo si{VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO}; si.minFilter = a->min ? VK_FILTER_LINEAR : VK_FILTER_NEAREST;
  si.magFilter = a->mag ? VK_FILTER_LINEAR : VK_FILTER_NEAREST; si.mipmapMode = a->mip == 2 ? VK_SAMPLER_MIPMAP_MODE_LINEAR : VK_SAMPLER_MIPMAP_MODE_NEAREST;
  si.addressModeU = modes[a->u]; si.addressModeV = modes[a->v]; si.addressModeW = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
  si.anisotropyEnable = a->anisotropy > 1; si.maxAnisotropy = static_cast<float>(a->anisotropy);
  si.minLod = static_cast<float>(a->lod_min); si.maxLod = a->mip == 0 ? si.minLod + 0.25f : static_cast<float>(a->lod_max);
  VkSampler sampler{}; VkResult r = vkCreateSampler(static_cast<Device *>(device)->device,&si,nullptr,&sampler); if (r == VK_SUCCESS) *out = reinterpret_cast<void *>(sampler); return r;
}
void destroy_sampler_now(Device *d, void *payload) { vkDestroySampler(d->device,reinterpret_cast<VkSampler>(payload),nullptr); }
void rv_sampler_destroy(void *device, void *sampler) { defer(static_cast<Device *>(device),destroy_sampler_now,sampler); }
struct rv_attachment { void *texture, *resolve; int32_t load, store; double clear[4]; };
struct rv_depth_attachment { void *texture; int32_t load, store; double clear; };
struct rv_rect { int32_t x, y, width, height; };
struct rv_size { uint32_t width, height; };
static const VkAttachmentLoadOp rv_load_ops[] = {VK_ATTACHMENT_LOAD_OP_LOAD,VK_ATTACHMENT_LOAD_OP_CLEAR,VK_ATTACHMENT_LOAD_OP_DONT_CARE};
static const VkPrimitiveTopology rv_topologies[] = {VK_PRIMITIVE_TOPOLOGY_POINT_LIST,VK_PRIMITIVE_TOPOLOGY_LINE_LIST,VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP,VK_PRIMITIVE_TOPOLOGY_LINE_STRIP};
static float rv_clear_component(const Texture *t, double v) {
  bool unorm8 = t->format == VK_FORMAT_R8G8B8A8_UNORM || t->format == VK_FORMAT_B8G8R8A8_UNORM || t->format == VK_FORMAT_R8_UNORM;
  if (!unorm8) return static_cast<float>(v); double c = std::clamp(v, 0.0, 1.0); return static_cast<float>(std::floor(c * 255.0 + 0.5) / 255.0);
}
int32_t rv_render_begin(void *commands, const rv_attachment *colors, uint32_t count, const rv_depth_attachment *depth, const rv_size *size) {
  if (count > 8) return VK_ERROR_TOO_MANY_OBJECTS; auto *c = static_cast<Command *>(commands); VkRenderingAttachmentInfo attachments[8]{};
  for (uint32_t i = 0; i < count; ++i) {
    auto *t = static_cast<Texture *>(colors[i].texture); auto *resolve = static_cast<Texture *>(colors[i].resolve);
    attachments[i] = {VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO}; attachments[i].imageView = t->view; attachments[i].imageLayout = VK_IMAGE_LAYOUT_GENERAL;
    if (colors[i].load < 0 || colors[i].load > 2) return VK_ERROR_VALIDATION_FAILED_EXT;
    attachments[i].loadOp = rv_load_ops[colors[i].load]; attachments[i].storeOp = colors[i].store == 0 ? VK_ATTACHMENT_STORE_OP_STORE : VK_ATTACHMENT_STORE_OP_DONT_CARE;
    for (uint32_t k = 0; k < 4; ++k) attachments[i].clearValue.color.float32[k] = rv_clear_component(t, colors[i].clear[k]);
    if (resolve) { attachments[i].resolveMode = VK_RESOLVE_MODE_AVERAGE_BIT; attachments[i].resolveImageView = resolve->view; attachments[i].resolveImageLayout = VK_IMAGE_LAYOUT_GENERAL; }
  }
  VkRenderingAttachmentInfo da{}; if (depth) {
    auto *t = static_cast<Texture *>(depth->texture); if (depth->load < 0 || depth->load > 2) return VK_ERROR_VALIDATION_FAILED_EXT;
    da.sType = VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO; da.imageView = t->view; da.imageLayout = VK_IMAGE_LAYOUT_GENERAL;
    da.loadOp = rv_load_ops[depth->load]; da.storeOp = depth->store == 0 ? VK_ATTACHMENT_STORE_OP_STORE : VK_ATTACHMENT_STORE_OP_DONT_CARE; da.clearValue.depthStencil.depth = static_cast<float>(depth->clear);
  }
  VkRenderingInfo info{VK_STRUCTURE_TYPE_RENDERING_INFO}; info.renderArea = {{0,0},{size->width,size->height}}; info.layerCount = 1;
  info.colorAttachmentCount = count; info.pColorAttachments = attachments; info.pDepthAttachment = depth ? &da : nullptr;
  vkCmdBeginRendering(c->cb,&info); c->rendering = true; c->target_height = size->height;
  VkViewport vp{0.f,static_cast<float>(size->height),static_cast<float>(size->width),-static_cast<float>(size->height),0.f,1.f};
  VkRect2D sc{{0,0},{size->width,size->height}}; vkCmdSetViewport(c->cb,0,1,&vp); vkCmdSetScissor(c->cb,0,1,&sc);
  vkCmdSetCullMode(c->cb,VK_CULL_MODE_NONE); vkCmdSetFrontFace(c->cb,VK_FRONT_FACE_CLOCKWISE);
  vkCmdSetDepthTestEnable(c->cb,VK_FALSE); vkCmdSetDepthWriteEnable(c->cb,VK_FALSE); vkCmdSetDepthCompareOp(c->cb,VK_COMPARE_OP_ALWAYS);
  vkCmdSetPrimitiveTopology(c->cb,VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST); return VK_SUCCESS;
}
void rv_render_end(void *commands) { auto *c = static_cast<Command *>(commands); vkCmdEndRendering(c->cb); c->rendering = false; full_barrier(c->cb); }
void rv_render_viewport(void *commands, const rv_rect *r) {
  VkViewport vp{static_cast<float>(r->x),static_cast<float>(r->y+r->height),static_cast<float>(r->width),-static_cast<float>(r->height),0.f,1.f};
  vkCmdSetViewport(static_cast<Command *>(commands)->cb,0,1,&vp);
}
void rv_render_scissor(void *commands, const rv_rect *r) {
  VkRect2D sc{{r->x,r->y},{static_cast<uint32_t>(r->width),static_cast<uint32_t>(r->height)}}; vkCmdSetScissor(static_cast<Command *>(commands)->cb,0,1,&sc);
}
int32_t rv_render_cull(void *commands, int32_t mode) {
  static const VkCullModeFlags modes[] = {VK_CULL_MODE_NONE,VK_CULL_MODE_FRONT_BIT,VK_CULL_MODE_BACK_BIT};
  if (mode < 0 || mode > 2) return VK_ERROR_VALIDATION_FAILED_EXT; vkCmdSetCullMode(static_cast<Command *>(commands)->cb,modes[mode]); return VK_SUCCESS;
}
void rv_render_winding(void *commands, bool clockwise) { vkCmdSetFrontFace(static_cast<Command *>(commands)->cb,clockwise?VK_FRONT_FACE_CLOCKWISE:VK_FRONT_FACE_COUNTER_CLOCKWISE); }
int32_t rv_render_depth(void *commands, int32_t compare, bool write) {
  static const VkCompareOp ops[] = {VK_COMPARE_OP_NEVER,VK_COMPARE_OP_LESS,VK_COMPARE_OP_EQUAL,VK_COMPARE_OP_LESS_OR_EQUAL,VK_COMPARE_OP_GREATER,VK_COMPARE_OP_NOT_EQUAL,VK_COMPARE_OP_GREATER_OR_EQUAL,VK_COMPARE_OP_ALWAYS};
  auto cb = static_cast<Command *>(commands)->cb; if (compare < -1 || compare > 7) return VK_ERROR_VALIDATION_FAILED_EXT;
  vkCmdSetDepthTestEnable(cb,compare >= 0); vkCmdSetDepthWriteEnable(cb,compare >= 0 && write); if (compare >= 0) vkCmdSetDepthCompareOp(cb,ops[compare]); return VK_SUCCESS;
}
int32_t rv_render_draw(void *commands, int32_t primitive, uint32_t first, uint32_t count, uint32_t instances) {
  if (primitive < 0 || primitive > 4) return VK_ERROR_VALIDATION_FAILED_EXT; auto *c = static_cast<Command *>(commands);
  push_descriptors(c); vkCmdSetPrimitiveTopology(c->cb,rv_topologies[primitive]); vkCmdDraw(c->cb,count,instances,first,0); return VK_SUCCESS;
}
static void bind_index(Command *c, Buffer *b, uint64_t offset, int32_t type) {
  c->index_buffer = b->buffer; c->index_offset = offset;
  vkCmdBindIndexBuffer(c->cb,b->buffer,offset,type?VK_INDEX_TYPE_UINT32:VK_INDEX_TYPE_UINT16);
}
int32_t rv_render_draw_indexed(void *commands, void *buffer, int32_t primitive, int32_t type, uint64_t offset, uint32_t count, uint32_t instances) {
  if (primitive < 0 || primitive > 4 || type < 0 || type > 1) return VK_ERROR_VALIDATION_FAILED_EXT; auto *c = static_cast<Command *>(commands);
  push_descriptors(c); vkCmdSetPrimitiveTopology(c->cb,rv_topologies[primitive]); bind_index(c,static_cast<Buffer *>(buffer),offset,type);
  vkCmdDrawIndexed(c->cb,count,instances,0,0,0); return VK_SUCCESS;
}
int32_t rv_render_draw_indexed_indirect(void *commands, void *buffer, void *arguments, int32_t primitive, int32_t type, uint64_t offset, uint64_t args_offset) {
  if (primitive < 0 || primitive > 4 || type < 0 || type > 1) return VK_ERROR_VALIDATION_FAILED_EXT; auto *c = static_cast<Command *>(commands);
  push_descriptors(c); vkCmdSetPrimitiveTopology(c->cb,rv_topologies[primitive]); bind_index(c,static_cast<Buffer *>(buffer),offset,type);
  vkCmdDrawIndexedIndirect(c->cb,static_cast<Buffer *>(arguments)->buffer,args_offset,1,20); return VK_SUCCESS;
}
static VkResult rv_swapchain_build(Surface *s) {
  Device *d = s->device; VkSurfaceCapabilitiesKHR caps{}; VkResult r = vkGetPhysicalDeviceSurfaceCapabilitiesKHR(d->physical,s->surface,&caps); if (r != VK_SUCCESS) return r;
  { std::lock_guard<std::recursive_mutex> lock(d->qlock); r = vkQueueWaitIdle(d->main_queue); } if (r != VK_SUCCESS) return r;
  VkSwapchainKHR old = s->swapchain; s->swapchain = VK_NULL_HANDLE; s->held_image = s->current_image = -1; s->stale = false;
  VkExtent2D extent = caps.currentExtent;
  if (extent.width == UINT32_MAX) extent = {std::clamp(s->wanted.width, caps.minImageExtent.width, caps.maxImageExtent.width), std::clamp(s->wanted.height, caps.minImageExtent.height, caps.maxImageExtent.height)};
  if (!s->wanted.width && caps.currentExtent.width == UINT32_MAX) extent = {};
  s->extent = extent;
  if (!extent.width || !extent.height) { if (old) vkDestroySwapchainKHR(d->device,old,nullptr); return VK_SUCCESS; }
  uint32_t minimum = std::max(caps.minImageCount + 1,3u); if (caps.maxImageCount && minimum > caps.maxImageCount) minimum = caps.maxImageCount;
  VkSwapchainCreateInfoKHR ci{VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR}; ci.surface = s->surface; ci.minImageCount = minimum;
  ci.imageFormat = VK_FORMAT_B8G8R8A8_UNORM; ci.imageColorSpace = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR; ci.imageExtent = extent;
  ci.imageArrayLayers = 1; ci.imageUsage = VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT; ci.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
  // An unthrottled request takes a supported mode: IMMEDIATE, else MAILBOX, else FIFO (which every surface offers).
  // Wayland compositors never tear, so there it is MAILBOX: Mesa 25.2's lavapipe segfaults in libwayland presenting
  // IMMEDIATE to WSLg's Weston. ponytail: Wayland skips IMMEDIATE outright; retry it once Mesa's path is known good.
  VkPresentModeKHR modes[8]; uint32_t mode_count = 8; vkGetPhysicalDeviceSurfacePresentModesKHR(d->physical,s->surface,&mode_count,modes);
  auto offered = [&](VkPresentModeKHR m) { return std::find(modes, modes + mode_count, m) != modes + mode_count; };
  VkPresentModeKHR mode = VK_PRESENT_MODE_FIFO_KHR;
  if (s->mode != VK_PRESENT_MODE_FIFO_KHR && offered(VK_PRESENT_MODE_MAILBOX_KHR)) mode = VK_PRESENT_MODE_MAILBOX_KHR;
#ifdef _WIN32
  if (s->mode != VK_PRESENT_MODE_FIFO_KHR && offered(VK_PRESENT_MODE_IMMEDIATE_KHR)) mode = VK_PRESENT_MODE_IMMEDIATE_KHR;
#endif
  ci.preTransform = caps.currentTransform; ci.compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR; ci.presentMode = mode; ci.clipped = VK_TRUE; ci.oldSwapchain = old;
  r = vkCreateSwapchainKHR(d->device,&ci,nullptr,&s->swapchain); if (old) vkDestroySwapchainKHR(d->device,old,nullptr); if (r != VK_SUCCESS) return r;
  s->image_count = 16; r = vkGetSwapchainImagesKHR(d->device,s->swapchain,&s->image_count,s->images); if (r != VK_SUCCESS) return r;
  VkSemaphoreCreateInfo si{VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO};
  for (uint32_t i=0;i<16;++i) if (!s->rendered[i]) vkCreateSemaphore(d->device,&si,nullptr,&s->rendered[i]);
  for (uint32_t i=0;i<17;++i) { if (s->acquired[i]) vkDestroySemaphore(d->device,s->acquired[i],nullptr); vkCreateSemaphore(d->device,&si,nullptr,&s->acquired[i]); }
  return VK_SUCCESS;
}
#ifndef _WIN32
// Matches native_layer_token_sys::WaylandLayer: the token's pointer names this pair.
struct WaylandLayer { wl_display *display; wl_surface *surface; };
#endif
int32_t rv_surface_create(void *device, void *hwnd, bool immediate, void **out) {
  auto *d = static_cast<Device *>(device); if (!hwnd) return VK_ERROR_SURFACE_LOST_KHR;
  if (!d->swapchain) return VK_ERROR_EXTENSION_NOT_PRESENT;
  auto *s = new (std::nothrow) Surface{}; if (!s) return VK_ERROR_OUT_OF_HOST_MEMORY; s->device=d; s->mode=immediate?VK_PRESENT_MODE_IMMEDIATE_KHR:VK_PRESENT_MODE_FIFO_KHR;
#ifdef _WIN32
  VkWin32SurfaceCreateInfoKHR si{VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR}; si.hinstance=GetModuleHandleW(nullptr); si.hwnd=static_cast<HWND>(hwnd);
  VkResult r=vkCreateWin32SurfaceKHR(d->instance,&si,nullptr,&s->surface);
#else
  auto *layer=static_cast<WaylandLayer *>(hwnd);
  VkWaylandSurfaceCreateInfoKHR si{VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR}; si.display=layer->display; si.surface=layer->surface;
  VkResult r=vkCreateWaylandSurfaceKHR?vkCreateWaylandSurfaceKHR(d->instance,&si,nullptr,&s->surface):VK_ERROR_EXTENSION_NOT_PRESENT;
#endif
  if(r!=VK_SUCCESS){delete s;return r;}
  VkBool32 supported=VK_FALSE; vkGetPhysicalDeviceSurfaceSupportKHR(d->physical,d->main_family,s->surface,&supported);
  r=supported?rv_swapchain_build(s):VK_ERROR_INCOMPATIBLE_DISPLAY_KHR;
  if(r!=VK_SUCCESS){vkDestroySurfaceKHR(d->instance,s->surface,nullptr);delete s;return r;} *out=s;return VK_SUCCESS;
}
void rv_surface_set_extent(void *surface, uint32_t width, uint32_t height) { auto *s=static_cast<Surface *>(surface); s->wanted={width,height}; s->stale=true; }
int32_t rv_surface_configure(void *surface, bool immediate) { auto *s=static_cast<Surface *>(surface); s->mode=immediate?VK_PRESENT_MODE_IMMEDIATE_KHR:VK_PRESENT_MODE_FIFO_KHR; return rv_swapchain_build(s); }
int32_t rv_surface_acquire(void *surface, int32_t *image) {
  auto *s=static_cast<Surface *>(surface);
  if(s->held_image>=0){s->current_image=s->held_image;s->current_semaphore=s->held_semaphore;s->held_image=-1;*image=s->current_image;return VK_SUCCESS;}
  for(int attempt=0;attempt<2;++attempt){
    if(!s->swapchain||s->stale){VkResult built=rv_swapchain_build(s);if(built!=VK_SUCCESS)return built;if(!s->swapchain){*image=-2;return VK_SUCCESS;}}
    VkSemaphore sem=s->acquired[s->acquire_at];s->acquire_at=(s->acquire_at+1)%17;uint32_t index=0;
    VkResult r=vkAcquireNextImageKHR(s->device->device,s->swapchain,1000000000ull,sem,VK_NULL_HANDLE,&index);
    if(r==VK_TIMEOUT||r==VK_NOT_READY){*image=-1;return VK_SUCCESS;}
    if(r==VK_ERROR_OUT_OF_DATE_KHR){s->stale=true;continue;} if(r==VK_SUBOPTIMAL_KHR)s->stale=true;else if(r!=VK_SUCCESS)return r;
    s->current_image=static_cast<int>(index);s->current_semaphore=sem;*image=static_cast<int32_t>(index);return VK_SUCCESS;
  }
  *image=-1;return VK_SUCCESS;
}
void rv_surface_discard(void *surface) { auto *s=static_cast<Surface *>(surface);s->held_image=s->current_image;s->held_semaphore=s->current_semaphore;s->current_image=-1; }
int32_t rv_commit_present(void *commands, void *surface, void *source, uint64_t *epoch, bool *committed) {
  auto *c=static_cast<Command *>(commands);auto *s=static_cast<Surface *>(surface);auto *t=static_cast<Texture *>(source);Device *d=s->device;
  *committed=false;
  if(s->current_image<0||static_cast<uint32_t>(s->current_image)>=s->image_count)return VK_ERROR_OUT_OF_DATE_KHR;
  uint32_t idx=static_cast<uint32_t>(s->current_image);VkImage image=s->images[idx];VkImageSubresourceRange range{VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1};
  VkImageMemoryBarrier2 b{VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2};b.srcStageMask=VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;b.dstStageMask=VK_PIPELINE_STAGE_2_BLIT_BIT;
  b.dstAccessMask=VK_ACCESS_2_TRANSFER_WRITE_BIT;b.oldLayout=VK_IMAGE_LAYOUT_UNDEFINED;b.newLayout=VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;b.image=image;
  b.srcQueueFamilyIndex=b.dstQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED;b.subresourceRange=range;
  VkDependencyInfo info{VK_STRUCTURE_TYPE_DEPENDENCY_INFO};info.imageMemoryBarrierCount=1;info.pImageMemoryBarriers=&b;full_barrier(c->cb);vkCmdPipelineBarrier2(c->cb,&info);
  VkImageBlit blit{};blit.srcSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1};blit.srcOffsets[1]={static_cast<int32_t>(t->width),static_cast<int32_t>(t->height),1};
  blit.dstSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1};blit.dstOffsets[1]={static_cast<int32_t>(s->extent.width),static_cast<int32_t>(s->extent.height),1};
  vkCmdBlitImage(c->cb,t->image,VK_IMAGE_LAYOUT_GENERAL,image,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,1,&blit,VK_FILTER_NEAREST);
  /* dst ALL_COMMANDS, not NONE: the transition must be ordered before the semaphore signal (sync validation PRESENT_AFTER_WRITE). */
  b.srcStageMask=VK_PIPELINE_STAGE_2_BLIT_BIT;b.srcAccessMask=VK_ACCESS_2_TRANSFER_WRITE_BIT;b.dstStageMask=VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;b.dstAccessMask=0;
  b.oldLayout=VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;b.newLayout=VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;vkCmdPipelineBarrier2(c->cb,&info);
  VkSemaphore rendered=s->rendered[idx];int64_t e=commit_cmd(c,s->current_semaphore,rendered);if(e<0)return static_cast<int32_t>(e);*committed=true;
  VkPresentInfoKHR pi{VK_STRUCTURE_TYPE_PRESENT_INFO_KHR};pi.waitSemaphoreCount=1;pi.pWaitSemaphores=&rendered;pi.swapchainCount=1;pi.pSwapchains=&s->swapchain;pi.pImageIndices=&idx;
  VkResult r;{std::lock_guard<std::recursive_mutex> lock(d->qlock);r=vkQueuePresentKHR(d->main_queue,&pi);}s->current_image=-1;
  if(r==VK_ERROR_OUT_OF_DATE_KHR||r==VK_SUBOPTIMAL_KHR)s->stale=true;else if(r!=VK_SUCCESS)return r;*epoch=static_cast<uint64_t>(e);return VK_SUCCESS;
}
void rv_surface_destroy(void *surface) {
  auto *s=static_cast<Surface *>(surface);Device *d=s->device;{std::lock_guard<std::recursive_mutex> lock(d->qlock);vkQueueWaitIdle(d->main_queue);}
  for(auto sem:s->rendered)if(sem)vkDestroySemaphore(d->device,sem,nullptr);for(auto sem:s->acquired)if(sem)vkDestroySemaphore(d->device,sem,nullptr);
  if(s->swapchain)vkDestroySwapchainKHR(d->device,s->swapchain,nullptr);vkDestroySurfaceKHR(d->instance,s->surface,nullptr);delete s;
}
#ifdef __linux__
// Compositor hooks. Format/modifier query, dmabuf import and sync_file handling are ported from wlroots 0.20.2
// (MIT; specification/licenses.md): render/vulkan/pixel_format.c, texture.c, renderer.c, pass.c, render/dmabuf_linux.c.
struct rv_dmabuf_plane { int32_t fd; uint32_t offset, stride; };
struct rv_dmabuf { uint32_t width, height; int32_t format; uint32_t plane_count; uint64_t modifier; rv_dmabuf_plane planes[4]; };
struct rv_dmabuf_modifier { uint64_t modifier; uint32_t planes, max_width, max_height; uint8_t texture, render; };
struct rv_external { uint8_t dmabuf, modifiers, sync_export, sync_import, dmabuf_sync; };
// wlroots vulkan_dma_tex_usage and vulkan_render_usage, plus transfer-source so either can be read back.
constexpr VkImageUsageFlags RV_TEXTURE_USAGE = VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT;
constexpr VkImageUsageFlags RV_RENDER_USAGE = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT;
constexpr VkFormatFeatureFlags RV_TEXTURE_FEATURES = VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT | VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT | VK_FORMAT_FEATURE_TRANSFER_SRC_BIT;
constexpr VkFormatFeatureFlags RV_RENDER_FEATURES = VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT | VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT | VK_FORMAT_FEATURE_TRANSFER_SRC_BIT;
// The modifiers of `format` (wlroots vulkan_format_props_query). Without VK_EXT_image_drm_format_modifier only
// linear exists: a single plane in a VK_IMAGE_TILING_LINEAR image.
static uint32_t rv_modifier_list(Device *d, VkFormat format, VkDrmFormatModifierPropertiesEXT *out, uint32_t capacity) {
  if (!d->modifiers) { VkFormatProperties p{}; vkGetPhysicalDeviceFormatProperties(d->physical, format, &p); out[0] = {0, 1, p.linearTilingFeatures}; return 1; }
  VkDrmFormatModifierPropertiesListEXT list{VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT}; list.drmFormatModifierCount = capacity; list.pDrmFormatModifierProperties = out;
  VkFormatProperties2 p{VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2}; p.pNext = &list; vkGetPhysicalDeviceFormatProperties2(d->physical, format, &p);
  return std::min(list.drmFormatModifierCount, capacity);
}
// wlroots query_modifier_usage_support: whether a dmabuf image of this format, modifier and usage can be
// imported, and its largest extent.
static bool rv_modifier_usage(Device *d, VkFormat format, uint64_t modifier, VkImageUsageFlags usage, VkExtent3D *max) {
  VkPhysicalDeviceImageDrmFormatModifierInfoEXT mi{VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT}; mi.drmFormatModifier = modifier; mi.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
  VkPhysicalDeviceExternalImageFormatInfo ei{VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO}; ei.handleType = VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT; ei.pNext = d->modifiers ? &mi : nullptr;
  VkPhysicalDeviceImageFormatInfo2 fi{VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2}; fi.pNext = &ei; fi.format = format; fi.type = VK_IMAGE_TYPE_2D; fi.usage = usage;
  fi.tiling = d->modifiers ? VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT : VK_IMAGE_TILING_LINEAR;
  VkExternalImageFormatProperties ep{VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES}; VkImageFormatProperties2 ip{VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2}; ip.pNext = &ep;
  if (vkGetPhysicalDeviceImageFormatProperties2(d->physical, &fi, &ip) != VK_SUCCESS || !(ep.externalMemoryProperties.externalMemoryFeatures & VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT)) return false;
  *max = ip.imageFormatProperties.maxExtent; return true;
}
// wlroots is_dmabuf_disjoint: planes in different buffers need one allocation each.
static bool rv_disjoint(const rv_dmabuf *a) {
  struct stat first{}; if (a->plane_count < 2) return false; if (fstat(a->planes[0].fd, &first)) return true;
  for (uint32_t i = 1; i < a->plane_count; ++i) { struct stat plane{}; if (fstat(a->planes[i].fd, &plane) || plane.st_ino != first.st_ino) return true; }
  return false;
}
static int rv_ioctl(int fd, unsigned long request, void *arg) { int r; do r = ioctl(fd, request, arg); while (r == -1 && (errno == EINTR || errno == EAGAIN)); return r; }
void rv_device_external(void *device, rv_external *out) {
  auto *d = static_cast<Device *>(device); *out = {d->dmabuf, d->modifiers, d->sync_export, d->sync_import, d->dmabuf_sync};
}
int32_t rv_dmabuf_modifiers(void *device, int32_t format, rv_dmabuf_modifier *out, uint32_t capacity, uint32_t *count) {
  auto *d = static_cast<Device *>(device); *count = 0; if (!d->dmabuf) return VK_SUCCESS;
  VkFormat vk = rv_color_format(format); VkDrmFormatModifierPropertiesEXT mods[64]; uint32_t n = rv_modifier_list(d, vk, mods, 64);
  for (uint32_t i = 0; i < n && *count < capacity; ++i) {
    const VkDrmFormatModifierPropertiesEXT &m = mods[i]; VkExtent3D tex{}, ren{};
    bool texture = (m.drmFormatModifierTilingFeatures & RV_TEXTURE_FEATURES) == RV_TEXTURE_FEATURES && rv_modifier_usage(d, vk, m.drmFormatModifier, RV_TEXTURE_USAGE, &tex);
    bool render = (m.drmFormatModifierTilingFeatures & RV_RENDER_FEATURES) == RV_RENDER_FEATURES && rv_modifier_usage(d, vk, m.drmFormatModifier, RV_RENDER_USAGE, &ren);
    if (!texture && !render) continue;
    VkExtent3D max = !render ? tex : !texture ? ren : VkExtent3D{std::min(tex.width, ren.width), std::min(tex.height, ren.height), 1};
    out[(*count)++] = {m.drmFormatModifier, m.drmFormatModifierPlaneCount, max.width, max.height, texture, render};
  }
  return VK_SUCCESS;
}
// wlroots vulkan_import_dmabuf. Ownership of the fds stays with the caller: each memory imports a duplicate.
int32_t rv_dmabuf_import(void *device, const rv_dmabuf *a, bool render, void **out) {
  auto *d = static_cast<Device *>(device); if (!d->dmabuf) return VK_ERROR_EXTENSION_NOT_PRESENT;
  if (!a->plane_count || a->plane_count > 4 || !a->width || !a->height) return VK_ERROR_VALIDATION_FAILED_EXT;
  VkFormat format = rv_color_format(a->format); VkDrmFormatModifierPropertiesEXT mods[64], *m = nullptr; uint32_t n = rv_modifier_list(d, format, mods, 64);
  for (uint32_t i = 0; i < n; ++i) if (mods[i].drmFormatModifier == a->modifier) m = &mods[i];
  VkImageUsageFlags usage = render ? RV_RENDER_USAGE : RV_TEXTURE_USAGE; VkFormatFeatureFlags features = render ? RV_RENDER_FEATURES : RV_TEXTURE_FEATURES; VkExtent3D max{};
  if (!m || (m->drmFormatModifierTilingFeatures & features) != features || !rv_modifier_usage(d, format, a->modifier, usage, &max)) return VK_ERROR_FORMAT_NOT_SUPPORTED;
  if (a->width > max.width || a->height > max.height || m->drmFormatModifierPlaneCount != a->plane_count) return VK_ERROR_VALIDATION_FAILED_EXT;
  bool disjoint = rv_disjoint(a); if (disjoint && !(m->drmFormatModifierTilingFeatures & VK_FORMAT_FEATURE_DISJOINT_BIT)) return VK_ERROR_FORMAT_NOT_SUPPORTED;
  // Without explicit modifiers the driver picks the linear layout itself: accept the buffer only where it agrees.
  if (!d->modifiers && a->planes[0].offset) return VK_ERROR_FORMAT_NOT_SUPPORTED;
  VkSubresourceLayout layouts[4]{}; for (uint32_t i = 0; i < a->plane_count; ++i) { layouts[i].offset = a->planes[i].offset; layouts[i].rowPitch = a->planes[i].stride; }
  VkImageDrmFormatModifierExplicitCreateInfoEXT mi{VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT}; mi.drmFormatModifier = a->modifier; mi.drmFormatModifierPlaneCount = a->plane_count; mi.pPlaneLayouts = layouts;
  VkExternalMemoryImageCreateInfo ei{VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO}; ei.handleTypes = VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT; ei.pNext = d->modifiers ? &mi : nullptr;
  VkImageCreateInfo ii{VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO}; ii.pNext = &ei; ii.flags = disjoint ? VK_IMAGE_CREATE_DISJOINT_BIT : 0; ii.imageType = VK_IMAGE_TYPE_2D; ii.format = format;
  ii.extent = {a->width, a->height, 1}; ii.mipLevels = ii.arrayLayers = 1; ii.samples = VK_SAMPLE_COUNT_1_BIT; ii.usage = usage; ii.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
  ii.tiling = d->modifiers ? VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT : VK_IMAGE_TILING_LINEAR;
  auto *t = new (std::nothrow) Texture{}; if (!t) return VK_ERROR_OUT_OF_HOST_MEMORY;
  t->device = d; t->format = format; t->aspect = VK_IMAGE_ASPECT_COLOR_BIT; t->width = a->width; t->height = a->height; t->depth = t->mips = t->samples = 1;
  VkResult r = vkCreateImage(d->device, &ii, nullptr, &t->image); if (r != VK_SUCCESS) { delete t; return r; }
  auto fail = [&](VkResult code) { vkDestroyImage(d->device, t->image, nullptr); for (uint32_t i = 0; i < t->memory_count; ++i) vkFreeMemory(d->device, t->memories[i], nullptr); delete t; return code; };
  if (!d->modifiers) { VkImageSubresource sub{VK_IMAGE_ASPECT_COLOR_BIT, 0, 0}; VkSubresourceLayout l{}; vkGetImageSubresourceLayout(d->device, t->image, &sub, &l);
    if (l.offset || l.rowPitch != a->planes[0].stride) return fail(VK_ERROR_FORMAT_NOT_SUPPORTED); }
  static const VkImageAspectFlagBits planes[] = {VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT, VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT, VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT, VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT};
  uint32_t count = disjoint ? a->plane_count : 1; VkBindImageMemoryInfo binds[4]{}; VkBindImagePlaneMemoryInfo plane_binds[4]{};
  for (uint32_t i = 0; i < count; ++i) {
    VkMemoryFdPropertiesKHR fp{VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR}; r = vkGetMemoryFdPropertiesKHR(d->device, VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT, a->planes[i].fd, &fp);
    if (r != VK_SUCCESS) return fail(r);
    VkImagePlaneMemoryRequirementsInfo pr{VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO}; pr.planeAspect = planes[i];
    VkImageMemoryRequirementsInfo2 ri{VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2}; ri.image = t->image; ri.pNext = disjoint ? &pr : nullptr;
    VkMemoryRequirements2 req{VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2}; vkGetImageMemoryRequirements2(d->device, &ri, &req);
    uint32_t bits = req.memoryRequirements.memoryTypeBits & fp.memoryTypeBits; if (!bits) return fail(VK_ERROR_FORMAT_NOT_SUPPORTED);
    int dup = fcntl(a->planes[i].fd, F_DUPFD_CLOEXEC, 0); if (dup < 0) return fail(VK_ERROR_TOO_MANY_OBJECTS);
    // wlroots also passes a dedicated-allocation record for disjoint images; the spec forbids that (VUID-VkMemoryDedicatedAllocateInfo-image-01797).
    VkMemoryDedicatedAllocateInfo dedicated{VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO}; dedicated.image = t->image;
    VkImportMemoryFdInfoKHR import{VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR}; import.handleType = VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT; import.fd = dup; import.pNext = disjoint ? nullptr : &dedicated;
    VkMemoryAllocateInfo mai{VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO}; mai.pNext = &import; mai.allocationSize = req.memoryRequirements.size; mai.memoryTypeIndex = static_cast<uint32_t>(__builtin_ctz(bits));
    r = vkAllocateMemory(d->device, &mai, nullptr, &t->memories[i]); if (r != VK_SUCCESS) { close(dup); return fail(r); }
    t->memory_count = i + 1; binds[i] = {VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO}; binds[i].image = t->image; binds[i].memory = t->memories[i];
    if (disjoint) { plane_binds[i] = {VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO}; plane_binds[i].planeAspect = planes[i]; binds[i].pNext = &plane_binds[i]; }
  }
  r = vkBindImageMemory2(d->device, count, binds); if (r != VK_SUCCESS) return fail(r);
  VkImageViewCreateInfo vi{VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO}; vi.image = t->image; vi.viewType = VK_IMAGE_VIEW_TYPE_2D; vi.format = format; vi.subresourceRange = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1};
  r = vkCreateImageView(d->device, &vi, nullptr, &t->view); if (r != VK_SUCCESS) return fail(r);
  // The image's one layout transition, first in submission order: the pending upload commands run before
  // anything recorded after this import. Every later acquire and release keeps GENERAL. A render target's
  // contents may be discarded here (wlroots transitions it from UNDEFINED); a client texture's must survive.
  // ponytail: a client texture with a compressed (non-linear) modifier keeps GENERAL, which the validation
  // layer reports as a layout mismatch; VK_EXT_external_memory_acquire_unmodified is the clean fix.
  std::lock_guard<std::recursive_mutex> lock(d->qlock);
  Command *u = upload_cmd(d); if (!u) { vkDestroyImageView(d->device, t->view, nullptr); return fail(VK_ERROR_OUT_OF_HOST_MEMORY); }
  VkImageLayout old = render || a->modifier == 0 ? VK_IMAGE_LAYOUT_UNDEFINED : VK_IMAGE_LAYOUT_GENERAL;
  VkImageMemoryBarrier2 first[2] = {external_barrier(d, t->image, true, old), external_barrier(d, t->image, false, VK_IMAGE_LAYOUT_GENERAL)};
  for (VkImageMemoryBarrier2 &b : first) { VkDependencyInfo info{VK_STRUCTURE_TYPE_DEPENDENCY_INFO}; info.imageMemoryBarrierCount = 1; info.pImageMemoryBarriers = &b; vkCmdPipelineBarrier2(u->cb, &info); }
  t->serial = ++d->external_serial; t->next_external = d->externals; d->externals = t;
  *out = t; return VK_SUCCESS;
}
// wlroots vulkan_sync_render_pass_release: a binary semaphore signalled after everything submitted so far,
// exported as a sync_file (vkGetSemaphoreFdKHR resets it).
int32_t rv_sync_file_export(void *device, int32_t *fd) {
  auto *d = static_cast<Device *>(device); if (!d->sync_export) return VK_ERROR_EXTENSION_NOT_PRESENT;
  VkExportSemaphoreCreateInfo ei{VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO}; ei.handleTypes = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT;
  VkSemaphoreCreateInfo si{VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO}; si.pNext = &ei; VkSemaphore sem{}; VkResult r = vkCreateSemaphore(d->device, &si, nullptr, &sem); if (r != VK_SUCCESS) return r;
  std::lock_guard<std::recursive_mutex> lock(d->qlock); uint64_t tick = 0;
  r = submit_locked(d, nullptr, nullptr, 0, VK_NULL_HANDLE, sem, &tick);
  if (r == VK_SUCCESS) { VkSemaphoreGetFdInfoKHR fi{VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR}; fi.semaphore = sem; fi.handleType = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT; r = vkGetSemaphoreFdKHR(d->device, &fi, fd); }
  defer(d, destroy_semaphore_now, reinterpret_cast<void *>(sem)); return r;
}
int32_t rv_sync_file_wait(int32_t fd, int32_t timeout_ms, uint8_t *ready) {
  // ponytail: EINTR restarts the full timeout; track the deadline if a signal-heavy host needs exact waits.
  pollfd p{fd, POLLIN, 0}; int r; do r = poll(&p, 1, timeout_ms); while (r < 0 && errno == EINTR);
  if (r < 0) return VK_ERROR_UNKNOWN;
  *ready = r > 0; return VK_SUCCESS;
}
// wlroots render_pass_wait_sync_file: a temporary SYNC_FD import gates the next submit. Without semaphore
// import the CPU waits instead. Takes the fd in every case.
int32_t rv_sync_file_import(void *device, int32_t fd) {
  auto *d = static_cast<Device *>(device);
  if (!d->sync_import) { uint8_t ready = 0; int32_t r = rv_sync_file_wait(fd, -1, &ready); close(fd); return r; }
  VkSemaphoreCreateInfo si{VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO}; VkSemaphore sem{}; VkResult r = vkCreateSemaphore(d->device, &si, nullptr, &sem);
  if (r != VK_SUCCESS) { close(fd); return r; }
  VkImportSemaphoreFdInfoKHR ii{VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR}; ii.semaphore = sem; ii.flags = VK_SEMAPHORE_IMPORT_TEMPORARY_BIT; ii.handleType = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT; ii.fd = fd;
  r = vkImportSemaphoreFdKHR(d->device, &ii); if (r != VK_SUCCESS) { close(fd); vkDestroySemaphore(d->device, sem, nullptr); return r; }
  std::lock_guard<std::recursive_mutex> lock(d->qlock); uint64_t tick = 0;
  if (d->wait_count == 8) { r = submit_locked(d, nullptr, nullptr, 0, VK_NULL_HANDLE, VK_NULL_HANDLE, &tick); if (r != VK_SUCCESS) { vkDestroySemaphore(d->device, sem, nullptr); return r; } }
  d->waits[d->wait_count++] = sem; return VK_SUCCESS;
}
// wlroots render/dmabuf_linux.c: implicit-sync interop for consumers that do not take explicit fences.
int32_t rv_dmabuf_sync_file_export(int32_t dmabuf, bool write, int32_t *fd) {
  dma_buf_export_sync_file data{static_cast<__u32>(write ? DMA_BUF_SYNC_WRITE : DMA_BUF_SYNC_READ), -1};
  if (rv_ioctl(dmabuf, DMA_BUF_IOCTL_EXPORT_SYNC_FILE, &data)) return errno == ENOTTY || errno == EINVAL ? VK_ERROR_FORMAT_NOT_SUPPORTED : VK_ERROR_UNKNOWN;
  *fd = data.fd; return VK_SUCCESS;
}
int32_t rv_dmabuf_sync_file_import(int32_t dmabuf, int32_t sync, bool write) {
  dma_buf_import_sync_file data{static_cast<__u32>(write ? DMA_BUF_SYNC_WRITE : DMA_BUF_SYNC_READ), sync};
  if (rv_ioctl(dmabuf, DMA_BUF_IOCTL_IMPORT_SYNC_FILE, &data)) return errno == ENOTTY || errno == EINVAL ? VK_ERROR_FORMAT_NOT_SUPPORTED : VK_ERROR_UNKNOWN;
  return VK_SUCCESS;
}
#endif
struct rv_geometry { int32_t kind; void *buffer; uint64_t offset, stride; uint32_t count; uint8_t opaque, duplicate; };
struct rv_accel_sizes_t { uint64_t structure, build_scratch, update_scratch; };
static VkDeviceAddress rv_buffer_address(Device *d, VkBuffer buffer) {
  VkBufferDeviceAddressInfo i{VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO}; i.buffer=buffer; return vkGetBufferDeviceAddress(d->device,&i);
}
static VkDeviceAddress rv_accel_address(Accel *a) {
  VkAccelerationStructureDeviceAddressInfoKHR i{VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR}; i.accelerationStructure=a->handle;
  return vkGetAccelerationStructureDeviceAddressKHR(a->device->device,&i);
}
static void rv_set_geometries(Accel *a, const rv_geometry *geometries) {
  for(uint32_t i=0;i<a->geometry_count;++i){
    auto *b=static_cast<Buffer *>(geometries[i].buffer);VkDeviceAddress address=rv_buffer_address(a->device,b->buffer)+geometries[i].offset;
    auto &out=a->geometries[i];out={VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR};
    if(geometries[i].kind==0){
      out.geometryType=VK_GEOMETRY_TYPE_TRIANGLES_KHR;out.flags=VK_GEOMETRY_OPAQUE_BIT_KHR;
      out.geometry.triangles={VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR};
      out.geometry.triangles.vertexFormat=VK_FORMAT_R32G32B32_SFLOAT;out.geometry.triangles.vertexData.deviceAddress=address;
      out.geometry.triangles.vertexStride=geometries[i].stride;out.geometry.triangles.maxVertex=geometries[i].count?geometries[i].count-1:0;
      out.geometry.triangles.indexType=VK_INDEX_TYPE_NONE_KHR;a->primitives[i]=geometries[i].count/3;
    }else{
      out.geometryType=VK_GEOMETRY_TYPE_AABBS_KHR;out.flags=(geometries[i].opaque?VK_GEOMETRY_OPAQUE_BIT_KHR:0)|
        (geometries[i].duplicate?0:VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR);
      out.geometry.aabbs={VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR};
      out.geometry.aabbs.data.deviceAddress=address;out.geometry.aabbs.stride=geometries[i].stride;a->primitives[i]=geometries[i].count;
    }
  }
}
static VkResult rv_accel_storage(Accel *a) {
  Device *d=a->device;VkBufferCreateInfo bi{VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO};bi.size=a->structure_size;
  bi.usage=VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR|VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT;
  VmaAllocationCreateInfo ai{};ai.usage=VMA_MEMORY_USAGE_AUTO_PREFER_DEVICE;
  VkResult r=vmaCreateBuffer(d->allocator,&bi,&ai,&a->buffer,&a->allocation,nullptr);if(r!=VK_SUCCESS)return r;
  d->held.fetch_add(static_cast<int64_t>(a->structure_size),std::memory_order_relaxed);
  VkAccelerationStructureCreateInfoKHR ci{VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR};ci.buffer=a->buffer;ci.size=a->structure_size;ci.type=a->level;
  r=vkCreateAccelerationStructureKHR(d->device,&ci,nullptr,&a->handle);
  if(r!=VK_SUCCESS){d->held.fetch_sub(static_cast<int64_t>(a->structure_size),std::memory_order_relaxed);vmaDestroyBuffer(d->allocator,a->buffer,a->allocation);}return r;
}
static void rv_build_info(Accel *a,VkAccelerationStructureBuildGeometryInfoKHR *info,VkAccelerationStructureGeometryKHR *instance) {
  *info={VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR};info->type=a->level;info->flags=a->flags;
  if(a->level==VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR){
    *instance={VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR};instance->geometryType=VK_GEOMETRY_TYPE_INSTANCES_KHR;
    instance->geometry.instances={VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR};info->geometryCount=1;info->pGeometries=instance;
  }else{info->geometryCount=a->geometry_count;info->pGeometries=a->geometries;}
}
static void rv_measure(Accel *a) {
  VkAccelerationStructureBuildGeometryInfoKHR info{};VkAccelerationStructureGeometryKHR instance{};rv_build_info(a,&info,&instance);
  VkAccelerationStructureBuildSizesInfoKHR sizes{VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR};
  uint32_t n=a->instance_count;vkGetAccelerationStructureBuildSizesKHR(a->device->device,VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR,&info,
    a->level==VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR?&n:a->primitives,&sizes);
  a->structure_size=sizes.accelerationStructureSize;a->child_scratch=0;
  if(a->build_children)for(uint32_t i=0;i<a->structure_count;++i){auto *child=static_cast<Accel *>(a->structures[i]);a->child_scratch+=(child->build_scratch+255)&~VkDeviceSize(255);}
  a->build_scratch=sizes.buildScratchSize+128+a->child_scratch;a->update_scratch=sizes.updateScratchSize+128;
}
int32_t rv_accel_create_blas(void *device,const rv_geometry *geometries,uint32_t count,bool refit,void **out) {
  auto *a=new(std::nothrow) Accel{};if(!a)return VK_ERROR_OUT_OF_HOST_MEMORY;a->device=static_cast<Device *>(device);
  a->level=VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR;a->flags=refit?
    VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR|VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR:
    VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR|VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR;
  a->geometry_count=count;a->geometries=new(std::nothrow) VkAccelerationStructureGeometryKHR[count?count:1]{};
  a->primitives=new(std::nothrow) uint32_t[count?count:1]{};if(!a->geometries||!a->primitives){delete[]a->geometries;delete[]a->primitives;delete a;return VK_ERROR_OUT_OF_HOST_MEMORY;}
  rv_set_geometries(a,geometries);rv_measure(a);VkResult r=rv_accel_storage(a);
  if(r!=VK_SUCCESS){delete[]a->geometries;delete[]a->primitives;delete a;return r;}*out=a;return VK_SUCCESS;
}
int32_t rv_accel_create_tlas(void *device,void *instances,uint64_t offset,uint32_t count,void *const *structures,uint32_t structure_count,bool children,void **out) {
  auto *a=new(std::nothrow) Accel{};if(!a)return VK_ERROR_OUT_OF_HOST_MEMORY;a->device=static_cast<Device *>(device);
  a->level=VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR;a->flags=VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR;
  a->instances=static_cast<Buffer *>(instances);a->instances_offset=offset;a->instance_count=count;a->structure_count=structure_count;a->build_children=children;
  a->structures=new(std::nothrow) void*[structure_count?structure_count:1];if(!a->structures){delete a;return VK_ERROR_OUT_OF_HOST_MEMORY;}
  for(uint32_t i=0;i<structure_count;++i)a->structures[i]=structures[i];rv_measure(a);VkResult r=rv_accel_storage(a);
  if(r!=VK_SUCCESS){delete[]a->structures;delete a;return r;}*out=a;return VK_SUCCESS;
}
int32_t rv_accel_create_sized(void *device,uint64_t size,void *template_,void **out) {
  auto *t=static_cast<Accel *>(template_);auto *a=new(std::nothrow) Accel{};if(!a)return VK_ERROR_OUT_OF_HOST_MEMORY;
  a->device=static_cast<Device *>(device);a->level=t->level;a->structure_size=size;VkResult r=rv_accel_storage(a);if(r!=VK_SUCCESS){delete a;return r;}*out=a;return VK_SUCCESS;
}
void rv_accel_sizes(void *accel,rv_accel_sizes_t *out) { auto *a=static_cast<Accel *>(accel);*out={a->structure_size,a->build_scratch,a->update_scratch}; }
static void destroy_accel_now(Device *d,void *payload) {
  auto *a=static_cast<Accel *>(payload);if(a->wrap)destroy_accel_now(d,a->wrap);vkDestroyAccelerationStructureKHR(d->device,a->handle,nullptr);
  vmaDestroyBuffer(d->allocator,a->buffer,a->allocation);delete[]a->geometries;delete[]a->primitives;delete[]a->structures;delete a;
}
void rv_accel_destroy(void *accel) {
  auto *a=static_cast<Accel *>(accel);a->device->held.fetch_sub(static_cast<int64_t>(a->structure_size),std::memory_order_relaxed);
  defer(a->device,destroy_accel_now,a);
}
static VkResult rv_wrap_primitive(Command *c,Accel *a,VkAccelerationStructureKHR *out) {
  Device *d=a->device;if(!a->wrap){auto *w=new(std::nothrow) Accel{};if(!w)return VK_ERROR_OUT_OF_HOST_MEMORY;
    w->device=d;w->level=VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR;w->flags=VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR;w->instance_count=1;
    rv_measure(w);VkResult r=rv_accel_storage(w);if(r!=VK_SUCCESS){delete w;return r;}a->wrap=w;}
  Accel *w=a->wrap;VkAccelerationStructureInstanceKHR instance{};instance.transform.matrix[0][0]=instance.transform.matrix[1][1]=instance.transform.matrix[2][2]=1.f;
  instance.mask=0xff;instance.accelerationStructureReference=rv_accel_address(a);
  VkBuffer chunk{},scratch{};VkDeviceSize at{},scratch_at{};char *room=arena(c,sizeof instance,16,&chunk,&at);if(!room)return VK_ERROR_OUT_OF_DEVICE_MEMORY;
  memcpy(room,&instance,sizeof instance);if(!arena(c,w->build_scratch,256,&scratch,&scratch_at))return VK_ERROR_OUT_OF_DEVICE_MEMORY;
  VkAccelerationStructureBuildGeometryInfoKHR info{};VkAccelerationStructureGeometryKHR geometry{};rv_build_info(w,&info,&geometry);
  geometry.geometry.instances.data.deviceAddress=rv_buffer_address(d,chunk)+at;info.mode=VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR;info.dstAccelerationStructure=w->handle;
  info.scratchData.deviceAddress=(rv_buffer_address(d,scratch)+scratch_at+127)&~VkDeviceAddress(127);
  VkAccelerationStructureBuildRangeInfoKHR range{};range.primitiveCount=1;const VkAccelerationStructureBuildRangeInfoKHR *ranges=&range;
  full_barrier(c->cb);vkCmdBuildAccelerationStructuresKHR(c->cb,1,&info,&ranges);full_barrier(c->cb);*out=w->handle;return VK_SUCCESS;
}
int32_t rv_cmd_set_accel(void *commands,int32_t index,void *accel) {
  auto *c=static_cast<Command *>(commands);auto *a=static_cast<Accel *>(accel);if(index<0||index>=static_cast<int>(MAX_SLOTS))return VK_ERROR_TOO_MANY_OBJECTS;
  VkAccelerationStructureKHR h=a->handle;if(a->level==VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR){VkResult r=rv_wrap_primitive(c,a,&h);if(r!=VK_SUCCESS)return r;}
  c->slots[index]={};c->slots[index].kind=4;c->slots[index].accel=h;c->dirty=true;return VK_SUCCESS;
}
static VkResult rv_native_instances(Command *c,Accel *a,VkDeviceAddress *address) {
  VkBuffer chunk{};VkDeviceSize at{};VkDeviceSize bytes=static_cast<VkDeviceSize>(a->instance_count)*sizeof(VkAccelerationStructureInstanceKHR);
  char *room=arena(c,bytes?bytes:64,16,&chunk,&at);if(!room)return VK_ERROR_OUT_OF_DEVICE_MEMORY;
  VkDeviceSize records=static_cast<VkDeviceSize>(a->instance_count)*68;if(records>SIZE_MAX)return VK_ERROR_OUT_OF_HOST_MEMORY;
  char *host=static_cast<char *>(malloc(records?static_cast<size_t>(records):1));if(!host)return VK_ERROR_OUT_OF_HOST_MEMORY;
  VkResult r=staged_read(a->instances,a->instances_offset,records,host);if(r!=VK_SUCCESS){free(host);return r;}
  const char *from=host;
  for(uint32_t i=0;i<a->instance_count;++i,from+=68){const float *m=reinterpret_cast<const float *>(from);uint32_t user_id,mask,index,prototype;
    memcpy(&user_id,from+48,4);memcpy(&mask,from+52,4);memcpy(&index,from+56,4);memcpy(&prototype,from+60,4);
    if(index>=a->structure_count){free(host);return VK_ERROR_VALIDATION_FAILED_EXT;}
    VkAccelerationStructureInstanceKHR out{};out.transform.matrix[0][0]=m[0];out.transform.matrix[0][1]=m[3];out.transform.matrix[0][2]=m[6];out.transform.matrix[0][3]=m[9];
    out.transform.matrix[1][0]=m[1];out.transform.matrix[1][1]=m[4];out.transform.matrix[1][2]=m[7];out.transform.matrix[1][3]=m[10];
    out.transform.matrix[2][0]=m[2];out.transform.matrix[2][1]=m[5];out.transform.matrix[2][2]=m[8];out.transform.matrix[2][3]=m[11];
    out.instanceCustomIndex=user_id&0xffffff;out.mask=mask&0xff;out.instanceShaderBindingTableRecordOffset=a->build_children?prototype&0xffffff:0;
    out.accelerationStructureReference=rv_accel_address(static_cast<Accel *>(a->structures[index]));memcpy(room+i*sizeof out,&out,sizeof out);}
  free(host);*address=rv_buffer_address(a->device,chunk)+at;return VK_SUCCESS;
}
static VkResult rv_record_build(Command *c,Accel *a,Buffer *scratch,uint64_t offset,bool update) {
  if(a->build_children){uint64_t child_offset=offset;for(uint32_t i=0;i<a->structure_count;++i){auto *child=static_cast<Accel *>(a->structures[i]);
    VkResult r=rv_record_build(c,child,scratch,child_offset,false);if(r!=VK_SUCCESS)return r;child_offset+=(child->build_scratch+255)&~uint64_t(255);}offset+=a->child_scratch;}
  VkAccelerationStructureBuildGeometryInfoKHR info{};VkAccelerationStructureGeometryKHR instance{};rv_build_info(a,&info,&instance);
  VkAccelerationStructureBuildRangeInfoKHR stack_ranges[8]{};
  VkAccelerationStructureBuildRangeInfoKHR *ranges=stack_ranges;
  if(info.geometryCount>8){ranges=static_cast<VkAccelerationStructureBuildRangeInfoKHR *>(calloc(info.geometryCount,sizeof(*ranges)));if(!ranges)return VK_ERROR_OUT_OF_HOST_MEMORY;}
  VkResult r=VK_SUCCESS;
  if(a->level==VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR){VkDeviceAddress data=0;r=rv_native_instances(c,a,&data);
    if(r==VK_SUCCESS){instance.geometry.instances.data.deviceAddress=data;ranges[0].primitiveCount=a->instance_count;}}
  else for(uint32_t i=0;i<info.geometryCount;++i)ranges[i].primitiveCount=a->primitives[i];
  if(r==VK_SUCCESS){
    VkDeviceAddress scratch_at=rv_buffer_address(a->device,scratch->buffer)+offset;info.mode=update?VK_BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR:VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR;
    info.srcAccelerationStructure=update?a->handle:VK_NULL_HANDLE;info.dstAccelerationStructure=a->handle;info.scratchData.deviceAddress=(scratch_at+127)&~VkDeviceAddress(127);
    const VkAccelerationStructureBuildRangeInfoKHR *ptr=ranges;vkCmdBuildAccelerationStructuresKHR(c->cb,1,&info,&ptr);full_barrier(c->cb);
  }
  if(ranges!=stack_ranges)free(ranges);return r;
}
int32_t rv_accel_build(void *commands,void *accel,void *scratch,uint64_t offset) { return rv_record_build(static_cast<Command *>(commands),static_cast<Accel *>(accel),static_cast<Buffer *>(scratch),offset,false); }
int32_t rv_accel_refit(void *commands,void *accel,const rv_geometry *geometries,uint32_t count,void *scratch,uint64_t offset) {
  auto *a=static_cast<Accel *>(accel);if(count!=a->geometry_count)return VK_ERROR_VALIDATION_FAILED_EXT;rv_set_geometries(a,geometries);
  return rv_record_build(static_cast<Command *>(commands),a,static_cast<Buffer *>(scratch),offset,true);
}
void rv_accel_compact(void *commands,void *source,void *destination) {
  auto *c=static_cast<Command *>(commands);VkCopyAccelerationStructureInfoKHR i{VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR};
  i.src=static_cast<Accel *>(source)->handle;i.dst=static_cast<Accel *>(destination)->handle;i.mode=VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR;
  vkCmdCopyAccelerationStructureKHR(c->cb,&i);full_barrier(c->cb);
}
static void destroy_query_pool_now(Device *d,void *payload) { vkDestroyQueryPool(d->device,reinterpret_cast<VkQueryPool>(payload),nullptr); }
int32_t rv_accel_write_compacted(void *commands,void *accel,void *buffer,uint64_t offset,uint8_t uint64) {
  auto *c=static_cast<Command *>(commands);auto *a=static_cast<Accel *>(accel);VkQueryPoolCreateInfo qi{VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO};
  qi.queryType=VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR;qi.queryCount=1;VkQueryPool pool{};VkResult r=vkCreateQueryPool(a->device->device,&qi,nullptr,&pool);if(r!=VK_SUCCESS)return r;
  vkCmdResetQueryPool(c->cb,pool,0,1);vkCmdWriteAccelerationStructuresPropertiesKHR(c->cb,1,&a->handle,VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR,pool,0);
  full_barrier(c->cb);vkCmdCopyQueryPoolResults(c->cb,pool,0,1,static_cast<Buffer *>(buffer)->buffer,offset,uint64?8:4,VK_QUERY_RESULT_WAIT_BIT|(uint64?VK_QUERY_RESULT_64_BIT:0));full_barrier(c->cb);
  defer(a->device,destroy_query_pool_now,pool);return VK_SUCCESS;
}
}
