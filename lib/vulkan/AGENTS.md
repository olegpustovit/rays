# lib/vulkan rules

The Vulkan bridge: `vulkan_bridge.c` and the externals in `vulkan.ml`. Only `lib/ogpu_vulkan` uses it.

- It is shaped like OGPU's driver records, one C operation per driver operation. It is not a binding
  of the Vulkan API and there is no generator, unlike `lib/metal`: Vulkan's calls take nested structs,
  so one stub per call would build those structs from OCaml on hot paths for no gain. volk already
  generates the loader. Add an operation when `lib/ogpu_vulkan` has a caller for it.
- A handle is a C pointer as an OCaml int; a negative int is a VkResult. One function in
  `lib/ogpu_vulkan` maps a VkResult to `Ogpu.Error`.
- Hazards: a global barrier between dispatches and at each encoder's end. Every image stays in
  `VK_IMAGE_LAYOUT_GENERAL`. Each simplification has a `ponytail:` comment naming its ceiling.
- Uploads record into one command buffer that the next submit runs first; destruction is deferred
  past the next submit. Host reads go through the dedicated transfer queue and work from any domain.
- Every OGPU buffer is device-local (Shared too: a kernel reading host memory crosses PCIe, 40x slower on
  isogrid_city). Host writes stage into the next submit, host reads use the transfer queue, and a mapped view is
  `calloc`'d on its first ask (`mapped_view`, `invalidate_mapped`) and dropped on release or destroy: a buffer only
  the device touches has no host copy.
- Never hold the runtime lock across a wait. `qlock` guards the main queue and pending uploads,
  `xlock` the transfer queue.
- Tests run with `RAYS_VULKAN_VALIDATION=fatal` (`rw` sets it): the Khronos layer with synchronization
  validation, and any message ends the process. Never suppress one.
- On a machine where the process is elevated, the Vulkan loader ignores `VK_ADD_LAYER_PATH`: register
  the layer under `HKLM\SOFTWARE\Khronos\Vulkan\ExplicitLayers`.
