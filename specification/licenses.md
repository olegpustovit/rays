# GPU stack license and generated-source provenance

Rays source and its handwritten Rust/C/Objective-C++ bindings are released
under the repository MIT license. This file records the migration-specific
dependency and generated-source boundary; it is not a substitute for the final
release legal review.

| Surface | Distributed material / provenance | External obligation |
| --- | --- | --- |
| SDL3 core/image/mixer bindings | Rays-written bindings plus checked-in inventories, ABI facts and provenance hashes. Generated modules identify generator version and pinned headers. | SDL projects use the zlib license. System/shared libraries and their notices remain external dependencies; Rays does not vendor them here. |
| Metal binding | Rays-written safe/raw layers and typed Objective-C++ bridge. Checked-in Rust declarations and the owning sys crate bridge call the installed SDK, which checks native types at compile time. | Apple SDK headers/frameworks are system build inputs obtainable with the Command Line Tools and are not redistributed by Rays. Apple platform/tool terms apply to builders and shipped applications. The full IDE and offline shader toolchain are not required. |
| OGPU, OGPU Metal, scene execution, native runtime support, and low core | Original Rays Rust source under MIT. | No additional bundled third-party renderer implementation or shader binary is introduced by these libraries. |

Generated binding output is mechanical Rays source: retained enum constants
and typed direct calls. Metal declarations are checked in under `crates/metal_sys/src/`. It does not embed SDK header bodies,
vendor framework binaries, or invoke Python glue. Handwritten ownership,
lifetime, validation and callback policy remains distinguishable from generated
mechanics in the source tree.

External-tool geometry comparison utilities live outside this repository.
Final Phase 5 evidence must still inventory every installed native
artifact, copied asset, offline shader, license notice and generator version on
the release commit.

`runtime_license_manifest.json` is the machine-readable native-runtime inventory.
Release validation checks complete classifications, the MIT/zlib license and Apple
SDK provenance boundaries, and requires an explicit non-bundled declaration
for every staged surface. Final release artifact inspection remains separate.

Vulkan (Windows): `crates/vulkan_sys/vendor` holds two unmodified third-party files, both MIT.
[volk](https://github.com/zeux/volk) (`volk.c`, `volk.h`), copyright Arseny Kapoulkine, loads Vulkan
at run time. [Vulkan Memory Allocator](https://github.com/GPUOpen-LibrariesAndSDKs/VulkanMemoryAllocator)
v3.3.0 (`vk_mem_alloc.h`), copyright Advanced Micro Devices, suballocates. Their licence texts are in
the files themselves. The Vulkan headers and validation layers come from MSYS2 at build and test time and
are not distributed.
