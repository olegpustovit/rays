# GPU stack license and generated-source provenance

Rays source and its handwritten Rust/C/Objective-C++ bindings are released
under the repository MIT license. This file records the migration-specific
dependency and generated-source boundary; it is not a substitute for the final
release legal review.

| Surface | Distributed material / provenance | External obligation |
| --- | --- | --- |
| SDL3 core/image/ttf/mixer bindings | Rays-written bindings plus checked-in inventories, ABI facts and provenance hashes. Generated modules identify generator version and pinned headers. | SDL projects use the zlib license. System/shared libraries and their notices remain external dependencies; Rays does not vendor them here. |
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

Vulkan compositor hooks (Linux): the DRM format table, dmabuf import, the format/modifier query and the
sync_file handling are ported from [wlroots](https://gitlab.freedesktop.org/wlroots/wlroots) 0.20.2
(commit `d783533489e1f75d6886c2ab5c5960090ef268f8`), MIT. Ported files: `render/vulkan/pixel_format.c`
(format table, modifier usage query; in `crates/ogpu_core/src/dmabuf.rs` and `crates/vulkan_sys/src/vulkan_sys.cpp`),
`render/vulkan/texture.c` (`vulkan_import_dmabuf`, disjoint planes), `render/vulkan/vulkan.c` (extension and
SYNC_FD semaphore probing), `render/vulkan/renderer.c` and `render/vulkan/pass.c` (sync_file export and import,
foreign queue ownership barriers) and `render/dmabuf_linux.c` (dmabuf sync_file ioctls), all into
`crates/vulkan_sys/src/vulkan_sys.cpp`. Its notice:

> Copyright (c) 2017, 2018 Drew DeVault
> Copyright (c) 2014 Jari Vetoniemi
> Copyright (c) 2023 The wlroots contributors
>
> Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
> associated documentation files (the "Software"), to deal in the Software without restriction, including
> without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
> copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the
> following conditions:
>
> The above copyright notice and this permission notice shall be included in all copies or substantial
> portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
> LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO
> EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
> AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
> USE OR OTHER DEALINGS IN THE SOFTWARE.
