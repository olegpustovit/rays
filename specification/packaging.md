# Packaging native Rays

Rays is one Cargo workspace. `rays_bin` supplies the executable; workspace crates
provide the language, editor, runtime and native GPU stack. Supported native
platforms are Apple Silicon with Metal and Windows with Vulkan. Web export is a
separate 2D product target. [Packaging instructions](../packaging/README.md) list
current platform commands and prerequisites.

SDL3 core, image and mixer are system dependencies discovered through
pkg-config or the documented platform paths. The owning sys crates compile
required ABI code with `cc`; metadata, headers and runtime versions must agree.
Missing dependencies fail the build or return typed startup errors.

Metal, QuartzCore, CoreGraphics, IOSurface and Foundation come from the macOS SDK.
Metal ABI bindings are checked in under `crates/metal_sys/src/`; no external
language generator is required at build time. Windows Vulkan loaders and memory
allocation code live under the owning sys crate. Native resources and every SDL
operation stay on the initial thread.

Release validation must inspect the executable and its linked native artifacts,
package assets, notices and generated-source provenance. A successful developer
build does not establish relocatable-package correctness. Follow
[licenses](licenses.md) and retain each external dependency's license and notices.
