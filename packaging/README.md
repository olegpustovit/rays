# Native dependencies

Use Cargo and the toolchain in `rust-toolchain.toml`. SDL floors and tested releases live in
`pxui/packaging/sdl3.lock`; sdl3 sys build scripts enforce them. OCaml/opam packaging is retired.

macOS:

```sh
brew install pkgconf sdl3 sdl3_image sdl3_mixer
cargo build -p rays_bin
```

Windows: use MSYS2 bash with `C:/msys64/mingw64/bin` on PATH, SDL3 development packages and
a Vulkan SDK/driver. Build scripts accept `SDL3_DIR` or use pkg-config/MSYS2 discovery.
The linker config uses the pinned toolchain's lld; update its path in `.cargo/config.toml`
when changing toolchain/user installation. Native Vulkan sources and volk/VMA live in
`pxui/crates/open/vulkan_sys/`; conformance shaders live in `pxui/crates/open/ogpu_vulkan/tests/shaders/`.
Install glslangValidator and spirv-link for those tests. Run with `RAYS_VULKAN_VALIDATION=fatal`.

```sh
cargo check -p rays_bin
cargo t -p vulkan
cargo t -p ogpu_vulkan
```
