# PXUI

A small, fast, native UI engine in Rust. Plain events go in, a draw list comes out; one instanced
pipeline paints it on Metal (Apple Silicon) or Vulkan (Windows, Linux). It is the UI of
Rays, which embeds this repository as a submodule at `pxui/`, and it
builds and tests on its own.

Dependencies are std and `cc`; `pxui_text` alone adds cosmic-text for shaping and glyph rasterisation.

## Crates

| Crate | Role |
| --- | --- |
| `pxui` | ids, layout, widgets, theme, draw list. Knows nothing about the GPU or the window system. |
| `pxui_text` | cosmic-text, shaping cache, glyph atlases, the bundled Pragmasevka font |
| `pxui_render` | the painter: one instanced pipeline, atlas textures, draws into a pass the caller opened |
| `pxui_sdl` | SDL3 window host: lifecycle, event pump, surface token |
| `pxui_wayland` | Wayland client window host (Linux) |
| `pxui_atspi` | AT-SPI2 accessibility bridge (Linux): serves `pxui::a11y`'s tree over libdbus-1 |
| `pxui_gallery` | the kit gallery app (`cargo run -p pxui_gallery`): every widget, both themes, goldens |
| `ogpu_core`, `ogpu_mock` | portable GPU API and its mock |
| `ogpu_metal`, `metal_sys` | Metal backend and its Objective-C++ shim |
| `ogpu_vulkan`, `vulkan_sys` | Vulkan backend and its C++ shim (vendored volk and VMA) |
| `native_layer_token_sys` | the SDL-free handle between a window and the GPU |
| `input` | key, event and frame-input types |
| `sdl3`, `sdl3_sys` | SDL3 binding |
| `mem_sys`, `cfmt` | counting allocator (what the zero-allocation tests assert against), C-style formatting |
| `tools/t` | `cargo t`, the deadline-enforced test runner |
| `tools/gate` | the dependency and shape gate (`cargo run -p gate -- deps`, `-- shape`) |

Hosts expose the same few functions and there is no `Host` trait: an app links the one it wants.

## Host loop

The shape of an app (names settle with the engine rewrite; `crates/pxui_render/tests/gpu_pxui_render.rs`
and `crates/pxui/tests/gpu_pxui_parity.rs` hold the compiling version):

```rust
let mut win = pxui_sdl::Window::open("demo", 900, 600)?;          // or pxui_wayland
let gpu = ogpu_metal::device()?;                                  // or ogpu_vulkan
let mut ui = pxui::Ui::new(/* theme */);
let mut painter = pxui_render::Painter::new(&gpu, format, depth, 1, mask_size, color_size)?;
let mut events = Vec::with_capacity(256);
while win.pump(&mut events) {
    let frame = /* events, win.size(), win.scale(), win.time() */;
    let out = ui.frame(&frame, &mut |ui| app(ui));                // out.draw: layers, out.platform: cursor, IME, clipboard, repaint time
    painter.begin_frame();
    /* open a render pass on the window's target */
    painter.draw(&pass, size_px, scale, instances, breaks)?;
    /* present, then apply out.platform */
}
```

Input is data in, output is data out: there are no callbacks into the platform.

## Working here

```sh
cargo check -p pxui                 # inner loop
cargo t -p pxui [filter]            # never raw `cargo test`: cases die at 5 s, runs at 60 s
cargo run -p gate -- deps           # dependency boundaries (`shape` checks code shape)
tools/validate.sh                   # all of it on macOS; --windows / --linux print the commands for those machines
```

Validation is local; there is no hosted CI.

macOS needs `brew install pkgconf sdl3`. Linux Vulkan setup is in [docs/vulkan-linux.md](docs/vulkan-linux.md).
Contributor rules are in [AGENTS.md](AGENTS.md). [MIT license](LICENSE).
