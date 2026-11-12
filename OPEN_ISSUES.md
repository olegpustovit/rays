# Open issues

Left open by the PXUI stable ABI work (plan: artifact `5y36bm8pvvg9`, section 15; merged 2026-11-12, PXUI `b37e8df`).

- A7 frame-build targets met by the median on the pages of 100 nodes or more (static +0.6%/+0.7%, target 1%; dynamic +1.1%/+2.6%, target 3%), but one run moves by up to 0.8% on the loaded Mac, so those gates are the worst run + 25% (static 1.75%, dynamic 4.1%). Not met, gates at measured + headroom, targets beside them in `abi_bench.rs` and `pxui/docs/abi-baseline.md` ("Deferred performance work"): dynamic 4.4-5.6 us per 1000 nodes (target 5, gate 8.7); dynamic first frame within 8 ms of static (target 2); dynamic memory: private +272 KiB, shared code pages up to +1392 KiB (second OGPU copy, by design).
- Seen once under load, passed on reruns: `rays_editor::native_holder_frames` (Mac), `pxui_core::layout_scaling` (Windows).
- Sketch text (`crates/rays/src/font.rs`) keeps its own per-thread text engine, as before this work.
- The engine's picker and inspector header allocate about 14 times per frame while open (engine behaviour; the wrapper adds one).
- Not run: AT-SPI client walk, a real portal file dialog, the slow renderer suite, `abi_bench` timing on Windows/WSL, Asahi and native-Linux NVIDIA.
- `painter: dirty ranges ... instance count` after several builds before a first draw was reported once and could not be reproduced; `shared_skipped_draws` covers it.
- Dynamic `libpxui` packaging: run on Linux (WSL2) and Windows (`make install`, `$ORIGIN/../lib` and DLLs beside the exe, from another directory) and on macOS; macOS signing with a real identity was not run in the agent session (keychain locked: `errSecInternalComponent`); the app is not notarized. Linux needs a system SDL3 (`libSDL3*.so.0`) at run time; none is bundled.
- WSL2/lavapipe: `rays::window_sketch_retained_view` and `rays_bin::gpu_cli_render_canvas_fixed_clock_and_diagnostics` exceed the 5 s case limit under validation (pass with `RAYS_SLOW=1`).
