# Open issues

Left open by the PXUI stable ABI work (plan: artifact `5y36bm8pvvg9`, section 15; merged 2026-11-12, PXUI `b37e8df`).

- A7 plan targets not met; gates are at measured median + headroom, with the targets kept beside them in `abi_bench.rs` and `pxui/docs/abi-baseline.md` ("Deferred performance work"): static +0.5% to +3.1% (target 1%, gate 6%); dynamic +1.5% to +4.6% (target 3%, gate 6%); dynamic 5.9-7.0 us per 1000 nodes (target 5, gate 8.7); dynamic first frame within 8 ms of static (target 2); dynamic memory: private +272 KiB, shared code pages up to +1392 KiB (second OGPU copy, by design).
- Known failure, budget unchanged: `rays_editor::window_graph_costs` (`RAYS_SLOW=1`): an edit allocates 754 against a budget of 700, on all three platforms.
- Seen once under load, passed on reruns: `rays_editor::native_holder_frames` (Mac), `pxui_core::layout_scaling` (Windows).
- Sketch text (`crates/rays/src/font.rs`) keeps its own per-thread text engine, as before this work.
- The engine's picker and inspector header allocate about 14 times per frame while open (engine behaviour; the wrapper adds one).
- Not run: AT-SPI client walk, a real portal file dialog, the slow renderer suite, `abi_bench` timing on Windows/WSL, Asahi and native-Linux NVIDIA.
- `painter: dirty ranges ... instance count` after several builds before a first draw was reported once and could not be reproduced; `shared_skipped_draws` covers it.
- Dynamic `libpxui` packaging unverified: Linux (`$ORIGIN` rpaths, `make install` to `$(PREFIX)/lib`) and Windows (`pxui.dll` beside the exe; no `make lib`/install path) were not run; macOS signing with a real identity was not run in the agent session (keychain locked: `errSecInternalComponent`); the app is not notarized.
