# Fast Rust iteration

Work from the repository root. Keep one warm `target/`; Cargo serializes compilation.
No opam, Dune or OCaml compiler is needed.

```sh
cargo check -p flow_ir                  # typecheck after an edit
cargo t -p flow_ir filter               # focused regression with deadlines
cargo t -p web_export --test e2e          # corpus boundary at a stable checkpoint
cargo run -p gate -- deps               # architecture boundary check
cargo run -p buildtime -- check         # explicit build-time qualification
cargo run -p buildtime -- profile       # app build, flow_ir touch and bounded corpus
RAYS_SLOW=1 cargo t --workspace         # complete native qualification
cargo t --all                          # optional 240-second sequential check, stop at first red
```

`cargo t` enforces a 5-second case deadline and 60-second test-run deadline. Its build has a
separate 900-second safety cutoff. It serializes GPU/window cases, prints slowest cases and
appends `target/t-times.tsv`. Use `RAYS_SLOW=1 cargo t -p <crate> <filter>` for explicit qualification.

Stage tests: focused cases after edits, touched crates and consumers at stable checkpoints,
affected native checks before commit. Save broad qualification for the final boundary.
Keep focused warm checks under 30 seconds. Complete qualification uses the existing
slow mode: 60 seconds per case and 600 seconds for the run. The optional `--all`
dispatcher retains its separate 240-second cap; this is not a release acceptance target.

The dev profile retains O1, no debug symbol payload, incremental compilation and 256 codegen units.
Existing Mac measurements: sketch checks took 123.8 seconds at O0 versus 19.1 seconds at O1;
a flow_ir edit rebuilt the app in 4.5 versus 5.2 seconds. Keep this measured tradeoff until a
same-machine replacement wins total edit/build/run time. Allocator counters alone use O3.
Changing profiles must report compile AND debug runtime. [Historical measurements](history/build-speed.md)
cover past machines and settings, not current defaults.

## libpxui and installing

Rays loads `libpxui` (the PXUI stable C ABI) at run time; nothing links it statically. Cargo does not order a
cdylib before a dependent's link, so build it first: `make lib` (release; `make lib PROFILE=debug` for debug)
puts `libpxui.dylib` and the `libpxui.1.dylib` link (Linux: `libpxui.so`, `libpxui.so.1`; Windows: `pxui.dll` and its
import library `libpxui.dll.a`) in `target/<profile>/`, and `make build` does that and builds `rays`. `cargo t` builds
it for you. The binaries and tests find it there without `DYLD_LIBRARY_PATH`/`LD_LIBRARY_PATH`: rpaths to
`target/<profile>` (macOS: `pxui/tools/cc-stack.sh`; Linux: `$ORIGIN` in `.cargo/config.toml`), and on Windows the dll
sits beside the exe (its MinGW runtime and SDL3 DLLs come from the MSYS2 `bin` on `PATH`). So `cargo build -p rays_bin`,
`cargo run` and `cargo check` then work; without the library a link fails with a hint. On Windows (MSYS2 UCRT64 shell
with `make`) the plain build is `make build PROFILE=debug`, the same command as on Linux. After a PXUI
change rerun `make lib` (cargo does not rebuild the library for `cargo run`).

`make install` (macOS) builds release, assembles `Rays.app` (`packaging/bundle-macos.sh`: binary, `libpxui`, SDL3 and
their non-system dylibs in `Contents/Frameworks`, rpath `@executable_path/../Frameworks`, Info.plist, icon), signs inner
dylibs then the app (hardened runtime; a timestamp for Developer ID) and copies it to `APPDIR` (default `/Applications`),
with a `rays` link in `PREFIX/bin` (default `~/.local`). `CODESIGN_IDENTITY` picks the identity; empty means
"Developer ID Application", else "Apple Development", else ad hoc (printed). The app is not notarized, so `spctl` rejects it.
Runtime assets (fonts, themes, shaders, nodes) are embedded in the binary: Resources holds only the icon.

`make install PREFIX=...` elsewhere: Linux puts `bin/rays` and `lib/libpxui.so.1` under `PREFIX` (rpath `$ORIGIN/../lib`,
nothing into the build tree; SDL3 stays a system library found through pkg-config at build time and `ld.so` at run time,
check with `ldd`). Windows (MSYS2) puts `rays.exe`, `pxui.dll` and every MSYS2 DLL they load (SDL3, SDL3_image, SDL3_mixer,
libstdc++, libgcc, libwinpthread, libiconv) in `PREFIX/bin`, a directory that runs from anywhere. Both were run on the
Windows (RTX 2080 Ti) and WSL2 (lavapipe) boxes.

Allocation counts cross the library boundary: `libpxui` carries its own copy of `mem_sys`'s allocator, so `rays_shell::host::create`
registers its `pxui_counter` export with `mem::set_peer`. `mem_sys::alloc_count`, `alloc_bytes_total`, `allocated_bytes`,
`local_alloc_count`, `local_reset` and `local_peak_bytes` then cover the whole process (the peak is the sum of the two peaks, an
upper bound); every budget and the memory ledger use them.

## The pxui submodule and validation

PXUI, `ogpu_*`, the window hosts, `sdl3` and `cargo t` itself live in the `pxui/` submodule
(`git@github.com:olegpustovit/pxui.git`). After a clone: `git submodule update --init`. `cargo t -p pxui_core`
and the other PXUI crates run from the Rays root (the alias builds `pxui/tools/t` into this `target/`).
To change PXUI: edit in `pxui/`, `pxui/tools/validate.sh` there, commit and push in `pxui/`, then commit
the pointer here; `cargo run -p gate -- pxui` refuses a pointer that is not on the remote.
There is no hosted CI: validation is local. macOS (Metal) runs the Rays commands above and
`pxui/tools/validate.sh`; the Windows box and WSL run the same commands, on a bundle-synced pair of
checkouts when they have no GitHub access (`git config submodule.pxui.url <path of the PXUI clone>`,
then `git -c protocol.file.allow=always submodule update --init`).

## OCaml comparison

Annotated tag `ocaml-reference` points to `e92f94f0`, the complete mixed checkout before removal.

```sh
git worktree add --detach ../rays-ocaml-reference ocaml-reference
```

Follow that commit's AGENTS.md to bootstrap opam and build `dune build bin/rays.exe`.
Compare its native `_build/default/bin/rays.exe` against this checkout's `target/debug/rays` on
the same machine, sketch, command, frame count and environment. Build outside timed execution;
use warmups/repeated samples and record wall time, allocations, domains and hardware.
Do not include cargo compilation in runtime comparisons. Measure release separately.
Stored OCaml thresholds remain in the Rust debug lowering performance test; the tag provides
reproducible source for refreshing same-machine baselines.

## Same-machine native comparison (2026-11-10)

Apple M1, 16 GiB, macOS 27.0.1 (26A434), eight logical CPUs. The unmodified
`ocaml-reference` checkout used OCaml 5.3.0 and Dune 3.21.1, native dev build; Rust
used the repository's normal debug profile at `05ed0200`. Existing `telegram` opam
libraries supplied the reference dependencies. Dune alone was bootstrapped locally
from its checksum-verified release, without changing an installed switch or adding
OCaml dependencies to Rust. Bootstrap took 62.46 s; the first reference app build
took 56.75 s with two jobs. These preparation times are not paired cold-build data.

Both executables ran the same 20 byte-identical sketches (none imports another
file), then `mem sketches/basic.rays --frames 30`. One warmup preceded three
alternating samples per implementation, with compilation outside the timers and
other build/test jobs paused. Each sample starts a fresh process. The native `mem`
route uses a 960×640 offscreen GPU canvas, fixed 60 Hz and one editor domain in both
versions; checks retain their normal defaults. All 16 commands exited successfully.

| Workload | OCaml median | Rust debug median | Rust result |
| --- | --- | --- | --- |
| `check` common 20-sketch corpus | 1.850556 s | 2.027166 s | 9.5% slower |
| `mem` basic, 30 fixed frames | 0.177366 s | 0.165680 s | 6.6% faster |

OS maximum-resident-set medians were 182.4/311.6 MB for the check corpus and
97.6/73.3 MB for the native frame command (OCaml/Rust, decimal MB). Native ledgers
reported 664.9/113.5 KB allocated per frame and zero readback in both versions;
OCaml GC allocation and Rust allocator counters cover different memory and are not
a byte-parity metric. OCaml check samples were 1.828/1.851/2.563 s, Rust
2.027/2.033/2.018 s. This establishes a real Mac runtime comparison, not an overall
Rust win. Release, paired cold builds and Windows remain unmeasured.

The external comparison script is `../rays-support/paired_native.py`; its source
hashes, complete commands, samples and stdout/stderr are retained under
`/tmp/rays-reference-common-corpus.json`, `/tmp/rays-native-paired.log` and
`/tmp/rays-native-paired/`. The other 29 top-level sketch files changed public
spellings, units, integer conversions or topology recipes and were excluded from
this identical-input comparison.

## Root migration verification (2026-11-06, Mac M1)

Warm app typecheck: 0.10 s. All-target workspace typecheck: 7.13 s. Debug app build: 5.81 s
after the verification builds. Corpus: 206 cases, 15.18 s wall, slowest 2.117 s. Web: 119 cases,
14.77 s; shader goldens: eight cases, 2.24 s. Metal: 20 cases, 5.16 s; OGPU Metal: four cases,
3.53 s. These are warm migration checks, not clean-build benchmarks or proof that all historical
performance targets are met.

SDL tests set their own dummy driver hints: do not globally set SDL_VIDEODRIVER=dummy for their
whole suite (the environment overrides SDL_SetHint). Native window tests run on the process's
initial thread. The Cocoa native-layer case passes on this host; the former video-device failure
came from running Cocoa on a test worker. PXUI display parity uses a separate native-driver
process and this host's measured 1× density.

## Current profile measurements (2026-11-10)

Run `cargo run -q -p buildtime -- profile` on each machine using the checked-in dev profile.
It records app build, a real flow_ir source-touch app rebuild, and sorted sketch checks with
benchmark-only 1-second process and 15-second corpus limits in `target/build-times.tsv`. A cold build is
labelled cold only when the shared dependency cache is empty; preserve a warm cache. Set
`RAYS_PROFILE=1` on an individual `target/debug/rays check sketches/NAME.rays` to see exclusive
phase timings and static-map execution/refusal details. `buildtime -- check` separately checks
the leaf/middle/top source-touch budgets and runs the middle/top suites through `cargo t`.

| Machine / profile | Cold app build | Cached app build | flow_ir touch app build | Corpus |
| --- | --- | --- | --- | --- |
| Apple M1, arm64, rustc 1.99.1, current O1 | Unmeasured; warm cache retained | 0.131 s | 4.526 s | Red; 49 attempted in 14.612 s, gallery and swarm killed at 1 s |
| Windows / current profile | Not measured; benchmark retired | Not measured | Not measured | Not measured |

This is current-profile evidence, not a completed O0/O1 comparison. The corpus's one-second
budget still fails for `sop_gallery` and `swarm`; the other 47 sketches completed. Because two
checks were killed, the bounded total does not prove the complete corpus meets 15 s.
Complete probes after the final shared numeric-key change measured Swarm at
1.356 s (three quiet samples: 1.356/1.369/1.292 s); the preceding gallery probe was
1.357 s. These measurements have different sample counts. Swarm materializes 30.84 million exact integer corners,
10.28 million offsets and 5.148 million color vectors; validation keeps
those computed values checked. CLI lowering omits editor probe records, while editor lowering
retains them. Geometry bytes and selected computed-error diagnostics match in the regression.

The migration's remaining performance targets were retired from acceptance on 2026-11-10.
The 0.5 s exact-case target, one-second sketch checks, 15 GB cache target, per-frame
editor allocation ceilings and an overall win over archived OCaml are measurement goals,
not outstanding correctness failures.
The explicit benchmark commands retain their comparisons; ordinary test case deadlines
remain enforced. Cache GC reports size without rejecting a valid warm cache and keeps
its existing lock and age protections. Conservative pruning of 2,101 superseded completed
incremental sessions reclaimed 21.245 GB, leaving a dated 53.211 GB allocated /
53.782 GB logical snapshot; subsequent builds can grow it.

The normal-mode `buildtime -- check` passes: leaf/middle/top touch checks take
0.134/0.245/0.288 s and the editor touch chain takes 1.617 s. All-target workspace
checking takes 6.70 s after source changes, then 0.283 s fully warm with no warnings.
The shared numeric-key change passed 500 SOP cases in 29.615 s and 41 GPU cases in
3.592 s, zero skips. Its final dispatch-threshold refinement separately passed exact-key
and complete geometry/error/reference parity checks. Geometry preparation in Swarm
fell from 246.9 to 152.6 ms; full checks still exceed one second.

Final Mac qualification was staged across the workspace: 1,409 cases before SOP, 56 SOP
cases, and 1,739 remaining cases, plus the added fractional-index rejection regression.
Every failure found in those runs was corrected and rechecked; the byte ceilings are now
reported as references. Complete flow_web (125) and web_export library (20) runs pass,
as do all 10 native isogrid cases. After the signed-zero portability fix, all 162
parameter/checker/document consumer cases pass in 13.263 s. The final all-target
check passes in 5.002 s and `cargo run -q -p gate -- all` passes in 9.451 s. This is staged evidence, not a single green
`cargo t --all` result. Commit messages retain focused commands and timings.

Native Cocoa and measured-density PXUI checks pass on this Mac. Real macOS border
dragging still requires a manual gesture; see [known limitations](known-failures.md).

Windows native validation used Windows 11 Pro build 26200, Rust 1.99.1 GNU and an
RTX 2080 Ti (driver 591.86, Vulkan 1.4.325). The complete workspace sweep at
`94f49dad` executed 3,170 cases in 587.772 s (811.199 s summed), within the existing
slow-mode limit. It found two signed-zero parameter assertions and one test-fixture
JSON path failure; both root causes were fixed at `1947c6b6`. Full affected-package
rechecks pass on Windows: parameter 14/14 in 1.680 s and runner 15/15 in 2.357 s;
the final all-target check passes in 5.739 s. Thus every failure from the sweep is
closed through staged rechecks. No other cases failed.
The slowest cases were catalog perturbation (29.389 s) and the full shattered studio
(22.353 s). Native focused checks also passed: math 21, Vulkan/SDL 59, editor smoke 1,
warm allocation/topology 4 and CLI 4. No performance profile overrides were used.
The historical editor harness still prints four `SKIPPED` metadata notices; the
runner's zero skip count does not establish coverage for those excluded ported cases.

## Vulkan on Linux

See [pxui/docs/vulkan-linux.md](../pxui/docs/vulkan-linux.md) (the PXUI repository), which also covers the PXUI gallery.

## The Rays editor on Linux

`rays` runs as a Wayland client: `runtime` hosts its window through `pxui_wayland` on Linux (`pxui_sdl`
elsewhere) and renders through `ogpu_vulkan`. SDL3 stays linked for image decoding, audio, the preferences
path and the SDL event translation `runtime` shares, so the workspace needs SDL3, SDL3_image and SDL3_mixer
(the `pxui/packaging/sdl3.lock` versions), which Ubuntu 24.04 does not package. Build them into `~/vk/root` (needs
`cmake`, a C compiler, and `libwayland-dev`, `libxkbcommon-dev`, `libdecor-0-dev`, `libpulse-dev`/`libasound2-dev`,
`libpng-dev` for the backends you want; the system `.pc` files must be on `PKG_CONFIG_PATH` while configuring):

```sh
export PKG_CONFIG_PATH=/usr/lib/x86_64-linux-gnu/pkgconfig:/usr/share/pkgconfig
P=~/vk/root/usr L=lib/x86_64-linux-gnu   # sources: the release tarballs from github.com/libsdl-org
cmake -S SDL3-3.4.18 -B b-sdl -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$P -DCMAKE_INSTALL_LIBDIR=$L \
  -DSDL_TESTS=OFF -DSDL_EXAMPLES=OFF -DSDL_X11_XTEST=OFF && cmake --build b-sdl -j && cmake --install b-sdl
cmake -S SDL3_image-3.4.6 -B b-img -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$P -DCMAKE_INSTALL_LIBDIR=$L \
  -DCMAKE_PREFIX_PATH=$P -DSDLIMAGE_VENDORED=OFF -DSDLIMAGE_AVIF=OFF -DSDLIMAGE_JXL=OFF -DSDLIMAGE_TIF=OFF \
  -DSDLIMAGE_WEBP=OFF && cmake --build b-img -j && cmake --install b-img
cmake -S SDL3_mixer-3.2.4 -B b-mix -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$P -DCMAKE_INSTALL_LIBDIR=$L \
  -DCMAKE_PREFIX_PATH=$P -DSDLMIXER_VENDORED=OFF -DSDLMIXER_GME=OFF -DSDLMIXER_MOD=OFF -DSDLMIXER_MIDI=OFF \
  -DSDLMIXER_OPUS=OFF -DSDLMIXER_WAVPACK=OFF -DSDLMIXER_FLAC_LIBFLAC=OFF -DSDLMIXER_VORBIS_VORBISFILE=OFF \
  -DSDLMIXER_MP3_MPG123=OFF && cmake --build b-mix -j && cmake --install b-mix
```

Then, with the environment of [pxui/docs/vulkan-linux.md](../pxui/docs/vulkan-linux.md) (its `LD_LIBRARY_PATH` also finds the SDL libraries at run time):

```sh
export PKG_CONFIG_PATH=~/vk/root/usr/lib/x86_64-linux-gnu/pkgconfig:/usr/lib/x86_64-linux-gnu/pkgconfig:/usr/share/pkgconfig
ulimit -s 262144   # tests: the 256 MB main-thread stack macOS and Windows get from the linker
cargo run -p rays_bin -- sketches/pxui.rays
RAYS_ALLOW_SKIP=1 RAYS_SLOW=1 cargo t -p renderer   # lavapipe needs the 60 s limit for the heavier cases (rays too)
```

On Linux the main-thread stack is the `RLIMIT_STACK` the process was exec'd with (the ELF header is ignored), so
`rays` raises the soft limit to 256 MB and re-execs itself once at startup; tests take it from `ulimit -s`.
Sketch text uses DejaVu Sans, Noto Sans or Liberation Sans when installed, else the bundled Pragmasevka. The
preferences live in SDL's `~/.local/share/rays/editor`. What the Wayland host does differently:

- File dialogs go through the XDG desktop portal (`org.freedesktop.portal.FileChooser` on the session bus); the
  answer arrives as the same `FileDialog` event SDL sends. Without a portal (WSL runs none) `show_dialog` returns a
  typed `Unsupported` error (`pxui_wayland::NO_FILE_DIALOG`) and the editor keeps working.
- The clipboard is read synchronously through the newest window (its own selection, else the offer through a
  pipe, one second at most); with no window open it is process-local.
- Relative mouse uses pointer-constraints-v1 and relative-pointer-v1 (WSLg offers both), degrading to absolute
  motion deltas without them. An unthrottled (`vsync: false`) swapchain is MAILBOX on Wayland, else FIFO.

On lavapipe (Mesa 25.2, WSL2) every case passes, with `RAYS_SLOW=1` for `rays` and `renderer`: compiling their
pipelines on the CPU takes `window_sketch_retained_view` 6 s and `gpu_execution_retained_eviction_textured300_boundary`
5.0 s. The whole `RAYS_SLOW=1 cargo t -p rays_editor` run takes about 500 s of its 600 s cap. Pixel tests allow a CPU
driver (`Caps::cpu_device`) one level more than Vulkan hardware.
