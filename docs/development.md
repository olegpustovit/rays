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
cargo t --all                          # sequential crate qualification, stop at first red
```

`cargo t` enforces a 5-second case deadline and 60-second test-run deadline. Its build has a
separate 900-second safety cutoff. It serializes GPU/window cases, prints slowest cases and
appends `target/t-times.tsv`. Use `RAYS_SLOW=1 cargo t -p <crate> <filter>` for explicit qualification.

Stage tests: focused cases after edits, touched crates and consumers at stable checkpoints,
affected native checks before commit. Save broad qualification for the final boundary.
A timeout is work to fix. Keep focused warm checks under 30 seconds.

The dev profile retains O1, no debug symbol payload, incremental compilation and 256 codegen units.
Existing Mac measurements: sketch checks took 123.8 seconds at O0 versus 19.1 seconds at O1;
a flow_ir edit rebuilt the app in 4.5 versus 5.2 seconds. Keep this measured tradeoff until a
same-machine replacement wins total edit/build/run time. Allocator counters alone use O3.
Changing profiles must report compile AND debug runtime. [Historical measurements](history/build-speed.md)
cover past machines and settings, not current defaults.

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
hard 1-second process and 15-second corpus limits in `target/build-times.tsv`. A cold build is
labelled cold only when the shared dependency cache is empty; preserve a warm cache. Set
`RAYS_PROFILE=1` on an individual `target/debug/rays check sketches/NAME.rays` to see exclusive
phase timings and static-map execution/refusal details. `buildtime -- check` separately checks
the leaf/middle/top source-touch budgets and runs the middle/top suites through `cargo t`.

| Machine / profile | Cold app build | Cached app build | flow_ir touch app build | Corpus |
| --- | --- | --- | --- | --- |
| Apple M1, arm64, rustc 1.99.1, current O1 | Unmeasured; warm cache retained | 0.131 s | 4.526 s | Red; 49 attempted in 14.612 s, gallery and swarm killed at 1 s |
| Windows / current profile | Unmeasured; machine unavailable | Unmeasured | Unmeasured | Unmeasured |

This is current-profile evidence, not a completed O0/O1 comparison. The corpus's one-second
budget still fails for `sop_gallery` and `swarm`; the other 47 sketches completed. Because two
checks were killed, the bounded total does not prove the complete corpus meets 15 s. Separate
complete probes after one warmup measured medians of 1.543 s for swarm and 1.405 s for gallery
(three alternating samples). Swarm materializes 30.84 million exact integer corners,
10.28 million offsets and 5.148 million color vectors; validation keeps
those computed values checked. CLI lowering omits editor probe records, while editor lowering
retains them. Geometry bytes and selected computed-error diagnostics match in the regression.

`cargo t --all` is implemented and its dispatcher regression passes; complete native qualification
is still blocked by the cache-size check. GC retains the recent warm cache: its initial observed
size was 75.537 GB and zero artifacts qualified for seven-day deletion. A later inode-aware audit
measured 72.963 GB of allocated storage. Conservative pruning of 2,101 superseded completed
incremental sessions subsequently reclaimed 21.245 GB, leaving 53.211 GB allocated and
53.782 GB logical. The newest completed session for each exact configuration, fresh incomplete
sessions, working sessions and live-locked sessions are retained. The 15 GB limit remains red.
Native Cocoa and measured-density PXUI checks pass. The remaining limits stay visible as failures.
