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

## Root migration verification (2026-11-06, Mac M1)

Warm app typecheck: 0.10 s. All-target workspace typecheck: 7.13 s. Debug app build: 5.81 s
after the verification builds. Corpus: 206 cases, 15.18 s wall, slowest 2.117 s. Web: 119 cases,
14.77 s; shader goldens: eight cases, 2.24 s. Metal: 20 cases, 5.16 s; OGPU Metal: four cases,
3.53 s. These are warm migration checks, not clean-build benchmarks or proof that all historical
performance targets are met.

SDL tests set their own dummy driver hints: do not globally set SDL_VIDEODRIVER=dummy for their
whole suite (the environment overrides SDL_SetHint). The Cocoa window case needs a graphical
session; it reports No available video device in this environment.

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
| Apple M1, arm64, rustc 1.99.1, current O1 | Unmeasured; warm cache retained | 0.121 s | 4.011 s | Red; stopped at 15.019 s before all 49 files |
| Windows / current profile | Unmeasured; machine unavailable | Unmeasured | Unmeasured | Unmeasured |

This is current-profile evidence, not a completed O0/O1 comparison. The corpus's one-second
budget still fails for `sop_gallery`, the three swarm sketches and `ws_rosette`. Direct complete
probes measured swarm at 2.915 s and gallery at 1.510 s. Swarm materializes 30.84 million exact
integer corners (1.964 s), 10.28 million offsets and 5.148 million color vectors; validation keeps
those computed values checked. CLI lowering omits editor probe records, while editor lowering
retains them. Geometry bytes and selected computed-error diagnostics match in the regression.

`cargo t --all` is implemented and its dispatcher regression passes; complete native qualification
is still blocked here. GC retains the recent warm cache: its observed size is 75.537 GB and zero
artifacts qualified for seven-day deletion. The Cocoa window check also needs a graphical session.
These limits remain failures, not skipped successes.
