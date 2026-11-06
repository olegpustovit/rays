# Fast Rust iteration

Work from the repository root. Keep one warm `target/`; Cargo serializes compilation.
No opam, Dune or OCaml compiler is needed.

```sh
cargo check -p flow_ir                  # typecheck after an edit
cargo t -p flow_ir filter               # focused regression with deadlines
cargo t -p rays_bin --test e2e          # corpus boundary at a stable checkpoint
cargo run -p gate -- deps               # architecture boundary check
cargo run -p buildtime -- check         # explicit build-time qualification
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
