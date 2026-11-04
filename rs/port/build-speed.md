# Build and test speed (Windows-gnu, 24 cores, ~12 agents)

Measured 2026-11-04 on a copy of HEAD (`git archive`), `--config` overrides, one change at a time. Under
load every number swings 2-3x, so each claim below is a same-conditions A/B or a repeated run.

## Settings (rs/Cargo.toml `[profile.dev]`, rs/.cargo/config.toml)

| Setting | Before | After | Evidence |
|---|---|---|---|
| Linker | mingw `ld.bfd` | rustc's bundled `ld.lld` via `-fuse-ld=lld -B<toolchain>/.../gcc-ld` | flow_sop test exe (80 MB) relink 3.7-5.9 s -> 1.6-1.8 s (8 samples each); renderer, sdl3_image, flow_check, rays_math, flow_sop tests link and give the same results |
| `debug` | `line-tables-only` | `0` | cold flow_eval tests 42-45 s -> 32-41 s; test exe 26 MB -> 7 MB; target dir 0.26 -> 0.16 GB |
| `opt-level` of flow_syntax/check/eval/ir/sop, rdk_geom/ops/boolean | 1 | 0 (all removed) | flow chain cold 32-41 s -> 20.6 s; rdk_ops+rdk_boolean cold 99 s -> 29 s. Test CPU unchanged: flow_eval 83 tests 19.4 s (O0) vs 18.8-20 s (O1); rdk_ops 260 tests 23-28 s both; flow_ir tests identical at O0, O1, O3 (2.0 s) |

`codegen-units = 256` and `incremental = true` stay. `debug = 0` still prints panic locations; only
`RUST_BACKTRACE` source lines are gone. For a one-off: `cargo --config 'profile.dev.debug="line-tables-only"' ...`.

The lld `-B` path is the pinned toolchain's directory (`rust-toolchain.toml`, 1.99.1). If the pin or user
changes, update `rs/.cargo/config.toml`. The section only applies to `x86_64-pc-windows-gnu`.

### Recovery: `undefined reference ... llvm...` at link time
Seen after per-package opt-level / profile changes: stale incremental object files of a crate built with a
different profile. In that agent's target dir: `cargo clean -p flow_syntax -p flow_check -p flow_eval -p flow_ir -p flow_sop -p flow_web`
(add the rdk_* crates if they are named), then rebuild. If it persists, delete the whole `target/ag-<name>`.

## Budgets (tools/buildtime, `cargo run -p buildtime -- check`)
Canaries: leaf `rays_math`, middle `flow_check`, top `flow_sop` (override with `BUILDTIME_LEAF/MIDDLE/TOP`).
A red test no longer fails the tool (it warns; timing is still counted); a build failure or a hang still does.

## Why tests are slow under load, not at -O0
- `rays-t` runs one process per test with `available_parallelism()` = 24 workers. With ~12 agents compiling
  and testing, every CPU-bound test stretches 2-3x: `flow_web` `integer_registers_past_2_pow_53_a` is 1.7-1.9 s
  alone and 5.2 s in the 40-test run when the box is busy; the same full run is 2.3 s wall when idle.
- flow_web at opt-level 1 vs 0: no difference (2.3 vs 2.4 s full run). flow_ir O0/O1/O2: within noise.
- The ~2 s tests the lead listed (`test_compact_scan::core_and_executor_match_the_reference`,
  `core_lists_parity_part_0`, `loop_fusion_at_scale_*`, `static_bulk_maps`) take 2.0-2.1 s at flow_ir/flow_eval
  O0, O1 and O3 and with every crate at O1; run directly (no rays-t) the first is 1.4 s wall / 1.2 s CPU.
  So they are fixture size (e.g. 10 bodies x 3 domain counts x 4 times x reference+executor at 2049 rows), not
  opt-level. Shrink the fixture (the file already says "2049 keeps ... within 5 s"); under load 2 s becomes 5 s.
- `RAYS_SLOW=1 flow_eval sketch_checks_match_the_ocaml_oracle` (57 s) was not re-measured; it is a fixture
  problem of the same kind (opt-level does not move flow_eval test CPU).
- Do not run `cargo t` for several crates at once; the 24-way fan-out of each run multiplies.

## Edit loop after the change (HEAD copy, warm, appended `pub fn` per edit)
| Step | rays_math (leaf) | flow_check (middle) | flow_sop (top) |
|---|---|---|---|
| edit > `cargo check -p` | 0.2-0.4 s | 0.4-0.5 s | 0.6-1.3 s |
| edit > `cargo t -p` (build + run) | 1.2 s | 2.2-2.4 s | 9.9-11.6 s (before lld/O0; 7.8 s `cargo t` alone after) |

Clean `cargo build --workspace` in a fresh target dir: 60.8 s (budget 120 s), then
`cargo test --workspace --no-run`: +111.7 s. Cold `flow_sop --tests`: 201 s before (loaded), see the table in the report.

## Shared vs private target dirs
All agents share one source tree, so a broken edit in any crate breaks everyone's build whichever target dir
they use; private dirs isolate only artifacts. Costs measured: 96 private dirs = 78.9 GB (0.1-5 GB each; 42.9 GB after deleting 93 stale ones); a cold
dir costs 30-60 s for a flow_* crate and 200+ s for flow_sop/renderer under load; a no-op build in a warm dir is 0.03-0.5 s.
Lock cost (4 independent crates edited, `cargo test --no-run` each, warm): sequential 9.6 s, 4 concurrent in ONE dir 7.1 s (cargo serialises them, ~2 s each). A pile-up is cheap; a cold private dir is 30-200 s.
Policy:
1. Default: one shared dir `rs/target/shared` for agents that only run `cargo check` / `cargo t` on crates they
   do not edit (identical source paths, so fingerprints are shared; cargo's lock only serialises the compile step,
   which is 1-5 s once warm; tests run outside the lock).
2. Private dir `rs/target/ag-<name>` only for an agent editing a core crate that many others depend on
   (rays_math, param, flow_syntax, flow_check, flow_ir) or running a long `--timings`/profile experiment; delete it
   when the agent finishes.
3. Never pass `--config`/`RUSTFLAGS` overrides into the shared dir (it re-hashes every crate).
4. Stale `ag-*` dirs (agent finished) are deleted by the lead; `ag-*` older than 2 h with no running cargo are fair game.
