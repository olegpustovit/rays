# Debug-build lowering faster than native OCaml

Target (user's bar): a **debug** (`-O0`) Rust build lowers each benchmark faster than the native OCaml build does.
Gate: `RAYS_SLOW=1 cargo t -p flow_sop debug_lowering_beats -- --ignored --nocapture`
(`crates/flow_sop/src/lisp_sop/test_perf_lower.rs`). It times a cold `lower::of_checked` (the catalog and the check are
not timed) and fails while any case is at or above its OCaml time.

## Current memory work — 2026-11-05, Mac M1

Fresh unoptimized debug baseline after `7e547730`: `target/debug/rays mem
../sketches/ws_tree.rays --frames 3` exits 0 in 5.02 s under a 30-second deadline.
Live 77.9 MB, device 11.7 MB, footprint 121.5 MB, unattributed 58.4 MB; frame
allocation 4.7 MB and payload readback 13.9 KB/frame. Binary SHA-256
`967a6f3a08e7744057f7347b37e24c75ad2622f8352625ad1737de3fa7fd112c`;
raw log `/tmp/rays-memory-debug-tree-baseline.log`. This debug sample is separate
from the versioned release comparisons below. At this baseline, retained-memory visitors
covered checked/source arenas while evaluator arenas/caches had no corresponding
visitor; the following accounting change starts covering those owners.

The first partial evaluator visitor now reports `flow.eval.arenas`: source/checked
storage, plan-node fields, values, closure/residual environments, names/paths and
selected caches, deduplicated by allocation identity and capacity. It snapshots the
weak live-arena registry, never waits for an arena, and retains no arena after a
report. Busy arenas are omitted. Map-node storage, type/diagnostic payloads, callback
captures and compiled-code caches remain excluded; this is a lower bound.
The corresponding debug tree sample reports 2.8 MB for this owner, 78.0 MB live,
56.0 MB unattributed, 11.7 MB device, 123.7 MB footprint, 4.7 MB/frame allocated
and 28.4 KB/frame payload readback; exit 0 in 3.84 s. SHA-256
`7691abec11a79b3ddf71bc3efd4134374ad1986938676a3522e2ebdddc50ca01`,
log `/tmp/rays-eval-memory-tree-after.log`. Payload readback differs between these
three-frame samples; neither the timing nor live-byte difference is an isolated
performance claim. The new owner reclassifies retained memory rather than saving it.

The unoptimized native `recursive_signals` frame-allocation gate now passes at
771,271 bytes/frame (previously 13,895,611), below its unchanged 1,100,000-byte
ceiling. Built-in shader metadata is immutable and retained by nine `OnceLock`s;
the workspace state-reset digest is computed lazily once per evaluator arena and
shared by replay clones. Digest bytes and equal/changed-source reset behavior stay
covered by regressions. The original full-depth fixture and exact pixel comparison
remain; every resident frame asserts successful device-backed mark output and the
measured frames create no new GPU buffers. This does not close live-memory targets.

Compiled `Code` paths and closure route bases reuse the evaluator's interned `Arc<Path>`;
owned paths are materialized when recording results. This changes no Lisp names or record paths.
The full evaluator gate passes 92 tests, including the OCaml sketch-output oracle.

The embedded library now validates/indexes all source files once, retaining declaration headers,
references and fixed authored-ID bases. A workspace check parses only reached files on a prelude
cache miss; the existing eight-subset prelude cache remains bounded. The complete `sources()` and
`node_definitions()` APIs materialize the full immutable library on explicit request (manifest
generation/tests), accounted by `flow.library.syntax`. File order, authored IDs, spans and diagnostics
are unchanged; catalog/source parity tests cover the normal and full-library paths.

Native allocation profiling identified roughly 21 MB under full-library parsing. The ledger measured
18.9 MB of retained source arenas before lazy materialization; final same-machine memory figures
and binary hashes are recorded in `smoke-native.md` rather than inferred from the profile.

The initial-domain prelude cache now reports its retained syntax and checked arenas through
`flow.library.preludes`, using the same allocation identities as document roots. Shared arenas
are charged once across cache entries and documents. Catalog/operator closures and map nodes
remain excluded; this is a lower bound, not an estimate of the whole cache. The regression checks
both repeated traversal and repeated ledger reports. Accounting does not reduce live memory.

Inline replay now keeps immutable captured maps/lists shared when none of their contained
node, residual or function IDs changes. It still recursively relocates affected values, including
IDs nested in records, structs and host sources. Tests verify both pointer sharing and exact
relocation without mutating the original capture. The paired three-frame `ws_tree` release
measurement falls from 82.5 MB to 80.5 MB live; remaining memory work stays open.

Editor cache keys now retain only the evaluation identity they already compare, rather than
cloning an entire lowering. The immutable document owns its lowering through `Arc`; restoring
saved compiled IDs/sites uses copy-on-write, and actual literal edits still produce a changed
lowering. Frame/scope code borrows this shared lowering until a callback needs an owned handle.
This removes deep copies of plans, diagnostics and maps during immutable model updates without
changing cache invalidation policy. The camera test also reads its installed shell for viewport
bounds instead of repeatedly rebuilding/lowering the workspace as an observation side effect.
Its original four instances and all gestures/assertions remain; the default five-second budget
is still open despite the measured reduction from about 25.6 to 6.1 seconds diagnostically.

## Next checker phase: reuse inference work without losing effects

Cold gallery profiling on the Mac M1 (`cargo t --profile slow -p rays_bin --test e2e
sketches_sop_gallery`) records 9,243 definition applications, 6,228 generic function checks
and 6,234 specializations. In the instrumented 7.771 s run, exclusive times are 2.771 s,
1.121 s and 0.890 s respectively; the authored workspace check is only 0.060 s, parsing
0.254 s and printing 0.025 s. Raw evidence: `/tmp/rays-gallery-specialization-phases.log`
and `/tmp/rays-gallery-fresh.sample`. Temporary instrumentation was removed.

The generic and specialized results are not interchangeable. Generic checking preserves
captured dependencies; HOF application passes actual element/accumulator information, while
`instantiate_fn` deliberately checks typed leaf inputs. Reusing the first term directly
changes metadata even when the final evaluated number matches. A whole-result memo keyed
only by source/type is therefore insufficient.

The next implementation must separate reusable body analysis from context-dependent replay:

- Preserve all `Info` outputs: type, callable/captures, liveness, length, varying zones,
  groups, packed producers, approximation sources, state dependence and list fields.
- Preserve ordered diagnostics and checker effects: live/invariant/packed paths, roots,
  inline-path allocation, input paths, zone IDs, function/kind references and safety budgets.
- Rebind term IDs to the active builder; preserve authored IDs, spans and trivia. Closures
  must retain their original lexical environments. Unhandled effects use ordinary inference.
- Prove parity of complete checked workspaces and diagnostics, including unused functions,
  nested captures/HOFs, live and state inputs, packed refusals, and depth/step exhaustion.
  Evaluation goldens alone cannot prove equivalence of checker metadata.

Measure the same cold process and original gallery/camera fixtures after that work; the
default deadline stays five seconds. Rejected hash/context/import experiments in the
checkpoint are not a substitute for this phase. No inference-result cache is implemented yet.

## Where we are (2026-11-04, after the arena redesign 2bb65a8f, loaded 24-core Windows host)

| case | OCaml native | Rust debug | ratio | evaluator share (phase "evaluate") | Rust release |
|---|---|---|---|---|---|
| box | 36 ms | 89-100 ms | 2.6x | 60 ms | 49-55 ms |
| transform | 45 ms | 253-266 ms | 5.8x | 97 ms | 166-169 ms |
| sphere (cold) | 205 ms | 2.7-3.0 s | 14x | 2.7-3.0 s | 684-702 ms |
| garden (cold) | 260 ms | 4.5-5.1 s | 18x | 0.8-0.9 s | 1.23 s |

Garden outside the evaluator (one debug run, temporary phase marks): `finish` (network build: node factories,
kernel compiles, card cooks) 2.2 s, `qualify_workspace` minus evaluation 0.39 s, `lower` self 0.23 s.

The sphere is 1.12 M evaluator steps: 2.7 us/step in debug, 0.6 us/step in release, about 0.18 us/step in OCaml,
with 9.8 allocations per step. The profile (`PROF=1`, sphere, debug) is integer Lisp: `Let` 96 k calls / 428 ms,
`int/add` 50 k / 208 ms, `Ref` 443 k / 165 ms, `CallFn` 23 k / 148 ms, `If` 53 k / 134 ms, `Cond` 33 k / 133 ms,
`int/eq|lt|mul|div|sub|mod` about 25 k each / 70-100 ms, ~18 allocations per arithmetic op.

So debug needs about **15x less work per evaluator step** (and per lowering step outside it) than the tree walker
does now. `-O0` keeps every abstraction (no inlining, iterator adapters, `Arc` refcounts, bounds checks), so the
win must come from doing less, not from optimizing what is done.

## What a step costs today (`eval_raw_body`, `apply_op`)

- per step: an atomic `EVAL_STEPS` add, budget/interrupt checks, `current_residual_handlers()` clones a
  `ResidualHandlers` (nine `Option<Arc>`) when live, the `observe` match;
- `eval_sub` allocates a `RouteNode` and clones `ExecCtx` (seven `Arc`s, a `Context`, a `card`) for every argument;
- `Env` is an `Arc<BTreeMap<String, Value>>`: a lookup compares strings, a `bind` clones the whole map when shared;
- `apply_op`: clones the workspace `Arc`, finds the op by name (`find_indexed`, a string hash), copies the
  arguments, builds `typed` (a `String` clone and a `param::Value` per argument), computes `result_type`, wraps
  `mk_node` in a `RefCell` closure, and the op body takes `&[(String, Value)]`;
- values: `Value::Text(Vec<u8>)`, `Record(Vec<(String, Value)>)` clone names and payloads on every copy.

## Plan, in checkpoints (each: byte goldens vs OCaml, the six-crate gate, the stress test, the gate numbers)

1. **Free wins in the walker** (no new structure). Borrow the handler stack instead of cloning it; drop the
   workspace clone in `apply_op`; resolve each `Op` term's operator once per arena (side table keyed by the term's
   address: terms live in the arena's `Arc<Workspace>`); build `typed` only for ops whose result type depends on
   arguments; make the route lazy (an `(parent, segment)` index into a per-arena route table, materialised to a
   `Path` only when a site is recorded or a node is made). Expected 2-3x on the sphere.
2. **Compile checked terms once** to a `Code` tree (an enum of closures-free instructions): variables resolved
   to frame slots (`Env` becomes a `Vec<Value>` frame plus a parent link, no strings), operators pre-resolved to
   `fn` pointers with their result types, literals pre-built `Value`s, `Let`/`If`/`Cond`/`Case` as jumps over slots,
   scalar arithmetic (`int/*`, float ops, comparisons, `and`/`or`) on a fast path that takes two `Value`s by
   reference with no argument vector. The code is cached per arena by term id (`form.id`, path). One reused operand
   stack per evaluation. The OCaml semantics (records, routes, sites, residualisation on `$NEEDS_TIME`, the static
   budget) are kept by construction: every instruction maps to one current `eval_raw_body` arm. Expected 8-12x.
3. **Allocation per step to ~0**: `Value::Text` as `Arc<[u8]>`, records with interned field names and shared
   payloads, closures' environments as captured slot vectors, a per-evaluation bump arena for frames.
4. **Bulk work leaves the interpreter**: a numeric `map`/`for` over arrays (the uv_sphere/tube/scatter topology
   loops) runs as a flow_ir packed kernel at any size during lowering, not only past 4,096 elements; the walker
   keeps the first element for records where OCaml records them.
5. **Lowering outside the evaluator** (garden: 2.2 s `finish`, 0.4 s qualify): profile `finish` (node factories,
   kernel compiles, card cooks) and apply the same rules (no per-node string keys, no repeated digests, cached
   compiled kernels). Add permanent `phase_timer` marks for these phases so the gate prints them.

Rules that do not change: no `unsafe` outside `*_sys`/`parallel`; no new crates; OCaml-exact results (the sketch
byte goldens, `RAYS_SLOW=1 cargo t -p flow_eval sketch`); the arena discipline of eval.rs (lend around every host
callback). Each checkpoint reports the gate table above.
