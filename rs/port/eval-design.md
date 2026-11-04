# Debug-build lowering faster than native OCaml

Target (user's bar): a **debug** (`-O0`) Rust build lowers each benchmark faster than the native OCaml build does.
Gate: `RAYS_SLOW=1 cargo t -p flow_sop debug_lowering_beats -- --ignored --nocapture`
(`crates/flow_sop/src/lisp_sop/test_perf_lower.rs`). It times a cold `lower::of_checked` (the catalog and the check are
not timed) and fails while any case is at or above its OCaml time.

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
