# Post-parity optimization backlog

Speedups we skip today only because the output must match OCaml byte for byte (goldens, diagnostics
text and order, residual/region ids, float printing). Once the Rust port is the only implementation
and its own outputs become the goldens, take these in order of measured win. Each row: where, what
blocks it now, expected gain, evidence.

Rule for agents: when you reject an optimization for byte-exactness, add a row here instead of
dropping it.

| # | Where | Optimization | Blocked by | Expected gain | Evidence |
|---|---|---|---|---|---|
| 1 | `flow_ir` `core_lower::term_kernel` → `packed::compile_result` | Memoize kernel compiles by (parent, term, captured bindings); 475 compiles for ~280 distinct keys on garden | Each compile allocates fresh global residual ids (`Residual::with_bindings`) and stores the residual in the region (`residual`, `sources`, `stages`); skipping a call shifts later ids | ~40% of 430 ms (garden, debug) | garden profile, 2026-11-04 |
| 2 | `flow_ir` `core_lower::residual_body` | Cache lowered bodies with scope-relative local numbering, renumber on reuse | Locals/statements are numbered in the current `Scope`, visible in printed regions | part of ~370 ms (garden) | same |
| 3 | `flow_eval` residual/function ids | Per-evaluation (arena-local) ids instead of process-global counters; enables 1 and 2 directly, and parallel lowering | Ids appear in printed regions/goldens | unlocks 1, 2, parallelism | arena-lending redesign `2bb65a8f` |
| 4 | `flow_eval` `structure_term` | Structural hash of the term (u64) instead of `format!("{:?}")` + hex text as the reuse key | Key text equals OCaml's marshal-based key where it is printed or ordered | rest of ~630 ms (garden) after caching | garden profile |
| 5 | `flow_ir` strict validation | (Tried, no gain; kept as a note) Skipping residuals an earlier successful validation lowered inline cut strict-lowering misses only 88 -> 83: the roots are distinct and each re-lowers its own nested residuals, so order/dedup does not help. Real win needs rows 1-3 (shared lowering of nested residuals) | Per-root lowering of nested residuals yields distinct ids and scope-relative locals | none alone | nodebuild agent prototype, 2026-11-04 |
| 6 | Maps whose iteration order leaks into output | Replace order-preserving (OCaml `Map`-mirroring) `BTreeMap`s/sorted vectors with hash or index maps where the output order is not a contract | Order shows up in printed text/goldens | constant-factor wins on hot lookups | port cookbook §8 |
| 7 | `cfmt` float printing | Shortest round-trip (`{}`) instead of `%.17g`-style emulation where precision is not a contract | Printed floats must match OCaml `Printf` | text-heavy paths (fmt, web export) | |
| 8 | 63-bit `ocaml_int` semantics | Native `i64` arithmetic where no program can observe the 63-bit wrap | Overflow behaviour is part of the Lisp contract (fast precision) | small, everywhere | §9 semantic traps |
| 9 | Parallel results | Allow non-sequential reduction order for float sums where bit-exactness vs one domain is not required | AGENTS.md: parallel == sequential byte-for-byte | scaling on many-core | |

Not here: anything that changes a documented Lisp contract (names, defaults, ranges, errors). Those
stay; this list only frees implementation details that today are pinned by goldens.
