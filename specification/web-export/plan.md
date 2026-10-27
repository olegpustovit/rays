# Web export: implementation plan

Task-level plan for compiling a Rays Lisp workspace into a standalone WebGPU
browser application. It is written so that a small model can take one task per
loop iteration. Every file and line reference was read at commit `923dcecf`
(2026-10-10). Implementation progress and verified gates are recorded in the
ledger at the end. Do not code against a name this file introduces until the
task that creates it is ticked there.

Current image scope: the 2D export supports `image/map`, `image/noise`, `image/render` and static
`image/load` assets (P6.4b–P6.4e). File paths must be known when building the standalone bundle.
The historical P5/P6 refusal tables below describe their original runs.

## The design in one page

Today four pieces of code read the checked term tree (`Flow.Workspace.term`):
the tree walker `Flow.Eval.ev`, the closure tier `Flow.Eval.compile`, the kernel
compiler `Flow_ir.Packed.compile_impl`, and the drawing lowering
`Rays_editor.Drawing`. Nothing they produce at run time is a program that can be
written to a file: closures, GPU handles and caches are not a representation.

The plan replaces the last three readers with one lowering to a **core**, which
both native and web run:

- **Frame level.** Runs once per frame on one thread. Values: 64-bit float,
  integer, boolean, text, fixed vectors, records (flattened to fields), bounded
  lists (at most 4,096), packed arrays, state slots. Structured `if` and bounded
  loops. It emits draw statements and host effects.
- **Region level.** A pure numeric loop body over packed arrays: today's packed
  program (`Flow_ir.Packed`), widened to several outputs. A region runs in
  parallel blocks on the CPU or as a compute shader.

An operator is lowered by a compiler rule into core primitives. Only primitives
are implemented per target, so the primitive list is the whole per-target cost.

The run-time drawing contract is a **mark table**: per mark set, one buffer per
attribute (position, size, fill, ...), in painter order, plus a view transform.
The compiler optimizes drawing on that form (attribute fusion, rate split, batch
merging, placement by target) before any target prints it.

Targets are printers over the core:

| Part | Native | Web |
| --- | --- | --- |
| Frame program | core executor in OCaml | emitted JavaScript module `program.js` |
| Region on CPU | `Packed.force` over `Parallel` | emitted JavaScript loop on web workers over `SharedArrayBuffer` |
| Region on GPU | Metal (`lib/flow_gpu/emit.ml`) | WGSL (`kernels.wgsl`) |
| Marks | Metal pipeline through OGPU | WebGPU pipeline in the fixed runtime |

The browser has a fixed, hand-written runtime (`runtime.js`: events, frame loop,
state slots, worker pool, WebGPU device, presenter) that is the same for every
sketch. The compiler emits only the program: `program.js`, `kernels.wgsl`,
`data.bin`, `program.map`, `index.html`.

Decisions already taken, not to be reopened by a task:

- The compiler emits static JavaScript and WGSL. No OCaml compiled to
  WebAssembly, no bytecode VM. Results need not be bit-identical between native
  and web; native core results must be bit-identical to the tree walker.
- Scope is the 2D canvas: `:context draw` graphs and the values that feed them.
  PXUI, the editor, `sop` and `scene` graphs and the path tracer are not
  exported (`AGENTS.md`, Direction).
- Web workers are required. The page needs cross-origin isolation; there is no
  single-threaded path.
- The export is static files, no server and no live reload (decided 2026-10-27):
  `rays web FILE -o DIR`, previewed with `npx serve DIR`, which takes the
  isolation headers from the emitted `serve.json`. No scrub streaming, no HTML
  controls.
- The tree walker stays, as the compile-time evaluator of the static stage and
  as the oracle for every test.
- No new Lisp syntax.

## How to run the loop

Each task is one loop iteration for one model: it names its files, its change,
the command that proves it, and what to do when the proof fails. A task is done
only when its gate passes; a task whose gate cannot pass is reported, not worked
around.

### Which model takes which task

| Tag | Model | Kind of task |
| --- | --- | --- |
| **S** | Sonnet | A change that needs a design choice inside one module, or touches evaluation order, coercions or ownership: a lowering slice, the executor, a fusion rule, a renderer binding |
| **H** | Haiku | A mechanical change with an exact oracle: a snapshot, a golden, a printed table, a dune stanza, a gate rule |
| **R** | Sonnet, review | Reads a finished phase against its gate and the specification; changes only this file |

A Haiku task never decides semantics. If its oracle disagrees with its
instructions it stops and writes a note; the next Sonnet iteration picks it up.

### Loop rules

1. Read the ledger at the end of this file. Take the lowest-numbered unticked
   task whose **needs** are all ticked. One task per iteration, one commit per
   task.
2. Before editing, read the nested `AGENTS.md` of every directory in **files**.
3. Run the task's gate. If it fails twice after honest fixes, discard the
   working-tree changes, add a line `note: <what failed>` under the task in the
   ledger, commit only that, and stop.
4. Never weaken a test, refresh a golden or snapshot, or run `dune promote`
   unless the task says so by name.
5. Never delete an old path in the task that adds its replacement. Deletions are
   separate tasks after a review.
6. A task that measures appends a dated entry to
   `specification/web-export/results.md` (created by the first such task):
   command, machine, domains, input size, before and after.
7. On success, tick the task in the ledger in the same commit.
8. At the end of a phase the **R** task runs the phase gate and either ticks the
   phase or adds repair tasks (`P<n>.<k>a`) to the table and the ledger. The
   next phase does not start before that.

## Ground rules every task inherits

These restate the repository's rules (`AGENTS.md`). Where a task card and this
section disagree, stop and report.

### Commands

| Purpose | Command |
| --- | --- |
| Build the validation launcher, once per worktree | `dune build tools/check.exe` |
| Typecheck only | `dune build @check` |
| Focused tests | `_build/default/tools/check.exe @lib/<name>/runtest` |
| All default tests, no window | `SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy _build/default/tools/check.exe` |
| Every workspace, IR against reference | `_build/default/tools/check.exe @test/test_workspace_ir` |
| The same with native pixels (opens a window) | `_build/default/tools/check.exe @test/test_workspace_pixels` |
| GPU and display tests | `_build/default/tools/check.exe @runtest-native` |
| End-of-phase check | `_build/default/tools/check.exe --ship` |
| API manifest after an intended `.mli` change | `dune build @tools/api_manifest/runtest`, then `dune promote`, as its own commit `Promote the API manifest for <what>` |

Never pass `--force` and never clean `_build`. In an isolated git worktree,
`dune` needs `--root .`, and `dune promote` acts on the main checkout, so a
worktree agent leaves the API manifest unpromoted and says so in its commit.

### Code rules

- Warnings are errors. Every new module has an `.mli`. Libraries stay wrapped.
  Build and test glue is OCaml under Dune, never Python.
- A new library or a new dependency edge is a boundary change: its `dune`; the
  `rules` (line 82), `only` (line 147) and injected-edge checks (lines 301-340)
  of `test/dependency_gate.ml`; the Libraries table and Dependency rules in
  `AGENTS.md`; and `specification/backend.md`.
- The token scan (`test/dependency_gate.ml:211-222`) rejects `RAYS_WEB`,
  `RAYS_BACKEND`, `RAYS_RENDERER`, `RAYS_HEADLESS`, `--backend`, `--renderer`,
  `--headless` and `Dynlink` under `lib`, `examples` and `sketches`. Do not use
  those strings there. `tools/` and `test/` are not scanned.
- GPU and SDL calls run on the initial domain. Parallel work uses
  `Rays_math.Parallel` only, and every parallel change ships a one-domain
  against eight-domain equality test.
- A task that finds it needs new Lisp syntax stops.
- Commit messages are imperative sentence case with an area prefix:
  `flow_ir: Lower scalar residuals to core`.

### Promotes and goldens

| File | When a task may promote or rewrite it |
| --- | --- |
| `tools/api_manifest/api_stable.json` | After an intended `.mli` change, as a separate commit |
| `sketches/dune.rays.inc` | Only in a task that adds or removes a `.rays` sketch |
| `lib/sop_catalog/flow_manifest.sexp` | Never in this plan |
| `lib/flow_gpu/goldens/*.metal` | Never. They prove kernels did not change |
| Snapshots this plan adds (P2.1, P3.1) | Never after the task that creates them |
| PXUI parity goldens (`RAYS_UPDATE_FIXTURES`) | Never |

### The switch pattern

Every replacement lands behind a switch modelled on
`Flow.Eval.compile_residuals` (`lib/flow/eval.ml:302`, exported at `:1260`): a
`bool ref`, default off, exported through `Private`. Tests run both settings. A
later task flips the default; a still later task, after a review, deletes the
old path and the switch.

### Where new code goes

| New thing | Location | May depend on |
| --- | --- | --- |
| Core types, lowering, native executor | `lib/flow_ir/core.ml`, `core_lower.ml`, `core_exec.ml` (add to `modules` in `lib/flow_ir/dune`) | `flow`, `param`, `rays_math`; `scene_command` from P3.3b |
| Run-time drawing contract | `lib/scene_command/marks.ml` | Nothing; `scene_command` has no dependencies |
| Marks to `Rays.Scene` adapter | `lib/rays_editor/marks_scene.ml`, listed in `private_modules` | What `rays_editor` already has |
| WGSL and JavaScript printers | new library `lib/flow_web` | `flow`, `flow_ir`, `param`, `rays_math`, `scene_command` |
| Browser runtime | `tools/web/runtime/*.js`, `*.wgsl` | Not OCaml; copied into the export |
| Export command | `tools/lisp/rays.ml` (`rays web`) | As `tools/lisp/dune` allows; no HTTP anywhere |
| Benchmarks | `tools/bench_core.ml`, `tools/bench_marks.ml`, added to `bench-build` in `tools/dune` | |

The executor reaches `flow` through the existing `execute` hook on the evaluator
state (`lib/flow/eval.ml:118`, used at `:462`). Do not add a global hook and do
not make `flow` depend on `flow_ir`.

## Seams in today's code

Line numbers are from `923dcecf`. A task that finds a line has moved searches
for the named function.

### Per-frame execution

| Seam | Where | What a task needs to know |
| --- | --- | --- |
| Closure tier | `lib/flow/eval.ml:313` `fast_of`, `:337` `compile`, `:418` `force_res` | `compile` handles literals, `Time`, a 3-vector, field reads, scalar value ops, `Let` over constants, `If`, and `Sum`/`For` over a constant list of at most 4,096. Everything else raises `Unsupported` (`:296`). Budget of 2,048 nodes |
| Its switch | `eval.ml:302` `compile_residuals`; field `compiled_residuals` (`:119`) | The pattern to copy for `core_residuals` |
| Its error rule | `eval.ml:428-432` | Any run-time failure reruns the tree walker (`slow ()`), so the diagnostic and span are always the walker's. The core executor keeps this rule |
| Its exactness rule | `lib/flow/test_workspace_eval.ml:435-487` | Closure results are bit-identical to the walker: same operators, same order, same coercions; `Sum` of an empty list is `Int 0` |
| Executor hook | `eval.ml:118`, `:462` | How `flow_ir` injects an executor into `flow` without a dependency |
| Residual view | `eval.ml:1219-1228` `Private.residual_view` | Term, bindings and captures of a residual |
| Fold state | `eval.ml:170` `begin_frame`, `:666` `state_previous` | How folds are keyed and committed |
| Bounds | `eval.ml:646`, `:876` `E_ITER_BOUND`; `lib/flow/workspace.ml:882` `E_TIME_COUNT` | 4,096 everywhere |
| Kernel compiler | `lib/flow_ir/packed.ml:125` `compile_impl`, `:264-372` `expression` | Roots: `map`, `reduce`, `for`, `sum`, `fold`, `scan`, `array/sum`. Bodies: literals, references, vectors of 2 to 4, `Get`, `Let`, `If` as eager `Select`, `Cond`, `Case`, ops by `Op.packed_kind`. No records, lists, `Call_fn`, nested `fn`, `Str` or `State` |
| Its refusals | `packed.ml`: `E_PACKED_STATE`, `_FUNCTION`, `_FORM`, `_TYPE`, `_CAPTURE`, `_OPERATOR`, `_CONSTANT`, `_LIMIT`, `_LAYOUT`; `E_GPU_FORM` in `gpu_refusals` (`:43`) | `test_packed` pins these codes and their spans |
| Captures | `packed.ml:68` `captured_value`, `:215` `uniform` | A live or templated capture becomes `Uniform (i, j)`; a static one is inlined as `Const` |
| Fusion | `packed.ml:431` `fuse_maps` (called at `:416`) | Inlines `Collect` + `Zip` children of equal count or origin; register limit `Flow.Packed_ops.register_limit` (64) at `:203` |
| Block executor | `packed.ml:551` `force` | Blocks of 1,024 (`:616`), `Parallel.for_ ~chunk_size:1` for `Collect` (`:752`); sums and accumulators run sequentially in order. Non-finite raises `E_KERNEL` |
| Emitter interface | `Packed.Private.view`, `Private.inputs`; `lib/flow_gpu/emit.ml:53` | The only things the Metal emitter reads. `view` has `code`, `widths`, `output`, `uniform_widths`, `collecting` (the `zipped` and `skip` fields were removed by the refactor) |
| IR build | `lib/flow_ir/flow_ir.ml:441` `residual`, `:522` `of_evaluation` | `Packed_map` attached at `:488`; anything unsupported becomes `Reference`; display nodes become `Sink (Display kind)` (`:531`) |
| Placement | `flow_ir.ml:332` `place`; cost table `:133-160` | GPU needs a static count of at least 1,024, provenance in `approx`, no refusals, and a measured cost below the CPU's. `Cost.table` is no longer exported |
| Callers of the closure tier | `eval.ml:334`, `:428`, `:1233`, `:1260`; `flow_ir.ml:361`; `lib/flow/test_frame.ml:22-24`; `test_workspace_eval.ml:447-485` | The list the deletion task works from |

### Drawing

| Seam | Where | What a task needs to know |
| --- | --- | --- |
| Drawing lowering | `lib/rays_editor/drawing.ml:35` `render_prepared`; arms `:106-177` | A string match on node kind. Defaults for drawing operators live here, not in `Flow.Op`. The module moved here from `sketch_support`, which no longer exists |
| GPU eligibility | `drawing.ml:64`, `:127` | `draw/circles` with a `?gpu` callback, uniform radius, fill and stroke |
| Retention | `drawing.ml:19-33` (static test), `:53-54`, `:179`; `lib/rays_editor/environment.ml:69`, `:360` | A static node's scene is reused while size is unchanged |
| Callers | `lib/rays_editor/environment.ml:781-786`; `lib/rays_editor/rays_editor.ml:205-209` (`Workspace.export`, `:187`); `workspace_images.ml:31` | |
| Instance record | `lib/scene_command/shape_batch.ml:3`, `:17` | 64 bytes; kinds rect 0, circle 4, line 5 |
| Circle kernel | `lib/rays_execution/gpu_circles.ml:6`, `:63`; `lib/rays_editor/workspace_gpu.mli` `circles` | Fixed Metal source; one synchronous completion and status readback per dispatch |
| GPU vertex sources | `lib/scene_execution/scene_execution.ml:10` `register_gpu_vertices`, `:180` `resolve_gpu_vertices`, `:796`, `:869` signature, `:1228-1244` | One buffer per source, looked up by key and stamp |
| Shape shader | `lib/runtime/runtime_shaders.ml:18` `ui` | Shared with PXUI. Vertex pulling: `instance = vid/4`, reads `input + instance*64` (`:26-27`). Four vertices and six `Uint32` indices per instance, built on the CPU |
| Pipeline families | `scene_execution.ml:27` `pipeline_family`, `:260-272` `blends`, `families`, `pipeline_slot`; `lib/runtime/runtime.ml:368`, `:403`, `:426`, `:440`, `:498`, `:514` | Where a new family is declared and bound |
| Transform | `lib/rays_execution/rays_execution.ml:84` `write_affine`, `:607`, `:643` | Already a 24-byte uniform, not baked into instances. Clip is a scissor rect |
| Transform caveats | `drawing.ml:161`; `rays_execution.ml:638-640` | `draw/translate` snaps to integers. A changed batch reruns a byte comparison over all instances |
| Batching | `rays_execution.ml:616`; `scene_execution.ml:707` `coalesce_draws` | One draw per `Shapes` command at lowering; `coalesce_draws` merges later |
| Text | `lib/rays/scene.ml:166-171` `text_image` | A whole string is rasterized to an image by SDL_ttf and drawn as a quad |
| Frame facts | `lib/frame_input/frame_input.mli:2-24`; `lib/flow/op.ml:116`, `:148`, `:151` | `t`, `dt`, `frame`, `tick`, `size`, `pointer`, `buttons`, `keys`, `events`. The events list has live length |

### What the refactor at `923dcecf` removed

A task must not reach for these; the plan below replaces each.

| Gone | Replacement in this plan |
| --- | --- |
| `lib/sketch_support/` | `lib/rays_editor/drawing.ml`; new adapter in `lib/rays_editor/marks_scene.ml` |
| `specification/performance-log.md` | `specification/web-export/results.md` |
| `tools/bench_kernel.ml`, `bench_drawing.ml`, `bench_workspace_lower.ml` | `tools/bench_core.ml` (P1.13), `tools/bench_marks.ml` (P3.11). `tools/bench_gpu.ml` remains |
| `tools/workspace_parity.exe` as a command | `tools/workspace_parity.ml` is a library; `test/test_workspace_ir.ml` drives it over every workspace |
| `test/test_drawing.ml`, `lib/rays/test_shape_batch.ml`, `lib/rays_execution/test_gpu_circles.ml` | `@test/test_workspace_pixels`; `lib/rays_execution/test_dense_scene2.ml` and `test_gpu_draw.ml`; `lib/rays_editor/native_qualification/test_workspace_gpu_native.ml` |
| Library `procedural` | Renamed `sop` |

## Phase 0: core specification and census

Phase 0 writes no product code. It fixes what the core is, from a count of what
the checked-in workspaces use.

### The corpus

There are 45 `.rays` files. Fifteen have a `draw` graph, and none of those draw
graphs references a `sop` or `scene` graph. They are the web scope and the
parity corpus for every phase:

`examples/audio`, `basic`, `drawing`, `file_dialog`, `generative`, `noise`,
`particles`, `pxui`, `recursive_rectangles`; `sketches/flow_contours`,
`flow_image`, `flow_image_kernel`, `flow_particles_gpu`, `flow_shader`,
`flow_vectors`.

`flow_contours`, `flow_image` and `flow_particles_gpu` also hold `scene` or
`sop` graphs; only their draw graph is exported.

Head counts below were taken at `3baf0095` with a throwaway tokenizer; of the
fifteen files only `examples/basic` changed since (one line). P0.1 recounts.

| Finding | Evidence | Consequence for the core |
| --- | --- | --- |
| Records and field reads are everywhere | 216 dotted reads, 21 record literals, 21 `assoc` | Records are in the core, flattened to fields; keys are static |
| Lists are common and some have live length | `list` 20, `map` 16, `nth`, `count`, `concat`, `take`, `rest`, `reverse`, `filter`; frame events | A bounded list, capped at 4,096 |
| Text is computed per frame | `str` 7, `equal?` 49, `draw/text` with computed strings in 3 files | Text is a frame-level value; it never enters a region |
| State is the norm | `(state ...)` in 8 of 15 files | State slots are in the first lowering slices |
| Per-instance attributes are used | `draw/circles` with radius and fill arrays, `draw/rects` with sizes and fills, in 3 files | The mark contract carries per-instance size and colour from the start |
| Integer operators appear | `int` 30, `int/div` 19, `int/mul`, `int/and`, `int/xor`, `int/mod` | Integer width and overflow must be specified; OCaml's 63-bit `int` and JavaScript numbers differ |
| Host effects appear | `host/quit`, `host/dialog`, `host/play`, `host/save_png`, `audio/synth` | Each is an intrinsic with a per-target legality entry |

Smallest end-to-end target: `sketches/flow_vectors` (17 lines). Next:
`examples/basic` (state, frame facts, `str`, `draw/text`). Richest frame
program: `examples/drawing` (185 lines; an event-driven state machine over
records). Richest kernels: `sketches/flow_shader`.

### Tasks

| Id | M | Needs | Files | Do | Gate |
| --- | --- | --- | --- | --- | --- |
| P0.1 | H | | `tools/core_census.ml`, `tools/dune` | A tool that parses every `.rays` under `examples/`, `sketches/` and `specification/` with `Flow.Syntax.parse` and prints (a) each file's graph contexts and whether its draw graphs reach a `sop` or `scene` graph through `(ref ...)`, (b) the count of each head symbol inside `draw`, `host`, `image` and `value` graphs and top-level `defn` and `defmacro` bodies. Sorted, deterministic | `dune build tools/core_census.exe && _build/default/tools/core_census.exe` lists the 15 files above as eligible |
| P0.2 | S | P0.1 | `specification/core.md` | The specification: the two levels; value types; control forms; the region form (state that it is today's `Packed.instruction` set); state slots; draw statements; intrinsics. One table with a row for every head P0.1 prints and a column `rule`: `primitive`, `compiler rule` (naming the primitives it expands to), `region`, `intrinsic`, or `refused on web` | P0.R |
| P0.2a | S | P0.2, P0.7 | `specification/core.md` | P0.R repair: state the remaining leaf behavior without requiring a second reader to infer it from source: `color/parse` and both channel-conversion paths (including Apple-Silicon signed integer conversion for very large finite channels), packed-array defaults/index/count/layout checks, recursive `coerce`, and the `value/hsv` / `value/lerp` compiler expansions. Preserve the 54-leaf inventory; validate edge examples against the native bodies | P0.6 and native edge probes; P0.R |
| P0.3 | S | P0.2 | `specification/core.md` | Numeric model: frame-level floats are 64-bit; region floats are 32-bit on GPU and 64-bit on CPU as today; integer width and overflow for `int`, `int/div`, `int/mul`, `int/mod`, `int/and`, `int/xor`, from their bodies in `lib/flow/op.ml`, and what a JavaScript target must do to match | P0.R |
| P0.4 | S | P0.2 | `specification/core.md` | List model, from `E_TIME_COUNT` (`lib/flow/workspace.ml:882`) and `E_ITER_BOUND` (`eval.ml:646`, `:876`): which lists have static length (unrolled at lowering) and which are bounded dynamic lists; the element layout for a list of records | P0.R |
| P0.5 | S | P0.2 | `specification/core.md` | Text model, from `Value.show_with` (`lib/flow/value.ml:119`) and the float spelling rule (`specification/flow.md:693`, `:1455`). Specify `show` and `equal?` precisely enough to implement twice | P0.R |
| P0.6 | H | P0.2 | `tools/lisp/test/core_census.t`, `tools/lisp/test/dune` | A cram test: run the census tool, extract its head list, fail if any head has no row in the table of `specification/core.md` | `_build/default/tools/check.exe @tools/lisp/test/runtest` |
| P0.7 | H | P0.2 | `README.md`, `AGENTS.md`, `specification/flow.md` | Link `specification/core.md` from the README specification list (lines 213-224), the design-notes sentence in `AGENTS.md`, and `flow.md` section 14 | `git diff --check` |
| P0.R | R | P0.3 to P0.7, P0.2a | | Every census head has a rule; the primitive list is written out and counted; no rule says "same as OCaml" without stating the behaviour. Record the primitive count at the top of `core.md`. If the count is large enough that two targets would each be a second implementation, stop the loop and report | P0.6 passes |

## Phase 1: lowering and native executor

Phase 1 builds the frame level and runs it natively, slice by slice, behind a
switch, until the closure tier has nothing left to do. The oracle for every
slice is the tree walker: results are bit-identical, as the closure tier's are.

| Id | M | Needs | Files | Do | Gate |
| --- | --- | --- | --- | --- | --- |
| P1.1 | S | P0.R | `lib/flow_ir/core.ml`, `.mli`, `dune`, `test_core.ml` | The frame-level types from `specification/core.md`: expressions, statements, a program with numbered locals and state slots, and a `Region_ref` placeholder holding an existing `Packed.t`. A deterministic printer `Core.pp`. Test: three hand-built programs against inline expected strings | `@lib/flow_ir/runtest` |
| P1.2 | S | P1.1 | `lib/flow_ir/core_lower.ml`, `.mli`, `test_core_lower.ml` | `lower : Flow.Eval.residual -> (Core.program, string) result` for exactly the forms `Flow.Eval.compile` handles (`eval.ml:337` onward). Anything else returns `Error` with the form's name. Read captures through `Eval.Private.residual_view`. Test: `pp` expectations for the expressions in `test_workspace_eval.ml:435-487` | `@lib/flow_ir/runtest` |
| P1.2a | S | P1.9 | `core_lower.ml`, tests | Complete operator coverage within the initial forms: audit every `Context.value` scalar operator accepted by the old closure compiler, and add the remaining shared expansions (HSV, list convenience operations, packed sum, held-key/button membership). Reuse the list/frame/region lowering from slices D to F. Preserve named refusal for reached values outside the core layout/size contract; do not add target-side operator dispatch | `@lib/flow_ir/runtest`; compare successful lowering and native bits against every old-closure operator case |
| P1.3 | S | P1.2 | `lib/flow_ir/core_exec.ml`, `.mli`, `flow_ir.ml`, `lib/flow/eval.ml`, `.mli` | A direct interpreter of frame-level core returning `Flow.Eval.value`, using the walker's operator bodies and coercions. Add `Flow.Eval.Private.core_residuals : bool ref`, default `false`. When set, a residual is offered to the executor through the `execute` hook before the closure tier; on `Error` or any exception, fall through unchanged | `@lib/flow/runtest` and `@lib/flow_ir/runtest`, no test changed |
| P1.4 | H | P1.3 | `lib/flow_ir/test_core_parity.ml`, `dune` | For each fixture and expression in `test_workspace_eval.ml:435-487`, force with `core_residuals` on, off, and with `compile_residuals` off; assert bit-equal values (copy `same_bits`) and equal diagnostics on the error cases | `@lib/flow_ir/runtest` |
| P1.5 | S | P1.4 | core types/lowering/execution, Flow callable/bypass hooks, packed/IR bypass consumers, tests | Slice B: vectors of 2 and 4; `Cond`, `Case`; `Let` with destructuring patterns; `Call_fn` and local `Fn` by inlining the call site's checked body; `Expanded`; `Bypass`. `pp` and parity cases for each. Share the authored positional bypass selector across checking, evaluation and compilers | `@lib/flow/runtest`, `@lib/flow_ir/runtest` |
| P1.6 | S | P1.5 | `core_lower.ml`, Flow aggregate equality/coercion paths, tests, `results.md` | Slice C: records (`Record`, `Get`, `Assoc`) flattened to locals; text, `Str` through the `show` primitive, `equal?`. Resolve reactive captured data at the equality/coercion boundary in both native tiers | `@lib/flow/runtest`, `@lib/flow_ir/runtest` |
| P1.6a | S | P1.7 | record-fold benchmark, core lowering/execution, `results.md` | Repair the allocation regression measured during Slice C: prepare and force the same record fold through Executor's core path, keeping record accumulators as field locals. Reuse the structured loops from P1.7. Keep the interpreter/direct native checks | Focused Flow/IR tests; native record-fold median at most 1.1 times the P1.5 baseline time and allocations at most 1.05 times the baseline bytes, measured on the same machine |
| P1.7 | S | P1.6 | same | Slice D: lists per `core.md`: `List_lit`, `range`, `nth`, `count`, `concat`, `take`, `rest`, `reverse`; `Hof` `Map`, `Filter`, `Reduce`, `Sort_by`; `Loop` `For`, `Fold`, `Scan`, `Sum` with `skip`. Keep `E_ITER_BOUND` at 4,096 and `Sum` of an empty list as `Int 0` | same |
| P1.8 | S | P1.7 | same, `lib/flow/eval.ml` | Slice E: `State` as slots, keeping the keys of `state_previous` (`eval.ml:666`) and `begin_frame` (`:170`) so `fork_state` and `transaction` still work. Frame operators and `frame/input` events as a bounded list of records | `@lib/flow/runtest` including `test_state` and `test_frame`, switch on and off |
| P1.9 | S | P1.8 | same | Slice F: packed-array operators outside regions (`array/range`, `array/vec3`, `array/nth`, `array/count`, `array/slice`, `array/concat`) as primitives; a packed `map`, `for`, `sum`, `fold` or `reduce` becomes a `Region_ref` that calls `Packed.force` unchanged | `@lib/flow_ir/runtest` including `test_frame_kernel` |
| P1.10 | S | P1.9 | `core_exec.ml` | The error rule: any failure inside the executor reruns the tree walker and returns its diagnostic, as `eval.ml:428-432` does. Parity cases for a division by zero, an out-of-range `nth`, a non-finite result | `@lib/flow_ir/runtest` |
| P1.11 | H | P1.10 | `tools/workspace_parity.ml`, `.mli` | In `Workspace_parity.check`, add a pass that repeats the existing IR-against-reference comparison with `core_residuals` set, restoring it afterwards. `test/test_workspace_ir.ml` then covers every workspace with no change | `_build/default/tools/check.exe @test/test_workspace_ir` |
| P1.12 | H | P1.11 | `tools/workspace_parity.ml`, `test/test_workspace_ir.ml` | Coverage report under an environment variable `RAYS_CORE_COVERAGE`: per workspace, residuals lowered against residuals that returned `Error`, with the form names. Paste the table for the 15 eligible workspaces into `results.md` | Zero `Error` for all 15, or the review adds slice tasks |
| P1.12a | S | P1.12 | `core_lower.ml`, `core_exec.ml`, `lib/flow` value/eval accessors, tests, `results.md` | Repair: lower the 46 residuals P1.12 found refused (`packed_higher_order`, `noise3`, `ref`, `color/hsl`). A packed `map` or `filter` a region refuses is built element by element (`sequence/build` with a width, no list cap); `noise3` is its primitive; `color/hsl` and `color/gradient` expand per `core.md`; a `ref` without overrides inlines the graph in its cached instance. Parity cases beside the existing ones | Zero refused for the 15 workspaces; `@lib/flow/runtest`, `@lib/flow_ir/runtest`, `@test/test_workspace_ir` |
| P1.13 | H | P1.11 | `tools/bench_core.ml`, `tools/dune`, `results.md` | A new bench printing three rows for one scalar residual (the expression of the `Closure` row in `flow_ir.ml:133-160`): tree walker, closure tier, core. Add it to `bench-build`. Record the rows | Rows printed; entry present |
| P1.14 | S | P1.13 | `core_exec.ml` | Only if the core row is more than 1.1 times the closure row: compile a core program once into OCaml closures instead of interpreting it, keeping the interpreter as the test reference. Re-record | Core at most 1.1 times closure; P1.4 and P1.11 pass |
| P1.14a | S | P1.14 | `lib/flow/eval.ml`, `.mli`, `core_exec.ml`, `flow_ir.ml` | Repair: the evaluator keeps the executor's prepared program on the residual (an owner-checked slot filled by the hook on first offer), so a repeat force skips the cached-program lookup, the state snapshot, the second handler and optional-argument boxing; no `flow_ir` dependency, no global hook; the error rule is unchanged | Core at most 1.1 times closure on `tools/bench_core.exe`; `@lib/flow/runtest`, `@lib/flow_ir/runtest` |
| P1.14b | S | P1.14a | `core_exec.ml` | Repair: close the last 0.15 of the scalar ratio inside the postfix tier (superinstructions for constant or time operands, fewer dispatches, no per-call stack allocation); the hook cost is already gone | Core at most 1.25 times closure on `tools/bench_core.exe` (review threshold), ideally 1.1; `@lib/flow_ir/runtest` |
| P1.15 | S | P1.14 | `lib/flow/eval.ml` | Flip `core_residuals` to default `true` | All default tests, and `@test/test_workspace_ir` |
| P1.R | R | P1.15, P1.2a, P1.6a | | Run `--ship` and `@runtest-native`. Confirm P1.12 shows full coverage. Re-list the callers of `fast_of`, `closure_available` and `compile_residuals` for P1.16 | All green |
| P1.16 | S | P1.R | `lib/flow/eval.ml`, `.mli`, callers | Delete `compile`, `cnode`, `fast`, `fast_of` and both switches. Keep the `Closure` tier name in `Flow_ir.tier`, now meaning a core frame program, so placement tests are unchanged. Promote the API manifest in a second commit | `--ship` |

## Phase 2: kernels on the core

A region becomes a first-class part of the core, so `packed.ml` no longer walks
terms. Not one emitted instruction may change: the three Metal goldens and a new
instruction-level snapshot are the proof.

| Id | M | Needs | Files | Do | Gate |
| --- | --- | --- | --- | --- | --- |
| P2.1 | H | P1.R | `lib/flow_ir/test_region_snapshot.ml`, `lib/flow_ir/snapshots/*.txt`, `dune` | Before any change: for every program built in `lib/flow_ir/test_packed.ml` and `lib/flow_gpu/test_program.ml` (`fixtures`, `vector_fixtures`), and every kernel compiled from `examples/particles`, `sketches/flow_shader`, `flow_particles_gpu` and `flow_image_kernel`, print `Packed.Private.view` (`code`, `widths`, `output`, `uniform_widths`, `collecting`) and `Packed.gpu_refusals` to a text file and commit them. The test regenerates and diffs. Move `test_program.ml` where both tests can link it; do not copy it | `@lib/flow_ir/runtest` |
| P2.2 | S | P2.1 | `lib/flow_ir/core.ml`, `.mli` | Replace `Region_ref` with a real region: iteration (`Zip`, `Product`), sources, result (`Collect`, `Sum`, `Accumulate`), `skip`, a body of region-level expressions, captured uniforms, count and origin. Region expressions reuse `Flow.Packed_ops.binary` and `unary`. Extend `pp` | `@lib/flow_ir/runtest` |
| P2.3 | S | P2.2 | `core_lower.ml`, tests | Lower region roots and bodies to the new form, covering exactly what `Packed.expression` accepts (`packed.ml:264-372`). Refuse the rest with the same `E_PACKED_*` codes and spans | `test_packed` refusal cases pass against the new lowering |
| P2.4 | S | P2.3 | `lib/flow_ir/packed.ml` | `Packed.of_region : Core.region -> (t, Flow.Diagnostic.t list) result` builds the register program from a region instead of from terms. Behind a switch `core_regions`, route `compile_impl` through `core_lower` then `of_region` | P2.1 snapshots identical with the switch on |
| P2.5 | S | P2.4 | `packed.ml`, `core_lower.ml` | Captures and templates: `captured_value`, the live-or-dynamic `Uniform` rule, `compile_template` and `rebind` on regions. The template counts in `test_frame_kernel` must not change | `@lib/flow_ir/runtest` with the switch on |
| P2.6 | S | P2.5 | `packed.ml` or new `core_fuse.ml` | Fusion on regions: port `fuse_maps` (`packed.ml:431`) to combine a region with its `Collect` + `Zip` sources of equal count or origin, with the same common-subexpression rule and register limit | P2.1 snapshots identical with the switch on |
| P2.7 | H | P2.6 | none | Run with `core_regions` on and record: `@lib/flow_ir/runtest`, `@lib/flow_gpu/runtest` (Metal goldens), `@runtest-native` | All pass; goldens untouched in `git status` |
| P2.8 | H | P2.7 | `tools/bench_core.ml`, `results.md` | Add kernel rows: one `map` at 1,024 and 1,000,000, a fused pair of maps, a `sum` loop; each with the switch off and on, at 1 and 8 domains. Record `tools/bench_gpu.exe` too | No row regresses by more than 5 percent; otherwise a repair task |
| P2.9 | S | P2.8 | `packed.ml` | Flip `core_regions` to default on | `--ship` and `@runtest-native` |
| P2.R | R | P2.9 | | Snapshots and goldens are byte-identical in history since P2.1. List what in `packed.ml` is now dead | Phase gate |
| P2.10 | S | P2.R | `packed.ml`, `.mli` | Delete `expression`, `source_program` and the term-walking half of `compile_impl`; keep `force`, `Private.view`, `Private.inputs`, `gpu_refusals`. Promote the API manifest separately | `--ship` and `@runtest-native` |

## Phase 3: drawing on the core

The string match in `Rays_editor.Drawing` becomes lowering rules that emit draw
statements, and the run-time contract those statements fill is defined. The
picture must not change by one byte: the renderer is untouched in this phase and
an adapter feeds it.

| Id | M | Needs | Files | Do | Gate |
| --- | --- | --- | --- | --- | --- |
| P3.1 | H | P1.R | `test/test_draw_snapshot.ml`, `test/draw_snapshots/*`, `test/dune` | Before any change: for each of the 15 eligible workspaces, at fixed-step frames 0, 1 and 30 with a pinned size and pointer, write the SHA-256 of `Scene_command.Render_ir.serialize` of the draw graph's scene, built with `Drawing.prepare` and `Drawing.render_prepared` as `Rays_editor.Workspace.export` does (`rays_editor.ml:187-209`). No GPU callback. The test regenerates and diffs | `_build/default/tools/check.exe @test/runtest` |
| P3.2 | S | P0.R | `specification/marks.md` | The run-time contract: a frame is an ordered mark table. An entry is a mark set (kind: rect, circle, line, point; count; per attribute a constant or a buffer reference with width; attributes: position, size or radius, fill, stroke, stroke width), indexed path geometry produced by the existing pure `Scene_command.Shape2` helpers, an image, a text run, or a push or pop of view transform or clip. The geometry entry is required for P3.6's rounded-rectangle/polygon/polyline native serialization and pixel parity; preserve fill/stroke order and do not approximate paths by overlapping marks. Units (logical points), colour encoding, defaults taken from `drawing.ml`, the cap on count, painter order | P3.R |
| P3.3 | S | P3.2 | `lib/scene_command/marks.ml`, `.mli`, `test_marks.ml`, `dune` | The contract as pure data with no dependencies: types, a validity check, and a stable little-endian `serialize` in the style of `Render_ir.serialize`. A buffer reference is an abstract integer id plus count and width | `@lib/scene_command/runtest` |
| P3.3b | H | P3.3 | `lib/flow_ir/dune`, `test/dependency_gate.ml`, `AGENTS.md`, `specification/backend.md` | Boundary change: `flow_ir` may depend on `scene_command` (a leaf with no dependencies). Add it to `flow_ir`'s entry in `only`, to the Dependency rules sentence and Libraries row in `AGENTS.md`, and to `backend.md`. `flow_ir` still never reaches `rays`, the `gpu` group or any UI library | `_build/default/tools/check.exe @test/runtest` |
| P3.4 | S | P3.3b, P1.R | `lib/flow_ir/core.ml`, `core_lower.ml`, tests | Draw statements in the core and lowering rules, slice 1: `draw/background`, `draw/rect`, `draw/circle`, `draw/line`, `draw/point`, `draw/merge`, `draw/translate`, `draw/rotate`, `draw/scale`. Move each default out of `drawing.ml` into its rule (fill white when neither fill nor stroke is given; stroke width 1; `draw/translate` rounds to integers; `E_DRAW_RANGE`) | `pp` expectations; `@lib/flow_ir/runtest` |
| P3.5 | S | P3.4 | same | Slice 2: `draw/circles`, `draw/rects`, `draw/lines`, `draw/points` with constant or per-instance attributes, keeping `E_DRAW_LENGTH` and `E_DRAW_INPUT` | same |
| P3.6 | S | P3.5 | same | Slice 3: `draw/text`, `draw/image`, `draw/rounded_rect`, `draw/polygon`, `draw/polyline`; `E_DRAW_KIND` for anything else | same |
| P3.7 | S | P3.6 | `lib/flow_ir/core_exec.ml` | The executor produces a `Scene_command.Marks.t` for a draw graph. CPU attribute arrays are registered as buffer ids in a per-frame table the caller owns | `@lib/flow_ir/runtest` |
| P3.8 | S | P3.7 | `lib/rays_editor/marks_scene.ml`, `.mli`, `dune` | Adapter from `Marks.t` to `Rays.Scene`, building `Shape_batch` records exactly as `drawing.ml:83-104` does, so the renderer sees the same commands. No renderer change. List it in `private_modules` | Unit test comparing `Render_ir.serialize` for hand-built marks |
| P3.9 | S | P3.8 | `lib/rays_editor/drawing.ml`, `.mli` | Behind a switch `core_drawing`, `render_prepared` runs the core path and the adapter instead of its own arms. Keep the retention rules (`drawing.ml:19-33`, `:53-54`, `:179`) | P3.1 snapshots identical with the switch on |
| P3.10 | S | P3.9 | `drawing.ml`, `marks_scene.ml`, `lib/rays_editor/workspace_gpu.ml` | The GPU path through the contract: a circle mark set whose position is a GPU region output and whose other attributes are constants resolves to the existing `Workspace_gpu.circles` token (`drawing.ml:127`). Nothing else changes yet | `@runtest-native` (includes `test_workspace_gpu_native`) and `@test/test_workspace_pixels`, switch on |
| P3.11 | H | P3.10 | `tools/bench_marks.ml`, `tools/dune`, `results.md` | A new bench: `Drawing.render_prepared` for `draw/circles` and `draw/rects` at 10,000, 100,000 and 1,000,000, static and animated, switch off and on; wall time and allocated bytes per frame. Add to `bench-build`. Record | No row regresses by more than 5 percent; otherwise a repair task |
| P3.12 | S | P3.11 | `drawing.ml` | Flip `core_drawing` to default on | `--ship` and `@runtest-native` |
| P3.R | R | P3.12 | | P3.1 snapshots unchanged since they were taken. `specification/marks.md` matches `marks.mli` | Phase gate |
| P3.13 | S | P3.R | `drawing.ml`, `.mli` | Delete the per-kind arms and the switch; `Drawing` keeps only retention and the call into the core path. Update `specification/flow.md`, "Frame data and native 2D drawings" (line 1340) | `--ship` and `@runtest-native` |

## Phase 4: draw optimization and target placement

This is where pictures get faster: mark attributes are read by the shader
straight from kernel outputs, kernels gain several outputs, and placement learns
about marks and targets. Some tasks here change the renderer, so each ships with
a pixel-parity test against the path it replaces.

| Id | M | Needs | Files | Do | Gate |
| --- | --- | --- | --- | --- | --- |
| P4.1 | H | P3.R | `tools/bench_marks.ml`, `results.md` | Baseline rows at 100,000 and 1,000,000: circles with per-instance radius and fill, rects with per-instance size and fill, and a static batch under a changing `draw/translate`. Record | Rows printed; entry present |
| P4.2 | S | P4.1 | `lib/runtime/runtime_shaders.ml`, `lib/runtime/runtime.ml`, `lib/scene_execution/scene_execution.ml`, `.mli` | A new pipeline family `Marks`, leaving the `Ui` family and its shader untouched (`test_ui_parity` pins them). Its vertex stage reads position, size and colour from three separate buffers by `vid / 4` and applies the same affine uniform as `Ui`. Its fragment stage is the rect, circle and line cases of the `Ui` shader. Touch `pipeline_family` (`scene_execution.ml:27`), `families` and `pipeline_slot` (`:261-272`), and every `Ui` arm in `runtime.ml` (`:368`, `:403`, `:426`, `:440`, `:498`, `:514`). Read `lib/runtime/AGENTS.md` and `lib/ogpu/AGENTS.md` first | `@check`; exercised by P4.4 |
| P4.3 | S | P4.2 | `scene_execution.ml`, `.mli`, `lib/rays_execution/rays_execution.ml`, `.mli` | A registered GPU vertex source may carry several buffers: `register_gpu_vertices` (`scene_execution.ml:10`), `resolve_gpu_vertices` (`:180`), the bindings appended at `:1228-1244`, and the signatures at `:796` and `:869` covering every buffer id | `@lib/scene_execution/runtest`, `@lib/rays_execution/runtest` |
| P4.4 | S | P4.3 | `lib/rays_execution/test_marks_native.ml`, `dune` | Pixel parity, native: the same marks through `Shape_batch` and through the `Marks` family at 63, 64, 65 and 100,000 instances, for rect, circle and line, with and without stroke. Model it on `lib/rays_execution/test_dense_scene2.ml` | `@runtest-native` |
| P4.5 | S | P2.R | `lib/flow_ir/packed.ml`, `.mli`, `core.ml`, `lib/flow_gpu/emit.ml`, `run.ml` | Several outputs per region: `Private.view.output` becomes a list of output slot arrays, each with its width. `Packed.force` returns one packed array per output. The Metal emitter declares one output buffer per output. A single-output program prints exactly as before | P2.1 snapshots identical after a mechanical reformat stated in the commit; the three Metal goldens identical; a new golden `two_outputs.metal`; a `test_packed` case at 1 and 8 domains |
| P4.6 | S | P4.5 | `lib/flow_ir/flow_ir.ml` | Marks as IR nodes: replace `Sink (Display kind)` for mark sets with a node that names its attribute edges. Every pass (`share`, `hoist`, `prune`, `fuse`, `place`) carries it; provenance is the drawing call's path | `test_ir` unchanged plus new cases |
| P4.7 | S | P4.6 | `flow_ir.ml` or `core_fuse.ml` | Attribute fusion: when the regions feeding one mark set's attributes have equal count or origin, fuse them into one region with several outputs. Decline at the register limit; never fail | New `test_ir` cases; a `bench_core` row comparing fused against separate kernels at 1,000,000, recorded |
| P4.8 | S | P4.7 | `flow_ir.ml` | Rate split per attribute: an attribute whose region is static is computed once and its buffer kept; only frame-rate attributes rerun. Build on `hoist` and the `rate` field | Test: static positions with animated colour run the position region once over 10 frames (count with `Flow_ir.Profile`) |
| P4.9 | S | P4.4, P4.8 | `lib/rays_editor/marks_scene.ml`, `workspace_gpu.ml`, `rays_execution.ml` | Route a mark set whose attributes are GPU region outputs to the `Marks` family with those buffers bound directly. CPU attributes are uploaded to buffers of the same layout. `gpu_circles` is no longer called | P4.4, `@runtest-native` and `@test/test_workspace_pixels`. P3.1 snapshots are not the oracle here; if they change, the commit says why and the review decides |
| P4.10 | S | P4.9 | `rays_execution.ml` | A transform change must not redo CPU work on an unchanged batch: keep the transform out of the instance comparison (`:638-640`) and out of the mesh identity, so a new affine rewrites 24 bytes only. Keep integer snapping of `draw/translate` | The P4.1 moving-transform row at 1,000,000 improves and is recorded; allocations per frame do not scale with count |
| P4.11 | S | P4.9 | `rays_execution.ml`, `scene_execution.ml` | Batch merging: neighbouring mark sets with the same family, blend and clip become one draw, painter order kept. Read `coalesce_draws` (`scene_execution.ml:707`) first; extend it if it already covers the case | Pixel parity with interleaved sets; draw-call count asserted |
| P4.12 | S | P4.6 | `flow_ir.ml`, `.mli` | `place` takes a `target`: a legality predicate over nodes and a cost table. The native target reproduces today's decisions exactly. Mark sets are placed like kernels: GPU when every attribute region is GPU-legal | `test_ir` placement cases unchanged; new cases for a test-only target that forbids readback |
| P4.13 | H | P4.9 | `results.md` | Re-record the P4.1 rows | Per-instance circles and rects at 1,000,000 are within 2 times the uniform-style GPU row |
| P4.R | R | P4.13 | | Run `--ship`, `@runtest-native`. Decide from the P4.13 numbers whether a culling task is warranted; this plan includes none by default | Phase gate |
| P4.14 | H | P4.R | `lib/rays_execution/gpu_circles.ml`, `.mli`, `dune`, `test_gpu_draw.ml`, `rays_execution.ml`, `.mli`, `lib/rays_editor/workspace_gpu.ml`, `.mli`, `drawing.mli`, `environment.ml` | Delete `gpu_circles`, its registration (`rays_execution.ml:42`), `Workspace_gpu.circles` and the `?gpu` callback (`environment.ml:785`). Promote the API manifest separately | `--ship` and `@runtest-native` |

## Phase 5: web back ends

Phase 5 adds printers and nothing else: the core, regions and the mark contract
exist and native runs them. The JavaScript primitives are the only place
semantics are written a second time.

| Id | M | Needs | Files | Do | Gate |
| --- | --- | --- | --- | --- | --- |
| P5.1 | H | P4.R | `lib/flow_web/dune`, `flow_web.ml`, `.mli`, `test/dependency_gate.ml`, `AGENTS.md`, `specification/backend.md`, `specification/metal.md` | New empty library `flow_web`. In the gate: `rules` (never reaches `rays`, `sop`, `rdk`, `flow_sop`, `flow_gpu`, `rays_editor`, `pxui`, `pxui_shell`, `pxui_graph`, `editor_document`, `sop_catalog`, or the `gpu` group), `only` (`flow`, `flow_ir`, `param`, `rays_math`, `scene_command`), `upper`, and an injected-edge check beside the others (`:301-340`). Add its row to the Libraries table. In `backend.md` and `metal.md` (line 6), one sentence each: web export is a separate compiler target, not a fallback | `_build/default/tools/check.exe @test/runtest` |
| P5.2 | S | P5.1 | `lib/flow_web/wgsl.ml`, `.mli` | `Wgsl.kernel : Flow_ir.Packed.t -> (wgsl, Flow.Diagnostic.t) result`, mirroring `lib/flow_gpu/emit.ml` case by case over `Private.view`. Differences: inputs and outputs are `var<storage>` arrays of `f32`; constants and uniforms use `bitcast<f32>`; the finite check is an exponent-bits test; the status word is `atomic<u32>`; `@workgroup_size(64)`; division, `mod`, `pow` and `sqrt` keep the guards `emit.ml` has. Return the same binding interface and uniform layout | `@check` |
| P5.3 | H | P5.2 | `lib/flow_web/test_wgsl.ml`, `goldens/*.wgsl`, `dune` | Goldens for the programs behind `arithmetic.metal`, `noise.metal`, `select.metal` and `two_outputs.metal`, from the shared `test_program.ml` | `@lib/flow_web/runtest` |
| P5.4 | S | P5.2 | `wgsl.ml`, `lib/flow_ir/flow_ir.ml` | Web legality for regions, as diagnostics with spans: too many storage buffers in one kernel; too many elements in one dispatch; any in-frame readback (`Gpu_readback`, `(exact x)` over a GPU producer). The two numeric limits are not taken from memory: the task reads them from the WebGPU specification's default limits and cites the section in a comment. Expose it as the `legal` part of a `web` target for `place` | `@lib/flow_web/runtest`, one refusing case each |
| P5.5 | S | P5.1 | `lib/flow_web/js.ml`, `.mli` | `Js.frame : Flow_ir.Core.program -> string`: the frame level as one JavaScript module exporting `frame(input, state, rt)`. Structured control maps one to one. Locals are `let`. Primitives are calls on `rt`. Draw statements append to `rt.marks`. State slots are `state[i]`. Emit a side table from statement index to source span | `@check` |
| P5.6 | H | P5.5 | `lib/flow_web/test_js.ml`, `goldens/*.js` | One golden per core construct, from the `pp` fixtures of phases 1 and 3, plus goldens for `sketches/flow_vectors` and `examples/basic` | `@lib/flow_web/runtest` |
| P5.7 | S | P5.5 | `js.ml` | `Js.region : Flow_ir.Packed.t -> string`: a function `(inputs, uniforms, outputs, start, stop)` over typed arrays for one block range, for regions placed on the CPU. Same instruction cases as `wgsl.ml`, 64-bit floats | Goldens added to P5.6 |
| P5.8 | S | P0.R | `tools/web/runtime/prims.js`, `tools/core_fixtures.ml`, `tools/dune` | The primitive library named in `specification/core.md` and nothing more. Alongside, an OCaml tool that writes, for each primitive, a JSON file of inputs and the native results | `dune build tools/core_fixtures.exe` |
| P5.9 | H | P5.8 | `tools/lisp/test/web_prims.t`, `tools/web/test/prims.mjs` | A cram test that runs the fixtures through `prims.js` under `node` and compares: exact for integers, text and booleans; within 1e-12 relative for floats. Skip with a clear message if `node` is absent | `@tools/lisp/test/runtest` |
| P5.10 | S | P5.4, P5.7 | `tools/lisp/lisp.ml`, `tools/lisp/dune`, `specification/flow.md` section 12 | Subcommand `rays web FILE -o DIR`, beside `check`, `ml`, `source`, `dune`, `fmt` (`lisp.ml:103-109`): check, run the static stage with graph inputs at their defaults, lower, place for the web target, then write `program.js`, `kernels.wgsl`, `data.bin`, `program.map`, `index.html` and copy `tools/web/runtime/`. A refusal prints the diagnostic with its span and exits non-zero. Output is byte-deterministic | `dune build tools/lisp/lisp.exe` |
| P5.11 | H | P5.10 | `tools/lisp/test/web.t` | Cram test modelled on `test/lisp_build.t`: export `sketches/flow_vectors`, list the files, check a second run is byte-identical, and check a workspace whose draw graph references a `sop` graph is refused with a span | `@tools/lisp/test/runtest` |
| P5.12 | S | P5.10 | `tools/web_parity.ml`, `tools/web/test/frames.mjs`, `tools/dune` | Frame parity without a browser: export a workspace, run `frame` under `node` for 120 fixed-step frames with CPU regions only, and dump each frame's mark table as JSON. The OCaml side dumps the same from the core executor. Compare attributes within 1e-6 and structure exactly | `_build/default/tools/web_parity.exe sketches/flow_vectors examples/basic` passes |
| P5.13 | H | P5.12 | `results.md` | Run P5.12 over all 15 eligible workspaces and record the table: passes, refusals with their diagnostic codes, failures | Every workspace passes or is refused with a span; failures become repair tasks |
| P5.13a | S | P5.13 | `lib/flow_ir/core_draw.ml` (or the drawing lowering) | Repair: lift or split the 2,048-node residual budget so `examples/drawing` lowers (`E_WEB_LOWER`) and passes `tools/web_parity.exe` | `_build/default/tools/web_parity.exe examples/drawing` passes |
| P5.R | R | P5.13 | | Count lines in `prims.js` and compare the primitive list with `specification/core.md`. Anything in `prims.js` that is not a listed primitive is a second implementation and moves into a lowering rule | Phase gate |

## Phases 6 and 7: presenter, live loop, demonstration

Phase 6 is the only hand-written JavaScript of any size, written once for every
sketch. Phase 7 produces the numbers for the paper.

| Id | M | Needs | Files | Do | Gate |
| --- | --- | --- | --- | --- | --- |
| P6.1 | S | P5.R | `tools/web/runtime/runtime.js` | Frame loop and input. Build the frame input record with the fields of `Frame_input.t` (`lib/frame_input/frame_input.mli:2-24`): `t`, `dt`, `frame`, `tick`, `size`, `pointer`, `buttons`, `keys`, and the ordered event list with the record shapes the native `frame/input` gives (`lib/flow/op.ml:116-151`). Logical points, not device pixels. Own the state slot array | `node` unit test of event translation from synthetic DOM-like events |
| P6.2 | S | P6.1 | `tools/web/runtime/workers.js` | Worker pool over `SharedArrayBuffer`. Same schedule as native: blocks of 1,024, results written to disjoint ranges. At start, if `crossOriginIsolated` is false, show one plain message and stop | `node` test with `worker_threads`: a region's output equals the single-call output byte for byte at 1 and 8 workers |
| P6.3 | S | P6.1 | `tools/web/runtime/gpu.js` | WebGPU: request the device; compile every kernel in `kernels.wgsl` at load with `createComputePipelineAsync`; per dispatch, write uniforms, bind inputs and outputs, dispatch, and check the status word on the next frame. Buffers are owned by id and destroyed when replaced | Manual browser check recorded in `results.md`; automated in P6.8 |
| P6.4 | S | P6.3 | `tools/web/runtime/present.js`, `marks.wgsl` | The presenter for `Marks`: a WGSL port of the `Marks` family shader from P4.2, the view transform as a uniform, clip as a scissor rect, the blend factors of `scene_execution.ml:260`, painter order as given | Automated in P6.8 |
| P6.5 | S | P6.4 | `present.js` | Text and images. Text: rasterize the whole string on a 2D canvas at the device pixel ratio and draw it as a textured quad, as the native path does (`lib/rays/scene.ml:166-171`). Images: decode with the browser, draw as a textured quad with a linear sampler | Automated in P6.8 |
| P6.4a | S | P6.5 | `lib/flow_web/js.ml`, `.mli`, `tools/lisp/web_export.ml`, `tools/web/runtime/{runtime,gpu,present}.js` | Repair (found building P6.8): the export ran every region on the CPU and `start` created no `gpu.js` device. A mark set whose region attributes are all GPU-legal on the web target (never read by the frame) is not invoked on the CPU: the frame prints `rt.gpuArray(k)` (a packed array of a device reference), the runtime dispatches the kernel once per frame (once per load when static; fused attributes are one multi-output kernel), one `GPUDevice` is shared by `gpu.js` and `present.js`, and the presenter binds the kernel's output buffer. A set not wholly GPU keeps the CPU path | `web_parity.exe` still passes the 10; `@qualification-web` renders `flow_particles_gpu` and a synthetic set against `marks_shot`, with `window.__rays` counters showing GPU dispatches and no CPU invoke |
| P6.4b | S | P6.4a, P6.7 | `lib/flow_web/wgsl.ml`, `.mli`, `tools/lisp/web_export.ml`, `.mli`, `tools/web/runtime/{runtime,gpu,present}.js` | Repair: generated `image/map` images are 2D canvas work, not a host file resolver. Compile their instantiated pixel body with `Packed`, derive pixel-center UVs in WGSL (refine division to match native f32 UVs), compile live dimensions/captures with `Core_lower` and `Js.frame`, and publish a GPU-resident straight-alpha RGBA8 texture (clamp, ties to even, padded rows) on the shared device. No pixel readback or CPU pixel loop. Reuse one texture per image, replace on resize, release on reload/stop; capacity 64, checked WebGPU limits. Content-address argument modules so watch reload cannot use stale captures. Keep explicit refusals for other image kinds and unsupported pixel bodies | `@lib/flow_web/runtest`, `@tools/lisp/test/runtest`; `flow_contours`, `flow_image_kernel`, `flow_shader` export deterministically |
| P6.4c | H | P6.4b | `tools/marks_shot.ml`, `tools/web_parity.ml`, `tools/web/test/{frames,runtime,gpu_images_e2e}.mjs`, `tools/dune`, `results.md` | Pin image resource identities/transforms in 120-frame parity. Compare generated pixels in Chrome with native `Image_kernel` and, for hard edges/pow, native `Flow_gpu.Run` plus `Image_sink`, using the unchanged pixel metric. Check animation, static/shared reuse, odd row width, RGBA clamp/rounding, transformed alpha layers, resize, reload and dimension failures. Keep all prior web checks and native regression checks | `@qualification-web` and `_build/default/tools/check.exe --ship`; record results and commands |
| P6.4d | S | P6.4c | `lib/flow_web/js.ml`, `.mli`, `tools/lisp/{web_export,lisp,web_server}.ml`, `tools/web/runtime/{gpu,present,prims,runtime}.js` | Complete image producers: bundle native-decoded static file assets in data.bin, preserve seeded double noise and ties-even conversion, and lower recursive offscreen drawings through the same core/JS/GPU region placement and presenter. Namespace kernels, share folds by native source identity, distinguish frame size from image target size, preserve transparent RGBA8 output and DPR 1 image pixels. Own/reuse/resize/release textures under capacity 64 and device bounds. Watch imports/assets, keep last good exports after decoder errors, retry repairs and synchronize resource replacement with in-flight frames | Focused compiler/runtime checks and deterministic exports |
| P6.4e | H | P6.4d | `tools/marks_shot.ml`, `tools/web/test/{gpu_images_e2e,runtime}.mjs`, `tools/lisp/test/web.t`, `tools/dune`, `results.md` | Compare native decoder fixtures (including EXIF JPEG), static/live noise (alias, negative and wide seeds), nested transparent renders, CPU/GPU regions and shared folds with native pixels. Pin static/shared reuse, resizing, texture ownership, typed failures, asset-only watch updates and repairs, plus all existing generated image regressions | `@qualification-web`, 120-frame full corpus parity and `tools/check.exe --ship` |
| P6.6 | S | P5.10 | `tools/lisp/web_server.ml`, `.mli`, `lisp.ml` | `rays web --watch FILE`: poll the file's SHA-256 every 0.5 s as `lib/rays_editor/source_file.ml` does (`interval`, line 13), re-export on change, and serve the output directory on localhost with `Cross-Origin-Opener-Policy: same-origin` and `Cross-Origin-Embedder-Policy: require-corp`. Serve `/version` as the export digest. Standard library and `unix` only | Cram test starts the server on a free port, fetches `/version` and a file with an OCaml client, and checks both headers |
| P6.7 | H | P6.6 | `tools/web/runtime/runtime.js` | Reload: poll `/version` every 0.5 s; when it changes, re-import `program.js`, recompile only kernels whose text changed, reset state slots, keep the canvas | Manual check recorded: save in the native editor, the page updates |
| P6.8 | S | P6.5, P6.7 | `tools/web_shot.ml`, `test/dune` | Pixel parity in a real browser, under a new alias `@qualification-web`: launch the local Chrome headless with WebGPU against the development server, capture the canvas for `examples/particles` at fixed-step frames, and compare with the native fixed-step export within a stated tolerance. If headless WebGPU is unavailable, the tool says so and exits with a distinct code | `dune build @qualification-web` |
| P6.R | R | P6.8 | | All 15 eligible workspaces open in the browser or are refused at export. `--ship` is green. No file under `lib/` mentions HTTP | Phase gate |
| P7.1 | S | P6.R | `sketches/<demo>/sketch.rays`, `sketches/dune.rays.inc` | The demonstration workspace, scaffolded with `dune exec tools/new_example.exe -- --sketch <demo>` and written with the `write-rays-lisp` skill: marks from data, axes, pan and zoom from pointer and key state, at one million marks. No new operators. Promote `dune.rays.inc` | `rays check` and `rays web` both succeed |
| P7.2 | H | P7.1 | `results.md` | Native numbers: frame time and bytes per frame for the demo, static, panning and animated, at 10,000 and 1,000,000 marks | Entry present with commands |
| P7.3 | S | P7.1 | `tools/web_shot.ml` | Browser numbers for the same six cases, from `requestAnimationFrame` deltas over 300 frames, plus load time and kernel compile time | Entry present with browser, GPU and machine named |
| P7.4 | H | P7.3 | `results.md` | The evaluation tables: meaning preserved (P5.13, P6.8), coverage (15 workspaces), run-time cost (P7.2, P7.3), lines shared against lines per target (`wc -l` over `flow_ir`, `flow_gpu`, `flow_web`, `tools/web`) | Tables filled |

## Unknowns that gate later tasks

None of these was verified while writing the plan. The task named resolves it;
until then the tasks after it are provisional.

| Unknown | Why it matters | Resolved by |
| --- | --- | --- |
| How small the primitive list really is | It is the whole per-target cost | P0.R records the count |
| Integer semantics of the `int/*` operators | OCaml integers are 63-bit; JavaScript numbers are doubles | P0.3 |
| Whether `show` can match the native float printer in JavaScript | `draw/text` output would differ visibly | P0.5, then P5.9 |
| Whether an interpreted core is fast enough natively | The closure tier it replaces is compiled OCaml closures | P1.13; P1.14 is the fallback |
| Whether state keys can stay identical under slots | `fork_state`, `transaction` and reload depend on them | P1.8 |
| Whether `Workspace_parity.check` can toggle a switch cleanly | It is the corpus-wide oracle for phases 1 to 3 | P1.11 |
| Whether placement yields `Gpu` for width-1 outputs and non-position attributes | The GPU arm today serves positions only | P4.7, P4.9 |
| Whether `coalesce_draws` already merges mark batches | Batch merging may half-exist | P4.11 |
| WebGPU default limits | Legality on the web target; the earlier draft quoted them from memory | P5.4 |
| Headless Chrome with WebGPU on the build machine | Without it, browser pixel parity is manual | P6.8 |
| `Parallel.for_` sequential cutoff | The worker pool should use the same threshold | P6.2 reads `lib/rays_math/parallel.ml` |

### Left out on purpose

- Culling, level of detail and density aggregation. P4.R decides from
  measurements whether to add a culling task.
- `sop` and `scene` graphs, the path tracer, PXUI and the editor in the browser.
- Streaming scrubs, state kept across reloads, HTML controls.
- Any new Lisp syntax.

## Ledger

The original 95 tasks (63 for Sonnet, 25 for Haiku, 7 reviews) plus the four image
tasks P6.4b–P6.4e. Phases 0 to 4 are 68 of the original tasks
and touch no browser code. Tick a task in the commit that completes it; add
`note:` lines under a task that stopped.

Phase 0
- [x] P0.1 H
  Gate: `dune build tools/core_census.exe && _build/default/tools/core_census.exe`
  passed on 2026-10-10: 45 files, exactly the 15 expected eligible workspaces.
  `--self-check` passed; two complete census runs were byte-identical.
  Additional `@tools/runtest` exposed an existing `pxui_shell` API-manifest
  mismatch (explicit signatures in source, module aliases in the manifest);
  the manifest was not promoted.
- [x] P0.2 S
  Specification includes both execution levels, state/drawing/intrinsics and
  54 frame primitives. On 2026-10-11 the census/table audit found exactly
  114 unique lowering rows with no missing/extra heads; whitespace passed.
  These task commits are in /private/tmp/rays-web-export-progress while the
  shared checkout's .git is read-only; a bundle preserves their history.
- [x] P0.2a S
  Remaining leaf clauses now state color parsing/quantization, packed-array
  construction/access/copying, recursive coercion and HSV/lerp expansions.
  Native edge probes and P0.6 passed on 2026-10-11, including huge finite
  channels, trailing hex underscores, zero-count unused fills, empty endpoint
  slices, width/range refusals, record order and unclamped HSV/lerp values.
- [x] P0.3 S
  Numeric contract checked against Value/Op/Packed and native probes on
  2026-10-11: signed 63-bit wrapping, quotient/remainder signs, distinct
  explicit/coercion rounding and BigInt target boundaries.
- [x] P0.4 S
  List contract checked against Workspace/Eval/Frame_input on 2026-10-11:
  static/live lengths, product/skip bounds, flattened record columns and
  oversized-source fallback/refusal; no new universal language cap.
- [x] P0.5 S
  Display and equality contract checked against native probes on 2026-10-11:
  typed/ordered equality, fixed-format tie adjustment and large-value spelling.
  In particular show(17179869184.03125) is 17179869184.0312.
- [x] P0.6 H
  Gate: `_build/default/tools/check.exe @tools/lisp/test/runtest` passed in the
  shared source tree on 2026-10-11. The new cram case checks the live corpus
  and proves that an added unknown head fails the coverage gate. The same run
  included `@lib/flow/runtest`, which passed without changing its tests.
- [x] P0.7 H
  Links to `specification/core.md` added to README, the repository design-notes
  paragraph and Flow section 14. `git diff --check` passed on 2026-10-11.
- [x] P0.R R
  Review on 2026-10-11: P0.6 passes, all 114 census heads have one rule, and
  the 54 primitive names/counts are explicit. The scalar/list/text/state
  boundaries are consistent with the native probes. Most leaves reuse existing
  native bodies or stdlib operations; structured control and operator expansions
  remain shared compiler work, rather than target-side Lisp interpretation.
  Repair P0.2a is required before this phase passes: color, array and coercion
  leaves still need complete behavior clauses. A native channel-conversion
  probe showed `int_of_float 1e20 = -1` and `int_of_float (-1e20) = 0` on this
  Apple-Silicon OCaml switch, so clamping before conversion would change pixels.
  P3.2 now explicitly includes the indexed geometry required by P3.6; its
  omission would otherwise make the native path snapshot gate impossible.
  Re-review after P0.2a on 2026-10-11 passed: 114 unique census rules,
  54 explicit frame leaves, complete numeric/list/text/color/array/coercion
  clauses, and P0.6 green. The inventory is bounded scalar/storage work;
  operator expansions and control stay in the shared lowering, so two target
  runtimes do not require two Lisp interpreters. Phase 1 may start.

Phase 1
- [x] P1.1 S
  Core data types, closed primitive inventory, source identities and deterministic
  printer added. Three hand-built scalar/branch/loop-and-state programs pass
  inline printer expectations. `@lib/flow_ir/runtest` passed on 2026-10-11.
- [x] P1.2 S
  Initial lowering slice and reviewed printer expectations for all eleven
  closure-tier expressions added. Static folding, captured residual sharing,
  branch scope and unrolled iteration identities are explicit core data.
  `@lib/flow_ir/runtest` passed on 2026-10-11. Scalar operations outside the
  current leaf/expansion coverage decline by name; P1.2a records that remaining
  operator work and must pass before the phase review.
- [x] P1.2a S
  Every value-context scalar operator the closure compiler accepts now lowers:
  added first, last, linspace, array/sum, pointer/down and key/down (membership
  loop over the frame input's lists, with the walker's button check) and
  value/hsv (lazy per-sector arms). test_core_operators holds a live case for
  every such operator (1,296 executions over 16 frames) and fails when an
  operator has none; each matches the walker bit for bit or by diagnostic code.
  It exposed a walker defect: on Apple Silicon OCaml contracts value/hsv's
  `h*6-i`, `1-f*s` and value/lerp's `a*(1-u)+b*u` into fused multiply-adds,
  contradicting core.md's no-fusion rule and the packed kernels. Fixed in
  lib/flow/op.ml with Sys.opaque_identity, as `length` already does; values
  can differ from before in the last bit. Default runtest green. No existing
  expectation changed.
- [x] P1.3 S
  Direct core execution and the default-off evaluator-owned switch added.
  Native leaves reuse existing operator bodies/coercions. Executor caches
  successful/refused immutable residual identities with capacity 64.
  Hook errors/exceptions restore state before the existing path retries.
  Unchanged `@lib/flow/runtest` and `@lib/flow_ir/runtest` passed on 2026-10-11.
- [x] P1.4 H
  All five existing fixtures and eleven scalar expressions compared under core,
  closure and reference settings at six times. Additional failing expressions
  compare exact diagnostics; 358 successful direct core executions match native
  bits. Hook success/decline/error/exception cases assert entry and state rollback.
  `@lib/flow_ir/runtest` passed on 2026-10-11 without refreshing expectations.
- [x] P1.5 S
  Vectors, patterned lets, Cond/Case, specialized local/named/operator calls,
  macros and bypasses have deterministic printer and direct walker parity
  cases at seven times. Call counters reset on construction and advance only
  in executed branches; provenance preserves prefixes and dynamic indices.
  Keyword-first bypass exposed a native selector bug; all semantic consumers
  now share the authored positional selector. Packed scalar/vector bypasses
  match direct/reference execution; unsupported rounding coercions decline.
  Both focused gates passed on 2026-10-11; existing expectations are unchanged.
- [x] P1.6 S
  Ordered/nested record fields flatten to locals; branch order, replacement
  coercion, field access, text/display and structural equality have direct,
  closure/reference and Executor parity at ten times. Reactive captured
  equality and old-field coercion exposed native bugs, now fixed at their
  shared boundaries. Unrelated old fields remain lazy. Existing expectations
  are unchanged; focused Flow/IR gates passed on 2026-10-11. A native record
  fold improved in wall time but increased allocations; results.md records
  the reproducible measurement, and P1.6a must pass before P1.R.
- [x] P1.6a S
  The record accumulator already lowered to field locals; the regression was
  executor overhead. Core_exec now has prepare/execute (operators and
  constants resolved once, cached per residual by Executor) with inline
  comparison, and/or and equal?. Prepared core fold: 0.052119s and 84,631,776
  bytes against the P1.5 baseline 0.076312s and 152,390,016 (limits 1.1x and
  1.05x); recorded in results.md. Flow and flow_ir focused gates passed on
  2026-10-11; no expectation changed.
- [x] P1.7 S
  Lists, HOFs and structured loops lowered and executed: List_lit, range, nth,
  count, concat, take, rest, reverse, Map, Filter, Reduce, stable Sort_by,
  For, Fold, Scan, Sum with skip; E_ITER_BOUND stays 4,096 and empty Sum is
  Int 0. HOF call sites reuse instantiate_fn, so a generic record updater is
  specialized (parity case added). Direct/reference/closure/Executor parity
  incl. 1,000-item loops, captured data, induction above 2^53. Approved
  exception: core_lower.expected expression 5 changed from `refused sum` to its
  core print (one hunk; every earlier program print byte-identical). Flow and
  flow_ir focused gates passed on 2026-10-11.
- [x] P1.8 S
  State lowers to numbered slots keyed exactly as the walker (instance,
  prefix @ zone, iteration tuple, host or frame plane) through new
  Eval.Private.state_read/peek/write, so fork_state and transaction cover it.
  New State_peek statement; a `previous` residual lowers to the read alone.
  Frame operators and frame/input events were already core expressions;
  test_core_state compares 13 state shapes (loops, functions, records, event
  records, errors, coercion) over eight frames against the walker, with equal
  state stamps. One expectation updated: test_core_lower no longer pins the
  `state` refusal. Flow and flow_ir focused gates passed on 2026-10-11.
- [x] P1.9 S
  Packed-array primitives (array/range, float, vec3, nth, count, slice, concat)
  were already core primitives; test_core_regions now proves them against the
  walker. A residual whose own term is a packed map, reduce, loop or array/sum
  over arrays (or producing an array) and that Packed.compile accepts lowers to
  a Region_ref plus Invoke, executed by Packed.force unchanged (Executor passes
  its state, resolve, measure and the CPU placement check; a failing region
  declines to the existing packed path). Scalar list loops stay structured
  core. One expectation updated: test_core_lower's second refusal case (a
  packed array/sum, now lowered) became a 2,048-node budget refusal.
  Flow and flow_ir focused gates passed on 2026-10-11.
- [x] P1.10 S
  With a root residual, Core_exec.execute restores the evaluation state on any
  failure and returns the tree walker's result for that residual (new
  Eval.Private.checkpoint and rerun; the rerun uses the walker without
  re-offering the term to the executor, so it cannot recurse). The hook now
  returns that result instead of declining. test_core_errors: division by zero,
  out-of-range nth, non-finite result and a state write followed by a failure
  match the walker's diagnostic, span and state stamp, and at least one core
  diagnostic differs without the rule. Follow-up found on the way: static
  residuals carried the static state, so state statements had silently
  declined; residuals now remember the live evaluation that offered them
  (commit 60acf38a), and test_core_state fails on unexpected core errors.
  Flow and flow_ir focused gates and default runtest passed on 2026-10-11
  (API manifest promoted separately).
- [x] P1.11 H
  `Workspace_parity.check` now runs its IR-against-reference comparison (both
  domain settings) once with `core_residuals` off and once on, restored by
  `with_core_residuals`. `test/test_workspace_ir` passes unchanged: 42 standard
  files, 1 custom-catalog executable, 13 fixtures, cooked payloads equal, with
  the core pass included. The gate ran about 20 minutes wall on the M1 host
  (the comparison now runs twice); no timing claim is made. `workspace_parity.mli`
  unchanged, so no API manifest change.
- [x] P1.12 H
  `RAYS_CORE_COVERAGE` prints lowered and refused residuals per workspace from
  `Workspace_parity.check` (the test file is unchanged; the gate still passes,
  EXIT=0). Table and command in results.md, 2026-10-11. Zero-error NOT met: 46
  refused residuals in 7 of the 15 eligible workspaces: examples/drawing (`ref` 1),
  generative (`packed_higher_order` 6), noise (`packed_higher_order` 8),
  particles (`packed_higher_order` 6), pxui (`packed_higher_order` 2),
  recursive_rectangles (`packed_higher_order` 17, `color/hsl` 1),
  sketches/flow_particles_gpu (`noise3` 4, `packed_higher_order` 1). A lowering
  slice task must close these.
- [x] P1.12a S
  Coverage is now 2,009 lowered and 0 refused across the 15 workspaces (table in
  results.md). A packed map/filter that Packed refuses (dynamic `int`/`floor`,
  state, array captures) lowers to a counted loop over `Array_nth` storing into a
  packed builder: `Sequence_build` with a width allocates `Float_array`..`Vec4_array`
  (no 4,096 cap), `Store` writes through the new `Value.array_put` that
  `array_init` now shares, and filter ends in `Array_slice`; sort-by over arrays and
  packed `for`/`scan` collects still decline by name (none in the corpus). `noise3`
  maps to its primitive; `color/hsl` and `color/gradient` (constant opaque stops) are
  compiler expansions checked bit for bit over dense sweeps; a `ref` without overrides
  inlines the graph body in its cached default instance through the new
  `Eval.Private.residual_graph` and `graph_instance` (overrides still decline).
  No walker behaviour changed. Parity cases in test_core_regions and test_core_state
  (a comparison stored in a host record changes `state_stamp` through Marshal sharing
  only, so that case stores an integer flag). `@lib/flow/runtest`,
  `@lib/flow_ir/runtest` and `@test/test_workspace_ir` (EXIT=0, about 25 minutes)
  passed on 2026-10-11; the API manifest is promoted in the next commit.
- [x] P1.13 H
  `tools/bench_core.ml` (in `bench-build`) times one scalar residual, `(+ (* (sin t) 0.5) (* t t))`;
  the cost table names no expression, so this one was chosen. Medians of three runs:
  tree walker 940.2 ns/eval, closure 132.6 ns/eval, core 391.8 ns/eval (core/closure 2.95).
  That exceeds 1.1, so P1.14 is required. Command and rows in results.md, 2026-10-11.
- [x] P1.14 S
  The 1.1 micro-bench gate was not met (final ratio 1.26 at review; 1.23 after P1.14b, results.md); the project owner waived it on 2026-10-11.
  note (history): not met after two attempts; core/closure 2.92 to 1.39 (core 378.1 to 166.7 ns, closure
  119.5 ns; results.md, 2026-10-11). Kept: executor fast paths and a postfix scalar tier in
  `Core_exec.prepare` (`~fused:false` is the reference; test_core_parity compares both with the
  walker); parity, flow, flow_ir and default tests pass. Remaining hot spot: the hook round trip
  (about 40 ns: snapshot, handlers, cached-program lookup), which needs the evaluator to hold the
  prepared program on the residual. Ratio is at most 1.5, so P1.15 proceeds.
- [x] P1.14a S
  The 1.1 micro-bench gate was not met (ratio 1.26, results.md); the project owner waived it on 2026-10-11.
  note (history): not met after two attempts; core/closure 1.39 to 1.25 (core 166.7 to 148.0 ns, closure 118.1 ns,
  three-run medians, load average 4.5; results.md, 2026-10-11). Kept: `residual.core` slot (owner-checked,
  filled by `Executor` on first offer, called by `force_res` with no snapshot and no handler; `Core_exec.execute`
  now catches every exception and the postfix path no longer validates frame facts it does not read).
  Remaining about 30 ns is the postfix interpreter itself (about 9 dispatches plus `sin`) against the closure
  tier's boxed-float tree, not the hook.
- [x] P1.15 S
  `Eval.Private.core_residuals` now defaults to true. Default tests
  (`SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy _build/default/tools/check.exe`, EXIT=0,
  which includes the workspace IR sweep) pass on 2026-10-11; tests that compare the switch
  on and off still set both values explicitly, and no expectation changed. Flipping it also
  sends every dataflow program through the executor hook (`Executor.force` takes the
  `force_with_packed` path whenever the switch is on). It exposed one defect of P1.12a,
  fixed in the preceding commit: a workspace operator reusing the name `noise3`,
  `color/hsl` or `color/gradient` ran the shipped body; core now declines it. For P1.16:
  the callers of `fast_of` are `eval.ml` `classic ()` (the hook's fallthrough, gated on
  `c.st.compiled_residuals`), the `compile` function's `Residual` case and
  `Private.closure_available`/`compiled`; `closure_available` is used by `flow_ir.ml` line 365
  and `test_core_lower.ml` line 35; `compile_residuals` by `fast_of` and the tests
  `test_frame.ml`, `test_workspace_eval.ml`, `test_core_parity.ml` and `tools/bench_core.ml`.
- [x] P1.14b S
  Postfix superinstructions and depth-sized stack (ratio 1.23, min 1.21) plus whole-frame repairs in `Core_exec` (results.md). The 1.1 gate was not met (ratio 1.26 at review); the project owner waived it on 2026-10-11.
- [x] P1.R R
  Accepted 2026-10-11: P1.14 to P1.14b ticked on the owner's waiver of the micro-bench gate (see them). Review on 2026-10-11 (commit after 06149f40). Green: `--ship` (all, runtest, smoke, diff check;
  807 s, the only failure was the API manifest for the P1.14a `.mli` change, promoted in its own
  commit, then 2 s cached rerun EXIT=0) and `@runtest-native` (one run failed
  `runtime_live_resize` "a programmatic resize stalled the frame loop for 1033 ms" at load average
  5; `lib/runtime` is untouched since `723a7ff9` apart from a test, and the test passed on three
  reruns: a wall-clock flake, not Phase 1). Coverage re-run on the final tree: 15 eligible
  workspaces, zero refusals (same counts as the P1.12a table, 2,009 lowered). Callers for P1.16:
  `eval.ml` (`fast`/`fast_of`/`compile`/`cnode`/`cnode_of_value`, the `fast` field of every
  residual literal, `classic ()`, `compile_residuals`, `core_residuals`, `Private.compiled`,
  `Private.closure_available`, `Private.compile_residuals/core_residuals`); `flow_ir.ml` placement
  (`closure_available`, now `Core_lower.lower`), the `core_residuals` tests in `Executor.force`
  and the legacy IR interpreter branch (dead once the switch goes); tests `lib/flow/test_frame.ml`,
  `test_workspace_eval.ml`, `lib/flow_ir/test_core_{parity,lists,regions,state,errors,records,lower}.ml`;
  `tools/workspace_parity.ml`, `tools/bench_core.ml`. Decision for P1.14: core/closure is 1.25 to
  1.27 over five runs (median 1.26; load average 6, so not quiet), above the 1.25 acceptance
  threshold, so the phase is not ticked and P1.14b is added. P1.16 was prepared (an uncommitted
  working tree, kept as the newest `git stash` entry, applies on 4314b0fc's parent tree) with `@lib/flow/runtest`
  and `@lib/flow_ir/runtest` green, `--ship` not run; it resumes after P1.14b.
- [x] P1.16 S
  The closure tier (`compile`, `cnode`, `fast`, `fast_of`) and both switches (`compile_residuals`, `core_residuals`) are deleted; core residuals are the only path, `Closure` stays the tier name. `--ship` and `@test/test_workspace_ir` green on 2026-10-11 (the API manifest is promoted in the next commit). `tools/bench_core.exe` keeps the walker and core rows.

Phase 2
- [x] P2.1 H
  Done 2026-10-11. `Packed.Private.trace` records every public compilation result; `lib/flow_ir/snapshots/` holds the register programs (`code`, `widths`, `output`, `uniform_widths`, `collecting`, `gpu_refusals`, refusals with spans) of `test_packed` (deduplicated), the `test_program` fixtures and the kernels of `examples/particles`, `sketches/flow_shader`, `flow_image_kernel`; `test_program.ml` moved to `lib/flow_ir` and is linked into `lib/flow_gpu` by `copy_files`. `sketches/flow_particles_gpu` is recorded as skipped: it holds a `sop` graph that `flow_ir` tests cannot check. Two generation runs were byte-identical. `@lib/flow_ir/runtest` and `@lib/flow_gpu/runtest` pass.
- [x] P2.2 S
  Done 2026-10-11. `Core.Region` (instruction, iteration, result, origin, `t`) replaces `Region_ref of Packed.t`; `Core` no longer depends on `Packed`. A region is a register program (instruction n defines register n) plus sources, widths, captured uniforms, count, origin, skip, stages and the compile options, reusing `Flow.Packed_ops.binary`/`unary`. `Core.pp` prints it in full. Because `Packed` now depends on `Core`, `Packed.of_region` and `Packed.to_region` (lossless, `of_region` checks register order and the 64-register limit) land here; `Core_lower`/`Core_exec` convert at the boundary. test_core prints a region and round-trips it. `@lib/flow_ir/runtest`, `@lib/flow_gpu/runtest` pass; snapshots untouched.
- [x] P2.3 S
  Done 2026-10-11. `Core_region.lower` (new module, so `Packed` can call it without a cycle; the concurrent Phase 3 work in `core_lower.ml` is untouched) lowers a map, reduce, numeric loop or `array/sum` to a `Core.Region.t`, covering exactly what `Packed.expression` accepts and refusing with the same `E_PACKED_*` codes, spans and messages. `captured_value`, `count_of`, `reads_names` and the like moved there; `Packed` aliases them. Every program `test_packed` compiles (refusal cases included) is also lowered and compared with the old compiler (register program, count, origin, skip, sites, uniforms, or the same diagnostics). `@lib/flow_ir/runtest`, `@lib/flow_gpu/runtest` pass.
- [x] P2.4 S
  Done 2026-10-11. `Packed.Private.core_regions` (default off) routes `compile_impl` through `Core_region.lower` and `of_region` (the old `fuse_maps` still fuses the result until P2.6). Both snapshot tests also run in dune with the switch on (`REGION_CORE=1`) and match the P2.1 files byte for byte. With the default temporarily flipped, all of `@lib/flow_ir/runtest` (template counts of `test_frame_kernel` included) and `@lib/flow_gpu/runtest` (Metal goldens) passed. (`of_region` itself arrived in P2.2, which `Core`/`Packed` needed to stop depending on each other.)
- [x] P2.5 S
  Done 2026-10-11. The live-or-dynamic `Uniform` rule and `captured_value` were already part of `Core_region.lower` (P2.3: a template is `lower ~dynamic:true`). `rebind` now works on a region (`Core_region.rebind`); `Packed.rebind` delegates to it through `to_region`/`of_region`, and the term-based copy is deleted (it is not a term walker). `test_frame_kernel` template counts (2 compiled, 10 rebound, 12 executed; 1/5/6) are unchanged with the switch on and off; all of `@lib/flow_ir/runtest` passes with the default flipped.
- [x] P2.6 S
  Done 2026-10-11. `Core_fuse.fuse` (new module) ports `fuse_maps` to regions with the same rules: Collect + Zip children of equal static count or origin, a shared-instruction table (`Marshal` key, so equal instructions share a register), the 64-register limit declining to the unfused region, count/origin recorded even without a fusable child. With `core_regions` on, `compile_impl` is `Core_region.lower` then `Core_fuse.fuse` then `of_region`; the old `fuse_maps` serves the switch-off path until P2.10. P2.1 snapshots identical with the switch on (dune reruns both snapshot tests with it); all of `@lib/flow_ir/runtest` and `@lib/flow_gpu/runtest` pass with the default flipped.
- [x] P2.7 H
  Done 2026-10-11 with `core_regions` temporarily defaulted on (not committed): `@lib/flow_ir/runtest`, `@lib/flow_gpu/runtest` (three Metal goldens) and `@runtest-native` all passed (exit 0, including `runtime_live_resize`); `git status` shows `lib/flow_gpu/goldens` and the P2.1 snapshots untouched.
- [x] P2.8 H
  Done 2026-10-11. `tools/bench_core.exe kernels` (map 1,024 and 1,000,000, fused pair, sum loop; switch off and on; 1 and 8 domains) and `bench_gpu.exe` recorded in results.md. Force times are the same program, so unchanged within noise; compile with the switch on regresses by up to 50 percent (under a microsecond, once per kernel), recorded in DEFERRED.md per the owner decision that speed gates never block.
- [x] P2.9 S
  Done 2026-10-11. `Packed.Private.core_regions` now defaults to true; the dune reruns of the snapshot tests use `REGION_CORE=0` to keep the term-walking path covered until P2.10. Gates on the flipped tree: `@all`, `@runtest` (SDL dummy, includes `@test/test_workspace_ir`, 42 standard files, 13 fixtures, cooked payloads equal), `@runtest-native` and `@smoke` all exit 0, snapshots and Metal goldens untouched. The only `@runtest` failure was the API manifest for the `Core.Region`/`Core_region`/`Packed` interfaces of P2.2 to P2.6, promoted in the next commit.
- [x] P2.R R
  Review 2026-10-11: `git log 02ef72f5..HEAD -- lib/flow_ir/snapshots lib/flow_gpu/goldens` is empty, so the snapshots and the three Metal goldens are byte-identical since P2.1; P2.9 gates were green. Dead in `packed.ml` now that `core_regions` defaults on: the `expression` record type, `Compile_refused`, the term-walking branch of `compile_impl` (the whole `try` body), `source_program`, `fuse_maps`, the aliases `captured_value`, `count_of`, `count_of_sources`, `reads_names`, and the `core_regions` switch with its `Private` export and the `REGION_CORE` dune reruns. Still live: `force`, `Private.view`/`inputs`/`prepare`, `gpu_refusals`, `of_region`, `to_region`, `rebind`, `width_of`, `components`, `Unsupported`.
- [x] P2.10 S
  Done 2026-10-11. Deleted from `packed.ml` (864 to 433 lines): the term-walking `compile_impl` body, `expression`, `source_program`, `fuse_maps`, `Compile_refused`, the `captured_value`/`count_of`/`count_of_sources`/`reads_names` aliases and the `core_regions` switch; `compile_impl` is now `Core_region.lower`, `Core_fuse.fuse`, `of_region`. Kept: `force`, `Private.view`/`inputs`/`prepare`, `gpu_refusals`, `of_region`, `to_region`, `rebind`. `test_packed`'s old-against-new lowering comparison and the `REGION_CORE=0` dune reruns are removed with the path they compared; the P2.1 snapshots (unchanged) are the oracle. `bench_core.exe kernels` lost its switch column. Gates: `@all`, `@runtest` (SDL dummy; includes `@test/test_workspace_ir`), `@runtest-native` and `@smoke` pass; the API manifest diff is promoted in the next commit.

Phase 3
- [x] P3.1 H
  Order dependence explained 2026-10-11 (b147a1ec): text images take ids from the process-global
  texture counter and `Render_ir.serialize` includes them; not frame state. One process per workspace.
  Snapshots taken 2026-10-11 before any change to the drawing path: 15 eligible workspaces, 640x480,
  pointer (100, 100) with no buttons, fixed dt 1/60, frames 0..30 rendered in sequence, frames 0, 1 and 30
  recorded as SHA-256 of `Render_ir.serialize` (`test/draw_snapshots/`, generated by `test/test_draw_snapshot.ml`).
  Four workspace draw graphs record the diagnostic `E_IMAGE` instead of hashes (no image callback is reachable
  from `test/`): `sketches/flow_contours`, `flow_image`, `flow_image_kernel`, `flow_shader`.
  note: each workspace runs in its own process. In one shared process the output depends on order
  (`examples/audio` before `examples/basic` shifts basic's frames by one), so this is a product
  determinism defect to review; it was not fixed here.
- [x] P3.2 S
  `specification/marks.md` written 2026-10-11 (commit 4f08e4f6): mark sets,
  geometry, image, text, transform/clip push-pop; caps 1,000,000 per set
  (`Shape_batch`) and 1,048,576 entries (`Render_ir`). Reviewed by P3.R.
- [x] P3.3 S
  `Scene_command.Marks` added 2026-10-11 (commit 660ee85a): types, `validate`,
  little-endian `serialize` ("RMK1"). `@lib/scene_command/runtest` and
  `@test/runtest` passed; `scene_command` still has no dependencies.
- [x] P3.3b H
  `flow_ir` may depend on `scene_command` (dependency-free marks contract) 2026-10-11: added to
  `lib/flow_ir/dune`, the gate's `only` table, AGENTS.md and backend.md. `@test/runtest` passed.
- [x] P3.4 S
  Done 2026-10-11 on branch `worktree-agent-a30ec1f3dd78d1287` (commit b42c19e6); focused gates green.
- [x] P3.5 S
  Done 2026-10-11 on branch `worktree-agent-a30ec1f3dd78d1287` (commit 9caa36e2); focused gates green.
- [x] P3.6 S
  Done 2026-10-11 on branch `worktree-agent-a30ec1f3dd78d1287` (commit a15b7deb); focused gates green.
- [x] P3.7 S
  Done 2026-10-11 on branch `worktree-agent-a30ec1f3dd78d1287` (commit dae1acd7); focused gates green.
- [x] P3.8 S
  26 draw graphs serialize byte for byte the same through `Drawing` and core/marks/`Marks_scene`;
  text and image are checked at the marks level only. Adds `Scene.Affine` (six coefficients kept verbatim).
  Done 2026-10-11 on branch `worktree-agent-a30ec1f3dd78d1287` (commit 99ae9860); focused gates green.
- [x] P3.9 S
  2026-10-11, branch `worktree-agent-a30ec1f3dd78d1287`, 3bcc3199: `Drawing.core_drawing` switch; all 15 P3.1 snapshots byte-identical and `@test/test_workspace_pixels` green with it on.
- [x] P3.10 S
  2026-10-11, branch `worktree-agent-a30ec1f3dd78d1287`, f36a55a5: kept GPU path. `Marks` has no GPU attribute, so a frame with a GPU backend and a `draw/circles` node still uses the old arm and its token; everything else is core. The contract-level GPU buffer lands with P4.9. `@runtest-native` green.
- [x] P3.11 H
  2026-10-11, branch `worktree-agent-a30ec1f3dd78d1287`, d3896d87: `tools/bench_marks.ml`. Animated circles/rects at 10k to 1M: core path 4 to 6 times faster, about 45% fewer bytes (results in the P3.11 report; static rows are retention hits).
- [x] P3.12 S
  2026-10-11, branch `worktree-agent-a30ec1f3dd78d1287`, 982eff2a: `core_drawing` defaults on. `@all`, default suite (API manifest diff only), `@smoke`, `@runtest-native`, pixels green. Old arms still tested through `--arms` and the switch-off pixel rule.
- [x] P3.R R
  2026-10-11, 8c94e79b: draw snapshots unchanged since 7e806d2a; `marks.md` brought in line with `marks.mli` (Affine, image ids, Buffers).
- [x] P3.13 S
  2026-10-11, ac5c5eaa: arms and switch deleted; a lowering error is a diagnostic. Kept: `render_gpu` (GPU circles, narrower than the old arm: a transform around GPU circles now takes the CPU core path) until P4.9. Draw lowering budget raised to 32768 so `examples/drawing` lowers (this also resolves P5.13a natively). `?reference` removed from `render_prepared`; `test_marks_scene` checks 26 digests taken from the old arms.

Phase 4
- [x] P4.1 H
  2026-10-11, 4ad9125a: baseline rows at 100k and 1M (animated circles 1M: 160 ms, 608 MB/frame; moving translate 1M: 49 ms, 320 MB/frame).
- [x] P4.2 S
  2026-10-11, eb134dae: `Marks` pipeline family and shader; six storage buffers + affine; layout in `marks.md`.
- [x] P4.3 S
  2026-10-11, d109a87d: a GPU vertex source may carry several buffers; signatures cover every buffer id.
- [x] P4.4 S
  2026-10-11, 312972a1: byte-exact native pixel parity of `Marks` against `Shape_batch` at 63, 64, 65 and 100,000 instances, rect/circle/line, styled and unstyled, 1x and 2x.
- [x] P4.5 S
  Done 2026-10-11. `Core.Region.t.output : int array` is now `outputs : int array array` (component registers per output; `Packed.of_region` refuses an empty list, or several outputs unless the region collects). `Packed.Private.view.outputs` is the same. `Packed.force_all` returns one packed array per output (`Eval.value list`); `Packed.force` stays the single-output accessor and errors on a multi-output program, which has no reference fallback. `Core_fuse` fuses a multi-output parent but never absorbs a multi-output child. The Metal emitter declares one buffer per output (`output` when single, `output0..` otherwise) at bindings `n..n+k-1`, then uniforms, status, table; `Emit.msl.output_width` became `output_widths`; `Run` owns one buffer per output and `Run.readback ?index`/`Run.buffer ?index` pick one. A single-output program prints as before: the P2.1 snapshots and the `arithmetic`, `select`, `noise` goldens are byte-identical (no mechanical reformat was needed); the new `two_outputs.metal` golden and the `Test_program.two_outputs` fixture (built at region level) are checked by `test_emit`, `test_packed` (1 and 8 domains, byte-equal) and natively by `test_run`. `@lib/flow_ir/runtest`, `@lib/flow_gpu/runtest`, `@runtest-native` pass.
- [x] P4.6 S
  Done 2026-10-11. New IR kind `Marks of {set; attributes}` (`draw/circles`, `rects`, `lines`, `points` become it in `of_evaluation`; every other draw call stays `Sink (Display kind)`). An attribute is `{role; edge; output}`: the role (Position, Target, Size, Fill, Stroke, Width), the name of the argument edge that carries it, and which output of a fused region node it reads (0 until P4.7). `id`/`provenance` are the drawing call's path; `Flow_ir.attribute_edges` lists the (attribute, node) pairs. `share`, `hoist`, `prune`, `fuse` (a barrier, as `Sink`) and `place` carry the node; its tier is `Interp` until P4.12. `test_ir` is unchanged plus new cases. Also repaired a stale expectation: `core_draw.expected` still printed regions in the pre-P2.2 form (`instructions=6`), so `test_core_draw` failed on `dev`; regenerated, region lines only. `@lib/flow_ir/runtest`, `@test/dependency_gate` pass.
- [x] P4.7 S
  Done 2026-10-11. `Flow_ir.fuse_attributes` (run by `optimize` after `share` and before `prune`/`place`): the region attributes of one mark set that are single-output Collect+Zip `Packed_map` nodes used only by that set, with equal static count or origin (checked by the new `Core_fuse.merge`, which shares equal instructions through the factored-out register builder) and equal rate, scope and precision, become one `Packed_map` node with one output each, placed at the lowest member's index; the set's edges all point at it and `attribute.output` picks the output. A merge that would pass `register_limit` (64) or disagree leaves the members separate; nothing fails. A static and a frame-rate attribute never fuse (P4.8 keeps the static one from rerunning). `test_ir`: fused result equals the separate kernels byte for byte at 1 and 8 domains and at three times; counts, rates and the register limit decline. `bench_core.exe attributes` at 1,000,000: separate 92.7 / 62.8 ms, fused 72.6 / 41.6 ms at 1 / 8 domains (results.md). P2.1 snapshots and Metal goldens untouched; `@lib/flow_ir/runtest`, `@lib/flow_gpu/runtest`, `@test/dependency_gate` pass.
- [x] P4.8 S
  Done 2026-10-11. `Executor.compile_marks set attributes` builds the mark set's IR (one `Marks` node over its attribute values), optimizes it whole (P4.7 fusion, placement) and compiles each attribute; `Executor.force_marks` returns every attribute's value by name, grouping attributes by the IR node they read: a `Static`-rate unit runs once and its value is kept, a frame-rate unit reruns, a fused multi-output region runs once per frame via `Packed.force_all` for all its outputs (a failure reruns the attributes one by one for the walker's diagnostic). `Executor.marks_graph` shows the placed IR. `hoist` leaves `Packed_map` nodes alone, so the split reads the node's `rate` field, which `of_evaluation`/`add` already derive from frame and state dependence. Test: with static positions and animated colour over 4,096 elements, ten frames show one position run and ten colour runs (a clock whose readings are squares makes each execution's `seconds` unique, counted per `Profile` owner), values equal the tree walker's each frame; with both animated the fused pair runs ten times. `@lib/flow_ir/runtest`, `@lib/flow_gpu/runtest`, `@test/dependency_gate` pass.
- [x] P4.9 S
  Done 2026-10-11. `Marks.attr` gains `Device of buffer` (position, size, end, radius, width only; byte 2 in `serialize`; `marks.md`). A region whose every use is the positions, from, to, sizes or radius of a mark set (`Core_exec.device_regions`, per `Invoke`) is offered to `Core_exec.draw ~device`; `Drawing` answers with `Executor.region_display` (the `try_display` placement, factored out as `display_packed`, with a 64-kernel cache), keeps the `Gpu.value` for the frame and leaves a `Core_marks.device_value` placeholder in the locals. `Marks_scene.scene ?gpu ?device` sends a mark set with a `Device` attribute to `Workspace_gpu.marks` (one `Rays_execution.Private.gpu_marks` source per set and drawing, `id lsl 20 + ordinal`): device buffers are bound directly, constants and CPU arrays uploaded in the `Marks` layout (colour buffers converted to packed words on the CPU), and the set rides in the scene as a `Shape_batch` GPU token, so transform, clip, blend and order are unchanged; `lower_scene2` emits it as a `Marks` draw. Routing: a set with a device attribute uses `Marks`; every other set (and every set without a GPU host) stays `Shape_batch`, so the P3.1 snapshots are untouched (all 15 byte-identical). GPU circles inside a transform stay on the GPU. Not reachable from `Drawing`: fused multi-output regions (the core path lowers one region per attribute; `compile_marks` is still unused by `Drawing`). A region dispatched twice in a frame runs the second time on the CPU. `Drawing.render_gpu`, the per-argument programs and `?gpu_policy` are gone (the policy rides in `with_backend`); `gpu_circles` is no longer called. New `test_workspace_gpu_marks`: circle, rect and line sets with GPU positions, radii, sizes and ends and CPU colour buffers inside a translate equal the CPU path pixel for pixel at three times, two drawings alive at once. `@lib/rays_execution/runtest-native`, `@test/test_workspace_pixels`, `@lib/flow_ir/runtest`, `@test/dependency_gate` pass; `@runtest-native` passes except the known teardown-plateau test.
- [x] P4.10 S
  Done 2026-10-11. The renderer already kept the transform out of the mesh identity (a 24-byte `ui_affine` uniform per slot); what scaled with the count was the CPU work before it. (1) `Core_exec.static_invokes`: a top-level region whose residual is not frame or state dependent and whose arguments are write-once constants runs once per draw program and keeps its value. (2) `Marks_scene.cache`: a rect, circle, line or point set whose entry and attribute arrays are physically those of the previous frame reuses its scene node, so no `Shape_batch` is built again. (3) `rays_execution`: a slot remembers the batch it copied (`ui_source`), and the same published batch is unchanged without a byte comparison. `draw/translate` still snaps to integers. `tools/bench_marks.exe` moving translate: 49 ms / 320 MB per frame at 1M before, 0.014 ms / 6,768 bytes at 100k and at 1M after. `test_marks_scene` asserts the allocation is equal at 10,000 and 100,000 instances and that a kept batch serializes like a fresh drawing. 15 draw snapshots identical; `@lib/flow_ir/runtest`, `@lib/rays_editor/runtest`, `@lib/rays_execution/runtest-native`, `@lib/rays_editor/native_qualification/runtest-native`, `@test/test_workspace_pixels`, `@test/dependency_gate` pass.
- [x] P4.11 S
  Done 2026-10-11. `coalesce_draws` already covers the case, so it is unchanged: neighbouring `Shapes` draws of one family, blend, clip and transform (equal `state`, no GPU source) merge into one draw when the run is longer than 64 (shorter runs keep their own draws on purpose, so a changing member does not re-upload its neighbours), painter order kept (instance order). Mark sets of the `Marks` family never merge (each owns its buffers; merging would need a GPU copy), but keep their place in the order. `Rays_execution.Private.encoded_draws_for_test` (over `Scene_execution.Private.coalesced_draws_for_test`) reports the draws the renderer encodes, since `logical_draws` counts before merging. New `test_batch_merge` (native): 3 sets are 3 draws, 100 interleaved-position sets are 1 draw, alternating `Shape_batch` and `Marks` sets are 6 draws, a clip between sets separates them; every case is pixel-identical to one batch holding the same instances. `@lib/rays_execution/runtest-native` and `@lib/scene_execution/runtest` pass (gated in a private worktree: the shared queue was held by a long `@qualification-web`).
- [x] P4.12 S
  Done 2026-10-11. `Flow_ir.target = {legal : node -> tier -> bool; cost : tier -> count:int -> float}` is exported and constructible outside `flow_ir`; `Flow_ir.native` is Metal (a region is GPU-legal with a static count of at least 1,024 and no `Packed.gpu_refusals`; a `Marks` node is GPU-legal; every other tier legal; `cost` is `Cost.estimate`). `place` and `optimize` take `?target` (default `native`); the old placement cases of `test_ir` pass unchanged, and the measured GPU-against-CPU comparison and the CPU choice now read `target.cost`. A `Marks` node is placed like a kernel: `Gpu` when one attribute is a GPU region and the others are GPU regions or constants and `legal node Gpu`; `Cpu_kernel` when some attribute is a CPU region; else `Interp`. A target whose `legal _ Gpu_readback` is false never leaves a region on the GPU whose value would have to come back (the producer of an `(exact x)` read, or any region of a mark set that is not wholly GPU); a CPU-only target (`Gpu` illegal) places nothing on the GPU. New `test_ir` cases: native mark-set tiers, a no-readback target, a CPU-only target and a custom cost table. `@lib/flow_ir/runtest`, `@lib/flow_gpu/runtest`, `@test/dependency_gate`, `@check` pass.
- [x] P4.13 H
  Done 2026-10-11. Rows re-recorded in `results.md` ("P4.13"): P4.1 baseline built at 4ad9125a and the current build, run back to back, plus a new `native_qualification/bench_marks_gpu.exe` for GPU positions (uniform, per-instance radius or size, per-instance fill, both). Moving translate 1M: 48.9 ms / 320 MB to 0.006 ms / 6.8 KB. Animated CPU rows unchanged (circles 1M 137 to 153 ms, rects 136 to 136 ms; the ledger's 160 ms for P4.1 was a noisier run). GPU 1M: uniform circles 8.9 ms, per-instance 73.7 ms (8.3x); uniform rects 9.1 ms, per-instance 80.4 ms (8.8x). The 2x target is missed; the per-instance fill (a CPU colour buffer) is 7.7 to 7.9x of it, the GPU extent 1.4x and 2.1x. Recorded in `DEFERRED.md` (device colours); not optimized.
- [x] P4.R R
  2026-10-11, at a691b073: `_build/default/tools/check.exe --ship` (`@all`, `@runtest`, `@smoke`, `git diff --check`) exits 0; `@runtest-native` passes except the known `runtime_native_qualification` teardown-footprint plateau test (growth 21,008 KiB over a 9,456 KiB baseline), not chased. The 15 P3.1 draw snapshots are unchanged since they were taken. Culling: none. The P4.13 numbers put the cost of per-instance GPU marks in the CPU frame build (the fill colour buffer, 7.7 to 7.9x of the 8.3x; the GPU extent adds 1.4x to 2.1x), not in vertex or fill work on the GPU, and a static batch no longer costs anything per frame; culling instances outside the view would not move either. No culling task is added. The follow-up that would is device colours (`DEFERRED.md`).
- [x] P4.14 H
  Done 2026-10-11. Deleted `lib/rays_execution/gpu_circles.ml(i)`, its `Private` API and registry (`create_gpu_circles`, `gpu_circles`, `close_gpu_circles`, `Gpu_circles`) and the circles branch of the `Shapes` lowering (a GPU token now resolves to a `Marks` source or is stale), `Workspace_gpu.circles`, its sink LRU and the unused `Workspace_gpu.policy`. `Drawing.render_prepared ?gpu` stays: it is no longer the circles callback but the entry of the GPU route of P4.9 (`Workspace_gpu.marks`); the `?gpu_policy` argument went in P4.9. `test_gpu_draw` is ported to `gpu_marks`: byte parity under transform, clip, six blends and two densities, retained replay, stale refusal on replay and on lowering after the device source is overwritten, after a host attribute changes and after the lease is released, an empty set refused, no leaked handles. The nonfinite refusal moved to where host floats become binary32 (`Marks_scene.float_bytes`: `E_DRAW_RANGE`, tested in `test_marks_scene`); a device array's own non-finite values are the kernel's status check. `flow.md` and `marks.md` describe the one route. `--ship` exits 0; `@runtest-native` passes except the known teardown-plateau test; 15 draw snapshots identical. The API manifest has nothing to promote (none of the removed or added names are in it), so there is no promotion commit.

Phase 5
- [x] P5.1 H
  `flow_web` added 2026-10-11: empty library over `flow`, `flow_ir`, `param`,
  `rays_math`, `scene_command`; gate rules, `only`, `upper` and injected edges
  added; `@test/runtest` green, API manifest unchanged.
- [x] P5.2 S
  2026-10-11. `Wgsl.kernel : Packed.t -> (wgsl, Diagnostic.t) result` mirrors `Emit.kernel` over `Private.view` with the same `gpu_refusals` (`E_GPU_FORM`), binding numbering and uniform layout; `flow_web` defines its own `binding`/`kind` (no `flow_gpu`/`ogpu`). f32 storage arrays, `bitcast<f32>` constants and uniforms, exponent-bits finite test, `atomic<u32>` status, `@workgroup_size(64)`; uniforms are a `var<uniform>` `array<vec4<u32>, n>` (`uniform_bytes` rounded to 16) so they do not use a storage-buffer slot. Comparisons yield `select(0.0, 1.0, ..)`; `pow` goes through a `wpow` helper because WGSL `pow(0, y<=0)` is undefined. `@lib/flow_web/check` green.
- [x] P5.3 H
  2026-10-11. `lib/flow_web/test_wgsl.ml` diffs `goldens/{arithmetic,noise,select,two_outputs}.wgsl` (regenerate with `test_wgsl.exe --write-goldens` from `lib/flow_web`) and checks binding numbering against the matching Metal goldens and the shared `E_GPU_FORM` refusals; `test_program.ml` is linked with `copy_files`. No WGSL validator exists here (`naga`, `tint`, `wgsl_analyzer` absent from PATH; node has no WebGPU), so the goldens are read by hand only and the first real compile is P6.8. `@lib/flow_web/runtest` green.
- [x] P5.4 S
  2026-10-11. New `Flow_web.Target` (not `wgsl.ml`): `region packed ~count` refuses with `E_WEB_BUFFERS` (more than 8 storage buffers: inputs, outputs, status word and noise table all count, the uniform block does not), `E_WEB_ELEMENTS` (more than 64 x 65,535 = 4,194,240 elements in one 1-D dispatch of `@workgroup_size(64)`), or the printer's `E_GPU_FORM`, each with the region's span; `readback ir` gives `E_WEB_READBACK` for a `Gpu_readback` node or an `(exact x)` read over a GPU producer; `Target.web : Flow_ir.target` is native with `Gpu_readback` illegal and GPU regions required to pass `region`. Limits are the WebGPU specification's defaults, section 3.6.2 "Limits" (fetched 2026-10-11): `maxStorageBuffersPerShaderStage` 8, `maxComputeWorkgroupsPerDimension` 65535 (cited in `target.mli`). `Flow_web.Kernel_table` lifts the kernel-table JSON writer `gpu.js` loads. `test_target` has one refusing case per rule plus placement with `Target.web`. `@lib/flow_web/runtest`, `@test/dependency_gate` pass.
- [x] P5.5 S
  2026-10-11. `Js.frame : Core.program -> (string, Diagnostic.t) result` and `Js.frame_map` (lines `id start finish`). One ES module `frame(input, state, rt)` (`async` only when it invokes a region); locals are one `let` list, structured control maps one to one, primitives are `rt.<prims.js export>(...)` with arguments placed positionally by name (a primitive with an unplaced argument name is refused `E_WEB_PRIMITIVE`), draw statements append to `rt.marks`, state is `state[slot]` (Map of iteration key to a self-committing `{value, pending, stamp}` cell), regions are `await rt.invoke(k, [])`. The calling convention, the `rt` services and the mark entry shapes are in `js.mli`. Gap found: prims.js `sequence_build` ignores the packed `width` (core.md says a width allocates packed storage). `@lib/flow_web/check` green.
- [x] P5.6 H
  2026-10-11. `lib/flow_web/test_js_frame.ml`, `goldens/frame_*.js` (86): control, list, primitive, region, expression, state, 23 draw and `sketches/flow_vectors` and `examples/basic` (picture graph only) programs, built from `lib/flow_ir/core_fixtures.ml`, the fixture lists lifted out of `test_core_{control,lists,lower,draw,regions,state}.ml` (those tests now read them; `@lib/flow_ir/runtest` green). Every statement and expression form is checked to appear in a golden. Under node (`tools/web/test/{rt_shim,frame_semantics}.mjs`) 76 programs, 337 frames agree with `Core_exec` (exact for ints, text, bools, structure and error codes; floats 1e-12 relative); the eight-frame state programs and the mark tables are included. Not run under node: region programs (no worker pool in the shim), the two packed `sequence_build` programs (prims.js gap) and `workspace_basic` (draw state needs the editor evaluation). `@lib/flow_web/runtest` green; dependency gate green.
- [x] P5.7 S
  2026-10-11. `Js.region : Packed.t -> string` is `function region(inputs, uniforms, outputs, start, stop)` over `Float64Array`s, returning 0 or 1 (non-finite, native `E_KERNEL`). Covers `Collect`, `Sum`, `Accumulate` (fold and scan), several outputs, `Zip`/`Product` and `skip`, so no refusal exists; sums/accumulators keep a trailing state array (`width+1`, last entry the initialized flag) and ranges must run in ascending order. Frame values are the last element of `uniforms`. `test_js.ml` has the four goldens and runs 25 programs under node against `Packed.force_all` in two ranges (within 1e-12 relative; usually bit-equal, noise/sin/cos/sum differ in the last bits, probably FMA contraction and libm vs V8) and skips with a message when node is absent. `@lib/flow_web/runtest` green.
- [x] P5.8 S
  `prims.js` (54 primitives) and `core_fixtures.exe` added 2026-10-11 (commit
  c361391d). 9,524 native cases in 54 files, byte-identical across runs; all
  agree under node. Native details core.md left loose (colour channel check
  order, `%g` in the `nth` message, array length ceiling) are in the fixtures.
- [x] P5.9 H
  Cram test `web_prims.t` and `tools/web/test/prims.mjs` added 2026-10-11
  (commit aacf6bd8): 9,524 cases, 54 primitives, zero mismatches.
  `@tools/lisp/test/runtest` passed; a deliberately broken copy was detected.
- [x] P5.10 S
  2026-10-11. `rays web FILE -o DIR` (`tools/lisp/web_export.ml`, library `web_export`, so `tools/web_parity.exe` shares it): `lower` runs the static stage and `Core_draw.lower`; `files` writes `program.js` (`Js.frame` with packed constants as views into `rt.data`, then `export const regions`), `program.map`, `kernels.json`/`kernels.wgsl` (GPU-legal kernels, `Kernel_table`), `data.bin`, `index.html` and `runtime/` (embedded by `embed.exe` at build time). Formats are in `web_export.mli` and `specification/flow.md` section 12. Refusals with spans: `E_WEB_GRAPH`, `E_WEB_LOWER`, `E_WEB_IMAGE`, `E_WEB_REGION`, `E_WEB_CAPTURE`. Also fixed: `Js.frame` refused every `not` (its argument is named `a`). Gate: `dune build tools/lisp/lisp.exe`, `@tools/lisp/test/runtest`, `@lib/flow_web/runtest`.
- [x] P5.11 H
  2026-10-11. `tools/lisp/test/web.t`: exports `sketches/flow_vectors`, lists the files, a second run is byte-identical, a particles export keeps its arrays in `data.bin`, a draw graph reaching a `sop` graph is refused at the `(ref geometry)` span (`E_WEB_GRAPH`, nothing written), and so is a draw image. `@tools/lisp/test/runtest` green.
- [x] P5.12 S
  2026-10-11. `tools/web_parity.exe [--keep] DIR...` (library `web_export`, so it exports exactly as `rays web`) runs the core executor natively and the exported `program.js` under `node` (`tools/web/test/frames.mjs`: `createRuntime(program, {fixedDt: 1/60, now, size: [640, 480], data})`, `await rt.stepFrame({pointer: [100, 100]})`, no pool, so regions run `rt.region(k)` on the main thread) for 120 frames, dumps each frame's mark table as JSON on both sides and compares: structure exactly, floats within 1e-6, colours, counts, indices and text exactly; a buffer of more than 4,096 numbers by length, first and last 256 and sum. A refusal prints `refused CODE at LINE:COL`; node absent prints a skip. A deliberately changed native value is reported. `_build/default/tools/web_parity.exe sketches/flow_vectors examples/basic` passes.
- [x] P5.13 H
  2026-10-11. All 15 eligible workspaces under `tools/web_parity.exe`: 10 pass (120 frames each), 5 are refused with a span and a code (`examples/drawing` `E_WEB_LOWER` budget; `flow_contours`, `flow_image`, `flow_image_kernel`, `flow_shader` `E_WEB_IMAGE`), 0 fail. Table in `results.md`.
- [x] P5.13a S
  Repair row added 2026-10-11 by P5.R: `examples/drawing` is refused `E_WEB_LOWER` (2,048-node residual budget); lift or split the budget.
  Done 2026-10-11 by the native draw lowering budget raise (32,768, P3.13); the exporter has no budget of its own. `web_parity.exe` over all 15: 11 pass including `examples/drawing` (120 frames), 4 refused `E_WEB_IMAGE`, 0 fail. Addendum in `results.md`.
- [x] P5.R R
  Review 2026-10-11. State of P5.13: 10 pass, 5 refused with a span (`examples/drawing` `E_WEB_LOWER` from the 2,048-node residual budget, repair P5.13a; `flow_contours`, `flow_image`, `flow_image_kernel`, `flow_shader` `E_WEB_IMAGE`), 0 fail. `prims.js` is 382 lines with 56 exports: exactly the 54 primitives of the `core.md` inventory (names equal `Core.primitive_name`, the closed OCaml inventory) plus the value classes `Fail` and `PArray`; no export is an extra operator, so nothing moves into a lowering rule. The rest of the file is private helpers of those bodies (`printG`, `fixed`, `hexColor`, `permutation`, `build`). Outside the inventory and the file: `shape2.js` (134 lines) is a port of `Scene_command.Shape2` tessellation, a second implementation of native geometry (bit-equal by construction, tested under node), which `core.md` does not list. It is the one deliberate duplicate; the review leaves it as the mark contract requires geometry entries, and P6.R re-checks it. Six places `core.md` was looser than native (pinned by `tools/core_fixtures.ml`) are now stated there: `color/parse` check order (blue, green, red; list element 3 down to 0), `%g` in the `sequence/get` message, `sequence/build` placeholders and width, the double array ceiling, no finite check for `sqrt`, `float`, `abs` of a Float, and `color/parse` hex length in UTF-8 bytes. The ceiling clause comes from the native bodies; no fixture count sits between `ceiling/width` and the ceiling.

Phases 6 and 7
- [x] P6.1 S
  2026-10-11. `tools/web/runtime/runtime.js`: `translate(domEvent, rect)` (pure, logical points relative to the canvas rect), `Input` (pointer, held keys/buttons, per-frame ordered events; held keys sorted as native `KeySet`, constructors in declaration order then characters), `createRuntime(program, {now, fixedDt, pool, size})` with `rt.slots`, `rt.region(id)`, `rt.load(program)` (resets slots) and `rt.stepFrame(override)` (clock as `sketch.ml:211`: count from 1, fixed `t = count*dt`, real `dt = min(delta, 0.1)`), `frameValue` (the Lisp `frame/input` Map) and `start(canvas, program)`. Events are `Flow.Op.event_fields` Maps. Ambiguities (wheel scale and sign, unmapped keys, text/drop/pinch/trackpad events, key names under shift) are listed in the runtime header and the task report. `tools/web/test/runtime.mjs` runs under `tools/lisp/test/web_runtime.t`; `@tools/lisp/test/runtest` green.
- [x] P6.2 S
  2026-10-11. `tools/web/runtime/workers.js`: `checkIsolation` (one plain message, no fallback), `createPool({size, spawn})` over one adapter (`spawnWorker`: browser `Worker` from a Blob, or `node:worker_threads` with `eval`), `pool.load(id, source)`, `pool.alloc(n)` (shared `Float64Array`), `pool.collect(id, inputs, uniforms, outputs, count)` (async; chunks of 16 blocks x 1,024 as `packed.ml:208-209`, one chunk or fewer runs in the caller as `parallel.ml:123-137` does, calls served in order) and `pool.region(id)` for the ordered Sum/Accumulate path on the main thread. `tools/web/test/workers.mjs` (cram `web_runtime.t`): the four goldens, byte-equal to one call at 1 and 8 workers for 100,003, 3 x 16,384 and 1,000 elements, plus non-finite status, concurrent calls and the isolation message. Because workers are asynchronous, a frame that uses `collect` must be `async`. `@tools/lisp/test/runtest` green.
- [x] P6.3 S
  2026-10-11. `tools/web/runtime/gpu.js` (`createGpu(table, {onError, win})`, `dispatch`, `poll`, `flush`, `buffer`, `readback`, `dispose`; kernel table format in its header; `runtime.js` takes `gpu` and polls it each frame). `test_wgsl.exe --write-kernel-table DIR` writes `kernels.json`; `tools/web/test/gpu_check.mjs` + `chrome.mjs` (reusable launcher) compile and run the four goldens plus an `ops` kernel in headless Chrome 154 (`--headless=new` alone gives an Apple Metal adapter) and match the CPU regions; alias `@qualification-web`. The first real compile found no WGSL error; the goldens are unchanged. Result and tolerances in `results.md`.
- [x] P6.4 S
  2026-10-11. `tools/web/runtime/present.js` + `marks.wgsl` (port of the Metal `Marks` family; geometry, point sets; transform stack as `compose_raster`; clip as scissor; the six blends). `start` presents after each frame. `present_check.mjs`: 76 pixel probes in headless Chrome, 0 failures; `present_e2e.mjs`: `flow_vectors` and `examples/basic` equal the native picture (0 pixels outside text over 8, worst 1). `@qualification-web`, `@tools/lisp/test/runtest` green. results.md.
- [x] P6.5 S
  2026-10-11. Text (white string on a 2D canvas at dpr, tinted quad) and images (`images(resource)`, linear quad) in `present.js`; text is close, not exact (results.md); images tested by hand-built table only.
- [x] P6.4a S
  2026-10-11. GPU attribute regions. `Web_export.gpu_regions` decides the placement (the legality of `Flow_web.Target.web` for a GPU region: static count of at least 1,024, `Target.region` empty, no skip): a region is GPU when every one of its `Invoke` results is defined once and used only as an attribute of a rect/circle/line `Batch` (positions, from, to of width 2 or 3; radius of width 1; sizes, fill, stroke, color of width 3) and every region-valued attribute of that set is itself GPU; anything else (a frame read such as `array/nth`, a point set, a CPU-only attribute region) keeps the CPU path for the whole set. Attributes of one set with equal count and rate (no frame fact = static) fuse with `Core_fuse.merge` (now exported by `Flow_ir`) into one kernel with an output each. `Js.frame ?gpu` prints those `Invoke`s as `await rt.gpuArray(k)` (default output unchanged: the 86 goldens are byte-identical, new golden `frame_gpu_circles.js`, `test_js_gpu`). `program.js` adds `gpu: {kernelId: {count, inputs, uniforms, frame}}` and `"gpu": {"output": i}` on the member regions. Runtime: `start` requests ONE `GPUDevice` (`gpu.js requestDevice`) for `createGpu` and `createPresenter`; `rt.gpuArray` dispatches the group once per frame (once per load without a frame fact), uniforms by layout name, inputs uploaded once (`gpu.upload`), and returns a `PArray` whose data is the reference `{gpu, output, length}`; the presenter binds `gpu.buffer(kernel:outK)` with explicit size `count*width*4`; status words are checked by `poll()` on the next frame and fail it; `rt.counts = {invoke, gpu}`. Gates: `web_parity.exe` 11 of 15 pass (node answers `gpuArray` with the CPU value in `frames.mjs`); `tools/web/test/gpu_marks_e2e.mjs` under `@qualification-web`: synthetic fused two-output set (2,048 circles) and `flow_particles_gpu` (1,000,000), 5 fixed frames, 0 of 307,200 and 0 of 480,000 pixels differ by more than 8 against `marks_shot`, worst difference 1; 5 GPU dispatches and 0 CPU invokes in both; `index.html` runs with 0 invokes. Metric and numbers in `results.md`. `web.t` pins the program form, fusion and the CPU path for a frame read; `runtime.mjs` pins dispatch counts, rate, reload and errors.
- [x] P6.4b S
  2026-10-11. Generated `image/map` exports through the existing packed/core printers. WGSL derives pixel-center
  UVs with division refinement; scalar dimensions and captures are core-compiled into content-addressed modules.
  The shared WebGPU device converts packed vec4 output to straight-alpha RGBA8 with native clamp/ties-even rules
  and samples its resident texture. Static/shared reuse, resize/reload release, the 64-image capacity and device
  bounds are explicit. `flow_contours`, `flow_image_kernel`, `flow_shader` export; other image kinds keep typed
  refusals. Focused tests pass, previous JS/WGSL goldens unchanged. Commands and pixel results in `results.md`.
- [x] P6.4c H
  2026-10-11. `gpu_images_e2e.mjs` compares the synthetic, contour, gradient and shader pictures with native
  snapshots/display using the unchanged pixel metric: zero pixels differ by >8 in every fixture; contours and
  shaders differ by at most 1 channel level. Pins RGBA ties `[0,2,2,128]`, static/shared dispatch counts, odd-width
  rows, transformed alpha, reuse/resize/reload and invalid dimensions. The actual watch server reloads scalar
  captures while reusing unchanged pixel pipelines; a refused edit keeps the last good export. `web_parity.exe`
  passes 120 frames for 14 of the original 15 workspaces, with one explicit `image/noise` refusal. All previous
  `@qualification-web` checks, `--ship` and `@lib/flow_gpu/runtest-native` pass. The shipping workspace sweep covers
  43 standard files, one custom-catalog executable and 13 fixtures, at four times and one/eight domains.
- [x] P6.4d S
  2026-10-12. All three remaining image producers export. Static `image/load` paths resolve as on native;
  SDL3_image decodes every supported format, including EXIF orientation, into bundled RGBA8 bytes.
  `image/noise` reuses double noise with native defaults, frequency alias, wide/negative seeds and ties-even
  conversion. `image/render` uses the existing presenter on transparent RGBA8 offscreen targets at DPR 1,
  recursively resolving images and keeping main-frame facts distinct from target size. Scalar arguments and
  drawings share core/JS lowering, namespaced GPU regions and the shared CPU worker pool. Folds share slots
  by native instance/path/plane. Static/shared rates, texture ownership and device bounds remain explicit.
  Watch tracks imports/assets and synchronizes replacement with executing frames; bad assets preserve the
  last good export, repaired assets retry, and edits during export cannot be swallowed by its fingerprint.
- [x] P6.4e H
  2026-10-12. New file/noise/render fixtures match native RGB pixels exactly at all tested sizes and frames.
  The eight native decoder fixtures include PNG/JPEG/BMP/GIF/QOI/WebP/TGA and EXIF-rotated JPEG. Live noise,
  shared folds, nested alpha/transforms, image dimensions differing from main-frame facts, oversized target
  backgrounds, GPU/worker regions, static/shared counts, reuse/resize/reload and target bounds are pinned.
  Asset-only watch edits update resident pixels; corruption preserves the bundle and repair actually retries.
  Runtime tests pin in-flight frame/reload ordering; workers retire removed regions and reload byte-exactly.
  Focused aliases, all web qualification checks and `--ship` pass. Full 15-workspace 120-frame mark parity
  passes; all 15 open in Chrome. Existing WGSL goldens are unchanged; six JS frame goldens change only for
  awaited image resolution or explicit drawing-target background size. Commands and results in `results.md`.
- [x] P6.6 S
  2026-10-11. `rays web --watch FILE [-o DIR] [--port N]` (`tools/lisp/web_server.ml`, `.mli`; `lisp.ml`; stdlib and `unix`): one `select` loop on 127.0.0.1 (default port 8000, 0 picks a free one and the banner `serving http://127.0.0.1:PORT/` says which), polls the SHA-256 of FILE every 0.5 s, re-exports on change (`web_files` = check, `Web_export.lower`, `Web_export.files`; the files are renamed into DIR), and on a refusal prints the diagnostic and keeps serving the last good export (a refused text is not retried until it changes). GET only (405), paths with `..` or outside DIR are 404, every response carries COOP `same-origin` and COEP `require-corp`, `/version` is the SHA-256 of the export files. Imports of FILE are not watched. Gate `tools/lisp/test/web_server.t` (client `http_get.exe`): headers on 200, 404 and 405, content types, traversal attempts, version change on edit, refusal keeps the version. `@tools/lisp/test/runtest` green.
- [x] P6.7 H
  2026-10-11. `runtime.js` `watch(rt, opts)` (called from `start`, which also exposes `window.__rays` and an `rt.resume` that revives a frame loop a failed frame stopped): polls `/version` every 0.5 s; a changed version re-imports `program.js?v=`, refetches `data.bin` and `kernels.json`, calls `gpu.update(table)` (new in `gpu.js`: only kernels whose WGSL, entry, bindings or noise seeds changed are compiled) and `rt.load`; `/version` 404 or a fetch error stops polling silently. `rt.load` now keeps compiled regions by source text and sends the pool only changed sources (it also read the original program for `region(k)`; fixed). Gates: `tools/web/test/runtime.mjs` (fake fetch/import/gpu: swap, slot reset, no recompilation of unchanged text, silent stop) in `@tools/lisp/test/runtest`; `tools/web/test/reload_check.mjs` in headless Chrome 154 under `@qualification-web` (edit picked up without a navigation, one reload), recorded in `results.md`. Note: `web.t` lists `runtime/` files and needs `marks.wgsl` and `present.js` from the presenter task.
- [x] P6.8 S
  2026-10-11. `tools/web_shot.ml` (+ `tools/web/test/web_shot.mjs`, `tools/dune`): the OCaml tool renders the native frames (`marks_shot`), starts `rays web --watch` on 127.0.0.1 with COOP/COEP, and shells out to node, which drives the local headless Chrome over the DevTools protocol (`chrome.mjs`; no npm): the page runs `start`, the script halts the rAF loop (`rt.halt`, `rt.presenter` and `rt.error` are new), resets the state and steps `__rays.stepFrame` with a fixed clock to frames 1, 30 and 120 of `examples/particles` at 800x600, then compares canvas pixels with the native render (0.5% of pixels above 8, none above 64): 0 of 480,000 pixels differ at all three frames, worst 0. Exit 3 = WebGPU unavailable (verified with a Chrome started without a GPU), a missing node or Chrome is a printed skip. `dune build @qualification-web` green. Numbers in `results.md`.
- [x] P6.R R
  Review 2026-10-11. (1) All 15 eligible workspaces: 11 open in headless Chrome 154 through `index.html` (`start`: one device, worker pool, presenter; at least 5 frames, still advancing 0.5 s later, no error, marks present; `tools/web/test/open_all.mjs`, in `@qualification-web`) and 4 are refused at export with a code and span (`flow_contours` `E_WEB_IMAGE` 77:3, `flow_image` 5:3, `flow_image_kernel` 7:3, `flow_shader` 67:3); none fails. Per-workspace table in `results.md`. (2) `_build/default/tools/check.exe --ship` (`@all`, `@runtest`, `@smoke`, `git diff --check`) exit 0 on 2026-10-11 with the other agent's native Phase 4 work in the tree. (3) `grep -rniE 'http|socket' lib/`: no file serves HTTP; the matches are a W3C URL in a `flow_web/target.mli` comment, a rule in `lib/runtime/AGENTS.md` forbidding HTTP, and UI wire "sockets" in `flow_graph`, `flow_sop`, `pxui*`. (4) Ruling on `shape2.js` (134 lines, a port of `Scene_command.Shape2`): it is geometry for the mark contract (`marks.md` geometry entries), not a core primitive; no target may tessellate otherwise. Kept; `core.md` and `marks.md` now say so in one sentence, and `tools/web/test/shape2.mjs` pins it to the native output (indices and colours exactly, vertices within 1e-12). (5) Found and repaired in the phase: P6.4a (every region ran on the CPU), P5.13a (`examples/drawing` exports). Open for P7: a static CPU attribute buffer is converted to f32 and uploaded again on every present (no cache by array identity); a region's captures must be fixed at export and may read only the frame facts `t`, `frame/dt`, `frame/index`, `frame/width`, `frame/height` (`E_WEB_CAPTURE` otherwise), so pan and zoom must be `draw/translate`/`draw/scale` transforms, not data computed from the pointer.
- [x] P7.1 S
- [x] P7.2 H
- [x] P7.3 S
- [x] P7.4 H

## Native parity of the fast paths (2026-10-27)

The web target follows the native drawing optimizations instead of a separate, slower path. Measured on
recursive_rectangles (600x560, pointer alternating at 90% height, headless Chrome on the M-series,
`node tools/web/test/web_bench.mjs LISP SKETCH 30`): 192 ms a frame before, 5.6 ms after; a still
picture 0.2 ms (loop memory). Native draws the same frame in about 7 ms.

| Native | Web | Where |
| --- | --- | --- |
| A drawing keeps a region with arguments or frame inputs a region (`Core_lower`, draw) | The export accepts a region bound per call (`Js.bindable`) next to fixed ones; the binding plan prepares it each call | `tools/lisp/web_export.ml`, `lib/flow_web/js.ml` |
| `Core_exec.prepare ~remember_loops` | The frame keeps a top-level loop's or region call's last 4 results by its reads (`Core_exec.footprint`, `rt.recall`/`rt.keep`, `sameInput` mirrors `same_input`) | `js.ml`, `prims.js` |
| `:precision "fast"` (`Contexts.fast`) | The export reads the same setting; fast regions keep integers as exact 64-bit register pairs of plain numbers (`Js.region ~fast`, `fast64`), no BigInt per element | `js.ml`, `web_export.ml` |
| `Emit.kernel ~fast` | `Wgsl.kernel_fast`: `vec2<u32>` integer pairs, gathers, count words and a 40-byte count record (native's six words, then `drawIndirect` arguments) | `lib/flow_web/wgsl.ml` |
| Integer arithmetic stays 64-bit through float operators (`Emit` `whole`) | The same in WGSL (`whole`) | `emit.ml`, `wgsl.ml` |
| `Host.resident` | `resident.js`: placeholders, device counts from the plan's source shapes (`Packed.Private.source_shapes`), shared index buffer, compaction (`Wgsl.compaction`), per-level submits, one readback of the live writes (`Core_exec.resident_live`) | `tools/web/runtime/resident.js`, `gpu.js` |
| Display regions on the device, mark sets drawn indirect | `rt.gpuInvoke` for a display region bound per call; the presenter draws from its count record (`drawIndirect`) | `runtime.js`, `present.js` |

Checks: `lib/flow_web/test_js.ml` (fast 64-bit pairs against native exact past 2^53),
`tools/web/test/gpu_fast_check.mjs` (fast WGSL against native in Chrome), `tools/web/test/resident.mjs`
(a reused placeholder reads its own fold back), and in `@tools/qualification-web` recursive_rectangles at a
deep, moving pointer against native fast precision (0 pixels over 8, worst 1).
