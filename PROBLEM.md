# Residency-aware mesh execution and compiler fallback performance

## Request and intended outcome

The user asked for a port of <https://github.com/crystalboxes/isogrid-houdini-sop>
as a Rays 3D sketch, without adding an IsoGrid SOP node. That port now lives in
`sketches/isogrid.rays`. The user also wants the compiler to choose CPU or GPU
execution using the data's residency and the costs of moving it.

The follow-up problem is architectural: a mesh assembled from GPU-produced arrays
should not have to become host arrays merely because it crosses `sop/mesh` or
enters the 3D viewport. Placement should consider the producer, subsequent
operations, transfer costs, and the final consumer. A CPU reader may need host
storage; a GPU renderer should be able to consume suitable buffers directly.

Implement this through the shared compiler, geometry carrier, and renderer paths.
Do not solve it with an IsoGrid-specific renderer, a custom node, a sketch-only
GPU implementation, or a permanent “force GPU” switch.

The user subsequently reported that changing the recipe selector takes several
seconds and that resolution 64 is extremely slow. They explicitly rejected fixing
this by rewriting the sketch into compiler-friendly forms and asked for those
workarounds to be reverted. **Fix the shared compiler/execution behavior for the
ordinary authored Lisp.** Residency and CPU evaluation/fallback performance are
related but separate parts of this problem; neither can stand in for the other.

This document is a handoff, not an implemented fix. References below describe the
working tree inspected on 2026-10-31, at HEAD `03f1e062` with uncommitted changes.
Recheck the current code before editing: other work already changes Flow GPU
execution, resident loops, drawing, and web export in this same worktree.

## What has actually been proved

The sketch renders in the native 3D viewport. It generates exposed voxel faces,
flat normals, and the yellow/cyan/magenta axis palette. It now has eight recipes,
selected by the `volume/recipe-switch` integer card, with a default of 1 (Porous
Citadel), resolution 24 and seed 17. Preset 0 is the maze. The other presets combine
carving, edge/wire transforms, slicing, mirroring and architectural seed fields.
The user rejected the current presets as visually boring; they are not evidence
that the separate artistic request has been satisfied. Keep them usable as
performance reproductions without treating visual redesign as the compiler fix.

`tools/test_isogrid.ml` loads the sketch's actual `defn` forms and tests them in a
small separate workspace. Its native qualification path compiles and explicitly
dispatches two 8,000-element kernels: carving and hollowing. It compares each
readback with its CPU packed execution. The last observed successful run printed:

```text
GPU: 2 pipelines, 2 status reads, 64000 readback bytes
isogrid checks passed
```

Those readbacks are intentional parity-test readbacks. This proves kernel
compatibility and numerical parity for these cases. It does **not** prove:

- automatic production placement of the original sketch;
- GPU execution of its entire recipe or its maze walk;
- resident exposed-face compaction and vertex generation;
- resident mesh topology construction;
- a viewport frame that consumes those buffers without reading them back;
- correct ownership when a mesh outlives the dispatch that produced it.

Do not use these two successful kernel tests as evidence that the requested
end-to-end behavior already exists.

## Reproduction and existing checks

Run from the repository root with its opam environment:

```sh
opam exec -- dune build tools/check.exe tools/test_isogrid.exe bin/rays.exe tools/ui_shot.exe
_build/default/tools/check.exe @tools/isogrid
_build/default/tools/check.exe @tools/isogrid-native
_build/default/bin/rays.exe check sketches/isogrid.rays
RAYS_MAX_FRAMES=3 _build/default/bin/rays.exe sketches/isogrid.rays
_build/default/tools/ui_shot.exe sketches/isogrid.rays /tmp/isogrid.png 1280 900 2
```

`isogrid` is the CPU behavior check. `isogrid-native` requires native Metal and
performs the explicit kernel parity checks above. Neither alias currently asserts
resident mesh construction or production viewport placement.

Before the eight-preset expansion, a combined CPU/native focused run took 1.84
seconds wall time; the sketch check took 1.13 seconds and a bounded native launch
took 1.59 seconds. Those are historical numbers for the smaller earlier sketch.
After the preset expansion and the requested revert, the CPU focused alias took
4.28 seconds. A preceding native qualification run with all presets took 7.54
seconds, including its repeated evaluations. These are local observations, not
performance guarantees, switching latency, or CPU/GPU speedup measurements.

Use the repository's `tools/check.exe` launcher for focused validations. Do not run
the entire `@runtest` to check each edit, clean the build, or increase timeouts to
hide slow tests. Read root and affected nested `AGENTS.md` files before changing
their directories.

## Additional problem: ordinary Lisp falls into expensive evaluation paths

### User-visible failure

Dragging `recipe-switch` from 1 to 3 was verified to change Citadel to Coral in the
actual graph. However, the edit status strip reported approximately 4,376 ms in
`evaluate`, compared with about 32 ms in `check` and 11 ms in `cook`. That was a
24-cubed case. The selector works, but its latency is unacceptable for exploration.

At resolution 64, there are 262,144 voxels and 1,572,864 potential face entries.
Compared with resolution 24, this is about 19 times as many voxels. That explains
why unnecessary per-element interpretation becomes much more visible; it does
not justify multi-second or tens-of-seconds evaluation of ordinary numeric maps.

Do not respond by lowering the requested resolution, reducing the recipe's detail,
removing colors, substituting a simpler shape, or moving a manual implementation
into the sketch. The user specifically wants the compiler to handle these forms.

### Measurements collected during the investigation

All commands below were run locally with `/usr/bin/time -p`, using the existing
native dev binaries. The machine identifies itself as ARM64 macOS; the samples
are not a controlled benchmark series and were not collected at identical load.

| Observation | Wall time | What it proves and what it does not |
| --- | --- | --- |
| 64-cubed sketch check, after splitting neighbour edits but before packed lookup tables | 48.55 s | A real high-resolution evaluation problem. This is **not** a pristine pre-workaround baseline: one experimental rewrite was already present. |
| Same general 64-cubed case with additional packed lookup-table changes | 29.31 s | Partial improvement, still unusably slow; not an accepted fix. |
| 24-cubed check after those additional changes | 2.93 s | Even the smaller case retained noticeable startup/evaluation cost. |
| Temporary reduced 64-cubed value-only diagnostic | about 3.2–3.7 s | A different workload that removed the actual mesh sink and returned positions; cannot be used to claim the full sketch runs in this time. |

A one-second native `sample` during the 48.55-second run showed hot stacks in
`Flow_ir.qualify_workspace` and `Flow.Eval` functions (`ev`, operator application,
scalar evaluation, list operations). The observed physical footprint was about
258 MiB. This is evidence to investigate interpreter/qualification work, not proof
of a complete cost breakdown or that GPU rendering caused the delay.

Reproduce from the restored source and establish a fresh baseline. Temporary
files and sampled binaries may no longer match the current worktree. A safe
scratch-copy command for changing only the resolution is:

```sh
ruby -e 's = File.read("sketches/isogrid.rays"); abort "resolution default moved" unless s.include?("(resolution : int 24)"); File.write("/tmp/isogrid-64.rays", s.sub("(resolution : int 24)", "(resolution : int 64)"))'
/usr/bin/time -p _build/default/bin/rays.exe check /tmp/isogrid-64.rays
```

Use bounded, measured runs and the repository validation launcher for regression
tests. The command above measures complete command startup/checking, not a warm
in-process graph edit. Measure the real selector gesture separately, including
the editor's per-phase timing and the time until the new geometry is presented.

### Concrete compilation refusals observed

A temporary diagnostic extracted the sketch's definitions and volume graph,
converted the mesh-array portion to a host graph, and observed compilation during
`Flow_ir.qualify_workspace`. During experimental rewrites it printed:

```text
def:carve/@result: packed
sculpture/directions: packed
sculpture/corners: E_PACKED_CAPTURE: Capture corner-list has unsupported type list:vec3.
sculpture/faces#0: E_PACKED_FORM: Packed form requires reference evaluation.
sculpture/positions: packed
sculpture/normals: packed
sculpture/colors: packed
```

These lines describe that reduced diagnostic and its then-current source, not a
complete placement trace for the restored production sketch. In particular,
`corners` had already been rewritten into a small map over an ordinary captured
list. Do not present that specific refusal as an exact trace of the current file.

The face map still refused after replacing lookup lists with packed arrays,
extracting `volume.voxels` into a separate binding, and renaming the local
`direction` variable. Its refusal remained generic. Therefore the earlier claim
that ordinary list lookups were *the* cause was too strong. They were an observed
compilation obstacle, but the remaining face-map refusal and the complete
production evaluation cost were not resolved.

The recipe's fold over a list of text commands also reported `E_PACKED_TYPE`.
That does not imply that the text recipe interpreter itself must run on the GPU:
selecting static commands on the host can be correct while their numeric voxel
kernels compile. Avoid conflating orchestration refusal with failure to optimize
the expensive inner maps.

### Workarounds tried and explicitly reverted

The following experimental edits were removed at the user's request:

- Splitting `edit-cells` into a neighbour-stencil helper and a separate map for
  transforms/slices, manually hoisting operation dispatch outside the traversal.
- Replacing the literal direction and corner lists with packed lookup arrays.
- Adding a `voxel-data` binding solely to avoid a record-field capture.
- Renaming a local binding while testing a possible helper-name interaction.
- Inlining face-neighbour indexing and bounds logic in place of the readable
  `occupied` helper call.
- Adding a temporary `--profile` mode to `tools/test_isogrid.ml`.

The current sketch again contains the ordinary list-based direction/corner tables,
the shared `edit-cells` helper, and the readable helper-based face predicate. The
eight recipes and graph selector remain. The CPU focused check passed after the
revert. No compiler fix was implemented in that investigation.

**Do not quietly reintroduce these rewrites as the solution.** Use the restored
forms as compiler regression inputs. Equivalent packed/manual versions may be
useful comparison cases, but they are not replacements for the authored program.

### Compiler investigation required

Trace both successful compilation and actual execution. A reported `packed`
candidate is not sufficient if a later failure or consumer forces reference
evaluation. Identify which map is fully walked, which is only sampled, what
caused the refusal, and whether the same work is repeated during checking,
qualification, lowering, cooking, or a recipe edit.

Inspect at least these existing paths:

- `lib/flow/eval.ml`, `hof`: static numeric maps larger than `max_iterations`
  first interpret 4,096 elements, then offer a `Bulk_static` result. A supplied
  result must match the walked prefix; otherwise the walker finishes the array.
- `lib/flow_ir/flow_ir.ml`, `static_map`, `static`, and `qualify_workspace`: the
  CPU packed handler, actual-capture qualification, and GPU approximation handler.
- `lib/flow_ir/core_region.ml`, captures, helper inlining, invariant values,
  fixed-list indexing, and the generic `Unsupported` exception conversion to
  `E_PACKED_FORM`.
- `lib/flow_ir/test_static_map.ml` and relevant `lib/flow/test_workspace_eval.ml`
  tests: current value, error, call-index and probe-record parity expectations.
- `lib/flow_ir/core_lower.ml` and executor fallback paths: determine whether a
  successfully compiled subregion is lost at a surrounding structural operation.

Preserve language semantics while optimizing: ordinary lists still behave as
lists, out-of-range indexing still reports the correct error, and lazy branches
must not introduce errors from untaken arms. Immutable constant vector lookup
tables should be candidates for internal specialization without requiring a
different authored storage type. Static command selection and loop-invariant
operands should not be recomputed for every cell when the compiler can prove
otherwise.

The 4,096-element prefix is also potentially expensive, but do not arbitrarily
shrink or remove it to manufacture a better benchmark. Its current behavior is
coupled to recorded probes, function call indices and error/parity checks. If that
contract changes, define the replacement, update the relevant specifications and
prove the same observable behavior through focused tests. Likewise, do not add a
large global memo or cache results under incomplete keys to hide reevaluation.

Improve refusal diagnostics enough to locate the unsupported term or capture.
“Packed form requires reference evaluation” is too indirect to explain why a
million-element mesh map is unexpectedly interpreted. Diagnostic instrumentation
should expose the real fallback and its cost without becoming product UI clutter.

### Additional performance acceptance checks

1. The restored readable sketch works at resolutions 24 and 64 without manual
   list-to-array conversion, duplicated stencils, or inlined neighbour logic.
2. Fixed vector-list indexing, helper calls, record-field captures, invariant
   command selection and the face-map predicate have small independent compiler
   regression cases. Unsupported operations retain correct reference behavior.
3. The original expensive numeric maps execute through the intended compiled
   path. Assert/observe actual fallback counts and materialization, rather than
   accepting a successful qualification message alone.
4. Report cold load/check and warm selector edits separately for multiple recipe
   directions (including 1→3 and back), seed edits, and 24→64→24. Validate the
   actual resulting geometry, so stale cached results cannot pass a speed test.
5. Quantify reductions from a fresh restored-source baseline: elapsed time,
   allocations, interpreter work, compilation, CPU kernel work, GPU dispatch,
   transfers, and presentation. State any remaining bottleneck honestly. A
   29-second 64-cubed check is explicitly not completion.
6. Maintain exact CPU behavior and appropriate CPU/GPU parity, including empty
   geometry, boundaries, invalid indexing and errors past the sampled prefix.
7. Retain bounded caches and lifetimes under repeated selector changes; no growing
   retained source trees, arrays, pipeline sets, or GPU registrations.

There is no measured millisecond latency target yet. Establish and report an
interactive budget from real hardware measurements; do not invent an unsupported
“instant” claim. Both this performance problem and the residency requirements
below remain outstanding.

## Current data path and concrete boundaries

### 1. The Lisp contract does not specify CPU storage

`lib/flow_sop/operators.ml`, `mesh_fields`, `mesh`, and `mesh_planes` declare:

| Input | Logical type |
| --- | --- |
| `points` | packed vec3 array, or x/y/z float planes for `mesh_planes` |
| `vertex_points` | integer array |
| `primitive_offsets` | integer array |
| `kinds` | integer array |

The result is geometry. The logical types do not require user-authored CPU arrays.
“`sop/mesh` only accepts CPU arrays” is shorthand for its present implementation,
not a language restriction that should be preserved.

### 2. Mesh lowering forces values into a host topology constructor

In `lib/flow_sop/lower.ml`, trace:

```text
geometry_arrays_node
  -> geometry_arrays_node_in
  -> Attribute_kernel.prepare, sink = Flow_ir.Sop_input
  -> Flow_ir.Executor.force
  -> Topology_kernel.mesh
  -> Topology_kernel_impl.mesh
```

There is also a `host_geometry_arrays` route for calls encountered during host
execution. Audit both routes and every caller of the shared functions; fixing
only one call site would leave sibling construction paths with the same problem.

In `lib/flow_sop/topology_kernel_impl.ml`:

- `mesh_points` matches concrete `E.Vec3_array` or concrete x/y/z `E.Float_array`
  storage, validates it, and produces RDK position planes.
- `mesh` matches concrete `E.Int_array` topology inputs, checks primitive kinds,
  copies the indices and offsets, converts kinds to native constructors, and calls
  `Rdk.Topology.create_owned`.
- These copies enforce immutable ownership. Removing them without replacing the
  ownership contract is not a valid optimization.

`lib/rdk/core/topology.mli` exposes `create_owned` with OCaml `int array` and
`primitive_kind array` arguments. Counts, indexing, primitive ranges and topology
queries are currently expressed around that host representation.

Thus changing a placement heuristic alone cannot produce resident topology. The
consumer needs a representation and implementation capable of retaining it.

### 3. Residency already exists for positions and attributes

Do not rebuild residency from scratch. Relevant existing mechanisms include:

- `Rdk.Packed.Deferred` in `lib/rdk/core/packed.mli`: opaque extensible holders,
  deferred materialization, and release callbacks.
- `Rdk.Packed.Float3.deferred`: device-held position planes, metadata available
  without materialization, and an optional bounds operation.
- `Rdk.Attribute.create_deferred`, `resident`, `holder`, and `releaser` in
  `lib/rdk/core/attribute.mli`.
- `Flow_ir.Gpu.planes` in `lib/flow_ir/flow_ir.mli`: retained output planes,
  resident values, settlement, release, and bounds.
- `Flow_ir.Executor.record_on_device` in `lib/flow_ir/flow_ir.ml`: fused SOP
  attribute writes can discover resident inputs and retain output planes.
- `Flow_gpu.Host` and `Flow_gpu.Run`: pipeline/runner ownership, retained outputs,
  generation/status checks, and transfer/materialization counters.

The gap is not “Rays cannot hold any geometry on the GPU.” It can already hold
parts of geometry there. Fresh topology and the complete mesh consumer chain do
not have the same residency contract.

### 4. The existing device mesh still has host indices and other attributes

`Rays.Mesh.View.create_device` in `lib/rays/mesh.mli` retains device positions and
optionally normals under a registration key, but explicitly accepts host arrays
for indices, colors and texture coordinates. Its optional host thunk supplies a
host mesh for consumers that need one.

`Renderer.register_gpu_positions` in `lib/renderer/renderer.mli` borrows position
and normal buffers with a validity callback. It does not expose a complete
resident indexed-topology registration.

`lib/renderer/scene_execution.mli`, `Private.register_gpu_vertices`, documents
the Scene3 device path as packed float3 positions with host indices and an
attribute record. It also documents indirect drawing for existing mark streams.
Do not confuse the Marks indirect path with a completed resident Scene3 mesh path.

OGPU already exposes indexed and indexed-indirect drawing. Inspect the existing
capabilities before proposing new Metal or OGPU APIs. A missing connection between
existing APIs is not proof that a new backend capability is needed.

### 5. IsoGrid hits additional viewport restrictions after construction

In `lib/rdk/rays/surface.ml`, `device_points` currently allows resident positions
only for a restricted display case: a point cloud or all-triangle topology,
resident normals when present, and no listed color/UV/piece/material attributes.
Otherwise `of_geometry` goes through `of_host`.

IsoGrid currently emits polygon quads and explicitly writes point `Cd` colors.
Both matter. Retaining positions at `sop/mesh` alone would not make this original
colored quad mesh travel through the existing device display path.

Audit conversion/triangulation, normals, color binding, bounds and renderer upload
behavior as separate possible materialization points. An uncolored triangle test
is useful as a first milestone, but does not establish completion for IsoGrid.

### 6. Resident compaction and device counts already exist elsewhere

`lib/flow_ir/core_exec.mli` describes `resident_live`, `resident_tail`, and a
`resident` interface with invocation, compaction, retyping, range creation,
settlement, and explicit materialization. Its documented integration is currently
the structured drawing path.

`lib/flow_gpu/host.mli` and `specification/gpu-codegen.md` describe resident folds,
count records, counted/indirect dispatches, capacity history, and overflow reruns.
Related web implementations exist in `lib/flow_web/js.ml` and
`lib/web_export/runtime/resident.js`.

These are relevant building blocks for exposed-face compaction and subsequent
vertex generation. They are not evidence that the SOP mesh path already uses
them. Determine which parts can be reused without coupling geometry to drawing
or introducing another executor.

### 7. Production placement is distinct from legality and qualification

Inspect `Flow_ir.Gpu.config`, `Cost.sop_device`, static-map qualification and
display placement in `lib/flow_ir/flow_ir.ml`.

- Qualification policy can exercise legal GPU kernels independently of a normal
  production cost decision.
- The generic backend supplied by `Flow_gpu.Host.backend` currently returns
  `None` from its `cost` callback. Its interface explicitly states that absent
  cost information retains production CPU placement on the applicable path.
- Fused SOP writes have a separate `Cost.sop_device` policy that includes count,
  noise work and resident inputs. Do not assume all call paths share one decision.
- Region legality, placement, retained outputs, and rendering are separate checks.

An end-to-end fix must trace the actual production route. Changing tests to force
qualification everywhere would conceal the issue rather than solve it.

## Desired behavior

1. Preserve the authored Lisp API. Ordinary arrays and `sop/mesh` should remain the
   way a sketch expresses geometry; no raw GPU handles or backend selection in Lisp.
2. Represent enough geometry state to retain suitable generated positions,
   topology, attributes and counts on the device until a consumer needs host data.
   Extend existing carriers where practical; do not prescribe a second mesh system.
3. Make placement account for existing residency and consumer requirements, rather
   than evaluating each map as an isolated upload/dispatch/readback trip.
4. Allow the native 3D viewport to consume a suitable resident mesh without payload
   readback and re-upload. Include the original colored IsoGrid geometry, not only
   a reduced triangle/uncolored substitute.
5. Keep real CPU consumers correct through deliberate materialization. Existing RDK
   algorithms must not receive dummy arrays or silently see an empty topology.
6. Preserve static caching. A static sketch should cook once and retain its result;
   putting it on the GPU must not make unchanged geometry rebuild each frame.
7. Keep unsupported operations on the CPU when necessary. Residency awareness does
   not require porting every geometry algorithm to the GPU in this change.

## Contracts the implementation must resolve

### Topology validation and publication

Define where validation runs and when a mesh becomes publishable. Preserve the
current rules for indices, offsets, primitive counts/kinds and finite positions.
Inspect `Topology.create_owned` and its callers for the exact existing contract.
Cover negative/out-of-range point references, malformed/nonmonotone offsets,
incorrect terminal offsets, inconsistent lengths, invalid kinds, empty geometry,
and unsupported or oversized counts.

GPU-produced input is still input that needs validation. Do not remove checking
because the buffers were generated internally. A compact device status/error
record can be compatible with avoiding a payload readback. Distinguish those
categories explicitly in measurements. Preserve typed diagnostics, cancellation,
and the rule that a failed cook does not publish partial geometry.

### Counts, allocation and integer semantics

Separate logical count, allocated capacity, byte width and device count records.
Do not read a full index buffer merely to discover its count. Define the device
limits and checked conversion from Rays integers to GPU draw/index formats; keep
the language's exact integer semantics for topology decisions.

For compacted output, cover zero elements, partial workgroups, capacity growth and
overflow recovery. Reuse existing counted execution machinery when appropriate.
Never draw unused capacity or silently truncate an expanding mesh.

### Ownership, domains and materialization

Specify which owner retains each buffer across cooking, editor history, retained
scenes, multiple viewports, asynchronous rendering, and renderer/path-tracer use.
An old retained mesh must not start displaying a new dispatch's reused buffers.

Preserve generation/validity checks, bounded caches, explicit release and native
initial-domain rules. A deferred CPU reader may execute from another domain;
materialization must respect the existing threading and GPU service contract.
Test reads and destruction in both orders, cancellation during a cook, stale
registrations, source edits, closing a workspace, and pool reuse after release.

Metadata such as identity, count and capacity should not accidentally force a
payload. Bounds must have a deliberate path: a small GPU reduction/readback may
be appropriate, whereas implicitly materializing all positions to frame the camera
would defeat residency. Reuse current deferred bounds support.

### Consumers and fallback boundaries

Trace the raster viewport, wire view, triangulation, normal generation, color/UV
binding, shadow bounds, picking/inspection, CPU geometry operations and path tracer.
Document which paths remain resident and which legitimately materialize.

Do not require every consumer to become GPU-native immediately. Do require a CPU
consumer to obtain the correct immutable host data, and the normal supported
raster path to avoid accidental transfers. Treat path-tracer support as a concrete
compatibility decision, not an assumption based on raster success.

### Placement and diagnostics

Measure cold compilation, warm dispatch, uploads, readbacks, CPU kernels and
downstream consumption separately. Compare CPU-origin small meshes, GPU-origin
meshes, mixed-origin inputs, multiple consumers and topology-changing frames.

The policy should be able to choose CPU for a small CPU-origin workload and retain
GPU work when doing so avoids a transfer. Do not invent an unmeasured universal
size threshold or automatically equate “resident” with “always faster.”

Use or extend existing profiling to explain the selected tier and materialization
reason. Distinguish unsupported form, missing consumer support, validation read,
CPU consumer, absent cost evidence and measured CPU preference. A pipeline count
alone cannot explain placement.

## Suggested implementation sequence

These are investigation/implementation stages, not prescribed new APIs.

1. Establish a baseline from the real sketch and a minimal triangle workspace.
   Trace where each buffer first becomes host data; add the smallest missing
   instrumentation needed to distinguish execution, transfer and consumption.
2. Decide how existing deferred carriers can represent resident topology and
   counts while preserving RDK's UI/GPU-independent dependency boundaries.
3. Connect the existing compiler execution path to that carrier. Test ownership,
   validation and explicit CPU materialization before expanding display support.
4. Connect native indexed rendering and necessary count handling. A simple triangle
   is an intermediate check; retain colors and handle IsoGrid's quads before
   declaring the triggering case solved.
5. Integrate production placement using measured evidence. Keep qualification as
   a test policy, not the production implementation.
6. Run and report actual IsoGrid end-to-end behavior, compatibility checks and
   before/after measurements. Update architecture specifications to match.

## Acceptance evidence

| Requirement | Evidence needed |
| --- | --- |
| Original sketch works | Open the actual `sketches/isogrid.rays`; inspect the colored maze and at least one carved recipe; orbit, frame and switch relevant viewport modes. |
| Compiler chooses placement | Observe the ordinary production route and its placement reason; an explicit test-only `backend.prepare/run` call is insufficient. |
| Mesh payload stays resident | Count position, topology and attribute materializations/readbacks across generation, mesh creation and raster submission; explain every transfer. |
| Correct rendering | Compare CPU and resident mesh geometry/topology and rendered output under the repository's precision/tolerance contracts; preserve winding, normals and palette. |
| Dynamic topology works | Test compacted empty/nonempty output, increasing/decreasing counts, reused capacities and overflow recovery without stale or excess triangles. |
| CPU interoperability works | Feed the result into representative existing CPU topology/attribute consumers and compare with the normal host result. Materialization is intentional and reusable. |
| Lifetimes are correct | Retain old/new meshes simultaneously, use multiple viewports, cancel/recook, close and release; verify validity and bounded buffer/registration counts. |
| Validation remains real | Malformed GPU-origin topology produces a typed error before unsafe rendering or publication. |
| Static work remains cached | Warm unchanged frames do not reconstruct topology, compile pipelines, allocate size-dependent buffers or transfer full payloads. |
| Performance improves where claimed | Record commands, machine, input size, precision, cold/warm wall time, allocations, upload/readback bytes and dispatches. Include a CPU-origin counterexample. |
| Architecture remains valid | Dependency gate and focused owner tests pass; required backend boundaries remain intact. |

For the fully supported resident raster route, payload readback should be zero
between GPU production and drawing. Small, explicitly accounted-for status,
validation or bounds reads are a different category. Tests must make this
distinction; don't hide payload transfers inside generic “readback” totals.

Normal frames that genuinely require CPU-only work may materialize. Do not claim
the entire workload is resident in those cases, or force GPU placement merely to
make the acceptance report sound stronger.

## Test and source map

Start with these existing locations and extend the tests nearest the boundary:

| Area | Files/directories |
| --- | --- |
| Trigger and existing parity test | `sketches/isogrid.rays`, `tools/test_isogrid.ml`, `tools/dune` |
| Operator and geometry lowering | `lib/flow_sop/operators.ml`, `lower.ml`, `topology_kernel_impl.ml`, `attribute_kernel.ml` |
| Mesh validation and array behavior | `lib/flow_sop/test_geometry_arrays.ml`, `lib/rdk/core/topology.ml`, `topology.mli` |
| Deferred storage and ownership | `lib/rdk/core/packed.ml`, `packed.mli`, `attribute.ml`, `attribute.mli` |
| IR, placement and resident execution | `lib/flow_ir/flow_ir.ml`, `flow_ir.mli`, `core_exec.ml`, `core_exec.mli`, `packed.ml` |
| Native GPU owner and counters | `lib/flow_gpu/host.ml`, `host.mli`, `run.ml`, `run.mli` |
| Geometry-to-scene conversion | `lib/rdk/rays/surface.ml`, `rays_mesh.ml` |
| Workspace display bridge | `lib/flow_native/workspace_gpu.ml`, `workspace_gpu.mli` |
| Device mesh/renderer bridge | `lib/rays/mesh.ml`, `mesh.mli`, `lib/renderer/renderer.ml`, `renderer.mli`, `scene_execution.ml` |
| Native integration precedents | `lib/rays_editor/native_qualification/test_grid_gpu_native.ml`, `test_chain_gpu_native.ml`, `test_scrub_gpu_native.ml`, that directory's `dune` |
| Normative architecture | `specification/core.md`, `sop.md`, `gpu-codegen.md`, `backend.md`, `flow.md` |

Choose focused aliases after reading the affected Dune stanzas. Add an isolated
alias for new native integration checks so agents can run them without a broad
suite. Keep native GPU checks in the appropriate native/qualification aliases.
Keep default warm focused runs below 30 seconds and individual sections below
10 seconds; record new tests above five seconds or slow-test fixes in
`specification/test-times.md`. Inspect Dune action timings before finishing.

## Scope and architecture guardrails

- RDK remains independent of OGPU, Metal, runtime, renderer and UI libraries.
  Existing opaque holder mechanisms are a useful precedent; do not import a GPU
  backend into the topology core.
- Flow IR remains renderer/geometry/UI independent. Connect capabilities through
  existing boundaries rather than importing the renderer or RDK into the compiler.
- OGPU remains the virtual GPU boundary. Product code outside the Metal backend
  must not call Metal directly. Read the applicable skills if a new capability is
  genuinely needed; implement both required backends and capability gating.
- Lisp remains the authoring contract. No OCaml-only geometry knob, alternate
  sketch language or custom IsoGrid registration.
- Do not reinterpret `(exact ...)` as a user residency selector. Preserve its
  language semantics and explicitly test any interaction with placement/readback.
- Preserve original colored/polygon behavior while investigating intermediate
  triangle-only cases. Deleting `Cd` or changing the sketch into a point cloud is
  not completion.
- Do not port the maze's serial random walk merely to claim every operation runs
  on the GPU. The request is residency-aware execution, not universal GPU execution.
- Shared compiler changes affecting existing native/web paths must preserve their
  parity checks and contracts. Native Scene3 mesh support does not imply adding
  web Scene/SOP export: web export currently covers 2D draw graphs.
- Update `specification/backend.md` and the dependency gate if a real boundary
  changes. Keep specifications describing implemented behavior, not aspirations.
- Preserve other agents' uncommitted work. Do not reset or replace it to obtain
  a clean baseline; identify the changes relevant to this task and integrate.

## Completion handoff expected from the implementing agent

Report the chosen representation and ownership contract, the real production
placement decision, the exact remaining CPU boundaries, and the observed transfer
counts for the original sketch. Include focused test commands/times, visual or
geometry parity evidence, and reproducible before/after benchmarks.

Also report the resolved compiler refusals and actual warm graph-switch latency
at 64-cubed resolution using the restored ordinary Lisp. The user explicitly
requested reverting sketch-level workarounds; preserve that constraint.

Mark the work complete only when the original mesh can follow the supported
resident raster route and CPU consumers remain correct. A shader that compiles,
two passing kernel parity checks, or a resident position buffer with host-only
topology is useful progress, but does not resolve this problem by itself.
