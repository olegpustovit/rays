# Web export design

`rays web FILE.rays -o DIR` writes a standalone static application.
Preview it with `npx serve DIR`; the emitted `serve.json` supplies the
cross-origin isolation headers required by the worker pool. There is no
built-in server, watch mode or editor export.

## Scope

The exported product is the 2D canvas: `:context draw` graphs and the
values that feed them. PXUI and the native editor stay native. Scene and
SOP graphs are outside this target's current scope.

Generated images support `image/map`, `image/noise` and `image/render`.
Static `image/load` assets are decoded natively and bundled; their paths
must be known at export time. Image map kernels use WebGPU, image noise
preserves native double arithmetic, and image render uses offscreen textures.

## Shared compiler

Both targets read checked workspace terms through the shared frame and region
IR. A frame program handles records, state slots, control flow, arrays,
draw statements and host effects. Pure numeric regions run in CPU blocks
or on a GPU. The interpreter supplies static evaluation and the reference
meaning used by compiler parity tests.

| Part | Native | Web |
| --- | --- | --- |
| Frame program | OCaml Core executor | Generated JavaScript |
| CPU region | Packed blocks over Parallel | JavaScript workers over SharedArrayBuffer |
| GPU region | Metal emitter through OGPU | WGSL emitter and WebGPU |
| Drawing | Native mark, geometry and quad pipelines | The same pipelines in WebGPU, printed from the same `marks.rays` |

The fixed runtime lives in `lib/web_export/runtime/`. The exporter lives in
`lib/web_export/web_export.ml`; JavaScript and WGSL emitters live in
`lib/flow_web/`. The architecture cleanup tracks their target homes
and compiler consolidation in [../cleanup/status.md](../cleanup/status.md).
See [../core.md](../core.md) for the normative shared compiler contract.

## Output and runtime

The exporter emits the program, kernel sources, binary constants, source map,
HTML entry and runtime assets. Runtime services own events, the frame loop,
state, worker scheduling, the WebGPU device and presentation. The bundle
does not contain an OCaml bytecode interpreter or a shader compiler.

Workers require cross-origin isolation. GPU refusal is a checked diagnostic,
not an implicit single-threaded or browser fallback for native programs.
File replacement uses the existing atomic writer; all files are generated
before the write phase begins.

## Placement and precision

Mark tables contain packed attribute buffers in painter order and a view
transform. Attribute fusion, sharing, hoisting, pruning and placement follow
the native paths. Regions bind arguments and frame inputs per call. Fold
memory belongs to the workspace execution.

The workspace's precision policy applies to JavaScript and WGSL. Fast
integer paths use exact 64-bit register pairs. Resident GPU folds count and
compact records, make one readback and draw display regions indirectly.
A target-specific repair carries a parity check on the other target.

## Verification

Use the shared check launcher and the focused `flow_web` and exporter
aliases while changing code. `@tools/qualification-web` exercises native/web
parity and real browser WebGPU integration. Draw snapshots and the reference
Core sweep guard meaning; generated primitive fixtures guard target printers.

Historical migration logs were removed after completion; they remain in
Git history at `62582188`. Historical measurements retained for comparison
live in [../performance.md](../performance.md). Current architecture-cleanup
verification is recorded separately and must not inherit old green runs.
