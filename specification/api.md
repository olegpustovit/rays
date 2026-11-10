# Rust crate map

Lisp is the public authoring surface. The Rust workspace implements the language,
editor and native backends. Signatures, choices, defaults, ranges and units are
available through `rays doc NAME`; [the author guide](../docs/lisp.md) is generated
from the same registry. [Flow](flow.md), [SOP](sop.md) and [backend](backend.md)
define behavioral contracts.

| Crate | Responsibility |
| --- | --- |
| `flow_syntax` | Lisp syntax, types, immutable values and packed primitive facts |
| `flow_check` | Checked workspaces, registry metadata and diagnostics |
| `flow_eval` | Evaluation, reactive values, state and graph instances |
| `flow_ir` | Typed frame/region programs, qualification and packed CPU execution |
| `flow_shader` | Native shader emission |
| `flow_web`, `web_export` | Separate 2D web lowering and export |
| `flow_sop_catalog` | Checked node definitions and parameter manifest |
| `flow_sop` | Geometry graph lowering, value lanes and native SOP bridge |
| `sop` | Immutable cook nodes, context and bounded session caches |
| `rdk_core`, `rdk_ops` | Packed topology, attributes, geometry kernels and validation |
| `rays_math` | Vectors, colours, exact arithmetic helpers and deterministic randomness |
| `rays`, `scene_core`, `scene_command` | Native loop, pure scene data and render commands |
| `renderer` | Prepared scene execution, retained resources and offscreen GPU targets |
| `runtime`, `runtime_input`, `runtime_resources` | Native lifecycle, ordered input and owned resources |
| `sdl3`, `sdl3_sys` | SDL family safe API and required ABI bridge |
| `ogpu_core`, `ogpu_metal`, `ogpu_vulkan` | GPU boundary and supported native backends |
| `metal_sys`, `vulkan_sys` | Checked-in native GPU ABI bindings |
| `pxui`, `pxui_shell`, `pxui_graph` | Shared UI engine, chrome and editable graph projection |
| `editor_core`, `editor_document` | Intents, authoritative workspace text and atomic reducers |
| `rays_editor`, `rays_bin` | Native editor integration and CLI |

## Native loop and coordinates

`rays::sketch::run_state` initializes the model, updates it from each frame,
constructs pure scene data, renders, calls `after_present`, and finally calls
`on_stop` while the runtime remains alive. Configuration selects logical size,
FPS, domain count and a realtime or fixed clock. A finite maximum frame count
bounds automation; the CLI also reads `RAYS_MAX_FRAMES`.

`rays::frame::Frame` carries immutable ordered input, time, tick/frame numbers,
logical size, backing scale, FPS and pointer delta. Scene, UI and pointer positions
use logical points. Pixel conversion belongs at the backend boundary. Pointer
delta accumulates motion during one frame; captured motion cannot be reconstructed
by subtracting absolute positions. Preserve text, IME, scroll, pinch, focus,
visibility, file-drag and asynchronous dialog events.

Native startup, validation, render and PNG errors return typed errors. A missing
GPU is a startup failure. Fatal callbacks produce a report in the temporary
`rays-crash` folder or `RAYS_CRASH_DIR`; the CLI exits 2. Runtime diagnostics are
printed by the rendering command and produce exit 1.

## Scene data and ownership

Scene constructors return data. Native lowering owns rendering effects, pipelines,
textures and retained buffers. Pure scene data contains no backend/runtime objects.
Cache identity ignores transient scene caches while retaining image/resource
identity. Device mesh carriers preserve borrowed position/normal/index/colour
lifetimes; a consumer requiring host data explicitly materializes it.

Images, canvases, textures, fonts and audio have explicit owners. Create, use and
destroy them on the initial thread, before SDL shutdown. Canvas creation, render,
capture, pixel access, save and destruction return errors. PNGs preserve native
framebuffer dimensions. Canvas density is fixed at first rendering; borrowed GPU
output expires when its canvas changes or dies. Renderer-local text caches are
bounded. Native targets use Metal on Apple Silicon or Vulkan on Windows.

## Geometry, editor and extension rules

SOP declarations own their parameter schemas and cooks. Never add a Rust parameter
that Lisp cannot express. Cook nodes are immutable. Context carries frame, seed,
parallel grain and cancellation; sessions own bounded cached payloads and must be
closed. Validate geometry topology and attributes at the trust boundary.

PXUI is the only editor UI engine. Construct UI during a frame, return intents,
and apply them after construction. Reuse its focus, capture, text input, hit list,
carry/drop and renderer. Workspace text remains authoritative: graph gestures,
inspector controls and Lisp express the same capabilities. A gesture is one
atomic document/history update.

Pure coarse work uses the process-wide worker pool over immutable or disjoint
data. Split deterministic random streams before parallel work. Sequential and
parallel results must match exactly; resource operations remain on the initial
thread. Run focused `cargo check -p CRATE`, then bounded `cargo t -p CRATE FILTER`.
See [development](../docs/development.md) for qualification and measurement.
