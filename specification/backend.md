# Native backend

Rays ships one backend: the native Metal runtime on Apple Silicon. The supported host is
macOS on Apple Silicon with Metal available. Backend initialization either
creates that native stack or returns a typed startup error; applications do
not select an alternate renderer through environment variables or public API.
Web export is a separate compiler target emitted from a Lisp workspace (`flow_web`, `specification/web-export/plan.md`), not a fallback: the native program never selects it.
Generated `image/map` pixels use the shared packed compiler and WGSL target, then a GPU RGBA8 conversion and the
existing textured-quad presenter on one WebGPU device; no native renderer or geometry dependency enters `flow_web`.
`image/render` uses that presenter with an owned RGBA8 offscreen target on the same device. The native CLI
supplies the existing SDL3_image decoder to `web_export`, which validates positive dimensions and exact
straight-alpha RGBA8 storage before bundling it. The decoder dependency belongs to the caller, outside
`web_export` and `flow_web`. `image/noise` uses the shared JavaScript
noise implementation at double precision, matching the native CPU generator before uploading its pixels.
Every image producer shares the workspace state slots by native source identity (instance, path and plane).
An absent system-default Metal device maps to OGPU `No_adapter`. A failure reading the
device's registry identifier remains `Device_lost`, with source operation
`Metal.Device.registry_id`; other native failures and typed guard errors keep their kinds.

## Ownership and dependency direction

The Rust `ogpu_core` crate holds the portable types, validation, the `backend` layer and the startup
selection (`ogpu_core::impl_`, thread-local state, no default implementation). The backend factory record is
`ogpu_core::backend::Implementation`; its `create_driver` returns a `Box<dyn Driver>`. A backend implements the
driver *traits* in `ogpu_core::backend` (`Driver`, `DriverDevice`, `DriverQueue`, `DriverCommands`,
`DriverRenderEncoder`, `DriverResource`, ... one trait per native object kind) instead of filling closure
records: a driver object is one struct, an encoder is one boxed object rather than a boxed closure per
operation, and the `backend` layer keeps its liveness/state guards around each `&self` call. Metal, Vulkan
and mock depend only on `ogpu_core`; `native_layer_token` lives in `native_layer_token_sys::portable`.
Thin crates were folded into their owners (C10): SDL3_image/ttf/mixer are `sdl3::{image,ttf,mixer}` over
`sdl3_sys::{image,ttf,mixer}` (one build script), `runtime_input` is `runtime::runtime_input`, `cfmt_sys` is
`cfmt::sys`, and the safe Vulkan wrapper is `ogpu_vulkan::vulkan`.

The Rust renderer does not expose a raw SDL window accessor. Window ownership and
resizing stay in `runtime`; renderer callers use its typed native-runtime bridge
and presentation facts. The unused renderer/runtime window-handle escape hatch
is removed, along with the renderer's direct SDL dependency.
Native renderer frame-sweep tests pump and drain the existing `runtime_input`
event source before each draw, as the application host does. Their throughput
fixtures explicitly request immediate presentation and keep every original
frame, pixel, cache and lifetime assertion; product presentation defaults stay
unchanged. The shared offscreen body uses a no-op pre-draw hook.

Scene3's existing borrowed-position registration can also borrow a UInt32
index buffer with a settled count. Those draws carry no host index bytes;
ordinary and sun-shadow indexed draws bind the same producer-owned buffer.
The renderer validates count and storage size, includes index identity in
retained replay signatures, and checks the producer's validity before replay.
Device indices and indirect arguments are mutually exclusive. The existing
bounded mesh cache retains a fixed four-byte backing allocation for an empty
host payload, with no mesh upload. The registration also accepts packed
float3/float4 colours at the existing vertex attribute binding (4); uniform
word 81 must match their width. The Lisp shader preserves host 8-bit Color
rounding, including half-byte decisions using the FMA multiplication residual,
before interpolation. Float3 is opaque, float4 carries alpha, and UV is zero.
Absent device colours use constant opaque white/zero UV (uniform word 81 = -1),
so no per-vertex default attribute array or upload is needed.
Colour buffer identity participates in retained replay validation. Surface
accepts resident P/N/Cd and offers resident polygon topology to the checked
triangle/quad index generator. Resident topology requires authored point N;
other attributes/layouts retain the host bridge and its prepared
`Surface.host_reason`. CPU-only deferred topology is not offered as a device
source. Point-cloud host conversion ignores empty corner attributes and does
not promote primitive colours when no primitives exist.

The existing device mesh now carries the borrowed triangle index count and
colour width as metadata. Face lowering emits no host vertex, index or colour
payload; a named empty attribute record requires a live colour registration.
Public mesh counts and triangle counts remain non-forcing. Wire/vertex modes
of a device-indexed mesh and textured uses currently materialise its memoised
host copy. Maze/Coral native qualification verifies exact raster, wire, vertex,
orbit, framing and World-shadow pixels against the host, with path-tracer
preparation explicitly materialising once. Picking builds the existing CPU
surface index, and inspection reads the existing sheet columns. Native frozen
workspace checks exercise each as the first CPU reader, verify its materialised
bytes and prove repeated reads add no transfers; summary/count metadata remains
nonforcing. Under the ordinary Measured policy a mesh whose site chose the device
displays with zero payload readback and size-independent renderer upload.
`Surface` keeps the device mesh it last bound (and a declined preview's host
conversion) per surface while the display names the same registration, and
`Workspace_gpu.display` returns the live registration of a geometry it already
shows. Releasing a registration sweeps its borrowed-vertex entries from the renderer
mesh cache, and `Scene_execution` drops retained draw layers whose borrowed stream is
gone (`borrows_unregistered`). Picking materialises one host copy per surface; that copy
is not charged to the editor's prepared-geometry cache.

The Flow GPU display-index kernel reads resident positions and UInt32
topology, following the host's position-dependent quad ear order. A two-pass
rejection reduction reads four validation bytes. `Host.triangle_indices`
returns its owned index buffer/count/release through the existing bounded
display storage pool, shared with derived normals; ambiguous/unsupported
polygons return a typed host-fallback reason. The workspace display borrow
returns the actual index count and colour width together with its key and
lifetime, so Surface never guesses from concurrently forceable deferred state.
`Workspace_gpu.display_refusal` keeps the last initial-domain fallback reason.

If host conversion also fails, `Surface.scene3_result` returns its RDK cause;
the convenience form raises `Surface.Conversion_error`. The editor retains
that diagnostic with the cached drawing until its prepared geometry changes.
It never replaces a failed conversion with an apparently valid empty mesh.
Scene-resolution errors are cached with their own result, separately from
combined drawing/image/texture errors, so corrected geometry clears the old
message without re-resolving an unchanged scene graph.

`rays_bin` owns the product executable. `web_export` owns static export
and its embedded `runtime/` assets, with a narrow lower/files/load interface.
The gate enforces its direct dependency boundary, rejects UI dependencies and checks the binary's terminal
backend selection. The emitters in `flow_web` keep their existing lower-layer
boundary. Document/catalog integration remains in `web_export`, including its existing transitive
native dependencies. Image decoding is supplied by the caller through the `files` interface;
`web_export` uses SDL directly only in its decoder integration tests.

The shared file-table embedder belongs to `flow`; both the node catalog and
web runtime embedding invoke it. The manifest generator belongs to
`sop_catalog`, and the one SDL binding generator belongs to `sdl3`, reused by
the image, TTF and mixer bindings. Generator executables do not add product
library edges. The test-only `lisp_sop` helper depends on Flow/SOP/RDK/math and
has no document, renderer or UI reach; the gate pins that boundary.

`Runtime_resources` owns recursive directory creation and atomic file
replacement, including text writes and callback-written files such as PNG
exports. Crash reports, presets, source saves and web exports use the same
replacement path through `Runtime_resources.write_text` or `write_atomic`.
Its owned-resource APIs return `Ogpu.Error.t` directly. The shared kind set
includes `Wrong_domain`, `Destroyed`, `Decode` and `Io` alongside the GPU
validation kinds; SDL image, TTF and mixer binding errors remain their own
types and are translated at the resource boundary.

`flow` depends on the dependency-free `diagnostic`, `param`, `input` and `rays_math` libraries. Its value graphs, expressions,
contexts and coercions have no geometry, editor, UI, renderer or GPU
dependencies; the dependency gate rejects those transitive edges.
`flow_ir` consumes specialized Flow plans and residual captures. It depends
only on `flow`, `param`, `rays_math`, `parallel`, `lru` and the dependency-free `scene_command` (the marks
contract as pure data, P3.3b); its dataflow passes and packed/scalar
executor cannot reach geometry, presentation, editor or GPU libraries. Its
`Kernel_layout` plans register reachability, uniform/frame slots, noise tables,
gathers and binding offsets once for both Metal and WGSL; each backend owns only
its instruction printer. CPU JavaScript regions call the imported `noise3` from
`prims.js`, matching the shared web runtime primitive.
`flow` never depends on `flow_ir` and retains an independently callable
reference tree walker. The printers (`flow_shader`: `Emit`, `Spirv`, `Raster`) sit above the numeric IR and depend
directly only on `flow`, `flow_ir` and `ogpu_core` (`Shader.binding`, pure data); their
the Rust `print_shader` tool prints the renderer's, the Metal backend's and `flow_gpu`'s shaders at build
time, a build action that adds no library edge (the gate reads `libraries` only).
`flow_gpu` sits above them and the public OGPU API:
it runs Metal source printed from borrowed immutable register programs, caches at
most 64 library/pipeline pairs per device owner, and owns geometrically grown
upload/output buffers per program. It depends directly only on `flow`,
`flow_ir`, `flow_shader`, `rays_math`, `ogpu`, `renderer`, `lru` and `parallel`; it cannot reach
procedural geometry or editor/UI libraries. The dependency gate rejects an
upward edge from `flow_ir` to `flow_gpu`. Native leases, pipeline compilation,
uploads and dispatch remain initial-domain host work. Closing programs drops
their buffers before closing the pipeline owner and releasing the lease.

`Flow_ir.Packed.gpu_refusals` owns the pure GPU form restrictions shared by
placement, display execution and Metal emission. It requires collecting
effective-Zip programs without skips or ordered accumulators, and checks
output-reachable noise octaves and constants against `Flow.Packed_ops` limits.
One-source Product programs already have effective-Zip semantics. Reachability
is a fresh mask; qualification never mutates the borrowed program. This shared
check creates no dependency from the numeric IR to the GPU implementation.

`Flow_ir.qualify_workspace` joins immutable checker candidates to actual static
captures through the evaluator's temporary observation callback. It uses the
production packed compiler and that same pure form check, without importing
the emitter or creating a device. Missing/ambiguous specializations remain
pending; actual refusals dominate successes. Lowering owns this qualification
and exposes only derived path/reason metadata to the document and inspector.
The validation-only workspace tool additionally compares fused/unfused
programs with pure emission using each caller's actual catalog. No execution
proof cache or per-frame qualification is introduced.

`flow_sop` depends directly on `flow`, `flow_ir`, `lru`, `param`, `sop`, `diagnostic`, `rays_math`, `parallel` and `unix`; its typed overlay and value lane cannot reach presentation, the catalog, editor or GPU libraries.
Math-backed value declarations such as `noise3` live in `flow.Op` with the
other built-in Flow operators. Plain Flow checking remains independent of SOP
geometry and presentation libraries.
Cook-time `sop/attr` and `sop/with_attr` declarations live in `flow_sop`.
Its attribute kernel resolves immutable packed Flow sources against cooked
RDK inputs and writes through `Rdk.Kernel.edit_point_ranges` or point
attributes. The executor receives a resolver callback; neither `flow` nor
`flow_ir` imports geometry. Environment fold snapshots are passed through
the existing frame-node rebuild boundary.
Instantiated SOP topology/elementwise facts supply neutral count-origin proofs
to `flow_ir` for dynamic attribute-map fusion. No geometry types cross that
callback. The workspace owns a bounded atomic execution profile; hosts supply
the clock, and `flow_graph.Probe` receives only neutral tier/group/time metadata.
Neither `flow_ir` nor `flow_graph` gains a platform-clock dependency.

`flow_graph` depends only on `flow` and `param`. It owns `Projection`,
`Flow_edit`, `Exposure` and `Probe` for every domain. `pxui_graph` consumes
that neutral layer and menu entries supplied by the host; it never reaches
`sop` or `rdk`, even transitively. The gate checks both boundaries.

`flow_native` owns native Flow drawing, marks, live-frame conversion and
workspace execution. It consumes the UI-free `editor_document` model and
the shared renderer; the gate rejects PXUI or editor dependencies. Geometry
conversion lives in `rdk_rays.Surface` and `Packed_pieces`, using geometry,
instance transforms and typed parameter views rather than SOP nodes or
session outputs. The SOP host extracts those values at its existing boundary.

`Value.Deferred (ty, id)` identifies a typed node in the evaluator plan.
Geometry nodes lower through `flow_sop`; Drawing nodes lower through
`Rays_editor.Drawing` into existing native `Rays.Scene` commands. Canvas
panels use the same PXUI composition and renderer as every other panel.
`Rdk.Topology.deferred` is the GPU-free topology carrier for an external owner:
counts, identity and payload accounting never read its arrays. An optional
homogeneous kind/corner-count promise answers triangle classification without
forcing. Payload queries and `Topology.View.view` materialise once across racing
domains; the returned validated topology must match the promised counts and shape.
`Geometry.releasers` includes topology with the same held/unheld ownership rule
as positions and attributes. Owned topology keeps its direct array fields.
There is one evaluator, one UI engine and one native Metal renderer.
`rays_editor` may reach `flow_ir` to prepare drawing argument programs;
the editor retains that preparation while the plan is unchanged, and exports
prepare once before playback. Numeric maps inside frame folds dispatch through
the packed tier with current immutable bindings. `flow` owns fold identity and
transactions and receives only a private execution callback; it never imports
`flow_ir`. Reference drawing evaluation remains available for parity checks.

`input` owns immutable logical frame facts (time, step, index, size,
pointer, held keys/buttons and ordered events), with no dependencies. The host
captures these once from `Rays.Frame` through `Flow_native.Live_frame`.
`Sop.Context` carries the same snapshot for geometry-zone bodies;
its `Input` dependency projects every field into the cook cache key. No SDL,
runtime or renderer value crosses either pure boundary.

Material graph values lower into primitive surface attributes through
`sop` and `rdk_ops`. `rays_editor` converts these into shared
render mesh batches; neither the material context nor the assignment kernel
imports rendering code. `rdk_rays` expands primitive colors through the
existing attribute promotion kernel, preserving face boundaries.

```text
examples / sketches / pxui / editor / rdk_rays
                         |                  |                |
                         |                  v                v
                         |              sop         rays
                         |                  |
                         |                  v
                         |                 rdk
                         |                  |
                         |                  v
                         |               rdk_boolean
                         |                  |
                         |                  v
                         |               rdk_ops
                         |                  |
                         |                  v
                         |               rdk_geom
                         |                  |
                         |                  v
                         |               rdk_core
                         |                  |
                         |                  v
                         |             rays_math
                         |
                         v
                      rays ----------------------> input
                         |
                         v
                      renderer ---------------> runtime_resources
                         |                             |
                         v                             v
                      runtime ------> sdl3::{image, ttf, mixer}
                         |
                         v
                    ogpu_core ---> native_layer_token_sys

                    ogpu_metal  ---> ogpu_core, metal
                    ogpu_mock   ---> ogpu_core
                    ogpu_vulkan ---> ogpu_core, vulkan_sys, native_layer_token_sys
```

The RDK split currently puts packed identity, storage, topology, groups, and
geometry in `rdk_core`; exact predicates, planar constraints, Delaunay, Voronoi, spatial and
surface indices, proximity queries and point clustering live in `rdk_geom`. Attribute and group
operations, point generators, curve sampling and topology, and modeling operations live in
`rdk_ops`; Boolean stages are in `rdk_boolean`.
`Rdk` keeps the public module paths stable, and
the dependency gate rejects upward edges from lower to higher RDK libraries.

`rays` never depends directly on an SDL library: window, event, clipboard,
cursor, image, font and audio services sit behind `runtime`,
`runtime_input` and `runtime_resources`, and the dependency gate rejects a
direct `rays` → `sdl3*` edge. `Runtime.t` owns the window/view lifecycle,
logical/drawable frame facts and cached presentation facts, requeried when SDL
announces a drawable change. It has no render pipelines, GPU target, renderer
statistics or readback ownership. `Renderer` creates and owns the OGPU driver,
the prepared execution core, pipelines and render statistics. It supplies the
same draw, replay and readback paths for a window or offscreen shell, and
synchronizes GPU targets with Runtime's presentation facts on the next step
or replay after a resize. Hosts call Runtime's window operations through the
borrowed shell. Renderer destroys GPU resources before that shell. Window-only calls return
`Unsupported` offscreen. The former `scene_execution` and `rays_execution`
libraries are merged into `renderer`; `Scene_execution` is a module in it.
`Ogpu.Impl.backend` identifies the linked implementation for backend-tagged
shader descriptors; it is not a runtime backend selector. An implementation
without the required shader set returns typed `Unsupported`.
`rdk_rays` is the separate renderer conversion leaf.
`rays_editor` also composes `rays_pathtracer` for its shared 3D Renderer
control. It owns bounded per-viewport tracing resources and inserts their images
through the existing Scene presentation path. Raster and wireframe use Scene3;
all three modes run on the native Metal backend. This adds no dependency from
the renderer or foundational libraries back into the editor.
`sop` depends on `rdk_core`, `rdk_geom`, `rdk_ops`, `rdk_boolean` (the Rust port has no `rdk`
facade crate), `rays_math` (vectors, matrices, colors), `parallel` and `lru`; it never reaches `rays` or the GPU
runtime, and the dependency gate keeps it so. The Rays-dependent geometry glue
(`Surface`, `Packed_pieces`) lives in `rdk_rays` and accepts geometry/parameter
values, without a dependency on SOP or the editor.
`sop_catalog` depends only on `rays_math`, `rdk` and `sop`: its node
schemas name `Rays_math.Vec3`/`Mat4` directly, and the gate rejects any path
from it to `rays`, the runtime or a GPU library.

`sop` is preprocessed by `rays.ppx` (which links `ppxlib` and `flow`
at build time only, never into the library): the node declarations that
yield both the editor factory and the typed `Sop` constructor live there, in
private modules (`crates/sop/src/sop_topology.rs` and its siblings), and `Sop.Nodes` exports their factories to
`sop_catalog`. `Edit_graph` no longer calls `Sop`, so a declaration may use
`Edit_graph.factory` without a cycle.

`runtime` owns process setup, initial-domain lifecycle, the SDL3 window, its
Metal view, resize scheduling, and presentation. It depends on `sdl3` and the
virtual `ogpu` only: it obtains the driver through `Ogpu.Impl.create_driver`,
and the surface configuration carries the window's `Native_layer_token`, which
the Metal adapter adopts as its `CAMetalLayer` and releases with the surface.
`Ogpu.Backend.gpu_timing` reports the queue's accumulated GPU time, so the
runtime reads no Metal counter. `ogpu_metal_native` owns the translation from
the checked high-level GPU interface to typed Metal bindings; native executables
select `ogpu_metal` explicitly. Pure and mock test executables select
`ogpu_mock`. The virtual library has no default implementation, so configuring
an explicit mock target does not require Metal. Metal libraries, native tests
and the native executable are enabled only on macOS. `metal` owns the safe
Metal resource and command API. Rays owns pure scene
values and records rendering through the narrow GPU boundary; it never exposes
native handles in its public API.

Rust native Rays/editor/PXUI tests reuse the renderer test helper: Metal on macOS,
Vulkan with fatal validation on Windows. Their backend dev-dependencies are platform
specific. A supported platform's device-creation failure fails the test; it must not
return success through a GPU-unavailable early return. The editor runner keeps its
existing case sharding while sharing backend selection and lease cleanup.
Native lifetime tests release the helper's keepalive before their baseline: the
last canvas must own device teardown, including cached pipelines, for an exact
before/after native handle count. Other tests may retain the shared device.
Rust's native Metal handle destruction and reflection-binding cleanup open their
own autorelease pools: ARC's `CFBridgingRelease` can autorelease even when its
result is discarded. A subprocess regression checks both paths with
`OBJC_DEBUG_MISSING_POOLS=YES`; Rust handle counts alone cannot detect these leaks.
The Rust dependency gate checks backend selection separately for Apple and
Windows: mutually exclusive target dependencies do not count as two selections.
An unconditional second backend or a missing platform backend remains an error.
Raw Metal ABI declarations are checked in under `crates/metal_sys/src/`;
typed SDK calls are generated, while custom marshalling/ownership primitives
are explicitly registered as `Native` entries. This does not change the OGPU
boundary or expose the private raw ABI (see `specification/metal.md`).
The dependency-free `param` library owns typed parameter schemas under the
single `Param` name, including canonical float text and vector grouping, so
`Pxui_shell.Inspector` renders SOP nodes and plain sketch records alike
without the geometry stack. The `editor_core` library owns bounded,
labelled undo history with explicit edit merge rules, named commands, key
routing, and atomic file storage (user preferences and documents are
s-expressions printed by `Flow.Lisp`). Sketch hosts use
`Editor_core.History`, `Editor_core.Router`, and `Editor_core.Store`;
the router filters fly-mode keyboard events before leader and chord routing,
while passing `/` through to arm the leader after fly exits (`/` and Space both exit; only `/` is passed on, so Space does not also play).
`diagnostic` is the dependency-free shared error value and formatter; `flow` supplies the expression/value model and workspace language
(reader, checker, evaluator) over `param`. `ppx_rays` depends on `flow` for
declaration checks only.
`flow_sop` depends directly on `flow`, `flow_ir`, `lru`, `param`, `sop`, `diagnostic`, `rays_math`, `parallel` and `unix` for the SOP overlay, value lane, lowering, edits, projection and probes. The editor runs
the value lane on its initial domain before cook submissions and retains
applied values for the graph and inspector. The gate forbids
`flow` from reaching anything but `diagnostic`, `param`, `input` and `rays_math`, and `flow_sop` from reaching UI,
the SOP catalog, editor or GPU libraries.
`editor_document` owns the UI-free saved overlay, while `pxui_graph` and
`pxui_shell` are presentation adapters over it. The PXUI engine depends directly
on `rays`, `rays_math` and `param` for UI values, shared hashing and the number field's model; `pxui_shell`
depends on `rays`, `rays_math`, `editor_core` and `pxui`. `editor_core`,
`editor_document`, `pxui_graph` and `rays_editor` name math values from
`rays_math` directly. `rays_editor` applies their typed requests and schedules
cooks; none of these edges points back into
`flow` or `flow_sop`.
`pxui_graph` exports its graph commands as `Editor_core.Command.t` entries without handling key
events. `editor_core` depends on `rays` for frame and event values and on
`rays_math` for math values, never on UI or geometry libraries. `pxui_shell` owns editor chrome over the shared PXUI
handle; `Layout` computes pane geometry and `Chrome` handles standard splitters,
headers, and focus outline. Layout geometry is pure and has no mutable cache
inside the PXUI frame. Its which-key panel reads generic editor commands, while its
timeline and prompt widgets return requests without knowing about SOPs or
presets. `Shell.frame` owns the workspace's PXUI frame calls. `rays_editor`
supplies commands, playback state, and preset data. The one private
`editor_document` library contains Document, Settings, Objects, Layers and
Preset. Its package-private status and dependency gate enforce a transitive
ban on PXUI, shell, graph presentation and rays_editor.
Loaded documents validate before installation and current levels resolve
after load, undo, and removal. The host reduces stable-ID pane edits after UI
construction and records every edit path through its commit helper.
PXUI hit ancestry reports
presses on child controls to their pane roots; the sketch host reads those
signals for pane focus. When a click and scoped key share a frame, the router
reads the same PXUI hit tree before building the frame. `rays_editor`'s shared
`Environment.scene` path composes both 2D and 3D views: viewport adapters
supply camera and world painting, while visible/hidden composition, the
leader overlay, and the unchanged hidden-scene cache follow one path.
`Viewport` owns camera-node seeding, active-camera repair, look-through
navigation, and follow-viewport document writes. `Viewport2` owns 2D panel,
navigation, scene, and persistence operations. `Environment` shares scene
composition, render-request completion, and PNG status handling. Presets use
`Editor_core.Store` graph and viewport sections; `Editor_core.Store.Settings` saves the
same envelope and reads legacy `PXUI1` settings files.
OGPU's dormant Frame_graph, Descriptor_arena, Transfer_ring, Instance,
Device_lifecycle, and Acceleration_pass modules have no production callers and
are removed. Query validation stays in `Ogpu.Sync.resolve`; the redundant
Query_pass and scoped Native_pass metadata wrappers are removed. The Metal
queue owns its bounded submission epochs: every command buffer is recorded
through `Backend.begin_commands` and admitted by `commit` or `commit_present`.
Plan G4 removed the description-based paths that preceded the encoders: the
portable `Command`, `Compute_pass`, `Transfer_pass`, `Mock`, and `Cache`
modules, `Backend.submit`/`submit_sync`/`present`/`submit_present`, pipeline
adoption, the per-queue submission cache, the Metal adapter's classic and
retained render-plan caches, its Command4 scoped path, and the description
encoders. `Ogpu.Render_pass` now holds only the render-state enumerations the
encoders share, and `Types.origin`/`extent` describe blit regions.
The native queue can poll completion through an epoch without blocking using
Metal command-buffer status; a terminal poll uses the same ordered cleanup
and epoch accounting as a blocking wait.
The portable `Ogpu.Backend.status` polls a queue receipt and returns
`Pending` or `Completed`; terminal GPU errors remain typed errors. Its
`completed_epoch` query is scoped to that queue. The Metal driver passes the
native poll through the presentation cleanup path;
the mock uses independent clocks per queue and executes blit copies/fills
against its owned byte storage. It now executes texture upload, copy, and
readback through mip-aware RGBA8 storage as well. Shared conformance compares
exact bytes after padded-row and subregion uploads on mock and Metal. The
Metal adapter gives copy-only textures an explicit native usage bit because
Metal expands an empty usage mask during creation.

OGPU carries what the runtime and the path tracer call, and nothing a test
alone used (audit Q2). `Backend.create_library` compiles one MSL artifact into
a reusable library; `create_compute_pipeline_from` makes a pipeline per entry
point with typed function constants and an exact per-entry binding interface
checked against Metal reflection (buffer, texture, and sampler bindings occupy
separate index spaces, and `Acceleration_structure` is a binding kind). `max_total_threads` reads a compute pipeline's
`maxTotalThreadsPerThreadgroup`, which Metal lowers for a kernel that needs more
registers (the Flow kernels' register-pressure gate, `Flow_gpu.Run.max_total_threads`;
0 for a render pipeline). `begin_commands` records one command buffer through
compute, acceleration, blit, and render encoders (`set_pipeline`, `set_buffer`,
`set_bytes`, `set_texture`, `set_accel`, `dispatch_threads`, `dispatch_threadgroups_indirect`,
`build_accel`, `write_compacted_size`, `compact_accel`, `buffer_to_texture`)
and commits without blocking, so `Backend.status` and `gpu_duration`
observe it through the queue's epochs. Buffers take a `Types.memory` class;
device-local buffers reject host access with `Unsupported`. The Metal adapter
retains every referenced resource until the native command buffer completes;
the mock executes uploads exactly and answers compute, render, and ray tracing
with typed `Unsupported`.

**Mapped buffers and the portability contract.** `Backend.create_mapped_buffer` makes a
host-visible buffer with a lazily cached host view, a `Vec<f32>` of `size / 4` elements the caller
fills and reads in place. Buffers are modelled by intent, never by a storage mode one platform has:
device-local (`Memory::DeviceLocal`, no host access) or host-visible (`Memory::Shared`, and mapped
buffers among them), with a view only on host-visible ones. The view is the *host copy* and the
buffer the *device copy*; `ogpu_core` owns the host copy on every backend. They meet at two explicit
calls: `flush_mapped` after host writes and before encoding work that reads them, and
`invalidate_mapped` once the work writing the buffer has completed (the caller waits for its epoch
first) and before the host reads the view. Neither operation assumes shared memory is coherent.
The first `mapped_view` reads the buffer; repeated calls return the same cached view. `invalidate_mapped`
also creates a view if needed, then fills the requested range from the buffer. A buffer only the device
touches never needs a host copy. `release_mapped_view` drops the cached copy and fills any escaped
view with NaN; a successor also starts NaN until invalidated. A flush with no view since creation or
release is `InvalidArgument`, since the host wrote nothing. Metal, Vulkan and the mock share these
rules; native mapped allocations remain owned by their backend for lazy readback. `Flow_gpu.Run`
asks at each host use (packing an input, reading an output or a holder) and releases when a buffer
comes back to the shared pool; input and output slots keep theirs, since the host rewrites or reads
them at every dispatch. Over PCIe a readback costs its transfer, so readback stays explicit and lazy: a value
is materialised on the CPU only when it is read (`Flow_gpu.Run.readback`/`planes` after the
dispatch's completion and status check). `Run.keep` hands an output's buffer, still in flight, to
a lazy holder (a SOP write's deferred attribute, specification/sop.md, Device placement) without
waiting: the holder's `settle` (initial domain, once per dispatch) waits for the epoch and reads the
status, which is explicit synchronisation a discrete backend needs anyway; its `materialise` is the
only read (an `invalidate_mapped` and a conversion, callable from any domain once settled because it
does no queue work; on the initial domain it waits itself); its `release ~held` says its owner (a
`Sop.Session` dropping the result) no longer needs it: handed out (`held`), it is retired and stays
readable until read or collected; otherwise its buffer comes back at once and a later read raises.
A holder tells the GC its buffer's size (a custom block, `caml_alloc_custom`, in `flow_gpu`'s stub),
so device memory a dead holder pins drives collection as a Bigarray's data does; nothing assumes the
GC sees device memory otherwise. The runner takes a recycled or fresh buffer for its next dispatch; a
freed, read or collected holder's buffer is recycled into the shared pool (`Run.pool`, any 8 or
within 256 MiB) or destroyed on the initial domain at the next dispatch, and `Run.close` materialises
live holders before destroying their buffers.
A dispatch that resets the status word first reads an unsettled predecessor's status into a short
verdict list, so a late `settle` still sees its own failure. `materialise` cannot fail: `settle`
covered completion and every output's finiteness, the range is the buffer's own and the buffer is
destroyed only under the holder's lock. A held buffer is also
a later kernel's input without a readback: `keep` returns a one-element placeholder array, and
`Run.dispatch` (any runner) binds an input that is physically that array to the held buffer while
its holder has not read it back (the buffer is then released). On a discrete backend
`invalidate_mapped` becomes the copy, so `materialise` turns into queue work on the initial domain
(or a staging copy at the settle point).

A display draws a held buffer where it is. `Run.pin` (initial domain) borrows a settled holder's
buffer by its placeholder: the pool counts the display as a reader, so it neither recycles nor
reads back (`reclaim`'s thaw) the buffer until the display unpins, also after the holder is read or
collected; settling a dispatch settles every output of it. `Renderer.register_gpu_positions`
registers the borrowed packed float3 positions (and normals) under a `Gpu_vertices` key with a
validity closure (a close destroys the buffer: a draw of it is then refused as stale), and the
Scene3 vertex stage reads a device mesh at a stride of three floats from buffer 0 (normals from
buffer 1, or (0, 0, 1)) when the draw's uniform word 79 (and 80) says so, the 24-byte record
otherwise; indices and the 12-byte colour/uv record stay host bytes (for a plain point cloud the
same bytes every frame; a triangle mesh's indices are packed once per topology and cached by the
renderer under a key of their own, the `Gpu_vertices "borrow\nindices"` name). The World sun's
depth pass binds the borrowed positions and reads them at word 79's stride too, and a World-lit
draw (`Scene3_world`) binds them as the plain one does. Ordering is explicit: the display binds only a settled output (its
dispatch completed, on the producer's queue). A display is dropped when the editor's drawn scenes
no longer name its key (`Workspace_gpu.frame ~keep`, once a frame, when they change) or its mesh is
collected, and its buffers are unregistered and unpinned a frame later (the renderer settles its one
presented frame in flight before it draws again), not when the presenting queue completes. ponytail: that needs the renderer's
completion epoch exposed to the producer, which a discrete backend will want anyway. The buffers
carry `Storage` usage only: the vertex stage pulls them as storage, so no vertex-input layout is
assumed.

Ray tracing: geometry variants are `Triangles`, `Motion_triangles` (one vertex
buffer per keyframe), `Bounding_boxes`, and `Curves`; descriptors are `Blas`,
`Motion_blas`, `Tlas_of` with an `instance_kind` (user-id or motion
records packed by `pack_instance_records`/`pack_motion_instances` through the
driver's `instance_layout`, plus a keyframe transform buffer packed by
`pack_transforms`), and `Sized` structures that `compact_accel` fills after
`write_compacted_size` reports the size. Procedural geometry is a shader matter:
`Bounding_boxes` primitives are resolved in the kernel's own traversal loop
(Metal `intersection_query`, SPIR-V `OpRayQuery*`), so OGPU has no intersection
function tables or linked functions. `compute_use_accels` declares
the built bottom-level structures an instance structure references as read by
a dispatch (Metal `useResource:usage:`; they are otherwise evicted over time).
`Caps` is the live set: `Compute_pipeline`, `Render_pipeline`, `Ray_tracing`,
`Ray_tracing_motion`, `Ray_tracing_refit`, `Ray_tracing_curves` (Metal: Apple9 and later; the M1
answers curves with typed `Unsupported`) and `Memoryless_attachments`.
Memoryless (`create_memoryless_texture`): a render attachment with no memory, whose contents live only
inside one pass, so it clears and stores nothing but a resolve (Metal `MTLStorageModeMemoryless` on the
Apple families; Vulkan would be `TRANSIENT_ATTACHMENT` over lazily allocated memory). `Scene_attachment_pool`
asks for one when no later pass of the frame loads the attachment and falls back to an ordinary attachment
on a backend without the feature, with the same pass.
Refit (`Ray_tracing_refit`, the path tracer's device meshes): `create_accel ~refit:true` builds a
primitive structure that can be updated (Metal `MTLAccelerationStructureUsageRefit`, Vulkan
`ALLOW_UPDATE`), `accel_sizes.refit_scratch_size` is the update's scratch (Metal's refit scratch,
Vulkan's `updateScratchSize`), and `refit_accel encoder accel descriptor ~scratch` updates it in place
from a `Blas`/`Motion_blas` of the same geometries in kind, count and stride over new buffers or
contents (Metal `refitAccelerationStructure:` with a nil destination, Vulkan an update-mode build with
src = dst). The portable state machine refuses a structure not created refittable, unbuilt or
compacted (`Invalid_state`) and another shape (`Invalid_argument`); the mock has no ray tracing and
answers `Unsupported`. No unified-memory assumption: the new vertex data may be device-local and
device-written, ordered by the queue.
A colour attachment's `store` is honoured: `Store` keeps the contents (and
also resolves when a resolve texture is given), `Resolve` writes only the
resolve texture, `Discard` keeps nothing and is rejected with a resolve
texture; conformance resolves a stored multisample pass again from a `Load`
pass. `render_descriptor.depth_format` is portable description only: Metal 4
pipelines are not specialized on the depth format, so the Metal driver does
not read it.

Removed with their Metal bindings, mock arms, conformance cases, and
capability flags, because no product code called them: placement heaps and
aliasing, residency sets, intra-queue fences, timeline events, stage-boundary
timestamps, mesh and tile pipelines, dynamic libraries, binary archives,
sparse textures, the MetalFX upscaler, visible and intersection function
tables with linked functions and the instance table offset, structure copy, precompiled metallib shaders, the encoded buffer copy, fill,
texture copy and texture-to-buffer blits, and the portable `Memory`, `Sync`,
and `Diagnostics` modules. A capability returns through the
`add-ogpu-feature` workflow with its first caller. Shared conformance covers
what remains on both drivers: capabilities, buffer round trips, padded and
inset texture uploads read back by the host, the three pixel formats, linear
mip sampling, library lifetime and function-constant specializations, exact
compute output, abandoned commands, ray-query hits (primitive id, instance id,
`t`), bounding boxes resolved in the kernel's traversal loop (no product
caller since the path tracer's spheres moved to scene/trace), curves or their rejection,
motion primitives and motion instances at three shutter times, user-id masks,
compaction hit parity, the render path, lifetime rejection, and zero leaked
handles; the mock rejects what it cannot execute with typed `Unsupported` and
reproduces the record layouts.
The path tracer's kernels are Rays Lisp (`lib/rays_pathtracer/pathtrace.rays`), printed by `Flow_shader.Emit`
and dispatched through `Flow_gpu.Run` into the tracer's own buffers; `rays_pathtracer` depends on `flow`,
`flow_ir`, `flow_gpu` and `input` (the frame facts a kernel is prepared with) for that. `Run.dispatch
?resident` declares an instance structure's BLASes read, as the path tracer always did. The M1
fixed-image qualification covers flat and instanced renders across the move from pathtrace.metal.
The path-tracer camera input is now `Rays.Camera.t`; the current ray kernel
accepts only an unshifted perspective view. The M1 fixed-image qualification
also covers this API migration.
Path-tracer materials and environment light use
`Rays_pathtracer.Linear_color.t`, a floating-point RGB record. It preserves
low-intensity and HDR inputs that byte-channel `Rays_math.Color.t` cannot express;
the GPU upload keeps the same float channel order and the fixed M1 image digest.
`Ogpu.Caps` now owns the portable feature matrix and typed `Unsupported`
check. Metal probes populate that profile in `ogpu_metal_native.Device`, which also
translates native Metal errors to typed OGPU errors.
The former `Ogpu.Capabilities` record is folded into `Ogpu.Caps`: limits,
feature availability, and conservative-probe notes travel as one value.
`Ogpu_metal_native.Device` stores that profile once, and the deterministic OGPU mock
uses `Caps.require` for typed unsupported-feature results.
The old Metal `Adapter` module is removed; it no longer duplicates feature
decisions or wraps capability probes.
The shared native conformance checks cover capabilities, buffer/texture round
trips, padded rows, mip levels, inset origins, shader libraries, compute,
ray tracing, rendering, rejection, retention and teardown. Select one supported
backend at the process boundary. Mock checks remain isolated tests.

`cargo run -p gate -- deps` reads Cargo manifests and enforces dependency
direction. Foundational language/IR/shader/web crates remain UI-free; geometry
foundations cannot depend on the GPU. Test-only fixture support cannot become a
product dependency. Native binding code lives in its owning sys crate, with no
foundational crate built through an application or tool dependency. Existing
exceptions must be explicit, named and justified in the gate itself.

The IsoGrid native qualification benchmark may use the standard library's
`Dynlink.unsafe_get_global_value` solely to inspect the already-linked shared
scheduler's empty task-slot cells when measuring retained heap. Its source-token
allowance admits that read-only function at that one tool path; module loading
there, and symbol inspection in product code, remain rejected by injected checks.
This adds no renderer or backend selection path.

Texture formats use one backend-neutral `Types.texture_format` enum. It has
seven Metal formats: `R8_unorm`, `Rgba8_unorm`, `Bgra8_unorm`,
`Bgra8_unorm_srgb`, `Rgba16_float`, `Rgba32_float`, and `Depth32_float`.
`Types.texel_bytes` gives 1, 4, 8, or 16 bytes per texel. OGPU host texture
descriptors accept the three RGBA color formats; R8, BGRA/sRGB and depth are
backend resources for internal uploads, drawable presentation, and depth
attachments. Host bytes are little-endian; half floats are IEEE binary16
(`Types.half_of_float`/`float_of_half`, round to nearest even). Upload and
readback row pitches must cover `width * texel_bytes`; texture copies require
equal formats. The three RGBA formats are sampled with linear filtering and
mip LOD and are storage-capable on Apple7+ (the M1 reports
`supports32BitFloatFiltering`), so no capability gate exists. Float formats
reject `Render_attachment` and multisampling with a typed `Unsupported`:
pipelines target `Rgba8_unorm` only and HDR is tone-mapped in the shader.
Presentation and MetalFX sources must be `Rgba8_unorm`.

`Scene_execution` sampled textures take their format from level 0's byte
count (4, 8 or 16 bytes per texel); `world:` keys are identity keys like
`image:`. The `Scene3_world` family (World plan P7) binds the material
texture or camera map at 1/2, the World block at buffer 3 and the prefiltered
specular mips at 4/5; auxiliary blocks hit on physical identity before a byte
compare and count against a 256 MB byte capacity.

Qualification code reads the runtime and Metal counters at their owning
boundaries. Sketch does not retain a process-global diagnostics snapshot after
teardown; its coordinator is destroyed during `on_stop` cleanup.
`Sketch` owns frame time and ordered input events. The execution coordinator
owns GPU submissions and presentation facts; its step result carries no second
event queue, clock, or input snapshot.
SDL3 file-drop events enter `Runtime_input` as validated full paths only.
The pump never reads file bytes; the sketch receives the same path through
`Input.File_dropped` in `Frame.input.events` and decides when to perform I/O. The input queue retains
its event-count bound, with no separate file-size limit or byte payload.

Scene visibility, culling, batch selection, and Scene2/Scene3 lowering remain
Rays responsibilities. The Metal binding does not contain Rays vertex
layouts, fixed Scene binding slots, or scene-cache keys. Its private prepared
submission surface snapshots only generic Metal pass state, typed resource
sets, indexed draws or indirect-command ranges, and completion-owned resource
roots. `Backend.draw_indexed_indirect` (render pipelines; Metal
`drawIndexedPrimitives:indexType:indexBuffer:indexBufferOffset:indirectBuffer:indirectBufferOffset:`)
reads an indexed draw's index and instance counts from five uints the device wrote; the mock has
no render encoder and refuses it with the rest of rendering; conformance draws a triangle from
device arguments and refuses a misaligned argument range. `Backend.dispatch_threadgroups_indirect` (part of
`Compute_pipeline`; Metal `dispatchThreadgroupsWithIndirectBuffer:indirectBufferOffset:threadsPerThreadgroup:`,
WebGPU `dispatchWorkgroupsIndirect`) reads a dispatch's threadgroup counts from three uints the
device wrote; the mock has no compute encoder; conformance runs one group of two from a buffer
and refuses a range past the buffer, misaligned or negative. OGPU-Metal is the only layer that maps checked OGPU render values onto
that generic surface.

`Scene.D3.instances_array` lowers each instance batch to one indexed Metal draw.
Rays keeps one mesh and one material/light uniform block, then appends 48
float32 values per instance (model-view-projection, world, and normal matrices).
The vertex shader indexes that table with Metal's instance ID. OGPU carries a
checked positive instance count; OGPU-Metal uses an indexed instanced draw and
keeps the transform buffer alive through completion. Scene3 uses
counterclockwise front faces, matching RDK mesh winding. Other scene paths
retain their existing winding.

Scene execution uploads transform blocks into three bounded shared-buffer
pages, with each draw's offset aligned to 256 bytes. A changed transform set
packs into the next page; an unchanged set reuses the previous page without
another upload and permits retained-command replay when other state matches.
Submission is synchronous,
so a page is reused only after its prior GPU work completes. Each page is
capped at 256 MiB. Small uniforms use these pages rather than inline
`set_stage_bytes`, so retained indirect commands can reference them.

Scene3 raster accepts indexed triangles, lines, and points. Lines use native
Metal line draws; points use a point-topology pipeline with an explicit
one-pixel point size. Line strips and loops become indexed line pairs once
per immutable mesh. `Scene.D3.Wireframe` extracts unique edges from triangle
meshes, while callers with polygon topology can pass RDK's unique topology
edges as `Mesh.Lines` to avoid triangulation diagonals. Scene3 packs the immutable
float64 CPU mesh directly from its coordinate planes into two GPU streams:
24 bytes per vertex for float32 position and normal, and 12 bytes for packed
RGBA and float32 UV. Narrowing rejects non-finite float32 results as an invalid
mesh. Unchanged coordinate, normal and index planes reuse their packed bytes
and GPU mesh even when color or UV planes change. Attributes use the existing
bounded auxiliary buffer cache and binding 11. The raw prepared-draw interface
still accepts its legacy 68-byte layout; uniform word 82 selects the layout.
Both shader paths use the same material, transforms and lighting.
Index buffers contain exactly four bytes per declared index. Validation bounds
the count before multiplying, so an overflowing count cannot alias an empty
or short buffer. Preparation and render preflight refuse malformed lengths
with `Invalid_argument`; render refusal precedes uploads and invokes the
post-preparation release callback once.
Mesh packing is cached by immutable components and render mode, with at most
16 entries and 256 MiB of packed payload. The catalog Box SOP
defaults to quad faces, matching its inspector parameter default.

The initial domain owns every window, event, layer, drawable, and resource
operation. Pure geometry and scene preparation may use the shared parallel
pool, but all results join before crossing the native boundary.

`rays_pathtracer` may depend on `flow`, `flow_ir` and `flow_gpu` (never `flow_sop`, geometry or UI): its
kernels are `.rays` programs (P8) compiled by the shared Flow pipeline to the same emitters as every other kernel.
It is an ordinary sibling library on the virtual `ogpu`
API: it leases the presenting window's OGPU device through
`Renderer.acquire_gpu` (or a lazily created headless device when
no window exists), owns one queue on it, and builds its acceleration
structures, library, pipelines, and frames through OGPU encoders. Its
packed-mesh path builds one bottom-level structure and a top-level instance
structure; unchanged prototypes retain their GPU buffers and bottom-level
structure across transform edits. A device mesh (a resident P and N a fused SOP write holds,
registered by `Workspace_gpu.display`) is traced without a readback: `Renderer.gpu_positions` hands
the tracer the registration's buffers (same device, initial domain), a Flow-emitted gather kernel
(`Lisp_kernel.gather`, `(array/nth src v)` over the corners' vertex indices, uploaded once per topology
as floats) writes the per-corner positions and normals the trace kernel and its `scene/trace`
geometric normal read (a corner soup: the shared emitter's `scene_vertices[prim*3+c]` and `nrm`
reads stay unindexed), and the structure over that buffer is built refittable once per topology and
refit in place each frame after (`refit_accel`), in one command buffer ordered before the next frame.
The tracer keeps the device mesh it last gathered from until that gather's receipt completes. An
indexed triangle geometry would build from the device P directly but the trace kernel's corner reads
would need an index table in the shared emitter (MSL, SPIR-V and the CPU walker), so it is not
done. It hands results back to Rays only as
an ordinary `Image.t` with a stable identity. It imports neither `metal` nor
the runtime, and nothing below it imports it. See `specification/pathtracer.md`.

## Frame lifecycle

1. Runtime creates an SDL3 Metal view and obtains its `CAMetalLayer`.
2. The layer supplies a drawable for each presented frame; resize updates the
   drawable extent before recording work.
3. Rays lowers immutable `Scene` data into Renderer, which owns the OGPU
   execution core and pipelines. Native
   implementation code validates device identity, resource lifetime, numeric
   ranges, and command ordering before encoding Metal commands.
4. Scene passes render into one owned RGBA8 texture, which remains the exact
   native capture/readback source.
5. `Backend.commit_present` appends the presentation pass, which renders that
   RGBA8 texture into the acquired BGRA8 `CAMetalDrawable`, to the frame's own
   command buffer and schedules the drawable. The queue keeps the frame,
   drawable, and source alive until Metal reports completion. Production
   drawables are framebuffer-only; the windowed runtime tests cover
   presentation end to end.

Scene execution records each frame through the render encoder. A frame's
passes become a replay plan of indirect command buffers when two consecutive
frames present identical draws (the automatic plan, admitted through a
fingerprint of the mesh, uniform, texture, and state identities and then an
exact payload comparison); an explicit prepared plan is keyed by identity and
version. Indirect commands cover buffer-only pipelines; textured families are
re-encoded each frame. Every render pass first makes the batch's buffers and
argument-referenced textures resident with `use_resources`, so a texture
reached only through an argument buffer is never sampled from a non-resident
page. Scene execution's plans borrow the bounded mesh, texture, and auxiliary
caches. Resources evicted while preparing a frame remain alive until that
synchronous submission completes. Before releasing any deferred resource,
execution drops both the prepared and automatic plans, including the
admission candidate, on success and failure paths.
The next frame rebuilds from the immutable CPU description. A dense scene may
exceed cache capacity without retaining a graph that refers to destroyed GPU
objects or increasing the cache bounds. Regressions cover 257 meshes, 257
textures, 65 auxiliary buffers, replay, resize, native pixels, and teardown.

A prepared-run cache hit does not make local mesh labels globally unique.
A mesh-cache hit by key must also match the exact immutable source uploaded
into that slot; otherwise it checks the payload hash and uploads as needed.
Each slot remembers at most one source, only when its CPU payload fits within
the slot's already bounded GPU byte budget. Replacing the upload replaces
that source identity. Alternating retained light/dark scenes with identical
local keys is checked pixel-for-pixel at 1× and 2× backing resolution, including
returning to the older prepared scene. This guards against geometry vanishing
or taking another scene's contents when a hover revisits an earlier scene.

Dense Scene2 runs of more than 64 consecutive geometry commands pack directly
into one native vertex/index mesh, including each primitive's RGBA values (12 bytes a vertex:
the float2 position and colour word, marks.md "Geometry and quads").
This avoids preparing and caching thousands of temporary draws only to copy
them into a batch afterward. Clip, transform, blend, image, and text commands
end a run; painter order is preserved. Packing takes two passes with
O(commands + vertices + indices) work and final-buffer storage. Shorter runs
keep one draw per geometry. Scene2 lowering caches only by identity: retained
display-list segments by (identity, version), plans by IR identity (or exact
command content), and prepared submissions by (identity, version); per-geometry
content is deduplicated once, by the executor's mesh cache. Native regressions compare 63, 64, 65, and
1,024 primitives with identity-barrier reference preparation, alpha/additive
overlap, fractional transforms, clipping, repeated frames, 1×/2× backing sizes,
and zero handle deltas.
The frame coordinator and runtime use the executor's pipeline
family and OGPU blend types directly, so preparing a draw no longer maps two
duplicate enum sets on the way to the backend.
Sampled draws use the same named record from Scene3 staging through runtime
and scene execution. The runtime passes the list through without a
per-draw tuple conversion; an unchanged 1× runtime frame preserves the list's
identity, while Retina scaling copies only records whose viewport or scissor
changes. Batch coalescing also requires an equal sample count.

Each validated immutable Render IR has a process-local integer identity.
Repeated lowering of that exact IR can reuse a bounded Scene2 plan by identity,
density, extent, and resource generation without hashing its command array.
Separately constructed but equivalent IR still uses content comparison to
preserve cache hits. The identity does not enter serialized IR or rendered
artifacts; the Scene2 plan cache remains capped at 16 entries and 64 MiB.

Private scene staging may split consecutive Scene2 commands into multiple
ordered native layers so independently changing UI regions do not invalidate a
stable retained plan.  A staging boundary emits no rendering command, does not
alter transform, clip, blend, or clear semantics, and is not part of the public
scene-construction API.  Each layer is lowered through the same checked OGPU
path and submitted in original scene order.

Retained 3D views: a window execution keeps up to four prepared 3D layers (`adopt_retained_view`,
keyed by the identity of the immutable prepared layer).  A layer seen on two consecutive frames is
rendered once into an offscreen execution the size of the window and later frames composite that
texture as a Canvas image, so a frame that only changes the UI does not shade the 3D view again.
A target costs an offscreen execution (its pipelines and uploads, a million-point mesh's indices
among them) and pays back only over later frames: once a view is dropped having composited fewer
than eight frames (a live, time-driven layer replaced every few frames), later layers wait eight
sights for a target, until one serves eight frames. The window's drawable size is checked on every use; a different layer, a resize or any failure
falls back to drawing the layer directly, and `destroy` releases the views before the device-lease
check.  Scenes the engine does not cache (textured, `Scene.D3.Private.cacheable` false) never repeat
an identity, so they are never retained.  `Scene` stays pure data; this is invisible to it.
`test_retained_view_native` checks that direct, retained and camera-change frames agree.

PXUI paints through a native-only instance layer. `Scene.Private.ui` wraps a
renderer-neutral `Scene_command.Ui_batch` (64-byte rect, textured, Bézier
wire-segment, and dot-grid instances grouped by clip and
texture) and its bound textures. Staging keeps it as its own
`Ui_layer`. PXUI reaches the batch builder through `Scene.Private.Ui_batch`,
so its library depends on `rays` without a direct `scene_command` edge.
Like `view3d`, the Render_ir materializer skips it, and enclosing
Scene transforms and clips do not apply. `Renderer.Private
.lower_ui` turns each batch into one indexed draw of the `Ui` pipeline family:
vertex pulling reads the instances, a 24-byte affine uniform maps logical
screen points to clip space, and the logical scissor is scaled to physical
pixels once, with the other draws. `Ui` draws bind their texture directly,
so the executor gives the family its own attachment class; it never shares
an argument-buffer render pass with Scene2 draws. Instance and index bytes
go through the digest-keyed mesh cache, so an unchanged UI re-uploads
nothing. The glyph atlas is an ordinary image resource whose generation
changes only when new glyphs are rasterized.
PXUI publishes the atlas through public `Image.upload_rgba`, preserving the
image identity on replacement and retrying a failed upload on the next frame.

`Canvas.render` uses the same lowering, pipeline variants, validation, and
completion path against a layerless owned texture on the shared device: the
offscreen execution leases the presenting window's OGPU device (or the
lazily created headless device) through the same lease as GPU film
producers, and the scene executor borrows that device without destroying it.
A Canvas creates its offscreen coordinator lazily, reuses it for all
subsequent renders, and after each render publishes the completed target
texture to the Canvas resource. A window scene that draws the canvas samples
that texture directly, keyed by canvas generation, with no readback or
upload; CPU access (`pixel`, `pixels`, `capture`, `to_image`, `save_png`)
reads the texture back lazily, and a CPU mutation makes the CPU pixels
authoritative again. `Canvas.Private.gpu_source` borrows only a successfully
completed frame, on the initial SDL domain and platform main thread. Its source
callback expires before any later render attempt (including an invalid density
or failed staging), explicit invalidation, CPU mutation or destruction. It
checks the publication epoch, runtime generation and physical texture identity;
an expired callback cannot keep a retained Scene usable merely because its
published Image generation has not changed. Successful captures and actual
GPU pixel readbacks are counted independently by `Canvas.Private.pixel_stats`.
`Canvas.destroy` detaches the runtime resource before destroying the coordinator
and releasing the lease, without reading discarded GPU pixels. Owned CPU
captures and their leases survive that destruction.
Offscreen submission never creates a hidden window, acquires a drawable,
presents, or waits for display pacing.

Windowed runtime configuration selects FIFO presentation when vsync is enabled
and Immediate presentation otherwise. The selected mode is retained across
surface resize and is the mode reported in presentation facts. Layerless
Canvas targets always report `vsync = false` and `presented = 0`.

Drawable dimensions are physical pixels. `Frame.input.size`, scene
coordinates, input positions, and PXUI layout remain logical points; the
backend performs the logical-to-drawable conversion exactly once at the native
viewport boundary. Captures read the owned drawable-sized RGBA8 target;
they do not masquerade as a read of the BGRA window drawable.

## Resource rules

Public camera matrices use clip depth [-1,1]. Native
Scene3 lowering converts that depth once to [0,1] before packing ordinary and
instance MVPs or inverting the World background projection.
The fitted GPU sun-shadow matrix already uses [0,1].
Generic shaders accept native matrices directly. This preserves default
orthographic near/far clipping and perspective geometry close to the near plane.

Images, fonts, canvases, meshes, pipelines, and command resources are owned
native resources. Safe APIs make destruction idempotent, reject use after
release, and preserve same-device validation. Sketch-owned resources are
released through `Sketch.run_state ~on_stop` while the SDL3 and Metal runtime
is still live. Command completion retains any referenced resources until their
submitted work completes.
SDL3, TTF, and mixer finalizer tokens queue until the initial domain drains
them; no token is discarded when the queue grows.
Resource-level font rendering returns an owned text snapshot. Automatic scene
text and explicit high-level font caches are bounded by their Rays owners;
the resource font has no second renderer-keyed cache.
High-level `Font.render_text` consumes that snapshot into an `Image`, transferring
the copied SDL surface pixels without further RGBA copies. Public `Text.pixels`
and `Image.pixels` remain copy-returning; a consumed text snapshot is destroyed.

`Scene`, `Canvas`, `Image`, `Font`, and `Audio` remain high-level Rays
interfaces. Their implementation lowers to the native GPU stack without
changing public scene semantics. Native framebuffer capture and export use
the same checked readback path as presentation diagnostics.

An image whose CPU pixels are replaced behind a stable identity (watched
reload or `Image.replace`) carries its generation in the sampled-texture key;
the executor re-uploads a changed generation into same-shape GPU storage.
The path tracer instead publishes one of two borrowed OGPU film textures on
completion. Scene samples that texture directly without image staging or a
per-frame CPU readback. The tracer writes the other texture while rendering;
explicit `Image.pixels` and capture remain checked readback boundaries.
Private runtime image publication also accepts a validated borrowed-output
callback, with no CPU pixel allocation or readback. The callback must keep
returning the physically identical published texture; expired or substituted
outputs fail with a typed resource error. Resized republication updates image
dimensions and generation only after validation. Both scene lowering and a
pass over 2D/UI layer resources before retained replay check source liveness
even when generation is unchanged. Native stage stamps include every such
layer's resource generations, including retained segments and mixed layers.
Explicit CPU snapshots own their readback bytes independently of later output
updates. Images release the borrow, while producer owners release the GPU
resources. CPU replacement and Canvas copying clear GPU authority on success.
Private Texture views borrow these images without pixel arrays or ownership.
Their identity is stable; dimensions follow image publication, and native mesh
keys include image generation. CPU sampling, extraction and mipmap/subsection
operations require an explicit immutable snapshot. The native mesh callback
binds the GPU image directly instead of entering the CPU texture-level cache.
Both window and offscreen image consumers use resident GPU snapshots and retain
same-device validation. Fresh staging validates authored image resources before
constructing commands and preserves resource errors through the private checked
boundary. Cached-stage replay validates image dependencies, including meshes,
without rescanning unchanged authored geometry. Mixed 2D layer caches freeze
resource identity/generation lists beside their IR, rather than retaining stale
source/destination rectangles after resize; those lists count against the cache
byte capacity. Existing inspection APIs format these errors as strings.
The workspace shares one lazy GPU owner between drawing and image display.
Qualified image/map selection scopes that backend at resolution, including
preview and mesh callbacks. Its completed packed output converts through one
image sink per stable authored key; output stamps and runner identities do not
enter that key. The owner commits a new sink only after successful conversion
and publication, rejects a 65th site and closes uncommitted sinks on errors or
exceptions. CPU payload and display validity are independent; exact-first map
cooking creates no runtime image. CPU/GPU authority transitions preserve the
runtime image identity and reuse the borrowed Texture when GPU authority returns.
Display image/render retains one Canvas per stable authored entry and publishes
its completed texture through the same borrowed Image/Texture route. Every stale
request expires the previous source before argument forcing or drawing preparation.
Same-size frames reuse the target; resizing installs a replacement only after
rendering and publication succeed, then destroys the old target. Failed replacement
targets close immediately. Preparation records reachable omitted width/height
dependencies, so an explicit-size parent refreshes when its implicit-size child
changes; unrelated fixed-size images retain their cached frame.
CPU image/render cooks exact children into a separate temporary Canvas and owns
its captured payload. It preserves resident child images and the parent's display
image, generation and texture. The private image_render_stats hook counts actual
target creation/destruction, captures and GPU pixel readbacks across live and
closed targets. Legacy CPU publications invalidate old display stamps. Recursive
resolution reserves pending ancestor entries inside the 64-image bound. Close
destroys published Images, then retained Canvases, then sinks, host runners/pipelines
and the GPU lease. Explicit deferred frozen GPU-image snapshots remain separate work.
Private workspace GPU statistics combine live runner/sink counters with totals
frozen when an entry is retired. Failed uncommitted sinks and closed owners
remain accounted for; pipeline compilation and runner creation/release are
reported separately. Inspection requires the initial domain and happens outside
frame measurements, so eviction cannot hide uploads, creations or pixel reads.
Scene rejects malformed, destroyed, and foreign-device GPU image sources; a
canvas rendered on another device than the window's falls back to its CPU
pixels.

Canvas pixel storage is a compatibility/readback snapshot, not a renderer.
`Canvas.render` replaces it only with completed native GPU output. The
snapshot continues to support `pixels`, `capture`, `to_image`, `save_png`, and
explicit resource destruction; no CPU raster fallback participates in scene
rendering.

## Qualification

A surface is complete when focused correctness, ownership, conformance, and
performance tests pass. The dependency-direction gate and the
native link audit are release requirements: production artifacts may link only
the declared SDL3, Metal, OGPU, and platform frameworks for this backend.

Long-running, SDK-, driver- and machine-specific checks run under
`RAYS_SLOW=1 cargo t --all`; they are release evidence, not the pre-commit
gate.
# Native UI input and export

`native_layer_token` stores only an opaque handle, owner/generation identity
and its platform-supplied release operation; it links no Apple framework.
The macOS SDL adapter supplies CoreFoundation retain/release operations.
Invalidation releases the owned handle once and refuses future borrows.
SDL selects its macOS or portable C hooks and link flags through the owning Cargo build script's
`system` condition. Metal libraries, build actions and tests are enabled
only on macOS, and the native `rays` executable explicitly links that backend.
Mock conformance selects its own implementation independently.

The dependency-free `input` leaf owns keys, buttons, scroll phases, ordered
events and immutable logical frame facts. `Runtime_input.event` is an alias
of `Input.event`; the SDL adapter is the only native translation. The native
host and editor use the same variants, without an intermediate Rays event
type or string-valued frame events. `Input.key_name`, `button_name` and
`scroll_phase_name` serialize the documented Lisp/web vocabulary at that
boundary. Runtime depends directly on `input` and `sdl3`; `input` reaches
neither the binding nor the host, as the dependency gate checks.

The native window runs SDL3 text input only while a text field has focus. PXUI
emits a focused text region in logical points; Sketch forwards it on each
render, which starts text input and places SDL3's IME candidate area, and
stops text input when no region is focused. Text fields, text areas and the
picker search show the I-beam cursor under the pointer. PXUI measures the text
run and sends its caret offset in logical points to SDL3. Text fields, numeric
editors, and picker search share UTF-8 caret and selection editing. PXUI
applies text events only to a focused editor; Sketch UI
suppresses workspace and graph keyboard shortcuts while an editor has focus,
and its leader key (/) only
arms while no editor is focused. Camera PNG requests capture the
just-presented native framebuffer through `Sketch.run_state`'s `after_present`
hook. The UI offers the supported native 1× export factor. PXUI copy, cut, and
paste go through the public `Rays.Clipboard` result boundary; failed writes
never clear a text value.

PXUI splitters request horizontal or vertical resize cursors while hovered or
captured. The Sketch UI host sends that request through `Sketch.set_cursor` and
the execution/runtime boundary, restoring the default cursor when no control
requests one. `Runtime` creates SDL cursor handles lazily, reuses one per
shape, and destroys them with the window. PXUI never imports SDL3.

# Relative pointer mode

`Sketch.set_relative_mouse` is the only public entry to SDL relative mouse
mode: it runs `Renderer.set_relative_mouse` →
`Runtime.set_relative_mouse`
(`Sdl3.Window.set_relative_mouse`) and switches the shared
`Runtime_input` source to relative accounting, so `Frame.mouse_delta`
sums SDL `xrel`/`yrel` (the event pump reports them through
`Runtime_input.add_motion`; `Sdl3.Event.poll_coalesced` sums the relative
motion of the samples it drops) instead of absolute differences that stop at
the window edge. Frame aggregation lives in this shared input source; the SDL3
binding exposes no second mouse-delta reduction helper. No SDL value crosses
into Rays's public API, the sketch
loop turns it off when it stops, and the library dependency graph is
unchanged (`tools/gate/src/main.rs`). Sketch UI fly mode is its only
in-tree user.
The SDL3 boundary exposes single-event polling and the coalescing poller;
the runtime uses the latter to keep input floods bounded.

P3 lowers renderer-neutral `Render_ir.Shapes` through the existing `Ui` pipeline family, with
new SDF circle/line tags in the shared shader. It does not add a pipeline family or native
handles to Scene. The Scene command walker applies its full affine transform and clip/blend
stack before preparing these draws; this path is separate from PXUI's self-contained batch
coordinates. Shape instance/index/affine payloads retain per-slot storage when equal, allowing
the backend's identity/version upload cache to reuse them. Packed GPU output carries an opaque
identity/count/stamp. P5's private `Renderer` circle sink converts interleaved float32 xyz
positions into this same instance ABI on the GPU, then `Scene_execution` borrows the completed
vertex buffer. CPU uploads cover only retained quad indices and the existing affine uniforms;
the drawing path never reads positions or instances back. The full affine, clip and blend
stack still belongs to the normal command walker. One sink owns one geometrically grown buffer
up to one million instances; at most 64 sinks and 128 borrowed registrations are live.
Updating a sink invalidates its previous token, so independent draw inputs own independent
sinks. A reserved `gpu:vertices:` mesh key resolves only through typed private registration,
with creating-domain, device, producer-generation and closed-owner checks. Prepared and
automatic replay retain these lifetime guards; stale registrations return `Stale_handle`.
Registrations never destroy producer buffers. Closing a sink releases its registrations and
storage before its queue/device lease; releasing a GPU lease also closes its remaining sinks.
The conversion kernel rejects nonfinite coordinates or radius-expanded bounds through a
four-byte status read after completion; position and instance arrays remain resident.
The conversion shader is compiled offline; mock checks verify binding,
index-upload reuse, bounded allocation and ownership. Native exact instance bytes, transformed
circle pixels at 1x/2x, all six blend modes and stale replay remain qualification checks, never
inferred from mock output. Host resources pin a bounded table
for a workspace lifetime and close before the runtime and SDL lifecycle ends.

The 2026-10-11 audit restored `test_gpu_draw`'s actual invalid-input dispatch,
transformed/clipped full-image comparisons at both densities and all six
blends, and retained replay/lowering refusals after generation or lease
invalidation. Retained versions use the token stamp, and teardown checks the
handle baseline. Builds, typechecking and portable boundary/dependency tests
pass; this runner's native attempt fails at Metal device acquisition, so
these native assertions remain unverified.

Loaded/rendered Flow image nodes resolve through `Flow_sop.Lower.with_images` on
the initial domain. Value-lane reconstruction installs immutable CPU Image payload
nodes before submitting a graph to a worker; resource decoding and Canvas rendering
never happen in a cook callback. Drawing receives a typed image resolver from the
same environment owner. Resource failures remain `E_IMAGE`, and no alternate raster
backend is introduced.
## Mixed cooked SOP inputs

The cooked-payload boundary also permits mixed nominal SOP inputs.
`sop/attr_from_image` takes geometry and a renderer-independent image,
borrows its immutable normalized RGBA plane for the exact RDK bilinear
Point-UV sampler, and returns geometry with a float attribute. No renderer,
resource handle or native image type reaches RDK; the existing library
dependency edges are unchanged. Factory slot type names pass through the
generated catalog to Flow checking and graph projection.

Function keyword inputs use the same physical factory slots. A hidden
`flow.function` resource cooks captured geometry before supplying a
`Sop.Payload.Kernel`. Flow_sop owns the specialized function and its
compiled packed program; Sop borrows packed columns through the neutral
`Kernel.prepare` runner interface. It imports neither Flow nor Flow_ir.
The runner supports current float/vec3 packed signatures and returns
`E_KERNEL_FORM` for unsupported bodies. Reference execution remains available
for parity checks. Fresh binding identity and cooked payload IDs prevent reuse
across changed captures; cache hits retain their IDs. Zone element bindings and
forked state snapshots participate in resource identity, and frame-dependent
functions declare Input dependencies. Payload accounting includes retained
packed captures and captured input storage conservatively. No native handle
enters the function payload.

Captured image source plumbing stays in Flow_sop. `Lower.source_context` selects
the geometry plan node's owning instance, and `source_cone` restricts its network
to upstream inputs, filtering drives and frame callbacks before live resolution.
The unrestricted context remains available for nested captures. The existing
point-origin proof is exposed by `Attribute_kernel.source_origins`;
`Image_kernel.with_inputs` preserves the prepared UV/program only when that
ordered proof still matches. `Attribute_kernel.materialized_source` is a normal
one-input geometry consumer, so Session expands packed instances before exposing
their attributes. The workspace resolver receives the current network and
compiled map at its frame callback, preserving the enclosing context during
nested resolution. Source preflight retains compiled roots and exact context
projections; owner-monotonic revisions invalidate only reachable image stamps.
Stateful sources fork the supplied snapshot again for a new live input. The owner
retains at most 64 source records and 64 MiB of charged geometry/flattened arrays;
oversized captures execute uncached. One request owns a lazy zero-cache Session,
pending image reservations and the active dependency path; recursive callback
entry returns `E_IMAGE_CYCLE` and unwinds before worker resource effects.
Data retention requires its source metadata to remain present at the same
revision, so an active request cannot resurrect evicted metadata's cached data.
Private counters separate retained record/byte counts from cumulative successful
display-capture materialization and attribute flattening; exact CPU cooks bypass
that cache and its two work counters.
Exact CPU requests preserve resident images. Export uses reference lowering and
recursively resolves CPU images before uploading its independent image copies.

CPU packed execution and GPU input preparation share uniform evaluation through
Flow's existing evaluator execution hook. Supported numeric subexpressions run
in packed CPU kernels; scalar surrounding expressions and compilation refusals
retain reference evaluation. The context shares the current state, immutable
bindings, resolver and live input. No uniform result is cached across frames.
Canonical named `+` reductions check the captured operation environment and
packed declaration capability, then use the same typed ordered accumulator
instructions as an explicit lambda. Unsupported or changed declarations stay
interpreted. Existing transactions and reference fallback preserve diagnostic
and state behavior; this does not reassociate reductions or add GPU reductions.
The exact non-collecting Zip addition pattern at width 1–4, with no skips,
reads its single input directly after matching every input/accumulator/add
slot, output and dependency flag. Each ordered chunk visits elements and
components in ascending order with the same finite checks, without scratch
storage or instruction dispatch. Other layouts, Product folds and scans retain
generic packed execution.

Flow IR accepts an explicit renderer-neutral `Gpu.config`: backend callbacks,
precision and placement policy are immutable data, with no ambient installed
backend. The same configuration follows startup, lowering, cooking and deferred
attribute execution. `Flow_native.Workspace_gpu` owns each workspace's worker
proxy, mailbox, cancellation and initial-domain pump. A cook's requests come in chains (a
dispatch, its planes, the next dispatch), so a pump that served one waits 0.5 ms for the next
rather than leaving each link to the next frame; a settle, which blocks until the device finishes,
is never taken that way. Its proxy remains usable
by worker cooks between frame scopes, and closing the owner cancels pending
requests. Pre-window cost queries decline until that owner has entered its pump
loop, avoiding count-sized GPU calibration during static startup. Compiled
kernel and display caches distinguish precision as well as backend/program.
Each calibration sample waits for its fast dispatch to finish and validates
status through retained planes, releasing them without reading payload. A
failed completion/status supplies no cost evidence; the bounded measurement
cache remembers the refusal. Submission time alone is not a completed cost.
CPU input and table identities are fresh per sample, so warm-up cannot hide
their uploads behind the immutable-array cache. Index-range sources keep their
existing index-buffer binding instead of allocating dummy CPU source arrays.
GPU values carry identity, count, width, output index, generation and optional
completed device dispatch duration; they carry no backend handle. A
bounded `Flow_gpu.Host` owns pipelines and runners; the drawing boundary borrows
the current generation after checking completion and kernel status, preserving
typed execution failures; closed or superseded generations are absent.
Sibling mark attributes use the shared `Core_draw.device_groups` policy.
Their output views select distinct buffers of one dispatch while sharing its
owner and generation; selection does not extend a borrowed buffer's lifetime.
Production placement requires a measured cost and an approximable packed producer; absent
measurements keep the CPU tier. Explicit qualification can exercise the GPU
route, while references and exports retain the CPU path. The display/readback
path uses `Cost.choose` over total estimates, without an element-count floor.
Its named choices appear in `Profile.placements`, bounded to 512 recent owners,
with optional counts, costs and CPU-consumer counts. Invalid evidence and ties
keep CPU; unsupported forms and missing consumers have distinct reasons.
SOP record writes use the same comparison with explicit known CPU-reader counts,
amortised output materialisation and resident-input readback credit. Missing
reader information is an explicit CPU refusal; `Cook.prepared_context` supplies the
reader counts and `Lower.Chain` applies the same comparison to whole mesh chains
(specification/gpu-codegen.md). A rejected later record output releases
earlier borrowed outputs before CPU fallback. Compile, dispatch and
readback appear separately in execution reports. GPU group time uses the
completed device duration when available and wall time otherwise. Images use the existing UI
image batch for borrowed inspector thumbnails; probing dimensions never loads
resources or renders a Canvas inside a UI frame.

`Flow_gpu.Image_sink` consumes a current validated width-four runner output.
It owns one reusable storage/copy-source buffer and an RGBA8 texture with
binding and copy-source/destination usages. Conversion clamps finite float32
channels and rounds ×255 to nearest, ties to even, writing 256-byte-aligned
rows. Compute and the existing `Backend.buffer_to_texture` blit share one
conversion submission; publication follows completion. The producer's
four-byte finite-status read remains mandatory; the converter reads no
pixels. Borrowed texture tokens are generation checked. Resize allocates a
replacement pair before publication, cleans it up on failure and retains
the old pair until success. Close releases the pair, pipeline and library
on the initial domain before the GPU lease. This sink does not qualify an
authored image site or force an exact image snapshot; those belong to the
workspace/compiler and resource boundaries.

The compiler qualifies canonical authored `image/map` sites by adapting their
actual checked function argument to a representative packed pixel map. The
synthetic map retains lexical captures and distinct authored/runtime provenance;
all observed specializations must pass the existing packed compiler and GPU
form checks. Lowering records only the authored path per image plan node,
with ambiguity sticky in either observation order. Workspace image owners bind
the plan, qualified paths and image-site array, invalidating prepared maps
when that metadata changes. Display publishes the completed GPU texture through
the runtime image boundary; CPU consumers have independent exact payloads.
Native qualification exercises actual owner selection, resident publication and
draw/image and mesh consumers, alongside explicit full-byte source comparisons.

## `scene/trace` (P8.S2)

`(scene/trace scene origin dir tmin tmax time)` returns `{:t :prim :u :v :normal :hit}`. `scene` is an integer id
the host registered in `Flow.Ray_host` (a pure closure; `flow` never sees geometry). On the CPU the walker and the
Packed executor call it (`Flow_sop.Trace_scene` answers from a `Surface_index`, distances in world units along the
normalised direction). One region instruction, `Trace (field, registers)`, serves each field; the emitters memoise
the query per argument tuple and block. Metal emits `intersector<triangle_data>` over a
`primitive_acceleration_structure` plus a triangle-vertex buffer (for the normal) bound after the count bindings
(`Kernel_layout.scene_binding`; `Run.dispatch ~scene`). SPIR-V (1.4, `RayQueryKHR`, `SPV_KHR_ray_query`, Vulkan 1.2)
issues an opaque-flag ray query against the same two bindings. Exact precision keeps the op on the CPU; the web
targets refuse it (`E_WEB_SOURCE` for JS, `E_GPU_FORM` for WGSL). Procedural spheres are an in-loop analytic test after the ray query on every target: a `float4` (centre, radius)
buffer bound after the vertices and a sphere-count uniform; a sphere hit reports `prim = -2 - k`, `u = v = 0`, and the
nearer of triangle and sphere wins. Instancing is a static scene fact: `Emit.kernel`/`Spirv.kernel ~instanced:true` (and `Kernel_layout.plan ~instanced`)
bind a TLAS (`intersector<triangle_data, instancing>` / `OpRayQueryGetIntersectionInstanceIdKHR`) plus a table of 16
floats per instance (first primitive of its prototype in the concatenated vertex buffer, the world-to-object 3x4
row-major, three spare); the normal is the transpose of that matrix applied to the object normal, all in scalar
registers. The record's ninth field `instance` is the TLAS instance index (0 for a flat scene's triangles, -1 for a
sphere or a miss), and `prim` is the primitive within its prototype. `Flow_sop.Trace_scene.register_instances` is the
CPU host.

Motion (WN10) is a third static fact, `~motion` on `Emit.kernel`/`Kernel_layout.plan`, and makes `time` meaningful:
keyframes sit at times 0 and 1 and clamp outside them (Metal's `Clamp` borders). A flat moving scene is vertex
motion: `intersector<triangle_data, primitive_motion>` over an `acceleration_structure<primitive_motion>`, with the
shutter-end vertices bound after the spheres and the normal taken from the interpolated corners. An instanced moving
scene is instance motion: `intersector<triangle_data, instancing, instance_motion>`, and the instance table grows to
32 floats (first primitive, start and end object-to-world 3x4, seven spare); the normal is the inverse transpose of
the interpolated matrix (its cofactor columns, signed by the determinant). Moving prototypes inside an instanced scene
are not supported. `Run.build_scene` builds either kind through the OGPU acceleration API (`Motion_blas` /
`Motion_instances`) when the device has `Ray_tracing_motion`; without it the scene is built at the shutter midpoint
(the mean of the keyframes, as `Mesh_data.shutter_midpoint`) and reports `motion = false`, so its kernel is the static
one. SPIR-V refuses a moving kernel (`E_GPU_FORM`): `SPV_NV_ray_tracing_motion_blur` moves trace-rays pipelines only,
not ray queries, so a Vulkan host builds at the midpoint. The CPU host (`Trace_scene.register ~ending`,
`register_instances ~motion`) interpolates vertices (intersected directly) or transforms at the query's time.
`crates/flow_sop/src/lisp_sop/test_trace.rs` checks both kinds against the CPU at several times, and the midpoint fallback against
the CPU at 0.5. Static kernels print byte-identical source.

Strands (curves) are the fourth static fact, `~curves` (`` `Native `` or `` `Loop `` on `Emit.kernel`, a bool on
`Spirv.kernel` and `Kernel_layout.plan`). A strand segment is pathtrace.metal's linear round curve with sphere
caps: two float4s (end a, radius; end b, radius) in the `scene_strands` buffer bound after every other scene
buffer, followed by its native curve or swept-AABB acceleration structure. A hit reports `prim` = the segment,
`u` = the curve parameter, `v` = 0,
`instance` = -2 and the reference's normal (the offset from the axis point at `u`, less its tangential part); the
nearest of triangle, strand and sphere wins. `` `Native `` (where `Run.build_scene` finds `Ray_tracing_curves`,
Apple GPU family 9 and later) intersects a second, curve-only primitive structure built over the segment records
themselves (control-point stride 16, radius at `.w`) with `intersector<curve_data>` bound after the strands.
`` `Loop `` keeps the CPU capsule loop; GPUs without native curves build one swept AABB per segment and
run the capsule test only for a `RayQueryCandidateAABB`. The test solves the cylinder's roots within the
segment and the end spheres' beyond it, from the ray's point nearest the segment's middle so float32 stays
within 1e-5 of the CPU. Building the candidate structure is linear in segment count; rays test only its
intersections. `Trace_scene.register ~strands` is the CPU host. `test_trace` checks curve hits against the CPU;
`test_spirv` validates the curve candidate-query module (`fast_trace_curves`).

## Resident value scopes

Resident value execution uses `Flow_ir.Gpu.backend.residency`, a neutral scope
over `Core_exec.resident`, owned integer/float materialisation callbacks and
topology validation. No OGPU handle crosses into Flow IR or SOP libraries.
`Executor.force ?resident` forwards this scope through normal and cached root
execution. The native host supplies it in fast precision; workspace workers
route scope creation, invocation, counts, validation, retention and cleanup to
the owner domain. Materialisation and release retain their existing any-domain
holder semantics. Ordinary backends may decline the scope with `None`.
Shared mesh cooking uses this capability, when `Lower.Chain` selects the device (Qualification
forces it in tests), to validate and install deferred positions/topology. Resident storage is mapped: workers
invalidate its owned view and decode uint32 words without initial-domain-only
buffer reads. `Backend.mapped_view` exposes the live mapped view (made on first ask on a
discrete device), with shared conformance checks for identity, release and destroyed/unmapped
buffers. Fused attributes stay resident when their measured comparison selects the
device. `Workspace_gpu.close` calls `Lower.forget_backend`, so no lowered node or
placement state outlives its owner.

Stateless structured geometry calls can use the same scope. A neutral Core
host-source offer lets the SOP host consume resident mesh fields before the
ordinary CPU-reader boundary, through the shared mesh constructor. Other host
sources still materialise. Retention pins the transferred buffer and preserves
its placeholder/count cell until scope close, so later calls may reuse that
immutable input. CPU reads consult the holder's cache. Integer aliases over one
range buffer keep their own logical lengths and share one physical read;
different-length views retire together through the existing holder pool.
The Core offer contains Flow values only, with no geometry or backend types.

## Film accumulation (P8.S2)

A read-modify-write float film needs no new sink: a one-output kernel whose input 0 is `~bound` to the film buffer and
whose output goes `~into` that same buffer reads and writes only its own pixel. A second kernel divides by the sample
count and `Image_sink.convert` makes RGBA8 (ties to even). `crates/flow_gpu/tests/` runs four accumulations and the
resolve on Metal and checks every pixel exactly.

## Memory

`lib/mem` is the ledger: a leaf (`lru` depends on it) of owners in host or device bytes, safe from any
domain, one budget and one retreat (the plan's order, in `Mem.retreat`, run by the editor at each frame
boundary). `Lru.create ?shed` states when the retreat may touch a table: `Home` (its domain only),
`Locked` (skipped while its user holds the lock), `Never`. The session is shed by its own domain or
while `Async_cook`'s worker is provably idle. A cook over budget fails per node with `E_MEMORY`; the last
good frame stays, and the error clears with usage. Process facts and the pressure source are one small
function each per OS in `lib/mem/mem_stubs.c`; the source only stores a level the next boundary reads.
The environment (`RAYS_MEM_BUDGET`, `RAYS_MEM_CLOBBER`) reaches `Mem.configure` from the process boundary.
`rays mem FILE [--frames N]` prints the ledger of a workspace's real frames, window-free, through
`Rays_editor.Measure`, the same runner `tools/mem_gate` checks `specification/memory-budgets.sexp` with
(`@tools/mem`); the runner makes its canvas's renderer before the document opens, as a window does at startup.
A discrete device (`Caps.unified_memory = false`: Vulkan, the mock) has a second limit: its working set
(VMA's device-local budget on Vulkan) bounds the device's own bytes (`Mem.set_device_budget`), and the host
budget, 60% of RAM (`GlobalMemoryStatusEx` on Windows), bounds the footprint alone. `Mem.retreat` checks
the host first and then the device at the same frame boundary, from the backend's `allocated_size` (the
only poll; it is read once there). Each limit sheds only its own pool's caches in the same order; the
host limit's `later` steps (undo) are not run for VRAM. A large kernel buffer is admitted against the device
limit with `Mem.admit_bytes ~pool:Device`, so a refusal is `E_MEMORY` and the last good frame stays.
So is a host copy a kernel's inputs make (an integer array's float copy, a topology's fans, a mesh's owned
arrays), against the host limit. What a boundary drops is credited only until the next boundary, and an
admission over the limit believes it only after a collection: an entry something else still holds frees
nothing when its cache drops it.
A one-pool device (Metal) never sets a device limit, so its behaviour is unchanged. Out of device
memory from the driver (`VK_ERROR_OUT_OF_DEVICE_MEMORY`) is a typed `Capacity` error
(`crates/flow_native/tests/`). `@tools/mem` on Windows reads `win.KEY` budgets over `KEY`, because
a footprint there is committed private memory.

## Vulkan on Windows

`lib/vulkan` is a C bridge with one operation per OGPU driver operation, over volk and Vulkan Memory
Allocator (`lib/vulkan/AGENTS.md` says why there is no generator); `lib/ogpu_vulkan` holds the driver
records. `lib/ogpu_native` selects the platform's implementation; an executable names it. Required: Vulkan
1.3, a discrete device, push descriptors, ray queries, acceleration structures, `VK_EXT_robustness2` and
`VK_EXT_extended_dynamic_state3`. Missing any is a typed `No_adapter` at startup.

- Memory: a mapped buffer is a device-local buffer plus a Bigarray view, `calloc`'d on its first ask and
  dropped on release or destroy, so committed host memory follows what the host reads and writes, not device
  bytes. `flush_mapped` copies the view into
  staging and records the copy into an upload command buffer that the next submit runs first.
  `invalidate_mapped` copies through a dedicated transfer queue with the runtime lock released, from any domain.
- Hazards: a global barrier between dispatches and at each encoder's end, and every image in
  `VK_IMAGE_LAYOUT_GENERAL`. Correct on any device; `ponytail:` comments name where it costs.
- Binding: every stage shares one binding space, pushed at draw and dispatch, read from the module's own
  SPIR-V. An unset binding is written null, as Metal leaves it unbound. An indirect command buffer is its
  recorded calls replayed; an argument slice is a (texture, sampler) pair bound at a slot and the next.
- Lifetime: destruction waits for the next submit's completion. One `VkInstance` lives for the process,
  because destroying the last one unloads the driver under its own starting threads.
  Instance records hold logical fields the bridge makes native at build, when structure addresses are known.
  `Ray_tracing_motion` is true after Vulkan enables `VK_KHR_acceleration_structure` and
  `VK_KHR_ray_query` with their required features (both are startup requirements). Motion triangles use a
  swept AABB per triangle and a SPIR-V watertight test at ray time; motion instances use an AABB-only proxy TLAS
  and a separate prototype TLAS for nested, transform-interpolated queries.
  `Ray_tracing_curves` remains false because Vulkan has no native curve geometry; linear curves use per-segment swept AABBs with
  capsule candidate tests.
- Math and text: `specification/core.md`, "Goldens across operating systems".
- Facts about the tested card: `specification/vulkan/rtx2080ti.json`.

OGPU has no barrier call: a serial encoder orders its dispatches, and each backend makes that true. So nothing
above the backend can omit a barrier, and the mock has no such check to make; on Vulkan the backend inserts them
and synchronization validation is what checks it, on the tested GPU. What the API does leave to its callers is the
flush and invalidate contract, which the mock enforces by keeping the two copies apart, and a live allocation limit.
