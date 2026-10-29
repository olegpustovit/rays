# SOP graphs

Status: first coherent graph and core-node iteration implemented.

## Lisp implementation boundary

Accepted node bodies live in `lib/flow_sop/nodes/*.rays`.
The checked `defnode` owns names, parameters, defaults, ranges and facts.
Native geometry, spatial and exact-predicate capabilities stay behind typed host operations.
The editor and interpreter consume the same checked definitions.
Lisp-output geometry digests provide the regression contract for accepted bodies.
The architecture cleanup captures all fixture/case outputs before removing native twins.
It removes parity-only RDK code rather than retaining an oracle library.
Migration measurements and session logs are available in Git history at `62582188`.
Current work and verification are tracked in [cleanup/status.md](cleanup/status.md).

## Product intent

`rays.sop` combines Processing-style immediacy with a non-destructive
SOP workflow. A new user should be able to construct, animate, inspect, and
render an interesting model without learning packed storage. The same
`Rdk.Geometry.t` remains available to advanced users who descend to native
kernels; there is no separate easy-but-slow geometry model.

The common path is a pipeline written in Rays Lisp, the only way to build a SOP graph:

```lisp
(-> (sop/grid :columns 180 :rows 180 :size 12.0)
    (sop/noise_displace :amplitude 0.7 :frequency 0.18 :seed 42)
    (sop/color_by_height :low_red 63 :low_green 63 :low_blue 159 :high_red 255 :high_green 140 :high_blue 0))
```

One-input modifiers thread through `->`. Multi-input nodes name their ports,
for example `(sop/copy_to_points tree points)`. Familiar words
are preferred in this layer (`attribute`, `primitive`, `geometry`); abbreviated
HDK vocabulary remains available in the expert RDK namespace.
Use `(sop/delete_attributes input :point_pattern "tint")` to delete one point
attribute, or set patterns for several owners in one node. The editor uses the
same `Delete Attributes` node; the old single-name node is removed.
`sop/rename_attributes` with `:rules` handles exact names and wildcard
captures with owner and conflict rules; an unmatched rule leaves geometry
unchanged. The editor uses `Rename Attributes`.
`sop/promote_attributes :pattern "weight"` handles a single source attribute;
output and index patterns can rename its promoted values and source indices.
The editor uses the same `Promote Attributes` node for one or many attributes.
An unmatched pattern leaves geometry unchanged.
Use `sop/group_rename` with `:rules` for owner-scoped or multiple group renames;
the editor exposes the same rule-based `Group Rename` node.
Tests write their graphs as Lisp text through `lib/flow_sop/lisp_sop` (`Lisp_sop.node`);
there is no typed OCaml constructor per node.

## Ownership and dependency boundary

Sop owns immutable graph nodes, operator contracts, parameter/context
dependencies, validation, diagnostics, cooking, inspection, and bounded
session caches. It may use RDK and public Rays value semantics. It
does not import Runtime, SDL3, Metal, or platform policy.

Cooking produces `Payload.Geometry of Rdk.Geometry.t` or `Payload.Image of Image.t`.
The image stores row-major RGBA float samples in [0,1] or owned RGBA8 bytes, four per pixel. Its
constructor copies public input; its public pixel reader returns a copy.
Images and geometry share `Rdk.Data_id.fresh`, so session component accounting
cannot confuse their identities. An image contributes one component of
`width * height * 4 * 8` bytes for float storage, or `width * height * 4`
bytes for RGBA8. An exact float consumer explicitly expands byte storage to
normalized samples; the image retains no additional materialized copy. Cache keys use payload identity; selective
attribute keys, geometry deltas and instance materialization apply to geometry.
`Payload.geometry` and `Payload.image` return `E_PAYLOAD` for the other variant.
Existing SOPs use the geometry adapter and refuse image inputs at that boundary.

`image/noise :width :height :frequency :seed` in an `image` graph cooks seeded
grayscale noise with opaque alpha on the shared worker pool. Width and height
default to 256, frequency to 0.02 and seed to 0. Domain count does not enter its
cache key: one and eight domains produce identical samples.

`image/map (fn [uv] [uv.x uv.y 0.5 1]) :width :height` defaults to 256×256.
The Flow_sop bridge prepares one immutable packed pixel-center UV grid and
compiles the actual function and captures. A session cook executes the packed
map and converts finite Vec4 channels to an owned RGBA8 image with the shared
pool. Conversion clamps to [0,1], scales by 255 and rounds ties to even.
Existing float-backed noise and sampling keep their original precision.
Workspace display may independently execute a qualified pixel function on the
GPU. SOP payload resolution still cooks this CPU node, never a resident GPU
readback, and preserves older immutable payloads across display updates and
resize. No GPU handle enters a procedural payload or session cache.

An explicit bridge converts geometry to `Rays.Mesh.t`, which `Scene.D3` lowers
through the native OGPU/Metal path.
A cook context may copy scalar facts from `Rays.Frame.t`, but
it must not retain a canvas, renderer, texture, or backend handle.

Use materialized `sop/duplicate` or Copy to Points when downstream SOPs need
per-copy topology.

## Node-owned parameter templates and inspectors

`Param` separates an operator's immutable parameter template
from its current record value. This follows the same architectural distinction
as Houdini's `ParmTemplate` versus `Parm`: metadata describes type, internal
name, label, folder, default, and UI policy, while the user's immutable record
stores current values. Numeric templates have a soft drag range and optional
hard bounds. Soft bounds never truncate typed, programmatic, initial, or
persisted values; hard bounds normalize every write.

`Node.parameterize` couples one schema, one immutable record, and the pure
rebuild function to the concrete SOP node that owns them. Sessions hash the
schema cook key plus any intrinsic operator identity, such as a snapshot's
data id. Schema text used for inspection is excluded from that intrinsic key;
custom wrappers retain their wrapped operator's identity. `Node.parameter_fields`
is the narrow type-erasure boundary used by inspectors. `Edit_graph.apply_parameters`
edits a selected node in the immutable document; compiling it preserves logical
node IDs and shared DAG identity. A `Cook` field
replaces that node's cook closure and cache-key parameters; `View` and `Export`
fields update metadata without invalidating cooked geometry. `Node.parameterize`
also records `Param.cook_key` of the values as `Node.parameter_key`, and the
session cache identity includes it next to the operator's own parameter string,
so every cook-impact schema field changes the key even when an operator's
hand-written key omits it.

`Graph.inspect` is an uncached traversal. Repeated inspection can use the
bounded `Session.inspect` cache, whose lifetime follows the owning session.
The sketch cook scheduler retains one graph's dependency summary in its
immutable schedule state, so an unchanged graph needs no traversal per frame.

A session remains single-caller. Independent input branches and concrete zone
elements may cook on the shared `Parallel` pool, with one coarse branch per
chunk. The automatic gate requires at least two uncached branches and multiple
domains; a branch qualifies when the measured own times of its uncached
subtree sum to at least 2 ms, or, when a timing has expired, a retained output
has at least 10,000 points. Shared or cached ancestors contribute no work.
A new session
without these measurements runs sequentially. Cache hits remain inline.

Shared ancestors are evaluated once on the caller in the original DFS prefix
order. Each worker owns a copy of cache metadata (including CLOCK touch bits),
volatile slots and the cook memo, while immutable payload planes are shared.
The caller validates and applies read/write journals in input order. If an
earlier branch evicted an entry observed by a later worker, that later branch
is reevaluated against the current parent. Parent cache mutation, diagnostics,
retention and counters therefore follow sequential execution. Opaque expansion
stays on the caller until its concrete roots are known. Cancellation discards
uncommitted branch outputs and reports errors in input order.

SOPs and sketches attach schemas to individual nodes. There is no sketch-wide
promoted record that shadows the graph; `Edit_graph` owns document edits.

The runtime API is PPX-independent. Templates may be written directly with
`Param.field` and `Param.schema`. The optional `rays.ppx` deriver
removes record boilerplate:

```ocaml
type controls = {
  divisions : int
    [@sop.default 16] [@sop.label "Divisions"]
    [@sop.folder "Geometry"] [@sop.min 1] [@sop.max 64]
    [@sop.hard_min 1];
  amplitude : float
    [@sop.default 0.25] [@sop.folder "Noise"]
    [@sop.min 0.] [@sop.max 1.];
  explosion : float
    [@sop.default 0.] [@sop.folder "View"]
    [@sop.min 0.] [@sop.max 2.] [@sop.impact "view"];
}
[@@deriving sop_params]

let grid_node input p =
  Custom.node ~label:"deform" ~operation:"custom-deform"
    ~schema:controls_schema ~values:p [input]
    (fun ~label ~inputs ~parameters ->
      Custom.plain ~label ~parameters:(Param.key controls_schema parameters)
        ~operation:"custom-deform" inputs
        (fun ~context:_ geometries ->
          (* Call typed RDK operations here. *)
          Ok geometries.(0)))
```

The deriver emits `controls_default` and `controls_schema`. Every record field
requires `[@sop.default ...]`. Primitive `bool`, `int`, `float`, and `string`
kinds are inferred; numeric fields require soft `[@sop.min]`/`[@sop.max]`.
`[@sop.hard_min]` and `[@sop.hard_max]` opt into strict normalization.
`[@sop.label]`, `[@sop.name]`, `[@sop.description]`, slash-separated
`[@sop.folder]`, and `[@sop.impact "cook"|"view"|"export"]` provide layout and
behavior. `[@sop.kind expression]` supplies arbitrary typed choices, and
`[@sop.ignore]` retains a record field/default without promoting it.
`[@sop.primary]` marks default card rows; checked `[@sop.vec3 "center"]`
groups three consecutive float components into one vector row and port.

Schemas live in the dependency-free `param` library. `Pxui_shell.Inspector`
converts a node.s folders to nested accordions and kinds to native widgets; the
host applies the returned changes through
`Node.apply_parameters`, synchronizes hard-bound normalization, and returns
accumulated effects. The host applies that edit to `Edit_graph`.
Sop never imports PXUI, and PXUI never gains SOP knowledge. A schema
builds an O(1) field-name lookup once; ordinary frames with no relevant change
do no parameter synchronization or graph work.
Persisted parameter state belongs to the graph/node layer. The selected-node
inspector is regenerated from concrete node metadata and never imports an
independent canvas-wide shadow record.

A SOP node has one declaration: either an OCaml `parameters` record with
`[@@deriving sop_params, sop_node]`, or a Lisp `defnode` in
`lib/flow_sop/nodes/<key>.rays`. Both declarations supply the inspector schema,
Lisp manifest entry and editor factory. There is no typed OCaml constructor (the
former `Sop.Sop` facade is gone): Lisp (`sop/<key>`) and
`Edit_graph.instantiate` build nodes, and tests write graphs as Lisp text.
There is no separate function name, argument list or typed default override.
Defaults, validation and cache identity therefore
cannot drift between the API and the editor. OCaml declarations live in
`lib/sop` (`groups.ml`, `sop_topology.ml`, `attributes.ml`,
`shapes.ml`) next to their cooks; `Sop.Nodes` exports their
factories. Parameter-free nodes declare `type parameters = unit`;
the same derivation supplies an empty schema. Null and Compact Points use this
path, with no artificial fields.
An input-dependent inspector can supply `parameters_build ~schema`, deriving
presentation from the constructed node while retaining the record's fields and
defaults. The builder evaluates the operator once and refreshes that schema on
edits and rewiring. Labeled index choices retain integer values and port types.
Group Random and Noise Displace expose their existing `context_seed` switch:
both default to explicit seed 0, while `context_seed:true` selects the cook
context independently of whether a seed argument was supplied. Point Jitter's
vector argument is `axis`; Point Velocity uses `set` and `add`; UV Sphere uses
one `radius` vector alongside its three radius-mode choices, matching Lisp.
PolyWire and Sweep Circle share one parameter record and cook, exposed as
`sop/polywire` and `sop/sweep_circle` with distinct runtime identities. Their
U/V range toggles preserve the kernel's meaningful omitted-range path; the
Lisp defaults keep both ranges explicit. The typed APIs flatten endpoint and
UV ranges and use the same defaults and validation as their factories.
The ten recorded default drifts are settled; existing
OCaml callers explicitly pass their former defaults. Box defaults to quads;
Tube defaults to triangles with end caps. Box's normals choice includes Auto
(`~normals:None`), which lets the kernel decide; the default remains Vertex.
Bound exposes separate box/sphere resolution and grouped lower/upper padding;
its numeric and output-name checks run before construction. Match Size exposes
independent move/source/target selection owner and name fields, separate axis
toggles and a positional optional target. Target justification defaults to
Explicit zero; Auto inherits the source justification as the kernel does.
Origin Point Generate exposes its generated group and source metadata names.
Rewire Vertices defaults to deleting the target attribute, as in Lisp, and
exposes selection owner/name fields. Distance Along Geometry and Distance from
Target expose owner/name selections and a radius mode plus fixed radius.
Distance from Geometry uses the same fields for source/reference selections
and takes both nodes positionally.
Revolve derives optional divisions, origin and axis, with blank UV names
disabling output. Its resolution, axis, arc-span and cap-mode constraints
run at construction for both typed and factory paths.
Platonic exposes orientation choice and custom axis separately, defaulting to
Dodecahedron and Vertex normals as in Lisp. Edge Transport takes the First/Last/Group root
choice and defaults to Zero root value; constant integration or edge-length
scaling requires Total mode for network, curve and parent transport alike.
Blank distance output names disable that plane; at least one distance or mask
output must remain. Their numeric, name and mode constraints run at construction.
Material uses optional `material`, `color`, `roughness`, and `emission` fields
with the Lisp defaults and rejects non-finite or out-of-range channels at
construction. The node's
cache identity is the schema's `Param.cook_key`, and `Node.parameters`
reads as `Param.cook_text` (`name=value;...`) unless the cook retains
legacy diagnostic text; that text does not replace the schema cache key.
Checks involving several fields or encoded values use
`[@@sop.validate fun parameters -> ...]` on the record: a pure check returning
`(unit, string) result`. The generated builder runs it before constructing the
operator and returns its `Error`; a factory `build` is `Node.t option list ->
(Node.t, string) result`, so no caller catches `Invalid_argument` or `Failure`.
An operator that refuses by wiring (a missing optional input) is marked
`[@@sop.node_fallible]` and returns a result too. Invalid editor writes return an error and retain the
existing node. Extract Centroid, Ordered Group and Group by Normal use this
for reserved names, negative indices and non-zero directions respectively.
Delete Attributes takes its input and optional reference positionally. Curves
and Parent edge transport default the root value to Zero, as in Lisp; callers
that need the former OCaml default pass `Transport_root_hold` explicitly.
Every catalog node uses one of these declarations; custom cooks stay
hand-written through `Sop.Custom`. A node writes no parameter text of its own: its inspection text and
cache key are derived from the record. Switch keeps its `a` and `b` ports and
takes further branches through the optional repeated `inputs` port.

`rays.sop_catalog` registers OCaml-bodied nodes and declares none. A parameter type
uses `[@@deriving sop_params, sop_node]` together with stable key, runtime
operation identity, label, category-path, and input-arity attributes. The
operation defaults to the key; `sop.node_operation` records deliberate aliases.
`sop_node` also generates `parameters_build`: given an operator
`fun ~label parameters input0 ... -> node` (one `Node.t` per required slot, one
`Node.t option` per optional slot), it returns the recursive, arity-checked
`build ~label ~inputs parameters` that attaches the schema through
`Node.parameterize` and rebuilds itself after edits. The generated
`parameters_factory build`
binds the same defaults and rebuild function used by inspection. A catalog
module is therefore the parameter record, one `parameters_build` operator, and
`let factory = parameters_factory build`; only nodes with external callers keep
an ergonomic hand-written `create` in `sop_catalog.mli`. Marking the
containing module `[@@sop.register]` contributes its local factory to a
deterministic source-order `Sop_catalog.Editor.factories` manifest generated by
the PPX. There is no mutable global initializer and no hand-maintained second
list that can omit an implemented catalog node. Constructors delegate geometry
work to ordinary Sop/RDK operators; controls are never synthesized from
serialized cache-key strings.

For a Lisp body, the `defnode` header is the declaration. It retains the same
scalar fields, literal defaults, separate soft/hard ranges, labels, descriptions,
folders, units, primary flags, impact and vec3 presentation groups as its OCaml
record. Encoded lists and ramps use text recipes with explicit validation; a
text kind alone does not supply an encoded decoder. Typed slots retain their
names, order and required/optional/rest status. Validation predicates run with
the complete parameter scope before node construction; refused editor writes
keep the existing node.

```lisp
(defnode sop/shift :label "Shift" :category ["Deform"]
  :facts {:elementwise points :reads ["P"] :writes ["P"]
          :topology preserved :exact true}
  [(geo : geometry)
   (distance : float 0.1 :label "Distance" :min -10 :max 10 :primary true)]
  (sop/with_attr geo :P
    (exact (map (fn [p] (+ p [distance 0.0 0.0])) (sop/attr geo :P)))))
```

The Dune source rule embeds the node files through the existing
`lib/flow/embed.exe`, sorted by filename. `Flow_sop.Catalog.check_workspace`
puts these definitions in scope without changing authored form IDs. Catalog
calls keep their `sop/<key>` identity and project to one graph card; the body is
internal to that card. Lowering exposes the resulting factories alongside the
native factories for node menus and inspection. The generated manifest records
`(body lisp)` and `(calls ...)`; calls come from checked terms, including
specialized helper functions and macros, rather than a textual name scan.
Registration rejects a Lisp kind that collides with an OCaml kind.
The header label supplies menu and inspector text; the initial cook label is the
stable kind key, matching native factories and their context-seed namespaces.
`Node.relabel` retains its existing display-only cook identity; a subsequent
parameter rebuild follows the native label protocol.
Embedded definitions stay in the checked definition tables and outside authored
workspace source. Saving after a scrub or structural gesture writes the user's
calls without copying the library into the file. Document gestures use the same
library-aware checker as loading; editable navigation includes only authored
definitions. Kernel identity includes checked library bodies, so editing a body
invalidates the cook even when the authored call is unchanged.
For nodes without supplied function slots, the factory source key contains
its own declaration and transitively referenced declarations, helpers and macros,
including references in defaults, validation and normalizers. Call-site parameters
and cooked inputs have their own keys. Editing a call therefore preserves an
unchanged sibling's cache; unrelated helper or macro edits also preserve hits.
Changing a referenced body, default, validator, normalizer or nested macro
invalidates its consumers. Supplied
function slots retain the whole-source key until their closure identities can
be narrowed without losing capture or body invalidation.

A SOP graph is one program. A call of a Lisp node evaluates its body in
the calling program (`Flow.Eval`, the host's `inline_parameters`), so the region passes see
across cards; only the native OCaml SOPs are node boundaries. The call keeps the node's
boundaries: its arguments take the node path (schema changes over the defaults, hard ranges,
choices, encoded text through the normalizers, `sop/material` values as their fields), its
validations refuse the lowering as a node build did, an error in its body is that card's cook
error (the body's nodes are dropped and the card lowers as a node that returns it), and each
body has its own step budget. Nodes the body makes sit under the card's `node:kind` site and
compile strictly; their refusals cook as errors. There is one model: no Lisp node body is
lowered as a workspace of its own.

Each card is a marker plan node (`Flow.Eval.card_kind`) at the call's site carrying the body's
value, the kind, the canonical parameters and the call's function arguments (an iso surface's
`:field`, which the field probes sample); the call's value is the marker. A supplied function is a
value of the calling program, so its captures (geometry included) are ordinary inputs there. It lowers to a
pass-through node (`flow.card`, labelled with the kind key), or, when the body's geometry is
decided by cooked data, to that body's structural node; either carries the card's schema, so
the inspector, handles, display, probes, provenance and compiled ids address the card. An edit
of a card is an edit of its text. The kernels of a body key on the card (its declaration cone
and canonical parameters), not on the whole authored source, so an edit elsewhere keeps them
cached; a card with a live argument keys on the source. A network is unchanged for the editor
only when every node's key is.

Writes fuse across cards (`Lower.fold_writes`, outside the reference lowering): a point write
whose geometry is, through card markers and literal deletions of other attributes, a point write
that nothing else reads writes over that write's input instead. Its reads of an attribute the
chain writes become the values written, its other attribute reads and counts of the chain read the
input, the chain's earlier writes run first in the same node (one record program over every
written attribute, `$writes`) and its deletions after them (they commute with writes of other
attributes). The earlier writes are not cooked, so a chain of point deformers is one region: four
`noise_displace` cards over a cooked million-point grid cook in 0.13-0.17 s against 0.24 s
unfolded, and `color_by_height > noise_displace > transform` writes Cd and P in one node, with the
unfolded bytes. A count of a geometry that only writes or deletes attributes is its input's count,
so a reader of counts keeps no chain alive. The fused node keys on every card it folds (`$chain`)
and its copied residuals take deterministic ids, so an edit elsewhere keeps it cached (edit to
recook 0.018 s, as nested). The intermediate cards are unchanged and cook as themselves when
displayed or probed. Selected and guarded writes and chains through a structural card (a body
whose geometry follows cooked data) do not fold. A lazy term that reads the chain itself (a map
over `(sop/attr in0 :P)` written in the map, not bound by `let*`) folds as a bound read would: the
copy's term reads a written attribute through a capture of the values written (`Host.rewrite
~retarget`) and its geometry capture is the input; any other use of that capture (a field, a
rebinding, a call) refuses the fold.

A body stays one program when its structure is decided statically: a cooked test that only
guards a write is `sop/with_attr :when` (decided as the write cooks), `and`/`or` short-circuit on
a decided operand, and a cooked count that only skips a write that would keep every value is not
tested (`sop/transform` with no group, or a blank one, is a pass-through card over its point
writes, no longer structural). A structural card's host calls of `sop/delete_attr` cook directly,
and its other operator calls without a function argument lower without the library, so a call
no longer requalifies every definition: sop_gallery cooks in 0.92 s inline against 1.40 s nested
(was 2.30 against 2.18), boolean in 3.1 ms against 11.4 ms.

A `sop/has_attr` its producers decide is answered statically (`Flow.Eval.static_source`,
`Lower.static_attributes`): walking back through card markers and group writes, a literal write
of that owner and name answers it (with its type), a literal deletion of it answers false, a mesh
holds only point P; any other producer, a write with a `:when` still to cook, or a chain of more
than 64 steps leaves it to the cook. An inlined body's evaluation keys on those facts of its inputs
(`Flow.Eval.static_facts`), so a replay answers as its producer would. Inside a card's body an
`error_rows` whose mask is parameter data (policy `first`) is decided when the body evaluates, its
failure the card's error, so a grid stays a pass-through card. `attribute_noise` over a grid is then
a pass-through write and `grid > attribute_noise > transform` cooks noise, P and N in one node: 0.20
s against 0.28 s (1M points, 2026-10-28); sop_gallery cooks in 0.88 s against 1.04 s, lowers in
0.41 against 0.48 s and recooks an edit in 0.50 against 0.54 s. The grid's planes are then made
while lowering (0.17 s for 1M points), not while cooking. A write of planes (`{:x .. :y ..}`) does
not fold: a read of the attribute could not take it as its values. Ext-port goldens are unchanged,
since a port decides nothing.

**Device placement.** A fused write (a node with earlier writes, `$writes`) may cook on the GPU
(`Flow_ir.Executor.record_on_device`): when every field of its record program is an elementwise
region, the fields merge into one region (`Core_fuse.merge`, one output each), fused through the
`(exact x)` boundaries inside it (`Core_fuse.fuse ~through_exact`: the sink reads the whole region
back), so the chain is one dispatch, one upload of its sources and no readback: its outputs stay
in device buffers until a CPU reader asks (below). Its
count must match by origin: a fill of one value per point (`array/int (sop/count g :points) v`), a
map over arrays that share an origin and a card marker's input all count the points of the
geometry they come from (`Attribute_kernel.prepare`'s count source). Placement: the workspace
precision is fast (the editor lowers with the precision its settings read, `Lowering.of_workspace`;
exact work stays on the CPU, bit for bit), the backend is present, the region passes the GPU form
checks, and `Flow_ir.Cost.sop_device`: at least 2^18 elements and noise in its code (Qualification
places every legal region).

Data movement (`Flow_gpu.Run` over mapped buffers, specification/backend.md): sources are packed
straight into the kernel's mapped input buffers (one float32 rounding, finite check folded in).
Outputs are resident attributes: `Gpu.kernel.planes` hands each output's buffer to the write while
the dispatch is still in flight (`Run.keep`; the runner takes a recycled or fresh buffer for its next
dispatch) as a `Gpu.planes` whose `settle` waits for the dispatch and reads its status (initial
domain) and whose `materialise` reads it once as owned planes (an invalidate and a parallel-block
conversion, from any domain once settled, no queue work). Settling is the session's: the write
registers its `settle` with the cook context (`Sop.Context.defer`), and `Sop.Session` settles before
it cooks any node (a parallel branch only what its own domain registered, the shared prefix before
the branches fan out) and at the end of every cook, so no CPU reader sees an unsettled output and
the wait overlaps whatever the cook does after the dispatch. A status that fails there refuses the
device to that write (`Sop.Context.device_refused`): what the cook attempt wrote is dropped and the
graph cooks again, the write on the CPU, which reports its own errors (`test_settle`, a mock device
whose status fails, cooks the CPU's bytes). The
write installs it as deferred storage: `Rdk.Packed.Float3.deferred` for canonical P,
`Rdk.Attribute.create_deferred` for any other numeric attribute. Both types are abstract, so every
reader (`Float3.get`/`View.view`, `Attribute.storage`/`View.storage`/`get`) materialises on first
use through `Rdk.Packed.Deferred` (once, any domain, the thunk and its buffer dropped after), while
length, kind, identity and payload size never do. rdk stays GPU-free: it holds only a thunk and a
release (`Deferred.create ?release`, `Geometry.releasers`). A recook whose result is only held
(cached, compared by identity, counted) reads nothing back; the first CPU reader of P reads P alone.
When the session drops a result (an LRU or payload eviction, a volatile slot replaced), the
resident components no retained entry shares are released at the end of the first cook whose root
output, and the previous cook's, do not hold them (`release ~held`). A component some cook's root
held was handed out, so it is retired, never invalidated: it stays readable from any domain, and a
reader of a stale result (spreadsheet, pick, crash dump) never fails. A component no root ever held
(an intermediate write the cache evicted) is freed: its buffer is reused at once (`Host.stats.freed`;
`test_chain_gpu_native` drops the first of two consecutive writes and fails unless its P and N are
freed without a read). Retired buffers return to the host's pool (`Run.pool`, shared by every runner,
so a new runner after an edit reuses buffers by size class: any 8 free, more within 256 MiB) when
they are read or collected. Each holder carries a custom block that tells the GC its buffer's size
(`flow_gpu_stubs.c`, one major cycle per 64 MiB, as a Bigarray's data), so a dead result is
collected within a few cooks and its buffer comes back without a read; past 2 GiB of retired outputs
still unread (all alive), the oldest is read into owned planes (same values) and its buffer reused,
which bounds device memory. The pool never `Weak.get`s a retired holder it does not read: a get
while the GC marks keeps the holder alive another cycle, which had kept every dead result alive.
Measured (`bench_chain_recook.exe`, 1M points, one session toggling an edit, 32 warm and 32
measured cooks, medians of three interleaved rounds, loaded M1, 2026-10-28): results that die, 1.5-3.2
ms against dev's 6.0-12.1 ms (and 42-69 ms for the earlier read-the-oldest pool); the last 16 results
kept alive, 2.0-2.6 ms against 8.9-11.0 ms (39-63 ms), with no output read back in either.
A clear or a close leaves results to their holders' collection; closing
the runner first materialises every holder still alive, so a geometry outliving the GPU stays valid. `test_chain_gpu_native` fails if a recook reads back,
if reading P materialises more than one output or twice, or if a buffer outlives its geometry.
A later fused write reads a resident attribute where it is (GPU to GPU): the deferred storage
carries an rdk holder (`Rdk.Packed.Deferred.holder`, an extensible tag rdk never inspects) naming
the output's placeholder (`Gpu.planes.resident`, one element), `Attribute_kernel.resident_read`
answers a whole-attribute `sop/attr` of unread resident storage with it, and
`Packed.Host.prepare_values ?resident` evaluates each source with a resolve that answers
placeholders, keeping one only when the source is exactly that placeholder (counted as its count);
a source that computes over a placeholder is evaluated again with the cook's resolve, so a
placeholder never reaches a CPU kernel, and the CPU fallback never sees one (it resolves afresh).
`Run.dispatch` binds the placeholder to the held buffer. `sop/has_attr` and `sop/attr_type` read
the kind without materialising (`Attribute_metadata.kind`): a transform's `:when` on N used to read
N back. `test_chain_gpu_native` cooks two consecutive fused writes (the first is read twice, so
they do not fold into one) and fails if their recook uploads, reads back or materialises anything,
or if reading the second's P materialises more than one output. 1M points: 0.014-0.019 s for both
(0.007 each alone, loaded M1).
An input is resident when it is the same array as the last dispatch's, never by comparing floats:
sources are immutable, and a recook keeps them the same arrays because the facts they come from are
remembered by identity (`Flow.Value.remember`, weak): a static grid's point planes, an attribute's
interleaved or checked read, the copies a write adopts, a large array's finite check; a large float
fill is remembered by its value (`Flow.Op.constant_array`, 8 slots); and a region that only
forwards its one source (a branch chosen by constants) is that source
(`Packed.Host.prepare_values`). `test_chain_gpu_native` fails if a recook uploads an input. Measured
(M1, loaded machine, load 5-11, 2026-10-28, `test_chain_gpu_native.exe`, medians of 5): `grid
1000x1000 > attribute_noise > transform` recooks in 0.007-0.010 s with the device and nothing read
back (eager readback before: 0.009-0.027 s, of which 7-10 ms converted all seven planes), 0.005 s
when the previous results have been collected and their buffers recycled, against 0.11-0.15 s on
the CPU; about 2 ms of a recook is the kernel itself, waited for its status check, and 3 ms a fresh
28 MB of output buffers while the previous geometries are still uncollected. First cook
0.043-0.050 s (broken down below). One dispatch, largest difference 3.4e-7. A light map (transform alone) costs
about its transfer on the CPU blocks and stays there. Any device failure (a form refusal, a status)
leaves the record to the CPU, which reports its own errors.

With the settle point and deterministic return (2026-10-28, same machine, load 4-6): one session
toggling an edit (two lowerings of the chain, translations 1 and 2, `max_entries` 2: the grid hits
and each cook evicts the other write) recooks 1M points in 1.1-1.3 ms median with no buffer
created, nothing uploaded or read back and no more buffers held after 16 recooks, with no
collection (`test_chain_gpu_native` fails otherwise); of the 3.6 ms it took before, 2.3 ms was the
transform's write of N re-scanning its unchanged planes for finiteness and copying them again (a
write adopting the same planes now reuses their copies and their check). A session per cook (every
node recooks) takes 0.004-0.006 s, two consecutive fused writes 0.008-0.015 s. CPU: 0.11-0.13 s.
`sop_gallery` (interleaved with a dev build, `BENCH_PHASES=1`): cook 0.86-0.89 s against 0.84-0.95
s, edit to cook 0.47-0.53 s against 0.51-0.61 s, lower 0.38-0.42 s against 0.38-0.45 s.

First cook, measured (M1, 2026-10-28, `test_chain_gpu_native`, 1M points): of 0.043-0.050 s,
about 26 ms is acquiring the GPU lease lazily on the first prepare (`Workspace_gpu.owner`: a device
and a queue headless; in the editor the window's device is shared and only a queue is made), 4 ms
emitting the MSL, 2.4 ms compiling it (warm: the OS Metal shader cache already keys compiled
sources persistently; a cold, never-seen source takes 60 ms) and 7 ms the first dispatch's fresh
buffers and uploads. A Metal binary archive would only save part of the 2.4 ms, so none is kept; a
Vulkan backend will need its own `VkPipelineCache`, which has no OS cache behind it. The editor does
not pay the 26 ms: `Renderer.acquire_gpu` with the window's device makes only a queue (0.1 ms
measured against 26-29 ms for a headless device and queue), so its first fused cook of 1M points is
about 18-19 ms. Emitting the MSL is 3-5 ms per prepared program and is repeated only when a
re-lowering makes a new program (an edit relowers the workspace); a memo would need a key of
everything emission reads (the region's code, shapes, source types, origin), and a marshalled
region is 90-110 ms and unstable, so none is kept: 3-5 ms per GPU write per edit beside a 0.4-0.5 s
relowering.

Not yet, in order of value: (1) runners shared by equal programs: an edit relowers the workspace, so
each fused write gets a new program, a new runner (fresh buffers, every input uploaded once) and
its emission; deterministic return only recycles within a runner, so a host-wide pool of released
buffers by capacity is the next step; (2) extending the display binding (below) to lit triangles
without N (normals made from P on the device: the host's are float64 sums, so the pixels would
differ) and to colours or uv a device holds; (3)
grid planes and `array/range` generated from the thread
index (a new element-index instruction across the CPU blocks, MSL, SPIR-V and WGSL, and the grid's
point map fused into the write); fills bound as uniforms.

**Display binding.** A displayed point cloud whose P (and N, if it has one) the device still holds,
or a triangle mesh (every primitive a triangle) whose P and point N it holds, with no colour, uv,
vertex N, piece or material to show, is drawn from the device: `Rdk_rays.Surface`
keeps it as a device preview on the cook worker (nothing read), and `Surface.scene3` binds it on
the initial domain through the editor's `Flow_native.Workspace_gpu.display`
(`Attribute_kernel.resident_vec3` finds the placeholders, `Flow_gpu.Run.pin` borrows the held
buffers, `Renderer.register_gpu_positions` registers them, `Rays.Mesh.View.create_device` is the
mesh, with the topology's corners as its indices; specification/backend.md). The mesh keeps the
borrow: once collected, the registration and pins are released. A device mesh also carries how to
make its host copy (`create_device ?host`: `Rays_mesh.to_mesh`, reading P back, once): a reader
of vertices (the path tracer, a centroid) gets that copy, the raster lowering
reads only the mesh's own arrays (`Mesh.View.unread`). Its indices are packed once per topology (8
kept by the corners array's identity) and the renderer keeps them under their own key
(`Gpu_vertices "borrow\nindices"`), so a frame's new borrow neither repacks, rehashes nor reuploads
them. The World sun's depth pass reads a device mesh's positions as the colour pass does, and the
World-lit colour pass (`Scene3_world`) binds them as the plain one (it had refused a device mesh's
borrow as a malformed layout, so a World-lit view of a resident mesh failed to draw).
`test_chain_frames_native` (`CONNECTIVITY Triangles`) draws the resident lit mesh under a World
sun: the sun's fit reads the device bounds (at most 24 KB), nothing is materialised (the host copy,
which reads P, is never made) and the pixels equal the host display's under the same World.

A display is released when the editor stops drawing it: each frame `Editor_loop` calls
`Workspace_gpu.frame`, with the device keys its drawn scenes name (`Loop_model.display_keys`) when
they changed, and a binding left out (its node removed, its mesh recooked) or whose mesh was
collected is unregistered and unpinned two frames later (one presented frame stays in flight), so
its buffers return to the pool. It had waited for its mesh's collection (a one-word token, no GC
pressure) and then three more bound displays: removing the last displayed node kept its buffers
pinned until close, and a dropped one could be freed while the frame that drew it was in flight.
`test_chain_frames_native` removes the node and fails unless every binding is released two frames
later without a collection and, once its results die, the pool holds no buffer (16 stayed held
without the release).
Anything else (lit triangles without N, quads, a host device, a closed runner) builds the host mesh
as before, reading P. The cooked bounds a piece keeps are read when asked (framing, the
selected box), not per cook, and never read P while the device holds it (below). Measured (`test_chain_frames_native`, M1, 2026-10-29): every frame of
the animated 1M chain displays with nothing read back and the host display's pixels exactly,
preparing and drawing a 256 x 256 canvas in 0.010 s against 0.142 s reading P back (0.0031 s
against 0.025 s for 512 squared). A lit triangle grid (`CONNECTIVITY Triangles`, N transformed on
the device) the same way, pixels equal.

**Device bounds.** The box of a P the device holds (the selected object's box, framing, the World
sun fit) is reduced where P is: `lib/flow_gpu/shaders/bounds.rays`, a compute stage beside
`compact.rays` printed by `test_raster.exe --write lib` (MSL, SPIR-V), runs one group of 1,024
lanes, lane l taking the min and max of elements l, l + 1,024, ... (neighbouring lanes read
neighbouring elements) and storing six floats at l x 6; the host reads back those 24 KB (not 12 MB
of P) and finishes over the lanes. `Gpu.planes.bounds` offers it for a settled, unread vec3 output
(`Run` on the owner's domain; `Workspace_gpu` routes a cook worker's call there), the write passes
it to `Rdk.Packed.Float3.deferred ?bounds`, and `Float3.bounds` answers from it while P is unread
(otherwise, or when the device cannot, it scans the planes), so `Geometry_view.bounds` never
materialises P. A device mesh takes the same answer (`create_device ?bounds`, `Mesh.bounds`), which
the sun fit reads instead of the host copy; its caster scan had read a device mesh's mode through
`Mesh.View.view`, which made the host copy every frame a World sun lit one. min and max of float32
values are exact, so the box equals the host scan's bit for bit (`test_chain_frames_native` fails
otherwise, or if the bounds read more than 24 KB or materialise anything; `test_settle` checks the
mock device's answer leaves P resident). Measured (`bench_chain_frames.exe`, 1M points, M1,
2026-10-29, frames 1-7): the bounds of a frame's resident P took 31-59 ms reading P back, now
1.0-1.3 ms (2.5-2.8 ms with each lane on a contiguous chunk, uncoalesced).

Where a device display's frame went (1M points, `test_chain_frames_native`, M1, 2026-10-29): of
the 10.8 ms a cloud took, 6 ms was the host side of the mesh, not the renderer: a fresh identity
index array and its bounds check every frame (a device mesh now takes the last frame's: the same
cloud identity, the same topology's corners, made and checked once), and for a triangle mesh also a
scan of every primitive for `all_triangles` (kept for the last topology); the renderer's per-frame
upload and MD5 of the indices under a new borrow key was the rest of the waste (now kept under the
indices' own key). Now a frame prepares and draws 1M points in 0.0037 s (0.0013 s for 512 squared)
and a 1M-point lit triangle grid in 0.0027 s (0.0011 s), against 0.08-0.14 s reading P back; what
is left is the draw and its completion wait.

A body reads its cook context through reserved inputs (`Context_kernel.label_context`): each
distinct `sop/context_seed`, `context_frame` or `context_time` call, labelled with the card's
cook label (its kind key; helpers read it from the evaluation), is a cook-time source that
residuals capture as a binding. A kernel that captures one declares the Seed, Frame or Time
dependency and is built per cook context from its specialized data (`Lower.per_context`), so a
noise seed stays a region constant and seeds equal the specialized node's. `sop/provenance_base`
is a sentinel the lowering of the body's `sop/concat` replaces by the base it allocates. A
per-frame argument is passed to the body as it is (a drive inside it, a frame uniform) when its
field has nothing to canonicalize, and so is a cooked-data argument (`(sop/count g :points)`), a
source the cook resolves once. A live vec3 group (`:offset [(get (frame/input) :t) 0 0]`) is its
three components, each such an input. A validation that reads a per-frame argument, and an
`if`/`cond` test in the body that reads only frame facts and picks an arm that fails statically
(an `error`, a helper that refuses), are deferred checks (`Flow.Eval.card_kind`'s `guards`): the
body goes on as if the check passed, and the card cooks them with its cook's input, failing with
the diagnostic the written-out call refuses or fails with (a refusal is `E_LOWER`, at cook rather
than lowering). A write folded through a card with checks takes them (`$guards`), so they still
run when the card itself is not cooked. A body whose structure follows a per-frame argument
otherwise, or whose live argument a strict kernel cannot take as data (a noise seed, a region
constant; `Flow.Eval.live_kernels` asks the host), keeps the card a node of its own,
rebuilt per frame.

A noise's lacunarity and gain are registers of its `Noise3` (`Core.Region.live`: `Fixed` a
constant, `Live` a float register), so driven ones are uniforms of the one compiled kernel, as the
noise offset is: `attribute_noise :lacunarity (+ 1.5 t) :roughness (+ 0.25 (* t 0.5))` inlines and
its write stays one dispatch on the device, nothing compiled, uploaded or read back after the first
frame (`test_chain_frames_native` under `DRIVE`, largest difference from the CPU 3.0e-7 for 512
squared, 3.4e-7 for 1M). The device inlines `noise-fbm-fractal` (`lib/flow_gpu/noise.rays`) for any
other than 2 and 0.5, WGSL `wfbm_fractal` (`gpu_check` in Chrome: 1.6e-6 from the JS region); both
give NaN where `noise3` refuses the values, so the status hands the region to the CPU, which checks
each live pair as `noise3` does (`Flow.Op.check_noise_fractal`, `test_rand_at`: bytes and the
refusal equal the walker's). A constant pair prints and emits as before (2 and 0.5 keep `noise-fbm`
and `wfbm`, so no golden moved), and the GPU now places constant non-default pairs too (it refused
them). Measured (M1, load 3-4, 2026-10-29, medians of 7 frames): 1M points 0.0038 s a frame on the
device against 0.12 s on the CPU, where a driven lacunarity had cooked every frame (a node rebuilt
per frame, the GPU refusing any lacunarity but 2); 512 squared 0.0014 s against 0.039 s.
A live octave count is an Int register of the same `Noise3` (`Core.Region.live`: `Fixed` a
constant, `Live` a register), an Int uniform of the one compiled kernel: `attribute_noise :octaves
(int (+ 2.0 (* t 60.0)))` inlines (a live Int with hard bounds, `:octaves`' hard minimum 1, is
clamped in the program as the node's parameters clamp it, `Lower.inline_parameters`; a literal
out of range is still refused by the checker) and its write stays one dispatch. The device inlines
`noise-fbm-live` (`lib/flow_gpu/noise.rays`, compiled once, not per count): the fold's bound is the
register, `Loop_begin` with the static maximum every counted loop carries, and NaN outside the
device's 1 to 32, so the status hands the region to the CPU; WGSL `wfbm_live` takes the count as its
integer register pair (NaN outside 1 to 32), JS passes it to `noise3` as a BigInt. The CPU checks
each element's count as `noise3` does (`Flow.Op.check_noise_octaves`, E_NOISE_RANGE past 64): a live
count makes the region an integer program, so it runs on the typed path, not the block kernels. A
constant count prints and emits as before (the SPIR-V golden only gains `fast_noise_octaves`).
Checked by `test_rand_at` (bytes at counts 1, 3 and 32 and the refusal at 65 equal the walker's),
`test_js` (JS region exact and fast against native), `test_wgsl` and `gpu_fast_check`
(`noise_octaves` in Chrome, worst 7.0e-5 against the 1e-4 noise tolerance) and
`test_chain_frames_native` (`DRIVE` octaves 2 to 9 over the frames: after the first frame one
dispatch, nothing compiled, uploaded or read back, P resident). Measured (M1, load 4, 2026-10-29,
medians of 7 frames): 1M points 0.0044 s a frame on the device against 0.14 s on the CPU, largest
difference 2.7e-7; 512 squared 0.0020 s against 0.046 s (2.8e-7).

A body can pick a form for a live value: `(live? x)` is true while lowering when `x` is decided per
frame or by cooked data (a residual, a data source), false for a value at hand and wherever a
program runs (the evaluator answers it; `Core_lower` folds it to false). Both forms must compute the
same value. `sop/transform` uses it for a live matrix (shear, translate, scale, pivot driven): its
identity test is per point (`(if identity p (M p))`, a uniform), the normal matrix is the inverse
transpose or the identity when singular, each normal kept where the matrix is identity or singular,
and N is deleted by `sop/delete_attr :when` (live or cooked, decided as the delete cooks; a static
guard decides while lowering, as `sop/with_attr`'s) when singular and something is selected, so
the structure is fixed and the P and N writes fold with the chain. A static matrix keeps the
branches it had, so nothing static changes. A live record a kernel captures (the matrix) is one
uniform per field, each that field of the one residual (`Core_region.field_residual`), and a
kernel's uniforms are made once per dispatch from the residuals they share (`Packed.Host.prepare`:
the matrix and its inverse are evaluated once, not per field); a fold over a list in a strict
uniform (the inverse's pivots) is value work, made by reference. Kernel validation lowers each
residual of a shared DAG once (it lowered every binding's subtree again: 2-30 s for a 35-point
driven transform, 0.3 s now). Recomputed normals or a group the cook decides keep the card a node,
as before. `sop_gallery` (`bench_sop_cook ":seed 2026" ":seed 2027" 3`, five rounds interleaved
with dev, medians): check 0.283 s against 0.274, lower 0.395 against 0.403, cook 0.904 against
0.894, edit to cook 0.544 against 0.547. Bytes equal the written-out call's (`test_lisp_inline`: identity, singular with point or
vertex N, empty and cooked groups).

Measured (`test_chain_frames_native` with `XDRIVE`, M1, 2026-10-29): `grid > attribute_noise >
transform :translate [0 t 0] :shear_xy (* t 0.5)` (identity at frame 0) is one fused write: after
the first frame one dispatch, nothing compiled, uploaded or read back, P resident; 1M points 0.0086
s per frame (0.0071 s for 512 squared) against 0.151 s on the CPU (was a card relowered and cooked
on the CPU each frame), largest difference 2.6e-7. Of the 7 ms at 512 squared, about 1.6 ms is the
dispatch as the noise-driven chain's; the rest is the matrix's Lisp evaluation per frame (its
uniforms, the deferred validation and the two delete guards, each evaluating the matrix).
Profiled (`bench_chain_frames.exe` with `NODES=1`, per-node session timings, 512 squared): the
fused write took 4.9 ms, of which 3.7-4.0 ms was making its ten uniforms
(`Packed.Host.prepare`): the live matrix is a record whose rows are residuals, and only a
uniform's top-level residual bindings were made once, so every uniform that read the matrix (its
four rows, the identity test, the inverse) walked all four rows again. Rows inside a record or list
binding are made once now: 1.4-1.5 ms, and the frame 7.2 ms -> 4.4 ms (`test_chain_frames_native`
with `XDRIVE`: 0.0044 s for 512 squared, 0.0057 s for 1M, was 0.0071 and 0.0086). Left: each delete
guard walks the matrix and its inverse again (0.6 ms each; the two guards are one residual, but
separate nodes and walks), and about 0.8 ms of the first delete is waiting for the dispatch.
`sop_gallery` with this and the device bounds (`bench_sop_cook ":seed 2026" ":seed 2027" 3`, five
rounds interleaved with dev, medians, 2026-10-29): check 0.252 s against 0.252, lower 0.373 against
0.373, cook 0.846 against 0.840, edit to cook 0.501 against 0.503.
With live noise lacunarity/gain, World-lit device meshes and the frame release (same command, five
rounds interleaved with dev e89a2b9a, medians, load 3-4, 2026-10-29): check 0.251 s against 0.250,
lower 0.375 against 0.374, cook 0.843 against 0.875, edit to cook 0.506 against 0.521.

A transform whose group the cook decides (a native group node upstream) is a body cooked behind its
card: its normals' writes read every owner's N eagerly there, so a mesh without vertex N failed
(`vertex attribute N is missing`); the guard now decides first where it is at hand (`live?`).

Measured (`test_chain_frames_native`, M1, 2026-10-29): `grid > attribute_noise :offset [(get
(frame/input) :t) 0 0] > transform` stays one fused write: after the first frame each frame is one
dispatch with nothing compiled, uploaded or read back and P resident, 0.0028 s per frame for 1M
points (0.0015 s for 512 squared) against 0.122 s on the CPU (was about 0.1 s every frame, a card
relowered and cooked on the CPU), largest difference from the CPU 3.8e-7.

That node (and every factory node, for menus and inspection: `Lower.t.card_node` builds a card's,
with the card's id, parameters, functions and provenance base) owns one ordinary session cache
entry and schema-derived parameter identity. Its cook is one program of one call: a graph whose
inputs are the node's parameters (a vec3 group as one argument), slots and forwarded function
captures, and whose body calls the kind, which inlines there as any card does. That call staying a
node (it would cook the same lowering again) is the card's cook error. The graph cooks with the
existing `Session` cooker with zero retained entries and bytes: per-cook sharing, payload handling,
instance materialization, diagnostics and cancellation retain their native semantics. Its cook
derives read/write/topology facts from the inlined body, so mode and parameter changes rebuild the
relevant facts, and the context dependencies of delegated nodes belong to it. A card whose body
failed is a plan node with a typed `error` (`Flow.Eval.node.error`) that cooks as that diagnostic.

History: the nested model (each Lisp node body lowered as its own workspace) was measured against
inline before it was deleted (M-series, 2026-10-28, interleaved, `tools/bench_sop_cook.exe`):

| | inline | nested |
|---|---|---|
| `sop_gallery` cook | 0.93-1.08 s | 1.25-1.31 s |
| `sop_gallery` lower | 0.39 s | 2.0 s |
| edit to recook | 0.51 s | 0.75 s | SOP ownership remains in
`sop`; the Lisp declaration and lowering stay in `flow_sop`, and `flow` never
depends on geometry.

A default factory with an unconnected typed function retains its operation and
parameter schema for catalog and inspector use. Its facts are conservative and
cooking returns `disconnected_input`. Once the call supplies a checked function,
lowering derives the actual body facts and qualifies its packed kernel before
cooking.

Input metadata is a signature, not merely a port count. Fixed descriptors use
required slots; `[@@sop.node_optional "1,2"]` marks zero-based optional slots
and makes `sop_node` generate an `Edit_graph.factory_slots` descriptor.
`[@@sop.node_slots "input, target"]` names the slots. Generated factories
default to geometry inputs; `[@@sop.node_types "geometry, image"]` declares
mixed nominal inputs in the same order. The factory validates the names,
and the Flow catalog validates them against the type registry. An optional
type atom in a manifest slot preserves the existing untyped slot spelling
for geometry; the checker and graph rows use the declared type.
`sop/attr_from_image` uses this signature for geometry then image, with the
same declaration generating its schema and factory.
`[@@sop.node_types ["fn(vec3,float)->float"; "image"]]` also accepts a list
of serialized types, so commas inside function signatures stay unambiguous.
`[@@sop.node_keywords "field, picture"]` exposes those required physical
inputs as typed Lisp keywords. They
have no scalar Param field, default or drive. The manifest retains their physical
input order and keyword names; the catalog validates their Fn/Image types.
The Lisp declaration spells the same contract as
`(field : (fn [vec3] float) :keyword true)` or
`(picture : image :keyword true)`. Such slots are required and cannot repeat;
they retain their physical input order while appearing as keyword rows in Flow.
Function bodies and captures belong to the language host, which passes an
owned `Sop.Kernel` bulk runner through the ordinary payload boundary.
Sop never depends on Flow or Flow_ir.
`sop/iso_surface` is the first bulk field consumer: required `:field` is
`fn(vec3)->float`; `:resolution` defaults to `[64 64 64]` integral cell counts,
`:min`/`:max` to `[-2 -2 -2]`/`[2 2 2]`, `:iso` to zero and `:smooth` to true.
The cook builds `(rx+1)*(ry+1)*(rz+1)` packed XYZ positions, x fastest, with
the extractor's exact lattice arithmetic. The host compiles the field as a
static-count packed map and evaluates exact float64 samples; the shared
`Rdk.Iso_surface.extract_sampled` marcher borrows those samples read-only.
The node is irregular, topology-changing and exact. Constructor validation
rejects fractional/nonpositive resolution, overflowing packed cardinality,
nonfinite/unordered bounds and nonfinite iso; cook errors and cancellation
remain typed. Its generated keyword port and function zone use the ordinary
graph projection and checked argument gestures.
The exact CPU sampler writes a Point float attribute (`:attribute "image"`
by default) from a normalized Point Float2 or Float3 `:uv "uv"` attribute.
`:channel` accepts `"r"`, `"g"`, `"b"`, `"a"`, or `"luminance"` (Rec. 709
weights); red is the default. Sampling is bilinear, clamps both coordinates
to [0,1], and maps v=0 to the first image row. It preserves geometry/topology
and errors on missing/wrong-owner UV, invalid storage or nonfinite values.
It uses `Duplicate_input 0`, Point elementwise facts, reads the UV attribute
and writes the named attribute. The RDK kernel owns sampling and fills
disjoint point ranges through the shared Parallel pool.

Generated factories also carry field views, allowing the canvas to filter Tab results by
compatible value ports. Hand-built factories provide `~fields` to participate
in that typed search.
Disconnected optional references compile as absent operator arguments, while
connecting or disconnecting them rebuilds the physical SOP input list and
retains the document node's identity and parameter values. The generated
rebuild decodes the physical input list with the slot presence captured at
build time, so sparse optional connections keep their slot. Match Size exercises
this path with one required geometry input and one optional target input.

The node menu (`Pxui_graph.Node_menu`: leader `/ a`, `Tab` in the graph panel, or a
right-click on the empty canvas) is a search field over the kinds, the likeliest first.
Typing searches the whole catalog by stable key, display label, or category. Catalog tests require unique keys, instantiate every
registered factory with disconnected placeholders, and compare the constructed
`Node.operation` with the descriptor identity, so a descriptor that cannot
enter an editable graph or points at the wrong SOP fails the test suite. They
also perturb every cook-impact field of every registered node and require its
`Node.parameter_key` to change.
`Pxui_graph.Node_menu.entries_of_factories` is the single adapter from factories to
menu entries, and a reachability regression searches the node menu for every
generated stable key. `sop/blend_shapes`, `sop/attribute_composite`, `sop/skin` and
`sop/ordered_group` are registered as `sop/blend_shapes`, `sop/attribute_composite`,
`sop/skin` and `sop/ordered_group`; `sketches/ws_morph` uses all four.

Structured immutable parameters use `Param.encoded` when a SOP owns an
ordered rule list or nested value. The codec is typed at the descriptor,
validated on every inspector edit, encoded deterministically into node cache
identity, and currently exposed through the ordinary text editor. This keeps
the descriptor stable while a future PXUI multiparm adapter replaces the text
presentation with add/remove/reorderable rows; SOP wrappers never parse UI
state or serialized node cache keys.

Exploded View is a standard registered terminal SOP, not a sketch-local Null or
custom callback. It follows SideFX's public Exploded View piece-attribute and
outward-scale model while retaining Rays's explicit packed render boundary:
its view-impact controls update rigid piece transforms without recooking the
upstream Boolean topology. Geometry materialization remains an explicit later
boundary when a downstream SOP needs the transformed points.

`Sop.Custom.node` attaches a schema to an existing SOP composition without
recursive reconstruction boilerplate. `Custom.plain` builds a parameter-free
operator from immutable RDK inputs and a declared `Context`. Together they are
the typed OCaml extension boundary; `Param.key` supplies cache identity,
and parameter edits rebuild the cook closure while keeping the logical graph
ID. This is not a VEX interpreter; it is the native extension point for RDK
composition, with explicit dependency and cancellation contracts.

## Editable graph document and sketch environment

`Sop.Edit_graph` is an immutable network document distinct from a
compiled `Graph.t`. It retains every node, including disconnected nodes and
optional input slots, and validates add/delete/connect/disconnect/atomic-wire-
insert operations. Compilation rebuilds parameterized closures with their
current inputs, rejects cycles and disconnected paths, and produces an ordinary
immutable DAG for the existing deterministic Session cook path.
Dissolve reconnects every consumer through the selected chain's primary
inputs. Wire insertion fills the new node's primary slot and leaves any
additional slots disconnected.

The editor's document is workspace text (`Editor_document.Workspace_doc`), not a
hand-edited `Edit_graph`. `Flow_sop.Lower.workspace` lowers each evaluation of a `sop`
graph to one `Flow_sop.Network`: the `Edit_graph` plus the arguments that depend on `t`.
Before each visible-object cook submission, the editor's value
lane forces those arguments and applies the changed ones to a temporary graph without
changing stored literals. Time-dependent networks resolve on each advancing frame;
unchanged effective values retain their graph and cook keys. A `^:bypass` call makes no
node: the evaluator passes its first input through.

`rays.pxui_graph` presents the text as a graph (`Pxui_graph.Scope` over
`Flow_graph.Projection`): a left-to-right layout on a 24-point lattice, straight wires bent
clear of cards, explicit point/chip/card/full levels, zones for loops and functions.
Its gestures and keys are `specification/flow.md` §6 and §7. The canvas never mutates
the document or cooks geometry; it returns typed requests, and
`rays_editor` applies them as checked rewrites of the text, reports validation errors, and retains the prior
successful viewport result until a newly compiled graph finishes cooking.
This clean-room interaction contract follows SideFX's public descriptions
of the [Network Editor](https://www.sidefx.com/docs/houdini/ref/panes/network.html)
and [network navigation](https://www.sidefx.com/docs/houdini/network/navigate.html),
without copying implementation or assets.

`rays.rays_editor` composes a responsive workspace of panels (by default the
view/graph/inspector columns; a workspace's editor graph describes its own), selected-node inspector, `Easy_camera`
render controls, a sketch-owned playback clock, and one bounded latest-request
cook worker. Default column ratios are 45/35/20; gutters keep the sizes the text says
through resize and every panel can collapse. Empty selection puts camera/render
controls in the inspector, while node selection puts generated parameters there.
The previous successful image stays
visible while a new result cooks. Overlay scenes use view-local
coordinates, not full-window coordinates.
`Editor` is the one editor: `Viewport` supplies its camera widgets,
navigation, painting, view persistence and mode state.
Reachable `Context.Time`/`Context.Frame`
dependencies trigger external-effect recooks while static graphs remain cached.
Long dynamic cooks finish before the newest clock snapshot is submitted;
parameter edits remain urgent and supersede stale work. `/ p`, `x` and `r`
play or pause, stop, and reset the local clock; `/ g`, `i`, `c` and `h` control graph,
inspector, camera controls, and all UI.

The template/value split and soft-versus-strict behavior are based on the
public [Houdini ParmTemplate contract](https://www.sidefx.com/docs/houdini/hom/hou/ParmTemplate.html),
[Houdini numeric strict-range behavior](https://www.sidefx.com/docs/houdini/hom/hou/IntParmTemplate.html),
and [Blender's separate hard/soft property bounds](https://docs.blender.org/api/current/bpy.props.html#bpy.props.FloatProperty).
PPX attributes are declared
through PPXlib's typed attribute API so misspelled or malformed metadata fails
at the source location rather than being ignored.

## Nodes and cook contract

A graph is an immutable DAG. Shared OCaml values create shared subgraphs. Each
node has a stable ID, human-readable label, operation/version, immutable
parameters, ordered inputs, cook mode, and declared context dependencies.

Cook modes distinguish generators, input duplication, uniquely owned in-place
opportunities, instances, passthroughs, and generic operations. They describe
optimization opportunities; correctness never depends on a compiled/fused
execution path.

An operator receives a self-contained context, typed parameters, immutable
input snapshots, and operation-local scratch. It cannot reach back into a UI
node. Failures carry the node path and preserve the structured RDK cause.
Warnings such as removed degenerates, recomputed normals, and empty selections
are accumulated deterministically rather than printed from worker domains.

Ordinary graphs are acyclic. Feedback, solvers, and time-history state require
an explicit separate iteration contract; an accidental cycle is always an
error.

`sop/triangulate` and `sop/reverse` wrap the corresponding packed RDK kernels.
Triangulate accepts an optional named primitive group, passes unselected
polygon/curve primitives through exactly, and rejects a selected curve.
Reverse accepts an optional named
primitive group and either reverses winding or applies a signed cyclic corner
shift (`:operation "Shift vertices" :shift offset`, the two Lisp fields).
Missing groups become traced node diagnostics; group resolution,
validation, cancellation, normal policy, exact payload remapping, and
parallelism remain owned by RDK rather than being reimplemented by the graph
layer.

`sop/normals` exposes owner, weighting, cusp, zero-preservation, reversal,
and override-name fields. Its Lisp defaults are Vertex output and Vertex-angle
weighting; Point, Vertex, Primitive and Detail output are available. Normal,
Peak, Bend and Clip select components with the same optional `group_owner`
and `group` fields in Lisp and OCaml, with a blank name selecting the full
input. Bend defaults to a Y capture direction, Z up vector, unit length and
non-continuous twist. Clip defaults to preserving existing group membership.
Its cache identity includes every control, and missing groups become traced
diagnostics before the kernel is called. Sop does not compute or cache a
second set of normals.

`sop/measure_curvature` wraps the single packed RDK surface-curvature kernel.
Its stable node identity includes boundary policy, smoothing controls, point
group, and every optional output name. Point-group resolution and structured
manifold diagnostics remain graph-boundary concerns; triangulation, incidence,
metric estimation, smoothing, and output preservation are not reimplemented in
the cook closure. The operation is static and topology-preserving.

`sop/attribute_laplacian` uses the same RDK-owned surface metric and topology
index as curvature. Its identity contains the point group, weighting,
normalization, source, and output names. The cook closure resolves only the
named point group and delegates numeric storage conversion, manifold checks,
stable polygon triangulation, mixed-area normalization, incidence reduction,
and immutable output replacement to RDK. It is static and topology-preserving;
no matrix or solver state is hidden in the procedural session.

## Context, parameters, and determinism

The target-neutral context contains finite time, frame number, immutable seed,
domain/grain settings, and a cancellation token. Fixed-step sketches supply
their deterministic frame/time facts. Each node
declares exactly which facts affect it. A static box is not invalidated merely
because time advanced.

Constant parameters are ordinary OCaml values. Time/frame expressions, ramps,
easing, remapping, and deterministic noise are opt-in parameter values and mark
their owning nodes with the corresponding dependency. Random operations derive
stable streams from the user seed, stable node identity, and element index.
Domain count and work-stealing order cannot affect geometry or diagnostics.

`sop/attribute_fade` is the reference frame-dependent attribute node. Its
parameter key contains both ramps, all timing/name controls, and the roles of
its optional second and third graph inputs, while its context projection
contains only `Frame`. Re-cooking the same graph at another wall-time, seed, or
domain count therefore hits the same bounded cache entry; advancing the frame
invalidates it. This makes the node safe inside the explicit previous-snapshot
iteration model without putting mutable feedback inside a SOP cook.

`sop/poly_cut` is static and encodes the primitive group, owner-specific cut
group, element/strategy/detection variants, threshold field, and closure policy
in canonical cache identity. Group names are resolved only against the cooked
input; RDK owns classification, interpolation, compaction, and ancestry. This
keeps custom iterative curve sketches compositional: a previous snapshot may
feed PolyCut without storing temporal state or mutable fragments in the node.

`sop/separate_pieces` is also static. Its cache identity contains owner,
integer/text identity field, translation field, normalized-axis input, gap,
and Separate/Move Back mode. RDK computes the reversible per-piece mapping;
the SOP cook owns neither bounding state nor a retained lookup table. This is
particularly useful before per-piece proximity work in an iterative sketch:
separate, query or modify, then move back without introducing graph feedback.

`sop/edge_equalize` is static as well. Its cache identity includes the input
and output edge-group names, typed initial target policy, iteration ceiling,
and relative tolerance. The graph cook resolves topology-affine selection and
delegates target reduction, selected incidence, projection, convergence, and
normal invalidation to RDK. It retains no iteration buffers in the node or
session, so an iterative sketch may equalize its previous snapshot without
turning the acyclic per-frame SOP graph into a hidden feedback solver.

`sop/edge_relax` is a static two-input node whose role key distinguishes source
from matching-topology reference. Point/primitive selection, point pins,
individual or scale-independent reference targets, iterations, step size,
shorten-only policy, and tolerance all participate in cache identity. It is a
useful custom-sketch building block: a previous immutable snapshot can be the
source while a stable design shape supplies rest edge lengths, with all
temporary constraint buffers confined to one cook.

`sop/blend_shapes` is a static multi-input morph node. Input zero remains the
topology and payload owner. The shared declaration has four fixed optional shape
ports and a repeated `shapes` port for unlimited additional targets. The extra
`weights` table has one finite signed weight per row; missing rows use zero.
The `shape_masks` table has three columns: one-based slot index, mask attribute,
and mask source (`first` or `shape`). Blank cells inherit the global mask settings.
Fixed slots keep indices 1–4 when disconnected; connected extras start at 5.
Unused table rows remain available for later connections. Blank global names
disable their optional controls. Tables, patterns and weights are validated at
construction and every field participates in the schema/cache identity. Empty
targets retain the node's own identity and unchanged cooked geometry. The cook builds
integer/text point-ID maps only when requested, allocates every blended plane
once, and applies targets in descriptor order while point ranges execute in
parallel. No delta, ID, mask, or weight plane survives outside the completed
snapshot/session entry, so an iterative sketch can animate weights by building
a new acyclic node without hidden history growth.

`sop/attribute_composite` is the general static multi-input attribute fold.
Input zero owns topology and unselected payload. Four fixed optional layer
ports retain their independent weights, followed by an optional repeated
`layers` port for unlimited additional inputs. Its `weights` field is an
escaped one-column table of finite signed weights in connected-input order;
missing rows use 1 and unused rows remain available for later connections.
Lisp and the editor share that flat declaration. Independent owner patterns, first-input weight,
Mean/Maximum/Minimum/Over/Under mode, optional alpha name, and explicit `P`
eligibility all participate in the schema-derived node identity. Blank alpha
names disable masking. Patterns and weights are validated at construction;
attribute discovery, packed
planes, alpha distribution, finite/cardinality validation, and normal
invalidation live exclusively in RDK. No intermediate composite or denominator
plane survives outside the completed immutable snapshot/session entry, making
the node safe to rebuild inside an iterative sketch.

`sop/attribute_mirror` is a static one-input correspondence node. Its graph
value stores owner, method, named groups and mapping field, attribute pattern,
transform policy, text replacement, and optional metadata names. Named groups
are resolved from the current immutable input at cook time; RDK owns the
reflected spatial search, direct-index mapping, packed payload copies, and
atomic validation. Plane point/primitive and explicit point/vertex/primitive
mapping paths write disjoint ranges through the shared pool. No spatial index,
source map, destination bit-plane, or output builder survives outside the
completed snapshot/session entry, so it is safe to reconstruct around
a snapshot of the previous result in an iterative sketch (the test-only `Lisp_sop.snapshot` shows the shape).

`sop/rewire_vertices` is a static one-input topology modifier. The node stores
only typed selection, target owner/name, recursive-chain policy, target-field
deletion, newly-unused-point cleanup, and optional original-point provenance.
Selection names resolve against each immutable input cook. RDK owns target
validation, functional-graph resolution, corner rewiring, point payload/group
compaction, normal invalidation, and native-edge ancestry. Temporary target,
state, stack, incidence, and compaction planes become unreachable after the
new snapshot is committed, so iterative sketches can author changing
connectivity without hidden mutable graph state.

`sop/edge_transport`, `sop/edge_transport_curves`, and
`sop/edge_transport_parent` are static attribute-flow nodes over the same
immutable RDK snapshot. The network node resolves point and explicit-root
groups at cook time, the curve node resolves a primitive group and schedules
independent curves by stable primitive ranges, and the parent node resolves an
integer point-parent field plus an optional point group. Network and Parent
support forward copy/split and backward Add/Maximum/Minimum branch merging.
None retains a parent forest, distance plane, child index, or normalization
table in the graph cache beyond the completed geometry. Iterative sketches may
therefore transport a field from the previous `sop/snapshot` without hidden
feedback or unbounded per-frame state.

## Sessions and caching

Mutable evaluation state belongs to an explicit `Session.t`, never a global
graph. Its cache has both an entry limit and a retained-byte budget. The key is
the operator/version, canonical parameters, the schema-derived parameter key
of a parameterized node, input identities, declared context facts, and
explicit external-resource fingerprints. It is an opaque, length-prefixed
binary string built without `Printf` on the per-node cook path. Whole geometry
buffers are not rehashed on every frame.

`Node.facts` owns the cook mode, element-wise class, attribute reads/writes,
topology preservation and exactness. The optional `[@@sop.node_facts
{elementwise; reads; writes; topology; exact}]` type attribute refines the
conservative declaration and can refer to `parameters`; generated rebuilds
refresh it after parameter changes. Unannotated nodes read/write `"*"`, are
irregular, topology-changing and exact. The default declaration is exposed by
the factory and included in the generated Lisp manifest; `Flow.Catalog` carries
its domain-neutral kernel metadata.

`sop/noise_displace` declares two modes in its parameter schema. The default
`height_2d` reads P and adds signed X/Z noise to Y. `normal_3d` reads P and
point N and computes `P + N * (amplitude * Noise.sample3(P * frequency))`,
without normalizing N. At seed 0 this is the Flow `noise3` kernel's formula.
Both modes preserve topology and invalidate point/vertex N; changing mode
rebuilds the declared reads. Missing/wrong-storage point N, non-finite results
and cancellation return typed RDK errors.

A single-input `Duplicate_input 0` or `Passthrough 0` node with preserved
topology and explicit reads/writes uses topology, group and edge-group IDs,
the input attribute owner/name order, and IDs only for the payloads it reads.
Canonical positions are `"P"`; other names select every matching owner.
Changing an unread color payload can therefore reuse a normal or transform
computation. Attribute addition/removal/reordering conservatively recooks.
On reuse, the session combines the cached computed payloads with fresh untouched
input attributes, preserves output ordering and refreshes inherited diagnostics.
The result keeps its identity on subsequent unchanged hits. A node that replaces
declared preserved topology, changes groups, writes an undeclared payload or
returns packed instances fails with traced `E_NODE_FACTS`; no invalid entry is
published. Ordinary opaque nodes retain whole-snapshot keys.

Unchanged cache hits return the same immutable snapshot. Results are inserted only after
a complete successful cook; cancellation cannot publish partial geometry.
Eviction drops strong references, `Session.clear` releases all entries, and
`Session.close` provides prompt lifetime control. Session statistics expose
cooks, hits, misses, evictions, retained bytes, and last-node timings.

Independent branches may cook in parallel only when measured work justifies
it. Fine-grained RDK kernel parallelism is the default. Nested evaluation shares
Rays's one process-wide pool and must not oversubscribe it.

Interactive sketches submit expensive graphs through `Async_cook`, never by
calling `Session.cook` from the application update callback. One persistent
coordinator domain owns one single-caller session and a one-slot latest-request
queue. Submitting a newer graph atomically cancels the active context, replaces
the pending graph, and invalidates any unpublished completion. Stale or partial
geometry can therefore never replace the displayed snapshot. `poll` transfers
only an immutable prepared value back to the sketch; SDL, renderer, texture,
and GPU operations remain on the initial domain.

The preparation callback may perform target-neutral validation, shard or
component classification, attribute extraction, and packed render-mesh
construction. It must retain and check the request context's cancellation
token in long loops. Keep the prior successful mesh visible while `status`
reports elapsed cook time. Use a foreground `Sketch` domain count of one and
reserve one hardware domain from the background cook context; the persistent
coordinator releases its domain-owned cached Domainslib pools before exit.
`Async_cook.close` cancels work, joins the coordinator, closes its session, and
belongs in `Sketch.run_state ~on_stop`.

## Selection, attributes, and inspection

The approachable API uses typed, composable selections rather than beginning
with a group-expression language. Named groups can capture a selection for
reuse. Standard typed helpers cover `P`, `N`, `Cd`, `uv`, `v`, `id`, `pscale`,
`scale`, `orient`, and `up`; arbitrary attributes retain explicit owner and
storage types.

Inspection is part of the first release, not debug-only infrastructure:

- graph inspection reports node labels, operations, inputs, parameters, and
  time dependencies;
- cooked inspection reports element counts, memory estimate, attributes,
  groups, cache state, duration, domains, and grain;
- DOT export makes sharing and pipeline structure visible;
- errors identify graph path, input number, actual and expected geometry, and
  a likely corrective operation where possible.

## Implemented core nodes

The first coherent set includes:

- graph: null, merge, lazy switch, bounded session cache, graph/DOT inspection,
  and cooked geometry inspection;
- Lisp lowering of `sop/curve` and `sop/merge` (`lib/flow_sop`): `(sop/curve points :closed true)`
  joins the last point to the first; `:filled true` makes the same points one polygon
  primitive (a surface, so `sop/poly_extrude` and `sop/poly_fill` apply), and a closed curve
  can still be swept with `sop/sweep :caps true`. The gallery star is a filled curve extruded
  with `sop/poly_extrude`. `(sop/merge ... :source_attribute "src"
  :source_base 10)` writes the primitive integer attribute `src`, valued `source_base` plus the
  input index, beside the lowering's own provenance tag. Both tags share one
  geometry merge; existing tags retain their values and the original exact
  attribute-schema requirements apply;
- generators: immutable feedback snapshot, points, finite directed polygon or
  free-point Line, explicit polyline, production Box with triangle/quad,
  surface-point, and volume-lattice output, independent divisions, optional
  boundary welding, point/vertex normal policies, full Euler transform,
  per-face UVs/groups, production UV Sphere with regular/alternating triangles,
  quads, open row/column curves and points, shared/unique pole policies,
  ellipsoid radii, arbitrary pole axes, full Euler transforms, point/vertex
  normals and seam-safe UVs, production Torus with triangle/quad/curve/point
  connectivity, independent signed U/V ranges and wraps, polygon end caps,
  arbitrary hole axes, full Euler transforms, point/vertex normals and
  seam-safe UVs, production Tube with cylinder/frustum/cone geometry,
  triangle/quad/curve/point connectivity, consolidated or independent cap
  rims, hard-cap normals, UVs, cap groups, arbitrary axes and full Euler
  transforms, production Platonic Solids with all five regular convex solids
  plus the truncated-icosahedron soccer ball, natural N-gon faces, radial/hard
  normals, face groups, black/white primitive color and complete transforms,
  production Spiral with Archimedean/logarithmic profiles, turns or pitch
  extent, piecewise-linear height/radius ramps, equal-angle or integrated
  equal-arc sampling, phase-distributed copies, arbitrary axes/full Euler
  transforms, and optional angle/frame/orientation/distance attributes,
  production polygon-curve Revolve with arbitrary axes, full or open arcs,
  point/row/column/combined/quad/triangle connectivity, compact shared poles,
  optional polygon caps, normalized UVs, typed primitive restriction, complete
  payload ancestry, and exact parallel edge-range fills,
  production Circle with
  closed/open/chord/sliced arcs, ellipses, standard/custom planes and complete
  polygon transform controls, and production
  Grid with division/point-count resolution, point/row/column/quad and three
  triangle-connectivity modes, standard/custom plane frames, independent
  dimensions, center/rotation, normals, and optional normalized UVs;
- modifiers: raw-matrix or typed TRS/shear/pivot Transform with six affine and
  Euler orders, inversion, typed point/vertex/primitive/native-edge restriction,
  and explicit inverse-transpose/recomputed normal policy; Soft Transform with
  closest-radius, exact edge-path, or authored point-weight metrics, three
  rolloff profiles, inspectable falloff output, and normal repair; Distance
  Along Geometry with typed start/affected selections, exact geometric
  edge-path distance, unreachable sentinels, fixed or maximum-distance masks,
  and bounded mask-only cooking; Distance From Geometry with independent typed
  source/reference restrictions, closest reference-point or polygon-surface
  metrics, raw distance, fixed/maximum masks, and distance-only packed index
  queries; Distance From Target with spherical point, cylindrical axis, and
  absolute or signed planar analytic projections, typed affected restriction,
  and fixed/maximum masks; stable point/primitive Sort with deterministic
  random permutation, point topology/incident-primitive keys, Morton spatial
  locality, strict reorder-by-index, and composable indirect rank output;
  Blast by Attribute with scalar float/integer point or primitive fields,
  strict threshold and inclusive range/width modes, named base restriction,
  inversion, delete/group output, and primitive unused-point cleanup;
  Crease with all or named native-edge selection, coherent add/set/delete of
  Subdivide-ready vertex sharpness, and optional endpoint color visualization;
  materialized cumulative
  or non-cumulative Duplicate with optional primitive-source restriction,
  exact copy-major ancestry, and bounded per-copy primitive groups, reverse,
  triangulate, normals, production Clean pipeline with
  point consolidation, degeneracy/overlap/NaN repair, winding, compaction, and
  attribute/group cleanup, delete,
  normal-directed Peak, captured Bend/Twist in arbitrary frames, and
  deterministic Perlin-fBm Mountain with typed selection/mask/capture,
  custom-direction, and normal-recompute controls,
  deterministic Point Jitter with a point group, linearly applied float mask,
  stable integer IDs, overall/per-axis scales, and optional point `pscale`,
  topology-affine Edge Divide with an exact resulting segment count, shared or
  per-incidence inserted points, complete packed payload/group interpolation,
  connected-component Edge Collapse with center reduction, exact point-field
  partitions, degenerate cleanup, and optional existing-normal recomputation,
  manifold polygon Edge Flip with general mixed-face cycles, optional
  vertex-payload cycling, validity checks, and exact native-edge ancestry,
  Edge Cusp path-interior fan splitting with endpoint fade behavior, complete
  point payload/group duplication, and optional existing-normal rebuilding,
  Point Split unique-corner or mixed vertex/primitive attribute and named-group
  seam clustering for point/vertex/primitive selections, with inclusive
  tolerance, optional attribute promotion, stable point numbering, and exact
  one-to-many ancestry,
  cardinality-first Point Generate for no-input origin clouds or deterministic
  selected-source emission, with scaled expected counts, probability fields,
  every packed point payload, detail-copy patterns, retained topology,
  generated groups, and source/local provenance,
  Point Replicate local point/box/sphere/disk/line/custom clouds over the same
  emission plan and canonical instancing frames, with stable ID streams,
  quasi coordinates, shape ancestry, source payload, retained input, velocity
  synthesis/stretch, rest-space coherent noise, and optional copied-vector or
  inverse-transpose normal transformation through those same frames,
  connected-component Edge Straighten with stable least-squares line fitting,
  exact collinear identities, branch/cycle support, and output edge groups,
  Circle from Edges over topology boundaries or native edge groups, with
  scale-normalized least-squares plane/circle fitting, explicit radius/scale,
  and output edge groups,
  Graph Color over typed promoted selections with point-clique, primitive-
  shared-point, or primitive-shared-polygon-edge connectivity, optional stable
  color sorting and packed detail workset ranges (selection owner/name and
  workset output/names are separate optional fields),
  Triangulate 2D over exact-predicate packed Delaunay topology with named point
  groups, native-edge and primitive-perimeter constraints, opt-in exact
  crossing construction/atomization, exact authored-point-on-constraint
  splitting without synthetic duplication, generated-point groups,
  exact constraint-blocked convex-hull outside flooding,
  exact non-zero winding removal from closed constraint primitives,
  exact projected-face silhouette constraints and outside removal independent
  of global face orientation,
  optional constraint-endpoint-only seeding with ignored source points retained
  as isolated immutable payload,
  optional exact projected-duplicate removal that preserves unrelated unused
  or unselected source points,
  bounded incremental constrained-Delaunay refinement with minimum-angle,
  maximum-area, target-edge-length, minimum-edge-length, constraint-splitting,
  maximum-new-point, and refinement-point-group controls,
  deterministic exact-certified regularization of generated interior points,
  optional movement of original projected interior points, and compact
  provenance-DAG interpolation across repeated relaxation,
  optional shared-kernel unused-point compaction and conditional existing-point
  normal recomputation,
  optional retention of every non-constraint input polygon/curve as a stable
  prefix with exact fixed/ragged vertex and primitive payload, ordered-group,
  and native-edge ancestry plus typed defaults for the generated triangle
  suffix,
  explicit original-position restoration policy, including projected world-
  plane output for PCA/principal/explicit planes and XY output from the first
  two components of a point coordinate attribute,
  PCA/principal/explicit/attribute projection, deterministic insertion seed,
  deterministic split-point payload policy, and output triangle and
  constrained-edge groups,
  Edge Equalize with average/longest/shortest initial targets, a direct
  independent-edge path, bounded deterministic connected projection, and
  explicit convergence diagnostics,
  reference-driven Edge Relax with exact topology matching,
  individual/scale-independent length targets, point/primitive restriction,
  pinning, shorten-only policy, and the shared bounded constraint projector,
  deterministic forward/backward Edge Transport over shortest-path edge
  forests and integer parent forests, plus a linear independently parallelized
  Each Curve point/vertex fast path,
  optional orphan-point compaction, individual-face or connected-region Poly
  Extrude with named primitive selection, split-edge seams, straight divisions,
  independent front/back/side output, and primitive/native-boundary groups,
  deterministic point-group-restricted fuse and per-axis grid snap with
  movement tolerance, moved-point output, and optional exact consolidation;
  Fuse supports a read-only second target and independent target group,
  least-number/closest near targets, specified integer target points,
  point-radius expansion, equal/unequal scalar match filters, snap-only mode,
  and stable snapped-point/destination metadata. Same-input Modify Target,
  complete first/least/greatest, component-statistical and weighted position
  reducers, Keep Fused Points, repeated-vertex/degenerate cleanup, and
  selective or complete unused-point compaction share the packed RDK kernel.
  Ordered attribute/group pattern rules cover numerical, text,
  scalar-to-array/array concatenation, weighted, union/intersection, and
  strict-majority policies; fixed targets copy matching point payload and
  target-only groups while remaining immutable,
  arbitrary-plane mirror, arbitrary-plane clip with numeric point-coordinate
  attributes, normalized-normal distance or transform-derived placement,
  typed point/vertex/primitive/native-edge restriction with selected-boundary
  isolation, split/fill controls, and replace-or-union native clipped edges and
  primitive output groups, including disconnected concave-fragment
  reconstruction and winding-aware nested cap holes/islands,
  Catmull-Clark/Loop/bilinear subdivision with
  semi-sharp creases, a typed second topology input with named primitive
  restriction/override/result controls and point-number edge matching,
  stencil-contributing automatic/named hole faces with final-level removal,
  and
  named primitive-group local Do Not Close, Pull
  Closed/No Edge Division, Pull Closed/Divide Edges or Triangulate with bias,
  and Stitch/No Edge Division, Divide Edges, or Triangulate refinement, with
  optional topology-stable collinearity-independent closure,
  two-input closest/directional Ray projection with bounded seeded multi-ray
  jitter, average/median/shortest/longest combination, exact collision
  provenance, and attribute/group import, typed Convex Hull with exact affine
  and horizon decisions, deterministic lower-dimensional/closed-solid output,
  point/detail ancestry and generated face metadata, Extract Centroid over the
  detail, primitives, or stable integer/text pieces with point-mass, bounds,
  or the same exact hull core, Extract Point from Curve with constant,
  per-primitive, or current-time scalar cuts plus interpolated point/copied
  primitive payload and curve diagnostics, typed divided-box or
  sphere/ovoid Bound with asymmetric padding and detail/group metadata,
  Match Axis, and production Match Size with
  independent typed move/source/target selections, unit/numeric or node
  references, per-axis/cross-anchor alignment, offsets, and bounds/metric fit
  modes;
  typed topology editing: point/vertex/primitive Delete with destroy/heal and
  selected/non-selected policies, named-group Blast, and cache-sharing
  selected/remainder Split branches;
- topology groups: native topology-affine Edge Group with primitive,
  incidence, length, face-dihedral, and pairwise incident-edge angle filters;
  named edge groups remain
  distinct from side-specific outgoing-corner vertex groups; Group from
  Attribute Boundary finds typed point/vertex/primitive discontinuities and
  emits native-edge, point, or primitive selections with tuple tolerance and
  unshared-curve policy; Groups from Name creates stable, explicitly bounded
  point or primitive groups from non-empty text values with replace/union and
  ignore/force-valid naming policy, while Name from Groups converts selected
  point/vertex/primitive groups back to a compact text partition with explicit
  overlap and source-deletion policy; Group Random provides context-seeded or
  explicitly seeded point/vertex/primitive/native-edge chance selection with
  base restriction and packed merge algebra; Group Bounds provides inclusive
  box/sphere selection with full/partial primitive and geometric native-edge
  containment, using separate shape, center, size, and radius fields with
  finite/non-negative bounds checked at construction; Group Normal provides geometric or explicit-attribute
  direction/spread selection for points, primitives, and native edges with an
  optional opposite cap, while Group Non-Planar provides stable
  tolerance-based polygon selection and additive union composition; Group
  Backface selects winding-derived polygons relative to a viewpoint and can
  subtract them from another same-name criterion; Group Edge Depth performs
  bounded shortest-edge growth from a point group; Group Unshared selects
  one-sided topology as points, primitives, or native edges, while Group
  Boundary Components emits stable, bounded point groups for each connected
  polygon-surface boundary; Group Promotions
  converts among every point/vertex/primitive/native-edge owner using touching,
  complete-containment, or shared-edge rules and can emit an ordinary-owner
  integer mask instead of a group. Ordered rules compile wildcard
  selection/capture rewrites, safely snapshot same-rule sources, allow later
  rules to re-promote earlier outputs, and bound output count/payload; Group
  Promote Boundary retains only the
  converted selection frontier and can union typed attribute seams, controlled
  polygon/curve unshared edges, and point-sharing primitive boundaries, while
  Group Expand performs
  signed topology steps or a connected-component flood over every owner and can
  materialize the first-change/BFS distance as an integer attribute. Point and
  edge-connected primitive growth can additionally stop at an adjacent normal
  spread, typed point/vertex/primitive attribute discontinuities, or an
  independently owned collision boundary; collision membership can contain
  growth and boundary elements can be retained or excluded. Attribute and
  collision seams also define shrink-away boundaries; Group
  Range supplies absolute/relative/partition ranges and periodic filters,
  optionally evaluated independently over stable disconnected point or
  primitive regions with a one-region restriction,
  Group Combine and Invert provide four-owner packed boolean algebra, Group
  Delete/Rename provide ordered wildcard metadata rules, and two-input Group
  Copy maps membership by indices, primitive-local corners, or integer/text
  attributes; two-input Group Transfer maps point, primitive, and native-edge
  membership by accelerated exact geometric proximity and preserves ordinary
  source-group order using source sequence, proximity, and stable index ties;
- topology paths: Group Find Path builds ordered point-edge or manifold
  primitive-dual paths through contiguous ordered bases or start/end pairs,
  with stop/close endings, same-owner shared-element avoidance and collision
  exclusion/containment, and parallel independent routes;
- attributes/groups: constant float/int/vector/quaternion/color creation;
  float/int values, grouped Vec3 values and four quaternion channels take the
  same optional defaults in Lisp and OCaml, with finite values checked at construction;
  constant point transform matrices take 16 optional scalar entries, validated
  as finite and affine; constant color takes a grouped Vec3 plus alpha;
  Rest Position stores/extracts/swaps positions and optional normals, with a
  positional optional reference input; Enumerate separates integer/text storage
  from the text prefix;
  Attribute Blur and Smooth take a mode plus separate Laplacian/custom step
  sizes, with pattern/finite checks at construction. Smooth defaults to 10
  iterations without normal recomputation. Attribute promotion keeps sources
  by default. PolyFrame takes separate style/attribute and selection fields;
  blank tangent/bitangent names disable those outputs;
  exact-name delete/rename plus atomic owner-pattern Attribute Delete with
  reference-name prepend and keep mode, ordered capture-pattern Attribute
  Rename with skip/error/overwrite conflicts, ordered owner-specific Attribute
  Swap/Move/Copy with paired capture globs, packed payload sharing, missing-side
  copy behavior, and canonical float3 `P` handling, dense integer/prefixed-text
  enumeration within typed groups with integer/text piece-element or stable
  piece-ID modes,
  planar/cylindrical/spherical vertex UV projection with primitive-group,
  seam, pole, and separate U/V range controls (the default planar axes are X/Z),
  point/vertex UV transforms with separate U/V translation, scale and pivot fields,
  height colors with separate RGBA channels at each endpoint, automatic native
  angle/partition/existing-UV seam groups with stable island IDs, and
  per-face/per-island UV unit fitting, seam-aware harmonic UV flattening, and
  boundary-preserving UV relaxation,
  promotion across every geometry owner with deterministic mode/upper-median
  reductions, integer/text destination-piece partitions, and shared-plan
  plural promotion through compiled include/exclude name globs with optional
  aligned multi-term destination/index capture renaming, exclusion terms, and
  last-match precedence; deterministic contributing-source indices for scalar
  reductions and fixed-width float2/3/4 component-source CSR rows; documented
  text/index Average-to-median, Sum-concatenation, and numeric-to-First policy;
  plus packed scalar integer/float Array of All and sorted Unique Values CSR
  output with complete piece-row remapping; direct
  ordered/cyclic Attribute Copy with
  integer/text value or explicit-element matching, owner-independent topology
  projection, wildcard renaming, and structural sharing for identity plans;
  fused multi-input Attribute Combine across all numeric RDK storage with seven
  arithmetic modes, preprocessing, tuple conversion, per-element blending,
  integer/text correspondence, grouped postprocessing, and atomic cleanup;
  primitive/UVW or packed CSR point/vertex/primitive number-and-weight
  Attribute Interpolate from owner-compatible source fields into point,
  vertex, primitive, or detail destinations, with equivalent computed
  point/vertex weight output, signed-sum normalization/threshold influence,
  zero-sum preservation, inclusive group thresholding, opaque greatest-
  coefficient packed array-row transfer, canonical position support, exact
  group-pattern destination restriction, compiled
  owner-specific source-attribute/group expansion, weighted source-group
  membership, one atomic packed cook, and deterministic polygon/curve
  parameter spaces;
  nearest/inverse-distance and compact Links/RenderMan/Hart k-nearest
  point and primitive-barycenter transfer with independent sample count/radius
  and exact or compiled-pattern
  owner-matched groups and shared full-strength/blend-band controls,
  closest-polygon vertex transfer, structurally shared detail transfer, and
  explicit mixed-owner polygon-surface sampling into target points, vertices,
  or primitive barycenters with optional distance output; primitive and
  vertex source groups can intersect the sampled surface, with explicit
  all-corners or any-corner triangle retention; one
  `attribute_transfer_all` node can cook explicit point/vertex/primitive/detail
  patterns while sharing each owner's query plan and committing metadata once; native
  range kernels, typed packed or explicit-order group construction and
  combination, and deterministic
  connectivity-based `attribute_blur` over `P` and multiple point floating
  attributes with edge weighting, masks, pins, and custom iteration steps;
  dedicated topology-preserving `smooth` composition with primitive
  restriction, locked points, unshared/group-boundary constraints, and exact
  parallel alternating low-pass passes over the same packed kernel;
  two-input `ray` projection through the shared packed polygon surface index,
  including source/collision groups, normal/vector/attribute directions,
  deterministic direction/surface policies, bounded seeded cone jitter,
  average/upper-median/shortest/longest successful-hit combination, hit
  outputs, and compiled collision field/group import;
  indexed deterministic `attribute_randomize` across every owner with typed
  cross-owner/native-edge group expansion, seed or fraction attributes,
  arithmetic modes, bounded tails, isotropic multidimensional Cauchy, and
  twelve production distributions including spherical, inverse-CDF ramp, and
  weighted tuple/text sampling; coherent Perlin `attribute_noise` across every
  owner with point/element-number/custom-vector sampling locations, positive,
  zero-centered, or explicit ranges, set/add/subtract/multiply/min/max modes,
  blending, vector frequency/offset, and standard fBm controls; its scalar and
  vector modes cover the corresponding common Attribute Noise 2.0 surface,
  while normalized Float4 quaternion output is explicitly a Rays extension
  for Copy-to-Points `orient`; component-wise `attribute_remap` with explicit/automatic
  ranges, clamp/cycle/extrapolate, renaming, and piecewise-linear ramps;
  frame-only `attribute_fade` with a scalar point field, point restriction,
  independent equal-cardinality start/hold inputs, affine start retiming,
  float/integer timing attributes, per-point hold scaling, separate linear
  ramps, and optional grayscale point-color visualization;
  PolyCut with primitive plus point/native-edge restriction, remove or cut
  topology, all-edge/scalar-crossing/tuple-change detection, exact inserted
  endpoints, optional closed fragments, and every-owner payload/group ancestry;
  Separate Pieces with point/primitive integer or text identities, arbitrary
  packing axis and gap, same-owner float3 translations, strict rigid-piece
  validation, deterministic Move Back, and packed multicore position fills;
- analysis: bounds; stable point/primitive `sop/connectivity` with owner-correct
  point/primitive masks, native edge seams, vertex-UV islands, and integer or
  prefixed-text classes; and generalized `sop/measure` for
  primitive-group polygon area, curve/polygon perimeter, oriented signed
  volume, per-element/throughout accumulation, and optional detail totals;
- creative workflows: deterministic fixed-count surface Scatter with primitive
  groups, owner-typed density, compiled attribute/group transfer, and exact
  primitive/source-vertex-weight provenance; expanded copy to points
  with the standard `pscale`/`scale`, `orient` or `N`/`v`/`up`, post-`rot`,
  `pivot`, and `trans` stack plus packed affine `transform` overrides, count-
  or length-driven arc-length curve; Copy to Points also resolves named source
  primitive and target point restrictions before its packed expansion and
  supports integer/text source primitive or source point piece attributes,
  integer primitive-number fallback, unmatched-target omission, and stable
  target-major variant ordering; it
  applies last-match-wins point/vertex/primitive target-attribute and
  target-group rules afterward, including explicit `Nothing` overrides,
  resampling with primitive-group/count/length overrides and optional
  U/curve/distance/tangent fields, packed `sop/polyframe` point or
  seam-preserving vertex coordinate fields, and primitive-group curve carving
  with inside/outside/all-piece cuts, exact unselected topology pass-through,
  seam-aware closed complements, equal-parameter divided cuts, and divided
  free-point extraction,
  Catmull-Clark/bilinear polygon-curve Subdivide with recursive shared-graph
  cubic refinement, local primitive restriction, mixed surface/curve cooks,
  Houdini-compatible independent curve-corner point identity, default
  point-stencil `N`, and opt-in final recomputation only when point `N`
  existed on the input,
  native-edge-group `sop/edge_divide` for equal-parametric cuts on polygon and
  polygon-curve edges, with shared coincident points by default and an explicit
  unique-points mode,
  native-edge-group `sop/edge_collapse` using the shared Fuse reducer for
  stable center contraction, topology/payload ancestry, and cleanup,
  adaptive `sop/poly_reduce` using deterministic QEM-ranked independent
  contraction batches, ratio/absolute targets, primitive restriction, hard
  points/edges, strict boundary locking, original-position and normal-deviation
  controls, topology validity checks, and the same shared Fuse ancestry core,
  isotropic `sop/remesh` using fused midpoint subdivision, independent
  link-condition contractions, valence-improving flips, constrained relaxation,
  and optional packed-BVH projection, with uniform or point target sizes, hard
  points/native edges, UV-seam preservation, diagnostic outputs, and exact
  one/multi-domain topology and payload ancestry,
  optional one/two-input `sop/boolean_detect` using deterministic internal triangulation,
  packed two-pass BVH pair discovery, locally normalized crossing/touching and
  optional coplanar tests, independent source/collision primitive restrictions,
  and source-owned AxB intersection group, sorted collision-primitive CSR, and
  row count outputs without changing source topology or payload; its one-input
  AxA mode visits unordered pairs once, suppresses shared-topology-only contact,
  and emits symmetric self-intersection group/CSR/count outputs,
  optional one/two-input `sop/intersection_analysis`, which emits point-only
  triangle, polygon-curve, and mixed intersection geometry instead of passing
  either input through, welds repeated piece-pair events in stable
  first-incidence order, and attaches aligned point-owned
  input/primitive/parameter/incident-point CSR
  provenance for later attribute interpolation and seam construction,
  native-edge-group `sop/poly_bevel` for watertight partial or connected
  two-sided polygon fillets, with chamfer/round divisions, point-scale and
  flatness controls, collision-limited ring slides, connected corner handling,
  generated face/boundary groups, packed payload interpolation, and exact
  one/multi-domain output,
  typed-selection `sop/point_split` for unique selected corners or deterministic
  vertex/primitive attribute and named-group seams, with ordered globs,
  inclusive tolerance, optional attribute promotion, and the shared packed RDK
  point-remap core,
  `sop/point_generate_origin` and connected `sop/point_generate` over the same
  packed emission kernel, with point-group selection, context-derived or
  explicit immutable seeds, stable cache identity, copied payload patterns,
  retained input, generated grouping, and exact one/multi-domain order,
  `sop/point_replicate` for shaped high-density point clouds, with a second
  immutable custom-shape graph when requested and complete cache identity for
  distribution, standard source transforms, count scaling, velocity, and
  noise controls, including the copied-vector transform pattern,
  native-edge-group `sop/dissolve` for stable manifold face merging,
  disjoint/bridged/deleted hole policy, optional boundary curves, and inline
  plus unused-point cleanup through the shared RDK topology core,
  native-edge-group `sop/edge_flip` for deterministic general-polygon boundary
  rotation, with explicit sequencing for flips that share a primitive,
  native-edge-group `sop/edge_cusp` through the shared Facet point-fan kernel,
  preserving selected path endpoints and splitting only effective interiors,
  native-edge-group `sop/edge_straighten` using independent packed component
  fits and deterministic principal-axis projection,
  boundary/native-edge-group `sop/circle_from_edges` using the single RDK
  component planner and normalized plane/circle fitter, with no graph-local
  topology or projection implementation,
  typed-selection `sop/graph_color` using the single RDK union-find/incidence
  coloring core and shared Sort remapper, with no graph-local adjacency or
  topology implementation,
  native-edge-group `sop/edge_equalize` using the shared RDK target reduction
  and centroid-preserving connected projection rather than a graph-local
  geometry loop,
  two-input `sop/edge_relax` with role-complete cache identity and RDK-owned
  reference-length planning/iteration,
  multi-target `sop/blend_shapes` with normalized/differencing weights,
  source/shape masks, point-ID matching, fixed-width point payload blending,
  exact target-order accumulation, and RDK-owned temporary plans,
  ordered `sop/attribute_composite` with independent owner patterns, weighted
  Mean/Max/Min/Over/Under folds, optional same-owner alpha, opt-in `P`, and
  allocation-once RDK-owned temporary/output planes,
  `sop/attribute_mirror` with plane-nearest or named explicit correspondence,
  all packed storage kinds, destination-only transformations/replacement,
  cook-time group resolution, complete cache identity, and RDK-owned scratch,
  `sop/rewire_vertices` with point/vertex/primitive integer targets, typed
  selection promotion, recursive point chains, provenance/cleanup controls,
  and exact packed payload plus native-edge ancestry,
  static `sop/edge_transport` network, `sop/edge_transport_curves`, and
  `sop/edge_transport_parent` wrappers with complete parameter identity,
  cook-time group/parent resolution, and RDK-owned temporary traversal storage,
  ordered, typed point/vertex/primitive/native-edge-selected `sop/facet`
  normal, point-sharing,
  point/normal consolidation,
  inline-point removal, manifold orientation, dihedral-angle hard-edge cusping,
  degenerate cleanup, and conflict-safe polygon planarization stages,
  ordered `sop/group_ranges` batches with blank disabled slots and later-rule
  base/collision/merge dependencies,
  manifold-hole `sop/poly_fill` with native-edge
  auto-completion and shared/unique N-gon, concave-safe triangle, or averaged
  triangle-fan patches,
  selected unique-edge conversion to canonical line primitives or fused
  maximal paths with endpoint-distance/endpoint-only/loop-closure controls
  and optional final path length,
  polygon/curve Ends with open, straight-close, shared-seam unroll, or
  payload-duplicating new-seam unroll, detail-ordered, ordered-group, explicit
  user-picked-end, or global closest-end curve joining with fixed-size
  subgroups, optional original retention, and stable welding/component
  policies, packed `sop/poly_loft` triangulation across
  unequal open/closed curve or polygon sections with closest/rest-guided
  pairing, U/V wrap, source retention, and exact payload ancestry, packed
  `sop/skin` construction that retains quads for equal-cardinality linear
  polygon sections and uses the same zipper for unequal sections, direct
  multi-component `sop/poly_bridge` surfaces between simple native edge paths
  or loops with authored/centroid pairing, reverse/shift controls, input
  retention, uniform equal-cardinality straight divisions, packed payload/group
  interpolation, and exact boundary ancestry, and shared circular
  `sweep_circle`/`polywire` with primitive-group restriction and exact
  unselected pass-through, point-varying radial divisions, point-driven
  endpoint-averaged longitudinal segmentation, deterministic minimal
  triangle/quad ring transitions, point scale, snapped seam, authored
  V/joint-up attributes, constant or outgoing-corner segment placement,
  optional UV generation, constant or outgoing-corner U/V ranges,
  capped constant/point-authored radial joint miters, constant/point-authored
  smoothing and maximum-valence disconnection into real uncapped runs,
  outgoing-corner per-edge snapped texture seams, closed-frame correction, and
  hard-normal caps;
  two-input general-profile `sweep` with typed curve groups, all Grid
  connectivity modes, the five backbone tangent policies, closed-continuous
  roll/twist, standard point transform attributes, caps, UVs, and prefixed
  dual-input attribute/group/native-edge ancestry;
  terminal `Instances.create`/`Instances.duplicate` transform instancing
  without topology multiplication;
- expert extension: `sop/custom` defines an inspectable node from RDK
  composition with explicit version, parameter identity,
  cook mode, context dependencies, and cancellation responsibility;
- terminal conversion to Rays mesh, instance, and scene values.

The undocumented Blinn/Wyvill/Elendt transfer kernels and multi-sample vertex
kernel policy, ordered-edge Group Find Path ownership
and its remaining loop/ring/extension,
UV, mixed-owner collision, and boundary constraints, Group from Attribute
Boundary base-group conversion and degenerate-bridge cleanup, reversible
`encodeattrib` naming for Groups from Name, direct assignment/string-edit modes
for Name from Groups,
Group Promotions degenerate-bridge cleanup and indexed-capture substitution,
Group Expand constrained vertex/edge targets, shared-point primitive seam
semantics, group patterns/type inference, edge-owned distance output (edge
groups intentionally have no attribute owner), and parallel component
labeling, remaining
distance kernels,
packed-instance attribute-driven transforms, multiple packed prototypes and
packed piece matching, per-instance payloads, Boolean resolved output mode
and source restriction/naming controls (the public exact
`sop/boolean` already exposes solid/surface products, per-input
self-intersection resolution, full primitive/point/vertex payload and group
transfer, exact native-edge ancestry, point conflict promotion, seam-point
splitting, named solid Shatter pieces, bounded strict/diagnostic tiny-seam
cleanup, safe source-polygon detriangulation, and closed rounding
verification. Interpolated point/vertex `N` is normalized and follows exact
extraction ancestry: a reversed output facet negates its source normal so the
payload remains aligned with output winding. `sop/boolean_fracture`
additionally emits a dense primitive
piece identity from exact oriented Weiler cells for solid-by-surface fracture;
shared seam topology is retained and packing uses the identity rather than
polygon connectivity. Terminal packed-piece rendering expands triangle
vertices without discarding those authored normals. `sop/boolean_seam`
exposes named left-self, between-input,
right-self, and coincident-patch products), and the remaining
documented Remesh adaptive/group/boundary controls,
volumes, the remaining Boolean Fracture naming/interior-group/constraint modes,
native edge-group seam heuristics,
SCP/ABF flattening constraints, UV unwrap/layout, and a convenience iterative sketch-feedback
wrapper remain later work. A public placeholder is
not used for an operation that lacks a real result contract.

## Iterative creative sketches

Iterative creative sketches keep feedback outside the per-frame DAG. A
`Sketch.run_state` model owns the previous `Rdk.Geometry.t`; each fixed-timestep
update wraps it with `sop/snapshot`, composes ordinary SOPs or `sop/custom`
RDK work, cooks the next immutable value, and replaces the previous one.
The graph for each step remains acyclic and independently inspectable.

Only current/next snapshots are application-live by default. The associated
`Session.t` must have explicit entry and payload-byte limits, so prior cook and
mesh results age out predictably; optional history/checkpoints need a separate
bounded ring. Pure heavy kernels still use the reusable domain pool, then join
before mesh conversion and rendering. Render-only repetition should remain an
`Instances.t` transform payload rather than being fed back as multiplied
topology. This is enough for erosion-like iteration, growth, relaxation,
agent-written trails, reaction fields converted to geometry, and similar
creative feedback without claiming a rigging, dynamics, or VFX solver suite.

Point Jitter makes the minimal random-walk form concrete: each fixed update
wraps the prior snapshot, applies `(sop/point_jitter :seed step_seed)`, cooks the
new snapshot, and stores only that result. Use an integer ID attribute when
topology edits may renumber points, and derive `step_seed` from the immutable
sketch seed and step number. A point mask or `pscale` can carry persistent
per-point influence without allocating a history plane. The same pattern can
replace Point Jitter with a composed SOP operation or a typed RDK custom
kernel; feedback ownership and memory bounds do not change.

Rest and motion data use the same explicit boundary. `sop/rest_position`
stores, extracts, or swaps packed reference positions and optional point
normals; a second input can supply the reference snapshot without copying its
numeric planes. `sop/point_velocity` accepts explicit previous and/or next
snapshot inputs for backward, forward, or central finite differences. Integer
or text IDs preserve correspondence across deterministic topology edits,
central mode can emit acceleration, and a point group restricts writes while
preserving existing values elsewhere. There is no implicit time-shift cook or
retained previous-frame node state: the sketch model owns those snapshots.

A reusable custom procedural node is therefore an ordinary function from
immutable parameters and input `Node.t` values to `Node.t`. Its implementation
composes public SOPs when possible and uses `sop/custom` only for a missing
RDK kernel, with an explicit operation/version/parameter key. A sketch
solver may run one iteration per frame or several iterations until a measured
time/work budget is exhausted, but deterministic/export runs use an explicit
iteration count rather than a wall-clock cutoff. It commits only a complete
next snapshot.
It never exposes a half-mutated geometry if cancellation or a deadline fires.
This keeps realtime iteration memory proportional to current/next geometry,
bounded session state, and explicitly bounded optional history.

## Research basis

The design was checked through 2026-08-04 against SideFX's primary documentation:

- [SOP concepts](https://www.sidefx.com/docs/hdk/_h_d_k__data_flow__s_o_p.html)
  for cooking and changed-input duplication;
- [Solver](https://www.sidefx.com/docs/houdini/nodes/sop/solver.html) for the
  explicit previous-frame feedback model, [Rest Position](https://www.sidefx.com/docs/houdini/nodes/sop/rest.html)
  for reference-space storage, and [Point Velocity](https://www.sidefx.com/docs/houdini/nodes/sop/pointvelocity.html)
  for finite-difference motion and ID matching;
- [Dissolve 2.0](https://www.sidefx.com/docs/houdini/nodes/sop/dissolve.html)
  for edge-complement selection, bridge and boundary policies, inline cleanup,
  unused-point policy, and normal regeneration;
- [Blast by Attribute](https://www.sidefx.com/docs/houdini/nodes/sop/blastbyattribute.html)
  for point/primitive run-over ownership, base restriction, threshold/range/
  width selection, inversion, delete/group output, and primitive orphan-point
  cleanup;
- [Crease](https://www.sidefx.com/docs/houdini/nodes/sop/crease.html) for edge
  selection, Add/Set/Delete behavior, `creaseweight` authoring for Subdivide,
  and optional vertex-color visualization;
- [Attribute Fade](https://www.sidefx.com/docs/houdini/nodes/sop/attribfade.html)
  for point restriction, fade/start/hold-scale fields, role-specific reference
  inputs, affine frame retiming, in/hold/out timing, transition ramps, and
  visualization;
- [Attribute Composite](https://www.sidefx.com/docs/houdini/nodes/sop/attribcomposite.html)
  for stable input order, per-owner attribute patterns, global weights,
  same-owner alpha, opt-in P, and the five documented compositing operations;
- [Attribute Mirror](https://www.sidefx.com/docs/houdini/nodes/sop/attribmirror.html)
  for source/destination policy, point/primitive plane correspondence, explicit
  mapping, attribute transforms and literal text replacement, pair output, and
  side groups; topology traversal and plane-based vertex matching remain
  explicitly outside the current subset;
- [Rewire Vertices](https://www.sidefx.com/docs/houdini/nodes/sop/rewire.html)
  for owner-dependent selection expansion, invalid-target preservation,
  recursive point chains/cycles, target deletion, newly-unused cleanup, and
  original-point provenance;
- [PolyCut](https://www.sidefx.com/docs/houdini/nodes/sop/polycut.html) for its
  point/edge modes, remove/cut strategies, threshold crossing and change
  subdivision behavior, group restrictions, and closed-fragment policy;
- [Separate Pieces](https://www.sidefx.com/docs/houdini/nodes/sop/separatepieces.html)
  for integer/text piece identity, reversible translation metadata, and the
  two-input union-bound intent; its undocumented layout remains an explicit
  Rays axial-packing contract;
- [Edge Equalize](https://www.sidefx.com/docs/houdini/nodes/sop/edgeequalize.html)
  for selected-edge equal-length intent, average/longest/shortest initial
  target policies, and output edge grouping; its unpublished numerical solver
  remains an explicit Rays convergence contract;
- [Edge Relax](https://www.sidefx.com/docs/houdini/nodes/sop/edgerelax.html)
  for point/primitive restriction, matching-topology reference positions,
  individual/scale-independent targets, iterations, step size, pins,
  shorten-only behavior, and the explicitly deferred surface/anisotropy tabs;
- [Edge Transport](https://www.sidefx.com/docs/houdini/nodes/sop/edgetransport.html)
  for graph/curve methods, scalar operations, direction, roots,
  normalization, split/merge, and the explicitly audited remaining modes;
- [PolyReduce 2.0](https://www.sidefx.com/docs/houdini/nodes/sop/polyreduce.html)
- [Remesh 2.0](https://www.sidefx.com/docs/houdini/nodes/sop/remesh.html)
  for polygon/ratio targets, original positions, quad/boundary/seam/feature
  preservation, density/view/attribute weighting, and rest-geometry behavior;
- [PolyBevel](https://www.sidefx.com/docs/houdini/nodes/sop/polybevel.html)
  for edge eligibility, connected bevel networks, profile and collision
  policy, point scaling, and generated group contracts;
- [Point Split](https://www.sidefx.com/docs/houdini/nodes/sop/splitpoints.html)
  for selected shared-point separation, vertex/primitive seam attributes,
  per-dimension tolerance, group seams, and optional point promotion;
- [Extract Centroid](https://www.sidefx.com/docs/houdini/nodes/sop/extractcentroid.html)
  for whole-detail, primitive, and piece center modes and output metadata;
- [Extract Point from Curve](https://www.sidefx.com/docs/houdini/nodes/sop/extractpointfromcurve.html)
  for constant, primitive-attribute, and current-time cut sources, extracted
  point/primitive payload patterns, and curve diagnostics;
- [Circle from Edges](https://www.sidefx.com/docs/houdini/nodes/sop/circlefromedges.html)
  for selected geometry boundaries, optional radius/scale, and transformed
  edge-group output;
- [Graph Color](https://www.sidefx.com/docs/houdini/nodes/sop/graphcolor.html)
  for point/primitive connectivity, stable sorting, and detail workset arrays;
- [Point Generate](https://www.sidefx.com/docs/houdini/nodes/sop/pointgenerate.html)
  for origin and connected emission modes, count/probability attributes, input
  retention, payload patterns, generated groups, and source metadata;
- [Point Replicate](https://www.sidefx.com/docs/houdini/nodes/sop/pointreplicate.html)
  for input-group cloud replication, count scaling, local shapes, standard
  copy transforms, stable IDs/rest noise, velocity controls, payload,
  provenance, custom-shape ancestry, and generated groups;
- [Labs Measure Curvature](https://www.sidefx.com/docs/houdini/nodes/sop/labs--measure_curvature-3.0.html)
  for the artist-facing mean/Gaussian/principal curvature intent, smoothing,
  visualization, and downstream scatter/reduction use;
- [Measure 2.0](https://www.sidefx.com/docs/houdini/nodes/sop/measure.html)
  for scalar/vector attribute Laplacian intent, integrated versus area-divided
  output, and smoothing/sharpening use;
- [Laplacian](https://www.sidefx.com/docs/houdini/nodes/sop/laplacian.html)
  for cotangent and uniform/Tutte weighting distinctions and the explicitly
  separate sparse-matrix/linear-solver boundary;
- [Garland and Heckbert's QEM paper](https://www.cs.cmu.edu/~garland/Papers/quadrics.pdf)
  for compact ten-coefficient face-plane quadrics and iterative contraction,
  and [Papageorgiou et al.](https://doi.org/10.1371/journal.pone.0255832)
  for independent-region parallel contraction structure;
- [SOP NodeVerb](https://www.sidefx.com/docs/hdk/class_s_o_p___node_verb.html)
  and [CookParms](https://www.sidefx.com/docs/hdk/class_s_o_p___node_verb_1_1_cook_parms.html)
  for UI-independent operators, cook modes, parameters, inputs, and scratch;
- [Groups from Name](https://www.sidefx.com/docs/houdini/nodes/sop/groupsfromname.html)
  for point/primitive text classification, empty values, prefix/conflict/name
  policy, and its explicit high-group-count memory warning;
- [encodeattrib](https://www.sidefx.com/docs/houdini/vex/functions/encodeattrib.html)
  for the reversible `xn__` policy that remains a documented compatibility gap;
- [Name](https://www.sidefx.com/docs/houdini/nodes/sop/name.html) for the
  efficient text-partition representation, group-mask inverse conversion, and
  its explicitly undefined multi-group overlap behavior;
- [Group Create](https://www.sidefx.com/docs/houdini/nodes/sop/groupcreate.html)
  for random-chance, seed-attribute, owner, base, initial-merge,
  direction/spread/opposite-normal, vertex exclusion, and additive non-planar
  semantics, its distinct backface subtraction policy, and pairwise
  shared-point edge-angle selection and point-seeded edge depth;
- [compiled blocks](https://www.sidefx.com/docs/houdini/model/compile.html)
  for static parameter resolution, in-place chains, and piece parallelism;
- [Attribute Wrangle](https://www.sidefx.com/docs/houdini/nodes/sop/attribwrangle.html),
  [Scatter](https://www.sidefx.com/docs/houdini/nodes/sop/scatter.html),
  [Smooth 2.0](https://www.sidefx.com/docs/houdini/nodes/sop/smooth.html),
  [Ray](https://www.sidefx.com/docs/houdini/nodes/sop/ray.html),
  [Fuse 2.0](https://www.sidefx.com/docs/houdini/nodes/sop/fuse.html),
  [Bound](https://www.sidefx.com/docs/houdini/nodes/sop/bound.html),
  [Match Size](https://www.sidefx.com/docs/houdini/nodes/sop/matchsize.html),
  [Copy to Points](https://www.sidefx.com/docs/houdini/nodes/sop/copytopoints-.html),
  and [Sweep](https://www.sidefx.com/docs/houdini/nodes/sop/sweep.html) for the
  initial artist-facing vocabulary;
- [Line](https://www.sidefx.com/docs/houdini/nodes/sop/line.html),
  [Clean](https://www.sidefx.com/docs/houdini/nodes/sop/clean.html),
  [Poly Extrude](https://www.sidefx.com/docs/houdini/nodes/sop/polyextrude.html),
  [Revolve](https://www.sidefx.com/docs/houdini/nodes/sop/revolve.html),
  [PolyFill](https://www.sidefx.com/docs/houdini/nodes/sop/polyfill.html),
  [PolyWire](https://www.sidefx.com/docs/houdini/nodes/sop/polywire.html),
  [Convert Line](https://www.sidefx.com/docs/houdini/nodes/sop/convertline.html),
  [PolyPath](https://www.sidefx.com/docs/houdini/nodes/sop/polypath.html),
  [Carve](https://www.sidefx.com/docs/houdini/nodes/sop/carve.html),
  [Join](https://www.sidefx.com/docs/houdini/nodes/sop/join.html),
  [PolyLoft](https://www.sidefx.com/docs/houdini/nodes/sop/polyloft.html),
  [Skin](https://www.sidefx.com/docs/houdini/nodes/sop/skin.html),
  [PolyBridge](https://www.sidefx.com/docs/houdini/nodes/sop/polybridge.html), and
  [Ends](https://www.sidefx.com/docs/houdini/nodes/sop/ends.html) for the
  polygon-curve subset and its explicit spline/surface boundary;
- [UV Auto Seam](https://www.sidefx.com/docs/houdini/nodes/sop/uvautoseam.html),
  [UV Flatten](https://www.sidefx.com/docs/houdini/nodes/sop/uvflatten.html),
  and [Labs UV Unitize](https://www.sidefx.com/docs/houdini/nodes/sop/labs--uv_unitize.html)
  for seam/island graph vocabulary and unit-fit modes.
- [GA Edge Group](https://www.sidefx.com/docs/hdk/class_g_a___edge_group.html)
  and [Group](https://www.sidefx.com/docs/houdini/nodes/sop/group.html) for
  topology-affine edge identity and artist-facing incidence/angle filters.
- [Group Promote](https://www.sidefx.com/docs/houdini/nodes/sop/grouppromote.html)
  and [Group Expand](https://www.sidefx.com/docs/houdini/nodes/sop/groupexpand.html)
  for four-owner conversion, complete/shared-edge inclusion, signed topology
  steps, flood fill, adjacent-normal limits, attribute-defined connectivity,
  and collision containment/boundary policy.
- [Group Range](https://www.sidefx.com/docs/houdini/nodes/sop/grouprange.html),
  [Group Combine](https://www.sidefx.com/docs/houdini/nodes/sop/groupcombine.html),
  [Group Invert](https://www.sidefx.com/docs/houdini/nodes/sop/groupinvert.html),
  [Group Delete](https://www.sidefx.com/docs/houdini/nodes/sop/groupdelete.html),
  [Group Rename](https://www.sidefx.com/docs/houdini/nodes/sop/grouprename.html),
  and [Group Copy](https://www.sidefx.com/docs/houdini/nodes/sop/groupcopy.html)
  for named-group lifecycle, boolean algebra, range, remapping, and documented
  conflict behavior.

The OCaml API is intentionally smaller and strongly typed. It adopts documented
workflow concepts without copying Houdini source or wire formats.

## Packed outputs

A cook result may be packed: `Session.output.instances = Some transforms`
means "draw `geometry` once per transform". Copy to Points with **Pack and
instance** produces one (the source once, `Rdk.Instance_copy.copy_transforms`
of the targets: the same pscale/scale/orient/N/up/rot/trans/pivot/transform
rules as the unpacked copy). Packed instances stay outside `Rdk.Geometry.t`:
a node that consumes a packed result receives it materialized (the explicit
boundary), memoized per packed output so downstream cache keys keep hitting.
Renderers draw packed results as instances (`Scene.D3.instances_array`, the
path tracer's instance structures), so editing target attributes re-uploads
transforms, never multiplied topology.

Switch selection is the integer `:input` parameter. Its inspector presents
input-dependent branch labels, while the parameter value, drive type and cook
key retain the integer index when those labels change.

Transform uses one declaration for TRS and Matrix modes. TRS remains the Lisp
default; orders, shear, pivot, inversion and group selection are editable fields.
Matrix mode exposes all sixteen matrix coefficients, with identity defaults.
Lisp and the editor share this declaration and its defaults.

Attribute Noise takes its arguments from its Lisp fields. Explicit
seed zero is the default; context-seed mode retains its stable label stream
and declares the Seed dependency. Sampling and range choices use flat enums,
with grouped Vec3 bounds plus separate fourth components for quaternion ranges.

Attribute Randomize's custom ramp, weighted numeric and weighted text
distributions are Lisp choices. Ramps use comma-separated position:value knots;
weighted tables use escaped tab-separated columns and newline-separated rows.
Explicit selections use owner/name fields alongside the owner-matched group
shorthand, and per-component limits supplement the existing scalar limits.
Fraction sampling ignores seed controls, matching the Lisp factory.

Attribute Interpolate shares Lisp's flat driver fields and defaults. Attribute
rules are an escaped owner/source/target table; blank rules allow pattern-only
transfer. Group patterns take precedence over exact names. Explicit weight
drivers require a positive threshold, and computed arrays require the
primitive/UVW driver with point or vertex numbers. Invalid controls fail at
construction and inspector edits return the validation error.

Attribute Transfer uses flat sampling and falloff controls with positional
source/target inputs. `use_names` selects an escaped one-column name table;
otherwise the existing attribute pattern applies. Explicit distance remains
the Lisp default, while Auto enables unbounded transfer and detail ownership.
Group patterns take precedence over exact names. Source vertex groups retain
their all/any triangle-corner selection controls.

Lisp-authored delegated cook diagnostics may use the mapping-only `with_error`
scope. Internal child cooking remains in the zero-retention Session and happens
before the protected scope exits; hints and ordered child trace survive the
Flow/SOP boundary. Cancellation and errors from already cooked public input
edges remain unchanged. Mapping text belongs to the Lisp mapper, never to an
operation-name switch in Flow or a private SOP callback.
