# GPU code generation from Rays Lisp

A shader printed from Rays Lisp must be as fast as a handwritten one. A target
that is 7x slower (`pathtrace.rays` against `pathtrace.metal`,
[benchmarks/pathtracer-lisp.md](cleanup/benchmarks/pathtracer-lisp.md)) can't
be shipped. This note says why that happens today and how the emitters must work.

## Why the generated kernel is slow

`lib/flow_shader/emit.ml` prints MSL from `Packed`, the **CPU block IR**. Packed
is designed for 1,024-lane blocks on a CPU, and each of its choices is right
there and wrong on a GPU thread:

| Packed (CPU block) choice | What it prints in MSL | GPU cost |
|---|---|---|
| Control flow as per-instruction guards (`Guarded`) | `float r=0; if(c0!=0&&c1==0){ r=...; }` around each instruction | No real branches. Every variable is declared at function scope and conditionally assigned, which keeps everything live and inflates register pressure. The compiler sees thousands of tiny ifs instead of a few regions. |
| One flat register file, `r0..rN`, never reused | 8,200 lines; the register limit had to be raised to 16,384 | Live ranges span the whole kernel, causing spills and low occupancy. |
| Every integer is a 63-bit `long` (`as_type<long>(ulong(x)<<1)>>1`) | 64-bit multiply, shift and compare for PCG, indices and loop counters | Apple GPUs run 64-bit integers several times slower than 32-bit. |
| Booleans are floats (`!=0.0f`) | Float compares and selects for every condition | Extra ALU work and registers. |
| Error parity with the CPU walker: a finite check after each op | About 1,100 `atomic_fetch_or_explicit(status, …)` calls on device memory | An atomic per op serializes the thread, fences the compiler, and blocks reordering and fusion. |
| Gathers read one scalar component at a time with a clamp | `as_type<float>(gather[clamp(int(i),0,max(int(u)-1,0))*w+c])`, repeated per component | No vector loads, and the bounds math is redone per field. |

The handwritten kernel uses structured `for`/`if` with early exits, a few
dozen live values, `uint` PCG and `float3` loads, and it never writes device
memory except the film.

## How it must work: a structured, typed kernel IR

This is the design used by naga (the wgpu shader translator, which takes one
IR to WGSL, SPIR-V, MSL, GLSL and HLSL) and by Futhark's GPU backends
(ImpCode kernels):

1. **Structured statements over an expression arena.** A kernel is a tree of
   statements (`Block`, `If {cond; then; else}`, `Loop {body; break_if}`,
   `Break`, `Continue`, `Return`, `Store`) whose operands are typed SSA
   expressions. SPIR-V *requires* structured control flow (`OpSelectionMerge`,
   `OpLoopMerge`), and MSL and WGSL are structured languages. Predication is
   a CPU backend decision: Packed if-converts the structured IR for its
   lanes, and the GPU printers print the structure.
2. **Types are machine types.** `bool`, `i32`, `u32` (wraparound),
   `f32`, and `vec2/3/4<f32>` for loads and stores. `i64`/`f64` exist only
   in the exact tier, and a device kernel only gets them where the program
   asks for them. A uint32 PCG prints as `uint` (MSL), `OpTypeInt 32 0`
   (SPIR-V) and `u32` (WGSL), with the same bits as the CPU.
3. **SSA values printed where they are defined, in the innermost scope.**
   A single-use expression is printed inline in its user, and a multi-use one
   becomes a `const` local in the scope where it is computed (naga's "baking"
   rule). Nothing is declared at function scope unless it is loop-carried.
   Register allocation is left to the Metal and SPIR-V compilers; the
   emitter's job is to not extend live ranges.
4. **Loops are loops.** Bounces, lights, panels and CDF searches are
   `for`/`while` with `break`. Unrolling is the static stage's job, and only
   for small known trip counts.
5. **Errors don't touch memory in the hot path.** The approximate tier,
   which every device kernel uses, guarantees valid *outputs*: a thread ORs
   problems into a local bit set and publishes it with one atomic at the end,
   or checks only its outputs. Per-operation error parity belongs to the
   exact tier, which stays on the CPU.
6. **Data has layout.** Uniforms are one `constant` struct. A record table
   (materials, lights, panels) is a `device const` array of structs or
   `float4`s, read with one vector load per record and indexed once.
7. **Specialization is static folding.** Scene facts (light count, panel
   count, instanced or not, no world map, material classes) fold before
   emission, so dead branches never reach the GPU. That's how a generated
   kernel beats a handwritten one with uniform branches.

One IR still feeds every target. Region becomes this structured IR,
Packed is its CPU lowering, and MSL, SPIR-V, WGSL and JS print it.

Compute printers preserve guarded explicit errors through the small status
record: bit 8 is `E_LIST_RANGE`, alongside nonfinite (1), division (2) and shift
(4) failures. Slice bounds use bit 16 and invalid slice lengths bit 32, both
decoded as their native `E_ARRAY_RANGE` message (length takes precedence when
multiple rows fail). `Flow.Packed_ops.error_status` supplies the explicit-error
bits to MSL, SPIR-V and WGSL. Invalid list and slice gathers stay guarded, and
the error instruction cannot inline away, including an empty slice. Native and
web read the same typed failures. Raster
placements refuse explicit errors: those stages have no status record.

Native `Flow_gpu.Host.stats` separates payload, status, count, validation and bounds
bytes. The compatibility `readback_bytes` is payload plus bounds. Resident-executor
payload reads and four-byte count reads are included, as are lazy kept-output
materialisations from worker domains (atomic accounting). Cached materialisations
and bounds queries add nothing. Status bytes are four per successful status read;
topology validation (36 bytes per mesh, 4 for the display index kernel) counts as
validation, never payload.

The native compute printer accepts selected scalar uint32 buffers for topology:
`uint32_outputs` selects output indices, `uint32_inputs` selects source indices,
and `uint32_tables` selects gathered uniform indices. The buffers remain one
four-byte word per element; default float32 inputs/outputs, binding numbers and
uniform layouts are unchanged. Integer loads/stores bypass float32 conversion.
Selected output values outside 0..2^32-1 set status bit 64 and return
`E_GPU_UINT32_RANGE`. Readback returns exact `Int_array` storage; planar reads
convert the decoded words directly to doubles. Uint32 inputs require explicitly
bound device buffers (`E_GPU_UINT32_BINDING` otherwise), or the internal
`Run.dispatch ~uint32_cpu:true` checked host-array upload. Implicit float32 held
arrays always refuse. `uint32_scalars` selects exact unsigned integer captures
and frame words without changing their uniform offsets. `Host.resident` has an
internal uint32 mode with separate site caches and device-generated integer
ranges; its float-only display borrow declines in that mode. Uint32 output ownership is still a
separate integration step: `Run.keep` refuses `E_GPU_UINT32_KEEP` rather than
handing a uint32 buffer to the float32 planar-holder path.

`Run.keep_uint32` transfers a typed output to an integer holder using the
existing bounded pool, retirement budget and close lifecycle. It never
registers a float binding. Settled workers decode raw uint32 words exactly,
cache integers and read payload once; an unread settled stream can be pinned
for a native consumer. A runner close preserves live holders by materialising
them unless discard was requested. Both holder codecs check dispatch status
before an initial-domain materialisation; failed uint32 outputs never become
readable geometry.

The neutral integer carrier preserves an extensible backend storage token.
`Run.integers` wraps the same UInt32 holder without a copy or new ownership;
`Run.pin_integers` recognises its native token and applies the existing pin
rules (creating domain, settled, unread, live, logical view length). Other
backend tokens decline. `Topology_kernel.resident` exposes the three checked
carrier lengths without forcing topology. No native buffer type reaches
Flow IR, SOP or RDK, and no second binding registry is introduced.

`triangles.rays` generates UInt32 display indices from positions and exact
topology streams, following host Newell projection and ordered quad ear
clipping at the closed-form triangle base `offsets[p] - 2*p`. Two passes reduce
per-group rejection counts to four status bytes. Ambiguous float decisions,
collapsed quads and unsupported topology return `E_GPU_DISPLAY_TRIANGLES`
for host fallback. `Host.triangle_indices` reuses the bounded display pool and
counts the read as validation, with no payload read or input upload. Native
index parity remains separate from Surface/sketch viewport qualification.

`Host.resident_scope` exposes the executor plus stream retention. It settles
before transferring a buffer, preserves actual counts, and detaches slot
ownership. A scope pin and deferred count-cell return keep retained inputs
readable by later regions until close. CPU reads use the retained holder cache.
Equal integer views share a holder; different prefix lengths over one generated
range share physical storage with separate logical counts, pins and cached
arrays. Their owner reads the largest live view once; empty views read nothing.
Shared views retire together through the existing pool's GC/pressure path.
Generated range buffers transfer out of the host's range cache too. The retained
uint32 and float streams remain unread across executor close.
`Run.adopt_uint32`/`adopt_float` use the existing holder machinery for these
completed, status-validated mapped buffers, without copying. Resident slots
and generated ranges allocate mapped storage; `Backend.mapped_view` retrieves
its existing owned view. Unsigned materialisation invalidates that view and
reads uint32 words directly in a small C accessor, preserving NaN-shaped bits
without calling initial-domain-only `read_buffer` from a worker. Adopted
buffers rejoin the mapped pool after use. One lazy ownership runner per host
tracks their lifetime and materialisation counters.

`Flow_ir.Gpu.backend.residency` now carries this scope without native handles:
the executor, owned integer/float retention and a topology-validation callback
over resident Flow values. `Executor.force ?resident` forwards it through both
normal and cached root execution. The workspace GPU proxy routes all these
callbacks to its owner, including scope cleanup; workers only read settled
owned holders. Exact precision declines before acquiring a native owner.
When the placement below selects the device (the test-only Qualification policy forces it), shared mesh cooking validates resident fields
before transferring them into deferred Float3 positions and Topology. CPU
consumers force the existing carriers. Unsupported stream shapes materialise
before the scope closes and pass through the ordinary mesh constructor.
Fused N/Cd writes stay resident when their own measured comparison selects the device.

A static `sop/mesh_planes` cooks on the CPU, once: planes never stay resident (`mesh_resident` keeps Vec3
points), so its device route could only read the topology back. Mesh construction (`sop/mesh`, a frame-driven
`sop/mesh_planes`) and `flow.geometry_select` enter the resident scope by a
whole-chain measured comparison (`Lower.Chain`): per authored site and output size class (log2 of the
output's floats) each route keeps the cheaper of two measured cook rates (seconds per float, a route's first
cook pays one-time preparation). The CPU alternative adds the upload of the mesh it displays and the device
one the host materialisation known CPU readers force, both priced by `Cost.readback` at the site's last
output size, and `Cost.choose` decides with a recorded reason. Unknown consumers stay CPU
(`missing_consumer`); a site without both routes runs the device twice (its mesh is the real result), then samples the CPU
under `Cost.probe_budget` (twice the device's cost, at least 20 ms), because a CPU cook can cost minutes where
the device takes milliseconds. A sample past its budget is abandoned through `Eval.with_interrupt`, which
reaches worker chunks, is recorded at the budget and the frame cooks on the device; it costs a few device
cooks, once. A body that fails on the device falls back to the CPU. Until it has numbers it reports (`no_cost_evidence`). A write that reads no attribute and deletes none forwards its input
unread in the consumer census, and so does `flow.delete_attr` without a guard. A write over input the device
holds takes no CPU sample (it would read that input back); a write with a declared CPU reader that the cost table
prices under its device dispatch samples the CPU first, so a small capture is not dispatched and read back. A displayed root with a device owner has no CPU reader unless probed; a
write beside host positions counts one reader, and beside resident positions its CPU alternative is charged the
measured host display sink, so mesh and writes cannot split into a mixed mesh the display must convert.

Stateless structured geometry selection uses the same resident scope.
Core offers raw `Host_source` arguments to the host before its usual CPU
boundary; `sop/mesh` accepts them through the shared resident constructor and
registers its geometry handle. Other sources decline the offer and materialise
normally. Invalid returned types and unresolved markers are refused. A failed
device attempt closes its scope and retries the selection on CPU unless the
cook was cancelled. Constant integer-array descriptors use the existing
validated descriptor decoder and resident fill callback; no staging array is
built for a device-owned `array/int` kind stream.

Frame-dependent resident records also compile as one cached Core program.
Structured-value lowering makes captured array producers explicit and reuses
the drawing filter's mask/compaction path. The prepared program, including its
region identities, stays stable across forces; the general dispatcher still
supplies each force's state, resolver and strict policy. This lets counted
consumers learn capacities and replay the complete producer chain on overflow.
Host mesh eligibility uses a nonmaterialising count check, which settles work
without switching into the post-fold phase or disabling later capacity history.

Work meant for the device that runs on the CPU reports to `Flow_ir.Fallback` with its site and reason: a
refused kernel (interpreted element by element), a region the device passes over, a record that does not lower, a
refused display. The editor's status line shows the first; a measured choice of the CPU is not a fallback, nor is a
region under 256 elements. It resets when a document lowers.

A chain of point writes over a `sop/mesh` nothing else reads is folded into the mesh's
record (`Lower.fold_mesh_writes`), so their maps share its producers. As separate writes each would
evaluate a captured compacted list on the CPU (`Packed.Host.prepare`) and upload it every frame.
A static mesh under a frame-driven write keeps its own node, so it does not cook again each frame.

Sibling Zip regions with unknown counts can fuse when their ordered immutable
source sequences agree. Direct source identity or the existing residual
structural key establishes equality, with matching authored declarations and
operator implementations; stateful residuals decline this proof. Rewritten
captures may therefore share a count despite fresh residual IDs. No source is
forced to establish it. Different dynamic count expressions or declarations
retain separate regions. The frozen sketch's N/Cd writer uses this route.

A resident Executor failure is atomic across the whole value. A failed field's
Core execution closes its scope, invalidating placeholders from earlier fields
too; the Executor propagates that failure out of the record walk, closes the
scope and retries the entire value on CPU under the requested strict policy.
It must not resume a later field's reference fallback while retaining earlier
placeholders. The native publication test poisons float placeholders with NaN
to make any accidental host use deterministic, including signed-index declines.

The same source comparison compacts duplicate immutable inputs after fusion.
Preparation evaluates index counts and uniform roots through the existing
packed evaluator. When a root cannot compile, its reference evaluation offers
stateless nested kernels back to that evaluator, using the current resolver.
Stateful or unsupported kernels retain reference semantics. This avoids
walking nested maps once per element merely to obtain a count or uniform;
there is no additional cache or alternate evaluator.

Constant integer arrays use the same device range kernel: an optional fill word
selects either index generation or an exact uint32 constant. `Core_exec` offers
validated `array/int` counts/values through the resident `fill` callback; signed
values outside uint32 decline to the ordinary CPU semantics. No count-sized
CPU staging array is uploaded. The workspace worker proxy routes fill to its
owner, and the output uses ordinary retained-slot ownership. Capacity guards
reject an oversized fill before dispatch.

`shaders/compact.rays` supplies float32 and uint32 entry points through one
hygienic macro for the scan/count/scatter algorithm. `Run.compact ~uint32:true`
reads a uint32 mask and stores exact uint32 indices; count cells/scratch keep
their layout. This native storage specialization is not a web SOP target.
The original float entry remains shared with web export.

`shaders/topology.rays` validates exact uint32 vertex, offset and kind streams
in one 256-lane workgroup. Lanes scan disjoint rows, then nine uniform group
scans reduce rule failures into a 36-byte status record. It checks the initial
zero, terminal offset, kind/offset lengths, point references, offset ordering,
kind range, minimum corners and a caller-supplied polygon-size ceiling.
`Host.validate_topology` caches one status buffer; successful reads, including
rejected topology, increment `validation_readback_bytes` rather than payload.
Buffer capacities are checked before encoding; no input ownership transfers.
Deferred mesh publication runs it once per cook; a failure is `E_TOPOLOGY` and nothing is published.

## Raster stages (P9)

The renderer's raster pipelines are `lib/renderer/shaders/NAME.rays`, two defns per pipeline,
`NAME-vertex` and `NAME-fragment`, printed by `Flow_shader.Raster` into `NAME.metal`
at build time (below). A stage is a map: the vertex stage over vertex ids, the fragment stage over
fragments. It compiles to one Packed program whose outputs are the leaves of the entry's result
record (a record result is one output per numeric leaf), printed by `Emit`'s instruction printer in a
raster placement (`Emit.stage_body`): elements are `vid` or interpolated inputs read where used,
gathers read the bound buffers without clamps, and there is no status word.

- A stage's `(array float)` and `(array int)` parameters are the renderer's buffers. The vertex
  stage's bind in their order; a fragment buffer named like a vertex one shares its binding, any other
  follows the vertex stage's (one binding namespace, as OGPU's layouts). A float column in a buffer of
  words is read with `gpu/float-bits` (an Int's low 32 bits as a float: `as_type<float>`, `OpBitcast`,
  `bitcast<f32>`), so each buffer is bound once. Textures take the bindings the author names after them. The vertex
  entry's first parameter is the vertex id.
- The vertex result has `:position` (clip space), optionally `:point-size`, and the varyings; those
  under `:flat` are not interpolated. The fragment's other parameters are the varyings, by name, and its result is a colour
  or `{:color c :discard b}`.
- Stage intrinsics are `gpu/float-bits` (either stage), `gpu/fwidth`, `gpu/sample`, `gpu/sample-level` (an explicit mip level),
  `gpu/read` (a whole texel at integer coordinates, no sampler) and `gpu/texture-size` (Region
  `Stage`); texture `t` is sampled with sampler `t + 1`. An argument-buffer pipeline
  (`Raster.msl ~arguments:true`, Scene2's quad) holds texture `t` and its sampler in the argument buffer at
  buffer `t`. Built-in facts read like frame facts: `(gpu/instance-id)` in a vertex,
  `(gpu/front-facing)`, `(gpu/depth)` (window depth) and `(gpu/fragment-x)`/`(gpu/fragment-y)` (pixel
  position) in a fragment. The CPU and every other target
  refuse them all (`E_GPU_STAGE`).
- Blending, depth, culling and winding are OGPU pipeline state, never shader code; the renderer sets
  them per pipeline family (3D families wind counter-clockwise), never from which slots a draw binds.
- Pixels equal to an MSL intrinsic need its rounding, written out: MSL's `dot` is the fused chain
  `fma(z, z', fma(y, y', x*x'))`, `normalize(v)` is `v * rsqrt(dot(v, v))` (`rsqrt` is a Lisp
  unary), `cross` is `fma(a.y, b.z, -(a.z*b.y))` per component, and the compiler contracts `a + b*c`
  to `fma(b, c, a)`. Flow's `dot`, `length` and `normalize` keep the CPU's unfused sums. GPU `pow` and
  `exp2` are not exact on integer powers of two: scaling by one is `ldexp` (`shaders/scene.rays`).
- The same programs print as Vulkan SPIR-V (`Spirv.stage`, `Raster.spirv`): one module per stage,
  entry `main`, varyings at `Location`s in the vertex result's leaf order (`Flat` under `:flat`),
  `VertexIndex`/`Position` built-ins, buffers as storage runtime arrays at their bindings, texture `t`
  and sampler `t + 1` as separate descriptors, `OpFwidth`, `OpImageSampleImplicitLod`,
  `OpImageQuerySizeLod`, and `OpKill` after the stores for a discard. They are printed beside
  it as `NAME.{vert,frag}.spv` and validated with `spirv-val --target-env vulkan1.1`.
- And as WGSL (`Raster.stages`, `Flow_web.Wgsl.raster`, printed by the fast kernel's instruction
  printer in vertex and fragment placements): several pipelines in one module, every resource in group 0
  at its binding (`array<u32>` when any stage reads it as words, its floats then their bits), texture `t`
  and sampler `t + 1`, varyings at `@location`s in the vertex result's leaf order
  (`@interpolate(flat)` under `:flat`), `discard` after the stores. The vertex id is `vertex_index`, or for
  a quad pipeline `instance_index * 4 + corner` of a non-indexed six-vertex draw; a fragment's `fwidth`
  of a varying is read once before any branch (WGSL's derivative uniformity). The browser compiles it in
  `@tools/qualification-web`; nothing compiles it in `@runtest`.

## Compute stages, the presentation blit and the trace helpers

No shader text is written by hand in `lib/` outside the printers (P9.G1), and no printed shader is
checked in. The printers are `flow_shader` (`Emit`, `Spirv`, `Raster`, over
`flow`, `flow_ir` and `ogpu_core` only); `lib/flow_shader/print_shader.exe OWNER` prints an owner's
`shaders/NAME.rays` into `NAME.metal` and `NAME.{vert,frag,comp}.spv` in its build directory, run by a
rule in the owner's dune (`renderer`, `ogpu_metal`, `flow_gpu`), whose embedding rule reads the
`.metal`. `print_shader.exe web`, run by `web_export`'s dune, prints `compact.rays` and `dispatch.rays`
through `Flow_web.Wgsl.compute` into `compact.wgsl` and `dispatch.wgsl`, which `web_export` embeds and
passes to `Flow_web.Kernel_table.table` (`flow_web` holds the printer, so it cannot embed them), and the
presenter's pipelines (`lib/renderer/shaders/marks.rays`: marks, geometry and quad, the ones native
prints) through `Raster.stages` and `Flow_web.Wgsl.raster` into
`marks.wgsl`, which the export serves as `runtime/marks.wgsl` (marks.md, "The Marks pipeline family"). It is a build-time tool: no
library links it, so the owners' library graph is unchanged.
`@lib/flow_gpu/runtest` (`test_raster.exe`) validates the printed SPIR-V with `spirv-val` when it is
installed.

- The Metal backend's presentation blit is `lib/ogpu_metal/shaders/present.rays`, a raster pipeline
  printed into `present.metal` (embedded by `Presentation`; the library never links Flow, only its build
  runs the printer) and `present.{vert,frag}.spv`.
- A **compute stage** is one workgroup, or with `grid` any number (`Raster.compute_msl`,
  `Raster.compute_spirv`; `Flow_web.Wgsl.compute` is always grid): the entry's first parameter is the
  lane (with `grid` its index in the dispatch; the scan stays the group's), its array parameters the bound
  buffers in order, its last the group size (a constant of the target: 1,024 on Metal, 256 on Vulkan and
  WebGPU); its result is discarded. `(gpu/group-scan x)` is the exclusive prefix sum of an Int over the
  group with its total, in uniform control flow (a Hillis-Steele scan in threadgroup / `Workgroup` /
  `var<workgroup>` memory between `threadgroup_barrier` / `OpControlBarrier` / `workgroupBarrier`);
  `(gpu/store buffer index value)` and `gpu/store-int` write a bound buffer and are 1. The resident fold's
  compaction is `lib/flow_gpu/shaders/compact.rays`, printed into `compact.metal`,
  `compact.comp.spv` and the web's `compact.wgsl`. It runs three dispatches over
  blocks of the group size: each group counts its block, one group scans the block counts into first
  slots (and writes the total), each group scatters its block's kept indices from its slot; the block
  scratch comes from the capacity, the real count from count-in on the device, and so do the count and
  scatter passes' group counts (below).
- A **counted dispatch** (a resident region or compaction whose count is a device count cell x mul / div)
  sizes its grid on the device: `lib/flow_gpu/shaders/dispatch.rays` (one thread, printed into
  `dispatch.metal`, `dispatch.comp.spv` and the web's `dispatch.wgsl`)
  writes max(1, ceil(n / group)), 1, 1 for n = min(capacity, cell[0] x mul / div) into a 12-byte buffer,
  encoded before the dispatch in the same pass, which then runs as `Backend.dispatch_threadgroups_indirect`
  (Metal `dispatchThreadgroupsWithIndirectBuffer:`) or `dispatchWorkgroupsIndirect`. At least one group
  runs, so thread 0 writes the consumer's count record also for n = 0; every kernel keeps its `i >= n`
  guard for the last, partial group. The capacity sizes the host's placeholders and the device buffers.
  Narrowing reads a store's and a scan's operands as uint words, so the stage counts in 32 bits.
- A **slot** is a resident region site's call in one fold run (`Host.resident`, web `resident.js`;
  64 a site, the 32 latest sites). Inside the loop a call takes the site's first slot none of the
  draw's locals holds an array of (ping-pong: a level reads the last one's, so two take turns, each
  sized by the largest count it held), giving back at once what its last use made but its count
  cell, which the loop's end still checks; after the loop a call takes its site's next slot. It
  owns its region's output and count cell, its
  compactions' and retypings' buffers and the placeholders, which come back to it as the run closes and
  serve the next draw's run; what it kept and a run did not take (a size it outgrew) is freed then, and a
  site leaving the host frees its slots. A counted region whose slot has history takes the slot's
  capacity, the smallest power of two above the largest count it has held (never shrinking while the
  site lives), instead of its bound (the dispatch stops at the lesser), and no count is waited for. The
  counts become history once the fold's device work is done: native reads them at close, after the
  wait the run already made; the web copies them at the loop's end and records them when the map
  resolves, never awaited. Without history (a new site, the first draw) the capacity is the bound, and
  past 256K a native fold reads the device count once (one wait) and takes it as the bound (the web past
  4M only, 65,535 groups of 64 being a dispatch's reach); past 4M on the web and 64M natively, where Metal dispatches a grid of any size, a region settles on the host.
- A slot's count that reaches a capacity below its bound may have been cut (the kernel stops at its
  capacity). Before the host reads any resident array, `finish` reads every such count (after the wait
  the read makes anyway) and raises `Core_exec.Resident_overflow` (web: throws `resident.overflow`): the
  loop's writes are restored and it runs again, its regions from the first cut one on at their bounds.
  Kernels are deterministic, so the regions before it repeat their uncut counts; were one cut, the
  rerun would start earlier, so the reruns end. A cut run still records its counts (a cut one doubles
  its slot) and the rerun the true ones, so a growing count costs one rerun per power of two.
- `scene/trace` stays a host query on the CPU (`Flow.Ray_host`). A traced kernel instead inlines
  `lib/flow_shader/trace.rays` (`Flow_shader.Trace`): `trace-hit` (or `trace-occluded` for a ray whose only
  read field is its hit), compiled once a scene variant (instanced, motion, strands, spheres) and spliced
  into the region in place of the host's fields. The triangle fetch, face normal, instance and motion
  transforms, the strand capsule and the sphere loop are Lisp; the printers emit only the ray queries
  (`gpu/trace-triangles`, `gpu/trace-any`, `gpu/trace-curves`, `gpu/trace-curves-any`: a Metal
  `intersector`, SPIR-V `OpRayQuery*`) and the scene table reads (`gpu/scene-*`). A scene without spheres
  folds their tests (`Emit.kernel ?spheres`). Metal reflects only the tables a kernel reads, so its
  interface lists those.

- `noise3` on Metal and SPIR-V inlines `lib/flow_shader/noise.rays` (`Flow_shader.Noise`): `noise-fbm`,
  compiled once an octave count and spliced at each `Noise3` after the GPU refusals, its permutation
  reads the seed's 512 entries of the table binding (`Region.Noise_table`, an Int in [0, 255] that
  narrows). An exact kernel does not check the helper's operations finite, only the register that
  replaces the `Noise3` (a nonfinite coordinate makes it NaN), so the helper prints as it does in a fast
  kernel; the outputs are bit-identical to the former handwritten `fbm`. A float rounded with `floor`,
  `ceil`, `round` or `trunc` and read as a float prints in float arithmetic, not through a 64-bit integer.
  An integer-valued `mod` prints inline (`b==0?0:(a%b+b)%b`), so no MSL prelude remains in `lib/`.
  WGSL still prints its `wfbm` and 64-bit integer helpers (outside P9.G1).

## Gates

Nothing gets slower, on either target.

- **GPU time:** A/B against the handwritten reference in one process,
  interleaved, at least 5 pairs, with the machine load recorded. The
  generated kernel must be faster, not just level, because static
  specialization folds what the handwritten kernel branches on at run time.
- **CPU time:** the structured IR's CPU lowering (Packed if-conversion,
  blocks, domains) must be no slower than today. That covers `tools/bench_core.exe`
  (scalar and kernels), the Flow IR and marks benchmarks, and the CPU walker
  run of the same `.rays` kernels, measured the same interleaved way. The
  open scalar regression in
  [benchmarks/scalar-core.md](cleanup/benchmarks/scalar-core.md) is part of
  this gate, not a separate item. CPU results stay bit-identical at one and
  multiple domains.
- **Static shape, checked by tests:** no atomics inside loops and at most one
  per kernel; no `long` or `ulong` in a fast kernel unless the program uses
  64-bit integers; the line count of the emitted kernel within 2x of its
  handwritten reference.
- **Register pressure:** `MTLComputePipelineState.maxTotalThreadsPerThreadgroup`
  is recorded for each benchmark kernel. When a kernel uses more registers,
  Metal lowers this number, so a drop is a regression.
- **Film:** RMSE ≤ 1e-4 against the reference after N spp, with the CPU
  walker as the oracle.

## References

- naga, the wgpu shader translator: arena-based typed IR; MSL, SPIR-V,
  WGSL, GLSL and HLSL backends ([gogpu/naga](https://github.com/gogpu/naga),
  [Meganeura, arXiv 2608.01563](https://arxiv.org/pdf/2608.01563)).
- Futhark: imperative kernel IR (ImpCode) lowered from a functional
  language ([student report](https://www.futhark-lang.org/student-projects/steffen-msc-project.pdf),
  [PLDI 2017](https://researchprofiles.ku.dk/en/publications/futhark-purely-functional-gpu-programming-with-nested-parallelism/)).
- Predication vs branches on SIMT hardware: if-conversion is a late,
  machine-level choice
  ([NVIDIA forum](https://forums.developer.nvidia.com/t/solved-ptx-isa-predicated-execution-and-the-warp-divergence-issue/31957)).
- 64-bit integers on Apple GPUs are much slower than 32-bit
  ([Apple developer forum](https://developer.apple.com/forums/thread/718737)).
