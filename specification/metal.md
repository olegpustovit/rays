# Metal binding contract

`rays.metal` is the foundational Apple-Silicon Metal binding under
`ogpu_metal`. It does not depend on SDL3, OGPU, the runtime, Rays, or UI
libraries. The runtime obtains a Metal layer through OGPU; the binding itself
never owns an SDL window. There is no CPU, browser, or OpenGL fallback. Web export is a
separate compiler target emitted from a Lisp workspace, not a fallback: the native program never selects it.

## Build and registry

`lib/metal/discover.ml` rejects an installed macOS SDK older than 26.0 before
writing framework link flags. `lib/metal/build_bridge.ml` compiles Objective-C++
with ARC, a macOS 14 deployment target, and `-Wall -Wextra -Werror`. The build
uses the installed public SDK as the type and availability oracle. It needs the
Metal, QuartzCore, CoreGraphics, IOSurface, and Foundation frameworks;
Rays does not ship their headers or binaries.

`lib/metal/gen/registry.ml` is the source for generated binding declarations.
SDK entries name their Metal symbol, OCaml name, type, availability, and OGPU
capability. Custom `Native` entries name their existing primitive symbols,
exact OCaml signature and the reason their implementation remains native.
Dune writes `metal_gen.ml`, `metal_gen.mli`, and
`metal_gen_stubs.inc` plus feature-map checks only under `_build`. Its 10-entry
`Caps.feature` map names the Metal SDK types backing each capability. The map rejects
duplicates and missing entries, and the bridge compiles its type checks against
the installed SDK. Four retained enum families are
checked against SDK constants with native `static_assert`s. `Method` entries
declare any selector over scalar, enum, NSString and handle arguments, with
an optional trailing `NSError**` and a scalar, string or owned-handle result;
`Property` entries declare getters and setters; `Class_method` entries declare
typed class sends, including descriptor factories. Each becomes an external in
`Metal_raw.Registry` and a stub with a typed Objective-C receiver and direct
send, explicit availability guards, checked scalar conversion, and a
result-returning native exception boundary. The generator rejects duplicate
names/selectors, selector arity mismatches, and registry calls without a
safe-layer or raw-adapter reference; it parses both OCaml sources so a comment
cannot satisfy the reference check. Raw externals outside the registry are a
generation error. `Nsuint_int` and fixed `Tuple` arguments preserve the original
machine-int index and tuple ABI without introducing boxed integer conversions.
Unit methods can explicitly preserve their former pool-free success path;
string/error temporaries require a pool, and exceptions always copy diagnostics
inside a pool.

Blocks, callbacks, descriptor graphs, handle arrays and ownership transfer
remain implemented in `metal_bridge.mm`, with their declarations generated
from `Native` entries. Shared positional ABI types live in the private
`metal_raw_types.ml`; `metal_raw.ml` contains compatibility adapters over the
generated registry. No whole-SDK inventory participates in the build.
The registry declares `MTLSize` as a fixed scalar
record, emits its OCaml type, and checks its SDK field types. The safe
compute-pipeline `size3` re-exports that shape without another allocation.
The capability map covers every known `Caps.feature`, and OGPU conformance
checks runtime capability truth.

The registry migration audited all 187 formerly handwritten raw externals:
53 implementations now use generated calls, and 134 retain custom native
implementations with recorded reasons. The latter include hardware probes,
checked descriptor graphs, heterogeneous allocation/resource handles, native
memory, blocking waits, callbacks and compound snapshots. Some explicitly
record future record-adapter or property-path lowering opportunities. The
codemod removed the replaced stubs, unused macro families and compiler-reported
unused helpers; public safe signatures did not change.

Inspect the current inventory with
`dune exec tools/codemod/codemod.exe -- metal-registry --audit`.
The migration command (`metal-registry --apply`) parses the old OCaml ABI or
lowers registered native entries with audited recipes. Follow it with
`dead-stubs lib/metal/metal_bridge.mm lib tools test`,
`metal-registry --drop-unused-macros`, and `drop-c-unused lib/metal`.
Generator and codemod self-checks run under their `runtest` aliases.

Compute encoding was measured on Apple M1, one domain, the dev profile and
seven samples, with allocation counters and native completion outside the
timed loop. Before/after medians: 20,000 buffer binds, 3.347/3.392 ms and
72.00/72.00 bytes per call; 5,000 one-thread dispatches, 1.091/1.067 ms and
80.02/80.02 bytes per call, from a registry benchmark since removed. The benchmark checked the
kernel's output and released its owned resources; these are encoding costs, not GPU
throughput measurements.

## Safe layer and ownership

`Metal` exposes opaque handles and typed `result` errors. Before a native call,
it validates live state, same-device ownership, ranges, alignment, cardinality,
encoder state, and capability where applicable. Expected rejection does not
reach an Objective-C update method. Unknown native failures become typed
`Native_error`; Metal unavailability is an explicit `Unsupported` or startup
error rather than a no-op. Public `.mli` files hide raw values.

Safe validation chains use the private result operator `let*`. The compiler
codemod `tools/codemod/codemod.exe result-bind lib/metal/metal.ml` converts
plain propagation and `Result.bind` callbacks while preserving explicit cleanup
branches. Its self-check compares compiled success, failure, guard and cleanup
behavior; `result-bind --verify BEFORE.ml AFTER.ml PPX.exe` compares the whole
compiler tree after normalizing the migrated syntax. Only changed value
definitions are formatted. The Metal build runs `ppx/result_bind/result_bind_ppx.exe` before
typing to lower the operator and its native-error adapter to matches. This
avoids continuation allocations in the installed non-Flambda OCaml compiler;
public signatures and validation/ownership policy remain handwritten.
It also expands literal `on_main operation (fun () -> ...)` callbacks at
compile time. The resulting match calls the same `before_main` guard and
release-queue drain before the body. Dynamic callbacks and operation expressions
with effects retain the ordinary helper, preserving their evaluation order.

On Apple M1, one domain and the dev profile, six alternating before/after
process pairs each took seven samples of the same encoding benchmark. Medians
of those process medians were 3.335/3.229 ms for 20,000 buffer binds and
1.065/1.081 ms for 5,000 dispatches, with unchanged 72.00 and 80.02 bytes per
call respectively. Dispatch process medians ranged from 0.999–1.097 ms before
and 1.046–1.135 ms after. These measurements used
a registry benchmark since removed; native completion and exact output checks
run outside the timed loop.

The callback expansion was measured separately against the result-syntax
version above, again with six alternating pairs of seven-sample processes.
Before/after medians were 3.376/3.321 ms for buffer binds and 1.079/1.062 ms
for dispatches. Allocation fell from 72.00 to 16.00 bytes per buffer bind and
from 80.02 to 32.02 bytes per dispatch. The small timing differences remain
within the observed process variation; the allocation reduction is repeatable.

Native operations run on OCaml domain zero and the platform main executor. A
call from another domain returns `Wrong_domain`. The only any-domain
finalizer action enqueues an opaque retained pointer in the (unbounded)
release queue; every safe domain-zero entry drains it inside an autorelease
pool, so no Metal object is released on a GC domain.

An OCaml program has no Cocoa run loop, so no pool encloses a stub and
nothing would ever drain the thread's root autorelease page. Every stub that
messages Objective-C therefore opens its own pool, and handle lookups return
their object retained instead of autoreleased. Until 2026-10-11 the drain, the
buffer and texture copies, the render-pass setters and the generated pool-free
calls did not: each frame left its command buffer, encoder, pass descriptor,
drawable and written buffers retained for the life of the process (4.0 KiB
of footprint per frame, 0 after; see [performance.md](performance.md)).
`runtime_autorelease` under `@runtest-native` runs lifecycles and frames with
`OBJC_DEBUG_MISSING_POOLS` and fails on any report from the initial thread.

Handles track generation, destruction, device identity, and parent dependents.
Explicit destroy is idempotent; stale use, cross-device binding, or parent
destruction with live children fails before native access. Submission resources
remain retained through completion. A blocking wait releases the OCaml runtime
lock while keeping its native command handle rooted. Copied strings, arrays,
reflection metadata, and asynchronous callback roots do not escape their
native owners. `Metal.Release_queue.stats` reports handle and queue accounting
for lifecycle checks.

The safe API covers device and capability queries, buffers and mapped ranges,
textures, samplers, libraries and functions, compute and render pipelines,
command encoders and buffers, acceleration structures and intersection function
tables, and presentation: what `ogpu_metal` calls, no more. Metal 4
operations check their availability before use. `ogpu_metal` owns the portable
capability profile and returns typed `Unsupported` for unavailable features.

## Verification and extension

The focused binding loop is `dune build @lib/metal/runtest`; adapter behavior is
covered by `dune build @lib/ogpu_metal/runtest`. `dune build @check` typechecks,
and `dune build @all` links all targets. The native bridge is compiled under
`-Werror` during these builds. Safe tests exercise success and rejection,
resource retention, exact readback, and zero live-handle deltas. The ownership
stress runs measured handle cycles in isolated workers; presentation
fixtures exercise native lifecycle boundaries. The M1 AGX driver crashes in a
qualified Metal 4 static-linking fixture, so that fixture stays outside default
`runtest`.

`RAYS_METAL_SANITIZERS` accepts `address`, `undefined`, or `thread` for the
bridge build. ThreadSanitizer is exclusive; AddressSanitizer and
UndefinedBehaviorSanitizer may be combined. The MSL source path compiles at
runtime through the public Metal API; there is no offline shader artifact
pipeline.

A new binding begins with an `ogpu_metal` consumer and capability. Reuse an
existing safe operation when possible. Otherwise declare the call in the
registry (`Method` or `Property`) and let the generator write it; only
blocks, callbacks, descriptor graphs, handle arrays and ownership transfer
stay in the handwritten bridge. Add safe validation, typed errors, and a
success and rejection check. `.claude/skills/metal-workflow/SKILL.md` records
the workflow end to end; `tools/codemod` removes a binding once its last
consumer is gone.
