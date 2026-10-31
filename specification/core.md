# The shared Flow core

This is the compiler contract for the native and web targets. It introduces no Lisp syntax. The
checked workspace and the tree walker remain the language contract and the
native exactness oracle. A target prints or executes this core; it does not
walk `Workspace.term`, expand macros, or interpret Lisp operator names.

The frame primitive inventory below has **96 entries**. Region instruction
constructors, structured control, drawing statements and host intrinsics are
listed separately: none is a hidden primitive in a target's numeric runtime.

## Two execution levels

The static stage evaluates frame-independent expressions with the tree walker,
using graph input defaults. Its data becomes constants, including packed arrays
and resource descriptions. It never serializes an OCaml closure, cache, SDL
object or GPU handle. The remaining checked expressions lower once to a frame
program and its regions.

A static packed `map` of more than 4,096 elements is made differently, not
to a different value. The walker evaluates its first 4,096 elements as usual
(their records, call indices and failures are the ordinary ones). The host
may then supply the whole array from the CPU region the map would be at frame
rate (`Flow_ir.static`, which `qualify_workspace` uses); it is accepted only
when it is a packed array of the map's kind and length whose first 4,096
elements equal the walked ones bit for bit. When the region refuses the form,
fails, reads anything live or disagrees, the walker evaluates the remaining
elements itself, so a failure past element 4,096 is still the walker's
diagnostic. Records inside such a map are those of its first 4,096 elements;
the walker alone would keep visiting later elements until a path inside a
branch had 4,096 of its own. At 1,000,000 elements the walker costs about 22
microseconds and 79 KB an element; the region about 20 nanoseconds.

A program start runs the static stage once. `Rays_editor.Workspace.run` builds
the document the editor opens (`Lowering.of_workspace`) and reads the window
(`Lowering.document_window`: the settings graph's result in that document's
plan, the value `Lowering.window` evaluates for) and the declared camera from
it; the editor takes that document instead of lowering the workspace again.
The document is handed over, not cached: an edit, a save and a reload of the
file each lower the new text as before.

The **frame program** runs on the initial native domain, or the browser's main
thread, once per logical frame. It reads one immutable input snapshot, computes
scalar and bounded structured data, advances state slots, invokes regions, and
produces ordered drawing statements and host effects. A static subexpression
is retained; a changed view transform must not recompute unchanged attributes.

`Core_draw.device_groups` owns mark-attribute placement and sibling grouping
for both targets. Compatible single-output regions of one batch fuse only
within one statement block, with equal rate, precision and count/origin. The
group crosses only infallible scalar setup; outputs retain invocation order.
Native execution offers the merged region at its last invocation. A refusal
reruns the original regions in authored order, preserving their diagnostics;
CPU-only execution retains the original invocations. Multiple mark sets can
share a borrowed output without another dispatch overwriting it.

Each native drawing's display closure retains at most 16 literal Float
`array/range` inputs, bounded to 64 MiB. Equal lengths share immutable storage;
dynamic counts and other source forms follow ordinary input evaluation.
Uniforms and frame facts are prepared every frame.

Drawing regions lower captured array producers explicitly, including stateless
folds, so preparing a region cannot silently materialize its resident inputs on
the CPU. Scalar-only captures retain the direct packed path and grouping.
Immutable frame facts, including time, permit resident folds on both targets;
they disable reuse of loop results across frames, not device residency.
A resident fold sizes its arrays from earlier draws' counts; one a count may have outgrown
raises `Core_exec.Resident_overflow` before the host reads it, and the loop runs again from
its writes' values before it (`specification/gpu-codegen.md`, slots), on both targets. When only
counts, ranges over a count and regions read its arrays afterwards, they stay on the device until
the draw ends (`Core_exec.resident_tail`, `specification/flow.md`), on both targets.

A **region** is a pure data loop over packed arrays. It has explicit input
buffers, captured uniforms, an iteration domain, a checked body and output
slots. Independent elements run in disjoint 1,024-element CPU blocks, through
the shared native pool or required web workers. GPU placement emits that body
as Metal or WGSL. The SPIR-V printer also consumes the shared region layout;
its binary compute output is validated independently in tests. Ordered reductions and accumulator loops preserve iteration
order; they are not parallel tree reductions. Collect regions may copy immutable
Text leaves in a separate bank. Lists of records, host effects and frame state
are not region values, and shaders accept numeric registers only.

Native frame and CPU-region results must be bit-identical to the tree walker,
including coercions, evaluation order and integer/float tags. GPU regions have
the existing approximate numeric contract. The web frame and CPU-region
targets use double precision; they need not be bit-identical to native, but
integers, booleans, text and table structure are exact. Target-specific
rounding does not permit a different branch, list length or state identity.

The SPIR-V printer emits binary words directly, with the same bindings,
uniforms, table captures and resident-count protocol as the Metal printer.
Its current subset includes floating arithmetic, guarded registers, checked
arithmetic, seeded noise with structured octave loops, vectors, fast integer
operations and numeric gathers. Fast kernels that need integer carriers declare
the SPIR-V `Int64` capability; floating-only kernels do not. Structural
validation does not establish device execution parity. Ray queries and the
widened region control flow remain separate compiler work.

## Program data and ownership

`Flow.Phase_timer` takes its clock and phase names from callers. Names are
strings, including phases unknown to Flow; snapshots sort entries by name.
Nested scopes record exclusive duration and restore the enclosing sample on
exceptions. Editor and benchmark code own their phase vocabulary.

A frame program contains numbered locals, a typed constant table, structured
statements, result locals, a state-slot table and regions. Local and statement
numbers are assigned in source traversal order, not hash-table iteration
order. Every statement retains its authored path, graph instance, iteration
identity and half-open source byte span. Printers use these to generate
`program.map`; a diagnostic points to the original `.rays` form.

Inlining retains the callable's graph instance and prefix. A per-frame counter
slot supplies each invoked closure's next call index; only a selected branch
increments it. Constructing a local callable resets its slot, including each
loop iteration. Iteration identities contain static indices and local reads of
these call indices, so state inside an inlined function retains native keys.

The core contains data, not callbacks. A primitive is a closed member of the
inventory below with explicit operands and result layout. An intrinsic is a
closed effect/resource operation with target legality. Captures are constants,
locals, state reads or uniforms. Native compilation to closures is permitted
as an execution optimization after the core has been built; those closures
never become the representation consumed by another target. A program but for
its regions is plain data: a capture inside a host source lowers to
`Host_source` arguments, so the statements marshal without closures. A region
keeps the host residual its sources are evaluated from each frame. A kernel that
reads a state value takes it as an argument after the state's own statements; a
region compiled directly from a residual resolves it as a uniform with the
caller's state.

Every eligible pure numeric map is lowered once to a validated `Core.Region`;
the frame program invokes it with the current inputs. `Packed.t` is an alias of
that private validated representation, so CPU and device consumers share one
region layout and validation boundary. The region's instruction-dependency flags
are derived on first execution and cached atomically; each executor forces them
before dispatching work to the shared domain pool. CPU scratch blocks are capped
by the actual element count, up to the existing block limit. Branches retain
their selected lanes; captures, array ownership and integer range are checked
before execution. Effects, escaping locals, accumulators, filters, aliasing
input/output buffers and unsupported layouts use the general statements. A block
failure reruns those statements in element order, preserving diagnostics and
partial state. Stateless prepared residuals may bypass creation of evaluator
memo/call tables under the executor's owner token; frame validation and
diagnostic fallback remain mandatory.

Values are immutable when published. Sequence-building stores write only a
fresh, locally owned allocation. A published sequence, constant or previous
state value cannot be edited in place. Buffer ids belong to the caller's table;
they do not expose a backend buffer handle. Replaced GPU resources are released
by their owner, on the initial domain or browser main thread.

| Value | Layout and admissible uses |
| --- | --- |
| Float | One binary64 frame/CPU component; a binary32 GPU-region component. Non-finite inputs and results fail at the existing checked boundaries. |
| Int | The native signed 63-bit integer domain; distinct from Float even when their numeric values match. Explicit integer operations retain integer semantics. |
| Bool | One boolean frame value; region comparisons encode false as 0 and true as 1. |
| Text | A decoded byte string; no quotes are added on a text read or concatenation. Collect regions may copy, select and gather immutable Text leaves. |
| Vec2, Vec3, Vec4 | Two, three or four floating components. Field names are static `x`, `y`, `z`, `w`; a scalar broadcast is explicit. |
| Record | Ordered, statically named fields. Nested records flatten to ordered field locals; layout retains names, nesting and field order for reconstruction, `show` and equality. |
| List | An element layout and a length no greater than 4,096; a static length or a bounded dynamic length. Elements may contain records, text and other bounded lists. |
| Packed array | Contiguous numeric storage, element width 1, 2, 3 or 4 and an element count. Counts may exceed 4,096. Lists and packed arrays remain different value kinds. |
| Text array | Owned string storage with an element count that may exceed 4,096. Strings retain exact native bytes. Collect regions use a separate immutable Text bank; numeric operations and shaders refuse Text registers. |
| Image | A resource id and its description or producer, resolved by the host; no pixels or GPU handle are smuggled into a scalar value. |
| Drawing | An ordered sequence of drawing entries, possibly grouped by a transform. It is not a state value or a numeric region value. |
| Effect/sample | A typed intrinsic description, legal only when the target implements that intrinsic. |

Functions are compile-time callables: local `fn`, named `defn`, and operator
function references specialize and inline at their checked call sites. No
function survives as a stored frame, state, region or exported data value.
`Any` is a checker type, not permission for an unrestricted dynamic value VM;
lowering must resolve its layout or return a refusal. `nil` is empty geometry
in the source contract, not an empty numeric sequence; a reached geometry value
is refused by the web target.

## Structured control and lowering

Expressions are literals, local reads, primitive applications, vector/component
construction and statically laid-out structured values. Statements assign
locals, branch, loop, build a sequence, read/write a state slot, invoke a region,
append a draw entry or invoke an intrinsic. A branch or loop assigns its result
to explicit result locals. There are no jump offsets, bytecode opcodes or
general run-time function calls.

An `Invoke` carries the explicit Core locals used by a compiled region. Lowering
replaces computed scalar, integer, array and structured closure captures with
typed argument sources; native invocation checks exact arity, declared storage
and finite values before binding immutable inputs. It does not reevaluate the
producer. Production web export declines regions with deferred arguments and
lowers their computation as structured Core instead; fixed export-time probing
must never resolve these argument markers. An unsupported producer remains a
strict qualification error through either route.

Counted iteration converts endpoints with the source's list-index rounding,
computes `max(0,last-first)` with native integer subtraction, checks its bound,
and initializes its result builder once before visiting items. The ordinal
index and `Int(first+index)` item are separate locals. The induction uses
integer arithmetic even above binary64's exact-integer range; it is structured
control, not a new frame primitive.

Arguments and fields evaluate in authored order, once. `let*` extends the
environment after each binding. Sequence destructuring checks the required
length, and record destructuring reads static keys. A dotted reference is a
chain of component/field reads; its missing-field diagnostic is `E_FIELD`.
`assoc` evaluates the record first and updates in authored order, preserving
existing field position, coercing an existing field to its prior layout, and
appending a new field at the end.
An existing reactive field is resolved before its tag controls coercion.
Nested coercion reads only old fields present in the replacement record;
unrelated captured fields remain lazy. Accumulator coercion follows the same
rule, with the numeric Int-to-Float widening described below.

Record construction and matching ordered branch layouts retain field locals;
reconstruction occurs only at a structured result or primitive operand. A
branch whose records have different field order retains that order in its
selected structured value. A joined numeric local may still hold an Int from
one branch: explicit Float coercion must convert it, and `assoc` must use the
selected old value's tag rather than the checker's joined type.
A record branch joined with an error evaluates the selected branch before
projecting or coercing accumulator fields; an error has no record fields to
compile. Native and JavaScript execution retain the error's code, message and
optional cause.

`if` evaluates its condition and only the selected branch. `cond` tests arms in
order and stops at the first true test. `case` evaluates its scrutinee once;
literal number comparisons use numeric values, text compares text, and a Bool
on either side requests boolean numeric truth conversion. The compiler
expands these cases with `=`, `equal?` and `truthy`; it must not substitute
structural equality for numeric case matching. `and` and `or` are ordinary
eager source operators: both operand expressions evaluate before the result
is formed.

Within a compiled SOP `defnode`, a Fold over typed arrays may carry a geometry
handle or a record containing geometry and data. It runs as ordered structured
Core with no 4,096-item array bound. The actual accumulator type controls this
capability, including a body typed `Any` because it raises an error. Public
graph geometry-array folds and collecting geometry forms remain refused.
Nested numeric producers still require strict packed qualification; this
capability does not admit geometry to numeric regions.

Loops have explicit induction and accumulator locals. A list loop has at most
4,096 attempted iterations, including skipped iterations. Product clauses
nest in source order, with the last clause varying fastest; later sources may
depend on earlier bindings. `:skip` tuples keep their authored iteration
identity, avoid evaluating the skipped body, and advance the attempted index.
`for` collects results, `sum` adds them in order (an empty sum is `Int 0`),
`fold` returns its accumulator, and `scan` collects the updated accumulator
after each non-skipped step. An integer accumulator can widen to Float when
the next step is Float; that is different from the rounding used by `assoc`.

List `map` zips its inputs to their shortest length. `filter` retains source
order. `reduce` applies the function sequentially to the accumulator and each
item, returning its seed for empty input. `sort_by` computes a numeric key once
per item and orders stably by that key. Packed forms of these operations have
packed storage rather than the list cap; eligible pure numeric bodies become
regions. The checker still enforces `E_TIME_COUNT` where a live count could
change graph topology. A compiled bounded data loop does not relax that rule.

Frame list sorting expands to a stable bottom-up merge with locally owned
index buffers and one key evaluation per source element. Seeded record
accumulators retain their fields as locals; all next fields are read before
any old field is overwritten. Builders share a write permission across local
aliases. Embedding a builder in a record/list or storing it in another builder
seals it, and a store through any alias then fails with `E_CORE_OWNERSHIP`.

`Expanded` lowers its checked macro body. `Bypass` lowers only the first
positional input the checker selected and coerces it to the call result type;
it does not evaluate the bypassed operation's other inputs. `ref` specializes
a graph instance with its checked overrides/defaults and keeps the evaluator's
sharing and instance identity. Neither graph references nor macros become a
second target-side interpreter.

Native hosts may capture a typed unresolved `Source` in a structured expression.
Its resolver runs only when the expression is reached, checks the declared
result type and rejects another unresolved marker. Geometry handles remain
private immutable host captures; this expression is refused by the 2D web target.

`Host_source` constructs a typed source from computed locals and resolves it
when reached. A storage-valued argument of a cooked call may instead retain
its immutable query as `Source_descriptor`. The native attribute consumer
checks its storage kind and owner cardinality before sharing unchanged planes;
it does not interleave, copy or finite-scan them. This preserves native storage
transfer semantics, including NaN payloads and infinities. Computed arrays and
numeric consumers still materialize and validate their inputs. Both source
expressions are refused by the current 2D web target.

Canonical constant-array constructors may retain validated count and fill
metadata for cooked storage consumers. The compiler proves every use reaches
such a consumer through simple local aliases; mixed numeric, record, state,
loop or returned uses keep ordinary dense arrays. Validation runs at the
original constructor statement: authored count and fill expressions keep
their evaluation order, actual count and storage bounds are checked, and an
empty array does not validate its unused fill. The writer checks destination
cardinality and later selection/default arguments at their existing boundaries,
then creates owned planes without a dense intermediate. Ordinary source
resolution can still materialize the descriptor with the canonical constructor.
This does not change Lisp syntax, graph projection, reference evaluation or
ordinary JavaScript array constructors; the existing cooked-host export refusal
remains in force.

If native lowering cannot cover a form, it returns that form's name. During
the staged migration, execution continues through the existing path. If core
execution fails, the native caller reruns the tree walker to obtain its exact
diagnostic and span, with state restored before retry. Web export has no tree
walker fallback: an unsupported reached form is a source-located export
refusal and a non-zero exit, never a missing entry or a silent no-op.
Strict authored node execution accepts only compiled regions and structured
control whose numeric work compiles; executor refusals and failures propagate
without a reference retry. Packed arrays retain their native storage bounds
when iterated by structured Core; the 4,096 bound applies to lists.
Core lowering can emit a region whose input is another residual producer. Its
caller must qualify those region sources and captured uniforms before accepting
strict execution; the Executor applies the same recursive check to regions inside
structured Core and to top-level packed kernels. A text consumer cannot bypass
qualification of a nested numeric producer.
Text-array maps and collecting loops may compile immutable leaves into Collect
regions. Text transformations, filters and scans execute as structured Core,
including empty inputs and multiple invariant array clauses. The builder checks the product
of clause counts before allocation and leaves inner collections unevaluated when
an outer clause is empty. A dependent inner collecting clause requires a growing
builder and currently returns `text_collect_dependent_clause` from Core lowering;
the reference evaluator retains its ordinary nested-loop semantics. Strict mode
propagates that refusal instead of selecting a fallback.

## Frame primitive inventory

Each back end implements exactly these leaves. A parameter such as a type
layout, error span, or operation label is data fixed by lowering, not an
additional dynamically chosen Lisp operator. Structured sequence stores,
local assignments and component reads are core statements/expressions, not
unlisted calls in `prims.js`.

| Primitive names | Entries | Behavior |
| --- | ---: | --- |
| `+`, `-`, `*`, `/`, `mod`, `pow`, `min`, `max` | 8 | Numeric arithmetic with scalar/vector broadcasting and the source result tags. `/` and `mod` return zero for a zero divisor; `mod` is the double-remainder rule; `pow` uses the absolute base. Integer results of ordinary arithmetic pass through the double arithmetic and integer conversion boundary. |
| `remainder` | 1 | Raw floating remainder with the dividend's sign; used by color lowering. It must not add the divisor or repeat the remainder calculation as `mod` does, since that changes double rounding. A non-finite result fails with the operation's span. |
| `sin`, `cos`, `tan`, `asin`, `acos`, `sqrt`, `exp`, `log`, `abs`, `sign`, `atan2`, `hypot` | 12 | Scalar math with named finite-result checks, except the existing `sqrt(abs(x))` and floating `abs` contracts. `abs` retains an Int input's tag. `atan2(a,b)` takes the ordinate first; `hypot` preserves native overflow/underflow-safe length calculation. `sign` returns -1 or +1 for a nonzero input and preserves either signed zero. |
| `floor`, `ceil`, `round`, `int`, `float` | 5 | Numeric conversion; explicit `int` truncates. `round` uses the source explicit-round rule, while type coercion to Int uses the separate coercion rule. |
| `<`, `<=`, `>`, `>=`, `=`, `and`, `or`, `not` | 8 | Numeric comparisons and numeric boolean truth. These are not generic string/array comparisons, nor JavaScript truthiness. |
| `int/add`, `int/sub`, `int/mul`, `int/div`, `int/mod`, `int/and`, `int/or`, `int/xor`, `int/shl`, `int/shr` | 10 | Exact signed 63-bit integer operations, including wrapping overflow, division and remainder, bitwise operations and checked shifts; zero integer divisor is `E_RANGE`. No 32-bit JavaScript bitwise truncation. |
| `int/lt`, `int/le`, `int/gt`, `int/ge`, `int/eq` | 5 | Exact integer comparisons without a floating conversion. |
| `equal?`, `show`, `text/concat` | 3 | Typed structural equality, source value display, and ordered text concatenation. `show` has display formatting, distinct from workspace source-number printing. |
| `text/blank?` | 1 | True exactly when `String.trim` yields empty text: only ASCII space, tab, LF, CR and form feed are whitespace. Vertical tab, NUL and Unicode spaces remain nonblank; wrong input storage is `E_TYPE`. |
| `text/trim`, `text/lower` | 2 | `String.trim` removes only ASCII space, tab, LF, CR and form feed at either end. `String.lowercase_ascii` changes only ASCII A–Z; non-ASCII UTF-8 bytes are unchanged. Both accept text storage only (`E_TYPE`) and execute as structured text operations, never numeric packed instructions. |
| `text/repr` | 1 | Text to quoted text with exactly `Printf.%S` byte escaping: ASCII quotes and backslashes escape, backspace/tab/LF/CR use named escapes, and every other nonprintable byte uses three decimal digits. UTF-8 is quoted byte by byte, yielding ASCII output. Native text includes arbitrary bytes; JavaScript quotes their reversible byte representation. Wrong storage is `E_TYPE`. This is a structured text operation. |
| `text/split`, `text/tokens` | 2 | `(text/split text separator)` returns the `(array text)` pieces between each non-overlapping, left-to-right occurrence of a nonempty separator, keeping empty pieces (`"a,,b"` by `","` is three pieces, `""` is one empty piece); an empty separator is `E_TEXT_SEPARATOR`. `(text/tokens text)` splits on space, comma, TAB and LF only (not CR) and drops empty tokens. Separators and tokens match bytewise, so UTF-8 passes through unchanged. Wrong storage is `E_TYPE`. Structured text operations. |
| `text/to_float`, `text/to_int` | 2 | Total token readers returning an ordered record `{:value :valid}`; invalid text gives value zero and `valid` false, never an error. Input is trimmed with `String.trim` (ASCII space, TAB, LF, CR, FF). `text/to_float` accepts exactly `[+-]?(digits[.digits*]\|.digits)([eE][+-]?digits)?` with ASCII digits and a finite result (no hex, underscores, `nan`, `inf`; `1e999` is invalid). `text/to_int` accepts exactly `[+-]?digits` and must fit the 63-bit signed integer. Ramp text `pos:value,...` is `text/split` by `","` then `":"` then these readers. Wrong storage is `E_TYPE`. |
| `text/table_decode`, `text/table_encode` | 2 | Exact ordered text-cell tables, with tab-separated cells, LF-separated rows and only backslash, `t` and `n` escapes. Empty text decodes to an empty table; encoding empty rows retains the native codec's empty-row ambiguity. Invalid escapes are `E_TABLE`, invalid cell storage is `E_TYPE`. |
| `sequence/length`, `sequence/get`, `sequence/build`, `sequence/slice`, `sequence/concat` | 5 | Element count, checked list element access, fresh bounded-list storage, checked copying slice, and ordered concatenation with the 4,096-element cap. Element layouts and nested record field order are explicit. Given an element width of 1 to 4, `sequence/build` allocates packed storage with no list cap, filled by the same ordered stores (the walker's `array_init` checks per element). Integer and text builders require width one and explicit type `int` or `text`. Text builders store strings and do not create packed numeric storage. |
| `array/max_length`, `array/range`, `array/int`, `array/text`, `array/float`, `array/vec3`, `array/vec4`, `array/nth`, `array/count`, `array/slice`, `array/concat` | 11 | Typed-array construction, indexing, length and copying operations, including exact integer and decoded text storage. Range defaults to float and accepts the literal type `int`. Vector constructors accept the matching vector or broadcast a scalar; omitted values are typed zero. Count is in elements, not components; all products and offsets are checked before allocation or access. An array is not converted to a list. |
| `array/grade`, `array/index_of` | 2 | Whole-array CPU operations returning fresh IntArray indices. Grade accepts Int/finite Float/native-byte Text keys and stably grades original indices; descending mode swaps comparator orientation while retaining equal-key order, including ±0. Index_of accepts matching Int or Text keys/queries, preserves query order/count, returns the first original key index or -1, and allows duplicate keys. Inputs are immutable. Grade count is keys count; lookup count is queries count, never an implicit first-input owner origin. Text uses unsigned byte order/equality through the reversible JS codec, without Unicode normalization/UTF-16 collation. No Region/GPU sort or lookup instruction is implied. |
| `array/compact`, `array/scan_sum` | 2 | Whole-array ordered Int operations, structured Core on CPU. Compact accepts an Int 0/1 mask (`E_ARRAY_MASK` otherwise) and returns fresh increasing kept indices. Scan returns fresh exclusive boundary offsets of length n+1: initial zero then ordered signed63 `int/add` prefixes, including the terminal total; empty input gives `[0]`. Negative entries and wrap remain ordinary integer arithmetic. n must be below native Int-array capacity before n+1 is formed. No count/topology origin is inherited: compact cardinality depends on mask data, scan cardinality is n+1. Strict packed producers/consumers remain numeric kernels; dynamic per-row compact/scan is refused by the Region/GPU tier. |
| `array/scatter_add` | 1 | Fresh, zero-filled Int/Float/Vec2/3/4 array of the specified Int size. Indices must be IntArray and have exactly the values' length (`E_ARRAY_COUNT`). Source rows and vector components are added in order, including duplicate indices; Int additions wrap at signed 63 bits and Float additions round separately and reject nonfinite inputs/sums (`E_NONFINITE`). Each index must be inside the output (`E_ARRAY_RANGE`). No atomic GPU scatter or index regrouping is implied: the whole-array operation runs on the CPU; strict packed input maps retain their ordinary qualification. |
| `array/min`, `array/max`, `array/segment_sum`, `array/segment_min`, `array/segment_max` | 5 | Ordered typed numeric reductions, componentwise for vectors. Integer sums wrap at signed 63 bits; floating extrema retain `Float.min`/`Float.max` signed-zero semantics. Segment offsets must be an integer array starting at zero, nondecreasing and ending at the value count (`E_ARRAY_OFFSETS`). Duplicate offsets yield typed zero for sums and `E_ARRAY_EMPTY` for extrema. Global empty extrema also fail; offsets `[0]` yield zero output rows. Independent contiguous segments may use the shared pool; additions within each segment retain source order. These operations do not gather or rearrange values. |
| `array/segment_argmax` | 1 | Int or Float values with the same segment offsets (`E_ARRAY_OFFSETS`, `E_ARRAY_TYPE` for vectors, text or non-integer offsets): an IntArray with one entry per segment, the index into `values` (not relative to the segment) of the first maximum, so ties keep the earliest and signed zeros compare equal; -1 for an empty segment (no error). Float values must be finite (`E_NONFINITE`). Independent segments may use the shared pool; the result does not depend on the domain count. |
| `value/rand`, `noise3`, `color/parse`, `color/byte` | 4 | The deterministic keyed hash, seeded Perlin/fBm sample, source drawing-color decoding, and channel conversion. `noise3` takes constant `:seed`, `:octaves` (1 to 64), `:lacunarity` (finite, positive, default 2) and `:gain` (0 to 1, default 0.5); with more than one octave the bytes equal `Noise.fbm3_with_scratch` (and `Noise.fbm3` at the defaults), the CPU kernel and JavaScript twin agree, and Metal and WGSL refuse non-default shapes of more than one octave as `E_GPU_FORM`. Static noise permutations become data; dynamic-seed caches are bounded to 16 entries. Color conversion preserves native byte quantization. |
| `rand/at` | 1 | Two exact signed63 integers, seed and index, to a Float with the exact `Rand.float_at` keyed-hash bits. No floating conversion precedes hashing. The independently authored `rand_at` Lisp arithmetic helper remains an oracle, rather than a compiler-recognized name. |
| `fma` | 1 | Explicit scalar binary64 fused multiply-add: the exact product and sum round once, matching `Float.fma` including signed zero, subnormals and finite overflow cancellation. Nonfinite inputs or final output fail with `E_NONFINITE`; an overflowing intermediate product alone is allowed. JavaScript computes the exact significands with BigInt and rounds to nearest, ties to even. This primitive does not contract ordinary `*` and `+`. GPU targets refuse it when exact binary64 single rounding is unavailable. |
| `float/next_after` | 1 | `(float/next_after x y)`: the adjacent binary64 value after `x` in the direction of `y`, exactly `Float.next_after`: `y` when `x = y` (so `(next_after 0.0 -0.0)` is `-0.0`), the smallest subnormal of `y`'s sign from a zero `x`, otherwise one ulp step on the bit pattern. Both inputs and the result must be finite (`E_NONFINITE`; the result of a finite pair always is). JavaScript steps the BigUint64 bit pattern of a Float64 view. Pure scalar Core operation with no packed kernel, GPU or WGSL form. |
| `float/mul_checked`, `float/fma_checked` | 2 | Finite scalar inputs produce an ordered record `{:value float :valid bool}`. Finite results preserve exact binary64 bits, including signed zero; overflowing results produce positive zero and false. Multiplication rounds separately; FMA rounds once and permits finite cancellation of an overflowing exact product. Invalid inputs retain `E_NONFINITE`. No nonfinite value is published, and ordinary `*`/`fma` retain their errors. GPU targets refuse binary64 overflow-status instructions. |
| `error` | 1 | Text code and text message, with optional `:cause` Text, produce an authored diagnostic preserving all three exactly. Its checked result is `Any`, allowing an error branch to join a typed successful branch. Structured Core retains dynamic text arguments; literal/captured constant text in numeric packed bodies uses `Raise`, while unresolved dynamic messages or causes refuse packed qualification. Untaken branches and empty collections do not raise. Wrong argument storage is `E_TYPE`. |
| `coerce`, `truthy`, `fail` | 3 | Explicit checked source-value coercion, condition truth conversion, and a diagnostic with a code, message and source span. Coercion preserves record order and list element joins. |

Total: 8 + 1 + 12 + 5 + 8 + 10 + 5 + 3 + 1 + 2 + 1 + 2 + 5 + 10 + 5 + 4 + 1 + 1 + 2 + 1 + 3 = **90**.
`array/sum` remains an ordered typed reduction in structured Core, using exact
wrapping integer addition for integer arrays; its JavaScript twin is checked
alongside the intrinsic inventory without adding a Core primitive.
The numeric, list and text models below fix the behavior of these leaves.

Convenience operators lower to these leaves. For example `value/polar` is a
vector built from `*`, `cos`, `sin` and its optional height, `length` is an
ordered sum of component squares followed by `sqrt`, and `color/hsl` is
arithmetic, branches and `color/byte`. List `range`, `take`, `rest`, `reverse`,
`filter` and higher-order calls are sequence allocation/access plus structured
loops, not independent target runtime algorithms.

`color/gradient` first uses `color/parse` on each stop and refuses empty or
non-opaque stops with `E_DRAW_COLOR`. It clamps the position to [0,1], scales
by stop count minus one, selects the adjacent stops, interpolates each byte
with `left*(1-u)+right*u`, adds 0.5 and truncates, then divides RGB by 255.
A one-stop gradient returns that stop. `color/hsl` wraps hue in degrees to
[0,360), clamps saturation and lightness to [0,1], sets
`c=(1-abs(2*l-1))*s`, `x=c*(1-abs(remainder(h/60,2)-1))`, picks the six RGB
sectors, adds `l-c/2` to each channel, quantizes and divides by 255. Its alpha
is 255. These formulas are compiler rules shared by both targets.

## Numeric model

Frame and CPU-region floating arithmetic is IEEE-754 binary64. GPU regions
use binary32 inputs, uniforms, registers and outputs, as the existing Metal
emitter does. An instruction rounds to its own result precision before the
next instruction; lowering may not reassociate an ordered sum or introduce a
fused multiply/add that removes a rounding step. A GPU constant or uniform is
encoded by its binary32 bits. Converting a GPU attribute buffer to a CPU
consumer is an explicit readback/precision boundary, subject to target
legality; it is not an implicit float conversion.

Packed lowering shares identical instructions before enforcing the 64-register
budget, including when fusion is disabled. Constant sharing compares binary64
bits, preserving signed zero, and does not reassociate arithmetic. Metal and
WGSL use the same finite-check classification: inputs and operations that can
introduce nonfinite values remain checked, including intermediates hidden by
a selection or clamp. Constants qualified as finite, comparisons, selections,
absolute value, absolute-input square root, min and max need no additional
check after their operand producers have been checked.

Native Int has 63 signed bits: minimum -4,611,686,018,427,387,904 (`-2^62`),
maximum 4,611,686,018,427,387,903 (`2^62-1`). This range is distinct from the
checked float-to-int boundary: that conversion accepts only a finite double
with absolute value strictly less than `4e18`, then truncates toward zero.
An exact existing Int is not put through that narrower float boundary unless
the operator explicitly converts it to a double. Failures there are
`E_NONFINITE`, with the operator name in the message.

Number conversion maps Int to its rounded binary64 value, Bool to 0 or 1,
and Float to itself; any other value is `E_TYPE` ("Expected a number.").
Truth conversion accepts Bool unchanged and Int/Float unequal to zero;
all other kinds are `E_TYPE` ("Expected a bool."). Signed floating zero is
false. These conversions are not JavaScript's general `Number` or truthiness
on objects, strings and lists.

| Operation | Exact order and result |
| --- | --- |
| `int(x)` | Number conversion, truncate toward zero, checked float-to-int conversion; Int result. Thus `int(-2.7) = -2`. |
| `floor(x)`, `ceil(x)` | Number conversion, floor/ceiling, checked float-to-int conversion; Int result. |
| `round(x)` | Number conversion, nearest integer with an exact halfway case away from zero, then checked float-to-int conversion. `round(-2.5) = -3`. |
| Coercion to Int / `Value.int_of` | Number conversion, `floor(x + 0.5)` in binary64, then checked float-to-int conversion. This includes the rounding of the addition. Coercing `-2.5` gives `-2`. Bool-to-Int coercion directly gives 0 or 1. |
| `float(x)` | Number conversion; Float result, including an Int value rounded to binary64. |
| Ordinary `+`, `-`, `*`, `mod`, `min`, `max` | Convert both scalars to doubles, apply the operation, reject a non-finite result, then return Int only when both actual operands are Int. That Int result uses `floor(result + 0.5)` and the checked conversion. It is not exact integer arithmetic. |
| Ordinary `/` | Convert operands to doubles. If the divisor equals either signed zero, result is positive zero; otherwise divide. Reject a non-finite result and return Float. |
| Ordinary `mod` | If the divisor is zero, positive zero; otherwise `remainder(remainder(a,b) + b,b)`, in that order. This is not integer remainder and is not one remainder calculation. |
| Ordinary `pow` | `pow(abs(a),b)` with double operands; reject a non-finite result; Float result even for two Int inputs. |
| Vector arithmetic | Matching-width vectors apply the scalar floating operation in component order. A scalar numeric operand broadcasts to that width. Every result component is finite; result is a vector, not Int components. The checker rejects mismatched widths. |
| Scalar `sin`, `cos`, `tan`, `asin`, `acos`, `exp`, `log` | Number conversion followed by scalar math and the named finite-result check; Float result. Invalid inverse-trigonometric or logarithmic domains and overflowing results fail with `E_NONFINITE`. |
| Scalar `atan2(a,b)` | Convert both inputs to doubles, compute `atan2(a,b)` in ordinate/abscissa order, and check the finite result. Quadrants and signed-zero arguments retain native semantics. |
| Scalar `hypot(a,b)` | Convert both inputs to doubles and use native `Float.hypot`, with a finite-result check. Squaring and summing the inputs is not an equivalent implementation: finite large lengths must not overflow early, and subnormal lengths must not collapse to zero. CPU regions support it; Metal and WGSL report `E_GPU_FORM` until an equivalent target implementation is available. |
| Scalar `sign(x)` | Number conversion; return Float -1 for a negative input, +1 for a positive input, and the original zero otherwise, followed by the named finite-result check. |
| Scalar `sqrt` | Number conversion followed by `sqrt(abs(x))`; Float result. There is no finite check at frame level: `sqrt(NaN)` is NaN and `sqrt(inf)` is `inf`. Packed instructions additionally require every numeric result to remain finite. |
| Scalar `float` | Number conversion only. No finite check: NaN and the infinities pass through. |
| Scalar `abs` | An Int input uses signed integer absolute value, retaining the native wrap at minimum Int (`abs(min_int) = min_int`); all other numeric inputs use floating absolute value and yield Float. A Float is not finite-checked: `abs(-inf)` is `inf`, `abs(NaN)` is NaN. |
| `<`, `<=`, `>`, `>=`, `=` | Compare converted doubles, even for two Int inputs. In particular, numeric `=` may equate distinct large integers after double rounding; structural `equal?` does not. |
| `and`, `or`, `not` | Operands have already evaluated in authored order. Truth-convert operands as the source operator does and produce Bool. `and`/`or` may short-circuit the second truth conversion, but never the evaluation of the second operand expression. |

The vec3 operators `dot`, `cross` and `normalize` expand to existing numeric
instructions. Dot accumulates three separately rounded products left to right.
Cross rounds the first product, then subtracts the second product with one
fused rounding: its x component is `fma(-a.z, b.y, a.y * b.z)`, with the
corresponding cyclic expressions for y and z. This preserves native FMSUB
association, including signed zero and cancellation residues. CPU and
JavaScript use exact binary64 FMA; unsupported GPU targets refuse this form.
Normalize computes the square root of that dot product, returns positive zero
components for a zero length, and otherwise multiplies by the reciprocal length.
The generic language operator rejects nonfinite intermediate lengths. Port
helpers with a different native normalization policy preserve that policy
explicitly instead of changing this operator's meaning.

The shared SOP prelude represents a matrix as the ordered record
`{:r0 vec4 :r1 vec4 :r2 vec4 :r3 vec4}`: row-major storage, column vectors,
right-to-left composition. Its matrix and quaternion helpers preserve
`Rays_math` operation order. Matrix multiplication includes the initial
positive-zero accumulator and four explicit FMA updates in inner-index order.
These named operations do not contract ordinary language multiplication or
addition. Matrix inverse uses first-tie partial pivoting and
the native `1e-12` singular threshold, returning an empty list for singular or
a singleton matrix list for success. Point transforms leave the raw result
when the homogeneous divisor's magnitude is at most `1e-12`; direction
transforms omit translation. Quaternion normalization uses direct division
and returns identity for lengths at most `1e-12`, distinct from vec3's
zero-only reciprocal multiplication.

Prelude `float_floor` and `float_ceil` return floating results without changing
the Int contracts of `floor` and `ceil`. Magnitudes at least `2^52` are already
integral and bypass conversion; the unselected conversion input is guarded.
Zero inputs retain their sign, including negative zero from ceiling a small
negative value.

For an `int/*` operand, an existing Int is used directly; a Float or Bool
passes through `Value.int_of`, not explicit `int` truncation. Integer
operations then work in the signed 63-bit domain:

| Integer primitive | Behavior |
| --- | --- |
| `int/add(a,b)`, `int/sub(a,b)` | Exact addition/subtraction followed by signed 63-bit wrapping. |
| `int/mul(a,b)` | Multiply exactly, keep the low 63 bits, interpret those bits as signed. `int/mul(max_int,2) = -2`. |
| `int/div(a,b)` | Signed quotient truncated toward zero. Zero divisor is `E_RANGE` ("Integer division by zero."). The overflow pair `min_int / -1` wraps to `min_int`. |
| `int/mod(a,b)` | Signed remainder `a - trunc(a/b)*b`, with the dividend's sign. Zero divisor has the same `E_RANGE`; `-7 mod 3 = -1`, `7 mod -3 = 1`, and `min_int mod -1 = 0`. |
| `int/and(a,b)` | Bitwise AND of their 63-bit two's-complement representations, then signed interpretation. |
| `int/or(a,b)` | Bitwise OR of those representations, then signed interpretation. |
| `int/xor(a,b)` | Bitwise XOR of those representations, then signed interpretation. `min_int xor max_int = -1`. |
| `int/shl(a,b)`, `int/shr(a,b)` | Checked shift count 0 through 62; left shift wraps and right shift fills with zero bits, matching native logical `lsr`. Other counts produce `E_RANGE`. |
| `int/lt`, `int/le`, `int/gt`, `int/ge`, `int/eq` | Compare signed integer operands exactly and return Bool. |

The JavaScript target represents Int as `BigInt` and Float as `Number`.
Integer literals are emitted as decimal BigInt literals, never parsed through
Number first. `int/*` arithmetic wraps with `BigInt.asIntN(63, value)`; BigInt
division/remainder have the required truncation/sign, with explicit zero
checks and wrapping for quotient overflow. The target uses `Number(integer)`
only at the numeric-double conversion boundary. A checked Float-to-Int
conversion tests the finite/magnitude condition before creating a BigInt
from the truncated Number. Intermediate local tags, list joins and state
coercions remain explicit; a large integer must not lose bits on a field read,
in structural equality, in state, or in JSON fixture serialization. JSON
fixtures encode integers in decimal text with their Int tag.

`value/rand` is the keyed numeric hash, not mutable random state. Start at
unsigned 32-bit `h=0x9e3779b9`. For each numeric key in order, compute
`k=ToUint32(floor(key*1000003))`, then
`h=imul(h xor k,0x85ebca6b)`, `h=h xor (h >>> 13)`,
`h=imul(h,0xc2b2ae35)`, `h=h xor (h >>> 16)`. `imul` retains the low 32
bits; right shift is logical. `ToUint32` truncates a finite value modulo
`2^32` into [0,`2^32`); a non-finite value maps to zero, including overflow
of the key multiplication. The result is `(unsigned h mod 1000000)/1000000`.
JavaScript's 32-bit operations are appropriate for this hash only, not `int/*`.

Noise uses a seeded permutation of 0..255 duplicated to 512 entries, Perlin
gradient interpolation and optional ordered fBm. Static seed permutations
are exported as data. The dynamic permutation generator shuffles backwards
from index 255 to 1, taking the unsigned SplitMix64 output modulo index+1.
Its state adds `0x9e3779b97f4a7c15` modulo `2^64`, mixes with logical shifts
30, 27 and 31 and multipliers `0xbf58476d1ce4e5b9` and
`0x94d049bb133111eb`, also modulo `2^64`. Each generated index swaps two
entries; it does not consume a global generator.

For a sample, floor the position, use each integer lattice coordinate's low
8 bits and the permutation to select eight corner hashes. With `h=hash&15`,
gradient uses `u=x` if h<8 else y; `v=y` if h<4, x if h=12 or 14, else z;
bit 0 negates u and bit 1 negates v, then add. Fade is
`t*t*t*(t*(t*6-15)+10)`, with operations in that order. Interpolate by
`a+t*(b-a)`, along x for each corner pair, then y, then z. Map the raw result
with `(raw+1)/2` and clamp to [0,1]. Default seed is 0 and octaves is 1;
frame octaves outside 1..64 are `E_NOISE_RANGE`. fBm starts at frequency and
amplitude 1, adds sample*amplitude and amplitude to ordered sum/weights,
doubles frequency and halves amplitude each octave, then divides sum by
weights. Packed noise supports octaves 1..64 (Metal and WGSL, 1..32) with a constant seed and constant
octave count; non-constant or out-of-range settings keep their current refusals.

## List model

The core represents lists of at most 4,096 elements. This is a representation
limit, not a new source-language rejection. Today `range`, `linspace`,
`concat` and non-packed product loops enforce `E_ITER_BOUND`, but a list
literal, a supplied list value or `frame/input` events is not universally
length-checked by the walker. An oversized value that survives static
evaluation must therefore decline native core lowering/execution and use
the native reference path; the web target must issue a source-located
list-limit refusal. It cannot drop events, truncate data or change the
walker's errors to enforce the core limit. A large static list that is wholly
consumed at compile time need not survive in the core.

| List producer/use | Length facts and lowering |
| --- | --- |
| Literal `list` | Its authored number of elements is known. Evaluate elements in order and join their actual element types, then coerce every element to that joined type. Static contents fold; otherwise build a fixed-length list. |
| `range(n)` / `range(a,b)` | Length is `max(0,n)` / `max(0,b-a)`. Static endpoints fold or unroll; input-dependent endpoints produce a bounded dynamic list. End is exclusive and elements are Int. Existing source bounds/overflow behavior remain the native oracle. |
| `linspace(a,b,n)` | Length `max(0,n)`; literal n gives a known count even when endpoints are live. For n=1 the single element is a; otherwise element k is `a+(b-a)*k/(n-1)` in source operation order. |
| `map` | List inputs zip to minimum count; known input counts make the result count known. It may be unrolled only within the lowering budget; otherwise use the same bounded structured loop. |
| `filter` | Result count depends on the predicate; live predicates imply live length even with a fixed source count. Preserve order and joined element layout. |
| `sort-by` / `reverse` | Count is the source count. Compute keys once and sort stably; reverse copies source elements in descending index order. |
| `concat` | Sum input counts, retain input/element order, join element types and coerce, then enforce the source 4,096 cap. No repeated append of an ever-growing prefix. |
| `take` / `drop` / `rest` | `take` retains the first min(max(0,n),length) elements; `drop` removes that many; `rest` removes one if present. A live n or live source length implies dynamic length. |
| `for` / `scan` | Product iteration count, minus skipped tuples, controls the collected length. Later clauses may depend on earlier items. List loops count attempted iterations for the 4,096 bound, including skips. |
| `iterate` | Applies one function `n` times from a seed; `n <= 0` returns the seed. `n` is any Int expression, not a literal, and is not bounded by 4,096. With `:until (fn [prev next] bool)` the first pass whose `next` satisfies it is the last. Ordered coercion/widening as in `reduce`. |
| `fold` / `sum` / `reduce` | No collected output list except the accumulator's own data. Empty fold/reduce returns the seed; empty sum returns Int 0. Ordered accumulation and coercion/widening are preserved. |
| Frame events / state-held list fields | Bounded dynamic length when represented in core; the source frame/event and state identity rules still apply. Length is not made static merely by flattening a record. |

The checker distinguishes live values from live list lengths. Literal list
count is fixed even if elements mention t. Range/linspace counts are marked
live when their arguments are live; take/drop also inherit source-length
liveness. Branches, record fields, graph overrides and state propagate length
facts. `E_TIME_COUNT` rejects a live-length source in a `for` that would
produce graph nodes. A `sum` may consume a live length; `fold`/`scan` may
consume it when their accumulator is data, and packed data arrays may have
live lengths. Shape-producing calls that splice a live list retain the
checker's refusal. Core loops do not bypass checking.

A record element's flattened layout is ordered depth-first by authored field
order. For example `{:p vec2 :label text :style {:width int :visible bool}}`
has lanes `p.x`, `p.y`, `label`, `style.width`, `style.visible`, with their
scalar types. A list is a shared length plus one column per lane. Numeric
columns use the corresponding scalar storage; text and nested-list columns
hold their typed values/descriptors. A nested packed-array field is a buffer
reference/count/width, not thousands of copied scalar lanes. Column i at
row j always belongs to the same logical record, and filtering/slicing moves
all columns together. Layout metadata reconstructs field order, nesting and
component grouping for `show` and `equal?`. Published columns are immutable;
`assoc` on an element creates its changed value, without mutating the source.

`nth` (`sequence/get`) checks a Float index for whole-numberness before
conversion, then checks the converted index against [0,length); failures are
`E_LIST_RANGE`. The non-whole message prints the index with C `%g` (six
significant digits, `1.23457e+08`, `nan`, `inf`), not the display printer; an
Int index is never non-whole. `sequence/build` fills its elements with empty
placeholders (the native `No_geo`), which an ordered store replaces; with a
`width` it allocates zeroed packed storage instead, and the packed ceiling
below applies, not the 4,096 list cap. A sequence
pattern requires at least its number of pattern elements (additional source
elements are permitted); shortage is `E_PATTERN`. Empty `first`/`last` fail
with `E_LIST_RANGE`; empty `rest` remains empty. Packed-array indexing instead
uses `E_ARRAY_RANGE`, non-negative whole numeric indices, and element widths.
Packed arrays never inherit the 4,096 list cap.

## Packed-array leaves

The seven array leaves use element counts. Widths 1/2/3/4 store floats or
interleaved vector components; array length divides storage length by width.
An imported vector storage length not divisible by its width is
`E_ARRAY_TYPE`. Input validation checks every stored component for finiteness.
This validation happens at the input boundary, not on every slice or read.

The count/index conversion first converts the source number to binary64,
then requires a non-negative whole value no greater than
`Sys.max_floatarray_length`; failure is `E_ARRAY_RANGE` with
"<operator> needs a non-negative whole length." It then truncates to Int.
This is distinct from `coerce` and the narrower `4e18` conversion. The native
maximum is the platform storage ceiling; exported counts also obey the
chosen target's allocation and dispatch limits, with located refusals before
allocation. Neither ceiling permits truncating a requested array.

The ceiling is checked twice. The count/index conversion above compares the
count with `Sys.max_floatarray_length` (2^54-1 on 64-bit) and fails with the
"needs a non-negative whole length" message. A builder (`array/*` construction
and `sequence/build` with a width) then requires `count <= ceiling / width`;
a count just below the ceiling but above that quotient fails with `E_ARRAY_RANGE`
"Array length is outside native storage bounds." (`value.ml` `array_init`).
`sequence/build` with a width has only this second check.

| Leaf | Behavior |
| --- | --- |
| `array/max_length(:type float\|int\|text\|vec2\|vec3\|vec4)` | Exact Int logical element capacity, with `float` default. Native `int` and `text` use `Sys.max_array_length`; numeric arrays use `Sys.max_floatarray_length / width`. The selector must be recognized literal Text (`E_ARRAY_TYPE`); the checker rejects non-Text with `E_TYPE`. On Apple64, width one is 18014398509481983; widths two, three and four are 9007199254740991, 6004799503160661 and 4503599627370495. JavaScript returns the identical BigInt. This is a pure value and allocates no array; it does not promise available RAM or browser TypedArray capacity. Constructor limits are unchanged. Ordinary exact Int instructions retain GPU refusal; explicit Float conversion and folding follow ordinary Float qualification. |
| `array/range(count :type float\|int)` | Fresh width-1 storage containing values 0 through count-1. Default type is Float; explicit `int` retains exact integer storage. |
| `array/int(count,value)` | Fresh exact integer storage, with Int zero when value is omitted. Values use the ordinary Int coercion boundary and must then be Int. |
| `array/text(count,value)` | Fresh typed string-array storage, with empty text when value is omitted. Count/get/slice/concat preserve exact decoded string contents and copying ownership. Maps may collect immutable Text leaves in a separate region bank; Text transformations use structured compiled loops. Neither storage has the bounded-list cap. |
| `array/float(count,value)` | Fresh width-1 storage filled with the numeric value; omitted value is Float zero. |
| `array/vec3(count,value)` | Fresh width-3 storage; omitted value is Vec3 zero. A Vec3 fills its components; a scalar Int/Float/Bool broadcasts numerically. Other widths fail with `E_ARRAY_TYPE`. |
| `array/vec4(count,value)` | Fresh width-4 storage with the corresponding Vec4 zero, matching-vector fill and scalar broadcasting. Other vector widths fail with `E_ARRAY_TYPE`. Empty arrays retain the existing constructor's value-validation order. |
| `array/count(array)` | Int element count; non-array or malformed width is `E_ARRAY_TYPE`. |
| `array/nth(array,index)` | Convert index as above, require index < element count, and return Int, Float or the matching vector. Out of bounds is `E_ARRAY_RANGE` ("Array index is outside its length."). |
| `array/slice(array,first,count)` | Convert first and count as above; require first <= length and count <= length-first. Copy the component range into fresh same-kind storage. An empty slice at length is legal. Range failure is `E_ARRAY_RANGE` ("An array slice must be within its source."); non-array is `E_TYPE`. |
| `array/concat(first,second)` | Both operands must have the exact same array kind/width; otherwise `E_ARRAY_TYPE` ("Concatenated arrays must have the same element type."). Copy first then second into fresh same-kind storage, checking target storage bounds. |

Allocation checks count <= maximum component count / width before multiplying.
Each constructed component passes the finite check named `array`. Width-1
construction accepts Int/Float/Bool through number conversion; vectors fail
there with `E_TYPE`. Generic region construction supports Vec2/3/4, broadcasts
numeric scalars, and rejects other element layouts with `E_ARRAY_TYPE`.
Zero-count construction does not examine the fill value's numeric contents;
its argument expression still evaluates. Slice/concat copy existing validated
storage without re-quantizing or coercing components. Published arrays are
immutable, including empty arrays.

Canonical slice/concat expressions invariant in the current packed function
may supply a gather table directly, for Int, Float, Vec2/3/4 and Text. They use
the existing lazy `Array_prepare`: arguments and copying execute only when a
selected lane first reaches the table, in ordinary checked evaluation order.
Each force prepares fresh storage from that force's captures. Row-dependent
composition remains refused by the region tier; its structured Core primitive
is unchanged. Composition does not inherit a source count origin. Prepared
tables retain the existing GPU/WGSL refusal.
An invariant canonical `array/count` uses the existing lazy exact Int capture
path, including when its array is composed; qualification does not force it.
JavaScript kernels whose sources or captures read frame facts use per-frame
preparation even when they declare no computed arguments; fixed kernels keep
the existing static export metadata path.

## Coercion and color leaves

`coerce(wanted,value)` uses a statically supplied checker type. It is a
conversion rather than general validation: Any leaves the value unchanged;
Float to Int uses the coercion rounding above; Bool to Int gives 0/1;
Int/Bool to Float uses numeric conversion; Int/Float to Bool uses `truthy`.
Int/Float broadcasts to Vec2/3/4; Bool does not broadcast in this leaf.
List coercion recursively converts each element into fresh list storage.
Record coercion traverses the actual fields in their original order, converts
fields named in the wanted record, and retains unmatched fields unchanged.
It neither adds missing fields nor sorts them. All other pairs stay unchanged,
including packed arrays and already matching scalars/vectors. During native
staging a Residual stays unchanged until forced; a finished core value cannot
contain one. Layout checking by lowering must not invent a runtime coercion
error for a pair the source leaves unchanged.

`color/parse` returns four byte channels (Ints 0..255) from precisely these
source kinds:

- Text: remove one initial `#` if present. Length 3/4 doubles each character;
  resulting length must be 6/8, otherwise `E_DRAW_COLOR` with
  "Color.hex: expected #rgb, #rgba, #rrggbb, or #rrggbbaa". Each two-character
  channel is parsed in base 16, case insensitive. Besides two hex digits, the
  native integer parser accepts a hex digit followed by `_` as that single
  digit (e.g. `f_0000` is RGB 15,0,0). A leading underscore, sign or any other
  character fails with `E_DRAW_COLOR` ("Color.hex: invalid hexadecimal digit").
  Alpha defaults to 255. No whitespace trimming or named colors. The length
  counts UTF-8 bytes, not characters: `#é1` is three bytes, so it doubles to
  six and fails on its digit, not on its length.
- Vec3: finite-check components named `red`, `green`, `blue`, then convert
  each through the unclamped-input byte path below; alpha is 255. The checks
  run blue, then green, then red (OCaml evaluates the arguments right to
  left), so a vector with several non-finite channels reports `blue` first.
- List of exactly four elements: number-convert and finite-check each channel
  named `color`, then use the unclamped-input path, including alpha. Element 3
  is checked first and element 0 last, so a non-number in any element fails
  `E_TYPE` or `E_NONFINITE` for the highest-indexed offender.

Every other kind, including Vec4 and lists of another length, is `E_TYPE`
("A color is hex text, vec3 or (list r g b a)."). Parsing does not first
coerce a Vec4 into a List. A color returned to Lisp is normalized by dividing
bytes by 255: Vec3 when alpha is 255, otherwise a four-Float List. `color/hsla`
always returns the four-Float List, even for opaque alpha.

`color/byte` has a static conversion mode chosen by the compiler:

1. **Clamped input** (HSL): clamp the finite channel to [0,1], multiply by 255,
   add 0.5, truncate toward zero, then clamp the integer to [0,255].
2. **Unclamped input** (Vec3/List color): multiply the finite channel by 255,
   add 0.5, perform the native Apple-Silicon float-to-integer conversion, then
   clamp the resulting signed 63-bit integer to [0,255]. It is not the checked
   `int` leaf. The intermediate double may overflow: truncate toward zero and
   saturate to signed 64-bit bounds, then keep the low 63 bits and interpret
   them as signed; NaN converts to zero. Positive overflow/infinity therefore
   becomes -1, negative overflow/infinity becomes zero. The input finite check
   still rejects an authored non-finite channel before this conversion.

For example native `Color.of_floats 1e20 (-1e20) 0.5 1.` is RGBA 0,0,128,255;
clamping inputs first would incorrectly make its red channel 255. This mode
also preserves ordinary slightly negative values and half-byte rounding.
JavaScript implements the signed conversion using BigInt after double
arithmetic, then `BigInt.asIntN(63, ...)`, with explicit saturation at +/-2^63.
This is a color leaf parameter, not an extra numeric language operator.

`value/hsv` is a compiler expansion, with hue in turns and no saturation/value
clamp or byte quantization. Set h to
`remainder(remainder(h,1)+1,1)`, i to trunc(floor(h*6)), and f to h*6-float(i).
Set p=v*(1-s), q=v*(1-f*s), t=v*(1-(1-f)*s). The i mod 6 sectors select
`(v,t,p)`, `(q,v,p)`, `(p,v,t)`, `(p,q,v)`, `(t,p,v)`, `(v,p,q)` in order.
Return Vec3 and finite-check its final channels named `value/hsv`.
`value/lerp` evaluates a,b,u once and computes `a*(1-u)+b*u`, retaining that
arithmetic order, returning Float or Vec3. A scalar partner broadcasts to
Vec3. Native Vec2/4 operands are unsupported here (`E_TYPE`), despite the
broader vector arithmetic contract. It does not clamp u; final components
are finite-checked named `value/lerp`. Neither expansion becomes a target leaf.

## Text, display and equality

Text values retain decoded contents, with no quoting, escaping, Unicode
normalization, case folding or trimming at runtime. `str` evaluates its
arguments in order, concretizes each, applies `show` and joins the results
with no separator. The exporter must preserve text contents through its
JavaScript string encoding. Valid UTF-8 uses ordinary JavaScript strings;
exceptional native bytes use reversible low-surrogate escapes, including
through worker structured cloning. Text leaf registers never enter a numeric
operation or a shader.

| Value kind | `show` result |
| --- | --- |
| Int | Full signed base-10 integer, no exponent or floating conversion. |
| Float | The display-number rule below. |
| Bool | `true` or `false`. |
| Text | Its contents, without quotes. |
| Vec2/3/4 | `[` followed by display-formatted components separated by one space, then `]`. |
| List | `[` followed by recursively shown elements separated by one space, then `]`; empty is `[]`. |
| Record | `{` followed by ordered `:name value` pairs separated by one space, then `}`; empty is `{}`. |
| Packed array | Element count, space, multiplication sign, space, element name, e.g. `12 × vec3`. Storage component count is divided by width. |
| Residual in a diagnostic/debug display | `?`; frame program values are concretized before normal display. |
| Other native value | Its type spelling, e.g. `geometry`, `image`, `drawing`, `sample`, `fn`; this does not make the value legal for web export. |

Display-number formatting follows the actual `Value.show_with` implementation,
not the shorter intent comment in `value.ml` and not `Flow.Lisp.float`:

1. NaN displays `NaN`, positive/negative infinity `Infinity` / `-Infinity`.
   These spellings make debug display total; they do not allow non-finite
   numeric inputs or literals into a checked frame.
2. For a finite x, take its absolute binary64 magnitude. Format that magnitude
   to exactly 40 fractional decimal places with nearest-even rounding. A tie
   flag is true when fractional digit 5 is `5` and fractional digits 6..40
   are all zero (digits are counted starting at 1).
3. If flagged, add the binary64 value `1e-6` to the magnitude, rounding that
   addition to binary64. Otherwise retain the magnitude. Format the resulting
   double to exactly four fractional decimal places with nearest-even
   rounding, with a fixed decimal point and no exponent.
4. Remove trailing fractional zeroes and then an empty decimal point. Prefix
   `-` only when x<0 and the trimmed result is not `0`. Both signed zeros and
   a negative value rounded to zero display `0`.

A target can implement the fixed-format steps from the exact binary64 ratio:
decode sign/exponent/mantissa, scale the non-negative ratio by 10^places,
divide as integers, and increment the quotient if remainder exceeds half the
denominator, or equals half with an odd quotient. Decimal padding supplies
the specified fractional width. BigInt supplies these fixed-format steps in
JavaScript; Number supplies the intervening `+ 1e-6` exactly as above. Plain
`toFixed(4)`, `Math.round(x*10000)` or conversion back through Number is not a
complete implementation: large values switch to exponent notation, and the
native tie adjustment can disappear below an ULP.

Examples verified against the native implementation:

| Float | Display |
| --- | --- |
| `0.03125` | `0.0313` |
| `-0.03125` | `-0.0313` |
| `17179869184.03125` | `17179869184.0312` (the 1e-6 tie adjustment rounds away) |
| `1e21` | `1000000000000000000000` |
| `-0.0` | `0` |
| `0.00005` | `0.0001` |

Source-number printing remains separate: `Flow.Lisp.float` tries 15, 16,
then 17 significant digits until the result reads back as the same double,
expands any exponent to fixed decimal notation, and supplies a dot (`1.0`,
not `1`). Non-finite source-print input is `0.0`; a writer that needs to reject
it must do so before printing. Displaying `1.0` gives `1`; writing that
Float back to Lisp gives `1.0`. Neither printer is a reason to change the
other's contract.

`equal?` is typed structural equality, with no scalar numeric coercion.
Reactive fields in a captured list or record are concretized before comparison;
the evaluator's residual representation is never an equality operand.
Int 1 and Float 1.0 are unequal; Bool true and Int 1 are unequal. Floats compare
numerically within their Float tag, so positive and negative zero are equal;
NaN is unequal to itself. Vectors must have the same width and equal ordered
components. Lists/packed arrays must have the same value kind, element layout,
length and equal ordered elements/components. Records must have the same
ordered names and equal values: swapping two field pairs changes equality.
Text compares exact contents, not a collation order. Admissible finite core
data needs no pointer-identity rule. Native function comparison can fail with
`E_TYPE` ("Functions cannot be compared."); exported/core stored data does
not contain callable values. `=` and `case` numeric matching use the separate
numeric conversion rule, rather than substituting `equal?`.

## Region form

The initial body is exactly today's `Flow_ir.Packed.instruction` set, not a
new collection of shader operations:

| Instruction | Meaning |
| --- | --- |
| `Const(value)` | One numeric constant. |
| `Input(source,width,component)` | One component of the current element of a source array. |
| `Uniform(index,component)` | One component of a captured scalar/vector, forced once before dispatch. |
| `Frame(name)` | A numeric frame fact captured from the immutable input snapshot. |
| `Accumulator(component)` | One component of the prior accumulator; prevents independent GPU execution. |
| `Binary(op,a,b)` | A binary leaf over prior scalar slots. |
| `Unary(op,a)` | A unary leaf over a prior scalar slot. |
| `Noise3(x,y,z,seed,octaves)` | A seeded numeric sample; seed and octaves satisfy packed qualification. |
| `Fma(a,b,c)` | Explicit binary64 single-rounding multiply-add of three numeric registers. Input conversion is explicit where needed; untaken guarded instructions do not execute. CPU and JavaScript match `Float.fma` bits; GPU qualification refuses unavailable binary64 arithmetic. Ordinary multiply/add instructions retain their separate rounding. |
| `Checked_mul(projection,a,b)`, `Checked_fma(projection,a,b,c)` | `Value` emits the finite binary64 result or positive zero; `Valid` emits exactly one or zero. Compiler-only record fields connect the two registers. Both projections use existing guards, so untaken branches do not execute. No nonfinite register is published. The single-destination representation computes each projection separately; duplicate FMA work is a measured optimization candidate, not a speed claim. GPU targets refuse these instructions. |
| `Rand_at(seed,index)` | Exact signed63 integer registers to the native `Rand.float_at` Float result. CPU and emitted JavaScript retain all integer bits; GPU qualification refuses this instruction explicitly. |
| `Array_prepare(uniform,type)` | Lazily materialize a checked invariant Int/Float/Vec2/3/4/Text table at its argument's evaluation position. Its compiler-local handle is neither a numeric register nor an output. The table is separate from iteration sources and count/origin proofs. |
| `Gather(table,index,width,component)`, `Int_gather(table,index)` | Read one component or exact signed63 integer from a prepared immutable table. Check the integer element index against native bounds and row count before component address multiplication. GPU targets refuse table preparation and indexed reads, including Float-output kernels. |
| `Text_const(text)`, `Text_input(source)`, `Text_uniform(capture)`, `Text_gather(table,index)`, `Text_select(condition,yes,no)` | Immutable Text leaves in Collect regions: constants, source/capture reads, exact indexed lookup and selected copying. Text has a separate register bank, not a numeric width. Outputs allocate a fresh string array while sharing immutable strings. Text arithmetic, transformations and reductions remain unsupported in regions and use structured Core where eligible. GPU targets refuse all Text registers. |
| `Select(condition,yes,no)` | Selects by condition unequal to zero; branch instructions carry their execution guards. |
| `Guarded(conditions,instruction)` | Executes the instruction and its finite checks only when every condition selects this branch. Metal and WGSL declare the result at the top of its scope (the kernel or loop body) and assign it inside one block shared by consecutive instructions with the same guards (`Kernel_layout.Scope`), so later selections retain the same lazy behavior. An unselected overflow is unevaluated; `min` and `max` still evaluate both arguments. |
| `Loop_begin(first,last,finish,initial,limit,span)` | Starts an end-exclusive counted scope. Its register is the Int index. Bounds and initial values precede the scope; an empty or reversed range returns those initial values. The optional bound is checked before the body, with the authored source span. |
| `Loop_carry(header,component,type)` | One scalar carried register, contiguous after its header. Records and vectors flatten to Int, Float, Bool or Text leaves. |
| `Loop_end(header,next,stop)` | Ends the scope and reads all next values and the optional Bool stop register before updating every carried register simultaneously. A true stop commits the next state and exits immediately. Nested loops finish before the enclosing update. |
| `Loop_result(header,component,type)` | Projects a completed carried value into the enclosing lexical scope. Other body registers cannot escape. |

The binary set is `Add Sub Mul Div Mod Pow Min Max Lt Le Gt Ge Eq And Or`;
the unary set is `Sin Cos Sqrt Abs Not`. Booleans occupy numeric slots, vectors
occupy ordered groups of scalar slots, and instructions refer only to earlier
slots. The register limit is 64. A region carries its sources and widths,
`Zip` or `Product` iteration, captured uniforms, count/origin, skipped tuples,
result mode (`Collect`, `Sum`, or seeded `Accumulate`, with optional collection)
and output slots. Zip uses the shortest input; product uses source-order
strides. No inputs means no iterations. Outputs become an ordered list of
slot groups when several attributes are fused, with an explicit width for
each group. A single-output region keeps the existing instruction spelling.

Single-clause numeric `fold` bodies over canonical `range`, and `iterate`
with optional `:until`, lower to these controls inside packed maps and ordered
reductions. `iterate` retains its uncapped Int count: a nonpositive count
returns the seed, and early exit skips the remaining passes entirely. Its
predicate reads the previous and next state before the update. Checked
anonymous and captured functions and named definitions inline at the call
site; captured closures retain their own lexical bindings. Instruction and capture
sharing respect loop scope. Validation checks matching boundaries, contiguous
typed carried registers, lexical reads, guards and result projections.
The shared typed CPU block engine runs them with its existing chunk pool and
reference lane-error ordering; an untaken or empty body is unevaluated.
JavaScript supports the same controls, including exact Int bounds above 2^53.
Metal and fast WGSL visit their scopes through `Kernel_layout.iter`, keeping
their existing 64-bit integer representations.
Int operations and abs normalize those representations to signed63 results;
logical right shifts first retain the native 63-bit input representation.
This applies to the Metal, fast WGSL/JavaScript and SPIR-V printers. Their
separate whole-Float arithmetic helpers retain 64-bit intermediate storage.
Device kernels OR their status bits into a thread-local word and publish it to
the shared status word with the kernel's one atomic, at its end (a loop over its
pass limit flags bit 0 and runs no pass): bit 0
records nonfinite output, bit 1 a zero integer divisor, and bit 2 an integer
shift count outside `[0,62]`. Fast (approximate-tier) kernels check their inputs
and outputs only, not each operation, and keep only the loop-body instructions
that reach an output, a next value or a stop (`specification/gpu-codegen.md`);
the other tiers check every operation that can introduce a nonfinite value. Guards and empty loops retain lazy errors. Native readback and buffer
borrowing check completion and status before publishing a result; a resident
transaction retains all participating runners' flags until its final read.
An empty dispatch publishes a typed empty output without reading a status
buffer or inheriting a previous generation's failure. The browser checks its
queued status copies at poll/flush boundaries. The aggregate word reports a
stable class priority (invalid shift, zero divisor, nonfinite); it does not
identify the CPU's earliest failing row. Integer failures retain `E_RANGE`,
while nonfinite device output retains `E_KERNEL`.
Existing `int/*` operations and explicit masks express uint32 wraparound;
fast device kernels print such registers as 32-bit unsigned words (`uint`,
`u32`, `OpTypeInt 32 0`) when `Kernel_layout.narrow` proves the value lies in
`[0, 2^32)` or only its low 32 bits are read (wrapping arithmetic, bitwise
operations, selects, carried next values, clamped gather indices), which keeps
the CPU's bits; other Int registers keep the 64-bit representation.
the shared fixture exercises a carried uint32 permutation without another
integer language or executor. Text-carried loops stay on the CPU/JavaScript.
Carried Int/Float widening stays in structured Core: converting the seed to
the later Float type before its first step can lose exact Int bits. Regions
refuse that case, including nested record leaves, until their representation
can retain the initial numeric kind separately. Strict high-Int regressions
cover both fold and iterate without a walker fallback.
Region fusion remaps loop headers, carried registers, boundaries and results.
Instruction sharing stays inside the matching lexical loop scope; control
markers remain distinct and carried registers stay contiguous. A consumer
aliases an eager producer's result instead of copying its instruction across
a guard or loop boundary. The existing count/origin proofs and 64-register
limit still apply, including sibling outputs and ordered reductions. Other
nested collecting/reducing loop forms remain incomplete; this does not make
every authored loop a device kernel.

Numeric `array/nth` inside a kernel uses the same declaration, coercion and
`E_ARRAY_RANGE` messages as scalar indexing. A table can be a checked capture,
record field, helper array argument or invariant constructor. Lane-dependent
tables remain a located compilation refusal. Preparation precedes index
evaluation and follows the same branch guards: an untaken read does not force
its host source, and zero iterations do not resolve tables. Host descriptors
resolve on the initial domain once per selected execution; concrete immutable
tables can be read by workers. No table length truncates the iteration count.
Numeric source validation retains the whole-table finite/type boundary.

Gather programs keep fallible instructions and lazy captures in reference lane
order. Native Collect may execute a leading proven-total prefix over a whole
block: finite constants, checked Int/Text storage loads, wrapping Int arithmetic
and exact comparisons/selections, including guards whose operands precede them
in that prefix. These instructions only write private register banks. Each
block may extend this prefix incrementally through a table already published
after successful validation in the same execution, and through selected reads
whose ready guard/index banks prove every selected index in bounds. The proof
resolves no capture and raises no error. An unresolved table, wrong storage tag,
bad selected index, unproven float check or potentially failing operation stops batching;
the entire remaining suffix keeps reference lane order. Unknown and malformed
values therefore keep their original selected diagnostic position. Prepared
tables remain immutable and their publication cells reset on every execution.
An already published preparation has completed its only effect and is skipped;
admitted reads retain the existing checked span executor.
Parallel Collect
keeps each chunk's first diagnostic and reports the lowest failing chunk,
preserving code, message and cause. Iteration `:skip` omits the body; a discarded
output plane still executes selected reads and finite checks. New outputs stay
private until success, and source buffers remain immutable. Ordered folds,
scans and reductions retain their accumulator order. Text leaves use an
immutable bank in Collect only. Native outputs copy string references without
publishing a source array. JavaScript workers receive invariant string tables
once per execution and return disjoint chunk-local string arrays; the calling
thread assembles a fresh result. Worker tables are released after execution.

Fusion may combine equal-count/equal-origin elementwise producers and several
outputs, with provenance retained. It declines when qualification or register
limits fail; it must not turn a valid unfused program into an error. Static
outputs may be retained independently from frame-rate outputs. Placement is
target legality plus measured cost, never a backend environment selector.

The existing `E_PACKED_*` refusals and their spans survive lowering. GPU
qualification also excludes ordered accumulators, unsupported noise settings,
non-finite/unrepresentable constants and disallowed forms. The web target adds
its device-limit and in-frame-readback checks; their limits are established
from the WebGPU specification, not guessed here. An exact consumer
cannot secretly read a GPU buffer back during a browser frame.

An exported CPU region's input data and uniform captures must be proven
independent of the frame before they become fixed export tables. Frame facts
remain explicit runtime inputs. Comparing prepared values at two sample frames
is an additional check, not proof that a width-dependent or quantized count is
constant. Draw lowering retains structured compiled loops when a region's
sources or captures depend on the frame; it does not freeze their sampled
values. Integer capture storage follows the consuming integer uniform bank;
floating banks convert numerically. A packed vector has floating components
even when one component was computed by an integer instruction.

Fixed numeric lookup captures use typed views of `data.bin`, with explicit
offset, component length and integer-storage metadata. JavaScript keeps exact
BigInt indices until bounds checks finish. The production runtime and worker
pool preserve table constructors and diagnostics; worker failure order is the
lowest failing chunk, independent of completion order. Live tables stay in
structured Core instead of being frozen as export constants.

## State slots and frame facts

A state-slot descriptor has the evaluator's graph instance, lexical
`prefix @ zone` path, iteration tuple and state plane. Numbering a slot does
not replace this identity. Native state keeps the existing serialized key so
`fork_state`, scrubbing and document transactions keep working.

At a new logical frame, the prior pending values merge into the before-state,
and pending writes clear. A state read uses that before-state or evaluates its
initializer if absent. The first write computes and coerces the step; later
reads of that state in the same frame return the same pending result. The
native host plane advances by input `tick`; the drawing plane advances by
`frame`. A changed source or a backwards frame resets the plane. A failed
document/frame transaction restores its complete prior state. Browser program
reload resets all slots, while keeping the canvas.

Stored state is data, recursively: no functions, deferred geometry, images,
drawings, layouts or other deferred nodes. Core lowering keeps call and loop
identity, including skipped iterations, instead of keying state by a transient
statement index alone.

Frame input is an intrinsic read of one immutable `Input.t` snapshot:
`t`, `dt`, `frame`, `tick`, logical `size`, logical `pointer`, held `buttons`,
held `keys`, and ordered `events`. The Lisp `frame/input` view has `t`, `dt`,
`index`, `width`, `height`, `pointer`, `buttons`, `keys`, `events`; it does not
rename the host's internal `tick` into a Lisp field. Event records have the
fixed field layout from `Flow.Op.event_fields`, with empty/zero defaults for
fields absent in an event. Coordinates remain logical points. Device-pixel
conversion belongs only to the presenter.

Native keys, pointer buttons and trackpad phases are the variants in the
dependency-free `Input` module. The SDL adapter translates once; runtime,
Frame and the editor share those types. Lisp event records and held lists
serialize the same documented string names through `Input`'s name functions.

## Drawing statements and intrinsics

The frame produces a painter-ordered mark table. A drawing statement names
its kind, count, attribute edges, constant or buffer attributes, and view
transform. Simple marks are rects, circles, lines and points; other entries
are images, whole-string text runs and nested transform/clip boundaries.
Transforms do not get baked into per-instance attributes. Per-instance size,
radius and color are present from the first contract, not only uniform styles.

The core preserves all existing draw operators. `draw/background` is a filled
canvas rectangle; `draw/merge` appends its inputs in order. `draw/translate`
uses the existing integer coordinate conversion, while rotation is radians
and scale is scalar or two components of a Vec3. Text/image positions also
use their existing integer conversion. Point geometry and packed
rect/circle/line positions retain fractional coordinates. Default ink is white, default circle radius is 1,
default line width is 1, and a rect/circle with neither fill nor stroke gets
white fill. A text run defaults to size 16; empty text emits no entry. An image
defaults to origin, scale 1 and angle 0; its scale is positive and finite.

Rounded rectangles and polygon/polyline paths retain fractional coordinates
through their shared tessellation and stroke rules. Their lowering reuses pure
`Scene_command.Shape2` geometry (including indexed fill triangles and stroke
geometry) and carry it through the drawing contract; replacing a path with
overlapping analytic primitives would change translucent pixels. The geometry
entry and its native serialization oracle preserve this ordering.
The web runtime's `shape2.js` is the single port of `Shape2` (geometry for the mark contract, not a listed
primitive; a node test pins it to the native output); no other target tessellates
or rasterizes these paths on a CPU.

`E_DRAW_LENGTH` guards all per-instance attribute cardinalities; missing
inputs are `E_DRAW_INPUT`, unknown draw kinds are `E_DRAW_KIND`, invalid
coordinates are `E_DRAW_RANGE`, and a non-finite rotate angle or scale
factor is `E_NONFINITE` (as `drawing.ml` raises it). Color decoding keeps
`E_DRAW_COLOR` and its type errors. A count is an element count in every
attribute buffer. Resource entries resolve through an owned host resource
table; they never silently disappear when a resolver is absent.

`Core_draw.device_regions` owns mark-attribute placement for native execution
and web export. It counts direct local reads/writes through the Core traversals
and admits a region only when its single output is read exclusively as
compatible batch attributes. Each target supplies its eligible output widths.
An attribute read by the frame stays on the CPU; other attributes in the same
batch can stay on the device. The pass also returns eligible regions per batch
in argument order, which the web exporter uses for attribute fusion. Native
attribute fusion remains a separate incomplete part of the cleanup.

| Intrinsic | Native target | Web target |
| --- | --- | --- |
| Frame input | Reads the captured logical snapshot. | Reads the translated DOM-event snapshot, with the same Lisp field layout. |
| Image load | Environment-owned SDL image decoding. | Browser image decoding, owned texture and linear sampling; unavailable assets fail explicitly. |
| Image noise | Seeded numeric image producer with owned image storage. | Shared numeric lowering/regions and an owned texture, preserving dimensions and channel layout. |
| Image map | Checked Vec2-to-Vec4 numeric producer over image dimensions. | The same region producer, placed for workers or WebGPU; no per-pixel target interpreter. |
| Text | Rasterizes the whole string with the native font service and presents the texture. | Rasterizes the whole string on a device-scaled text canvas and presents the texture. Font/pixel differences are tested under the browser qualification tolerance. |
| Host quit | The host's conditional quit effect. | Refused when reached: a browser page has no native application quit operation. |
| Host dialog | Native conditional dialog and its ordered result events. | Refused when reached: no native file/folder dialog contract. |
| Host save PNG | Conditional file write through the host. | Refused when reached: a browser download does not satisfy the named-file effect contract. |
| Host play / audio synth / audio load | Owned SDL audio resources, synth settings and conditional playback. | Refused when reached until this target has an explicit audio implementation; never a no-op. |
| Scene, SOP, world, material, PXUI/editor or unknown nominal operation | Remains in its existing native subsystem. | Refused if it contributes to the selected draw program. An unrelated native graph in the workspace does not block export. |

Legality is checked after selecting the draw roots and pruning unreachable
work. A pure data field obtained from a host-context graph is legal if its
actual dependencies are legal. Encountering an unsupported intrinsic is a
source-located refusal even if its run-time condition is currently false;
only a statically pruned branch is absent from the program. A corpus workspace
is either fully represented by its selected draw program or refused, never
partially exported to make a coverage count pass.

## Census rule table

The source census parses 45 files and identifies 15 structurally eligible draw
workspaces. It counts 114 distinct authored heads in draw/host/image/value
graphs and all definition/macro bodies. This includes function parameter
calls, macro templates and SOP definitions unused by a draw graph; it is not
the exported dependency closure. Counts change with authored source, but each
head must have its own row here. `primitive` refers to the inventory above;
`compiler rule` names its leaves/control, `region` names the packed numeric
form, `intrinsic` names a host boundary, and `refused on web` forbids a reached
operation. A user function/macro row is not a new runtime primitive.

| Head | Rule | Lowering / leaves |
| --- | --- | --- |
| `*` | primitive | `*` |
| `+` | primitive | `+` |
| `-` | primitive | `-` |
| `/` | primitive | `/` |
| `<` | primitive | `<` |
| `<=` | primitive | `<=` |
| `=` | primitive | `=` |
| `>` | primitive | `>` |
| `>=` | primitive | `>=` |
| `abs` | primitive | `abs` |
| `add-point` | compiler rule | Inline its checked body: `array/concat`, `array/vec3`, `assoc` field lowering, `+`, coercion. |
| `and` | primitive | `and`; eager arguments. |
| `array/concat` | primitive | `array/concat` |
| `array/count` | primitive | `array/count` |
| `array/nth` | primitive | Scalar `array/nth`; invariant numeric/Text captures use `Array_prepare` and `Gather`/`Int_gather`/`Text_gather`, with lazy preparation, independent counts and CPU/JavaScript execution. |
| `array/max_length` | primitive | `array/max_length` |
| `array/range` | primitive | `array/range` |
| `array/slice` | primitive | `array/slice` |
| `array/vec3` | primitive | `array/vec3` |
| `assoc` | compiler rule | Ordered static field-local replacement/appending and `coerce`. |
| `audio/synth` | refused on web | Native audio-resource intrinsic; do not fabricate a sample id. |
| `case` | compiler rule | One scrutinee local, branches, `=`, `equal?`, `truthy`. |
| `ceil` | primitive | `ceil` |
| `color/gradient` | compiler rule | `color/parse`, `sequence/length`, `sequence/get`, `min`, `max`, `int`, `+`, `-`, `*`, `/`, branch and `fail`. |
| `color/hsl` | compiler rule | `remainder`, `min`, `max`, `abs`, `+`, `-`, `*`, `/`, `<`, `color/byte` and branches. |
| `concat` | compiler rule | Ordered `sequence/concat` and joined-element `coerce`. |
| `cond` | compiler rule | Ordered tests and branches through `truthy`. |
| `core` | compiler rule | Expand the macro; numeric leaves `let*`, `pow`, `mod`, `abs`, `+`, `-`, `*`, `/`, `max`. |
| `cos` | primitive | `cos` |
| `count` | compiler rule | `sequence/length` |
| `crosses` | compiler rule | Expand the image macro of `flow_survey`: `noise3`, `mod`, `abs`, `min`, `max`, `*`, `+`, `-`, `/`, comparisons, `and`, a branch and the draw image entry. |
| `double` | compiler rule | Inline the checked definition: `*` and argument `coerce`. |
| `draw/background` | compiler rule | `color/parse`, frame size, rectangle drawing entry. |
| `draw/circle` | compiler rule | Radius/position locals, `color/parse`, circle entry, defaults. |
| `draw/circles` | compiler rule | Packed attribute edges, `array/count`, `color/parse`, checked circle mark set. |
| `draw/image` | intrinsic | Image-resource entry with checked transform; source image producer is lowered separately. |
| `draw/lines` | compiler rule | Packed endpoint/width/color edges, `array/count`, checks, line mark set. |
| `draw/merge` | compiler rule | Ordered drawing-entry concatenation, flattening drawing lists. |
| `draw/points` | compiler rule | Packed fractional positions, `array/count`, color, point entries with rect antialiasing. |
| `draw/rect` | compiler rule | Position/size locals, `color/parse`, rectangle entry, defaults. |
| `draw/rects` | compiler rule | Constant or packed size/color edges, `array/count`, checks, rectangle mark set. |
| `draw/rotate` | compiler rule | `fail` on non-finite angle, view-transform push/body/pop. |
| `draw/rounded_rect` | compiler rule | Fractional position/size/radius, shared pure path geometry, drawing entry. |
| `draw/scale` | compiler rule | Scalar/component locals, `fail` on non-finite factors, view push/body/pop. |
| `draw/text` | intrinsic | `color/parse`, integer position/size, whole-string text entry. |
| `draw/translate` | compiler rule | Checked integer coordinate conversion, view push/body/pop. |
| `equal?` | primitive | `equal?` |
| `exact` | compiler rule | Identity precision boundary; web placement refuses an in-frame GPU readback. |
| `exp` | primitive | `exp` |
| `filter` | compiler rule | Inline predicate, ordered loop, `truthy`, `sequence/get`, `sequence/build`, copying retained items. Packed output retains packed layout. |
| `field` | compiler rule | Inline checked definition: `field-slot`, `floor`, `min`, arithmetic. |
| `field-slot` | compiler rule | Inline checked definition: `noise3`, `round`, arithmetic. |
| `finish` | compiler rule | Inline checked definition: ordered field updates, `array/concat`, `array/vec3`, list access/build, `coerce`, branches. |
| `floor` | primitive | `floor` |
| `fn` | compiler rule | Specialize and inline the checked body at each call site; retain capture and call identity. |
| `iterate` | compiler rule | One `Counted` loop over `max(0, n)` passes with a held accumulator: the function is inlined once per site (a packed `map` inside it is one region, lowered once and invoked per pass), results widen like `reduce`, and `:until` supplies the loop's stop local so remaining passes are not visited. Native and emitted JavaScript commit the next value before stopping. No 4,096 bound. Numeric forms inside a region use its carried controls. |
| `fold` | region | Numeric packed seeded accumulator; otherwise bounded sequential frame loop with `coerce` and accumulator widening. |
| `for` | region | Numeric packed product/collect; otherwise bounded frame product loop with sequence build and access. |
| `frame/dt` | intrinsic | Snapshot Float `dt`. |
| `frame/height` | intrinsic | Snapshot logical size height, as Int. |
| `frame/index` | intrinsic | Snapshot Int `frame`. |
| `frame/input` | intrinsic | Snapshot record with the Lisp frame/event field layouts. |
| `frame/width` | intrinsic | Snapshot logical size width, as Int. |
| `get` | compiler rule | Static ordered record field/component read, `fail` for `E_FIELD`. |
| `glyph` | compiler rule | Expand macro to image region and draw image entry; `noise3`, arithmetic, comparisons, branches. |
| `grad` | compiler rule | Expand gradient macro: `+`, `-`, `*`, component reads. |
| `half` | compiler rule | Inline definition: `/` and argument `coerce`. |
| `hash` | compiler rule | Expand the workspace macro of `million_marks`: `abs`, `mod`, `*`, `+`, `sin`. |
| `hatch` | compiler rule | Expand the image macro of `flow_survey`: `noise3`, `mod`, `abs`, `*`, `+`, `-`, `/`, comparisons, `and`, a branch and the draw image entry. |
| `haze` | compiler rule | Expand image-body macro: `pow`, `mod`, `max`, `+`, `-`, `*`, `/`. |
| `host/dialog` | refused on web | Native conditional dialog intrinsic and result-event boundary. |
| `host/play` | refused on web | Native conditional sample-playback intrinsic. |
| `host/quit` | refused on web | Native conditional quit intrinsic. |
| `host/save_png` | refused on web | Native conditional file-write intrinsic. |
| `if` | compiler rule | `truthy` and one selected structured branch. |
| `image/map` | region | Checked Vec2-to-Vec4 pixel body, explicit width/height and image-resource result. |
| `image/noise` | intrinsic | Seeded numeric image producer; lower its numeric work to the shared core/regions. |
| `inside` | compiler rule | Inline checked definition: component reads, `>=`, `<`, `+`, `and`. |
| `int` | primitive | `int` |
| `float` | primitive | `float` |
| `float/mul_checked` | primitive | Finite scalar value/status record, separately rounded product. |
| `float/fma_checked` | primitive | Finite scalar value/status record, explicit single rounding. |
| `int/and` | primitive | `int/and` |
| `int/div` | primitive | `int/div` |
| `int/mod` | primitive | `int/mod` |
| `int/mul` | primitive | `int/mul` |
| `int/xor` | primitive | `int/xor` |
| `key/down` | compiler rule | Frame keys, ordered text membership with `sequence/length`, `sequence/get`, `equal?`. |
| `let*` | compiler rule | Ordered assignments, pattern reads and `coerce`. |
| `lines` | compiler rule | Expand contour macro to image region: `noise3`, arithmetic, `mod`, comparisons and branches. |
| `list` | compiler rule | Fresh `sequence/build`, ordered element stores and joined-layout `coerce`. |
| `make` | compiler rule | Specialize the callable parameter of `ring` and inline its checked body; SOP instantiations are refused on web. |
| `map` | region | Numeric packed zip/collect; otherwise inline body in bounded frame zip loop with sequence access/build. |
| `max` | primitive | `max` |
| `min` | primitive | `min` |
| `mod` | primitive | `mod` |
| `noise3` | primitive | `noise3`; also the region `Noise3` instruction. |
| `not` | primitive | `not` |
| `nth` | compiler rule | Checked index conversion, `sequence/get`; preserve `E_LIST_RANGE`. |
| `or` | primitive | `or`; eager arguments. |
| `palette-color` | compiler rule | Inline checked definition: `int/mul`, `int/and`, `int/xor`, `int/div`, `int/mod`, arithmetic, comparisons, branches and the `color/hsl` lowering. |
| `panel` | compiler rule | Expand shader macro to ordered image entries and pixel regions using its constituent macros. |
| `pointer/down` | intrinsic | Snapshot primary-button state, as Bool. |
| `pointer/x` | intrinsic | Snapshot logical pointer x, as Float. |
| `pointer/y` | intrinsic | Snapshot logical pointer y, as Float. |
| `pow` | primitive | `pow` |
| `range` | compiler rule | Checked bounded count, `sequence/build`, integer induction locals; start inclusive, end exclusive. |
| `ref` | compiler rule | Checked graph specialization/sharing, input `coerce`; refuse a reached native-only context. Native lowering inlines the body of a `ref` without overrides in the graph's cached default instance (state keys are the graph's own, so every reference shares them); a `ref` with overrides is refused by name. |
| `rest` | compiler rule | `sequence/length`, `sequence/slice`, with an empty-list identity. |
| `reverse` | compiler rule | Ordered loop, `sequence/build`, `sequence/get` at length minus one minus index. |
| `rim` | compiler rule | Expand image-body macro: `pow`, `mod`, `abs`, `max`, arithmetic. |
| `ring` | compiler rule | Expand the shader macro or specialize the checked definition at its resolved call site; an instantiated SOP body is refused on web. |
| `round` | primitive | `round` |
| `scheme-hash` | compiler rule | Inline checked definition: `int/mul`, `int/and`, numeric conversion. |
| `sin` | primitive | `sin` |
| `sop/box` | refused on web | A geometry factory in a definition counted by the census, outside the 2D target. |
| `sop/merge` | refused on web | Native geometry composition; not a numeric sequence operation. |
| `sop/transform` | refused on web | Native geometry transform; not a drawing view transform. |
| `sop/delete_attr` | intrinsic | Geometry metadata deletion by owner and Houdini-style name pattern. The host protocol is an immutable geometry record with ordered attribute records carrying owner, name and payload. Native uses `Rdk.Attribute_ops.delete`; the JavaScript twin preserves topology, positions, ordering and unselected payloads. Outside the current 2D export scope. |
| `sop/attr`, `sop/with_attr` | intrinsic | Cooked owner-aware attributes, default point/vec3. Float and tuple storage use packed arrays; full vec2/3/4 writes additionally accept exact Float-plane records x/y, x/y/z or x/y/z/w (field order unrestricted, no extra/duplicate keys). Each plane matches the owner count and is finite-checked, then copied into separately owned native planes; native capacity is scalar per plane, without a width-divided tuple buffer. Reached host-source fields hydrate in authored order before direct storage dispatch; selected writes remain tuple-array-only; integer storage stays exact signed native integers; text storage uses typed string arrays with exact decoded contents. `float_array` and `int_array` use records of integer CSR offsets and typed flat values; offsets start at zero, remain monotone and end at the flat count. Missing/wrong storage is `E_ATTR_TYPE`, owner cardinality is `E_ATTR_COUNT`, nonfinite computed writes are `E_NONFINITE`. Unchanged native attribute queries share immutable storage and preserve its bytes. Point P is canonical vec3 positions. Host geometry attributes carry owner, name, type and immutable copied values; twins preserve topology and other payloads. Outside the current 2D export scope. |
| `sop/has_attr` | intrinsic | A genuine Boolean cooked host source, with owner and optional literal storage type. Without type it tests any storage; with type it tests exact storage. Point P always has vec3 storage. Lazy branches can explicitly preserve native absent/wrong-normal fallbacks without reading nonexistent data. Sources retain truthful geometry/attribute dependencies. |
| `sop/with_attr :selection :default` | intrinsic | Numeric selected update through the ordinary writer. An owner-sized zero/one Int mask selects finite replacement array rows; unselected rows preserve matching native storage bytes, including nonfinite values, or use the explicit finite default row for absent/wrong storage. Both keywords are required together, with numeric Float/Vec2/Vec3/Vec4 types. Counts/types/bits retain `E_ATTR_COUNT`, `E_ATTR_TYPE`, `E_ATTR_SELECTION`; computed values retain `E_NONFINITE`. Sources and frame/state dependencies include selection/default captures. The JavaScript geometry host twin agrees; reached geometry remains outside the 2D exporter. |
| `sop/attr_checked` | intrinsic | Cooked numeric attribute query returning owned `values` and exact integer `valid` arrays. Float/Vec2/Vec3/Vec4 on every attribute owner use the ordinary query's defaults and missing/type errors. A row is valid iff all components are finite; valid bytes, including signed zero, are retained and every invalid row component becomes positive zero. Nonnumeric storage is `E_ATTR_TYPE`. Selection and error policy remain in the Lisp body. Named attribute dependency and lazy source validation remain explicit. The JavaScript host-protocol twin agrees; reached cooked sources remain outside 2D web export. |
| `sop/count` | intrinsic | Cooked topology count over the immutable geometry host protocol. Selectors `points`, `vertices`, `primitives`, `edges` return exact signed native integers. Invalid selectors are `E_TOPOLOGY_COUNT`; unavailable geometry is `E_DATA_SOURCE`. Reads depend on topology, rather than coordinate or attribute values. Outside the current 2D export scope. |
| `sop/edges` | intrinsic | Cooked topology source returning separate exact integer arrays `a`, `b`, with normalized endpoint pairs in first outgoing-corner occurrence order; self-edges remain and open polylines have no outgoing final corner. Arrays are owned copies. Native indexing uses `Topology_index`; JavaScript preserves the same order. Unavailable geometry is `E_DATA_SOURCE`. Outside the current 2D export scope. |
| `sop/topology` | intrinsic | Cooked geometry source returning owned exact integer arrays in `{:vertex_points ... :primitive_offsets ... :kinds ...}`. Kind codes are 0 polygon, 1 open polyline and 2 closed polyline. Vertex points have vertex-owner count, kinds primitive-owner count and offsets primitive count plus one; the three count origins remain distinct through direct field access and captured records. Reads depend on topology, rather than positions or attributes. The JavaScript geometry host twin preserves the same arrays. The JS frame printer refuses explicit host sources with `E_WEB_SOURCE`; the production 2D exporter refuses a reached SOP graph with `E_WEB_GRAPH`. |
| `sop/adjacency` | intrinsic | Cooked geometry source returning owned exact integer arrays in `{:primitive_of_vertex :next_vertex :previous_vertex :edge_of_vertex :opposite_vertex :edge_a :edge_b :point_edge_offsets :point_edges :point_vertex_offsets :point_vertices}`, copied from `Topology_index.Private.view`. Vertex rows use -1 for absent neighbours or edges; edges keep `sop/edges` order; point rows are CSR with ascending vertex and edge indices. The JavaScript geometry host twin rebuilds the same tables and agrees on every topology fixture. Reads depend on topology only; outside the current 2D export scope. |
| `sop/fans` | intrinsic | `(sop/fans geometry)` returns `{:first :corners}`, owned exact integer arrays with one row per slot of `sop/adjacency`'s `point_vertices`: the first vertex of the slot's face and the corners its fan covers (0 unless the face is a polygon of three corners or more). `sop/transform` reads them in one load each to gather a point's area normal. Native only, kept with the topology like the adjacency. |
| `sop/components` | intrinsic | `(sop/components geometry :over "edges" :mask m)` returns `{:id (array int) :count int}`, the labelling of `Analysis.classify_connectivity`. `:over "edges"` (default) labels points joined along edges; `:over "points"` labels primitives joined through shared points. Classes number included elements by first element. The optional 0/1 `:mask` has the labelled owner's length and excludes elements (id -1, not counted); the optional 0/1 `:seams` has one entry per `sop/edges` edge (`E_COMPONENTS_SEAMS`): a seam edge joins nothing, and `:over "points"` then joins primitives along shared non-seam edges instead of shared points (`Analysis.classify_connectivity ~seams`); a bad `:over` is `E_COMPONENTS_OVER`, a bad mask `E_COMPONENTS_MASK`. Native uses the existing union-find; the JavaScript geometry host twin agrees on every topology fixture. Reads topology only; outside the current 2D export scope. |
| `sop/triangles` | intrinsic | `(sop/triangles geometry)` returns `{:offsets :ok :primitive :a :b :c}`, every primitive through RDK's shared ear clipper (`Polygon_triangulation.primitive`) in its emission order, with no three-corner or collapsed-quad shortcut: `:a :b :c` are the source vertex indices of each triangle, `:primitive` its primitive, `:offsets` the per-primitive CSR into the triangles, and `:ok` 0 where the clipper refused (curve, non-finite, degenerate projection, non-simple), which then emits nothing. It never refuses; nodes keep their own messages. The C.15 canonical triangulation capability (`attribute_laplacian`, `measure`, `scatter`). Reads positions and topology; rdk-backed, no JavaScript twin (compiled with jsoo later). |
| `graph/shortest_paths` | intrinsic | `(graph/shortest_paths adjacency lengths roots :radius r :hops h :blocked_points bp :blocked_edges be :target t :ties s)` over the `sop/adjacency` record (point/edge fields), one non-negative float length per edge and integer root points returns `{:distance (array float) :parent (array int) :path (array int) :order (array int)}`. Default search is `Rdk.Shortest_paths.search`, the heap factored out of `Transform_ops` edge distances: equal distances settle by lower point index and a point's parent is the first settled neighbour giving its final distance. Roots have distance 0 and parent -1; points beyond `:radius` (default unbounded) or unreachable report distance -1 (Flow floats are finite) and parent -1. `:hops true` runs `Rdk.Shortest_paths.hop_search` (the search `Group_path` uses, so native and Lisp are one implementation): layer-synchronous breadth first, a point takes the predecessor in the previous layer with the least accumulated length, ties to the lower predecessor index; `:blocked_points` (one 0/1 per point) are never entered and `:blocked_edges` (one 0/1 per edge) never crossed, roots and `:target` exempt from the point mask; with `:target` the search stops once the target's layer is settled, so other points deeper than the target stay unreached. `:path` is the parent chain of `:target` root first, empty without a target or when it is unreached. `:ties "lowest"` runs `Rdk.Shortest_paths.forest_search`, the Edge Transport forest (shared with `Edge_transport`): the heap orders by (distance, root point, point) and an unsettled neighbour at an equal distance takes the smaller root and then the smaller parent instead of the first settled one; it takes `:blocked_edges` (no `:blocked_points`, no `:radius`) and a root is its own tree. `:order` lists the reached points in the order the search settled them (`:hops` and `:ties "lowest"` only; empty for the default search). Refusals: `E_GRAPH_ADJACENCY`, `E_GRAPH_LENGTH` (also a non-finite accumulated hop length), `E_GRAPH_ROOT`, `E_GRAPH_RADIUS` (also with `:hops`), `E_GRAPH_TARGET`, `E_GRAPH_MASK`, `E_GRAPH_OPTION` (a block mask or a non-boolean `:hops` without `:hops true`). A host source over concrete arrays with a JavaScript twin (`graph_shortest_paths`, options positional, null when absent) fixture-checked on every topology plus ties. |
| `graph/forest` | intrinsic | `(graph/forest parents :mask m)` over one integer parent per point (and an optional 0/1 selection mask) returns `{:order :depth :root :offsets :children}` from `Rdk.Forest.scan`, the tree scan of Edge Transport Parent. A parent that is out of range, the point itself or unselected makes the point a root; unselected points belong to no tree. `:offsets`/`:children` are the children CSR over every selected non-root point in index order; `:order` lists roots by increasing index, each followed by its tree breadth first (members of parent cycles are reachable from no root, so absent from `:order` and `-1` in `:depth` and `:root`); `:depth` is hops from the root and `:root` the root point. Refusals: `E_GRAPH_PARENT`, `E_GRAPH_MASK`. JavaScript twin `graph_forest(parents, mask)`. |
| `graph/color` | intrinsic | `(graph/color rows order)` takes neighbour rows `{:offsets (array int) :values (array int)}` and the node order to color, returning `{:color (array int) :count int}`. Each node in `order` takes the smallest color unused by its already colored neighbours (`Rdk.Graph_color.greedy`, the first-fit rule of `Graph_color.run`); nodes outside `order` stay -1. `order` must list nodes at most once (`E_GRAPH_ORDER`); malformed rows are `E_GRAPH_ADJACENCY`. JavaScript twin `graph_color`. |
| `graph/chains` | intrinsic | `(graph/chains adjacency :mask m)` walks the selected non-self edges (0/1 `:mask`, one entry per unique edge) with `Rdk.Poly_path.chains`, the traversal `PolyPath` uses (branch paths by point number, then isolated loops), returning ragged `{:offsets :values :closed}`: point rows, `closed` 1 for an isolated loop whose first point repeats last. `E_GRAPH_MASK`, `E_GRAPH_ADJACENCY` refuse. JavaScript twin `graph_chains`. |
| `sop/mesh` | intrinsic | Geometry from four required keyword arrays: `:points` Vec3, `:vertex_points` Int, `:primitive_offsets` Int and `:kinds` Int. Ordinary Flow point layout/finiteness is checked first, then 0/1/2 kinds, then native `Topology.create_owned` validation. Invalid kinds report `E_TOPOLOGY_KIND`; native topology errors report `E_TOPOLOGY` with the unchanged native message. Owned copies protect retained arguments; no attributes, groups or instances are implied by these four arrays. Live/state/zone captures use the ordinary retained data and cooked-call boundaries, with lazy selected errors and causes. The JavaScript host twin agrees; SOP geometry remains outside current 2D export. |
| `sop/mesh_planes` | intrinsic | The same owned topology/geometry boundary with a required `:points` socket typed as exactly `{x: Array(Float), y: Array(Float), z: Array(Float)}`. Plane lengths must agree (`E_ARRAY_COUNT`); point finiteness, kind codes and native topology validation retain Mesh ordering. Native construction copies three owned planes directly and avoids an interleaved Vec3 buffer, including the reference path. Other required keyword rows remain exact Int arrays. Known extra/missing/wrong typed fields are checked errors; concrete runtime records are validated too. Existing Mesh remains Vec3-array typed. Cards, defaults, menu insertion, source save, live/state/zone captures and host JavaScript twins use the ordinary paths; SOP geometry is still refused by 2D export. |
| `sop/group` | intrinsic | Cooked point/vertex/primitive/edge group mask as an owned exact integer array of zeros and ones. `:owner` selects the source group. Optional `:as_owner point` returns a point-length mask containing any point incident to a selected source element; omitted promotion preserves the source-owner mask. Repeated corners and shared edges contribute membership once, and isolated points are unselected for non-point sources. This mask does not carry traversal order. Missing source groups report `E_GROUP_MISSING`; invalid promotion targets report `E_GROUP_TARGET`. Group names retain exact byte spelling and native ASCII whitespace validation. Packed count origins use the output point owner for promoted masks. Geometry host groups preserve owner, name, membership and optional explicit traversal order; `sop/group_order` remains a query of the source group. Outside the current 2D export scope. |
| `sop/group_names` | intrinsic | Owner-filtered names in native insertion order using the shared Houdini-style wildcard, class, escape and exclusion rules. Invalid patterns report `E_GROUP_PATTERN`; names remain exact text arrays. |
| `sop/group_entries` | intrinsic | `(sop/group_entries geometry pattern :owner "any")` returns ordered parallel text arrays in `{:owners ... :names ...}`. Ordinary groups retain global insertion order across owners, followed by edge groups. Owner filtering and patterns use the same rules as `sop/group_names`; invalid owner/type/pattern or unavailable geometry retain the existing group-source diagnostics. Arrays have no 4,096-entry bound. The native and JavaScript host protocols agree; reached cooked sources remain outside 2D web export. |
| `sop/attr_entries` | intrinsic | `(sop/attr_entries geometry :owner point)` returns the record `{:name :type :owner}` of parallel text arrays over the stored attributes of one owner (`point`, `vertex`, `primitive`, `detail`; `any` lists every owner) in native attribute order, `type` spelled as `sop/attr_type` does and `owner` as lower-case owner text. Canonical `P` is not a stored attribute and is absent, as in `sop/attr_names`. Dynamic native sources use `Host_source`; 2D web export refuses those sources with `E_WEB_SOURCE`. |
| `text/float` | intrinsic | Pure Float-to-Text using `Param.float_text`: the shortest round-trip digits, preserving signed zero. Encoded `group_ranges` and `group_promotions` tables use the same formatter as native parameters. No JavaScript twin; dynamic native source. |
| `text/glob_literal` | intrinsic | Pure nonempty Text-to-Text quoting for the native attribute/group pattern parser. Escapes wildcard/class/escape/exclusion syntax and its ASCII separators (space, TAB, LF, CR, FF); hyphens, VT and other bytes retain their literal spelling. Empty input is `E_METADATA_PATTERN`; wrong input type is `E_METADATA_TEXT`. Native bytes remain exact, including invalid UTF-8, through the shared reversible JavaScript Text codec. Dynamic native sources use `Host_source`; 2D web export refuses those sources with `E_WEB_SOURCE`. |
| `text/group_name_valid`, `text/group_name_forced` | intrinsic | Pure Text predicates over the native Groups-from-Name spelling (`Rdk.Group_ops.valid_group_name` / `force_valid_group_name`): valid means nonempty ASCII letters, digits and underscore without a leading digit; forced turns every other byte into `_` and prefixes `_` when the first byte is a digit (a UTF-8 character becomes one `_` per byte; empty stays empty). Wrong input type is `E_METADATA_TEXT`. Dynamic native sources use `Host_source`; 2D web export refuses those sources with `E_WEB_SOURCE`. |
| `text/glob_rewrite` | intrinsic | Pure `(text/glob_rewrite pattern replacement text)` uses the same compiled wildcard captures and replacement rules as native metadata renaming, returning the original text when no term matches. Invalid pattern/replacement pairs are `E_METADATA_PATTERN` with the native parser message; wrong arguments are `E_METADATA_TEXT`. Native/JavaScript fixtures preserve classes, exclusions, escaped literals and UTF-8. Dynamic native sources use `Host_source`; 2D web export refuses those sources with `E_WEB_SOURCE`. |
| `text/glob_rewrite_set` | intrinsic | Pure `(text/glob_rewrite_set pattern replacement text)` renames with a whole-token set, as native `Attribute_pattern.rewrite_set`: whitespace-separated terms (ASCII space, TAB, LF, CR, FF; a backslash escapes a separator), every positive source term paired in order with one replacement term, exclusion terms consuming none; when several terms match the last wins and an unmatched name is returned unchanged. `text/glob_rewrite_set_error pattern replacement` returns the native compile error text or `""`. Invalid sets are `E_METADATA_PATTERN`; wrong arguments are `E_METADATA_TEXT`. Native/JavaScript fixtures cover tab and newline separators, escaped spaces, exclusions and UTF-8. Dynamic native sources use `Host_source`; 2D web export refuses those sources with `E_WEB_SOURCE`. |
| `text/glob_rewrite_set_matches` | intrinsic | Pure `(text/glob_rewrite_set_matches pattern replacement text)`: whether some positive term of the rename set matches the whole name (native `Attribute_pattern.rewrite_set` returns a name), so a caller can refuse an unmatched name that `text/glob_rewrite_set` would return unchanged. Same errors and dynamic-source handling as `text/glob_rewrite_set`. |
| `sop/group_order` | intrinsic | Record of Boolean `ordered` and exact integer `indices`, preserving ordered-empty versus unordered groups. Edge groups return false and an empty index array. |
| `sop/with_group` | intrinsic | Immutable owner-aware mask write, preserving topology and unrelated payloads. Optional integer order must enumerate selected elements once, or an order record preserves the query's ordering state. Edge explicit order is `E_GROUP_OWNER`; wrong cardinality, mask or order is `E_GROUP_COUNT`, `E_GROUP_MASK` or `E_GROUP_ORDER`. |
| `sop/context_seed` | intrinsic | A defnode body cook-context constant, specialized before core lowering. Its literal namespace and node label are UTF-8 FNV-1a hashed into 64 bits, multiplied by `0x9e3779b97f4a7c15`, xor-ed with the context seed, then xor-ed with the unsigned upper 32 bits and truncated to a signed native 63-bit int. The optional literal `:salt` int (low 63 bits of a 64-bit word, bit 63 clear) is xor-ed into the identity before the multiply, as native `mixed_seed(context, identity xor salt)` does; absent, the result is unchanged. Fixtures pass the implicit context as a record of decimal seed text and label; use outside a body or in a pure parameter validator is `E_NODE_CONTEXT`. |
| `sop/uv_sphere` | refused on web | Native geometry factory. |
| `split-level` | compiler rule | Inline checked definition: packed slices/concatenation, nested maps, `value/rand`, arithmetic and numeric branches. |
| `sqrt` | primitive | `sqrt` |
| `state` | compiler rule | Identified state-slot read, init/step, `coerce`, transactional write. |
| `str` | compiler rule | Ordered `show` calls and `text/concat`. |
| `take` | compiler rule | Clamp count, `sequence/length`, `sequence/slice`. |
| `value/polar` | compiler rule | `*`, `cos`, `sin`, vector construction with optional height. |
| `value/rand` | primitive | `value/rand` |
| `values` | compiler rule | Static ordered record construction/field locals, used by a counted SOP definition. |

## Verification and sources

The live census checks this table and its primitive count and semantics. Native
execution is checked against the tree
walker, region instruction snapshots and unchanged Metal goldens, native draw
serialization snapshots, and native pixels. Web primitives have native-output
fixtures; CPU frames compare exact structure and attributes within 1e-6 over
120 fixed-step frames. Browser pixels and native/web performance have separate
qualification and measurement gates. A green scalar test cannot substitute for
those whole-program and presenter gates.

The source contracts read for this specification are `lib/flow/value.ml`
(arithmetic, coercions, display and arrays), `lib/flow/op.ml` (operators and
frame/event records), `lib/flow/eval.ml` (control, function identity and state),
`lib/flow/workspace.ml` (static count/type restrictions),
`lib/flow_ir/packed.ml` and `packed.mli` (regions and refusal codes),
`lib/flow_ir/operators.ml` (noise and colors), `lib/rays_math/color.ml`,
`noise.ml`, `rand.ml`, and `lib/rays_editor/drawing.ml` / `lib/rays/scene.ml`
(drawing defaults and coordinate conversion). The runnable census is
`dune build tools/core_census.exe && _build/default/tools/core_census.exe`.

### Lazy diagnostic mapping

`(with_error protected mapper)` preserves the protected value's type and maps
only a reached typed failure. The pure mapper takes and returns a record with
`code: text`, `message: text`, and `cause: list text` (zero or one cause). It
always rethrows; it does not recover a value, retry a child, or catch cancellation
or unrelated host exceptions. Hints, ordered native trace and source location
survive mapping. A failing mapper is not caught recursively by itself.

The checked term stays residual during static evaluation. Structured Core's
`Protect` executes the protected block and completes a direct cooked result
inside its scope before invoking a handler. It does not hoist child construction,
numeric Invokes or errors across the scope; every reached numeric kernel still
needs genuine compilation. `$core/complete` is a private host request and returns
the same handle after cooking/memoizing its payload, diagnostics and instances.
Existing `$core/cooked` retains its lazy handle behavior.
Diagnostic scopes in unused `let*` bindings still execute: static evaluation
retains the enclosing expression for the execution boundary, including scopes
inside called local helpers. Nested empty-geometry results need no host lookup.

Initial scope excludes state, drawing/UI effects, graph references and containers
of cooked values. Pure numeric/data values and direct geometry/image results are
supported. Existing public input edges cook before a body-local boundary; put a
producer inside the protected expression to protect its deferred cook. JavaScript
uses the same structured blocks and diagnostic metadata; host-owned geometry
completion remains an explicit `E_WEB_SOURCE` refusal. Per-element packed/GPU
error boundaries are refused rather than interpreted or retried.

### Diagnostic row policies

`(error_rows flags mapper [:policy "first"|"range-last"])` is a diagnostic
boundary over a genuine Int array. Every row must be 0 or 1; validation reads
the entire immutable mask before reporting an ordinary failure. Empty or
all-zero masks return `nil` and evaluate neither the mapper nor a host lookup.
For a failure, the pure Int-to-diagnostic mapper runs once and always throws;
its row index cannot be returned as successful numeric data. Mapping retains
the same lossless diagnostic envelope and cancellation rule as `with_error`.

The default `first` policy is host-independent. `range-last` matches native
bulk diagnostics: choose the first grain range containing a 1, then the last
1 in that range. The host supplies cook grain through the private diagnosis
resolver, rather than an ordinary Lisp value or operator. Valid geometry,
array values and output fingerprints remain independent of grain. The native
selector uses overflow-safe range bounds and coarse cancellation checks;
errors never enter a persistent successful-output cache. The row producer
must itself retain the target native operation's validation and error order.
This boundary does not turn an arbitrary early-throwing numeric kernel into a
mask producer.
Initial host-independent full-mask validation has no native cancellation token;
Session protects entry/completion, while native failing-row selection polls its
token. The implementation records this validation-latency ceiling explicitly.

Structured Core owns the nonescaping mapper index local and lazy block. The
current 2D JavaScript emitter supports `first` and refuses reached
`range-last` with `E_WEB_SOURCE`. The separate geometry host twin binds private
`{grain, check}` context to the same deterministic selector and is tested with
real checked raw-attribute validity, including grain boundaries and tails.
There is no authored grain getter, numeric result, GPU fallback or automatic
change to ordinary packed arithmetic.

Checked geometry rest collections and their live `map`/`filter` results use native
array capacity in structured Core, matching the evaluator's existing variadic
geometry collection semantics. Their private `Sequence_build` flag follows the
static `list geometry` type; it is unavailable as authored numeric data or a Lisp
option. Repeated cooked handles keep their source order and output multiplicity;
the host still cooks each dependency once. Ordinary value lists, `concat`,
`range`, and authored `for`/`scan`/`fold` iteration bounds retain their existing
4,096 limits. A large dynamic AST remains subject to the independent compiler
budget. JavaScript mirrors typed collection building; reached cooked captures
continue to require a geometry host and are refused by the current 2D exporter.

### Compiled region argument binding in JavaScript

When any region in a frame consumes computed arguments, every CPU Invoke in that
frame carries a compiled preparation plan, including argument-free producers.
This evaluates nested producer sources and captures against the actual current
frame. Frames whose regions are all argument-free retain their existing fixed
`data.bin` preparation and artifact contract.

An argument-bearing `Core.Invoke` passes its actual computed locals and a private
compiled preparation plan to the runtime. Preparation uses ordinary Core lowering
and the frame printer; it is not an evaluator for workspace terms. The internal
`$core/argument` marker is available only to those preparers. Other cooked/host
sources retain `E_WEB_SOURCE` in the 2D target. Existing fixed export admission
and its `data.bin` path remain unchanged.

The invocation validates argument count, exact storage tags, vector shapes and
finite ordinary values before preparing sources. Source preparation keeps written
Zip/Product order, including suppression after an empty Product clause. Actual
source lengths determine iteration and output counts; native language capacity
checks precede the separate browser addressing/allocation guard. Output storage
follows the terminal registers, including exact signed63 Int and immutable Text.

Captured uniforms are compiled synchronous functions, memoized only when a
selected instruction reads their slot. Empty outputs and untaken guards do not
force captures. Stateful/call-index or asynchronous capture preparers are target
refusals. Workers reconstruct typed immutable argument wrappers without numeric
conversion, then use the same lazy capture mechanism and earliest-chunk failure
order. Each force has fresh values; only immutable preparer functions are retained
in a FIFO capped at 64. Region source and argument buffers retain existing
immutable ownership and copying rules. This bridge does not add SOP web export.

### Exact single-source geometry payload remap

`sop/remap` is a cooked geometry host intrinsic. It adds no numeric Core or
Region instruction, shader capability, or native SOP callback:

```lisp
(sop/remap source target
  :points point_sources :vertices vertex_sources :primitives primitive_sources
  :positions "target"
  :edges {:offsets edge_offsets :values edge_sources})
```

The three required integer arrays map each **target** owner row to a **source**
row. They must match target Point, Vertex and Primitive counts; every source
index must be in bounds or `-1`. `-1` is a created element: every attribute
storage takes its default (Float 0, Int 0, Text "", vector components 0, an
empty ragged row, the same defaults as `Point_generate`'s created points), it is in no
group, and under `:positions "mapped"` its P is the origin. Any other negative
index is `E_REMAP_INDEX`.

**Leading target attributes.** `:lead true` lists the attributes already on `target` before the
carried source attributes (native `Revolve` conses its generated `uv` in front); default `false`
appends nothing (the target's own attributes are otherwise dropped). `:lead_groups true` does the
same for groups and edge groups (native `Sweep` lists the backbone's groups ahead of the
cross-section's); the target's groups are valid as they are because the topology is the same.

**Weighted carry.** In place of `:points`, `:vertices` or `:primitives` an owner may be given
`:point_mix`, `:vertex_mix` or `:primitive_mix` (not both, `E_ARGS`), a record
`{:a (array int) :b (array int) :t (array float)}` (exactly those three keys, each as long as the
target owner count): new element `i` is the mix of source rows `a[i]` and
`b[i]` at weight `t[i]`. Both indices must be in `[0, source count)` (`-1` is not a weighted
row: `E_REMAP_INDEX`), `t` must be finite (`E_REMAP_WEIGHT`; values outside `[0,1]`
extrapolate), and any other shape is `E_REMAP_TYPE`. The rule applies to every owner given a
weighted map, to attributes, groups and `:positions "mapped"` alike:

- Float, Float2/3/4 components: `a + (b - a) * t`, or `a * (1 - t) + b * t` when the delta
  `b - a` is not finite (the `Curve_ops.interpolate_array` rule). A Float3 attribute named
  `N` is then renormalised in place when its length exceeds `1e-20`; `P` is not.
- Int, Text, ragged Int and Float rows (matrix rows included) and group membership take row
  `a` when `t < 0.5` and row `b` otherwise. Ordered groups follow the plain ordered rule over
  the chosen rows.
- Detail attributes and edge groups are unchanged (`:edges` ancestry stays the caller's).

One helper, `Topology_remap.blend_attribute` (with `blend_group`, `blend_positions`), implements
this; `Curve_ops.interpolate_attribute` calls it, so a curve resample and a weighted remap of
the same point attribute are byte-identical. `Resample_curves` keeps its own copy: it has no
non-finite fallback (`a + (b - a) * t` always), so sharing it would change bytes for infinite or
NaN data.
The JavaScript twin has no weighted form and no fixture for it.
Target supplies topology and, by default, canonical P. `:positions "mapped"`
selects source P through the point map. This selection copies raw native bits,
including signed zero, subnormals, infinities and NaN payloads; it is a storage
transfer, not a numeric attribute read. It does not relax ordinary finite
arithmetic, `sop/attr` or `sop/mesh` validation.

Source attributes and ordinary groups replace target metadata, in source list
order. Every native storage is supported: Float, exact Int, Text, Float2/3/4,
ragged Int arrays and ragged Float arrays (including 16-value matrix rows).
Detail attributes retain their immutable objects. Validated identity maps may
retain unchanged owner storage; non-identity selections own their copied
buffers. No normal transformation, interpolation, or owner
conversion is inferred; defaults appear only for `-1` rows. Ordinary ordered groups follow native source rank,
with target indices increasing within each rank; ordered-empty remains ordered.
Callers that require native copy-major order author that policy with existing
`sop/group_order` and `sop/with_group :order`, for example concatenating source
order and a shifted copy after remapping two copies.

`:edges` is integer CSR from each target unique-edge ordinal to its source
edge ancestors. Offsets start at zero, are nondecreasing, have target edge
count plus one entries and end at values length. Ancestors must be valid source
unique-edge indices. Membership is OR across each row; an empty row is false,
repeated ancestors are valid. Edge groups have native topology affinity and
ascending edge ordinal traversal, not an independent explicit order. Omission
is permitted when source has no edge groups or target shares the exact source
topology; changed topology with existing edge groups requires explicit CSR.
An explicit relation is validated even without source edge groups. The native
consumer is the factored Rewire Vertices CSR consumer in `Topology_remap`.

Cooked child failures keep their real code, message, cause, hints and trace.
After arguments have been evaluated in authored order, validation checks policy,
Point/Vertex/Primitive cardinality and bounds, then edge CSR, before payload
copy and final geometry validation. Malformed maps produce `E_REMAP_TYPE`,
`E_REMAP_COUNT` or `E_REMAP_INDEX`; invalid policy is `E_REMAP_POLICY`, edge
ancestry is `E_REMAP_EDGES`, final geometry failure is `E_REMAP`. Cancellation
propagates unchanged and no input buffers are mutated. Conservative facts read
and write all payload and declare changed topology; both geometry handles,
all map/CSR data, live captures and authored body take part in ordinary cache
invalidation. Projection exposes both geometry inputs and all supplied typed
keyword rows; map or record edits use the full checker.

The opaque geometry-host transport retains native topology affinity with a
private `$topology_token`, scoped to the issuing host lifetime. It is not a
Lisp argument or an authored numeric value. Two independently serialized
handles sharing native topology keep the same token even when P differs;
identical connectivity under a different token does not grant affinity.

The JavaScript geometry-host twin uses raw typed-array byte copies, including
CSR segments. Its environment-owned `remapHost({check})` supplies cancellation;
that callback is not authored data. Host fixtures compare every numeric storage
bit and exact integer/text contents against native results. The current 2D
exporter still refuses reached SOP geometry with `E_WEB_GRAPH`, and direct
reached native host sources keep `E_WEB_SOURCE`; a host twin is not a browser
fallback. Single-source remap does not qualify the full piece modes of Copy To
Points, or any consuming whole production node.

### Geometry concatenation

`sop/concat` is a cooked geometry host intrinsic and wraps the exact function
the native Merge node runs, `Rdk.Mesh_merge.run`:

```lisp
(sop/concat geometries :source_attribute "src" :source_base 0)
```

`geometries` is a list of cooked geometry (a `nil` element is skipped; an empty
list gives empty geometry). Points, vertices and primitives are appended in list
order with indices shifted; every attribute of every storage type and owner, and
every ordinary, ordered and edge group, follows native Merge: group names are
unioned in first-seen order and a missing group contributes no members, matching
attribute schemas are required, and Detail attributes are refused. Errors keep
native's code, message, cause and hints. `:source_attribute` (trimmed; blank means
none) adds a primitive Int attribute holding `:source_base` plus the input index,
and an input that already has it keeps its values. No Core or Region instruction
is added, there is no JavaScript twin (the rdk function is compiled for the
browser later), and facts read and write all payload with changed topology.

`:tag_base` (int, default -1) additionally writes the editor's provenance attribute
`__flow_src` (`Rdk.Mesh_merge.provenance_attribute`), numbered from it, beside an
authored `:source_attribute` (`Rdk.Mesh_merge.run_tagged`). Its only author is the
`merge` node: `sop/provenance_base`, a zero-argument int, is available in a defnode
body only (`E_NODE_CONTEXT` elsewhere or in a validator) and is specialized to the
base the editor lowering allocated for that call, or -1 when the node is lowered
without provenance (the port gate). The base is part of the node's cache source.
The node's inputs are tagged `base + input index`, and `Lower.tags` reports them
exactly as the former native lowering did.

`sop/context_frame` is the cook frame as a float (`Int64.to_float (Context.frame context)`, what the
native `attribute_fade` reads). Like `sop/context_seed` it is available in a defnode body only,
specialized to a literal before core lowering, so a node that reaches it declares the Frame cache
dependency and its body is rebuilt when the cook frame changes. No JavaScript twin and no fixture
(the web target runs `:context draw` graphs, which have `frame/index`).

`sop/context_time` is the same for the cook time in seconds (`Context.time`, what native
`extract_point_from_curve`'s Current time cut reads) and declares the Time dependency.


### Float attributes as planes with a cook-time width

A Lisp body cannot read an attribute whose storage type is only known at cook
time, because `:type` of `sop/attr` is a literal. Two host operators handle every
float-family storage internally:

```lisp
(sop/attr_planes geometry "name" :owner "point")
  ; -> {:width int :x (array float) :y (array float) :z (array float) :w (array float)}
(sop/with_planes geometry "name" planes :owner "point")
```

`sop/attr_planes` reads Float (width 1), Vec2, Vec3 and Vec4 attributes of any
owner, and canonical point `P`. `:width` is the stored width; planes past it are
zero arrays of the owner count (always four planes keep the record type static).
Other storage, or a missing attribute, is `E_ATTR_TYPE`. `sop/with_planes` writes
the first `:width` planes of the record with the matching float, vec2, vec3 or vec4
storage through the ordinary planar `sop/with_attr` writer: owner count
(`E_ATTR_COUNT`), finiteness (`E_NONFINITE`) and the `P` vec3 rule are unchanged,
and a width outside 1 to 4 is `E_ATTR_WIDTH`. A round trip restores the storage
type. Neither adds a Core instruction; there is no JavaScript twin (decision 4).
