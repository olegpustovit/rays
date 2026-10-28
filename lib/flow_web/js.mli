(** JavaScript for packed regions placed on the CPU. The frame-level printer joins this module in a
    later task. *)

val region : ?fast:bool -> Flow_ir.Packed.t -> string
(** The source of [function region(inputs, uniforms, outputs, start, stop)],
    which runs iteration indices [start, stop) of one region over typed arrays
    and returns a status: [0] for success, [1] when a value is not finite
    (native [Packed.force]'s [Nonfinite], reported as [E_KERNEL]; outputs and
    state are then partly written and must be discarded). Arithmetic is 64-bit
    and follows [Packed.force] instruction by instruction, including the
    division/[mod]/[pow]/[sqrt] guards and the finite checks, and every
    instruction runs, reachable or not.

    - [inputs]: one [Float64Array] per source, packed with that source's width.
      For a product iteration of several sources each array must hold exactly
      its source's elements; the index space is the product of their lengths.
    - [uniforms]: one [Float64Array] per [uniform_widths] entry, then, as the
      last element, the frame array indexed by instruction slot (as
      [Packed.Host.inputs.frame]).
    - [outputs]: one [Float64Array] per output, the collecting kinds only, sized
      [kept * width] where [kept] is the iteration count minus skipped
      indices. A [Sum] or [Accumulate] region appends one state array of
      [width + 1] doubles: the accumulator components, then an initialized flag
      (0 or 1). The caller seeds it before the first range: a [Sum true] region
      starts at zeros with flag 1, [Sum false] at flag 0 (flag still 0 after
      an empty iteration means the integer 0), [Accumulate] at the seed
      components with flag 1. [Accumulate (_, true)] ([scan]) also collects.

    Text leaf regions use immutable strings and string arrays, collect only,
    and accept an optional sixth [outputBase = 0] argument for worker-local
    output slices. The region loader supplies [noise3] from the web runtime's
    [prims.js] module as its closure binding. Their inputs, captures and outputs
    never use float buffers.

    Covers every result kind: [Collect], [Sum] and [Accumulate], one or several
    outputs ([Collect] only), [Zip] and [Product] iteration, and [skip].
    Sums and accumulators fold elements strictly in index order, so successive
    ranges of one region must be run in ascending order with the same state
    array, never concurrently; only [Collect] ranges may run in parallel.
    Skipped indices are not evaluated and do not occupy an output slot. Noise
    permutation tables are embedded in the source.

    [~fast:true] (a workspace's [:precision "fast"], the default): a collecting region's integer
    registers are plain numbers, exact to 2^53 (native fast precision carries integers as floats
    too); division and [mod] truncate, bitwise operations split into 32-bit halves, integer tables
    are read as numbers, integer outputs are still written as [BigInt]. Sums, accumulators and
    [rand_at] keep the exact form. *)

val frame :
  ?store:(float array -> string) ->
  ?fast:bool ->
  ?kernel:(int -> string option) ->
  ?gpu:(int -> bool) ->
  ?state_slot:(int -> int) ->
  Flow_ir.Core.program ->
  (string, Diagnostic.t) result
(** [store], when given, prints every packed-array constant: it receives the elements and returns a
    JavaScript expression for a [Float64Array] of them (the export keeps them in [data.bin]);
    without it the elements are written inline.

    [state_slot] maps program-local slots into a shared workspace slot array. The default is
    identity; the web exporter shares a fold read by the main drawing and any image argument or
    nested drawing.

    [gpu region], when given and true for a region index, prints the region's [Invoke] as
    [await rt.gpuArray(k)] instead of [await rt.invoke(k, [])]: the export placed the region on the
    GPU, and the runtime answers with a packed array whose [data] is not elements but a device
    reference [{gpu: kernelId, output, length}], which only a mark set attribute may carry (it
    appears as the [buffer] of an attribute; the presenter binds the kernel's output buffer).
    Without [gpu] the output is exactly as before.

    The frame level as one ES module exporting [frame(input, state, rt)] (an [async] function
    exactly when the program invokes a region or resolves an image). It returns the program's single
    result, or an array of results when there are not exactly one (a draw program returns [[]]). A
    failure is thrown as [rt.Fail] (or a JavaScript error) with the executing statement's
    {!C.statement} [id] set as [error.statement]; {!frame_map} turns it into a source span. Frame
    generation returns a [Diagnostic.t] refusal for target-incompatible primitives, host sources,
    range-last diagnosis, or region captures; these use [E_WEB_PRIMITIVE] or [E_WEB_SOURCE].

    Values are those of [lib/web_export/runtime/prims.js]: Int is BigInt, Float is Number, Vec is
    [Float64Array], Record is [Map], list is [Array], packed array is [rt.PArray].

    What the module expects:
    {ul
     {- [input]: [{t, dt, frame, tick, size: [w, h], pointer: [x, y], buttons, keys, events}], the
        fields of [Input.t] as JavaScript numbers (Int facts are converted to BigInt by the module),
        logical points.
     }
     {- [state]: the runtime's slot array ([[]] at start). The module creates [state[i]] on first
        use; it holds a [Map] from the iteration key to a cell [{value, pending, stamp}]. A cell
        commits its pending write when it is first touched in a later frame ([input.frame], or
        [input.tick] on the host plane), so the runtime needs no commit or rollback step; replacing
        the array resets the state.
     }
     {- [rt]: every export of [prims.js] under its name ([rt.add], [rt.sequence_get], [rt.coerce],
        [rt.PArray], [rt.fail], ...; the first arguments of [coerce], [color_byte] and [fail] are
        the static lowering parameters), plus
        - [rt.frameValue(input)]: the Lisp [frame/input] record ([Map]);
        - [rt.marks]: the mark table, an array the runtime empties before each frame;
        - [rt.invoke(k, args)]: the value (a [PArray] or number, as [Packed.force]) of region [k] of
          the exporter's region table, possibly a promise; [args] is always [[]];
        - [rt.image(n)]: the host resource id (positive integer), or its promise, of plan image node
          [n];
        - [rt.drawSize]: optional logical target size for backgrounds; defaults to [input.size];
        - [rt.polygon(points, fill, stroke)], [rt.polyline(points, color)] and
          [rt.rounded_rect(width, height, radius, fill, stroke)]: the pure [Scene_command.Shape2]
          tessellation, each returning an array of geometry entries (fill then stroke); [points] are
          [[x, y]] integer pairs, colours packed int32 or [null].
     }
    }

    [rt.marks] entries follow [specification/marks.md], with colours as signed int32 numbers:
    [{type: "mark_set", kind: "rect"|"circle"|"line"|"point", count, position, size | radius | end,
     fill, stroke, stroke_width}] where an attribute is [{constant: v}] (a position, size or end is
    [[x, y]]) or [{buffer: Float64Array, count, width}] referenced directly, and a point set's
    [fill] is a bare number; [{type: "geometry", vertices, indices, color}] (from the [rt]
    tessellation); [{type: "image", resource, x, y, scale, angle}];
    [{type: "text", x, y, size, color, text}]; [{type: "push_transform", xx, xy, yx, yy, tx, ty}];
    [{type: "pop_transform"}]. [Marks.validate] is not run by the module. *)

val bindable : Flow_ir.Packed.t -> bool
(** A region with arguments or frame-dependent inputs can be bound per call (its inputs, captures
    and count prepared by emitted code): a draw lowering may keep it a region on the web. *)

val frame_map : Flow_ir.Core.program -> string
(** The side table for [program.map]: one line [<statement id> <start> <finish>] per statement that
    has a source span, in program order. *)
