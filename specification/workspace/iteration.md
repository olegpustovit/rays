# Iteration, functions, data and macros in workspaces

**Status: implemented.** §2 and §7 are normative for the workspace language
(`Flow.Syntax`, `Flow.Lisp`, `Flow.Macro`, `Flow.Workspace` and `Flow.Eval` implement
them; the study's `prototype/check.cjs` is ported to `lib/flow/test_workspace*.ml`).
§3 to §5a are the design the graph pane, the editor graph and `.rays` sketches were built
from (`Flow_graph.Projection`, `Pxui_graph.Scope`, `Rays_editor`, `tools/lisp`); §6 says which
module implements what. [`flow.md`](../flow.md) has the reader, the printer, argument
rules, the pane's gestures and keys and the layout forms, and is the current statement
where it is more specific than §3.

The interactive study is [`prototype/index.html`](prototype/index.html). It
checks and runs every construct below and draws it in the Pxui kit. The
[case studies](case-studies.md) are the reference programs, and the
[ambiguity register](ambiguities.md) lists every decision a reader could
otherwise make two ways.

## 1. Goals

1. **Every Lisp form has exactly one drawing, and every drawing prints to
   one form.** Loops, scopes and branches are structure on the canvas, never
   hidden inside a text field unless the author put them in an argument.
2. **Iterations are visible.** A loop shows each of its iterations. Any node
   inside a loop shows its value at a chosen iteration and across all of
   them. A shape in the viewport leads back to the iteration that made it.
3. **Bounded, pure and deterministic.** There is no `while`, no recursion and
   no stateful randomness. Parallel and sequential evaluation give identical
   bytes, as `AGENTS.md` requires.
4. **A sketch is a `.rays` file.** `sketches/<name>/sketch.rays` holds one
   workspace with no OCaml wrapper. `dune build` checks it, reports errors at
   lines in the file, and links a native program. The running editor saves
   back to the file and reloads it when it changes on disk.
5. **Frame facts are live, structure is not.** Time, input and size drive values
   and recook their dependents. Structural list counts and shape choices never
   depend on them. Packed-array lengths are data (§2.5).

## 2. Language additions

### 2.1 Grammar delta

The grammar of loops, branches and references. Conventions: keywords for parameters,
positional geometry slots (`flow.md` §11.6), and `let*` for sharing. The `material` context returns a typed material value
from `material/standard`; a SOP assigns it through `sop/material` and a
material graph reference. Group assignment and renderer behavior are specified
in [materials.md](materials.md).

```text
graph    = "(" "graph" name ":context" context [ inputs ] body ")" ;
defn     = "(" "defn"  name ":context" context inputs body ")" ;
inputs   = "[" { "(" name ":" type [ literal ] ")" } "]" ;   (* graph inputs need a default *)
expr    += for | fold | scan | sum | if | ref | "t" ;
for      = "(" "for"  "[" clause { clause } "]" [ skip ] body ")" ;
fold     = "(" "fold" "[" name expr "]" "[" clause { clause } "]" body ")" ;
scan     = "(" "scan" "[" name expr "]" "[" clause { clause } "]" body ")" ;
sum      = "(" "sum"  "[" clause { clause } "]" body ")" ;
clause   = name expr ;                              (* expr is a list *)
skip     = ":skip" "[" { int | "[" int { int } "]" } "]" ;   (* for, scene/merge: iteration tuples left out *)
if       = "(" "if" expr expr expr ")" ;
ref      = "(" "ref" name { ":" name expr } ")" ;   (* input overrides *)
body     = "(" "let*" "[" { name expr } "]" expr ")" | expr ;
```

The new list producers are `(range n)`, `(range a b)`, `(linspace a b n)`,
`(sop/point_list g)` and `(sop/piece_list g)`. `(count xs)` reads a length.
`value/rand` is a pure hash; `value/hsv`, `value/lerp` and `value/polar` are
the value helpers the case studies use. The comparisons `< > <= >= =` and
`and`, `or` and `not` produce Bool. Numbers convert on their own: an Int
reaching a Float input is widened, and a Float reaching an Int input (a
SOP keyword, `range`, `nth`, a typed parameter) rounds half away from
zero. `floor`, `ceil`, `round` and `int` (truncation toward zero) make the
conversion explicit and give an Int; `float` gives a Float.

### 2.2 Semantics

| Form | Meaning | Result type |
|---|---|---|
| `(for [x xs] b)` | evaluate `b` once per element of `xs`, collect in order | `list T` where `b : T` |
| `(for [x xs y ys] b)` | product, row-major, last clause fastest; `ys` may read `x` | `list T` |
| `(fold [a init] [i xs] b)` | `a := init`; per element `a := b`; return `a` | the wider of `init`'s and `b`'s types (an int seed does not round a float body) |
| `(scan [a init] [i xs] b)` | as fold, but collect each new `a` | `list A` |
| `(sum [i xs] b)` | add `b` over the elements; empty is `0` | Int, Float or Vec3 |
| `(if c a b)` | only the taken branch runs | the type both branches fit (int and float give float) |
| `(let* [...] r)` inside an expression | a scope: names are private, only `r` leaves | type of `r` |
| `(ref g :k v)` | evaluate graph `g` with input `k` overridden; cached by input values | `g`'s context type |

**Lists and splicing.** A list is a value like any other. Variadic slots
(`sop/merge`, `scene/merge`, `ui/tile`) splice a list argument one level. No
other slot flattens. That keeps `(sop/merge ring)` a single wire while
`(for …)` stays a list, which scalar consumers reject with a type error.

**Captures.** A zone body may read any name bound outside it. Those values
are evaluated once, before the zone runs, and are the same in every
iteration. Because evaluation is pure, the compiler may hoist a loop-invariant
body node. The result is identical, so the editor can offer the same move.

**Skipping** (`:skip`, register L16). `(for [x xs …] :skip tuples body)` leaves out the iterations whose
*iteration tuple* is listed, and `(scene/merge a b … :skip tuples)` leaves out the arguments at the listed
tuples. A tuple is the running index of each enclosing loop, outermost first (the same tuple records,
probes and compiled ids use), then the index inside this form: for a `for` the row-major running index of its
clause product, for a `scene/merge` the position of the argument as written. A tuple of one may be written as
a bare integer, so `:skip [0 3]` is `:skip [[0] [3]]` for a top-level loop. The list is a static literal of
non-negative integers (`E_SKIP` otherwise), so it never depends on `t` and cannot raise `E_TIME_COUNT`; only
`for` and `scene/merge` have it (`sum`, `fold` and `scan` are `E_ZONE`), and a `for` over the elements of
geometry has none. A skipped iteration is not evaluated, makes no plan node and is absent from the result list;
the others keep their index, so their compiled ids, per-tuple cache keys and provenance do not move. The loop
variables are still recorded at a skipped iteration, so a selector counts it and a node inside shows
`not run here`. A scene sync writes it: deleting a loop-made object adds to a `:skip`, never rewrites the
collection (register V4).

**Shadowing is an error** (`E_SHADOW`), including for loop and accumulator
names. On a canvas, two nodes called `x` on either side of a zone border
would make every wire ambiguous to read.

**Randomness** comes only from `(value/rand key …)`, a pure integer hash of
its keys in `[0, 1)`. Per-iteration variation passes the index explicitly,
as in `(value/rand seed i)`, so hoisting, reordering and parallel evaluation
cannot change a result.

**The hash, bit for bit** (`Flow.Eval.hash`; the study's `hash`). All
arithmetic is on 32-bit integers, kept unsigned:

```text
h := 0x9e3779b9
for each key x (Int, Float or Bool, read as a double; Bool is 0 or 1):
  k := ToInt32(floor(x * 1000003.0))   (* JS ToInt32: NaN and infinities give 0,
                                          otherwise truncate and reduce mod 2^32 *)
  h := imul(h xor k, 0x85ebca6b)        (* low 32 bits of the product *)
  h := h xor (h >>> 13)                 (* logical shift *)
  h := imul(h, 0xc2b2ae35)
  h := h xor (h >>> 16)
result := float(h mod 1000000) / 1000000.0
```

No keys give `0.435769`; other vectors: `(value/rand 0)` is `0.009611`,
`(value/rand 3 4 5)` is `0.706664`, `(value/rand 1.5 -2)` is `0.622571`,
`(value/rand 1 2 3 4 5 6)` is `0.659002`. A key beyond 2^53 loses its low
bits before the floor, on every platform alike, because the product is a
double.

**Formatting** (`str`, register C2). An int prints as its digits. A float
prints as `toFixed(4)` with trailing zeros and a trailing point removed, and
`-0` prints as `0`; an exact tie at the fifth decimal rounds away from zero
(`0.03125` is `0.0313`). A vec3 prints `[x y z]` with the same rule per
component, a list `[a b]`, a record `{:a 1 :b 2}` in written order.

### 2.3 Checking

The checker has two passes (`Flow.Workspace` and `Flow.Eval`; the study's `model.js` has both):

1. **Static pass.** It types every definition, graph and zone body once,
   whatever the iteration count, and types both branches of every `if`.
   Arity, keywords, contexts, shadowing, recursion, fold accumulator
   agreement (`E_ACC_TYPE`), group names (`W_UNKNOWN_GROUP`, §3.7) and
   literal loop bounds are all reported here.
2. **Run pass.** `Flow.Eval` evaluates everything that is not geometry, in
   order, on IEEE doubles. Geometry calls become a plan (see `eval.mli`), so
   nothing here cooks. These are reported here, each with its code and the
   path of the zone or term in the message:

   | Code | When |
   |---|---|
   | `E_ITER_BOUND` | a `range`, `linspace` or `concat` past 4,096, or a zone running more than 4,096 iterations, with a count that was not a literal |
   | `E_EVAL_BUDGET` | more than 600,000 evaluation steps |
   | `E_LIST_RANGE` | `first`, `last` or `nth` outside the list |
   | `E_PATTERN` | a pattern longer than a driven list |
   | `E_NONFINITE` | a scalar result that is NaN or infinite (`pow`, `*`) |
   | `E_RANGE` | `ui/tile` holds 1–16 panels; a computed split axis, ratio or settings value out of range |
   | `E_DEPTH` | more than 64 nested calls |

   Division and `mod` by zero give 0 and `sqrt` takes the absolute value, as
   in the study.

   `t` is not known to the static run: a term that depends on it is kept as
   a *residual* (the term with its environment) and evaluated by
   `Flow.Eval.residual_eval` for a given time (register T1).

`rays-lisp check` runs both at build time: it checks the file, then evaluates and lowers
it with its default inputs as a window opens it.

### 2.4 Bounds

The limits are 4,096 iterations per structural list or geometry zone
(`Flow.Workspace.max_iterations`), 600,000
evaluation steps and 64 nested calls (`Flow.Eval`), each named in its diagnostic
(register L3). The study's cap of 20,000 primitives is not part of the evaluator, which
builds no geometry. A literal count is checked at compile time; a count that depends on
an input, `t` or geometry is checked when the program runs.

### 2.5 Frames, frame folds and packed arrays

`Flow.Eval.live` is `Frame_input.t`: `{ t; dt; frame; size; pointer;
buttons; keys; events }`. Size and pointer coordinates are logical points;
events retain host order. Every field is live. The host captures one snapshot
before routing editor keys and passes it to probes, value lanes, context
lowering and drawing. Geometry workers receive the same snapshot through
`Sop.Context.input`. Cache equality includes all fields and floating
point bits, including signed zero.

`frame/dt`, `frame/index`, `frame/width`, `frame/height`, `pointer/x`,
`pointer/y`, `(pointer/down [button])` and `(key/down key)` read these facts.
The pointer button defaults to `"left"`; other names are `"middle"`,
`"right"`, `"x1"`, `"x2"`. `(frame/input)` returns the complete typed
record, including ordered event records; their fields and spelling are
declared in `Flow.Op` and `Frame_input`.

A picture's frame is the pane that shows it: a canvas pane evaluates its drawing with
`size` set to the pane's own logical size, so `frame/width` and `frame/height` are the
viewport and a drawing sized with them fills each pane exactly, whatever the window or the
layout. Probes, value lanes and geometry keep the window snapshot, and pointer
coordinates remain window points. Export uses the configured window size.

In the editor the pointer is the sketch's only on a picture, a canvas or viewport pane. Over
another pane, a menu or a prompt, `pointer/x` and `pointer/y` keep the last position the picture
had, no button is down, and pointer, scroll and pinch events are left out of `(frame/input)`.
A press that began on a picture stays the sketch's until its release, wherever the pointer goes;
a press that began elsewhere never becomes the sketch's drag. Keys are not affected.

`(state [previous init] step)` folds over frames. The seed is static data;
the step reads the previous value and current frame, and returns the next
value. Int/float results use the wider type. A cell is identified by source,
graph instance, lexical path and iteration tuple. Repeated reads in one
frame return the same next value. Advancing commits the previous frame;
failed evaluation rolls the transaction back. Backward seeks, explicit reset,
stop, source reload and a new export reset the fold. Packed data and records
may be stored; functions, deferred nodes and layouts may not.

The environment owns fold state; probes inspect a copy. A fold outside a
geometry loop is captured as an immutable snapshot for every worker;
one and many domains produce identical geometry. A fold declared inside a
cooked-geometry element body is `E_STATE_ELEMENT`: bind it outside that
zone. Packed-array iteration may contain folds, keyed by element index.

The graph draws `state` as a zone: the editable seed and previous-value rail
on the left, step cards inside, and a next-value yield/feedback wire. The
ordinary add-node gesture adds it; `Flow_edit.Set_arg` with `Bv (1, 1)` edits
the seed. It has no iteration selector.

`(array float)` and `(array vec3)` are packed data types, with finite native
float storage. Constructors are `(array/range count)`, `(array/float count
[value])` and `(array/vec3 count [value])`; omitted values are zero. Access
uses `array/count`, `array/nth` and `array/sum`. Negative counts, native
storage overflow, non-finite values and out-of-range indices are typed
errors. There is no 4,096-element cap for packed arrays. The evaluation
budget and native storage bounds still apply. Packed higher-order operations
and zones apply the 600,000-step budget independently to each 1,024-element
block, then restore the enclosing evaluation's counter. This admits large
arrays while refusing an expensive body within a block. The order of element
evaluation and floating-point operations is unchanged.

The CPU register tier supports numeric packed maps, Cartesian `for`, `sum`,
`fold`, `scan`, `reduce` and `array/sum`. Live packed accumulators defer the
complete iteration during static evaluation, so specialization does not build
a residual for every element. Independent elements may run in parallel;
accumulator steps keep their original order. Equal-count map stages may fuse
without intermediate arrays. Correlated clauses and unsupported bodies keep
reference evaluation. Placement and reduction order are specified in
[`flow.md` §13.3](../flow.md#133-determinism).

`map` (one to three arrays), `filter`, `sort-by`, `reduce`, `for`, `fold`,
`scan` and `sum` operate directly on packed data. A packed map/collection
body returns float or vec3 data and cannot construct deferred nodes. Array
length may depend on frame input or a fold; structural lists retain T2's
`E_TIME_COUNT` and T3's prohibition on live shape choices. Packed-array
branches select data, so they are allowed without changing the plan.

## 3. Graph representation

### 3.1 Form → drawing

| Lisp | Canvas |
|---|---|
| `name (op …)` in a `let*` | node card: title, one row per slot and keyword, footer |
| a keyword left at its default | a dimmed row; scrubbing it writes the keyword |
| a call of a node kind in an argument, a step of a `->` | a card of its own before the card that holds it, wired to its row (`flow.md` §11.3) |
| an expression or a function call in an argument | a chip, `ƒ (op …)`, whose numbers stay scrubbable |
| `name (for …)` / `fold` / `scan` / `sum` | a **zone**: a tinted region with rail, iteration selector, body and yield |
| `name (let* …)` | a **scope**: a dashed region with a rail and a result |
| an inline loop or scope in an argument | a chip whose glyph names it: `for`, `⟲`, `Σ`, `let` |
| a symbol argument | a wire, solid for a whole argument, dashed when inside an expression |
| a loop variable, accumulator or captured name | a **rail row** on the zone's left edge |
| the zone body's result | the **yield** row on the right edge: collect, next, add, result |
| `(if c a b)` | a node with `if`, `then` and `else` rows, plus branch counts inside loops |
| group writer `:name "x"` → reader `:group "x"` | a dotted named link, `▦ x` |
| graph inputs `[(n : int 12)]` | input nodes at the left, and sliders in the navigator |

Frames (visual grouping), positions, detail levels and collapsed zones are layout.
They are stored with the document and never printed in the workspace form (`flow.md` §4.1).

### 3.2 Zones

The anatomy follows Blender's zones and vvvv's regions, which both use a
visible border as the scope of repetition. It adds what neither shows: the
iterations themselves.

- **Title**: the kind glyph, the binding name, the total count and the result
  type, with a collapse toggle. A list output has a *stacked* socket.
- **Iteration selector**: `‹`, a slider and `›`, with ticks when there are
  48 iterations or fewer. It sets the zone's **probe**; arrow keys, Home and
  End work when it has focus. The readout names the loop variable's value,
  for example `i = 5 · 6 of 12`. For `fold` it selects the state after each
  step, for a λ zone the call. It deliberately shows no thumbnails: the
  viewport is where results are seen, and it highlights the probed
  iteration.
- **Rail** (left): one row per iteration clause (`i ∈ (range petals)`, with
  the count scrubbable in place), the accumulator (`shape ⟲ from frame`) and
  every captured outer name (`seed · same for all`). Outer wires end at the
  rail and inner wires start from it. Captures are derived from the body's
  free names and are never authored, so the rail cannot disagree with the
  Lisp.
- **Yield** (right): the only exit. Wiring from inside a zone to anything
  outside is refused with a reason, as in Blender.
- **Feedback**: `fold` and `scan` draw a dashed line from *next* back to the
  accumulator row, labelled `⟲ next becomes shape`.
- **Collapsed**: a stacked card that keeps the rail rows, so its wiring stays
  readable. Collapse is layout.

Nested zones probe independently. An inner zone's selector and values are those
of the inner loop *at the outer probe*.

### 3.3 Scopes and frames

A nested `let*` bound to a name is a scope. It has private names, a dashed
border, a rail of captures and a result row. From outside it is one node, and
Fold (ƒ) inlines it. A frame is only a label around nodes and prints
nothing. Register L12 lists the five box-like things and how they differ.

### 3.4 Per-iteration display

A node inside a zone shows, in its footer:

- its value at the probe, or *not run here* for an untaken branch;
- a sparkline across the innermost loop's iterations (numbers and Bool);
- `×n`, its evaluation count;
- for `if`, `then a · else b`;
- or a green **↥ same each time** when it does not depend on any loop name.
  Clicking the badge hoists the node: it moves out and runs once, with the
  identical result.

The inspector lists every iteration's value, and clicking one sets the
probe. `[` and `]` step the probe.

### 3.5 Provenance

Primitives produced by a `for` carry `(zone path, index)` tags. The tags are
provenance metadata and are never readable by the program. The viewport:

- highlights the probed iteration and dims the rest when a zone, or a node
  inside one, is selected;
- draws a `fold`'s state at the probe as a dashed overlay;
- on a click, selects the zone that made the shape and sets the probe there.
  The status line says which iteration it was.

### 3.6 Direct manipulation → Lisp

| Gesture | Rewrite |
|---|---|
| drag a number, including in a chip, a vector or a default | replace that literal; one undo step per drag |
| drag an output onto an input | set the argument to the symbol, then stable topological reorder |
| drag a loop variable onto a number `v` | `(* i v)`: the number becomes the per-iteration step |
| drag from inside a zone to outside | refused: values leave only through the yield |
| click ƒ on a chip | the argument becomes a new binding in the same scope; a loop becomes a zone |
| click ƒ on a title (single use) | inline the binding into its only use |
| **Repeat** (R) on a selection | `x_each (for [i (range 6)] body)` plus `x (sop/merge x_each)`, or `(sum …)` for numbers |
| **Iterate** (⇧R) on a selection | `x (fold [prev input] [i (range 4)] body[input := prev])` |
| click ↥ same each time | move the binding to the enclosing scope |
| Only iteration k differs | argument becomes `(if (= i k) v v)` |
| double-click inside a zone | the add palette inserts into that zone's body |
| rename in the inspector | rename the binding and every use in its scope |

A scrub inside a loop changes every iteration. The status line says so while
dragging, and the explicit per-iteration special case is the `if` above. That
is register V4.

**Loop-made objects (normative).** The copies of a loop that makes scene objects are instances of one
template. Editing a copy in the viewport, list or inspector edits the template: a literal argument (or a
literal component of a vector) is written once and every copy changes, and the status says so; an argument
the loop computes from its variable is refused with its expression, and `=(expression)` typed in the row
replaces it. Renaming writes the template's `:name`. Deleting a copy is exact at any nesting depth, for any
number of clauses, and when a copy makes several objects (register L16): the iteration that made it is added
to the `:skip` list of its loop (the outermost iteration all of whose objects go, when there is one), or, when
the iteration makes other objects that stay, the object's place is added to the `:skip` of the `scene/merge`
that holds it. Nothing else is rewritten: every other copy keeps its iteration tuple, so its id, cache key and
provenance are unchanged. Repeated deletes accumulate in one list, and a loop whose copies are all deleted stays
a loop that makes nothing.

### 3.7 Groups

Geometry groups are named selections carried on geometry. The catalog marks
a parameter as a *group writer* (`group_bounds :name`, `group_random :name`)
or a *group reader* (`blast :group`, `set_color :group`). The checker tracks
literal names written upstream on the same wire. It warns
(`W_UNKNOWN_GROUP`) when a reader names a group that nothing upstream writes,
and the graph draws a dotted named link from writer to reader. Computed group
names are open question G2.

## 4. Loops in the editor graph

The editor graph is ordinary Lisp, so it can loop too:

```lisp
sheet (ui/tile (for [s (range 4)]
                 (ui/viewport (ref scene :seed (+ s 1)))))
```

A panel made by a loop has no binding. Its header says *from a loop* and
links to the loop node. Retyping edits its template. Splitting, closing or docking
one copy into another split is refused; its disclosure and floating bounds can
change independently. Panel focus is keyed by iteration index (register E1). The
host's Restore layout stays outside the described tree.

### Named layouts

A workspace's layouts are one editor graph with a switch:

```lisp
(graph editor :context editor
  (let* [preview (ui/viewport (ref scene))
         network (ui/graph)
         inspector (ui/inspector)
         code (ui/lisp)
         build (ui/split-at "horizontal" 0.4 preview
                 (ui/split-at "horizontal" 0.62 network inspector))
         write (ui/split-at "horizontal" 0.46 preview
                 (ui/split-at "vertical" 0.5 network code))
         look preview]
    (ui/workspace (ui/switch build write look :active 0))))
```

`(ui/switch panel... :active n)` evaluates to its `n`th panel (0-based, in text); it can sit at the
root of the workspace or inside a split, where it swaps one column. The panels are shared bindings,
so a viewport keeps its orbit and the text pane its draft across layouts. A layout has no stored
name: `Editor_core.Panels.label` reads it from its tree. A leaf is its panel word (`View`, `Graph`,
`List`, `Lisp`, `Inspector`, `Outline`, `Timeline`); side by side joins with `|`, stacked with `/`
(which binds tighter, so only a row inside a stack is bracketed); same-axis nesting flattens;
neighbours of one name collapse to `×n` (a tile of four viewports is `View ×4`); floating windows
come last after `+`. Layouts that read the same add their largest panel and its share
(`View | Graph · Graph 70%`), and any that still match get a number.

- Inspector: `:active` is a choice whose labels are the computed names.
- Graph pane: one input row per layout, labelled with its name. The active wire is solid and
  accented, the others dashed grey; a click on a row switches.
- Keys (`Space [`): the first switch's layouts as `0`..`9` (name, `(active)` mark), `Space [ 0..9`
  switches, `Space [ n` adds a copy of the current layout and activates it (a document without a
  switch gets one around its tree; ten layouts at most), `Space [ x` removes the current one (the
  last stays). A switch is `Flow_edit.Set_layout`, history label "Layout"; repeats within 1.5
  seconds merge into one entry. These edits address the first switch of the layout: one at the root
of the workspace, or nested in its splits, tiles and floats (bound to a name or written in place),
so a switch that swaps one column is edited where it is.
- Windows: `Space n` plus a kind letter of `Space l` (`g l t i u m w`) wraps the active layout in a
  split whose second side is `(ui/floating ...)` (`Layout_window`); `Space o f` floats the focused
  docked panel or docks the focused window beside the rest (`Layout_float`). Both rewrite only the
  active layout; the splits it names are copied into one expression, its panels stay shared.
  Window bounds are `(layout (panel ... :window [x y w h]))`. There are no OS windows.

An older file with several `:context editor` graphs and `(layout (editor "studio"))` still
loads: the selected graph is the shell and `Space [` lists the graphs by name (`Select_layout`,
one undo entry). It is not rewritten on load (that would mark an untouched file changed). The first
`Space [ n` there migrates it in one undo entry, "Merge layouts" (`Flow_edit.Merge_layouts`): the
other editor graphs join the selected graph's switch (made around its tree when it has none) as
layouts, in file order, each graph's bindings renamed `graph_name` so they cannot clash, the graphs
are removed and the selected-layout key of `(layout ...)` is cleared; then the new copy is made as
usual. Ten layouts at most. There is no top bar any more, so the layouts menu is `Space [`, the
palette and the switch node itself, which all read the same switch.

### Panel arrangement

The dotted header handle undocks and moves a panel. Drop at a docked panel's
edge to create a split; the header menu's Dock returns a floating panel to its
original place. Window mode floats inside the editor. The lower-right handle
resizes it, and the disclosure chevron collapses or expands its body. One drag
is one undo entry. Splitter ratios and fixed sizes (`flow.md` §11.11 has the layout forms, fixed
sizes, strips and start keywords), docking syntax, disclosure and window bounds
save with the selected layout, including leader-key visibility changes.

```lisp
(layout
  (editor "studio")
  (panel ["studio" "network"] :collapsed false :window [500 80 620 450]))
```

A workspace without an editor graph writes its current shell on the first panel
edit. Floating viewports share the shell's UI paint order and hit tree.

Layout has one codec, `Layout_by_path`. There is no display, bend or wireless entry: a file
that has one reads and the entry is dropped, and an entry whose path is no longer in the
workspace is pruned on load and on structural edits (`flow.md` §4.1). Its readers are:

| Saved field | Reader / behavior |
|---|---|
| `editor` | `Workspace_doc.editor_graph` selects the authored shell. |
| `panels` | `Core.panel_state` supplies disclosure and floating bounds to shell geometry. |
| `at`, `collapsed`, `frames` | `Core.sync_scope` supplies Scope positions, zone disclosure and canvas frames. |
| `level`, `pinned`, `rows` | `Core.sync_scope` supplies each node's detail level and its row pins. |

Scope movement/frame sizing, shell splitters and floating-window movement keep
their held-drag offsets in pane/chrome state. Release emits one document edit;
one undo restores the previous placement. Parameter scrubs instead apply while
held and coalesce into one history gesture.

Each viewport has its own free orbit and scene membership, including lights
and camera objects drawn as guides. The render controller remains shared:
ACTIVE resolves only among primary scene cameras, so look-through and PNG
export use that camera and its lens/settings even while a comparison panel is
focused. Comparison camera declarations retain their authored values and do
not select or replace the primary render camera. The primary World, its day
cycle and map gestures are also shared across views; `ui/viewport` does not
declare an independent World. Editing a comparison ref leaves that World's
baked output and the primary render camera unchanged. Independent authored
per-view render-camera/World controllers would require an explicit API extension.

## 5. Sketches as `.rays` files

```lisp
; sketches/bloom/sketch.rays: the only authored file
(workspace bloom
  (graph flower :context sop [(petals : int 12) (seed : int 7)]
    (let* [ring (for [i (range petals)] …)]
      (sop/merge ring))))
```

- **The build.**
  - `sketches/dune` includes a generated `dune.rays.inc`, which
    `rays-lisp dune sketches` writes.
  - `runtest` diffs the include, and `dune promote` accepts a new sketch.
  - For each sketch the include has a `subdir` stanza: a rule running
    `rays-lisp ml sketch.rays` to produce `main.ml`, an executable, and
    a `smoke-all` run.
  - There is no custom dune stanza and no per-sketch `dune` file.
- **Errors at their line.** `rays-lisp check` prints `File "…/sketch.rays",
  line L, characters A-B:` diagnostics, the OCaml compiler's format, so dune
  and editors jump to them (register O3). Warnings are errors unless the workspace form
  carries `^:allow-warnings`. `rays-lisp fmt` prints the canonical text, also for a file
  with warnings.
- **One plan.** The generated `main.ml` embeds the verbatim source and its
  digest, and calls `Rays_editor.Workspace.main`. That re-parses the
  source at startup and runs the same plan the live editor edits (register
  O4).
- **The file is the source** (register O1).
  - Save rewrites `sketch.rays` atomically with the comment-preserving
    printer, if its digest still matches the build.
  - Otherwise edits are saved as a document preset (an s-expression: the
    same text plus layout and settings).
  - Changes to the file on disk reload the running sketch as one history
    entry. Probes and layout are kept by path id.
- **OCaml hosts.**
  - There is no antiquotation (register O2).
  - A host program checks a `.rays` text with `Rays_editor.Workspace.load`; graph inputs
    are overridden by graph name through the `?inputs` of `Flow.Eval.static` and
    `Flow_sop.Lower.workspace`.
  - A `[%workspace]` PPX with generated input records is deferred until a
    host needs it.

## 5a. Live values and realtime recooking

- **Liveness** (register T1). A term is live when it mentions `t` or depends
  on a live binding, capture, accumulator, `ref` override, graph or
  function. Live nodes show **◷ t**. While playing, they and their
  downstream nodes recook every frame, and every other node stays cached.
  `t` is reserved.
- **Fixed structure** (T2, T3).
  - `E_TIME_COUNT`: a loop count, collection or geometry list length
    depends on `t`.
  - `E_TIME_BRANCH`: a live test chooses between geometry arms.
  - Live choices between values are parameters and are allowed. One
    lowering serves every frame.
- **Realtime** (T4). Each frame, live parameters are re-evaluated and only
  changed ones are applied. Then comes an incremental compile and a
  latest-request background cook.
  - A slow cook shows the last finished result and never blocks.
  - Fixed-step runs and exports wait for each frame and are deterministic.
  - Live nodes keep one cache slot, so they never evict static entries.
- **Drags are live edits.** Scrubbing a value recooks the affected cone and
  commits one history entry on release.
- **Case study.** The [Orrery](case-studies.md#orrery). `Flow_sop.Value_lane` is the
  implementation (`flow.md` §13.2).

## 6. Where it is implemented

| Part | Module |
|---|---|
| Reader, printer | `Flow.Syntax`, `Flow.Lisp` |
| Macros | `Flow.Macro` |
| Static pass: names, types, zones, liveness, loop invariance | `Flow.Workspace` |
| Run pass: values, the geometry plan, residuals | `Flow.Eval` |
| Lowering to SOP networks, compiled ids by iteration tuple (register I1) | `Flow_sop.Lower`, `Flow_sop.Instance_path` |
| Live values | `Flow_sop.Value_lane` |
| Gestures as rewrites of the text | `Flow_graph.Flow_edit` |
| Zones, rails, rows and chips, without drawing | `Flow_graph.Projection` |
| Footers, sparklines, iteration lists | `Flow_graph.Probe` |
| The pane: zones, selectors, typed requests | `Pxui_graph.Scope` |
| Scene, World, settings and editor graphs | `Editor_document.Contexts`, `Scene_sync` |
| `.rays` build and check | `tools/lisp` (`rays-lisp`), `sketches/dune.rays.inc` |

## 7. Functions, data, branches and macros

This section extends §2–§3 with the rest of the language a sketch needs.
The rules are in the [register](ambiguities.md) (F, D, C, M and N items). All
of them are implemented in `lib/flow` and in the study, whose `prototype/check.cjs` covers each.

### 7.1 Grammar delta

```text
expr    += fn | list | record | cond | case | str | hof | access | quasi ;
fn       = "(" "fn" "[" { pattern | "(" name ":" type ")" } "]" body ")" ;
pattern  = name | "[" { pattern } "]" | "{" ":keys" "[" { name } "]" "}" ;
list     = "(" "list" { expr } ")" ;                   (* homogeneous *)
record   = "{" { ":" name expr } "}" | "(" "values" { ":" name expr } ")" ;
access   = name "." name { "." name } | "(" "get" expr ":" name ")" ;
cond     = "(" "cond" { expr expr } ":else" expr ")" ;
case     = "(" "case" expr { literal expr } ":else" expr ")" ;
hof      = "(" ( "map" | "filter" | "reduce" | "sort-by" ) expr { expr } ")" ;
type    += "fn" | "(" "list" type ")" | "{" { ":" name type } "}" ;
macro    = "(" "defmacro" name "[" { name } [ "&" name ] "]" template ")" ;
template = "`" form ;       (* ~p fills a hole, ~@rest splices, x# is fresh *)
meta     = "^:bypass" form ;
```

List operations are `first last rest nth reverse take drop concat count`.
Destructuring works wherever a name is bound: `let*`, loop clauses and `fn`
parameters.

### 7.2 Semantics that shape the editor

- **Functions are non-escaping values** (F1). A `fn` may be bound, passed to a
  higher-order form or a defn parameter of type `fn`, and called. It can't be
  returned, stored or carried through `ref` or `fold`. Every program keeps one
  finite graph, and recursion stays impossible.
- **A function is a loop over its calls** (F3). Each call pushes an
  iteration frame `(function, call index)`. Records, probes, sparklines and
  viewport provenance therefore work for functions exactly as for loops.
- **map zips** (F4) and **for multiplies** (L2), so the two forms never
  overlap. Nothing broadcasts implicitly (D1).
- **Records are structural** (D2). `values` is a record, so several results
  and several outputs are the same thing.
- **Branches are exhaustive** (C1), and `str` formats deterministically (C2),
  which makes computed group names reproducible (G2).
- **Macros are hygienic declarative templates** (M1). A hole may be a name
  chosen by the caller (M2), which is what makes `radial` usable. The
  expansion is a read-only view (M3).
- **Comments attach to the next binding by name** (N1), and `^:bypass` is
  the only metadata of an expression (N2). A `defmacro` template is always quasiquoted, and a
  catalog kind may be passed as a function value: `(map sop/facet xs)`.

### 7.3 Drawing and gestures

| Construct | Canvas | Direct manipulation → Lisp |
|---|---|---|
| `name (fn [a b] …)` | hollow λ zone; the rail lists parameters and captures; the selector steps through every call; the output socket is a diamond | **L** on a selection: outside inputs become parameters, and the selection becomes the first call |
| `(map f xs)` and other higher-order forms | a card with a diamond `f` row; filter shows `kept a of b` | wire a λ output, a defn or an operator into `f`; an inline `fn` is a chip that ƒ lifts into a zone |
| `(list …)` | one row per item with its index; a stacked socket | scrub; ↑ moves an item up; + appends, continuing a numeric step; a wire dropped on + appends a symbol |
| records and `values` | one output row per field under the card | + field takes `name value`; dragging from a field row writes `r.field` |
| destructuring | a *split* card with one output row per name | every name is wireable |
| `cond` / `case` | when/then rows with a required else | scrub and wire each arm |
| `str` | a row per part; the result in the footer | + adds a text part; a wire makes a part |
| macro call | ◆ card; rows are holes; a binder hole is a pill; ⤵ opens a lens with steps from call to full expansion | **M** on a selection turns chosen literals into holes and inner names into `x#`; *Replace call with expansion* inlines it |
| `; note` | a note row on the node | type in the inspector (**N**) |
| `^:bypass` | a hatched card with dimmed text | press **B**, or the context menu |

### 7.4 Why these designs

A research pass and three competing mockups per problem converged on one
principle: add nothing that doesn't extend the existing zone, chip, row and
socket vocabulary. The precedents we adopted, and what each costs elsewhere:

- **λ zones** follow Blender 5 closure zones and Snap! rings. We avoid Snap!'s
  implicit parameters and Blender's name-matched sockets.
- **Calls as iterations** turn Excel's per-element spill of `MAP` into the
  loop's iteration selector that already exists.
- **Field rows** borrow Unreal's split struct pins, without the lock-out on
  recombining.
- **The expansion lens** is DrRacket's macro stepper, driven by the same probe
  gesture as loops.
- **Hole punching** follows Figma component properties: a hole is a
  per-instance override.
- **The list fill** is the spreadsheet fill handle.

Deferred ideas, recorded so they aren't lost:
- *spill a list wire* into a floating strip;
- *override for this call*, which evolves a template from a ghost edit;
- *livelit rows*, macro-declared widgets for holes;
- *bypass preview*, ghost sparklines before committing.

## 8. Editor consistency contract (2 October 2026)

These are implementation requirements for the current editor.

- Authored source, layout and sketch settings install as one checked document.
  A derived scene/World/settings edit reconciles through its source home before
  entering history. A refusal preserves the installed document.
- Document Lisp and whole-document copy serialize saved metadata as well as
  graphs. An absent metadata form keeps its supplied fallback; explicit settings
  start from schema defaults, and an explicit empty layout clears layout. A full
  preset starts with default settings, independently of the running session.
- Text applies before cook submission. Asynchronous rendering may retain a
  successful previous cook; an awaited update renders the applied document.
- Geometry, lights and camera bookkeeping resolve the same viewport instance.
  An empty override is distinct from a missing override. Focused previews remain
  focused when the UI is hidden; renderer caches key by viewport identity.
- Inspector ownership is independent of the navigation level. Composition
  invalidation includes the actual scene, level and viewport membership.
  Named SOP handle selection, transforms and argument edits also resolve the
  compiled node's owner. Viewport framing works without a graph pane and uses
  only the focused scene instance's bounds; an empty instance has no frame target.
- Unique named viewport bindings key their orbit/render slots by editor graph
  and binding name. Reordering and docking keep those keys; docking introduces
  a split wrapper without renaming either panel. Renaming changes the authored
  key. Inline, looped, or repeated uses of the same binding use placement keys;
  their transient state may reset when rearranged. No additional persistence
  format is introduced.
- Each lowered viewport retains its panel origin, authored scene ref where
  available, and evaluated instance in `Document.shell.preview_sources`.
  Derived auxiliary objects remain read-only: their inspector explains the
  source-edit route, and shared reconciliation refuses write-back with the
  viewport identity. Editing ref overrides remains the authored route.

Light `:intensity` and `:color` retain residual expressions and resolve at the
editor timeline's `t` during composition, including independent preview
overrides. The saved document stays at its checked source-derived state;
object/panel identities, SOP networks and prepared geometry stay unchanged.
Only recorded live fields are evaluated, using their checked port descriptors;
one resolved scene is cached by authored scene/drives/time. Non-finite or other
evaluation failures retain that light's last successful values while healthy
siblings advance, and report `E_CONTEXT_LIVE` with object/view/field attribution;
recovery clears the error.
Intensity/color keep the schema's ordinary hard-bound normalization.

Other time-dependent fields in scene, World, settings, editor and material structs are refused
when installing the document or reading startup configuration, with
`E_CONTEXT_TIME` naming the graph and field. They are not silently evaluated at
zero. Live SOP/value drives remain supported, including a scene's reference to
time-driven geometry. Startup window settings and the cook seed are captured
when the host starts; their built-in inspector labels say “on restart.” The
selected live fields do not change the window or its captured cook seed.

## 9. References

- Blender manual: [Repeat Zone](https://docs.blender.org/manual/en/latest/modeling/geometry_nodes/utilities/repeat_zone.html),
  [For Each Geometry Element Zone](https://docs.blender.org/manual/en/4.3/modeling/geometry_nodes/utilities/for_each_geometry_zone.html).
- vvvv gamma, The Gray Book: [Loops](https://thegraybook.vvvv.org/reference/language/loops.html)
  (splicers, accumulators).
- SideFX: [Looping in SOPs](https://www.sidefx.com/docs/houdini/model/looping.html).
- Grasshopper data matching: [forum discussion](https://www.grasshopper3d.com/xn/detail/2985220:Comment:666154).
- Bret Victor, [Learnable Programming](https://worrydream.com/LearnableProgramming/) (2012).
- Enso: [dual visual and textual representation](https://github.com/enso-org/enso).
