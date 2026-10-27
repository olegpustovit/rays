---
name: write-rays-lisp
description: Write or refactor Rays Lisp (a .rays workspace or sketch, macros, GPU pixel kernels) so it reads well as text and as a graph. Use when creating or editing any .rays file, writing a defmacro, or when a graph has many parallel wires or unnamed inputs.
---

# Write Rays Lisp

A `.rays` file is read two ways: as text, and as the graph and inspector drawn from
it (`specification/flow.md`). Every binding is a card, every macro parameter is a row
a person scrubs. Write for both readers. `sketches/flow_contours.rays` is the
worked example.

## Names

- A macro parameter is an inspector row label. Name it for what turning it does, in
  words: `share_from`, `stroke`, `radius`, `grey`; never `d0`, `kx`, `w`, `g`.
- The same goes for the gensyms inside a body (`height#`, `cell_x#`, `hash#`): they
  appear as rails and in diagnostics. One-letter names only where the math is named
  that way in the comment above it (`a b` distances, `x y`).
- Bindings are nouns for the thing they hold (`field`, `contours`, `zeros`); a layer is
  named for what it draws, not for the macro that makes it.
- Order macro parameters as a person tunes them: the shared input first, then where,
  how many, shape, colour. Drop a parameter every caller passes the same value for.

## Dataflow

- Draw the graph on paper first: controls -> shared values -> layers -> result. That is
  the `let*` order too.
- A value several cards need is one binding, wired to each. Never repeat a literal or an
  expression across calls; a person changing it must change it once.
- Several values that travel together are one record card
  (`field {:drift (* t speed) :scale scale :levels levels :base base}`) and the macro
  takes the record: one wire a layer, not four.
- A tunable is a literal binding with a comment line above it (the comment becomes its
  note; a trailing comment attaches to the next binding). Give floats a decimal point
  (`48.0`) so the field scrubs as a float.
- No number twice, in macro bodies too: an image's `:width`/`:height`, a grid's columns
  and rows, a position are fields of that record (`:width field#.width`), written once in
  the graph where a person can edit them. Derive what follows from them (a pixel's size is
  `scale * columns / width`) instead of typing the result.
- The viewport is given, never typed: `(frame/width)` and `(frame/height)` are the size
  in points of the canvas pane showing the picture (the whole window when there is no
  editor around it), live like `t`, `(frame/dt)`, `(frame/index)`, `(pointer/x)` and
  `(pointer/y)` (`specification/workspace/iteration.md`). A full-viewport effect sizes
  its image with them and places it `:at [0 0 0]`; a grid is `(/ (frame/width) cell)`
  columns of a `cell` control. The pointer is still in window points.
- Derived values (`(* t speed)`) live in the graph, in the record, not inside each
  kernel.
- Cards that are layers of one picture start as chips in the `(layout ...)` form, so the
  overview is the dataflow and a click opens the rows.

## Macros

- One macro per idea. If two macros share most of a body, find the parameter that makes
  them one (an `x` is a box with a small `diagonal`) before copying the body.
- Put a comment above each macro saying what it draws and what each parameter means, with
  the formula if the shape is one.
- Macros unquote only their parameters: `~field.scale` is `E_MACRO_UNQUOTE`. Bind it
  first, outside the kernel: `(let* [field# ~field] (draw/image (image/map (fn [uv#] ...
  field#.scale ...))))`.
- Bind a constant used more than once (`one# 1 half# 0.5`): every literal occurrence is
  its own register.

## Pixel kernels (`image/map`)

- A kernel is a packed program of at most 64 registers (`E_PACKED_LIMIT`): each input
  component, literal, capture and operation is one. Split a picture into layers
  composited by alpha with `draw/merge` rather than squeezing one kernel.
- Operators are those in `lib/flow/packed_ops.ml` (`+ - * / mod pow min max`, the
  comparisons, `and or not`, `sin cos sqrt abs`, `length`, `noise3`, `if`). No `floor`:
  quantize with `(- x (mod x 1))`.
- A kernel captures scalars and vectors, and record fields by name; a whole record is
  `E_PACKED_CAPTURE`.
- Anything that picks per cell (which glyph, which colour) hashes the cell index, never
  an animated value, or it flickers every frame.

## Check

```sh
dune build tools/ui_shot.exe
_build/default/tools/ui_shot.exe sketches/<name>.rays /tmp/out.png 1500 900
UI_SHOT_DO="select:picture/<card>" _build/default/tools/ui_shot.exe ...   # its inspector rows
```

Read the picture: the status strip shows the first diagnostic, the graph shows whether
the dataflow is the one you drew, the inspector whether the rows read as words. A new
sketch also needs `dune build @sketches/runtest; dune promote sketches/dune.rays.inc`.
