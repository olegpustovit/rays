# flow_survey

A survey chart of a drifting island field, every pixel computed by a Rays Lisp kernel on the
GPU lane. It is the second iteration of `flow_contours` and keeps its idea: one animated height
field, `(noise3 [x y drift])`, read per pixel for lines and once per cell of a square grid for
glyphs that are boxes, rings and diagonals of cell-local distances, with no font and no texture.

What it adds is the vocabulary of a film interface: a near-black ground, one bone ink in three
thinner tints, and one signal ink that is on screen only while something happens.

- `contours`: the coast is the brightest line and each line inland is fainter.
- `plateau`: everything above the last contour is hatched, so a summit is a tone, not a hole.
- `sea`: registration crosses on a coarse grid, only where the height under them is open sea.
- `xs`, `dots`, `rings`, `squares`: one small glyph per band, darker outside and brighter
  towards the summit. None fills its cell, so neighbours never join into a slab.
- `echo` and `sparks`: the ping. A level climbs from under the coast to over the summit `rate`
  times a second; `echo` draws the contours near it in the signal ink and `sparks` marks a
  share of the cells in the band it is crossing. Between pings the chart is monochrome.

A cell's glyph is chosen by a hash of the cell index, never of the height or the time, so a
glyph only appears or leaves as a contour or the ping passes over its cell.

## The pointer

- Its height moves the coast by `swing` levels: up is more land.
- Its side zooms about the middle of the viewport, `reach` times in at the left and out at the
  right.
- The left button multiplies time by ten.

A kernel cannot read a `(state ...)` value (`E_PACKED_STATE`), so nothing accumulates: there is
no scrolled pan or wheel zoom, and the fast clock is `t` times ten, which jumps to another part
of the drift when the button goes down and back when it comes up.

## Kernels

Every layer is its own kernel of at most 64 registers, and the glyph kernel is near that limit.
That is why a layer takes an ink and no separate opacity: a dimmer layer is drawn in a thinner
tint (`chalk`, `ash`, `dust`, each `bone` times a number).

The dataflow is pointer, controls -> `field`, `ping`, inks -> layers -> `result`. `speed`,
`scale`, `cell`, `levels` and `base` are those of `flow_contours`; `summit` is how many contours
there are from the coast up, `fade` how far inland the lines reach, `rate` the pings a second,
`bone` and `signal` the two inks.
Run: `dune exec sketches/flow_survey/main.exe`.
