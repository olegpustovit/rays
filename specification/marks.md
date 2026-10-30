# Marks: the run-time drawing contract

Status: implemented. `Scene_command.Marks` (`marks.mli`) is this contract as pure
data; `Flow_ir.Core_draw` lowers a draw graph, `Flow_ir.Core_exec.draw` produces the
table, and `Flow_native.Marks_scene` adapts it to `Scene`. Defaults and error codes were taken
from the per-kind arms of `lib/rays_editor/drawing.ml` (removed in P3.13); the
`test/draw_snapshots` digests of the 15 eligible workspaces pin them.

A draw graph, run once per frame by the core executor
([core.md](core.md), "Drawing statements and intrinsics"), produces one **mark
table**: an ordered array of entries. Every default is resolved and every
check is made before the table exists. A consumer applies no default, never
reorders, and never reads a Lisp value. The native consumer is the adapter
`Flow_native.Marks_scene` (P3.8), which builds the same `Render_ir` commands `drawing.ml`
builds today; the web consumer is the fixed runtime.

## Units and encodings

- Coordinates, sizes, radii and widths are **logical points**. The origin is
  the canvas top-left, x grows right, y grows down. Conversion to drawable
  pixels happens once, in the presenter (`AGENTS.md`, "Coordinates").
- Table floats are binary64. They are narrowed to binary32 once, when an
  instance or a vertex is written for the GPU. Sums such as `x + width` are
  computed in binary64 and then narrowed.
- A **colour** is packed `0xRRGGBBAA` in an `int32` (red in the top byte),
  straight alpha, 8 bits per channel, the value `Rays_math.Color` gives and
  `Scene.rgba` packs. Constants are decoded by the compiler before the table
  (`Flow.Op.drawing_color`): hex text `#rgb`, `#rgba`, `#rrggbb`,
  `#rrggbbaa` (`#` optional; `E_DRAW_COLOR` otherwise), a vec3 of 0..1 floats
  (alpha 255), or `(list r g b a)` of 0..1 floats. A float channel `c` becomes
  `clamp(trunc(c * 255 + 0.5), 0, 255)`; a nonfinite channel is `E_NONFINITE`.
- A **colour buffer** holds three binary floats per element (red, green, blue,
  0..1); each element decodes as the vec3 constant does, alpha 255.
- The colour value `0` (`#00000000`) means "none" for a stroke: no outline.
  A fill of `0` is simply fully transparent.

## The table

```
Marks.t = entry array, painter order, first entry drawn first

entry =
  | Mark_set of { count : int; kind : kind }   (* the kind carries its attributes *)
  | Geometry of { vertices : float array; indices : int array; color : int32 }
  | Image of { resource : int; x : int; y : int; scale : float; angle : float }
  | Text of { x : int; y : int; size : int; color : int32; text : string }
  | Push_transform of { xx; xy; yx; yy; tx; ty : float }  | Pop_transform
  | Push_clip of { x; y; width; height : float }          | Pop_clip

kind = Rect of { position; size; fill; stroke; stroke_width }
     | Circle of { position; radius; fill; stroke; stroke_width }
     | Line of { position; end_; stroke; stroke_width }
     | Point of { position; fill : int32 }            (* fill is a constant only *)

attribute = Constant of value | Buffer of { id : int; count : int; width : int }
          | Device of { id : int; count : int; width : int }   (* a GPU array; a colour is float r, g, b *)
value     = (float * float) for position, size, end_; float for radius, stroke_width;
            int32 (0xRRGGBBAA) for fill, stroke
```

A **buffer reference** is an abstract positive integer `id`, an element `count`
and a `width` in floats per element. The id names a per-frame table the caller
owns (a CPU array, or later a GPU region output); the contract never holds the
data. Natively the table is `Flow_ir.Core_exec.Buffers` (`create`, `clear`, `find`):
the executor registers each CPU attribute array in it under a fresh positive id,
the caller clears it once per frame and `Flow_native.Marks_scene` reads it while adapting.
A colour buffer in that table is a packed vec3 array of 0..1 floats, as above. `count` is an element count, not a float count.

A **device buffer** (`Device`) has the same `id`, `count` and `width` but names a GPU array the
caller owns: a region's output, float32, `count * width` elements. Any attribute may be one; a
device colour has width 3 like a colour buffer, float r, g, b in 0..1 and opaque, and the shader
quantizes it (`floor(clamp(c, 0, 1) * 255 + 0.5)`, fill and stroke mode 1). That is the GPU
region's approximate contract, not the host conversion bit for bit: the channel is binary32, a
byte may differ by one at a rounding boundary, and a channel beyond the integer range clamps to
255 where the host conversion wraps (`core.md`, `color/byte`). It is `byte 2` in the
serialization. Natively a region whose every use is such an attribute of a mark set may stay on
the GPU: `Core_exec.draw ~device` offers it, and `Buffers.device` resolves the id to the host's
token (`Core_marks.device_value` is the placeholder in the locals). The CPU adapter has no GPU
and refuses a device attribute (`E_GPU`).

### Mark sets

A mark set is `count` instances of one kind, drawn in index order 0 to
`count - 1`; instance `i` is drawn completely (fill, then stroke) before
instance `i + 1`. Each attribute is a constant (one value for every instance)
or a buffer. Attribute lists are fixed per kind:

| Kind | Attribute | Constant | Buffer width | Default (applied by the compiler) |
| --- | --- | --- | --- | --- |
| rect | `position` (top-left) | x, y | 2 or 3 | none, required |
| | `size` | width, height, each >= 0 | 2 or 3 | none, required |
| | `fill`, `stroke` | colour | 3 | see "Style resolution" |
| | `stroke_width` | float >= 0 | 1 | 1 |
| circle | `position` (centre) | x, y | 2 or 3 | none, required |
| | `radius` | float >= 0 | 1 | 1 |
| | `fill`, `stroke`, `stroke_width` | as rect | | as rect |
| line | `position` (from) | x, y | 2 or 3 | none, required |
| | `end` (to) | x, y | 2 or 3 | none, required |
| | `stroke` (the line's colour) | colour | 3 | white `0xffffffff` |
| | `stroke_width` | float >= 0 | 1 | 1 |
| point | `position` | x, y | 2 or 3 | none, required |
| | `fill` (the ink) | colour | 3 constant only | white `0xffffffff` |

A width-3 buffer is the packed vec3 layout of `Vec3_array`: the third float of
each element (z) is ignored. A constant has width 2 for a position; there is no
constant z. `draw/points` ignores a per-instance `color` (today it falls back to
white), so a point's `fill` is constant only.

**Style resolution** (`Ui_batch.Builder`; the compiler does this, the table
holds the result):

- Neither `fill` nor `stroke` given: fill is white `0xffffffff`, stroke is `0`.
- Only `fill` given: stroke is `0`.
- Only `stroke` given: fill is `0` (transparent, not white).
- The effective stroke width of a rect or circle is `0` when the resolved
  stroke is `0`, otherwise the `stroke_width` attribute. A line's width is its
  `stroke_width` and is not affected by its colour.
- Operators today set `stroke_width` only for lines (`draw/line` `:width`,
  `draw/lines` `:width`, scalar); rects and circles always carry the constant 1.

**Validity** of a mark set:

- `count` is in `0 .. 1,000,000`.
- Every buffer reference has `count` equal to the mark set's count and a width
  from the table; every id is positive.
- Every constant is finite with `abs v <= 3.4028234663852886e38` (largest
  binary32), and a size, radius or width constant is `>= 0`. This is
  `Ui_batch.Builder`'s check; buffer values are checked when read.
- A point set has no stroke and no stroke width.

Kind meanings on the native adapter (the oracle for pixel parity):

| Kind | `Ui_batch` instance (64 bytes, little-endian) |
| --- | --- |
| rect | kind word 10 = 0; words 0..3 = `x, y, x + w, y + h` |
| circle | kind 4; words 0..3 = `x - r, y - r, x + r, y + r` |
| line | kind 5; words 0..3 = `x0, y0, x1, y1`; the colour is in the fill word, the stroke word is 0 |
| all | fill word 8, stroke word 9, stroke width word 12, anti-alias flag word 13 = `1.0`; every other word 0 |

A mark set becomes one `Shapes` command (a count-0 set becomes an empty
`Shapes`, as `drawing.ml` emits today). A **point set** is also `Shapes`: each
position retains its finite logical subpixel coordinates and is appended as a
unit rectangle to the same packed `Ui_batch` buffer. It uses the rect
pipeline's analytic antialiasing on native and web. A count-0 point set emits
nothing. The cap in this section is the one binding `Ui_batch` limit
(`1_000_000`), applied to point sets too.

### Geometry entries

Rounded rectangles, polygons, polylines, the single `draw/point` and
`draw/background` are **indexed path geometry**, not marks. The compiler calls the
existing pure `Scene_command.Shape2` helper and stores its output unchanged:
`vertices` is x, y pairs, `indices` is triangle triples into them, `color` is the
packed colour. A helper that returns a fill and a stroke geometry gives two
entries in that order, **fill then stroke**. No target tessellates, approximates a
path with marks, or merges, splits or reorders these entries (translucent pixels
would change). An empty geometry (no vertices) is a valid entry and is kept. The web runtime's `shape2.js` is
the one port of `Shape2`: it is geometry for this contract, not a core primitive, and a node test pins it to the
native output.

| Statement | Helper and arguments | Entries |
| --- | --- | --- |
| `draw/polygon` | `Shape2.polygon points ~fill ~stroke`; point coordinates retain fractions | fill, stroke (either may be absent; neither gives a white fill) |
| `draw/polyline` | `Shape2.polyline points ~color:ink`; fractional point coordinates, width 1 | one |
| `draw/rounded_rect` | `Shape2.rounded_rect ~width ~height ~radius ~fill ~stroke`, local coordinates | `Push_transform` translate by fractional `at`, fill, stroke, `Pop_transform` |
| `draw/point` | `Shape2.polygon` of the unit square at fractional `at`, fill ink | one |
| `draw/background` | `Shape2.polygon` of `(0,0) (w,0) (w,h) (0,h)`, `w`, `h` the integer canvas size, fill the colour | one |

`rounded_rect`: `at`, `size`, and `radius` retain fractional values (default
radius 0); the helper clamps radius to `[0, min(abs w, abs h) / 2]`. Fill and
stroke are colours; neither given means a white fill. Stroke width is 1.

Validity: `vertices` length even, `indices` length a multiple of 3, every vertex
finite, every index in `0 .. vertices/2 - 1` (the `Render_ir` rules).

### Images and text

- `Image`: `resource` is an abstract positive integer naming an entry of the
  host resource table; an absent resolver is `E_IMAGE`, never a dropped entry.
  Natively `Drawing` numbers the images a frame resolves 1, 2, 3, ... in the order
  the executor resolves them (the core's `image` callback returns the next id) and
  keeps the images in an array, so `Flow_native.Marks_scene` maps `resource` to entry `resource - 1`.
  Ids are meaningful for one frame only.
  `x`, `y` are `at` truncated to integers (default `(0, 0)`). `scale` is finite
  and `> 0` (default 1) and `angle` is finite radians (default 0); otherwise
  `E_DRAW_RANGE`. Native lowering: destination `(x, y, w * scale, h * scale)`,
  source `(0, 0, w, h)`; if `angle <> 0`, the image is wrapped in a transform
  rotating about the destination centre `(px, py)` with `xx = cos`, `xy = -sin`,
  `yx = sin`, `yy = cos`, `tx = px - xx*px - yx*py`, `ty = py - xy*px - yy*py` (a point maps to `(xx x + yx y + tx, xy x + yy y + ty)`).
- `Text`: a whole string, drawn left-aligned, unwrapped, with the automatic
  font, as one image at `(x, y)` (`Scene.text_image`). `x`, `y` are `at`
  truncated to integers; `size` is `floor(s + 0.5)`, default 16; `color` is the
  ink, default white. The empty string emits no entry. Rasterisation is the
  target's; the table carries the string.

### View transforms and clips

`Push_transform` carries the six affine coefficients `Render_ir.transform` uses
(natively a `Scene.Affine` node, built by `Scene.Private.transform`, which keeps the six
coefficients verbatim: a rotation is not re-derived from an angle, so the stored signs
below survive; `Scene.Affine` lowers to `Render_ir.Push_transform`/`Pop_transform`); entries until the matching `Pop_transform` are in the pushed local coordinates.
A transform is **never baked into per-instance attributes**. A statement writes:

| Statement | Coefficients |
| --- | --- |
| `draw/translate` | `xx=1 xy=0 yx=0 yy=1`, `tx`, `ty` the offset truncated to integers (`E_DRAW_RANGE` if nonfinite or out of int range) |
| `draw/rotate` | `a` radians, finite: `xx=cos a  xy=sin a  yx=-sin a  yy=cos a`, `tx=ty=0` |
| `draw/scale` | `xx=sx`, `yy=sy`, others 0; a scalar factor means `sx = sy`, a vec3 uses its first two components; finite |

`Push_clip` carries a rect (`x`, `y`, `width`, `height` finite, extents `>= 0`),
a scissor rectangle in the coordinates in force at the push. No draw operator
emits one yet; the entry exists so a later one needs no format change.

Transform and clip nesting are two independent depth counters, as in
`Render_ir`: every pop has an open push of its own family before it, and both
counters end at 0. Every coefficient is finite.

### Painter order

Entries are drawn in table order, later over earlier, at every level; the
table is flat, nesting is expressed only by push and pop. `draw/merge` appends
its inputs in order and flattens drawing lists; one referenced node used twice
appears twice, in place. The contract does not merge or coalesce marks; batch
merging is an optimisation that preserves this order. The table cap is
`1,048,576` entries (the `Render_ir` command limit).

## Errors

These are raised while building the table; the first one stops the frame.

| Code | When |
| --- | --- |
| `E_DRAW_INPUT` | A required input is missing: `%s needs %s.` (kind, input name) |
| `E_DRAW_LENGTH` | A per-instance buffer's element count differs from the count. `Colors and positions have different lengths.` (fill, stroke or colour), `Radii and positions have different lengths.`, `Sizes and positions have different lengths.`, `Line endpoints have different lengths.` The count is that of `positions` (`from` for lines). Checked before any instance is built |
| `E_DRAW_RANGE` | A coordinate that must be an integer is nonfinite or does not fit an int (`A drawing coordinate must fit an integer.`); and any rejection by a builder or helper: a nonfinite or over-large float, a negative radius, extent or width, a count above 1,000,000, an image scale `<= 0`, a nonfinite angle |
| `E_DRAW_KIND` | A deferred drawing node of any other kind: `No native drawing implementation for %s.` |
| `E_DRAW_COLOR` | Colour decoding failed (`Color.hex`, empty gradient stops) |
| `E_TYPE` | Radius is not a number or float array; sizes are not vec3 or vec3 array; text is not text; a colour has the wrong type |
| `E_NONFINITE` | A rotate angle, scale factor, rounded-rect radius, text size or colour channel is nonfinite |
| `E_IMAGE` | No image resolver |

## Serialization

`Marks.serialize` is stable, versioned and explicitly little-endian, in the
style of `Render_ir.serialize`. It is the P3.1 snapshot oracle for a frame's
marks. All integers are signed 64-bit, floats are the binary64 bit pattern,
colours are `int32`; a string is an integer byte length then the UTF-8 bytes.

```
int32  0x524d4b31 ("RMK1")        magic and version
int    entry count
per entry: byte tag, then
  0 mark set:   byte kind (rect 0, circle 1, line 2, point 3); int count;
                attributes in the table's order (rect: position size fill
                stroke stroke_width; circle: position radius fill stroke
                stroke_width; line: position end stroke stroke_width;
                point: position fill); each is
                  byte 0, then the constant (2 floats for position, size and
                  end; 1 float for radius and stroke_width; 1 int32 for fill and
                  stroke)
                | byte 1, then int id, int count, int width
  1 geometry:   int32 color; int vertex float count; the floats;
                int index count; the indices as ints
  2 image:      int resource, int x, int y, float scale, float angle
  3 text:       int x, int y, int size, int32 color, string
  4 push transform: float xx xy yx yy tx ty      5 pop transform
  6 push clip:  float x y width height           7 pop clip
```

`Flow_native.Marks_scene.scene ~buffers ?image ?gpu ?device marks` adapts a table: it runs `validate` first
(a rejection is `E_DRAW_RANGE`), builds one `Ui_batch` per rect, circle, line or point set
(a colour-buffer element goes through `drawing_color`, so a nonfinite channel is
`E_NONFINITE`). A `Push_clip` rectangle truncates to
integers there. `Pop_transform` and `Pop_clip` close the innermost open push.

A rect, circle or line set whose colours are constants is written whole by
`Ui_batch.Builder.rects`, `circles` or `lines` from the attribute arrays themselves (a
constant is one element read with stride 0): the instances, their order and the refusal are
those of one builder call per mark, with nothing allocated per mark, and the batch takes the
builder's storage instead of a copy of it. A set with a colour buffer goes mark by mark. The
64 bytes an instance are then held once on the CPU (the scene's batch, which lowering draws
from without copying) and once on the GPU.

**Routing (P4.9).** A mark set with at least one `Device` attribute becomes one draw of the
`Marks` family: `gpu ~slot ~kind ~count attributes` (the `slot` numbers the frame's GPU sets from
0) registers a `Renderer.Private.gpu_marks` source which binds each device attribute's
buffer directly and uploads the others in the layout below (constants with stride 0; a CPU
position or extent array as float32 with stride `width`; a colour buffer converted on the CPU, as
for `Ui_batch`, to `0xRRGGBBAA` words, fill and stroke mode 0; a device colour bound with stride
3 and mode 1). The bytes of a host array are kept while the array is physically the previous
frame's, so a static attribute beside a device one is converted and uploaded once, not per
frame. When the positions are a fast-precision kernel's output, the set is drawn indirect: the
kernel wrote its count record `{n, 6n, 1, 0, 0, 0}` (`Flow_gpu.Run.count_record`), and from byte 4
that is the indexed indirect draw of `n` quads (`gpu_marks ?indirect`,
`Scene_execution.Private.register_gpu_vertices ?indirect`, `Ogpu.Backend.draw_indexed_indirect`),
so the instance count the draw uses is the one the device wrote, never batched into an ICB.
`lib/renderer/test_marks_native.ml` draws a set registered for 100 rects whose record says
63 and matches the 63-rect `Ui_batch` pixel for pixel. The set rides in the scene
as a `Marks.gpu_token` carried by `Ui_batch`, so transform, clip, blend and painter order are the ordinary
ones. A rect's stroke width is the constant 1, as `Ui_batch.rect`'s; a line's fill is unused.
Every other mark set, including every set when there is no GPU host, stays a `Ui_batch`:
the draw snapshots hash the CPU scene and do not change. That includes a
static set of any size. It could ride the `Marks` family with every attribute a host buffer
(16 bytes of corners an instance instead of 64 twice), but not with the same pixels: the
shader forms a circle's and a rect's corners in binary32 from binary32 inputs, where
`Ui_batch` narrows the binary64 sum, so a position that is not a binary32 value moves a
corner by one ulp. Measured 2026-10-11 with the static positions of `sketches/million_marks`
as a host buffer beside a GPU radius, against the `Ui_batch` picture at 900 by 700: 85
pixels differ at 2,000 marks, 643 at 20,000, 1,676 at 200,000 and 466 at 1,000,000, each by
one step of one channel. The route needs the corners added on the host and a layout that takes
them (see the end of this file), and a rule for the scene a static drawing keeps, whose token
would only be valid while its GPU source lives. A mark set is
never split: CPU and device attributes of one set share one draw. A region runs on the GPU
only if every use of its result is a `positions`, `from`, `to`, `sizes`, `radius`, `fill`,
`stroke` or `color` of a mark set, the region is placed `Gpu` by the current backend (site in
`approx`, static count of at least 1,024, a measured cheaper cost or `Qualification`), and
no other offer of the same region has run this frame (a second dispatch would overwrite the
first's output; it runs on the CPU). A host float in a device set (a CPU position, extent or
width array) is written as binary32 and a nonfinite or over-large one is `E_DRAW_RANGE`, as
`Ui_batch.Builder` refuses it; a device array's own non-finite values are the kernel's
status check. A token is valid while every device buffer is the one registered and the
source has not been updated; an updated or closed source, or a released GPU lease, makes
lowering and retained replay refuse it (`GPU shapes are stale or closed`).

A table is accepted by the validity check only when every mark set, geometry,
image, text, transform and clip meets the rules above. Image `resource` must be
`> 0`, text `size` is an unvalidated integer (see below), and buffer ids are not
checked against any table: that is the caller's.

## Ambiguities in today's `drawing.ml` that this file fixes

An implementer must keep these exactly or parity with the P3.1 snapshots breaks:

- `draw/point`, `draw/points`, polygon and polyline points, and rounded-rect
  `at`, `size`, and `radius` preserve finite fractional values. Text and image
  positions and `draw/translate` retain their integer conversion; text `size`
  rounds with `floor(x + 0.5)`. Positions of rects, circles and lines are floats.
- `draw/rect`, `draw/circle` and `draw/line` take the `Shapes` route (not path
  geometry), while `draw/point` and `draw/background` take the path-geometry
  route even though they look like a 1x1 rect and a full-canvas rect.
- Rotate stores `xy = sin, yx = -sin`; an image with an angle stores
  `xy = -sin, yx = sin`. The two signs differ and are both kept.
- A stroke-only mark has a transparent fill (`0`), not white; a stroke colour of
  `0` is "no stroke" even for a hex colour `#00000000`.
- `core.md` says invalid transforms are `E_DRAW_RANGE`; `drawing.ml` raises
  `E_NONFINITE` for a nonfinite rotate angle or scale factor. This file keeps
  `E_NONFINITE`; a later task may change both together.
- Text `size <= 0` is not validated in `drawing.ml`; the font layer decides.
- `draw/points` has no count cap in `drawing.ml`; the 1,000,000 cap on point
  sets is new here.

## The `Marks` pipeline family (GPU layout)

`Scene_execution.Marks` is a pipeline family beside `Ui` (the `Ui` shader and its 64-byte
instance are untouched). One draw is one mark set of rect, circle or line: `count`
instances, four vertices (a quad, indices `0 1 2 0 2 3` plus `4 i` per instance) and six
vertex buffers at bindings 0 to 5, then the affine at 6 (24 bytes, as for `Ui`). The
vertex stage reads instance `vid / 4`. No texture, sampler, depth or argument buffer;
blending is the draw's blend as for `Ui`. Instance `i` of an attribute is element
`i * stride` of its buffer; **stride 0 is a constant** (element 0 for every instance), so
a constant and a buffer take the same code. The stages are `lib/renderer/shaders/marks.rays`; the web
prints the same entries into `marks.wgsl` (below).

| Binding | Attribute | Element | Rect | Circle | Line |
| --- | --- | --- | --- | --- | --- |
| 0 | position | float x, y (z, if the stride is 3, ignored) | top-left | centre | from |
| 1 | extent | float | width, height | radius | end x, y |
| 2 | fill | uint `0xRRGGBBAA` (mode 0) or float r, g, b in 0..1 (mode 1, alpha 255) | fill | fill | unused |
| 3 | stroke | as fill | outline; word 0 = none | outline; word 0 = none | the line's colour |
| 4 | width | float | stroke width | stroke width | line width |
| 5 | params | 8 uint (below) | | | |

`params` = `kind` (0 rect, 4 circle, 5 line, the `Ui_batch` kind words), then the strides in
floats or words for position, extent, fill, stroke and width (one 32-bit element each, so a
vec3 position has stride 3 and a float radius array stride 1), then fill mode and stroke mode.
A float colour channel `c` becomes `floor(clamp(c, 0, 1) * 255 + 0.5)`, the same 8-bit value
`Flow_native.Marks_scene` writes, then is unpacked as for `Ui`. A rect or circle with stroke word 0 has
border width 0; otherwise the border is the width attribute. Geometry is computed in binary32
on the GPU (`x + width`, `x - r`), so pixel parity with `Ui_batch` is exact when the inputs are
binary32 values and may differ by one ulp of the sum otherwise; a CPU writer that wants exact
parity pre-adds in binary64 and uploads the corners (use stride 0 or a second layout for that
only if a test shows a difference). A buffer shorter than `count * stride` elements is the
caller's error; the shader does not check.

Routing, the device attributes and the cases that stay on the CPU are under "Routing (P4.9)"
above. The web's `marks.wgsl` is printed at build time from the same `marks.rays` (`print_shader.exe web`,
`Flow_web.Wgsl.raster`) and reads the same six buffers and the same `params`; the host binds the
region's output as the attribute buffer, with its width as the stride. A float colour (mode 1) is
read from the same buffer as a word's bits (`gpu/float-bits`), so fill and stroke are bound once on both
targets. Three differences live in the printer's web placement and its bindings, not in the source: the
vertex id is `instance_index * 4 + corner` of a non-indexed six-vertex draw per instance (so `vid / 4` is
the instance, with no index buffer); the affine shares group 0 at 6 with every draw's constants; and
`fwidth` of a varying is read once at the top of the fragment, before any branch, for WGSL's derivative
uniformity.

### Geometry and quads (Scene2)

`marks.rays` also holds the 2D `geometry` and `quad` pipelines, printed for native (`Scene2`,
`Scene2_textured`) and into `marks.wgsl`. A draw's constants are 20 floats: the affine 0-5 (as the
marks'), the colour flag 6, the ink 8-11, the quad's rectangle 12-15 (`x y w h`) and its texture's
16-19 (`u0 v0 u1 v1`).

- `geometry` draws indexed triangles. Its vertex buffer is words: with flag 0 (the web) a float2
  position a vertex, in the ink; with flag 1 (native) 12 bytes a vertex, the float2 position and the
  packed colour word, a triangle taking its first vertex's (one geometry has one colour, so a packed run
  of them keeps its colours). Native binds the vertices at 0 and the constants at 1, the web at 0 and 6.
- `quad` draws one textured rectangle tinted by the ink, texture 7 (sampler 8). Its constants are its
  only buffer: quad `k` reads floats `20k..20k+19`, so natively a quad's 80 bytes of constants are its
  mesh's vertices (four vertices, indices `0 1 2 0 2 3`) and a coalesced run of quads is one draw, its
  texture in an argument buffer at 7; the web draws one quad an instance of six vertices, its constants
  at 6. The texture coordinate is `u0 (1 - c) + u1 c` per corner `c`, exact at the corners.

Native positions are binary32: the CPU narrows each vertex once (Scene2 meshes), and the affine
(the projection composed with the transform, narrowed from binary64) runs on the GPU, so a quad's corners
are `x + w` in binary32 under the affine, as the web's. A Scene2 vertex was a 68-byte record of doubles
(position, colour word, uv) expanded per index; a mesh is now 12 bytes a vertex, indexed, and a quad 80
bytes of constants (it was four, then six expanded, 68-byte records).
