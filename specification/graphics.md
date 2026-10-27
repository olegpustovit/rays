# Graphics and scene drawing

Code builds immutable `Scene.t` values and lets `Sketch` present them through
the SDL3-windowed OGPU/Metal renderer, the only execution path.

## Drawing vocabulary

The 2D surface includes clear, points, lines and thick lines, rectangles and
rounded rectangles, circles and ellipses, triangles and polygons, polylines,
arcs, pies, Bézier curves, images, system UI text, and loaded-font text.
High-level scene constructors take explicit colors and return pure nodes;
the compatibility functions may use the current color when `?color` is absent.

Later nodes draw over earlier nodes within a scene or immediate frame. Scoped
`Scene.group`, `translate`, `rotate`, `scale`, `clip`, and `blend` preserve
nesting without leaking state to following nodes. Path contour boundaries and
even-odd/non-zero fill rules survive native command lowering.

## Coordinates and density

Coordinates are logical points with `(0, 0)` at the top-left, positive X to
the right, and positive Y downward. The convenience `Scene` constructors use
integer coordinates; `Path` commands retain floating-point coordinates and
curves are adaptively tessellated without rounding to integers. `Frame.input.size`,
scene geometry, input positions, and PXUI layout share that coordinate system.

`Frame.drawable_size` exposes native Metal drawable pixels and
`Frame.pixel_scale` gives backing pixels per logical point. Application code
must not multiply ordinary drawing or pointer coordinates by this scale. The
runtime converts a logical viewport to drawable edges once at the native
boundary. Capture and export are the explicit backing-pixel operations.

## Images and text

`Scene.image` references owned `Image.t` values. The renderer snapshots identity and generation, uploads only
changed content, validates resource lifetime/device identity, and retains the
sampled texture until command completion.

`Scene.text ?size` resolves an installed platform UI font, with
`RAYS_UI_FONT` as a portable override. The size is expressed in logical
points; glyphs rasterize at native density and draw at logical dimensions.
Renderer-local text textures use a bounded 256-entry LRU. Empty strings are
safe no-ops.

## Native effect boundary

Scene construction is pure. `Scene.render` and `Sketch` are effect
boundaries that stage checked render commands, resolve resources, encode Metal
work through OGPU, and present one SDL3 Metal drawable. Native Metal or drawable
unavailability is a typed startup/runtime error; it never selects another
renderer.

Window, event, resource, and presentation operations stay on the initial OCaml
domain. Pure geometry preparation may use `Parallel`, but it joins before
native command recording. Draw ordering and resource lifetime remain
deterministic for a fixed scene and input stream.
