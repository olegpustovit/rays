# Source Atlas

```sh
tools/source_atlas_index
cargo run -p rays_bin -- sketches/source_atlas.rays
```

A Lisp remake of the OCaml `code_quadtree` sketch ("Source Strata", last in `923dcecfe^`): the same
plate, layout, marks and camera, over rust-analyzer symbols of Rays and the PXUI submodule.

Scroll or pinch zooms about the pointer (the point under it stays there on every frame, down to the
smallest symbol), click dives into the file or symbol under it, drag (left or middle)
pans, right click steps back, `R` or Space resets, `T` shows cells instead of branches, `H` or Tab
hides the overlay, `S` saves `_out/source-atlas.png`, Escape quits. The camera eases to its target.

## Data

The indexer requires `rust-analyzer` on PATH. It opens every `.rs` file as a detached file (no Cargo
workspace, build or sysroot) and asks for `documentSymbol`. An unanswered request (`null`, while the
server still loads) is retried and never stored: an earlier version cached it as "no symbols" for
most of the repository. Generated output, historical copies, fixtures, vendor directories and
symlinks are excluded. Only Rust files are indexed. Real data and the
content-digest cache live in `_out/source_atlas/`, outside Git.

`atlas.tsv` holds the whole tree in breadth-first rows, a node's children contiguous: the file
quadtree (unequal rotated quadrant budgets over prefix sums of `1 + sqrt(lines)`), then each indexed
file's nested symbol field (the same packing over `sqrt(span)`, a named scope inset inside its
symbol). The layout is a transliteration of the OCaml `build` and `symbol_layout`; `x`, `y`, `size`
are world units. `kind` is 0 branch, 1 file, 2 unindexed file, 3 packing node, 4 scope, 5 leaf
symbol, 6 empty file. `crate` is a row of `crates.tsv` (the directory of the nearest `Cargo.toml`):
its extent on the sheet, the corner of its longest file, its files, lines, name and capital `title`,
and its `rank` by lines. A leaf's `cell` is its place in its crate's reading order, one cell skipped between files; a file
or a scope has the `cell` of its first leaf and the `cells` to its last, and a symbol's `level`
counts the scopes around it. A file's `top` is the lines of the longer files of its crate. `profile.tsv` has two 32-bin line-length
profiles a file, row `file * 32 + bin`: `width` hashes a line to its number modulo 32 and keeps the
longest, `mean` cuts the file into 32 slices from first line to last and keeps each slice's mean.

The symbol cache is keyed by rust-analyzer's version and the file's bytes. A cache written by an
indexer older than `4574a9481` may hold empty answers for files that have symbols (a quarter of the
repository on one machine); delete `_out/source_atlas/symbols` once if whole crates draw blank.

`table/load` reads a typed UTF-8 TSV relative to the workspace file: `name:int`, `name:float` or
`name:text` headers. Reload the workspace after regenerating a table. Web export reports
`E_WEB_TABLE`, and the sketch is left out of the web check goldens because its table is generated.

## Drawing

All of it is Lisp over the table. Per frame, device kernels project every node (its collapsed point
or scope mark), every branch (orthogonal paths in the file tree, straight ones between symbols) and
four stipple samples a leaf. The finer stipple levels (16, 64 and 256 samples a leaf, visible from
8, 16 and 32 points) are bounded by the screen, not by the table: a lattice of slots at that pitch
looks up the leaf under each lattice point by descending the tree, and the leaf's first lattice
point owns it. File labels use the same slots at 180 points. Picking is that same descent, once.
Marks have no alpha, so a fading mark is mixed into the ink; the art's clip is flat ink drawn over
its margins. The slot owners stay on the device as exact 32-bit integers (-1 for no owner); the
host reads only the 96 label slots' owners it needs for their text, two words a slot: 768 B a
frame, still or moving, at 1440 by 1000.

`tree/` contains 32 tiny Rust files across Rays and the three PXUI tiers. `atlas.tsv`, `crates.tsv`
and `profile.tsv` are their real indexer output; `overview.png` and `zoom.png` are native GPU views at
zoom 1 and 8, and each sister sketch below has its own.

```sh
tools/check-source-atlas
cargo t -p flow_native source_atlas
cargo t -p rays_editor canvas_pane_input
RAYS_SLOW=1 cargo t -p flow_native --test source_atlas full_table
```

The regressions check the tree (every file once, children contiguous and inside their parent,
siblings apart), picking of every file and symbol under the pointer, the eased wheel zoom and its
anchor, click, right click, drag, reset and the toggles, the web refusal, the two rendered views,
and that a mark kernel calling a local function over a state stays a kernel. Set
`RAYS_UPDATE_FIXTURES=1` only when intentionally refreshing goldens.

Measure the device path with `rays mem sketches/source_atlas.rays --frames 301`; `rays render` uses
the CPU reference evaluator. `rays mem` holds the camera still, which the executor remembers: to time
motion, make the scale follow `frame/index` in a copy, or run the full-table case above. On an
Apple M1 at 1440 by 1000 with about 69,000 nodes, a release build draws 11 ms a frame with the camera
moving every frame (measured under heavy unrelated load; the full-table case enforces 16.7 ms).

## Sister sketches

Four more sketches read the same tables; the atlas itself is unchanged. Each is a sheet in fixed
units (a square of 1000, or 1000 by 1300 and 1000 by 1280) drawn as large as the window allows, with
its lettering set through `draw/text :font :align :tracking` in the faces of `crates/rays/fonts`.
None has an inspector: the sheet is the whole window. Shift with `+` or `-` enlarges or shrinks the
whole sheet a step about the pointer, lettering and all, and Shift with `0` fits it again.

### Netwerk

```sh
cargo run -p rays_bin -- sketches/source_netwerk.rays
```

The atlas as a printed map sheet, with the same tree, camera and picking. The art is a square with
the world in it; title, scale, the files, symbols and crates, and hex survey numbers on all four sides
sit in the margins. Per frame, device kernels project every node (its collapsed point), every branch
(the diagonal from a parent's centre in the file tree, straight lines between symbols), a scope's
outline and the survey grid. The inks are computed once, in OKLCH, four a node so a kernel picks the
palette per element: the crate's hue (a golden-angle step a crate, drifting 70 degrees across the
crate's extent); cobalt source and vermilion tests; blue short spans to orange long ones; and the dark
plate. `C` changes them. The grid has 64 lines across the sheet and four times as many each time the
zoom quadruples; the same lines carry on as ticks outside the frame. The 16 largest crates are named
on the corner of their longest file, and a 180-point lattice of slots names files once they are that
large.

A leaf is the atlas's stipple without a bottom. Its samples are one quadtree of lattices nested in its
cell: sample `(a, b)` of the `2^n` lattice sits at `(a + 2^n / 32) / 2^n` of the cell, so each level's
dots are among the next one's, and a sample is lit by a hash of its place on a `2^20` lattice under the
leaf's weight (the share its span earns), so a dot keeps its state at every level. Below 64 points
this is exactly the atlas: a point, then 4 samples, then 16, 64 and 256 from 8, 16 and 32 points, each
level drawn for the leaves that own a slot of a lattice at its pitch. From 64 points the lattice is the
one whose spacing is 2 to 4 points; the samples new at that level fade in as the spacing grows, so the
picture at 4 points is the next level's at 2 and it divides at any zoom. That part is bounded by the
screen: 32-point blocks, each drawing the lattices of the leaves at its four corners (a leaf of 64
points that touches a block holds one), up to 16 by 16 samples each; a block's four lattices are
worked out once a frame and read back small. Dots are 2 points (1.5 dots tall for functions and
methods), smaller only while a leaf is under 64 points.

On the full table at 1440 by 1000, a frame takes about 51 ms (the original Netwerk 20, the atlas 17;
the 60 Hz budget check fails for both, as it did before), 12 of it the deep levels, with 47 KB read
back. The first frame compiles the kernels, about a second.

### Ledger

```sh
cargo run -p rays_bin -- sketches/source_ledger.rays
```

No tree: every leaf symbol in reading order, one tick each, like a page of text whose words are
functions. A crate starts a new line and a file leaves a gap (the indexer's `cell`); a tick's height
follows its span, a one-line symbol a dash; blues are a function, a method and a field, grey the rest,
orange a span of 40 lines and red one over 70. The layout is computed once: the fewest columns at
which every crate's lines fit the field, a cell nine fifths as tall as it is wide. A frame only
projects the ticks through the camera, which is the atlas's: the wheel zooms about the point under
the pointer and eases there, a drag pans, `R` or Space resets. Zooming in shows the ledger's nesting:
from 8 points a line each file is ruled over its ticks and each scope under them (a scope inside a
scope a step higher, a rule carrying on along the next line where it runs over the end of one), and
from 44 points a tick the symbols are named upward beside their ticks and a file over its first.
A cell of the ledger holds its leaf, so a name is a lookup. `C` changes the inks to greys with the long
spans in black and red. `ledger.png` is its view.

### Strata

```sh
cargo run -p rays_bin -- sketches/source_strata.rays
```

A news page: the 30 largest crates as columns hung from one line, largest first. A file is a block as
tall as its line count (the longer files of a crate on top, the indexer's `top`), cut into 32 bands
from its first line to its last and coloured by the `mean` profile: a band's place on the scale is its
slice's rank among all slices, blue for the shortest lines of the repository through pale at the
median to orange for the longest. Both inks of every band are computed once. Bands are snapped to
whole points, so a file under 32 points keeps the bands that land on one. The longest file is called
out, and the notes band counts what the four largest crates hold and what the page leaves out. The
chart has the atlas's camera: the wheel zooms the columns about the pointer, a drag pans, `R` or Space
resets; the scale counts every 1,000 lines from 4 times and every 100 from 32, and the 160 longest
files are named in their blocks once those are large enough. `C` changes the inks to one blue.
`strata.png` is its view.

### Contour

```sh
cargo run -p rays_bin -- sketches/source_contour.rays
```

The atlas set in type, one ink on white. A 104 by 104 lattice lies over the quadtree; a lattice point
inside a leaf's square (the atlas descent, once a point) carries a digit, the whole part of log2 of the
symbol's span: 0 is one line, 5 is 32 to 63, 9 is 512 and up. A digit is 3 by 5 square marks from a
bit mask: `draw/text` is one run a call, and nine thousand runs would not lower. The digits' mass,
blurred by three seven-point box passes each way, is contoured at ten levels by marching squares, and
a cell with three or four digits at its corners is left to them, so the lines fill the gaps the
symbols leave. All of that is computed once; a frame only scales the segments and marks to the window.
The wheel or a pinch scales the sheet as Shift with `+` and `-` do. `C` changes the inks (cobalt; cobalt
with spans of 64 lines and more in vermilion; black). `contour.png` is its view.
