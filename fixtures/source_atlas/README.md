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
its extent on the sheet, the corner of its longest file, its files, lines and name, and its `rank` by
lines. A leaf's `cell` is its place in its crate's reading order, one cell skipped between files; a
file's `top` is the lines of the longer files of its crate. `profile.tsv` has two 32-bin line-length
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
its margins. A still camera reads nothing back; a moving one reads back the slot owners (about
74 KB a frame at 1440 by 1000).

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

Four more sketches read the same tables; the atlas itself is unchanged.

### Netwerk

```sh
cargo run -p rays_bin -- sketches/source_netwerk.rays
```

`sketches/source_netwerk.rays` is the atlas as a white map sheet, with the same tree, camera and
picking. Per frame, device kernels project every node (its collapsed
point or scope mark), every branch (the diagonal from a parent's centre in the file tree, straight
lines between symbols), one square a leaf and the survey grid. A leaf's square is centred in its cell
and its side is `0.2 + 0.62 sqrt(span / 70)` of the cell's, the span capped at 70 lines; its hue is
its crate's (a golden-angle step a crate, drifting 70 degrees across the crate's extent), pale for a
short span. The grid has 64 lines across a sheet 768 points wide and four times as many each time
the scale quadruples; the same lines carry on as ticks outside the neatline, every fourth numbered in
hex. Two lattices of slots are bounded by the screen, not by the table: at 16 points a slot looks up
the leaf under its lattice point by descending the tree (the leaf's first lattice point owns it) and
rules a function's square in paper from the `width` profile; at 180 points the same lookup names files.
Crate names sit on the corner of the crate's longest file below five times. Picking is that same
descent, once. Marks have no alpha, so a fading mark is mixed into the paper; the art's clip is flat
paper drawn over its margins. A still camera reads nothing back; a moving one reads back the slot
owners (about 19 KB a frame at 1440 by 1000).
The keys are the atlas's, plus `C`, which changes the inks: the crate's hue; cobalt source and vermilion
tests; blue short spans to orange long ones. The art window fills the frame and the title, the scale,
the survey numbers and the inspector sit in the sheet's four margins. `netwerk_overview.png` and
`netwerk_zoom.png` are its views.
