# Source Atlas

```sh
tools/source_atlas_index
cargo run -p rays_bin -- sketches/source_atlas.rays
```

A Lisp remake of the OCaml `code_quadtree` sketch ("Source Strata", last in `923dcecfe^`): the same
plate, layout, marks and camera, over rust-analyzer symbols of Rays and the PXUI submodule.

Scroll or pinch zooms toward the pointer, click dives into the cell under it, drag (left or middle)
pans, right click steps back, `R` or Space resets, `T` shows cells instead of branches, `H` or Tab
hides the overlay, `S` saves `_out/source-atlas.png`, Escape quits. The camera eases to its target.

## Data

The indexer requires `rust-analyzer` on PATH. It opens every `.rs` file as a detached file (no Cargo
workspace, build or sysroot) and asks for `documentSymbol`. An unanswered request (`null`, while the
server still loads) is retried and never stored: an earlier version cached it as "no symbols" for
most of the repository. Generated output, historical copies, fixtures, vendor directories and
symlinks are excluded; non-Rust files are rows without symbols and stay hollow. Real data and the
content-digest cache live in `_out/source_atlas/`, outside Git.

`atlas.tsv` holds the whole tree in breadth-first rows, a node's children contiguous: the file
quadtree (unequal rotated quadrant budgets over prefix sums of `1 + sqrt(lines)`), then each indexed
file's nested symbol field (the same packing over `sqrt(span)`, a named scope inset inside its
symbol). The layout is a transliteration of the OCaml `build` and `symbol_layout`; `x`, `y`, `size`
are world units. `kind` is 0 branch, 1 file, 2 unindexed file, 3 packing node, 4 scope, 5 leaf
symbol, 6 empty file. `profile.tsv` is each file's 32-bin line-length profile, row `file * 32 + bin`.

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

`tree/` contains 40 tiny source files across Rays and the three PXUI tiers. `atlas.tsv` and
`profile.tsv` are their real indexer output; `overview.png` and `zoom.png` are native GPU views at
zoom 1 and 8.

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
Apple M1 at 1440 by 1000 with 69,163 nodes, a release build draws 11 ms a frame with the camera
moving every frame (measured under heavy unrelated load; the full-table case enforces 16.7 ms).
