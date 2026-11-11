# PXUI baseline (phase P0)

Measured at commit `896e1754` plus the measurement case `window_pxui_baseline`
(`crates/pxui/tests/cases/ui_a.rs`), on an Apple M1 (M-series Mac), debug (`dev`) profile, warm shared
state, one cargo process at a time. Re-run with `cargo t -p pxui window_pxui_baseline`.

| Measure | Value |
| --- | --- |
| Heap allocations, warm kit frame (`Ui::frame`, panel with label, toggle, text field, button, 2x, 200 frames after 30 warm-up) | 15.0 per frame, 317 B per frame |
| CPU per warm kit frame (`Ui::frame` only) | about 5.7 us |
| `Ui::scene(None)` after the frame (clones the node list) | 2.0 allocs, 200 B, about 0.1 us |
| Draw batches, same kit panel (`stage_native`, one UI layer) | 2 |
| Clean debug `cargo build -p pxui` (cold `target/`, includes rays, renderer, runtime, sdl3 and the rest of its closure) | 12.35 s wall (42.7 s user) |
| Warm `cargo check -p pxui` after editing `ui_core.rs` | 0.12 to 0.13 s cargo, 0.17 s wall (a bare `touch` does not trigger a re-check) |
| `cargo t -p pxui`, warm (29 existing cases plus baseline, 2 runs) | 2.37 s wall |
| `cargo t -p pxui`, first run with test dependencies cold | 12.75 s wall (14.3 s including cargo) |
| Slowest cases | `gpu_parity_overlays` 0.20 s, `gpu_parity_commands` 0.18 s, `gpu_parity_panel` 0.18 s |

## Not isolated, and caveats

- There was no existing zero-allocation test for PXUI (the proposal's claim is not backed by one in
  `crates/pxui/tests`). The kit frame is not allocation-free today: 15 allocations per warm frame.
  The case reports the count only and asserts nothing; the P2 budget (0) is not met yet.
- Allocation count is the measuring thread's (`mem_sys::local_alloc_count`); bytes are the process-wide
  total (`alloc_bytes_total`), so worker-thread allocations inflate bytes only.
- The frame measured is one small panel, not the whole editor. The proposal's 462 allocations and
  31,882 bytes per frame (editor, empty document) are not reproduced here.
- Not measured: `renderer::lower_ui` (allocates a `Vec<Draw>` per call) and the dirty-atlas RGBA
  expansion. Both need a GPU or mock backend frame and belong to P2's baseline. The 2 allocations above are
  `Ui::scene` only.
- Draw calls: `batches()` counts batches, which is the draw count for the one UI layer; a clipped scroll
  list was not measured, only the plain panel.

## After P2

`window_pxui_baseline` now asserts 0 allocations per warm kit frame (it measures 0 B and about 2 us at
2x, debug). The 15 were the per-frame `ui_batch` publication, `display` copies of labels, the hit
closures of the toggle and button, and the text field's value copies; labels now live in the slot's
cached labels, hit shapes are kept per shape or per slot, and `text_field` returns the new value only
on the frames that change it. `gpu_parity_draw_calls` asserts one painter draw call per layer for the
panel and for the clipped table. A focused text field still copies its value while editing.

## After P5 commit 1 (retained nodes)

Nodes are retained records with `Handle`s; `Look` data replaces the panel's, label's and carry
ghost's paint closures; canvas closures live in the frame arena. `window_pxui_baseline`: 0
allocations, about 3.9 to 4.0 us per warm kit frame before and after (debug, 2x).
`window_pxui_arena_first_frame` asserts the first frame of a new canvas node allocates nothing.

## After P5 commit 2 (hybrid views, engine)

`window_pxui_view_costs` (`crates/pxui/tests/cases/views.rs`): ten views of 50 kit widgets (25 labels,
25 buttons) each, `Ui::frame` only, debug, 2x, 100 frames after 10 warm-up, two runs:

| Frame | CPU per frame | Allocations | Nodes declared |
| --- | --- | --- | --- |
| All ten views kept | 6.4 to 6.5 us | 0 | 11 |
| One view (50 widgets) rebuilt | 42 us | 0 | 61 |
| All 500 widgets rebuilt | 370 to 490 us | 0 | 511 |

The kept frame is mostly the per-slot scans (`prune`, `next_repaint`), O(nodes) but a few ns each.
`window_pxui_baseline` stays at 0 allocations (4.2 to 5.5 us, noisy under load). Editor
`alloc_budget` unchanged: empty/basic/bare 445/652/600 allocations per frame. `gpu_retained_bytes`
asserts an all-kept frame writes 0 instance bytes to the painter and a one-instance patch 64 bytes in
each of the painter's three frame copies.

## After P5 layout and accessibility

`layout_stress_zero_alloc` and `layout_scaling` (`crates/pxui/tests/cases/layout.rs`): a tree of
groups of 20 nodes cycling through every layout mode (flex weights and shrink, min/max, aspect, gap,
per-edge padding and margin, every justify and align mode with baseline, wrap, a fixed + fr + fit grid
with a span, a z-ordered stack, a both-axis scroll box with a sticky header), right to left, with an
anchored popup outside the cached view, in a 2,400-point window; `Ui::frame` only, 1x, warm, best of
alternating rounds. Debug is the `dev` profile (O1); release is `cargo t --release -p pxui layout_s`.

| Measure | Debug | Release |
| --- | --- | --- |
| Full rebuild + layout + paint, 2,006 nodes | 1.05 to 1.15 ms per frame (about 525 to 570 ns per node) | 1.14 ms (568 ns per node) |
| Layout alone (`intrinsic` + `arrange`), 2,006 nodes | 210 to 240 us (105 to 120 ns per node) | 186 us (93 ns per node) |
| The 2,006-node view cached | 14 us per frame | 14 us |
| Cached, one 42-node view rebuilt | 35 to 39 us per frame | 35 us |
| Allocations, any of the above, warm | 0 | 0 |
| Per node at 506 / 2,006 / 8,006 nodes, rebuild | 570 / 580 / 650 ns | 534 / 530 / 614 ns |
| Per node at 506 / 2,006 / 8,006 nodes, layout | 118 / 118 / 145 ns | 91 / 89 / 108 ns |

Release is barely faster than the O1 debug profile here: the frame is dominated by text-key hashing,
glyph emission and the instance placement, which O1 already optimises. The 8,000-node tree is about
1.15x the smaller per node (cache misses; the scaling case allows 1.5x). Layout is O(nodes): wrap,
grid content tracks, baseline and aspect read each child's measured size once per pass; text is
never re-shaped in layout (a baseline is a pxui_text layout-cache hit); stack order sorts in a kept
scratch vector. Two allocation sources were found and removed on the way: building at the root
(`enter_root`, popups, tooltips, `ui.anchored`) replaced the parent and seed stacks (two allocations
per call), and pxui_text dropped every layout not laid out in the frame, so a view kept for one frame
re-shaped its text on its next rebuild (now kept `KEEP_FRAMES` = 600 frames after last use).
`a11y_warm_zero_alloc` asserts the accessibility walk (every frame, published only when it differs)
allocates nothing on the kit panel.

`window_pxui_view_costs` after layout and accessibility (debug, 2x, alone on the machine; before in
brackets): all ten views kept 9.6 to 10.0 us (6.4 to 8.0), one 50-widget view rebuilt 68 to 71 us
(42 to 48), all 500 widgets rebuilt 490 to 520 us (370 to 490); 0 allocations throughout. Nodes
declared went from 61 / 511 to 86 / 761: a button is now a padded row laid out by alignment holding
the clickable box (it was one node with a placed look and a hit closure). The rest of the rebuilt
frames' growth is the accessibility walk, which covers every view's nodes on a frame that rebuilds any
view; a frame that rebuilds nothing and whose root declares no role skips the walk (without that gate
the all-kept frame was 29 us). Editor `alloc_budget` (RAYS_SLOW=1): empty/basic/bare 443/652/596
allocations per frame (445/652/598 before).

## After P5 widget set and gallery

`widgets_zero_alloc` (`crates/pxui/tests/cases/widgets.rs`): a 480 x 760 frame of every new widget
(menu bar, checkbox, radio, slider, both progress bars, spinner, tabs, segmented, dropdown, icon
button, badge, search, scroll view, image, a 100,000-row list, tree and table, two toasts), 30 warm
frames after 10: 0 allocations rebuilt every frame (225 nodes declared) and 0 kept in a view (5
nodes). `widgets_busy_patches`: 60 steps of the indeterminate bar and the spinner, 0 allocations, no
frame wanted, the view's build skipped (2 nodes declared, the root and the view box). Virtual list,
tree and table over 100,000 items build under 30 rows each. `gpu_widgets_draw_calls`: one draw call
for the one layer. The gallery (`crates/pxui_gallery/tests/gpu_gallery.rs`): the kit page is
`kit_panel_{1,2}x.png` with a largest channel difference of 0 on Metal; run hidden with no input it
built 1 frame in its first half second and 0 in the next two (`gpu_gallery_idle`); a changed theme file
cost one frame.
