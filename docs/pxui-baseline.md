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

## After the P5 performance pass

`PXUI_BENCH=1 RAYS_SLOW=1 cargo t [--release] -p pxui pxui_bench` (`crates/pxui/tests/cases/bench.rs`)
prints min and median of 7 rounds (`Ui::frame` only, warm) and, with `Ui::set_timing`, the median
round's phases; `PXUI_NO_TIMING=1` measures without the phase clocks (about 0.15 us a frame),
`PXUI_PROFILE=<case>` loops one case 8 s for `sample <pid>`. M1, us per frame, min / median. Dev is
the `dev` profile (opt-level 1); release is `cargo t --release` (opt-level 3, thin LTO, 16 codegen
units), the truly optimised build. Before = commit `ffc9d7d5` (phase clocks always on).

| Frame | Dev before | Dev after | Release before | Release after |
| --- | --- | --- | --- | --- |
| Kit baseline (panel, 4 widgets, 2x) | 5.7 / 5.8 | 3.4 / 3.6 | 5.2 / 5.2 | 3.3 / 3.4 |
| 500 widgets, all ten views kept | 10.1 / 10.1 | 3.9 / 3.9 | 8.9 / 9.0 | 3.4 / 3.4 |
| 500 widgets, one 50-widget view rebuilt | 67.8 / 67.9 | 30.9 / 31.0 | 60.1 / 61.9 | 29.4 / 30.1 |
| 500 widgets, same, accessibility on | 67.8 / 67.9 (always on) | 43.2 / 43.3 | 60.1 / 61.9 | 40.1 / 41.3 |
| 500 widgets, all rebuilt | 487 / 491 | 281 / 281 | 426 / 429 | 259 / 272 |
| 2,006-node stress tree, full rebuild | 1154 / 1165 (575 ns per node) | 740 / 741 (369) | 1019 / 1024 (508) | 693 / 699 (346) |
| Stress tree cached | 15.5 / 15.6 | 1.0 / 1.1 | 13.7 / 13.7 | 0.9 / 0.9 |
| Stress cached + one 42-node view rebuilt | 45.5 / 46.5 | 15.4 / 15.4 | 39.1 / 39.1 | 14.5 / 14.6 |
| Allocations, any of the above, warm | 0 | 0 | 0 | 0 |

Full-rebuild phases of the stress tree (median round, us; dev before -> after, release after): begin
2.9 -> 2.9 (2.9); build 117 -> 94 (83); intrinsic 62 -> 30 (29); arrange 178 -> 136 (115); paint 785
-> 471 (468); a11y 5.8 -> 0 (0, off); finish 13.8 -> 13.0 (11.2). Layout alone 120 -> 83 ns per node
(release 86 -> 72). Text has no phase of its own: `sample` puts glyph emission (`Paint::text_styled`
with `pxui_text::Text::glyphs`, cosmic-text's run iteration and `LayoutGlyph::physical`, and instance
construction) at about 41% of a full rebuild, the text layout-cache lookup (`lay_out`) at 7%.

What bought it, in order: the accessibility tree only while `set_a11y(true)` (a one-view rebuild
walked every view's nodes); pxui_text's layout and glyph caches and the a11y index on a multiply-
rotate hasher (SipHash of glyph cache keys was 19% of a full rebuild); a cached frame walks no node
(prune frees only from the rebuilt views' last builds, moving scrolls are listed, the text-layout
sweep runs every 64 frames: it was 14 us of the 15.5); hit testing's view check without an
out-of-line slice compare and the phase clocks behind `set_timing`; `Rgba` channels as bytes (Look
408 -> 144 B, Node 720 -> 424 B, copied per node); repeated labels resolving their duplicate keys in
one step (25 "LABEL"s probed the table 325 times); layout reading node fields in place instead of
copying the Node per child; a frame with input events copying them into kept storage (2 to 3
allocations per input frame).

Why release was no faster than debug: it was a real optimised build (the `cargo t` runner passes
`--release` through), but the frame was dominated by code whose speed does not depend on our
opt-level: std's SipHash (precompiled, optimised in both) on every glyph cache key and text key,
`memmove` of 720-byte nodes, and per-node scans over large slot records (cache misses). After the pass
release is 6 to 10% faster than dev; what remains is mostly per-glyph emission, memory-bound walks of
cosmic-text's layout runs.

Asserted budgets (all load-independent): `window_pxui_budget_counts` (a kept frame of ten views
declares and lays out 11 nodes, a one-view rebuild 86; the a11y tree stays empty with revision 0 until
asked for; one-view rebuilds with it on allocate nothing), `window_pxui_budget_cached_per_view` (20 kept
views of 200 nodes within 1.5x of 20 of 10 nodes; 6.3 us both), `window_pxui_budget_zero_alloc` (a
virtual list scrolled a row a frame; hover in and out of a kept view's button with its transition),
`window_pxui_node_duplicate_keys`; with the earlier `layout_stress_zero_alloc` (warm full rebuild,
cached, one-view rebuild), `window_pxui_view_costs`, `styling_transition_without_rebuild` and
`gpu_retained_bytes` (a cached frame writes 0 instance bytes, a one-instance patch 64 per frame copy).

Not fixed: glyph emission (above), which repeats per glyph each rebuild; caching a text node's glyph
quads by origin was tried and dropped, since the same label at many origins (the common case) missed
and was slower. With accessibility on, a frame that rebuilds any view still walks every view's nodes
(12 us for 761 nodes): per-view spans in the tree when trees get large. The host's `paint_layer` does
per-frame work in views (view table) and dirty ranges, not in instances.
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
