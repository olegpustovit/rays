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
