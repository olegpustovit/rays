# Source Atlas

```sh
tools/source_atlas_index
cargo run -p rays_bin -- sketches/source_atlas.rays
```

The indexer requires `rust-analyzer` on PATH. It uses `documentSymbol` without loading/building the
Cargo workspace. Both Rays and the PXUI submodule are included; generated output, historical copies,
fixtures, vendor directories and symlinks are excluded. Non-Rust files have file rows and no invented
symbols. Real data and content-digest caches live in `_out/source_atlas/`, outside Git.

Wheel/pinch zooms at the pointer, drag pans, click dives, right click zooms out, R/Home resets;
`=`/`-` also zoom. Hover shows the real path, line range, scope name and first source line.
The Lisp owns weighted quadrant descent, nested symbol placement, camera state, picking, fades,
containment paths, 24 label slots and 16 stipple samples per row. The indexer supplies sibling
addressing and integer prefix weights, never coordinates. Non-Rust tiles stay hollow.

`table/load` reads a typed UTF-8 TSV relative to the workspace file: `name:int`, `name:float`, or
`name:text` headers. It checks ragged rows, duplicate/invalid headers, bad numbers and bad text escapes
(`\\`, `\t`, `\n`, `\r`). Numeric columns are packed arrays; text columns work with `array/nth` and
`array/count`. Reload the workspace after regenerating its table. Web export reports `E_WEB_TABLE`.

`tree/` contains 40 tiny source files across Rays and the three PXUI tiers. `atlas.tsv` is their real
rust-analyzer output; `overview.png` and `zoom.png` are native GPU fixture views at zoom 1 and 8.

```sh
tools/check-source-atlas
cargo t -p flow_check typed_tsv_and_diagnostics
cargo t -p rays_bin table_load_is_relative
cargo t -p flow_eval mark_colour_arrays
cargo t -p flow_native source_atlas
cargo t -p flow_gpu closed_runner_preserves_pending_status
RAYS_SLOW=1 cargo t -p flow_native source_atlas_full_table_budget
```

The fixture regression checks every row once, positive finite cells, sibling separation, child
containment, picking every row, pointer anchored wheel zoom, click/back/reset, web refusal, two
rendered views, zero warm payload readback and zero warm GPU buffer creation. Set
`RAYS_UPDATE_FIXTURES=1` only when intentionally refreshing goldens.

Measure the real device path with `rays mem sketches/source_atlas.rays --frames 301`;
`rays render` uses the CPU reference evaluator. The static layout stays on the CPU for exact picking;
per-frame mark arrays run as GPU kernels and hidden marks have zero size. Picking reruns only when
the pointer/camera changes; label ranking reruns only when the camera changes. No per-frame filter
feeds the drawing. The optional bulk-text, clip, alpha, churn and dependency overlays from the plan
are not needed by this sketch.

Apple M1 qualification at 1280×900 measured 4.2 ms idle and 8.4 ms with changing pointer/zoom
(32 completed native frames each), with zero warm payload readback and GPU buffer creation.
The full-table check enforces 16.7 ms/frame; timings depend on the machine and concurrent load.
