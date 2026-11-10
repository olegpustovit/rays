# web_export fixtures

Rust-owned output fixtures for `web_export`, initially captured from the retired OCaml exporter.
`crates/web_export/src/test_web_export.rs` exports each sketch with the current Rust crate and
compares every file byte for byte. Z5 updates the shipped hash/RNG runtime, generated kernel identifiers and
content-addressed module names. Captured numeric/binary data and large-file checksums remain unchanged.

| Path | Content |
|---|---|
| `NAME/` | every file of the export except `runtime/` (`program.js`, `program.map`, `kernels.json`, `kernels.wgsl`, `data.bin`, `index.html`, `serve.json`, `image*.js` / `drawing*.js` modules) |
| `NAME/F.md5` | a file over 1 MiB (`data.bin` of `million_marks`, `flow_particles_gpu`; `program.js` of `recursive_signals`) is held as its MD5 only |
| `_runtime/` | the `runtime/` directory, identical for every sketch (the nine embedded files, `marks.wgsl` printed at build time), plus the build-printed `compact.wgsl` and `dispatch.wgsl` |

Sketches: audio basic drawing file_dialog flow_contours flow_image flow_image_kernel flow_particles_gpu
flow_shader flow_vectors generative million_marks noise particles pxui recursive_rectangles
recursive_signals. The other sketches have no `:context draw` graph (E_WEB_GRAPH) or, for
`flow_survey`, are refused by the exporter (E_GPU_FORM); no sketch uses `image/load`.

## Regenerating

Capture current exports with:

    WEB_EXPORT_DUMP=/tmp/rays-web-export cargo t -p web_export --lib

The dump is written before comparison, so failures leave reviewable outputs. Review each
changed file against the intended behavior before updating its fixture. Files over 1 MiB keep
the existing MD5 checksum representation; runtime files are shared under `_runtime/`.
