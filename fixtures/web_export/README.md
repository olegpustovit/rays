# web_export fixtures

Output of the OCaml `rays web sketches/NAME.rays -o DIR` (`bin/rays.exe`) for the sketches that export,
read by `rs/crates/web_export/src/test_web_export.rs`, which exports each sketch with the Rust crate and
compares every file byte for byte.

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

Build the OCaml binary if stale, then from the repository root:

    ~/.local/bin/rw 'dune build bin/rays.exe'
    ~/.local/bin/rw 'sh rs/fixtures/web_export/capture.sh'
