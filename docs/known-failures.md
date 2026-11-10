# Known limitations and outstanding qualification

Record current reproduction commands and results here or in the checked commit.
The migration's remaining performance targets were retired from acceptance on
2026-11-10. Completed Mac and Windows native validation, measured timings and cache
size remain in [development](development.md).

| Item | Reproduce / establish evidence | Current evidence |
| --- | --- | --- |
| Windows fused-multiply-add implementation | Retain the UCRT shim; evaluate any replacement with opaque-input exact rounding, subnormal, overflow-cancellation and nonfinite cases on the pinned Windows GNU toolchain. | The original subnormal bug was OCaml’s emulation. Rust’s separate [MinGW FMA issue](https://github.com/rust-lang/rust/issues/140515) also has a finite-input witness. The checked regression covers it; no shim removal is required for this migration. |
| Legacy editor harness exclusions | Inspect `crates/rays_editor/tests/suite_t2/consistency_b.rs` and `workspace_doc.rs`. | The ported bundle still prints four historical `SKIPPED` metadata notices. Runner skip counts do not establish coverage for those excluded ported cases. |
| Real macOS border-drag responsiveness | Open `cargo run -p rays_bin -- sketches/basic.rays` and drag the native window border while rendering. | Programmatic resize checks cannot prove an AppKit tracking-loop gesture. |

The historical 43-case GPU failure list is superseded by the later passing Metal
runs. New failures need a current command and result, rather than an earlier red count.
