# Web printer fixtures (flow_web)

Rust-owned printer fixtures for `crates/flow_web`, initially captured from the retired OCaml
printers. Z5 replaces generated kernel identifiers with the shared two-lane word hash; each
former MD5 identifier was verified against its source before changing only the identifier.
Shader bodies and the documented register-order differences remain unchanged.

| Directory | Printer | Cases |
|---|---|---|
| `wgsl/` | `Wgsl.kernel` (f32 kernel) | the 40 `programs` and `carried` cases of `lib/flow_web/test_js.ml` |
| `wgsl_fast/` | `Wgsl.kernel_fast` | `f_<name>`: the 14 `fast_cases` of `test_wgsl.ml`; `c4096_<name>`: `Test_program.carried_fixtures 4096` |
| `wgsl_error/` | `Wgsl.kernel_fast` | the 15 `error_cases` of `test_wgsl.ml` |
| `js_region/`, `js_region_fast/` | `Js.region` (binary64 / fast precision) | all of the above; a fast region the printer cannot print (OCaml raises `Invalid_argument "fast integer register"`) is stored as `EXN Invalid_argument fast integer register` |

File format: WGSL files are `entry-name`, a newline, then the source (or `ERROR <code> <message>` when the
printer refuses); JS files are the `region` source. File names are the case name with every character
outside `[A-Za-z0-9-]` replaced by `_`.

The frame-level goldens (`frame_*.js`, 93 files) and the four `.wgsl`/`.js` goldens of the basic
fixtures are not copied: the Rust tests read them in place from `crates/flow_web/goldens/`.

## Regenerating

Use the current Rust `wgsl::kernel`, `wgsl::kernel_fast` and `js::region` printers. Run
`cargo t -p flow_web test_oracle`; differing actual texts are written to the system temporary
directory as `flow_web_actual_*`. Review each intended difference before updating its fixture.
`oracle/dump.ml.txt` and `oracle/dune.txt` retain the original capture evidence.

The Rust side is `crates/flow_web/src/test_oracle.rs`. It lists the few cases that differ today because
`flow_ir` hands the printers a different register program (OCaml evaluates the branches of `if` right to
left, so text constants are numbered the other way round) (the narrowing differences found earlier have since been fixed in flow_ir); a
test fails if a listed case starts to match or an unlisted one differs.
