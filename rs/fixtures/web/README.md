# Web printer fixtures (flow_web)

Oracle texts for `rs/crates/flow_web`, printed by the OCaml `Flow_web` printers (`lib/flow_web`) at
the time the Rust port was written. Nothing here is hand-edited.

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
fixtures are not copied: the Rust tests read them in place from `lib/flow_web/goldens/`.

## Regenerating

`oracle/dump.ml.txt` is the throwaway OCaml dumper (`oracle/dune.txt` its dune stanza). To regenerate: copy
both into a temporary directory at the repository root as `dump.ml` and `dune`, build and run it with the
Windows environment wrapper, then delete the directory:

    rw 'cd /c/Users/snowbear/projects/rays && dune build ./scratch_flowweb_dump/dump.exe && ./_build/default/scratch_flowweb_dump/dump.exe rs/fixtures/web'

The Rust side is `crates/flow_web/src/test_oracle.rs`. It lists the few cases that differ today because
`flow_ir` hands the printers a different register program (OCaml evaluates the branches of `if` right to
left, so text constants are numbered the other way round) (the narrowing differences found earlier have since been fixed in flow_ir); a
test fails if a listed case starts to match or an unlisted one differs.
