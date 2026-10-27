---
name: prune-dead-code
description: Delete unreferenced code in rays with the compiler-driven codemod (tools/codemod) - exports nothing uses, test-only features, dead Metal bindings and stubs - to a fixpoint, instead of hand-editing. Use after removing a feature or caller, or when auditing a library for dead code.
---

# Prune dead code with tools/codemod

The compiler is the oracle: the tool resolves every identifier in the
`_build` `.cmt` files to its definition through compiler shapes, drops what no
other file references, rebuilds, and deletes whatever the compiler then
flags unused, until nothing changes. Builds use `--profile codemod` (the dev
warnings plus unused modules).

```sh
eval "$(opam env --switch=. --set-switch)"
dune build @check                      # fresh .cmt files first
dune exec tools/codemod/codemod.exe -- dead-exports lib/<name>     # report only
dune exec tools/codemod/codemod.exe -- prune lib/<name> [lib/<other> ...]
```

- `--users-exclude SUBSTR` (repeatable): references from matching files do not
  count, e.g. `--users-exclude lib/metal/test_` finds what only tests use.
- `--modules`: a submodule whose values are all dead goes as a unit (kept, with
  its values dropped, while a type or live value still names it).
- `--cut-tests`: a test that stops compiling is removed (its top-level item, or
  the failing statement or local binding inside `run`/`let () =`; the next
  rounds cut what named that binding). Cutting can leave an assertion whose
  setup is gone: run the tests and repair. A suite whose checks all go through
  a removed operation (the OGPU conformance run) is rewritten by hand.
- Driver records: the compiler does not warn about an unread field of an
  exported record. `codemod.exe -- dead-fields [--users-exclude S] DIR...`
  lists them, `--apply` removes the field and its `field = e` in every
  literal (`--skip TYPE.FIELD` keeps one); then `prune` again. A record listed
  as `type.*` has every field unread: delete the type by hand.
- `codemod.exe -- dead-optionals [--users-exclude S] DIR...` lists optional
  parameters no application passes; `drop-optionals` removes them (the
  signature loses the parameter, the body binds its default: `let x = (d) in`).
  What the constant makes unreachable is then simplified by hand, outermost
  wrapper first; rerun until the list is empty.
- `codemod.exe -- dead-types DIR...` lists exported types that nothing names
  (itself, a constructor or a field, in any interface or implementation);
  `drop-types` removes them from the `.mli` and `prune` takes the rest. Loop
  `prune` and `drop-types` until both are quiet.
- `dead-exports` counts a unit passed to a functor or packed as a first-class
  module as used whole.
- Metal: after pruning `lib/metal`, run
  `codemod.exe -- dead-stubs lib/metal/metal_bridge.mm lib tools test`
  (`dead-stubs --report ...` only lists them) and
  `codemod.exe -- drop-c-unused lib/metal` (clang's unused-function errors as
  the oracle); unused registry entries are dropped during `prune`.

## Rules

- Only prune libraries whose exports are internal. For user-facing APIs
  (`rays`, `pxui`, `sop`, `rays_pathtracer`, `rays_editor`,
  `rays_math`) list with `dead-exports` and decide by hand: unused is not
  unwanted there.
- Unused `let x = e in` locals are deleted; check the printed `26 ...` lines
  when `e` could have a side effect. Unused parameters become `_x` / `~x:_`
  (listed as `27 ...`); remove them with their arguments by hand.
- Records or variants left with only dead fields/constructors are marked
  `[@@warning "-69"]` / `[@@warning "-37"]`: delete those types by hand.
- The loop stops on a hard error and prints it; fix it and rerun. Review the
  diff, run focused aliases through `_build/default/tools/check.exe`, then
  run `_build/default/tools/check.exe --ship` for the shipping check.
