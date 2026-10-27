---
name: focused-test
description: Run only the tests for the code you touched in rays. Use after editing a file under lib/, tools/ or test/ when you want a fast pass/fail instead of the full runtest.
---

1. `eval "$(opam env --switch=. --set-switch)"`.
2. Map the edited path to its directory: `lib/<name>/…` → `lib/<name>`; a
   file under `test/` → `test`.
3. Build `tools/check.exe` once with `dune build tools/check.exe`.
4. Time `_build/default/tools/check.exe @check @<dir>/runtest`.
   Keep the full output and fix every reported failure. Focused runs have a
   180-second timeout; a timeout is a defect to fix, not a limit to raise.
5. A green default `runtest` is the baseline, so any failure here is yours.
   Tests under `@runtest-native` / `@qualification` require the supported
   display, device or SDK; run the lanes affected by your change.
6. Review intended `.mli` changes directly; Lisp is the contract. Finish with
   `git diff --check`. Reserve `check.exe --ship` for the shipping check.
