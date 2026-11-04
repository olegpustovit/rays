#!/usr/bin/env bash
# PreToolUse (Bash|PowerShell): Rust tests run only through the timed runner `cargo t`
# (5 s per test, 60 s per run after the build). Plain `cargo test`/`nextest` and test
# executables run straight from target/.../deps have no timeout, so they are denied.
# Only tool_input.command is checked (not the description); git commands (commit
# messages) and the hook's own self-test (NO_RAW_TESTS_SELFTEST) are skipped.
cmd=$(perl -MJSON::PP -0777 -ne 'my $j = eval { decode_json($_) } // {}; print(($j->{tool_input} // {})->{command} // "")')
deny() {
  printf '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"%s"}}\n' "$1"
  exit 0
}
case "$cmd" in *NO_RAW_TESTS_SELFTEST*) exit 0 ;; esac
if printf '%s' "$cmd" | grep -Eq '^[[:space:]]*(cd [^;&]*(;|&&)[[:space:]]*)?git[[:space:]]'; then exit 0; fi
if printf '%s' "$cmd" | grep -Eq 'cargo(\.exe)?[^a-zA-Z0-9_|;&]*[[:space:]]+(\+[^[:space:]]+[[:space:]]+)?(test|nextest)([^a-zA-Z0-9_-]|$)'; then
  if ! printf '%s' "$cmd" | grep -Eq -- '--no-run|-p[[:space:]]+(rays-t|gate)([^a-zA-Z0-9_-]|$)'; then
    deny 'Use `cargo t -p <crate> [filter]` (5 s per-test / 60 s per-run timeouts). Plain cargo test has no timeout. RAYS_SLOW=1 for slow tests, once.'
  fi
fi
if printf '%s' "$cmd" | grep -Eq 'target[\\/]+([^"[:space:]]*[\\/]+)?deps[\\/]+[A-Za-z0-9_]+-[0-9a-f]{8,}' && ! printf '%s' "$cmd" | grep -q 'gdb'; then
  deny 'Do not run test executables from target/.../deps directly (no timeout). Use `cargo t -p <crate> [filter]`; attach gdb only to reproduce a hang.'
fi
exit 0
