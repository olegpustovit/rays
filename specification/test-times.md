# Test times

The log behind "Test time is a budget" in `AGENTS.md`: one row each time a slow test is found, reworked, gated
or deleted. Newest first. Times are wall seconds on the author's machine, warm cache, dune dev profile.

| Date | Test | Before | After | What changed |
|---|---|---|---|---|
| 2026-10-25 | `test_text_pane` | about 240 s | not reduced | open: time goes to editor cooks (`Flow_ir.walk`, `Context_kernel.term`, about 4 GB); find which cases cook a whole workspace and shrink them or share one cook |
| 2026-10-25 | G3 additions (`@lib/pxui_graph/runtest`, `@lib/flow_graph/runtest`) | 8 s | 8 s | about 10 new checks, none over 1 s |
| 2026-10-25 | `test_workspace_shell` (whole) | about 360 s with every section | 45 s default, rest skipped and counted | Sections run under `RAYS_SUITE_BUDGET`; `RAYS_SLOW=1` or `=900` runs all. Each section still takes 25-30 s: root cause not found, next thing to profile |
| 2026-10-25 | 9 tests in `flow_sop`, `flow_web`, `flow_ir` | minutes together | 0 in the default loop | `enabled_if RAYS_SLOW=1` |
| 2026-10-25 | `run_sop`, `run_diagnostics` (`lib/sop`) | minutes | 0 in the default loop | moved to `qualification-scale` |
| 2026-10-25 | whole `@runtest` | about 20 min | not run for an edit | focused aliases through `tools/check.exe`, 180 s hard timeout |
| 2026-10-25 | graph drag in `ui_shot` (recursive_rectangles) | 27 s for 30 frames | 3.3 s | probes are no longer forced on every frame (`core_scope.ml`) |

Open: find why a `test_workspace_shell` section costs 25-30 s (a per-section evaluation of the footers is the
suspect); `core_census.t` fails on crosses and hatch since before these changes.
