# Test times

The log behind "Test time is a budget" in `AGENTS.md`: one row each time a slow test is found, reworked, gated
or deleted. Newest first. Times are wall seconds on the author's machine, warm cache, dune dev profile.

| Date | Test | Before | After | What changed |
|---|---|---|---|---|
| 2026-10-25 | `test_text_pane` | about 240 s (editor_text 124, editor_w9 70, editor_binding 40) | about 37 s in the test binary | The sunflower and rosette fixtures cooked 240 seeds / 12 petals on every edit: each edit re-lowers the workspace and the cost follows the number of `sop/uv_sphere` calls (about 0.07 s a seed, 16 s a cook at 240), not the text pane. The tests now use `small_sunflower` (8 seeds) and `small_rosette` (2 petals, 1 inner), same checks. What is left is a fixed 1.8 s a cook in `editor_w9` (7 scenarios, 22 s); under 30 s needs a faster workspace lowering (`Flow_ir.walk`, `Context_kernel.map`), not a smaller fixture |
| 2026-10-25 | G3 additions (`@lib/pxui_graph/runtest`, `@lib/flow_graph/runtest`) | 8 s | 8 s | about 10 new checks, none over 1 s |
| 2026-10-25 | `test_workspace_shell` (whole) | about 360 s with every section | 45 s default, rest skipped and counted | Sections run under `RAYS_SUITE_BUDGET`; `RAYS_SLOW=1` or `=900` runs all. Each section still takes 25-30 s: root cause not found, next thing to profile |
| 2026-10-25 | 9 tests in `flow_sop`, `flow_web`, `flow_ir` | minutes together | 0 in the default loop | `enabled_if RAYS_SLOW=1` |
| 2026-10-25 | `run_sop`, `run_diagnostics` (`lib/sop`) | minutes | 0 in the default loop | moved to `qualification-scale` |
| 2026-10-25 | whole `@runtest` | about 20 min | not run for an edit | focused aliases through `tools/check.exe`, 180 s hard timeout |
| 2026-10-25 | graph drag in `ui_shot` (recursive_rectangles) | 27 s for 30 frames | 3.3 s | probes are no longer forced on every frame (`core_scope.ml`) |

Open: find why a `test_workspace_shell` section costs 25-30 s (a per-section evaluation of the footers is the
suspect); `core_census.t` fails on crosses and hatch since before these changes.
