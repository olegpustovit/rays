---
name: smoke
description: Run a rays example or sketch for a bounded number of frames and capture its last frame as a PNG. Use to check a rendering, UI, or lifecycle change in the real native app.
---

1. `eval "$(opam env --switch=. --set-switch)"`.
2. One program: `RAYS_MAX_FRAMES=30 dune exec rays -- sketches/<name>.rays`.
   `Sketch` stops after that many frames.
3. Window-free editor screenshot: `dune build tools/ui_shot.exe`, then
   `_build/default/tools/ui_shot.exe sketches/<name>.rays /tmp/rays-shot.png`.
4. Build `tools/check.exe` once, then time
   `_build/default/tools/check.exe @smoke` for the basic and SOP gallery windows.
   `@smoke-all` opens every sketch sequentially for 30 frames. Preserve the
   full output so a failure can be attributed to its program.
