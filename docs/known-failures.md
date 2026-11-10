# Known failures and outstanding qualification

This is the live list of unresolved evidence. Remove an item only after running
its command and recording the result in `PROGRESS.md`. Dated checkpoint failures
that were subsequently fixed do not belong here.

| Item | Reproduce / establish evidence | Current evidence |
| --- | --- | --- |
| SDL Cocoa display unavailable | `cargo t -p sdl3 cocoa_layer_ownership` | Fails with `No available video device`; 13.263 s including compilation, case 0.053 s, one counted skip. Offscreen GPU checks do not establish display availability. |
| Debug geometry lowering versus the archived native thresholds | `ONLY=box,transform,sphere,garden PHASES=1 cargo t -p flow_sop debug_lowering_beats_archived_native` | The last recorded Mac gate fails transform/garden against stored Windows thresholds. It is not a same-machine comparison. |
| Same-machine native performance comparison | Follow the detached reference-worktree procedure in [development](development.md), then run the same corpus and fixed-frame native command in both checkouts. | A fresh paired Mac/Windows comparison has not been recorded. Old cross-machine timings do not prove a win. |
| Windows native/profile qualification | Run `cargo t --all` and the profile measurement procedure in [development](development.md) on the Vulkan Windows host. | This Mac checkout cannot establish the second machine's measurements. |
| Real macOS border-drag responsiveness | Open `cargo run -p rays_bin -- sketches/basic.rays` and drag the native window border while rendering. | Programmatic resize checks cannot prove an AppKit tracking-loop gesture. |

The historical 43-case GPU failure list is superseded by the later 83-case passing
Metal run in [the checkpoint](history/CHECKPOINT.md). New failures must include a
current command and result here, rather than inheriting an earlier red count.
