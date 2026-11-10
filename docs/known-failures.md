# Known failures and outstanding qualification

This is the live list of unresolved evidence. Remove an item only after running
its command and recording the result in `PROGRESS.md`. Dated checkpoint failures
that were subsequently fixed do not belong here.

| Item | Reproduce / establish evidence | Current evidence |
| --- | --- | --- |
| Debug geometry lowering versus the archived native thresholds | `ONLY=box,transform,sphere,garden PHASES=1 cargo t -p flow_sop debug_lowering_beats_archived_native` | The last recorded Mac gate fails transform/garden against stored Windows thresholds. It is not a same-machine comparison. |
| Debug sketch-check deadlines | `cargo run -q -p buildtime -- profile` | M1 cached app build 0.121 s and touch rebuild 4.011 s; corpus hard-stopped at 15.019 s before all 49 sketches completed. `sop_gallery`, the three swarm sketches and `ws_rosette` exceeded their 1 s deadlines. Full direct checks measured swarm 2.915 s and gallery 1.510 s. See [development](development.md) for the recorded profile. |
| Target cache size | `cargo run -q -p buildtime -- profile`; inspect the target-size row after `cargo run -p buildtime -- gc` | Current target size is 75.537 GB against the 15 GB limit. Retaining the shared warm cache does not establish the requested size budget. |
| Same-machine native performance comparison | Follow the detached reference-worktree procedure in [development](development.md), then run the same corpus and fixed-frame native command in both checkouts. | A fresh paired Mac/Windows comparison has not been recorded. Old cross-machine timings do not prove a win. |
| Windows native/profile qualification | Run `cargo t --all` and the profile measurement procedure in [development](development.md) on the Vulkan Windows host. | This Mac checkout cannot establish the second machine's measurements. |
| Real macOS border-drag responsiveness | Open `cargo run -p rays_bin -- sketches/basic.rays` and drag the native window border while rendering. | Programmatic resize checks cannot prove an AppKit tracking-loop gesture. |

The historical 43-case GPU failure list is superseded by the later 83-case passing
Metal run in [the checkpoint](history/CHECKPOINT.md). New failures must include a
current command and result here, rather than inheriting an earlier red count.
