# Known failures and outstanding qualification

This is the live list of unresolved evidence. Remove an item only after running
its command and recording the result in `PROGRESS.md`. Dated checkpoint failures
that were subsequently fixed do not belong here.

| Item | Reproduce / establish evidence | Current evidence |
| --- | --- | --- |
| SOP exact-case target | `cargo t -p flow_sop` | All 499 cases pass without skips in 29.451 s, meeting the 30 s suite target. Thirty of the 98 exact cases still exceed 0.5 s; the slowest is dense-curve subdivision at 0.989 s. |
| Debug geometry lowering versus the archived native thresholds | `ONLY=box,transform,sphere,garden PHASES=1 cargo t -p flow_sop debug_lowering_beats_archived_native` | The last recorded Mac gate fails transform/garden against stored Windows thresholds. It is not a same-machine comparison. |
| Debug sketch-check deadlines | `cargo run -q -p buildtime -- profile` | M1 cached app build 0.131 s and touch rebuild 4.526 s. All 49 checks were attempted in 14.612 s, but gallery and swarm were killed at their 1 s deadlines; 47 completed. Complete direct medians are swarm 1.543 s and gallery 1.405 s. See [development](development.md). |
| Target cache size | `cargo run -p buildtime -- gc` | Pruning 2,101 superseded completed incremental sessions reclaimed 21.245 GB. The retained target is still 53.211 GB allocated / 53.782 GB logical against the 15 GB limit. |
| Same-machine native performance comparison | Follow the detached reference-worktree procedure in [development](development.md), then run the same corpus and fixed-frame native command in both checkouts. | Fresh Mac dev-runtime pairs are recorded: Rust is 9.5% slower on the identical 20-sketch check corpus and 6.6% faster on basic’s 30 fixed native frames. An overall win is not established; release, paired cold-build and Windows comparisons remain open. |
| Windows native/profile qualification | Run `cargo t --all` and the profile measurement procedure in [development](development.md) on the Vulkan Windows host. | This Mac checkout cannot establish the second machine's measurements. |
| Windows fused-multiply-add shim removal | `cargo t -p rays_math` on the pinned Windows GNU toolchain, compare opaque-input `f64::mul_add` with UCRT, then sequential/parallel qualification | The original subnormal bug was OCaml’s emulation. Rust’s separate [MinGW FMA issue](https://github.com/rust-lang/rust/issues/140515) remains open in the pinned source; finite rounding and overflow cancellation also need coverage. The UCRT shim remains until Windows runtime/link evidence proves replacement safe. Mac tests cannot establish this. |
| Real macOS border-drag responsiveness | Open `cargo run -p rays_bin -- sketches/basic.rays` and drag the native window border while rendering. | Programmatic resize checks cannot prove an AppKit tracking-loop gesture. |

The historical 43-case GPU failure list is superseded by the later 83-case passing
Metal run in [the checkpoint](history/CHECKPOINT.md). New failures must include a
current command and result here, rather than inheriting an earlier red count.
