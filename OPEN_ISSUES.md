# Open issues

Left over from the GPU-residency work on draw graphs (commits `4940b7338` through `825bdf7b0` on
`dev`). Numbers come from `RAYS_SLOW=1 cargo t -p flow_native full_table_budget`, run against the
real `_out/source_atlas` tables at 1440×1000 on Metal.

## Atlas still copies 768 B back per moving frame

The 96 file-label slots in `sketches/source_atlas.rays` (`labels`, around line 411) each read
`(array/nth owners slot)` twice on the host, because the label text needs the owner there:
96 × 2 × 4 B. A still frame reads nothing back.

- **Cheap fix:** read each slot's owner once. That halves it to 384 B.
- **Real fix:** read the label slots in one batched read of the owners array, or look the label up
  on the device, so the host only receives text.

## Drawings with a resident loop store integers as 32-bit floats

Loop-free drawings keep integer arrays exact as signed 32-bit words (`Resident::exact`). Drawings
with a resident fold loop keep the float scope, so an integer array in them is a float32. Since
`825bdf7b0`, a value past 2^24 there is a typed error (`E_GPU_FLOAT_RANGE`) rather than silently
rounded. It is still not exact.

- **Blocker:** `crates/flow_gpu/src/host.rs` `compact()` refuses a float mask in the uint32 scope
  (`if self.uint32 && !p.uint32`). Putting loop drawings on the exact scope broke
  `frame_alloc_recursive_signals`: 340 buffers and 11 MB per frame.
- **Fix:** let the uint32 scope compact by a float mask (convert the mask in the kernel), then
  choose the exact scope for every drawing.
- **Watch for:** the float32 range check applies to every float32 integer output, so an existing
  drawing that held integers past 2^24 now errors where it used to round.

## Some fused regions still go through the CPU

A fused sibling group is kept resident only when its members already read kept arrays. Fused
groups over arrays that are not kept still take the old host-forced path (readback, then upload).
The tails loop in `crates/flow_ir/src/core_exec.rs` `prepare` skips every fused region.

- **Design:** allow tails for at-first, non-mark fused groups, and have the reuse wrapper clear
  `fused_done` for the members it skipped.
- **Missing first:** a test that actually shows the effect. A trial of this design compiled and
  passed, but no test showed it doing anything, so it was reverted.

## Netwerk uploads 49–55 KB and dispatches 3 kernels per frame, untraced

Even in the budget test's still phase, `source_netwerk` uploads 6–8 buffers (48,756 B still,
54,699 B moving) and runs 3 resident dispatches per frame. Atlas and Ledger upload 0 B. Not
traced yet. Suspects:

- per-frame host tables under the 65,536-element adoption cache threshold
  (`crates/flow_gpu/src/host.rs` `adopt`);
- the camera still easing during the "still" frames;
- the deep-lattice `origin`/`index` arrays.

## Integer storage is only verified on Metal

Signed 32-bit word storage (`signed_words` in `crates/flow_shader/src/emit.rs` and `spirv.rs`,
`pack_word` in `crates/flow_gpu/src/run.rs`) is implemented for SPIR-V but has only run on
macOS/Metal. Run the suites on the Windows/Vulkan box and on WSL/lavapipe
(`pxui/tools/validate.sh --windows` and `--linux` print the commands).

## Frame budget

`source_atlas_full_table_budget` (60 Hz, 16.7 ms) still fails, and it already failed before this
work.

| Sketch | ms per frame (still / moving) | Over budget |
|---|---|---|
| Netwerk | 53.0 / 51.3 | yes |
| Atlas | 14.5 / 16.7 | sometimes |
| Ledger | about 4.6 | no |

Netwerk's cost breakdown:

- about 20 ms for the old sheet (grid, frames, branches, labels);
- about 17 ms for the atlas stipple (nodes, coarse level, three slot levels);
- about 12 ms for the deep levels (about 860k marks in 32-point blocks).

**First frame:** about 1 s of kernel compile, about 0.16 s per stipple level. A merged single-kernel
stipple compiled slower (2.6 s), so the five levels stay separate calls.

## A map reused while its inputs repeat can't be cached by the host memo

Resident results are held while their inputs repeat (`reuse_device` in
`crates/flow_ir/src/core_exec.rs`). The host loop memo (`remember`) still skips any statement that
reads or writes a kept array. Anything new kept on the device has to go through `reuse_device`, or
it reruns every frame.

## Older and unrelated

- `flow_check` `declarations_match_owned_schema` fails against `golden/ops.txt` on `dev`.
- `cargo t -p rays_editor` exceeds the 60 s whole-run limit, with no failing case.
- Netwerk's margin ticks (`tick_at`, `tick_size`) are interpreted on the CPU: "a capture still reads
  a state after binding". The netwerk fixture test excludes them.
- `tools/source_atlas_index` still writes `lines.tsv` and `chars.tsv` (about 450k rows), which no
  sketch reads any more.
