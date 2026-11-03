# Vulkan on Windows, performance pass (P11)

Machine: RTX 2080 Ti (11 GB, 214 MiB device-local host-visible heap), Windows 11, 24 hardware threads.
Command: `rays mem sketches/X.rays --frames 300`, whole-process wall time including setup, no validation.
Footprint is committed private memory. Each row is one commit.

| change | sketch | before | after |
|---|---|---|---|
| Shared buffers are device-local (host memory was read by kernels over PCIe) | isogrid_city | 65.7 s | 13.75 s |
| | gpu_sea | 14.1 s | 13.76 s |
| | million_marks | 1.5 s | 1.31 s |
| | recursive_signals | 0.9 s | 0.71 s |
| | flow_particles_gpu | 31.3 s | 31.45 s |

Why: isogrid_city's `triangle_indices` kernel was 96% of its GPU time (160 ms a frame, 100 frames of 5,500
groups; GPU time 16.7 s of 100 frames). Its inputs are `Shared` buffers, which the Vulkan backend placed in
system memory (VMA host-access, and the device-local host-visible heap is 214 MiB). The kernel gathered over PCIe.
In VRAM it takes 4 ms, GPU time for the 100 frames 0.86 s.

## Mapped views are zeroed by the OS, not by a fill

`create_mapped_buffer` made a host Bigarray and filled it with 0., which touched every page: 3 GB of physical
memory on isogrid_city and 5.6 s of setup. `Vulkan.zeroed_view` is `calloc` (fresh zero pages, untouched until the
host writes). Committed private memory (the `mem` footprint) is unchanged: the view still reserves its size.

| sketch | wall before | wall after | peak working set before | after |
|---|---|---|---|---|
| isogrid_city (300 frames) | 13.75 s | 8.13 s | 3,170 MiB (100 frames) | 429 MiB (100 frames) |
| gpu_sea | 13.76 s | 12.73 s | | 1,197 MiB |
| flow_particles_gpu | 31.45 s | 31.72 s | | |

## Windows pools are at most 8 domains wide

`Parallel.recommended_domains` was `Domain.recommended_domain_count ()`, 24 here, and Domainslib makes a pool per
owning domain, so a stop-the-world minor or major collection waited on 48 participants.
`RAYS_DOMAINS` (a temporary override) 100 frames, whole process:

| sketch | 24 | 12 | 8 | 4 | 1 |
|---|---|---|---|---|---|
| flow_particles_gpu | 12.29 s | 9.38 s | 9.06 s | 9.06 s | 9.67 s |
| gpu_sea | 7.44 s | 6.67 s | 6.47 s | 6.50 s | |
| million_marks | 0.88 s | 0.79 s | 0.80 s | 0.79 s | |
| isogrid_city | 2.75 s | 2.64 s | 2.68 s | 2.65 s | |

Applied (Windows only, 8): 300 frames flow_particles_gpu 31.72 s to 27.25 s, gpu_sea 12.73 s to 11.80 s.

## Open

- Committed memory (`mem` footprint, 5.7 GB on isogrid_city, 3.0 GB on gpu_sea) is twice the device bytes: each mapped
  buffer has a full-size host view that is reserved, though now untouched (resident 0.4 GB). Cutting it needs a lazy
  `driver_mapped.view` (an OGPU interface change: a view created at first host access).
- flow_particles_gpu (27.3 s against the Mac's 22 s) is CPU-bound, not GPU-bound: GPU 0.53 s for 100 frames. The main
  thread is in `Packed.bounds` (six `Float.min`/`max` calls, each a C `caml_signbit` call, per point on 1M points
  a frame), the 12 MB a frame readback, and `rays mem`'s own `Obj.reachable_words`.
- `Unix.gettimeofday` on this machine steps by about 0.9 ms (577 distinct values in 0.5 s) and `Sys.time` by 15.6 ms:
  Measured placement quantises a sub-millisecond cook to 0 or 1 ms. No placement was decided on one in these sketches.
- Barriers (`full_barrier` 6,000 for 100 isogrid frames), pushed descriptors and the per-call Hashtbl lookups are not
  where a frame goes: GPU time is 0.86 s of 100 frames' 2.7 s, and the CPU wait on it is the same. Left unchanged.

## Minor heap per domain, Windows (vulkan-lat)

A stop-the-world leader (runtime events) took 1.9 ms per stop at 8 domains and 10 to 16 ms at 24 on this machine. The
256k-word default minor heap made `bench_isogrid --flips 24 64` run 16838 minor collections at 24 domains. An `OCAMLRUNPARAM s=` sweep at 24 domains gave evaluate p95 175 ms at 1M words, 156 at 2M, 141 at 4M, 140 at 8M and 156 at 32M. Each domain now sets its own heap
once (`Gc.set` sizes only the calling domain): 64 MB on the main domain, 8 MB on a worker. Windows only.

| `bench_isogrid --flips 24 64`, evaluate | minor collections | p50 | p95 | max |
|---|---|---|---|---|
| 24 domains, before | 16838 | 52 ms | 218 ms | 279 ms |
| 24 domains, after | 1080 | 39 ms | 132 ms | 150 ms |
| 8 domains (the cap), before | n/a | 44 ms | 208 ms | 213 ms |
| 8 domains (the cap), after | 986 | 36 ms | 133 ms | 150 ms |

`rays mem` footprint of flow_particles_gpu at 8 domains: 1.01 GB to 1.33 GB.

## All 24 domains on Windows (vulkan-lat)

The `min n 8` cap is gone, at the owner's request. With the per-domain minor heap above, 100 frames, whole process:

| workload | 8 | 16 | 24 |
|---|---|---|---|
| flow_particles_gpu | 9.32 s | 9.91 s | 11.25 s |
| gpu_sea | 6.63 s | 6.90 s | 7.38 s |
| isogrid_city | 2.67 s | 2.62 s | 2.69 s |
| bench_isogrid evaluate p50 / p95 / max | 36 / 133 / 150 ms | (12: 36 / 135 / 145) | 39 / 132 / 150 ms |

flow_particles_gpu and gpu_sea are slower at 24 than at 8 (+21 % and +11 %): their parallel regions are 101 bursts and
0.2 s of 9 s, and what grows with the domain count is the stop-the-world cost (357 stops, leader 1.0 s at 8 domains, 5.1 s
at 24), not the work. Not capped silently: the decision to restore a cap is the owner's.

## Recipe 0's evaluate (vulkan-par)

Recipe 0 (Labyrinth) is serial by construction: `maze-links` is an Aldous-Broder walk, an `iterate` of 368 passes whose
body maps 6, 64 and 384 elements, each under the 4,096-element walker head, so the walker evaluates them and nothing
spreads. Its evaluate at side 24 was 120 to 150 ms at every domain count. Probe: `Flow_sop.Lower.of_checked` of the
recipe-0 workspace, 30 runs, p50 (`lower` without a device, `gpu` with the editor's fast device config).

| change | measure | before | after |
|---|---|---|---|
| Walker: a binding's record path is built only when recording, one-field reads build no dotted name, an operator's declared types and positional names are read in place | probe `lower` p50 / allocation | 73 ms / 251 MB | 71 ms / 236 MB |
| Qualification lowers an observed map again only when it can change the producer's outcome: a settled refusal stands, and an accepted lowering is kept by the residual's view (captures by value, a map's tuple left out: it only names sites). 501 lowerings to 196 on recipe 0 with the fast device config | probe `gpu` p50 / allocation | 80 ms / 275 MB | 71 ms / 236 MB |
| | `bench_isogrid --flips 24 64`, recipe 0 evaluate (8 flips) | 118 to 161 ms | 105 to 120 ms |
| | evaluate p95 / max, all recipes | 136 / 161 ms | 114 / 120 ms |

## One worker pool for every domain (vulkan-par)

Domainslib made a pool per owning domain: the editor's 24-wide pool and its cook domain's 23-wide one ran 47 domains on
24 hardware threads (runtime events: domain ids up to 47, main stop-the-world leader 5.2 ms a stop on average, 1046
stops in 128 isogrid flips). `Parallel` now owns one process-wide set of `recommended_domains () - 1` workers that every
domain's jobs share; jobs are cut into about 16 chunks per participant, the caller wakes one worker and each joining
worker the next, idle workers poll briefly and then park. `rays mem --frames 100`, whole process, two rounds (the second
had another workload on the machine, about 50 % CPU, so its walls are high for both builds):

| sketch | domains | before | after | footprint before | after |
|---|---|---|---|---|---|
| flow_particles_gpu | 8 | 9.43 / 9.18 s | (first run 13.29) / 9.04 s | 1.38 GB | 1.26 GB |
| | 16 | 12.04 / 11.09 s | 8.99 / 10.06 s | 1.51 GB | 1.35 GB |
| | 24 | 12.43 / 12.69 s | 9.53 / 11.29 s | 1.66 GB | 1.47 GB |
| gpu_sea | 8 | 7.30 / 7.07 s | 7.55 / 7.57 s | 3.36 GB | 3.33 GB |
| | 24 | 8.51 / 8.38 s | 8.09 / 9.12 s | 3.39 GB | 3.58 GB |
| isogrid_city | 8 | 2.74 / 3.49 s | 3.55 / 3.79 s | 6.68 GB | 6.43 GB |
| | 24 | 2.70 / 3.39 s | 3.06 / 3.74 s | 6.77 GB | 6.67 GB |

Earlier quiet runs of the same builds: flow_particles_gpu at 24 domains 11.8 to 12.1 s before, 9.2 to 10.1 s after; at 8
domains 9.0 to 9.6 s before, 8.3 to 8.7 s after; gpu_sea at 24 7.43 s before, 7.41 s after (8: 7.25 s). A stop with 23
parked workers costs about 0.15 ms (`Gc.minor` in a loop), so parked domains are no longer what a stop pays for; the
particles sketch is still about 10 % slower at 24 than at 8. Capping each job at 8 participants with 24 workers recovers
half of that gap. Neither a 1 ms Windows timer resolution (`timeBeginPeriod`; the runtime's spin back-off calls `Sleep`)
nor 32 MB worker minor heaps changed the walls, so neither is applied.
