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
