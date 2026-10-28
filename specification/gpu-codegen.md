# GPU code generation from Rays Lisp

A shader printed from Rays Lisp must be as fast as a handwritten one. A target
that is 7x slower (`pathtrace.rays` against `pathtrace.metal`,
[benchmarks/pathtracer-lisp.md](cleanup/benchmarks/pathtracer-lisp.md)) can't
be shipped. This note says why that happens today and how the emitters must work.

## Why the generated kernel is slow

`lib/flow_gpu/emit.ml` prints MSL from `Packed`, the **CPU block IR**. Packed
is designed for 1,024-lane blocks on a CPU, and each of its choices is right
there and wrong on a GPU thread:

| Packed (CPU block) choice | What it prints in MSL | GPU cost |
|---|---|---|
| Control flow as per-instruction guards (`Guarded`) | `float r=0; if(c0!=0&&c1==0){ r=...; }` around each instruction | No real branches. Every variable is declared at function scope and conditionally assigned, which keeps everything live and inflates register pressure. The compiler sees thousands of tiny ifs instead of a few regions. |
| One flat register file, `r0..rN`, never reused | 8,200 lines; the register limit had to be raised to 16,384 | Live ranges span the whole kernel, causing spills and low occupancy. |
| Every integer is a 63-bit `long` (`as_type<long>(ulong(x)<<1)>>1`) | 64-bit multiply, shift and compare for PCG, indices and loop counters | Apple GPUs run 64-bit integers several times slower than 32-bit. |
| Booleans are floats (`!=0.0f`) | Float compares and selects for every condition | Extra ALU work and registers. |
| Error parity with the CPU walker: a finite check after each op | About 1,100 `atomic_fetch_or_explicit(status, …)` calls on device memory | An atomic per op serializes the thread, fences the compiler, and blocks reordering and fusion. |
| Gathers read one scalar component at a time with a clamp | `as_type<float>(gather[clamp(int(i),0,max(int(u)-1,0))*w+c])`, repeated per component | No vector loads, and the bounds math is redone per field. |

The handwritten kernel uses structured `for`/`if` with early exits, a few
dozen live values, `uint` PCG and `float3` loads, and it never writes device
memory except the film.

## How it must work: a structured, typed kernel IR

This is the design used by naga (the wgpu shader translator, which takes one
IR to WGSL, SPIR-V, MSL, GLSL and HLSL) and by Futhark's GPU backends
(ImpCode kernels):

1. **Structured statements over an expression arena.** A kernel is a tree of
   statements (`Block`, `If {cond; then; else}`, `Loop {body; break_if}`,
   `Break`, `Continue`, `Return`, `Store`) whose operands are typed SSA
   expressions. SPIR-V *requires* structured control flow (`OpSelectionMerge`,
   `OpLoopMerge`), and MSL and WGSL are structured languages. Predication is
   a CPU backend decision: Packed if-converts the structured IR for its
   lanes, and the GPU printers print the structure.
2. **Types are machine types.** `bool`, `i32`, `u32` (wraparound),
   `f32`, and `vec2/3/4<f32>` for loads and stores. `i64`/`f64` exist only
   in the exact tier, and a device kernel only gets them where the program
   asks for them. A uint32 PCG prints as `uint` (MSL), `OpTypeInt 32 0`
   (SPIR-V) and `u32` (WGSL), with the same bits as the CPU.
3. **SSA values printed where they are defined, in the innermost scope.**
   A single-use expression is printed inline in its user, and a multi-use one
   becomes a `const` local in the scope where it is computed (naga's "baking"
   rule). Nothing is declared at function scope unless it is loop-carried.
   Register allocation is left to the Metal and SPIR-V compilers; the
   emitter's job is to not extend live ranges.
4. **Loops are loops.** Bounces, lights, panels and CDF searches are
   `for`/`while` with `break`. Unrolling is the static stage's job, and only
   for small known trip counts.
5. **Errors don't touch memory in the hot path.** The approximate tier,
   which every device kernel uses, guarantees valid *outputs*: a thread ORs
   problems into a local bit set and publishes it with one atomic at the end,
   or checks only its outputs. Per-operation error parity belongs to the
   exact tier, which stays on the CPU.
6. **Data has layout.** Uniforms are one `constant` struct. A record table
   (materials, lights, panels) is a `device const` array of structs or
   `float4`s, read with one vector load per record and indexed once.
7. **Specialization is static folding.** Scene facts (light count, panel
   count, instanced or not, no world map, material classes) fold before
   emission, so dead branches never reach the GPU. That's how a generated
   kernel beats a handwritten one with uniform branches.

One IR still feeds every target. Region becomes this structured IR,
Packed is its CPU lowering, and MSL, SPIR-V, WGSL and JS print it.

## Gates

- **GPU time:** A/B against the handwritten reference in one process,
  interleaved, at least 5 pairs, with the machine load recorded. The
  generated kernel must be no slower.
- **Static shape, checked by tests:** no atomics inside loops and at most one
  per kernel; no `long` or `ulong` in a fast kernel unless the program uses
  64-bit integers; the line count of the emitted kernel within 2x of its
  handwritten reference.
- **Register pressure:** `MTLComputePipelineState.maxTotalThreadsPerThreadgroup`
  is recorded for each benchmark kernel. When a kernel uses more registers,
  Metal lowers this number, so a drop is a regression.
- **Film:** RMSE ≤ 1e-4 against the reference after N spp, with the CPU
  walker as the oracle.

## References

- naga, the wgpu shader translator: arena-based typed IR; MSL, SPIR-V,
  WGSL, GLSL and HLSL backends ([gogpu/naga](https://github.com/gogpu/naga),
  [Meganeura, arXiv 2608.01563](https://arxiv.org/pdf/2608.01563)).
- Futhark: imperative kernel IR (ImpCode) lowered from a functional
  language ([student report](https://www.futhark-lang.org/student-projects/steffen-msc-project.pdf),
  [PLDI 2017](https://researchprofiles.ku.dk/en/publications/futhark-purely-functional-gpu-programming-with-nested-parallelism/)).
- Predication vs branches on SIMT hardware: if-conversion is a late,
  machine-level choice
  ([NVIDIA forum](https://forums.developer.nvidia.com/t/solved-ptx-isa-predicated-execution-and-the-warp-divergence-issue/31957)).
- 64-bit integers on Apple GPUs are much slower than 32-bit
  ([Apple developer forum](https://developer.apple.com/forums/thread/718737)).
