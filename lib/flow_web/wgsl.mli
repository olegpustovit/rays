(** WGSL compute kernels for packed regions: the web counterpart of
    [Flow_gpu.Emit], with the same refusals ([Packed.gpu_refusals]), binding
    numbering and uniform layout. Nothing here is compiled by a WebGPU
    implementation in the test suite; the first real compile is the browser
    run. *)

type kind = Uniform_buffer | Storage_read | Storage_read_write
type binding = {group : int; binding : int; kind : kind}

type wgsl = private {
  source : string;
  entry : string;  (** Compute entry point, [kernel_<digest>], workgroup size 64. *)
  interface : binding list;
  (** Bindings in group 0, ascending. Input [i] is binding [i] (an input no
      reachable instruction reads has no binding), output [k] is binding
      [inputs + k], then the uniform block, the status word and, when the
      kernel uses noise, the permutation table. Inputs and the table are
      [Storage_read], outputs and the status word [Storage_read_write]. *)
  uniform_layout : (string * int * int) list;
  (** [(name, byte offset, width)] exactly as the Metal emitter lays them out:
      word 0 is the element count (u32), then [uniform:n] and [frame:slot:name]
      words hold f32 bit patterns. *)
  uniform_bytes : int;
  (** Metal's size rounded up to a multiple of 16: the block is an
      [array<vec4<u32>, n>] uniform buffer. *)
  table_seeds : int array;
  (** Noise seeds, ascending; seed [k]'s 512-entry [i32] permutation starts at
      table element [512*k]. *)
  output_widths : int array;
  input_widths : int array;
  gathers : (int * int) array;
  (** {!kernel_fast}: the uniform whose array gather table [k] holds, and its element width. *)
  counts : int option;
  (** {!kernel_fast}: the byte offset of the three count words (mul, div, use) in the uniform block. *)
}

val kernel : ?image:bool -> Flow_ir.Packed.t -> (wgsl, Diagnostic.t) result
(** Elements are f32; dispatch [ceil(count/64)] workgroups in x. The status
    word uses bit 1 for nonfinite values, bit 2 for integer division by zero,
    and bit 4 for an invalid integer shift; the host zeroes it before dispatch
    and maps these to [E_KERNEL] or [E_RANGE]. Dispatch is
    one-dimensional, so [count <= 64 * maxComputeWorkgroupsPerDimension].
    [image] replaces the sole vec2 source with pixel-center UV coordinates,
    using the [image:width] and [image:height] uniforms; output must be vec4.
    No UV storage buffer is allocated or uploaded. *)

val kernel_fast : Flow_ir.Packed.t -> (wgsl, Diagnostic.t) result
(** The web counterpart of [Flow_gpu.Emit.kernel ~fast] (a workspace's fast precision): the same
    refusals ([Packed.gpu_refusals ~fast:true]), bindings and uniform layout as Metal's. Integer
    registers are 64-bit ([vec2<u32>] low and high words: wrapping add, sub and mul, truncating
    division with [0] for a zero divisor, logical right shift, shifts by [n & 63]); integers travel
    in and out as f32. Gather [k] reads the [f32] table at binding [gather_base + k] (after the
    noise table), clamped to the element count in its [gather:k:u] uniform word. The count words
    ([counts]) bound the element count by [count_in[0] * mul / div] when use is set, and thread 0
    writes the 40-byte count record [{n, 6n, 1, 0, 0, 0, 6, n, 0, 0}] to [count_out]: native's six
    words, then from byte 24 the arguments of WebGPU's [drawIndirect] of n six-vertex instances (the
    presenter draws a mark set from them); both follow the gathers. *)

val compaction : string
(** The WGSL of a resident fold's [array/compact] (entry [flow_compact_mask], one workgroup of 256):
    bindings mask (f32), kept (f32 indices), total (the count cell), the uniform [vec4<u32>] bound
    (count, use) and count_in. *)
