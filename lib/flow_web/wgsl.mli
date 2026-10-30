(** WGSL compute kernels for packed regions: the web counterpart of [Flow_shader.Emit], with the same
    refusals ([Packed.gpu_refusals]), binding numbering and uniform layout. Nothing here is compiled
    by a WebGPU implementation in the test suite; the first real compile is the browser run. *)

type kind = Uniform_buffer | Storage_read | Storage_read_write
type binding = { group : int; binding : int; kind : kind }

type wgsl = private {
  source : string;
  entry : string;  (** Compute entry point, [kernel_<digest>], workgroup size 64. *)
  interface : binding list;
      (** Bindings in group 0, ascending. Input [i] is binding [i] (an input no reachable
          instruction reads has no binding), output [k] is binding [inputs + k], then the uniform
          block, the status word and, when the kernel uses noise, the permutation table. Inputs and
          the table are [Storage_read], outputs and the status word [Storage_read_write]. *)
  uniform_layout : (string * int * int) list;
      (** [(name, byte offset, width)] exactly as the Metal emitter lays them out: word 0 is the
          element count (u32), then [uniform:n] and [frame:slot:name] words hold f32 bit patterns.
      *)
  uniform_bytes : int;
      (** Metal's size rounded up to a multiple of 16: the block is an [array<vec4<u32>, n>] uniform
          buffer. *)
  table_seeds : int array;
      (** Noise seeds, ascending; seed [k]'s 512-entry [i32] permutation starts at table element
          [512*k]. *)
  output_widths : int array;
  input_widths : int array;
  gathers : (int * int) array;
      (** {!kernel_fast}: the uniform whose array gather table [k] holds, and its element width. *)
  counts : int option;
      (** {!kernel_fast}: the byte offset of the three count words (mul, div, use) in the uniform
          block. *)
}

val kernel : ?image:bool -> Flow_ir.Packed.t -> (wgsl, Diagnostic.t) result
(** Elements are f32; dispatch [ceil(count/64)] workgroups in x. The status word uses bit 1 for
    nonfinite values, bit 2 for integer division by zero, and bit 4 for an invalid integer shift;
    the host zeroes it before dispatch and maps these to [E_KERNEL] or [E_RANGE]. Dispatch is
    one-dimensional, so [count <= 64 * maxComputeWorkgroupsPerDimension]. [image] replaces the sole
    vec2 source with pixel-center UV coordinates, using the [image:width] and [image:height]
    uniforms; output must be vec4. No UV storage buffer is allocated or uploaded. *)

val kernel_fast : Flow_ir.Packed.t -> (wgsl, Diagnostic.t) result
(** The web counterpart of [Flow_shader.Emit.kernel ~fast] (a workspace's fast precision): the same
    refusals ([Packed.gpu_refusals ~fast:true]), bindings and uniform layout as Metal's. Integer
    registers are 64-bit ([vec2<u32>] low and high words: wrapping add, sub and mul, truncating
    division with [0] for a zero divisor, logical right shift, shifts by [n & 63]); integers travel
    in and out as f32. Gather [k] reads the [f32] table at binding [gather_base + k] (after the
    noise table), clamped to the element count in its [gather:k:u] uniform word. The count words
    ([counts]) bound the element count by [count_in[0] * mul / div] when use is set, and thread 0
    writes the 40-byte count record [{n, 6n, 1, 0, 0, 0, 6, n, 0, 0}] to [count_out]: native's six
    words, then from byte 24 the arguments of WebGPU's [drawIndirect] of n six-vertex instances (the
    presenter draws a mark set from them); both follow the gathers. *)

val compute :
  size:int ->
  name:string ->
  buffers:(int * int * bool * bool) list ->
  Flow_ir.Packed.t ->
  (string, Diagnostic.t) result
(** A compute stage (Flow_shader.Raster's) as the module of entry [name], workgroups of [size]
    ([@builtin(global_invocation_id)] is its element): [buffers] are (array uniform, binding, u32
    words, stored), read unclamped; [gpu/group-scan] is a [var<workgroup>] scan between
    [workgroupBarrier]s and [gpu/store] a statement. Its result is discarded. *)

type stage = {
  entry : string;
  program : Flow_ir.Packed.t;
  buffers : (int * int * bool) list;  (** (array uniform, binding, u32 words) *)
  elements : string list;  (** the element parameters, in order *)
  outputs : (string * Flow.Ty.t * bool) list;  (** the result's leaves: name, type, flat *)
}
(** A raster stage, as [Flow_shader.Raster.stages] gives it. *)

val raster : (bool * stage * stage) list -> (string, Diagnostic.t) result
(** Raster pipelines [(quads, vertex, fragment)] as one WGSL module, entries named as the stages,
    printed by {!kernel_fast}'s instruction printer in raster placements: every resource in group 0
    at its binding (a binding that any stage reads as words is [array<u32>], its floats their bits),
    texture [t] with its sampler at [t + 1], varyings at locations in the vertex result's leaf order.
    The vertex id is [vertex_index], or with [quads] [instance_index * 4 + corner] of a non-indexed
    six-vertex draw (native's [vid / 4] instance and [vid % 4] corner); a fragment's [fwidth] of a
    varying is read once at the top, before any branch, for WGSL's derivative uniformity. *)
