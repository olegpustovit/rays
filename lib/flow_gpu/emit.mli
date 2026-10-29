type msl = private {
  source : string;
  entry : string;
  interface : Ogpu.Shader.binding list;
  uniform_layout : (string * int * int) list;
  uniform_bytes : int;
  table_seeds : int array;
  output_widths : int array;
  input_widths : int array;
  gathers : (int * int) array;
  counts : int option;
  scene : int option;
}

(* [gathers.(k)] is the uniform whose array the kernel's [gather<k>] buffer holds, and its element width.
   [counts] (fast kernels): the byte offset of three uniform words mul, div and use; the kernel's
   count is the dispatch count, or its minimum with [count_in[0] * mul / div] when use is set,
   and is written as the 24-byte count record {n, 6n, 1, 0, 0, 0} to [count_out] (buffers after the
   gathers): from byte 4 it is the indexed indirect draw of n six-index quads. *)
val names : string list

val compaction : msl
(** The kernel of a resident fold's [array/compact] (shaders/compact.rays, one threadgroup of
    1,024): buffers mask, kept (float indices), total (uint), (n, use) and count_in: the mask's
    count is n, or its minimum with [count_in[0]] when use is set. *)

val bounds : msl
(** A resident packed float3 array's bounds (shaders/bounds.rays, one threadgroup of 1,024): buffers
    the array, the lanes' floats (lane l's min x y z, max x y z of the elements l, l + 1,024, ... at
    l x 6; a lane at or past the count stores nothing) and the count (uint). *)

val normals : msl
(** A triangle mesh's display normals from its resident packed float3 P (shaders/normals.rays, one
    thread a point, groups of 256): buffers P, the corners (uint), each point's CSR offsets and
    corners (uint), the normals (packed float3) and the count (uint). *)

val kernel :
  ?fast:bool ->
  ?instanced:bool ->
  ?motion:bool ->
  ?curves:[ `Loop | `Native ] ->
  ?spheres:bool ->
  Flow_ir.Packed.t ->
  (msl, Diagnostic.t) result

(* [instanced], [motion], [curves] and [spheres] are static scene facts of a [scene/trace] kernel
   ({!Run.build_scene} reports the first three): [`Native] strands are Metal curve primitives, [`Loop] an
   in-loop capsule test; [spheres] false (default true) folds the sphere tests away. Each scene/trace is
   trace.rays' helper (Trace), inlined; only its ray queries and table reads are printed here. *)
(* [fast] (the workspace's fast precision) also emits signed-63 integer arithmetic and gathers from
   bound tables. The shared status word uses bits 1/2/4 for nonfinite output, integer division by
   zero, and an out-of-range integer shift, respectively. *)
type raster = {
  input : int -> int -> string;
  int_input : int -> string;
  buffer : int -> string;
  output : int -> int -> string -> string;
  builtin : string -> string;
}

(* A raster placement of {!kernel}'s instruction printer: [input source component] and [int_input source]
   print an element's inputs (a vertex id, an interpolated fragment input), [buffer u] names the bound
   buffer behind array uniform [u], [output k component value] is the statement storing an output, and
   [builtin name] reads a built-in fact ([gpu/instance-id], [gpu/front-facing]). *)
val stage_body : raster:raster -> Flow_ir.Packed.t -> (string, Diagnostic.t) result
(* The statements of one stage output program (fast tier), with no bounds clamps, status atomics or
   kernel wrapper; Raster prints the stage function around them. *)
