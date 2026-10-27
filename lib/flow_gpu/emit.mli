type msl = private {source : string; entry : string; interface : Ogpu.Shader.binding list;
  uniform_layout : (string * int * int) list; uniform_bytes : int;
  table_seeds : int array; output_widths : int array; input_widths : int array;
  gathers : (int * int) array; counts : int option; scene : int option}
(* [gathers.(k)] is the uniform whose array the kernel's [gather<k>] buffer holds, and its element width.
   [counts] (fast kernels): the byte offset of three uniform words mul, div and use; the kernel's
   count is the dispatch count, or its minimum with [count_in[0] * mul / div] when use is set,
   and is written as the 24-byte count record {n, 6n, 1, 0, 0, 0} to [count_out] (buffers after the
   gathers): from byte 4 it is the indexed indirect draw of n six-index quads. *)
val names : string list
val compaction : msl
(* The kernel of a resident fold's [array/compact]: buffers mask, kept (float indices), total (uint),
   uniform (n, use) and count_in: the mask's count is n, or its minimum with [count_in[0]] when use is set. *)
val kernel : ?fast:bool -> Flow_ir.Packed.t -> (msl, Diagnostic.t) result
(* [fast] (the workspace's fast precision) also emits signed-63 integer arithmetic and gathers from
   bound tables. The shared status word uses bits 1/2/4 for nonfinite output, integer division by
   zero, and an out-of-range integer shift, respectively. *)
val image_rgba8_source : string
