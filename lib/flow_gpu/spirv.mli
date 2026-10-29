type t = private {
  bytes : bytes;
  entry : string;
  interface : Ogpu.Shader.binding list;
  layout : Flow_ir.Kernel_layout.t;
  uniform_bytes : int;
}
(** Binary SPIR-V compute emission from the shared kernel layout. *)

val kernel :
  ?fast:bool ->
  ?instanced:bool ->
  ?motion:bool ->
  ?curves:bool ->
  ?spheres:bool ->
  Flow_ir.Packed.t ->
  (t, Diagnostic.t) result
(** A moving scene ([motion]) is refused ([E_GPU_FORM]): SPIR-V ray queries have no motion form.
    [curves] strands are the in-loop capsule test of {!Emit.kernel}'s [`Loop]. *)

type output = Position | Point_size | Varying of int * bool | Colour | Discard

type stage = {
  vertex : bool;
  inputs : (int * int * bool) array;
  outputs : output array;
  buffers : (int * int * bool) list;
  workgroup : int option;
  grid : bool;
}
(** A raster stage's interface: [inputs.(k)] is element [k]'s (location, width, flat); [outputs.(k)]
    is output [k]'s role ([Varying (location, flat)]); [buffers] are (array uniform, binding, uint
    words). [workgroup] [Some n] makes it a compute stage of one workgroup of [n] threads
    (GLCompute, LocalSize n): its element is LocalInvocationIndex, its outputs are discarded and it
    may store into its buffers. [grid] makes its element GlobalInvocationId.x instead, a stage
    dispatched over any number of workgroups. *)

val stage : stage -> Flow_ir.Packed.t -> (bytes, Diagnostic.t) result
(** A Vulkan vertex or fragment module (entry [main]) of a raster stage program (Flow_gpu.Raster).
*)
