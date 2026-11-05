type t = private {
  bytes : bytes;
  entry : string;
  interface : Ogpu_core.Shader.binding list;
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
  ?uint32_scalars:bool ->
  ?uint32_outputs:int list ->
  ?uint32_inputs:int list ->
  ?uint32_tables:int list ->
  ?unfold:int ->
  Flow_ir.Packed.t ->
  (t, Diagnostic.t) result
(** Moving triangles and instances use swept AABB candidates; linear curves use per-segment swept AABBs
    and capsule tests in the ray-query candidate loop. *)

type output = Position | Point_size | Varying of int * bool | Colour | Discard

type stage = {
  vertex : bool;
  inputs : (int * int * bool) array;
  outputs : output array;
  buffers : (int * int * bool) list;
  workgroup : int option;
  grid : bool;
}

val stage : stage -> Flow_ir.Packed.t -> (bytes, Diagnostic.t) result
(** A Vulkan vertex or fragment module (entry [main]) from the shared raster-stage program. Workgroup
    stages are GLCompute modules; [grid] selects dispatch-wide rather than workgroup-local indexing. *)
