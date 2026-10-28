type t = private {
  bytes : bytes;
  entry : string;
  interface : Ogpu.Shader.binding list;
  layout : Flow_ir.Kernel_layout.t;
  uniform_bytes : int;
}
(** Binary SPIR-V compute emission from the shared kernel layout. *)

val kernel : ?fast:bool -> ?instanced:bool -> ?motion:bool -> ?curves:bool -> Flow_ir.Packed.t -> (t, Diagnostic.t) result
(** A moving scene ([motion]) is refused ([E_GPU_FORM]): SPIR-V ray queries have no motion form. [curves]
    strands are the in-loop capsule test of {!Emit.kernel}'s [`Loop]. *)
