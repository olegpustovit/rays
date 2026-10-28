type t = private {
  bytes : bytes;
  entry : string;
  interface : Ogpu.Shader.binding list;
  layout : Flow_ir.Kernel_layout.t;
  uniform_bytes : int;
}
(** Binary SPIR-V compute emission from the shared kernel layout. *)

val kernel : ?fast:bool -> ?instanced:bool -> Flow_ir.Packed.t -> (t, Diagnostic.t) result
