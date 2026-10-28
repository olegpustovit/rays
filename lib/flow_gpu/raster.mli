(** Raster stages from Rays Lisp: a vertex stage is a map over vertex ids and a fragment stage a map
    over fragments, two placements of the same region IR printed by {!Emit}'s instruction printer.
*)

val msl :
  ?arguments:bool ->
  declarations:string ->
  vertex:string ->
  fragment:string ->
  unit ->
  (string, Diagnostic.t) result
(** The Metal source of the [vertex] and [fragment] defns of [declarations] as one pipeline's
    stages. A stage's [(array int)] and [(array float)] parameters are the renderer's buffers, bound
    at their position among the array parameters; the vertex entry's first parameter is the vertex
    id, and it returns [:position] (clip space) beside the varyings, those under [:flat] not
    interpolated. The fragment entry takes varyings by name and returns the colour. With [arguments]
    each texture [t] and its sampler are an argument buffer at buffer [t] (ids 0 and 1). *)

val spirv :
  declarations:string -> vertex:string -> fragment:string -> (bytes * bytes, Diagnostic.t) result
(** The same pipeline as Vulkan SPIR-V vertex and fragment modules (entry [main]); varyings are at
    locations in the vertex result's leaf order. *)

val compute_msl : size:int -> declarations:string -> entry:string -> (string, Diagnostic.t) result
(** The Metal kernel of a compute stage, the [entry] defn of [declarations] run by one threadgroup
    of [size] threads ([lid]): its first parameter is the thread's index in the group, its array
    parameters the bound buffers in order (writable when it stores into them with [gpu/store]) and
    its last the group size, given as the constant [size]; [gpu/group-scan] is a threadgroup scan
    between barriers. Its result is discarded. *)

val compute_spirv : size:int -> declarations:string -> entry:string -> (bytes, Diagnostic.t) result
(** The same stage as a Vulkan GLCompute module (entry [main], LocalSize [size]): Workgroup memory
    and OpControlBarrier for the scan. *)

val compute_program :
  size:int ->
  declarations:string ->
  entry:string ->
  (Flow_ir.Packed.t * (int * int * bool * bool) list, Diagnostic.t) result
(** The compute stage's program and its buffers (array uniform, binding, uint words, stored), for
    another target's printer ([Flow_web.Wgsl.compute]). *)
