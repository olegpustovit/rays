(** Raster stages from Rays Lisp: a vertex stage is a map over vertex ids and a fragment stage a map
    over fragments, two placements of the same region IR printed by {!Emit}'s instruction printer. *)

val msl : ?arguments:bool -> declarations:string -> vertex:string -> fragment:string -> unit -> (string, Diagnostic.t) result
(** The Metal source of the [vertex] and [fragment] defns of [declarations] as one pipeline's stages.
    A stage's [(array int)] and [(array float)] parameters are the renderer's buffers, bound at their
    position among the array parameters; the vertex entry's first parameter is the vertex id, and it
    returns [:position] (clip space) beside the varyings, those under [:flat] not interpolated. The
    fragment entry takes varyings by name and returns the colour. With [arguments] each texture [t] and
    its sampler are an argument buffer at buffer [t] (ids 0 and 1). *)

val spirv : declarations:string -> vertex:string -> fragment:string -> (bytes * bytes, Diagnostic.t) result
(** The same pipeline as Vulkan SPIR-V vertex and fragment modules (entry [main]); varyings are at
    locations in the vertex result's leaf order. *)
