(** Raster stages from Rays Lisp: a vertex stage is a map over vertex ids and a fragment stage a map
    over fragments, two placements of the same region IR printed by {!Emit}'s instruction printer. *)

val msl : declarations:string -> vertex:string -> fragment:string -> (string, Diagnostic.t) result
(** The Metal source of the [vertex] and [fragment] defns of [declarations] as one pipeline's stages.
    A stage's [(array int)] and [(array float)] parameters are the renderer's buffers, bound at their
    position among the array parameters; the vertex entry's first parameter is the vertex id, and it
    returns [:position] (clip space) beside the varyings, those under [:flat] not interpolated. The
    fragment entry takes varyings by name and returns the colour. *)
