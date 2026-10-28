(** Sketch-owned settings kept in the editor document: a typed [Param] record, stored type-erased so
    the document stays one immutable value. The inspector shows them while no node is selected;
    edits are undoable, saved in presets, and passed to [prepare]. *)

type t

val none : t
val make : 'record Param.schema -> 'record -> t

val get : 'record Param.schema -> t -> 'record
(** The stored record read through [schema], matching fields by name. Raises [Invalid_argument] when
    [schema] does not describe it. *)

val fields : t -> Param.field_view list

val defaults : t -> t
(** The same schema at its defaults, for a complete saved document. *)

val apply : t -> (string * Param.value) list -> (t * Param.effects, string) result
