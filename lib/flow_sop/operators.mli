val all : Flow.Op.t list
(** Default immutable operator declarations owned by the SOP workspace host. *)

val seed_for : ?salt:int -> seed:int64 -> namespace:string -> label:string -> unit -> int
(** Exact context-seed mixing used by [sop/context_seed], shared with host primitive fixtures.
    [salt] (default 0) is its low 63 bits xor-ed into the identity before mixing. Names and labels
    are hashed as UTF-8 bytes. *)

val context_label : string
(** The reserved argument naming the card whose inlined body reads its cook context: a context
    operator carrying it is a cook-time source. *)

val provenance_sentinel : int
(** What [sop/provenance_base] is in an inlined body: the lowering of its [sop/concat] replaces it
    with the provenance base it allocates. *)

(** How the lowerer builds a node of this kind; [None] is an ordinary catalog or Lisp node. *)
type lowering =
  | Geometry_arrays
  | Metadata
  | Attribute_write
  | Group_write
  | Attribute_delete
  | Curve
  | Zone
  | Resource

val lowering : string -> lowering option
