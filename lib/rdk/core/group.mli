type owner = Point | Vertex | Primitive
type t
type group = t

val init : ?grain:int -> owner:owner -> name:string -> int -> (int -> bool) -> t
(* [ordered ~owner ~name ~length elements] creates membership whose explicit
   traversal order is [elements]. Indices must be unique and in bounds. *)
val ordered :
  owner:owner -> name:string -> length:int -> int array -> (t, string) result
val name : t -> string
val owner : t -> owner
val length : t -> int
val data_id : t -> int
val with_name : string -> t -> t
val payload_bytes : t -> int
val is_ordered : t -> bool
(* A defensive copy of the explicit order, when the group has one. *)
val ordered_elements : t -> int array option
val mem : int -> t -> bool
val cardinality : t -> int
val union : t -> t -> (t, string) result
(* Union a non-empty collection of same-owner, same-length groups in one
    packed byte pass. Explicit traversal orders are intentionally discarded:
    the result iterates in increasing element order. Time is
    O(groups * packed-bytes), auxiliary storage is one result bitset, and
    disjoint byte ranges are deterministic across domain counts. *)
val union_many :
  ?cancel:Cancel.t -> ?grain:int -> name:string -> t list -> (t, string) result
val intersection : t -> t -> (t, string) result
val difference : t -> t -> (t, string) result
val symmetric_difference : t -> t -> (t, string) result
(* Invert membership while keeping padding bits clear. The packed byte pass
   uses the reusable domain pool above its internal scheduling cutoff. *)
(* Iterate members in increasing element-number order, preserving the legacy
   membership traversal contract. *)
val iter : (int -> unit) -> t -> unit
(* Iterate in explicit order when present, otherwise in increasing order. *)
val iter_ordered : (int -> unit) -> t -> unit

module Builder : sig
  type t
  val create : owner:owner -> name:string -> int -> t
  val set : t -> int -> bool -> unit
  val freeze : t -> group
end

module Private : sig
  val bits_view : t -> bytes
  (** Borrow packed membership storage for audited kernels. The returned bytes
      must never be mutated or retained beyond the immutable group's lifetime. *)

  val with_owner : owner -> t -> t
  (** Zero-copy ownership view for internal kernels whose packed index space is
      already validated by their caller. Membership and identity are shared. *)

  val of_owned_bits : owner:owner -> name:string -> length:int -> bytes -> t
  (** Trusted ownership-transfer boundary for packed kernels. The byte buffer
      must contain exactly [(length + 7) / 8] bytes and all padding bits must
      be clear. *)

  val order_view : t -> int array option
  (** Borrow the explicit order for internal kernels. Do not mutate it. *)

  val with_owned_order : int array -> t -> t
  (** Install an owned explicit order after validating that it contains every
      member exactly once. The caller must not mutate the array afterwards. *)

  val remap_order : source:t -> source_of_target:int array -> t -> t
  (** Preserve a source order through a target-to-source ancestry map. Multiple
      targets of one source remain in target-index order; selected targets with
      no selected ancestor are appended in target-index order. *)
end
