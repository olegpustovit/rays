type owner = Point | Vertex | Primitive | Detail

type storage =
  | Float of float array
  | Int of int array
  | Int_array of Packed.Int_array.t
  | Float_array of Packed.Float_array.t
  | Float2 of Packed.Float2.t
  | Float3 of Packed.Float3.t
  | Float4 of Packed.Float4.t
  | Text of string array

type t

(** A typed attribute key resolves a name, owner, and packed storage kind once.
    Values retrieved through a key cannot have the wrong OCaml representation. *)
type 'a kind
type 'a key

val float : float array kind
val float2 : Packed.Float2.t kind
val float3 : Packed.Float3.t kind
val key : name:string -> owner:owner -> 'a kind -> 'a key
val create_key_owned : 'a key -> 'a -> (t, string) result
val get : 'a key -> t -> 'a option
(** Scalar, integer, and text planes are returned as copies. Packed tuple and
    CSR array values are immutable outside their explicitly unsafe [View]
    views. *)

val normal : owner:owner -> Packed.Float3.t key
val color : owner:owner -> Packed.Float4.t key
val tex_coord : owner:owner -> Packed.Float2.t key

val create_owned : name:string -> owner:owner -> storage -> (t, string) result
val name : t -> string
val owner : t -> owner
val storage : t -> storage
val length : t -> int
val data_id : t -> int
val storage_id : t -> int
(* Identity of the structurally shared packed payload. Renaming an attribute
   changes [data_id] but preserves [storage_id]. *)
val with_name : string -> t -> (t, string) result
val payload_bytes : t -> int
val kind_name : t -> string

module View : sig
  val storage : t -> storage
  (** Borrowed zero-copy storage for audited sibling-library kernels. The
      returned arrays must never be mutated or retained beyond the immutable
      attribute's lifetime. *)
end

val float_planes : storage -> float array array option
(** Borrowed component planes of a [Float]..[Float4] storage (one to four, in
    x y z w order); [None] for other storages. Never mutate them. *)

val of_float_planes : like:storage -> float array array -> storage
(** Owned planes rebuilt as the same storage kind as [like]. *)
