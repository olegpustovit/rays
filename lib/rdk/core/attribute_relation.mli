(** View compact source-to-destination incidence representation. *)
type t =
  | Identity of int
  | Constant_zero of int
  | Direct of int array
  | All of { source_count : int }
  | Csr_identity of { offsets : int array }
  | Csr_map of { offsets : int array; indices : int array }

val count : t -> int
val first : t -> int -> int
val last : t -> int -> int
val source : t -> int -> int -> int
val incidence_count : t -> int
val flat_first : t -> int -> int
val flat_last : t -> int -> int
val source_flat : t -> int -> int -> int
