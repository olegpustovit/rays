val mix : int -> int -> int
(** Stable 65599 polynomial step used by scene and cache fingerprints. *)
val mix_with : int -> int -> int -> int
(** Polynomial step with an explicit multiplier for fingerprints with a distinct salt. *)
val fnv_int_mix : int -> int -> int
(** One 63-bit FNV-1a step, used for integer UI keys. *)
val fnv_int_range : seed:int -> string -> int -> int -> int
(** FNV-1a over a string byte range, preserving the supplied 63-bit seed. *)
val fnv64_mix : int64 -> int -> int64
(** One 64-bit FNV-1a step. *)
val float_sequence32 : float list -> int
(** Stable 32-bit fingerprint for a sequence of float values. *)

val string : string -> int64
(** Stable FNV-1a 64-bit hash. *)
