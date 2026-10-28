(** Small shared kernel helpers: packed bitsets, open-addressing key maps,
    and index quickselect. *)

module Bits : sig

  val mem : Bytes.t -> int -> bool
  (** Unchecked bit read; the index must be in range. *)

  val set : Bytes.t -> int -> unit
  (** Unchecked bit set; the index must be in range. *)
end

module Identity_cache : sig
  type ('k, 'v) t
  (** A domain-safe cache keyed by physical identity with a fixed capacity.
      Entries are ephemerons, so a value is retained only while its key is
      alive; at most [capacity] entries are held, replaced first-in
      first-out. Lookup is O(capacity) integer comparisons and never cleans
      or rehashes the table. *)

  val create : id:('k -> int) -> int -> ('k, 'v) t
  (** [create ~id capacity]; [id] is a cheap stable identifier for a key,
      compared before physical identity. *)

  val find_or_add : ('k, 'v) t -> 'k -> (unit -> 'v) -> 'v
  (** Return the cached value for a key, or build, insert, and return it. The
      builder runs outside the lock; a racing insert of the same key wins. *)
end

val select : ?cancel:Cancel.t -> float array -> int array -> int -> int -> int -> unit
(** [select keys order first last k] reorders [order.(first..last)] so that
    [order.(k)] is the element that ordering by [keys.(i)] (ties by [i]) would
    put there, with smaller elements before it and larger after. Expected
    O(last - first) time, in place. *)

val get_ok : ('a, string) result -> 'a
(** The value, or [Invalid_argument] with the error message. *)

val ceiling_div : int -> int -> int
(** Quotient rounded up, for a non-negative value and a positive divisor. *)

val run_ranges : ?grain:int -> int -> (first:int -> last:int -> unit) -> unit
(** [run_ranges ~grain count work] runs [work ~first ~last] over consecutive half-open ranges of at most [grain], one task per range. Raises [Invalid_argument] when [grain <= 0]. *)

val parallel_for : ?cancel:Cancel.t -> grain:int -> int -> (int -> unit) -> unit
(** [parallel_for ~grain count work] runs [work] on [0 .. count - 1] in chunks
    of [grain] on the shared pool, checking [cancel] every 4096 elements. *)
