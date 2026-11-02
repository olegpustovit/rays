(** Leaf memory ledger: every large retained allocation names one owner. A cache, a plan, or a
    document charges bytes here; {!report} compares the sum to the process footprint. No library
    depends on this one; [Lru] takes an optional owner so the compiler will later list every site. *)

type kind = Cache | Plan | Document

type owner
(** A named account. Equal names of the same kind share one account. *)

val cache : string -> owner
val plan : string -> owner
val document : string -> owner

val name : owner -> string
val kind : owner -> kind

val charge : owner -> int -> unit
(** Add [n] retained bytes (no-op for [n <= 0]). *)

val release : owner -> int -> unit
(** Drop [n] retained bytes, floored at zero. *)

val bytes : owner -> int
val peak : owner -> int

module Report : sig
  type entry = { name : string; kind : kind; bytes : int; peak : int }

  type t = {
    owners : entry list;  (** Non-zero peak, largest first. *)
    attributed : int;
    ocaml_live : int;
    device : int;
    footprint : int;
    unattributed : int;
    budget : int;
  }

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end

val report : ?device:int -> ?budget:int -> unit -> Report.t
(** [ocaml_live] is [Gc.stat] live words after a full major. [device] is what the caller measured
    (Metal [currentAllocatedSize], or the mock's count). [footprint] is the process physical
    footprint when available, else [ocaml_live + device]. [unattributed] is
    [max 0 (footprint - attributed)] when footprint is known, else [max 0 (ocaml_live - attributed)]. *)

val reset : unit -> unit
(** Tests only: clear every owner. *)

(** Process metrics from [proc_pid_rusage]. Unavailable platforms return [None]. *)
val phys_footprint : unit -> int option
val instructions : unit -> int64 option
