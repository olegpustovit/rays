(** Leaf memory ledger: every large retained allocation names one owner. A cache, a plan, or a
    document charges bytes here; {!report} compares the sum to explicit Rays memory. [Lru] takes a
    required owner and registers an evictor so {!retreat} can drop rebuildable data. *)

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

val attributed : unit -> int
(** Sum of every owner's current bytes. *)

val set_budget : int -> unit
(** Soft ceiling for attributed + device bytes. [0] means unlimited. Also set via [RAYS_MEM_BUDGET]
    ([4G], [200M], bare bytes). *)

val budget : unit -> int

val register_evictor : owner -> (unit -> bool) -> unit
(** [f ()] drops one rebuildable entry and returns [true] when it did. Cache owners only. *)

val retreat : ?device:int -> unit -> [ `Ok | `Still_over ]
(** Evict registered caches until under budget (or nothing left to drop). *)

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
    [max 0 (ocaml_live + device - attributed)] — owners versus explicit Rays memory. *)

val reset : unit -> unit
(** Tests only: clear every owner and evictor. *)

(** Process metrics from [proc_pid_rusage]. Unavailable platforms return [None]. *)
val phys_footprint : unit -> int option
val instructions : unit -> int64 option
