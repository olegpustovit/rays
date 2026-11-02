(** Leaf memory ledger: every large retained allocation names one owner. A cache, a plan, or a
    document charges bytes here; {!report} compares the sum to explicit Rays memory. [Lru] takes a
    required owner and registers an evictor so {!retreat} can drop rebuildable data.

    Charges come from any domain: a charge or release is one or two atomic operations and allocates
    nothing. Making an owner or registering an evictor takes a lock. *)

type kind = Cache | Plan | Document

type pool = Host | Device
(** Host and device bytes are never added on the ledger's unattributed line: the two compare with
    [ocaml_live] and the backend's measured device bytes separately. *)

type owner
(** A named account. Equal names of the same kind and pool share one account. *)

val cache : ?pool:pool -> string -> owner
val plan : ?pool:pool -> string -> owner
val document : ?pool:pool -> string -> owner
val name : owner -> string
val kind : owner -> kind
val pool : owner -> pool

val charge : owner -> int -> unit
(** Add [n] retained bytes (no-op for [n <= 0]). *)

val release : owner -> int -> unit
(** Drop [n] retained bytes, floored at zero. *)

val bytes : owner -> int
val peak : owner -> int

val attributed : unit -> int
(** Sum of every owner's current bytes, host and device. *)

(** {1 The budget} *)

type source = Machine | Preference | Override

val set_budget : ?source:source -> int -> unit
(** Ceiling for what the process uses ([0]: unlimited). A lower [source] never replaces a higher
    one's: the machine's default, then the editor preference, then the process's override. *)

val budget : unit -> int

val machine_budget : working_set:int -> int
(** The default: the smaller of the device's working set ([<= 0]: unknown) and 60% of RAM. *)

val configure : (string -> string option) -> (unit, string) result
(** What the process environment says, read through the function the process boundary passes
    ([Sys.getenv_opt]; a library never reads it): [RAYS_MEM_BUDGET] ([4G], [200M], bare bytes) and
    [RAYS_MEM_CLOBBER=1]. A malformed size is an error. *)

val clobber : unit -> bool
(** [RAYS_MEM_CLOBBER=1]: a reuse point fills a dead buffer with NaN the moment it is dead, so a
    wrong lifetime fails at once. *)

(** {1 The retreat} *)

val register_evictor :
  owner ->
  ?rebuild:float ->
  ?held:(unit -> int) ->
  ?last_use:(unit -> int) ->
  ?live:(unit -> bool) ->
  ?forget:(unit -> unit) ->
  (unit -> bool) ->
  unit
(** [drop ()] drops one rebuildable entry and returns [true] when it did; [false] when it cannot right
    now (busy, nothing left). Cache owners only; [drop] and the other functions run on the domain
    calling {!retreat} and must be safe there. [held] is the bytes one drop chooses among (default:
    the owner's), [last_use] the {!epoch} last used, [rebuild] a relative cost to make it again.
    Once [live] is false the evictor is dropped ([forget] runs once then), so a table made per call
    must hold itself weakly here. *)

val evictor_count : unit -> int

val epoch : unit -> int
(** Frames since start: one per {!retreat}. Reading it is one load. *)

type overdraft = { used : int; limit : int; top : (string * int) list }
(** What [E_MEMORY] says: bytes in use, the limit, the largest owners. *)

type verdict = Within | Over of overdraft

val retreat :
  ?device:(unit -> int) ->
  ?usage:(unit -> int) ->
  ?later:(unit -> bool) list ->
  unit ->
  verdict
(** One frame boundary, on the initial domain. Under a limit that pressure may have lowered,
    [usage] (default: the physical footprint, which on one memory pool includes the device) decides
    if anything goes. Over: caches, coldest and largest per rebuild cost first; then each of [later]
    in order, called until it answers [false] (plans nothing shows, undo past 16 entries); then
    [Over]. Bytes already dropped are credited until the collector could have freed them, so a late
    collection does not evict the next cache. Going [Over] runs at most one full collection per
    crossing, then latches until usage is back under the limit: never a stall per frame.

    A critical pressure event also runs [Gc.full_major] and the allocator's pressure relief once,
    after evicting; nothing else does. *)

val admit : ?shed:(unit -> bool) -> unit -> (unit, overdraft) result
(** A cook about to run, on its own domain: [Error] when usage is over the limit after [shed] (the
    caller's own cache, which it alone may touch) and at most one collection per crossing. Reads the
    footprint: one syscall, for work that takes far longer. *)

val overdraft : unit -> overdraft option
(** The latched [Over], if the process is still over. *)

val describe : overdraft -> string

val total_owner : unit -> (string * int) option
(** The largest owner now, for a status line. *)

(** {1 Reports} *)

module Report : sig
  type entry = { name : string; kind : kind; pool : pool; bytes : int; peak : int }

  type t = {
    owners : entry list;  (** Non-zero peak, largest first. *)
    attributed : int;  (** Host and device. *)
    attributed_device : int;
    ocaml_live : int;
    device : int;
    footprint : int;
    unattributed : int;
    budget : int;
  }

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val format_bytes : int -> string
end

val report : ?collect:bool -> ?device:int -> ?budget:int -> unit -> Report.t
(** [ocaml_live] is [Gc.stat] live words after a full major ([~collect:false], for a crash report:
    the heap's size, no collection). [device] is what the caller measured through OGPU.
    [footprint] is the process physical footprint when available, else [ocaml_live + device].
    [unattributed] is host bytes no owner claims plus device bytes no owner claims. Never called on a
    frame path. *)

val reset : unit -> unit
(** Tests only: zero every owner, drop every evictor and the budget. Owners stay valid. *)

(** Process metrics from [proc_pid_rusage]. Unavailable platforms return [None]. *)
val phys_footprint : unit -> int option

val instructions : unit -> int64 option

val physical_ram : unit -> int
(** Installed memory; [0] when unknown. *)

val pressure_start : unit -> bool
(** Starts the OS memory-pressure source (idempotent); [false] where there is none. Its callback
    only stores a level the next {!retreat} reads: no OCaml runs on the OS's thread. *)

module Private : sig
  val pressure_inject : int -> unit
  (** As if the OS said 0 normal, 1 warning, 2 critical. *)

  val major_cycles : unit -> int
end
