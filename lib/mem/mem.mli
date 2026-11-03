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

val reachable_bytes : 'a -> int
(** The size of plain data, by walking it: once per cache miss on a value about to be kept, never
    per frame. *)

val root : owner -> (unit -> Obj.t list) -> unit
(** This owner's size is what these roots reach beyond the owners registered before it and every table
    that charges its own bytes, walked when a {!report} walks. For what has no size to charge as it is
    built (a lowered document); a later call
    replaces the roots, so a frame can name its current document, and the closure keeps what it names
    alive: replace it with [fun () -> []] on close. *)

val walk_table : owner -> 'a -> uncharged:(unit -> bool) -> unit
(** {!Lru}'s: a table held weakly, walked by a {!report} while [uncharged] (a count-only cache has no
    byte size to charge, and forgetting one must not leave its bytes on no owner). *)

val bytes : owner -> int
val peak : owner -> int

val attributed : unit -> int
val attributed_device : unit -> int
(** Sum of every owner's current bytes, host and device. *)

(** {1 The budget} *)

type source = Machine | Preference | Override

val set_budget : ?source:source -> int -> unit
(** Ceiling for what the process uses ([0]: unlimited). A lower [source] never replaces a higher
    one's: the machine's default, then the editor preference, then the process's override. *)

val budget : unit -> int

val set_device_budget : ?source:source -> int -> unit
(** Ceiling for the device's own bytes, [0]: none. Set only where they are not in the footprint (a
    discrete GPU's VRAM); unset, the budget above covers the device as before. Ranked like {!set_budget}. *)

val device_budget : unit -> int

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
    [Over]. Bytes dropped are credited until the collector could have freed them, so a late collection
    does not evict the next cache; but a drop frees nothing while another holder keeps the entry, so
    the credit lasts only to the next boundary, which reads what is still there. Going [Over] runs at most one full collection per
    crossing, then latches until usage is back under the limit: never a stall per frame.

    With a device budget (see {!set_device_budget}) the footprint is the host pool and the backend's bytes
    ([device]) are checked against it after: the same caches, device-pool ones only, then [Over]; no
    [later] step, which frees host bytes.

    A critical pressure event also runs [Gc.full_major] and the allocator's pressure relief once,
    after evicting; nothing else does. *)

val admit : ?shed:(unit -> bool) -> unit -> (unit, overdraft) result
(** A cook about to run, on its own domain: [Error] when usage is over the limit after [shed] (the
    caller's own cache, which it alone may touch) and at most one collection per crossing. Over the
    limit, what was dropped counts as freed only once that collection says so, or while none may run
    yet. Reads the footprint: one syscall, for work that takes far longer. *)

exception Refused of overdraft
(** A large allocation that would pass the limit, refused by {!admit_bytes}. Not a {!Diagnostic}: no
    kernel's fallback path can take it for a form the device declined and run on the CPU. A cook's
    node turns it into [E_MEMORY]. *)

val admit_bytes : ?pool:pool -> int -> unit
(** Called before a large allocation, from any domain; no owner sheds here, the cook's boundary did.
    One footprint read, at most one collection per crossing; credit as for {!admit}. [~pool:Device] under a device budget
    compares the ledger's device bytes plus [n] with it instead.
    @raise Refused when usage plus [n] passes the limit. *)

val retry_worthwhile : unit -> bool
(** Whether the limit rose since {!admit} last refused: only then is a refused cook worth another
    try (a pressure event ending, a raised budget). An edit makes a new cook of its own. *)

val overdraft : unit -> overdraft option
(** The latched [Over], if the process is still over. *)

val describe : overdraft -> string

val total_owner : unit -> (string * int) option
(** The largest owner now. *)

val status_line : unit -> string option
(** Set by {!retreat} from three quarters of the limit up: the total and the top owner. *)

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
    overclaimed : int;  (** Owners claim more than exists: a wrong size, which would hide the rest. *)
    budget : int;
  }

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val format_bytes : int -> string
end

val report : ?collect:bool -> ?walk:bool -> ?device:int -> ?budget:int -> unit -> Report.t
(** [ocaml_live] is [Gc.stat] live words after a full major ([~collect:false], for a crash report:
    the heap's size, no collection). [device] is what the caller measured through OGPU.
    [footprint] is the process physical footprint when available, else [ocaml_live + device].
    [unattributed] is host bytes no owner claims plus device bytes no owner claims. Never called on a
    frame path. *)

val reset : unit -> unit
(** Tests only: zero every owner, drop every evictor and the budget. Owners stay valid. *)

(** Process metrics from [proc_pid_rusage]. Unavailable platforms return [None]. *)
val phys_footprint : unit -> int option

val peak_footprint : unit -> int option
(** The process's highest footprint so far, what [/usr/bin/time -l] prints. *)

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
