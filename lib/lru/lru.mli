(** The one bounded least-recently-used table for every capacity-limited cache in rays: a count
    capacity, an optional byte capacity, and a release callback for owned values such as images and
    GPU buffers. A hit or an add allocates nothing beyond the entry itself. Not domain-safe: keep
    one table per domain ([Domain.DLS]) when several domains cache. *)

val cache : ?pool:Mem.pool -> string -> Mem.owner
val plan : ?pool:Mem.pool -> string -> Mem.owner
val document : ?pool:Mem.pool -> string -> Mem.owner
(** Ledger owners for {!Make.create} / {!Identity_list.create}; same accounts as [Mem]. *)

type shed =
  | Home  (** Its users stay on the domain that made it: the retreat drops only from that domain. *)
  | Locked of Mutex.t
      (** Every user holds this lock: the retreat skips the table while another domain holds it. *)
  | Never  (** Not droppable at a boundary: the site says why (not rebuildable, or no safe moment). *)

module Make (K : Hashtbl.HashedType) : sig
  type 'v t

  val create :
    owner:Mem.owner ->
    ?shed:shed ->
    ?rebuild:float ->
    ?byte_capacity:int ->
    ?evictable:(K.t -> 'v -> bool) ->
    ?release:(K.t -> 'v -> unit) ->
    int ->
    'v t
  (** [create ~owner capacity] keeps at most [capacity] entries and [byte_capacity] bytes (unbounded
      by default). Only [evictable] entries are eviction candidates (all by default), so a pinned
      entry can hold the table over its limits until it is released. [release] runs exactly once for
      every entry that leaves the table, including [clear]. [owner] charges and releases the entry
      byte sizes on the memory ledger. [shed] (default [Home]) says when {!Mem.retreat} may drop an
      entry, [rebuild] its relative cost to make again (default 1, cheaper first). The retreat holds
      the table weakly, and what a dead table still charged leaves the ledger with it. *)

  val find : 'v t -> K.t -> 'v
  (** A hit marks the entry recently used; a miss raises [Not_found]. This is the frame-path lookup:
      it allocates nothing and writes one flag. *)

  val find_opt : 'v t -> K.t -> 'v option
  (** [find] returning an option (one allocation per hit). *)

  val peek : 'v t -> K.t -> 'v option
  (** A hit that does not count as use. *)

  val add : 'v t -> ?bytes:int -> K.t -> 'v -> unit
  (** Inserts as newest, releasing a different previous value under [key] (the same value again only
      changes its size and recency), then evicts until the table is within its limits: the victim is the least recently inserted entry that has not been found
      since (second-chance order), and pinned entries are skipped. An entry larger than the byte
      capacity is released again immediately. *)

  val remove : 'v t -> K.t -> unit

  val take : 'v t -> K.t -> 'v option
  (** Removes without releasing: the caller now owns the value. *)

  val drop_oldest : 'v t -> bool
  (** Releases the oldest evictable entry, for bounds the table cannot see (shared byte pools);
      [false] when nothing can go. *)

  val find_first : 'v t -> (K.t -> 'v -> bool) -> 'v option
  (** Oldest inserted first, without reordering. *)

  val iter : 'v t -> (K.t -> 'v -> unit) -> unit
  (** Oldest inserted first. [f] must not add or remove entries. *)

  val copy : release:(K.t -> 'v -> unit) -> 'v t -> 'v t
  (** Independent cache metadata with the same ordering, touch bits, limits and values. Values are
      shared; the caller supplies their release policy. The source stays the values' one owner: a
      copy charges nothing and registers no evictor. *)

  val length : 'v t -> int
  val bytes : 'v t -> int
  val clear : 'v t -> unit
end

module Identity_list : sig
  type ('key, 'value) t

  val create :
    owner:Mem.owner ->
    ?shed:shed ->
    ?rebuild:float ->
    ?release:('key -> 'value -> unit) ->
    equal:('key -> 'key -> bool) ->
    int ->
    ('key, 'value) t
  (** A bounded MRU list for typed keys whose equality is identity-based or otherwise cheap.
      [release] runs when an entry leaves the list. [owner], [shed] and [rebuild] as for {!Make.create}. *)

  val find : ('key, 'value) t -> 'key -> 'value option
  (** A hit moves the entry to the front. *)

  val peek : ('key, 'value) t -> 'key -> 'value option
  (** Look up without changing recency. *)

  val add : ('key, 'value) t -> ?bytes:int -> 'key -> 'value -> unit
  (** [bytes], when set, charges [owner] until the entry leaves. *)

  val remove : ('key, 'value) t -> 'key -> unit
  val head : ('key, 'value) t -> ('key * 'value) option

  val copy : release:('key -> 'value -> unit) -> ('key, 'value) t -> ('key, 'value) t
  (** Copies recency while sharing immutable keys and values. The caller supplies the new cache's
      release policy explicitly; like {!Make.copy} it charges nothing and registers no evictor. *)

  val iter : ('key, 'value) t -> ('key -> 'value -> unit) -> unit
  val filter : ('key, 'value) t -> ('key -> 'value -> bool) -> unit
  val length : ('key, 'value) t -> int
  val clear : ('key, 'value) t -> unit
end
