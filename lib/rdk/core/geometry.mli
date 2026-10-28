(** Immutable geometry snapshots. Point positions, topology, attributes, and
    groups carry independent data IDs for precise graph invalidation. *)

type t

(** Point position [P] is represented canonically by [positions] and cannot be
    installed as a second ordinary attribute. *)

val create :
  positions:Packed.Float3.t ->
  topology:Topology.t ->
  ?attributes:Attribute.t list ->
  ?groups:Group.t list ->
  ?edge_groups:Edge_group.t list ->
  unit ->
  (t, string) result
val data_id : t -> int
val fresh_data_id : unit -> int
(** A new identity for a non-geometry payload (image, kernel). *)
val positions : t -> Packed.Float3.t
val topology : t -> Topology.t
val point_count : t -> int
val vertex_count : t -> int
val primitive_count : t -> int
(* Sum of packed payload storage. This deliberately excludes OCaml headers
   and may double-count a buffer deliberately installed under two names. *)
val payload_bytes : t -> int

(** Globally unique data ID and packed byte size for each structurally shared
    position, topology, attribute, and group component. *)
val payload_components : t -> (int * int) list

val attributes : t -> Attribute.t list
val groups : t -> Group.t list
val edge_groups : t -> Edge_group.t list
val find_attribute : owner:Attribute.owner -> string -> t -> Attribute.t option
val find_group : owner:Group.owner -> string -> t -> Group.t option
val find_edge_group : string -> t -> Edge_group.t option
val with_positions : Packed.Float3.t -> t -> (t, string) result
val with_attribute : Attribute.t -> t -> (t, string) result
val with_group : Group.t -> t -> (t, string) result
val with_edge_group : Edge_group.t -> t -> (t, string) result
val without_attribute : owner:Attribute.owner -> string -> t -> t
val without_group : owner:Group.owner -> string -> t -> t
val without_edge_group : string -> t -> t
val rename_edge_group :
  from:string -> into:string -> t -> (t, string) result

module View : sig
  val attributes : t -> Attribute.t array
  (** Borrowed zero-copy metadata view for audited RDK kernels. The array must
      never be mutated or retained beyond the immutable geometry's lifetime. *)

  val with_attributes_owned : Attribute.t array -> t -> (t, string) result
  (** Replace ordinary attribute metadata in one geometry rebuild. Ownership
      of the array transfers to the returned immutable geometry. This is an
      audited internal boundary for batch attribute operators; callers must
      neither mutate nor retain the array after a successful call. *)

  val with_merged_attributes_owned : Attribute.t array -> t -> (t, string) result
  (** Replace same-owner/same-name attributes and append new attributes in one
      expected-linear metadata rebuild. Replacement order is stable and later
      duplicate replacements win. Ownership of the array transfers to the
    call, and callers must not mutate or retain it after success. *)

  val with_merged_owned :
    ?positions:Packed.Float3.t ->
    attributes:Attribute.t array ->
    groups:Group.t array ->
    t ->
    (t, string) result
  (** Atomically replace optional positions plus same-owner/same-name ordinary
      attributes and groups in one validated metadata rebuild. Ownership of
      the replacement arrays transfers on success. *)

  val with_positions_and_attributes_owned :
    Packed.Float3.t -> Attribute.t array -> t -> (t, string) result
  (** Atomically replace canonical positions and ordinary attribute metadata in
      one geometry rebuild. Both payloads must already match the unchanged
      topology; ownership of the attribute array transfers to the result. *)
end

val generate_point_ranges :
  int ->
  (first:int -> last:int ->
   x:float array -> y:float array -> z:float array -> unit) ->
  t
(** High-throughput O(n) point generator. The callback runs once per stable
    disjoint half-open range and may mutate only indices from [first] inclusive
    to [last] exclusive in the owned output planes; it must not retain them. *)
