(** HDK-style polygon topology. Vertices are corners which reference points;
    each primitive owns one half-open range of consecutive vertices. *)

type t
type topology = t
type primitive_kind = Polygon | Open_polyline | Closed_polyline

val empty : point_count:int -> t
val polygons_owned :
  point_count:int -> vertex_points:int array -> primitive_offsets:int array ->
  (t, string) result
val create_owned :
  point_count:int -> vertex_points:int array -> primitive_offsets:int array ->
  primitive_kinds:primitive_kind array -> (t, string) result
val point_count : t -> int
val vertex_count : t -> int
val primitive_count : t -> int
val data_id : t -> int
val payload_bytes : t -> int
val point_of_vertex : t -> int -> int
val primitive_vertex_range : t -> int -> int * int
val primitive_size : t -> int -> int
val primitive_kind : t -> int -> primitive_kind
val all_triangles : t -> bool

module Builder : sig
  type t
  val create : ?vertex_capacity:int -> ?primitive_capacity:int -> point_count:int -> unit -> t
  val add_open_polyline : t -> int array -> unit
  val add_closed_polyline : t -> int array -> unit
  val freeze : t -> topology
end

module Private : sig
  type view = {
    point_count : int;
    vertex_points : int array;
    primitive_offsets : int array;
    primitive_kinds : bytes;
  }
  val view : t -> view
  val polygons_shared :
    point_count:int -> vertex_points:int array -> primitive_offsets:int array ->
    (t, string) result
  val create_validated_owned :
    point_count:int -> vertex_points:int array -> primitive_offsets:int array ->
    primitive_kinds:bytes -> t
  (** Trusted ownership-transfer constructor for cardinality-first RDK
      generators that established all topology invariants while filling. *)

end
