(** Packed reverse topology derived from {!Topology.t}.

    The source topology stores forward primitive-to-corner and corner-to-point
    relations. This index adds point-to-corner CSR incidence and an undirected
    edge table. Polygon and closed-polyline corners own a cyclic outgoing edge;
    the final corner of an open polyline has no outgoing edge.

    Construction is O(vertices + edges) expected time and uses O(vertices +
    edges + points) auxiliary storage. The open-addressed edge table uses
    integer keys and does not allocate tuples in the insertion hot loop. *)

type t

val create : ?cancel:Cancel.t -> Topology.t -> t
(** Return the process-shared index for this immutable topology. The weak,
    thread-safe cache retains no topology after its source becomes unreachable. *)

val topology_data_id : t -> int
(* Data identity of the topology from which this index was derived. *)
val point_count : t -> int
val edge_count : t -> int

val primitive_of_vertex : t -> int -> int
val next_vertex : t -> int -> int

val edge_of_vertex : t -> int -> int
(** The outgoing undirected edge of a corner, or [-1] for the last corner of
    an open polyline. *)

val edge_points : t -> int -> int * int
val find_edge_index : t -> a:int -> b:int -> int
(* Allocation-free endpoint lookup. Returns [-1] when the edge is absent or
    either endpoint is outside the topology. *)
val find_edge : t -> a:int -> b:int -> int option
val edge_incidence_count : t -> int -> int
val edge_vertex : t -> edge:int -> local:int -> int
val point_incidence_count : t -> int -> int
val point_vertex : t -> point:int -> local:int -> int
val point_edge_count : t -> int -> int
val point_edge : t -> point:int -> local:int -> int
val boundary_edge_count : t -> int
val non_manifold_edge_count : t -> int

module Private : sig
  type view = {
    primitive_of_vertex : int array;
    next_vertex : int array;
    previous_vertex : int array;
    edge_of_vertex : int array;
    opposite_vertex : int array;
    edge_a : int array;
    edge_b : int array;
    edge_offsets : int array;
    edge_vertices : int array;
    point_offsets : int array;
    point_vertices : int array;
    point_edge_offsets : int array;
    point_edges : int array;
  }

  val view : t -> view
  (** Borrowed immutable planes for audited RDK kernels. *)

end
