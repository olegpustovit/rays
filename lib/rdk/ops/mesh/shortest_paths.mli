(** Shortest paths over the unique-edge graph of a {!Topology_index}. *)

val search :
  ?cancel:Cancel.t ->
  ?radius:float ->
  point_edge_offsets:int array ->
  point_edges:int array ->
  edge_a:int array ->
  edge_b:int array ->
  lengths:float array ->
  int array ->
  float array * int array
(** [search ~point_edge_offsets ~point_edges ~edge_a ~edge_b ~lengths roots]
    returns [(distances, parents)], one entry per point. [lengths] holds one
    non-negative length per edge. Roots have distance [0.] and parent [-1];
    points not reached within [radius] (default unbounded) keep [infinity] and
    parent [-1]. Equal distances are settled by the lower point index, and a
    point's parent is the first settled neighbour that gave its final
    distance. Roots must be valid point indices. *)

exception Hop_overflow
(** An accumulated length was not finite. *)

val hop_search :
  ?blocked_point:(int -> bool) ->
  ?blocked_edge:(int -> bool) ->
  ?target:int ->
  depths:int array ->
  distances:float array ->
  previous:int array ->
  queue:int array ->
  point_edge_offsets:int array ->
  point_edges:int array ->
  edge_a:int array ->
  edge_b:int array ->
  lengths:float array ->
  int array ->
  unit
(** [hop_search ~depths ~distances ~previous ~queue ... roots] fills the
    caller-owned scratch arrays (each one entry per point; [queue] at least
    that long). Order is (hop count from the nearest root, accumulated length,
    lower predecessor index); the chosen predecessor of a point is the
    lexicographic minimum over the previous layer. [depths] is [-1] for
    unreached points, whose [distances] and [previous] are stale. A point for
    which [blocked_point] holds is never entered, an edge for which
    [blocked_edge] holds is never crossed, except that [target] (default none)
    is always enterable. With a [target] the search stops once the target's
    predecessor layer is settled. Raises {!Hop_overflow} when an accumulated
    length is not finite. Roots must be valid point indices. *)

type forest = {
  distance : float array;  (** [infinity] where unreached *)
  parent : int array;  (** a root is its own parent, unreached is [-1] *)
  edge : int array;  (** the edge into each point, [-1] for roots and unreached *)
  root : int array;  (** the root of each point's tree, [-1] when unreached *)
  order : int array;  (** the first [order_count] entries are the settle order *)
  order_count : int;
}

val forest_search :
  valid_edge:(int -> bool) ->
  capacity:int ->
  point_edge_offsets:int array ->
  point_edges:int array ->
  edge_a:int array ->
  edge_b:int array ->
  lengths:float array ->
  int array ->
  forest
(** Shortest-path forest of the roots, crossing only [valid_edge]s. The heap
    orders by (distance, root point, point); at an equal distance an unsettled
    neighbour takes the smaller root, then the smaller parent, so the parent
    is not the first settled neighbour as in {!search}. [drain_each] settles
    each root's whole component before adding the next root (the per-tree
    result is the same either way, the settle order interleaves). [capacity]
    sizes [order]; it must reach the number of settled points. *)
