
type mode = Through_each | Start_end_pairs
type ending = Stop_at_end | Close_path

exception Invalid of string

type dual = {
  offsets : int array;
  edges : int array;
  edge_a : int array;
  edge_b : int array;
}

type primitive_graph = {
  topology : Topology.Private.view;
  index : Topology_index.Private.view;
  dual : dual;
}

(* The primitive graph as point/edge adjacency: one edge per two-sided
   topology edge (same edge ids), listed per primitive in vertex order. *)

type graph =
  | Point_graph of Topology_index.Private.view
  | Primitive_graph of primitive_graph

type scratch = {
  depths : int array;
  distances : float array;
  previous : int array;
  queue : int array;
}
