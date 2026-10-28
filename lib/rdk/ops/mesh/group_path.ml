


type dual = {
  offsets : int array;
  edges : int array;
  edge_a : int array;
  edge_b : int array;
}

(* The primitive graph as point/edge adjacency: one edge per two-sided
   topology edge (same edge ids), listed per primitive in vertex order. *)

