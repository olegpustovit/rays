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
  topology : Rdk_core.Topology.Private.view;
  index : Rdk_core.Topology_index.Private.view;
  dual : dual;
}
type graph =
    Point_graph of Rdk_core.Topology_index.Private.view
  | Primitive_graph of primitive_graph
type scratch = {
  depths : int array;
  distances : float array;
  previous : int array;
  queue : int array;
}
