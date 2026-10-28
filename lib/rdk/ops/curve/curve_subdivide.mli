type scheme = Catmull_clark | Bilinear

type plan = {
  source : Rdk_core.Geometry.t;
  output_topology : Rdk_core.Topology.t;
  output_index : Rdk_core.Topology_index.t option;
  point_offsets : int array;
  point_sources : int array;
  point_weights : float array;
  point_representative : int array;
  point_old : bytes;
  vertex_left : int array;
  vertex_right : int array;
  vertex_weight : float array;
  vertex_old : bytes;
  primitive_source : int array;
  source_edge_of_output_vertex : int array;
}

val iterate :
  ?cancel:Rdk_core.Cancel.t ->
  ?grain:int ->
  independent:bool ->
  scheme ->
  int ->
  Rdk_core.Geometry.t ->
  Rdk_core.Geometry.t
