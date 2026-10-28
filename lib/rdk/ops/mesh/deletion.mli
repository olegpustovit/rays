type topology_policy = Destroy_touched_primitives | Heal_primitives
type plan = {
  point_map : int array;
  vertex_map : int array;
  primitive_map : int array;
  topology : Rdk_core.Topology.t;
  healed : bool;
  point_identity : bool;
  unchanged : bool;
}
val primitive_partitions :
  piece_count:int ->
  primitive_pieces:int array ->
  Rdk_core.Geometry.t -> Rdk_core.Geometry.t array
val delete :
  ?cancel:Rdk_core.Cancel.t ->
  ?grain:int ->
  ?selected:bool ->
  ?compact_points:bool ->
  Rdk_core.Group.t ->
  Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
val delete_primitives :
  ?cancel:Rdk_core.Cancel.t ->
  ?grain:int ->
  ?compact_points:bool ->
  Rdk_core.Group.t ->
  Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
