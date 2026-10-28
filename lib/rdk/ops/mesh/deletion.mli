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
type delete_params = {
  cancel : Rdk_core.Cancel.t option;
  grain : int option;
  selected : bool;
  compact_points : bool;
}

val delete : delete_params -> Rdk_core.Group.t -> Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
type delete_primitives_params = {
  cancel : Rdk_core.Cancel.t option;
  grain : int option;
  compact_points : bool;
}

val delete_primitives : delete_primitives_params -> Rdk_core.Group.t -> Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result