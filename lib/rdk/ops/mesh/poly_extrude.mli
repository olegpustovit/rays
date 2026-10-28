type divide = Extrude_individual | Extrude_connected_components
module Int_builder :
  sig
    type t = { mutable values : int array; mutable length : int; }
    val add : t -> int -> int
  end
module Pair_table :
  sig
    type t = {
      mutable components : int array;
      mutable points : int array;
      mutable values : int array;
      mutable size : int;
    }
  end
module Edge_lookup = Rdk_core.Topology_edge_lookup
val run :
  ?cancel:Rdk_core.Cancel.t ->
  ?grain:int ->
  ?primitives:Rdk_core.Group.t ->
  ?split_edges:Rdk_core.Edge_group.t ->
  ?divide:divide ->
  ?divisions:int ->
  ?output_front:bool ->
  ?output_back:bool ->
  ?output_side:bool ->
  ?front_group:string ->
  ?back_group:string ->
  ?side_group:string ->
  ?front_boundary_group:string ->
  ?back_boundary_group:string ->
  distance:float ->
  Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Error.t) result
