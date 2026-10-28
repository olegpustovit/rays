type divide = Extrude_individual | Extrude_connected_components

module Int_builder : sig
  type t = { mutable values : int array; mutable length : int }

  val add : t -> int -> int
end

module Pair_table : sig
  type t = {
    mutable components : int array;
    mutable points : int array;
    mutable values : int array;
    mutable size : int;
  }
end

module Edge_lookup = Rdk_core.Topology_edge_lookup

type params = {
  cancel : Rdk_core.Cancel.t option;
  grain : int option;
  primitives : Rdk_core.Group.t option;
  split_edges : Rdk_core.Edge_group.t option;
  divide : divide;
  divisions : int;
  output_front : bool;
  output_back : bool;
  output_side : bool;
  front_group : string option;
  back_group : string option;
  side_group : string option;
  front_boundary_group : string option;
  back_boundary_group : string option;
  distance : float;
}

val run : params -> Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Error.t) result
