type operation = Dissolve_selected | Dissolve_non_selected
type bridge_policy =
    Create_bridged_polygons
  | Create_disjoint_polygons
  | Delete_bridge_polygons
module Int_buffer :
  sig
    type t = { mutable values : int array; mutable length : int; }
    val add : t -> int -> unit
  end
module Output :
  sig
    type t = {
      points : Int_buffer.t;
      vertices : Int_buffer.t;
      offsets : Int_buffer.t;
      primitives : Int_buffer.t;
      mutable kinds : Rdk_core.Topology.primitive_kind array;
      mutable primitive_count : int;
    }
    val add :
      t ->
      source:int ->
      kind:Rdk_core.Topology.primitive_kind -> int array -> int array -> unit
  end
val run :
  ?cancel:Rdk_core.Cancel.t ->
  ?grain:int ->
  ?edges:Rdk_core.Edge_group.t ->
  ?operation:operation ->
  ?bridge_policy:bridge_policy ->
  ?remove_inline_points:bool ->
  ?collinearity_tolerance:float ->
  ?remove_unused_points:bool ->
  ?create_boundary_curves:bool ->
  ?recompute_normals:bool ->
  Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
