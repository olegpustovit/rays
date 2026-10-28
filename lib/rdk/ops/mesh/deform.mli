type planes = { x : float array; y : float array; z : float array }

val validate_selection : Rdk_core.Topology.t -> Element_selection.t option -> (unit, string) result
val point_selected : Element_selection.t option -> Rdk_core.Topology_index.t option -> int -> bool
val point_vector_attribute : string -> String.t -> Rdk_core.Geometry.t -> (planes, string) result

val resolve_directions :
  ?cancel:Rdk_core.Cancel.t -> grain:int -> Rdk_core.Geometry.t -> (planes, string) result

type params = {
  cancel : Rdk_core.Cancel.t option;
  grain : int;
  primitives : Rdk_core.Group.t option;
}

val normals : params -> Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, string) result
