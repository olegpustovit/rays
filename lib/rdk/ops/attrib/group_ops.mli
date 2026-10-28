type owner = Group_points | Group_vertices | Group_primitives | Group_edges
type promote_mode = Include_any | Include_all | Include_shared_edge
type boundary_attribute = {
  boundary_attribute_owner : Rdk_core.Attribute.owner;
  boundary_attribute_pattern : string;
}
type promote_boundary_options = {
  promote_boundary_attributes : boundary_attribute list;
  promote_boundary_tolerance : float;
  promote_include_unshared_edges : bool;
  promote_all_unshared_edges : bool;
  promote_all_boundary_prims : bool;
}
and promote_operation =
    Promote_elements of promote_mode
  | Promote_boundary of promote_boundary_options
and promotion_rule = {
  promotion_source : owner;
  promotion_destination : owner;
  promotion_pattern : string;
  promotion_new_name : string option;
  promotion_keep_original : bool;
  promotion_output_as_attribute : bool;
  promotion_operation : promote_operation;
}
type boolean_operation =
    Group_replace
  | Group_union
  | Group_intersection
  | Group_subtract
  | Group_xor
type range_collision = {
  collision_owner : owner;
  collision_pattern : string;
  keep_boundary : bool;
}
type range_connectivity =
    Range_disconnected of { region : int option; }
  | Range_connected of { connectivity_attributes : string option;
      connectivity_tolerance : float; collision : range_collision option;
      region : int option; remove_other_regions : bool;
    }
type rename_conflict =
    Rename_skip
  | Rename_error
  | Rename_overwrite
  | Rename_union
type rename_rule = {
  rename_owner : owner option;
  rename_pattern : string;
  rename_replacement : string;
  rename_conflict : rename_conflict;
}
type delete_rule = { delete_owner : owner option; delete_pattern : string; }
type copy_conflict = Copy_skip | Copy_overwrite | Copy_add_suffix
type transfer_rule = {
  transfer_owner : owner;
  transfer_pattern : string;
  transfer_prefix : string;
}
type bounds =
    Bounds_box of { minimum : Rays_math.Vec3.t;
      maximum : Rays_math.Vec3.t;
    }
  | Bounds_sphere of { center : Rays_math.Vec3.t; radius : float; }
type selection =
    Ordinary of Rdk_core.Group.t
  | Native_edges of Rdk_core.Edge_group.t
type named_entry = {
  owner : owner;
  mutable entry_name : string;
  mutable value : selection;
  mutable alive : bool;
}
type group_store = {
  table : (owner * string, named_entry) Hashtbl.t;
  initial : named_entry array;
  mutable added_rev : named_entry list;
  mutable dirty : bool;
}

(* Groups-from-name spelling: nonempty ASCII letters, digits and underscore, no leading digit. *)
val valid_group_name : string -> bool
(* Every other byte becomes "_" and a leading digit gets a "_" prefix. *)
val force_valid_group_name : string -> string
(* Typed boundary with the former Ops error and cancellation behavior. *)
(* Typed boundary with the former Ops error and cancellation behavior. *)
(* Typed boundary with the former Ops error and cancellation behavior. *)
(* Typed boundary with the former Ops error and cancellation behavior. *)
val rename :
  rules:rename_rule list ->
  Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
type delete_params = {
  rules : delete_rule list;
  delete_unused : bool;
}

val delete : delete_params -> Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
type element_map =
    Identity of int
  | Explicit of int array
  | Proximity of { indices : int array; distances_squared : float array; }
type transfer_params = {
  cancel : Rdk_core.Cancel.t option;
  grain : int option;
  rules : transfer_rule list;
  conflict : copy_conflict;
  create_empty : bool;
  distance : float;
  source : Rdk_core.Geometry.t;
  target : Rdk_core.Geometry.t;
}

val transfer : transfer_params -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result