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
  promote_include_all_unshared_curve_edges : bool;
  promote_include_all_primitives_sharing_boundary_points : bool;
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
type primitive_connectivity = Primitive_share_points | Primitive_share_edges
type expand_normal_attribute = {
  expand_normal_owner : Rdk_core.Attribute.owner;
  expand_normal_name : string;
}
type expand_collision = {
  expand_collision_owner : owner;
  expand_collision_group : string;
  expand_collision_contain : bool;
  expand_collision_allow_boundary : bool;
}
type boolean_operation =
    Group_replace
  | Group_union
  | Group_intersection
  | Group_subtract
  | Group_xor
type operand = { pattern : string; inverted : bool; }
type combine_step = { operation : boolean_operation; operand : operand; }
type range =
    Range_start_end of { start : int; end_ : int; }
  | Range_from_ends of { start : int; end_offset : int; }
  | Range_start_length of { start : int; length : int; }
  | Range_partition of { partition : int; partitions : int; }
type range_filter = { select : int; of_ : int; offset : int; }
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
type range_rule = {
  range_owner : owner;
  range_name : string;
  range_base : string option;
  range_invert : bool;
  range_filter : range_filter option;
  range_connectivity : range_connectivity option;
  range_merge : boolean_operation;
  range_specification : range;
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
type copy_rule = {
  copy_owner : owner;
  copy_pattern : string;
  copy_prefix : string;
  match_attribute : string option;
}
type transfer_rule = {
  transfer_owner : owner;
  transfer_pattern : string;
  transfer_prefix : string;
}
type name_conflict = Name_replace | Name_union
type invalid_name_policy = Ignore_invalid | Force_valid
type name_overlap = First_group | Last_group | Error_on_overlap
type bounds =
    Bounds_box of { minimum : Rays_math.Vec3.t;
      maximum : Rays_math.Vec3.t;
    }
  | Bounds_sphere of { center : Rays_math.Vec3.t; radius : float; }
type containment = Fully_contained | Partially_contained
type selection =
    Ordinary of Rdk_core.Group.t
  | Native_edges of Rdk_core.Edge_group.t
type boundary_storage =
    Boundary_selection of selection
  | Boundary_position of Rdk_core.Packed.Float3.Private.view
  | Boundary_float of float array
  | Boundary_int of int array
  | Boundary_int_array of Rdk_core.Packed.Int_array.Private.view
  | Boundary_float_array of Rdk_core.Packed.Float_array.Private.view
  | Boundary_float2 of Rdk_core.Packed.Float2.Private.view
  | Boundary_float3 of Rdk_core.Packed.Float3.Private.view
  | Boundary_float4 of Rdk_core.Packed.Float4.Private.view
  | Boundary_text of string array
type boundary_plane = {
  boundary_owner : Rdk_core.Attribute.owner;
  boundary_name : string;
  boundary_storage : boundary_storage;
}
type expand_normals = {
  expand_nx : float array;
  expand_ny : float array;
  expand_nz : float array;
  expand_minimum_dot : float;
}
type expand_constraints = {
  expand_seams : Rdk_core.Edge_group.t option;
  expand_containment : selection option;
  expand_collision_boundary : selection option;
  expand_shrink_boundary : selection option;
  expand_normals : expand_normals option;
}
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
type range_connectivity_configuration = {
  range_attributes : string option;
  range_tolerance : float;
  range_collision : range_collision option;
  range_region : int option;
  range_remove_other_regions : bool;
}
val rename :
  rules:rename_rule list ->
  Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
val delete :
  rules:delete_rule list ->
  ?delete_unused:bool ->
  Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
type element_map =
    Identity of int
  | Explicit of int array
  | Proximity of { indices : int array; distances_squared : float array; }
val transfer :
  ?cancel:Rdk_core.Cancel.t ->
  ?grain:int ->
  ?rules:transfer_rule list ->
  ?conflict:copy_conflict ->
  ?create_empty:bool ->
  ?distance:float ->
  source:Rdk_core.Geometry.t ->
  target:Rdk_core.Geometry.t -> unit -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
