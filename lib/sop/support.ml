(* Helpers shared by the node declarations ([Groups], ...) and the
   hand-written SOPs. A node declaration is one [parameters] record
   deriving [sop_params, sop_node]: the editor schema and factory both come from that record and its input
   ports. *)

open Rays_math

type group_invert_owner = Any | Owner of Rdk.Group_ops.owner
type reverse_operation = Reverse | Shift
type sort_key = Sort_x | Sort_y | Sort_z | Sort_distance | Sort_vector
  | Sort_attribute | Sort_vertex_order | Sort_primitive_index | Sort_spatial
  | Sort_random | Sort_index_attribute | Sort_reverse | Sort_shift
type group_bounds_shape = Box | Sphere
type uv_projection = Planar | Cylindrical | Spherical
type axis_orientation = Axis_x | Axis_y | Axis_z | Axis_custom
type transport_roots = Transport_first | Transport_last | Transport_group
type kernel_mode = Kernel_explicit | Kernel_auto
let kernel_mode_parameter = Param.choice ~equal:( = )
    ["Explicit", Kernel_explicit; "Auto", Kernel_auto]
type clean_overlap = Clean_keep_first | Clean_delete_pairs | Clean_overlap_auto
type numeric_kind = Numeric_scalar | Numeric_vec2 | Numeric_vec3 | Numeric_vec4
let numeric_kind_parameter = Param.choice ~equal:( = )
    ["Scalar", Numeric_scalar; "Vector 2", Numeric_vec2; "Vector 3", Numeric_vec3; "Vector 4", Numeric_vec4]
let numeric_value kind x y z w = match kind with
  | Numeric_scalar -> Rdk.Attribute_generate.Scalar x
  | Numeric_vec2 -> Rdk.Attribute_generate.Vec2 (Rays_math.Vec2.create x y)
  | Numeric_vec3 -> Rdk.Attribute_generate.Vec3 (Rays_math.Vec3.create x y z)
  | Numeric_vec4 -> Rdk.Attribute_generate.Vec4 (x, y, z, w)
type remap_range = Remap_automatic | Remap_explicit

let decode_parameter_ramp ?(allow_single = false) name text =
  if String.trim text = "" then [] else
  let knots = String.split_on_char ',' text |> List.map (fun token ->
    match String.split_on_char ':' token with
    | [position; value] ->
        (match float_of_string_opt (String.trim position), float_of_string_opt (String.trim value) with
         | Some position, Some value when Float.is_finite position && Float.is_finite value
             && position >= 0. && position <= 1. -> position, value
         | _ -> invalid_arg (name ^ ": ramp needs finite values and positions in [0, 1]"))
    | _ -> invalid_arg (name ^ ": ramp needs comma-separated position:value knots")) in
  let rec validate previous = function
    | [] -> if previous <> 1. then invalid_arg (name ^ ": ramp must end at 1")
    | (position, _) :: rest ->
        if position <= previous then invalid_arg (name ^ ": ramp positions must increase");
        validate position rest in
  match knots with
  | [_] when allow_single -> knots
  | (position, _) :: _ when position = 0. -> validate (-1.) knots; knots
  | _ -> invalid_arg (name ^ ": ramp must start at 0")
let validate_transport_options name operation integrate_constant scale_by_edge_length =
  if (integrate_constant || scale_by_edge_length) && operation <> Rdk.Edge_transport.Transport_total then
    invalid_arg ("sop/" ^ name ^ ": constant integration and edge-length scaling require Total")
type connectivity_output = Integer | Text
type enumerate_storage = Enumerate_integer | Enumerate_text
type smoothing_mode = Laplacian | Custom
type polyframe_style = Style_first_edge | Style_two_edges | Style_centroid
  | Style_texture_uv | Style_texture_uv_gradient | Style_attribute_gradient
type group_range_mode = Start_end | From_ends | Start_length | Partition
type group_range_connectivity = No_connectivity | Disconnected | Connected
type centroid_run_mode = Detail | Primitives | Point_pieces | Primitive_pieces
type element_group = Point_group of string | Vertex_group of string
  | Primitive_group of string | Edge_group of string

(* [rays.ppx] names the generated schema and node code [Sop.X]; inside
   this library those units are the bare [X]. *)
module Sop = struct
  module Node = Node
  module Edit_graph = Edit_graph
end

let vec3_copy value = Rays_math.Vec3.create value.Rays_math.Vec3.x value.y value.z

let cooked geometry = Ok Node.Private.{ geometry; instances = None }
let rdk_error ?(hints = []) operation message =
  Error (Diagnostic.error ~code:(operation ^ "_failed") ~cause:message ~hints
    (operation ^ " could not produce valid geometry"))

let structured_rdk_error error =
  Error (Diagnostic.error ~code:(Rdk.Error.code error)
    ~cause:(Rdk.Error.to_string error) ~hints:(Rdk.Error.hints error)
    (Rdk.Error.operation error ^ " could not produce valid geometry"))

let optional_text value =
  let value = String.trim value in
  if value = "" then None else Some value

let transport_roots mode group = match mode with
  | Transport_first -> None, Rdk.Edge_transport.Transport_first_point
  | Transport_last -> None, Rdk.Edge_transport.Transport_last_point
  | Transport_group -> optional_text group, Rdk.Edge_transport.Transport_first_point

let element_owner_parameter = Param.choice ~equal:( = ) [
    "Point", Rdk.Group_ops.Group_points; "Vertex", Rdk.Group_ops.Group_vertices;
    "Primitive", Rdk.Group_ops.Group_primitives; "Edge", Rdk.Group_ops.Group_edges;
  ]

let optional_element_group owner name = match optional_text name with
  | None -> None
  | Some name -> Some (match owner with
      | Rdk.Group_ops.Group_points -> Point_group name | Rdk.Group_ops.Group_vertices -> Vertex_group name
      | Rdk.Group_ops.Group_primitives -> Primitive_group name | Rdk.Group_ops.Group_edges -> Edge_group name)

let resolve_element_group ~operation selection geometry = match selection with
  | None -> Ok None
  | Some (Point_group name) ->
      (match Rdk.Geometry.find_group ~owner:Rdk.Group.Point name geometry with
       | Some group -> Ok (Some (Rdk.Element_selection.Selected_points group))
       | None -> Error (Diagnostic.error ~code:"missing_group"
           (Printf.sprintf "%s could not find point group %S" operation name)))
  | Some (Vertex_group name) ->
      (match Rdk.Geometry.find_group ~owner:Rdk.Group.Vertex name geometry with
       | Some group -> Ok (Some (Rdk.Element_selection.Selected_vertices group))
       | None -> Error (Diagnostic.error ~code:"missing_group"
           (Printf.sprintf "%s could not find vertex group %S" operation name)))
  | Some (Primitive_group name) ->
      (match Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive name geometry with
       | Some group -> Ok (Some (Rdk.Element_selection.Selected_primitives group))
       | None -> Error (Diagnostic.error ~code:"missing_group"
           (Printf.sprintf "%s could not find primitive group %S" operation name)))
  | Some (Edge_group name) ->
      (match Rdk.Geometry.find_edge_group name geometry with
       | Some group -> Ok (Some (Rdk.Element_selection.Selected_edges group))
       | None -> Error (Diagnostic.error ~code:"missing_group"
           (Printf.sprintf "%s could not find edge group %S" operation name)))

type target_justify_mode = Justify_explicit | Justify_auto
type distance_radius_mode = Radius_fixed | Radius_maximum
let distance_radius_parameter = Param.choice ~equal:( = ) [
    "Fixed", Radius_fixed; "Maximum distance", Radius_maximum;
  ]

let group_merge_parameter = Param.choice ~equal:( = ) [
    "Replace", Rdk.Group_ops.Group_replace; "Union", Rdk.Group_ops.Group_union;
    "Intersection", Rdk.Group_ops.Group_intersection;
    "Subtract", Rdk.Group_ops.Group_subtract; "Exclusive or", Rdk.Group_ops.Group_xor;
  ]

let group_owner_parameter = Param.choice ~equal:( = ) [
    "Points", Rdk.Group_ops.Group_points; "Vertices", Rdk.Group_ops.Group_vertices;
    "Primitives", Rdk.Group_ops.Group_primitives; "Edges", Rdk.Group_ops.Group_edges;
  ]

let stable_string_hash = Rays_math.Hash.string

let mixed_seed context identity =
  let mixed = Int64.logxor (Context.seed context)
      (Int64.mul identity 0x9e3779b97f4a7c15L) in
  Int64.to_int (Int64.logxor mixed (Int64.shift_right_logical mixed 32))

let encode_table = Param.String_table.encode
let decode_table = Param.String_table.decode

let attribute_owner_token = function
  | Rdk.Attribute.Point -> "point"
  | Rdk.Attribute.Vertex -> "vertex"
  | Rdk.Attribute.Primitive -> "primitive"
  | Rdk.Attribute.Detail -> "detail"

let attribute_owner_of_token = function
  | "point" | "points" -> Ok Rdk.Attribute.Point
  | "vertex" | "vertices" -> Ok Rdk.Attribute.Vertex
  | "primitive" | "primitives" -> Ok Rdk.Attribute.Primitive
  | "detail" -> Ok Rdk.Attribute.Detail
  | token -> Error (Printf.sprintf "unknown attribute owner %S" token)

let encode_boundary_attributes attributes = encode_table (List.map
    (fun (attribute : Rdk.Group_ops.boundary_attribute) ->
      [attribute_owner_token attribute.boundary_attribute_owner;
       attribute.boundary_attribute_pattern]) attributes)

let decode_boundary_attributes text = Result.bind (decode_table text) (fun rows ->
    List.fold_left (fun result row -> Result.bind result (fun attributes ->
      match row with
      | [owner; pattern] -> Result.map (fun boundary_attribute_owner ->
          { Rdk.Group_ops.boundary_attribute_owner;
            boundary_attribute_pattern = pattern } :: attributes)
          (attribute_owner_of_token
            (String.lowercase_ascii (String.trim owner)))
      | row -> Error (Printf.sprintf
          "boundary attribute needs owner and pattern, got %d columns"
          (List.length row)))) (Ok []) rows |> Result.map List.rev)

let boundary_attributes_parameter = Param.encoded ~equal:( = )
    ~encode:encode_boundary_attributes ~decode:decode_boundary_attributes

let fuse_attribute_methods =
  let open Rdk.Fuse_rules in
  ["average", Attribute_average; "least", Attribute_least_point;
   "greatest", Attribute_greatest_point; "maximum", Attribute_maximum;
   "minimum", Attribute_minimum; "mode", Attribute_mode; "median", Attribute_median;
   "sum", Attribute_sum; "sum_squares", Attribute_sum_squares;
   "root_mean_square", Attribute_root_mean_square; "concatenate", Attribute_concatenate;
   "weighted_average", Attribute_weighted_average; "weighted_sum", Attribute_weighted_sum;
   "minimum_weight", Attribute_minimum_weight; "maximum_weight", Attribute_maximum_weight;
   "concatenate_weight_order", Attribute_concatenate_weight_order]

let fuse_group_methods =
  let open Rdk.Fuse_rules in
  ["least", Group_least_point; "greatest", Group_greatest_point;
   "union", Group_union; "intersection", Group_intersection; "most_common", Group_most_common]

let parameter_table_rows name text = match decode_table (if String.trim text = "" then "" else text) with
  | Ok rows -> rows | Error message -> invalid_arg (name ^ ": " ^ message)

let decode_parameter_weights name text = parameter_table_rows name text
  |> List.map (function
    | [text] -> (match float_of_string_opt (String.trim text) with
        | Some value when Float.is_finite value -> value
        | _ -> invalid_arg (name ^ ": weights must be finite"))
    | _ -> invalid_arg (name ^ ": weights need one column per row"))

let fuse_rule_method name choices token = match List.assoc_opt token choices with
  | Some method_ -> method_
  | None -> invalid_arg (name ^ ": unknown reduction method " ^ token)

let validate_parameter_pattern name pattern = match Rdk.Attribute_pattern.compile pattern with
  | Ok _ -> () | Error message -> invalid_arg (name ^ ": " ^ message)

let decode_fuse_attribute_rules name text = List.map (function
  | [pattern; method_; weight] ->
      validate_parameter_pattern name pattern;
      let method_ = fuse_rule_method name fuse_attribute_methods method_ in
      let weight_attribute = optional_text weight in
      let needs_weight = match method_ with
        | Rdk.Fuse_rules.Attribute_weighted_average | Attribute_weighted_sum
        | Attribute_minimum_weight | Attribute_maximum_weight | Attribute_concatenate_weight_order -> true
        | _ -> false in
      if needs_weight && weight_attribute = None then
        invalid_arg (name ^ ": weighted reduction requires a weight attribute");
      { Rdk.Fuse_rules.pattern; method_; weight_attribute }
  | _ -> invalid_arg (name ^ ": attribute rule needs pattern, method and weight columns"))
    (parameter_table_rows name text)

let decode_fuse_group_rules name text = List.map (function
  | [group_pattern; method_] ->
      validate_parameter_pattern name group_pattern;
      let group_method = fuse_rule_method name fuse_group_methods method_ in
      { Rdk.Fuse_rules.group_pattern; group_method }
  | _ -> invalid_arg (name ^ ": group rule needs pattern and method columns"))
    (parameter_table_rows name text)

let decode_curve_join_picks text =
  if String.trim text = "" then None else
  if String.trim text = "[]" then Some [||] else
  let seen = Hashtbl.create 16 in
  let picks = String.split_on_char ',' text |> List.map (fun token ->
    match String.split_on_char ':' (String.trim token) with
    | [primitive; end_] ->
        let primitive = match int_of_string_opt (String.trim primitive) with
          | Some value when value >= 0 -> value
          | _ -> invalid_arg "sop/join_curves: picked primitive must be nonnegative" in
        if Hashtbl.mem seen primitive then
          invalid_arg "sop/join_curves: duplicate picked primitive";
        Hashtbl.add seen primitive ();
        let end_ = match String.trim end_ with
          | "start" -> Rdk.Curve_join.Join_curve_start
          | "end" -> Rdk.Curve_join.Join_curve_end
          | _ -> invalid_arg "sop/join_curves: picked end must be start or end" in
        { Rdk.Curve_join.primitive; end_ }
    | _ -> invalid_arg "sop/join_curves: picks must be comma-separated primitive:start/end pairs")
      |> Array.of_list in
  Some picks


let element_attribute_owner_parameter = Param.choice ~equal:( = ) [
    "Point", Rdk.Attribute.Point; "Vertex", Rdk.Attribute.Vertex;
    "Primitive", Rdk.Attribute.Primitive;
  ]

let group_owner_token = function
  | Rdk.Group_ops.Group_points -> "point"
  | Rdk.Group_ops.Group_vertices -> "vertex"
  | Rdk.Group_ops.Group_primitives -> "primitive"
  | Rdk.Group_ops.Group_edges -> "edge"

let group_owner_of_token = function
  | "point" | "points" -> Ok Rdk.Group_ops.Group_points
  | "vertex" | "vertices" -> Ok Rdk.Group_ops.Group_vertices
  | "primitive" | "primitives" -> Ok Rdk.Group_ops.Group_primitives
  | "edge" | "edges" -> Ok Rdk.Group_ops.Group_edges
  | token -> Error (Printf.sprintf "unknown group owner %S" token)

let group_copy_conflict_parameter = Param.choice ~equal:( = ) [
    "Skip", Rdk.Group_ops.Copy_skip; "Overwrite", Rdk.Group_ops.Copy_overwrite;
    "Add suffix", Rdk.Group_ops.Copy_add_suffix;
  ]

let ordinary_group_owner_parameter = Param.choice ~equal:( = ) [
    "Points", Rdk.Group.Point; "Vertices", Rdk.Group.Vertex;
    "Primitives", Rdk.Group.Primitive;
  ]

let group_owner_key = function
  | Rdk.Group.Point -> "point"
  | Rdk.Group.Vertex -> "vertex"
  | Rdk.Group.Primitive -> "primitive"

let delete_topology_policy_parameter = Param.choice ~equal:( = ) [
    "Destroy touched primitives", Rdk.Deletion.Destroy_touched_primitives;
    "Heal primitives", Rdk.Deletion.Heal_primitives;
  ]

let uv_parameterize_seams operation input = function
  | None -> Ok (None, None)
  | Some group_name ->
      (match Rdk.Geometry.find_edge_group group_name input with
       | Some value -> Ok (Some value, None)
       | None ->
           match Rdk.Geometry.find_group ~owner:Rdk.Group.Vertex group_name input with
           | Some value -> Ok (None, Some value)
           | None -> Error (Diagnostic.error ~code:"missing_group"
               ~hints:["Create a native edge group or compatibility vertex-edge group before " ^ operation]
               (Printf.sprintf "%s could not find edge or vertex seam group %S"
                 operation group_name)))

let attribute_owner_key = function
  | Rdk.Attribute.Point -> "point"
  | Rdk.Attribute.Vertex -> "vertex"
  | Rdk.Attribute.Primitive -> "primitive"
  | Rdk.Attribute.Detail -> "detail"

let group_rename_conflict_parameter = Param.choice ~equal:( = ) [
    "Skip", Rdk.Group_ops.Rename_skip; "Error", Rdk.Group_ops.Rename_error;
    "Overwrite", Rdk.Group_ops.Rename_overwrite; "Union", Rdk.Group_ops.Rename_union;
  ]

let group_boolean_token = function
  | Rdk.Group_ops.Group_replace -> "replace"
  | Rdk.Group_ops.Group_union -> "union"
  | Rdk.Group_ops.Group_intersection -> "intersection"
  | Rdk.Group_ops.Group_subtract -> "subtract"
  | Rdk.Group_ops.Group_xor -> "xor"

let group_boolean_of_token = function
  | "replace" -> Ok Rdk.Group_ops.Group_replace
  | "union" -> Ok Rdk.Group_ops.Group_union
  | "intersection" -> Ok Rdk.Group_ops.Group_intersection
  | "subtract" -> Ok Rdk.Group_ops.Group_subtract
  | "xor" -> Ok Rdk.Group_ops.Group_xor
  | token -> Error (Printf.sprintf "unknown group operation %S" token)

let bool_token value = if value then "true" else "false"

let bool_of_token value = match String.lowercase_ascii (String.trim value) with
  | "true" | "1" | "yes" -> Ok true
  | "false" | "0" | "no" -> Ok false
  | token -> Error (Printf.sprintf "expected boolean, got %S" token)

let int_of_token value = match int_of_string_opt (String.trim value) with
  | Some value -> Ok value
  | None -> Error (Printf.sprintf "expected integer, got %S" value)

let float_of_token value = match float_of_string_opt (String.trim value) with
  | Some value when Float.is_finite value -> Ok value
  | Some _ | None -> Error (Printf.sprintf "expected finite float, got %S" value)

let group_normal_owner_parameter = Param.choice ~equal:( = ) [
    "Points", Rdk.Group_ops.Group_points; "Primitives", Rdk.Group_ops.Group_primitives;
    "Edges", Rdk.Group_ops.Group_edges;
  ]

let uv_owner_parameter = Param.choice ~equal:( = ) [
    "Point", Rdk.Attribute.Point; "Vertex", Rdk.Attribute.Vertex;
  ]

let edge_transport_direction_parameter = Param.choice ~equal:( = ) [
    "Forward", Rdk.Edge_transport.Transport_forward;
    "Backward", Rdk.Edge_transport.Transport_backward;
  ]

let edge_transport_operation_parameter = Param.choice ~equal:( = ) [
    "Transport", Rdk.Edge_transport.Transport;
    "From root", Rdk.Edge_transport.Transport_from_root;
    "Total", Rdk.Edge_transport.Transport_total;
    "Maximum", Rdk.Edge_transport.Transport_maximum;
    "Minimum", Rdk.Edge_transport.Transport_minimum;
  ]

let edge_transport_root_value_parameter = Param.choice ~equal:( = ) [
    "Zero", Rdk.Edge_transport.Transport_root_zero;
    "Hold", Rdk.Edge_transport.Transport_root_hold;
  ]

let edge_transport_normalization_parameter = Param.choice ~equal:( = ) [
    "None", Rdk.Edge_transport.Transport_no_normalization;
    "Per component", Rdk.Edge_transport.Transport_normalize_components;
    "Global", Rdk.Edge_transport.Transport_normalize_global;
  ]

let resolve_optional_primitive_group operation name geometry = match name with
  | None -> Ok None
  | Some name ->
      (match Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive name geometry with
       | Some group -> Ok (Some group)
       | None -> Error (Diagnostic.error ~code:"missing_group"
           (Printf.sprintf "%s could not find primitive group %S" operation name)))

let edge_transport_split_parameter = Param.choice ~equal:( = ) [
    "Copy", Rdk.Edge_transport.Transport_copy; "Split", Rdk.Edge_transport.Transport_split;
  ]

let edge_transport_merge_parameter = Param.choice ~equal:( = ) [
    "Add", Rdk.Edge_transport.Transport_merge_add;
    "Maximum", Rdk.Edge_transport.Transport_merge_maximum;
    "Minimum", Rdk.Edge_transport.Transport_merge_minimum;
  ]

let resolve_optional_point_group operation name geometry = match name with
  | None -> Ok None
  | Some name ->
      (match Rdk.Geometry.find_group ~owner:Rdk.Group.Point name geometry with
       | Some group -> Ok (Some group)
       | None -> Error (Diagnostic.error ~code:"missing_group"
           (Printf.sprintf "%s could not find point group %S" operation name)))

let resolve_optional_edge_group operation name geometry = match name with
  | None -> Ok None
  | Some name ->
      (match Rdk.Geometry.find_edge_group name geometry with
       | Some group -> Ok (Some group)
       | None -> Error (Diagnostic.error ~code:"missing_group"
           (Printf.sprintf "%s could not find edge group %S" operation name)))

let uv_projection_copy = function
  | Rdk.Uv_ops.Planar { origin; u_axis; v_axis } ->
      Rdk.Uv_ops.Planar {
        origin = vec3_copy origin;
        u_axis = vec3_copy u_axis;
        v_axis = vec3_copy v_axis;
      }
  | Rdk.Uv_ops.Cylindrical { origin; axis; seam; height } ->
      Rdk.Uv_ops.Cylindrical {
        origin = vec3_copy origin;
        axis = vec3_copy axis;
        seam = vec3_copy seam;
        height;
      }
  | Rdk.Uv_ops.Spherical { origin; axis; seam } ->
      Rdk.Uv_ops.Spherical {
        origin = vec3_copy origin;
        axis = vec3_copy axis;
        seam = vec3_copy seam;
      }

let attribute_owner_parameter = Param.choice ~equal:( = ) [
    "Point", Rdk.Attribute.Point; "Vertex", Rdk.Attribute.Vertex;
    "Primitive", Rdk.Attribute.Primitive; "Detail", Rdk.Attribute.Detail;
  ]

let attribute_count geometry = function
  | Rdk.Attribute.Point -> Rdk.Geometry.point_count geometry
  | Rdk.Attribute.Vertex -> Rdk.Geometry.vertex_count geometry
  | Rdk.Attribute.Primitive -> Rdk.Geometry.primitive_count geometry
  | Rdk.Attribute.Detail -> 1

let set_attribute_cook ~operation ~owner ~name make geometry =
  match make (attribute_count geometry owner) with
      | Error message -> rdk_error operation message
      | Ok storage ->
          match Rdk.Attribute.create_owned ~name ~owner storage with
          | Error message -> rdk_error operation message
          | Ok attribute ->
              match Rdk.Geometry.with_attribute attribute geometry with
              | Ok geometry -> cooked geometry
              | Error message -> rdk_error operation message

let soft_falloff_parameter = Param.choice ~equal:( = ) [
    "Linear", Rdk.Transform_ops.Soft_linear; "Quadratic", Rdk.Transform_ops.Soft_quadratic;
    "Cubic", Rdk.Transform_ops.Soft_cubic;
  ]

let transport_roots_parameter = Param.choice ~equal:( = ) [
    "First point", Transport_first; "Last point", Transport_last;
    "Root group", Transport_group;
  ]

let attribute_group_owner = function
  | Rdk.Attribute.Point -> Some Rdk.Group.Point
  | Rdk.Attribute.Vertex -> Some Rdk.Group.Vertex
  | Rdk.Attribute.Primitive -> Some Rdk.Group.Primitive
  | Rdk.Attribute.Detail -> None

let resolve_attribute_group ~operation ~owner name geometry = match name with
  | None -> Ok None
  | Some _ when owner = Rdk.Attribute.Detail ->
      Error (Diagnostic.error ~code:"invalid_group"
        (operation ^ " does not accept a group for detail attributes"))
  | Some name ->
      let group_owner = Option.get (attribute_group_owner owner) in
      (match Rdk.Geometry.find_group ~owner:group_owner name geometry with
       | Some group -> Ok (Some group)
       | None -> Error (Diagnostic.error ~code:"missing_group"
           (Printf.sprintf "%s could not find %s group %S" operation
              (attribute_owner_key owner) name)))

type mirror_method = Mirror_plane | Mirror_mapping
type mirror_transform = Mirror_copy | Mirror_uv | Mirror_vector | Mirror_point

let attribute_mirror_group_owner = function
  | Rdk.Attribute.Point -> Rdk.Group.Point
  | Rdk.Attribute.Vertex -> Rdk.Group.Vertex
  | Rdk.Attribute.Primitive -> Rdk.Group.Primitive
  | Rdk.Attribute.Detail -> invalid_arg "Attribute Mirror: detail attributes cannot be mirrored"

type velocity_initialization = Velocity_compute | Velocity_keep | Velocity_set | Velocity_from_attribute

type spiral_extent_mode = Spiral_turns_height | Spiral_height_pitch
type spiral_radius_mode = Spiral_archimedean_change | Spiral_archimedean_end | Spiral_logarithmic_change | Spiral_logarithmic_end
type spiral_divisions_mode = Spiral_per_curve | Spiral_per_turn

type circle_arc = Circle_closed | Circle_open | Circle_chord | Circle_sliced
type plane_orientation = Plane_xy | Plane_xz | Plane_yz | Plane_axes
let validate_plane_axes operation horizontal vertical =
  let normalize value =
    let scale = Float.max (Float.abs value.Rays_math.Vec3.x) (Float.max (Float.abs value.y) (Float.abs value.z)) in
    if scale = 0. then invalid_arg (operation ^ ": custom axes must be nonzero and independent");
    let x = value.x /. scale and y = value.y /. scale and z = value.z /. scale in
    let length = sqrt (x *. x +. y *. y +. z *. z) in
    Rays_math.Vec3.create (x /. length) (y /. length) (z /. length) in
  let horizontal = normalize horizontal and vertical = normalize vertical in
  let projection = Rays_math.Vec3.dot vertical horizontal in
  ignore (normalize (Rays_math.Vec3.create (vertical.x -. projection *. horizontal.x)
    (vertical.y -. projection *. horizontal.y) (vertical.z -. projection *. horizontal.z)))

type blast_attribute_mode = Blast_below | Blast_range | Blast_width
type blast_attribute_output = Blast_delete | Blast_group


type facet_consolidation = Consolidation_none | Consolidation_points | Consolidation_normals
let facet_consolidation_parameter = Param.choice ~equal:( = ) [
  "None", Consolidation_none; "Points", Consolidation_points; "Normals", Consolidation_normals]

type transfer_mode = Transfer_nearest | Transfer_inverse | Transfer_links
  | Transfer_renderman | Transfer_hart
let transfer_mode_parameter = Param.choice ~equal:( = ) [
    "Nearest", Transfer_nearest; "Inverse distance", Transfer_inverse;
    "Links kernel", Transfer_links; "RenderMan kernel", Transfer_renderman;
    "Hart kernel", Transfer_hart;
  ]
type transfer_falloff = Transfer_linear | Transfer_smoothstep | Transfer_uniform
let transfer_falloff_parameter = Param.choice ~equal:( = ) [
    "Linear", Transfer_linear; "Smoothstep", Transfer_smoothstep;
    "Uniform", Transfer_uniform;
  ]
let transfer_unmatched_parameter = Param.choice ~equal:( = ) [
    "Keep target", Rdk.Attribute_transfer.Keep_target;
    "Default value", Rdk.Attribute_transfer.Default_value;
  ]
let transfer_mode mode neighbors power radius = match mode with
  | Transfer_nearest -> Rdk.Attribute_transfer.Nearest
  | Transfer_inverse -> Rdk.Attribute_transfer.Inverse_distance { neighbors; power }
  | Transfer_links -> Rdk.Attribute_transfer.Kernel {
      neighbors; radius; kernel = Rdk.Attribute_transfer.Links }
  | Transfer_renderman -> Rdk.Attribute_transfer.Kernel {
      neighbors; radius; kernel = Rdk.Attribute_transfer.RenderMan }
  | Transfer_hart -> Rdk.Attribute_transfer.Kernel {
      neighbors; radius; kernel = Rdk.Attribute_transfer.Hart }
let transfer_falloff falloff bias = match falloff with
  | Transfer_linear -> Rdk.Attribute_transfer.Linear
  | Transfer_smoothstep -> Rdk.Attribute_transfer.Smoothstep
  | Transfer_uniform -> Rdk.Attribute_transfer.Uniform bias

type extract_point_cut = Extract_point_constant | Extract_point_primitive_attribute | Extract_point_current_time

type point_generation_mode = Point_generate_total | Point_generate_per_point | Point_generate_probability

type poly_cut_detection = Cut_all | Cut_crossing | Cut_change

type poly_reduce_target = Reduce_ratio | Reduce_primitive_count

type soft_transform_metric = Soft_radius | Soft_edge | Soft_attribute

let exact_or_pattern exact pattern = match optional_text pattern with
  | Some pattern -> None, Some pattern | None -> optional_text exact, None

let decode_surface_attributes text = List.map (function
  | [owner; source_name; target_name] ->
      let source_owner = match attribute_owner_of_token (String.lowercase_ascii (String.trim owner)) with
        | Ok owner -> owner | Error message -> invalid_arg ("sop/attribute_transfer_surface: " ^ message) in
      if source_owner = Rdk.Attribute.Detail then invalid_arg "sop/attribute_transfer_surface: detail source is not spatial";
      if String.trim source_name = "" || String.trim target_name = "" then
        invalid_arg "sop/attribute_transfer_surface: attribute names must be nonblank";
      if source_owner = Rdk.Attribute.Point && source_name = "P" then
        invalid_arg "sop/attribute_transfer_surface: source P is not an ordinary attribute";
      { Rdk.Attribute_transfer.source_owner; source_name; target_name }
  | _ -> invalid_arg "sop/attribute_transfer_surface: attributes need owner, source and target columns")
    (parameter_table_rows "sop/attribute_transfer_surface" text)

let group_length owner geometry = match owner with
  | Rdk.Group.Point -> Rdk.Geometry.point_count geometry
  | Rdk.Group.Vertex -> Rdk.Geometry.vertex_count geometry
  | Rdk.Group.Primitive -> Rdk.Geometry.primitive_count geometry

let compile_transfer_group_pattern operation label = function
  | None -> None
  | Some pattern ->
      match Rdk.Attribute_pattern.compile pattern with
      | Ok compiled -> Some compiled
      | Error message -> invalid_arg
          (Printf.sprintf "sop/%s: invalid %s group pattern: %s"
             operation label message)

let resolve_transfer_group ~operation ~owner ~owner_name ~exact ~pattern
    ~cancel ~grain geometry =
  match exact, pattern with
  | Some _, Some _ -> invalid_arg
      (Printf.sprintf "sop/%s: exact and patterned %s groups are mutually exclusive"
         operation owner_name)
  | Some name, None ->
      (match Rdk.Geometry.find_group ~owner name geometry with
       | Some group -> Ok (Some group)
       | None -> Error (Diagnostic.error ~code:"missing_group"
           (Printf.sprintf "%s could not find %s group %S"
              operation owner_name name)))
  | None, None -> Ok None
  | None, Some pattern ->
      Rdk.Cancel.check cancel;
      let matches = Rdk.Geometry.groups geometry
          |> List.filter (fun group -> Rdk.Group.owner group = owner
            && Rdk.Attribute_pattern.matches pattern (Rdk.Group.name group)) in
      let selection = match matches with
        | [] -> Ok (Rdk.Group.init ~grain ~owner
            ~name:"__attribute_transfer_empty" (group_length owner geometry)
            (fun _ -> false))
        | [group] -> Ok group
        | groups -> Rdk.Group.union_many ~cancel ~grain
            ~name:"__attribute_transfer_union" groups in
      Result.map_error (fun message -> Diagnostic.error ~code:"invalid_group"
        (operation ^ ": " ^ message)) selection
      |> Result.map Option.some

type copy_match = Copy_cyclic | Copy_by_values | Copy_to_element
let decode_copy_rules text =
  let rules = List.map (function
    | [owner; copy_pattern; into] ->
        let copy_owner = match attribute_owner_of_token (String.lowercase_ascii (String.trim owner)) with
          | Ok owner -> owner | Error message -> invalid_arg ("sop/attribute_copy: " ^ message) in
        let copy_into = optional_text into in
        (match copy_into with
         | None -> validate_parameter_pattern "sop/attribute_copy" copy_pattern
         | Some replacement -> match Rdk.Attribute_pattern.compile_rewrite ~pattern:copy_pattern ~replacement with
             | Ok _ -> () | Error message -> invalid_arg ("sop/attribute_copy: " ^ message));
        {Rdk.Attribute_copy.copy_owner;copy_pattern;copy_into}
    | _ -> invalid_arg "sop/attribute_copy: rules need owner, pattern and destination columns")
      (parameter_table_rows "sop/attribute_copy" text) in
  if rules = [] then invalid_arg "sop/attribute_copy: at least one copy rule is required";
  rules

type ray_direction = Direction_vector | Direction_normal | Direction_attribute

type poly_bevel_shape = Poly_chamfer | Poly_round

type fuse_targeting = Fuse_near_points | Fuse_specified_points

type subdivision_cracks = Cracks_do_not_close | Cracks_pull_no_division | Cracks_pull_divide | Cracks_pull_triangulate
  | Cracks_stitch_no_division | Cracks_stitch_divide | Cracks_stitch_triangulate

type boolean_closed_policy = Closed_default | Closed_required | Closed_not_required

type triangulate_2d_projection = Triangulate_best_fit | Triangulate_xy | Triangulate_yz | Triangulate_zx | Triangulate_plane | Triangulate_attribute

module Quaternion_noise_encoding = struct
  let encode_location = function
    | Rdk.Attribute_generate.Noise_position -> "position"
    | Noise_element_number -> "element-number"
    | Noise_attribute name -> "attribute:" ^ name

  let decode_location = function
    | "position" -> Ok Rdk.Attribute_generate.Noise_position
    | "element-number" -> Ok Noise_element_number
    | value when String.starts_with ~prefix:"attribute:" value ->
        Ok (Noise_attribute (String.sub value 10 (String.length value - 10)))
    | _ -> Error "location must be position, element-number, or attribute:name"

  let encode_numeric value = String.concat "," (match value with
    | Rdk.Attribute_generate.Scalar x -> ["scalar"; Param.float_text x]
    | Vec2 value -> ["vec2"; Param.float_text value.Rays_math.Vec2.x;
        Param.float_text value.y]
    | Vec3 value -> ["vec3"; Param.float_text value.Rays_math.Vec3.x;
        Param.float_text value.y; Param.float_text value.z]
    | Vec4 (x, y, z, w) -> ["vec4"; Param.float_text x;
        Param.float_text y; Param.float_text z; Param.float_text w])

  let decode_numeric value = match String.split_on_char ',' value with
    | ["scalar"; x] -> Option.map (fun x -> Rdk.Attribute_generate.Scalar x)
        (float_of_string_opt x)
    | ["vec2"; x; y] ->
        (match float_of_string_opt x, float_of_string_opt y with
         | Some x, Some y -> Some (Vec2 (Rays_math.Vec2.create x y)) | _ -> None)
    | ["vec3"; x; y; z] ->
        (match float_of_string_opt x, float_of_string_opt y,
            float_of_string_opt z with
         | Some x, Some y, Some z -> Some (Vec3 (Rays_math.Vec3.create x y z))
         | _ -> None)
    | ["vec4"; x; y; z; w] ->
        (match float_of_string_opt x, float_of_string_opt y,
            float_of_string_opt z, float_of_string_opt w with
         | Some x, Some y, Some z, Some w -> Some (Vec4 (x, y, z, w))
         | _ -> None)
    | _ -> None

  let encode_range = function
    | Rdk.Attribute_generate.Noise_positive -> "positive"
    | Noise_zero_centered -> "zero-centered"
    | Noise_min_max (minimum, maximum) ->
        String.concat ";" ["min-max"; encode_numeric minimum;
          encode_numeric maximum]

  let decode_range value = match String.split_on_char ';' value with
    | ["positive"] -> Ok Rdk.Attribute_generate.Noise_positive
    | ["zero-centered"] -> Ok Noise_zero_centered
    | ["min-max"; minimum; maximum] ->
        (match decode_numeric minimum, decode_numeric maximum with
         | Some minimum, Some maximum -> Ok (Noise_min_max (minimum, maximum))
         | _ -> Error "invalid noise range bounds")
    | _ -> Error "range must be positive, zero-centered, or min-max bounds"

end

type transform_mode = Transform_trs | Transform_matrix

type noise_location = Noise_position | Noise_element_number | Noise_attribute
type noise_range = Noise_positive | Noise_zero_centered | Noise_min_max

type random_distribution =
  Random_constant | Random_two_values | Random_uniform | Random_uniform_discrete
  | Random_normal | Random_exponential | Random_log_normal | Random_cauchy
  | Random_direction | Random_inside_sphere | Random_inside_sphere_cone
  | Random_custom_ramp | Random_custom_discrete | Random_custom_discrete_text

let random_distribution_value ~distribution ~kind ~a ~a_w ~b ~b_w ~step ~step_w
    ~probability_b ~cone_angle ~dimensions ~ramp ~entries ~text_entries =
  let number value w = numeric_value kind value.Rays_math.Vec3.x value.y value.z w in
  let a = number a a_w and b = number b b_w and step = number step step_w in
  let finite text = match float_of_string_opt text with
    | Some value when Float.is_finite value -> value
    | _ -> invalid_arg "sop/attribute_randomize: table values must be finite numbers" in
  let rows text = match decode_table text with
    | Ok rows -> rows | Error message -> invalid_arg ("sop/attribute_randomize: " ^ message) in
  match distribution with
  | Random_constant -> Rdk.Attribute_generate.Random_constant a
  | Random_two_values -> Random_two_values {a;b;probability_b}
  | Random_uniform -> Random_uniform {min=a;max=b}
  | Random_uniform_discrete -> Random_uniform_discrete {min=a;max=b;step}
  | Random_normal -> Random_normal {middle=a;scale=b}
  | Random_exponential -> Random_exponential {median=a}
  | Random_log_normal -> Random_log_normal {median=a;stddev=b}
  | Random_cauchy -> Random_cauchy {median=a;scale=b}
  | Random_direction -> Random_direction {direction=a;cone_angle}
  | Random_inside_sphere -> Random_inside_sphere {dimensions}
  | Random_inside_sphere_cone -> Random_inside_sphere_cone {direction=a;cone_angle}
  | Random_custom_ramp -> Random_custom_ramp {
      ramp=decode_parameter_ramp "sop/attribute_randomize" ramp;fit_min=a;fit_max=b}
  | Random_custom_discrete -> Random_custom_discrete (rows entries |> List.map (fun row ->
      match List.rev row with
      | weight :: components ->
          let components = List.rev components |> List.map finite in
          let value = match kind,components with
            | Numeric_scalar,[x] -> Rdk.Attribute_generate.Scalar x
            | Numeric_vec2,[x;y] -> Vec2 (Rays_math.Vec2.create x y)
            | Numeric_vec3,[x;y;z] -> Vec3 (Rays_math.Vec3.create x y z)
            | Numeric_vec4,[x;y;z;w] -> Vec4 (x,y,z,w)
            | _ -> invalid_arg "sop/attribute_randomize: table dimension does not match value type" in
          value,finite weight
      | _ -> invalid_arg "sop/attribute_randomize: each table row needs values and weight"))
  | Random_custom_discrete_text -> Random_custom_discrete_text (rows text_entries |> List.map (function
      | [text;weight] -> text,finite weight
      | _ -> invalid_arg "sop/attribute_randomize: text table rows need text and weight"))

let validate_random_distribution ~direction_bias distribution =
  let refuse message = invalid_arg ("sop/attribute_randomize: " ^ message) in
  let components = function
    | Rdk.Attribute_generate.Scalar x -> [x]
    | Vec2 v -> [v.Rays_math.Vec2.x;v.y]
    | Vec3 v -> [v.Rays_math.Vec3.x;v.y;v.z]
    | Vec4 (x,y,z,w) -> [x;y;z;w] in
  let all predicate value = List.for_all predicate (components value) in
  let positive value = all (fun x -> x > 0.) value
  and nonnegative value = all (fun x -> x >= 0.) value in
  let ordered a b = List.for_all2 ( <= ) (components a) (components b) in
  let direction ~orientation direction cone_angle =
    let values = components direction in
    let dimension = List.length values in
    if dimension < 2 || List.for_all (( = ) 0.) values then refuse "direction requires a nonzero vector";
    let maximum = if orientation && dimension = 4 then 2. *. Float.pi else Float.pi in
    if cone_angle < 0. || cone_angle > maximum then refuse "cone angle is outside its valid range" in
  let weights entries =
    if entries = [] then refuse "custom entries must not be empty";
    let total = List.fold_left (fun total (_,weight) ->
      if not (Float.is_finite weight) || weight < 0. then refuse "weights must be finite and nonnegative";
      total +. weight) 0. entries in
    if not (Float.is_finite total) || total <= 0. then refuse "weight sum must be finite and positive" in
  if direction_bias <= -1. then refuse "direction bias must be greater than -1";
  (match distribution with
   | Rdk.Attribute_generate.Random_direction _ | Random_inside_sphere_cone _ -> ()
   | _ -> if direction_bias <> 0. then refuse "direction bias requires Direction or Inside sphere cone");
  match distribution with
  | Rdk.Attribute_generate.Random_constant _ -> ()
  | Random_two_values {probability_b;_} ->
      if probability_b < 0. || probability_b > 1. then refuse "probability must be in [0,1]"
  | Random_uniform {min;max} -> if not (ordered min max) then refuse "bounds must be ordered"
  | Random_uniform_discrete {min;max;step} ->
      if not (ordered min max && positive step) then refuse "bounds must be ordered and steps positive"
  | Random_normal {scale;_} -> if not (nonnegative scale) then refuse "scale must be nonnegative"
  | Random_exponential {median} -> if not (positive median) then refuse "median must be positive"
  | Random_log_normal {median;stddev} ->
      if not (positive median && nonnegative stddev) then refuse "median must be positive and deviation nonnegative";
      if not (List.for_all2 (fun median deviation ->
        let ratio = deviation *. deviation /. (median *. median) in
        Float.is_finite (if deviation = 0. then 0. else sqrt (log (0.5 *. (1. +. sqrt (1. +. 4. *. ratio))))))
        (components median) (components stddev)) then refuse "log-normal scale overflowed"
  | Random_cauchy {scale;_} ->
      if not (nonnegative scale) then refuse "scale must be nonnegative";
      let values = components scale in
      if List.exists (fun value -> value <> List.hd values) values then refuse "multidimensional Cauchy scale must be isotropic"
  | Random_direction {direction=axis;cone_angle} -> direction ~orientation:true axis cone_angle
  | Random_inside_sphere {dimensions} -> if dimensions < 2 || dimensions > 4 then refuse "sphere dimensions must be in [2,4]"
  | Random_inside_sphere_cone {direction=axis;cone_angle} -> direction ~orientation:false axis cone_angle
  | Random_custom_ramp {ramp;_} -> if List.length ramp < 2 then refuse "ramp needs at least two knots"
  | Random_custom_discrete entries -> weights entries
  | Random_custom_discrete_text entries -> weights entries

type interpolate_driver = Interpolate_primitive_uvw | Interpolate_point_weights
  | Interpolate_vertex_weights | Interpolate_primitive_weights
