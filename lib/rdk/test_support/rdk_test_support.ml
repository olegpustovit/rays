(* The helpers the rdk and sop tests share. A test file opens this module and keeps
   only the helpers that are its own. *)
open Rdk

let fail message = raise (Failure message)
let check condition message = if not condition then fail message
let get_ok = function Ok value -> value | Error error -> fail (Error.to_string error)
let get_rdk = function Ok value -> value | Error error -> fail (Error.to_string error)
let get = function Ok value -> value | Error message -> fail message
let get_string = function Ok value -> value | Error message -> fail message
let get_string_ok = function Ok value -> value | Error message -> fail message

let contains text pattern =
  let rec search offset =
    offset + String.length pattern <= String.length text
    && (String.sub text offset (String.length pattern) = pattern || search (offset + 1)) in
  pattern = "" || search 0

let near ?(epsilon = 1e-10) left right = abs_float (left -. right) <= epsilon

let expect_code code = function
  | Error error when String.equal (Error.code error) code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, received %s" code
      (Error.to_string error))
  | Ok _ -> fail ("expected error " ^ code)

let positions geometry = Packed.Float3.Private.view (Geometry.positions geometry)

(* [Analysis.bounds] was pruned from the library; the tests keep an independent
   bounding box of the points under its old name. *)
module Analysis = struct
  include Analysis
  type bounds = { min : Rays_math.Vec3.t; max : Rays_math.Vec3.t;
                  center : Rays_math.Vec3.t; size : Rays_math.Vec3.t }
  let bounds geometry =
    let p = positions geometry in
    if Array.length p.x = 0 then None else
    let lo a = Array.fold_left Float.min infinity a
    and hi a = Array.fold_left Float.max neg_infinity a in
    let open Rays_math in
    let min = Rays_math.Vec3.create (lo p.x) (lo p.y) (lo p.z)
    and max = Rays_math.Vec3.create (hi p.x) (hi p.y) (hi p.z) in
    Some { min; max;
      center = Rays_math.Vec3.create ((min.x +. max.x) /. 2.) ((min.y +. max.y) /. 2.)
          ((min.z +. max.z) /. 2.);
      size = Rays_math.Vec3.create (max.x -. min.x) (max.y -. min.y) (max.z -. min.z) }
end

let same_float_array left right =
  Array.length left = Array.length right
  && let same = ref true in
     for index = 0 to Array.length left - 1 do
       if Int64.bits_of_float left.(index) <> Int64.bits_of_float right.(index)
       then same := false
     done;
     !same

let compare_storage floats left right = match Attribute.storage left, Attribute.storage right with
  | Attribute.Float left, Attribute.Float right -> floats left right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.Private.view left
      and right = Packed.Float2.Private.view right in
      floats left.x right.x && floats left.y right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.Private.view left
      and right = Packed.Float3.Private.view right in
      floats left.x right.x && floats left.y right.y && floats left.z right.z
  | Attribute.Float4 left, Attribute.Float4 right ->
      let left = Packed.Float4.Private.view left
      and right = Packed.Float4.Private.view right in
      floats left.x right.x && floats left.y right.y && floats left.z right.z
      && floats left.w right.w
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.Private.view left
      and right = Packed.Int_array.Private.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.Private.view left
      and right = Packed.Float_array.Private.view right in
      left.offsets = right.offsets && floats left.values right.values
  | _ -> false

let equal_storage left right = compare_storage ( = ) left right

let equal_attribute left right =
  Attribute.owner left = Attribute.owner right
  && String.equal (Attribute.name left) (Attribute.name right)
  && String.equal (Attribute.kind_name left) (Attribute.kind_name right)
  && equal_storage left right

(* Exact authored bytes, excluding allocation IDs and derived caches. *)
let geometry_bytes geometry =
  let encode value = Marshal.to_string value [Marshal.No_sharing] in
  let attribute attribute =
    let storage = match Attribute.storage attribute with
      | Float values -> encode values | Int values -> encode values | Text values -> encode values
      | Int_array values -> encode (Packed.Int_array.Private.view values)
      | Float_array values -> encode (Packed.Float_array.Private.view values)
      | Float2 values -> let v = Packed.Float2.Private.view values in encode (v.x, v.y)
      | Float3 values -> let v = Packed.Float3.Private.view values in encode (v.x, v.y, v.z)
      | Float4 values -> let v = Packed.Float4.Private.view values in encode (v.x, v.y, v.z, v.w) in
    Attribute.owner attribute, Attribute.name attribute, Attribute.kind_name attribute, storage in
  let group group = Group.owner group, Group.name group,
    Array.init (Group.length group) (fun i -> Group.mem i group), Group.ordered_elements group in
  let edge_group group = Edge_group.name group,
    Array.init (Edge_group.length group) (fun i -> Edge_group.mem i group) in
  let p = Packed.Float3.Private.view (Geometry.positions geometry) in
  encode (p.x, p.y, p.z, Topology.Private.view (Geometry.topology geometry),
    List.map attribute (Geometry.attributes geometry), List.map group (Geometry.groups geometry),
    List.map edge_group (Geometry.edge_groups geometry))

let equal_group left right =
  Group.owner left = Group.owner right
  && String.equal (Group.name left) (Group.name right)
  && Group.length left = Group.length right
  && Group.ordered_elements left = Group.ordered_elements right
  && begin
    let equal = ref true in
    for element = 0 to Group.length left - 1 do
      if Group.mem element left <> Group.mem element right then equal := false
    done;
    !equal
  end

let equal_edge_group left right =
  String.equal (Edge_group.name left) (Edge_group.name right)
  && Edge_group.length left = Edge_group.length right
  && begin
    let equal = ref true in
    for edge = 0 to Edge_group.length left - 1 do
      if Edge_group.mem edge left <> Edge_group.mem edge right then equal := false
    done;
    !equal
  end

let compare_geometry floats left right =
  let lp = Packed.Float3.Private.view (Geometry.positions left)
  and rp = Packed.Float3.Private.view (Geometry.positions right)
  and lt = Topology.Private.view (Geometry.topology left)
  and rt = Topology.Private.view (Geometry.topology right) in
  floats lp.x rp.x && floats lp.y rp.y && floats lp.z rp.z
  && lt.point_count = rt.point_count
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && List.equal (fun left right ->
       Attribute.owner left = Attribute.owner right
       && String.equal (Attribute.name left) (Attribute.name right)
       && compare_storage floats left right)
       (Geometry.attributes left) (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)
  && List.equal equal_edge_group
       (Geometry.edge_groups left) (Geometry.edge_groups right)

let equal_geometry left right = compare_geometry ( = ) left right

(* ULPs are measured at the larger magnitude, including across binade boundaries.
   Subnormal spacing is always 2^-1074; signed zero compares equal here, while
   [geometry_bytes] remains the bit-exact self-consistency gate. *)
let close_geometry ~ulps left right =
  if ulps < 0 then invalid_arg "close_geometry: negative ulps";
  let close left right =
    left = right || (Float.is_finite left && Float.is_finite right
      && let _, exponent = Float.frexp (Float.max (abs_float left) (abs_float right)) in
         let unit = Float.ldexp 1. (max (-1074) (exponent - 53)) in
         abs_float (left -. right) /. unit <= float_of_int ulps) in
  compare_geometry (fun left right ->
    Array.length left = Array.length right && Array.for_all2 close left right) left right

(* Polygon topologies for these fixtures: the builder offers polylines only. *)
(* Primitives in the order added, polygons and polylines mixed. *)
module Tb = struct
  type t = { point_count : int; mutable primitives : (Topology.primitive_kind * int array) list }
  let create ~point_count () = { point_count; primitives = [] }
  let add kind t points = t.primitives <- (kind, points) :: t.primitives
  let add_triangle t a b c = add Topology.Polygon t [| a; b; c |]
  let add_polygon t points = add Topology.Polygon t points
  let add_open_polyline t points = add Topology.Open_polyline t points
  let add_closed_polyline t points = add Topology.Closed_polyline t points
  let freeze t =
    let primitives = List.rev t.primitives in
    let offsets = List.fold_left (fun acc (_, p) -> (List.hd acc + Array.length p) :: acc) [ 0 ] primitives in
    Topology.create_owned ~point_count:t.point_count
      ~vertex_points:(Array.concat (List.map snd primitives))
      ~primitive_offsets:(Array.of_list (List.rev offsets))
      ~primitive_kinds:(Array.of_list (List.map fst primitives)) |> Result.get_ok
end

let equal_positions left right =
  let left = Packed.Float3.Private.view (Geometry.positions left)
  and right = Packed.Float3.Private.view (Geometry.positions right) in
  left.x = right.x && left.y = right.y && left.z = right.z

let near_array expected actual =
  Array.length expected = Array.length actual
  && Array.for_all2 (fun expected actual -> abs_float (expected -. actual) <= 1e-12)
       expected actual

let float_array_values ~owner ~name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float_array values -> Packed.Float_array.Private.view values
       | _ -> fail ("unexpected non-float-array storage for " ^ name))
  | None -> fail ("missing float-array attribute " ^ name)



(* One attribute by exact name: [promote_pattern] with a literal pattern. *)
let promote ?cancel ?grain ?into ?method_ ?delete_source ?piece_attribute ?index_attribute
    ~source ~destination ~name geometry =
  Attribute_ops.promote_pattern ?cancel ?grain ?into_pattern:into ?method_ ?delete_source
    ?piece_attribute ?index_pattern:index_attribute ~source ~destination ~pattern:name geometry

(* [Triangulate2d.run] with the defaults tests rely on; the SOP declaration owns the real ones. *)
let triangulate2d ?cancel ?(grain = 16_384) ?selection ?constraint_edges
    ?constraint_primitives ?(projection = Triangulate2d.Best_fit) ?(seed = 0L)
    ?(split_crossing_constraints = false) ?(flood_from_hull_boundary = false)
    ?(remove_outside_constraint_polygons = false)
    ?(silhouette_constraints = false) ?(remove_outside_silhouette = false)
    ?(ignore_non_constraint_points = false) ?(remove_duplicate_points = false)
    ?(refine = false) ?(allow_constraint_splitting = true)
    ?(minimum_angle = Float.pi /. 9.) ?maximum_area ?target_edge_length
    ?(minimum_edge_length = 0.) ?(maximum_new_points = 100_000)
    ?(regularization_steps = 0) ?(allow_movement_of_interior_input_points = false)
    ?(preserve_point_payload = true) ?(restore_original_point_positions = true)
    ?(keep_primitives = false) ?split_point_group ?refinement_point_group
    ?triangle_group ?constraint_group ?(remove_unused_points = false)
    ?(recompute_point_normals = false) geometry =
  Triangulate2d.run ?cancel
    { grain; selection; constraint_edges; constraint_primitives; projection; seed;
      split_crossing_constraints; flood_from_hull_boundary;
      remove_outside_constraint_polygons; silhouette_constraints;
      remove_outside_silhouette; ignore_non_constraint_points;
      remove_duplicate_points; refine; allow_constraint_splitting; minimum_angle;
      maximum_area; target_edge_length; minimum_edge_length; maximum_new_points;
      regularization_steps; allow_movement_of_interior_input_points;
      preserve_point_payload; restore_original_point_positions; keep_primitives;
      split_point_group; refinement_point_group; triangle_group; constraint_group;
      remove_unused_points; recompute_point_normals }
    geometry

(* [Ray.run] with the defaults tests rely on; the SOP declaration owns the real ones. *)
let ray ?cancel ?(grain = 16_384) ?selection ?collision_primitives
    ?(method_ = Ray.Ray_project) ?(direction = Ray.Ray_normal)
    ?(direction_mode = Ray.Ray_forward) ?(surface_hit = Ray.Ray_first_surface)
    ?(samples = 1) ?(jitter_scale = 1.) ?(seed = 0) ?(combine = Ray.Ray_average)
    ?(min_distance = 0.) ?max_distance ?(tolerance = 0.) ?(scale = 1.)
    ?(lift = 0.) ?distance_attribute ?primitive_attribute
    ?source_vertex_numbers_attribute ?source_vertex_weights_attribute
    ?hit_group ?normal_attribute ?point_pattern ?vertex_pattern
    ?primitive_pattern ?detail_pattern ?(match_groups = false) ~source ~collision () =
  Ray.run ?cancel
    { grain; selection; collision_primitives; method_; direction; direction_mode;
      surface_hit; samples; jitter_scale; seed; combine; min_distance;
      max_distance; tolerance; scale; lift; distance_attribute; primitive_attribute;
      source_vertex_numbers_attribute; source_vertex_weights_attribute; hit_group;
      normal_attribute; point_pattern; vertex_pattern; primitive_pattern;
      detail_pattern; match_groups }
    ~source ~collision ()
