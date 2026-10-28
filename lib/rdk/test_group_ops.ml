open Rays
open Rdk
open Rdk_test_support

let two_quads () =
  let positions = Packed.Float3.Builder.create 6 in
  Array.iteri
    (fun point (x, y, z) -> Packed.Float3.Builder.set positions point x y z)
    [| (0., 0., 0.); (1., 0., 0.); (2., 0., 0.); (0., 1., 0.); (1., 1., 0.); (2., 1., 0.) |];
  let topology = Rdk_test_support.Tb.create ~point_count:6 () in
  Rdk_test_support.Tb.add_polygon topology [| 0; 1; 4; 3 |];
  Rdk_test_support.Tb.add_polygon topology [| 1; 2; 5; 4 |];
  Geometry.create
    ~positions:(Packed.Float3.Builder.freeze positions)
    ~topology:(Rdk_test_support.Tb.freeze topology)
    ()
  |> function
  | Ok geometry -> geometry
  | Error message -> fail message

let point_touching_triangles () =
  let positions = Packed.Float3.Builder.create 5 in
  Array.iteri
    (fun point (x, y, z) -> Packed.Float3.Builder.set positions point x y z)
    [| (0., 0., 0.); (-1., 0., 0.); (0., 1., 0.); (1., 0., 0.); (0., -1., 0.) |];
  let topology = Rdk_test_support.Tb.create ~point_count:5 () in
  Rdk_test_support.Tb.add_triangle topology 0 1 2;
  Rdk_test_support.Tb.add_triangle topology 0 3 4;
  Geometry.create
    ~positions:(Packed.Float3.Builder.freeze positions)
    ~topology:(Rdk_test_support.Tb.freeze topology)
    ()
  |> function
  | Ok geometry -> geometry
  | Error message -> fail message

let bent_strip () =
  let positions = Packed.Float3.Builder.create 8 in
  Array.iteri
    (fun point (x, y, z) -> Packed.Float3.Builder.set positions point x y z)
    [|
      (0., 0., 0.);
      (1., 0., 0.);
      (2., 0., 0.);
      (3., 0., 1.);
      (0., 1., 0.);
      (1., 1., 0.);
      (2., 1., 0.);
      (3., 1., 1.);
    |];
  let topology = Rdk_test_support.Tb.create ~point_count:8 () in
  Rdk_test_support.Tb.add_polygon topology [| 0; 1; 5; 4 |];
  Rdk_test_support.Tb.add_polygon topology [| 1; 2; 6; 5 |];
  Rdk_test_support.Tb.add_polygon topology [| 2; 3; 7; 6 |];
  Geometry.create
    ~positions:(Packed.Float3.Builder.freeze positions)
    ~topology:(Rdk_test_support.Tb.freeze topology)
    ()
  |> function
  | Ok geometry -> geometry
  | Error message -> fail message

let with_attribute owner name storage geometry =
  let attribute =
    Attribute.create_owned ~owner ~name storage |> function
    | Ok value -> value
    | Error message -> fail message
  in
  Geometry.with_attribute attribute geometry |> function
  | Ok value -> value
  | Error message -> fail message

let with_group owner name members geometry =
  let count =
    match owner with
    | Group.Point -> Geometry.point_count geometry
    | Group.Vertex -> Geometry.vertex_count geometry
    | Group.Primitive -> Geometry.primitive_count geometry
  in
  Geometry.with_group (Group.init ~grain:1 ~owner ~name count (fun index -> members index)) geometry
  |> function
  | Ok geometry -> geometry
  | Error message -> fail message

let group owner name geometry =
  match Geometry.find_group ~owner name geometry with
  | Some group -> group
  | None -> fail ("missing group " ^ name)

let edge_group name geometry =
  match Geometry.find_edge_group name geometry with
  | Some group -> group
  | None -> fail ("missing edge group " ^ name)

let int_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> (
      match Attribute.storage attribute with
      | Attribute.Int values -> values
      | _ -> fail ("non-integer attribute " ^ name))
  | None -> fail ("missing attribute " ^ name)

let same_group left right =
  Group.owner left = Group.owner right
  && Group.length left = Group.length right
  &&
  let equal = ref true in
  for index = 0 to Group.length left - 1 do
    if Group.mem index left <> Group.mem index right then equal := false
  done;
  !equal && Group.ordered_elements left = Group.ordered_elements right

let same_edge_group left right =
  Edge_group.length left = Edge_group.length right
  &&
  let equal = ref true in
  for edge = 0 to Edge_group.length left - 1 do
    if Edge_group.mem edge left <> Edge_group.mem edge right then equal := false
  done;
  !equal

let promotion_rule ?new_name ?(keep_original = false) ?(output_as_attribute = false)
    ?(mode = Group_ops.Include_any) ~source ~destination ~pattern () =
  {
    Group_ops.promotion_source = source;
    promotion_destination = destination;
    promotion_pattern = pattern;
    promotion_new_name = new_name;
    promotion_keep_original = keep_original;
    promotion_output_as_attribute = output_as_attribute;
    promotion_operation = Group_ops.Promote_elements mode;
  }

let boundary_promotion_rule ?new_name ?keep_original ?(attributes = []) ?(tolerance = 1e-6)
    ?(include_unshared_edges = false) ~source ~destination ~pattern () =
  {
    (promotion_rule ?new_name ?keep_original ~source ~destination ~pattern ()) with
    Group_ops.promotion_operation =
      Group_ops.Promote_boundary
        {
          promote_boundary_attributes = attributes;
          promote_boundary_tolerance = tolerance;
          promote_include_unshared_edges = include_unshared_edges;
          promote_all_unshared_edges = false;
          promote_all_boundary_prims = false;
        };
  }

(* One group by exact name through the rule form; an attribute output takes the
   attribute's name. *)

let run () = print_endline "group operation tests passed"
