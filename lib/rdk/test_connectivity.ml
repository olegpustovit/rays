open Rays
open Rdk
open Rdk_test_support

let two_quads () =
  let positions =
    Packed.Float3.View.of_owned_exn ~x:[| 0.; 1.; 2.; 0.; 1.; 2. |] ~y:[| 0.; 0.; 0.; 1.; 1.; 1. |]
      ~z:(Array.make 6 0.)
  in
  let topology = Rdk_test_support.Tb.create ~point_count:6 () in
  Rdk_test_support.Tb.add_polygon topology [| 0; 1; 4; 3 |];
  Rdk_test_support.Tb.add_polygon topology [| 1; 2; 5; 4 |];
  Geometry.create ~positions ~topology:(Rdk_test_support.Tb.freeze topology) () |> function
  | Ok geometry -> geometry
  | Error message -> fail message

let with_group owner name predicate geometry =
  let length =
    match owner with
    | Group.Point -> Geometry.point_count geometry
    | Group.Vertex -> Geometry.vertex_count geometry
    | Group.Primitive -> Geometry.primitive_count geometry
  in
  Geometry.with_group (Group.init ~grain:1 ~owner ~name length predicate) geometry |> function
  | Ok geometry -> geometry
  | Error message -> fail message

let shared_edge geometry =
  let topology = Geometry.topology geometry in
  let index = Topology_index.create topology in
  match Topology_index.find_edge index ~a:1 ~b:4 with
  | None -> fail "fixture has no shared edge"
  | Some shared ->
      Edge_group.init ~grain:1 ~topology ~index ~name:"seam" (fun edge -> edge = shared)

let int_attribute owner name geometry =
  Geometry.find_attribute ~owner name geometry |> Option.get |> Attribute.View.storage |> function
  | Attribute.Int values -> values
  | _ -> fail "wrong int storage"

let text_attribute owner name geometry =
  Geometry.find_attribute ~owner name geometry |> Option.get |> Attribute.View.storage |> function
  | Attribute.Text values -> values
  | _ -> fail "wrong text storage"

let expect_error code = function
  | Error error ->
      check
        (Error.code error = code)
        (Printf.sprintf "expected error %S, got %S" code (Error.code error))
  | Ok _ -> fail ("expected error " ^ code)

let test_point_and_primitive_modes () =
  let geometry = two_quads () in
  let classes, count = Analysis.classify_connectivity Attribute.Primitive geometry |> get_ok in
  check (classes = [| 0; 0 |] && count = 1) "shared-point primitive connectivity";
  let classes, count = Analysis.classify_connectivity Attribute.Point geometry |> get_ok in
  check (classes = [| 0; 0; 0; 0; 0; 0 |] && count = 1) "edge-connected point connectivity";
  let masked =
    with_group Group.Point "without_bridge" (fun point -> point <> 1 && point <> 4) geometry
  in
  let points = Geometry.find_group ~owner:Group.Point "without_bridge" masked |> Option.get in
  let classes, count = Analysis.classify_connectivity ~points Attribute.Point masked |> get_ok in
  check
    (classes = [| 0; -1; 1; 0; -1; 1 |] && count = 2)
    "excluded bridge points split the point graph";
  let masked = with_group Group.Primitive "right" (fun primitive -> primitive = 1) geometry in
  let primitives = Geometry.find_group ~owner:Group.Primitive "right" masked |> Option.get in
  let classes, count =
    Analysis.classify_connectivity ~primitives Attribute.Primitive masked |> get_ok
  in
  check (classes = [| -1; 0 |] && count = 1) "excluded primitives retain the minus-one sentinel"

let test_seams () =
  let geometry = two_quads () in
  let seam = shared_edge geometry in
  let classes, count =
    Analysis.classify_connectivity ~seams:seam Attribute.Primitive geometry |> get_ok
  in
  check
    (classes = [| 0; 1 |] && count = 2)
    "native shared-edge seam did not split primitive islands"

let test_validation () =
  let geometry = two_quads () in
  let point_group = Group.init ~owner:Group.Point ~name:"points" 6 (fun _ -> true)
  and wrong_length = Group.init ~owner:Group.Primitive ~name:"bad" 1 (fun _ -> true)
  and seam = shared_edge geometry in
  expect_error "invalid_connectivity"
    (Analysis.classify_connectivity ~points:point_group Attribute.Primitive geometry);
  let point_classes, point_count =
    Analysis.classify_connectivity ~seams:seam Attribute.Point geometry |> get_ok
  in
  check
    (Array.length point_classes = Geometry.point_count geometry && point_count = 1)
    "point seam classification did not preserve the connected remainder";
  expect_error "invalid_connectivity"
    (Analysis.classify_connectivity ~primitives:wrong_length Attribute.Primitive geometry);
  let unrelated = two_quads () in
  expect_error "invalid_connectivity"
    (Analysis.classify_connectivity ~seams:(shared_edge unrelated) Attribute.Primitive geometry)

let run () =
  test_point_and_primitive_modes ();
  test_seams ();
  test_validation ();
  print_endline "connectivity tests passed"
