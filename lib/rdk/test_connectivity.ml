open Rays
open Rdk
open Rdk_test_support

let two_quads () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 1.; 2.; 0.; 1.; 2.|]
      ~y:[|0.; 0.; 0.; 1.; 1.; 1.|]
      ~z:(Array.make 6 0.) in
  let topology = Rdk_test_support.Tb.create ~point_count:6 () in
  Rdk_test_support.Tb.add_polygon topology [|0; 1; 4; 3|];
  Rdk_test_support.Tb.add_polygon topology [|1; 2; 5; 4|];
  Geometry.create ~positions ~topology:(Rdk_test_support.Tb.freeze topology) ()
  |> function Ok geometry -> geometry | Error message -> fail message

let with_group owner name predicate geometry =
  let length = match owner with
    | Group.Point -> Geometry.point_count geometry
    | Group.Vertex -> Geometry.vertex_count geometry
    | Group.Primitive -> Geometry.primitive_count geometry in
  Geometry.with_group (Group.init ~grain:1 ~owner ~name length predicate) geometry
  |> function Ok geometry -> geometry | Error message -> fail message

let shared_edge geometry =
  let topology = Geometry.topology geometry in
  let index = Topology_index.create topology in
  match Topology_index.find_edge index ~a:1 ~b:4 with
  | None -> fail "fixture has no shared edge"
  | Some shared ->
      Edge_group.init ~grain:1 ~topology ~index ~name:"seam"
        (fun edge -> edge = shared)

let with_uv name values geometry =
  let packed = Packed.Float2.of_owned
      ~x:(Array.map fst values) ~y:(Array.map snd values)
    |> function Ok packed -> packed | Error message -> fail message in
  let attribute = Attribute.create_owned ~name ~owner:Attribute.Vertex
      (Attribute.Float2 packed)
    |> function Ok attribute -> attribute | Error message -> fail message in
  Geometry.with_attribute attribute geometry
  |> function Ok geometry -> geometry | Error message -> fail message

let int_attribute owner name geometry =
  Geometry.find_attribute ~owner name geometry |> Option.get
  |> Attribute.View.storage |> (function Attribute.Int values -> values | _ -> fail "wrong int storage")

let text_attribute owner name geometry =
  Geometry.find_attribute ~owner name geometry |> Option.get
  |> Attribute.View.storage |> (function Attribute.Text values -> values | _ -> fail "wrong text storage")

let expect_error code = function
  | Error error -> check (Error.code error = code)
      (Printf.sprintf "expected error %S, got %S" code (Error.code error))
  | Ok _ -> fail ("expected error " ^ code)

let test_point_and_primitive_modes () =
  let geometry = two_quads () in
  let classes, count = Analysis.classify_connectivity
      Analysis.Connectivity_primitives geometry |> get_ok in
  check (classes = [|0; 0|] && count = 1)
    "shared-point primitive connectivity";
  let classes, count = Analysis.classify_connectivity
      Analysis.Connectivity_points geometry |> get_ok in
  check (classes = [|0; 0; 0; 0; 0; 0|] && count = 1)
    "edge-connected point connectivity";
  let masked = with_group Group.Point "without_bridge"
      (fun point -> point <> 1 && point <> 4) geometry in
  let points = Geometry.find_group ~owner:Group.Point "without_bridge" masked
      |> Option.get in
  let classes, count = Analysis.classify_connectivity ~points
      Analysis.Connectivity_points masked |> get_ok in
  check (classes = [|0; -1; 1; 0; -1; 1|] && count = 2)
    "excluded bridge points split the point graph";
  let masked = with_group Group.Primitive "right" (fun primitive -> primitive = 1)
      geometry in
  let primitives = Geometry.find_group ~owner:Group.Primitive "right" masked
      |> Option.get in
  let classes, count = Analysis.classify_connectivity ~primitives
      Analysis.Connectivity_primitives masked |> get_ok in
  check (classes = [|-1; 0|] && count = 1)
    "excluded primitives retain the minus-one sentinel"

let test_seams_and_uv_islands () =
  let geometry = two_quads () in
  let seam = shared_edge geometry in
  let classes, count = Analysis.classify_connectivity ~seams:seam
      Analysis.Connectivity_primitives geometry |> get_ok in
  check (classes = [|0; 1|] && count = 2)
    "native shared-edge seam did not split primitive islands";
  let continuous = with_uv "uv" [|
      0., 0.; 1., 0.; 1., 1.; 0., 1.;
      1., 0.; 2., 0.; 2., 1.; 1., 1.|] geometry in
  let classes, count = Analysis.classify_connectivity ~uv_attribute:"uv"
      Analysis.Connectivity_primitives continuous |> get_ok in
  check (classes = [|0; 0|] && count = 1)
    "equal endpoint UVs split a continuous island";
  let discontinuous = with_uv "uv" [|
      0., 0.; 1., 0.; 1., 1.; 0., 1.;
      3., 0.; 4., 0.; 4., 1.; 3., 1.|] geometry in
  let classes, count = Analysis.classify_connectivity ~uv_attribute:"uv"
      Analysis.Connectivity_primitives discontinuous |> get_ok in
  check (classes = [|0; 1|] && count = 2)
    "UV discontinuity did not split primitive islands"

let test_validation_and_cancellation () =
  let geometry = two_quads () in
  let point_group = Group.init ~owner:Group.Point ~name:"points" 6 (fun _ -> true)
  and wrong_length = Group.init ~owner:Group.Primitive ~name:"bad" 1 (fun _ -> true)
  and seam = shared_edge geometry in
  expect_error "invalid_connectivity"
    (Analysis.classify_connectivity ~points:point_group
       Analysis.Connectivity_primitives geometry);
  let point_classes, point_count = Analysis.classify_connectivity ~seams:seam
      Analysis.Connectivity_points geometry |> get_ok in
  check (Array.length point_classes = Geometry.point_count geometry
      && point_count = 1)
    "point seam classification did not preserve the connected remainder";
  expect_error "invalid_connectivity"
    (Analysis.classify_connectivity ~seams:seam ~uv_attribute:"uv"
       Analysis.Connectivity_primitives geometry);
  expect_error "invalid_connectivity"
    (Analysis.classify_connectivity ~primitives:wrong_length
       Analysis.Connectivity_primitives geometry);
  expect_error "invalid_connectivity"
    (Analysis.classify_connectivity ~uv_attribute:"missing"
       Analysis.Connectivity_primitives geometry);
  expect_error "invalid_parameter"
    (Analysis.classify_connectivity ~grain:0
       Analysis.Connectivity_primitives geometry);
  let unrelated = two_quads () in
  expect_error "invalid_connectivity"
    (Analysis.classify_connectivity ~seams:(shared_edge unrelated)
       Analysis.Connectivity_primitives geometry);
  let bad_uv = Attribute.create_owned ~name:"bad_uv" ~owner:Attribute.Vertex
      (Attribute.Int (Array.make 8 0))
    |> function Ok attribute -> attribute | Error message -> fail message in
  let bad_uv_geometry = Geometry.with_attribute bad_uv geometry
      |> function Ok value -> value | Error message -> fail message in
  expect_error "invalid_connectivity"
    (Analysis.classify_connectivity ~uv_attribute:"bad_uv"
       Analysis.Connectivity_primitives bad_uv_geometry);
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  expect_error "cancelled"
    (Analysis.classify_connectivity ~cancel
       Analysis.Connectivity_primitives geometry)

let run () =
  test_point_and_primitive_modes ();
  test_seams_and_uv_islands ();
  test_validation_and_cancellation ();
  print_endline "connectivity tests passed"
