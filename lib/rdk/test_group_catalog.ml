open Rays
open Rdk
open Rdk_test_support

let get_group_ok = function Ok value -> value | Error message -> fail message

let point_cloud count =
  Line_geometry.points (Array.init count (fun point -> (float_of_int point, 0., 0.)))

let mesh primitive_points point_count =
  let positions = Packed.Float3.Builder.create point_count in
  for point = 0 to point_count - 1 do
    Packed.Float3.Builder.set positions point (float_of_int point) 0. 0.
  done;
  let topology = Rdk_test_support.Tb.create ~point_count () in
  Array.iter (Rdk_test_support.Tb.add_polygon topology) primitive_points;
  Geometry.create
    ~positions:(Packed.Float3.Builder.freeze positions)
    ~topology:(Rdk_test_support.Tb.freeze topology)
    ()
  |> function
  | Ok geometry -> geometry
  | Error message -> fail message

let two_quads () = mesh [| [| 0; 1; 4; 3 |]; [| 1; 2; 5; 4 |] |] 6

let owner_count geometry = function
  | Group.Point -> Geometry.point_count geometry
  | Group.Vertex -> Geometry.vertex_count geometry
  | Group.Primitive -> Geometry.primitive_count geometry

let with_group owner name predicate geometry =
  Geometry.with_group
    (Group.init ~grain:1 ~owner ~name (owner_count geometry owner) predicate)
    geometry
  |> function
  | Ok geometry -> geometry
  | Error message -> fail message

let with_ordered_group owner name elements geometry =
  let group =
    Group.ordered ~owner ~name ~length:(owner_count geometry owner) elements |> get_group_ok
  in
  Geometry.with_group group geometry |> function
  | Ok geometry -> geometry
  | Error message -> fail message

let with_edge_group name predicate geometry =
  let topology = Geometry.topology geometry in
  let index = Topology_index.create topology in
  Geometry.with_edge_group (Edge_group.init ~grain:1 ~topology ~index ~name predicate) geometry
  |> function
  | Ok geometry -> geometry
  | Error message -> fail message

let with_int owner name values geometry =
  let attribute =
    Attribute.create_owned ~owner ~name (Attribute.Int values) |> function
    | Ok value -> value
    | Error message -> fail message
  in
  Geometry.with_attribute attribute geometry |> function
  | Ok geometry -> geometry
  | Error message -> fail message

let with_text owner name values geometry =
  let attribute =
    Attribute.create_owned ~owner ~name (Attribute.Text values) |> function
    | Ok value -> value
    | Error message -> fail message
  in
  Geometry.with_attribute attribute geometry |> function
  | Ok geometry -> geometry
  | Error message -> fail message

let ordinary owner name geometry =
  match Geometry.find_group ~owner name geometry with
  | Some group -> group
  | None -> fail ("missing group " ^ name)

let edge name geometry =
  match Geometry.find_edge_group name geometry with
  | Some group -> group
  | None -> fail ("missing edge group " ^ name)

let group_members group =
  let members = ref [] in
  Group.iter (fun element -> members := element :: !members) group;
  List.rev !members

let ordered_group_members group =
  let members = ref [] in
  Group.iter_ordered (fun element -> members := element :: !members) group;
  List.rev !members

let edge_members group =
  let members = ref [] in
  Edge_group.iter (fun element -> members := element :: !members) group;
  List.rev !members

let expect_members expected group message = check (group_members group = expected) message

let same_group left right =
  Group.owner left = Group.owner right
  && Group.length left = Group.length right
  && group_members left = group_members right
  && Group.ordered_elements left = Group.ordered_elements right

let transfer_rule owner pattern prefix =
  { Group_ops.transfer_owner = owner; transfer_pattern = pattern; transfer_prefix = prefix }

let curve_mesh positions curves =
  let positions =
    Packed.Float3.View.of_owned_exn
      ~x:(Array.map (fun (x, _, _) -> x) positions)
      ~y:(Array.map (fun (_, y, _) -> y) positions)
      ~z:(Array.map (fun (_, _, z) -> z) positions)
  in
  let topology = Rdk_test_support.Tb.create ~point_count:(Packed.Float3.length positions) () in
  Array.iter (Rdk_test_support.Tb.add_open_polyline topology) curves;
  Geometry.create ~positions ~topology:(Rdk_test_support.Tb.freeze topology) () |> function
  | Ok geometry -> geometry
  | Error message -> fail message

let test_transfer () =
  let source =
    point_cloud 2
    |> with_group Group.Point "picked" (fun point -> point = 0)
    |> with_group Group.Point "unused" (fun point -> point = 1)
  in
  let source =
    Transform_ops_front.transform (Rays_math.Mat4.scaling (Rays_math.Vec3.create 10. 1. 1.)) source
  in
  let target = Line_geometry.points [| (0.1, 0., 0.); (9.9, 0., 0.); (5., 0., 0.) |] in
  let rules = [ transfer_rule Group_ops.Group_points "picked" "near_" ] in
  let transferred =
    Group_ops_front.transfer ~grain:1 ~distance:0.2 ~rules ~source ~target () |> get_ok
  in
  expect_members [ 0 ]
    (ordinary Group.Point "near_picked" transferred)
    "Group Transfer point proximity/threshold";
  let tied = Group_ops_front.transfer ~grain:1 ~distance:5. ~rules ~source ~target () |> get_ok in
  expect_members [ 0; 2 ]
    (ordinary Group.Point "near_picked" tied)
    "Group Transfer point equal-distance lower-index tie";
  let ordered_source =
    Line_geometry.points [| (0., 0., 0.); (10., 0., 0.) |]
    |> with_ordered_group Group.Point "path" [| 1; 0 |]
  in
  let ordered_target =
    Line_geometry.points [| (0.9, 0., 0.); (0.1, 0., 0.); (9.8, 0., 0.); (10.5, 0., 0.) |]
  in
  let ordered_transfer =
    Group_ops_front.transfer ~grain:1 ~distance:1.
      ~rules:[ transfer_rule Group_ops.Group_points "path" "mapped_" ]
      ~source:ordered_source ~target:ordered_target ()
    |> get_ok
  in
  check
    (ordered_group_members (ordinary Group.Point "mapped_path" ordered_transfer) = [ 2; 3; 1; 0 ])
    "Group Transfer orders destinations by source sequence then proximity";
  let conflict_target = target |> with_group Group.Point "near_picked" (fun point -> point = 2) in
  let skipped =
    Group_ops_front.transfer ~grain:1 ~distance:0.2 ~rules ~source ~target:conflict_target ()
    |> get_ok
  in
  expect_members [ 2 ] (ordinary Group.Point "near_picked" skipped) "Group Transfer skip conflict";
  let overwritten =
    Group_ops_front.transfer ~grain:1 ~distance:0.2 ~rules ~conflict:Group_ops.Copy_overwrite
      ~source ~target:conflict_target ()
    |> get_ok
  in
  expect_members [ 0 ]
    (ordinary Group.Point "near_picked" overwritten)
    "Group Transfer overwrite conflict";
  let suffixed =
    Group_ops_front.transfer ~grain:1 ~distance:0.2 ~rules ~conflict:Group_ops.Copy_add_suffix
      ~source ~target:conflict_target ()
    |> get_ok
  in
  expect_members [ 2 ]
    (ordinary Group.Point "near_picked" suffixed)
    "Group Transfer suffix preserves destination";
  expect_members [ 0 ]
    (ordinary Group.Point "near_picked2" suffixed)
    "Group Transfer suffix begins at two";
  let empty_rule = [ transfer_rule Group_ops.Group_points "unused" "near_" ] in
  let omitted =
    Group_ops_front.transfer ~grain:1 ~distance:0.05 ~rules:empty_rule ~source ~target () |> get_ok
  in
  check
    (Geometry.find_group ~owner:Group.Point "near_unused" omitted = None)
    "Group Transfer created an empty group by default";
  let retained =
    Group_ops_front.transfer ~grain:1 ~distance:0.05 ~rules:empty_rule ~create_empty:true ~source
      ~target ()
    |> get_ok
  in
  check
    (Group.cardinality (ordinary Group.Point "near_unused" retained) = 0)
    "Group Transfer create-empty policy";
  let no_op =
    Group_ops_front.transfer
      ~rules:[ transfer_rule Group_ops.Group_points "missing*" "mapped_" ]
      ~source ~target ()
    |> get_ok
  in
  check (no_op == target) "Group Transfer unmatched pattern rebuilt target";
  let curve_source =
    curve_mesh
      [| (-1., 0., 0.); (1., 0., 0.); (5., 0., 0.); (6., 0., 0.) |]
      [| [| 0; 1 |]; [| 2; 3 |] |]
    |> with_group Group.Primitive "crossing_curve" (fun primitive -> primitive = 0)
    |> with_edge_group "crossing_edge" (fun edge -> edge = 0)
  in
  let curve_target = curve_mesh [| (0., -1., 0.); (0., 1., 0.) |] [| [| 0; 1 |] |] in
  let curve_transferred =
    Group_ops_front.transfer ~grain:1 ~distance:0.
      ~rules:
        [
          transfer_rule Group_ops.Group_primitives "crossing_curve" "mapped_";
          transfer_rule Group_ops.Group_edges "crossing_edge" "mapped_";
        ]
      ~source:curve_source ~target:curve_target ()
    |> get_ok
  in
  expect_members [ 0 ]
    (ordinary Group.Primitive "mapped_crossing_curve" curve_transferred)
    "Group Transfer exact crossing curve primitives";
  check
    (edge_members (edge "mapped_crossing_edge" curve_transferred) = [ 0 ])
    "Group Transfer exact crossing native edges";
  let triangle_source = mesh [| [| 0; 1; 2 |]; [| 3; 4; 5 |] |] 6 in
  let triangle_positions =
    Packed.Float3.View.of_owned_exn ~x:[| 0.; 0.; 0.; -0.2; 0.2; 0. |]
      ~y:[| -1.; 1.; 0.; -0.2; -0.2; 0.2 |]
      ~z:[| -1.; -1.; 1.; 0.05; 0.05; 0.05 |]
  in
  let triangle_source =
    Geometry.with_positions triangle_positions triangle_source |> function
    | Ok geometry -> geometry
    | Error message -> fail message
  in
  let triangle_source =
    with_group Group.Primitive "intersecting" (fun primitive -> primitive = 0) triangle_source
  in
  let triangle_target = mesh [| [| 0; 1; 2 |] |] 3 in
  let target_positions =
    Packed.Float3.View.of_owned_exn ~x:[| -0.2; 0.2; 0. |] ~y:[| -0.2; -0.2; 0.2 |]
      ~z:[| 0.; 0.; 0. |]
  in
  let triangle_target =
    Geometry.with_positions target_positions triangle_target |> function
    | Ok geometry -> geometry
    | Error message -> fail message
  in
  let triangle_transferred =
    Group_ops_front.transfer ~grain:1 ~distance:0.
      ~rules:[ transfer_rule Group_ops.Group_primitives "intersecting" "mapped_" ]
      ~source:triangle_source ~target:triangle_target ()
    |> get_ok
  in
  expect_members [ 0 ]
    (ordinary Group.Primitive "mapped_intersecting" triangle_transferred)
    "Group Transfer triangle intersection proximity";
  (match Group_ops_front.transfer ~distance:(-1.) ~source ~target () with
  | Error error -> check (Error.code error = "invalid_group") "Group Transfer invalid distance code"
  | Ok _ -> fail "Group Transfer accepted a negative distance");
  (match
     Group_ops_front.transfer
       ~rules:[ transfer_rule Group_ops.Group_vertices "*" "" ]
       ~source ~target ()
   with
  | Error error ->
      check (Error.code error = "invalid_group") "Group Transfer vertex-owner rejection code"
  | Ok _ -> fail "Group Transfer accepted vertex groups");
  let degenerate = mesh [| [| 0; 1; 2 |] |] 3 |> with_group Group.Primitive "bad" (fun _ -> true) in
  (match
     Group_ops_front.transfer ~distance:1.
       ~rules:[ transfer_rule Group_ops.Group_primitives "bad" "" ]
       ~source:degenerate ~target:triangle_target ()
   with
  | Error error ->
      check (Error.code error = "invalid_group") "Group Transfer degenerate-primitive error code"
  | Ok _ -> fail "Group Transfer accepted a degenerate polygon");
  let nonfinite =
    Line_geometry.points [| (Float.nan, 0., 0.) |] |> with_group Group.Point "bad" (fun _ -> true)
  in
  (match
     Group_ops_front.transfer ~distance:1.
       ~rules:[ transfer_rule Group_ops.Group_points "bad" "" ]
       ~source:nonfinite ~target ()
   with
  | Error error ->
      check (Error.code error = "invalid_group") "Group Transfer non-finite source error code"
  | Ok _ -> fail "Group Transfer accepted a non-finite source point");
  let huge_curve =
    curve_mesh [| (-.max_float, 0., 0.); (max_float, 0., 0.) |] [| [| 0; 1 |] |]
    |> with_group Group.Primitive "huge" (fun _ -> true)
  in
  match
    Group_ops_front.transfer ~distance:1.
      ~rules:[ transfer_rule Group_ops.Group_primitives "huge" "" ]
      ~source:huge_curve ~target:huge_curve ()
  with
  | Error error ->
      check (Error.code error = "invalid_group") "Group Transfer finite-overflow diagnostic code"
  | Ok _ -> fail "Group Transfer silently accepted overflowing distance math"

let test_transfer_parallel_exactness () =
  let source = Plane_generators.grid ~columns:100 ~rows:80 ~size:20. () |> get_ok in
  let source =
    source
    |> with_group Group.Point "point_band" (fun point -> point mod 101 < 7)
    |> with_ordered_group Group.Point "ordered_seed" [| 404; 5; 8_000; 2 |]
    |> with_group Group.Primitive "face_band" (fun primitive -> primitive mod 19 < 3)
    |> with_edge_group "edge_band" (fun edge -> edge mod 23 < 2)
  in
  let target =
    Transform_ops_front.transform ~grain:257
      (Rays_math.Mat4.translation (Rays_math.Vec3.create 0.0001 0. 0.0001))
      source
  in
  let rules =
    [
      transfer_rule Group_ops.Group_points "point_band" "mapped_";
      transfer_rule Group_ops.Group_points "ordered_seed" "mapped_";
      transfer_rule Group_ops.Group_primitives "face_band" "mapped_";
      transfer_rule Group_ops.Group_edges "edge_band" "mapped_";
    ]
  in
  let run domains =
    Parallel.run ~domains (fun () ->
        Group_ops_front.transfer ~grain:257 ~distance:0.01 ~rules ~conflict:Group_ops.Copy_overwrite
          ~source ~target ()
        |> get_ok)
  in
  let one = run 1 and four = run 4 in
  check
    (same_group
       (ordinary Group.Point "mapped_point_band" one)
       (ordinary Group.Point "mapped_point_band" four))
    "Group Transfer point one/four-domain exactness";
  check
    (same_group
       (ordinary Group.Primitive "mapped_face_band" one)
       (ordinary Group.Primitive "mapped_face_band" four))
    "Group Transfer primitive one/four-domain exactness";
  check
    (same_group
       (ordinary Group.Point "mapped_ordered_seed" one)
       (ordinary Group.Point "mapped_ordered_seed" four))
    "Group Transfer ordered point one/four-domain exactness";
  check
    (edge_members (edge "mapped_edge_band" one) = edge_members (edge "mapped_edge_band" four))
    "Group Transfer edge one/four-domain exactness";
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  match Group_ops_front.transfer ~cancel:cancelled ~rules ~source ~target () with
  | Error error -> check (Error.code error = "cancelled") "Group Transfer cancellation code"
  | Ok _ -> fail "cancelled Group Transfer published geometry"

let run () =
  test_transfer ();
  test_transfer_parallel_exactness ();
  print_endline "group catalog tests passed"
