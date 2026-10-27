open Rays
open Rdk
open Rdk_test_support

let geometry positions primitives =
  let packed = Packed.Float3.Builder.create (Array.length positions) in
  Array.iteri (fun point (x, y, z) ->
    Packed.Float3.Builder.set packed point x y z) positions;
  let topology = Rdk_test_support.Tb.create ~point_count:(Array.length positions) () in
  Array.iter (fun (kind, points) -> match kind with
    | `Polygon -> Rdk_test_support.Tb.add_polygon topology points
    | `Open -> Rdk_test_support.Tb.add_open_polyline topology points) primitives;
  Geometry.create ~positions:(Packed.Float3.Builder.freeze packed)
    ~topology:(Rdk_test_support.Tb.freeze topology) () |> Result.get_ok

let group owner name geometry = match Geometry.find_group ~owner name geometry with
  | Some group -> group
  | None -> fail ("missing group " ^ name)

let edge_group name geometry = match Geometry.find_edge_group name geometry with
  | Some group -> group
  | None -> fail ("missing edge group " ^ name)

let members group =
  let output = ref [] in
  Group.iter (fun member -> output := member :: !output) group;
  List.rev !output

let edge_members group =
  let output = ref [] in
  Edge_group.iter (fun member -> output := member :: !output) group;
  List.rev !output

let expect_members expected group message =
  check (members group = expected) message

let expect_invalid operation message = match operation () with
  | Error error -> check (Error.code error = "invalid_group") message
  | Ok _ -> fail (message ^ ": unexpectedly succeeded")

let test_primitive_geometric_normals () =
  let source = geometry
      [|(0., 0., 0.); (1., 0., 0.); (0., 1., 0.);
        (3., 0., 0.); (3., 1., 0.); (4., 0., 0.);
        (6., 0., 0.); (6., 1., 0.); (6., 0., 1.);
        (9., 0., 0.); (10., 0., 0.)|]
      [|`Polygon, [|0; 1; 2|]; `Polygon, [|3; 4; 5|];
        `Polygon, [|6; 7; 8|]; `Open, [|9; 10|]|] in
  let positive = Group_ops.group_normal ~direction:(Rays_math.Vec3.create 0. 0. 7.)
      ~spread_angle:0. ~owner:Group_ops.Group_primitives ~name:"positive" source
      |> get_ok in
  expect_members [0] (group Group.Primitive "positive" positive)
    "Group Normal primitive winding direction";
  let both = Group_ops.group_normal ~direction:(Rays_math.Vec3.create 0. 0. 1.)
      ~spread_angle:0. ~include_opposite:true ~owner:Group_ops.Group_primitives
      ~name:"both" source |> get_ok in
  expect_members [0; 1] (group Group.Primitive "both" both)
    "Group Normal opposite direction";
  let hemisphere = Group_ops.group_normal ~direction:(Rays_math.Vec3.create 0. 0. 1.)
      ~spread_angle:(Float.pi *. 0.5) ~owner:Group_ops.Group_primitives
      ~name:"hemisphere" source |> get_ok in
  expect_members [0; 2] (group Group.Primitive "hemisphere" hemisphere)
    "Group Normal inclusive right-angle boundary and curve exclusion"

let test_point_geometric_normals () =
  let source = geometry
      [|(0., 0., 0.); (1., 0., 0.); (1., 1., 0.); (0., 1., 0.);
        (4., 4., 4.)|]
      [|`Polygon, [|0; 1; 2; 3|]|] in
  let selected = Group_ops.group_normal ~direction:(Rays_math.Vec3.create 0. 0. 1.)
      ~spread_angle:0. ~owner:Group_ops.Group_points ~name:"up" source |> get_ok in
  expect_members [0; 1; 2; 3] (group Group.Point "up" selected)
    "Group Normal angle-weighted point normals and isolated point exclusion"

let float3_attribute ~owner ~name values =
  let packed = Packed.Float3.Builder.create (Array.length values) in
  Array.iteri (fun index (x, y, z) ->
    Packed.Float3.Builder.set packed index x y z) values;
  Attribute.create_owned ~owner ~name
    (Attribute.Float3 (Packed.Float3.Builder.freeze packed)) |> Result.get_ok

let test_attribute_and_edge_normals () =
  let source = geometry
      [|(0., 0., 0.); (1., 0., 0.); (2., 0., 0.); (3., 0., 0.)|]
      [|`Open, [|0; 1; 2; 3|]|] in
  let normals = float3_attribute ~owner:Attribute.Point ~name:"N"
      [|(0., 1., 0.); (0., 1., 0.); (0., -1., 0.); (0., -1., 0.)|] in
  let source = Geometry.with_attribute normals source |> Result.get_ok in
  let selected = Group_ops.group_normal ~direction:(Rays_math.Vec3.create 0. 1. 0.)
      ~spread_angle:0.
      ~owner:Group_ops.Group_edges ~name:"guided" source |> get_ok in
  check (edge_members (edge_group "guided" selected) = [0])
    "Group Normal automatically uses point N for edges";
  let geometric = Group_ops.group_normal ~use_existing_normal:false
      ~direction:(Rays_math.Vec3.create 0. 1. 0.) ~spread_angle:0.
      ~owner:Group_ops.Group_edges ~name:"geometric" source |> get_ok in
  check (edge_members (edge_group "geometric" geometric) = [])
    "Group Normal can force geometric normals instead of point N";
  let both = Group_ops.group_normal ~normal_attribute:"N"
      ~direction:(Rays_math.Vec3.create 0. 1. 0.) ~spread_angle:0.
      ~include_opposite:true ~owner:Group_ops.Group_edges ~name:"both" source
      |> get_ok in
  check (edge_members (edge_group "both" both) = [0; 2])
    "Group Normal edge opposite attribute directions";
  let primitive_source = geometry
      [|(0., 0., 0.); (1., 0., 0.); (0., 1., 0.);
        (2., 0., 0.); (3., 0., 0.); (2., 1., 0.)|]
      [|`Polygon, [|0; 1; 2|]; `Polygon, [|3; 4; 5|]|] in
  let authored = float3_attribute ~owner:Attribute.Primitive ~name:"authored"
      [|(1., 0., 0.); (0., 1., 0.)|] in
  let primitive_source = Geometry.with_attribute authored primitive_source
      |> Result.get_ok in
  let selected = Group_ops.group_normal ~normal_attribute:"authored"
      ~direction:(Rays_math.Vec3.create 1. 0. 0.) ~spread_angle:0.
      ~owner:Group_ops.Group_primitives ~name:"authored_x" primitive_source |> get_ok in
  expect_members [0] (group Group.Primitive "authored_x" selected)
    "Group Normal primitive attribute override"

let test_extreme_coordinates () =
  let magnitude = max_float /. 4. in
  let source = geometry
      [|(magnitude, magnitude, magnitude);
        (-.magnitude, magnitude, magnitude);
        (magnitude, -.magnitude, magnitude)|]
      [|`Polygon, [|0; 1; 2|]|] in
  let selected = Group_ops.group_normal ~direction:(Rays_math.Vec3.create 0. 0. 1.)
      ~spread_angle:0. ~owner:Group_ops.Group_primitives ~name:"extreme" source
      |> get_ok in
  expect_members [0] (group Group.Primitive "extreme" selected)
    "Group Normal overflow-safe extreme coordinates"

let test_base_merge_and_failures () =
  let source = Plane_generators.grid ~columns:3 ~rows:2 ~size:2. () |> get_ok in
  let base = Group.init ~grain:1 ~owner:Group.Primitive ~name:"base"
      (Geometry.primitive_count source) (fun primitive -> primitive land 1 = 0) in
  let existing = Group.init ~grain:1 ~owner:Group.Primitive ~name:"selection"
      (Geometry.primitive_count source) (fun primitive -> primitive = 3) in
  let source = Geometry.with_group base source |> Result.get_ok
      |> Geometry.with_group existing |> Result.get_ok in
  let based = Group_ops.group_normal ~base:"base" ~direction:(Rays_math.Vec3.create 0. 1. 0.)
      ~spread_angle:0. ~owner:Group_ops.Group_primitives ~name:"based" source
      |> get_ok in
  expect_members [0; 2; 4; 6; 8; 10]
    (group Group.Primitive "based" based) "Group Normal exact base restriction";
  let unioned = Group_ops.group_normal ~base:"base" ~merge:Group_ops.Group_union
      ~direction:(Rays_math.Vec3.create 0. 1. 0.) ~spread_angle:0.
      ~owner:Group_ops.Group_primitives ~name:"selection" source |> get_ok in
  expect_members [0; 2; 3; 4; 6; 8; 10]
    (group Group.Primitive "selection" unioned) "Group Normal union merge";
  let absent_intersection = Group_ops.group_normal ~merge:Group_ops.Group_intersection
      ~direction:(Rays_math.Vec3.create 0. 1. 0.) ~spread_angle:0.
      ~owner:Group_ops.Group_primitives ~name:"absent_intersection" source |> get_ok in
  check (Group.cardinality
      (group Group.Primitive "absent_intersection" absent_intersection) = 0)
    "Group Normal absent destination intersection identity";
  let absent_subtract = Group_ops.group_normal ~merge:Group_ops.Group_subtract
      ~direction:(Rays_math.Vec3.create 0. 1. 0.) ~spread_angle:0.
      ~owner:Group_ops.Group_primitives ~name:"absent_subtract" source |> get_ok in
  check (Group.cardinality
      (group Group.Primitive "absent_subtract" absent_subtract) = 0)
    "Group Normal absent destination subtraction identity";
  expect_invalid (fun () -> Group_ops.group_normal ~direction:Rays_math.Vec3.zero
      ~spread_angle:0. ~owner:Group_ops.Group_primitives ~name:"bad" source)
    "Group Normal rejects zero direction";
  expect_invalid (fun () -> Group_ops.group_normal ~direction:(Rays_math.Vec3.create 0. 1. 0.)
      ~spread_angle:(Float.pi +. 0.1) ~owner:Group_ops.Group_primitives ~name:"bad"
      source) "Group Normal rejects out-of-range spread";
  expect_invalid (fun () -> Group_ops.group_normal ~direction:(Rays_math.Vec3.create 0. 1. 0.)
      ~spread_angle:0. ~owner:Group_ops.Group_vertices ~name:"bad" source)
    "Group Normal rejects vertex owner";
  expect_invalid (fun () -> Group_ops.group_normal ~normal_attribute:"missing"
      ~direction:(Rays_math.Vec3.create 0. 1. 0.) ~spread_angle:0.
      ~owner:Group_ops.Group_points ~name:"bad" source)
    "Group Normal rejects missing attribute";
  let wrong_n_source = Line_geometry.points [|(0., 0., 0.)|] in
  let wrong_n = Attribute.create_owned ~owner:Attribute.Point ~name:"N"
      (Attribute.Int [|1|]) |> Result.get_ok in
  let wrong_n_source = Geometry.with_attribute wrong_n wrong_n_source
      |> Result.get_ok in
  expect_invalid (fun () -> Group_ops.group_normal ~direction:(Rays_math.Vec3.create 0. 1. 0.)
      ~spread_angle:0. ~owner:Group_ops.Group_points ~name:"bad" wrong_n_source)
    "Group Normal rejects malformed automatic point N";
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match Group_ops.group_normal ~cancel:cancelled ~direction:(Rays_math.Vec3.create 0. 1. 0.)
      ~spread_angle:0. ~owner:Group_ops.Group_primitives ~name:"bad" source with
   | Error error -> check (Error.code error = "cancelled")
       "Group Normal cancellation code"
   | Ok _ -> fail "cancelled Group Normal published geometry")

let same_group left right =
  Group.owner left = Group.owner right && Group.length left = Group.length right
  && members left = members right

let test_scale_parallel_exactness () =
  let source = Plane_generators.grid ~columns:500 ~rows:300 ~size:20. () |> get_ok in
  let run domains = Parallel.run ~domains (fun () ->
    source
    |> Group_ops.group_normal ~grain:1_009 ~direction:(Rays_math.Vec3.create 0. 1. 0.)
         ~spread_angle:0. ~owner:Group_ops.Group_points ~name:"points" |> get_ok
    |> Group_ops.group_normal ~grain:1_009 ~direction:(Rays_math.Vec3.create 0. 1. 0.)
         ~spread_angle:0. ~owner:Group_ops.Group_primitives ~name:"primitives" |> get_ok
    |> Group_ops.group_normal ~grain:1_009 ~direction:(Rays_math.Vec3.create 0. 1. 0.)
         ~spread_angle:0. ~owner:Group_ops.Group_edges ~name:"edges" |> get_ok) in
  let one = run 1 and four = run 4 in
  check (same_group (group Group.Point "points" one)
      (group Group.Point "points" four))
    "Group Normal point one/four-domain exactness";
  check (same_group (group Group.Primitive "primitives" one)
      (group Group.Primitive "primitives" four))
    "Group Normal primitive one/four-domain exactness";
  check (Group.cardinality (group Group.Primitive "primitives" one)
      = Geometry.primitive_count source
      && Group.cardinality (group Group.Point "points" one)
         = Geometry.point_count source)
    "Group Normal flat +Y grid selects every point and primitive";
  check (edge_members (edge_group "edges" one)
      = edge_members (edge_group "edges" four))
    "Group Normal edge one/four-domain exactness"

let run () =
  test_primitive_geometric_normals ();
  test_point_geometric_normals ();
  test_attribute_and_edge_normals ();
  test_extreme_coordinates ();
  test_base_merge_and_failures ();
  test_scale_parallel_exactness ();
  print_endline "group normal tests passed"
