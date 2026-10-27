open Rays
open Rdk
open Rdk_test_support

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

let same_group left right =
  Group.owner left = Group.owner right && Group.length left = Group.length right
  && members left = members right

let with_group group geometry = Geometry.with_group group geometry |> Result.get_ok

let with_int owner name values geometry =
  let attribute = Attribute.create_owned ~owner ~name (Attribute.Int values)
      |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let expect_invalid operation message = match operation () with
  | Error error -> check (Error.code error = "invalid_group") message
  | Ok _ -> fail (message ^ ": unexpectedly succeeded")

let test_points_and_base () =
  let source = Line_geometry.points (Array.init 64 (fun point ->
      float_of_int point, 0., 0.)) in
  let seed = Rays_math.Rand.seed 17 and probability = 0.37 in
  let output = Group_ops.group_random ~grain:7 ~seed ~probability
      ~owner:Group_ops.Group_points ~name:"random" source |> get_ok in
  let random = group Group.Point "random" output in
  for point = 0 to 63 do
    check (Group.mem point random
        = (Rays_math.Rand.float_at seed ~index:point < probability))
      "Group Random point indexed sample"
  done;
  let even = Group.init ~grain:1 ~owner:Group.Point ~name:"even" 64
      (fun point -> point land 1 = 0) in
  let based = source |> with_group even
      |> Group_ops.group_random ~probability:1. ~base:"even"
           ~owner:Group_ops.Group_points ~name:"based" |> get_ok in
  check (members (group Group.Point "based" based) = members even)
    "Group Random exact base restriction";
  let empty = Group_ops.group_random ~probability:0. ~owner:Group_ops.Group_points
      ~name:"none" source |> get_ok in
  check (Group.cardinality (group Group.Point "none" empty) = 0)
    "Group Random zero endpoint";
  let full = Group_ops.group_random ~probability:1. ~owner:Group_ops.Group_points
      ~name:"all" source |> get_ok in
  check (Group.cardinality (group Group.Point "all" full) = 64)
    "Group Random one endpoint"

let test_seed_attributes_and_owners () =
  let source = Plane_generators.grid ~columns:4 ~rows:3 ~size:2. () |> get_ok in
  let point_count = Geometry.point_count source
  and primitive_count = Geometry.primitive_count source in
  let point_seeds = Array.init point_count (fun point -> point / 2) in
  let point_source = with_int Attribute.Point "seed_id" point_seeds source in
  let points = Group_ops.group_random ~seed:(Rays_math.Rand.seed 31) ~seed_attribute:"seed_id"
      ~probability:0.5 ~owner:Group_ops.Group_points ~name:"points" point_source
      |> get_ok |> group Group.Point "points" in
  for point = 0 to point_count - 2 do
    if point_seeds.(point) = point_seeds.(point + 1) then
      check (Group.mem point points = Group.mem (point + 1) points)
        "Group Random equal point seed values"
  done;
  let vertices = Group_ops.group_random ~seed:(Rays_math.Rand.seed 47)
      ~seed_attribute:"seed_id" ~probability:0.5 ~owner:Group_ops.Group_vertices
      ~name:"vertices" point_source |> get_ok |> group Group.Vertex "vertices" in
  let topology = Topology.Private.view (Geometry.topology source) in
  for left = 0 to Geometry.vertex_count source - 1 do
    for right = left + 1 to Geometry.vertex_count source - 1 do
      if topology.vertex_points.(left) = topology.vertex_points.(right) then
        check (Group.mem left vertices = Group.mem right vertices)
          "Group Random vertex seed follows referenced point"
    done
  done;
  let primitive_seeds = Array.init primitive_count (fun primitive -> primitive / 3) in
  let primitive_source = with_int Attribute.Primitive "seed_id"
      primitive_seeds source in
  let primitives = Group_ops.group_random ~seed:(Rays_math.Rand.seed 53)
      ~seed_attribute:"seed_id" ~probability:0.5 ~owner:Group_ops.Group_primitives
      ~name:"primitives" primitive_source |> get_ok
      |> group Group.Primitive "primitives" in
  for primitive = 0 to primitive_count - 2 do
    if primitive_seeds.(primitive) = primitive_seeds.(primitive + 1) then
      check (Group.mem primitive primitives = Group.mem (primitive + 1) primitives)
        "Group Random equal primitive seed values"
  done;
  let edges = Group_ops.group_random ~seed:(Rays_math.Rand.seed 59) ~seed_attribute:"seed_id"
      ~probability:0.5 ~owner:Group_ops.Group_edges ~name:"edges" point_source
      |> get_ok |> edge_group "edges" in
  check (Edge_group.length edges
      = Topology_index.edge_count (Topology_index.create (Geometry.topology source)))
    "Group Random native-edge ownership";
  let curve_count = 64 and positions = Packed.Float3.Builder.create 128
  and curves = Topology.Builder.create ~point_count:128 () in
  for curve = 0 to curve_count - 1 do
    let first = curve * 2 in
    Packed.Float3.Builder.set positions first (float_of_int curve) 0. 0.;
    Packed.Float3.Builder.set positions (first + 1) (float_of_int curve) 1. 0.;
    Topology.Builder.add_open_polyline curves [|first; first + 1|]
  done;
  let curves = Geometry.create ~positions:(Packed.Float3.Builder.freeze positions)
      ~topology:(Topology.Builder.freeze curves) () |> Result.get_ok in
  let left = Array.init 128 (fun point ->
      if point land 1 = 0 then point / 2 else 10_000 + (point / 2))
  and right = Array.init 128 (fun point ->
      if point land 1 = 0 then 10_000 + (point / 2) else point / 2) in
  let left = with_int Attribute.Point "seed_id" left curves
  and right = with_int Attribute.Point "seed_id" right curves in
  let select geometry = Group_ops.group_random ~seed:(Rays_math.Rand.seed 61)
      ~seed_attribute:"seed_id" ~probability:0.5 ~owner:Group_ops.Group_edges
      ~name:"edges" geometry |> get_ok |> edge_group "edges" |> edge_members in
  check (select left = select right)
    "Group Random combines native-edge endpoint seeds symmetrically"

let test_merge_and_failures () =
  let source = Line_geometry.points (Array.init 8 (fun point ->
      float_of_int point, 0., 0.)) in
  let existing = Group.init ~grain:1 ~owner:Group.Point ~name:"selection" 8
      (fun point -> point < 2) in
  let source = with_group existing source in
  let unioned = Group_ops.group_random ~probability:0. ~merge:Group_ops.Group_union
      ~owner:Group_ops.Group_points ~name:"selection" source |> get_ok in
  check (members (group Group.Point "selection" unioned) = [0; 1])
    "Group Random union merge";
  let replaced = Group_ops.group_random ~probability:0. ~merge:Group_ops.Group_replace
      ~owner:Group_ops.Group_points ~name:"selection" source |> get_ok in
  check (Group.cardinality (group Group.Point "selection" replaced) = 0)
    "Group Random replace merge";
  let absent_intersection = Group_ops.group_random ~probability:1.
      ~merge:Group_ops.Group_intersection ~owner:Group_ops.Group_points
      ~name:"absent_intersection" source |> get_ok in
  check (Group.cardinality
      (group Group.Point "absent_intersection" absent_intersection) = 0)
    "Group Random absent destination intersection identity";
  let absent_subtract = Group_ops.group_random ~probability:1.
      ~merge:Group_ops.Group_subtract ~owner:Group_ops.Group_points
      ~name:"absent_subtract" source |> get_ok in
  check (Group.cardinality
      (group Group.Point "absent_subtract" absent_subtract) = 0)
    "Group Random absent destination subtraction identity";
  expect_invalid (fun () -> Group_ops.group_random ~probability:(-0.1)
      ~owner:Group_ops.Group_points ~name:"bad" source)
    "Group Random negative probability";
  expect_invalid (fun () -> Group_ops.group_random ~probability:Float.nan
      ~owner:Group_ops.Group_points ~name:"bad" source)
    "Group Random non-finite probability";
  expect_invalid (fun () -> Group_ops.group_random ~probability:0.5 ~base:"missing"
      ~owner:Group_ops.Group_points ~name:"bad" source)
    "Group Random missing base";
  expect_invalid (fun () -> Group_ops.group_random ~probability:0.5
      ~seed_attribute:"missing" ~owner:Group_ops.Group_points ~name:"bad" source)
    "Group Random missing seed attribute";
  let wrong = with_int Attribute.Point "ok" (Array.make 8 0) source in
  let wrong_attribute = Attribute.create_owned ~owner:Attribute.Point ~name:"wrong"
      (Attribute.Text (Array.make 8 "x")) |> Result.get_ok in
  let wrong = Geometry.with_attribute wrong_attribute wrong |> Result.get_ok in
  expect_invalid (fun () -> Group_ops.group_random ~probability:0.5
      ~seed_attribute:"wrong" ~owner:Group_ops.Group_points ~name:"bad" wrong)
    "Group Random non-integer seed attribute";
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match Group_ops.group_random ~cancel:cancelled ~probability:0.5
      ~owner:Group_ops.Group_points ~name:"bad" source with
   | Error error -> check (Error.code error = "cancelled")
       "Group Random cancellation code"
   | Ok _ -> fail "cancelled Group Random published geometry")

let test_scale_parallel_exactness () =
  let source = Plane_generators.grid ~columns:500 ~rows:300 ~size:20. () |> get_ok in
  let run domains = Parallel.run ~domains (fun () ->
    let seed = Rays_math.Rand.seed 0x514e in
    source
    |> Group_ops.group_random ~grain:1_009 ~seed ~probability:0.431
         ~owner:Group_ops.Group_points ~name:"points" |> get_ok
    |> Group_ops.group_random ~grain:1_009 ~seed ~probability:0.377
         ~owner:Group_ops.Group_vertices ~name:"vertices" |> get_ok
    |> Group_ops.group_random ~grain:1_009 ~seed ~probability:0.293
         ~owner:Group_ops.Group_primitives ~name:"primitives" |> get_ok
    |> Group_ops.group_random ~grain:1_009 ~seed ~probability:0.217
         ~owner:Group_ops.Group_edges ~name:"edges" |> get_ok) in
  let one = run 1 and four = run 4 in
  check (same_group (group Group.Point "points" one)
      (group Group.Point "points" four))
    "Group Random point one/four-domain exactness";
  let count = Geometry.point_count source in
  let selected = Group.cardinality (group Group.Point "points" one) in
  check (selected > count * 38 / 100 && selected < count * 48 / 100)
    "Group Random point selection tracks probability 0.431";
  check (same_group (group Group.Vertex "vertices" one)
      (group Group.Vertex "vertices" four))
    "Group Random vertex one/four-domain exactness";
  check (same_group (group Group.Primitive "primitives" one)
      (group Group.Primitive "primitives" four))
    "Group Random primitive one/four-domain exactness";
  check (edge_members (edge_group "edges" one)
      = edge_members (edge_group "edges" four))
    "Group Random edge one/four-domain exactness";
  let expected = ((Geometry.point_count source + 7) / 8)
      + ((Geometry.vertex_count source + 7) / 8)
      + ((Geometry.primitive_count source + 7) / 8)
      + ((Topology_index.edge_count (Topology_index.create
          (Geometry.topology source)) + 7) / 8) in
  let actual = Group.payload_bytes (group Group.Point "points" one)
      + Group.payload_bytes (group Group.Vertex "vertices" one)
      + Group.payload_bytes (group Group.Primitive "primitives" one)
      + Edge_group.payload_bytes (edge_group "edges" one) in
  check (actual = expected) "Group Random exact packed scale payload"

let test_checked_boundary () =
  let source = Line_geometry.points [|(0., 0., 0.); (1., 0., 0.)|] in
  let seed = Rays_math.Rand.seed 29 in
  let selected = Group_ops.group_random ~seed ~probability:0.5
      ~owner:Group_ops.Group_points ~name:"selected" source |> get_ok
      |> group Group.Point "selected" in
  for point = 0 to 1 do
    check (Group.mem point selected
        = (Rays_math.Rand.float_at seed ~index:point < 0.5))
      "Group Random checked boundary selection"
  done;
  let expect_error result expected_code =
    match result with
    | Error error ->
        check (Error.operation error = "group_random"
            && Error.code error = expected_code)
          "Group Random checked boundary typed error"
    | Ok _ -> fail "Group Random checked boundary accepted invalid input" in
  expect_error
    (Group_ops.group_random ~probability:(-1.)
       ~owner:Group_ops.Group_points ~name:"bad" source) "invalid_group";
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  expect_error
    (Group_ops.group_random ~cancel ~probability:0.5
       ~owner:Group_ops.Group_points ~name:"bad" source) "cancelled"

let run () =
  test_points_and_base ();
  test_seed_attributes_and_owners ();
  test_merge_and_failures ();
  test_scale_parallel_exactness ();
  test_checked_boundary ();
  print_endline "group random tests passed"
