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

let with_group group geometry = Geometry.with_group group geometry |> Result.get_ok

let expect_invalid operation message = match operation () with
  | Error error -> check (Error.code error = "invalid_group") message
  | Ok _ -> fail (message ^ ": unexpectedly succeeded")

let box minimum maximum = Group_ops.Bounds_box { minimum; maximum }
let sphere center radius = Group_ops.Bounds_sphere { center; radius }

let test_point_box_and_sphere () =
  let source = Line_geometry.points
      [|(-1., 0., 0.); (0., 0., 0.); (1., 0., 0.); (2., 0., 0.)|] in
  let bounds = box (Rays_math.Vec3.create (-1.) (-0.1) (-0.1))
      (Rays_math.Vec3.create 1. 0.1 0.1) in
  let boxed = Group_ops.group_bounds bounds ~owner:Group_ops.Group_points ~name:"boxed"
      source |> get_ok in
  expect_members [0; 1; 2] (group Group.Point "boxed" boxed)
    "Group Bounds inclusive point box";
  let spherical = Group_ops.group_bounds (sphere Rays_math.Vec3.zero 1.)
      ~owner:Group_ops.Group_points ~name:"spherical" source |> get_ok in
  expect_members [0; 1; 2] (group Group.Point "spherical" spherical)
    "Group Bounds inclusive point sphere"

let test_vertex_and_primitive_containment () =
  let source = geometry
      [|(-2., 0., 0.); (0., 0., 0.); (2., 0., 0.);
        (-0.25, 0., 0.); (0.25, 0., 0.); (0., 0.25, 0.)|]
      [|`Polygon, [|0; 1; 2|]; `Polygon, [|3; 4; 5|]|] in
  let bounds = box (Rays_math.Vec3.create (-0.5) (-0.5) (-0.5))
      (Rays_math.Vec3.create 0.5 0.5 0.5) in
  let vertices = Group_ops.group_bounds bounds ~owner:Group_ops.Group_vertices
      ~name:"vertices" source |> get_ok in
  expect_members [1; 3; 4; 5] (group Group.Vertex "vertices" vertices)
    "Group Bounds vertex position ownership";
  let full = Group_ops.group_bounds bounds ~containment:Group_ops.Fully_contained
      ~owner:Group_ops.Group_primitives ~name:"full" source |> get_ok in
  expect_members [1] (group Group.Primitive "full" full)
    "Group Bounds full primitive containment";
  let partial = Group_ops.group_bounds bounds ~containment:Group_ops.Partially_contained
      ~owner:Group_ops.Group_primitives ~name:"partial" source |> get_ok in
  expect_members [0; 1] (group Group.Primitive "partial" partial)
    "Group Bounds partial primitive containment"

let test_edge_intersection_and_extremes () =
  let source = geometry [|(-2., 0., 0.); (2., 0., 0.)|]
      [|`Open, [|0; 1|]|] in
  let bounds = box (Rays_math.Vec3.create (-0.25) (-0.25) (-0.25))
      (Rays_math.Vec3.create 0.25 0.25 0.25) in
  let full = Group_ops.group_bounds bounds ~containment:Group_ops.Fully_contained
      ~owner:Group_ops.Group_edges ~name:"full" source |> get_ok in
  check (edge_members (edge_group "full" full) = [])
    "Group Bounds full edge containment";
  let partial = Group_ops.group_bounds bounds ~containment:Group_ops.Partially_contained
      ~owner:Group_ops.Group_edges ~name:"partial" source |> get_ok in
  check (edge_members (edge_group "partial" partial) = [0])
    "Group Bounds segment-box intersection with outside endpoints";
  let spherical = Group_ops.group_bounds (sphere Rays_math.Vec3.zero 0.25)
      ~containment:Group_ops.Partially_contained ~owner:Group_ops.Group_edges
      ~name:"sphere" source |> get_ok in
  check (edge_members (edge_group "sphere" spherical) = [0])
    "Group Bounds segment-sphere intersection with outside endpoints";
  let extreme = geometry [|(-.max_float, 0., 0.); (max_float, 0., 0.)|]
      [|`Open, [|0; 1|]|] in
  let extreme_box = Group_ops.group_bounds bounds
      ~containment:Group_ops.Partially_contained ~owner:Group_ops.Group_edges
      ~name:"extreme" extreme |> get_ok in
  check (edge_members (edge_group "extreme" extreme_box) = [0])
    "Group Bounds overflow-safe extreme segment-box intersection";
  let extreme_sphere = Group_ops.group_bounds (sphere Rays_math.Vec3.zero 1.)
      ~containment:Group_ops.Partially_contained ~owner:Group_ops.Group_edges
      ~name:"extreme" extreme |> get_ok in
  check (edge_members (edge_group "extreme" extreme_sphere) = [0])
    "Group Bounds overflow-safe extreme segment-sphere intersection"

let reference_box minimum maximum (ax, ay, az) (bx, by, bz) =
  let first = ref 0. and last = ref 1. and hit = ref true in
  let axis a b low high =
    let direction = b -. a in
    if direction = 0. then begin
      if a < low || a > high then hit := false
    end else begin
      let enter = (low -. a) /. direction
      and leave = (high -. a) /. direction in
      let enter, leave = if enter <= leave then enter, leave else leave, enter in
      if enter > !first then first := enter;
      if leave < !last then last := leave;
      if !first > !last then hit := false
    end in
  axis ax bx minimum.Rays_math.Vec3.x maximum.Rays_math.Vec3.x;
  axis ay by minimum.y maximum.y;
  axis az bz minimum.z maximum.z;
  !hit

let reference_sphere center radius (ax, ay, az) (bx, by, bz) =
  let dx = bx -. ax and dy = by -. ay and dz = bz -. az in
  let denominator = (dx *. dx) +. (dy *. dy) +. (dz *. dz) in
  let parameter = if denominator = 0. then 0. else
      max 0. (min 1. ((((center.Rays_math.Vec3.x -. ax) *. dx)
        +. ((center.y -. ay) *. dy) +. ((center.z -. az) *. dz))
        /. denominator)) in
  let x = ax +. (parameter *. dx) -. center.x
  and y = ay +. (parameter *. dy) -. center.y
  and z = az +. (parameter *. dz) -. center.z in
  (x *. x) +. (y *. y) +. (z *. z) <= radius *. radius

let test_randomized_segment_reference () =
  let segment_count = 4_096 and point_count = 8_192 in
  let positions = Packed.Float3.Builder.create point_count
  and topology = Rdk_test_support.Tb.create ~point_count () in
  let random coordinate =
    (Rays_math.Rand.float_at (Rays_math.Rand.seed 0x6b17) ~index:coordinate *. 20.) -. 10. in
  for segment = 0 to segment_count - 1 do
    let point = segment * 2 in
    Packed.Float3.Builder.set positions point (random (point * 3))
      (random ((point * 3) + 1)) (random ((point * 3) + 2));
    Packed.Float3.Builder.set positions (point + 1) (random ((point + 1) * 3))
      (random (((point + 1) * 3) + 1)) (random (((point + 1) * 3) + 2));
    Rdk_test_support.Tb.add_open_polyline topology [|point; point + 1|]
  done;
  let source = Geometry.create ~positions:(Packed.Float3.Builder.freeze positions)
      ~topology:(Rdk_test_support.Tb.freeze topology) () |> Result.get_ok in
  let minimum = Rays_math.Vec3.create (-2.75) (-1.5) (-3.25)
  and maximum = Rays_math.Vec3.create 3.5 4.25 2.125
  and center = Rays_math.Vec3.create 1.25 (-0.75) 2.5 and radius = 3.125 in
  let boxed = Group_ops.group_bounds (box minimum maximum)
      ~containment:Group_ops.Partially_contained ~owner:Group_ops.Group_edges
      ~name:"boxed" source |> get_ok |> edge_group "boxed"
  and spherical = Group_ops.group_bounds (sphere center radius)
      ~containment:Group_ops.Partially_contained ~owner:Group_ops.Group_edges
      ~name:"spherical" source |> get_ok |> edge_group "spherical" in
  let packed = Packed.Float3.Private.view (Geometry.positions source)
  and index = Topology_index.create (Geometry.topology source) in
  for edge = 0 to Topology_index.edge_count index - 1 do
    let a, b = Topology_index.edge_points index edge in
    let point point = packed.x.(point), packed.y.(point), packed.z.(point) in
    check (Edge_group.mem edge boxed
        = reference_box minimum maximum (point a) (point b))
      "Group Bounds randomized segment-box reference";
    check (Edge_group.mem edge spherical
        = reference_sphere center radius (point a) (point b))
      "Group Bounds randomized segment-sphere reference"
  done

let test_base_merge_and_failures () =
  let source = Line_geometry.points (Array.init 8 (fun point ->
      float_of_int point, 0., 0.)) in
  let even = Group.init ~grain:1 ~owner:Group.Point ~name:"even" 8
      (fun point -> point land 1 = 0) in
  let existing = Group.init ~grain:1 ~owner:Group.Point ~name:"selection" 8
      (fun point -> point = 7) in
  let source = source |> with_group even |> with_group existing in
  let all_box = box (Rays_math.Vec3.create (-1.) (-1.) (-1.))
      (Rays_math.Vec3.create 10. 1. 1.) in
  let based = Group_ops.group_bounds all_box ~base:"even" ~owner:Group_ops.Group_points
      ~name:"based" source |> get_ok in
  expect_members [0; 2; 4; 6] (group Group.Point "based" based)
    "Group Bounds exact base restriction";
  let unioned = Group_ops.group_bounds all_box ~base:"even" ~merge:Group_ops.Group_union
      ~owner:Group_ops.Group_points ~name:"selection" source |> get_ok in
  expect_members [0; 2; 4; 6; 7] (group Group.Point "selection" unioned)
    "Group Bounds union merge";
  let absent_intersection = Group_ops.group_bounds all_box
      ~merge:Group_ops.Group_intersection ~owner:Group_ops.Group_points
      ~name:"absent_intersection" source |> get_ok in
  check (Group.cardinality
      (group Group.Point "absent_intersection" absent_intersection) = 0)
    "Group Bounds absent destination intersection identity";
  let absent_subtract = Group_ops.group_bounds all_box ~merge:Group_ops.Group_subtract
      ~owner:Group_ops.Group_points ~name:"absent_subtract" source |> get_ok in
  check (Group.cardinality
      (group Group.Point "absent_subtract" absent_subtract) = 0)
    "Group Bounds absent destination subtraction identity";
  expect_invalid (fun () -> Group_ops.group_bounds
      (box (Rays_math.Vec3.create 1. 0. 0.) Rays_math.Vec3.zero)
      ~owner:Group_ops.Group_points ~name:"bad" source)
    "Group Bounds rejects reversed box";
  expect_invalid (fun () -> Group_ops.group_bounds (sphere Rays_math.Vec3.zero (-1.))
      ~owner:Group_ops.Group_points ~name:"bad" source)
    "Group Bounds rejects negative radius";
  expect_invalid (fun () -> Group_ops.group_bounds
      (sphere (Rays_math.Vec3.create Float.nan 0. 0.) 1.)
      ~owner:Group_ops.Group_points ~name:"bad" source)
    "Group Bounds rejects non-finite center";
  expect_invalid (fun () -> Group_ops.group_bounds all_box ~base:"missing"
      ~owner:Group_ops.Group_points ~name:"bad" source)
    "Group Bounds missing base";
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match Group_ops.group_bounds ~cancel:cancelled all_box ~owner:Group_ops.Group_points
      ~name:"bad" source with
   | Error error -> check (Error.code error = "cancelled")
       "Group Bounds cancellation code"
   | Ok _ -> fail "cancelled Group Bounds published geometry")

let same_group left right =
  Group.owner left = Group.owner right && Group.length left = Group.length right
  && members left = members right

let test_scale_parallel_exactness () =
  let source = Plane_generators.grid ~columns:500 ~rows:300 ~size:20. () |> get_ok in
  let region = sphere (Rays_math.Vec3.create 1. 0. (-2.)) 7.5 in
  let run domains = Parallel.run ~domains (fun () ->
    source
    |> Group_ops.group_bounds ~grain:1_009 region ~owner:Group_ops.Group_points
         ~name:"points" |> get_ok
    |> Group_ops.group_bounds ~grain:1_009 region ~owner:Group_ops.Group_vertices
         ~name:"vertices" |> get_ok
    |> Group_ops.group_bounds ~grain:1_009 ~containment:Group_ops.Partially_contained region
         ~owner:Group_ops.Group_primitives ~name:"primitives" |> get_ok
    |> Group_ops.group_bounds ~grain:1_009 ~containment:Group_ops.Partially_contained region
         ~owner:Group_ops.Group_edges ~name:"edges" |> get_ok) in
  let one = run 1 and four = run 4 in
  check (same_group (group Group.Point "points" one)
      (group Group.Point "points" four))
    "Group Bounds point one/four-domain exactness";
  let selected = Group.cardinality (group Group.Point "points" one) in
  check (selected > 0 && selected < Geometry.point_count source)
    "Group Bounds scale fixture selects a proper subset";
  check (same_group (group Group.Vertex "vertices" one)
      (group Group.Vertex "vertices" four))
    "Group Bounds vertex one/four-domain exactness";
  check (same_group (group Group.Primitive "primitives" one)
      (group Group.Primitive "primitives" four))
    "Group Bounds primitive one/four-domain exactness";
  check (edge_members (edge_group "edges" one)
      = edge_members (edge_group "edges" four))
    "Group Bounds edge one/four-domain exactness"

let test_checked_boundary () =
  let source = Line_geometry.points [|(0., 0., 0.); (1., 0., 0.)|] in
  let region = box (Rays_math.Vec3.create (-0.5) (-0.5) (-0.5))
      (Rays_math.Vec3.create 0.5 0.5 0.5) in
  let selected = Group_ops.group_bounds region
      ~owner:Group_ops.Group_points ~name:"selected" source |> get_ok in
  expect_members [0] (group Group.Point "selected" selected)
    "Group Bounds checked boundary selection";
  let expect_error result expected_code =
    match result with
    | Error error ->
        check (Error.operation error = "group_bounds"
            && Error.code error = expected_code)
          "Group Bounds checked boundary typed error"
    | Ok _ -> fail "Group Bounds checked boundary accepted invalid input" in
  expect_error
    (Group_ops.group_bounds (box (Rays_math.Vec3.create 1. 0. 0.) Rays_math.Vec3.zero)
       ~owner:Group_ops.Group_points ~name:"bad" source)
    "invalid_group";
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  expect_error
    (Group_ops.group_bounds ~cancel region
       ~owner:Group_ops.Group_points ~name:"bad" source) "cancelled"

let run () =
  test_point_box_and_sphere ();
  test_vertex_and_primitive_containment ();
  test_edge_intersection_and_extremes ();
  test_randomized_segment_reference ();
  test_base_merge_and_failures ();
  test_scale_parallel_exactness ();
  test_checked_boundary ();
  print_endline "group bounds tests passed"
