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

let group name geometry = match Geometry.find_group ~owner:Group.Primitive name geometry with
  | Some group -> group
  | None -> fail ("missing group " ^ name)

let members group =
  let output = ref [] in
  Group.iter (fun member -> output := member :: !output) group;
  List.rev !output

let expect_members expected group message =
  check (members group = expected) message

let expect_invalid operation message = match operation () with
  | Error error -> check (Error.code error = "invalid_group") message
  | Ok _ -> fail (message ^ ": unexpectedly succeeded")

let sample () = geometry
    [|(-1., -1., 0.); (1., -1., 0.); (0., 1., 0.);
      (-1., -1., 2.); (0., 1., 2.); (1., -1., 2.);
      (0., -1., 3.); (0., 1., 3.); (0., 0., 5.);
      (-1., 0., 7.); (1., 0., 7.)|]
    [|`Polygon, [|0; 1; 2|]; `Polygon, [|3; 4; 5|];
      `Polygon, [|6; 7; 8|]; `Open, [|9; 10|]|]

let test_winding_viewpoint_and_edge_on () =
  let source = sample () in
  let above = Group_ops.group_backface ~viewpoint:(Rays_math.Vec3.create 0. 0. 10.)
      ~name:"above" source |> get_ok in
  expect_members [1] (group "above" above)
    "Group Backface winding, curve, and edge-on behavior";
  let below = Group_ops.group_backface ~viewpoint:(Rays_math.Vec3.create 0. 0. (-10.))
      ~name:"below" source |> get_ok in
  expect_members [0] (group "below" below)
    "Group Backface viewpoint reversal"

let test_extreme_coordinates () =
  let magnitude = max_float /. 4. in
  let source = geometry
      [|(magnitude, magnitude, magnitude); (-.magnitude, magnitude, magnitude);
        (magnitude, -.magnitude, magnitude)|]
      [|`Polygon, [|0; 1; 2|]|] in
  let front = Group_ops.group_backface ~viewpoint:(Rays_math.Vec3.create 0. 0. max_float)
      ~name:"front" source |> get_ok in
  expect_members [] (group "front" front)
    "Group Backface overflow-safe extreme front face";
  let back = Group_ops.group_backface ~viewpoint:(Rays_math.Vec3.create 0. 0. (-.max_float))
      ~name:"back" source |> get_ok in
  expect_members [0] (group "back" back)
    "Group Backface overflow-safe extreme back face"

let test_base_merge_and_failures () =
  let source = sample () in
  let base = Group.init ~grain:1 ~owner:Group.Primitive ~name:"base" 4
      (fun primitive -> primitive = 0) in
  let visible = Group.init ~grain:1 ~owner:Group.Primitive ~name:"visible" 4
      (fun _ -> true) in
  let source = Geometry.with_group base source |> Result.get_ok
      |> Geometry.with_group visible |> Result.get_ok in
  let based = Group_ops.group_backface ~base:"base"
      ~viewpoint:(Rays_math.Vec3.create 0. 0. 10.) ~name:"based" source |> get_ok in
  expect_members [] (group "based" based)
    "Group Backface exact primitive base restriction";
  let visible = Group_ops.group_backface ~merge:Group_ops.Group_subtract
      ~viewpoint:(Rays_math.Vec3.create 0. 0. 10.) ~name:"visible" source |> get_ok in
  expect_members [0; 2; 3] (group "visible" visible)
    "Group Backface subtracts from an existing criteria group";
  let absent_intersection = Group_ops.group_backface ~merge:Group_ops.Group_intersection
      ~viewpoint:(Rays_math.Vec3.create 0. 0. 10.) ~name:"absent_intersection" source
      |> get_ok in
  check (Group.cardinality (group "absent_intersection" absent_intersection) = 0)
    "Group Backface absent destination intersection identity";
  let absent_subtract = Group_ops.group_backface ~merge:Group_ops.Group_subtract
      ~viewpoint:(Rays_math.Vec3.create 0. 0. 10.) ~name:"absent_subtract" source
      |> get_ok in
  check (Group.cardinality (group "absent_subtract" absent_subtract) = 0)
    "Group Backface absent destination subtraction identity";
  expect_invalid (fun () -> Group_ops.group_backface
      ~viewpoint:(Rays_math.Vec3.create Float.nan 0. 0.) ~name:"bad" source)
    "Group Backface rejects a non-finite viewpoint";
  expect_invalid (fun () -> Group_ops.group_backface ~base:"missing"
      ~viewpoint:Rays_math.Vec3.zero ~name:"bad" source)
    "Group Backface rejects a missing base";
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match Group_ops.group_backface ~cancel:cancelled ~viewpoint:Rays_math.Vec3.zero
      ~name:"bad" source with
   | Error error -> check (Error.code error = "cancelled")
       "Group Backface cancellation code"
   | Ok _ -> fail "cancelled Group Backface published geometry")

let test_scale_parallel_exactness () =
  let source = Plane_generators.grid ~columns:600 ~rows:400 ~size:20. () |> get_ok in
  let run domains = Parallel.run ~domains (fun () ->
    Group_ops.group_backface ~grain:1_009 ~viewpoint:(Rays_math.Vec3.create 0. (-10.) 0.)
      ~name:"backfaces" source |> get_ok) in
  let one = run 1 and four = run 4 in
  let one = group "backfaces" one and four = group "backfaces" four in
  check (Group.length one = Group.length four && members one = members four)
    "Group Backface one/four-domain exactness";
  check (Group.cardinality one = Geometry.primitive_count source)
    "Group Backface scale cardinality"

let run () =
  test_winding_viewpoint_and_edge_on ();
  test_extreme_coordinates ();
  test_base_merge_and_failures ();
  test_scale_parallel_exactness ();
  print_endline "group backface tests passed"
