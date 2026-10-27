open Rays
open Rdk
open Rdk_test_support

let add_attribute ~owner ~name storage geometry =
  Attribute.create_owned ~owner ~name storage |> get_string_ok
  |> Fun.flip Geometry.with_attribute geometry |> get_string_ok

let equal_positions left right =
  let left = positions left and right = positions right in
  left.x = right.x && left.y = right.y && left.z = right.z

let near left right = abs_float (left -. right) <= 1e-14

let sample seed identity component =
  let identity = identity * 0x1e3779b97f4a7c15 in
  Rays_math.Rand.float_at seed ~index:(identity lxor
    ((component + 1) * 0x11b54a32d192ed03)) -. 0.5

let expect_code code = function
  | Error error when String.equal (Error.code error) code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, received %s: %s"
      code (Error.code error) (Error.to_string error))
  | Ok _ -> fail ("expected error " ^ code)

let source () =
  Line_geometry.points [|0., 1., 2.; 10., 20., 30.; -3., 4., -5.; 8., 9., 10.|]
  |> add_attribute ~owner:Attribute.Point ~name:"mask"
       (Attribute.Float [|1.; 0.5; 2.; 0.|])
  |> add_attribute ~owner:Attribute.Point ~name:"pscale"
       (Attribute.Float [|2.; 3.; 0.25; 1.|])
  |> add_attribute ~owner:Attribute.Point ~name:"stable_id"
       (Attribute.Int [|7; 7; 9; 10|])
  |> add_attribute ~owner:Attribute.Point ~name:"N"
       (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
          ~x:[|1.; 1.; 1.; 1.|] ~y:[|0.; 0.; 0.; 0.|]
          ~z:[|0.; 0.; 0.; 0.|]))
  |> add_attribute ~owner:Attribute.Vertex ~name:"N"
       (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
          ~x:[||] ~y:[||] ~z:[||]))
  |> add_attribute ~owner:Attribute.Primitive ~name:"N"
       (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
          ~x:[||] ~y:[||] ~z:[||]))
  |> add_attribute ~owner:Attribute.Detail ~name:"tag"
       (Attribute.Text [|"preserve"|])

let run () =
  let input = source () in
  let selected = Group.init ~grain:1 ~owner:Group.Point ~name:"selected" 4
      (fun point -> point < 3) in
  check (Group.mem 0 selected && Group.mem 1 selected && Group.mem 2 selected
      && not (Group.mem 3 selected)) "Point Jitter test selection";
  let seed = Rays_math.Rand.seed 91 in
  let axis_scales = Rays_math.Vec3.create 1. (-2.) 0.5 in
  let output = Parallel.run ~domains:1 (fun () ->
    Point_jitter.run ~grain:1 ~points:selected
      ~mask_attribute:"mask" ~id_attribute:"stable_id" ~use_point_scale:true
      ~seed ~scale:0.8 ~axis_scales input |> get_ok) in
  let before = positions input and after = positions output in
  let identities = [|7; 7; 9; 10|]
  and amplitudes = [|1.6; 1.2; 0.4; 0.|] in
  for point = 0 to 3 do
    let expected component original axis =
      if point < 3 then original +. sample seed identities.(point) component
          *. amplitudes.(point) *. axis
      else original in
    let expected_x = expected 0 before.x.(point) 1. in
    check (near after.x.(point) expected_x)
      (Printf.sprintf "point %d X jitter: %.17g <> %.17g"
         point after.x.(point) expected_x);
    check (near after.y.(point) (expected 1 before.y.(point) (-2.)))
      (Printf.sprintf "point %d Y jitter" point);
    check (near after.z.(point) (expected 2 before.z.(point) 0.5))
      (Printf.sprintf "point %d Z jitter" point)
  done;
  check (Geometry.point_count output = Geometry.point_count input
      && Geometry.vertex_count output = Geometry.vertex_count input
      && Geometry.primitive_count output = Geometry.primitive_count input)
    "Point Jitter changed cardinality";
  check (Geometry.topology output == Geometry.topology input)
    "Point Jitter did not structurally share topology";
  check (Geometry.find_attribute ~owner:Attribute.Point "N" output = None)
    "Point Jitter retained stale point normals";
  check (Geometry.find_attribute ~owner:Attribute.Vertex "N" output = None)
    "Point Jitter retained stale vertex normals";
  check (Geometry.find_attribute ~owner:Attribute.Primitive "N" output <> None)
    "Point Jitter removed unrelated primitive normals";
  check (Geometry.find_attribute ~owner:Attribute.Detail "tag" output <> None)
    "Point Jitter dropped unrelated attributes";

  let zero = Point_jitter.run ~seed ~scale:0. input |> get_ok in
  check (zero == input) "zero Point Jitter did not return its input snapshot";
  let missing_id = Point_jitter.run ~seed ~scale:0.7
      ~id_attribute:"absent" input |> get_ok
  and point_numbers = Point_jitter.run ~seed ~scale:0.7 input |> get_ok in
  check (equal_positions missing_id point_numbers)
    "missing Point Jitter ID did not fall back to point number";
  let missing_pscale = input |> Geometry.without_attribute
      ~owner:Attribute.Point "pscale" in
  let pscale_fallback = Point_jitter.run ~seed ~scale:0.7
      ~use_point_scale:true missing_pscale |> get_ok
  and unit_scale = Point_jitter.run ~seed ~scale:0.7 missing_pscale |> get_ok in
  check (equal_positions pscale_fallback unit_scale)
    "missing pscale did not default to one";
  let changed_seed = Point_jitter.run ~seed:(Rays_math.Rand.seed 92) ~scale:0.7 input
      |> get_ok in
  check (not (equal_positions changed_seed point_numbers))
    "Point Jitter ignored its random seed";

  let count = 200_003 in
  let large = Line_geometry.points (Array.init count (fun point ->
      let value = float_of_int point in value *. 0.01, value *. -0.02, value *. 0.03))
      |> add_attribute ~owner:Attribute.Point ~name:"mask"
           (Attribute.Float (Array.init count (fun point ->
              float_of_int (point mod 11) /. 10.)))
      |> add_attribute ~owner:Attribute.Point ~name:"stable_id"
           (Attribute.Int (Array.init count (fun point -> (point * 17) mod 100_003)))
      |> add_attribute ~owner:Attribute.Point ~name:"pscale"
           (Attribute.Float (Array.init count (fun point ->
              0.25 +. float_of_int (point mod 7) *. 0.125))) in
  let large_group = Group.init ~grain:257 ~owner:Group.Point ~name:"stripe" count
      (fun point -> point mod 5 <> 0) in
  let run domains = Parallel.run ~domains (fun () ->
    Point_jitter.run ~grain:257 ~points:large_group ~mask_attribute:"mask"
      ~id_attribute:"stable_id" ~use_point_scale:true ~seed:(Rays_math.Rand.seed 1234)
      ~scale:2.75 ~axis_scales:(Rays_math.Vec3.create (-0.25) 1.5 0.75) large |> get_ok) in
  let sequential = run 1 and parallel = run 4 in
  check (equal_positions sequential parallel)
    "Point Jitter differed between one and four domains";
  check (Geometry.point_count sequential = count)
    "parallel Point Jitter changed point cardinality";

  let wrong_owner = Group.init ~grain:1 ~owner:Group.Vertex ~name:"wrong" 0
      (Fun.const false) in
  expect_code "invalid_selection"
    (Point_jitter.run ~points:wrong_owner ~seed ~scale:1. input);
  let wrong_length = Group.init ~grain:1 ~owner:Group.Point ~name:"short" 3
      (Fun.const true) in
  expect_code "invalid_selection"
    (Point_jitter.run ~points:wrong_length ~seed ~scale:1. input);
  expect_code "invalid_attribute"
    (Point_jitter.run ~mask_attribute:"absent" ~seed ~scale:1. input);
  let wrong_mask = input |> add_attribute ~owner:Attribute.Point ~name:"bad_mask"
      (Attribute.Int [|1; 1; 1; 1|]) in
  expect_code "invalid_attribute"
    (Point_jitter.run ~mask_attribute:"bad_mask" ~seed ~scale:1. wrong_mask);
  expect_code "invalid_attribute"
    (Point_jitter.run ~id_attribute:"mask" ~seed ~scale:1. input);
  let wrong_pscale = input |> add_attribute ~owner:Attribute.Point ~name:"pscale"
      (Attribute.Int [|1; 1; 1; 1|]) in
  expect_code "invalid_attribute"
    (Point_jitter.run ~use_point_scale:true ~seed ~scale:1. wrong_pscale);
  let nonfinite_pscale = input
      |> add_attribute ~owner:Attribute.Point ~name:"pscale"
           (Attribute.Float [|1.; 1.; infinity; 1.|]) in
  expect_code "invalid_attribute"
    (Point_jitter.run ~use_point_scale:true ~seed ~scale:1. nonfinite_pscale);
  expect_code "invalid_attribute"
    (Point_jitter.run ~mask_attribute:"" ~seed ~scale:1. input);
  expect_code "invalid_attribute"
    (Point_jitter.run ~id_attribute:"" ~seed ~scale:1. input);
  expect_code "invalid_attribute"
    (Point_jitter.run ~seed ~scale:nan input);
  expect_code "invalid_attribute"
    (Point_jitter.run ~seed ~scale:1.
       ~axis_scales:(Rays_math.Vec3.create infinity 1. 1.) input);
  let nonfinite = input |> add_attribute ~owner:Attribute.Point ~name:"bad"
      (Attribute.Float [|1.; nan; 1.; 1.|]) in
  expect_code "invalid_attribute"
    (Point_jitter.run ~mask_attribute:"bad" ~seed ~scale:1. nonfinite);
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled"
    (Point_jitter.run ~cancel:cancelled ~grain:1 ~seed ~scale:1. input);
  print_endline "point jitter tests passed"
