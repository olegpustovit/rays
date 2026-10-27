open Rays
open Rdk
open Rdk_test_support

let near_arrays left right =
  Array.length left = Array.length right
  && Array.for_all2 (fun left right -> abs_float (left -. right) <= 1e-12)
       left right

let float_values ~owner name geometry =
  Geometry.find_attribute ~owner name geometry |> Option.get
  |> Attribute.get (Attribute.key ~name ~owner Attribute.float) |> Option.get

let float2_values ~owner name geometry =
  Geometry.find_attribute ~owner name geometry |> Option.get
  |> Attribute.get (Attribute.key ~name ~owner Attribute.float2) |> Option.get
  |> Packed.Float2.Private.view

let float3_values ~owner name geometry =
  Geometry.find_attribute ~owner name geometry |> Option.get
  |> Attribute.get (Attribute.key ~name ~owner Attribute.float3) |> Option.get
  |> Packed.Float3.Private.view

let add_attribute geometry attribute =
  Geometry.with_attribute attribute geometry |> get_string_ok

let scalar_attribute ~owner ~name values =
  Attribute.create_owned ~name ~owner (Attribute.Float values) |> get_string_ok

let float2_attribute ~owner ~name x y =
  let values = Packed.Float2.of_owned ~x ~y |> get_string_ok in
  Attribute.create_owned ~name ~owner (Attribute.Float2 values) |> get_string_ok

let int_attribute ~owner ~name values =
  Attribute.create_owned ~name ~owner (Attribute.Int values) |> get_string_ok

let expect_error code = function
  | Error error when String.equal (Error.code error) code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, received %s: %s"
      code (Error.code error) (Error.to_string error))
  | Ok _ -> fail ("expected error " ^ code)

let numeric value = Attribute_ops.Scalar value

let randomize ?selection ?element_selection ?seed_attribute ?fraction_attribute
    ?minimum ?maximum
    ?(direction_bias = 0.)
    ?(operation = Attribute_ops.Random_set)
    ?(scale = 1.) ~owner ~name distribution geometry =
  Attribute_ops.randomize ~grain:1 ?selection ?element_selection ?seed_attribute
    ?fraction_attribute ?minimum ?maximum ~seed:(Rays_math.Rand.seed 0x5eed)
    ~owner ~name ~direction_bias ~operation ~scale
    distribution geometry
  |> get_ok

let run () =
  let geometry = Plane_generators.grid ~columns:1 ~rows:1 ~size:2. () |> get_ok in
  let selected = Group.init ~owner:Group.Point ~name:"selected" 4
      (fun point -> point land 1 = 0) in
  let owners = [
    Attribute.Point, Geometry.point_count geometry;
    Attribute.Vertex, Geometry.vertex_count geometry;
    Attribute.Primitive, Geometry.primitive_count geometry;
    Attribute.Detail, 1;
  ] in
  List.iter (fun (owner, count) ->
    let result = randomize ~owner ~name:"constant"
        (Attribute_ops.Random_constant (numeric 3.25)) geometry in
    let values = float_values ~owner "constant" result in
    if values <> Array.make count 3.25 then
      fail (Printf.sprintf
        "Attribute Randomize owner result has length %d instead of %d"
        (Array.length values) count)) owners;

  let restricted = randomize ~selection:selected ~owner:Attribute.Point
      ~name:"restricted" (Attribute_ops.Random_constant (numeric 7.)) geometry in
  if float_values ~owner:Attribute.Point "restricted" restricted
      <> [|7.; 0.; 7.; 0.|] then
    fail "Attribute Randomize did not preserve unselected new defaults";

  let with_source = add_attribute geometry
      (scalar_attribute ~owner:Attribute.Point ~name:"source"
        [|2.; -2.; 0.5; 5.|]) in
  let operation expected operation =
    let result = randomize ~operation ~owner:Attribute.Point ~name:"source"
        (Attribute_ops.Random_constant (numeric 2.)) with_source in
    if float_values ~owner:Attribute.Point "source" result <> expected then
      fail "Attribute Randomize arithmetic operation is incorrect" in
  operation [|2.; 2.; 2.; 2.|] Attribute_ops.Random_set;
  operation [|4.; 0.; 2.5; 7.|] Attribute_ops.Random_add;
  operation [|2.; -2.; 0.5; 2.|] Attribute_ops.Random_minimum;
  operation [|2.; 2.; 2.; 5.|] Attribute_ops.Random_maximum;
  operation [|4.; -4.; 1.; 10.|] Attribute_ops.Random_multiply;
  let scaled = randomize ~scale:0.5 ~owner:Attribute.Point ~name:"scaled"
      (Attribute_ops.Random_constant (numeric 8.)) geometry in
  if float_values ~owner:Attribute.Point "scaled" scaled <> [|4.; 4.; 4.; 4.|]
  then fail "Attribute Randomize global scale is incorrect";
  let signed_zero_source = add_attribute geometry
      (scalar_attribute ~owner:Attribute.Point ~name:"signed_zero"
         [|-0.; -0.; -0.; -0.|]) in
  let signed_zero = randomize ~owner:Attribute.Point ~name:"signed_zero"
      (Attribute_ops.Random_constant (numeric 0.)) signed_zero_source
      |> float_values ~owner:Attribute.Point "signed_zero" in
  if Array.exists (fun value -> Int64.bits_of_float value <> 0L) signed_zero then
    fail "Attribute Randomize lost a bit-level signed-zero update";

  let paired = randomize ~owner:Attribute.Point ~name:"paired"
      (Attribute_ops.Random_two_values {
        a = Attribute_ops.Vec2 (Rays_math.Vec2.create 1. 10.);
        b = Attribute_ops.Vec2 (Rays_math.Vec2.create 2. 20.);
        probability_b = 0.5;
      }) geometry |> float2_values ~owner:Attribute.Point "paired" in
  for point = 0 to 3 do
    if not ((paired.x.(point) = 1. && paired.y.(point) = 10.)
        || (paired.x.(point) = 2. && paired.y.(point) = 20.)) then
      fail "Attribute Randomize two-value tuples chose components independently"
  done;

  let seeded = add_attribute geometry
      (int_attribute ~owner:Attribute.Point ~name:"seed_id"
         [|11; 11; 29; 29|]) in
  let seeded = randomize ~seed_attribute:"seed_id" ~owner:Attribute.Point
      ~name:"uv" (Attribute_ops.Random_uniform {
        min = Attribute_ops.Vec2 Rays_math.Vec2.zero;
        max = Attribute_ops.Vec2 (Rays_math.Vec2.create 1. 1.);
      }) seeded in
  let uv = float2_values ~owner:Attribute.Point "uv" seeded in
  if uv.x.(0) <> uv.x.(1) || uv.y.(0) <> uv.y.(1)
      || uv.x.(2) <> uv.x.(3) || uv.y.(2) <> uv.y.(3)
      || (uv.x.(0) = uv.x.(2) && uv.y.(0) = uv.y.(2)) then
    fail "Attribute Randomize seed attribute stream is not stable";

  let check_distribution name distribution predicate =
    let values = randomize ~owner:Attribute.Point ~name distribution geometry
        |> float_values ~owner:Attribute.Point name in
    if not (Array.for_all predicate values) then
      fail ("Attribute Randomize distribution escaped its contract: " ^ name) in
  check_distribution "discrete" (Attribute_ops.Random_uniform_discrete {
      min = numeric 2.; max = numeric 8.; step = numeric 2. })
    (fun value -> value >= 2. && value <= 8.
      && Float.rem (value -. 2.) 2. = 0.);
  check_distribution "normal" (Attribute_ops.Random_normal {
      middle = numeric 3.; scale = numeric 1.25 }) Float.is_finite;
  check_distribution "exponential" (Attribute_ops.Random_exponential {
      median = numeric 2. }) (fun value -> Float.is_finite value && value >= 0.);
  check_distribution "log_normal" (Attribute_ops.Random_log_normal {
      median = numeric 2.; stddev = numeric 0.5 })
    (fun value -> Float.is_finite value && value > 0.);
  check_distribution "cauchy" (Attribute_ops.Random_cauchy {
      median = numeric 0.; scale = numeric 1. }) Float.is_finite;

  let cauchy_fraction_source = add_attribute
      (Line_geometry.points [|(0., 0., 0.)|])
      (float2_attribute ~owner:Attribute.Point ~name:"cauchy_fraction"
         [|0.5|] [|0.5|]) in
  let cauchy_fraction = randomize ~fraction_attribute:"cauchy_fraction"
      ~owner:Attribute.Point ~name:"cauchy2"
      (Attribute_ops.Random_cauchy {
        median = Attribute_ops.Vec2 Rays_math.Vec2.zero;
        scale = Attribute_ops.Vec2 (Rays_math.Vec2.create 1. 1.);
      }) cauchy_fraction_source
      |> float2_values ~owner:Attribute.Point "cauchy2" in
  if abs_float cauchy_fraction.x.(0) > 1e-12
      || abs_float (cauchy_fraction.y.(0) -. sqrt 3.) > 1e-12 then
    fail "Attribute Randomize isotropic Cauchy inverse radius is incorrect";

  let cloud_count = 100_001 in
  let cloud = Line_geometry.points (Array.make cloud_count (0., 0., 0.)) in
  let directions = randomize ~owner:Attribute.Point ~name:"direction"
      (Attribute_ops.Random_direction {
        direction = Attribute_ops.Vec3 Rays_math.Vec3.unit_y;
        cone_angle = Float.pi /. 3.;
      }) cloud |> float3_values ~owner:Attribute.Point "direction" in
  let minimum_dot = cos (Float.pi /. 3.) -. 1e-12 in
  for point = 0 to cloud_count - 1 do
    let x = directions.x.(point) and y = directions.y.(point)
    and z = directions.z.(point) in
    let length = sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
    if abs_float (length -. 1.) > 2e-12 || y < minimum_dot then
      fail "Attribute Randomize direction escaped its unit spherical cap"
  done;

  let spheres = randomize ~owner:Attribute.Point ~name:"sphere"
      (Attribute_ops.Random_inside_sphere { dimensions = 3 }) cloud
      |> float3_values ~owner:Attribute.Point "sphere" in
  let sum_x = ref 0. and sum_y = ref 0. and sum_z = ref 0.
  and sum_radius2 = ref 0. in
  for point = 0 to cloud_count - 1 do
    let x = spheres.x.(point) and y = spheres.y.(point) and z = spheres.z.(point) in
    let radius2 = (x *. x) +. (y *. y) +. (z *. z) in
    if radius2 >= 1. then fail "Attribute Randomize sphere sample is not interior";
    sum_x := !sum_x +. x; sum_y := !sum_y +. y; sum_z := !sum_z +. z;
    sum_radius2 := !sum_radius2 +. radius2
  done;
  let inverse_count = 1. /. float_of_int cloud_count in
  if abs_float (!sum_x *. inverse_count) > 0.01
      || abs_float (!sum_y *. inverse_count) > 0.01
      || abs_float (!sum_z *. inverse_count) > 0.01
      || abs_float ((!sum_radius2 *. inverse_count) -. 0.6) > 0.01 then
    fail "Attribute Randomize sphere sampling failed its uniform-volume moments";

  let cauchy_cloud = randomize ~owner:Attribute.Point ~name:"isotropic_cauchy"
      (Attribute_ops.Random_cauchy {
        median = Attribute_ops.Vec2 Rays_math.Vec2.zero;
        scale = Attribute_ops.Vec2 (Rays_math.Vec2.create 1. 1.);
      }) cloud |> float2_values ~owner:Attribute.Point "isotropic_cauchy" in
  let x_dominant = ref 0 and positive_x = ref 0 and positive_y = ref 0 in
  for point = 0 to cloud_count - 1 do
    let x = cauchy_cloud.x.(point) and y = cauchy_cloud.y.(point) in
    if not (Float.is_finite x && Float.is_finite y) then
      fail "Attribute Randomize isotropic Cauchy emitted a non-finite sample";
    if abs_float x > abs_float y then incr x_dominant;
    if x > 0. then incr positive_x;
    if y > 0. then incr positive_y
  done;
  let frequency count = float_of_int count /. float_of_int cloud_count in
  if abs_float (frequency !x_dominant -. 0.5) > 0.012
      || abs_float (frequency !positive_x -. 0.5) > 0.012
      || abs_float (frequency !positive_y -. 0.5) > 0.012 then
    fail "Attribute Randomize multidimensional Cauchy is not rotationally symmetric";

  let cone_spheres = randomize ~direction_bias:3. ~owner:Attribute.Point
      ~name:"cone_sphere" (Attribute_ops.Random_inside_sphere_cone {
        direction = Attribute_ops.Vec3 Rays_math.Vec3.unit_y;
        cone_angle = Float.pi /. 2.;
      }) cloud |> float3_values ~owner:Attribute.Point "cone_sphere" in
  let sum_direction_y = ref 0. and sum_cone_radius2 = ref 0. in
  for point = 0 to cloud_count - 1 do
    let x = cone_spheres.x.(point) and y = cone_spheres.y.(point)
    and z = cone_spheres.z.(point) in
    let radius2 = (x *. x) +. (y *. y) +. (z *. z) in
    if radius2 >= 1. || y < -1e-12 then
      fail "Attribute Randomize biased sphere cone escaped its support";
    if radius2 > 0. then sum_direction_y := !sum_direction_y +. y /. sqrt radius2;
    sum_cone_radius2 := !sum_cone_radius2 +. radius2
  done;
  if (!sum_direction_y *. inverse_count) < 0.7
      || abs_float ((!sum_cone_radius2 *. inverse_count) -. 0.6) > 0.01 then
    fail "Attribute Randomize sphere-cone bias or radial distribution is wrong";

  let quantile_source = add_attribute
      (Line_geometry.points (Array.make 5 (0., 0., 0.)))
      (scalar_attribute ~owner:Attribute.Point ~name:"fraction"
        [|0.; 0.25; 0.5; 0.75; 1.|]) in
  let quantiles seed = Attribute_ops.randomize ~grain:2
      ~fraction_attribute:"fraction" ~seed:(Rays_math.Rand.seed seed)
      ~owner:Attribute.Point ~name:"quantile"
      (Attribute_ops.Random_custom_ramp {
        ramp = [0., 0.; 0.5, 0.2; 1., 1.];
        fit_min = numeric 10.; fit_max = numeric 20.;
      }) quantile_source |> get_ok
      |> float_values ~owner:Attribute.Point "quantile" in
  if quantiles 1 <> [|10.; 11.; 12.; 16.; 20.|]
      || quantiles 1 <> quantiles 99 then
    fail "Attribute Randomize ramp quantiles are incorrect or seed-dependent";
  let limited_tails = randomize ~fraction_attribute:"fraction"
      ~minimum:(numeric (-3.)) ~maximum:(numeric 3.) ~scale:2.
      ~owner:Attribute.Point ~name:"limited_normal"
      (Attribute_ops.Random_normal { middle = numeric 0.; scale = numeric 1. })
      quantile_source |> float_values ~owner:Attribute.Point "limited_normal" in
  if limited_tails.(0) <> -6. || limited_tails.(2) <> 0.
      || limited_tails.(4) <> 6.
      || abs_float (limited_tails.(1) +. limited_tails.(3)) > 1e-12
      || abs_float (abs_float limited_tails.(1) -. 1.3489795) > 1e-6 then
    fail "Attribute Randomize tail limits or pre-scale ordering are incorrect";
  let discrete = Attribute_ops.randomize ~grain:2 ~fraction_attribute:"fraction"
      ~seed:(Rays_math.Rand.seed 1) ~owner:Attribute.Point ~name:"choice"
      (Attribute_ops.Random_custom_discrete [
        Attribute_ops.Vec2 (Rays_math.Vec2.create 1. 10.), 1.;
        Attribute_ops.Vec2 (Rays_math.Vec2.create 2. 20.), 3.;
      ]) quantile_source |> get_ok
      |> float2_values ~owner:Attribute.Point "choice" in
  if discrete.x <> [|1.; 2.; 2.; 2.; 2.|]
      || discrete.y <> [|10.; 20.; 20.; 20.; 20.|] then
    fail "Attribute Randomize weighted discrete quantiles are incorrect";
  let vertex_selection = randomize
      ~element_selection:(Attribute_ops.Random_points selected)
      ~owner:Attribute.Vertex ~name:"expanded"
      (Attribute_ops.Random_constant (numeric 9.)) geometry
      |> float_values ~owner:Attribute.Vertex "expanded" in
  let topology = Topology.Private.view (Geometry.topology geometry) in
  for vertex = 0 to Array.length vertex_selection - 1 do
    let expected = if Group.mem topology.vertex_points.(vertex) selected
      then 9. else 0. in
    if vertex_selection.(vertex) <> expected then
      fail "Attribute Randomize did not expand a point group to vertices"
  done;
  let topology_value = Geometry.topology geometry in
  let topology_index = Topology_index.create topology_value in
  let topology_reverse = Topology_index.Private.view topology_index in
  let selected_edge = Edge_group.init ~topology:topology_value
      ~index:topology_index ~name:"selected_edge" (fun edge -> edge = 0) in
  let edge_points = randomize
      ~element_selection:(Attribute_ops.Random_edges selected_edge)
      ~owner:Attribute.Point ~name:"edge_points"
      (Attribute_ops.Random_constant (numeric 3.)) geometry
      |> float_values ~owner:Attribute.Point "edge_points" in
  for point = 0 to Array.length edge_points - 1 do
    let expected = if point = topology_reverse.edge_a.(0)
        || point = topology_reverse.edge_b.(0) then 3. else 0. in
    if edge_points.(point) <> expected then
      fail "Attribute Randomize edge-to-point expansion is incorrect"
  done;
  let edge_vertices = randomize
      ~element_selection:(Attribute_ops.Random_edges selected_edge)
      ~owner:Attribute.Vertex ~name:"edge_vertices"
      (Attribute_ops.Random_constant (numeric 4.)) geometry
      |> float_values ~owner:Attribute.Vertex "edge_vertices" in
  for vertex = 0 to Array.length edge_vertices - 1 do
    let outgoing = topology_reverse.edge_of_vertex.(vertex)
    and previous = topology_reverse.previous_vertex.(vertex) in
    let expected = if outgoing = 0
        || (previous >= 0 && topology_reverse.edge_of_vertex.(previous) = 0)
      then 4. else 0. in
    if edge_vertices.(vertex) <> expected then
      fail "Attribute Randomize edge-to-vertex expansion is incorrect"
  done;
  let edge_primitives = randomize
      ~element_selection:(Attribute_ops.Random_edges selected_edge)
      ~owner:Attribute.Primitive ~name:"edge_primitives"
      (Attribute_ops.Random_constant (numeric 5.)) geometry
      |> float_values ~owner:Attribute.Primitive "edge_primitives" in
  for primitive = 0 to Array.length edge_primitives - 1 do
    let expected = ref 0. in
    for vertex = topology.primitive_offsets.(primitive)
        to topology.primitive_offsets.(primitive + 1) - 1 do
      if topology_reverse.edge_of_vertex.(vertex) = 0 then expected := 5.
    done;
    if edge_primitives.(primitive) <> !expected then
      fail "Attribute Randomize edge-to-primitive expansion is incorrect"
  done;
  let weighted = randomize ~owner:Attribute.Point ~name:"weighted"
      (Attribute_ops.Random_custom_discrete [numeric 1., 1.; numeric 2., 3.])
      cloud |> float_values ~owner:Attribute.Point "weighted" in
  let second_count = Array.fold_left (fun count value ->
      if value = 2. then count + 1
      else if value = 1. then count
      else fail "Attribute Randomize emitted an unknown discrete value") 0 weighted in
  let second_fraction = float_of_int second_count /. float_of_int cloud_count in
  if abs_float (second_fraction -. 0.75) > 0.01 then
    fail "Attribute Randomize weighted discrete frequencies are biased";
  let median_quantile = Attribute_ops.randomize ~grain:2
      ~fraction_attribute:"fraction" ~seed:(Rays_math.Rand.seed 1)
      ~owner:Attribute.Point ~name:"normal_quantile"
      (Attribute_ops.Random_normal { middle = numeric 7.; scale = numeric 2. })
      quantile_source in
  expect_error "invalid_randomize" median_quantile;
  let middle_source = add_attribute (Line_geometry.points [|(0., 0., 0.)|])
      (scalar_attribute ~owner:Attribute.Point ~name:"middle_fraction" [|0.5|]) in
  let median_quantile = randomize ~fraction_attribute:"middle_fraction"
      ~owner:Attribute.Point ~name:"normal_quantile"
      (Attribute_ops.Random_normal { middle = numeric 7.; scale = numeric 2. })
      middle_source |> float_values ~owner:Attribute.Point "normal_quantile" in
  if abs_float (median_quantile.(0) -. 7.) > 1e-14 then
    fail "Attribute Randomize normal inverse CDF missed the median";
  let tail_source = add_attribute
      (Line_geometry.points [|(0., 0., 0.); (0., 0., 0.)|])
      (scalar_attribute ~owner:Attribute.Point ~name:"tail_fraction"
        [|0.025; 0.975|]) in
  let tails = randomize ~fraction_attribute:"tail_fraction"
      ~owner:Attribute.Point ~name:"normal_tail"
      (Attribute_ops.Random_normal { middle = numeric 0.; scale = numeric 1. })
      tail_source |> float_values ~owner:Attribute.Point "normal_tail" in
  if abs_float (tails.(0) +. 1.959963984540054) > 1e-7
      || abs_float (tails.(1) -. 1.959963984540054) > 1e-7 then
    fail "Attribute Randomize normal inverse CDF tails are inaccurate";

  let direction_fractions = add_attribute
      (Line_geometry.points [|(0., 0., 0.); (0., 0., 0.)|])
      (float2_attribute ~owner:Attribute.Point ~name:"direction_uv"
        [|0.; 1.|] [|0.; 1.|]) in
  let fraction_directions = randomize ~fraction_attribute:"direction_uv"
      ~owner:Attribute.Point ~name:"fraction_direction"
      (Attribute_ops.Random_direction {
        direction = Attribute_ops.Vec3 Rays_math.Vec3.unit_z;
        cone_angle = Float.pi /. 2.;
      }) direction_fractions |> float3_values ~owner:Attribute.Point
        "fraction_direction" in
  if fraction_directions.z.(0) < 1. -. 1e-12
      || fraction_directions.z.(1) > 1e-12 then
    fail "Attribute Randomize direction fraction mapping is incorrect";

  let with_normals = Normal_ops.run geometry |> get_ok in
  let moved = randomize ~owner:Attribute.Point ~name:"P"
      (Attribute_ops.Random_constant
        (Attribute_ops.Vec3 (Rays_math.Vec3.create 1. 2. 3.))) with_normals in
  let positions = Packed.Float3.Private.view (Geometry.positions moved) in
  if positions.x <> [|1.; 1.; 1.; 1.|]
      || positions.y <> [|2.; 2.; 2.; 2.|]
      || positions.z <> [|3.; 3.; 3.; 3.|]
      || Geometry.find_attribute ~owner:Attribute.Point "N" moved <> None
      || Geometry.find_attribute ~owner:Attribute.Vertex "N" moved <> None then
    fail "Attribute Randomize P did not invalidate stale normals";

  expect_error "invalid_randomize"
    (Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0) ~owner:Attribute.Point
      ~name:"bad" (Attribute_ops.Random_uniform {
        min = numeric 2.; max = numeric 1. }) geometry);
  expect_error "invalid_randomize"
    (Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0) ~owner:Attribute.Point
      ~name:"source" (Attribute_ops.Random_constant
        (Attribute_ops.Vec2 Rays_math.Vec2.zero)) with_source);
  expect_error "invalid_randomize"
    (Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0) ~seed_attribute:"source"
      ~owner:Attribute.Point ~name:"bad"
      (Attribute_ops.Random_constant (numeric 1.)) with_source);
  expect_error "invalid_randomize"
    (Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0) ~selection:selected
      ~owner:Attribute.Vertex ~name:"bad"
      (Attribute_ops.Random_constant (numeric 1.)) geometry);
  List.iter (expect_error "invalid_randomize") [
    Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0) ~owner:Attribute.Point
      ~name:"bad" (Attribute_ops.Random_direction {
        direction = numeric 1.; cone_angle = 0. }) geometry;
    Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0) ~owner:Attribute.Point
      ~name:"bad" (Attribute_ops.Random_direction {
        direction = Attribute_ops.Vec3 Rays_math.Vec3.zero; cone_angle = 0. }) geometry;
    Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0) ~owner:Attribute.Point
      ~name:"bad" (Attribute_ops.Random_inside_sphere { dimensions = 5 }) geometry;
    Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0) ~owner:Attribute.Point
      ~name:"bad" (Attribute_ops.Random_cauchy {
        median = Attribute_ops.Vec2 Rays_math.Vec2.zero;
        scale = Attribute_ops.Vec2 (Rays_math.Vec2.create 1. 2.);
      }) geometry;
    Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0) ~minimum:(numeric 2.)
      ~maximum:(numeric 1.) ~owner:Attribute.Point ~name:"bad"
      (Attribute_ops.Random_normal { middle = numeric 0.; scale = numeric 1. })
      geometry;
    Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0)
      ~operation:Attribute_ops.Random_add ~owner:Attribute.Point ~name:"bad"
      (Attribute_ops.Random_custom_discrete_text ["x", 1.]) geometry;
    Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0) ~direction_bias:(-1.)
      ~owner:Attribute.Point ~name:"bad" (Attribute_ops.Random_direction {
        direction = Attribute_ops.Vec3 Rays_math.Vec3.unit_y; cone_angle = 1. }) geometry;
    Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0) ~direction_bias:1.
      ~owner:Attribute.Point ~name:"bad" (Attribute_ops.Random_uniform {
        min = numeric 0.; max = numeric 1. }) geometry;
    Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0) ~owner:Attribute.Point
      ~name:"bad" (Attribute_ops.Random_inside_sphere_cone {
        direction = Attribute_ops.Vec3 Rays_math.Vec3.zero; cone_angle = 1. }) geometry;
    Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0) ~owner:Attribute.Point
      ~name:"bad" (Attribute_ops.Random_custom_ramp {
        ramp = [0., 0.; 0.5, 1.]; fit_min = numeric 0.; fit_max = numeric 1.;
      }) geometry;
    Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0) ~owner:Attribute.Point
      ~name:"bad" (Attribute_ops.Random_custom_discrete []) geometry;
    Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0) ~owner:Attribute.Point
      ~name:"bad" (Attribute_ops.Random_custom_discrete [numeric 1., -1.])
      geometry;
    Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0) ~seed_attribute:"seed_id"
      ~fraction_attribute:"fraction" ~owner:Attribute.Point ~name:"bad"
      (Attribute_ops.Random_uniform { min = numeric 0.; max = numeric 1. })
      quantile_source;
    Attribute_ops.randomize ~seed:(Rays_math.Rand.seed 0) ~fraction_attribute:"fraction"
      ~owner:Attribute.Point ~name:"bad"
      (Attribute_ops.Random_inside_sphere { dimensions = 3 }) quantile_source;
  ];
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_error "cancelled"
    (Attribute_ops.randomize ~cancel:cancelled ~seed:(Rays_math.Rand.seed 0)
      ~owner:Attribute.Point ~name:"bad"
      (Attribute_ops.Random_constant (numeric 1.)) geometry);

  let values_geometry = add_attribute geometry
      (scalar_attribute ~owner:Attribute.Point ~name:"value"
         [|-1.; 0.; 0.5; 2.|]) in
  let remap ?selection ?into ?(policy = Attribute_ops.Remap_clamp) ?(ramp = [])
      ~input ~output_min ~output_max geometry =
    Attribute_ops.remap ~grain:1 ?selection ?into ~owner:Attribute.Point
      ~name:"value" ~input ~output_min ~output_max ~policy ~ramp geometry
    |> get_ok in
  let explicit = Attribute_ops.Remap_explicit {
      min = numeric 0.; max = numeric 1. } in
  let remapped = remap ~input:explicit ~output_min:(numeric 10.)
      ~output_max:(numeric 20.) values_geometry in
  if float_values ~owner:Attribute.Point "value" remapped
      <> [|10.; 10.; 15.; 20.|] then
    fail "Attribute Remap clamp is incorrect";
  let extrapolated = remap ~policy:Attribute_ops.Remap_extrapolate
      ~input:explicit ~output_min:(numeric 0.) ~output_max:(numeric 1.)
      values_geometry in
  if float_values ~owner:Attribute.Point "value" extrapolated
      <> [|-1.; 0.; 0.5; 2.|] then
    fail "Attribute Remap extrapolation is incorrect";
  let cycled = remap ~policy:Attribute_ops.Remap_cycle ~input:explicit
      ~output_min:(numeric 0.) ~output_max:(numeric 1.) values_geometry in
  if float_values ~owner:Attribute.Point "value" cycled
      <> [|0.; 0.; 0.5; 0.|] then
    fail "Attribute Remap cycling is incorrect";
  let ramped = remap ~ramp:[0., 0.; 0.5, 0.25; 1., 1.]
      ~input:explicit ~output_min:(numeric 0.) ~output_max:(numeric 1.)
      values_geometry in
  if float_values ~owner:Attribute.Point "value" ramped
      <> [|0.; 0.; 0.25; 1.|] then
    fail "Attribute Remap piecewise-linear ramp is incorrect";
  let automatic = remap ~into:"automatic" ~input:Attribute_ops.Remap_auto
      ~output_min:(numeric (-1.)) ~output_max:(numeric 1.) values_geometry in
  if not (near_arrays (float_values ~owner:Attribute.Point "automatic" automatic)
      [|-1.; -1. /. 3.; 0.; 1.|]) then
    fail "Attribute Remap automatic range is incorrect";
  let restricted = remap ~selection:selected ~into:"restricted_remap"
      ~input:Attribute_ops.Remap_auto ~output_min:(numeric 0.)
      ~output_max:(numeric 1.) values_geometry in
  if float_values ~owner:Attribute.Point "restricted_remap" restricted
      <> [|0.; 0.; 1.; 0.|] then
    fail "Attribute Remap selection range/defaults are incorrect";

  let tuple = randomize ~owner:Attribute.Point ~name:"tuple"
      (Attribute_ops.Random_constant
        (Attribute_ops.Vec3 (Rays_math.Vec3.create 0. 5. 10.))) geometry in
  let tuple = Attribute_ops.remap ~owner:Attribute.Point ~name:"tuple"
      ~input:(Attribute_ops.Remap_explicit {
        min = Attribute_ops.Vec3 Rays_math.Vec3.zero;
        max = Attribute_ops.Vec3 (Rays_math.Vec3.create 1. 10. 20.); })
      ~output_min:(Attribute_ops.Vec3 Rays_math.Vec3.zero)
      ~output_max:(Attribute_ops.Vec3 (Rays_math.Vec3.create 2. 2. 2.)) tuple |> get_ok
      |> float3_values ~owner:Attribute.Point "tuple" in
  if tuple.x <> [|0.; 0.; 0.; 0.|] || tuple.y <> [|1.; 1.; 1.; 1.|]
      || tuple.z <> [|1.; 1.; 1.; 1.|] then
    fail "Attribute Remap tuple components are incorrect";

  List.iter (fun (owner, count) ->
    let source = add_attribute geometry
        (scalar_attribute ~owner ~name:"owner_value"
          (Array.init count float_of_int)) in
    let result = Attribute_ops.remap ~owner ~name:"owner_value"
        ~input:(Attribute_ops.Remap_explicit { min = numeric 0.; max = numeric 1. })
        ~output_min:(numeric 2.) ~output_max:(numeric 4.) source |> get_ok in
    if Array.length (float_values ~owner "owner_value" result) <> count then
      fail "Attribute Remap did not support every attribute owner") owners;

  expect_error "invalid_remap"
    (Attribute_ops.remap ~owner:Attribute.Point ~name:"value"
      ~input:(Attribute_ops.Remap_explicit { min = numeric 1.; max = numeric 1. })
      ~output_min:(numeric 0.) ~output_max:(numeric 1.) values_geometry);
  expect_error "invalid_remap"
    (Attribute_ops.remap ~owner:Attribute.Point ~name:"value" ~input:explicit
      ~output_min:(numeric 0.) ~output_max:(numeric 1.)
      ~ramp:[0., 0.; 0.5, 1.] values_geometry);
  expect_error "invalid_remap"
    (Attribute_ops.remap ~owner:Attribute.Point ~name:"value" ~into:"P"
      ~input:explicit ~output_min:(numeric 0.) ~output_max:(numeric 1.)
      values_geometry);
  expect_error "cancelled"
    (Attribute_ops.remap ~cancel:cancelled ~owner:Attribute.Point ~name:"value"
      ~input:explicit ~output_min:(numeric 0.) ~output_max:(numeric 1.)
      values_geometry);

  let large = Plane_generators.grid ~columns:500 ~rows:200 ~size:10. () |> get_ok in
  let large_bytes = geometry_bytes large in
  let generated domains = Parallel.run ~domains (fun () ->
    Attribute_ops.randomize ~grain:2_048 ~seed:(Rays_math.Rand.seed 77)
      ~owner:Attribute.Point ~name:"sample"
      (Attribute_ops.Random_uniform {
        min = Attribute_ops.Vec4 (-2., -1., 0., 1.);
        max = Attribute_ops.Vec4 (2., 1., 4., 3.);
      }) large |> get_ok) in
  let one = generated 1 and many = generated 4 in
  check (geometry_bytes one = geometry_bytes many)
    "Uniform attribute randomization differs across domains";
  let advanced domains = Parallel.run ~domains (fun () ->
    let geometry = Attribute_ops.randomize ~grain:2_048 ~seed:(Rays_math.Rand.seed 81)
        ~owner:Attribute.Point ~name:"random_direction"
        (Attribute_ops.Random_direction {
          direction = Attribute_ops.Vec4 (0., 0., 0., 1.);
          cone_angle = Float.pi;
        }) large |> get_ok in
    let geometry = Attribute_ops.randomize ~grain:2_048 ~seed:(Rays_math.Rand.seed 82)
        ~owner:Attribute.Point ~name:"random_sphere"
        (Attribute_ops.Random_inside_sphere { dimensions = 3 }) geometry
        |> get_ok in
    let geometry = Attribute_ops.randomize ~grain:2_048 ~seed:(Rays_math.Rand.seed 821)
        ~direction_bias:2. ~owner:Attribute.Point ~name:"random_cone_sphere"
        (Attribute_ops.Random_inside_sphere_cone {
          direction = Attribute_ops.Vec3 Rays_math.Vec3.unit_z;
          cone_angle = Float.pi /. 2.;
        }) geometry |> get_ok in
    let geometry = Attribute_ops.randomize ~grain:2_048 ~seed:(Rays_math.Rand.seed 83)
        ~owner:Attribute.Point ~name:"random_ramp"
        (Attribute_ops.Random_custom_ramp {
          ramp = [0., 0.; 0.25, 0.05; 0.7, 0.9; 1., 1.];
          fit_min = Attribute_ops.Vec2 (Rays_math.Vec2.create (-2.) 10.);
          fit_max = Attribute_ops.Vec2 (Rays_math.Vec2.create 3. 20.);
        }) geometry |> get_ok in
    Attribute_ops.randomize ~grain:2_048 ~seed:(Rays_math.Rand.seed 84)
      ~owner:Attribute.Point ~name:"random_choice"
      (Attribute_ops.Random_custom_discrete [
        Attribute_ops.Vec3 (Rays_math.Vec3.create 1. 2. 3.), 1.;
        Attribute_ops.Vec3 (Rays_math.Vec3.create 4. 5. 6.), 4.;
        Attribute_ops.Vec3 (Rays_math.Vec3.create 7. 8. 9.), 2.;
      ]) geometry |> get_ok) in
  let advanced_one = advanced 1 and advanced_many = advanced 4 in
  check (geometry_bytes advanced_one = geometry_bytes advanced_many)
    "Direction/sphere/ramp/discrete randomization differs across domains";
  let expanded_group = Group.init ~grain:2_048 ~owner:Group.Point
      ~name:"expanded_parallel" (Geometry.point_count large)
      (fun point -> point mod 5 = 0) in
  let large_topology = Geometry.topology large in
  let large_index = Topology_index.create large_topology in
  let expanded_edges = Edge_group.init ~grain:2_048 ~topology:large_topology
      ~index:large_index ~name:"expanded_edges"
      (fun edge -> edge mod 11 = 0) in
  let extended domains = Parallel.run ~domains (fun () ->
    let geometry = Attribute_ops.randomize ~grain:2_048
        ~element_selection:(Attribute_ops.Random_points expanded_group)
        ~seed:(Rays_math.Rand.seed 91) ~owner:Attribute.Vertex ~name:"expanded_parallel"
        (Attribute_ops.Random_constant (numeric 2.)) large |> get_ok in
    let geometry = Attribute_ops.randomize ~grain:2_048
        ~minimum:(Attribute_ops.Vec2 (Rays_math.Vec2.create (-20.) (-20.)))
        ~maximum:(Attribute_ops.Vec2 (Rays_math.Vec2.create 20. 20.))
        ~seed:(Rays_math.Rand.seed 92) ~owner:Attribute.Point ~name:"cauchy_parallel"
        (Attribute_ops.Random_cauchy {
          median = Attribute_ops.Vec2 Rays_math.Vec2.zero;
          scale = Attribute_ops.Vec2 (Rays_math.Vec2.create 1. 1.);
        }) geometry |> get_ok in
    let geometry = Attribute_ops.randomize ~grain:2_048
        ~element_selection:(Attribute_ops.Random_edges expanded_edges)
        ~seed:(Rays_math.Rand.seed 921) ~owner:Attribute.Primitive
        ~name:"edge_expanded_parallel"
        (Attribute_ops.Random_constant (numeric 3.)) geometry |> get_ok in
    Attribute_ops.randomize ~grain:2_048 ~seed:(Rays_math.Rand.seed 93)
      ~owner:Attribute.Primitive ~name:"text_parallel"
      (Attribute_ops.Random_custom_discrete_text ["a", 1.; "b", 2.])
      geometry |> get_ok) in
  let extended_one = extended 1 and extended_many = extended 4 in
  check (geometry_bytes extended_one = geometry_bytes extended_many)
    "Expanded selections/Cauchy/text randomization differs across domains";
  let noise_input = Line_geometry.points (Array.make 4_097 (0., 0., 0.)) in
  let noise_bytes = geometry_bytes noise_input in
  let noisy domains = Parallel.run ~domains (fun () ->
    Attribute_ops.noise ~grain:512 ~seed:73 ~owner:Attribute.Point
      ~name:"orient" ~kind:Attribute_ops.Noise_quaternion
      ~location:Attribute_ops.Noise_element_number
      ~range:Attribute_ops.Noise_zero_centered
      ~frequency:(Rays_math.Vec3.create 0.071 0.071 0.071)
      ~octaves:3 ~lacunarity:2. ~roughness:0.55 noise_input |> get_ok) in
  let noise_one = noisy 1 and noise_many = noisy 4 in
  check (geometry_bytes noise_one = geometry_bytes noise_many)
    "Quaternion attribute noise differs across domains";
  let remapped domains geometry = Parallel.run ~domains (fun () ->
    Attribute_ops.remap ~grain:2_048 ~owner:Attribute.Point ~name:"sample"
      ~into:"mapped" ~input:Attribute_ops.Remap_auto
      ~output_min:(Attribute_ops.Vec4 (0., 0., 0., 0.))
      ~output_max:(Attribute_ops.Vec4 (1., 1., 1., 1.)) geometry |> get_ok) in
  let sample_bytes = geometry_bytes one in
  let remapped_one = remapped 1 one and remapped_many = remapped 4 many in
  check (geometry_bytes remapped_one = geometry_bytes remapped_many)
    "Automatic attribute remapping differs across domains";
  check (geometry_bytes one = sample_bytes && geometry_bytes many = sample_bytes
      && geometry_bytes large = large_bytes && geometry_bytes noise_input = noise_bytes)
    "Attribute generation/noise/remapping mutated its source";
  print_endline "attribute generate tests passed"
