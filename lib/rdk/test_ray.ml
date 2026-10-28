open Rays
open Rdk
open Rdk_test_support

let translated y geometry =
  Transform_ops_front.transform (Rays_math.Mat4.translation (Rays_math.Vec3.create 0. y 0.)) geometry

let add_attribute attribute geometry =
  Geometry.with_attribute attribute geometry |> get_string_ok

let add_group group geometry = Geometry.with_group group geometry |> get_string_ok

let point_float geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float values -> values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing point float " ^ name)

let point_int geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int values -> values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing point integer " ^ name)

let point_float3 geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.View.view values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing point float3 " ^ name)

let point_int_array geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int_array values -> Packed.Int_array.View.view values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing point int array " ^ name)

let point_float_array geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float_array values -> Packed.Float_array.View.view values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing point float array " ^ name)

let plane ?(size = 4.) y =
  Plane_generators.grid ~columns:1 ~rows:1 ~size () |> get_ok |> translated y

let source_points values = Line_geometry.points values

let equal_attribute left right =
  Attribute.owner left = Attribute.owner right
  && String.equal (Attribute.name left) (Attribute.name right)
  && match Attribute.View.storage left, Attribute.View.storage right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.View.view left
      and right = Packed.Float2.View.view right in
      left.x = right.x && left.y = right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.View.view left
      and right = Packed.Float3.View.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
  | Attribute.Float4 left, Attribute.Float4 right ->
      let left = Packed.Float4.View.view left
      and right = Packed.Float4.View.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
      && left.w = right.w
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.View.view left
      and right = Packed.Int_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.View.view left
      and right = Packed.Float_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | _ -> false

let check_basic_projection () =
  let source = source_points [|(-1., 2., -1.); (0.5, 3., 0.25); (4., 2., 0.)|]
  and collision = plane 0. in
  let projected = ray ~direction:(Ray.Ray_vector (Rays_math.Vec3.create 0. (-5.) 0.))
      ~distance_attribute:"dist" ~primitive_attribute:"hit_prim"
      ~source_vertex_numbers_attribute:"hit_vertices"
      ~source_vertex_weights_attribute:"hit_weights" ~hit_group:"hits"
      ~normal_attribute:"hit_N" ~source ~collision () |> get_ok in
  let p = positions projected and distances = point_float projected "dist"
  and primitives = point_int projected "hit_prim"
  and normals = point_float3 projected "hit_N"
  and numbers = point_int_array projected "hit_vertices"
  and weights = point_float_array projected "hit_weights" in
  check (near p.y.(0) 0. && near p.y.(1) 0. && near p.y.(2) 2.)
    "Ray basic projection positions";
  check (near distances.(0) 2. && near distances.(1) 3.
      && distances.(2) = -1.) "Ray world-space distances/miss sentinel";
  check (primitives.(0) >= 0 && primitives.(1) >= 0 && primitives.(2) = -1)
    "Ray primitive provenance";
  check (Array.length numbers.offsets = 4 && numbers.offsets = weights.offsets
      && numbers.offsets.(3) = 6) "Ray CSR provenance layout";
  let collision_positions = positions collision
  and collision_topology = Topology.View.view (Geometry.topology collision) in
  for point = 0 to 1 do
    let first = numbers.offsets.(point) in
    let total = weights.values.(first) +. weights.values.(first + 1)
        +. weights.values.(first + 2) in
    let reconstruct coordinate =
      let value = ref 0. in
      for local = 0 to 2 do
        let vertex = numbers.values.(first + local) in
        let source_point = collision_topology.vertex_points.(vertex) in
        value := !value +. (weights.values.(first + local) *. coordinate.(source_point))
      done;
      !value in
    check (near total 1. && near (abs_float normals.y.(point)) 1.)
      "Ray barycentric/normal output";
    check (near (reconstruct collision_positions.x) p.x.(point)
        && near (reconstruct collision_positions.y) p.y.(point)
        && near (reconstruct collision_positions.z) p.z.(point))
      "Ray exact source-vertex provenance reconstruction"
  done;
  let hits = Geometry.find_group ~owner:Group.Point "hits" projected
      |> Option.get in
  check (Group.cardinality hits = 2 && Group.mem 0 hits && Group.mem 1 hits
      && not (Group.mem 2 hits)) "Ray hit group";
  let scaled = ray ~method_:Ray.Ray_minimum_distance ~scale:0.5 ~lift:0.1
      ~normal_attribute:"Nhit" ~source ~collision () |> get_ok |> positions in
  check (near scaled.y.(0) 1.1 && near scaled.y.(1) 1.6)
    "Ray scale/lift transform"

let check_direction_policies () =
  let source = source_points [|(0., 0., 0.)|] in
  let collision = Mesh_merge.run [plane 1.; plane 3.; plane (-2.)] |> get_ok in
  let project ?(mode = Surface_index.Ray_forward) ?(surface = Surface_index.Ray_first_surface) () =
    ray ~direction:(Ray.Ray_vector Rays_math.Vec3.unit_y) ~direction_mode:mode
      ~surface_hit:surface ~source ~collision () |> get_ok |> positions in
  check (near (project ()).y.(0) 1.) "Ray first forward surface";
  check (near (project ~surface:Surface_index.Ray_last_surface ()).y.(0) 3.)
    "Ray last forward surface";
  check (near (project ~mode:Surface_index.Ray_reverse ()).y.(0) (-2.))
    "Ray reverse direction";
  check (near (project ~mode:Surface_index.Ray_bidirectional_closest ()).y.(0) 1.)
    "Ray bidirectional closest";
  check (near (project ~mode:Surface_index.Ray_bidirectional_farthest ()).y.(0) (-2.))
    "Ray bidirectional farthest";
  let limited = ray ~direction:(Ray.Ray_vector Rays_math.Vec3.unit_y)
      ~min_distance:1.5 ~max_distance:2.5 ~source ~collision () |> get_ok in
  check ((positions limited).y.(0) = 0.) "Ray distance bounds did not reject hits";
  let near_edge = source_points [|(2.0001, 1., 0.)|] in
  let missed = ray ~direction:(Ray.Ray_vector (Rays_math.Vec3.neg Rays_math.Vec3.unit_y))
      ~source:near_edge ~collision:(plane 0.) () |> get_ok in
  let tolerated = ray ~direction:(Ray.Ray_vector (Rays_math.Vec3.neg Rays_math.Vec3.unit_y))
      ~tolerance:0.001 ~source:near_edge ~collision:(plane 0.) () |> get_ok in
  check ((positions missed).y.(0) = 1. && near (positions tolerated).y.(0) 0.)
    "Ray world-space edge tolerance";
  let upper = Group.init ~owner:Group.Primitive ~name:"upper"
      (Geometry.primitive_count collision) (fun primitive -> primitive >= 2
        && primitive < 4) in
  let restricted = ray ~collision_primitives:upper
      ~direction:(Ray.Ray_vector Rays_math.Vec3.unit_y) ~source ~collision () |> get_ok in
  check (near (positions restricted).y.(0) 3.) "Ray collision restriction";
  let public_surface = Surface_index.create collision |> get_ok in
  (match Surface_index.raycast
      public_surface ~origin:Rays_math.Vec3.zero ~direction:Rays_math.Vec3.unit_y with
   | Ok (Some hit) -> check (near hit.distance 1.) "Surface_index.raycast distance"
   | Ok None -> fail "Surface_index.raycast missed"
   | Error error -> fail (Error.to_string error))

let check_multi_samples () =
  let source = source_points [|(0.,2.,0.)|] and collision = plane ~size:20. 0. in
  let project combine jitter_scale seed =
    ray ~samples:7 ~jitter_scale ~seed ~combine
      ~direction:(Ray.Ray_vector (Rays_math.Vec3.neg Rays_math.Vec3.unit_y))
      ~distance_attribute:"dist" ~normal_attribute:"hit_N"
      ~source ~collision () |> get_ok in
  let shortest = project Ray.Ray_shortest 0.35 71
  and longest = project Ray.Ray_longest 0.35 71
  and average = project Ray.Ray_average 0.35 71
  and median = project Ray.Ray_median 0.35 71 in
  let shortest_distance = (point_float shortest "dist").(0)
  and longest_distance = (point_float longest "dist").(0)
  and average_distance = (point_float average "dist").(0)
  and median_distance = (point_float median "dist").(0) in
  check (near shortest_distance 2.)
    "Ray multi-sample shortest did not retain the unjittered ray";
  check (longest_distance > average_distance && average_distance > 2.
      && median_distance >= 2. && median_distance <= longest_distance)
    "Ray multi-sample combine ordering";
  check (near (positions average).y.(0) (2. -. average_distance))
    "Ray average did not move along the unjittered direction";
  let normal = point_float3 average "hit_N" in
  check (near (abs_float normal.y.(0)) 1.) "Ray averaged hit normal";
  let repeat = project Ray.Ray_average 0.35 71 in
  check (equal_geometry average repeat) "Ray seeded jitter is not deterministic";
  let collision_positions = positions collision in
  let signal = Attribute.create_owned ~name:"signal" ~owner:Attribute.Point
      (Attribute.Float (Array.copy collision_positions.x)) |> get_string_ok in
  let collision_with_signal = add_attribute signal collision in
  let drivers = ray ~samples:4 ~jitter_scale:0. ~seed:9
      ~combine:Ray.Ray_average
      ~direction:(Ray.Ray_vector (Rays_math.Vec3.neg Rays_math.Vec3.unit_y))
      ~source_vertex_numbers_attribute:"vertices"
      ~source_vertex_weights_attribute:"weights" ~point_pattern:"signal"
      ~source ~collision:collision_with_signal () |> get_ok in
  let numbers = point_int_array drivers "vertices"
  and weights = point_float_array drivers "weights" in
  check (numbers.offsets = [|0;12|] && weights.offsets = numbers.offsets)
    "Ray average provenance did not retain every successful sample";
  check (near (Array.fold_left ( +. ) 0. weights.values) 1.)
    "Ray average provenance weights do not sum to one";
  let collision_topology = Topology.View.view
      (Geometry.topology collision_with_signal) in
  let reconstructed = ref 0. in
  for at = numbers.offsets.(0) to numbers.offsets.(1) - 1 do
    let point = collision_topology.vertex_points.(numbers.values.(at)) in
    reconstructed := !reconstructed +. (weights.values.(at)
        *. collision_positions.x.(point))
  done;
  check (near !reconstructed (point_float drivers "signal").(0))
    "Ray average attribute import ignored contributing sample drivers";
  let selected_driver = ray ~samples:4 ~jitter_scale:0. ~seed:9
      ~combine:Ray.Ray_median
      ~direction:(Ray.Ray_vector (Rays_math.Vec3.neg Rays_math.Vec3.unit_y))
      ~source_vertex_numbers_attribute:"vertices"
      ~source_vertex_weights_attribute:"weights" ~source ~collision () |> get_ok
      |> fun geometry -> point_int_array geometry "vertices" in
  check (selected_driver.offsets = [|0;3|])
    "Ray selected-sample combiner retained all sample provenance";
  List.iter (fun combine ->
    let output = project combine 0. 11 in
    check (near (point_float output "dist").(0) 2.)
      "Ray zero-jitter combiner changed an identical hit")
    [Ray.Ray_average; Ray.Ray_median; Ray.Ray_shortest; Ray.Ray_longest]

let check_selection_and_directions () =
  let source = source_points [|(-1.,2.,0.); (0.,2.,0.); (1.,2.,0.)|] in
  let selection = Group.init ~owner:Group.Point ~name:"middle" 3
      (fun point -> point = 1) in
  let selected = ray ~selection:(Element_selection.Selected_points selection)
      ~direction:(Ray.Ray_vector (Rays_math.Vec3.neg Rays_math.Vec3.unit_y))
      ~source ~collision:(plane 0.) () |> get_ok |> positions in
  check (selected.y = [|2.; 0.; 2.|]) "Ray point restriction";
  let directions = Packed.Float3.View.of_owned_exn
      ~x:(Array.make 3 0.) ~y:[|-1.; -2.; -3.|] ~z:(Array.make 3 0.) in
  let direction_attribute = Attribute.create_owned ~name:"ray_dir"
      ~owner:Attribute.Point (Attribute.Float3 directions) |> get_string_ok in
  let attributed = source |> add_attribute direction_attribute
      |> fun source -> ray ~direction:(Ray.Ray_attribute "ray_dir")
          ~source ~collision:(plane 0.) () |> get_ok in
  check ((positions attributed).y = [|0.;0.;0.|]) "Ray attribute directions";
  let normal_source = Plane_generators.grid ~columns:2 ~rows:2 ~size:1. () |> get_ok
      |> translated 2. in
  let normal_projected = ray ~direction:Ray.Ray_normal
      ~direction_mode:Surface_index.Ray_reverse ~source:normal_source
      ~collision:(plane 0.) () |> get_ok in
  check (Array.for_all (fun y -> near y 0.) (positions normal_projected).y)
    "Ray authored/computed normal direction"

let check_imports () =
  let collision = plane 0. in
  let point_count = Geometry.point_count collision
  and vertex_count = Geometry.vertex_count collision
  and primitive_count = Geometry.primitive_count collision in
  let temperature = Attribute.create_owned ~name:"temperature"
      ~owner:Attribute.Point (Attribute.Float (Array.init point_count (fun point ->
        float_of_int point))) |> get_string_ok
  and corner = Attribute.create_owned ~name:"corner"
      ~owner:Attribute.Vertex (Attribute.Float (Array.init vertex_count (fun vertex ->
        10. +. float_of_int vertex))) |> get_string_ok
  and material = Attribute.create_owned ~name:"material"
      ~owner:Attribute.Primitive (Attribute.Text
        (Array.init primitive_count (fun primitive -> "m" ^ string_of_int primitive)))
      |> get_string_ok
  and revision = Attribute.create_owned ~name:"revision" ~owner:Attribute.Detail
      (Attribute.Int [|7|]) |> get_string_ok in
  let collision = collision |> add_attribute temperature |> add_attribute corner
      |> add_attribute material |> add_attribute revision
      |> add_group (Group.init ~owner:Group.Primitive ~name:"marked"
           primitive_count (fun _ -> true)) in
  let source = source_points [|(0.5,2.,0.25); (5.,2.,0.)|] in
  let output = ray ~direction:(Ray.Ray_vector (Rays_math.Vec3.neg Rays_math.Vec3.unit_y))
      ~point_pattern:"temperature" ~vertex_pattern:"corner"
      ~primitive_pattern:"material marked" ~detail_pattern:"revision"
      ~match_groups:true ~source ~collision () |> get_ok in
  (* The hit (0.5, 0, 0.25) lies in the second triangle, points 0, 3, 1 and
     vertices 3, 4, 5, at barycentric weights 0.375, 0.5625, 0.0625. *)
  check (near (point_float output "temperature").(0) ((3. *. 0.5625) +. 0.0625)
      && near (point_float output "corner").(0)
           ((13. *. 0.375) +. (14. *. 0.5625) +. (15. *. 0.0625)))
    "Ray did not interpolate imported numeric fields at the hit";
  (match Geometry.find_attribute ~owner:Attribute.Point "material" output with
   | Some attribute ->
       (match Attribute.View.storage attribute with
        | Attribute.Text values -> check
            (values.(0) = "m1")
            "Ray primitive text import"
        | _ -> fail "Ray material output storage")
   | None -> fail "Ray material output missing");
  check ((point_int output "revision").(0) = 7)
    "Ray detail field import";
  (match Geometry.find_group ~owner:Group.Point "marked" output with
   | Some group -> check (Group.mem 0 group) "Ray group import"
   | None -> fail "Ray imported group missing");
  let imported_normals = Packed.Float3.View.of_owned_exn
      ~x:(Array.make point_count 1.) ~y:(Array.make point_count 0.)
      ~z:(Array.make point_count 0.) in
  let imported_normals = Attribute.create_owned ~name:"N" ~owner:Attribute.Point
      (Attribute.Float3 imported_normals) |> get_string_ok in
  let collision = add_attribute imported_normals collision in
  let imported_normal = ray
      ~direction:(Ray.Ray_vector (Rays_math.Vec3.neg Rays_math.Vec3.unit_y))
      ~normal_attribute:"N" ~point_pattern:"N" ~source ~collision () |> get_ok
      |> fun geometry -> point_float3 geometry "N" in
  check (near imported_normal.x.(0) 1. && near imported_normal.y.(0) 0.)
    "Ray geometric normal overrode explicitly imported collision N"

let check_validation () =
  let source = source_points [|(0.,2.,0.)|] and collision = plane 0. in
  expect_code "invalid_parameter" (ray ~grain:0 ~source ~collision ());
  expect_code "invalid_distance" (ray ~max_distance:(-1.) ~source ~collision ());
  expect_code "invalid_direction" (ray
      ~direction:(Ray.Ray_vector Rays_math.Vec3.zero) ~source ~collision ());
  expect_code "invalid_direction" (ray
      ~direction:(Ray.Ray_attribute "missing") ~source ~collision ());
  expect_code "invalid_name" (ray ~distance_attribute:"P" ~source ~collision ());
  expect_code "invalid_name" (ray ~distance_attribute:"same"
      ~primitive_attribute:"same" ~source ~collision ());
  expect_code "invalid_name" (ray
      ~source_vertex_numbers_attribute:"vertices" ~source ~collision ());
  expect_code "invalid_pattern" (ray ~match_groups:true ~source ~collision ());
  expect_code "invalid_parameter" (ray ~samples:0 ~source ~collision ());
  expect_code "invalid_parameter" (ray ~samples:1025 ~source ~collision ());
  expect_code "invalid_parameter" (ray ~jitter_scale:(-1.) ~source ~collision ());
  let extreme_jitter = ray ~samples:2 ~jitter_scale:max_float
      ~direction:(Ray.Ray_vector (Rays_math.Vec3.neg Rays_math.Vec3.unit_y)) ~source ~collision ()
      |> get_ok |> positions in
  check (Array.for_all Float.is_finite extreme_jitter.x
      && Array.for_all Float.is_finite extreme_jitter.y
      && Array.for_all Float.is_finite extreme_jitter.z)
    "Ray finite extreme jitter produced non-finite positions";
  expect_code "invalid_parameter" (ray ~method_:Ray.Ray_minimum_distance
      ~samples:2 ~source ~collision ());
  let wrong_collision_group = Group.init ~owner:Group.Point ~name:"wrong"
      (Geometry.point_count collision) (fun _ -> true) in
  expect_code "invalid_group" (ray ~collision_primitives:wrong_collision_group
      ~source ~collision ());
  let curve = Line_geometry.polyline [|(-1.,0.,0.); (1.,0.,0.)|] |> get_ok in
  expect_code "invalid_surface" (ray ~source ~collision:curve ());
  let bad_direction = Packed.Float3.View.of_owned_exn ~x:[|0.|]
      ~y:[|Float.nan|] ~z:[|0.|] in
  let bad_direction = Attribute.create_owned ~name:"bad" ~owner:Attribute.Point
      (Attribute.Float3 bad_direction) |> get_string_ok in
  let bad_source = add_attribute bad_direction source in
  expect_code "invalid_direction" (ray ~direction:(Ray.Ray_attribute "bad")
      ~source:bad_source ~collision ());
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled" (ray ~cancel:cancelled ~source ~collision ())

let run () =
  check_basic_projection ();
  check_direction_policies ();
  check_multi_samples ();
  check_selection_and_directions ();
  check_imports ();
  check_validation ();
  print_endline "ray tests passed"
