open Rays
open Rdk
open Rdk_test_support

let bounds geometry = match Analysis.bounds geometry with
  | Some bounds -> bounds
  | None -> fail "geometry has no bounds"

let normal_values owner geometry =
  match Geometry.find_attribute ~owner "N" geometry with
  | Some attribute ->
      (match Attribute.get (Attribute.normal ~owner) attribute with
       | Some values -> Packed.Float3.Private.view values
       | None -> fail "N has incompatible storage")
  | None -> fail "N is missing"

let equal_storage left right = match Attribute.storage left, Attribute.storage right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.Private.view left
      and right = Packed.Int_array.Private.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.Private.view left
      and right = Packed.Float_array.Private.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.Private.view left
      and right = Packed.Float2.Private.view right in
      left.x = right.x && left.y = right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.Private.view left
      and right = Packed.Float3.Private.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
  | Attribute.Float4 left, Attribute.Float4 right ->
      let left = Packed.Float4.Private.view left
      and right = Packed.Float4.Private.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
      && left.w = right.w
  | _ -> false

let equal_attribute left right =
  Attribute.owner left = Attribute.owner right
  && String.equal (Attribute.name left) (Attribute.name right)
  && String.equal (Attribute.kind_name left) (Attribute.kind_name right)
  && equal_storage left right

let equal_geometry left right =
  let left_positions = positions left and right_positions = positions right
  and left_topology = Topology.Private.view (Geometry.topology left)
  and right_topology = Topology.Private.view (Geometry.topology right) in
  left_positions.x = right_positions.x
  && left_positions.y = right_positions.y
  && left_positions.z = right_positions.z
  && left_topology.point_count = right_topology.point_count
  && left_topology.vertex_points = right_topology.vertex_points
  && left_topology.primitive_offsets = right_topology.primitive_offsets
  && Bytes.equal left_topology.primitive_kinds right_topology.primitive_kinds
  && List.equal equal_attribute (Geometry.attributes left) (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)
  && List.equal equal_edge_group (Geometry.edge_groups left)
       (Geometry.edge_groups right)

let check_numeric_targets () =
  let source = Box_generator.box ~size:(Rays_math.Vec3.create 2. 4. 8.) () |> get_ok in
  let unit = Match_size.run source |> get_ok |> bounds in
  check (near unit.center.x 0. && near unit.center.y 0. && near unit.center.z 0.
      && near unit.size.x 0.25 && near unit.size.y 0.5 && near unit.size.z 1.)
    "default unit reference contain fit";
  let explicit = Match_size.run ~fit:Match_size.Stretch
      ~target_center:(Rays_math.Vec3.create 10. 20. 30.)
      ~target_size:(Rays_math.Vec3.create 4. 8. 12.) source |> get_ok |> bounds in
  check (near explicit.center.x 10. && near explicit.center.y 20.
      && near explicit.center.z 30. && near explicit.size.x 4.
      && near explicit.size.y 8. && near explicit.size.z 12.)
    "explicit numeric stretch reference";
  let axes = Match_size.run ~fit:Match_size.Stretch ~scale_axes:(true, false, true)
      ~target_center:(Rays_math.Vec3.create 10. 20. 30.)
      ~target_size:(Rays_math.Vec3.create 4. 8. 12.) source |> get_ok |> bounds in
  check (near axes.size.x 4. && near axes.size.y 4. && near axes.size.z 12.
      && near axes.center.x 10. && near axes.center.y 20.
      && near axes.center.z 30.)
    "per-axis scale enable";
  let aligned = Match_size.run ~fit:Match_size.Translate_only
      ~justify:(Rays_math.Vec3.create 1. 0. 0.)
      ~target_justify:(Rays_math.Vec3.create (-1.) 0. 0.)
      ~offset:(Rays_math.Vec3.create 0.5 2. 3.)
      ~translate_axes:(true, false, false)
      ~target_center:(Rays_math.Vec3.create 10. 20. 30.)
      ~target_size:(Rays_math.Vec3.create 4. 8. 12.) source |> get_ok |> bounds in
  check (near aligned.max.x 8.5 && near aligned.center.y 0.
      && near aligned.center.z 0.)
    "cross-anchor, offset, and translation-axis controls"

let with_group group geometry = Geometry.with_group group geometry |> Result.get_ok

let check_selections () =
  let source = Line_geometry.points [|(0.,0.,0.); (2.,0.,0.); (10.,10.,10.); (11.,10.,10.)|] in
  let source_bounds = Group.init ~owner:Group.Point ~name:"source_bounds" 4
      (fun point -> point < 2)
  and move = Group.init ~owner:Group.Point ~name:"move" 4
      (fun point -> point = 0) in
  let source = source |> with_group source_bounds |> with_group move in
  let output = Match_size.run ~fit:Match_size.Translate_only
      ~selection:(Transform_ops.Selected_points move)
      ~source_selection:(Transform_ops.Selected_points source_bounds)
      ~justify:(Rays_math.Vec3.create 1. 0. 0.)
      ~target_justify:(Rays_math.Vec3.create (-1.) 0. 0.)
      ~target_center:(Rays_math.Vec3.create 20. 0. 0.)
      ~target_size:(Rays_math.Vec3.create 1. 1. 1.) source |> get_ok in
  let output = positions output in
  check (near output.x.(0) 17.5 && near output.x.(1) 2.
      && near output.x.(2) 10. && near output.x.(3) 11.)
    "independent move and source-bounds selections";
  let target = Line_geometry.points [|(100.,0.,0.); (200.,0.,0.)|] in
  let target_group = Group.init ~owner:Group.Point ~name:"anchor" 2
      (fun point -> point = 0) in
  let target = with_group target_group target in
  let output = Match_size.run ~fit:Match_size.Translate_only
      ~source_selection:(Transform_ops.Selected_points source_bounds)
      ~target_selection:(Transform_ops.Selected_points target_group) ~target source
      |> get_ok |> bounds in
  check (near output.center.x 104.5)
    "target selection controls reference bounds";
  let component_source = Box_generator.box ~size:(Rays_math.Vec3.create 1. 2. 3.) () |> get_ok
      |> Group_mesh.group_edges ~name:"all_edges" |> get_ok in
  let primitives = Group.init ~owner:Group.Primitive ~name:"all_primitives"
      (Geometry.primitive_count component_source) (fun _ -> true) in
  let edges = Geometry.find_edge_group "all_edges" component_source
      |> Option.get in
  let component_target = Box_generator.box ~size:(Rays_math.Vec3.create 4. 5. 6.) () |> get_ok
      |> Transform_ops.transform (Rays_math.Mat4.translation (Rays_math.Vec3.create 7. 8. 9.)) in
  let vertices = Group.init ~owner:Group.Vertex ~name:"all_vertices"
      (Geometry.vertex_count component_target) (fun _ -> true) in
  let component_output = Match_size.run ~fit:Match_size.Stretch
      ~selection:(Transform_ops.Selected_edges edges)
      ~source_selection:(Transform_ops.Selected_primitives primitives)
      ~target_selection:(Transform_ops.Selected_vertices vertices)
      ~target:component_target component_source |> get_ok |> bounds in
  check (near component_output.center.x 7. && near component_output.center.y 8.
      && near component_output.center.z 9. && near component_output.size.x 4.
      && near component_output.size.y 5. && near component_output.size.z 6.)
    "edge move, primitive source, and vertex target selections";
  let empty_move = Group.init ~owner:Group.Point ~name:"empty_move" 4
      (fun _ -> false) in
  let unchanged = Match_size.run ~selection:(Transform_ops.Selected_points empty_move)
      ~fit:Match_size.Stretch ~target source |> get_ok in
  check (Geometry.data_id unchanged = Geometry.data_id source)
    "empty move selection is a structural identity"

let check_fit_modes () =
  let source = Box_generator.box ~size:(Rays_math.Vec3.create 1. 2. 4.) () |> get_ok
  and target = Box_generator.box ~size:(Rays_math.Vec3.create 4. 6. 8.) () |> get_ok in
  let size fit = Match_size.run ~fit ~target source |> get_ok |> bounds
      |> fun bounds -> bounds.size in
  let x = size Match_size.Match_x and y = size Match_size.Match_y and z = size Match_size.Match_z
  and contain = size Match_size.Contain and cover = size Match_size.Cover in
  check (near x.x 4. && near x.y 8. && near x.z 16.) "uniform X fit";
  check (near y.x 3. && near y.y 6. && near y.z 12.) "uniform Y fit";
  check (near z.x 2. && near z.y 4. && near z.z 8.) "uniform Z fit";
  check (near contain.x 2. && near contain.y 4. && near contain.z 8.)
    "uniform contain fit";
  check (near cover.x 4. && near cover.y 8. && near cover.z 16.)
    "uniform cover fit";
  let doubled = Box_generator.box ~size:(Rays_math.Vec3.create 2. 4. 6.) () |> get_ok in
  List.iter (fun fit ->
    let measured = Match_size.run ~fit ~translate_axes:(false, false, false)
        ~target:doubled (Box_generator.box ~size:(Rays_math.Vec3.create 1. 2. 3.) () |> get_ok)
        |> get_ok |> bounds in
    check (near measured.size.x 2. && near measured.size.y 4.
        && near measured.size.z 6.) "metric fit linear scale")
    [Match_size.Match_perimeter; Match_size.Match_area; Match_size.Match_volume];
  let source = Box_generator.box ~size:(Rays_math.Vec3.create 1. 2. 3.) () |> get_ok in
  let source_faces = Group.init ~owner:Group.Primitive ~name:"source_faces"
      (Geometry.primitive_count source) (fun _ -> true)
  and target_faces = Group.init ~owner:Group.Primitive ~name:"target_faces"
      (Geometry.primitive_count doubled) (fun _ -> true) in
  let selected = Match_size.run ~fit:Match_size.Match_area
      ~source_selection:(Transform_ops.Selected_primitives source_faces)
      ~target_selection:(Transform_ops.Selected_primitives target_faces)
      ~target:doubled source |> get_ok |> bounds in
  check (near selected.size.x 2. && near selected.size.y 4.
      && near selected.size.z 6.)
    "primitive-selected metric fit"

let check_normals () =
  let source = Line_geometry.polyline [|(0.,0.,0.); (1.,1.,0.)|] |> get_ok in
  let root = 1. /. sqrt 2. in
  let normal owner = Attribute.create_key_owned (Attribute.normal ~owner)
      (Packed.Float3.Private.of_owned_exn ~x:[|root; root|]
         ~y:[|root; root|] ~z:[|0.; 0.|]) |> Result.get_ok in
  let selected = Group.init ~owner:Group.Point ~name:"first" 2
      (fun point -> point = 0) in
  let source = source |> Geometry.with_attribute (normal Attribute.Point)
      |> Result.get_ok |> Geometry.with_attribute (normal Attribute.Vertex)
      |> Result.get_ok |> with_group selected in
  let output = Match_size.run ~fit:Match_size.Stretch
      ~selection:(Transform_ops.Selected_points selected)
      ~translate_axes:(false, false, false)
      ~target_center:Rays_math.Vec3.zero ~target_size:(Rays_math.Vec3.create 2. 1. 1.) source
      |> get_ok in
  let expected_x = 1. /. sqrt 5. and expected_y = 2. /. sqrt 5. in
  List.iter (fun owner ->
    let normals = normal_values owner output in
    check (near normals.x.(0) expected_x && near normals.y.(0) expected_y
        && near normals.x.(1) root && near normals.y.(1) root)
      "selected inverse-transpose normal transform")
    [Attribute.Point; Attribute.Vertex]

let check_validation () =
  let source = Box_generator.box ~size:(Rays_math.Vec3.create 1. 1. 1.) () |> get_ok
  and target = Box_generator.box ~size:(Rays_math.Vec3.create 2. 2. 2.) () |> get_ok in
  expect_code "invalid_geometry" (Match_size.run ~grain:0 ~target source);
  expect_code "invalid_geometry" (Match_size.run
      ~justify:(Rays_math.Vec3.create 2. 0. 0.) ~target source);
  expect_code "invalid_geometry" (Match_size.run
      ~offset:(Rays_math.Vec3.create Float.nan 0. 0.) ~target source);
  expect_code "invalid_geometry" (Match_size.run ~scale:(-1.) ~target source);
  expect_code "invalid_geometry" (Match_size.run
      ~target_size:(Rays_math.Vec3.create 1. (-1.) 1.) source);
  expect_code "invalid_geometry" (Match_size.run ~target
      ~target_center:Rays_math.Vec3.zero source);
  let target_points = Group.init ~owner:Group.Point ~name:"target" 24
      (fun point -> point = 0) in
  expect_code "invalid_geometry" (Match_size.run
      ~target_selection:(Transform_ops.Selected_points target_points) source);
  let malformed = Group.init ~owner:Group.Point ~name:"bad" 1 (fun _ -> true) in
  expect_code "invalid_geometry" (Match_size.run
      ~selection:(Transform_ops.Selected_points malformed) ~target source);
  let empty = Group.init ~owner:Group.Point ~name:"empty" 24 (fun _ -> false) in
  expect_code "invalid_geometry" (Match_size.run
      ~source_selection:(Transform_ops.Selected_points empty) ~target source);
  expect_code "invalid_geometry" (Match_size.run ~fit:Match_size.Match_area source);
  expect_code "invalid_geometry" (Match_size.run ~fit:Match_size.Match_x
      (Line_geometry.points [|(0.,0.,0.); (0.,1.,0.)|]));
  let points = Group.init ~owner:Group.Point ~name:"points" 24 (fun _ -> true) in
  expect_code "invalid_geometry" (Match_size.run ~fit:Match_size.Match_area
      ~source_selection:(Transform_ops.Selected_points points) ~target source);
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled" (Match_size.run ~cancel:cancelled ~target source)

let check_parallel_exact () =
  let source = Plane_generators.grid ~columns:800 ~rows:600 ~size:30. () |> get_ok
      |> Deform.noise_displace ~seed:934 ~amplitude:1.75 ~frequency:0.21 |> get_ok
      |> Normal_ops.run |> get_ok in
  let target = Box_generator.box ~size:(Rays_math.Vec3.create 8. 5. 12.) () |> get_ok
      |> Transform_ops.transform (Rays_math.Mat4.translation (Rays_math.Vec3.create 3. 7. (-2.))) in
  let run domains = Parallel.run ~domains (fun () ->
    Match_size.run ~grain:1024 ~fit:Match_size.Stretch
      ~scale_axes:(true, false, true)
      ~justify:(Rays_math.Vec3.create (-1.) 0. 1.)
      ~target_justify:(Rays_math.Vec3.create 1. (-1.) 0.)
      ~offset:(Rays_math.Vec3.create 0.25 0.5 (-0.75)) ~target source |> get_ok) in
  let one = run 1 and many = run 4 in
  check (equal_geometry one many)
    "one-domain and four-domain Match Size geometry differ";
  check (Geometry.point_count one = 481_401
      && Geometry.primitive_count one = 960_000)
    "Match Size scale fixture cardinality";
  let faces = Group.init ~owner:Group.Primitive ~name:"alternating_faces"
      (Geometry.primitive_count source) (fun primitive -> primitive mod 3 <> 0) in
  let run_selected domains = Parallel.run ~domains (fun () ->
    Match_size.run ~grain:1024 ~selection:(Transform_ops.Selected_primitives faces)
      ~source_selection:(Transform_ops.Selected_primitives faces) ~fit:Match_size.Contain
      ~target source |> get_ok) in
  let selected_one = run_selected 1 and selected_many = run_selected 4 in
  check (equal_geometry selected_one selected_many)
    "one-domain and four-domain incidence-selected Match Size differ"

let run () =
  check_numeric_targets ();
  check_selections ();
  check_fit_modes ();
  check_normals ();
  check_validation ();
  check_parallel_exact ();
  print_endline "match size tests passed"
