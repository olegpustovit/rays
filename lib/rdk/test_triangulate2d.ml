open Rdk_test_support
open Rays
open Rdk

let get = function Ok value -> value | Error error -> fail (Error.to_string error)

let topology_signature geometry =
  let topology = Topology.View.view (Geometry.topology geometry) in
  (Array.copy topology.vertex_points, Array.copy topology.primitive_offsets)

let position_signature geometry =
  let positions = Packed.Float3.View.view (Geometry.positions geometry) in
  (Array.copy positions.x, Array.copy positions.y, Array.copy positions.z)

let attribute_storage ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> Attribute.View.storage attribute
  | None -> fail ("missing attribute " ^ name)

let test_xy_payload_and_group () =
  let input = Line_geometry.points [| (0., 0., 3.); (1., 0., 4.); (1., 1., 5.); (0., 1., 6.) |] in
  let id =
    Attribute.create_owned ~name:"id" ~owner:Attribute.Point (Attribute.Int [| 10; 11; 12; 13 |])
    |> Result.get_ok
  in
  let selected = Group.init ~owner:Group.Point ~name:"selected" 4 (fun _ -> true) in
  let input =
    Geometry.with_attribute id input |> Result.get_ok |> Geometry.with_group selected
    |> Result.get_ok
  in
  let output =
    triangulate2d ~projection:Triangulate2d.Plane_xy ~triangle_group:"triangles" input |> get
  in
  check
    (Geometry.point_count output = 4
    && Geometry.primitive_count output = 2
    && Geometry.vertex_count output = 6)
    "XY cardinality";
  check
    (Geometry.positions output == Geometry.positions input)
    "positions were not structurally shared";
  check
    (Option.is_some (Geometry.find_attribute ~owner:Attribute.Point "id" output))
    "point payload was dropped";
  check
    (Option.is_some (Geometry.find_group ~owner:Group.Point "selected" output))
    "point group was dropped";
  match Geometry.find_group ~owner:Group.Primitive "triangles" output with
  | Some group -> check (Group.cardinality group = 2) "triangle group cardinality"
  | None -> fail "triangle group is missing"

let test_best_fit_and_explicit_plane () =
  let input =
    Line_geometry.points
      [| (0., 0., 0.); (1., 0., 1.); (1., 1., 3.); (0., 1., 2.); (0.5, 0.5, 1.5) |]
  in
  let best = triangulate2d input |> get in
  check (Geometry.primitive_count best = 4) "best-fit tilted plane cardinality";
  let explicit =
    triangulate2d
      ~projection:
        (Triangulate2d.Plane
           { origin = Rays_math.Vec3.zero; normal = Rays_math.Vec3.create (-1.) (-2.) 1. })
      input
    |> get
  in
  check (Geometry.primitive_count explicit = 4) "explicit tilted plane cardinality"

let test_projected_output_positions () =
  let selected =
    Group.ordered ~owner:Group.Point ~name:"selected" ~length:5 [| 0; 1; 2; 3 |] |> Result.get_ok
  in
  let xy =
    Line_geometry.points
      [| (0., 0., 10.); (2., 0., 20.); (2., 2., 30.); (0., 2., 40.); (9., 9., 9.) |]
    |> Geometry.with_group selected |> Result.get_ok
  in
  let xy =
    triangulate2d ~grain:1 ~selection:(Element_selection.Selected_points selected)
      ~projection:Triangulate2d.Plane_xy ~restore_original_point_positions:false xy
    |> get
  in
  let x, y, z = position_signature xy in
  check
    (x = [| 0.; 2.; 2.; 0.; 9. |] && y = [| 0.; 0.; 2.; 2.; 9. |] && z = [| 0.; 0.; 0.; 0.; 9. |])
    "XY projected output or unselected-point policy";
  let yz =
    Line_geometry.points [| (10., 0., 0.); (11., 1., 0.); (12., 1., 1.); (13., 0., 1.) |]
    |> triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_yz
         ~restore_original_point_positions:false
    |> get
  in
  let x, y, z = position_signature yz in
  check
    (x = [| 0.; 0.; 0.; 0. |] && y = [| 0.; 1.; 1.; 0. |] && z = [| 0.; 0.; 1.; 1. |])
    "YZ projected output";
  let zx =
    Line_geometry.points [| (0., 10., 0.); (0., 11., 1.); (1., 12., 1.); (1., 13., 0.) |]
    |> triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_zx
         ~restore_original_point_positions:false
    |> get
  in
  let x, y, z = position_signature zx in
  check
    (x = [| 0.; 0.; 1.; 1. |] && y = [| 0.; 0.; 0.; 0. |] && z = [| 0.; 1.; 1.; 0. |])
    "ZX projected output";
  let explicit_source =
    Line_geometry.points [| (0., 0., -5.); (2., 0., 20.); (2., 2., -10.); (0., 2., 30.) |]
  in
  let explicit =
    triangulate2d ~grain:1
      ~projection:
        (Triangulate2d.Plane
           { origin = Rays_math.Vec3.create 0. 0. 10.; normal = Rays_math.Vec3.create 0. 0. 1. })
      ~restore_original_point_positions:false explicit_source
    |> get
  in
  let x, y, z = position_signature explicit in
  check
    (x = [| 0.; 2.; 2.; 0. |] && y = [| 0.; 0.; 2.; 2. |] && z = [| 10.; 10.; 10.; 10. |])
    "explicit-plane projected output";
  let best_source =
    Line_geometry.points [| (0., 0., 3.); (2., 0., 5.); (2., 2., 9.); (0., 2., 7.); (1., 1., 6.) |]
  in
  let best = triangulate2d ~grain:1 ~restore_original_point_positions:false best_source |> get in
  check
    (Geometry.positions best != Geometry.positions best_source)
    "best-fit projected output retained source P identity";
  let bx, by, bz = position_signature best in
  for point = 0 to Array.length bx - 1 do
    check
      (abs_float (bz.(point) -. bx.(point) -. (2. *. by.(point)) -. 3.) < 1e-10)
      "best-fit output left its fitted plane"
  done;
  let attribute_source =
    Line_geometry.points [| (9., 9., 9.); (8., 8., 8.); (7., 7., 7.); (6., 6., 6.) |]
  in
  let coordinates =
    Attribute.create_owned ~owner:Attribute.Point ~name:"planar3"
      (Attribute.Float3
         (Packed.Float3.View.of_owned_exn ~x:[| 0.; 2.; 2.; 0. |] ~y:[| 0.; 0.; 2.; 2. |]
            ~z:[| nan; 100.; -200.; infinity |]))
    |> Result.get_ok
  in
  let attribute_source = Geometry.with_attribute coordinates attribute_source |> Result.get_ok in
  let attribute_output =
    triangulate2d ~grain:1 ~projection:(Triangulate2d.Point_attribute "planar3")
      ~restore_original_point_positions:false attribute_source
    |> get
  in
  let x, y, z = position_signature attribute_output in
  check
    (x = [| 0.; 2.; 2.; 0. |] && y = [| 0.; 0.; 2.; 2. |] && z = [| 0.; 0.; 0.; 0. |])
    "float3 projection did not use exactly its first two components";
  let refined_source =
    Line_geometry.points [| (0., 0., 10.); (2., 0., 20.); (2., 2., 30.); (0., 2., 40.) |]
  in
  let run domains =
    Parallel.run ~domains (fun () ->
        triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy
          ~restore_original_point_positions:false ~refine:true ~maximum_area:0.2
          ~maximum_new_points:64 ~regularization_steps:2 refined_source
        |> get)
  in
  let refined = run 1 and parallel = run 4 in
  check
    (topology_signature refined = topology_signature parallel
    && position_signature refined = position_signature parallel)
    "projected refinement differs across domains";
  let _, _, z = position_signature refined in
  check
    (Geometry.point_count refined > 4 && Array.for_all (( = ) 0.) z)
    "generated/refined points were not embedded on the output plane"

let test_attribute_and_selection () =
  let input =
    Line_geometry.points [| (0., 0., 0.); (1., 0., 0.); (2., 0., 0.); (3., 0., 0.); (4., 0., 0.) |]
  in
  let uv =
    Packed.Float2.of_owned ~x:[| 0.; 1.; 1.; 0.; 5. |] ~y:[| 0.; 0.; 1.; 1.; 5. |] |> Result.get_ok
  in
  let uv =
    Attribute.create_owned ~name:"planar" ~owner:Attribute.Point (Attribute.Float2 uv)
    |> Result.get_ok
  in
  let input = Geometry.with_attribute uv input |> Result.get_ok in
  let group =
    Group.ordered ~owner:Group.Point ~name:"four" ~length:5 [| 0; 1; 2; 3 |] |> Result.get_ok
  in
  let output =
    triangulate2d ~selection:(Element_selection.Selected_points group)
      ~projection:(Triangulate2d.Point_attribute "planar") input
    |> get
  in
  check (Geometry.primitive_count output = 2) "attribute/selection cardinality";
  let topology = Topology.View.view (Geometry.topology output) in
  check
    (Array.for_all (fun point -> point < 4) topology.vertex_points)
    "unselected point entered triangulation"

let test_keep_primitives_payload_groups_and_edges () =
  let positions =
    Packed.Float3.View.of_owned_exn ~x:[| 0.; 2.; 2.; 0.; 0.5; 1.5 |]
      ~y:[| 0.; 0.; 2.; 2.; 0.5; 1.5 |] ~z:[| 0.; 1.; 2.; 3.; 4.; 5. |]
  in
  let topology =
    Topology.create_owned ~point_count:6 ~vertex_points:[| 0; 1; 2; 3; 0; 2; 4; 5 |]
      ~primitive_offsets:[| 0; 4; 6; 8 |]
      ~primitive_kinds:[| Topology.Polygon; Topology.Open_polyline; Topology.Open_polyline |]
    |> Result.get_ok
  in
  let vertex_weight =
    Attribute.create_owned ~owner:Attribute.Vertex ~name:"vertex_weight"
      (Attribute.Float [| 10.; 11.; 12.; 13.; 14.; 15.; 16.; 17. |])
    |> Result.get_ok
  and vertex_id =
    Attribute.create_owned ~owner:Attribute.Vertex ~name:"vertex_id"
      (Attribute.Int [| 10; 11; 12; 13; 14; 15; 16; 17 |])
    |> Result.get_ok
  and vertex_rows =
    Attribute.create_owned ~owner:Attribute.Vertex ~name:"vertex_rows"
      (Attribute.Int_array
         (Packed.Int_array.create_owned ~offsets:[| 0; 1; 3; 3; 4; 5; 7; 8; 10 |]
            ~values:[| 0; 10; 11; 30; 40; 50; 51; 60; 70; 71 |]
         |> Result.get_ok))
    |> Result.get_ok
  and vertex_float_rows =
    Attribute.create_owned ~owner:Attribute.Vertex ~name:"vertex_float_rows"
      (Attribute.Float_array
         (Packed.Float_array.create_owned ~offsets:[| 0; 1; 3; 3; 4; 5; 7; 8; 10 |]
            ~values:[| 0.; 10.; 11.; 30.; 40.; 50.; 51.; 60.; 70.; 71. |]
         |> Result.get_ok))
    |> Result.get_ok
  and vertex_uv =
    Attribute.create_owned ~owner:Attribute.Vertex ~name:"vertex_uv"
      (Attribute.Float2
         (Packed.Float2.of_owned
            ~x:[| 0.; 1.; 2.; 3.; 4.; 5.; 6.; 7. |]
            ~y:[| 10.; 11.; 12.; 13.; 14.; 15.; 16.; 17. |]
         |> Result.get_ok))
    |> Result.get_ok
  and vertex_vector =
    Attribute.create_owned ~owner:Attribute.Vertex ~name:"vertex_vector"
      (Attribute.Float3
         (Packed.Float3.View.of_owned_exn
            ~x:[| 0.; 1.; 2.; 3.; 4.; 5.; 6.; 7. |]
            ~y:[| 10.; 11.; 12.; 13.; 14.; 15.; 16.; 17. |]
            ~z:[| 20.; 21.; 22.; 23.; 24.; 25.; 26.; 27. |]))
    |> Result.get_ok
  and vertex_color =
    Attribute.create_owned ~owner:Attribute.Vertex ~name:"vertex_color"
      (Attribute.Float4
         (Packed.Float4.of_owned
            ~x:[| 0.; 1.; 2.; 3.; 4.; 5.; 6.; 7. |]
            ~y:[| 10.; 11.; 12.; 13.; 14.; 15.; 16.; 17. |]
            ~z:[| 20.; 21.; 22.; 23.; 24.; 25.; 26.; 27. |]
            ~w:[| 30.; 31.; 32.; 33.; 34.; 35.; 36.; 37. |]
         |> Result.get_ok))
    |> Result.get_ok
  and primitive_label =
    Attribute.create_owned ~owner:Attribute.Primitive ~name:"primitive_label"
      (Attribute.Text [| "surface"; "constraint"; "guide" |])
    |> Result.get_ok
  and detail_id =
    Attribute.create_owned ~owner:Attribute.Detail ~name:"detail_id" (Attribute.Int [| 73 |])
    |> Result.get_ok
  in
  let constraints =
    Group.init ~owner:Group.Primitive ~name:"constraints" 3 (fun primitive -> primitive = 1)
  and vertex_order =
    Group.ordered ~owner:Group.Vertex ~name:"vertex_order" ~length:8 [| 7; 4; 2; 0 |]
    |> Result.get_ok
  and primitive_order =
    Group.ordered ~owner:Group.Primitive ~name:"primitive_order" ~length:3 [| 2; 1; 0 |]
    |> Result.get_ok
  and old_triangles =
    Group.init ~owner:Group.Primitive ~name:"triangles" 3 (fun primitive -> primitive = 0)
  in
  let source_index = Topology_index.create topology in
  let retained_edge = Topology_index.find_edge_index source_index ~a:0 ~b:1 in
  let boundary =
    Edge_group.init ~topology ~index:source_index ~name:"boundary" (fun edge ->
        edge = retained_edge)
  and old_recovered =
    Edge_group.init ~topology ~index:source_index ~name:"recovered" (fun edge ->
        edge = retained_edge)
  in
  let input =
    Geometry.create ~positions ~topology
      ~attributes:
        [
          vertex_weight;
          vertex_id;
          vertex_rows;
          vertex_float_rows;
          vertex_uv;
          vertex_vector;
          vertex_color;
          primitive_label;
          detail_id;
        ]
      ~groups:[ constraints; vertex_order; primitive_order; old_triangles ]
      ~edge_groups:[ boundary; old_recovered ] ()
    |> Result.get_ok
  in
  let run domains =
    Parallel.run ~domains (fun () ->
        triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy ~constraint_primitives:constraints
          ~keep_primitives:true ~triangle_group:"triangles" ~constraint_group:"recovered" input
        |> get)
  in
  let output = run 1 and parallel = run 4 in
  check
    (topology_signature output = topology_signature parallel)
    "Keep Primitives topology differs across domains";
  let output_topology = Geometry.topology output in
  let view = Topology.View.view output_topology in
  check
    (Geometry.primitive_count output > 2
    && Topology.primitive_kind output_topology 0 = Topology.Polygon
    && Topology.primitive_kind output_topology 1 = Topology.Open_polyline
    && Array.sub view.vertex_points 0 6 = [| 0; 1; 2; 3; 4; 5 |]
    && Array.sub view.primitive_offsets 0 3 = [| 0; 4; 6 |])
    "Keep Primitives did not preserve the non-constraint primitive prefix";
  (match attribute_storage ~owner:Attribute.Vertex "vertex_weight" output with
  | Attribute.Float values ->
      check
        (Array.sub values 0 6 = [| 10.; 11.; 12.; 13.; 16.; 17. |]
        && Array.for_all (( = ) 0.) (Array.sub values 6 (Array.length values - 6)))
        "Keep Primitives vertex fixed payload/defaults"
  | _ -> fail "Keep Primitives vertex storage changed");
  (match attribute_storage ~owner:Attribute.Vertex "vertex_rows" output with
  | Attribute.Int_array values ->
      check
        (Packed.Int_array.get values 0 = [| 0 |]
        && Packed.Int_array.get values 1 = [| 10; 11 |]
        && Packed.Int_array.get values 4 = [| 60 |]
        && Packed.Int_array.get values 5 = [| 70; 71 |]
        && Packed.Int_array.get values 6 = [||])
        "Keep Primitives vertex ragged payload/defaults"
  | _ -> fail "Keep Primitives vertex ragged storage changed");
  (match attribute_storage ~owner:Attribute.Vertex "vertex_id" output with
  | Attribute.Int values ->
      check
        (Array.sub values 0 6 = [| 10; 11; 12; 13; 16; 17 |] && values.(6) = 0)
        "Keep Primitives vertex integer payload/defaults"
  | _ -> fail "Keep Primitives vertex integer storage changed");
  (match attribute_storage ~owner:Attribute.Vertex "vertex_float_rows" output with
  | Attribute.Float_array values ->
      check
        (Packed.Float_array.get values 5 = [| 70.; 71. |])
        "Keep Primitives vertex float-row payload";
      let generated = Packed.Float_array.get values 6 in
      check (Array.length generated = 0) "Keep Primitives vertex float-row defaults"
  | _ -> fail "Keep Primitives vertex float-row storage changed");
  (match attribute_storage ~owner:Attribute.Vertex "vertex_uv" output with
  | Attribute.Float2 values ->
      let values = Packed.Float2.View.view values in
      check
        (values.x.(4) = 6. && values.y.(5) = 17. && values.x.(6) = 0. && values.y.(6) = 0.)
        "Keep Primitives vertex float2 payload/defaults"
  | _ -> fail "Keep Primitives vertex float2 storage changed");
  (match attribute_storage ~owner:Attribute.Vertex "vertex_vector" output with
  | Attribute.Float3 values ->
      let values = Packed.Float3.View.view values in
      check
        (values.x.(4) = 6.
        && values.z.(5) = 27.
        && values.x.(6) = 0.
        && values.y.(6) = 0.
        && values.z.(6) = 0.)
        "Keep Primitives vertex float3 payload/defaults"
  | _ -> fail "Keep Primitives vertex float3 storage changed");
  (match attribute_storage ~owner:Attribute.Vertex "vertex_color" output with
  | Attribute.Float4 values ->
      let values = Packed.Float4.View.view values in
      check
        (values.x.(4) = 6. && values.w.(5) = 37. && values.x.(6) = 0. && values.w.(6) = 0.)
        "Keep Primitives vertex float4 payload/defaults"
  | _ -> fail "Keep Primitives vertex float4 storage changed");
  (match attribute_storage ~owner:Attribute.Primitive "primitive_label" output with
  | Attribute.Text values ->
      check
        (values.(0) = "surface"
        && values.(1) = "guide"
        && Array.for_all (( = ) "") (Array.sub values 2 (Array.length values - 2)))
        "Keep Primitives primitive payload/defaults"
  | _ -> fail "Keep Primitives primitive storage changed");
  (match attribute_storage ~owner:Attribute.Detail "detail_id" output with
  | Attribute.Int values -> check (values = [| 73 |]) "Keep Primitives detail payload"
  | _ -> fail "Keep Primitives detail storage changed");
  (match Geometry.find_group ~owner:Group.Vertex "vertex_order" output with
  | Some group ->
      check
        (Group.ordered_elements group = Some [| 5; 2; 0 |])
        "Keep Primitives ordered vertex-group ancestry"
  | None -> fail "Keep Primitives vertex group missing");
  (match Geometry.find_group ~owner:Group.Primitive "primitive_order" output with
  | Some group ->
      check
        (Group.ordered_elements group = Some [| 1; 0 |])
        "Keep Primitives ordered primitive-group ancestry"
  | None -> fail "Keep Primitives primitive group missing");
  (match Geometry.find_group ~owner:Group.Primitive "constraints" output with
  | Some group ->
      check (Group.cardinality group = 0) "Keep Primitives retained constraint primitive membership"
  | None -> fail "Keep Primitives constraint source group missing");
  (match Geometry.find_group ~owner:Group.Primitive "triangles" output with
  | Some group ->
      check
        (Group.cardinality group = Geometry.primitive_count output - 2
        && (not (Group.mem 0 group))
        && not (Group.mem 1 group))
        "Keep Primitives triangle output group"
  | None -> fail "Keep Primitives triangle group missing");
  let output_index = Topology_index.create output_topology in
  let output_edge = Topology_index.find_edge_index output_index ~a:0 ~b:1 in
  (match Geometry.find_edge_group "boundary" output with
  | Some group ->
      check
        (output_edge >= 0 && Edge_group.mem output_edge group)
        "Keep Primitives native-edge ancestry"
  | None -> fail "Keep Primitives native edge group missing");
  (match Geometry.find_edge_group "recovered" output with
  | Some group ->
      check
        (Edge_group.cardinality group = 3 && not (Edge_group.mem output_edge group))
        "Keep Primitives constraint group did not replace a colliding source name"
  | None -> fail "Keep Primitives recovered constraint group missing");
  let parallel_weights =
    match attribute_storage ~owner:Attribute.Vertex "vertex_weight" parallel with
    | Attribute.Float values -> values
    | _ -> fail "parallel Keep Primitives vertex storage changed"
  in
  check
    (match attribute_storage ~owner:Attribute.Vertex "vertex_weight" output with
    | Attribute.Float values -> values = parallel_weights
    | _ -> false)
    "Keep Primitives payload differs across domains";
  let duplicate_positions =
    Packed.Float3.View.of_owned_exn ~x:[| 0.; 1.; 1.; 0.; 0. |] ~y:[| 0.; 0.; 1.; 1.; 0. |]
      ~z:[| 0.; 0.; 0.; 0.; 9. |]
  in
  let duplicate_topology =
    Topology.polygons_owned ~point_count:5 ~vertex_points:[| 4; 1; 2 |]
      ~primitive_offsets:[| 0; 3 |]
    |> Result.get_ok
  in
  let duplicate_input =
    Geometry.create ~positions:duplicate_positions ~topology:duplicate_topology () |> Result.get_ok
  in
  let deduplicated =
    triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy ~keep_primitives:true
      ~remove_duplicate_points:true duplicate_input
    |> get
  in
  let deduplicated_topology = Topology.View.view (Geometry.topology deduplicated) in
  check
    (Geometry.point_count deduplicated = 4
    && Geometry.primitive_count deduplicated = 3
    && Array.sub deduplicated_topology.vertex_points 0 3 = [| 0; 1; 2 |])
    "Keep Primitives duplicate compaction deleted rather than rewired a retained face"

let constrained_square () =
  let positions =
    Packed.Float3.View.of_owned_exn ~x:[| 0.; 1.; 1.; 0. |] ~y:[| 0.; 0.; 1.; 1. |]
      ~z:[| 0.; 0.; 0.; 0. |]
  in
  let topology =
    Topology.create_owned ~point_count:4 ~vertex_points:[| 1; 3 |] ~primitive_offsets:[| 0; 2 |]
      ~primitive_kinds:[| Topology.Open_polyline |]
    |> Result.get_ok
  in
  let geometry = Geometry.create ~positions ~topology () |> Result.get_ok in
  let primitives = Group.init ~owner:Group.Primitive ~name:"constraint" 1 (fun _ -> true) in
  (geometry, primitives)

let check_constraint_edge geometry =
  match Geometry.find_edge_group "constraints" geometry with
  | None -> fail "constraint output group is missing"
  | Some group ->
      check (Edge_group.cardinality group = 1) "constraint group cardinality";
      let index = Topology_index.create (Geometry.topology geometry) in
      let edge = Topology_index.find_edge_index index ~a:1 ~b:3 in
      check (edge >= 0 && Edge_group.mem edge group) "forced constraint edge is missing"

let test_constraints () =
  let input, primitives = constrained_square () in
  let output =
    triangulate2d ~projection:Triangulate2d.Plane_xy ~constraint_primitives:primitives
      ~constraint_group:"constraints" input
    |> get
  in
  check_constraint_edge output;
  let source_index = Topology_index.create (Geometry.topology input) in
  let edge = Topology_index.find_edge_index source_index ~a:1 ~b:3 in
  let edges =
    Edge_group.init ~topology:(Geometry.topology input) ~index:source_index ~name:"edge_constraint"
      (fun candidate -> candidate = edge)
  in
  let output =
    triangulate2d ~projection:Triangulate2d.Plane_xy ~constraint_edges:edges
      ~constraint_group:"constraints" input
    |> get
  in
  check_constraint_edge output;
  let positions =
    Packed.Float3.View.of_owned_exn ~x:[| 0.; 1.; 2.; 0.; 2.; 1. |] ~y:[| 0.; 0.; 0.; 2.; 2.; 1. |]
      ~z:[| 0.; 0.; 0.; 0.; 0.; 0. |]
  in
  let topology =
    Topology.create_owned ~point_count:6 ~vertex_points:[| 0; 2 |] ~primitive_offsets:[| 0; 2 |]
      ~primitive_kinds:[| Topology.Open_polyline |]
    |> Result.get_ok
  in
  let geometry = Geometry.create ~positions ~topology () |> Result.get_ok in
  let primitives = Group.init ~owner:Group.Primitive ~name:"embedded" 1 (fun _ -> true) in
  let run domains =
    Parallel.run ~domains (fun () ->
        triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy ~constraint_primitives:primitives
          ~constraint_group:"constraints" geometry
        |> get)
  in
  let embedded = run 1 and parallel = run 4 in
  check
    (Geometry.point_count embedded = 6)
    "authored constraint point was materialized as a new point";
  check
    (topology_signature embedded = topology_signature parallel)
    "embedded constraint point differs across domains";
  match Geometry.find_edge_group "constraints" embedded with
  | Some group ->
      check (Edge_group.cardinality group = 2) "embedded point did not split the output constraint"
  | None -> fail "embedded constraint output group is missing"

let crossing_constraints () =
  let positions =
    Packed.Float3.View.of_owned_exn ~x:[| 0.; 2.; 2.; 0. |] ~y:[| 0.; 0.; 2.; 2. |]
      ~z:[| 0.; 100.; 10.; 200. |]
  in
  let topology =
    Topology.create_owned ~point_count:4 ~vertex_points:[| 0; 2; 1; 3 |]
      ~primitive_offsets:[| 0; 2; 4 |]
      ~primitive_kinds:[| Topology.Open_polyline; Topology.Open_polyline |]
    |> Result.get_ok
  in
  let weight =
    Attribute.create_owned ~name:"weight" ~owner:Attribute.Point
      (Attribute.Float [| 0.; 100.; 10.; 200. |])
    |> Result.get_ok
  and id =
    Attribute.create_owned ~name:"id" ~owner:Attribute.Point (Attribute.Int [| 10; 11; 12; 13 |])
    |> Result.get_ok
  and label =
    Attribute.create_owned ~name:"label" ~owner:Attribute.Point
      (Attribute.Text [| "a"; "b"; "c"; "d" |])
    |> Result.get_ok
  in
  let selected = Group.init ~owner:Group.Point ~name:"selected" 4 (fun point -> point = 2) in
  let constraints = Group.init ~owner:Group.Primitive ~name:"constraints" 2 (fun _ -> true) in
  ( Geometry.create ~positions ~topology ~attributes:[ weight; id; label ]
      ~groups:[ selected; constraints ] ()
    |> Result.get_ok,
    constraints )

let test_crossing_constraints_and_payload () =
  let input, constraints = crossing_constraints () in
  (match
     triangulate2d ~projection:Triangulate2d.Plane_xy ~constraint_primitives:constraints input
   with
  | Error _ -> ()
  | Ok _ -> fail "crossing constraints succeeded without opt-in splitting");
  (match
     triangulate2d ~projection:Triangulate2d.Plane_xy ~constraint_primitives:constraints
       ~split_crossing_constraints:true ~refine:true ~maximum_new_points:0 input
   with
  | Error _ -> ()
  | Ok _ -> fail "required arrangement split escaped the refinement budget");
  let run domains =
    Parallel.run ~domains (fun () ->
        triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy ~constraint_primitives:constraints
          ~split_crossing_constraints:true ~split_point_group:"crossings"
          ~constraint_group:"constraints" input
        |> get)
  in
  let output = run 1 and parallel = run 4 in
  check
    (Geometry.point_count output = 5
    && Geometry.primitive_count output = 4
    && Geometry.vertex_count output = 12)
    "crossing output cardinality";
  check
    (topology_signature output = topology_signature parallel)
    "crossing one/four-domain topology differs";
  let positions = Packed.Float3.View.view (Geometry.positions output) in
  check
    (positions.x.(4) = 1. && positions.y.(4) = 1. && positions.z.(4) = 5.)
    "crossing 3D interpolation";
  let float_attribute name geometry =
    match Geometry.find_attribute ~owner:Attribute.Point name geometry with
    | Some attribute -> (
        match Attribute.View.storage attribute with
        | Attribute.Float values -> values
        | _ -> fail "float storage changed")
    | None -> fail ("missing point attribute " ^ name)
  in
  let int_attribute name geometry =
    match Geometry.find_attribute ~owner:Attribute.Point name geometry with
    | Some attribute -> (
        match Attribute.View.storage attribute with
        | Attribute.Int values -> values
        | _ -> fail "integer storage changed")
    | None -> fail ("missing point attribute " ^ name)
  in
  let text_attribute name geometry =
    match Geometry.find_attribute ~owner:Attribute.Point name geometry with
    | Some attribute -> (
        match Attribute.View.storage attribute with
        | Attribute.Text values -> values
        | _ -> fail "text storage changed")
    | None -> fail ("missing point attribute " ^ name)
  in
  let weights = float_attribute "weight" output
  and parallel_weights = float_attribute "weight" parallel in
  check (weights = parallel_weights && weights.(4) = 5.) "crossing float payload interpolation";
  check
    ((int_attribute "id" output).(4) = 12 && (text_attribute "label" output).(4) = "c")
    "crossing discrete payload policy";
  (match Geometry.find_group ~owner:Group.Point "selected" output with
  | Some group -> check (Group.mem 4 group) "split point group ancestry"
  | None -> fail "source point group was dropped");
  (match Geometry.find_group ~owner:Group.Point "crossings" output with
  | Some group -> check (Group.cardinality group = 1 && Group.mem 4 group) "split output group"
  | None -> fail "split output group is missing");
  (match Geometry.find_edge_group "constraints" output with
  | Some group -> check (Edge_group.cardinality group = 4) "split constraint edge cardinality"
  | None -> fail "split constraint output group is missing");
  let projected =
    triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy ~constraint_primitives:constraints
      ~split_crossing_constraints:true ~restore_original_point_positions:false input
    |> get
  in
  let _, _, projected_z = position_signature projected in
  check
    (Array.for_all (( = ) 0.) projected_z)
    "crossing split point was not embedded on the projected output plane";
  let authored_positions =
    Packed.Float3.View.of_owned_exn ~x:[| 0.; 2.; 2.; 0.; 1. |] ~y:[| 0.; 0.; 2.; 2.; 1. |]
      ~z:[| 0.; 0.; 0.; 0.; 7. |]
  in
  let authored_topology =
    Topology.create_owned ~point_count:5 ~vertex_points:[| 0; 2; 1; 3 |]
      ~primitive_offsets:[| 0; 2; 4 |]
      ~primitive_kinds:[| Topology.Open_polyline; Topology.Open_polyline |]
    |> Result.get_ok
  in
  let authored =
    Geometry.create ~positions:authored_positions ~topology:authored_topology () |> Result.get_ok
  in
  let authored_constraints =
    Group.init ~owner:Group.Primitive ~name:"authored_crossing" 2 (fun _ -> true)
  in
  let authored =
    triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy
      ~constraint_primitives:authored_constraints ~split_crossing_constraints:true
      ~split_point_group:"generated" ~constraint_group:"constraints" authored
    |> get
  in
  check
    (Geometry.point_count authored = 5 && Geometry.primitive_count authored = 4)
    "authored crossing point was duplicated by exact construction";
  (match Geometry.find_group ~owner:Group.Point "generated" authored with
  | Some group ->
      check (Group.cardinality group = 0) "authored crossing point was marked as generated"
  | None -> fail "authored crossing generated-point group is missing");
  match Geometry.find_edge_group "constraints" authored with
  | Some group ->
      check (Edge_group.cardinality group = 4) "authored crossing constraint cardinality"
  | None -> fail "authored crossing constraint group is missing"

let test_hull_boundary_flood () =
  let positions =
    Packed.Float3.View.of_owned_exn
      ~x:[| -2.; 2.; 2.; -2.; -1.; 1.; 1.; -1.; 0. |]
      ~y:[| -2.; -2.; 2.; 2.; -1.; -1.; 1.; 1.; 0. |]
      ~z:[| 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0. |]
  in
  let topology =
    Topology.create_owned ~point_count:9 ~vertex_points:[| 4; 5; 6; 7 |]
      ~primitive_offsets:[| 0; 4 |] ~primitive_kinds:[| Topology.Closed_polyline |]
    |> Result.get_ok
  in
  let constraint_group = Group.init ~owner:Group.Primitive ~name:"boundary" 1 (fun _ -> true) in
  let input =
    Geometry.create ~positions ~topology ~groups:[ constraint_group ] () |> Result.get_ok
  in
  let run domains =
    Parallel.run ~domains (fun () ->
        triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy
          ~constraint_primitives:constraint_group ~flood_from_hull_boundary:true
          ~constraint_group:"boundary" input
        |> get)
  in
  let output = run 1 and parallel = run 4 in
  check
    (topology_signature output = topology_signature parallel)
    "hull flood one/four-domain topology differs";
  check (Geometry.primitive_count output = 4) "hull flood retained the wrong triangle count";
  let view = Topology.View.view (Geometry.topology output) in
  check
    (Array.for_all (fun point -> point >= 4) view.vertex_points)
    "hull flood retained exterior geometry";
  (match Geometry.find_edge_group "boundary" output with
  | Some group -> check (Edge_group.cardinality group = 4) "hull flood constrained-edge cardinality"
  | None -> fail "hull flood constraint group is missing");
  let polygon =
    triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy
      ~constraint_primitives:constraint_group ~remove_outside_constraint_polygons:true
      ~constraint_group:"boundary" input
    |> get
  in
  check
    (topology_signature polygon = topology_signature output)
    "constraint-polygon winding differs from simple hull-flood interior";
  let empty =
    triangulate2d ~projection:Triangulate2d.Plane_xy ~flood_from_hull_boundary:true
      (Line_geometry.points [| (0., 0., 0.); (1., 0., 0.); (0., 1., 0.) |])
    |> get
  in
  check
    (Geometry.primitive_count empty = 0)
    "unblocked adapter hull flood did not remove all triangles";
  let edge_only =
    triangulate2d ~projection:Triangulate2d.Plane_xy
      ~constraint_edges:
        (let index = Topology_index.create topology in
         Edge_group.init ~topology ~index ~name:"edge_only" (fun _ -> true))
      ~remove_outside_constraint_polygons:true input
    |> get
  in
  check
    (Geometry.primitive_count edge_only = 0)
    "edge-only constraints incorrectly contributed polygon winding"

let test_ignore_non_constraint_points () =
  let positions =
    Packed.Float3.View.of_owned_exn ~x:[| 0.; 2.; 2.; 0.; 1. |] ~y:[| 0.; 0.; 2.; 2.; 1. |]
      ~z:[| 0.; 0.; 0.; 0.; 9. |]
  in
  let topology =
    Topology.polygons_owned ~point_count:5 ~vertex_points:[| 0; 1; 2; 3 |]
      ~primitive_offsets:[| 0; 4 |]
    |> Result.get_ok
  in
  let authored_normal =
    Attribute.create_owned ~name:"N" ~owner:Attribute.Point
      (Attribute.Float3
         (Packed.Float3.View.of_owned_exn ~x:(Array.make 5 1.) ~y:(Array.make 5 0.)
            ~z:(Array.make 5 0.)))
    |> Result.get_ok
  in
  let input =
    Geometry.create ~positions ~topology ~attributes:[ authored_normal ] () |> Result.get_ok
  in
  let boundary = Group.init ~owner:Group.Primitive ~name:"boundary" 1 (fun _ -> true) in
  let ordinary =
    triangulate2d ~projection:Triangulate2d.Plane_xy ~constraint_primitives:boundary input |> get
  in
  check
    (Geometry.primitive_count ordinary = 4)
    "ordinary triangulation did not include the interior point";
  let run domains =
    Parallel.run ~domains (fun () ->
        triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy ~constraint_primitives:boundary
          ~ignore_non_constraint_points:true ~constraint_group:"boundary" input
        |> get)
  in
  let ignored = run 1 and parallel = run 4 in
  check
    (Geometry.point_count ignored = 5 && Geometry.primitive_count ignored = 2)
    "Ignore Non-Constraint Points cardinality";
  check
    (topology_signature ignored = topology_signature parallel)
    "Ignore Non-Constraint Points differs across domains";
  let view = Topology.View.view (Geometry.topology ignored) in
  check
    (Array.for_all (fun point -> point <> 4) view.vertex_points)
    "ignored interior point entered output topology";
  (match Geometry.find_edge_group "boundary" ignored with
  | Some group ->
      check (Edge_group.cardinality group = 4) "Ignore Non-Constraint Points lost constraint edges"
  | None -> fail "Ignore Non-Constraint Points boundary group is missing");
  let compacted =
    triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy ~constraint_primitives:boundary
      ~ignore_non_constraint_points:true ~remove_unused_points:true ~recompute_point_normals:true
      input
    |> get
  in
  check (Geometry.point_count compacted = 4) "Triangulate 2D unused-point compaction cardinality";
  (match Geometry.find_attribute ~owner:Attribute.Point "N" compacted with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Float3 values ->
          let values = Packed.Float3.View.view values in
          check
            (Array.for_all (( = ) 0.) values.x
            && Array.for_all (( = ) 0.) values.y
            && Array.for_all (( = ) 1.) values.z)
            "Triangulate 2D point normals were not recomputed"
      | _ -> fail "Triangulate 2D recomputed N changed storage")
  | None -> fail "Triangulate 2D did not recompute existing point N");
  match
    triangulate2d ~projection:Triangulate2d.Plane_xy ~ignore_non_constraint_points:true
      (Line_geometry.points [| (0., 0., 0.); (1., 0., 0.); (0., 1., 0.) |])
  with
  | Error _ -> ()
  | Ok _ -> fail "Ignore Non-Constraint Points succeeded without constraints"

let test_remove_duplicate_points () =
  let input =
    Line_geometry.points
      [| (0., 0., 0.); (1., 0., 0.); (1., 1., 0.); (0., 1., 0.); (0., 0., 7.); (9., 9., 9.) |]
  in
  let id =
    Attribute.create_owned ~name:"id" ~owner:Attribute.Point
      (Attribute.Int [| 10; 11; 12; 13; 14; 15 |])
    |> Result.get_ok
  in
  let selected =
    Group.ordered ~owner:Group.Point ~name:"selected" ~length:6 [| 0; 1; 2; 3; 4 |] |> Result.get_ok
  and markers =
    Group.init ~owner:Group.Point ~name:"markers" 6 (fun point -> point = 4 || point = 5)
  in
  let input =
    Geometry.with_attribute id input |> Result.get_ok |> Geometry.with_group selected
    |> Result.get_ok |> Geometry.with_group markers |> Result.get_ok
  in
  let ordinary =
    triangulate2d ~projection:Triangulate2d.Plane_xy
      ~selection:(Element_selection.Selected_points selected) input
    |> get
  in
  check
    (Geometry.point_count ordinary = 6)
    "projected duplicate was removed without the output policy";
  let run domains =
    Parallel.run ~domains (fun () ->
        triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy
          ~selection:(Element_selection.Selected_points selected) ~remove_duplicate_points:true
          input
        |> get)
  in
  let output = run 1 and parallel = run 4 in
  check
    (Geometry.point_count output = 5 && Geometry.primitive_count output = 2)
    "projected duplicate removal cardinality";
  check
    (topology_signature output = topology_signature parallel)
    "projected duplicate removal topology differs across domains";
  let positions = Packed.Float3.View.view (Geometry.positions output) in
  check
    (positions.x = [| 0.; 1.; 1.; 0.; 9. |]
    && positions.y = [| 0.; 0.; 1.; 1.; 9. |]
    && positions.z = [| 0.; 0.; 0.; 0.; 9. |])
    "projected duplicate removal deleted or reordered an unrelated point";
  (match Geometry.find_attribute ~owner:Attribute.Point "id" output with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Int values ->
          check (values = [| 10; 11; 12; 13; 15 |]) "projected duplicate point attribute remap"
      | _ -> fail "projected duplicate point attribute changed storage")
  | None -> fail "projected duplicate removal dropped point payload");
  (match Geometry.find_group ~owner:Group.Point "selected" output with
  | Some group ->
      check
        (Group.cardinality group = 4 && not (Group.mem 4 group))
        "projected duplicate selection group remap"
  | None -> fail "projected duplicate removal dropped selection group");
  match Geometry.find_group ~owner:Group.Point "markers" output with
  | Some group ->
      check
        (Group.cardinality group = 1 && Group.mem 4 group)
        "projected duplicate removal lost unrelated group membership"
  | None -> fail "projected duplicate removal dropped marker group"

let test_quality_refinement () =
  let input = Line_geometry.points [| (0., 0., 0.); (2., 0., 2.); (2., 2., 6.); (0., 2., 4.) |] in
  let value =
    Attribute.create_owned ~name:"value" ~owner:Attribute.Point
      (Attribute.Float [| 0.; 2.; 6.; 4. |])
    |> Result.get_ok
  in
  let input = Geometry.with_attribute value input |> Result.get_ok in
  let run domains =
    Parallel.run ~domains (fun () ->
        triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy ~refine:true ~maximum_area:0.3
          ~maximum_new_points:100 ~refinement_point_group:"refined" input
        |> get)
  in
  let output = run 1 and parallel = run 4 in
  check
    (Geometry.point_count output > 4 && Geometry.point_count output <= 104)
    "quality refinement point budget";
  let output_positions = Packed.Float3.View.view (Geometry.positions output)
  and parallel_positions = Packed.Float3.View.view (Geometry.positions parallel) in
  check
    (topology_signature output = topology_signature parallel
    && output_positions.x = parallel_positions.x
    && output_positions.y = parallel_positions.y
    && output_positions.z = parallel_positions.z)
    "quality refinement differs across domains";
  (match Geometry.find_group ~owner:Group.Point "refined" output with
  | Some group ->
      check
        (Group.cardinality group = Geometry.point_count output - 4)
        "quality refinement output group"
  | None -> fail "quality refinement point group is missing");
  let positions = output_positions in
  (match Geometry.find_attribute ~owner:Attribute.Point "value" output with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Float values ->
          for point = 0 to Geometry.point_count output - 1 do
            check
              (abs_float (positions.z.(point) -. values.(point)) < 1e-10)
              "quality refinement payload interpolation"
          done
      | _ -> fail "quality refinement payload storage changed")
  | None -> fail "quality refinement payload is missing");
  let topology = Topology.View.view (Geometry.topology output) in
  for primitive = 0 to Geometry.primitive_count output - 1 do
    let first = topology.primitive_offsets.(primitive) in
    let a = topology.vertex_points.(first)
    and b = topology.vertex_points.(first + 1)
    and c = topology.vertex_points.(first + 2) in
    let twice_area =
      abs_float
        (((positions.x.(b) -. positions.x.(a)) *. (positions.y.(c) -. positions.y.(a)))
        -. ((positions.y.(b) -. positions.y.(a)) *. (positions.x.(c) -. positions.x.(a))))
    in
    check (0.5 *. twice_area <= 0.3 +. 1e-12) "quality refinement maximum area"
  done;
  let constrained_positions =
    Packed.Float3.View.of_owned_exn ~x:[| 0.; 4.; 2. |] ~y:[| 0.; 0.; 0.2 |] ~z:[| 0.; 8.; 9. |]
  in
  let constrained_topology =
    Topology.create_owned ~point_count:3 ~vertex_points:[| 0; 1 |] ~primitive_offsets:[| 0; 2 |]
      ~primitive_kinds:[| Topology.Open_polyline |]
    |> Result.get_ok
  in
  let constrained =
    Geometry.create ~positions:constrained_positions ~topology:constrained_topology ()
    |> Result.get_ok
  in
  let constraint_primitives = Group.init ~owner:Group.Primitive ~name:"base" 1 (fun _ -> true) in
  let split =
    triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy ~constraint_primitives ~refine:true
      ~minimum_angle:(Float.pi /. 6.) ~maximum_new_points:1 ~allow_constraint_splitting:true
      ~refinement_point_group:"refined" ~constraint_group:"constraints" constrained
    |> get
  in
  check (Geometry.point_count split = 4) "constraint refinement point cardinality";
  let split_positions = Packed.Float3.View.view (Geometry.positions split) in
  check
    (split_positions.x.(3) = 2. && split_positions.y.(3) = 0. && split_positions.z.(3) = 4.)
    "constraint refinement midpoint interpolation";
  (match Geometry.find_edge_group "constraints" split with
  | Some group ->
      check
        (Edge_group.cardinality group = 2)
        "constraint refinement did not split the protected edge"
  | None -> fail "constraint refinement edge group is missing");
  let boundary_positions =
    Packed.Float3.View.of_owned_exn ~x:[| 0.; 2.; 2.; 0. |] ~y:[| 0.; 0.; 2.; 2. |]
      ~z:[| 0.; 0.; 0.; 0. |]
  in
  let boundary_topology =
    Topology.polygons_owned ~point_count:4 ~vertex_points:[| 0; 1; 2; 3 |]
      ~primitive_offsets:[| 0; 4 |]
    |> Result.get_ok
  in
  let boundary_geometry =
    Geometry.create ~positions:boundary_positions ~topology:boundary_topology () |> Result.get_ok
  in
  let boundary_group = Group.init ~owner:Group.Primitive ~name:"boundary" 1 (fun _ -> true) in
  let boundary_refined =
    triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy ~constraint_primitives:boundary_group
      ~ignore_non_constraint_points:true ~remove_outside_constraint_polygons:true ~refine:true
      ~minimum_angle:1e-6 ~maximum_area:0.3 ~maximum_new_points:64 ~allow_constraint_splitting:true
      ~constraint_group:"constraints" boundary_geometry
    |> get
  in
  check
    (Geometry.primitive_count boundary_refined > 2)
    "constraint-splitting refinement lost a bounded polygon interior";
  let crossing_topology =
    Topology.create_owned ~point_count:4 ~vertex_points:[| 0; 1; 2; 3; 0; 2; 1; 3 |]
      ~primitive_offsets:[| 0; 4; 6; 8 |]
      ~primitive_kinds:[| Topology.Polygon; Topology.Open_polyline; Topology.Open_polyline |]
    |> Result.get_ok
  in
  let crossing_positions =
    Packed.Float3.View.of_owned_exn ~x:[| 20.; 300.; 300.; 20. |] ~y:[| 20.; 20.; 200.; 200. |]
      ~z:[| 0.; 0.; 0.; 0. |]
  in
  let crossing_geometry =
    Geometry.create ~positions:crossing_positions ~topology:crossing_topology () |> Result.get_ok
  in
  let crossing_group =
    Group.init ~owner:Group.Primitive ~name:"all_constraints" 3 (fun _ -> true)
  in
  Array.iter
    (fun maximum_new_points ->
      let refined =
        triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy
          ~constraint_primitives:crossing_group ~split_crossing_constraints:true
          ~flood_from_hull_boundary:true ~remove_outside_constraint_polygons:true
          ~silhouette_constraints:true ~remove_outside_silhouette:true
          ~ignore_non_constraint_points:true ~refine:true ~minimum_angle:(Float.pi /. 18.)
          ~maximum_area:1_500. ~maximum_new_points ~allow_constraint_splitting:true
          crossing_geometry
        |> get
      in
      check
        (Geometry.primitive_count refined > 0)
        (Printf.sprintf "multi-constraint refinement lost its interior at point budget %d"
           maximum_new_points))
    [| 1; 2; 4; 8; 16; 32; 64; 96 |];
  let unsplit =
    triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy ~constraint_primitives ~refine:true
      ~minimum_angle:(Float.pi /. 6.) ~maximum_new_points:1 ~allow_constraint_splitting:false
      ~constraint_group:"constraints" constrained
    |> get
  in
  (match Geometry.find_edge_group "constraints" unsplit with
  | Some group ->
      check
        (Edge_group.cardinality group = 1)
        "disabled constraint splitting changed the protected edge"
  | None -> fail "unsplit refinement constraint group is missing");
  let targeted =
    triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy ~refine:true ~minimum_angle:1e-6
      ~target_edge_length:0.75 ~maximum_new_points:100 input
    |> get
  in
  let targeted_positions = Packed.Float3.View.view (Geometry.positions targeted) in
  let targeted_index =
    Topology_index.create (Geometry.topology targeted) |> Topology_index.View.view
  in
  for edge = 0 to Array.length targeted_index.edge_a - 1 do
    let a = targeted_index.edge_a.(edge) and b = targeted_index.edge_b.(edge) in
    let dx = targeted_positions.x.(b) -. targeted_positions.x.(a)
    and dy = targeted_positions.y.(b) -. targeted_positions.y.(a) in
    check (sqrt ((dx *. dx) +. (dy *. dy)) <= 0.75 +. 1e-12) "quality refinement target edge length"
  done;
  let stopped =
    triangulate2d ~projection:Triangulate2d.Plane_xy ~refine:true ~maximum_area:0.01
      ~maximum_new_points:0 input
    |> get
  in
  check (Geometry.point_count stopped = 4) "zero refinement point budget was not respected";
  let edge_limited =
    triangulate2d ~projection:Triangulate2d.Plane_xy ~refine:true ~maximum_area:0.01
      ~minimum_edge_length:10. ~maximum_new_points:100 input
    |> get
  in
  check (Geometry.point_count edge_limited = 4) "minimum refinement edge length was not respected";
  let irregular =
    Line_geometry.points [| (0., 0., 0.); (3., 0., 3.); (2., 2., 6.); (0., 1., 2.) |]
  in
  let run_regularized domains =
    Parallel.run ~domains (fun () ->
        triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy ~refine:true ~minimum_angle:1e-6
          ~maximum_area:0.2 ~maximum_new_points:64 ~regularization_steps:2 irregular
        |> get)
  in
  let unregularized =
    triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy ~refine:true ~minimum_angle:1e-6
      ~maximum_area:0.2 ~maximum_new_points:64 irregular
    |> get
  in
  let regularized = run_regularized 1 and regularized_parallel = run_regularized 4 in
  let regularized_positions = Packed.Float3.View.view (Geometry.positions regularized)
  and parallel_positions = Packed.Float3.View.view (Geometry.positions regularized_parallel)
  and unregularized_positions = Packed.Float3.View.view (Geometry.positions unregularized) in
  check
    (topology_signature regularized = topology_signature regularized_parallel
    && regularized_positions.x = parallel_positions.x
    && regularized_positions.y = parallel_positions.y
    && regularized_positions.z = parallel_positions.z)
    "quality regularization differs across domains";
  let moved = ref false in
  for point = 4 to Geometry.point_count regularized - 1 do
    if
      regularized_positions.x.(point) <> unregularized_positions.x.(point)
      || regularized_positions.y.(point) <> unregularized_positions.y.(point)
    then moved := true;
    check
      (abs_float
         (regularized_positions.z.(point)
         -. (regularized_positions.x.(point) +. (2. *. regularized_positions.y.(point))))
      < 1e-9)
      "regularization provenance lost linear 3D payload"
  done;
  check !moved "quality regularization did not move any generated interior point";
  let movable_positions =
    Packed.Float3.View.of_owned_exn ~x:[| 0.; 2.; 2.; 0.; 0.2 |] ~y:[| 0.; 0.; 2.; 2.; 0.3 |]
      ~z:[| 0.; 2.; 6.; 4.; 0.8 |]
  in
  let movable_topology =
    Topology.polygons_owned ~point_count:5 ~vertex_points:[| 0; 1; 2; 3 |]
      ~primitive_offsets:[| 0; 4 |]
    |> Result.get_ok
  in
  let movable =
    Geometry.create ~positions:movable_positions ~topology:movable_topology () |> Result.get_ok
  in
  let movable_boundary = Group.init ~owner:Group.Primitive ~name:"boundary" 1 (fun _ -> true) in
  let move_input domains =
    Parallel.run ~domains (fun () ->
        triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy
          ~constraint_primitives:movable_boundary ~remove_outside_constraint_polygons:true
          ~refine:true ~minimum_angle:1e-6 ~maximum_new_points:0 ~regularization_steps:2
          ~allow_movement_of_interior_input_points:true ~constraint_group:"boundary" movable
        |> get)
  in
  let moved_one = move_input 1 and moved_four = move_input 4 in
  check
    (topology_signature moved_one = topology_signature moved_four
    && Geometry.point_count moved_one = 5)
    "input-point regularization differs across domains";
  match Geometry.find_edge_group "boundary" moved_one with
  | Some group ->
      check
        (Edge_group.cardinality group = 4)
        "input-point regularization moved or lost a protected boundary"
  | None -> fail "input-point regularization boundary group is missing"

let test_projected_silhouette () =
  let positions =
    Packed.Float3.View.of_owned_exn
      ~x:[| -2.; 2.; 2.; -2.; -1.; 1.; 1.; -1.; 0. |]
      ~y:[| -2.; -2.; 2.; 2.; -1.; -1.; 1.; 1.; 0. |]
      ~z:[| 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0. |]
  in
  let topology =
    Topology.polygons_owned ~point_count:9 ~vertex_points:[| 4; 5; 6; 4; 6; 7 |]
      ~primitive_offsets:[| 0; 3; 6 |]
    |> Result.get_ok
  in
  let input = Geometry.create ~positions ~topology () |> Result.get_ok in
  let constrained =
    triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy ~silhouette_constraints:true
      ~constraint_group:"silhouette" input
    |> get
  in
  (match Geometry.find_edge_group "silhouette" constrained with
  | Some group ->
      check (Edge_group.cardinality group = 4) "silhouette retained an internal same-facing edge"
  | None -> fail "silhouette constraint group is missing");
  let run domains =
    Parallel.run ~domains (fun () ->
        triangulate2d ~grain:1 ~projection:Triangulate2d.Plane_xy ~remove_outside_silhouette:true
          ~constraint_group:"silhouette" input
        |> get)
  in
  let output = run 1 and parallel = run 4 in
  check
    (topology_signature output = topology_signature parallel)
    "silhouette one/four-domain topology differs";
  check (Geometry.primitive_count output = 4) "silhouette removal retained the wrong triangle count";
  let view = Topology.View.view (Geometry.topology output) in
  check
    (Array.for_all (fun point -> point >= 4) view.vertex_points)
    "silhouette removal retained an exterior point";
  let reversed_topology =
    Topology.polygons_owned ~point_count:9 ~vertex_points:[| 6; 5; 4; 7; 6; 4 |]
      ~primitive_offsets:[| 0; 3; 6 |]
    |> Result.get_ok
  in
  let reversed =
    Geometry.create ~positions ~topology:reversed_topology ()
    |> Result.get_ok
    |> triangulate2d ~projection:Triangulate2d.Plane_xy ~remove_outside_silhouette:true
    |> get
  in
  check
    (topology_signature reversed = topology_signature output)
    "silhouette removal depends on global face orientation";
  let partial =
    Group.init ~owner:Group.Point ~name:"partial" 9 (fun point ->
        point = 4 || point = 5 || point = 7)
  in
  (match
     triangulate2d ~projection:Triangulate2d.Plane_xy
       ~selection:(Element_selection.Selected_points partial) ~silhouette_constraints:true input
   with
  | Error _ -> ()
  | Ok _ -> fail "silhouette accepted a partially selected polygon");
  let flat_positions =
    Packed.Float3.View.of_owned_exn ~x:[| 0.; 1.; 2.; 0.; 2. |] ~y:[| 0.; 0.; 0.; 1.; 1. |]
      ~z:[| 0.; 0.; 0.; 0.; 0. |]
  in
  let flat_topology =
    Topology.polygons_owned ~point_count:5 ~vertex_points:[| 0; 1; 2 |]
      ~primitive_offsets:[| 0; 3 |]
    |> Result.get_ok
  in
  let flat =
    Geometry.create ~positions:flat_positions ~topology:flat_topology ()
    |> Result.get_ok
    |> triangulate2d ~projection:Triangulate2d.Plane_xy ~remove_outside_silhouette:true
    |> get
  in
  check (Geometry.primitive_count flat = 0) "zero-area projected silhouette invented an interior"

let test_domain_exactness () =
  let count = 2_000 in
  let input =
    Line_geometry.points
      (Array.init count (fun point ->
           let x = float_of_int (point * 7919 mod 2003) in
           let y = float_of_int (point * 3571 mod 2011) +. (float_of_int point *. 1e-8) in
           (x, y, (x *. 0.25) -. (y *. 0.125))))
  in
  let run domains =
    Parallel.run ~domains (fun () -> triangulate2d ~grain:31 input |> get |> topology_signature)
  in
  check (run 1 = run 4) "one/four-domain topology differs"

let test_errors () =
  let input = Line_geometry.points [| (0., 0., 0.); (1., 0., 0.); (0., 1., 0.) |] in
  let expect = function Error _ -> () | Ok _ -> fail "invalid input succeeded" in
  expect
    (triangulate2d
       ~projection:
         (Triangulate2d.Plane { origin = Rays_math.Vec3.zero; normal = Rays_math.Vec3.zero })
       input);
  expect (triangulate2d ~projection:(Triangulate2d.Point_attribute "missing") input);
  let nonfinite = Line_geometry.points [| (0., 0., 0.); (1., 0., 0.); (0., 1., 0.) |] in
  let bad_coordinates =
    Attribute.create_owned ~owner:Attribute.Point ~name:"bad_coordinates"
      (Attribute.Float2
         (Packed.Float2.of_owned ~x:[| 0.; nan; 0. |] ~y:[| 0.; 0.; 1. |] |> Result.get_ok))
    |> Result.get_ok
  in
  let nonfinite = Geometry.with_attribute bad_coordinates nonfinite |> Result.get_ok in
  expect (triangulate2d ~projection:(Triangulate2d.Point_attribute "bad_coordinates") nonfinite);
  expect (triangulate2d ~refine:true ~minimum_angle:(Float.pi /. 3.) input);
  expect (triangulate2d ~refine:true ~maximum_area:0. input);
  expect (triangulate2d ~refine:true ~target_edge_length:nan input);
  expect (triangulate2d ~refine:true ~minimum_edge_length:(-1.) input);
  expect (triangulate2d ~refine:true ~maximum_new_points:(-1) input);
  expect (triangulate2d ~refine:true ~regularization_steps:(-1) input);
  expect (triangulate2d ~refinement_point_group:" " input);
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  expect (triangulate2d ~cancel input);
  expect (triangulate2d ~cancel ~refine:true ~maximum_area:0.01 input)

let run () =
  test_xy_payload_and_group ();
  test_best_fit_and_explicit_plane ();
  test_projected_output_positions ();
  test_attribute_and_selection ();
  test_keep_primitives_payload_groups_and_edges ();
  test_constraints ();
  test_crossing_constraints_and_payload ();
  test_hull_boundary_flood ();
  test_ignore_non_constraint_points ();
  test_remove_duplicate_points ();
  test_quality_refinement ();
  test_projected_silhouette ();
  test_domain_exactness ();
  test_errors ()
