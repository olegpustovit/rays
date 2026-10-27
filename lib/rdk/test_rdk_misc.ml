open Rays
open Rdk
open Rdk_test_support

let get_ok = function Ok value -> value | Error _ -> fail "unexpected error"

let measure_merge ~box ~grid ~selected ~triangle_geometry =
  let box_area = Analysis.surface_area box |> get_ok in
  if abs_float (box_area -. 88.) > 1e-12 then fail "box surface area";
  let polygon_geometry points =
    let count = Array.length points in
    let x = Array.make count 0. and y = Array.make count 0.
    and z = Array.make count 0. in
    Array.iteri (fun index (px, py, pz) ->
      x.(index) <- px; y.(index) <- py; z.(index) <- pz) points;
    let topology = Tb.create ~point_count:count () in
    Tb.add_polygon topology (Array.init count Fun.id);
    Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
      ~topology:(Tb.freeze topology) () |> get_ok in
  let concave = polygon_geometry
      [|(0.,0.,0.); (3.,0.,0.); (3.,3.,0.); (2.,3.,0.);
        (2.,1.,0.); (1.,1.,0.); (1.,3.,0.); (0.,3.,0.)|] in
  let concave_area = Analysis.surface_area ~grain:2 concave |> get_ok
  and concave_perimeter = Analysis.perimeter ~grain:2 concave |> get_ok in
  if abs_float (concave_area -. 7.) > 1e-12
      || abs_float (concave_perimeter -. 16.) > 1e-12 then
    fail "concave polygon area/perimeter";
  let bow_tie = polygon_geometry
      [|(0.,0.,0.); (1.,1.,0.); (0.,1.,0.); (1.,0.,0.)|] in
  (match Analysis.surface_area bow_tie with
   | Error _ -> ()
   | Ok _ -> fail "measure accepted a self-intersecting polygon");
  let open_curve = Line_geometry.polyline
      [|(0.,0.,0.); (1.,0.,0.); (1.,1.,0.)|] |> get_ok
  and closed_curve = Line_geometry.polyline ~closed:true
      [|(0.,0.,0.); (1.,0.,0.); (1.,1.,0.)|] |> get_ok in
  let open_length = Analysis.perimeter open_curve |> get_ok
  and closed_length = Analysis.perimeter closed_curve |> get_ok in
  if abs_float (open_length -. 2.) > 1e-12
      || abs_float (closed_length -. (2. +. sqrt 2.)) > 1e-12 then
    fail "open/closed curve length";
  (match Analysis.surface_area open_curve with
   | Error _ -> ()
   | Ok _ -> fail "area measurement accepted an open curve");
  let box_volume = Analysis.signed_volume ~grain:3 box |> get_ok in
  let reversed_box = Reverse_faces.run box |> get_ok in
  let reversed_volume = Analysis.signed_volume ~grain:3 reversed_box |> get_ok in
  if abs_float (abs_float box_volume -. 48.) > 1e-12
      || abs_float (box_volume +. reversed_volume) > 1e-12 then
    fail "closed box signed volume/winding";
  let first_triangle = Group.init ~owner:Group.Primitive ~name:"first_triangle"
      2 (fun primitive -> primitive = 0) in
  let old_measure = Attribute.create_owned ~name:"measured"
      ~owner:Attribute.Primitive (Attribute.Float [|99.; 88.|]) |> get_ok in
  let restricted_source = Geometry.with_attribute old_measure triangle_geometry
      |> get_ok in
  let restricted_measure = Analysis.with_measure ~grain:1
      ~primitives:first_triangle ~name:"measured" ~total_name:"measured_total"
      Analysis.Area restricted_source |> get_ok in
  let restricted_values = Geometry.find_attribute ~owner:Attribute.Primitive
      "measured" restricted_measure |> Option.get |> Attribute.get
      (Attribute.key ~name:"measured" ~owner:Attribute.Primitive Attribute.float)
      |> Option.get
  and restricted_total = Geometry.find_attribute ~owner:Attribute.Detail
      "measured_total" restricted_measure |> Option.get |> Attribute.get
      (Attribute.key ~name:"measured_total" ~owner:Attribute.Detail Attribute.float)
      |> Option.get in
  if restricted_values <> [|0.5; 88.|] || restricted_total <> [|0.5|] then
    fail "restricted measure preservation/detail total";
  let throughout = Analysis.with_measure ~grain:1
      ~accumulation:Analysis.Throughout ~name:"surface_total"
      ~total_name:"surface_total_detail" Analysis.Area triangle_geometry
      |> get_ok in
  let throughout_values = Geometry.find_attribute ~owner:Attribute.Primitive
      "surface_total" throughout |> Option.get |> Attribute.get
      (Attribute.key ~name:"surface_total" ~owner:Attribute.Primitive Attribute.float)
      |> Option.get in
  if throughout_values <> [|1.; 1.|] then
    fail "throughout area accumulation";
  (match Analysis.with_measure ~primitives:selected Analysis.Area triangle_geometry with
   | Error error when Error.code error = "invalid_measure" -> ()
   | _ -> fail "measure accepted a point-owned primitive selection");
  let cancelled_measure = Cancel.create () in
  Cancel.cancel cancelled_measure;
  (match Analysis.with_measure ~cancel:cancelled_measure Analysis.Area grid with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "measure ignored cancellation");
  (match Analysis.with_measure ~grain:0 Analysis.Area grid with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | _ -> fail "measure accepted non-positive grain");
  let merged = Mesh_merge.run [grid; grid] |> get_ok in
  if Geometry.point_count merged <> 90 || Geometry.primitive_count merged <> 128
  then fail "merge cardinality";
  (let make xyz vertex_points primitive_offsets primitive_kinds =
     let positions = Geometry.positions (Line_geometry.points xyz) in
     let topology = Topology.create_owned ~point_count:(Array.length xyz)
       ~vertex_points ~primitive_offsets ~primitive_kinds |> get_ok in
     Geometry.create ~positions ~topology () |> get_ok in
   let triangle_xyz = [|(0.,0.,0.);(1.,0.,0.);(0.,1.,0.)|]
   and free_xyz = [|(3.,0.,0.);(4.,0.,0.)|]
   and line_xyz = [|(5.,0.,0.);(6.,0.,0.)|] in
   let triangle = make triangle_xyz [|0;1;2|] [|0;3|] [|Topology.Polygon|]
   and free = Line_geometry.points free_xyz
   and empty = Line_geometry.points [||]
   and line = make line_xyz [|0;1|] [|0;2|] [|Topology.Open_polyline|] in
   let free_line = make [|(3.,0.,0.);(4.,0.,0.);(5.,0.,0.);(6.,0.,0.)|]
     [|2;3|] [|0;2|] [|Topology.Open_polyline|] in
   let expected = make [|(0.,0.,0.);(1.,0.,0.);(0.,1.,0.);
     (3.,0.,0.);(4.,0.,0.);(5.,0.,0.);(6.,0.,0.)|]
     [|0;1;2;5;6|] [|0;3;5|] [|Topology.Polygon;Topology.Open_polyline|] in
   let free_triangle = make [|(3.,0.,0.);(4.,0.,0.);
     (0.,0.,0.);(1.,0.,0.);(0.,1.,0.)|]
     [|2;3;4|] [|0;3|] [|Topology.Polygon|] in
   List.iter (fun (inputs, expected) ->
     let before = List.map geometry_bytes inputs in
     let expected = geometry_bytes expected in
     List.iter (fun domains ->
       let output = Parallel.run ~domains (fun () ->
         Mesh_merge.run ~grain:1 inputs |> get_ok) in
       if geometry_bytes output <> expected || List.map geometry_bytes inputs <> before then
         fail "merge empty/free-point topology rebasing or input ownership";
       let cancelled = Cancel.create () in Cancel.cancel cancelled;
       (match Parallel.run ~domains (fun () -> Mesh_merge.run ~cancel:cancelled inputs) with
        | Error error when Error.code error="cancelled" -> ()
        | _ -> fail "merge ignored precancellation");
       if List.map geometry_bytes inputs <> before then fail "cancelled merge mutated inputs")
       [1;8])
     [[empty;triangle;free;empty;line],expected;
      [triangle;free_line],expected; [free;triangle],free_triangle;
      [empty;triangle],triangle; [triangle;empty],triangle];
   let left_count = 32_769 and right_count = 16_385 in
   let left = make triangle_xyz (Array.init left_count (fun i -> i mod 3))
     [|0;left_count|] [|Topology.Open_polyline|]
   and right = make line_xyz (Array.init right_count (fun i -> i mod 2))
     [|0;right_count|] [|Topology.Open_polyline|] in
   let inputs = [left;right] in
   let before = List.map geometry_bytes inputs in
   let expected = make [|(0.,0.,0.);(1.,0.,0.);(0.,1.,0.);
     (5.,0.,0.);(6.,0.,0.)|]
     (Array.init (left_count + right_count) (fun i ->
       if i < left_count then i mod 3 else 3 + (i - left_count) mod 2))
     [|0;left_count;left_count + right_count|]
     [|Topology.Open_polyline;Topology.Open_polyline|] |> geometry_bytes in
   List.iter (fun domains -> List.iter (fun grain ->
     let output = Parallel.run ~domains (fun () ->
       Mesh_merge.run ~grain inputs |> get_ok) in
     if geometry_bytes output <> expected || List.map geometry_bytes inputs <> before then
       fail "merge chunk boundaries or input ownership";
     let cancelled = Cancel.create () in Cancel.cancel cancelled;
     (match Parallel.run ~domains (fun () -> Mesh_merge.run ~cancel:cancelled ~grain inputs) with
      | Error error when Error.code error="cancelled" -> ()
      | _ -> fail "chunked merge ignored precancellation");
     if List.map geometry_bytes inputs <> before then fail "cancelled chunked merge mutated inputs")
     [257;16_384;65_536;max_int]) [1;8]);
  (let source = "__src" in
   let tagged = Mesh_merge.run ~source_attribute:source [grid; box; grid] |> get_ok in
   let ints geometry = match Geometry.find_attribute ~owner:Attribute.Primitive source geometry with
     | Some attribute -> (match Attribute.Private.storage attribute with
         | Attribute.Int values -> values | _ -> fail "source attribute is not int")
     | None -> fail "merge wrote no source attribute" in
   let expected = Array.concat [
     Array.make (Geometry.primitive_count grid) 0;
     Array.make (Geometry.primitive_count box) 1;
     Array.make (Geometry.primitive_count grid) 2] in
   if ints tagged <> expected then fail "merge source attribute values";
   (* a merge of tagged merges keeps the inner tags; a base offsets the others *)
   let nested = Mesh_merge.run ~source_attribute:source ~source_base:100 [tagged; box] |> get_ok in
   let outer = ints nested in
   if outer <> Array.append expected (Array.make (Geometry.primitive_count box) 101)
   then fail "nested merge source attribute";
   let plain = Mesh_merge.run [grid; box; grid] |> get_ok in
   if Geometry.find_attribute ~owner:Attribute.Primitive source plain <> None
   then fail "merge without source_attribute added one";
   let left_count = 8193 and right_count = 8191 in
   let make xyz count index_base payload_base =
     let topology = Topology.create_owned ~point_count:(Array.length xyz)
       ~vertex_points:(Array.init (3 * count) (fun i -> index_base + i mod 3))
       ~primitive_offsets:(Array.init (count + 1) (fun i -> 3 * i))
       ~primitive_kinds:(Array.make count Topology.Polygon) |> get_ok in
     let payload = Attribute.create_owned ~name:"payload" ~owner:Attribute.Primitive
       (Attribute.Int (Array.init count (fun i -> payload_base + i))) |> get_ok in
     Geometry.create ~positions:(Geometry.positions (Line_geometry.points xyz))
       ~topology ~attributes:[payload] () |> get_ok in
   let left_xyz = [|(0.,0.,0.);(1.,0.,0.);(0.,1.,0.)|]
   and right_xyz = [|(3.,0.,0.);(4.,0.,0.);(3.,1.,0.)|] in
   let left = make left_xyz left_count 0 100
   and right = make right_xyz right_count 0 1000
   and empty = make [||] 0 0 0 in
   let expected_topology = Topology.create_owned ~point_count:6
       ~vertex_points:(Array.init (3 * (left_count + right_count)) (fun i ->
         if i < 3 * left_count then i mod 3 else 3 + i mod 3))
       ~primitive_offsets:(Array.init (left_count + right_count + 1) (fun i -> 3 * i))
       ~primitive_kinds:(Array.make (left_count + right_count) Topology.Polygon) |> get_ok in
   let expected_payload = Attribute.create_owned ~name:"payload" ~owner:Attribute.Primitive
       (Attribute.Int (Array.init (left_count + right_count) (fun i ->
         if i < left_count then 100 + i else 1000 + i - left_count))) |> get_ok in
   let expected_plain = Geometry.create
       ~positions:(Geometry.positions (Line_geometry.points (Array.append left_xyz right_xyz)))
       ~topology:expected_topology ~attributes:[expected_payload] () |> get_ok in
   let expected first = Attribute.create_owned ~name:source ~owner:Attribute.Primitive
       (Attribute.Int (Array.init (left_count + right_count) (fun i ->
         if i < left_count then first else -4))) |> get_ok
       |> fun attribute -> Geometry.with_attribute attribute expected_plain |> get_ok in
   let inputs = [empty;left;empty;right;empty] in
   let existing = Attribute.create_owned ~name:source ~owner:Attribute.Primitive
       (Attribute.Int (Array.make left_count 42)) |> get_ok in
   let tagged_left = Geometry.with_attribute existing left |> get_ok in
   List.iter (fun (inputs, expected) ->
     let before = List.map geometry_bytes inputs in
     let expected = geometry_bytes expected in
     List.iter (fun domains ->
       let output = Parallel.run ~domains (fun () ->
         Mesh_merge.run ~source_attribute:source ~source_base:(-7) inputs |> get_ok) in
       if geometry_bytes output <> expected || List.map geometry_bytes inputs <> before then
         fail "merge source-tag empty indexing, attribute order or ownership";
       let cancelled = Cancel.create () in Cancel.cancel cancelled;
       (match Parallel.run ~domains (fun () ->
          Mesh_merge.run ~cancel:cancelled ~source_attribute:source inputs) with
        | Error error when Error.code error = "cancelled" -> ()
        | _ -> fail "tagged merge ignored cancellation");
       if List.map geometry_bytes inputs <> before then fail "cancelled tagged merge mutated inputs")
       [1;8]) [inputs,expected (-6); [empty;tagged_left;empty;right;empty],expected 42];
   (match Mesh_merge.merge ~grain:0 ~source_attribute:" " inputs with
    | Error "Attribute.create_owned: empty name" -> ()
    | _ -> fail "tagged merge changed invalid-name error precedence");
   if Geometry.find_attribute ~owner:Attribute.Primitive source
       (Mesh_merge.run ~source_attribute:source [] |> get_ok) <> None then
     fail "empty input-list merge added a source attribute";
   Parallel.run ~domains:1 (fun () ->
     let allocated run =
       Gc.full_major ();
       let before = Gc.allocated_bytes () in
       let result = run () |> get_ok in
       let bytes = Gc.allocated_bytes () -. before in
       if Geometry.primitive_count result <> left_count + right_count then
         fail "source-tag allocation fixture cardinality";
       bytes in
     let plain = allocated (fun () -> Mesh_merge.run inputs) in
     let tagged = allocated (fun () -> Mesh_merge.run ~source_attribute:source inputs) in
     let ceiling = float_of_int (8 * (left_count + right_count) + 4096) in
     if tagged -. plain > ceiling then
       fail (Printf.sprintf "merge source tags allocate twice: extra %.0f B, ceiling %.0f B"
         (tagged -. plain) ceiling)));
  let grouped_inputs = List.map (fun name ->
    let group = Group.init ~owner:Group.Primitive ~name
        (Geometry.primitive_count box) (fun primitive -> primitive = 0) in
    Geometry.with_group group box |> get_ok)
      ["floor_0"; "floor_1"; "floor_2"] in
  let grouped_merge = Mesh_merge.run grouped_inputs |> get_ok in
  List.iteri (fun index name ->
    match Geometry.find_group ~owner:Group.Primitive name grouped_merge with
    | Some group when Group.length group = 3 * Geometry.primitive_count box
        && Group.cardinality group = 1
        && Group.mem (index * Geometry.primitive_count box) group -> ()
    | _ -> fail ("merge did not pad group " ^ name))
    ["floor_0"; "floor_1"; "floor_2"];
  (match Mesh_merge.merge ~pad_groups:false grouped_inputs with
   | Error _ -> ()
   | Ok _ -> fail "merge ignored strict group schema mode");
  let grouped_merge_parallel = Parallel.run ~domains:4 (fun () ->
    Mesh_merge.run ~grain:1 grouped_inputs |> get_ok) in
  let topology = Topology.Private.view (Geometry.topology grouped_merge)
  and parallel_topology =
    Topology.Private.view (Geometry.topology grouped_merge_parallel) in
  let memberships geometry = List.map (fun group ->
    Array.init (Group.length group) (fun index -> Group.mem index group))
      (Geometry.groups geometry) in
  if not (equal_positions grouped_merge grouped_merge_parallel)
      || topology.vertex_points <> parallel_topology.vertex_points
      || topology.primitive_offsets <> parallel_topology.primitive_offsets
      || topology.primitive_kinds <> parallel_topology.primitive_kinds
      || memberships grouped_merge <> memberships grouped_merge_parallel then
    fail "group-padded merge differs across domain counts";
  let degenerate_positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.; 1.; 2.|] ~y:[|0.; 0.; 0.|] ~z:[|0.; 0.; 0.|] in
  let degenerate_topology = Tb.create ~point_count:3 () in
  Tb.add_triangle degenerate_topology 0 1 2;
  let degenerate = Geometry.create ~positions:degenerate_positions
      ~topology:(Tb.freeze degenerate_topology) () |> get_ok in
  let cleaned = Clean.run degenerate |> get_ok in
  if Geometry.primitive_count cleaned <> 0 || Geometry.point_count cleaned <> 3
  then fail "clean degenerate primitive/point identity";
  let cleaned_compact = Clean.run ~remove_unused_points:true degenerate |> get_ok in
  if Geometry.primitive_count cleaned_compact <> 0
     || Geometry.point_count cleaned_compact <> 0 then
    fail "clean remove-unused-points policy";
  let compact_positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.; 1.; 2.; 3.; 4.|] ~y:(Array.make 5 0.) ~z:(Array.make 5 0.) in
  let compact_topology = Tb.create ~point_count:5 () in
  Tb.add_triangle compact_topology 0 2 4;
  let compact_ids = Attribute.create_owned ~name:"id" ~owner:Attribute.Point
      (Attribute.Int [|10; 11; 12; 13; 14|]) |> get_ok
  and compact_group = Group.init ~owner:Group.Point ~name:"marked" 5
      (fun point -> point = 2 || point = 3) in
  let compact_source = Geometry.create ~positions:compact_positions
      ~topology:(Tb.freeze compact_topology)
      ~attributes:[compact_ids] ~groups:[compact_group] () |> get_ok
      |> Group_mesh.group_edges ~name:"compact_edges" |> get_ok in
  let compacted domains = Parallel.run ~domains (fun () ->
      Compact_points.run ~grain:1 compact_source |> get_ok) in
  let compact_one = compacted 1 and compact_many = compacted 4 in
  (match Geometry.find_group ~owner:Group.Point "marked" compact_one with
   | Some group when Group.cardinality group = 1 && Group.mem 1 group -> ()
   | _ -> fail "compact points group remap");
  (match Geometry.find_edge_group "compact_edges" compact_one,
      Geometry.find_edge_group "compact_edges" compact_many with
   | Some one, Some many when Edge_group.cardinality one = 3
       && Edge_group.length one = 3 && Edge_group.length many = 3 ->
       for edge = 0 to 2 do
         if Edge_group.mem edge one <> Edge_group.mem edge many then
           fail "compact points native edge group differs by domain count"
       done
   | _ -> fail "compact points native edge-group remap");
  let match_source = Box_generator.box ~size:(Rays_math.Vec3.create 1. 2. 4.) () |> get_ok
  and match_target = Box_generator.box ~size:(Rays_math.Vec3.create 4. 6. 8.) () |> get_ok
      |> Transform_ops.transform (Rays_math.Mat4.translation (Rays_math.Vec3.create (-3.) 5. 2.)) in
  let matched domains = Parallel.run ~domains (fun () ->
      Match_size.run ~grain:1 ~fit:Match_size.Stretch ~target:match_target match_source
      |> get_ok) in
  let matched_one = matched 1 and matched_many = matched 4 in
  check (geometry_bytes matched_one = geometry_bytes matched_many
      && equal_positions matched_one match_target)
    "Match Size stretch matches target bounds/complete domain exactness";
  let axis_source = Line_geometry.points [|(1., 0., 0.); (2., 0., 0.)|] in
  let aligned = Match_size.match_axis ~grain:1 ~from:Rays_math.Vec3.unit_x ~into:Rays_math.Vec3.unit_y
      axis_source |> get_ok in
  let aligned_positions = Packed.Float3.Private.view (Geometry.positions aligned) in
  if abs_float aligned_positions.x.(0) > 1e-12
     || abs_float (aligned_positions.y.(0) -. 1.) > 1e-12 then
    fail "match axis quarter turn";
  let opposed = Match_size.match_axis ~from:Rays_math.Vec3.unit_x ~into:(Rays_math.Vec3.neg Rays_math.Vec3.unit_x)
      axis_source |> get_ok in
  let opposed_positions = Packed.Float3.Private.view (Geometry.positions opposed) in
  if abs_float (opposed_positions.x.(1) +. 2.) > 1e-12 then
    fail "match axis deterministic half turn";
  (match Match_size.match_axis ~from:Rays_math.Vec3.zero ~into:Rays_math.Vec3.unit_y axis_source with
   | Error error when Error.code error = "invalid_axis" -> ()
   | _ -> fail "match axis accepted a zero vector");
  (compact_source, match_source, match_target)

let blur ~grid =
  let float_values name geometry =
    match Geometry.find_attribute ~owner:Attribute.Point name geometry with
    | Some attribute ->
        (match Attribute.Private.storage attribute with
         | Attribute.Float values -> values
         | _ -> fail ("unexpected storage for " ^ name))
    | None -> fail ("missing point attribute " ^ name) in
  let blur_curve values =
    let geometry = Line_geometry.polyline [|(0.,0.,0.); (1.,0.,0.); (3.,0.,0.)|]
        |> get_ok in
    let attribute = Attribute.create_owned ~name:"value" ~owner:Attribute.Point
        (Attribute.Float values) |> get_ok in
    Geometry.with_attribute attribute geometry |> get_ok in
  let uniform_blur = Attribute_ops.blur_points ~iterations:1
      ~mode:(Attribute_ops.Laplacian 1.) ~pattern:"value"
      (blur_curve [|0.; 10.; 0.|]) |> get_ok in
  if float_values "value" uniform_blur <> [|10.; 0.; 10.|] then
    fail "uniform Attribute Blur";
  let edge_blur = Attribute_ops.blur_points ~iterations:1
      ~method_:Attribute_ops.Edge_length ~mode:(Attribute_ops.Laplacian 1.)
      ~pattern:"value" (blur_curve [|0.; 0.; 12.|]) |> get_ok in
  if abs_float ((float_values "value" edge_blur).(1) -. 4.) > 1e-12 then
    fail "edge-length Attribute Blur";
  let controls = blur_curve [|0.; 10.; 0.|] in
  let weight = Attribute.create_owned ~name:"weight" ~owner:Attribute.Point
      (Attribute.Float [|1.; 0.5; 1.|]) |> get_ok
  and alpha = Attribute.create_owned ~name:"alpha" ~owner:Attribute.Point
      (Attribute.Float [|1.; 1.; 0.|]) |> get_ok in
  let controls = controls |> Geometry.with_attribute weight |> get_ok
      |> Geometry.with_attribute alpha |> get_ok in
  let center = Group.init ~owner:Group.Point ~name:"center" 3
      (fun point -> point = 1) in
  let controlled = Attribute_ops.blur_points ~selection:center
      ~weight_attribute:"weight" ~alpha_attribute:"alpha"
      ~mode:(Attribute_ops.Laplacian 1.) ~pattern:"value" controls |> get_ok in
  if float_values "value" controlled <> [|0.; 5.; 0.|] then
    fail "selected weighted/alpha Attribute Blur";
  let blended = Attribute_ops.blur_points ~original_blend:1. ~blurred_blend:1.
      ~mode:(Attribute_ops.Laplacian 1.) ~pattern:"value"
      (blur_curve [|0.; 10.; 0.|]) |> get_ok in
  if float_values "value" blended <> [|10.; 10.; 10.|] then
    fail "Attribute Blur output blending";
  let custom = Attribute_ops.blur_points ~iterations:2
      ~mode:(Attribute_ops.Custom_steps { odd = 1.; even = 0. })
      ~pattern:"value" (blur_curve [|0.; 10.; 0.|]) |> get_ok in
  if float_values "value" custom <> [|10.; 0.; 10.|] then
    fail "Attribute Blur custom odd/even steps";
  let tuple_source = blur_curve [|0.; 10.; 0.|] in
  let uv = Attribute.create_owned ~name:"f_uv" ~owner:Attribute.Point
      (Attribute.Float2 (Packed.Float2.of_owned
        ~x:[|0.; 10.; 0.|] ~y:[|2.; 4.; 2.|] |> get_ok)) |> get_ok
  and color = Attribute.create_owned ~name:"f_color" ~owner:Attribute.Point
      (Attribute.Float4 (Packed.Float4.of_owned
        ~x:[|0.; 1.; 0.|] ~y:[|0.; 2.; 0.|]
        ~z:[|0.; 3.; 0.|] ~w:[|1.; 1.; 1.|] |> get_ok)) |> get_ok in
  let tuple_source = tuple_source |> Geometry.with_attribute uv |> get_ok
      |> Geometry.with_attribute color |> get_ok in
  let tuple_blur = Attribute_ops.blur_points
      ~mode:(Attribute_ops.Laplacian 1.) ~pattern:"f_*" tuple_source |> get_ok in
  (match Geometry.find_attribute ~owner:Attribute.Point "f_uv" tuple_blur,
      Geometry.find_attribute ~owner:Attribute.Point "f_color" tuple_blur with
   | Some uv, Some color ->
       let uv = match Attribute.Private.storage uv with
         | Attribute.Float2 values -> Packed.Float2.Private.view values
         | _ -> fail "Attribute Blur float2 storage"
       and color = match Attribute.Private.storage color with
         | Attribute.Float4 values -> Packed.Float4.Private.view values
         | _ -> fail "Attribute Blur float4 storage" in
       if uv.x <> [|10.; 0.; 10.|] || uv.y <> [|4.; 2.; 4.|]
           || color.z <> [|3.; 0.; 3.|] || color.w <> [|1.; 1.; 1.|]
       then fail "Attribute Blur tuple planes"
   | _ -> fail "Attribute Blur dropped tuple attributes");
  let pinned = Attribute_ops.blur_points ~pin_borders:true
      ~mode:(Attribute_ops.Laplacian 1.) ~pattern:"value"
      (blur_curve [|0.; 10.; 0.|]) |> get_ok in
  if float_values "value" pinned <> [|0.; 10.; 0.|] then
    fail "Attribute Blur border pins";
  let smoothed_grid = Attribute_ops.blur_points ~iterations:1
      ~mode:(Attribute_ops.Laplacian 0.25) ~pattern:"P" grid |> get_ok in
  if Geometry.find_attribute ~owner:Attribute.Point "N" smoothed_grid <> None
      || Geometry.find_attribute ~owner:Attribute.Vertex "N" smoothed_grid <> None
  then fail "position Attribute Blur retained stale normals";
  (match Attribute_ops.blur_points ~iterations:(-1) ~pattern:"P" grid with
   | Error error when Error.code error = "invalid_blur" -> ()
   | _ -> fail "Attribute Blur accepted negative iterations");
  (match Attribute_ops.blur_points ~weight_attribute:"missing" ~pattern:"P" grid with
   | Error error when Error.code error = "invalid_blur" -> ()
   | _ -> fail "Attribute Blur accepted a missing weight attribute");
  (match Attribute_ops.blur_points ~pattern:"value"
      (blur_curve [|Float.nan; 1.; 2.|]) with
   | Error error when Error.code error = "invalid_blur" -> ()
   | _ -> fail "Attribute Blur accepted non-finite source data");
  let extreme_curve = Line_geometry.polyline
      [|(max_float,0.,0.); (-.max_float,0.,0.)|] |> get_ok in
  let extreme_value = Attribute.create_owned ~name:"value"
      ~owner:Attribute.Point (Attribute.Float [|0.; 1.|]) |> get_ok in
  let extreme_curve = Geometry.with_attribute extreme_value extreme_curve |> get_ok in
  (match Attribute_ops.blur_points ~method_:Attribute_ops.Edge_length
      ~pattern:"value" extreme_curve with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | _ -> fail "Attribute Blur accepted a non-finite edge metric");
  ()
