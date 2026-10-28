open Rays
open Rdk
open Rdk_test_support

let get_ok = get

let equal_storage left right =
  match Attribute.View.storage left, Attribute.View.storage right with
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
  | _ -> false

let geometry_equal left right =
  let lp = Packed.Float3.View.view (Geometry.positions left)
  and rp = Packed.Float3.View.view (Geometry.positions right)
  and lt = Topology.View.view (Geometry.topology left)
  and rt = Topology.View.view (Geometry.topology right) in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.point_count = rt.point_count
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && List.equal (fun left right ->
      Attribute.owner left = Attribute.owner right
      && String.equal (Attribute.name left) (Attribute.name right)
      && equal_storage left right)
      (Geometry.attributes left) (Geometry.attributes right)
  && List.equal (fun left right ->
      Group.owner left = Group.owner right
      && String.equal (Group.name left) (Group.name right)
      && Group.ordered_elements left = Group.ordered_elements right
      && Group.length left = Group.length right
      && let equal = ref true in
         for element = 0 to Group.length left - 1 do
           if Group.mem element left <> Group.mem element right then equal := false
         done;
         !equal) (Geometry.groups left) (Geometry.groups right)
  && List.equal (fun left right ->
      String.equal (Edge_group.name left) (Edge_group.name right)
      && Edge_group.length left = Edge_group.length right
      && let equal = ref true in
         for edge = 0 to Edge_group.length left - 1 do
           if Edge_group.mem edge left <> Edge_group.mem edge right then equal := false
         done;
         !equal) (Geometry.edge_groups left) (Geometry.edge_groups right)

let make_geometry ~x ~y ~z ~vertex_points ~primitive_offsets ~primitive_kinds =
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  let topology = Topology.create_owned ~point_count:(Array.length x)
      ~vertex_points ~primitive_offsets ~primitive_kinds |> get_ok in
  Geometry.create ~positions ~topology () |> get_ok

let run () =
  let tolerance_source = make_geometry
      ~x:[|0.; 0.1; 0.; -.1e308; 1e308; 0.; 2.; 2.|]
      ~y:[|0.; 0.; 0.1; 0.; 0.; 1e308; 0.; 0.|]
      ~z:(Array.make 8 0.)
      ~vertex_points:[|0;1;2; 3;4;5; 6;7|]
      ~primitive_offsets:[|0;3;6;8|]
      ~primitive_kinds:[|Topology.Polygon; Topology.Polygon;
        Topology.Open_polyline|] in
  let coarse = Clean.run ~epsilon:0.1 tolerance_source |> get_rdk
  and fine = Clean.run ~epsilon:0.05 tolerance_source |> get_rdk in
  check (Geometry.primitive_count coarse = 1
      && Geometry.primitive_count fine = 2)
    (Printf.sprintf
      "Clean edge-length degeneracy tolerance or extreme-coordinate robustness (%d/%d)"
      (Geometry.primitive_count coarse) (Geometry.primitive_count fine));
  let clean_at domains = Parallel.run ~domains (fun () ->
      Clean.run ~grain:1 ~epsilon:0.05 tolerance_source |> get_rdk) in
  check (geometry_equal (clean_at 1) (clean_at 4))
    "Clean degenerate output differs across domain counts";

  let overlap_source = make_geometry
      ~x:[|0.; 1.; 1.; 0.|] ~y:[|0.; 0.; 1.; 1.|] ~z:(Array.make 4 0.)
      ~vertex_points:[|0;1;2; 1;2;0; 2;1;0; 0;2;3; 0;1;2|]
      ~primitive_offsets:[|0;3;6;9;12;15|]
      ~primitive_kinds:[|Topology.Polygon; Topology.Polygon; Topology.Polygon;
        Topology.Polygon; Topology.Open_polyline|] in
  let primitive_id = Attribute.create_owned ~name:"primitive_id"
      ~owner:Attribute.Primitive (Attribute.Int [|10;20;30;40;50|]) |> get_ok in
  let overlap_source = Geometry.with_attribute primitive_id overlap_source |> get_ok in
  let overlap_bytes = geometry_bytes overlap_source in
  let overlap_at domains policy = Parallel.run ~domains (fun () -> Clean.run ~grain:1
      ~remove_degenerate:false ~overlaps:policy overlap_source |> get_rdk) in
  let keep_first = overlap_at 1 Clean.Keep_first_overlap
  and delete_pairs = overlap_at 1 Clean.Delete_overlap_pairs in
  List.iter (fun (policy, one, expected_ids, expected_vertices) ->
    let many = overlap_at 4 policy in
    check (geometry_bytes one = geometry_bytes many)
      "Clean overlap policy differs across domains";
    let ids = match Geometry.find_attribute ~owner:Attribute.Primitive "primitive_id" one
        |> Option.get |> Attribute.View.storage with
      | Attribute.Int values -> values
      | _ -> fail "Clean changed primitive ID storage" in
    let topology = Topology.View.view (Geometry.topology one) in
    check (ids = expected_ids && topology.vertex_points = expected_vertices
        && topology.primitive_offsets = Array.init (Array.length ids + 1) (fun i -> i * 3))
      "Clean overlap policy lost primitive/corner ancestry") [
    Clean.Keep_first_overlap, keep_first, [|10;40;50|], [|0;1;2;0;2;3;0;1;2|];
    Clean.Delete_overlap_pairs, delete_pairs, [|40;50|], [|0;2;3;0;1;2|]
  ];
  check (geometry_bytes overlap_source = overlap_bytes) "Clean overlap repair mutated its source";

  let duplicate_points = make_geometry ~x:[|0.;0.;1.|] ~y:[|0.;0.;0.|]
      ~z:[|0.;0.;0.|] ~vertex_points:[||] ~primitive_offsets:[|0|]
      ~primitive_kinds:[||] in
  let consolidated = Clean.run ~remove_degenerate:false
      ~consolidate_distance:0. duplicate_points |> get_rdk in
  check (Geometry.point_count consolidated = 2)
    "Clean exact point consolidation";

  let metadata_source = make_geometry ~x:[|0.;1.;0.|] ~y:[|0.;0.;1.|]
      ~z:[|0.;0.;0.|] ~vertex_points:[|0;1;2|]
      ~primitive_offsets:[|0;3|] ~primitive_kinds:[|Topology.Polygon|] in
  let attributes = [
    Attribute.create_owned ~name:"temp_point" ~owner:Attribute.Point
      (Attribute.Int [|0;1;2|]) |> get_ok;
    Attribute.create_owned ~name:"keep_vertex" ~owner:Attribute.Vertex
      (Attribute.Int [|0;1;2|]) |> get_ok;
    Attribute.create_owned ~name:"temp_vertex" ~owner:Attribute.Vertex
      (Attribute.Float [|1.;2.;3.|]) |> get_ok;
    Attribute.create_owned ~name:"temp_primitive" ~owner:Attribute.Primitive
      (Attribute.Int [|4|]) |> get_ok;
    Attribute.create_owned ~name:"temp_detail" ~owner:Attribute.Detail
      (Attribute.Text [|"drop"|]) |> get_ok;
  ] in
  let groups = [
    Group.init ~owner:Group.Point ~name:"drop_points" 3 (fun p -> p = 0);
    Group.init ~owner:Group.Vertex ~name:"empty_vertices" 3 (fun _ -> false);
    Group.init ~owner:Group.Primitive ~name:"keep_faces" 1 (fun _ -> true);
  ] in
  let metadata_source = Geometry.create ~positions:(Geometry.positions metadata_source)
      ~topology:(Geometry.topology metadata_source) ~attributes ~groups () |> get_ok in
  let cleaned_metadata = Clean.run ~remove_degenerate:false ~reverse_winding:true
      ~delete_unused_groups:true ~point_attributes:"temp*"
      ~vertex_attributes:"temp*" ~primitive_attributes:"temp*"
      ~detail_attributes:"temp*" ~point_groups:"drop*" metadata_source |> get_rdk in
  check (Geometry.attributes cleaned_metadata |> List.map Attribute.name
      = ["keep_vertex"]
      && Geometry.find_group ~owner:Group.Point "drop_points" cleaned_metadata = None
      && Geometry.find_group ~owner:Group.Vertex "empty_vertices" cleaned_metadata = None
      && Geometry.find_group ~owner:Group.Primitive "keep_faces" cleaned_metadata <> None)
    "Clean attribute/group/empty-group cleanup";

  let valid = Plane_generators.grid ~columns:2 ~rows:2 ~size:2. () |> get_rdk in
  check (Clean.run valid |> get_rdk == valid) "Clean no-op lost geometry identity";
  (match Clean.run ~consolidate_distance:(-1.) valid with
   | Error error -> check (Error.code error = "invalid_geometry")
       "Clean consolidate-distance diagnostic"
   | Ok _ -> fail "Clean accepted a negative consolidate distance");
  (match Clean.run ~point_attributes:"[" valid with
   | Error error -> check (Error.code error = "invalid_geometry")
       "Clean pattern diagnostic"
   | Ok _ -> fail "Clean accepted an invalid pattern");
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match Clean.run ~cancel:cancelled valid with
   | Error error -> check (Error.code error = "cancelled")
       "Clean cancellation diagnostic"
   | Ok _ -> fail "cancelled Clean published geometry");

  let base = Plane_generators.grid ~columns:80 ~rows:60 ~size:10. () |> get_rdk in
  let base_topology = Topology.View.view (Geometry.topology base) in
  let copies = 3 and base_primitives = Geometry.primitive_count base
  and base_vertices = Geometry.vertex_count base in
  let vertex_points = Array.make (base_vertices * copies) 0
  and offsets = Array.init ((base_primitives * copies) + 1)
      (fun primitive -> primitive * 3)
  and kinds = Array.make (base_primitives * copies) Topology.Polygon in
  for primitive = 0 to base_primitives - 1 do
    let source = base_topology.primitive_offsets.(primitive) in
    let a = base_topology.vertex_points.(source)
    and b = base_topology.vertex_points.(source + 1)
    and c = base_topology.vertex_points.(source + 2) in
    let output = primitive * 9 in
    vertex_points.(output) <- a; vertex_points.(output + 1) <- b;
    vertex_points.(output + 2) <- c;
    vertex_points.(output + 3) <- b; vertex_points.(output + 4) <- c;
    vertex_points.(output + 5) <- a;
    vertex_points.(output + 6) <- c; vertex_points.(output + 7) <- b;
    vertex_points.(output + 8) <- a
  done;
  let dense_topology = Topology.create_owned
      ~point_count:(Geometry.point_count base) ~vertex_points
      ~primitive_offsets:offsets ~primitive_kinds:kinds |> get_ok in
  let dense_id = Attribute.create_owned ~name:"source_id"
      ~owner:Attribute.Primitive (Attribute.Int
        (Array.init (base_primitives * copies) (fun primitive -> primitive / copies)))
      |> get_ok in
  let dense = Geometry.create ~positions:(Geometry.positions base)
      ~topology:dense_topology ~attributes:[dense_id] () |> get_ok in
  let run domains = Parallel.run ~domains (fun () ->
      Clean.run ~grain:257 ~remove_degenerate:false
        ~overlaps:Clean.Keep_first_overlap dense |> get_rdk) in
  let one = run 1 and many = run 4 in
  check (geometry_equal one many)
    "Clean overlap output differs across domain counts";
  check (Geometry.primitive_count one = base_primitives
      && Geometry.vertex_count one = base_vertices)
    "Clean overlap scale cardinality";
  print_endline "clean tests passed"
