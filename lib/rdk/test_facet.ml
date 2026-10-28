open Rays
open Rdk
open Rdk_test_support

let add_attribute owner name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> get_string in
  Geometry.with_attribute attribute geometry |> get_string

let primitive_group ?(name = "selected") geometry predicate =
  Group.init ~owner:Group.Primitive ~name (Geometry.primitive_count geometry)
    predicate

let source () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 1.; 0.; 0.; 4.|]
      ~y:[|0.; 0.; 1.; 1.; 5.|]
      ~z:[|0.; 0.; 0.; 1.; 6.|] in
  let topology = Topology.polygons_owned ~point_count:5
      ~vertex_points:[|0; 1; 2; 0; 2; 3|]
      ~primitive_offsets:[|0; 3; 6|] |> get_string in
  let geometry = Geometry.create ~positions ~topology () |> get_string in
  let indices = Array.init 5 Fun.id in
  let csr_offsets = [|0; 2; 4; 6; 8; 10|] in
  let geometry = geometry
      |> add_attribute Attribute.Point "weight"
           (Attribute.Float (Array.map (fun value -> float_of_int value +. 0.25)
             indices))
      |> add_attribute Attribute.Point "id" (Attribute.Int indices)
      |> add_attribute Attribute.Point "label"
           (Attribute.Text (Array.map (Printf.sprintf "p%d") indices))
      |> add_attribute Attribute.Point "uv"
           (Attribute.Float2 (Packed.Float2.of_owned
             ~x:(Array.map float_of_int indices)
             ~y:(Array.map (fun value -> float_of_int (-value)) indices)
             |> get_string))
      |> add_attribute Attribute.Point "vector"
           (Attribute.Float3 (Packed.Float3.View.of_owned_exn
             ~x:(Array.map float_of_int indices)
             ~y:(Array.map (fun value -> float_of_int (value + 10)) indices)
             ~z:(Array.map (fun value -> float_of_int (value + 20)) indices)))
      |> add_attribute Attribute.Point "quaternion"
           (Attribute.Float4 (Packed.Float4.of_owned
             ~x:(Array.map float_of_int indices) ~y:(Array.make 5 0.)
             ~z:(Array.make 5 0.) ~w:(Array.make 5 1.) |> get_string))
      |> add_attribute Attribute.Point "neighbors"
           (Attribute.Int_array (Packed.Int_array.create_owned
             ~offsets:(Array.copy csr_offsets)
             ~values:(Array.init 10 (fun value -> value)) |> get_string))
      |> add_attribute Attribute.Point "samples"
           (Attribute.Float_array (Packed.Float_array.create_owned
             ~offsets:(Array.copy csr_offsets)
             ~values:(Array.init 10 (fun value -> float_of_int value *. 0.5))
             |> get_string)) in
  let point_group = Group.ordered ~owner:Group.Point ~name:"marked" ~length:5
      [|2; 0; 4|] |> get_string
  and vertex_group = Group.init ~owner:Group.Vertex ~name:"corners" 6
      (fun vertex -> vertex land 1 = 0)
  and primitive_group = Group.init ~owner:Group.Primitive ~name:"faces" 2
      (fun primitive -> primitive = 1) in
  let geometry = geometry |> Geometry.with_group point_group |> get_string
      |> Geometry.with_group vertex_group |> get_string
      |> Geometry.with_group primitive_group |> get_string in
  let index = Topology_index.create topology in
  let view = Topology_index.View.view index in
  let shared_edge = ref (-1) in
  for edge = 0 to Array.length view.edge_a - 1 do
    if view.edge_a.(edge) = 0 && view.edge_b.(edge) = 2 then shared_edge := edge
  done;
  let edges = Edge_group.init ~topology ~index ~name:"crease"
      (fun edge -> edge = !shared_edge) in
  Geometry.with_edge_group edges geometry |> get_string

let point_source_map = [|0; 1; 2; 0; 2; 3; 4|]

let point_storage_matches source output =
  let all predicate =
    let valid = ref true in
    for output_point = 0 to Array.length point_source_map - 1 do
      if not (predicate output_point point_source_map.(output_point)) then
        valid := false
    done;
    !valid in
  match Attribute.storage source, Attribute.storage output with
  | Attribute.Float source, Attribute.Float output ->
      all (fun target origin -> output.(target) = source.(origin))
  | Attribute.Int source, Attribute.Int output ->
      all (fun target origin -> output.(target) = source.(origin))
  | Attribute.Text source, Attribute.Text output ->
      all (fun target origin -> String.equal output.(target) source.(origin))
  | Attribute.Float2 source, Attribute.Float2 output ->
      all (fun target origin -> Packed.Float2.get output target
        = Packed.Float2.get source origin)
  | Attribute.Float3 source, Attribute.Float3 output ->
      all (fun target origin -> Packed.Float3.get output target
        = Packed.Float3.get source origin)
  | Attribute.Float4 source, Attribute.Float4 output ->
      all (fun target origin -> Packed.Float4.get output target
        = Packed.Float4.get source origin)
  | Attribute.Int_array source, Attribute.Int_array output ->
      all (fun target origin -> Packed.Int_array.get output target
        = Packed.Int_array.get source origin)
  | Attribute.Float_array source, Attribute.Float_array output ->
      all (fun target origin -> Packed.Float_array.get output target
        = Packed.Float_array.get source origin)
  | _ -> false

let check_unique_points () =
  let source = source () in
  let output = Facet.run ~unique_points:true source |> get_ok in
  check (Geometry.point_count output = 7 && Geometry.vertex_count output = 6
      && Geometry.primitive_count output = 2)
    "Facet Unique Points cardinality";
  let topology = Topology.View.view (Geometry.topology output) in
  check (topology.vertex_points = [|0; 1; 2; 3; 4; 5|])
    "Facet Unique Points topology";
  let positions = Packed.Float3.View.view (Geometry.positions output)
  and source_positions = Packed.Float3.View.view (Geometry.positions source) in
  Array.iteri (fun output_point source_point ->
    check (positions.x.(output_point) = source_positions.x.(source_point)
        && positions.y.(output_point) = source_positions.y.(source_point)
        && positions.z.(output_point) = source_positions.z.(source_point))
      "Facet Unique Points position ancestry") point_source_map;
  List.iter (fun name ->
    match Geometry.find_attribute ~owner:Attribute.Point name source,
        Geometry.find_attribute ~owner:Attribute.Point name output with
    | Some source_attribute, Some output_attribute ->
        check (point_storage_matches source_attribute output_attribute)
          ("Facet point attribute ancestry: " ^ name)
    | _ -> fail ("Facet lost point attribute " ^ name))
    ["weight"; "id"; "label"; "uv"; "vector"; "quaternion";
     "neighbors"; "samples"];
  let marked = Geometry.find_group ~owner:Group.Point "marked" output
      |> Option.get in
  check (Group.cardinality marked = 5
      && Group.ordered_elements marked = Some [|2; 4; 0; 3; 6|])
    "Facet ordered point-group ancestry";
  let crease = Geometry.find_edge_group "crease" output |> Option.get in
  check (Edge_group.cardinality crease = 2)
    "Facet one-to-many native edge-group ancestry";
  let consolidated = Facet.run ~unique_points:true ~consolidate_distance:0.
      source |> get_ok in
  check (Geometry.point_count consolidated = 5)
    "Facet Unique Points then Consolidate pipeline order"

let check_primitive_group_unique_points () =
  let source = source () in
  let selected = primitive_group source (fun primitive -> primitive = 0) in
  let output = Facet.run ~selection:(Transform_ops.Selected_primitives selected) ~unique_points:true source
      |> get_ok in
  let topology = Topology.View.view (Geometry.topology output) in
  check (Geometry.point_count output = 7
      && topology.vertex_points = [|4;5;6; 0;1;2|])
    "grouped Facet Unique Points topology";
  (match Geometry.find_attribute ~owner:Attribute.Point "id" output with
   | Some attribute ->
       check (Attribute.storage attribute
          = Attribute.Int [|0;2;3;4; 0;1;2|])
         "grouped Facet Unique Points point ancestry"
   | None -> fail "grouped Facet Unique Points lost point id");
  let source_topology = Topology.View.view (Geometry.topology source) in
  for local = 0 to 2 do
    let output_point = topology.vertex_points.(3 + local)
    and source_point = source_topology.vertex_points.(3 + local) in
    let output_id = match Geometry.find_attribute ~owner:Attribute.Point "id"
        output with
      | Some attribute ->
          (match Attribute.storage attribute with
           | Attribute.Int values -> values.(output_point)
           | _ -> assert false)
      | None -> assert false in
    check (output_id = source_point)
      "grouped Facet changed an unselected primitive point"
  done;
  let empty = primitive_group source (fun _ -> false) in
  check (Facet.run ~selection:(Transform_ops.Selected_primitives empty) ~unique_points:true source |> get_ok
      == source)
    "empty Facet primitive group is not identity";
  let all = primitive_group source (fun _ -> true) in
  check (equal_geometry
      (Facet.run ~selection:(Transform_ops.Selected_primitives all) ~unique_points:true source |> get_ok)
      (Facet.run ~unique_points:true source |> get_ok))
    "all-selected Facet Unique Points differs from compatibility path"

let check_typed_selections () =
  let source = source () in
  let first = primitive_group source (fun primitive -> primitive = 0) in
  let expected = Facet.run ~selection:(Transform_ops.Selected_primitives first) ~unique_points:true source
      |> get_ok in
  let points = Group.init ~owner:Group.Point ~name:"facet_point" 5
      (fun point -> point = 1) in
  let point_output = Facet.run ~selection:(Transform_ops.Selected_points points)
      ~unique_points:true source |> get_ok in
  check (equal_geometry point_output expected)
    "Facet point selection did not promote to incident primitives";
  let vertices = Group.init ~owner:Group.Vertex ~name:"facet_vertex" 6
      (fun vertex -> vertex = 1) in
  let vertex_output = Facet.run ~selection:(Transform_ops.Selected_vertices vertices)
      ~unique_points:true source |> get_ok in
  check (equal_geometry vertex_output expected)
    "Facet vertex selection did not promote to its owning primitive";
  let topology = Geometry.topology source in
  let index = Topology_index.create topology in
  let view = Topology_index.View.view index in
  let boundary_edge = ref (-1) in
  for edge = 0 to Array.length view.edge_a - 1 do
    if view.edge_a.(edge) = 0 && view.edge_b.(edge) = 1 then
      boundary_edge := edge
  done;
  let edges = Edge_group.init ~topology ~index ~name:"facet_edge"
      (fun edge -> edge = !boundary_edge) in
  let edge_output = Facet.run ~selection:(Transform_ops.Selected_edges edges)
      ~unique_points:true source |> get_ok in
  check (equal_geometry edge_output expected)
    "Facet edge selection did not promote to its incident primitive";
  let shared = Geometry.find_edge_group "crease" source |> Option.get in
  let all = Facet.run ~selection:(Transform_ops.Selected_edges shared)
      ~unique_points:true source |> get_ok in
  check (equal_geometry all
      (Facet.run ~unique_points:true source |> get_ok))
    "Facet shared-edge selection did not promote both incident primitives";
  let empty = Group.init ~owner:Group.Point ~name:"empty" 5 (fun _ -> false) in
  check (Facet.run ~selection:(Transform_ops.Selected_points empty)
      ~unique_points:true source |> get_ok == source)
    "Facet empty typed selection is not identity";
  let wrong = Group.init ~owner:Group.Primitive ~name:"wrong" 2
      (fun _ -> true) in
  (match Facet.run ~selection:(Transform_ops.Selected_points wrong)
      ~unique_points:true source with
   | Error error when Error.code error = "invalid_geometry" -> ()
   | _ -> fail "Facet accepted a typed selection with the wrong owner");
  let foreign = Plane_generators.grid ~columns:1 ~rows:1 ~size:1. () |> get_ok in
  let foreign_topology = Geometry.topology foreign in
  let foreign_index = Topology_index.create foreign_topology in
  let foreign_edge = Edge_group.init ~topology:foreign_topology
      ~index:foreign_index ~name:"foreign" (fun edge -> edge = 0) in
  (match Facet.run ~selection:(Transform_ops.Selected_edges foreign_edge)
      ~unique_points:true source with
   | Error error when Error.code error = "invalid_geometry" -> ()
   | _ -> fail "Facet accepted a foreign-topology edge selection")

let normal_attribute geometry =
  match Geometry.find_attribute ~owner:Attribute.Point "N" geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.View.view values
       | _ -> fail "Facet N has wrong storage")
  | None -> fail "Facet lost N"

let check_normals () =
  let geometry = source () in
  let pre = Facet.run ~pre_compute_normals:true ~unique_points:true geometry
      |> get_ok
  and post = Facet.run ~unique_points:true ~post_compute_normals:true geometry
      |> get_ok in
  let pre_n = normal_attribute pre and post_n = normal_attribute post in
  check (near pre_n.x.(0) pre_n.x.(3) && near pre_n.y.(0) pre_n.y.(3)
      && near pre_n.z.(0) pre_n.z.(3))
    "Facet pre-computed smooth normals were not duplicated";
  check (not (near post_n.x.(0) post_n.x.(3)
      && near post_n.y.(0) post_n.y.(3)
      && near post_n.z.(0) post_n.z.(3)))
    "Facet post-computed normals did not become face-hard";
  let source = source () |> add_attribute Attribute.Point "N"
      (Attribute.Float3 (Packed.Float3.View.of_owned_exn
        ~x:(Array.make 5 2.) ~y:(Array.make 5 0.) ~z:(Array.make 5 0.))) in
  let adjusted = Facet.run ~make_normals_unit_length:true ~reverse_normals:true
      source |> get_ok in
  let normal = normal_attribute adjusted in
  check (normal.x = Array.make 5 (-1.) && normal.y = Array.make 5 0.
      && normal.z = Array.make 5 0.)
    "Facet normal normalization/reversal"

let check_consolidate_normals () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;0.01; 1.;2.; 0.;2.|] ~y:[|0.;0.;0.;0.;1.;1.|]
      ~z:(Array.make 6 0.) in
  let topology = Topology.polygons_owned ~point_count:6
      ~vertex_points:[|0;2;4; 0;4;5; 1;3;5|]
      ~primitive_offsets:[|0;3;6;9|] |> get_string in
  let point_normal = Attribute.create_owned ~owner:Attribute.Point ~name:"N"
      (Attribute.Float3 (Packed.Float3.View.of_owned_exn
        ~x:[|1.;0.;2.;4.;0.;0.|] ~y:[|0.;1.;0.;0.;0.;0.|]
        ~z:(Array.make 6 0.))) |> get_string
  and vertex_normal = Attribute.create_owned ~owner:Attribute.Vertex ~name:"N"
      (Attribute.Float3 (Packed.Float3.View.of_owned_exn
        ~x:[|1.;2.;3.; 0.;4.;5.; 0.;6.;7.|]
        ~y:[|0.;0.;0.; 1.;0.;0.; 0.;0.;0.|]
        ~z:[|0.;0.;0.; 0.;0.;0.; 1.;0.;0.|])) |> get_string
  and tag = Attribute.create_owned ~owner:Attribute.Detail ~name:"tag"
      (Attribute.Text [|"normals"|]) |> get_string in
  let selected = Group.ordered ~owner:Group.Point ~name:"selected" ~length:6
      [|1;0;4|] |> get_string in
  let source = Geometry.create ~positions ~topology
      ~attributes:[point_normal; vertex_normal; tag] ~groups:[selected] ()
      |> get_string in
  let output = Facet.run ~consolidate_normals_distance:0.01 source |> get_ok in
  check (Geometry.positions output == Geometry.positions source
      && Geometry.topology output == Geometry.topology source
      && List.equal equal_group (Geometry.groups output) (Geometry.groups source))
    "Facet Consolidate Normals changed non-normal geometry payload";
  let point = Geometry.find_attribute ~owner:Attribute.Point "N" output
      |> Option.get
  and vertex = Geometry.find_attribute ~owner:Attribute.Vertex "N" output
      |> Option.get in
  (match Attribute.View.storage point with
   | Attribute.Float3 values ->
       let values = Packed.Float3.View.view values in
       check (near values.x.(0) 0.5 && near values.y.(0) 0.5
           && near values.x.(1) 0.5 && near values.y.(1) 0.5)
         "Facet point-normal arithmetic average";
       check (Int64.bits_of_float values.x.(2) = Int64.bits_of_float 2.
           && Int64.bits_of_float values.x.(3) = Int64.bits_of_float 4.)
         "Facet point-normal singleton bit preservation"
   | _ -> fail "Facet consolidated point N has wrong storage");
  (match Attribute.View.storage vertex with
   | Attribute.Float3 values ->
       let values = Packed.Float3.View.view values in
       List.iter (fun element ->
         check (near values.x.(element) (1. /. 3.)
             && near values.y.(element) (1. /. 3.)
             && near values.z.(element) (1. /. 3.))
           "Facet vertex-normal point-cluster average") [0;3;6];
       check (values.x.(2) = 3. && values.x.(4) = 4.)
         "Facet vertex-normal singleton fan changed"
   | _ -> fail "Facet consolidated vertex N has wrong storage");
  let unit = Facet.run ~make_normals_unit_length:true
      ~consolidate_normals_distance:0.01 source |> get_ok in
  let unit = normal_attribute unit in
  check (near unit.x.(0) 0.5 && near unit.y.(0) 0.5)
    "Facet unit-normal then consolidation pipeline order";
  let no_normals = Line_geometry.points [|(0.,0.,0.); (0.,0.,0.)|] in
  check (Facet.run ~consolidate_normals_distance:0. no_normals |> get_ok
      == no_normals)
    "Facet Consolidate Normals no-normal identity";
  let extreme = Line_geometry.points [|(0.,0.,0.); (0.,0.,0.)|]
      |> Geometry.with_attribute
        (Attribute.create_owned ~owner:Attribute.Point ~name:"N"
          (Attribute.Float3 (Packed.Float3.View.of_owned_exn
            ~x:[|Float.max_float; Float.max_float|] ~y:[|0.;0.|]
            ~z:[|0.;0.|])) |> get_string)
      |> get_string
      |> Facet.run ~consolidate_normals_distance:0. |> get_ok
      |> normal_attribute in
  check (Float.is_finite extreme.x.(0)
      && extreme.x.(0) = Float.max_float)
    "Facet Consolidate Normals overflowed a finite average";
  List.iter (fun distance ->
    match Facet.run ~consolidate_normals_distance:distance source with
    | Error error when Error.code error = "invalid_geometry" -> ()
    | _ -> fail "Facet accepted an invalid normal consolidation distance")
    [(-0.1); Float.nan];
  (match Facet.run ~consolidate_distance:0.
      ~consolidate_normals_distance:0. source with
   | Error error when Error.code error = "invalid_geometry" -> ()
   | _ -> fail "Facet accepted mutually exclusive consolidation modes");
  let wrong = source |> Geometry.without_attribute ~owner:Attribute.Point "N"
      |> Geometry.with_attribute
        (Attribute.create_owned ~owner:Attribute.Point ~name:"N"
          (Attribute.Float (Array.make 6 1.)) |> get_string)
      |> get_string in
  (match Facet.run ~consolidate_normals_distance:0.01 wrong with
   | Error error when Error.code error = "invalid_geometry" -> ()
   | _ -> fail "Facet accepted scalar N for normal consolidation")

let inline_source () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;2.;3.;3.;0.; 9.; 1.;2.; 0.;1.;2.|]
      ~y:[|0.;0.;0.;0.;0.;0.; 9.; 0.;1.; 3.;3.;3.|]
      ~z:[|0.;0.;0.;0.;2.;2.; 9.; 1.;1.; 0.;0.;0.|] in
  let primitive_kinds = Bytes.make 3 '\000' in
  Bytes.set primitive_kinds 2 '\001';
  let topology = Topology.View.create_validated_owned ~point_count:12
      ~vertex_points:[|0;1;2;3;4;5; 1;7;8; 9;10;11|]
      ~primitive_offsets:[|0;6;9;12|] ~primitive_kinds in
  let point_id = Attribute.create_owned ~owner:Attribute.Point ~name:"point_id"
      (Attribute.Int (Array.init 12 Fun.id)) |> get_string
  and corner_id = Attribute.create_owned ~owner:Attribute.Vertex
      ~name:"corner_id" (Attribute.Int (Array.init 12 Fun.id)) |> get_string
  and face_id = Attribute.create_owned ~owner:Attribute.Primitive ~name:"face_id"
      (Attribute.Int [|10;11;12|]) |> get_string
  and detail = Attribute.create_owned ~owner:Attribute.Detail ~name:"tag"
      (Attribute.Text [|"inline"|]) |> get_string in
  let points = Group.ordered ~owner:Group.Point ~name:"point_order" ~length:12
      [|2;1;6|] |> get_string
  and corners = Group.ordered ~owner:Group.Vertex ~name:"corner_order"
      ~length:12 [|1;2;4;10|] |> get_string
  and faces = Group.ordered ~owner:Group.Primitive ~name:"face_order" ~length:3
      [|2;0|] |> get_string in
  let geometry = Geometry.create ~positions ~topology
      ~attributes:[point_id; corner_id; face_id; detail]
      ~groups:[points; corners; faces] () |> get_string in
  let index = Topology_index.create topology in
  let view = Topology_index.View.view index in
  let segment = Edge_group.init ~topology ~index ~name:"removed_segment"
      (fun edge -> view.edge_a.(edge) = 1 && view.edge_b.(edge) = 2) in
  Geometry.with_edge_group segment geometry |> get_string

let check_remove_inline_points () =
  let source = inline_source () in
  let output = Facet.run ~remove_inline_points:true source |> get_ok in
  check (Geometry.point_count output = 11
      && Geometry.vertex_count output = 10
      && Geometry.primitive_count output = 3)
    "Facet Remove Inline Points cardinality";
  let topology = Topology.View.view (Geometry.topology output) in
  check (topology.vertex_points = [|0;2;3;4; 1;6;7; 8;9;10|]
      && topology.primitive_offsets = [|0;4;7;10|]
      && Bytes.get topology.primitive_kinds 2 = '\001')
    "Facet Remove Inline Points topology or curve pass-through";
  (match Geometry.find_attribute ~owner:Attribute.Point "point_id" output with
   | Some attribute ->
       (match Attribute.View.storage attribute with
        | Attribute.Int values ->
            check (values = [|0;1;3;4;5;6;7;8;9;10;11|])
              "Facet Remove Inline Points point ancestry"
        | _ -> fail "Facet inline point_id has wrong storage")
   | None -> fail "Facet Remove Inline Points lost point_id");
  (match Geometry.find_attribute ~owner:Attribute.Vertex "corner_id" output with
   | Some attribute ->
       (match Attribute.View.storage attribute with
        | Attribute.Int values ->
            check (values = [|0;3;4;5;6;7;8;9;10;11|])
              "Facet Remove Inline Points vertex ancestry"
        | _ -> fail "Facet inline corner_id has wrong storage")
   | None -> fail "Facet Remove Inline Points lost corner_id");
  (match Geometry.find_attribute ~owner:Attribute.Primitive "face_id" output,
      Geometry.find_attribute ~owner:Attribute.Detail "tag" output with
   | Some face, Some detail ->
       check (Attribute.storage face = Attribute.Int [|10;11;12|]
          && Attribute.storage detail = Attribute.Text [|"inline"|])
         "Facet Remove Inline Points changed primitive/detail payload"
   | _ -> fail "Facet Remove Inline Points lost primitive/detail payload");
  let points = Geometry.find_group ~owner:Group.Point "point_order" output
      |> Option.get
  and corners = Geometry.find_group ~owner:Group.Vertex "corner_order" output
      |> Option.get
  and faces = Geometry.find_group ~owner:Group.Primitive "face_order" output
      |> Option.get in
  check (Group.ordered_elements points = Some [|1;5|])
    "Facet Remove Inline Points ordered point group";
  check (Group.ordered_elements corners = Some [|2;8|])
    "Facet Remove Inline Points ordered vertex group";
  check (Group.ordered_elements faces = Some [|2;0|])
    "Facet Remove Inline Points primitive group";
  let healed = Geometry.find_edge_group "removed_segment" output |> Option.get
  and healed_index = Topology_index.create (Geometry.topology output) in
  let healed_view = Topology_index.View.view healed_index in
  let expected_edge = ref (-1) in
  for edge = 0 to Array.length healed_view.edge_a - 1 do
    if healed_view.edge_a.(edge) = 0 && healed_view.edge_b.(edge) = 2 then
      expected_edge := edge
  done;
  check (!expected_edge >= 0 && Edge_group.cardinality healed = 1
      && Edge_group.mem !expected_edge healed)
    "Facet Remove Inline Points healed edge ancestry";
  let retained = Facet.run ~remove_inline_points:true ~inline_distance:0.
      (Plane_generators.grid ~columns:8 ~rows:6 ~size:2. () |> get_ok) |> get_ok in
  check (Geometry.vertex_count retained = 8 * 6 * 6)
    "Facet Remove Inline Points changed non-inline triangles";
  let translated middle_offset =
    let base = 1e12 in
    let positions = Packed.Float3.View.of_owned_exn
        ~x:[|base; base +. 1.; base +. 2.; base +. 2.; base|]
        ~y:(Array.make 5 0.)
        ~z:[|base; base +. middle_offset; base; base +. 2.; base +. 2.|] in
    let topology = Topology.polygons_owned ~point_count:5
        ~vertex_points:[|0;1;2;3;4|] ~primitive_offsets:[|0;5|]
        |> get_string in
    Geometry.create ~positions ~topology () |> get_string in
  let translated_corner = translated 0.01 in
  check (Facet.run ~remove_inline_points:true ~inline_distance:0.001
      translated_corner |> get_ok == translated_corner)
    "Facet inline tolerance changed under a large translation";
  check (Geometry.point_count
      (Facet.run ~remove_inline_points:true (translated 0.) |> get_ok) = 4)
    "Facet missed a translated exact-inline corner";
  List.iter (fun distance ->
    match Facet.run ~remove_inline_points:true ~inline_distance:distance source with
    | Error error when Error.code error = "invalid_geometry" -> ()
    | _ -> fail "Facet accepted an invalid inline distance")
    [(-0.1); Float.nan]

let check_primitive_group_inline_points () =
  let source = inline_source () in
  let first = primitive_group source (fun primitive -> primitive = 0) in
  let output = Facet.run ~selection:(Transform_ops.Selected_primitives first) ~remove_inline_points:true source
      |> get_ok in
  check (Geometry.vertex_count output = 10
      && Geometry.point_count output = 11)
    "grouped Facet Remove Inline Points cardinality";
  (match Geometry.find_attribute ~owner:Attribute.Vertex "corner_id" output with
   | Some attribute ->
       (match Attribute.storage attribute with
        | Attribute.Int values ->
            check (values = [|0;3;4;5; 6;7;8; 9;10;11|])
              "grouped Facet changed unselected vertex payload"
        | _ -> fail "grouped Facet inline corner id has wrong storage")
   | None -> fail "grouped Facet inline lost corner id");
  let curve_only = primitive_group source (fun primitive -> primitive = 2) in
  check (Facet.run ~selection:(Transform_ops.Selected_primitives curve_only) ~remove_inline_points:true source
      |> get_ok == source)
    "grouped Facet removed inline points outside its selection"

let check_orient_polygons () =
  let base = source () in
  let old = Topology.View.view (Geometry.topology base) in
  let topology = Topology.polygons_owned ~point_count:old.point_count
      ~vertex_points:[|0; 1; 2; 0; 3; 2|]
      ~primitive_offsets:[|0; 3; 6|] |> get_string in
  let corner = Attribute.create_owned ~owner:Attribute.Vertex ~name:"corner_id"
      (Attribute.Int (Array.init 6 Fun.id)) |> get_string in
  let corners = Group.ordered ~owner:Group.Vertex ~name:"corner_order"
      ~length:6 [|3; 5|] |> get_string in
  let geometry = Geometry.create ~positions:(Geometry.positions base) ~topology
      ~attributes:[corner] ~groups:[corners] () |> get_string in
  let index = Topology_index.create topology in
  let view = Topology_index.View.view index in
  let crease = Edge_group.init ~topology ~index ~name:"shared"
      (fun edge -> view.edge_a.(edge) = 0 && view.edge_b.(edge) = 2) in
  let geometry = Geometry.with_edge_group crease geometry |> get_string in
  let oriented = Facet.run ~orient_polygons:true geometry |> get_ok in
  let topology = Topology.View.view (Geometry.topology oriented) in
  check (Array.sub topology.vertex_points 3 3 = [|2; 3; 0|])
    "Facet Orient Polygons did not reverse the inconsistent face";
  (match Geometry.find_attribute ~owner:Attribute.Vertex "corner_id" oriented with
   | Some attribute ->
       (match Attribute.View.storage attribute with
        | Attribute.Int values -> check (values = [|0;1;2;5;4;3|])
            "Facet Orient Polygons vertex-attribute ancestry"
        | _ -> fail "Facet oriented corner_id has wrong storage")
   | None -> fail "Facet Orient Polygons lost corner_id");
  let corners = Geometry.find_group ~owner:Group.Vertex "corner_order" oriented
      |> Option.get in
  check (Group.ordered_elements corners = Some [|5;3|])
    "Facet Orient Polygons ordered vertex-group ancestry";
  check (Geometry.find_edge_group "shared" oriented |> Option.get
      |> Edge_group.cardinality = 1)
    "Facet Orient Polygons native edge-group ancestry";
  let nonmanifold_topology = Topology.polygons_owned ~point_count:5
      ~vertex_points:[|0;1;2; 1;0;3; 0;1;4|]
      ~primitive_offsets:[|0;3;6;9|] |> get_string in
  let nonmanifold_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;0.;0.;0.|] ~y:[|0.;0.;1.;0.;(-1.)|]
      ~z:[|0.;0.;0.;1.;0.|] in
  let nonmanifold = Geometry.create ~positions:nonmanifold_positions
      ~topology:nonmanifold_topology () |> get_string in
  (match Facet.run ~orient_polygons:true nonmanifold with
   | Error error when Error.code error = "invalid_geometry" -> ()
   | _ -> fail "Facet Orient Polygons accepted a non-manifold edge")

let check_primitive_group_orient_polygons () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;0.;1.;2.|] ~y:[|0.;0.;1.;1.;1.|]
      ~z:(Array.make 5 0.) in
  let topology = Topology.polygons_owned ~point_count:5
      ~vertex_points:[|0;1;2; 0;3;2; 2;3;4|]
      ~primitive_offsets:[|0;3;6;9|] |> get_string in
  let corners = Attribute.create_owned ~owner:Attribute.Vertex ~name:"corner_id"
      (Attribute.Int (Array.init 9 Fun.id)) |> get_string in
  let source = Geometry.create ~positions ~topology ~attributes:[corners] ()
      |> get_string in
  let selected = primitive_group source (fun primitive -> primitive < 2) in
  let output = Facet.run ~selection:(Transform_ops.Selected_primitives selected) ~orient_polygons:true source
      |> get_ok in
  let topology = Topology.View.view (Geometry.topology output) in
  check (topology.vertex_points = [|0;1;2; 2;3;0; 2;3;4|])
    "grouped Facet Orient Polygons changed selection boundary behavior";
  (match Geometry.find_attribute ~owner:Attribute.Vertex "corner_id" output with
   | Some attribute ->
       check (Attribute.storage attribute
          = Attribute.Int [|0;1;2; 5;4;3; 6;7;8|])
         "grouped Facet Orient Polygons changed unselected vertices"
   | None -> fail "grouped Facet Orient Polygons lost corner id");
  let nonmanifold_topology = Topology.polygons_owned ~point_count:5
      ~vertex_points:[|0;1;2; 1;0;3; 0;1;4|]
      ~primitive_offsets:[|0;3;6;9|] |> get_string in
  let nonmanifold = Geometry.create ~positions ~topology:nonmanifold_topology ()
      |> get_string in
  let one_face = primitive_group nonmanifold (fun primitive -> primitive = 0) in
  check (Facet.run ~selection:(Transform_ops.Selected_primitives one_face) ~orient_polygons:true nonmanifold
      |> get_ok == nonmanifold)
    "grouped Facet inspected an unselected non-manifold neighborhood"

let check_cusp_polygons () =
  let source = Box_generator.box ~connectivity:Box_generator.Box_quads ~consolidate_points:true
      ~normals:Box_generator.Box_point_normals ~uv_attribute:"uv"
      ~size:(Rays_math.Vec3.create 2. 2. 2.) () |> get_ok in
  let source = source |> add_attribute Attribute.Point "point_id"
      (Attribute.Int (Array.init (Geometry.point_count source) Fun.id)) in
  let corner = Group.ordered ~owner:Group.Point ~name:"corner" ~length:8
      [|0|] |> get_string in
  let source = Geometry.with_group corner source |> get_string in
  let topology = Geometry.topology source in
  let index = Topology_index.create topology in
  let crease = Edge_group.init ~topology ~index ~name:"marked_edge"
      (fun edge -> edge = 0) in
  let source = Geometry.with_edge_group crease source |> get_string in
  let smooth = Facet.run ~cusp_angle:2. source |> get_ok in
  check (smooth == source)
    "Facet Cusp Polygons split edges below the threshold";
  let exactly_threshold = Facet.run ~cusp_angle:(Float.pi /. 2.) source
      |> get_ok in
  check (exactly_threshold == source)
    "Facet Cusp Polygons split edges equal to the threshold";
  let hard = Facet.run ~cusp_angle:1. source |> get_ok in
  check (Geometry.point_count hard = 24 && Geometry.vertex_count hard = 24)
    "Facet Cusp Polygons cube cardinality";
  let corner = Geometry.find_group ~owner:Group.Point "corner" hard
      |> Option.get in
  check (Group.cardinality corner = 3)
    "Facet Cusp Polygons point-group ancestry";
  (match Geometry.find_attribute ~owner:Attribute.Point "point_id" hard with
   | Some attribute ->
       (match Attribute.View.storage attribute with
        | Attribute.Int values ->
            check (Array.fold_left (fun count value ->
                if value = 0 then count + 1 else count) 0 values = 3)
              "Facet Cusp Polygons point-attribute ancestry"
        | _ -> fail "Facet cusped point_id has wrong storage")
   | None -> fail "Facet Cusp Polygons lost point_id");
  check (Geometry.find_edge_group "marked_edge" hard |> Option.get
      |> Edge_group.cardinality = 2)
    "Facet Cusp Polygons one-to-many edge ancestry";
  let shaded = Facet.run ~cusp_angle:1. ~post_compute_normals:true source
      |> get_ok in
  let normal = normal_attribute shaded in
  for point = 0 to Geometry.point_count shaded - 1 do
    let length = sqrt ((normal.x.(point) *. normal.x.(point))
      +. (normal.y.(point) *. normal.y.(point))
      +. (normal.z.(point) *. normal.z.(point))) in
    check (near length 1.) "Facet cusped post normal"
  done;
  List.iter (fun angle -> match Facet.run ~cusp_angle:angle source with
    | Error error when Error.code error = "invalid_geometry" -> ()
    | _ -> fail "Facet accepted an invalid cusp angle")
    [(-0.1); Float.pi +. 0.1; Float.nan];
  let curve = Line_geometry.polyline [|(0.,0.,0.); (1.,0.,0.)|] |> get_ok in
  (match Facet.run ~cusp_angle:1. curve with
   | Error error when Error.code error = "invalid_geometry" -> ()
   | _ -> fail "Facet Cusp Polygons accepted curve topology")

let check_primitive_group_cusp_polygons () =
  let source = Box_generator.box ~connectivity:Box_generator.Box_quads ~consolidate_points:true
      ~size:(Rays_math.Vec3.create 2. 2. 2.) () |> get_ok in
  let selected = primitive_group source (fun primitive -> primitive = 0) in
  let output = Facet.run ~selection:(Transform_ops.Selected_primitives selected) ~cusp_angle:1. source |> get_ok in
  check (Geometry.point_count output = 12)
    "grouped Facet Cusp Polygons cardinality";
  let topology = Topology.View.view (Geometry.topology output) in
  for vertex = topology.primitive_offsets.(0)
      to topology.primitive_offsets.(1) - 1 do
    check (topology.vertex_points.(vertex) >= 8)
      "grouped Facet did not separate a selected cusp corner"
  done;
  for primitive = 1 to Geometry.primitive_count output - 1 do
    for vertex = topology.primitive_offsets.(primitive)
        to topology.primitive_offsets.(primitive + 1) - 1 do
      check (topology.vertex_points.(vertex) < 8)
        "grouped Facet changed an unselected cusp point"
    done
  done

let polygon_planarity_error geometry primitive =
  let topology = Topology.View.view (Geometry.topology geometry)
  and positions = Packed.Float3.View.view (Geometry.positions geometry) in
  let first = topology.primitive_offsets.(primitive)
  and last = topology.primitive_offsets.(primitive + 1) in
  if last - first <= 3 then 0.
  else
    let point index = topology.vertex_points.(first + index) in
    let a = point 0 and b = point 1 and c = point 2 in
    let ux = positions.x.(b) -. positions.x.(a)
    and uy = positions.y.(b) -. positions.y.(a)
    and uz = positions.z.(b) -. positions.z.(a)
    and vx = positions.x.(c) -. positions.x.(a)
    and vy = positions.y.(c) -. positions.y.(a)
    and vz = positions.z.(c) -. positions.z.(a) in
    let nx = (uy *. vz) -. (uz *. vy)
    and ny = (uz *. vx) -. (ux *. vz)
    and nz = (ux *. vy) -. (uy *. vx) in
    let length = sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) in
    let maximum = ref 0. in
    if length > 0. then
      for vertex = first + 3 to last - 1 do
        let point = topology.vertex_points.(vertex) in
        let distance = abs_float (((positions.x.(point) -. positions.x.(a)) *. nx)
            +. ((positions.y.(point) -. positions.y.(a)) *. ny)
            +. ((positions.z.(point) -. positions.z.(a)) *. nz)) /. length in
        if distance > !maximum then maximum := distance
      done;
    !maximum

let warped_quads () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;1.;0.; 0.;(-1.)|]
      ~y:[|0.;0.;1.;1.; 2.;2.|]
      ~z:[|0.;0.;0.6;0.; 0.4;0.|] in
  let topology = Topology.polygons_owned ~point_count:6
      ~vertex_points:[|0;1;2;3; 0;3;4;5|]
      ~primitive_offsets:[|0;4;8|] |> get_string in
  let point_id = Attribute.create_owned ~owner:Attribute.Point ~name:"point_id"
      (Attribute.Int (Array.init 6 Fun.id)) |> get_string in
  Geometry.create ~positions ~topology ~attributes:[point_id] () |> get_string

let check_make_planar () =
  let source = warped_quads () in
  let selected = primitive_group source (fun primitive -> primitive = 0) in
  let output = Facet.run ~selection:(Transform_ops.Selected_primitives selected) ~make_planar:true source |> get_ok in
  check (Geometry.point_count output = 8
      && polygon_planarity_error output 0 <= 1e-12)
    "grouped Facet Make Planar did not flatten the selected polygon";
  check (polygon_planarity_error output 1 = polygon_planarity_error source 1)
    "grouped Facet Make Planar moved an unselected polygon";
  let topology = Topology.View.view (Geometry.topology output) in
  check (topology.vertex_points.(0) >= 6 && topology.vertex_points.(3) >= 6
      && Array.sub topology.vertex_points 4 4 = [|0;3;4;5|])
    "Facet Make Planar did not isolate shared projected corners";
  (match Geometry.find_attribute ~owner:Attribute.Point "point_id" output with
   | Some attribute ->
       (match Attribute.storage attribute with
        | Attribute.Int values ->
            check (values.(topology.vertex_points.(0)) = 0
                && values.(topology.vertex_points.(3)) = 3)
              "Facet Make Planar point ancestry"
        | _ -> fail "Facet Make Planar point id has wrong storage")
   | None -> fail "Facet Make Planar lost point id");
  let planar_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;1.;0.|] ~y:[|0.;0.;1.;1.|] ~z:(Array.make 4 0.) in
  let planar_topology = Topology.polygons_owned ~point_count:4
      ~vertex_points:[|0;1;2;3|] ~primitive_offsets:[|0;4|] |> get_string in
  let planar = Geometry.create ~positions:planar_positions
      ~topology:planar_topology () |> get_string in
  check (Facet.run ~make_planar:true planar |> get_ok == planar)
    "Facet Make Planar changed an already planar polygon";
  let curve = Line_geometry.polyline
      [|(0.,0.,0.); (1.,0.,1.); (2.,1.,0.); (3.,0.,1.)|] |> get_ok in
  check (Facet.run ~make_planar:true curve |> get_ok == curve)
    "Facet Make Planar changed a polygon curve";
  let degenerate_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;2.;3.|] ~y:(Array.make 4 0.) ~z:(Array.make 4 0.) in
  let degenerate = Geometry.create ~positions:degenerate_positions
      ~topology:planar_topology () |> get_string in
  (match Facet.run ~make_planar:true degenerate with
   | Error error when Error.code error = "invalid_geometry" -> ()
   | _ -> fail "Facet Make Planar accepted a plane-less polygon");
  let nonfinite_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;1.;0.; 2.;3.;3.;2.|]
      ~y:[|0.;0.;1.;1.; 0.;0.;1.;1.|]
      ~z:[|0.;0.;0.;0.; 0.;0.;Float.nan;0.|] in
  let nonfinite_topology = Topology.polygons_owned ~point_count:8
      ~vertex_points:[|0;1;2;3; 4;5;6;7|]
      ~primitive_offsets:[|0;4;8|] |> get_string in
  let nonfinite = Geometry.create ~positions:nonfinite_positions
      ~topology:nonfinite_topology () |> get_string in
  let finite_only = primitive_group nonfinite (fun primitive -> primitive = 0) in
  check (Facet.run ~selection:(Transform_ops.Selected_primitives finite_only) ~make_planar:true nonfinite
      |> get_ok == nonfinite)
    "Facet Make Planar inspected an unselected non-finite polygon";
  let all = primitive_group nonfinite (fun _ -> true) in
  (match Facet.run ~selection:(Transform_ops.Selected_primitives all) ~make_planar:true nonfinite with
   | Error error when Error.code error = "invalid_geometry" -> ()
   | _ -> fail "Facet Make Planar accepted a selected non-finite polygon")

let check_primitive_group_normals_and_cleanup () =
  let source = source ()
      |> add_attribute Attribute.Point "N"
        (Attribute.Float3 (Packed.Float3.View.of_owned_exn
          ~x:[|1.;2.;3.;4.;Float.nan|] ~y:(Array.make 5 0.)
          ~z:(Array.make 5 0.)))
      |> add_attribute Attribute.Vertex "N"
        (Attribute.Float3 (Packed.Float3.View.of_owned_exn
          ~x:[|1.;2.;3.;4.;5.;6.|] ~y:(Array.make 6 0.)
          ~z:(Array.make 6 0.))) in
  let selected = primitive_group source (fun primitive -> primitive = 0) in
  let output = Facet.run ~selection:(Transform_ops.Selected_primitives selected) ~reverse_normals:true source
      |> get_ok in
  (match Geometry.find_attribute ~owner:Attribute.Point "N" output with
   | Some attribute ->
       (match Attribute.storage attribute with
        | Attribute.Float3 values ->
            let values = Packed.Float3.View.view values in
            check (values.x.(0) = -1. && values.x.(1) = -2.
                && values.x.(2) = -3. && values.x.(3) = 4.
                && Float.is_nan values.x.(4))
              "grouped Facet point-normal ownership"
        | _ -> fail "grouped Facet point N has wrong storage")
   | None -> fail "grouped Facet lost point N");
  (match Geometry.find_attribute ~owner:Attribute.Vertex "N" output with
   | Some attribute ->
       (match Attribute.storage attribute with
        | Attribute.Float3 values ->
            let values = Packed.Float3.View.view values in
            check (values.x = [|-1.;-2.;-3.;4.;5.;6.|])
              "grouped Facet vertex-normal ownership"
        | _ -> fail "grouped Facet vertex N has wrong storage")
   | None -> fail "grouped Facet lost vertex N");
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;2.; 0.;1.;2.|] ~y:[|0.;0.;0.; 1.;1.;1.|]
      ~z:(Array.make 6 0.) in
  let topology = Topology.polygons_owned ~point_count:6
      ~vertex_points:[|0;1;2; 3;4;5|] ~primitive_offsets:[|0;3;6|]
      |> get_string in
  let face_id = Attribute.create_owned ~owner:Attribute.Primitive ~name:"face_id"
      (Attribute.Int [|10;20|]) |> get_string in
  let degenerate = Geometry.create ~positions ~topology ~attributes:[face_id] ()
      |> get_string in
  let selected = primitive_group degenerate (fun primitive -> primitive = 0) in
  let output = Facet.run ~selection:(Transform_ops.Selected_primitives selected) ~remove_degenerate:true
      ~post_compute_normals:true ~reverse_normals:true degenerate |> get_ok in
  check (Geometry.primitive_count output = 1)
    "grouped Facet degenerate cleanup cardinality";
  (match Geometry.find_attribute ~owner:Attribute.Primitive "face_id" output with
   | Some attribute -> check (Attribute.storage attribute = Attribute.Int [|20|])
       "grouped Facet degenerate cleanup ancestry"
   | None -> fail "grouped Facet cleanup lost primitive payload");
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;0.;1.; 0.;2.;3.|] ~y:[|0.;0.;0.; 0.;0.;0.|]
      ~z:[|0.;0.;1.; 0.;1.;1.|] in
  let topology = Topology.polygons_owned ~point_count:6
      ~vertex_points:[|0;1;2; 3;4;5|] ~primitive_offsets:[|0;3;6|]
      |> get_string in
  let geometry = Geometry.create ~positions ~topology () |> get_string in
  let selected = primitive_group geometry (fun primitive -> primitive = 0) in
  let fused = Facet.run ~selection:(Transform_ops.Selected_primitives selected) ~consolidate_distance:0. geometry
      |> get_ok in
  check (Geometry.point_count fused = 5)
    "grouped Facet point consolidation cardinality";
  let topology = Topology.View.view (Geometry.topology fused) in
  check (topology.vertex_points.(0) = topology.vertex_points.(1)
      && topology.vertex_points.(3) <> topology.vertex_points.(0))
    "grouped Facet point consolidation crossed its selection boundary"

let check_validation () =
  let source = source () in
  check (Facet.run source |> get_ok == source) "Facet default is not identity";
  (match Facet.run ~consolidate_distance:(-1.) source with
   | Error error when Error.code error = "invalid_geometry" -> ()
   | _ -> fail "Facet accepted a negative consolidation distance");
  let wrong = source |> add_attribute Attribute.Point "N"
      (Attribute.Float (Array.make 5 1.)) in
  (match Facet.run ~make_normals_unit_length:true wrong with
   | Error error when Error.code error = "invalid_geometry" -> ()
   | _ -> fail "Facet accepted non-vector normals");
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match Facet.run ~cancel:cancelled ~unique_points:true source with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "Facet ignored cancellation");
  (match Facet.run ~cancel:cancelled ~remove_inline_points:true source with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "Facet inline removal ignored cancellation");
  (match Facet.run ~cancel:cancelled ~consolidate_normals_distance:0. source with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "Facet normal consolidation ignored cancellation");
  (match Facet.run ~cancel:cancelled ~make_planar:true (warped_quads ()) with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "Facet Make Planar ignored cancellation");
  let selected_points = Group.init ~owner:Group.Point ~name:"selected" 5
      (fun point -> point = 0) in
  (match Facet.run ~cancel:cancelled
      ~selection:(Transform_ops.Selected_points selected_points) ~unique_points:true source with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "Facet typed selection promotion ignored cancellation");
  let wrong_owner = Group.init ~owner:Group.Point ~name:"wrong" 5
      (fun _ -> true)
  and wrong_length = Group.init ~owner:Group.Primitive ~name:"wrong" 1
      (fun _ -> true) in
  List.iter (fun primitives ->
    match Facet.run ~selection:(Transform_ops.Selected_primitives primitives) ~unique_points:true source with
    | Error error when Error.code error = "invalid_geometry" -> ()
    | _ -> fail "Facet accepted a malformed primitive selection")
    [wrong_owner; wrong_length];
  let collision = Group.init ~owner:Group.Primitive
      ~name:"__rdk_facet_selection_0" 2 (fun primitive -> primitive = 1) in
  let collision_source = Geometry.with_group collision source |> get_string in
  let selected = primitive_group collision_source (fun primitive -> primitive = 0) in
  let output = Facet.run ~selection:(Transform_ops.Selected_primitives selected) ~unique_points:true
      collision_source |> get_ok in
  (match Geometry.find_group ~owner:Group.Primitive
      "__rdk_facet_selection_0" output with
   | Some group -> check (equal_group group collision)
       "Facet replaced a colliding user primitive group"
   | None -> fail "Facet removed a colliding user primitive group")

let repeated_inline_source polygons =
  let points = polygons * 6 in
  let x = Array.make points 0. and y = Array.make points 0.
  and z = Array.make points 0. in
  for primitive = 0 to polygons - 1 do
    let point = primitive * 6 and base = float_of_int primitive *. 4. in
    x.(point) <- base;
    x.(point + 1) <- base +. 1.;
    x.(point + 2) <- base +. 2.;
    x.(point + 3) <- base +. 3.;
    x.(point + 4) <- base +. 3.; z.(point + 4) <- 1.;
    x.(point + 5) <- base; z.(point + 5) <- 1.
  done;
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  let topology = Topology.polygons_owned ~point_count:points
      ~vertex_points:(Array.init points Fun.id)
      ~primitive_offsets:(Array.init (polygons + 1)
        (fun primitive -> primitive * 6)) |> get_string in
  let point_id = Attribute.create_owned ~owner:Attribute.Point ~name:"point_id"
      (Attribute.Int (Array.init points Fun.id)) |> get_string in
  Geometry.create ~positions ~topology ~attributes:[point_id] () |> get_string

let repeated_normal_source pairs =
  let point_count = pairs * 2 in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. and nx = Array.make point_count 0.
  and ny = Array.make point_count 0. and nz = Array.make point_count 0. in
  for pair = 0 to pairs - 1 do
    let point = pair * 2 and base = float_of_int pair *. 2. in
    x.(point) <- base;
    x.(point + 1) <- base +. 0.0001;
    nx.(point) <- 1.;
    ny.(point + 1) <- 1.
  done;
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  let topology = Topology.View.create_validated_owned ~point_count
      ~vertex_points:[||] ~primitive_offsets:[|0|] ~primitive_kinds:Bytes.empty in
  let normal = Attribute.create_owned ~owner:Attribute.Point ~name:"N"
      (Attribute.Float3 (Packed.Float3.View.of_owned_exn ~x:nx ~y:ny ~z:nz))
      |> get_string in
  Geometry.create ~positions ~topology ~attributes:[normal] () |> get_string

let repeated_warped_quads polygons =
  let point_count = polygons * 4 in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. in
  for primitive = 0 to polygons - 1 do
    let point = primitive * 4 and base = float_of_int primitive *. 2. in
    x.(point) <- base;
    x.(point + 1) <- base +. 1.;
    x.(point + 2) <- base +. 1.; y.(point + 2) <- 1.;
    y.(point + 3) <- 1.;
    z.(point + 2) <- if primitive land 1 = 0 then 0.25 else -0.25
  done;
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  let topology = Topology.polygons_owned ~point_count
      ~vertex_points:(Array.init point_count Fun.id)
      ~primitive_offsets:(Array.init (polygons + 1)
        (fun primitive -> primitive * 4)) |> get_string in
  Geometry.create ~positions ~topology () |> get_string

let check_parallel_exact () =
  let source = Plane_generators.grid ~columns:300 ~rows:240 ~uv_attribute:"uv" ~size:20. ()
      |> get_ok in
  let run domains = Parallel.run ~domains (fun () ->
      Facet.run ~grain:257 ~pre_compute_normals:true ~unique_points:true
        ~make_normals_unit_length:true ~reverse_normals:true source |> get_ok) in
  let one = run 1 and many = run 4 in
  check (equal_geometry one many)
    "one-domain and four-domain Facet geometry differ";
  let inline = repeated_inline_source 40_000 in
  let run domains = Parallel.run ~domains (fun () ->
      Facet.run ~grain:257 ~remove_inline_points:true inline |> get_ok) in
  let one = run 1 and many = run 4 in
  check (Geometry.point_count one = 160_000
      && Geometry.vertex_count one = 160_000)
    "Facet Remove Inline Points parallel cardinality";
  check (equal_geometry one many)
    "one-domain and four-domain Facet inline removal differ";
  let normal_source = repeated_normal_source 200_000 in
  let run domains = Parallel.run ~domains (fun () ->
      Facet.run ~grain:257 ~consolidate_normals_distance:0.001 normal_source
      |> get_ok) in
  let one = run 1 and many = run 4 in
  check (equal_geometry one many)
    "one-domain and four-domain Facet normal consolidation differ";
  let normal = normal_attribute one in
  check (near normal.x.(0) 0.5 && near normal.y.(0) 0.5
      && near normal.x.(399_999) 0.5 && near normal.y.(399_999) 0.5)
    "Facet parallel consolidated-normal values";
  let source_topology = Topology.View.view (Geometry.topology source) in
  let vertex_points = Array.copy source_topology.vertex_points in
  for primitive = 1 to Geometry.primitive_count source - 1 do
    if primitive land 1 = 1 then begin
      let first = source_topology.primitive_offsets.(primitive)
      and last = source_topology.primitive_offsets.(primitive + 1) in
      for local = 0 to (last - first) / 2 - 1 do
        let left = first + local and right = last - local - 1 in
        let swap = vertex_points.(left) in
        vertex_points.(left) <- vertex_points.(right);
        vertex_points.(right) <- swap
      done
    end
  done;
  let topology = Topology.View.create_validated_owned
      ~point_count:source_topology.point_count ~vertex_points
      ~primitive_offsets:(Array.copy source_topology.primitive_offsets)
      ~primitive_kinds:(Bytes.copy source_topology.primitive_kinds) in
  let inconsistent = Geometry.create ~positions:(Geometry.positions source)
      ~topology ~attributes:(Geometry.attributes source)
      ~groups:(Geometry.groups source) () |> get_string in
  let run domains = Parallel.run ~domains (fun () ->
      Facet.run ~grain:257 ~orient_polygons:true inconsistent |> get_ok) in
  let one = run 1 and many = run 4 in
  check (equal_geometry one many)
    "one-domain and four-domain Facet orientation differ";
  check (not (equal_geometry one inconsistent))
    "Facet orientation left the inconsistent grid unchanged";
  let displaced = Plane_generators.grid ~columns:240 ~rows:180 ~uv_attribute:"uv" ~size:20.
      () |> get_ok
      |> displace_y ~amplitude:0.8 ~frequency:0.7
           ~seed:927 |> get_string_ok in
  let run domains = Parallel.run ~domains (fun () ->
      Facet.run ~grain:257 ~cusp_angle:0.08 displaced |> get_ok) in
  let one = run 1 and many = run 4 in
  check (equal_geometry one many)
    "one-domain and four-domain Facet cusping differ";
  check (Geometry.point_count one > Geometry.point_count displaced)
    "Facet cusping split no displaced edges";
  let selected = primitive_group source
      (fun primitive -> primitive land 1 = 0) in
  let run domains = Parallel.run ~domains (fun () ->
      Facet.run ~grain:257 ~selection:(Transform_ops.Selected_primitives selected) ~pre_compute_normals:true
        ~unique_points:true ~reverse_normals:true source |> get_ok) in
  let one = run 1 and many = run 4 in
  check (equal_geometry one many)
    "one-domain and four-domain grouped Facet geometry differ";
  let selected_points = Group.init ~owner:Group.Point ~name:"facet_left"
      (Geometry.point_count source)
      (fun point -> point < Geometry.point_count source / 2) in
  let run domains = Parallel.run ~domains (fun () ->
      Facet.run ~grain:257 ~selection:(Transform_ops.Selected_points selected_points)
        ~unique_points:true source |> get_ok) in
  let one = run 1 and many = run 4 in
  check (equal_geometry one many)
    "one-domain and four-domain point-selected Facet geometry differ";
  check (Geometry.point_count one > Geometry.point_count source)
    "point-selected Facet split no points";
  let topology = Geometry.topology source in
  let index = Topology_index.create topology in
  let selected_edges = Edge_group.init ~topology ~index ~name:"facet_edges"
      (fun edge -> edge mod 7 = 0) in
  let run domains = Parallel.run ~domains (fun () ->
      Facet.run ~grain:257 ~selection:(Transform_ops.Selected_edges selected_edges)
        ~unique_points:true source |> get_ok) in
  let one = run 1 and many = run 4 in
  check (equal_geometry one many)
    "one-domain and four-domain edge-selected Facet geometry differ";
  check (Geometry.point_count one > Geometry.point_count source)
    "edge-selected Facet split no points";
  let inline_selected = primitive_group inline
      (fun primitive -> primitive land 1 = 0) in
  let run domains = Parallel.run ~domains (fun () ->
      Facet.run ~grain:257 ~selection:(Transform_ops.Selected_primitives inline_selected)
        ~remove_inline_points:true inline |> get_ok) in
  let one = run 1 and many = run 4 in
  check (Geometry.point_count one = 200_000
      && Geometry.vertex_count one = 200_000)
    "grouped Facet inline parallel cardinality";
  check (equal_geometry one many)
    "one-domain and four-domain grouped Facet inline removal differ";
  let warped = repeated_warped_quads 100_000 in
  let warped_selected = primitive_group warped
      (fun primitive -> primitive land 1 = 0) in
  let run domains = Parallel.run ~domains (fun () ->
      Facet.run ~grain:257 ~selection:(Transform_ops.Selected_primitives warped_selected) ~make_planar:true warped
      |> get_ok) in
  let one = run 1 and many = run 4 in
  check (equal_geometry one many)
    "one-domain and four-domain grouped Facet Make Planar differ";
  check (polygon_planarity_error one 0 <= 1e-12
      && polygon_planarity_error one 1 > 0.1)
    "grouped Facet Make Planar parallel selection"

let run () =
  check_unique_points ();
  check_primitive_group_unique_points ();
  check_typed_selections ();
  check_normals ();
  check_consolidate_normals ();
  check_remove_inline_points ();
  check_primitive_group_inline_points ();
  check_orient_polygons ();
  check_primitive_group_orient_polygons ();
  check_cusp_polygons ();
  check_primitive_group_cusp_polygons ();
  check_make_planar ();
  check_primitive_group_normals_and_cleanup ();
  check_validation ();
  check_parallel_exact ();
  print_endline "Facet tests passed"
