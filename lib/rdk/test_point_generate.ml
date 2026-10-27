open Rays
open Rdk

let fail message = prerr_endline message; exit 1
let check condition message = if not condition then fail message
let get = function Ok value -> value | Error error -> fail (Error.to_string error)

let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> Result.get_ok

let source () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|1.;2.;3.;9.|] ~y:[|0.;1.;2.;9.|] ~z:[|0.;0.;0.;9.|] in
  let topology = Topology.polygons_owned ~point_count:4
      ~vertex_points:[|0;1;2|] ~primitive_offsets:[|0;3|]
      |> Result.get_ok in
  let uv = Packed.Float2.of_owned ~x:[|0.;0.5;1.;2.|]
      ~y:[|1.;0.5;0.;2.|] |> Result.get_ok in
  let rows = Packed.Int_array.create_owned ~offsets:[|0;1;3;3;5|]
      ~values:[|10;20;21;30;31|] |> Result.get_ok in
  let float_rows = Packed.Float_array.create_owned ~offsets:[|0;2;3;3;5|]
      ~values:[|0.1;0.2;1.1;3.1;3.2|] |> Result.get_ok
  and direction = Packed.Float3.Private.of_owned_exn
      ~x:[|1.;2.;3.;4.|] ~y:[|5.;6.;7.;8.|] ~z:[|9.;10.;11.;12.|]
  and rgba = Packed.Float4.of_owned ~x:[|0.;1.;2.;3.|]
      ~y:[|4.;5.;6.;7.|] ~z:[|8.;9.;10.;11.|] ~w:[|1.;0.8;0.6;0.4|]
      |> Result.get_ok in
  let selected = Group.ordered ~owner:Group.Point ~name:"selected"
      ~length:4 [|2;0|] |> Result.get_ok
  and face = Group.init ~grain:1 ~owner:Group.Primitive ~name:"face" 1
      (Fun.const true) in
  let index = Topology_index.create topology in
  let boundary = Edge_group.init ~grain:1 ~topology ~index ~name:"boundary"
      (Fun.const true) in
  Geometry.create ~positions ~topology ~attributes:[
    attribute Attribute.Point "id" (Attribute.Int [|10;11;12;13|]);
    attribute Attribute.Point "weight" (Attribute.Float [|1.;0.5;2.;0.|]);
    attribute Attribute.Point "probability" (Attribute.Float [|1.;0.;1.;0.|]);
    attribute Attribute.Point "tag" (Attribute.Text [|"a";"b";"c";"free"|]);
    attribute Attribute.Point "uv" (Attribute.Float2 uv);
    attribute Attribute.Point "direction" (Attribute.Float3 direction);
    attribute Attribute.Point "rgba" (Attribute.Float4 rgba);
    attribute Attribute.Point "rows" (Attribute.Int_array rows);
    attribute Attribute.Point "float_rows" (Attribute.Float_array float_rows);
    attribute Attribute.Vertex "corner" (Attribute.Int [|4;5;6|]);
    attribute Attribute.Primitive "material" (Attribute.Int [|7|]);
    attribute Attribute.Detail "author" (Attribute.Text [|"rays"|]);
  ] ~groups:[selected;face] ~edge_groups:[boundary] () |> Result.get_ok

let int_attr name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Int values -> values | _ -> fail (name ^ " is not integer"))
  | None -> fail ("missing point attribute " ^ name)

let float_attr name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float values -> values | _ -> fail (name ^ " is not float"))
  | None -> fail ("missing point attribute " ^ name)

let text_attr name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Text values -> values | _ -> fail (name ^ " is not text"))
  | None -> fail ("missing point attribute " ^ name)

let test_total () =
  let output = Point_generate.run ~mode:(Point_generate.Generate_total 5) (source ()) |> get in
  check (Geometry.point_count output = 5 && Geometry.vertex_count output = 0)
    "Point Generate total cardinality/topology";
  let positions = Packed.Float3.Private.view (Geometry.positions output) in
  check (positions.x = Array.make 5 0. && positions.y = Array.make 5 0.
      && positions.z = Array.make 5 0.) "Point Generate total positions";
  check (int_attr "sourcepoint" output = Array.make 5 (-1)
      && int_attr "sourceindex" output = [|0;1;2;3;4|])
    "Point Generate total provenance";
  check (Geometry.find_attribute ~owner:Attribute.Point "id" output = None)
    "Point Generate total copied unrelated input payload"

let test_per_point_and_probability () =
  let input = source () in
  let selected = Geometry.find_group ~owner:Group.Point "selected" input
      |> Option.get in
  let output = Point_generate.run ~points:selected
      ~mode:(Point_generate.Generate_per_point {
        points_per_point = 2.; scale_attribute = None }) input |> get in
  check (Geometry.point_count output = 4) "Point Generate selected count";
  check (int_attr "sourcepoint" output = [|0;0;2;2|]
      && int_attr "sourceindex" output = [|0;1;0;1|])
    "Point Generate stable selected provenance";
  check (int_attr "id" output = [|10;10;12;12|]
      && text_attr "tag" output = [|"a";"a";"c";"c"|])
    "Point Generate copied source payload";
  (match Geometry.find_attribute ~owner:Attribute.Point "direction" output
      |> Option.get |> Attribute.Private.storage with
   | Attribute.Float3 values ->
       let values = Packed.Float3.Private.view values in
       check (values.x = [|1.;1.;3.;3.|] && values.z = [|9.;9.;11.;11.|])
         "Point Generate float3 payload"
   | _ -> fail "Point Generate direction storage");
  (match Geometry.find_attribute ~owner:Attribute.Point "rgba" output
      |> Option.get |> Attribute.Private.storage with
   | Attribute.Float4 values ->
       let values = Packed.Float4.Private.view values in
       check (values.w = [|1.;1.;0.6;0.6|]) "Point Generate float4 payload"
   | _ -> fail "Point Generate rgba storage");
  (match Geometry.find_attribute ~owner:Attribute.Point "float_rows" output
      |> Option.get |> Attribute.Private.storage with
   | Attribute.Float_array values ->
       let values = Packed.Float_array.Private.view values in
       check (values.offsets = [|0;2;4;4;4|]
           && values.values = [|0.1;0.2;0.1;0.2|])
         "Point Generate float-array payload"
   | _ -> fail "Point Generate float_rows storage");
  let scaled = Point_generate.run
      ~mode:(Point_generate.Generate_per_point {
        points_per_point = 2.; scale_attribute = Some "weight" }) input |> get in
  check (Geometry.point_count scaled = 7
      && int_attr "sourcepoint" scaled = [|0;0;1;2;2;2;2|])
    "Point Generate scaled counts";
  let probability = Point_generate.run ~seed:(Rays_math.Rand.seed 19)
      ~mode:(Point_generate.Generate_probability { attribute = "probability" }) input
      |> get in
  check (Geometry.point_count probability = 2
      && int_attr "sourcepoint" probability = [|0;2|]
      && int_attr "sourceindex" probability = [|0;0|])
    "Point Generate probability endpoints"

let test_keep_input_patterns_groups_and_edges () =
  let input = source () in
  let output = Point_generate.run ~keep_input:true ~generated_group:"generated"
      ~copy_point_attributes:"id rows ^tag"
      ~mode:(Point_generate.Generate_per_point {
        points_per_point = 1.; scale_attribute = None }) input |> get in
  check (Geometry.point_count output = 8 && Geometry.vertex_count output = 3
      && Geometry.primitive_count output = 1)
    "Point Generate keep-input cardinality";
  let source_topology = Topology.Private.view (Geometry.topology input)
  and target_topology = Topology.Private.view (Geometry.topology output) in
  check (source_topology.vertex_points == target_topology.vertex_points
      && source_topology.primitive_offsets == target_topology.primitive_offsets
      && source_topology.primitive_kinds == target_topology.primitive_kinds)
    "Point Generate did not structurally share input topology planes";
  check (int_attr "id" output = [|10;11;12;13;10;11;12;13|])
    "Point Generate keep-input copied selected attribute";
  check (text_attr "tag" output = [|"a";"b";"c";"free";"";"";"";""|])
    "Point Generate keep-input unmatched defaults";
  let generated = Geometry.find_group ~owner:Group.Point "generated" output
      |> Option.get in
  check (Group.cardinality generated = 4
      && List.init 4 (fun index -> Group.mem (index + 4) generated)
         = [true;true;true;true])
    "Point Generate generated group";
  let selected = Geometry.find_group ~owner:Group.Point "selected" output
      |> Option.get in
  check (Group.cardinality selected = 2
      && Group.ordered_elements selected = Some [|2;0|])
    "Point Generate source point-group preservation";
  let selected_output = Point_generate.run ~keep_input:true
      ~generated_group:"selected"
      ~mode:(Point_generate.Generate_per_point {
        points_per_point = 1.; scale_attribute = None }) input |> get
      |> Geometry.find_group ~owner:Group.Point "selected" |> Option.get in
  check (Group.cardinality selected_output = 6
      && Group.ordered_elements selected_output = Some [|2;0;4;5;6;7|])
    "Point Generate generated-group ordered union";
  let edge = Geometry.find_edge_group "boundary" output |> Option.get in
  check (Edge_group.cardinality edge = 3
      && Edge_group.topology_data_id edge = Topology.data_id (Geometry.topology output))
    "Point Generate native-edge rebind";
  check (Geometry.find_attribute ~owner:Attribute.Vertex "corner" output
      |> Option.get
      == (Geometry.find_attribute ~owner:Attribute.Vertex "corner" input
          |> Option.get))
    "Point Generate vertex payload sharing";
  check (Geometry.find_attribute ~owner:Attribute.Detail "author" output
      |> Option.get
      == (Geometry.find_attribute ~owner:Attribute.Detail "author" input
          |> Option.get))
    "Point Generate detail payload sharing"

let test_detail_pattern_and_ragged () =
  let output = Point_generate.run ~copy_point_attributes:"rows uv"
      ~copy_detail_attributes:"author"
      ~mode:(Point_generate.Generate_per_point {
        points_per_point = 1.; scale_attribute = None }) (source ()) |> get in
  check (Geometry.find_attribute ~owner:Attribute.Detail "author" output <> None)
    "Point Generate detail pattern";
  check (Geometry.find_attribute ~owner:Attribute.Point "id" output = None)
    "Point Generate point exclusion";
  let rows = Geometry.find_attribute ~owner:Attribute.Point "rows" output
      |> Option.get in
  (match Attribute.Private.storage rows with
   | Attribute.Int_array values ->
       let values = Packed.Int_array.Private.view values in
       check (values.offsets = [|0;1;3;3;5|]
           && values.values = [|10;20;21;30;31|])
         "Point Generate ragged copy"
   | _ -> fail "Point Generate rows storage")

let test_existing_provenance () =
  let input = source ()
      |> Geometry.with_attribute (attribute Attribute.Point "sourcepoint"
           (Attribute.Int [|90;91;92;93|])) |> Result.get_ok
      |> Geometry.with_attribute (attribute Attribute.Point "sourceindex"
           (Attribute.Int [|8;7;6;5|])) |> Result.get_ok in
  let output = Point_generate.run ~keep_input:true
      ~mode:(Point_generate.Generate_per_point {
        points_per_point = 1.; scale_attribute = None }) input |> get in
  check (int_attr "sourcepoint" output = [|90;91;92;93;0;1;2;3|]
      && int_attr "sourceindex" output = [|8;7;6;5;0;0;0;0|])
    "Point Generate retained provenance prefix"

let test_malformed_and_cancel () =
  let input = source () in
  let expect label result = match result with
    | Error _ -> () | Ok _ -> fail ("Point Generate accepted " ^ label) in
  expect "negative total" (Point_generate.run
    ~mode:(Point_generate.Generate_total (-1)) input);
  expect "negative per-point count" (Point_generate.run
    ~mode:(Point_generate.Generate_per_point {
      points_per_point = -1.; scale_attribute = None }) input);
  expect "missing scale" (Point_generate.run
    ~mode:(Point_generate.Generate_per_point {
      points_per_point = 1.; scale_attribute = Some "missing" }) input);
  let bad_probability = Geometry.with_attribute
      (attribute Attribute.Point "probability"
        (Attribute.Float [|1.;nan;0.;0.|])) input |> Result.get_ok in
  expect "non-finite probability" (Point_generate.run
    ~mode:(Point_generate.Generate_probability { attribute = "probability" })
    bad_probability);
  let wrong = Geometry.find_group ~owner:Group.Primitive "face" input
      |> Option.get in
  expect "wrong selection owner" (Point_generate.run ~points:wrong
    ~mode:(Point_generate.Generate_total 1) input);
  expect "duplicate metadata names" (Point_generate.run
    ~source_point_attribute:"source" ~source_index_attribute:"source"
    ~mode:(Point_generate.Generate_total 1) input);
  expect "P metadata name" (Point_generate.run
    ~source_point_attribute:"P" ~mode:(Point_generate.Generate_total 1) input);
  expect "empty generated group" (Point_generate.run
    ~generated_group:" " ~mode:(Point_generate.Generate_total 1) input);
  let out_of_range = Geometry.with_attribute
      (attribute Attribute.Point "probability"
        (Attribute.Float [|1.;1.1;0.;0.|])) input |> Result.get_ok in
  expect "out-of-range probability" (Point_generate.run
    ~mode:(Point_generate.Generate_probability { attribute = "probability" })
    out_of_range);
  let negative_scale = Geometry.with_attribute
      (attribute Attribute.Point "weight"
        (Attribute.Float [|1.;-0.1;1.;1.|])) input |> Result.get_ok in
  expect "negative count scale" (Point_generate.run
    ~mode:(Point_generate.Generate_per_point {
      points_per_point = 1.; scale_attribute = Some "weight" }) negative_scale);
  let wrong_metadata = Geometry.with_attribute
      (attribute Attribute.Point "sourcepoint"
        (Attribute.Text [|"a";"b";"c";"d"|])) input |> Result.get_ok in
  expect "wrong metadata storage" (Point_generate.run
    ~mode:(Point_generate.Generate_total 1) wrong_metadata);
  expect "malformed point pattern" (Point_generate.run
    ~copy_point_attributes:"[" ~mode:(Point_generate.Generate_total 1) input);
  let cancel = Cancel.create () in Cancel.cancel cancel;
  (match Point_generate.run ~cancel
      ~mode:(Point_generate.Generate_per_point {
        points_per_point = 2.; scale_attribute = None }) input with
   | Error error when Error.code error = "cancelled" -> ()
   | Error error -> fail ("Point Generate cancel code: " ^ Error.to_string error)
   | Ok _ -> fail "Point Generate ignored cancellation")

let geometry_hash geometry =
  let hash = ref 17 in
  let add value = hash := Rays_math.Hash.mix !hash value in
  let positions = Packed.Float3.Private.view (Geometry.positions geometry) in
  Array.iter (fun value -> add (Hashtbl.hash value)) positions.x;
  Array.iter (fun value -> add (Hashtbl.hash value)) positions.y;
  Array.iter (fun value -> add (Hashtbl.hash value)) positions.z;
  Array.iter add (int_attr "sourcepoint" geometry);
  Array.iter add (int_attr "sourceindex" geometry);
  Array.iter (fun value -> add (Hashtbl.hash value)) (float_attr "density" geometry);
  !hash

let test_parallel_exact () =
  let count = 100_000 in
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:(Array.init count float_of_int) ~y:(Array.make count 0.)
      ~z:(Array.make count 0.) in
  let topology = Topology.empty ~point_count:count in
  let density = attribute Attribute.Point "density"
      (Attribute.Float (Array.init count (fun point ->
        float_of_int ((point mod 5) + 1) /. 5.))) in
  let input = Geometry.create ~positions ~topology ~attributes:[density] ()
      |> Result.get_ok in
  let cook domains = Parallel.run ~domains (fun () ->
    Point_generate.run ~grain:4096 ~seed:(Rays_math.Rand.seed 778)
      ~mode:(Point_generate.Generate_per_point {
        points_per_point = 10.; scale_attribute = Some "density" }) input
    |> get) in
  let one = cook 1 and four = cook 4 in
  check (Geometry.point_count one = 600_000)
    "Point Generate parallel fixture cardinality";
  check (geometry_hash one = geometry_hash four)
    "Point Generate one/four-domain output differs"

let run () =
  test_total ();
  test_per_point_and_probability ();
  test_keep_input_patterns_groups_and_edges ();
  test_detail_pattern_and_ragged ();
  test_existing_provenance ();
  test_malformed_and_cancel ();
  test_parallel_exact ();
  print_endline "point generate tests passed"
