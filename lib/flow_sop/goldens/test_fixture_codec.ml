open Rdk

let get = function Ok value -> value | Error message -> failwith message

let geometry () =
  let topology = Topology.create_owned ~point_count:3 ~vertex_points:[|0;1;2;2;1|]
      ~primitive_offsets:[|0;3;5|] ~primitive_kinds:[|Topology.Polygon; Open_polyline|] |> get in
  let positions = Packed.Float3.of_owned ~x:[|0.; -0.; infinity|]
      ~y:[|1.; 2.; 3.|] ~z:[|4.; 5.; 6.|] |> get in
  let geometry = Geometry.create ~positions ~topology () |> get in
  let add owner name storage geometry =
    Geometry.with_attribute (Attribute.create_owned ~owner ~name storage |> get) geometry |> get in
  let geometry = List.fold_left (fun geometry owner ->
    let length = match owner with
      | Attribute.Point -> 3 | Vertex -> 5 | Primitive -> 2 | Detail -> 1 in
    let floats = Array.init length (fun i -> if i = 0 then -0. else float i) in
    let ints = Array.init length (fun i -> i - 2) in
    let offsets = Array.init (length + 1) Fun.id in
    let geometry = add owner "f" (Attribute.Float floats) geometry
      |> add owner "i" (Int ints)
      |> add owner "s" (Text (Array.make length "é🐈")) in
    let geometry = add owner "v2" (Float2 (Packed.Float2.of_owned ~x:floats ~y:floats |> get)) geometry
      |> add owner "v3" (Float3 (Packed.Float3.of_owned ~x:floats ~y:floats ~z:floats |> get))
      |> add owner "v4" (Float4 (Packed.Float4.of_owned ~x:floats ~y:floats ~z:floats ~w:floats |> get))
      |> add owner "ia" (Int_array (Packed.Int_array.create_owned ~offsets ~values:ints |> get))
      |> add owner "fa" (Float_array (Packed.Float_array.create_owned ~offsets ~values:floats |> get)) in
    geometry) geometry [Attribute.Point; Vertex; Primitive; Detail] in
  let geometry = List.fold_left (fun geometry owner ->
    let length = match owner with Group.Point -> 3 | Vertex -> 5 | Primitive -> 2 in
    let ordered = Group.ordered ~owner ~name:"ordered" ~length [|length - 1; 0|] |> get in
    let unordered = Group.init ~owner ~name:"unordered" length (fun i -> i mod 2 = 0) in
    Geometry.with_group unordered geometry |> get |> Geometry.with_group ordered |> get)
      geometry [Group.Point; Vertex; Primitive] in
  let index = Topology_index.create topology in
  let edge_groups = [Edge_group.init ~topology ~index ~name:"selected" (fun edge -> edge mod 2 = 0)] in
  Geometry.create ~positions ~topology ~attributes:(Geometry.attributes geometry)
      ~groups:(Geometry.groups geometry) ~edge_groups () |> get

let same left right = Rdk_test_support.geometry_bytes left = Rdk_test_support.geometry_bytes right
let expect_bad label action =
  match action () with
  | () -> failwith (label ^ " was accepted")
  | exception Failure _ -> ()

let () =
  let source = geometry () in
  let encoded = Fixture_codec.encode source in
  let rebuilt = Fixture_codec.decode encoded in
  if not (same source rebuilt) then failwith "geometry fixture round-trip changed bytes";
  let image = Sop.Image.of_owned_rgba8 ~width:2 ~height:1 (Bytes.of_string "\000\001\127\255\255\128\064\000")
      |> function Ok image -> image | Error error -> failwith (Diagnostic.error_to_string error) in
  let rebuilt_image = Fixture_codec.decode_image (Fixture_codec.encode_image image) in
  if Sop.Image.rgba8 rebuilt_image <> Some (Option.get (Sop.Image.rgba8 image)) then
    failwith "RGBA8 image fixture round-trip changed bytes";
  expect_bad "bad float bit string" (fun () ->
    ignore (Fixture_codec.decode "(geometry (positions (\"000000000000000g\") () ()) (topology 1 () (0) ()) (attributes) (groups) (edge_groups))"));
  expect_bad "invalid geometry" (fun () -> ignore (Fixture_codec.decode "(geometry)"))
