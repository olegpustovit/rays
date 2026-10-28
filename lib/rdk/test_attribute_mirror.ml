open Rdk_test_support
open Rdk


let points xs =
  let count = Array.length xs in
  Geometry.create
    ~positions:(Packed.Float3.Private.of_owned_exn ~x:(Array.copy xs)
      ~y:(Array.make count 0.) ~z:(Array.make count 0.))
    ~topology:(Topology.empty ~point_count:count) () |> Result.get_ok

let add ~owner ~name storage geometry =
  Attribute.create_owned ~owner ~name storage |> Result.get_ok
  |> fun attribute -> Geometry.with_attribute attribute geometry |> Result.get_ok

let group owner name length members =
  let builder = Group.Builder.create ~owner ~name length in
  List.iter (fun member -> Group.Builder.set builder member true) members;
  Group.Builder.freeze builder

let scalar owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> (match Attribute.Private.storage attribute with
      | Attribute.Float values -> values | _ -> fail ("wrong scalar " ^ name))
  | None -> fail ("missing scalar " ^ name)

let integer owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> (match Attribute.Private.storage attribute with
      | Attribute.Int values -> values | _ -> fail ("wrong integer " ^ name))
  | None -> fail ("missing integer " ^ name)

let text owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> (match Attribute.Private.storage attribute with
      | Attribute.Text values -> values | _ -> fail ("wrong text " ^ name))
  | None -> fail ("missing text " ^ name)

let point_fixture () =
  let count = 4 in
  let destination = group Group.Point "destination" count [2;3] in
  points [|-2.;-1.;1.;2.|]
  |> add ~owner:Attribute.Point ~name:"map" (Attribute.Int [|-1;-1;1;0|])
  |> add ~owner:Attribute.Point ~name:"value"
      (Attribute.Float [|10.;20.;0.;0.|])
  |> add ~owner:Attribute.Point ~name:"id" (Attribute.Int [|10;20;0;0|])
  |> add ~owner:Attribute.Point ~name:"label"
      (Attribute.Text [|"left_A";"left_B";"old";"old"|])
  |> add ~owner:Attribute.Point ~name:"uv" (Attribute.Float2
      (Packed.Float2.of_owned ~x:[|-0.2;-0.4;0.;0.|]
        ~y:[|0.1;0.2;0.;0.|] |> Result.get_ok))
  |> add ~owner:Attribute.Point ~name:"Cd" (Attribute.Float4
      (Packed.Float4.of_owned ~x:[|1.;0.5;0.;0.|] ~y:[|0.;0.2;0.;0.|]
        ~z:[|0.1;0.3;0.;0.|] ~w:[|1.;1.;0.;0.|] |> Result.get_ok))
  |> add ~owner:Attribute.Point ~name:"rows" (Attribute.Int_array
      (Packed.Int_array.create_owned ~offsets:[|0;1;3;3;3|]
        ~values:[|7;8;9|] |> Result.get_ok))
  |> Geometry.with_group destination |> Result.get_ok

let test_explicit_mapping_all_storage () =
  let source = point_fixture () in
  let destination = Geometry.find_group ~owner:Group.Point "destination" source
      |> Option.get in
  let output = Attribute_mirror.run ~owner:Attribute_mirror.Mirror_point_attributes
      ~method_:(Attribute_mirror.Mirror_by_mapping {
        mapping_attribute = "map"; destination_group = destination })
      ~attributes:"value id label uv Cd rows" ~string_replace:("left", "right")
      ~transform:(Attribute_mirror.Mirror_uv { origin_u = 0.; origin_v = 0.;
        direction_u = 0.; direction_v = 1. })
      ~output_mapping:"mirror_pair" ~source_group:"mirror_source"
      ~destination_group:"mirror_destination" source |> get_ok in
  check (scalar Attribute.Point "value" output = [|10.;20.;20.;10.|]
      && integer Attribute.Point "id" output = [|10;20;20;10|]
      && text Attribute.Point "label" output
        = [|"left_A";"left_B";"right_B";"right_A"|])
    "Attribute Mirror explicit scalar/discrete mapping";
  let uv = Geometry.find_attribute ~owner:Attribute.Point "uv" output
      |> Option.get |> Attribute.Private.storage |> function
    | Attribute.Float2 values -> Packed.Float2.Private.view values
    | _ -> assert false in
  check (uv.x = [|-0.2;-0.4;0.4;0.2|]
      && uv.y = [|0.1;0.2;0.2;0.1|])
    "Attribute Mirror UV reflection";
  let rows = Geometry.find_attribute ~owner:Attribute.Point "rows" output
      |> Option.get |> Attribute.Private.storage |> function
    | Attribute.Int_array values -> Packed.Int_array.Private.view values
    | _ -> assert false in
  check (rows.offsets = [|0;1;3;5;6|] && rows.values = [|7;8;9;8;9;7|])
    "Attribute Mirror ragged remap";
  check (integer Attribute.Point "mirror_pair" output = [|0;1;1;0|])
    "Attribute Mirror output mapping";
  let source_group = Geometry.find_group ~owner:Group.Point "mirror_source" output
      |> Option.get
  and destination_group = Geometry.find_group ~owner:Group.Point
      "mirror_destination" output |> Option.get in
  check (Group.cardinality source_group = 2 && Group.mem 0 source_group
      && Group.mem 1 source_group && Group.cardinality destination_group = 2
      && Group.mem 2 destination_group && Group.mem 3 destination_group)
    "Attribute Mirror output side groups";
  check (Geometry.topology output == Geometry.topology source)
    "Attribute Mirror rebuilt topology"

let test_plane_points_and_transforms () =
  let geometry = points [|-2.;-1.;0.9;2.1|]
      |> add ~owner:Attribute.Point ~name:"value"
        (Attribute.Float [|10.;20.;0.;0.|])
      |> add ~owner:Attribute.Point ~name:"N" (Attribute.Float3
        (Packed.Float3.Private.of_owned_exn ~x:[|1.;2.;0.;0.|]
          ~y:[|0.;0.;0.;0.|] ~z:[|0.;0.;0.;0.|]))
      |> add ~owner:Attribute.Point ~name:"rest" (Attribute.Float3
        (Packed.Float3.Private.of_owned_exn ~x:[|-2.;-1.;0.;0.|]
          ~y:[|1.;2.;0.;0.|] ~z:[|0.;0.;0.;0.|])) in
  let method_ = Attribute_mirror.Mirror_by_plane { origin = Rays_math.Vec3.zero;
    normal = Rays_math.Vec3.unit_x; distance = 0.; tolerance = 0.11 } in
  let copied = Attribute_mirror.run ~owner:Attribute_mirror.Mirror_point_attributes ~method_
      ~attributes:"value" geometry |> get_ok in
  check (scalar Attribute.Point "value" copied = [|10.;20.;20.;10.|])
    "Attribute Mirror plane point correspondence";
  let vector = Attribute_mirror.run ~owner:Attribute_mirror.Mirror_point_attributes ~method_
      ~attributes:"N" ~transform:Attribute_mirror.Mirror_vector geometry |> get_ok in
  let normal = Geometry.find_attribute ~owner:Attribute.Point "N" vector
      |> Option.get |> Attribute.Private.storage |> function
    | Attribute.Float3 values -> Packed.Float3.Private.view values
    | _ -> assert false in
  check (normal.x = [|1.;2.;-2.;-1.|])
    "Attribute Mirror vector transformation";
  let points_output = Attribute_mirror.run ~owner:Attribute_mirror.Mirror_point_attributes
      ~method_ ~attributes:"P rest" ~transform:Attribute_mirror.Mirror_point geometry
    |> get_ok in
  let positions = Packed.Float3.Private.view (Geometry.positions points_output)
  and rest = Geometry.find_attribute ~owner:Attribute.Point "rest" points_output
      |> Option.get |> Attribute.Private.storage |> function
    | Attribute.Float3 values -> Packed.Float3.Private.view values
    | _ -> assert false in
  check (positions.x = [|-2.;-1.;1.;2.|]
      && rest.x = [|-2.;-1.;1.;2.|])
    "Attribute Mirror point transformation"

let paired_triangles () =
  let topology = Rdk_test_support.Tb.create ~point_count:6 () in
  Rdk_test_support.Tb.add_triangle topology 0 1 2;
  Rdk_test_support.Tb.add_triangle topology 3 4 5;
  let geometry = Geometry.create
      ~positions:(Packed.Float3.Private.of_owned_exn
        ~x:[|-2.;-1.;-1.;1.;1.;2.|]
        ~y:[|0.;-1.;1.;-1.;1.;0.|] ~z:[|0.;0.;0.;0.;0.;0.|])
      ~topology:(Rdk_test_support.Tb.freeze topology) () |> Result.get_ok in
  geometry
  |> add ~owner:Attribute.Vertex ~name:"vertex_map"
      (Attribute.Int [|-1;-1;-1;2;1;0|])
  |> add ~owner:Attribute.Vertex ~name:"v"
      (Attribute.Float [|1.;2.;3.;0.;0.;0.|])
  |> add ~owner:Attribute.Primitive ~name:"primitive_map"
      (Attribute.Int [|-1;0|])
  |> add ~owner:Attribute.Primitive ~name:"p" (Attribute.Float [|9.;0.|])
  |> Geometry.with_group (group Group.Vertex "vertex_dest" 6 [3;4;5])
  |> Result.get_ok
  |> Geometry.with_group (group Group.Primitive "primitive_dest" 2 [1])
  |> Result.get_ok

let test_vertex_primitive_and_plane_primitive () =
  let geometry = paired_triangles () in
  let vertex_dest = Geometry.find_group ~owner:Group.Vertex "vertex_dest" geometry
      |> Option.get
  and primitive_dest = Geometry.find_group ~owner:Group.Primitive
      "primitive_dest" geometry |> Option.get in
  let vertices = Attribute_mirror.run ~owner:Attribute_mirror.Mirror_vertex_attributes
      ~method_:(Attribute_mirror.Mirror_by_mapping { mapping_attribute = "vertex_map";
        destination_group = vertex_dest }) ~attributes:"v" geometry |> get_ok in
  check (scalar Attribute.Vertex "v" vertices = [|1.;2.;3.;3.;2.;1.|])
    "Attribute Mirror vertex explicit mapping";
  let primitives = Attribute_mirror.run ~owner:Attribute_mirror.Mirror_primitive_attributes
      ~method_:(Attribute_mirror.Mirror_by_mapping { mapping_attribute = "primitive_map";
        destination_group = primitive_dest }) ~attributes:"p" geometry |> get_ok in
  check (scalar Attribute.Primitive "p" primitives = [|9.;9.|])
    "Attribute Mirror primitive explicit mapping";
  let plane = Attribute_mirror.run ~owner:Attribute_mirror.Mirror_primitive_attributes
      ~method_:(Attribute_mirror.Mirror_by_plane { origin = Rays_math.Vec3.zero;
        normal = Rays_math.Vec3.unit_x; distance = 0.; tolerance = 1e-12 })
      ~attributes:"p" geometry |> get_ok in
  check (scalar Attribute.Primitive "p" plane = [|9.;9.|])
    "Attribute Mirror primitive bounding-box correspondence"

let test_group_policies_and_noop () =
  let geometry = point_fixture () in
  let destination = Geometry.find_group ~owner:Group.Point "destination" geometry
      |> Option.get in
  let method_ = Attribute_mirror.Mirror_by_mapping { mapping_attribute = "map";
    destination_group = destination } in
  let source_selection = group Group.Point "only_source" 4 [0] in
  let source = Attribute_mirror.run ~group:source_selection
      ~group_use:Attribute_mirror.Mirror_group_as_source
      ~owner:Attribute_mirror.Mirror_point_attributes ~method_ ~attributes:"value" geometry
    |> get_ok in
  check (scalar Attribute.Point "value" source = [|10.;20.;0.;10.|])
    "Attribute Mirror source restriction";
  let destination_selection = group Group.Point "only_destination" 4 [2] in
  let destination = Attribute_mirror.run ~group:destination_selection
      ~group_use:Attribute_mirror.Mirror_group_as_destination
      ~owner:Attribute_mirror.Mirror_point_attributes ~method_ ~attributes:"value" geometry
    |> get_ok in
  check (scalar Attribute.Point "value" destination = [|10.;20.;20.;0.|])
    "Attribute Mirror destination restriction";
  let empty = group Group.Point "empty" 4 [] in
  let unchanged = Attribute_mirror.run ~group:empty
      ~group_use:Attribute_mirror.Mirror_group_as_destination
      ~owner:Attribute_mirror.Mirror_point_attributes ~method_ ~attributes:"*" geometry
    |> get_ok in
  check (unchanged == geometry)
    "Attribute Mirror empty correspondence did not preserve identity"

let expect_error ?(code = "invalid_attribute_mirror") work =
  match work () with
  | Error error -> check (Error.code error = code)
      "Attribute Mirror wrong structured error"
  | Ok _ -> fail "Attribute Mirror accepted malformed input"

let test_errors_and_cancellation () =
  let geometry = point_fixture () in
  let destination = Geometry.find_group ~owner:Group.Point "destination" geometry
      |> Option.get in
  let mapping = Attribute_mirror.Mirror_by_mapping { mapping_attribute = "map";
    destination_group = destination } in
  expect_error (fun () -> Attribute_mirror.run ~grain:0
    ~owner:Attribute_mirror.Mirror_point_attributes ~method_:mapping geometry);
  expect_error (fun () -> Attribute_mirror.run ~attributes:"["
    ~owner:Attribute_mirror.Mirror_point_attributes ~method_:mapping geometry);
  expect_error (fun () -> Attribute_mirror.run ~transform:Attribute_mirror.Mirror_vector
    ~owner:Attribute_mirror.Mirror_point_attributes ~method_:mapping geometry);
  expect_error (fun () -> Attribute_mirror.run ~string_replace:("", "x")
    ~owner:Attribute_mirror.Mirror_point_attributes ~method_:mapping geometry);
  expect_error (fun () -> Attribute_mirror.run
    ~owner:Attribute_mirror.Mirror_vertex_attributes
    ~method_:(Attribute_mirror.Mirror_by_plane { origin = Rays_math.Vec3.zero;
      normal = Rays_math.Vec3.unit_x; distance = 0.; tolerance = 1. }) geometry);
  expect_error (fun () -> Attribute_mirror.run
    ~owner:Attribute_mirror.Mirror_point_attributes
    ~method_:(Attribute_mirror.Mirror_by_plane { origin = Rays_math.Vec3.zero;
      normal = Rays_math.Vec3.zero; distance = 0.; tolerance = 1. }) geometry);
  expect_error (fun () -> Attribute_mirror.run
    ~owner:Attribute_mirror.Mirror_point_attributes
    ~method_:(Attribute_mirror.Mirror_by_plane { origin = Rays_math.Vec3.zero;
      normal = Rays_math.Vec3.unit_x; distance = 0.; tolerance = -1. }) geometry);
  expect_error (fun () -> Attribute_mirror.run
    ~transform:(Attribute_mirror.Mirror_uv { origin_u = 0.; origin_v = 0.;
      direction_u = 0.; direction_v = 0. })
    ~owner:Attribute_mirror.Mirror_point_attributes ~method_:mapping geometry);
  expect_error (fun () -> Attribute_mirror.run ~source_group:"same"
    ~destination_group:"same" ~owner:Attribute_mirror.Mirror_point_attributes
    ~method_:mapping geometry);
  let wrong_group = group Group.Primitive "wrong" 0 [] in
  expect_error (fun () -> Attribute_mirror.run ~group:wrong_group
    ~owner:Attribute_mirror.Mirror_point_attributes ~method_:mapping geometry);
  let bad = geometry |> add ~owner:Attribute.Point ~name:"value"
      (Attribute.Float [|1.;nan;0.;0.|]) in
  expect_error (fun () -> Attribute_mirror.run ~attributes:"value"
    ~owner:Attribute_mirror.Mirror_point_attributes ~method_:mapping bad);
  let wrong_mapping = geometry |> add ~owner:Attribute.Point ~name:"map"
      (Attribute.Float [|-1.;-1.;1.;0.|]) in
  expect_error (fun () -> Attribute_mirror.run
    ~owner:Attribute_mirror.Mirror_point_attributes ~method_:(Attribute_mirror.Mirror_by_mapping {
      mapping_attribute = "map"; destination_group = destination }) wrong_mapping);
  let cancel = Cancel.create () in Cancel.cancel cancel;
  expect_error ~code:"cancelled" (fun () -> Attribute_mirror.run ~cancel
    ~owner:Attribute_mirror.Mirror_point_attributes ~method_:mapping geometry)

let test_parallel_scale () =
  let count = 100_000 and half = 50_000 in
  let geometry = points (Array.init count (fun point ->
      if point < half then -.float_of_int (half - point)
      else float_of_int (point - half + 1)))
    |> add ~owner:Attribute.Point ~name:"map"
      (Attribute.Int (Array.init count (fun point ->
        if point < half then -1 else count - point - 1)))
    |> add ~owner:Attribute.Point ~name:"Cd" (Attribute.Float4
      (Packed.Float4.of_owned
        ~x:(Array.init count (fun point -> float_of_int (point mod 251)))
        ~y:(Array.init count (fun point -> float_of_int (point mod 127)))
        ~z:(Array.init count (fun point -> float_of_int (point mod 67)))
        ~w:(Array.make count 1.) |> Result.get_ok))
    |> Geometry.with_group (group Group.Point "dest" count
      (List.init half (fun point -> point + half))) |> Result.get_ok in
  let destination = Geometry.find_group ~owner:Group.Point "dest" geometry
      |> Option.get in
  let run domains = Parallel.run ~domains (fun () ->
    Attribute_mirror.run ~grain:127 ~owner:Attribute_mirror.Mirror_point_attributes
      ~method_:(Attribute_mirror.Mirror_by_mapping { mapping_attribute = "map";
        destination_group = destination }) ~attributes:"Cd"
      ~output_mapping:"pair" geometry |> get_ok) in
  let one = run 1 and four = run 4 in
  let color geometry = Geometry.find_attribute ~owner:Attribute.Point "Cd" geometry
      |> Option.get |> Attribute.Private.storage |> function
    | Attribute.Float4 values -> Packed.Float4.Private.view values
    | _ -> assert false in
  check (color one = color four
      && integer Attribute.Point "pair" one = integer Attribute.Point "pair" four)
    "Attribute Mirror differs across domain counts";
  check (Geometry.point_count one = count
      && Geometry.topology one == Geometry.topology geometry)
    "Attribute Mirror scale cardinality/topology sharing"

let run () =
  test_explicit_mapping_all_storage ();
  test_plane_points_and_transforms ();
  test_vertex_primitive_and_plane_primitive ();
  test_group_policies_and_noop ();
  test_errors_and_cancellation ();
  test_parallel_scale ();
  print_endline "Attribute Mirror tests passed"
