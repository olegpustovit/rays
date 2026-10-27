open Rays
open Rdk
open Rdk_test_support

let with_attribute owner name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let make_curves positions primitive_points =
  let point_count = Array.length positions in
  let primitive_count = Array.length primitive_points in
  let primitive_offsets = Array.make (primitive_count + 1) 0 in
  for primitive = 0 to primitive_count - 1 do
    primitive_offsets.(primitive + 1) <- primitive_offsets.(primitive)
      + Array.length primitive_points.(primitive)
  done;
  let vertex_points = Array.make primitive_offsets.(primitive_count) 0 in
  Array.iteri (fun primitive points ->
    Array.blit points 0 vertex_points primitive_offsets.(primitive)
      (Array.length points)) primitive_points;
  let topology = Topology.create_owned ~point_count ~vertex_points
      ~primitive_offsets
      ~primitive_kinds:(Array.make primitive_count Topology.Open_polyline)
      |> Result.get_ok in
  let x = Array.map (fun (x, _, _) -> x) positions
  and y = Array.map (fun (_, y, _) -> y) positions
  and z = Array.map (fun (_, _, z) -> z) positions in
  Geometry.create
    ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology () |> Result.get_ok

let position_view geometry =
  Packed.Float3.Private.view (Geometry.positions geometry)

let float3_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.Private.view values
       | _ -> fail (name ^ " is not float3"))
  | None -> fail (name ^ " is missing")

let equal_storage left right = match left, right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      Packed.Float2.Private.view left = Packed.Float2.Private.view right
  | Attribute.Float3 left, Attribute.Float3 right ->
      Packed.Float3.Private.view left = Packed.Float3.Private.view right
  | Attribute.Float4 left, Attribute.Float4 right ->
      Packed.Float4.Private.view left = Packed.Float4.Private.view right
  | Attribute.Int_array left, Attribute.Int_array right ->
      Packed.Int_array.Private.view left = Packed.Int_array.Private.view right
  | Attribute.Float_array left, Attribute.Float_array right ->
      Packed.Float_array.Private.view left = Packed.Float_array.Private.view right
  | _ -> false

let equal_geometry left right =
  position_view left = position_view right
  && Topology.Private.view (Geometry.topology left)
     = Topology.Private.view (Geometry.topology right)
  && List.equal (fun left right ->
      Attribute.owner left = Attribute.owner right
      && Attribute.name left = Attribute.name right
      && equal_storage (Attribute.Private.storage left)
           (Attribute.Private.storage right))
      (Geometry.attributes left) (Geometry.attributes right)
  && List.equal (fun left right ->
      Group.owner left = Group.owner right
      && Group.name left = Group.name right
      && Group.ordered_elements left = Group.ordered_elements right)
      (Geometry.groups left) (Geometry.groups right)
  && List.equal (fun left right ->
      Edge_group.name left = Edge_group.name right
      && Array.init (Edge_group.length left) (fun edge -> Edge_group.mem edge left)
         = Array.init (Edge_group.length right) (fun edge -> Edge_group.mem edge right))
      (Geometry.edge_groups left) (Geometry.edge_groups right)

let test_primitive_integer_and_move_back () =
  let source = make_curves
      [|(0.,0.,0.);(2.,0.,0.);(-1.,1.,0.);(0.,1.,0.);
        (10.,2.,0.);(11.,2.,0.)|]
      [|[|0;1|];[|2;3|];[|4;5|]|]
      |> with_attribute Attribute.Primitive "piece" (Attribute.Int [|7;3;7|])
      |> with_attribute Attribute.Point "weight"
           (Attribute.Float [|0.;1.;2.;3.;4.;5.|]) in
  let output = Separate_pieces.run ~grain:1 ~gap:1.
      ~mode:Separate_pieces.Separate_pieces_separate ~piece_attribute:"piece" source
      |> get_ok in
  let positions = position_view output in
  check (positions.x = [|0.;2.;12.;13.;10.;11.|])
    "Separate Pieces primitive packing positions";
  let translation = float3_values Attribute.Primitive "piece_translation"
      output in
  check (translation.x = [|0.;13.;0.|]
      && translation.y = [|0.;0.;0.|]
      && translation.z = [|0.;0.;0.|])
    "Separate Pieces primitive translation field";
  check (Geometry.topology output == Geometry.topology source)
    "Separate Pieces copied unchanged topology";
  let source_weight = Geometry.find_attribute ~owner:Attribute.Point "weight" source
      |> Option.get
  and output_weight = Geometry.find_attribute ~owner:Attribute.Point "weight" output
      |> Option.get in
  check (Attribute.storage_id source_weight = Attribute.storage_id output_weight)
    "Separate Pieces copied unchanged payload";
  let restored = Separate_pieces.run ~grain:1
      ~mode:Separate_pieces.Separate_pieces_move_back ~piece_attribute:"piece" output
      |> get_ok in
  check (position_view restored = position_view source)
    "Separate Pieces Move Back did not restore positions";
  check (float3_values Attribute.Primitive "piece_translation" restored
      = translation) "Separate Pieces Move Back changed translation metadata"

let test_text_axis_and_point_owner () =
  let source = make_curves
      [|(0.,0.,0.);(1.,2.,0.);(3.,-2.,0.);(4.,0.,0.)|]
      [|[|0;1|];[|2;3|]|]
      |> with_attribute Attribute.Primitive "name"
           (Attribute.Text [|"left";"right"|]) in
  let output = Separate_pieces.run ~grain:1 ~axis:Rays_math.Vec3.unit_y ~gap:2.
      ~mode:Separate_pieces.Separate_pieces_separate ~piece_attribute:"name" source
      |> get_ok in
  let positions = position_view output in
  check (positions.y = [|0.;2.;4.;6.|])
    "Separate Pieces text/Y-axis packing";
  let point_source = make_curves
      [|(0.,0.,0.);(1.,0.,0.);(-2.,1.,0.);(-1.,1.,0.)|]
      [|[|0;1|];[|2;3|]|]
      |> with_attribute Attribute.Point "island"
           (Attribute.Text [|"a";"a";"b";"b"|]) in
  let point_output = Separate_pieces.run ~grain:1 ~owner:Attribute.Point
      ~gap:0.5 ~mode:Separate_pieces.Separate_pieces_separate
      ~piece_attribute:"island" point_source |> get_ok in
  check ((position_view point_output).x = [|0.;1.;1.5;2.5|])
    "Separate Pieces point-owned packing";
  check ((float3_values Attribute.Point "piece_translation" point_output).x
      = [|0.;0.;3.5;3.5|])
    "Separate Pieces point-owned translation field"

let expect_invalid work message = match work () with
  | Error error ->
      check (Error.code error = "invalid_separate_pieces")
        (message ^ ": wrong code")
  | Ok _ -> fail (message ^ ": unexpectedly accepted")

let test_validation_and_cancellation () =
  let source = make_curves [|(0.,0.,0.);(1.,0.,0.);(2.,0.,0.)|]
      [|[|0;1|];[|1;2|]|]
      |> with_attribute Attribute.Primitive "piece" (Attribute.Int [|0;1|]) in
  expect_invalid (fun () -> Separate_pieces.run
      ~mode:Separate_pieces.Separate_pieces_separate ~piece_attribute:"piece" source)
    "shared point across primitive pieces";
  let mixed_points = make_curves [|(0.,0.,0.);(1.,0.,0.)|] [|[|0;1|]|]
      |> with_attribute Attribute.Point "piece" (Attribute.Int [|0;1|]) in
  expect_invalid (fun () -> Separate_pieces.run ~owner:Attribute.Point
      ~mode:Separate_pieces.Separate_pieces_separate ~piece_attribute:"piece" mixed_points)
    "mixed point pieces in one primitive";
  let valid = make_curves [|(0.,0.,0.);(1.,0.,0.)|] [|[|0;1|]|]
      |> with_attribute Attribute.Primitive "piece" (Attribute.Int [|0|]) in
  expect_invalid (fun () -> Separate_pieces.run ~grain:0
      ~mode:Separate_pieces.Separate_pieces_separate ~piece_attribute:"piece" valid)
    "zero grain";
  expect_invalid (fun () -> Separate_pieces.run ~gap:(-1.)
      ~mode:Separate_pieces.Separate_pieces_separate ~piece_attribute:"piece" valid)
    "negative gap";
  expect_invalid (fun () -> Separate_pieces.run ~axis:Rays_math.Vec3.zero
      ~mode:Separate_pieces.Separate_pieces_separate ~piece_attribute:"piece" valid)
    "zero axis";
  expect_invalid (fun () -> Separate_pieces.run ~owner:Attribute.Vertex
      ~mode:Separate_pieces.Separate_pieces_separate ~piece_attribute:"piece" valid)
    "unsupported owner";
  expect_invalid (fun () -> Separate_pieces.run
      ~mode:Separate_pieces.Separate_pieces_separate ~piece_attribute:"missing" valid)
    "missing piece field";
  let wrong_storage = valid
      |> with_attribute Attribute.Primitive "piece" (Attribute.Float [|0.|]) in
  expect_invalid (fun () -> Separate_pieces.run
      ~mode:Separate_pieces.Separate_pieces_separate ~piece_attribute:"piece" wrong_storage)
    "wrong piece storage";
  let malformed = make_curves [|(Float.nan,0.,0.);(1.,0.,0.)|] [|[|0;1|]|]
      |> with_attribute Attribute.Primitive "piece" (Attribute.Int [|0|]) in
  expect_invalid (fun () -> Separate_pieces.run
      ~mode:Separate_pieces.Separate_pieces_separate ~piece_attribute:"piece" malformed)
    "non-finite position";
  let unrepresentable_gap = make_curves
      [|(max_float,0.,0.);(max_float,1.,0.)|] [||]
      |> with_attribute Attribute.Point "piece" (Attribute.Int [|0;1|]) in
  expect_invalid (fun () -> Separate_pieces.run ~owner:Attribute.Point
      ~mode:Separate_pieces.Separate_pieces_separate ~piece_attribute:"piece"
      unrepresentable_gap) "unrepresentable positive gap";
  let single_extreme = make_curves [|(max_float,0.,0.)|] [||]
      |> with_attribute Attribute.Point "piece" (Attribute.Int [|0|]) in
  check ((position_view (Separate_pieces.run ~owner:Attribute.Point
      ~mode:Separate_pieces.Separate_pieces_separate ~piece_attribute:"piece"
      single_extreme |> get_ok)).x = [|max_float|])
    "Separate Pieces moved a single extreme piece";
  let bad_translation = valid
      |> with_attribute Attribute.Primitive "piece_translation"
           (Attribute.Float [|1.|]) in
  expect_invalid (fun () -> Separate_pieces.run
      ~mode:Separate_pieces.Separate_pieces_move_back ~piece_attribute:"piece"
      bad_translation) "wrong translation storage";
  let conflict_translation = source
      |> with_attribute Attribute.Primitive "piece_translation"
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
             ~x:[|0.;1.|] ~y:[|0.;0.|] ~z:[|0.;0.|])) in
  expect_invalid (fun () -> Separate_pieces.run
      ~mode:Separate_pieces.Separate_pieces_move_back ~piece_attribute:"piece"
      conflict_translation) "conflicting shared-point translations";
  let opaque_free_point = make_curves
      [|(0.,0.,0.);(1.,0.,0.);(Float.nan,0.,0.)|] [|[|0;1|]|]
      |> with_attribute Attribute.Primitive "piece" (Attribute.Int [|0|])
      |> with_attribute Attribute.Primitive "piece_translation"
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
             ~x:[|1.|] ~y:[|0.|] ~z:[|0.|])) in
  let opaque_free_point = Separate_pieces.run
      ~mode:Separate_pieces.Separate_pieces_move_back ~piece_attribute:"piece"
      opaque_free_point |> get_ok |> position_view in
  check (opaque_free_point.x.(0) = -1. && opaque_free_point.x.(1) = 0.
      && Float.is_nan opaque_free_point.x.(2))
    "Separate Pieces inspected or changed an unowned free point";
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  (match Separate_pieces.run ~cancel ~mode:Separate_pieces.Separate_pieces_separate
      ~piece_attribute:"piece" valid with
   | Error error -> check (Error.code error = "cancelled")
       "Separate Pieces cancellation code"
   | Ok _ -> fail "cancelled Separate Pieces published geometry")

let test_dense_parallel_exactness () =
  let pieces = 600 and points_per_piece = 241 in
  let point_count = pieces * points_per_piece in
  let positions = Array.init point_count (fun point ->
      let local = point mod points_per_piece in
      float_of_int local *. 0.01,
      sin (float_of_int local *. 0.03),
      float_of_int (point / points_per_piece mod 5) *. 0.02) in
  let primitives = Array.init pieces (fun piece ->
      Array.init points_per_piece (fun local -> piece * points_per_piece + local)) in
  let source = make_curves positions primitives
      |> with_attribute Attribute.Primitive "piece"
           (Attribute.Int (Array.init pieces (fun piece -> piece mod 431)))
      |> with_attribute Attribute.Point "id"
           (Attribute.Int (Array.init point_count Fun.id)) in
  let cook domains = Parallel.run ~domains (fun () ->
      Separate_pieces.run ~grain:257 ~axis:(Rays_math.Vec3.create 1. 2. 3.) ~gap:0.01
        ~mode:Separate_pieces.Separate_pieces_separate ~piece_attribute:"piece" source
      |> get_ok) in
  let one = cook 1 and four = cook 4 in
  check (equal_geometry one four)
    "Separate Pieces one/four-domain geometry differs";
  let one_mesh = Rdk_rays.Rays_mesh.to_mesh one |> get_ok
  and four_mesh = Rdk_rays.Rays_mesh.to_mesh four |> get_ok in
  check (Mesh.Private.packed_view one_mesh = Mesh.Private.packed_view four_mesh)
    "Separate Pieces one/four-domain render mesh differs"

let run () =
  test_primitive_integer_and_move_back ();
  test_text_axis_and_point_owner ();
  test_validation_and_cancellation ();
  test_dense_parallel_exactness ();
  print_endline "rdk Separate Pieces tests passed"
