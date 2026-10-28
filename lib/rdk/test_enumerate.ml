open Rays
open Rdk
open Rdk_test_support

let get_ok = get

let add_attribute owner name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> get_ok in
  Geometry.with_attribute attribute geometry |> get_ok

let int_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int values -> values
       | _ -> fail (name ^ " is not integer"))
  | None -> fail ("missing " ^ name)

let text_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Text values -> values
       | _ -> fail (name ^ " is not text"))
  | None -> fail ("missing " ^ name)

let equal_output left right owner name =
  Packed.Float3.View.view (Geometry.positions left)
    = Packed.Float3.View.view (Geometry.positions right)
  && Topology.View.view (Geometry.topology left)
     = Topology.View.view (Geometry.topology right)
  && int_values owner name left = int_values owner name right

let fixture () =
  let geometry = Plane_generators.grid ~connectivity:Plane_generators.Grid_quads
      ~columns:2 ~rows:1 ~size:2. () |> get_rdk in
  geometry
  |> add_attribute Attribute.Point "piece"
       (Attribute.Int [|10; 10; 20; 10; 20; 20|])
  |> add_attribute Attribute.Vertex "corner_piece"
       (Attribute.Text [|"a"; "b"; "a"; "b"; "a"; "b"; "a"; "b"|])
  |> add_attribute Attribute.Primitive "face_piece"
       (Attribute.Int [|42; 42|])

let run () =
  let source = fixture () in
  let point_elements = Attribute_ops.enumerate ~grain:1 ~start:5 ~step:2
      ~piece_attribute:"piece"
      ~mode:Attribute_ops.Enumerate_piece_elements
      ~owner:Attribute.Point ~name:"local_index" source |> get_rdk in
  check (int_values Attribute.Point "local_index" point_elements
      = [|5; 7; 5; 9; 7; 9|])
    "point piece-element enumeration";
  check (Topology.data_id (Geometry.topology point_elements)
      = Topology.data_id (Geometry.topology source)
      && Packed.Float3.data_id (Geometry.positions point_elements)
         = Packed.Float3.data_id (Geometry.positions source)
      && Geometry.point_count point_elements = 6
      && Geometry.vertex_count point_elements = 8
      && Geometry.primitive_count point_elements = 2)
    "Enumerate changed topology, positions, or cardinality";

  let point_pieces = Attribute_ops.enumerate ~grain:1
      ~storage:(Attribute_ops.Text { prefix = "piece_" })
      ~piece_attribute:"piece" ~mode:Attribute_ops.Enumerate_pieces
      ~owner:Attribute.Point ~name:"piece_number" source |> get_rdk in
  check (text_values Attribute.Point "piece_number" point_pieces
      = [|"piece_0"; "piece_0"; "piece_1"; "piece_0"; "piece_1"; "piece_1"|])
    "stable first-occurrence piece numbering or text output";

  let corner_elements = Attribute_ops.enumerate ~grain:2
      ~piece_attribute:"corner_piece"
      ~mode:Attribute_ops.Enumerate_piece_elements
      ~owner:Attribute.Vertex ~name:"corner_local" source |> get_rdk in
  check (int_values Attribute.Vertex "corner_local" corner_elements
      = [|0; 0; 1; 1; 2; 2; 3; 3|])
    "vertex text-piece enumeration";
  let face_elements = Attribute_ops.enumerate ~grain:1
      ~piece_attribute:"face_piece"
      ~mode:Attribute_ops.Enumerate_piece_elements
      ~owner:Attribute.Primitive ~name:"face_local" source |> get_rdk
  and face_pieces = Attribute_ops.enumerate ~grain:1
      ~piece_attribute:"face_piece" ~mode:Attribute_ops.Enumerate_pieces
      ~owner:Attribute.Primitive ~name:"face_class" source |> get_rdk in
  check (int_values Attribute.Primitive "face_local" face_elements = [|0; 1|]
      && int_values Attribute.Primitive "face_class" face_pieces = [|0; 0|])
    "primitive piece modes";

  let selected = Group.init ~owner:Group.Point ~name:"selected" 6
      (fun point -> point = 0 || point = 2 || point = 3 || point = 5) in
  let existing = source |> add_attribute Attribute.Point "restricted"
      (Attribute.Int (Array.make 6 99)) in
  let restricted = Attribute_ops.enumerate ~grain:2 ~selection:selected
      ~start:10 ~piece_attribute:"piece"
      ~mode:Attribute_ops.Enumerate_piece_elements
      ~owner:Attribute.Point ~name:"restricted" existing |> get_rdk in
  check (int_values Attribute.Point "restricted" restricted
      = [|10; 99; 10; 11; 99; 11|])
    "piece enumeration did not restrict discovery/ranks or preserve outside values";

  let in_place = Attribute_ops.enumerate ~grain:1 ~piece_attribute:"piece"
      ~mode:Attribute_ops.Enumerate_pieces
      ~owner:Attribute.Point ~name:"piece" source |> get_rdk in
  check (int_values Attribute.Point "piece" in_place = [|0; 0; 1; 0; 1; 1|])
    "in-place piece enumeration did not snapshot source keys";

  let extremes = Line_geometry.points [|(0.,0.,0.); (1.,0.,0.); (2.,0.,0.); (3.,0.,0.)|]
      |> add_attribute Attribute.Point "piece"
           (Attribute.Int [|min_int; max_int; min_int; max_int|]) in
  let extremes = Attribute_ops.enumerate ~piece_attribute:"piece"
      ~mode:Attribute_ops.Enumerate_piece_elements
      ~owner:Attribute.Point ~name:"rank" extremes |> get_rdk in
  check (int_values Attribute.Point "rank" extremes = [|0; 0; 1; 1|])
    "integer piece table mishandled extreme keys";

  (match Attribute_ops.enumerate ~piece_attribute:"missing"
      ~owner:Attribute.Point ~name:"rank" source with
   | Error error -> check (Error.code error = "invalid_enumeration")
       "missing piece attribute diagnostic"
   | Ok _ -> fail "Enumerate accepted a missing piece attribute");
  let wrong_kind = source |> add_attribute Attribute.Point "float_piece"
      (Attribute.Float (Array.make 6 1.)) in
  (match Attribute_ops.enumerate ~piece_attribute:"float_piece"
      ~owner:Attribute.Point ~name:"rank" wrong_kind with
   | Error error -> check (Error.code error = "invalid_enumeration")
       "wrong-kind piece attribute diagnostic"
   | Ok _ -> fail "Enumerate accepted a float piece attribute");
  (match Attribute_ops.enumerate ~piece_attribute:"face_piece"
      ~owner:Attribute.Point ~name:"rank" source with
   | Error error -> check (Error.code error = "invalid_enumeration")
       "cross-owner piece attribute diagnostic"
   | Ok _ -> fail "Enumerate accepted a cross-owner piece attribute");
  (match Attribute_ops.enumerate ~start:max_int ~step:1
      ~piece_attribute:"piece" ~mode:Attribute_ops.Enumerate_piece_elements
      ~owner:Attribute.Point ~name:"overflow" source with
   | Error error -> check (Error.code error = "invalid_enumeration")
       "piece-local sequence overflow diagnostic"
   | Ok _ -> fail "Enumerate accepted an overflowing piece sequence");
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match Attribute_ops.enumerate ~cancel:cancelled ~piece_attribute:"piece"
      ~mode:Attribute_ops.Enumerate_pieces
      ~owner:Attribute.Point ~name:"cancelled" source with
   | Error error -> check (Error.code error = "cancelled")
       "piece enumeration cancellation diagnostic"
   | Ok _ -> fail "cancelled piece enumeration published geometry");

  let dense_count = 300_000 in
  let dense = Line_geometry.points (Array.init dense_count (fun point ->
      float_of_int point *. 0.001, 0., 0.))
      |> add_attribute Attribute.Point "piece"
           (Attribute.Int (Array.init dense_count (fun point ->
             (point * 31) mod 4093))) in
  let run domains = Parallel.run ~domains (fun () ->
      Attribute_ops.enumerate ~grain:2048 ~start:7 ~step:3
        ~piece_attribute:"piece"
        ~mode:Attribute_ops.Enumerate_piece_elements
        ~owner:Attribute.Point ~name:"local" dense |> get_rdk) in
  let one = run 1 and many = run 4 in
  check (equal_output one many Attribute.Point "local")
    "one-domain and four-domain piece enumeration differ";
  check (Array.length (int_values Attribute.Point "local" one) = dense_count)
    "piece enumeration scale cardinality";
  print_endline "Enumerate tests passed"
