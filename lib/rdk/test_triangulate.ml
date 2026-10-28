open Rays
open Rdk
open Rdk_test_support

let attribute owner name storage = Attribute.create_owned ~owner ~name storage |> get_string

let int_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> (
      match Attribute.storage attribute with
      | Attribute.Int values -> values
      | _ -> fail ("wrong integer storage for " ^ name))
  | None -> fail ("missing integer attribute " ^ name)

let equal_group left right =
  Group.owner left = Group.owner right
  && String.equal (Group.name left) (Group.name right)
  && Group.ordered_elements left = Group.ordered_elements right
  && Group.length left = Group.length right
  && Array.for_all
       (fun index -> Group.mem index left = Group.mem index right)
       (Array.init (Group.length left) Fun.id)

let group owner name geometry = Geometry.find_group ~owner name geometry |> Option.get

let test_collapsed_quad_and_deterministic_failure () =
  let collapsed_positions =
    Packed.Float3.View.of_owned_exn ~x:[| 0.; 1.; 1.; 0. |] ~y:[| 0.; 0.; 0.; 1. |]
      ~z:(Array.make 4 0.)
  in
  let collapsed_topology =
    Topology.polygons_owned ~point_count:4 ~vertex_points:[| 0; 1; 2; 3 |]
      ~primitive_offsets:[| 0; 4 |]
    |> get_string
  in
  let collapsed =
    Geometry.create ~positions:collapsed_positions ~topology:collapsed_topology
      ~attributes:[ attribute Attribute.Vertex "corner" (Attribute.Int [| 10; 11; 12; 13 |]) ]
      ()
    |> get_string |> Triangulate_front.run |> get_rdk
  in
  let collapsed_view = Topology.View.view (Geometry.topology collapsed) in
  check
    (collapsed_view.vertex_points = [| 0; 1; 3 |]
    && int_values Attribute.Vertex "corner" collapsed = [| 10; 11; 13 |])
    "Triangulate collapsed-edge quad policy";
  let degenerate_positions =
    Packed.Float3.View.of_owned_exn
      ~x:[| 0.; 1.; 2.; 3.; 4.; 5.; 6.; 7. |]
      ~y:(Array.make 8 0.) ~z:(Array.make 8 0.)
  in
  let degenerate_topology =
    Topology.polygons_owned ~point_count:8 ~vertex_points:[| 0; 1; 2; 3; 4; 5; 6; 7 |]
      ~primitive_offsets:[| 0; 4; 8 |]
    |> get_string
  in
  let degenerate =
    Geometry.create ~positions:degenerate_positions ~topology:degenerate_topology () |> get_string
  in
  let run domains =
    Parallel.run ~domains (fun () ->
        match Triangulate_front.run ~grain:1 degenerate with
        | Error error -> Error.message error
        | Ok _ -> fail "Triangulate accepted degenerate polygons")
  in
  let one = run 1 and four = run 4 in
  check
    (String.equal one four
    && String.equal one
         "Rdk_mesh.Triangulate.triangulate: primitive 0 has degenerate projected area")
    "Triangulate did not report the stable lowest failing primitive"

let run () =
  test_collapsed_quad_and_deterministic_failure ();
  print_endline "Triangulate tests passed"
