open Rays
open Rdk
open Rdk_test_support

let get_ok = get

let geometry_owned positions edges values =
  let positions = Array.of_list positions and edge_count = Array.length edges in
  let point_count = Array.length positions in
  let x = Array.map (fun (x,_,_) -> x) positions
  and y = Array.map (fun (_,y,_) -> y) positions
  and z = Array.map (fun (_,_,z) -> z) positions in
  let vertex_points = Array.make (edge_count * 2) 0 in
  Array.iteri (fun edge (a,b) ->
    vertex_points.(edge * 2) <- a; vertex_points.(edge * 2 + 1) <- b) edges;
  let topology = Topology.create_owned ~point_count ~vertex_points
      ~primitive_offsets:(Array.init (edge_count + 1) (fun edge -> edge * 2))
      ~primitive_kinds:(Array.make edge_count Topology.Open_polyline) |> get_ok in
  let attributes = match values with
    | None -> []
    | Some values -> [Attribute.create_owned ~owner:Attribute.Point ~name:"value"
        (Attribute.Float values) |> get_ok] in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology ~attributes () |> get_ok

let fixture () = geometry_owned
    [0.,0.,0.;1.,0.,0.;0.,2.,0.;2.,0.,0.;1.,3.,0.;10.,0.,0.;14.,0.,0.]
    [|0,1;0,2;1,3;1,4;5,6|]
    (Some [|10.;2.;5.;7.;1.;20.;3.|])

let values name geometry = match Geometry.find_attribute
    ~owner:Attribute.Point name geometry with
  | Some attribute -> (match Attribute.View.storage attribute with
      | Attribute.Float values -> values | _ -> fail "wrong output storage")
  | None -> fail "missing output attribute"

let with_parent parents geometry =
  let attribute = Attribute.create_owned ~owner:Attribute.Point ~name:"parent"
      (Attribute.Int parents) |> get_ok in
  Geometry.with_attribute attribute geometry |> get_ok

let close_array left right = Array.length left = Array.length right
  && Array.for_all2 (fun a b -> abs_float (a -. b) < 1e-9) left right

let group owner name length predicate = Group.init ~grain:1 ~owner ~name length predicate

let expect_code code = function
  | Error error when Error.code error = code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, got %s: %s"
      code (Error.code error) (Error.to_string error))
  | Ok _ -> fail ("expected error " ^ code)

let equal_geometry left right =
  let lv = values "distance" left and rv = values "distance" right in
  lv = rv && Geometry.point_count left = Geometry.point_count right

let long_curve point_count =
  let positions = Array.init point_count (fun point ->
      float_of_int point *. 0.001, sin (float_of_int point *. 0.003) *. 0.02, 0.)
      |> Array.to_list in
  let edges = Array.init (point_count - 1) (fun edge -> edge, edge + 1) in
  geometry_owned positions edges None

let curve_geometry positions vertex_points primitive_offsets primitive_kinds
    attributes =
  let positions = Array.of_list positions in
  let point_count = Array.length positions in
  let x = Array.map (fun (x,_,_) -> x) positions
  and y = Array.map (fun (_,y,_) -> y) positions
  and z = Array.map (fun (_,_,z) -> z) positions in
  let topology = Topology.create_owned ~point_count ~vertex_points
      ~primitive_offsets ~primitive_kinds |> get_ok in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology ~attributes () |> get_ok

let curve_fixture () =
  let point_values = Attribute.create_owned ~owner:Attribute.Point ~name:"value"
      (Attribute.Float [|10.;2.;5.;20.;3.;7.|]) |> get_ok
  and vertex_values = Attribute.create_owned ~owner:Attribute.Vertex
      ~name:"vertex_value" (Attribute.Float [|5.;2.;10.;20.;3.;7.|]) |> get_ok in
  curve_geometry
    [0.,0.,0.;1.,0.,0.;3.,0.,0.;0.,2.,0.;2.,2.,0.;5.,2.,0.]
    [|2;1;0;3;4;5|] [|0;3;6|]
    [|Topology.Open_polyline;Topology.Open_polyline|]
    [point_values;vertex_values]

let many_curves curve_count curve_size =
  let point_count = curve_count * curve_size in
  let positions = Array.init point_count (fun point ->
      let curve = point / curve_size and local = point mod curve_size in
      float_of_int local *. 0.01, float_of_int curve *. 0.002, 0.)
      |> Array.to_list in
  curve_geometry positions (Array.init point_count Fun.id)
    (Array.init (curve_count + 1) (fun curve -> curve * curve_size))
    (Array.make curve_count Topology.Open_polyline) []

let run () =
  let cancelled = Cancel.create () in Cancel.cancel cancelled;
  let cancelled = Cancel.create () in Cancel.cancel cancelled;
  let cancelled = Cancel.create () in Cancel.cancel cancelled;
  print_endline "edge transport tests passed"
