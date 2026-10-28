open Rays
open Rdk
open Rdk_test_support

let get_ok = get

let disjoint lengths =
  let edge_count = Array.length lengths and point_count = Array.length lengths * 2 in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. in
  Array.iteri (fun edge length ->
    let point = edge * 2 and base = float_of_int edge *. 5. in
    x.(point) <- base; x.(point + 1) <- base +. length) lengths;
  let topology = Topology.create_owned ~point_count
      ~vertex_points:(Array.init point_count Fun.id)
      ~primitive_offsets:(Array.init (edge_count + 1) (fun edge -> edge * 2))
      ~primitive_kinds:(Array.make edge_count Topology.Open_polyline) |> get_ok in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology () |> get_ok

let edge_lengths geometry =
  let p = positions geometry and index = Topology_index.create
      (Geometry.topology geometry) in
  Array.init (Topology_index.edge_count index) (fun edge ->
    let a, b = Topology_index.edge_points index edge in
    let dx = p.x.(b) -. p.x.(a) and dy = p.y.(b) -. p.y.(a)
    and dz = p.z.(b) -. p.z.(a) in
    sqrt (dx *. dx +. dy *. dy +. dz *. dz))

let close left right = abs_float (left -. right) < 1e-5
let array_close left right = Array.for_all2 close left right

let group owner name length predicate = Group.init ~grain:1 ~owner ~name length predicate

let expect_code code = function
  | Error error when Error.code error = code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, received %s: %s"
      code (Error.code error) (Error.to_string error))
  | Ok _ -> fail ("expected error " ^ code)

let equal_geometry left right =
  let left = positions left and right = positions right in
  left.x = right.x && left.y = right.y && left.z = right.z

let run () =
  let _source = disjoint [|1.;2.;3.|]
  and _reference = disjoint [|2.;1.;4.|] in
  let _shorten_source = disjoint [|1.;3.|]
  and _shorten_reference = disjoint [|2.;2.|] in
  let _zero = disjoint [|0.;1.|] and _positive = disjoint [|1.;1.|] in
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  let _large_source = disjoint (Array.init 50_000 (fun edge ->
      0.5 +. float_of_int (edge mod 17) *. 0.1))
  and _large_reference = disjoint (Array.init 50_000 (fun edge ->
      0.8 +. float_of_int (edge mod 11) *. 0.1)) in
  let _connected_source = Line_geometry.polyline
      [|0.,0.,0.;1.,0.4,0.;3.,-0.2,0.;6.,0.5,0.|] |> get_rdk
  and _connected_reference = Line_geometry.polyline
      [|0.,0.,0.;2.,0.,0.;3.,0.,0.;7.,0.,0.|] |> get_rdk in
  print_endline "edge relax tests passed"
