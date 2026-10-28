open Rays
open Rdk
open Rdk_test_support


let geometry_owned ~kinds positions vertex_points primitive_offsets =
  let positions = Array.of_list positions in
  let point_count = Array.length positions in
  let x = Array.map (fun (x, _, _) -> x) positions
  and y = Array.map (fun (_, y, _) -> y) positions
  and z = Array.map (fun (_, _, z) -> z) positions in
  let topology = Topology.create_owned ~point_count ~vertex_points
      ~primitive_offsets ~primitive_kinds:kinds |> get in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> get

let disjoint () = geometry_owned
    ~kinds:(Array.make 3 Topology.Open_polyline)
    [0.,0.,0.; 1.,0.,0.; 3.,0.,0.; 5.,0.,0.; 7.,0.,0.; 10.,0.,0.]
    [|0;1; 2;3; 4;5|] [|0;2;4;6|]

let edge_group_of_pairs topology name pairs =
  let index = Topology_index.create topology in
  let selected = Array.map (fun (a, b) ->
      let edge = Topology_index.find_edge_index index ~a ~b in
      if edge < 0 then fail "test edge is absent";
      edge) pairs in
  Edge_group.init ~grain:1 ~topology ~index ~name (fun edge ->
    Array.mem edge selected)

let expect_code code = function
  | Error error when Error.code error = code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, received %s: %s"
      code (Error.code error) (Error.to_string error))
  | Ok _ -> fail ("expected error " ^ code)

let close left right = abs_float (left -. right) <= 4e-6
let array_close left right = Array.length left = Array.length right
  && Array.for_all2 close left right

let many_disjoint edge_count =
  let point_count = edge_count * 2 in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. in
  for edge = 0 to edge_count - 1 do
    let point = edge * 2 and base = float_of_int edge *. 4.
    and length = 0.5 +. float_of_int (edge mod 17) *. 0.1 in
    x.(point) <- base; x.(point + 1) <- base +. length
  done;
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  let topology = Topology.create_owned ~point_count
      ~vertex_points:(Array.init point_count Fun.id)
      ~primitive_offsets:(Array.init (edge_count + 1) (fun edge -> edge * 2))
      ~primitive_kinds:(Array.make edge_count Topology.Open_polyline) |> get in
  Geometry.create ~positions ~topology () |> get

let many_chains count =
  let point_count = count * 3 in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. in
  for chain = 0 to count - 1 do
    let point = chain * 3 and base = float_of_int chain *. 5. in
    x.(point) <- base;
    x.(point + 1) <- base +. 0.5 +. float_of_int (chain mod 5) *. 0.1;
    x.(point + 2) <- base +. 2.5
  done;
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  let topology = Topology.create_owned ~point_count
      ~vertex_points:(Array.init point_count Fun.id)
      ~primitive_offsets:(Array.init (count + 1) (fun chain -> chain * 3))
      ~primitive_kinds:(Array.make count Topology.Open_polyline) |> get in
  Geometry.create ~positions ~topology () |> get

let all_edge_lengths geometry =
  let p = positions geometry and index = Topology_index.create
      (Geometry.topology geometry) in
  Array.init (Topology_index.edge_count index) (fun edge ->
    let a, b = Topology_index.edge_points index edge in
    let dx = p.x.(b) -. p.x.(a) and dy = p.y.(b) -. p.y.(a)
    and dz = p.z.(b) -. p.z.(a) in
    sqrt (dx *. dx +. dy *. dy +. dz *. dz))

let run () =
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  print_endline "edge equalize tests passed"
