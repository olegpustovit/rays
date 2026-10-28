open Rays
open Rdk
open Rdk_test_support

let geometry positions curves =
  let packed = Packed.Float3.Builder.create (Array.length positions) in
  Array.iteri (fun point (x, y, z) -> Packed.Float3.Builder.set packed point x y z) positions;
  let topology = Topology.Builder.create ~point_count:(Array.length positions) () in
  Array.iter (Topology.Builder.add_open_polyline topology) curves;
  Geometry.create
    ~positions:(Packed.Float3.Builder.freeze packed)
    ~topology:(Topology.Builder.freeze topology)
    ()
  |> Result.get_ok

let edge_group name geometry =
  match Geometry.find_edge_group name geometry with
  | Some group -> group
  | None -> fail ("missing edge group " ^ name)

let edge index a b = Topology_index.find_edge index ~a ~b |> Option.get

let mem_pair geometry group a b =
  let index = Topology_index.create (Geometry.topology geometry) in
  Edge_group.mem (edge index a b) group

let expect_invalid operation message =
  match operation () with
  | Error error -> check (Error.code error = "invalid_edge_group") message
  | Ok _ -> fail (message ^ ": unexpectedly succeeded")

let star () =
  geometry
    [| (0., 0., 0.); (1., 0., 0.); (0., 1., 0.); (-1., 0., 0.); (4., 0., 0.); (5., 0., 0.) |]
    [| [| 0; 1 |]; [| 0; 2 |]; [| 0; 3 |]; [| 4; 5 |] |]

let run () = print_endline "group edge-angle tests passed"
