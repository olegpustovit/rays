open Rays
open Rdk
open Rdk_test_support

let geometry positions add_primitives =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:(Array.map (fun (x, _, _) -> x) positions)
      ~y:(Array.map (fun (_, y, _) -> y) positions)
      ~z:(Array.map (fun (_, _, z) -> z) positions) in
  let topology = Rdk_test_support.Tb.create ~point_count:(Packed.Float3.length positions) () in
  add_primitives topology;
  Geometry.create ~positions ~topology:(Rdk_test_support.Tb.freeze topology) ()
  |> function Ok value -> value | Error message -> fail message

let two_quads () = geometry
    [|(0.,0.,0.); (1.,0.,0.); (2.,0.,0.);
      (0.,1.,0.); (1.,1.,0.); (2.,1.,0.)|]
    (fun topology ->
      Rdk_test_support.Tb.add_polygon topology [|0;1;4;3|];
      Rdk_test_support.Tb.add_polygon topology [|1;2;5;4|])

let open_curve () = geometry
    [|(0.,0.,0.); (1.,0.,0.); (2.,0.,0.); (3.,0.,0.)|]
    (fun topology -> Rdk_test_support.Tb.add_open_polyline topology [|0;1;2;3|])

let with_attribute owner name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage
      |> function Ok value -> value | Error message -> fail message in
  Geometry.with_attribute attribute geometry
  |> function Ok value -> value | Error message -> fail message

let rule owner pattern = {
  Group_ops.boundary_attribute_owner = owner;
  boundary_attribute_pattern = pattern;
}

let edge_group name geometry = Geometry.find_edge_group name geometry |> Option.get
let group owner name geometry = Geometry.find_group ~owner name geometry |> Option.get

let selected_edge geometry group a b =
  let index = Topology_index.create (Geometry.topology geometry) in
  match Topology_index.find_edge index ~a ~b with
  | Some edge -> Edge_group.mem edge group
  | None -> fail (Printf.sprintf "missing fixture edge %d-%d" a b)

let members group =
  let result = ref [] in
  Group.iter (fun element -> result := element :: !result) group;
  List.rev !result

let same_edges left right =
  Edge_group.length left = Edge_group.length right
  && begin
    let equal = ref true in
    for edge = 0 to Edge_group.length left - 1 do
      if Edge_group.mem edge left <> Edge_group.mem edge right then equal := false
    done;
    !equal
  end

let run () =
  print_endline "group attribute boundary tests passed"
