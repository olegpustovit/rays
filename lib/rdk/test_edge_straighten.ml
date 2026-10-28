open Rays
open Rdk
open Rdk_test_support


let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> get

let edge_group_of_pairs topology name pairs =
  let index = Topology_index.create topology in
  let selected = Array.map (fun (a, b) ->
      let edge = Topology_index.find_edge_index index ~a ~b in
      if edge < 0 then fail "test edge is absent";
      edge) pairs in
  Edge_group.init ~grain:1 ~topology ~index ~name (fun edge ->
    Array.mem edge selected)

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

let equal_attribute left right =
  Attribute.owner left = Attribute.owner right
  && Attribute.name left = Attribute.name right
  && match Attribute.View.storage left, Attribute.View.storage right with
     | Attribute.Float a, Attribute.Float b -> a = b
     | Attribute.Int a, Attribute.Int b -> a = b
     | Attribute.Text a, Attribute.Text b -> a = b
     | Attribute.Float3 a, Attribute.Float3 b ->
         let a = Packed.Float3.View.view a
         and b = Packed.Float3.View.view b in
         a.x = b.x && a.y = b.y && a.z = b.z
     | _ -> false

let equal_group left right =
  Group.owner left = Group.owner right && Group.name left = Group.name right
  && Group.length left = Group.length right
  && Group.ordered_elements left = Group.ordered_elements right
  && let equal = ref true in
     for i = 0 to Group.length left - 1 do
       if Group.mem i left <> Group.mem i right then equal := false
     done;
     !equal

let equal_edge_group left right =
  Edge_group.name left = Edge_group.name right
  && Edge_group.length left = Edge_group.length right
  && let equal = ref true in
     for edge = 0 to Edge_group.length left - 1 do
       if Edge_group.mem edge left <> Edge_group.mem edge right then
         equal := false
     done;
     !equal

let expect_code code = function
  | Error error when Error.code error = code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, received %s: %s"
      code (Error.code error) (Error.to_string error))
  | Ok _ -> fail ("expected error " ^ code)

let decorated_bend () =
  let base = Line_geometry_front.polyline [|-1.,0.,0.; 0.,1.,0.; 1.,0.,0.|] |> get_rdk in
  let topology = Geometry.topology base in
  let bend = edge_group_of_pairs topology "bend" [|0,1;1,2|]
  and first = edge_group_of_pairs topology "first" [|0,1|] in
  let ordered = Group.ordered ~owner:Group.Point ~name:"ordered"
      ~length:3 [|2;0|] |> get in
  Geometry.create ~positions:(Geometry.positions base) ~topology
    ~attributes:[
      attribute Attribute.Point "id" (Attribute.Int [|10;11;12|]);
      attribute Attribute.Point "N" (Attribute.Float3
        (Packed.Float3.View.of_owned_exn ~x:[|0.;0.;0.|]
          ~y:[|0.;0.;0.|] ~z:[|1.;1.;1.|]));
      attribute Attribute.Vertex "corner" (Attribute.Float [|3.;4.;5.|]);
      attribute Attribute.Vertex "N" (Attribute.Float3
        (Packed.Float3.View.of_owned_exn ~x:[|0.;0.;0.|]
          ~y:[|0.;0.;0.|] ~z:[|1.;1.;1.|]));
      attribute Attribute.Primitive "material" (Attribute.Text [|"wire"|]);
      attribute Attribute.Detail "meta" (Attribute.Int [|42|]);
    ] ~groups:[ordered] ~edge_groups:[bend;first] () |> get

let many_bends count =
  let point_count = count * 3 in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. in
  for component = 0 to count - 1 do
    let point = component * 3
    and ox = float_of_int (component mod 500) *. 4.
    and oy = float_of_int (component / 500) *. 3. in
    x.(point) <- ox -. 1.; y.(point) <- oy;
    x.(point + 1) <- ox; y.(point + 1) <- oy +. 1.;
    x.(point + 2) <- ox +. 1.; y.(point + 2) <- oy
  done;
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  let topology = Topology.create_owned ~point_count
      ~vertex_points:(Array.init point_count Fun.id)
      ~primitive_offsets:(Array.init (count + 1) (fun primitive -> primitive * 3))
      ~primitive_kinds:(Array.make count Topology.Open_polyline) |> get in
  Geometry.create ~positions ~topology
    ~attributes:[attribute Attribute.Point "id"
      (Attribute.Int (Array.init point_count Fun.id))] () |> get

let run () =
  let source = decorated_bend () in
  let _bend = Geometry.find_edge_group "bend" source |> Option.get
  and _first = Geometry.find_edge_group "first" source |> Option.get in
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  print_endline "edge straighten tests passed"
