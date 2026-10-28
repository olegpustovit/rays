open Rays
open Rdk
open Rdk_test_support

let get_ok = get

let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> get_ok

let edge_group_of_pairs topology name pairs =
  let index = Topology_index.create topology in
  let selected = Array.map (fun (a, b) ->
      let edge = Topology_index.find_edge_index index ~a ~b in
      if edge < 0 then fail "test edge is absent";
      edge) pairs in
  Edge_group.init ~grain:1 ~topology ~index ~name (fun edge ->
    Array.mem edge selected)

let geometry_owned positions vertex_points primitive_offsets =
  let positions = Array.of_list positions in
  let point_count = Array.length positions in
  let x = Array.map (fun (x, _, _) -> x) positions
  and y = Array.map (fun (_, y, _) -> y) positions
  and z = Array.map (fun (_, _, z) -> z) positions in
  let topology = Topology.polygons_owned ~point_count ~vertex_points
      ~primitive_offsets |> get_ok in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> get_ok

let vertex_points geometry =
  (Topology.View.view (Geometry.topology geometry)).vertex_points

let vertex_float name geometry =
  match Geometry.find_attribute ~owner:Attribute.Vertex name geometry
      |> Option.get |> Attribute.View.storage with
  | Attribute.Float values -> values
  | _ -> fail (name ^ " is not vertex float")

let edge_group_pairs name geometry =
  let group = Geometry.find_edge_group name geometry |> Option.get
  and index = Topology_index.create (Geometry.topology geometry) in
  let result = ref [] in
  Edge_group.iter (fun edge ->
    let a, b = Topology_index.edge_points index edge in
    result := (min a b, max a b) :: !result) group;
  List.sort compare !result

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
     | Attribute.Int_array a, Attribute.Int_array b ->
         let a = Packed.Int_array.View.view a
         and b = Packed.Int_array.View.view b in
         a.offsets = b.offsets && a.values = b.values
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

let equal_geometry left right =
  let lp = Packed.Float3.View.view (Geometry.positions left)
  and rp = Packed.Float3.View.view (Geometry.positions right)
  and lt = Topology.View.view (Geometry.topology left)
  and rt = Topology.View.view (Geometry.topology right) in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.point_count = rt.point_count
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && List.equal equal_attribute (Geometry.attributes left)
       (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)
  && List.map Edge_group.name (Geometry.edge_groups left)
       = List.map Edge_group.name (Geometry.edge_groups right)
  && List.for_all (fun group ->
       edge_group_pairs (Edge_group.name group) left
       = edge_group_pairs (Edge_group.name group) right)
       (Geometry.edge_groups left)

let expect_code code = function
  | Error error when Error.code error = code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, received %s: %s"
      code (Error.code error) (Error.to_string error))
  | Ok _ -> fail ("expected error " ^ code)

let decorated_quad () =
  let base = geometry_owned
      [0.,0.,0.; 1.,0.,0.; 1.,1.,0.; 0.,1.,0.]
      [|0;1;2; 0;2;3|] [|0;3;6|] in
  let topology = Geometry.topology base in
  let flip = edge_group_of_pairs topology "flip" [|0,2|]
  and border = edge_group_of_pairs topology "border" [|0,1;1,2|] in
  let corner = Group.ordered ~owner:Group.Vertex ~name:"corner"
      ~length:6 [|0;2;4|] |> get_ok in
  let rows = Packed.Int_array.create_owned ~offsets:[|0;1;3;3;4;6;7|]
      ~values:[|0;1;2;3;4;5;6|] |> get_ok in
  Geometry.create ~positions:(Geometry.positions base) ~topology
    ~attributes:[
      attribute Attribute.Point "id" (Attribute.Int [|10;11;12;13|]);
      attribute Attribute.Point "N" (Attribute.Float3
        (Packed.Float3.View.of_owned_exn ~x:[|1.;1.;1.;1.|]
          ~y:[|0.;0.;0.;0.|] ~z:[|0.;0.;0.;0.|]));
      attribute Attribute.Vertex "uv" (Attribute.Float
        [|10.;20.;30.;40.;50.;60.|]);
      attribute Attribute.Vertex "rows" (Attribute.Int_array rows);
      attribute Attribute.Vertex "N" (Attribute.Float
        [|1.;1.;1.;1.;1.;1.|]);
      attribute Attribute.Primitive "material" (Attribute.Text [|"a";"b"|]);
      attribute Attribute.Detail "meta" (Attribute.Int [|42|]);
    ] ~groups:[corner] ~edge_groups:[flip;border] () |> get_ok

let disconnected_triangle_pairs count =
  let points = count * 4 and vertices = count * 6 in
  let x = Array.make points 0. and y = Array.make points 0.
  and z = Array.make points 0. and vertex_points = Array.make vertices 0
  and offsets = Array.init (count * 2 + 1) (fun i -> i * 3) in
  for pair = 0 to count - 1 do
    let p = pair * 4 and v = pair * 6 in
    let origin = float_of_int (pair mod 100) *. 2. in
    x.(p) <- origin; x.(p + 1) <- origin +. 1.;
    x.(p + 2) <- origin +. 1.; x.(p + 3) <- origin;
    y.(p + 2) <- 1.; y.(p + 3) <- 1.;
    vertex_points.(v) <- p; vertex_points.(v + 1) <- p + 1;
    vertex_points.(v + 2) <- p + 2; vertex_points.(v + 3) <- p;
    vertex_points.(v + 4) <- p + 2; vertex_points.(v + 5) <- p + 3
  done;
  let topology = Topology.polygons_owned ~point_count:points ~vertex_points
      ~primitive_offsets:offsets |> get_ok in
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  let geometry = Geometry.create ~positions ~topology () |> get_ok in
  let index = Topology_index.create topology in
  let edges = Edge_group.init ~grain:127 ~topology ~index ~name:"flip"
      (fun edge ->
        let a, b = Topology_index.edge_points index edge in
        abs (a - b) = 2 && min a b mod 4 = 0) in
  geometry, edges

let run () =
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  let _large, _large_edges = disconnected_triangle_pairs 5_000 in
  print_endline "edge flip tests passed"
