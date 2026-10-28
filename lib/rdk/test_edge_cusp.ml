open Rays
open Rdk
open Rdk_test_support

let get_ok = function Ok value -> value | Error message -> fail message

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
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology () |> get_ok

let vertex_points geometry =
  (Topology.Private.view (Geometry.topology geometry)).vertex_points

let point_int name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry
      |> Option.get |> Attribute.Private.storage with
  | Attribute.Int values -> values
  | _ -> fail (name ^ " is not point integer")

let point_normal geometry =
  match Geometry.find_attribute ~owner:Attribute.Point "N" geometry
      |> Option.get |> Attribute.Private.storage with
  | Attribute.Float3 values -> Packed.Float3.Private.view values
  | _ -> fail "N is not point float3"

let edge_cardinality name geometry =
  Geometry.find_edge_group name geometry |> Option.get |> Edge_group.cardinality

let equal_attribute left right =
  Attribute.owner left = Attribute.owner right
  && Attribute.name left = Attribute.name right
  && match Attribute.Private.storage left, Attribute.Private.storage right with
     | Attribute.Float a, Attribute.Float b -> a = b
     | Attribute.Int a, Attribute.Int b -> a = b
     | Attribute.Text a, Attribute.Text b -> a = b
     | Attribute.Float3 a, Attribute.Float3 b ->
         let a = Packed.Float3.Private.view a
         and b = Packed.Float3.Private.view b in
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

let equal_geometry left right =
  let lp = Packed.Float3.Private.view (Geometry.positions left)
  and rp = Packed.Float3.Private.view (Geometry.positions right)
  and lt = Topology.Private.view (Geometry.topology left)
  and rt = Topology.Private.view (Geometry.topology right) in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.point_count = rt.point_count
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && List.equal equal_attribute (Geometry.attributes left)
       (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)
  && List.equal equal_edge_group (Geometry.edge_groups left)
       (Geometry.edge_groups right)

let expect_code code = function
  | Error error when Error.code error = code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, received %s: %s"
      code (Error.code error) (Error.to_string error))
  | Ok _ -> fail ("expected error " ^ code)

let decorated_patch () =
  let base = geometry_owned
      [0.,0.,0.; 1.,0.,0.; 1.,1.,0.; 0.,1.,0.]
      [|0;1;2; 0;2;3|] [|0;3;6|] in
  let topology = Geometry.topology base in
  let cusp = edge_group_of_pairs topology "cusp" [|0,2;2,3|]
  and diagonal = edge_group_of_pairs topology "diagonal" [|0,2|] in
  let marked = Group.ordered ~owner:Group.Point ~name:"marked"
      ~length:4 [|2;0|] |> get_ok in
  Geometry.create ~positions:(Geometry.positions base) ~topology
    ~attributes:[
      attribute Attribute.Point "id" (Attribute.Int [|10;11;12;13|]);
      attribute Attribute.Point "N" (Attribute.Float3
        (Packed.Float3.Private.of_owned_exn ~x:[|1.;1.;1.;1.|]
          ~y:[|0.;0.;0.;0.|] ~z:[|0.;0.;0.;0.|]));
      attribute Attribute.Vertex "corner"
        (Attribute.Float [|0.;1.;2.;3.;4.;5.|]);
      attribute Attribute.Vertex "N" (Attribute.Float3
        (Packed.Float3.Private.of_owned_exn
          ~x:[|0.;0.;0.;0.;0.;0.|] ~y:[|1.;1.;1.;1.;1.;1.|]
          ~z:[|0.;0.;0.;0.;0.;0.|]));
      attribute Attribute.Primitive "material" (Attribute.Text [|"a";"b"|]);
      attribute Attribute.Detail "meta" (Attribute.Int [|42|]);
    ] ~groups:[marked] ~edge_groups:[cusp;diagonal] () |> get_ok

let disconnected_patches count =
  let points = count * 4 and vertices = count * 6 in
  let x = Array.make points 0. and y = Array.make points 0.
  and z = Array.make points 0. and vertex_points = Array.make vertices 0 in
  for patch = 0 to count - 1 do
    let p = patch * 4 and v = patch * 6 in
    let ox = float_of_int (patch mod 200) *. 2.
    and oy = float_of_int (patch / 200) *. 2. in
    x.(p) <- ox; y.(p) <- oy;
    x.(p + 1) <- ox +. 1.; y.(p + 1) <- oy;
    x.(p + 2) <- ox +. 1.; y.(p + 2) <- oy +. 1.;
    x.(p + 3) <- ox; y.(p + 3) <- oy +. 1.;
    vertex_points.(v) <- p; vertex_points.(v + 1) <- p + 1;
    vertex_points.(v + 2) <- p + 2; vertex_points.(v + 3) <- p;
    vertex_points.(v + 4) <- p + 2; vertex_points.(v + 5) <- p + 3
  done;
  let topology = Topology.polygons_owned ~point_count:points ~vertex_points
      ~primitive_offsets:(Array.init (count * 2 + 1) (fun face -> face * 3))
      |> get_ok in
  let positions = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
  let geometry = Geometry.create ~positions ~topology () |> get_ok in
  let index = Topology_index.create topology in
  let cusp = Edge_group.init ~grain:127 ~topology ~index ~name:"cusp"
      (fun edge ->
        let a, b = Topology_index.edge_points index edge in
        let a = min a b and b = max a b and local = a mod 4 in
        (b - a = 2 && local = 0) || (b - a = 1 && local = 2)) in
  geometry, cusp

let run () =
  let source = decorated_patch () in
  let _cusp = Geometry.find_edge_group "cusp" source |> Option.get
  and _diagonal = Geometry.find_edge_group "diagonal" source |> Option.get in
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  let _large, _large_edges = disconnected_patches 10_000 in
  print_endline "edge cusp tests passed"
