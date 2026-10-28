open Rays
open Rdk
open Rdk_test_support


let equal_storage left right =
  match Attribute.View.storage left, Attribute.View.storage right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.View.view left
      and right = Packed.Float2.View.view right in
      left.x = right.x && left.y = right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.View.view left
      and right = Packed.Float3.View.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
  | Attribute.Float4 left, Attribute.Float4 right ->
      let left = Packed.Float4.View.view left
      and right = Packed.Float4.View.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
      && left.w = right.w
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.View.view left
      and right = Packed.Int_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.View.view left
      and right = Packed.Float_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | _ -> false

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

let two_quads () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 1.; 2.; 0.; 1.; 2.|]
      ~y:[|0.; 0.; 0.; 1.; 1.; 1.|]
      ~z:[|0.; 0.; 0.; 0.; 0.; 0.|] in
  let topology = Topology.polygons_owned ~point_count:6
      ~vertex_points:[|0;1;4;3; 1;2;5;4|]
      ~primitive_offsets:[|0;4;8|] |> get in
  let cut = edge_group_of_pairs topology "cut" [|1, 4|] in
  let index = Topology_index.create topology in
  let all_edges = Edge_group.init ~grain:1 ~topology ~index ~name:"all_edges"
      (Fun.const true) in
  let point_order = Group.ordered ~owner:Group.Point ~name:"edge_points"
      ~length:6 [|4;1|] |> get
  and vertex_group = Group.init ~grain:1 ~owner:Group.Vertex
      ~name:"edge_corners" 8 (fun vertex -> Array.mem vertex [|1;2;4;7|])
  and primitive_group = Group.ordered ~owner:Group.Primitive ~name:"faces"
      ~length:2 [|1;0|] |> get in
  let point_rows = Packed.Int_array.create_owned
      ~offsets:[|0;1;2;3;4;5;6|] ~values:[|0;1;2;3;4;5|] |> get in
  Geometry.create ~positions ~topology
    ~attributes:[
      attribute Attribute.Point "weight"
        (Attribute.Float [|0.;10.;20.;30.;40.;50.|]);
      attribute Attribute.Point "id" (Attribute.Int [|0;1;2;3;4;5|]);
      attribute Attribute.Point "label"
        (Attribute.Text [|"p0";"p1";"p2";"p3";"p4";"p5"|]);
      attribute Attribute.Point "uv" (Attribute.Float2
        (Packed.Float2.of_owned ~x:[|0.;1.;2.;0.;1.;2.|]
           ~y:[|0.;0.;0.;1.;1.;1.|] |> get));
      attribute Attribute.Point "rows" (Attribute.Int_array point_rows);
      attribute Attribute.Vertex "corner"
        (Attribute.Float [|0.;1.;2.;3.;4.;5.;6.;7.|]);
      attribute Attribute.Primitive "material" (Attribute.Text [|"a";"b"|]);
      attribute Attribute.Detail "meta" (Attribute.Float4
        (Packed.Float4.of_owned ~x:[|1.|] ~y:[|2.|] ~z:[|3.|] ~w:[|4.|]
         |> get));
    ] ~groups:[point_order; vertex_group; primitive_group]
      ~edge_groups:[cut; all_edges] () |> get

let float_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry |> Option.get
      |> Attribute.View.storage with
  | Attribute.Float values -> values
  | _ -> fail (name ^ " is not float")

let int_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry |> Option.get
      |> Attribute.View.storage with
  | Attribute.Int values -> values
  | _ -> fail (name ^ " is not integer")

let edge_cardinality name geometry =
  Geometry.find_edge_group name geometry |> Option.get |> Edge_group.cardinality

let expect_code code = function
  | Error error when String.equal (Error.code error) code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, received %s: %s"
      code (Error.code error) (Error.to_string error))
  | Ok _ -> fail ("expected error " ^ code)

let run () =
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  print_endline "edge divide tests passed"
