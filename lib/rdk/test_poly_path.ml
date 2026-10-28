open Rays
open Rdk
open Rdk_test_support

let get_ok = function Ok value -> value | Error message -> fail message

let equal_storage left right =
  match Attribute.Private.storage left, Attribute.Private.storage right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.Private.view left
      and right = Packed.Float2.Private.view right in
      left.x = right.x && left.y = right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.Private.view left
      and right = Packed.Float3.Private.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
  | Attribute.Float4 left, Attribute.Float4 right ->
      let left = Packed.Float4.Private.view left
      and right = Packed.Float4.Private.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
      && left.w = right.w
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.Private.view left
      and right = Packed.Int_array.Private.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.Private.view left
      and right = Packed.Float_array.Private.view right in
      left.offsets = right.offsets && left.values = right.values
  | _ -> false

let equal_geometry left right =
  let left_positions = Packed.Float3.Private.view (Geometry.positions left)
  and right_positions = Packed.Float3.Private.view (Geometry.positions right)
  and left_topology = Topology.Private.view (Geometry.topology left)
  and right_topology = Topology.Private.view (Geometry.topology right) in
  left_positions.x = right_positions.x
  && left_positions.y = right_positions.y
  && left_positions.z = right_positions.z
  && left_topology.point_count = right_topology.point_count
  && left_topology.vertex_points = right_topology.vertex_points
  && left_topology.primitive_offsets = right_topology.primitive_offsets
  && Bytes.equal left_topology.primitive_kinds right_topology.primitive_kinds
  && List.equal (fun left right ->
       Attribute.owner left = Attribute.owner right
       && String.equal (Attribute.name left) (Attribute.name right)
       && equal_storage left right)
       (Geometry.attributes left) (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)
  && List.equal equal_edge_group
       (Geometry.edge_groups left) (Geometry.edge_groups right)

let add_attribute owner name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> get_ok in
  Geometry.with_attribute attribute geometry |> get_ok

let graph_fixture () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.; 1.; 2.; 3.; 2.; 2.; 10.; 11.; 10.5|]
      ~y:[|0.; 0.; 0.; 0.; 1.; -1.; 0.; 0.; 1.|]
      ~z:(Array.make 9 0.) in
  let vertex_points = [|
    0;1;2;3;
    2;4;
    2;5;
    0;1;
    5;5;
    6;7;8
  |] in
  let topology = Topology.create_owned ~point_count:9 ~vertex_points
      ~primitive_offsets:[|0;4;6;8;10;12;15|]
      ~primitive_kinds:[|Topology.Open_polyline; Topology.Open_polyline;
        Topology.Open_polyline; Topology.Open_polyline;
        Topology.Open_polyline; Topology.Polygon|] |> get_ok in
  let geometry = Geometry.create ~positions ~topology () |> get_ok in
  let geometry = geometry
      |> add_attribute Attribute.Point "point_float"
           (Attribute.Float (Array.init 9 float_of_int))
      |> add_attribute Attribute.Point "point_vector"
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
             ~x:(Array.init 9 float_of_int) ~y:(Array.make 9 2.)
             ~z:(Array.make 9 3.)))
      |> add_attribute Attribute.Point "point_quaternion"
           (Attribute.Float4 (Packed.Float4.of_owned
             ~x:(Array.init 9 float_of_int) ~y:(Array.make 9 0.)
             ~z:(Array.make 9 0.) ~w:(Array.make 9 1.) |> get_ok))
      |> add_attribute Attribute.Point "point_array"
           (Attribute.Int_array (Packed.Int_array.create_owned
             ~offsets:(Array.init 10 Fun.id) ~values:(Array.init 9 Fun.id)
             |> get_ok))
      |> add_attribute Attribute.Vertex "vertex_id"
           (Attribute.Int (Array.init 15 Fun.id))
      |> add_attribute Attribute.Vertex "vertex_weight"
           (Attribute.Float (Array.init 15 float_of_int))
      |> add_attribute Attribute.Vertex "vertex_name"
           (Attribute.Text (Array.init 15 (Printf.sprintf "v%d")))
      |> add_attribute Attribute.Vertex "vertex_uv"
           (Attribute.Float2 (Packed.Float2.of_owned
             ~x:(Array.init 15 float_of_int) ~y:(Array.make 15 0.) |> get_ok))
      |> add_attribute Attribute.Vertex "vertex_vector"
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
             ~x:(Array.init 15 float_of_int) ~y:(Array.make 15 1.)
             ~z:(Array.make 15 2.)))
      |> add_attribute Attribute.Vertex "vertex_quaternion"
           (Attribute.Float4 (Packed.Float4.of_owned
             ~x:(Array.init 15 float_of_int) ~y:(Array.make 15 0.)
             ~z:(Array.make 15 0.) ~w:(Array.make 15 1.) |> get_ok))
      |> add_attribute Attribute.Vertex "vertex_int_array"
           (Attribute.Int_array (Packed.Int_array.create_owned
             ~offsets:(Array.init 16 Fun.id) ~values:(Array.init 15 Fun.id)
             |> get_ok))
      |> add_attribute Attribute.Vertex "vertex_array"
           (Attribute.Float_array (Packed.Float_array.create_owned
             ~offsets:(Array.init 16 Fun.id)
             ~values:(Array.init 15 float_of_int) |> get_ok))
      |> add_attribute Attribute.Primitive "primitive_id"
           (Attribute.Int [|10;20;30;40;50;60|])
      |> add_attribute Attribute.Primitive "primitive_weight"
           (Attribute.Float [|1.;2.;3.;4.;5.;6.|])
      |> add_attribute Attribute.Primitive "primitive_name"
           (Attribute.Text [|"trunk";"up";"down";"duplicate";"self";"loop"|])
      |> add_attribute Attribute.Primitive "primitive_uv"
           (Attribute.Float2 (Packed.Float2.of_owned
             ~x:[|1.;2.;3.;4.;5.;6.|] ~y:(Array.make 6 0.) |> get_ok))
      |> add_attribute Attribute.Primitive "primitive_vector"
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
             ~x:[|1.;2.;3.;4.;5.;6.|] ~y:(Array.make 6 1.)
             ~z:(Array.make 6 2.)))
      |> add_attribute Attribute.Primitive "primitive_quaternion"
           (Attribute.Float4 (Packed.Float4.of_owned
             ~x:[|1.;2.;3.;4.;5.;6.|] ~y:(Array.make 6 0.)
             ~z:(Array.make 6 0.) ~w:(Array.make 6 1.) |> get_ok))
      |> add_attribute Attribute.Primitive "primitive_int_array"
           (Attribute.Int_array (Packed.Int_array.create_owned
             ~offsets:(Array.init 7 Fun.id) ~values:(Array.init 6 Fun.id)
             |> get_ok))
      |> add_attribute Attribute.Primitive "primitive_float_array"
           (Attribute.Float_array (Packed.Float_array.create_owned
             ~offsets:(Array.init 7 Fun.id)
             ~values:(Array.init 6 float_of_int) |> get_ok))
      |> add_attribute Attribute.Detail "tag" (Attribute.Text [|"fixture"|]) in
  let points = Group.init ~owner:Group.Point ~name:"even_points" 9
      (fun point -> point land 1 = 0)
  and vertices = Group.init ~owner:Group.Vertex ~name:"even_vertices" 15
      (fun vertex -> vertex land 1 = 0)
  and duplicate = Group.init ~owner:Group.Primitive ~name:"duplicate_source" 6
      (fun primitive -> primitive = 3) in
  let geometry = geometry |> Geometry.with_group points |> get_ok
      |> Geometry.with_group vertices |> get_ok
      |> Geometry.with_group duplicate |> get_ok in
  let index = Topology_index.create topology in
  let trunk = Edge_group.init ~topology ~index ~name:"trunk" (fun edge ->
      let a, b = Topology_index.edge_points index edge in
      (a = 0 && b = 1) || (a = 1 && b = 2)) in
  Geometry.with_edge_group trunk geometry |> get_ok

let attribute owner name geometry =
  Geometry.find_attribute ~owner name geometry |> Option.get

let endpoint_fixture () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.; 1.; 1.04; 2.; 5.; 6.|]
      ~y:[|0.; 0.; 0.; 0.; 0.02; 1.|] ~z:(Array.make 6 0.) in
  let topology = Topology.create_owned ~point_count:6
      ~vertex_points:[|0;1; 2;3; 4;5|]
      ~primitive_offsets:[|0;2;4;6|]
      ~primitive_kinds:(Array.make 3 Topology.Open_polyline) |> get_ok in
  Geometry.create ~positions ~topology () |> get_ok

let scale_fixture columns rows =
  let point_count = (columns + 1) * (rows + 1) in
  let point x y = y * (columns + 1) + x in
  let primitive_count = rows * (columns + 1) + columns * (rows + 1) in
  let vertex_points = Array.make (primitive_count * 2) 0
  and primitive_offsets = Array.init (primitive_count + 1) (fun p -> p * 2) in
  let primitive = ref 0 in
  for x = 0 to columns do
    for y = 0 to rows - 1 do
      vertex_points.(!primitive * 2) <- point x y;
      vertex_points.(!primitive * 2 + 1) <- point x (y + 1);
      incr primitive
    done
  done;
  for y = 0 to rows do
    for x = 0 to columns - 1 do
      vertex_points.(!primitive * 2) <- point x y;
      vertex_points.(!primitive * 2 + 1) <- point (x + 1) y;
      incr primitive
    done
  done;
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:(Array.init point_count (fun p -> float_of_int (p mod (columns + 1))))
      ~y:(Array.init point_count (fun p -> float_of_int (p / (columns + 1))))
      ~z:(Array.make point_count 0.) in
  let topology = Topology.create_owned ~point_count ~vertex_points
      ~primitive_offsets
      ~primitive_kinds:(Array.make primitive_count Topology.Open_polyline)
      |> get_ok in
  let geometry = Geometry.create ~positions ~topology () |> get_ok
      |> add_attribute Attribute.Point "id"
           (Attribute.Int (Array.init point_count Fun.id))
      |> add_attribute Attribute.Vertex "corner"
           (Attribute.Float (Array.init (primitive_count * 2) float_of_int))
      |> add_attribute Attribute.Primitive "source"
           (Attribute.Int (Array.init primitive_count Fun.id)) in
  let group = Group.init ~owner:Group.Primitive ~name:"alternating"
      primitive_count (fun p -> p land 1 = 0) in
  let geometry = Geometry.with_group group geometry |> get_ok in
  let index = Topology_index.create topology in
  let edges = Edge_group.init ~topology ~index ~name:"alternating_edges"
      (fun edge -> edge land 1 = 0) in
  Geometry.with_edge_group edges geometry |> get_ok

let run () =
  print_endline "poly path tests passed"
