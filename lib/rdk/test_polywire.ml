open Rays
open Rdk
open Rdk_test_support

let near ?(epsilon = 1e-9) left right = abs_float (left -. right) <= epsilon

let float2_attribute geometry name =
  match Geometry.find_attribute ~owner:Attribute.Vertex name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float2 values -> Packed.Float2.View.view values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing attribute " ^ name)

let float3_attribute ~owner geometry name =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.View.view values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing attribute " ^ name)

let with_float name values geometry =
  let attribute = Attribute.create_owned ~owner:Attribute.Point ~name
      (Attribute.Float values) |> get_string in
  Geometry.with_attribute attribute geometry |> get_string

let with_int name values geometry =
  let attribute = Attribute.create_owned ~owner:Attribute.Point ~name
      (Attribute.Int values) |> get_string in
  Geometry.with_attribute attribute geometry |> get_string

let with_float3 name ~x ~y ~z geometry =
  let values = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  let attribute = Attribute.create_owned ~owner:Attribute.Point ~name
      (Attribute.Float3 values) |> get_string in
  Geometry.with_attribute attribute geometry |> get_string

let with_vertex_attribute name storage geometry =
  let attribute = Attribute.create_owned ~owner:Attribute.Vertex ~name storage
      |> get_string in
  Geometry.with_attribute attribute geometry |> get_string

let attribute ~owner name storage =
  Attribute.create_owned ~owner ~name storage |> get_string

let find_storage ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | None -> fail ("missing attribute " ^ name)
  | Some value -> Attribute.View.storage value

let expect_invalid = function
  | Error error when Error.code error = "invalid_geometry" -> ()
  | Error error -> fail ("unexpected error " ^ Error.to_string error)
  | Ok _ -> fail "expected invalid PolyWire input"

let normalize x y z =
  let length = sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
  x /. length, y /. length, z /. length

let transport tx0 ty0 tz0 tx1 ty1 tz1 nx ny nz =
  let ax = (ty0 *. tz1) -. (tz0 *. ty1)
  and ay = (tz0 *. tx1) -. (tx0 *. tz1)
  and az = (tx0 *. ty1) -. (ty0 *. tx1) in
  let sine = sqrt ((ax *. ax) +. (ay *. ay) +. (az *. az))
  and cosine = (tx0 *. tx1) +. (ty0 *. ty1) +. (tz0 *. tz1) in
  if sine <= 1e-12 then nx, ny, nz
  else
    let kx = ax /. sine and ky = ay /. sine and kz = az /. sine in
    let cross_x = (ky *. nz) -. (kz *. ny)
    and cross_y = (kz *. nx) -. (kx *. nz)
    and cross_z = (kx *. ny) -. (ky *. nx)
    and dot = (kx *. nx) +. (ky *. ny) +. (kz *. nz) in
    (nx *. cosine) +. (cross_x *. sine) +. (kx *. dot *. (1. -. cosine)),
    (ny *. cosine) +. (cross_y *. sine) +. (ky *. dot *. (1. -. cosine)),
    (nz *. cosine) +. (cross_z *. sine) +. (kz *. dot *. (1. -. cosine))

let advanced_source () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 2.; 0.; 2.; 9.|] ~y:[|0.; 0.; 3.; 3.; 9.|]
      ~z:(Array.make 5 0.) in
  let topology = Topology.create_owned ~point_count:5
      ~vertex_points:[|0;1; 2;3|] ~primitive_offsets:[|0;2;4|]
      ~primitive_kinds:[|Topology.Open_polyline; Topology.Open_polyline|]
    |> get_string in
  let selection = Group.ordered ~owner:Group.Primitive ~name:"wire"
      ~length:2 [|0|] |> get_string
  and ordered_points = Group.ordered ~owner:Group.Point ~name:"ordered_points"
      ~length:5 [|1;0|] |> get_string
  and ordered_vertices = Group.ordered ~owner:Group.Vertex ~name:"ordered_vertices"
      ~length:4 [|1;0|] |> get_string
  and ordered_primitives = Group.ordered ~owner:Group.Primitive
      ~name:"ordered_primitives" ~length:2 [|1;0|] |> get_string in
  let source_index = Topology_index.create topology in
  let selected_edge = Topology_index.find_edge_index source_index ~a:0 ~b:1 in
  let edges = Edge_group.init ~topology ~index:source_index ~name:"wire_edge"
      (fun edge -> edge = selected_edge) in
  let int_rows = Packed.Int_array.create_owned ~offsets:[|0;1;3;4;6;7|]
      ~values:[|0;10;11;20;30;31;40|] |> get_string
  and float_rows = Packed.Float_array.create_owned ~offsets:[|0;1;3;4;6;7|]
      ~values:[|0.;10.;11.;20.;30.;31.;40.|] |> get_string
  and vertex_rows = Packed.Int_array.create_owned ~offsets:[|0;1;3;4;6|]
      ~values:[|0;10;11;20;30;31|] |> get_string in
  let attributes = [
    attribute ~owner:Attribute.Point "div" (Attribute.Int [|4;6;5;5;5|]);
    attribute ~owner:Attribute.Point "seg" (Attribute.Int [|1;3;1;1;1|]);
    attribute ~owner:Attribute.Point "pf" (Attribute.Float [|0.;10.;20.;30.;40.|]);
    attribute ~owner:Attribute.Point "pi" (Attribute.Int [|0;10;20;30;40|]);
    attribute ~owner:Attribute.Point "pt"
      (Attribute.Text [|"a";"b";"c";"d";"free"|]);
    attribute ~owner:Attribute.Point "p2" (Attribute.Float2
      (Packed.Float2.of_owned ~x:[|0.;10.;20.;30.;40.|]
        ~y:[|1.;11.;21.;31.;41.|] |> get_string));
    attribute ~owner:Attribute.Point "p3" (Attribute.Float3
      (Packed.Float3.View.of_owned_exn ~x:[|0.;10.;20.;30.;40.|]
        ~y:[|1.;11.;21.;31.;41.|] ~z:[|2.;12.;22.;32.;42.|]));
    attribute ~owner:Attribute.Point "p4" (Attribute.Float4
      (Packed.Float4.of_owned ~x:[|0.;10.;20.;30.;40.|]
        ~y:[|1.;11.;21.;31.;41.|] ~z:[|2.;12.;22.;32.;42.|]
        ~w:[|3.;13.;23.;33.;43.|] |> get_string));
    attribute ~owner:Attribute.Point "pia" (Attribute.Int_array int_rows);
    attribute ~owner:Attribute.Point "pfa" (Attribute.Float_array float_rows);
    attribute ~owner:Attribute.Vertex "vf" (Attribute.Float [|0.;10.;20.;30.|]);
    attribute ~owner:Attribute.Vertex "via" (Attribute.Int_array vertex_rows);
    attribute ~owner:Attribute.Primitive "primitive_name"
      (Attribute.Text [|"selected";"retained"|]);
    attribute ~owner:Attribute.Detail "detail" (Attribute.Int [|73|]);
  ] in
  Geometry.create ~positions ~topology ~attributes
    ~groups:[selection; ordered_points; ordered_vertices; ordered_primitives]
    ~edge_groups:[edges] () |> get_string

let run () =
  print_endline "PolyWire tests passed"
