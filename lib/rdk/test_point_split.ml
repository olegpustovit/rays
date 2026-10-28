open Rdk_test_support
open Rays
open Rdk

let get = function Ok value -> value | Error error -> fail (Error.to_string error)

let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> Result.get_ok

let base ?(delta = 1.) () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;1.;0.;2.|] ~y:[|0.;0.;1.;1.;2.|]
      ~z:(Array.make 5 0.) in
  let topology = Topology.polygons_owned ~point_count:5
      ~vertex_points:[|0;1;2;0;2;3|] ~primitive_offsets:[|0;3;6|]
      |> Result.get_ok in
  let uv = Packed.Float2.of_owned
      ~x:[|0.;1.;1.;delta;1. +. delta;0.|]
      ~y:[|0.;0.;1.;delta;1. +. delta;1.|] |> Result.get_ok in
  let weights = Packed.Float_array.create_owned
      ~offsets:[|0;2;4;6;8;10;12|]
      ~values:[|0.;10.; 1.;11.; 2.;12.; delta;13.;
                2. +. delta;14.; 3.;15.|] |> Result.get_ok in
  let seam_points = Group.ordered ~owner:Group.Point ~name:"seam_points"
      ~length:5 [|2;0|] |> Result.get_ok
  and one_corner = Group.init ~grain:1 ~owner:Group.Vertex ~name:"one_corner" 6
      (fun vertex -> vertex = 3)
  and first_face = Group.init ~grain:1 ~owner:Group.Primitive ~name:"first_face" 2
      (fun primitive -> primitive = 0) in
  let index = Topology_index.create topology in
  let diagonal = Topology_index.find_edge_index index ~a:0 ~b:2 in
  check (diagonal >= 0) "Point Split fixture diagonal missing";
  let diagonal = Edge_group.init ~grain:1 ~topology ~index ~name:"diagonal"
      (fun edge -> edge = diagonal) in
  Geometry.create ~positions ~topology
    ~attributes:[
      attribute Attribute.Point "id" (Attribute.Int [|0;1;2;3;4|]);
      attribute Attribute.Vertex "uv" (Attribute.Float2 uv);
      attribute Attribute.Vertex "tag"
        (Attribute.Text [|"a";"b";"c";"z";"w";"d"|]);
      attribute Attribute.Vertex "weights" (Attribute.Float_array weights);
      attribute Attribute.Primitive "material" (Attribute.Int [|0;1|]);
      attribute Attribute.Detail "source" (Attribute.Text [|"split"|]);
    ] ~groups:[seam_points;one_corner;first_face] ~edge_groups:[diagonal] ()
    |> Result.get_ok

let group owner name geometry =
  match Geometry.find_group ~owner name geometry with
  | Some value -> value | None -> fail ("missing group " ^ name)

let attr owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some value -> value | None -> fail ("missing attribute " ^ name)

let float2 attribute = match Attribute.View.storage attribute with
  | Attribute.Float2 values -> Packed.Float2.View.view values
  | _ -> fail "expected float2 attribute"

let int_values attribute = match Attribute.View.storage attribute with
  | Attribute.Int values -> values | _ -> fail "expected integer attribute"

let text_values attribute = match Attribute.View.storage attribute with
  | Attribute.Text values -> values | _ -> fail "expected text attribute"

let float_rows attribute = match Attribute.View.storage attribute with
  | Attribute.Float_array values -> Packed.Float_array.View.view values
  | _ -> fail "expected float-array attribute"

let row values row =
  Array.sub values.Packed.Float_array.View.values values.offsets.(row)
    (values.offsets.(row + 1) - values.offsets.(row))

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
      left.x = right.x && left.y = right.y && left.z = right.z && left.w = right.w
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.View.view left
      and right = Packed.Int_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.View.view left
      and right = Packed.Float_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | _ -> false

let equal_group left right =
  Group.owner left = Group.owner right && Group.name left = Group.name right
  && Group.ordered_elements left = Group.ordered_elements right
  && Group.length left = Group.length right
  && let equal = ref true in
     for element = 0 to Group.length left - 1 do
       if Group.mem element left <> Group.mem element right then equal := false
     done;
     !equal

let equal_edge_group left right =
  Edge_group.name left = Edge_group.name right
  && Edge_group.length left = Edge_group.length right
  && let equal = ref true in
     for edge = 0 to Edge_group.length left - 1 do
       if Edge_group.mem edge left <> Edge_group.mem edge right then equal := false
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
  && List.equal (fun left right -> Attribute.owner left = Attribute.owner right
      && Attribute.name left = Attribute.name right && equal_storage left right)
      (Geometry.attributes left) (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)
  && List.equal equal_edge_group (Geometry.edge_groups left)
      (Geometry.edge_groups right)

let large_fixture () =
  let geometry = Plane_generators.grid ~connectivity:Plane_generators.Grid_quads ~columns:160 ~rows:120
      ~size:10. () |> get in
  let material = attribute Attribute.Primitive "material"
      (Attribute.Int (Array.init (Geometry.primitive_count geometry)
        (fun primitive -> (primitive / 7) mod 3))) in
  let seam_region = Group.init ~grain:1 ~owner:Group.Primitive
      ~name:"seam_region" (Geometry.primitive_count geometry)
      (fun primitive -> primitive land 3 < 2) in
  geometry |> Geometry.with_attribute material |> Result.get_ok
      |> Geometry.with_group seam_region |> Result.get_ok

let run () =
  print_endline "point split tests passed"
