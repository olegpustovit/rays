open Rdk_test_support
open Rays
open Rdk

let get = function Ok value -> value | Error error -> fail (Error.to_string error)

let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> Result.get_ok

let source () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|1.;2.;3.;9.|] ~y:[|0.;1.;2.;9.|] ~z:[|0.;0.;0.;9.|] in
  let topology = Topology.polygons_owned ~point_count:4
      ~vertex_points:[|0;1;2|] ~primitive_offsets:[|0;3|]
      |> Result.get_ok in
  let uv = Packed.Float2.of_owned ~x:[|0.;0.5;1.;2.|]
      ~y:[|1.;0.5;0.;2.|] |> Result.get_ok in
  let rows = Packed.Int_array.create_owned ~offsets:[|0;1;3;3;5|]
      ~values:[|10;20;21;30;31|] |> Result.get_ok in
  let float_rows = Packed.Float_array.create_owned ~offsets:[|0;2;3;3;5|]
      ~values:[|0.1;0.2;1.1;3.1;3.2|] |> Result.get_ok
  and direction = Packed.Float3.Private.of_owned_exn
      ~x:[|1.;2.;3.;4.|] ~y:[|5.;6.;7.;8.|] ~z:[|9.;10.;11.;12.|]
  and rgba = Packed.Float4.of_owned ~x:[|0.;1.;2.;3.|]
      ~y:[|4.;5.;6.;7.|] ~z:[|8.;9.;10.;11.|] ~w:[|1.;0.8;0.6;0.4|]
      |> Result.get_ok in
  let selected = Group.ordered ~owner:Group.Point ~name:"selected"
      ~length:4 [|2;0|] |> Result.get_ok
  and face = Group.init ~grain:1 ~owner:Group.Primitive ~name:"face" 1
      (Fun.const true) in
  let index = Topology_index.create topology in
  let boundary = Edge_group.init ~grain:1 ~topology ~index ~name:"boundary"
      (Fun.const true) in
  Geometry.create ~positions ~topology ~attributes:[
    attribute Attribute.Point "id" (Attribute.Int [|10;11;12;13|]);
    attribute Attribute.Point "weight" (Attribute.Float [|1.;0.5;2.;0.|]);
    attribute Attribute.Point "probability" (Attribute.Float [|1.;0.;1.;0.|]);
    attribute Attribute.Point "tag" (Attribute.Text [|"a";"b";"c";"free"|]);
    attribute Attribute.Point "uv" (Attribute.Float2 uv);
    attribute Attribute.Point "direction" (Attribute.Float3 direction);
    attribute Attribute.Point "rgba" (Attribute.Float4 rgba);
    attribute Attribute.Point "rows" (Attribute.Int_array rows);
    attribute Attribute.Point "float_rows" (Attribute.Float_array float_rows);
    attribute Attribute.Vertex "corner" (Attribute.Int [|4;5;6|]);
    attribute Attribute.Primitive "material" (Attribute.Int [|7|]);
    attribute Attribute.Detail "author" (Attribute.Text [|"rays"|]);
  ] ~groups:[selected;face] ~edge_groups:[boundary] () |> Result.get_ok

let int_attr name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Int values -> values | _ -> fail (name ^ " is not integer"))
  | None -> fail ("missing point attribute " ^ name)

let float_attr name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float values -> values | _ -> fail (name ^ " is not float"))
  | None -> fail ("missing point attribute " ^ name)

let text_attr name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Text values -> values | _ -> fail (name ^ " is not text"))
  | None -> fail ("missing point attribute " ^ name)

let geometry_hash geometry =
  let hash = ref 17 in
  let add value = hash := Rays_math.Hash.mix !hash value in
  let positions = Packed.Float3.Private.view (Geometry.positions geometry) in
  Array.iter (fun value -> add (Hashtbl.hash value)) positions.x;
  Array.iter (fun value -> add (Hashtbl.hash value)) positions.y;
  Array.iter (fun value -> add (Hashtbl.hash value)) positions.z;
  Array.iter add (int_attr "sourcepoint" geometry);
  Array.iter add (int_attr "sourceindex" geometry);
  Array.iter (fun value -> add (Hashtbl.hash value)) (float_attr "density" geometry);
  !hash

let run () =
  print_endline "point generate tests passed"
