open Rdk_test_support
open Rays
open Rdk

let get = function Ok value -> value | Error error -> fail (Error.to_string error)
let near a b = abs_float (a -. b) <= 1e-12

let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> Result.get_ok

let with_attribute attribute geometry =
  Geometry.with_attribute attribute geometry |> Result.get_ok

let fixture () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;2.;3.;4.; 10.;11.;11.;10.|]
      ~y:[|0.;0.;0.;0.;0.; 0.;0.;1.;1.|] ~z:(Array.make 9 0.) in
  let topology = Topology.create_owned ~point_count:9
      ~vertex_points:[|0;1;2;3;4; 5;6;7;8|]
      ~primitive_offsets:[|0;5;9|]
      ~primitive_kinds:[|Topology.Open_polyline;Topology.Closed_polyline|]
      |> Result.get_ok in
  let rows = Packed.Float_array.create_owned
      ~offsets:[|0;1;2;3;4;5;6;7;8;9|]
      ~values:[|0.;10.;20.;30.;40.;50.;60.;70.;80.|] |> Result.get_ok in
  Geometry.create ~positions ~topology ~attributes:[
    attribute Attribute.Point "distance"
      (Attribute.Float [|-1.;0.;0.;1.;-1.; 0.;1.;-1.;0.|]);
    attribute Attribute.Point "weight"
      (Attribute.Float [|0.;10.;20.;30.;40.;50.;60.;70.;80.|]);
    attribute Attribute.Point "id" (Attribute.Int [|0;1;2;3;4;5;6;7;8|]);
    attribute Attribute.Point "label"
      (Attribute.Text [|"a";"b";"c";"d";"e";"f";"g";"h";"i"|]);
    attribute Attribute.Point "rows" (Attribute.Float_array rows);
    attribute Attribute.Primitive "cut" (Attribute.Float [|0.;0.5|]);
    attribute Attribute.Primitive "material" (Attribute.Int [|7;9|]);
    attribute Attribute.Detail "author" (Attribute.Text [|"curve"|]);
  ] () |> Result.get_ok

let positions geometry = Packed.Float3.View.view (Geometry.positions geometry)

let float_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> (match Attribute.View.storage attribute with
      | Attribute.Float values -> values | _ -> fail (name ^ " storage"))
  | None -> fail (name ^ " missing")

let int_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> (match Attribute.View.storage attribute with
      | Attribute.Int values -> values | _ -> fail (name ^ " storage"))
  | None -> fail (name ^ " missing")

let text_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> (match Attribute.View.storage attribute with
      | Attribute.Text values -> values | _ -> fail (name ^ " storage"))
  | None -> fail (name ^ " missing")

let expect code work message = match work () with
  | Error error -> check (Error.code error = code) message
  | Ok _ -> fail (message ^ ": unexpectedly accepted")

let equal_geometry left right =
  let lp = positions left and rp = positions right in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && List.map (fun attribute -> Attribute.name attribute,
      Attribute.owner attribute, Attribute.storage attribute)
      (Geometry.attributes left)
     = List.map (fun attribute -> Attribute.name attribute,
         Attribute.owner attribute, Attribute.storage attribute)
         (Geometry.attributes right)

let large_fixture count =
  let points = count * 2 in
  let positions = Packed.Float3.View.of_owned_exn
      ~x:(Array.init points (fun point -> Float.of_int (point / 2)))
      ~y:(Array.init points (fun point -> Float.of_int (point land 1)))
      ~z:(Array.make points 0.) in
  let topology = Topology.create_owned ~point_count:points
      ~vertex_points:(Array.init points Fun.id)
      ~primitive_offsets:(Array.init (count + 1) (fun primitive -> primitive * 2))
      ~primitive_kinds:(Array.make count Topology.Open_polyline) |> Result.get_ok in
  Geometry.create ~positions ~topology ~attributes:[
    attribute Attribute.Point "distance"
      (Attribute.Float (Array.init points (fun point -> if point land 1 = 0 then -1. else 1.)));
    attribute Attribute.Point "weight"
      (Attribute.Float (Array.init points Float.of_int));
    attribute Attribute.Primitive "material"
      (Attribute.Int (Array.init count (fun primitive -> primitive mod 17)))] ()
  |> Result.get_ok

let long_curve_fixture point_count =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:(Array.init point_count Float.of_int) ~y:(Array.make point_count 0.)
      ~z:(Array.make point_count 0.) in
  let topology = Topology.create_owned ~point_count
      ~vertex_points:(Array.init point_count Fun.id)
      ~primitive_offsets:[|0;point_count|]
      ~primitive_kinds:[|Topology.Open_polyline|] |> Result.get_ok in
  Geometry.create ~positions ~topology ~attributes:[
    attribute Attribute.Point "distance" (Attribute.Float
      (Array.init point_count (fun point -> if point land 1 = 0 then -1. else 1.)));
    attribute Attribute.Point "weight"
      (Attribute.Float (Array.init point_count Float.of_int))] ()
  |> Result.get_ok

let run () =
  print_endline "extract point from curve tests passed"
