open Rdk_test_support
open Rays
open Rdk

let get = function Ok value -> value | Error error -> fail (Error.to_string error)
let near a b = abs_float (a -. b) <= 1e-12
let positions geometry = Packed.Float3.View.view (Geometry.positions geometry)
let attribute owner name storage = Attribute.create_owned ~owner ~name storage |> Result.get_ok
let with_attribute attribute geometry = Geometry.with_attribute attribute geometry |> Result.get_ok

let check_point geometry point (x, y, z) message =
  let p = positions geometry in
  check (near p.x.(point) x && near p.y.(point) y && near p.z.(point) z) message

let two_triangles () =
  let positions =
    Packed.Float3.View.of_owned_exn ~x:[| 0.; 2.; 0.; 10.; 14.; 10. |]
      ~y:[| 0.; 0.; 2.; 0.; 0.; 2. |] ~z:(Array.make 6 0.)
  in
  let topology =
    Topology.polygons_owned ~point_count:6 ~vertex_points:[| 0; 1; 2; 3; 4; 5 |]
      ~primitive_offsets:[| 0; 3; 6 |]
    |> Result.get_ok
  in
  Geometry.create ~positions ~topology () |> Result.get_ok

let int_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Int values -> values
      | _ -> fail (name ^ " storage"))
  | None -> fail (name ^ " missing")

let text_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Text values -> values
      | _ -> fail (name ^ " storage"))
  | None -> fail (name ^ " missing")

let expect code work message =
  match work () with
  | Error error -> check (Error.code error = code) message
  | Ok _ -> fail (message ^ ": unexpectedly accepted")

let equal left right =
  let lp = positions left and rp = positions right in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && List.map
       (fun attribute ->
         (Attribute.name attribute, Attribute.owner attribute, Attribute.storage attribute))
       (Geometry.attributes left)
     = List.map
         (fun attribute ->
           (Attribute.name attribute, Attribute.owner attribute, Attribute.storage attribute))
         (Geometry.attributes right)

let run () = print_endline "extract centroid tests passed"
