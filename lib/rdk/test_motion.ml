open Rays
open Rdk

let fail message = prerr_endline ("test_motion: " ^ message); exit 1
let check condition message = if not condition then fail message
let get = function Ok value -> value | Error message -> fail message
let get_rdk = function Ok value -> value | Error error -> fail (Error.to_string error)

let points values = Line_geometry.points values

let add_int name values geometry =
  let attribute = Attribute.create_owned ~name ~owner:Attribute.Point
      (Attribute.Int values) |> get in
  Geometry.with_attribute attribute geometry |> get

let add_float3 name x y z geometry =
  let values = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
  let attribute = Attribute.create_owned ~name ~owner:Attribute.Point
      (Attribute.Float3 values) |> get in
  Geometry.with_attribute attribute geometry |> get

let float3 name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.Private.view values
       | _ -> fail (name ^ " has wrong storage"))
  | None -> fail ("missing " ^ name)

let positions geometry = Packed.Float3.Private.view (Geometry.positions geometry)

let check_array expected actual message =
  if expected <> actual then fail message

let run () =
  print_endline "test_motion: ok"
