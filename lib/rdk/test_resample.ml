open Rays
open Rdk
open Rdk_test_support

let near ?(epsilon = 1e-9) left right = abs_float (left -. right) <= epsilon

let attribute geometry owner name =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> attribute
  | None -> fail ("missing attribute " ^ name)

let float_attribute geometry name =
  match Attribute.View.storage (attribute geometry Attribute.Point name) with
  | Attribute.Float values -> values
  | _ -> fail (name ^ " has unexpected storage")

let int_attribute geometry name =
  match Attribute.View.storage (attribute geometry Attribute.Point name) with
  | Attribute.Int values -> values
  | _ -> fail (name ^ " has unexpected storage")

let float3_attribute geometry name =
  match Attribute.View.storage (attribute geometry Attribute.Point name) with
  | Attribute.Float3 values -> Packed.Float3.View.view values
  | _ -> fail (name ^ " has unexpected storage")

let expect_invalid = function
  | Error error when Error.code error = "invalid_geometry" -> ()
  | Error error -> fail ("unexpected error " ^ Error.to_string error)
  | Ok _ -> fail "expected invalid Resample input"

let x_positions geometry =
  (Packed.Float3.View.view (Geometry.positions geometry)).x

let with_primitive_attribute name storage geometry =
  let attribute = Attribute.create_owned ~owner:Attribute.Primitive ~name storage
      |> get_string in
  Geometry.with_attribute attribute geometry |> get_string

let run () =
  print_endline "Resample tests passed"
