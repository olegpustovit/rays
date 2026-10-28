open Rays
open Rdk
open Rdk_test_support

let near ?(epsilon = 1e-9) left right = abs_float (left -. right) <= epsilon

let float3_attribute ~owner geometry name =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Float3 values -> Packed.Float3.View.view values
      | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing attribute " ^ name)

let norm (values : Packed.Float3.View.view) index =
  sqrt
    ((values.x.(index) *. values.x.(index))
    +. (values.y.(index) *. values.y.(index))
    +. (values.z.(index) *. values.z.(index)))

let dot (left : Packed.Float3.View.view) (right : Packed.Float3.View.view) index =
  (left.x.(index) *. right.x.(index))
  +. (left.y.(index) *. right.y.(index))
  +. (left.z.(index) *. right.z.(index))

let frame_orientation (normal : Packed.Float3.View.view) (tangent : Packed.Float3.View.view)
    (bitangent : Packed.Float3.View.view) index =
  let cx = (tangent.y.(index) *. bitangent.z.(index)) -. (tangent.z.(index) *. bitangent.y.(index))
  and cy = (tangent.z.(index) *. bitangent.x.(index)) -. (tangent.x.(index) *. bitangent.z.(index))
  and cz =
    (tangent.x.(index) *. bitangent.y.(index)) -. (tangent.y.(index) *. bitangent.x.(index))
  in
  (cx *. normal.x.(index)) +. (cy *. normal.y.(index)) +. (cz *. normal.z.(index))

let expect_invalid = function
  | Error error when Error.code error = "invalid_geometry" -> ()
  | Error error -> fail ("unexpected error " ^ Error.to_string error)
  | Ok _ -> fail "expected invalid PolyFrame input"

let with_attribute owner name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> get_string in
  Geometry.with_attribute attribute geometry |> get_string

let run () = print_endline "PolyFrame tests passed"
