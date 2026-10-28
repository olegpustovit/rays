open Rays
open Rdk
open Rdk_test_support

let points count =
  Line_geometry.points (Array.init count (fun point -> (float_of_int point, 0., 0.)))

let with_attribute owner name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let with_float name values geometry =
  with_attribute Attribute.Point name (Attribute.Float values) geometry

let with_int name values geometry =
  with_attribute Attribute.Point name (Attribute.Int values) geometry

let float_values name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Float values -> values
      | _ -> fail (name ^ " is not scalar float"))
  | None -> fail ("missing " ^ name)

let color_values geometry =
  match Geometry.find_attribute ~owner:Attribute.Point "Cd" geometry with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Float4 values -> Packed.Float4.View.view values
      | _ -> fail "Cd is not float4")
  | None -> fail "missing Cd"

let check_floats expected actual message =
  check (Array.length expected = Array.length actual) (message ^ " length");
  Array.iteri
    (fun index expected ->
      if expected <> actual.(index) then
        fail
          (Printf.sprintf "%s at %d: expected %.17g, got %.17g" message index expected
             actual.(index)))
    expected

let expect_invalid work message =
  match work () with
  | Error error -> check (Error.code error = "invalid_attribute_fade") message
  | Ok _ -> fail (message ^ ": unexpectedly accepted")

let equal_float2 left right =
  let left = Packed.Float2.View.view left and right = Packed.Float2.View.view right in
  left.x = right.x && left.y = right.y

let equal_float3 left right =
  let left = Packed.Float3.View.view left and right = Packed.Float3.View.view right in
  left.x = right.x && left.y = right.y && left.z = right.z

let equal_float4 left right =
  let left = Packed.Float4.View.view left and right = Packed.Float4.View.view right in
  left.x = right.x && left.y = right.y && left.z = right.z && left.w = right.w

let equal_storage left right =
  match (left, right) with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right -> equal_float2 left right
  | Attribute.Float3 left, Attribute.Float3 right -> equal_float3 left right
  | Attribute.Float4 left, Attribute.Float4 right -> equal_float4 left right
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.View.view left and right = Packed.Int_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.View.view left and right = Packed.Float_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | _ -> false

let equal_attributes left right =
  List.equal
    (fun left right ->
      Attribute.owner left = Attribute.owner right
      && String.equal (Attribute.name left) (Attribute.name right)
      && equal_storage (Attribute.View.storage left) (Attribute.View.storage right))
    (Geometry.attributes left) (Geometry.attributes right)

let run () = print_endline "rdk attribute fade tests passed"
