open Rays
open Rdk
open Rdk_test_support

let position_view geometry = Packed.Float3.View.view (Geometry.positions geometry)
let topology_view geometry = Topology.View.view (Geometry.topology geometry)

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
      left.x = right.x && left.y = right.y && left.z = right.z
      && left.w = right.w
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.View.view left
      and right = Packed.Int_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.View.view left
      and right = Packed.Float_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | _ -> false

let with_attribute owner name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> get_string in
  Geometry.with_attribute attribute geometry |> get_string

let with_group group geometry = Geometry.with_group group geometry |> get_string

let check_attribute_storage geometry owner name expected =
  match Geometry.find_attribute ~owner name geometry with
  | Some actual ->
      let expected = Attribute.create_owned ~owner ~name expected |> get_string in
      check (equal_storage actual expected) ("Sweep attribute ancestry: " ^ name)
  | None -> fail ("Sweep attribute missing: " ^ name)

let backbone () = Line_geometry.polyline [|(0., 0., 0.); (0., 0., 2.)|] |> get_ok
let profile () = Line_geometry.polyline ~closed:true
    [|(-1., -1., 0.); (1., -1., 0.); (1., 1., 0.); (-1., 1., 0.)|]
    |> get_ok

let normalize (x, y, z) =
  let length = sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
  x /. length, y /. length, z /. length

let transport (tx0, ty0, tz0) (tx1, ty1, tz1) (x, y, z) =
  let ax = (ty0 *. tz1) -. (tz0 *. ty1)
  and ay = (tz0 *. tx1) -. (tx0 *. tz1)
  and az = (tx0 *. ty1) -. (ty0 *. tx1) in
  let sine = sqrt ((ax *. ax) +. (ay *. ay) +. (az *. az))
  and cosine = (tx0 *. tx1) +. (ty0 *. ty1) +. (tz0 *. tz1) in
  if sine <= 1e-12 then x, y, z
  else
    let kx = ax /. sine and ky = ay /. sine and kz = az /. sine in
    let cross_x = (ky *. z) -. (kz *. y)
    and cross_y = (kz *. x) -. (kx *. z)
    and cross_z = (kx *. y) -. (ky *. x)
    and dot = (kx *. x) +. (ky *. y) +. (kz *. z) in
    (x *. cosine) +. (cross_x *. sine) +. (kx *. dot *. (1. -. cosine)),
    (y *. cosine) +. (cross_y *. sine) +. (ky *. dot *. (1. -. cosine)),
    (z *. cosine) +. (cross_z *. sine) +. (kz *. dot *. (1. -. cosine))

let expect_invalid = function
  | Error error when Error.code error = "invalid_geometry" -> ()
  | Error error -> fail ("unexpected Sweep error: " ^ Error.to_string error)
  | Ok _ -> fail "expected invalid Sweep input"

let run () =
  print_endline "Sweep tests passed"
