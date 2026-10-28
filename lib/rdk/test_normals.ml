open Rays
open Rdk
open Rdk_test_support

let near a b = abs_float (a -. b) <= 1e-10

let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> get_string

let normal owner ?(name = "N") geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some value ->
      (match Attribute.Private.storage value with
       | Attribute.Float3 values -> Packed.Float3.Private.view values
       | _ -> fail "normal attribute has wrong storage")
  | None -> fail "normal attribute is missing"

let check_vec values index (x, y, z) message =
  check (near values.Packed.Float3.Private.x.(index) x
      && near values.y.(index) y && near values.z.(index) z) message

let group owner name geometry =
  Geometry.find_group ~owner name geometry |> Option.get

let equal_normals owner left right =
  let left = normal owner left and right = normal owner right in
  left.x = right.x && left.y = right.y && left.z = right.z

let with_constant_normal owner geometry =
  let count = match owner with
    | Attribute.Point -> Geometry.point_count geometry
    | Vertex -> Geometry.vertex_count geometry
    | Primitive -> Geometry.primitive_count geometry
    | Detail -> 1 in
  let values = Packed.Float3.Private.of_owned_exn
      ~x:(Array.make count 0.) ~y:(Array.make count 1.)
      ~z:(Array.make count 0.) in
  Geometry.with_attribute
    (attribute owner "N" (Attribute.Float3 values)) geometry |> get_string

let changed_indices owner geometry =
  let values = normal owner geometry in
  let changed = ref [] in
  for index = 0 to Array.length values.x - 1 do
    if not (near values.x.(index) 0. && near values.y.(index) 1.
        && near values.z.(index) 0.) then changed := index :: !changed
  done;
  List.rev !changed

let run () =
  print_endline "Normals tests passed"
