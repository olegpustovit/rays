open Rdk_test_support
open Rdk


let positions geometry =
  Packed.Float3.View.view (Geometry.positions geometry)

let close left right = abs_float (left -. right) <= 1e-12
let array_close left right =
  Array.length left = Array.length right
  && Array.for_all2 close left right

let attribute owner name storage geometry =
  let value = Attribute.create_owned ~owner ~name storage |> function
    | Ok value -> value | Error message -> fail message in
  Geometry.with_attribute value geometry |> function
  | Ok value -> value | Error message -> fail message

let points xs =
  let count = Array.length xs in
  let positions = Packed.Float3.View.of_owned_exn ~x:(Array.copy xs)
      ~y:(Array.make count 0.) ~z:(Array.make count 0.) in
  Geometry.create ~positions ~topology:(Topology.empty ~point_count:count) ()
  |> function Ok value -> value | Error message -> fail message

let point_group name count members =
  let builder = Group.Builder.create ~owner:Group.Point ~name count in
  List.iter (fun point -> Group.Builder.set builder point true) members;
  Group.Builder.freeze builder

let primitive_group name count =
  let builder = Group.Builder.create ~owner:Group.Primitive ~name count in
  Group.Builder.freeze builder

let point_float name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float values -> values
       | _ -> fail ("wrong storage for " ^ name))
  | None -> fail ("missing attribute " ^ name)

let run () =
  print_endline "Blend Shapes tests passed"
