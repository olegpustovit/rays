open Rays
open Rdk
open Rdk_test_support

let point_cloud count =
  Line_geometry.points (Array.init count (fun point -> float_of_int point, 0., 0.))

let mesh () =
  let positions = Packed.Float3.Builder.create 4 in
  Array.iteri (fun point (x, y, z) ->
    Packed.Float3.Builder.set positions point x y z)
    [|(0., 0., 0.); (1., 0., 0.); (1., 1., 0.); (0., 1., 0.)|];
  let topology = Rdk_test_support.Tb.create ~point_count:4 () in
  Rdk_test_support.Tb.add_polygon topology [|0; 1; 2|];
  Rdk_test_support.Tb.add_polygon topology [|0; 2; 3|];
  Geometry.create ~positions:(Packed.Float3.Builder.freeze positions)
    ~topology:(Rdk_test_support.Tb.freeze topology) ()
  |> Result.get_ok

let with_text owner name values geometry =
  let attribute = Attribute.create_owned ~owner ~name (Attribute.Text values)
      |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let with_int owner name values geometry =
  let attribute = Attribute.create_owned ~owner ~name (Attribute.Int values)
      |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let with_group group geometry = Geometry.with_group group geometry |> Result.get_ok

let group owner name geometry =
  match Geometry.find_group ~owner name geometry with
  | Some group -> group
  | None -> fail ("missing group " ^ name)

let members group =
  let output = ref [] in
  Group.iter (fun element -> output := element :: !output) group;
  List.rev !output

let expect_members expected group message =
  check (members group = expected) message

let expect_invalid operation message = match operation () with
  | Error error -> check (Error.code error = "invalid_group") message
  | Ok _ -> fail (message ^ ": unexpectedly succeeded")

let same_group left right =
  Group.owner left = Group.owner right
  && Group.name left = Group.name right
  && Group.length left = Group.length right
  && Group.is_ordered left = Group.is_ordered right
  && members left = members right

let text_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Text values -> values
       | _ -> fail ("non-text attribute " ^ name))
  | None -> fail ("missing attribute " ^ name)

let run () =
  print_endline "groups from name tests passed"
