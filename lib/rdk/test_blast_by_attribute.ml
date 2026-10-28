open Rdk
open Rdk_test_support

let with_attribute owner name storage geometry =
  Attribute.create_owned ~owner ~name storage |> Result.get_ok
  |> fun attribute -> Geometry.with_attribute attribute geometry |> Result.get_ok

let group_members owner name geometry =
  match Geometry.find_group ~owner name geometry with
  | None -> fail ("missing group " ^ name)
  | Some group ->
      let result = ref [] in
      Group.iter (fun element -> result := element :: !result) group;
      List.rev !result

let equal_storage left right =
  match Attribute.View.storage left, Attribute.View.storage right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.View.view left
      and right = Packed.Int_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.View.view left
      and right = Packed.Float_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
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
  | _ -> false

let point_source () =
  let geometry = Line_geometry.points (Array.init 6 (fun point -> float_of_int point, 0., 0.))
      |> with_attribute Attribute.Point "value"
           (Attribute.Float [|0.; 1.; 2.; 3.; 4.; 5.|])
      |> with_attribute Attribute.Point "class"
           (Attribute.Int [|0; 1; 2; 3; 4; 5|]) in
  let base = Group.init ~grain:1 ~owner:Group.Point ~name:"base" 6
      (fun point -> point = 1 || point = 2 || point = 4)
  and old = Group.init ~grain:1 ~owner:Group.Point ~name:"picked" 6
      (fun _ -> true) in
  geometry |> Geometry.with_group base |> Result.get_ok
  |> Geometry.with_group old |> Result.get_ok

let expect_invalid work message = match work () with
  | Error error -> check (Error.code error = "invalid_blast") message
  | Ok _ -> fail (message ^ ": unexpectedly accepted")

let run () =
  print_endline "blast by attribute tests passed"
