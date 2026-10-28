open Rdk_test_support
open Rays
open Rdk

let get = function Ok value -> value | Error error -> fail (Error.to_string error)
let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> Result.get_ok

let int_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some value -> (match Attribute.View.storage value with
      | Attribute.Int values -> values
      | _ -> fail (name ^ " storage"))
  | None -> fail ("missing " ^ name)

let text_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some value -> (match Attribute.View.storage value with
      | Attribute.Text values -> values
      | _ -> fail (name ^ " storage"))
  | None -> fail ("missing " ^ name)

let group owner name geometry =
  Geometry.find_group ~owner name geometry |> Option.get

let run () =
  print_endline "Duplicate tests passed"
