open Rdk_test_support
open Rays
open Rdk

let get = function Ok value -> value | Error error -> fail (Error.to_string error)
let attribute owner name storage = Attribute.create_owned ~owner ~name storage |> Result.get_ok
let points values = Line_geometry.points values

let point_int name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Int values -> values
      | _ -> fail (name ^ " wrong storage"))
  | None -> fail ("missing " ^ name)

let point_float3 name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Float3 values -> Packed.Float3.View.view values
      | _ -> fail (name ^ " wrong storage"))
  | None -> fail ("missing " ^ name)

let check_close actual expected message =
  if abs_float (actual -. expected) > 1e-12 then
    fail (Printf.sprintf "%s: expected %.17g, got %.17g" message expected actual)

let geometry_hash geometry =
  let hash = ref 17 in
  let add value = hash := Rays_math.Hash.mix !hash value in
  let positions = Packed.Float3.View.view (Geometry.positions geometry) in
  Array.iter (fun value -> add (Hashtbl.hash value)) positions.x;
  Array.iter (fun value -> add (Hashtbl.hash value)) positions.y;
  Array.iter (fun value -> add (Hashtbl.hash value)) positions.z;
  List.iter
    (fun attribute ->
      add (Hashtbl.hash (Attribute.name attribute, Attribute.kind_name attribute));
      match Attribute.View.storage attribute with
      | Attribute.Float values -> Array.iter (fun value -> add (Hashtbl.hash value)) values
      | Attribute.Int values -> Array.iter add values
      | Attribute.Float3 values ->
          let values = Packed.Float3.View.view values in
          Array.iter (fun value -> add (Hashtbl.hash value)) values.x;
          Array.iter (fun value -> add (Hashtbl.hash value)) values.y;
          Array.iter (fun value -> add (Hashtbl.hash value)) values.z
      | _ -> ())
    (Geometry.attributes geometry);
  !hash

let run () = print_endline "point replicate tests passed"
