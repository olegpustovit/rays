type t = {
  operation : string;
  code : string;
  message : string;
  hints : string list;
}

let make ?(hints = []) ~operation ~code message =
  if String.trim operation = "" then invalid_arg "Rdk.Error.make: empty operation";
  if String.trim code = "" then invalid_arg "Rdk.Error.make: empty code";
  { operation; code; message; hints }

let of_string ~operation ~code message = make ~operation ~code message
exception Fail of string
let fail message = raise (Fail message)
let guard ~operation ~code work =
  try Result.map_error (of_string ~operation ~code) (work ()) with
  | Fail message -> Error (make ~operation ~code message)
  | Cancel.Cancelled -> Error (make ~operation ~code:"cancelled"
      "geometry operation was cancelled")
let get = function
  | Ok value -> value
  | Error value when value.code = "cancelled" -> raise Cancel.Cancelled
  | Error value -> raise (Fail value.message)
let operation value = value.operation
let code value = value.code
let message value = value.message
let hints value = value.hints
let to_string value =
  let base = Printf.sprintf "%s [%s]: %s" value.operation value.code value.message in
  match value.hints with
  | [] -> base
  | hints -> base ^ " (" ^ String.concat "; " hints ^ ")"
