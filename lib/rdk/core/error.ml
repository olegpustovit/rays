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

exception Fail of string
exception Rdk of t
let fail message = raise (Fail message)
let fail_with ~operation ~code message = raise (Rdk (make ~operation ~code message))
let guard ?(cancelled = "geometry operation was cancelled") ?invalid_argument
    ~operation ~code work =
  try Result.map_error (make ~operation ~code) (work ()) with
  | Rdk error -> Error error
  | Fail message -> Error (make ~operation ~code message)
  | Cancel.Cancelled -> Error (make ~operation ~code:"cancelled" cancelled)
  | Invalid_argument message as exn -> (match invalid_argument with
      | Some code -> Error (make ~operation ~code message)
      | None -> raise exn)
let get = function
  | Ok value -> value
  | Error value when value.code = "cancelled" -> raise Cancel.Cancelled
  | Error value -> raise (Fail value.message)
let run ?cancelled ~operation ~code work =
  guard ?cancelled ~invalid_argument:code ~operation ~code (fun () -> Ok (work ()))
let operation value = value.operation
let code value = value.code
let message value = value.message
let hints value = value.hints
let to_string value =
  let base = Printf.sprintf "%s [%s]: %s" value.operation value.code value.message in
  match value.hints with
  | [] -> base
  | hints -> base ^ " (" ^ String.concat "; " hints ^ ")"
