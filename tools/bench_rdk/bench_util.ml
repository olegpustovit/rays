(* Helpers the rdk microbenches share. *)
let integer_env name default = match Sys.getenv_opt name with
  | None -> default | Some value -> max 1 (int_of_string value)

let get = function Ok value -> value | Error error -> failwith (Rdk.Error.to_string error)

let median values =
  let values = Array.copy values in
  Array.sort Float.compare values;
  values.(Array.length values / 2)
