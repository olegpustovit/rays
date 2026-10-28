(* The one-liners every test file used to copy. *)
let fail format = Printf.ksprintf failwith format
let check condition message = if not condition then failwith message
let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
