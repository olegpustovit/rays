type t = bool Atomic.t
exception Cancelled

let create () = Atomic.make false
let cancel value = Atomic.set value true
let is_cancelled value = Atomic.get value
let check value = if is_cancelled value then raise Cancelled
let check_opt = function None -> () | Some value -> check value

(* Polls every [mask + 1] steps; [mask] is a power of two minus one. *)
let[@inline] poll cancel i mask = if i land mask = 0 then check_opt cancel
