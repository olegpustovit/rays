type t = { x : float; y : float }

let create x y = { x; y }
let zero = { x = 0.; y = 0. }
let add a b = { x = a.x +. b.x; y = a.y +. b.y }
let sub a b = { x = a.x -. b.x; y = a.y -. b.y }
let to_string a = Printf.sprintf "(%g, %g)" a.x a.y
