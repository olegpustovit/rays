type t = { x : float; y : float; z : float; w : float }

let create ~x ~y ~z ~w = { x; y; z; w }
let identity = { x = 0.; y = 0.; z = 0.; w = 1. }

let length_squared value =
  (value.x *. value.x) +. (value.y *. value.y) +. (value.z *. value.z) +. (value.w *. value.w)

let normalize value =
  let length = sqrt (length_squared value) in
  if length <= 1e-12 then identity
  else
    { x = value.x /. length; y = value.y /. length; z = value.z /. length; w = value.w /. length }

let mul left right =
  {
    w = (left.w *. right.w) -. (left.x *. right.x) -. (left.y *. right.y) -. (left.z *. right.z);
    x = (left.w *. right.x) +. (left.x *. right.w) +. (left.y *. right.z) -. (left.z *. right.y);
    y = (left.w *. right.y) -. (left.x *. right.z) +. (left.y *. right.w) +. (left.z *. right.x);
    z = (left.w *. right.z) +. (left.x *. right.y) -. (left.y *. right.x) +. (left.z *. right.w);
  }
