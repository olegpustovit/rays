open Rays
open Rdk
open Rdk_test_support

let geometry positions primitives =
  let packed = Packed.Float3.Builder.create (Array.length positions) in
  Array.iteri (fun point (x, y, z) -> Packed.Float3.Builder.set packed point x y z) positions;
  let topology = Rdk_test_support.Tb.create ~point_count:(Array.length positions) () in
  Array.iter
    (fun (kind, points) ->
      match kind with
      | `Polygon -> Rdk_test_support.Tb.add_polygon topology points
      | `Open -> Rdk_test_support.Tb.add_open_polyline topology points)
    primitives;
  Geometry.create
    ~positions:(Packed.Float3.Builder.freeze packed)
    ~topology:(Rdk_test_support.Tb.freeze topology)
    ()
  |> Result.get_ok

let group owner name geometry =
  match Geometry.find_group ~owner name geometry with
  | Some group -> group
  | None -> fail ("missing group " ^ name)

let edge_group name geometry =
  match Geometry.find_edge_group name geometry with
  | Some group -> group
  | None -> fail ("missing edge group " ^ name)

let members group =
  let output = ref [] in
  Group.iter (fun member -> output := member :: !output) group;
  List.rev !output

let edge_members group =
  let output = ref [] in
  Edge_group.iter (fun member -> output := member :: !output) group;
  List.rev !output

let expect_members expected group message = check (members group = expected) message
let with_group group geometry = Geometry.with_group group geometry |> Result.get_ok

let expect_invalid operation message =
  match operation () with
  | Error error -> check (Error.code error = "invalid_group") message
  | Ok _ -> fail (message ^ ": unexpectedly succeeded")

let box minimum maximum = Group_ops.Bounds_box { minimum; maximum }
let sphere center radius = Group_ops.Bounds_sphere { center; radius }

let reference_box minimum maximum (ax, ay, az) (bx, by, bz) =
  let first = ref 0. and last = ref 1. and hit = ref true in
  let axis a b low high =
    let direction = b -. a in
    if direction = 0. then begin
      if a < low || a > high then hit := false
    end
    else begin
      let enter = (low -. a) /. direction and leave = (high -. a) /. direction in
      let enter, leave = if enter <= leave then (enter, leave) else (leave, enter) in
      if enter > !first then first := enter;
      if leave < !last then last := leave;
      if !first > !last then hit := false
    end
  in
  axis ax bx minimum.Rays_math.Vec3.x maximum.Rays_math.Vec3.x;
  axis ay by minimum.y maximum.y;
  axis az bz minimum.z maximum.z;
  !hit

let reference_sphere center radius (ax, ay, az) (bx, by, bz) =
  let dx = bx -. ax and dy = by -. ay and dz = bz -. az in
  let denominator = (dx *. dx) +. (dy *. dy) +. (dz *. dz) in
  let parameter =
    if denominator = 0. then 0.
    else
      max 0.
        (min 1.
           ((((center.Rays_math.Vec3.x -. ax) *. dx)
            +. ((center.y -. ay) *. dy)
            +. ((center.z -. az) *. dz))
           /. denominator))
  in
  let x = ax +. (parameter *. dx) -. center.x
  and y = ay +. (parameter *. dy) -. center.y
  and z = az +. (parameter *. dz) -. center.z in
  (x *. x) +. (y *. y) +. (z *. z) <= radius *. radius

let same_group left right =
  Group.owner left = Group.owner right
  && Group.length left = Group.length right
  && members left = members right

let run () = print_endline "group bounds tests passed"
