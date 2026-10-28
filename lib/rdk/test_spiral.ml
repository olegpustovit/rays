open Rays
open Rdk
open Rdk_test_support

let near ?(epsilon = 1e-9) left right = abs_float (left -. right) <= epsilon

(* v + 2w (q x v) + 2 q x (q x v) for a unit quaternion. *)
let rotate (q : Rays_math.Quat.t) (v : Rays_math.Vec3.t) =
  let cross (ax, ay, az) (bx, by, bz) =
    ((ay *. bz) -. (az *. by), (az *. bx) -. (ax *. bz), (ax *. by) -. (ay *. bx))
  in
  let u = (q.x, q.y, q.z) in
  let tx, ty, tz = cross u (v.x, v.y, v.z) in
  let cx, cy, cz = cross u (tx, ty, tz) in
  Rays_math.Vec3.create
    (v.x +. (2. *. ((q.w *. tx) +. cx)))
    (v.y +. (2. *. ((q.w *. ty) +. cy)))
    (v.z +. (2. *. ((q.w *. tz) +. cz)))

let float_attribute geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Float values -> values
      | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing attribute " ^ name)

let float3_attribute geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Float3 values -> Packed.Float3.View.view values
      | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing attribute " ^ name)

let float4_attribute geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Float4 values -> Packed.Float4.View.view values
      | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing attribute " ^ name)

let run () = print_endline "Spiral tests passed"
