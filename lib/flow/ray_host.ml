type hit = {
  t : float;
  prim : int;
  u : float;
  v : float;
  nx : float;
  ny : float;
  nz : float;
  hit : bool;
  instance : int;
}

let miss =
  { t = 0.; prim = -1; u = 0.; v = 0.; nx = 0.; ny = 0.; nz = 0.; hit = false; instance = -1 }

(* ponytail: one process-wide id registry; replace with a typed scene value carried by the evaluation
   when two hosts in one process need overlapping ids. *)
module M = Map.Make (Int)

let scenes : (float array -> hit) M.t Atomic.t = Atomic.make M.empty

let rec update f =
  let old = Atomic.get scenes in
  if not (Atomic.compare_and_set scenes old (f old)) then update f

let register id closure = update (M.add id closure)
let unregister id = update (M.remove id)

let query id args =
  match M.find_opt id (Atomic.get scenes) with
  | Some closure -> closure args
  | None ->
      Value.fail "E_TRACE" (Printf.sprintf "scene/trace: no scene is registered with id %d." id)

(* The inverse of a row-major 3x4 affine transform. *)
let world_to_object m =
  let a = m.(0)
  and b = m.(1)
  and c = m.(2)
  and d = m.(4)
  and e = m.(5)
  and f = m.(6)
  and g = m.(8)
  and h = m.(9)
  and i = m.(10) in
  let det =
    (a *. ((e *. i) -. (f *. h))) -. (b *. ((d *. i) -. (f *. g))) +. (c *. ((d *. h) -. (e *. g)))
  in
  let r =
    [|
      (e *. i) -. (f *. h);
      (c *. h) -. (b *. i);
      (b *. f) -. (c *. e);
      (f *. g) -. (d *. i);
      (a *. i) -. (c *. g);
      (c *. d) -. (a *. f);
      (d *. h) -. (e *. g);
      (b *. g) -. (a *. h);
      (a *. e) -. (b *. d);
    |]
  in
  let r = Array.map (fun x -> x /. det) r in
  let t k = -.((r.(3 * k) *. m.(3)) +. (r.((3 * k) + 1) *. m.(7)) +. (r.((3 * k) + 2) *. m.(11))) in
  [| r.(0); r.(1); r.(2); t 0; r.(3); r.(4); r.(5); t 1; r.(6); r.(7); r.(8); t 2 |]

(* Motion keyframes span the shutter [0, 1] and clamp outside it, as Metal's motion structures do. *)
let shutter time = Float.min 1. (Float.max 0. time)
let lerp a b time = Array.map2 (fun x y -> x +. ((y -. x) *. time)) a b
