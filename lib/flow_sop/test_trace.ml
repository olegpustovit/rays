(* scene/trace on the CPU. A .rays kernel traces a lattice of rays at a two-triangle quad; the
   Packed kernel is bit-identical to the reference walker at 1 and 8 domains, and its t and prim match a
   direct Surface_index.raycast. *)
open Rdk
open Rdk_test_support
module P = Flow_ir.Packed
open Test_support

let id = 7
let count = 256

let x i = (0.15 *. float (i mod 16)) -. 1.2
and y i = (0.15 *. float (i / 16)) -. 1.2

let () =
  let quad =
    Plane_generators.grid ~counts:Grid_point_counts ~connectivity:Grid_quads ~orientation:Grid_xy
      ~columns:2 ~rows:2 ~size:2. ()
    |> get_ok
  in
  (match Flow_sop.Trace_scene.register id quad with Ok () -> () | Error e -> failwith e);
  let program =
    Test_program.compile
      (Printf.sprintf
         "(map (fn [(i : int)] (let* [x (- (* 0.15 (float (int/mod i 16))) 1.2) y (- (* 0.15 \
          (float (int/div i 16))) 1.2)\n\
         \        h (scene/trace %d [x y 5.] [0. 0. -1.] 0. 100. t)]\n\
         \      [h.t (float h.prim) (+ h.u h.v) (if h.hit (+ 1. (* h.normal.z h.normal.z)) 0.)])) \
          (array/range %d :type int))"
         id count)
  in
  let live = Input.at_time 1. in
  let bytes v = Marshal.to_string v [ Marshal.No_sharing ] in
  let reference = ok (P.Host.reference program ~live) in
  List.iter
    (fun domains ->
      Parallel.run ~domains (fun () ->
          assert (bytes (ok (P.force ~strict:true program ~live)) = bytes reference)))
    [ 1; 8 ];
  let values = match reference with Flow.Eval.Vec4_array v -> v | _ -> assert false in
  let index = Result.get_ok (Surface_index.create quad) and hits = ref 0 in
  for i = 0 to count - 1 do
    let direct =
      Surface_index.raycast index
        ~origin:(Rays_math.Vec3.create (x i) (y i) 5.)
        ~direction:(Rays_math.Vec3.create 0. 0. (-1.))
      |> get_ok
    in
    match direct with
    | Some hit ->
        incr hits;
        assert (
          values.(4 * i) = hit.distance
          && values.((4 * i) + 1) = float hit.primitive
          && values.((4 * i) + 3) > 0.)
    | None -> assert (values.((4 * i) + 3) = 0. && values.((4 * i) + 1) = -1.)
  done;
  assert (!hits > 0 && !hits < count);
  (* one host query per ray, not one per field *)
  let calls = Atomic.make 0 in
  Flow.Ray_host.register 8 (fun q ->
      Atomic.incr calls;
      Flow.Ray_host.query id q);
  let counted =
    Test_program.compile
      (Printf.sprintf
         "(map (fn [(i : int)] (let* [h (scene/trace 8 [(* 0.01 (float i)) 0. 5.] [0. 0. -1.] 0. \
          100. t)] [h.t (float h.prim) h.u h.normal.z])) (array/range %d :type int))"
         count)
  in
  ignore (ok (P.force ~strict:true counted ~live));
  assert (Atomic.get calls = count);
  Flow.Ray_host.unregister 8;
  (* flow_web refuses it with a typed diagnostic *)
  (match Flow_web.Js.region program with
  | _ -> assert false
  | exception Diagnostic.Error d -> assert (d.code = "E_WEB_SOURCE"));
  (* vertex motion with an unmoved end keyframe intersects directly: the same prim, t and uv *)
  (match Flow_sop.Trace_scene.register ~ending:quad 9 quad with
  | Ok () -> ()
  | Error e -> failwith e);
  List.iter
    (fun i ->
      let q = [| x i; y i; 5.; 0.; 0.; -1.; 0.; 100.; 0.3 |] in
      let a = Flow.Ray_host.query id q and b = Flow.Ray_host.query 9 q in
      assert (
        a.hit = b.hit && a.prim = b.prim
        && Float.abs (a.t -. b.t) < 1e-12
        && Float.abs (a.u -. b.u) < 1e-12
        && Float.abs (a.v -. b.v) < 1e-12
        && a.nz = b.nz))
    (List.init count Fun.id);
  Flow_sop.Trace_scene.unregister 9;
  Flow_sop.Trace_scene.unregister id;
  Printf.printf
    "scene/trace CPU: %d of %d rays hit; Packed = walker at 1 and 8 domains; t and prim = \
     Surface_index.raycast\n"
    !hits count
