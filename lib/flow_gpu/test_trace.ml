(* P8.S2: scene/trace on Metal. The emitted `intersector<triangle_data>` (flat scene) and
   `intersector<triangle_data, instancing>` (instanced scene, with the per-instance vertex-base and
   world-to-object table) over structures from Run.build_scene match the CPU host: prim, instance and hit
   exactly; t, uv and normal within 1e-5. Procedural spheres ride along. Motion (WN10): vertex keyframes
   (flat, `primitive_motion`) and instance keyframes (`instance_motion`) match the CPU host at several times,
   and without Ray_tracing_motion the static kernel over the shutter-midpoint scene matches it at time 0.5.
   Strands: Metal curve primitives and, without Ray_tracing_curves, the in-loop capsule test match it too. *)
open Rdk
open Rdk_test_support
module B = Ogpu.Backend
module R = Flow_gpu.Run
open Test_support

let count = 256
let spheres = [| (0.3, 0.2, 1.0, 0.4); (-0.5, -0.4, 0.5, 0.3) |]

let grid size =
  match
    Plane_generators.grid ~counts:Grid_point_counts ~connectivity:Grid_triangles
      ~orientation:Grid_xy ~columns:2 ~rows:2 ~size ()
  with
  | Ok g -> g
  | Error e -> failwith (Error.to_string e)

(* the two triangles of a prototype in primitive order, as packed float3s *)
let vertex_bytes geometry =
  let index =
    match Surface_index.create geometry with Ok i -> i | Error e -> failwith (Error.to_string e)
  in
  assert (Surface_index.triangle_count index = 2);
  let positions = Geometry.positions geometry and bytes = Bytes.create 72 in
  for triangle = 0 to 1 do
    let prim = Surface_index.View.triangle_primitive index triangle in
    for corner = 0 to 2 do
      let x, y, z =
        Packed.Float3.get positions (Surface_index.View.triangle_point index triangle corner)
      in
      List.iteri
        (fun k v ->
          Bytes.set_int32_le bytes (((prim * 9) + (corner * 3) + k) * 4) (Int32.bits_of_float v))
        [ x; y; z ]
    done
  done;
  bytes

let floats values =
  let bytes = Bytes.create (4 * Array.length values) in
  Array.iteri (fun k v -> Bytes.set_int32_le bytes (4 * k) (Int32.bits_of_float v)) values;
  bytes

let sphere_data =
  floats (Array.concat (List.map (fun (x, y, z, r) -> [| x; y; z; r |]) (Array.to_list spheres)))

let program id =
  Test_program.compile
    (Printf.sprintf
       "(map (fn [(i : int)] (let* [x (- (* 0.15 (float (int/mod i 16))) 1.2) y (- (* 0.15 (float \
        (int/div i 16))) 1.17)\n\
       \      h (scene/trace %d [x y 5.] [0. 0. -1.] 0. 100. t)]\n\
       \    [h.t (float h.prim) (+ (float h.instance) (+ h.u h.v))\n\
       \     (if h.hit (+ 1. (+ (* h.normal.x 0.3) (+ (* h.normal.y 0.6) (* h.normal.z \
        h.normal.z)))) 0.)])) (array/range %d :type int))"
       id count)

let values program time =
  ok (Flow_ir.Packed.Host.prepare ~fast:true program ~live:(Input.at_time time))

let cpu program time =
  match ok (Flow_ir.Packed.force program ~live:(Input.at_time time)) with
  | Flow.Eval.Vec4_array v -> v
  | _ -> assert false

(* the GPU run at each [time] against the CPU host at [cpu_time time] *)
let compare_runs ?caps ?(cpu_time = Fun.id) ?(tolerance = 1e-5) name lease id description times =
  let built = ok (R.build_scene ?caps lease description) in
  let program = program id in
  let pipelines = Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday (Renderer.gpu_device lease) in
  let msl =
    ok
      (Flow_gpu.Emit.kernel ~fast:true ~instanced:built.instanced ~motion:built.motion
         ?curves:built.curves program)
  in
  let runner = R.create lease pipelines msl in
  List.iter
    (fun time ->
      let expected = cpu program (cpu_time time) in
      let actual =
        match
          ok
            (R.readback
               (ok
                  (R.dispatch runner ~scene:built.scene ~extras:built.extras (values program time))))
        with
        | Flow.Eval.Vec4_array v -> v
        | _ -> assert false
      in
      let worst = ref 0. and hits = ref 0 in
      for i = 0 to count - 1 do
        if expected.((4 * i) + 3) <> 0. then incr hits;
        (* prim and hit/miss agree exactly; the instance (z's integer part) within the 1e-5 below *)
        let same =
          actual.((4 * i) + 1) = expected.((4 * i) + 1)
          && actual.((4 * i) + 3) <> 0. = (expected.((4 * i) + 3) <> 0.)
        in
        if not same then
          Printf.printf "ray %d: gpu %g %g %g %g cpu %g %g %g %g\n" i
            actual.(4 * i)
            actual.((4 * i) + 1)
            actual.((4 * i) + 2)
            actual.((4 * i) + 3)
            expected.(4 * i)
            expected.((4 * i) + 1)
            expected.((4 * i) + 2)
            expected.((4 * i) + 3);
        assert same;
        for k = 0 to 3 do
          if Float.abs (actual.((4 * i) + k) -. expected.((4 * i) + k)) > tolerance then
            Printf.printf "ray %d field %d: gpu %.7g cpu %.7g\n" i k
              actual.((4 * i) + k)
              expected.((4 * i) + k);
          worst := Float.max !worst (Float.abs (actual.((4 * i) + k) -. expected.((4 * i) + k)))
        done
      done;
      Printf.printf
        "scene/trace Metal (%s, time %g): %d of %d rays hit, prim exact, max abs difference %.3g\n"
        name time !hits count !worst;
      assert (!hits > 0 && !worst <= tolerance))
    times;
  R.close runner;
  Flow_gpu.Pipelines.close pipelines;
  built.release ();
  built

let tilted ~angle ~shift geometry =
  Transform_ops_front.transform
    (Rays_math.Mat4.mul
       (Rays_math.Mat4.translation (Rays_math.Vec3.create shift 0.1 0.5))
       (Rays_math.Mat4.rotation_x angle))
    geometry

let flat lease =
  let a = grid 2. in
  (match Flow_sop.Trace_scene.register ~spheres 9 a with Ok () -> () | Error e -> failwith e);
  ignore
    (compare_runs "flat, triangles + spheres" lease 9
       {
         R.triangles = vertex_bytes a;
         triangles_end = None;
         instances = [||];
         sphere_data;
         strand_data = Bytes.empty;
       }
       [ 1. ]);
  let ending = tilted ~angle:0.5 ~shift:0.3 a in
  (match Flow_sop.Trace_scene.register ~spheres ~ending 9 a with
  | Ok () -> ()
  | Error e -> failwith e);
  let moving =
    {
      R.triangles = vertex_bytes a;
      triangles_end = Some (vertex_bytes ending);
      instances = [||];
      sphere_data;
      strand_data = Bytes.empty;
    }
  in
  assert (compare_runs "flat, vertex motion" lease 9 moving [ 0.; 0.35; 1.; 1.5 ]).motion;
  let caps = { (B.capabilities (Renderer.gpu_device lease)) with ray_tracing_motion = false } in
  assert (
    not
      (compare_runs ~caps
         ~cpu_time:(fun _ -> 0.5)
         "flat, shutter midpoint without motion" lease 9 moving [ 0.; 1. ])
        .motion);
  Flow_sop.Trace_scene.unregister 9

(* strands over the flat grid: a tilted span, a long thin one, and an upright one whose top cap the rays meet *)
let strands =
  [|
    (-1.3, 0.1, 1.0, 1.3, 0.35, 1.2, 0.08);
    (0.6, -1.0, 0.8, 0.7, 0.9, 0.5, 0.05);
    (-0.45, -0.6, 0.2, -0.45, -0.6, 1.5, 0.1);
  |]

let stranded lease =
  let a = grid 2. in
  (match Flow_sop.Trace_scene.register ~spheres ~strands 13 a with
  | Ok () -> ()
  | Error e -> failwith e);
  let strand_data =
    floats
      (Array.concat
         (List.map
            (fun (ax, ay, az, bx, by, bz, r) -> [| ax; ay; az; r; bx; by; bz; r |])
            (Array.to_list strands)))
  in
  let description =
    {
      R.triangles = vertex_bytes a;
      triangles_end = None;
      instances = [||];
      sphere_data;
      strand_data;
    }
  in
  let caps = B.capabilities (Renderer.gpu_device lease) in
  if Ogpu.Caps.has caps Ray_tracing_curves then
    assert (
      (compare_runs ~tolerance:1e-4 "flat + native curve strands" lease 13 description [ 1. ])
        .curves = Some `Native)
  else begin
    (* below Apple GPU family 9 curves do not intersect; their kernel still compiles to a pipeline *)
    let pipelines =
      Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday (Renderer.gpu_device lease)
    in
    let msl = ok (Flow_gpu.Emit.kernel ~fast:true ~curves:`Native (program 13)) in
    ignore (ok (Flow_gpu.Pipelines.get pipelines msl));
    Flow_gpu.Pipelines.close pipelines;
    print_endline
      "scene/trace Metal (flat + native curve strands): compiled; the device lacks \
       Ray_tracing_curves"
  end;
  assert (
    (compare_runs
       ~caps:{ caps with ray_tracing_curves = false }
       "flat + capsule strands" lease 13 description [ 1. ])
      .curves = Some `Loop);
  Flow_sop.Trace_scene.unregister 13

let instanced lease =
  let a = grid 2. and b = grid 1. in
  let rotation = 0.5 and tilt = 0.4 in
  let transforms =
    [|
      [| 1.; 0.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 1.; 0. |];
      (* Rx(tilt) * diag(1, 2, 1), then Rz(rotation), then a translation: non-uniform, so the normal needs the
       inverse transpose *)
      (let c = cos tilt and s = sin tilt and cr = cos rotation and sr = sin rotation in
       let mul x y =
         Array.init 9 (fun k ->
             let r = k / 3 and q = k mod 3 in
             (x.(3 * r) *. y.(q)) +. (x.((3 * r) + 1) *. y.(3 + q)) +. (x.((3 * r) + 2) *. y.(6 + q)))
       in
       let m =
         mul
           [| cr; -.sr; 0.; sr; cr; 0.; 0.; 0.; 1. |]
           (mul [| 1.; 0.; 0.; 0.; c; -.s; 0.; s; c |] [| 1.; 0.; 0.; 0.; 2.; 0.; 0.; 0.; 1. |])
       in
       [| m.(0); m.(1); m.(2); 0.3; m.(3); m.(4); m.(5); 0.2; m.(6); m.(7); m.(8); 1.0 |]);
      [| 0.4; 0.; 0.; -1.0; 0.; 0.4; 0.; -0.8; 0.; 0.; 0.4; -0.5 |];
    |]
  in
  (* the shutter's end: the first instance tilts and slides, the second squashes, the third stays *)
  let ends =
    [|
      [| 1.; 0.; 0.; 0.4; 0.; 0.8; -0.6; 0.; 0.; 0.6; 0.8; 0.3 |];
      Array.mapi (fun k v -> if k = 4 || k = 5 || k = 6 then v *. 0.5 else v) transforms.(1);
      transforms.(2);
    |]
  in
  let prototypes = [| a; b; a |] in
  let instances transform_end =
    Array.mapi
      (fun k transform ->
        {
          R.first = (if prototypes.(k) == b then 2 else 0);
          triangles = 2;
          transform;
          transform_end = transform_end k;
        })
      transforms
  in
  let description transform_end =
    {
      R.triangles = Bytes.cat (vertex_bytes a) (vertex_bytes b);
      triangles_end = None;
      instances = instances transform_end;
      sphere_data;
      strand_data = Bytes.empty;
    }
  in
  let register ?motion () =
    match
      Flow_sop.Trace_scene.register_instances ~spheres ?motion 11
        (Array.mapi (fun k t -> (prototypes.(k), t)) transforms)
    with
    | Ok () -> ()
    | Error e -> failwith e
  in
  register ();
  ignore
    (compare_runs "instanced, three instances of two prototypes + spheres" lease 11
       (description (fun _ -> None))
       [ 1. ]);
  register ~motion:ends ();
  let moving = description (fun k -> Some ends.(k)) in
  assert (compare_runs "instanced, instance motion" lease 11 moving [ 0.; 0.25; 0.7; 1. ]).motion;
  let caps = { (B.capabilities (Renderer.gpu_device lease)) with ray_tracing_motion = false } in
  assert (
    not
      (compare_runs ~caps
         ~cpu_time:(fun _ -> 0.5)
         "instanced, shutter midpoint without motion" lease 11 moving [ 0.3 ])
        .motion);
  Flow_sop.Trace_scene.unregister 11

let () =
  match Renderer.acquire_gpu () with
  | Error e ->
      Format.eprintf "%a@." Renderer.pp_error e;
      exit 2
  | Ok lease ->
      Fun.protect
        ~finally:(fun () -> Renderer.release_gpu lease)
        (fun () ->
          let caps = B.capabilities (Renderer.gpu_device lease) in
          if not (Ogpu.Caps.has caps Ray_tracing) then
            print_endline "SKIP: backend lacks Ray_tracing"
          else if not (Ogpu.Caps.has caps Ray_tracing_motion) then
            print_endline "SKIP: backend lacks Ray_tracing_motion"
          else begin
            flat lease;
            stranded lease;
            instanced lease
          end)
