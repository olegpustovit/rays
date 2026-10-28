(* P8.S2: scene/trace on Metal. The emitted `intersector<triangle_data>` (flat scene) and
   `intersector<triangle_data, instancing>` (instanced scene, with the per-instance vertex-base and
   world-to-object table) over real OGPU acceleration structures match the CPU host: prim, instance and hit
   exactly; t, uv and normal within 1e-5. Procedural spheres ride along in both. *)
open Rdk
module B = Ogpu.Backend
let ok = function Ok v -> v | Error d -> failwith (Diagnostic.to_string d)
let gpu r = match r with Ok v -> v | Error e -> failwith (Ogpu.Error.to_string e)
let count = 256
let spheres = [| (0.3, 0.2, 1.0, 0.4); (-0.5, -0.4, 0.5, 0.3) |]
let sphere_total = Array.length spheres
let grid size = match Plane_generators.grid ~counts:Grid_point_counts ~connectivity:Grid_triangles
    ~orientation:Grid_xy ~columns:2 ~rows:2 ~size () with Ok g -> g | Error e -> failwith (Error.to_string e)
(* the two triangles of a prototype in primitive order, as packed float3s *)
let vertex_bytes geometry =
  let index = match Surface_index.create geometry with Ok i -> i | Error e -> failwith (Error.to_string e) in
  assert (Surface_index.triangle_count index = 2);
  let positions = Geometry.positions geometry and bytes = Bytes.create 72 in
  for triangle = 0 to 1 do
    let prim = Surface_index.Private.triangle_primitive index triangle in
    for corner = 0 to 2 do
      let x, y, z = Packed.Float3.get positions (Surface_index.Private.triangle_point index triangle corner) in
      List.iteri (fun k v -> Bytes.set_int32_le bytes ((prim * 9 + corner * 3 + k) * 4) (Int32.bits_of_float v)) [x; y; z]
    done
  done;
  bytes
let floats values = let bytes = Bytes.create (4 * List.length values) in
  List.iteri (fun k v -> Bytes.set_int32_le bytes (4 * k) (Int32.bits_of_float v)) values; bytes
let program id = Test_program.compile (Printf.sprintf
  "(map (fn [(i : int)] (let* [x (- (* 0.15 (float (int/mod i 16))) 1.2) y (- (* 0.15 (float (int/div i 16))) 1.17)
      h (scene/trace %d [x y 5.] [0. 0. -1.] 0. 100. t)]
    [h.t (float h.prim) (+ (float h.instance) (+ h.u h.v))
     (if h.hit (+ 1. (+ (* h.normal.x 0.3) (+ (* h.normal.y 0.6) (* h.normal.z h.normal.z)))) 0.)])) (array/range %d :type int))" id count)
let compare_runs name ~instanced program lease device queue ~expected ~make =
  let buffer ?(memory = Ogpu.Types.Shared) size = gpu (B.create_buffer ~memory device
    { label = Some "trace test"; size = Int64.of_int size; usage = [ Ogpu.Types.Storage ] }) in
  let upload bytes = let b = buffer (Bytes.length bytes) in gpu (B.write_buffer b ~offset:0L bytes); b in
  let build structures =
    let scratch = buffer ~memory:Ogpu.Types.Device_local
      (List.fold_left (fun m s -> max m (Int64.to_int (B.accel_sizes s).build_scratch_size)) 16 structures) in
    List.iter (fun structure ->
      let commands = gpu (B.begin_commands queue) in
      let encoder = gpu (B.accel_encoder commands) in
      gpu (B.build_accel encoder structure ~scratch ());
      gpu (B.end_accel encoder);
      gpu (B.complete_through queue (gpu (B.commit commands)).epoch)) structures;
    scratch in
  let scene, cleanup = make ~device ~upload ~build in
  let pipelines = Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday device in
  let msl = ok (Flow_gpu.Emit.kernel ~fast:true ~instanced program) in
  assert (msl.scene <> None);
  let runner = Flow_gpu.Run.create lease pipelines msl in
  let live = Input.at_time 1. in
  let values = ok (Flow_ir.Packed.Private.prepare ~fast:true program ~live) in
  let actual = match ok (Flow_gpu.Run.readback (ok (Flow_gpu.Run.dispatch runner ~scene values))) with
    | Flow.Eval.Vec4_array v -> v | _ -> assert false in
  let worst = ref 0. and hits = ref 0 in
  for i = 0 to count - 1 do
    if expected.(4 * i + 3) <> 0. then incr hits;
    (* prim, the instance (in the integer part of z) and hit/miss agree exactly *)
    if actual.(4 * i + 1) <> expected.(4 * i + 1) || (actual.(4 * i + 3) <> 0.) <> (expected.(4 * i + 3) <> 0.)
       || Float.round actual.(4 * i + 2) <> Float.round expected.(4 * i + 2) then
      Printf.printf "ray %d: gpu %g %g %g %g cpu %g %g %g %g\n" i actual.(4*i) actual.(4*i+1) actual.(4*i+2) actual.(4*i+3)
        expected.(4*i) expected.(4*i+1) expected.(4*i+2) expected.(4*i+3);
    assert (actual.(4 * i + 1) = expected.(4 * i + 1) && (actual.(4 * i + 3) <> 0.) = (expected.(4 * i + 3) <> 0.));
    for k = 0 to 3 do worst := Float.max !worst (Float.abs (actual.(4 * i + k) -. expected.(4 * i + k))) done
  done;
  Printf.printf "scene/trace Metal (%s): %d of %d rays hit, prim exact, max abs difference %.3g\n" name !hits count !worst;
  assert (!hits > 0 && !worst <= 1e-5);
  Flow_gpu.Run.close runner; Flow_gpu.Pipelines.close pipelines; cleanup ()
let cpu program = match ok (Flow_ir.Packed.force program ~live:(Input.at_time 1.)) with
  | Flow.Eval.Vec4_array v -> v | _ -> assert false
let sphere_buffer upload = upload (floats (List.concat_map (fun (x, y, z, r) -> [x; y; z; r]) (Array.to_list spheres)))
let flat lease device queue =
  let a = grid 2. in
  (match Flow_sop.Trace_scene.register ~spheres 9 a with Ok () -> () | Error e -> failwith e);
  let program = program 9 in
  compare_runs "flat, triangles + spheres" ~instanced:false program lease device queue ~expected:(cpu program)
    ~make:(fun ~device ~upload ~build ->
      let vertices = upload (vertex_bytes a) in
      let blas = gpu (B.create_accel device (B.Blas { geometries = [ B.Triangles
        { vertices; offset = 0L; length = 72L; vertex_stride = 12; vertex_count = 6 } ] })) in
      let scratch = build [ blas ] in
      let spheres = sphere_buffer upload in
      ({ Flow_gpu.Run.accel = blas; vertices; spheres; sphere_count = sphere_total; instances = None },
       fun () -> ignore (B.destroy_accel blas); List.iter (fun b -> ignore (B.destroy_buffer b)) [ scratch; vertices; spheres ]));
  Flow_sop.Trace_scene.unregister 9
let instanced lease device queue =
  let a = grid 2. and b = grid 1. in
  let rotation = 0.5 and tilt = 0.4 in
  let transforms = [|
    [| 1.; 0.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 1.; 0. |];
    (* Rx(tilt) * diag(1, 2, 1), then Rz(rotation), then a translation: non-uniform, so the normal needs the
       inverse transpose *)
    (let c = cos tilt and s = sin tilt and cr = cos rotation and sr = sin rotation in
     let mul x y = Array.init 9 (fun k -> let r = k / 3 and q = k mod 3 in
       (x.(3 * r) *. y.(q)) +. (x.(3 * r + 1) *. y.(3 + q)) +. (x.(3 * r + 2) *. y.(6 + q))) in
     let m = mul [| cr; -.sr; 0.; sr; cr; 0.; 0.; 0.; 1. |] (mul [| 1.; 0.; 0.; 0.; c; -.s; 0.; s; c |] [| 1.; 0.; 0.; 0.; 2.; 0.; 0.; 0.; 1. |]) in
     [| m.(0); m.(1); m.(2); 0.3; m.(3); m.(4); m.(5); 0.2; m.(6); m.(7); m.(8); 1.0 |]);
    [| 0.4; 0.; 0.; -1.0; 0.; 0.4; 0.; -0.8; 0.; 0.; 0.4; -0.5 |] |] in
  let prototypes = [| a; b; a |] in
  (match Flow_sop.Trace_scene.register_instances ~spheres 11 (Array.mapi (fun k t -> (prototypes.(k), t)) transforms) with
   | Ok () -> () | Error e -> failwith e);
  let program = program 11 in
  compare_runs "instanced, three instances of two prototypes + spheres" ~instanced:true program lease device queue
    ~expected:(cpu program)
    ~make:(fun ~device ~upload ~build ->
      let vertices = upload (Bytes.cat (vertex_bytes a) (vertex_bytes b)) in
      let blas offset = gpu (B.create_accel device (B.Blas { geometries = [ B.Triangles
        { vertices; offset = Int64.of_int offset; length = 72L; vertex_stride = 12; vertex_count = 6 } ] })) in
      let blas_a = blas 0 and blas_b = blas 72 in
      let records = gpu (B.pack_instance_records device (Array.mapi (fun k t ->
        { B.instance = { transform = t; mask = 0xFFFF_FFFF; structure_index = (if prototypes.(k) == b then 1 else 0) };
          user_id = k; table_offset = 0 }) transforms)) in
      let instances = upload records in
      let tlas = gpu (B.create_accel device (B.Tlas_of { instances; offset = 0L; instance_count = Array.length transforms;
        kind = B.User_id_instances; structures = [ blas_a; blas_b ]; motion_transforms = None })) in
      let scratch = build [ blas_a; blas_b ] in
      let tlas_scratch = build [ tlas ] in
      let table = upload (floats (List.concat_map (fun k ->
        (if prototypes.(k) == b then 2. else 0.) :: (Array.to_list (Flow_sop.Trace_scene.world_to_object transforms.(k)) @ [0.; 0.; 0.]))
        [ 0; 1; 2 ])) in
      let spheres = sphere_buffer upload in
      ({ Flow_gpu.Run.accel = tlas; vertices; spheres; sphere_count = sphere_total; instances = Some table },
       fun () -> List.iter (fun s -> ignore (B.destroy_accel s)) [ tlas; blas_a; blas_b ];
         List.iter (fun x -> ignore (B.destroy_buffer x)) [ scratch; tlas_scratch; vertices; spheres; instances; table ]));
  Flow_sop.Trace_scene.unregister 11
let () =
  match Renderer.acquire_gpu () with
  | Error e -> Format.eprintf "%a@." Renderer.pp_error e; exit 2
  | Ok lease -> Fun.protect ~finally:(fun () -> Renderer.release_gpu lease) (fun () ->
      let device = Renderer.gpu_device lease and queue = Renderer.gpu_queue lease in
      if not (Ogpu.Caps.has (B.capabilities device) Ray_tracing) then print_endline "SKIP: backend lacks Ray_tracing"
      else begin flat lease device queue; instanced lease device queue end)
