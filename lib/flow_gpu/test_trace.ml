(* P8.S2(b): scene/trace on Metal. The emitted `intersector<triangle_data>` over a real OGPU acceleration
   structure of two triangles matches the CPU host (Surface_index): prim and hit exactly, t, uv and normal
   within 1e-5. *)
open Rdk
module B = Ogpu.Backend
let ok = function Ok v -> v | Error d -> failwith (Diagnostic.to_string d)
let gpu r = match r with Ok v -> v | Error e -> failwith (Ogpu.Error.to_string e)
let id = 9 and count = 256
let spheres = [| (0.3, 0.2, 1.0, 0.4); (-0.5, -0.4, 0.5, 0.3) |]
let () =
  let grid = match Plane_generators.grid ~counts:Grid_point_counts ~connectivity:Grid_triangles
      ~orientation:Grid_xy ~columns:2 ~rows:2 ~size:2. () with Ok g -> g | Error e -> failwith (Error.to_string e) in
  (match Flow_sop.Trace_scene.register ~spheres id grid with Ok () -> () | Error e -> failwith e);
  let index = match Surface_index.create grid with Ok i -> i | Error e -> failwith (Error.to_string e) in
  assert (Surface_index.triangle_count index = 2);
  let positions = Geometry.positions grid in
  let vertices = Bytes.create 72 in
  for triangle = 0 to 1 do
    let prim = Surface_index.Private.triangle_primitive index triangle in
    for corner = 0 to 2 do
      let x, y, z = Packed.Float3.get positions (Surface_index.Private.triangle_point index triangle corner) in
      List.iteri (fun k v -> Bytes.set_int32_le vertices ((prim * 9 + corner * 3 + k) * 4) (Int32.bits_of_float v)) [x; y; z]
    done
  done;
  let program = Test_program.compile (Printf.sprintf
    "(map (fn [(i : int)] (let* [x (- (* 0.15 (float (int/mod i 16))) 1.2) y (- (* 0.15 (float (int/div i 16))) 1.17)
        h (scene/trace %d [x y 5.] [0. 0. -1.] 0. 100. t)]
      [h.t (float h.prim) (+ h.u h.v) (if h.hit (+ 1. (* h.normal.z h.normal.z)) 0.)])) (array/range %d :type int))" id count) in
  let live = Input.at_time 1. in
  let expected = match ok (Flow_ir.Packed.force program ~live) with Flow.Eval.Vec4_array v -> v | _ -> assert false in
  match Renderer.acquire_gpu () with
  | Error e -> Format.eprintf "%a@." Renderer.pp_error e; exit 2
  | Ok lease -> Fun.protect ~finally:(fun () -> Renderer.release_gpu lease) (fun () ->
      let device = Renderer.gpu_device lease and queue = Renderer.gpu_queue lease in
      if not (Ogpu.Caps.has (B.capabilities device) Ray_tracing) then print_endline "SKIP: backend lacks Ray_tracing"
      else begin
        let buffer ?(memory = Ogpu.Types.Shared) size = gpu (B.create_buffer ~memory device
          { label = Some "trace test"; size = Int64.of_int size; usage = [ Ogpu.Types.Storage ] }) in
        let vertex_buffer = buffer 72 in
        let sphere_buffer = buffer 32 in
        let sphere_bytes = Bytes.create 32 in
        Array.iteri (fun k (x, y, z, r) -> List.iteri (fun c v -> Bytes.set_int32_le sphere_bytes ((k * 4 + c) * 4) (Int32.bits_of_float v)) [x; y; z; r]) spheres;
        gpu (B.write_buffer sphere_buffer ~offset:0L sphere_bytes);
        gpu (B.write_buffer vertex_buffer ~offset:0L vertices);
        let blas = gpu (B.create_accel device (B.Blas { geometries = [ B.Triangles
          { vertices = vertex_buffer; offset = 0L; length = 72L; vertex_stride = 12; vertex_count = 6 } ] })) in
        let scratch = buffer ~memory:Ogpu.Types.Device_local (Int64.to_int (B.accel_sizes blas).build_scratch_size) in
        let commands = gpu (B.begin_commands queue) in
        let encoder = gpu (B.accel_encoder commands) in
        gpu (B.build_accel encoder blas ~scratch ());
        gpu (B.end_accel encoder);
        let receipt = gpu (B.commit commands) in
        gpu (B.complete_through queue receipt.epoch);
        let pipelines = Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday device in
        let msl = ok (Flow_gpu.Emit.kernel ~fast:true program) in
        assert (msl.scene <> None);
        let runner = Flow_gpu.Run.create lease pipelines msl in
        let values = ok (Flow_ir.Packed.Private.prepare ~fast:true program ~live) in
        let actual = match ok (Flow_gpu.Run.readback (ok (Flow_gpu.Run.dispatch runner ~scene:(blas, vertex_buffer, sphere_buffer, Array.length spheres) values))) with
          | Flow.Eval.Vec4_array v -> v | _ -> assert false in
        let worst = ref 0. and hits = ref 0 in
        for i = 0 to count - 1 do
          if expected.(4 * i + 3) <> 0. then incr hits;
          if actual.(4 * i + 1) <> expected.(4 * i + 1) || (actual.(4 * i + 3) <> 0.) <> (expected.(4 * i + 3) <> 0.) then Printf.printf "ray %d: gpu %g %g %g %g cpu %g %g %g %g\n" i actual.(4*i) actual.(4*i+1) actual.(4*i+2) actual.(4*i+3) expected.(4*i) expected.(4*i+1) expected.(4*i+2) expected.(4*i+3);
          assert (actual.(4 * i + 1) = expected.(4 * i + 1) && (actual.(4 * i + 3) <> 0.) = (expected.(4 * i + 3) <> 0.));
          for k = 0 to 3 do worst := Float.max !worst (Float.abs (actual.(4 * i + k) -. expected.(4 * i + k))) done
        done;
        Printf.printf "scene/trace Metal (triangles + spheres): %d of %d rays hit, prim exact, max abs difference %.3g\n" !hits count !worst;
        assert (!hits > 0 && !worst <= 1e-5);
        Flow_gpu.Run.close runner; Flow_gpu.Pipelines.close pipelines;
        ignore (B.destroy_accel blas); ignore (B.destroy_buffer scratch); ignore (B.destroy_buffer vertex_buffer); ignore (B.destroy_buffer sphere_buffer)
      end);
  Flow_sop.Trace_scene.unregister id
