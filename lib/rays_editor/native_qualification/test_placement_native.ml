module Gpu = Flow_native.Workspace_gpu

let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)

let compile_source source =
  let forms = ok (Flow.Syntax.parse source) in
  let workspace =
    match Flow.Workspace.check Flow.Catalog.{ version = 1; kinds = [] } forms with
    | Some workspace, [] -> workspace
    | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
  in
  let value = List.assoc "g" (ok (Flow.Eval.static workspace)).results in
  Option.get (Flow_ir.Executor.packed_root (ok (Flow_ir.Executor.compile value)))

let compile body =
  compile_source
    ("(workspace placement (graph g :context host (map (fn [i] " ^ body ^ ") (array/range 32768))))")

let () =
  let packed = compile "(+ (sin (+ i t)) 0.25)" in
  let handles = Ogpu.Impl.live_handles () in
  let owner = Gpu.create () in
  Fun.protect
    ~finally:(fun () -> Gpu.close owner)
    (fun () ->
      Gpu.with_backend ~fast:true owner (fun config ->
          assert (config.policy = Flow_ir.Gpu.Measured);
          let backend = Option.get config.backend in
          let stats () =
            let host, _, _, _, _ = Gpu.image_stats owner in
            host
          in
          let before = stats () in
          let seconds = backend.cost ~fast:true packed ~count:32768 in
          assert (Option.fold ~none:false ~some:(fun s -> Float.is_finite s && s >= 0.) seconds);
          let measured = stats () in
          assert (measured.payload_readback_bytes = before.payload_readback_bytes);
          assert (measured.status_readback_bytes > before.status_readback_bytes);
          assert (backend.cost ~fast:true packed ~count:32768 = seconds);
          assert (stats () = measured);
          let input =
            compile_source
              "(workspace input (graph g :context host [(data : (array float) (array/float 32768))]\n\
               (map (fn [x] (+ x t)) data)))"
          in
          assert (Array.length (Flow_ir.Packed.Host.view input).widths = 1);
          let cpu_seconds = backend.cost ~fast:true input ~count:32768 in
          assert (Option.is_some cpu_seconds);
          let transferred = stats () in
          assert (transferred.input_uploaded_bytes - measured.input_uploaded_bytes = 4 * 32768 * 4);
          assert (transferred.payload_readback_bytes = measured.payload_readback_bytes);
          assert (backend.cost ~fast:true input ~count:32768 = cpu_seconds);
          assert (stats () = transferred);
          let ranged = compile "(+ (float i) t)" in
          let integers =
            compile_source
              "(workspace input (graph g :context host [(data : (array int) (array/int 32768))]\n\
               (map (fn [x] (+ (float x) t)) data)))"
          in
          assert (Option.is_some (backend.cost ~fast:true ranged ~count:32768));
          let range_done = stats () in
          let integer_cost = backend.cost ~fast:true integers ~count:32768 in
          assert (Option.is_some integer_cost);
          let integer_done = stats () in
          assert (
            integer_done.input_uploaded_bytes - range_done.input_uploaded_bytes = 4 * 32768 * 4);
          assert (integer_done.payload_readback_bytes = range_done.payload_readback_bytes);
          assert (backend.cost ~fast:true integers ~count:32768 = integer_cost);
          assert (stats () = integer_done);
          Printf.printf
            "Integer-input calibration after a range: %d uploaded bytes, payload +0; cached query \
             adds no work\n\
             %!"
            (integer_done.input_uploaded_bytes - range_done.input_uploaded_bytes);
          Printf.printf
            "CPU-input calibration: %.6f s, %d uploaded bytes across four samples, payload +0; \
             cached query adds no work\n\
             %!"
            (Option.get cpu_seconds)
            (transferred.input_uploaded_bytes - measured.input_uploaded_bytes);
          let table = compile_source
              "(workspace table (graph g :context host\n\
               [(data : (array float) (array/float 1024 0.25))]\n\
               (map (fn [i] (+ (array/nth data (int (mod i 1024))) t)) (array/range 32768))))" in
          let inputs = ok (Flow_ir.Packed.Host.prepare ~fast:true ~index:true table
              ~live:(Input.at_time 0.)) in
          assert (Array.exists (fun values -> Array.length values = 1024) inputs.tables);
          let before_table = stats () in
          let table_cost = backend.cost ~inputs ~fast:true table ~count:32768 in
          assert (Option.is_some table_cost);
          let table_done = stats () in
          assert (table_done.input_uploaded_bytes - before_table.input_uploaded_bytes = 4 * 1024 * 4);
          assert (table_done.payload_readback_bytes = before_table.payload_readback_bytes);
          assert (backend.cost ~inputs ~fast:true table ~count:32768 = table_cost);
          assert (stats () = table_done);
          let larger = { inputs with tables = Array.map (fun values ->
              if Array.length values = 0 then values else Array.append values values) inputs.tables } in
          let larger_cost = backend.cost ~inputs:larger ~fast:true table ~count:32768 in
          assert (Option.is_some larger_cost);
          let larger_done = stats () in
          assert (larger_done.input_uploaded_bytes - table_done.input_uploaded_bytes = 4 * 2048 * 4);
          assert (backend.cost ~inputs:larger ~fast:true table ~count:32768 = larger_cost);
          assert (stats () = larger_done);
          Printf.printf "Prepared-table calibration: 1024/2048 entries upload %d/%d bytes; distinct layout cache, cached queries add no work\n%!"
            (table_done.input_uploaded_bytes - before_table.input_uploaded_bytes)
            (larger_done.input_uploaded_bytes - table_done.input_uploaded_bytes);
          let mixed = compile_source
              "(workspace mixed (graph g :context host\n\
               [(a : (array float) (array/float 32768 1.))\n\
                (b : (array float) (array/float 32768 2.))]\n\
               (map (fn [x y] (+ (+ x y) t)) a b)))" in
          let mixed_inputs = ok (Flow_ir.Packed.Host.prepare ~fast:true mixed ~live:(Input.at_time 0.)) in
          assert (Array.length mixed_inputs.arrays = 2);
          let before_cpu_mix = stats () in
          assert (Option.is_some (backend.cost ~inputs:mixed_inputs ~fast:true mixed ~count:32768));
          let cpu_mix = stats () in
          assert (cpu_mix.input_uploaded_bytes - before_cpu_mix.input_uploaded_bytes = 4 * 2 * 32768 * 4);
          let producer = ok (backend.prepare ~fast:true ranged) in
          let producer_inputs = ok (Flow_ir.Packed.Host.prepare ~fast:true ~index:true ranged
              ~live:(Input.at_time 0.)) in
          let kept = ok (producer.planes (ok (producer.run producer_inputs))) in
          Fun.protect ~finally:(fun () -> kept.release ~held:false) (fun () ->
              ok (kept.settle ());
              let placeholder = match Option.get kept.resident with
                | Flow.Eval.Float_array values -> values | _ -> assert false in
              let held_inputs = { mixed_inputs with arrays = [| placeholder; mixed_inputs.arrays.(1) |] } in
              let before_held = stats () in
              let held_cost = backend.cost ~inputs:held_inputs ~fast:true mixed ~count:32768 in
              assert (Option.is_some held_cost);
              let held_done = stats () in
              assert (held_done.input_uploaded_bytes - before_held.input_uploaded_bytes = 4 * 32768 * 4);
              assert (held_done.payload_readback_bytes = before_held.payload_readback_bytes);
              assert (backend.cost ~inputs:held_inputs ~fast:true mixed ~count:32768 = held_cost);
              assert (stats () = held_done);
              let values = kept.materialise () in
              assert (values.(0).(32767) = 32767.);
              Printf.printf "Prepared mixed-origin calibration: CPU/CPU uploads %d bytes, resident/CPU %d; payload +0 during measurement, old source unchanged\n%!"
                (cpu_mix.input_uploaded_bytes - before_cpu_mix.input_uploaded_bytes)
                (held_done.input_uploaded_bytes - before_held.input_uploaded_bytes));
          let recovered_kernel = compile "(if (> t 0.) (* (* (+ i 1.) 1e20) 1e20) 0.)" in
          let bad_frame = ok (Flow_ir.Packed.Host.prepare ~fast:true ~index:true recovered_kernel
              ~live:(Input.at_time 1.)) in
          assert (backend.cost ~inputs:bad_frame ~fast:true recovered_kernel ~count:32768 = None);
          let good_frame = ok (Flow_ir.Packed.Host.prepare ~fast:true ~index:true recovered_kernel
              ~live:(Input.at_time 0.)) in
          let recovered_cost = backend.cost ~inputs:good_frame ~fast:true recovered_kernel ~count:32768 in
          assert (Option.is_some recovered_cost);
          let recovered = stats () in
          assert (backend.cost ~inputs:good_frame ~fast:true recovered_kernel ~count:32768 = recovered_cost);
          assert (stats () = recovered);
          print_endline "Prepared-frame calibration: failed frame does not poison a valid frame; successful result is cached";
          let before_rejection = stats () in
          let failing = compile "(* (* (+ (+ i t) 1.) 1e20) 1e20)" in
          assert (backend.cost ~fast:true failing ~count:32768 = None);
          let rejected = stats () in
          assert (rejected.payload_readback_bytes = before_rejection.payload_readback_bytes);
          assert (rejected.status_readback_bytes > before_rejection.status_readback_bytes);
          assert (backend.cost ~fast:true failing ~count:32768 = None);
          assert (stats () = rejected);
          Printf.printf
            "placement calibration: %.6f s completed, status +%d bytes, payload +0; cached \
             estimate issues no work\n\
             %!"
            (Option.get seconds)
            (measured.status_readback_bytes - before.status_readback_bytes)));
  assert (Ogpu.Impl.live_handles () = handles)

(* The measured attribute route, with an explicit resident drawing consumer.
   Keep two generations alive while subsequent dispatches reuse the same host. *)
let retained_generations () =
  let baseline = Ogpu.Impl.live_handles () in
  let gpu = Gpu.create () in
  let string_ok = function Ok value -> value | Error message -> failwith message in
  Fun.protect ~finally:(fun () -> Gpu.close gpu) (fun () ->
      Gpu.with_backend ~fast:true gpu (fun config ->
          let wave = List.fold_left (fun x _ -> "(sin " ^ x ^ ")") "(+ (float i) t)"
              (List.init 16 Fun.id) in
          let displacement time =
            let x = ref time in
            for _ = 1 to 16 do x := sin !x done;
            !x *. 0.01
          in
          let source =
            "(workspace generations (graph g :context sop\n\
             (sop/with_attr\n\
               (sop/mesh_planes :points {:x (array/float 262144) :y (array/float 262144)\n\
                 :z (array/float 262144)} :vertex_points (array/int 0)\n\
                 :primitive_offsets (array/int 1) :kinds (array/int 0)) :P\n\
               (exact (map (fn [i] [(+ (+ (- (/ (mod i 256) 256) 0.5) (* t 0.002)) (* " ^ wave ^ " 0.01))\n\
                 (- (/ (floor (/ i 256)) 256) 0.5) 0]) (array/range 262144))))))"
          in
          let lower source =
            let lowered = ok (Flow_sop.Lower.workspace ~gpu:config ~defer:true (ok (Flow.Syntax.parse source))) in
            let graph = List.hd lowered.graphs in
            let node = string_ok (Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)) in
            (lowered, node)
          in
          let small, small_node = lower (Str.global_replace (Str.regexp_string "262144") "32" source) in
          let small_session = string_ok (Sop.Session.create ~max_entries:0 ~max_payload_bytes:0) in
          Fun.protect ~finally:(fun () -> Sop.Session.close small_session) (fun () ->
              let context = string_ok (Sop.Context.create ~domains:1 ()) in
              let output = ok (Sop.Session.cook ~cpu_consumers:0 small_session ~context small_node) in
              let geometry = ok (Sop.Payload.geometry output.payload) in
              assert (not (Rdk.Packed.Float3.resident (Rdk.Geometry.positions geometry)));
              let placements = Flow_ir.Profile.placements small.profile in
              if not (List.exists (fun (p : Flow_ir.placement) ->
                  p.reason = Cpu_consumer || p.reason = Cpu_measured_cheaper) placements)
                 || List.exists (fun (p : Flow_ir.placement) -> Flow_ir.Cost.on_device p.reason) placements then
                failwith ("small placement: " ^ String.concat ", "
                  (List.map (fun (p : Flow_ir.placement) -> Flow_ir.Cost.reason_name p.reason
                    ^ ":" ^ Option.fold ~none:"unknown" ~some:string_of_int p.cpu_consumers) placements)));
          let lowered, node = lower source in
          let session = string_ok (Sop.Session.create ~max_entries:4 ~max_payload_bytes:(1 lsl 26)) in
          Fun.protect ~finally:(fun () -> Sop.Session.close session) (fun () ->
              let canvas = Rays.Canvas.create_exn ~width:64 ~height:64
              and other = Rays.Canvas.create_exn ~width:64 ~height:64 in
              Fun.protect
                ~finally:(fun () ->
                  string_ok (Rays.Canvas.destroy canvas);
                  string_ok (Rays.Canvas.destroy other))
                (fun () ->
                  let stats () = let host, _, _, _, _ = Gpu.image_stats gpu in host in
                  let camera = Rays.Camera.orthographic ~height:2.
                      ~at:(Rays_math.Vec3.create 0. 0. 3.) ~target:Rays_math.Vec3.zero () in
                  let cook ?(node = node) time =
                    let context = string_ok (Sop.Context.create ~domains:1 ~time ()) in
                    let output = ok (Sop.Session.cook ~cpu_consumers:0 session ~context node) in
                    let geometry = ok (Sop.Payload.geometry output.payload) in
                    if not (Rdk.Packed.Float3.resident (Rdk.Geometry.positions geometry)) then
                      failwith ("Measured lifetime fixture chose CPU: " ^ String.concat ", "
                        (List.map (fun (p : Flow_ir.placement) ->
                            Printf.sprintf "%s cpu=%s device=%s" (Flow_ir.Cost.reason_name p.reason)
                              (Option.fold ~none:"unknown" ~some:string_of_float p.cpu_seconds)
                              (Option.fold ~none:"unknown" ~some:string_of_float p.device_seconds))
                          (Flow_ir.Profile.placements lowered.profile)));
                    let surface = string_ok (Rdk_rays.Surface.of_geometry geometry) in
                    let scene = Rdk_rays.Surface.scene3 ~device:(Gpu.display gpu) surface in
                    let mesh = (List.hd (Rays.Scene.D3.Walk.drawings scene)).mesh in
                    let key = fst (Option.get (Rays.Mesh.View.device mesh)) in
                    let frame = Rays.Scene.[ clear Rays_math.Color.black; view3d ~camera scene ] in
                    (geometry, key, frame, time)
                  in
                  let draw target (_, _, frame, _) =
                    string_ok (Rays.Canvas.render target frame);
                    string_ok (Rays.Canvas.pixels target)
                  in
                  let previous = ref (cook 0.) in
                  let expected = ref (draw canvas !previous) in
                  assert (Array.exists (fun (c : Rays_math.Color.t) -> c.r + c.g + c.b > 0) !expected);
                  let warm = ref None and heap = ref 0 in
                  for i = 1 to 200 do
                    let next = cook (float i) in
                    ignore (draw other next);
                    assert (draw canvas !previous = !expected);
                    let _, old_key, _, _ = !previous and _, key, _, _ = next in
                    Gpu.frame ~keep:[ old_key; key ] gpu;
                    assert (Gpu.held_displays gpu <= 6);
                    previous := next;
                    expected := draw canvas next;
                    if i mod 8 = 0 then begin
                      Gc.full_major ();
                      if i = 16 then begin
                        warm := Some (stats ());
                        heap := (Gc.stat ()).live_words
                      end
                    end
                  done;
                  Gc.full_major ();
                  let first = Option.get !warm and last = stats () in
                  let heap_delta = (Gc.stat ()).live_words - !heap in
                  assert (last.pipeline_compilations = first.pipeline_compilations);
                  assert (last.runners_created = first.runners_created);
                  assert (last.buffer_creations <= first.buffer_creations + 4);
                  assert (last.status_reads >= first.status_reads + 184);
                  assert (last.held_buffers <= first.held_buffers + 4);
                  assert (heap_delta <= 262144);
                  assert (last.payload_readback_bytes = 0);
                  assert (List.exists (fun (p : Flow_ir.placement) ->
                      p.cpu_consumers = Some 0 && Flow_ir.Cost.on_device p.reason)
                    (Flow_ir.Profile.placements lowered.profile));
                  let cancel = Rdk.Cancel.create () in
                  Rdk.Cancel.cancel cancel;
                  let context = string_ok (Sop.Context.create ~domains:1 ~cancel ~time:201. ()) in
                  (match Sop.Session.cook ~cpu_consumers:0 session ~context node with
                  | Error d -> assert (d.code = "E_CANCELLED" || d.code = "cancelled")
                  | Ok _ -> failwith "cancelled placement cook published geometry");
                  assert ((stats ()).payload_readback_bytes = 0);
                  let recovered = cook 201. in
                  ignore (draw other recovered);
                  assert (draw canvas !previous = !expected);
                  let edited_source =
                    Str.global_replace (Str.regexp_string "0.002") "0.004" source
                  in
                  let _, edited_node = lower edited_source in
                  let edited_result = cook ~node:edited_node 202. in
                  ignore (draw other edited_result);
                  assert (draw canvas !previous = !expected);
                  assert ((stats ()).payload_readback_bytes = 0);
                  let geometry, _, _, time = !previous in
                  let x, y, z = Rdk.Packed.Float3.get (Rdk.Geometry.positions geometry) 0 in
                  assert (Float.abs (x -. (-0.5 +. time *. 0.002 +. displacement time)) < 1e-6 && y = -0.5 && z = 0.);
                  Gpu.frame ~keep:[] gpu;
                  Gpu.frame gpu;
                  Gpu.frame gpu;
                  assert (Gpu.held_displays gpu = 0);
                  (* Read one generation while registered, the others after display
                     release; all three still own their immutable storage. *)
                  List.iter
                    (fun ((geometry, _, _, time), scale) ->
                      let read () =
                        let x, y, z = Rdk.Packed.Float3.get (Rdk.Geometry.positions geometry) 0 in
                        assert (Float.abs (x -. (-0.5 +. time *. scale +. displacement time)) < 1e-6
                          && y = -0.5 && z = 0.)
                      in
                      Parallel.run ~domains:4 (fun () ->
                          Parallel.for_ ~chunk_size:1 ~start:0 ~finish:7 (fun _ -> read ()));
                      let bytes = (stats ()).payload_readback_bytes in
                      read ();
                      assert ((stats ()).payload_readback_bytes = bytes))
                    [ (recovered, 0.002); (edited_result, 0.004) ];
                  Printf.printf "Measured retained generations: small CPU-origin stays CPU; 200 dispatches, two canvases, old pixels stable, cancel/recook/source edit/read-release orders pass; payload before explicit read=0, held=%d, buffers=%d (+%d warm), pipelines=%d, heap_delta_words=%d\n%!"
                    last.held_buffers last.buffer_creations
                    (last.buffer_creations - first.buffer_creations)
                    last.pipeline_compilations heap_delta))));
  assert (Ogpu.Impl.live_handles () = baseline)

let () = if Array.exists (( = ) "--lifetimes") Sys.argv then retained_generations ()
