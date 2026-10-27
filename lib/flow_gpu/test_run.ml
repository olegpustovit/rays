let array = function Flow.Eval.Float_array values | Vec2_array values | Vec3_array values
  | Vec4_array values -> values | _ -> assert false
let () = match Rays_execution.acquire_gpu () with
  | Error error -> Format.eprintf "%a@." Rays_execution.pp_error error; exit 2
  | Ok gpu -> Fun.protect ~finally:(fun () -> Rays_execution.release_gpu gpu) (fun () ->
      if not (Ogpu.Caps.has (Ogpu.Backend.capabilities (Rays_execution.gpu_device gpu)) Compute_pipeline) then
        print_endline "SKIP: backend lacks Compute_pipeline"
      else begin
        let pipelines=Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday (Rays_execution.gpu_device gpu) in
        Fun.protect ~finally:(fun () -> Flow_gpu.Pipelines.close pipelines) (fun () ->
          List.iter (fun count -> List.iter (fun (name,program) ->
            let msl=Test_program.ok (Flow_gpu.Emit.kernel program) in
            let compiled=Test_program.ok (Flow_gpu.Pipelines.get pipelines msl) in
            assert (Test_program.ok (Flow_gpu.Pipelines.get pipelines msl)==compiled);
            let runner=Flow_gpu.Run.create gpu pipelines msl in
            Fun.protect ~finally:(fun () -> Flow_gpu.Run.close runner) (fun () ->
              let live=Frame_input.at_time 1. in
              let values=Test_program.ok (Flow_ir.Packed.Private.prepare program ~live) in
              let output=Test_program.ok (Flow_gpu.Run.dispatch runner values) in
              let actual_value=Test_program.ok (Flow_gpu.Run.readback output)
              and expected_value=Test_program.ok (Flow_ir.Packed.force program ~live) in
              assert(Flow_gpu.Run.Private.status_reads runner=1);
              assert(Flow_gpu.Run.Private.readback_bytes runner=count*Flow_gpu.Run.width output*4);
              assert (Flow.Value.ty_of actual_value = Flow.Value.ty_of expected_value);
              let actual=array actual_value and expected=array expected_value in
              let maximum=ref 0. in
              Array.iteri (fun index value -> maximum:=max !maximum (abs_float(value-.expected.(index)))) actual;
              Printf.printf "%s,%d,max_abs_error=%.9g\n%!" name count !maximum;
              (* Native rows, Apple M1:
                 noise 8.82e-7 at 1,024 and 9.89e-5 at 65,536 elements. *)
              if name<>"noise" then assert (!maximum=0.) else assert (!maximum<=2e-4);
              let created=Flow_gpu.Run.Private.buffer_creations runner in
              ignore (Test_program.ok (Flow_gpu.Run.dispatch runner values));
              assert (Flow_gpu.Run.Private.buffer_creations runner=created);
              assert (Flow_gpu.Run.Private.input_uploads runner=Array.length values.arrays);
              if name="vec2" then begin
                let replacement={values with arrays=Array.map Array.copy values.arrays}in
                replacement.arrays.(0).(0)<-17.;
                let changed=Test_program.ok(Flow_gpu.Run.dispatch runner replacement)
                  |> Flow_gpu.Run.readback |> Test_program.ok |> array in
                assert(changed.(0)=17. && changed.(1)=18.);
                let restored=Test_program.ok(Flow_gpu.Run.dispatch runner values)
                  |> Flow_gpu.Run.readback |> Test_program.ok |> array in
                assert(restored=expected);
                let later=Test_program.ok(Flow_ir.Packed.Private.prepare program ~live:(Frame_input.at_time 2.))in
                let animated=Test_program.ok(Flow_gpu.Run.dispatch runner
                  {values with frame=later.frame;uniforms=later.uniforms})
                  |> Flow_gpu.Run.readback |> Test_program.ok |> array in
                assert(animated.(0)=0. && animated.(1)=2.);
                assert(Flow_gpu.Run.Private.input_uploads runner=3)
              end))
            (Test_program.fixtures count @ Test_program.vector_fixtures count)) [1024;65536];
        (* fast precision: int32 arithmetic and a gather from a live vec3 table match the CPU kernel *)
        (let program=Test_program.compile ("(let* [tab (map (fn [x] [x (+ x t) 0]) (array/range 64))] \
            (map (fn [i] (let* [p (array/nth tab (int/mod (int/mul (int i) 7) 64))] \
              (+ p.y (int/and (int/add (int i) 1) 3)))) (array/range 4096)))") in
        let runner=Flow_gpu.Run.create gpu pipelines (Test_program.ok(Flow_gpu.Emit.kernel ~fast:true program))in
        Fun.protect ~finally:(fun()->Flow_gpu.Run.close runner)(fun()->
          let live=Frame_input.at_time 1. in
          let output=Test_program.ok(Flow_gpu.Run.dispatch runner (Test_program.ok(Flow_ir.Packed.Private.prepare ~fast:true program ~live)))in
          let actual=array(Test_program.ok(Flow_gpu.Run.readback output))
          and expected=array(Test_program.ok(Flow_ir.Packed.force program ~live)) in
          if actual<>expected then failwith "fast int32 and gather kernel differs from the CPU";
          print_endline "fast int32 and gather kernel matches the CPU at 4,096 elements";
          (* a drawing's region read back from the device in fast precision, and declined in exact *)
          let host=Flow_gpu.Host.create ~clock:Unix.gettimeofday gpu in
          Fun.protect ~finally:(fun()->Flow_gpu.Host.close host)(fun()->
            Flow_ir.Gpu.with_backend (Flow_gpu.Host.backend host)(fun()->
              let readback=Flow_ir.Executor.region_readback () in
              (match Test_program.ok(readback program ~live) with
               | None -> print_endline "exact precision keeps the region on the CPU"
               | Some _ -> failwith "exact precision sent a region to the device");
              Domain.DLS.set Flow_ir.Gpu.fast true;
              Fun.protect ~finally:(fun()->Domain.DLS.set Flow_ir.Gpu.fast false)(fun()->
                match Test_program.ok(readback program ~live) with
                | Some value when array value=expected -> print_endline "fast precision reads the region back from the device"
                | Some _ -> failwith "the device region differs from the CPU"
                | None -> failwith "fast precision kept a 4,096-element region on the CPU")))));
        (let program=Test_program.two_outputs()in
        let runner=Flow_gpu.Run.create gpu pipelines (Test_program.ok(Flow_gpu.Emit.kernel program))in
        Fun.protect ~finally:(fun()->Flow_gpu.Run.close runner)(fun()->
          let live=Frame_input.at_time 1. in
          let output=Test_program.ok(Flow_gpu.Run.dispatch runner (Test_program.ok(Flow_ir.Packed.Private.prepare program ~live)))in
          let expected=Test_program.ok(Flow_ir.Packed.force_all program ~live)in
          List.iteri(fun index expected->
            assert(array(Test_program.ok(Flow_gpu.Run.readback ~index output))=array expected))expected));
        List.iter (fun (body,refuses) ->
        let hidden=Test_program.compile ("(map (fn [x] " ^ body ^ ") (array/range 1024))")in
        let msl=Test_program.ok(Flow_gpu.Emit.kernel hidden)in
        let runner=Flow_gpu.Run.create gpu pipelines msl in
        Fun.protect ~finally:(fun()->Flow_gpu.Run.close runner)(fun()->
          let live=Frame_input.at_time 1. in
          assert(Array.for_all((=)0.)(array(Test_program.ok(Flow_ir.Packed.force hidden ~live))));
          let inputs=Test_program.ok(Flow_ir.Packed.Private.prepare hidden ~live)in
          match Flow_gpu.Run.dispatch runner inputs with
            |Error diagnostic->assert(refuses && diagnostic.Flow.Diagnostic.code="E_KERNEL")
            |Ok output->assert(not refuses);
                assert(Array.for_all((=)0.)(array(Test_program.ok(Flow_gpu.Run.readback output))))))
          ["(if (> (+ x t) 0) 0 (* (* (+ x t) 1e20) (* (+ x t) 1e20)))",false;
           "(min (* (* (+ x t) 1e20) (* (+ x t) 1e20)) 0)",true]);
        print_endline "GPU run: emitted kernels match the CPU tier within the recorded native noise tolerance"
      end)

(* A drawing's fold whose filter and maps run in fast precision: the resident fold keeps its arrays on
   the device and draws the CPU's points (integers, exact in 32 bits). *)
let resident_fold gpu ~count:range =
  let ok=Test_program.ok in
  let body="(let* [p (fold [b (map (fn [i] [(+ i (floor t)) (* i 2) 0]) (array/range 9000))] [d (range 3)] \
    (let* [kept (filter (fn [i] (>= (int/mod (int i) 3) 1)) (array/range (array/count b)))] \
      (map (fn [i] (let* [q (array/nth b (int (array/nth kept (int i))))] [(+ q.x 1) q.y 0])) \
        (array/range " ^ range ^ "))))] (draw/points p))" in
  let forms=ok(Flow.Syntax.parse("(workspace w (graph g :context draw "^body^"))"))in
  let ws=match Flow.Workspace.check {Flow.Check.version=1;kinds=[]} forms with
    |Some ws,[]->ws|_,ds->failwith(String.concat "; "(List.map Flow.Diagnostic.to_string ds))in
  let evaluation=ok(Flow.Eval.static ws)in
  let program,root=Result.get_ok(Flow_ir.Core_draw.lower evaluation.plan(List.assoc "g" evaluation.results))in
  let prepared=Flow_ir.Core_exec.prepare ~remember_loops:true program in
  let live={(Frame_input.at_time 0.)with size=(200,100)}in
  let points ()=
    let buffers=Flow_ir.Core_exec.Buffers.create()in
    match ok(Flow_ir.Core_exec.draw ?root prepared ~live ~size:(200,100) ~buffers)with
    |[|Scene_command.Marks.Mark_set{kind=Point{position=Buffer{id;_};_};count}|]->
        count,Flow_ir.Core_exec.Buffers.find buffers id
    |_->failwith "resident fold: one point set expected" in
  let host=Flow_gpu.Host.create ~clock:Unix.gettimeofday gpu in
  Fun.protect ~finally:(fun()->Flow_gpu.Host.close host)(fun()->
    Domain.DLS.set Flow_ir.Gpu.fast true;
    Fun.protect ~finally:(fun()->Domain.DLS.set Flow_ir.Gpu.fast false)(fun()->
      let expected=points()in
      (* a fresh prepare, so the CPU draw's remembered loop does not answer *)
      let prepared'=Flow_ir.Core_exec.prepare ~remember_loops:true program in
      let r=Flow_gpu.Host.resident host and ran=ref 0 in
      let resident={r with invoke=(fun p ~live->let v=r.invoke p ~live in if v<>None then incr ran;v)}in
      let buffers=Flow_ir.Core_exec.Buffers.create()in
      let actual=match ok(Flow_ir.Core_exec.draw ?root ~resident prepared' ~live ~size:(200,100) ~buffers)with
        |[|Scene_command.Marks.Mark_set{kind=Point{position=Buffer{id;_};_};count}|]->
            count,Flow_ir.Core_exec.Buffers.find buffers id
        |_->failwith "resident fold: one point set expected" in
      if !ran=0 then failwith "resident fold: no region ran on the device";
      let count,xs=expected and count',ys=actual in
      if count<>count' || Array.sub xs 0 (count*3)<>Array.sub ys 0 (count*3) then
        failwith "resident fold: device points differ from the CPU";
      (* fast display dispatches left in flight (never borrowed) between resident draws share the
         queue's three command buffers with the fold's levels: none is refused *)
      let display=List.assoc "arithmetic"(Test_program.fixtures 4096)in
      let inputs=ok(Flow_ir.Packed.Private.prepare ~fast:true display ~live:(Frame_input.at_time 1.))in
      let backend=Flow_gpu.Host.backend host in
      let kernels=List.init 2 (fun _->ok(backend.prepare display))in
      for _=1 to 4 do
        List.iter(fun (kernel:Flow_ir.Gpu.kernel)->ignore(ok(kernel.run inputs)))kernels;
        let buffers=Flow_ir.Core_exec.Buffers.create()in
        ignore(ok(Flow_ir.Core_exec.draw ?root ~resident:(Flow_gpu.Host.resident host)
          (Flow_ir.Core_exec.prepare ~remember_loops:true program) ~live ~size:(200,100) ~buffers))
      done;
      Printf.printf "resident fold over %s: %d device regions draw the CPU's %d points\n" range !ran count))

let ()=
  let module H=Flow_gpu.Host in
  let _,handles=Ogpu.Impl.create_driver()in
  let baseline=handles()in
  let gpu=match Rays_execution.acquire_gpu()with Ok gpu->gpu|Error e->
    failwith(Format.asprintf "%a" Rays_execution.pp_error e)in
  Fun.protect ~finally:(fun()->Rays_execution.release_gpu gpu)(fun()->
    (* the device keeps a count it can take ([array/range] of a count times a factor) and reads
       back one it cannot *)
    resident_fold gpu ~count:"(array/count kept)";
    resident_fold gpu ~count:"(+ (* (array/count kept) 1) 0)";
    let host=H.create ~clock:Unix.gettimeofday gpu in
    Fun.protect ~finally:(fun()->H.close host)(fun()->
      let packed=List.assoc "arithmetic"(Test_program.fixtures 1024)in
      let inputs=Flow_ir.Packed.Private.prepare packed ~live:(Frame_input.at_time 1.) |> Test_program.ok in
      let backend=H.backend host in
      let first=backend.prepare packed |> Test_program.ok in
      let value=first.run inputs |> Test_program.ok in
      ignore(first.readback value |> Test_program.ok);
      let initial=H.Private.stats host in
      assert(initial.runners_created=1 && initial.runners_released=0 && initial.status_reads=1);
      for _=1 to 64 do
        let kernel=backend.prepare packed |> Test_program.ok in
        ignore(kernel.run inputs |> Test_program.ok)
      done;
      let evicted=H.Private.stats host in
      assert(evicted.runners_created>=65 && evicted.runners_released=evicted.runners_created-64
        && evicted.pipeline_compilations=1 && evicted.status_reads=65);
      assert(evicted.buffer_creations=65*initial.buffer_creations
        && evicted.input_uploads=65*initial.input_uploads
        && evicted.input_uploaded_bytes=65*initial.input_uploaded_bytes
        && evicted.readback_bytes=initial.readback_bytes && initial.readback_bytes=4096);
      assert(H.output host value=None);
      ignore(first.run inputs |> Test_program.ok);
      let renewed=H.Private.stats host in
      assert(renewed.runners_created=evicted.runners_created+1
        && renewed.runners_released=evicted.runners_released+1 && renewed.status_reads=66);
      assert(Domain.join(Domain.spawn(fun()->try ignore(H.Private.stats host);false with Invalid_argument _->true)));
      H.close host;H.close host;
      let closed=H.Private.stats host in
      assert(closed.runners_created=renewed.runners_created && closed.runners_released=closed.runners_created
        && closed.pipeline_compilations=1 && closed.pipeline_releases=1
        && closed.status_reads=renewed.status_reads && closed.readback_bytes=initial.readback_bytes
        && closed.buffer_creations=renewed.buffer_creations
        && closed.input_uploads=renewed.input_uploads
        && closed.input_uploaded_bytes=renewed.input_uploaded_bytes)));
  assert(handles()=baseline);
  print_endline "GPU Host counters: eviction, reacquisition and idempotent close preserve cumulative successful work, zero handles"
