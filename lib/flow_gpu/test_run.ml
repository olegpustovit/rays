let array = function
  | Flow.Eval.Float_array values | Vec2_array values | Vec3_array values | Vec4_array values ->
      values
  | _ -> assert false

let () =
  match Renderer.acquire_gpu () with
  | Error error ->
      Format.eprintf "%a@." Renderer.pp_error error;
      exit 2
  | Ok gpu ->
      Fun.protect
        ~finally:(fun () -> Renderer.release_gpu gpu)
        (fun () ->
          if
            not
              (Ogpu.Caps.has (Ogpu.Backend.capabilities (Renderer.gpu_device gpu)) Compute_pipeline)
          then print_endline "SKIP: backend lacks Compute_pipeline"
          else begin
            let pipelines =
              Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday (Renderer.gpu_device gpu)
            in
            Fun.protect
              ~finally:(fun () -> Flow_gpu.Pipelines.close pipelines)
              (fun () ->
                List.iter
                  (fun count ->
                    List.iter
                      (fun (name, program) ->
                        let msl = Test_program.ok (Flow_shader.Emit.kernel program) in
                        let compiled = Test_program.ok (Flow_gpu.Pipelines.get pipelines msl) in
                        assert (Test_program.ok (Flow_gpu.Pipelines.get pipelines msl) == compiled);
                        let runner = Flow_gpu.Run.create gpu pipelines msl in
                        Fun.protect
                          ~finally:(fun () -> Flow_gpu.Run.close runner)
                          (fun () ->
                            let live = Input.at_time 1. in
                            let values =
                              Test_program.ok (Flow_ir.Packed.Host.prepare program ~live)
                            in
                            let output = Test_program.ok (Flow_gpu.Run.dispatch runner values) in
                            let actual_value = Test_program.ok (Flow_gpu.Run.readback output)
                            and expected_value =
                              Test_program.ok (Flow_ir.Packed.force program ~live)
                            in
                            assert (Flow_gpu.Run.Private.status_reads runner = 1);
                            assert (
                              Flow_gpu.Run.Private.readback_bytes runner
                              = count * Flow_gpu.Run.width output * 4);
                            assert (Flow.Value.ty_of actual_value = Flow.Value.ty_of expected_value);
                            let actual = array actual_value and expected = array expected_value in
                            let maximum = ref 0. in
                            Array.iteri
                              (fun index value ->
                                maximum := max !maximum (abs_float (value -. expected.(index))))
                              actual;
                            Printf.printf "%s,%d,max_abs_error=%.9g\n%!" name count !maximum;
                            (* Native rows, Apple M1:
                 noise 8.82e-7 at 1,024 and 9.89e-5 at 65,536 elements. *)
                            if name <> "noise" then assert (!maximum = 0.)
                            else assert (!maximum <= 2e-4);
                            let created = Flow_gpu.Run.Private.buffer_creations runner in
                            ignore (Test_program.ok (Flow_gpu.Run.dispatch runner values));
                            assert (Flow_gpu.Run.Private.buffer_creations runner = created);
                            assert (
                              Flow_gpu.Run.Private.input_uploads runner = Array.length values.arrays);
                            if name = "vec2" then begin
                              let replacement =
                                { values with arrays = Array.map Array.copy values.arrays }
                              in
                              replacement.arrays.(0).(0) <- 17.;
                              let changed =
                                Test_program.ok (Flow_gpu.Run.dispatch runner replacement)
                                |> Flow_gpu.Run.readback |> Test_program.ok |> array
                              in
                              assert (changed.(0) = 17. && changed.(1) = 18.);
                              let restored =
                                Test_program.ok (Flow_gpu.Run.dispatch runner values)
                                |> Flow_gpu.Run.readback |> Test_program.ok |> array
                              in
                              assert (restored = expected);
                              let later =
                                Test_program.ok
                                  (Flow_ir.Packed.Host.prepare program ~live:(Input.at_time 2.))
                              in
                              let animated =
                                Test_program.ok
                                  (Flow_gpu.Run.dispatch runner
                                     { values with frame = later.frame; uniforms = later.uniforms })
                                |> Flow_gpu.Run.readback |> Test_program.ok |> array
                              in
                              assert (animated.(0) = 0. && animated.(1) = 2.);
                              assert (Flow_gpu.Run.Private.input_uploads runner = 3)
                            end))
                      (Test_program.fixtures count @ Test_program.vector_fixtures count))
                  [ 1024; 65536 ];
                (* fast precision: int32 arithmetic and a gather from a live vec3 table match the CPU kernel *)
                List.iter
                  (fun (name, program) ->
                    let runner =
                      Flow_gpu.Run.create gpu pipelines
                        (Test_program.ok (Flow_shader.Emit.kernel ~fast:true program))
                    in
                    Fun.protect
                      ~finally:(fun () -> Flow_gpu.Run.close runner)
                      (fun () ->
                        let live = Input.at_time 1. in
                        let expected =
                          Test_program.ok (Flow_ir.Packed.Host.reference program ~live) |> array
                        in
                        let cpu =
                          Test_program.ok (Flow_ir.Packed.force ~strict:true program ~live) |> array
                        in
                        assert (cpu = expected);
                        let values =
                          Test_program.ok (Flow_ir.Packed.Host.prepare ~fast:true program ~live)
                        in
                        let actual =
                          Test_program.ok (Flow_gpu.Run.dispatch runner values)
                          |> Flow_gpu.Run.readback |> Test_program.ok |> array
                        in
                        if actual <> expected then
                          failwith (name ^ ": carried GPU loop differs from reference");
                        Printf.printf "%s: 4096 native GPU rows match strict CPU and reference\n%!"
                          name))
                  (Test_program.carried_fixtures 4096);
                List.iter
                  (fun (name, source, selected, expected_message) ->
                    let program = Test_program.compile source in
                    let runner =
                      Flow_gpu.Run.create gpu pipelines
                        (Test_program.ok (Flow_shader.Emit.kernel ~fast:true program))
                    in
                    Fun.protect
                      ~finally:(fun () -> Flow_gpu.Run.close runner)
                      (fun () ->
                        let live = Input.at_time 1. in
                        let inputs =
                          Test_program.ok (Flow_ir.Packed.Host.prepare ~fast:true program ~live)
                        in
                        match Flow_gpu.Run.dispatch ~wait:false runner inputs with
                        | Error _ -> assert false
                        | Ok output -> (
                            match Flow_gpu.Run.readback output with
                            | Error d ->
                                assert (
                                  selected && d.Diagnostic.code = "E_RANGE"
                                  && d.message = expected_message);
                                assert (Flow_gpu.Run.Private.status_reads runner = 1);
                                let empty =
                                  Test_program.ok
                                    (Flow_gpu.Run.dispatch ~wait:false runner
                                       { inputs with count = 0 })
                                in
                                assert (
                                  array (Test_program.ok (Flow_gpu.Run.readback empty)) = [||]
                                  && Flow_gpu.Run.Private.status_reads runner = 1);
                                if name = "divide-selected" then begin
                                  let commands =
                                    Test_program.ok
                                      (Result.map_error Flow_gpu.Run.Private.diagnostic
                                         (Ogpu.Backend.begin_commands (Renderer.gpu_queue gpu)))
                                  in
                                  ignore
                                    (Test_program.ok
                                       (Flow_gpu.Run.dispatch ~commands runner inputs));
                                  let empty =
                                    Test_program.ok
                                      (Flow_gpu.Run.dispatch ~wait:false runner
                                         { inputs with count = 0 })
                                  in
                                  assert (
                                    array (Test_program.ok (Flow_gpu.Run.readback empty)) = [||]);
                                  let receipt =
                                    Test_program.ok
                                      (Result.map_error Flow_gpu.Run.Private.diagnostic
                                         (Ogpu.Backend.commit commands))
                                  in
                                  match
                                    Flow_gpu.Run.Private.check_status runner ~epoch:receipt.epoch
                                  with
                                  | Error d ->
                                      assert (
                                        d.Diagnostic.code = "E_RANGE"
                                        && d.message = expected_message)
                                  | Ok () ->
                                      failwith
                                        "empty dispatch cleared an earlier resident GPU error"
                                end;
                                if name = "divide-selected" then begin
                                  let host = Flow_gpu.Host.create ~clock:Unix.gettimeofday gpu in
                                  Fun.protect
                                    ~finally:(fun () -> Flow_gpu.Host.close host)
                                    (fun () ->
                                      let kernel =
                                        Test_program.ok
                                          ((Flow_gpu.Host.backend host).prepare ~fast:true program)
                                      in
                                      let value = Test_program.ok (kernel.run inputs) in
                                      (match Flow_gpu.Host.output host value with
                                      | Error d ->
                                          assert (
                                            d.Diagnostic.code = "E_RANGE"
                                            && d.message = expected_message)
                                      | Ok _ ->
                                          failwith
                                            "Host.output exposed a GPU buffer with a selected \
                                             integer error");
                                      assert ((Flow_gpu.Host.stats host).status_reads = 1))
                                end
                            | Ok value ->
                                assert ((not selected) && array value = Array.init 8 float))))
                  [
                    ( "shift-selected",
                      "(map (fn [(x : int)] (if (> (+ (float x) t) 0.) (float (int/shr x (int/add \
                       63 (int/mod (frame/index) 1)))) (float x))) (array/range 8 :type int))",
                      true,
                      "Integer shift count must be within [0, 62]." );
                    ( "shift-untaken",
                      "(map (fn [(x : int)] (if (< (+ (float x) t) 0.) (float (int/shr x (int/add \
                       63 (int/mod (frame/index) 1)))) (float x))) (array/range 8 :type int))",
                      false,
                      "" );
                    ( "negative-shift-selected",
                      "(map (fn [(x : int)] (if (> (+ (float x) t) 0.) (float (int/shr x (int/sub \
                       (frame/index) (int/add (frame/index) 1)))) (float x))) (array/range 8 :type \
                       int))",
                      true,
                      "Integer shift count must be within [0, 62]." );
                    ( "negative-shift-untaken",
                      "(map (fn [(x : int)] (if (< (+ (float x) t) 0.) (float (int/shr x (int/sub \
                       (frame/index) (int/add (frame/index) 1)))) (float x))) (array/range 8 :type \
                       int))",
                      false,
                      "" );
                    ( "divide-selected",
                      "(map (fn [(x : int)] (if (> (+ (float x) t) 0.) (float (int/div x (int/sub \
                       (frame/index) (frame/index)))) (float x))) (array/range 8 :type int))",
                      true,
                      "Integer division by zero." );
                    ( "divide-untaken",
                      "(map (fn [(x : int)] (if (< (+ (float x) t) 0.) (float (int/div x (int/sub \
                       (frame/index) (frame/index)))) (float x))) (array/range 8 :type int))",
                      false,
                      "" );
                    ( "modulo-selected",
                      "(map (fn [(x : int)] (if (> (+ (float x) t) 0.) (float (int/mod x (int/sub \
                       (frame/index) (frame/index)))) (float x))) (array/range 8 :type int))",
                      true,
                      "Integer division by zero." );
                    ( "modulo-untaken",
                      "(map (fn [(x : int)] (if (< (+ (float x) t) 0.) (float (int/mod x (int/sub \
                       (frame/index) (frame/index)))) (float x))) (array/range 8 :type int))",
                      false,
                      "" );
                  ];
                let fresh = Test_program.compile "(map (fn [x] (+ x t)) (array/range 8))" in
                let runner =
                  Flow_gpu.Run.create gpu pipelines
                    (Test_program.ok (Flow_shader.Emit.kernel ~fast:true fresh))
                in
                Fun.protect
                  ~finally:(fun () -> Flow_gpu.Run.close runner)
                  (fun () ->
                    let live = Input.at_time 1. in
                    let values =
                      Test_program.ok (Flow_ir.Packed.Host.prepare ~fast:true fresh ~live)
                    in
                    let empty =
                      Test_program.ok
                        (Flow_gpu.Run.dispatch ~wait:false runner { values with count = 0 })
                    in
                    assert (
                      array (Test_program.ok (Flow_gpu.Run.readback empty)) = [||]
                      && Flow_gpu.Run.Private.status_reads runner = 0));
                (let program = Test_program.two_outputs () in
                 let host = Flow_gpu.Host.create ~clock:Unix.gettimeofday gpu in
                 Fun.protect
                   ~finally:(fun () -> Flow_gpu.Host.close host)
                   (fun () ->
                     let backend = Flow_gpu.Host.backend host in
                     let kernel = Test_program.ok (backend.prepare ~fast:false program) in
                     let values =
                       Test_program.ok
                         (Flow_ir.Packed.Host.prepare program ~live:(Input.at_time 1.))
                     in
                     let empty = Test_program.ok (kernel.run { values with count = 0 }) in
                     let borrowed =
                       Option.get (Test_program.ok (Flow_gpu.Host.output host empty))
                     in
                     assert (
                       array
                         (Test_program.ok
                            (kernel.readback { empty with width = Flow_gpu.Run.width borrowed }))
                       = [||]
                       && (Flow_gpu.Host.stats host).status_reads = 0)));
                List.iter
                  (fun (body, refuses) ->
                    let program =
                      Test_program.compile ("(map (fn [x] " ^ body ^ ") (array/range 2))")
                    in
                    let runner =
                      Flow_gpu.Run.create gpu pipelines
                        (Test_program.ok (Flow_shader.Emit.kernel ~fast:true program))
                    in
                    Fun.protect
                      ~finally:(fun () -> Flow_gpu.Run.close runner)
                      (fun () ->
                        let live = Input.at_time 1. in
                        let values =
                          Test_program.ok (Flow_ir.Packed.Host.prepare ~fast:true program ~live)
                        in
                        match Flow_gpu.Run.dispatch runner values with
                        | Error diagnostic ->
                            assert (refuses && diagnostic.Diagnostic.code = "E_KERNEL")
                        | Ok output ->
                            assert (not refuses);
                            assert (
                              array (Test_program.ok (Flow_gpu.Run.readback output)) = [| 0.; 1. |])))
                  [
                    ("(fold [a x] [i (range (+ 4097 (int t)))] (+ a 1.))", true);
                    ("(if (> t 0.) x (fold [a x] [i (range (+ 4097 (int t)))] (+ a 1.)))", false);
                  ];
                (let program =
                   Test_program.compile
                     "(let* [tab (map (fn [x] [x (+ x t) 0]) (array/range 64))] (map (fn [i] (let* \
                      [p (array/nth tab (int/mod (int/mul (int i) 7) 64))] (+ p.y (int/and \
                      (int/add (int i) 1) 3)))) (array/range 4096)))"
                 in
                 let runner =
                   Flow_gpu.Run.create gpu pipelines
                     (Test_program.ok (Flow_shader.Emit.kernel ~fast:true program))
                 in
                 Fun.protect
                   ~finally:(fun () -> Flow_gpu.Run.close runner)
                   (fun () ->
                     let live = Input.at_time 1. in
                     let output =
                       Test_program.ok
                         (Flow_gpu.Run.dispatch runner
                            (Test_program.ok (Flow_ir.Packed.Host.prepare ~fast:true program ~live)))
                     in
                     let actual = array (Test_program.ok (Flow_gpu.Run.readback output))
                     and expected = array (Test_program.ok (Flow_ir.Packed.force program ~live)) in
                     if actual <> expected then
                       failwith "fast int32 and gather kernel differs from the CPU";
                     print_endline "fast int32 and gather kernel matches the CPU at 4,096 elements";
                     (* a drawing's region read back from the device in fast precision, and declined in exact *)
                     let host = Flow_gpu.Host.create ~clock:Unix.gettimeofday gpu in
                     Fun.protect
                       ~finally:(fun () -> Flow_gpu.Host.close host)
                       (fun () ->
                         let config =
                           Flow_ir.Gpu.{ default with backend = Some (Flow_gpu.Host.backend host) }
                         in
                         let readback = Flow_ir.Executor.region_readback ~gpu:config () in
                         (match Test_program.ok (readback program ~live) with
                         | None -> print_endline "exact precision keeps the region on the CPU"
                         | Some _ -> failwith "exact precision sent a region to the device");
                         let readback =
                           Flow_ir.Executor.region_readback ~gpu:{ config with fast = true } ()
                         in
                         match Test_program.ok (readback program ~live) with
                         | Some value when array value = expected ->
                             print_endline "fast precision reads the region back from the device"
                         | Some _ -> failwith "the device region differs from the CPU"
                         | None -> failwith "fast precision kept a 4,096-element region on the CPU")));
                (let program = Test_program.two_outputs () in
                 let runner =
                   Flow_gpu.Run.create gpu pipelines
                     (Test_program.ok (Flow_shader.Emit.kernel program))
                 in
                 Fun.protect
                   ~finally:(fun () -> Flow_gpu.Run.close runner)
                   (fun () ->
                     let live = Input.at_time 1. in
                     let output =
                       Test_program.ok
                         (Flow_gpu.Run.dispatch runner
                            (Test_program.ok (Flow_ir.Packed.Host.prepare program ~live)))
                     in
                     let expected = Test_program.ok (Flow_ir.Packed.force_all program ~live) in
                     List.iteri
                       (fun index expected ->
                         let selected = Option.get (Flow_gpu.Run.select_output ~index output) in
                         assert (
                           Flow_gpu.Run.width selected
                           = Array.length (Flow_ir.Packed.Host.view program).outputs.(index));
                         assert (
                           match
                             (Flow_gpu.Run.buffer selected, Flow_gpu.Run.buffer ~index output)
                           with
                           | Some selected, Some original -> selected == original
                           | None, None -> true
                           | _ -> false);
                         assert (
                           array (Test_program.ok (Flow_gpu.Run.readback selected)) = array expected))
                       expected;
                     assert (Flow_gpu.Run.select_output ~index:(-1) output = None);
                     assert (Flow_gpu.Run.select_output ~index:(List.length expected) output = None);
                     let host = Flow_gpu.Host.create ~clock:Unix.gettimeofday gpu in
                     Fun.protect
                       ~finally:(fun () -> Flow_gpu.Host.close host)
                       (fun () ->
                         let kernel =
                           Test_program.ok
                             ((Flow_gpu.Host.backend host).prepare ~fast:false program)
                         in
                         let value =
                           Test_program.ok
                             (kernel.run
                                (Test_program.ok (Flow_ir.Packed.Host.prepare program ~live)))
                         in
                         List.iteri
                           (fun output expected ->
                             let width =
                               Array.length (Flow_ir.Packed.Host.view program).outputs.(output)
                             in
                             assert (
                               array
                                 (Test_program.ok (kernel.readback { value with output; width }))
                               = array expected))
                           expected;
                         assert (
                           Test_program.ok
                             (Flow_gpu.Host.output host
                                { value with output = List.length expected })
                           = None);
                         ignore
                           (Test_program.ok
                              (kernel.run
                                 (Test_program.ok (Flow_ir.Packed.Host.prepare program ~live))));
                         assert (Test_program.ok (Flow_gpu.Host.output host value) = None))));
                List.iter
                  (fun (body, refuses) ->
                    let hidden =
                      Test_program.compile ("(map (fn [x] " ^ body ^ ") (array/range 1024))")
                    in
                    let msl = Test_program.ok (Flow_shader.Emit.kernel hidden) in
                    let runner = Flow_gpu.Run.create gpu pipelines msl in
                    Fun.protect
                      ~finally:(fun () -> Flow_gpu.Run.close runner)
                      (fun () ->
                        let live = Input.at_time 1. in
                        assert (
                          Array.for_all (( = ) 0.)
                            (array (Test_program.ok (Flow_ir.Packed.force hidden ~live))));
                        let inputs = Test_program.ok (Flow_ir.Packed.Host.prepare hidden ~live) in
                        match Flow_gpu.Run.dispatch runner inputs with
                        | Error diagnostic ->
                            assert (refuses && diagnostic.Diagnostic.code = "E_KERNEL")
                        | Ok output ->
                            assert (not refuses);
                            assert (
                              Array.for_all (( = ) 0.)
                                (array (Test_program.ok (Flow_gpu.Run.readback output))))))
                  [
                    ("(if (> (+ x t) 0) 0 (* (* (+ x t) 1e20) (* (+ x t) 1e20)))", false);
                    ("(min (* (* (+ x t) 1e20) (* (+ x t) 1e20)) 0)", true);
                  ]);
            print_endline
              "GPU run: emitted kernels match the CPU tier within the recorded native noise \
               tolerance"
          end)

(* A drawing's fold whose filter and maps run in fast precision: the resident fold keeps its arrays on
   the device and draws the CPU's points (integers, exact in 32 bits). *)
let resident_fold ?(n = 9000) ?(levels = 3) ?(keep = "(>= (int/mod (int i) 3) 1)")
    ?(slot = "(int i)") gpu ~count:range =
  let ok = Test_program.ok in
  let body =
    Printf.sprintf
      "(let* [p (fold [b (map (fn [i] [(+ i (floor t)) (* i 2) 0]) (array/range %d))] [d (range \
       %d)] (let* [kept (filter (fn [i] %s) (array/range (array/count b)))] (map (fn [i] (let* [q \
       (array/nth b (int (array/nth kept %s)))] [(+ q.x (+ 1 (floor t))) q.y 0])) (array/range \
       %s))))] (draw/points p))"
      n levels keep slot range
  in
  let forms = ok (Flow.Syntax.parse ("(workspace w (graph g :context draw " ^ body ^ "))")) in
  let ws =
    match Flow.Workspace.check { Flow.Catalog.version = 1; kinds = [] } forms with
    | Some ws, [] -> ws
    | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
  in
  let evaluation = ok (Flow.Eval.static ws) in
  let program, root =
    Result.get_ok (Flow_ir.Core_draw.lower evaluation.plan (List.assoc "g" evaluation.results))
  in
  let prepared = Flow_ir.Core_exec.prepare ~remember_loops:true program in
  let live = { (Input.at_time 0.) with size = (200, 100) } in
  let points ?resident prepared =
    let buffers = Flow_ir.Core_exec.Buffers.create () in
    match ok (Flow_ir.Core_exec.draw ?root ?resident prepared ~live ~size:(200, 100) ~buffers) with
    | [| Scene_command.Marks.Mark_set { kind = Point { position = Buffer { id; _ }; _ }; count } |]
      ->
        (count, Array.sub (Flow_ir.Core_exec.Buffers.find buffers id) 0 (count * 3))
    | _ -> failwith "resident fold: one point set expected"
  in
  let host = Flow_gpu.Host.create ~clock:Unix.gettimeofday gpu in
  Fun.protect
    ~finally:(fun () -> Flow_gpu.Host.close host)
    (fun () ->
      let expected = points prepared in
      (* a fresh prepare, so the CPU draw's remembered loop does not answer *)
      let prepared' = Flow_ir.Core_exec.prepare ~remember_loops:true program in
      let r = Flow_gpu.Host.resident ~fast:true host and ran = ref 0 in
      let resident =
        {
          r with
          invoke =
            (fun p ~live ->
              let v = r.invoke p ~live in
              if v <> None then incr ran;
              v);
        }
      in
      let actual = points ~resident prepared' in
      if !ran = 0 then failwith "resident fold: no region ran on the device";
      if actual <> expected then failwith "resident fold: device points differ from the CPU";
      let bad_body =
        "(let* [p (fold [b (map (fn [i] [i (* i 2) 0]) (array/range 9000))] [d (range 2)] (map (fn \
         [i] [(float (int/div (int i) (int/sub (frame/index) (frame/index)))) 0 0]) (array/range \
         (array/count b))))] (draw/points p))"
      in
      let bad_forms =
        ok (Flow.Syntax.parse ("(workspace w (graph g :context draw " ^ bad_body ^ "))"))
      in
      let bad_ws =
        match Flow.Workspace.check { Flow.Catalog.version = 1; kinds = [] } bad_forms with
        | Some ws, [] -> ws
        | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
      in
      let bad_eval = ok (Flow.Eval.static bad_ws) in
      let bad_program, bad_root =
        Result.get_ok (Flow_ir.Core_draw.lower bad_eval.plan (List.assoc "g" bad_eval.results))
      in
      let bad_prepared = Flow_ir.Core_exec.prepare ~remember_loops:true bad_program in
      let bad =
        Flow_ir.Core_exec.draw ?root:bad_root
          ~resident:(Flow_gpu.Host.resident ~fast:true host)
          bad_prepared ~live ~size:(200, 100)
          ~buffers:(Flow_ir.Core_exec.Buffers.create ())
      in
      (match bad with
      | Error d -> assert (d.Diagnostic.code = "E_RANGE" && d.message = "Integer division by zero.")
      | Ok _ -> failwith "resident fold did not report its integer division status");
      (* fast display dispatches left in flight (never borrowed) between resident draws share the
         queue's three command buffers with the fold's levels: none is refused, and each draw, on
         buffers the last ones left, is the CPU's *)
      let display = List.assoc "arithmetic" (Test_program.fixtures 4096) in
      let inputs = ok (Flow_ir.Packed.Host.prepare ~fast:true display ~live:(Input.at_time 1.)) in
      let backend = Flow_gpu.Host.backend host in
      let kernels = List.init 2 (fun _ -> ok (backend.prepare ~fast:true display)) in
      for _ = 1 to 4 do
        List.iter (fun (kernel : Flow_ir.Gpu.kernel) -> ignore (ok (kernel.run inputs))) kernels;
        if
          points
            ~resident:(Flow_gpu.Host.resident ~fast:true host)
            (Flow_ir.Core_exec.prepare ~remember_loops:true program)
          <> expected
        then failwith "resident fold: a later draw's device points differ from the CPU"
      done;
      Printf.printf "resident fold over %s: %d device regions draw the CPU's %d points\n" range !ran
        (fst expected))

(* One host draws a resident fold again and again while its counts (k of every 8 kept, doubled, each
   level) grow past the capacities earlier draws gave its slots, shrink to none, then grow again:
   every draw is the CPU's, and the cut ones ran again. *)
let resident_history gpu =
  let ok = Test_program.ok in
  let body =
    "(let* [p (fold [b (map (fn [i] [i (* i 2) 0]) (array/range 2000))] [d (range 3)] (let* [kept \
     (filter (fn [i] (< (int/mod (int i) 8) (int t))) (array/range (array/count b)))] (map (fn [i] \
     (let* [q (array/nth b (int (array/nth kept (int/div (int i) 2))))] [(+ q.x d) (+ q.y (mod i \
     2)) 0])) (array/range (* (array/count kept) 2)))))] (draw/points p))"
  in
  let forms = ok (Flow.Syntax.parse ("(workspace w (graph g :context draw " ^ body ^ "))")) in
  let ws =
    match Flow.Workspace.check { Flow.Catalog.version = 1; kinds = [] } forms with
    | Some ws, [] -> ws
    | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
  in
  let evaluation = ok (Flow.Eval.static ws) in
  let program, root =
    Result.get_ok (Flow_ir.Core_draw.lower evaluation.plan (List.assoc "g" evaluation.results))
  in
  let points ?resident prepared t =
    let buffers = Flow_ir.Core_exec.Buffers.create () in
    let live = { (Input.at_time t) with size = (200, 100) } in
    match ok (Flow_ir.Core_exec.draw ?root ?resident prepared ~live ~size:(200, 100) ~buffers) with
    | [| Scene_command.Marks.Mark_set { kind = Point { position = Buffer { id; _ }; _ }; count } |]
      ->
        (count, Array.sub (Flow_ir.Core_exec.Buffers.find buffers id) 0 (count * 3))
    | _ -> failwith "resident history: one point set expected"
  in
  let host = Flow_gpu.Host.create ~clock:Unix.gettimeofday gpu in
  Fun.protect
    ~finally:(fun () -> Flow_gpu.Host.close host)
    (fun () ->
      let prepared = Flow_ir.Core_exec.prepare ~remember_loops:true program in
      let cuts = ref 0 in
      List.iter
        (fun t ->
          let expected = points (Flow_ir.Core_exec.prepare ~remember_loops:true program) t in
          let r = Flow_gpu.Host.resident ~fast:true host in
          let resident =
            {
              r with
              finish =
                (fun v ->
                  try r.finish v
                  with Flow_ir.Core_exec.Resident_overflow as e ->
                    incr cuts;
                    raise e);
            }
          in
          if points ~resident prepared t <> expected then
            failwith (Printf.sprintf "resident history: the draw at t = %g differs from the CPU" t))
        [ 1.; 6.; 6.; 2.; 0.; 1.; 7.; 8.; 3.; 8. ];
      if !cuts = 0 then failwith "resident history: no draw outgrew its slots";
      Printf.printf
        "resident history: 10 draws growing past, shrinking below and regrowing their slots (%d \
         cut) are the CPU's\n"
        !cuts)

(* A counted dispatch (Run.dispatch ~count) sizes its grid on the device from a count cell, never
   reading it: for device counts of 0, 1, a group of 256 either side of one, the capacity and past
   it, times mul over div, the kernel writes its count record {n, 6n, 1, 0, 0, 0} and its first n
   elements are the uncounted dispatch's, over a capacity of 4,096 and buffers filled with garbage. *)
let counted gpu =
  let module B = Ogpu.Backend in
  let get = function Ok x -> x | Error _ -> failwith "counted: device call failed" in
  let device = Renderer.gpu_device gpu and queue = Renderer.gpu_queue gpu in
  let pipelines = Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday device in
  let packed = List.assoc "arithmetic" (Test_program.fixtures 4096) in
  let values =
    Test_program.ok (Flow_ir.Packed.Host.prepare ~fast:true packed ~live:(Input.at_time 1.))
  in
  let runner =
    Flow_gpu.Run.create gpu pipelines (Test_program.ok (Flow_shader.Emit.kernel ~fast:true packed))
  in
  let buffer bytes =
    get
      (B.create_buffer device
         { label = Some "counted test"; size = Int64.of_int bytes; usage = [ Storage; Copy_src ] })
  in
  Fun.protect
    ~finally:(fun () ->
      Flow_gpu.Run.close runner;
      Flow_gpu.Pipelines.close pipelines)
    (fun () ->
      let full = Test_program.ok (Flow_gpu.Run.dispatch runner values) in
      let width = Flow_gpu.Run.width full in
      let expected = array (Test_program.ok (Flow_gpu.Run.readback full)) in
      let check (c, mul, div) =
        let n = min values.count (c * mul / div) in
        let out = buffer (values.count * width * 4) and record = buffer 24 and cell = buffer 4 in
        Fun.protect
          ~finally:(fun () -> List.iter (fun b -> ignore (B.destroy_buffer b)) [ out; record; cell ])
          (fun () ->
            get (B.write_buffer out ~offset:0L (Bytes.make (values.count * width * 4) '\xff'));
            get (B.write_buffer record ~offset:0L (Bytes.make 24 '\x07'));
            let word = Bytes.create 4 in
            Bytes.set_int32_le word 0 (Int32.of_int c);
            get (B.write_buffer cell ~offset:0L word);
            let commands = get (B.begin_commands queue) in
            ignore
              (Test_program.ok
                 (Flow_gpu.Run.dispatch ~commands ~count:(cell, mul, div) ~into:(out, record)
                    runner values));
            get (B.complete_through queue (get (B.commit commands)).epoch);
            let words = get (B.read_buffer record ~offset:0L ~length:24) in
            let floats =
              get (Flow_gpu.Run.read_floats queue out ~epoch:0L ~length:(values.count * width))
            in
            if
              List.init 6 (fun k -> Int32.to_int (Bytes.get_int32_le words (4 * k)))
              <> [ n; 6 * n; 1; 0; 0; 0 ]
              || Array.sub floats 0 (n * width) <> Array.sub expected 0 (n * width)
            then
              failwith
                (Printf.sprintf "counted: device count %d x %d / %d: not the first %d elements" c
                   mul div n))
      in
      List.iter check
        [
          (0, 1, 1); (1, 1, 1); (255, 1, 1); (256, 1, 1); (257, 1, 1); (4096, 1, 1); (9999, 1, 1);
          (100, 3, 1); (171, 3, 2); (513, 1, 2);
        ];
      print_endline
        "counted dispatch: the device count sizes the grid, from 0 past the capacity, mul / div")

(* Run.compact against the CPU at every edge of its blocks of 1,024: empty, one element, a block
   either side of one, several blocks; an irregular mask, all kept, none kept; the count the
   capacity or only on the device (count-in below it). A wrong block offset moves every index past
   the first block. *)
let compaction gpu =
  let module B = Ogpu.Backend in
  let get = function Ok x -> x | Error _ -> failwith "compaction: device call failed" in
  let device = Renderer.gpu_device gpu and queue = Renderer.gpu_queue gpu in
  let pipelines = Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday device in
  let check (n, count_in, pattern) =
    let mask = Array.init n (fun i -> if pattern i then 1. else 0.) in
    let buffer bytes =
      get
        (B.create_buffer device
           {
             label = Some "compaction test";
             size = Int64.of_int (max 4 bytes);
             usage = [ Storage; Copy_src; Copy_dst ];
           })
    in
    let m = buffer (4 * n) and kept = buffer (4 * n) and total = buffer 4 and cell = buffer 4 in
    let blocks = buffer (Flow_gpu.Run.compact_blocks n) and groups = buffer 12 in
    Fun.protect
      ~finally:(fun () ->
        List.iter (fun b -> ignore (B.destroy_buffer b)) [ m; kept; total; cell; blocks; groups ])
      (fun () ->
        let bytes = Bytes.create (max 4 (4 * n)) in
        Array.iteri (fun i x -> Bytes.set_int32_le bytes (4 * i) (Int32.bits_of_float x)) mask;
        get (B.write_buffer m ~offset:0L bytes);
        let count = Option.value ~default:n count_in in
        let word = Bytes.create 4 in
        Bytes.set_int32_le word 0 (Int32.of_int count);
        get (B.write_buffer cell ~offset:0L word);
        let commands = get (B.begin_commands queue) in
        (match
           Flow_gpu.Run.compact commands pipelines ~mask:m ~count:n
             ~count_in:(Option.map (fun _ -> cell) count_in)
             ~kept ~total ~blocks ~groups
         with
        | Ok () -> ()
        | Error d -> failwith (Diagnostic.to_string d));
        let receipt = get (B.commit commands) in
        get (B.complete_through queue receipt.epoch);
        let expected = List.filter (fun i -> mask.(i) <> 0.) (List.init count Fun.id) in
        let k = List.length expected in
        let total' =
          Int32.to_int (Bytes.get_int32_le (get (B.read_buffer total ~offset:0L ~length:4)) 0)
        in
        let kept' = get (B.read_buffer kept ~offset:0L ~length:(max 4 (4 * k))) in
        if
          total' <> k
          || List.exists
               (fun (j, i) -> Int32.float_of_bits (Bytes.get_int32_le kept' (4 * j)) <> float i)
               (List.mapi (fun j i -> (j, i)) expected)
        then
          failwith (Printf.sprintf "compaction: n %d: device kept indices differ from the CPU" n))
  in
  let irregular i = (i * 2654435761) lsr 13 land 7 < 3 in
  Fun.protect
    ~finally:(fun () -> Flow_gpu.Pipelines.close pipelines)
    (fun () ->
      List.iter check
        (List.concat_map
           (fun n ->
             [
               (n, None, irregular);
               (n, Some (n * 5 / 6), irregular);
               (n, None, Fun.const true);
               (n, Some (n / 2), Fun.const false);
             ])
           [ 0; 1; 1023; 1024; 1025; 300_000 ]);
      print_endline
        "compaction: the CPU's kept indices from 0 to 300,000 elements, every mask and device \
         count")

let () =
  let module H = Flow_gpu.Host in
  let handles = Ogpu.Impl.live_handles in
  let baseline = handles () in
  let gpu =
    match Renderer.acquire_gpu () with
    | Ok gpu -> gpu
    | Error e -> failwith (Format.asprintf "%a" Renderer.pp_error e)
  in
  Fun.protect
    ~finally:(fun () -> Renderer.release_gpu gpu)
    (fun () ->
      (* the device keeps a count it can take ([array/range] of a count times a factor) and reads
       back one it cannot *)
      resident_fold gpu ~count:"(array/count kept)";
      resident_fold gpu ~count:"(+ (* (array/count kept) 1) 0)";
      resident_fold ~n:400_000 gpu ~count:"(array/count kept)";
      (* the grid follows the device count, not the bound: tripled each level while the count
         stays (a third kept, each three times), the bound passes the count 27 times over, then
         256K, where it settles to the device count; halved, the count drops below a halved bound;
         emptied, every later count is 0 *)
      let third = "(= (int/mod (int i) 3) 1)" in
      resident_fold gpu ~levels:6 ~keep:third ~slot:"(int/div (int i) 3)"
        ~count:"(* (array/count kept) 3)";
      resident_fold gpu ~count:"(int/div (array/count kept) 2)";
      resident_fold gpu ~keep:"(< (int i) 0)" ~count:"(array/count kept)";
      resident_history gpu;
      counted gpu;
      compaction gpu;
      let host = H.create ~clock:Unix.gettimeofday gpu in
      Fun.protect
        ~finally:(fun () -> H.close host)
        (fun () ->
          let packed = List.assoc "arithmetic" (Test_program.fixtures 1024) in
          let inputs =
            Flow_ir.Packed.Host.prepare packed ~live:(Input.at_time 1.) |> Test_program.ok
          in
          let backend = H.backend host in
          let first = backend.prepare ~fast:false packed |> Test_program.ok in
          let value = first.run inputs |> Test_program.ok in
          ignore (first.readback value |> Test_program.ok);
          assert (Option.is_some (Test_program.ok (H.output host value)));
          let initial = H.stats host in
          assert (
            initial.runners_created = 1 && initial.runners_released = 0 && initial.status_reads = 1);
          for _ = 1 to 64 do
            let kernel = backend.prepare ~fast:false packed |> Test_program.ok in
            ignore (kernel.run inputs |> Test_program.ok)
          done;
          let evicted = H.stats host in
          assert (
            evicted.runners_created >= 65
            && evicted.runners_released = evicted.runners_created - 64
            && evicted.pipeline_compilations = 1 && evicted.status_reads = 65);
          assert (
            evicted.buffer_creations = 65 * initial.buffer_creations
            && evicted.input_uploads = 65 * initial.input_uploads
            && evicted.input_uploaded_bytes = 65 * initial.input_uploaded_bytes
            && evicted.readback_bytes = initial.readback_bytes
            && initial.readback_bytes = 4096);
          assert (Test_program.ok (H.output host value) = None);
          ignore (first.run inputs |> Test_program.ok);
          let renewed = H.stats host in
          assert (
            renewed.runners_created = evicted.runners_created + 1
            && renewed.runners_released = evicted.runners_released + 1
            && renewed.status_reads = 66);
          assert (
            Domain.join
              (Domain.spawn (fun () ->
                   try
                     ignore (H.stats host);
                     false
                   with Invalid_argument _ -> true)));
          H.close host;
          H.close host;
          let closed = H.stats host in
          assert (
            closed.runners_created = renewed.runners_created
            && closed.runners_released = closed.runners_created
            && closed.pipeline_compilations = 1 && closed.pipeline_releases = 1
            && closed.status_reads = renewed.status_reads
            && closed.readback_bytes = initial.readback_bytes
            && closed.buffer_creations = renewed.buffer_creations
            && closed.input_uploads = renewed.input_uploads
            && closed.input_uploaded_bytes = renewed.input_uploaded_bytes)));
  assert (handles () = baseline);
  print_endline
    "GPU Host counters: eviction, reacquisition and idempotent close preserve cumulative \
     successful work, zero handles"
