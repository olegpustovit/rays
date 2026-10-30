let get = function Ok value -> value | Error error -> failwith (Ogpu.Error.to_string error)

let () =
  let base = Ogpu.Impl.create_driver () in
  let live = Ogpu.Impl.live_handles in
  (* The portable mock deliberately cannot execute compute. This test driver
     adds owned pipeline handles only; shader execution stays in test_run. *)
  let pipelines = ref 0 and next = ref 0L in
  let driver =
    Ogpu.Backend.
      {
        create_device =
          (fun () ->
            Result.map
              (fun raw ->
                {
                  raw with
                  capabilities = { raw.capabilities with compute_pipeline = true };
                  create_library =
                    (fun shader ->
                      Result.map
                        (fun library ->
                          {
                            library with
                            create_compute_pipeline_in =
                              (fun ~entry:_ ~constants:_ ~interface:_ ->
                                incr pipelines;
                                next := Int64.succ !next;
                                Ok
                                  {
                                    max_threads = 1024;
                                    pipeline_token = !next;
                                    destroy_pipeline =
                                      (fun () ->
                                        decr pipelines;
                                        Ok ());
                                  });
                          })
                        (raw.create_library shader));
                })
              (base.create_device ()));
      }
  in
  let device = get (Ogpu.Backend.create_device driver) in
  let cache = Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday device in
  Fun.protect
    ~finally:(fun () ->
      Flow_gpu.Pipelines.close cache;
      ignore (Ogpu.Backend.destroy_device device))
    (fun () ->
      let first = List.assoc "arithmetic" (Test_program.fixtures 1024) in
      let same = List.assoc "arithmetic" (Test_program.fixtures 65536) in
      let first_msl = Test_program.ok (Flow_shader.Emit.kernel first) in
      let first = Test_program.ok (Flow_gpu.Pipelines.get cache first_msl) in
      let same =
        Test_program.ok
          (Flow_gpu.Pipelines.get cache (Test_program.ok (Flow_shader.Emit.kernel same)))
      in
      assert (first == same && Flow_gpu.Pipelines.Private.compilations cache = 1);
      for number = 1 to 65 do
        let program =
          Test_program.compile
            (Printf.sprintf "(map (fn [x] (+ x (+ t %d))) (array/range 1024))" number)
        in
        ignore
          (Test_program.ok
             (Flow_gpu.Pipelines.get cache (Test_program.ok (Flow_shader.Emit.kernel program))))
      done;
      assert (Flow_gpu.Pipelines.Private.count cache = 64);
      assert (Flow_gpu.Pipelines.Private.releases cache = 2);
      Flow_gpu.Pipelines.close cache;
      assert (Flow_gpu.Pipelines.Private.releases cache = 66);
      Flow_gpu.Pipelines.close cache;
      assert (Flow_gpu.Pipelines.Private.releases cache = 66);
      assert (Result.is_error (Flow_gpu.Pipelines.get cache first_msl)));
  assert (live () = 0);
  assert (!pipelines = 0);
  print_endline
    "GPU pipeline ownership: shared sources, capacity 64, eviction release and idempotent close \
     passed on mock"
