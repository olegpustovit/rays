module B = Ogpu.Backend
module R = Flow_gpu.Run

let get = function Ok value -> value | Error error -> failwith (Ogpu.Error.to_string error)

let wrong_domain f =
  Domain.join
    (Domain.spawn (fun () ->
         try
           f ();
           false
         with Invalid_argument _ -> true))

(* a retired, pinned output with no reader left: only its stand-in, pin and release stay *)
let[@inline never] pinned_holder run values =
  let planes = Test_program.ok (R.keep (Test_program.ok (R.dispatch run values))) in
  ignore (Test_program.ok (planes.settle ()));
  let stand_in =
    match planes.resident with
    | Some (Float_array a | Vec2_array a | Vec3_array a | Vec4_array a) -> a
    | _ -> failwith "the output has no stand-in"
  in
  let _, _, valid, unpin = Option.get (R.pin stand_in) in
  planes.release ~held:true;
  (stand_in, valid, unpin, planes.release)

let () =
  let base = Ogpu.Impl.create_driver () in
  let live = Ogpu.Impl.live_handles in
  let fail = ref false and pipelines = ref 0 and submitted = ref 0 and invalid_output = ref false in
  let fail_table = ref false and table_writes = ref 0 in
  let fail_input = ref false and input_writes = ref 0 and creates = ref 0 in
  let working_set = ref None in
  (* This driver checks ownership and failed-command lifetimes only. It does
     not execute a shader or stand in for native numerical qualification. *)
  let driver =
    B.
      {
        create_device =
          (fun () ->
            Result.map
              (fun raw ->
                {
                  raw with
                  capabilities = { raw.capabilities with compute_pipeline = true };
                  recommended_working_set =
                    (fun () ->
                      match !working_set with
                      | Some bytes -> bytes
                      | None -> raw.recommended_working_set ());
                  create_buffer =
                    (fun memory descriptor ->
                      incr creates;
                      Result.map
                        (fun (resource : B.driver_resource) ->
                          {
                            resource with
                            write =
                              (fun offset bytes ->
                                if descriptor.Ogpu.Types.size = 2048L then begin
                                  incr table_writes;
                                  if !fail_table then
                                    Error
                                      (Ogpu.Error.make "test" Invalid_state "injected table upload")
                                  else resource.write offset bytes
                                end
                                else if descriptor.Ogpu.Types.size >= 4096L then begin
                                  incr input_writes;
                                  let result = resource.write offset bytes in
                                  if !fail_input then
                                    Error
                                      (Ogpu.Error.make "test" Invalid_state
                                         "injected partial input upload")
                                  else result
                                end
                                else resource.write offset bytes);
                            read =
                              (fun offset length ->
                                if !invalid_output && length = 4 then (
                                  let flags = Bytes.make 4 '\000' in
                                  Bytes.set_int32_le flags 0 1l;
                                  Ok flags)
                                else resource.read offset length);
                          })
                        (raw.create_buffer memory descriptor));
                  (* inputs are mapped: an upload is the flush of the host view *)
                  create_mapped_buffer =
                    (fun descriptor ->
                      Result.map
                        (fun (mapped : B.driver_mapped) ->
                          {
                            mapped with
                            flush =
                              (fun length ->
                                if descriptor.Ogpu.Types.size >= 4096L then begin
                                  incr input_writes;
                                  let result = mapped.flush length in
                                  if !fail_input then
                                    Error
                                      (Ogpu.Error.make "test" Invalid_state
                                         "injected partial input upload")
                                  else result
                                end
                                else mapped.flush length);
                          })
                        (raw.create_mapped_buffer descriptor));
                  create_library =
                    (fun shader ->
                      Result.map
                        (fun library ->
                          {
                            library with
                            create_compute_pipeline_in =
                              (fun ~entry:_ ~constants:_ ~interface:_ ->
                                incr pipelines;
                                Ok
                                  {
                                    max_threads = 1024;
                                    pipeline_token = 1L;
                                    destroy_pipeline =
                                      (fun () ->
                                        decr pipelines;
                                        Ok ());
                                  });
                          })
                        (raw.create_library shader));
                  create_queue =
                    (fun () ->
                      Result.map
                        (fun queue ->
                          {
                            queue with
                            complete_through =
                              (fun epoch ->
                                if !fail then
                                  Error (Ogpu.Error.make "test" Invalid_state "injected completion")
                                else queue.complete_through epoch);
                            begin_commands =
                              (fun () ->
                                Result.map
                                  (fun commands ->
                                    {
                                      commands with
                                      commit =
                                        (fun () ->
                                          incr submitted;
                                          commands.commit ());
                                      compute_encoder =
                                        (fun () ->
                                          Ok
                                            {
                                              set_pipeline = (fun _ -> Ok ());
                                              set_buffer = (fun ~index:_ ~offset:_ _ -> Ok ());
                                              set_bytes = (fun ~index:_ _ -> Ok ());
                                              set_texture = (fun ~index:_ _ -> assert false);
                                              set_accel = (fun ~index:_ _ -> assert false);
                                              compute_use_accels = (fun _ -> assert false);
                                              dispatch_threads =
                                                (fun ~threads:_ ~threadgroup:_ -> Ok ());
                                              dispatch_indirect =
                                                (fun _ ~offset:_ ~threadgroup:_ -> Ok ());
                                              end_compute = (fun () -> Ok ());
                                            });
                                    })
                                  (queue.begin_commands ()));
                          })
                        (raw.create_queue ()));
                })
              (base.create_device ()));
      }
  in
  let device = get (B.create_device driver) in
  let queue = get (B.create_queue device) in
  let cache = Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday device in
  let packed = List.assoc "arithmetic" (Test_program.fixtures 1024) in
  let msl = Test_program.ok (Flow_shader.Emit.kernel packed) in
  let run = R.Private.create_owned device queue cache msl in
  Fun.protect
    ~finally:(fun () ->
      R.close run;
      Flow_gpu.Pipelines.close cache;
      ignore (B.destroy_queue queue);
      ignore (B.destroy_device device))
    (fun () ->
      let values = Test_program.ok (Flow_ir.Packed.Host.prepare packed ~live:(Input.at_time 1.)) in
      let first = Test_program.ok (R.dispatch run values) in
      assert (R.buffer first <> None);
      assert (Domain.join (Domain.spawn (fun () -> R.buffer first = None)));
      assert (wrong_domain (fun () -> R.close run));
      assert (wrong_domain (fun () -> Flow_gpu.Pipelines.close cache));
      ignore (Test_program.ok (R.dispatch run values));
      assert (!input_writes = 1 && R.Private.input_uploads run = 1);
      let replacement = { values with arrays = Array.map Array.copy values.arrays } in
      replacement.arrays.(0).(0) <- 17.;
      fail_input := true;
      assert (Result.is_error (R.dispatch run replacement));
      assert (!input_writes = 2 && R.Private.input_uploads run = 1);
      fail_input := false;
      ignore (Test_program.ok (R.dispatch run values));
      assert (!input_writes = 3);
      ignore (Test_program.ok (R.dispatch run replacement));
      ignore (Test_program.ok (R.dispatch run replacement));
      assert (!input_writes = 4);
      ignore (Test_program.ok (R.dispatch run { replacement with count = 512 }));
      ignore (Test_program.ok (R.dispatch run replacement));
      assert (!input_writes = 6 && R.Private.input_uploads run = 5);
      (* the bytes each upload covered: four of 1,024 floats and one of 512 *)
      assert (R.Private.input_uploaded_bytes run = (4 * 4096) + 2048);
      fail := true;
      let larger = List.assoc "arithmetic" (Test_program.fixtures 65536) in
      let values = Test_program.ok (Flow_ir.Packed.Host.prepare larger ~live:(Input.at_time 1.)) in
      assert (Result.is_error (R.dispatch run values));
      assert (R.buffer first = None && Result.is_error (R.readback first));
      assert (R.Private.buffer_creations run = 5 && !submitted = 8 && !input_writes = 7);
      fail := false;
      let fresh = Test_program.ok (R.dispatch run values) in
      assert (R.buffer fresh <> None);
      invalid_output := true;
      assert (
        match R.dispatch run values with Error d -> d.Diagnostic.code = "E_KERNEL" | Ok _ -> false);
      assert (R.buffer fresh = None);
      invalid_output := false;
      let invalid = { values with arrays = Array.map Array.copy values.arrays } in
      invalid.arrays.(0).(0) <- 3.5e38;
      assert (Result.is_error (R.dispatch run invalid));
      assert (R.buffer fresh = None);
      (* A live graph's cooks: each keeps its output, settles it (as Sop.Session does), hands it
         out and retires it (a root a caller took), and drops it. A dropped holder is collected
         by the next major cycle and its buffer comes back to the pool, so the pool holds only
         the newest and nothing is read back to make room. Settling reaches the earlier holders'
         flags without a Weak.get, which during marking would keep every one of them alive. *)
      let values = Test_program.ok (Flow_ir.Packed.Host.prepare packed ~live:(Input.at_time 1.)) in
      let creations = R.Private.buffer_creations run and thaws = R.Private.materialisations run in
      for _ = 1 to 24 do
        let output = Test_program.ok (R.dispatch run values) in
        let planes = Test_program.ok (R.keep output) in
        ignore (Test_program.ok (planes.settle ()));
        planes.release ~held:true;
        Gc.major ()
      done;
      Gc.major ();
      assert (R.Private.materialisations run = thaws);
      assert (R.Private.held_buffers run <= 2);
      assert (R.Private.buffer_creations run - creations <= 4);
      (* Over the retired budget the oldest holder's buffer comes back, and it stays readable: a
         retired holder may still have a reader, so it is read first, never freed unread. *)
      working_set := Some 4L;
      let oldest = Test_program.ok (R.keep (Test_program.ok (R.dispatch run values))) in
      ignore (Test_program.ok (oldest.settle ()));
      let expected_count = oldest.count * oldest.width in
      oldest.release ~held:true;
      let before = R.Private.materialisations run in
      let later =
        List.init 8 (fun _ ->
            let next = Test_program.ok (R.keep (Test_program.ok (R.dispatch run values))) in
            ignore (Test_program.ok (next.settle ()));
            next)
      in
      assert (R.Private.materialisations run > before);
      (match oldest.materialise () with
      | planes -> assert (Array.fold_left (fun n p -> n + Array.length p) 0 planes = expected_count)
      | exception Invalid_argument message ->
          failwith ("a retired holder was freed under a live reader: " ^ message));
      List.iter (fun (next : Flow_ir.Gpu.planes) -> next.release ~held:false) later;
      working_set := None;
      let thaws = R.Private.materialisations run in
      (* RAYS_MEM_CLOBBER=1: a buffer back in the pool is NaN the moment it is dead *)
      Result.get_ok (Mem.configure (function "RAYS_MEM_CLOBBER" -> Some "1" | _ -> None));
      let free_owner = Mem.cache ~pool:Device "flow_gpu.free" in
      let free_before = Mem.bytes free_owner in
      let pool = R.pool () in
      let pooled = R.Private.create_owned ~pool device queue cache msl in
      let dead = Test_program.ok (R.keep (Test_program.ok (R.dispatch pooled values))) in
      ignore (Test_program.ok (dead.settle ()));
      dead.release ~held:false;
      R.Private.pool_drain pool;
      (match R.Private.pool_free_floats pool with
      | [] -> failwith "the dead buffer did not come back to the pool"
      | floats ->
          List.iter
            (fun floats ->
              for i = 0 to Bigarray.Array1.dim floats - 1 do
                assert (Float.is_nan floats.{i})
              done)
            floats);
      (* idle ageing: a free buffer nothing takes goes back to the device, and the ledger follows *)
      assert (Mem.bytes free_owner > free_before);
      for _ = 1 to 1100 do
        R.Private.pool_drain pool
      done;
      assert (R.Private.pool_free_floats pool = []);
      assert (Mem.bytes free_owner = free_before);
      Result.get_ok (Mem.configure (fun _ -> None));
      R.close ~discard:true pooled;
      R.close_pool pool;
      (* free buffers of one size class past the budget do not turn another class's buffer away:
         it would be destroyed and made again each frame, a cook asking for a collection *)
      working_set := Some (Int64.of_int (2048 lsl 20));
      let pool = R.pool () in
      let pooled = R.Private.create_owned ~pool device queue cache msl in
      for _ = 1 to 5 do
        R.Private.pool_give pool device (64 lsl 20)
      done;
      let small = Test_program.ok (R.keep (Test_program.ok (R.dispatch pooled values))) in
      ignore (Test_program.ok (small.settle ()));
      small.release ~held:false;
      R.Private.pool_drain pool;
      assert (List.length (R.Private.pool_free_floats pool) = 6);
      (* and all the classes together stop at a quarter of the device's working set: the small
         buffer the dispatch took comes back to no room, and is destroyed *)
      for _ = 1 to 2 do
        R.Private.pool_give pool device (128 lsl 20)
      done;
      let over = Test_program.ok (R.keep (Test_program.ok (R.dispatch pooled values))) in
      ignore (Test_program.ok (over.settle ()));
      over.release ~held:false;
      R.Private.pool_drain pool;
      assert (List.length (R.Private.pool_free_floats pool) = 7);
      working_set := None;
      R.close ~discard:true pooled;
      R.close_pool pool;
      (* an evicted runner on a shared pool reads nothing: its live holder stays unread and readable,
         until the pool closes *)
      let pool = R.pool () in
      let evicted = R.Private.create_owned ~pool device queue cache msl in
      let kept = Test_program.ok (R.keep (Test_program.ok (R.dispatch evicted values))) in
      let unread = Test_program.ok (R.keep (Test_program.ok (R.dispatch evicted values))) in
      kept.release ~held:true;
      R.close evicted;
      assert (R.Private.materialisations evicted = 0);
      assert (Array.fold_left (fun n p -> n + Array.length p) 0 (kept.materialise ()) = kept.count * kept.width);
      R.close_pool pool;
      assert (match unread.materialise () with _ -> false | exception Invalid_argument _ -> true);
      (* A display's pin and a session's release keep a holder's fate, not the holder: with no reader
         left it is collected under the pin (a pin finds a holder through its weak entry, so [None]
         is the holder gone), and the unpin alone gives the buffer back. *)
      let pool = R.pool () in
      let pooled = R.Private.create_owned ~pool device queue cache msl in
      let stand_in, valid, unpin, release = pinned_holder pooled values in
      Gc.full_major ();
      Gc.full_major ();
      assert (R.pin stand_in = None);
      assert (valid () && R.Private.pool_held pool = 1);
      unpin ();
      assert ((not (valid ())) && R.Private.pool_held pool = 0);
      release ~held:true;
      (* ... and a pin of a collected holder learns of the pool's close *)
      let _, valid, _, _ = pinned_holder pooled values in
      Gc.full_major ();
      Gc.full_major ();
      assert (valid ());
      R.close ~discard:true pooled;
      R.close_pool pool;
      assert (not (valid ()));
      (* a slot asking far under its size for long enough comes back down *)
      let big =
        Test_program.ok
          (Flow_ir.Packed.Host.prepare
             (List.assoc "arithmetic" (Test_program.fixtures 400_000))
             ~live:(Input.at_time 1.))
      in
      let shrinking = R.Private.create_owned device queue cache msl in
      ignore (Test_program.ok (R.dispatch shrinking big));
      let grown = R.Private.buffer_creations shrinking in
      for _ = 1 to R.Private.slot_shrink_after do
        ignore (Test_program.ok (R.dispatch shrinking values))
      done;
      assert (R.Private.buffer_creations shrinking > grown);
      let settled = R.Private.buffer_creations shrinking in
      for _ = 1 to 16 do
        ignore (Test_program.ok (R.dispatch shrinking values))
      done;
      assert (R.Private.buffer_creations shrinking = settled);
      R.close ~discard:true shrinking;
      (* a host's shutdown closes with [~discard:true]: a live holder is not read back *)
      let live = Test_program.ok (R.keep (Test_program.ok (R.dispatch run values))) in
      ignore (Test_program.ok (live.settle ()));
      R.close ~discard:true run;
      assert (R.Private.materialisations run = thaws);
      assert (match live.materialise () with _ -> false | exception Invalid_argument _ -> true);
      R.close run;
      assert (R.buffer fresh = None);
      let packed = List.assoc "noise" (Test_program.fixtures 1024) in
      let noise =
        R.Private.create_owned device queue cache (Test_program.ok (Flow_shader.Emit.kernel packed))
      in
      Fun.protect
        ~finally:(fun () -> R.close noise)
        (fun () ->
          let values =
            Test_program.ok (Flow_ir.Packed.Host.prepare packed ~live:(Input.at_time 1.))
          in
          fail_table := true;
          assert (Result.is_error (R.dispatch noise values));
          assert (!table_writes = 1);
          fail_table := false;
          ignore (Test_program.ok (R.dispatch noise values));
          assert (!table_writes = 2);
          ignore (Test_program.ok (R.dispatch noise values));
          assert (!table_writes = 2));
      (* a resident vec3's bounds: the reduction's lanes are one buffer made once, and a box is
         read back once per output however often it is asked *)
      let packed = Test_program.compile "(map (fn [x] [x t 0.0]) (array/range 1024))" in
      let sea =
        R.Private.create_owned device queue cache (Test_program.ok (Flow_shader.Emit.kernel packed))
      in
      Fun.protect
        ~finally:(fun () -> R.close sea)
        (fun () ->
          let values =
            Test_program.ok (Flow_ir.Packed.Host.prepare packed ~live:(Input.at_time 1.))
          in
          let settled () =
            let planes = Test_program.ok (R.keep (Test_program.ok (R.dispatch sea values))) in
            ignore (Test_program.ok (planes.settle ()));
            planes
          in
          let first = settled () in
          let c0 = !creates in
          ignore (first.bounds ());
          let read = R.Private.readback_bytes sea in
          assert (!creates - c0 = 1 && read = 1024 * 24);
          ignore (first.bounds ());
          assert (R.Private.readback_bytes sea = read);
          let second = settled () in
          let c1 = !creates in
          ignore (second.bounds ());
          assert (!creates = c1 && R.Private.readback_bytes sea = 2 * read);
          assert (R.Private.bounds_readback_bytes sea = 2 * read);
          ignore (Domain.join (Domain.spawn first.materialise));
          let payload = 1024 * 3 * 4 in
          assert (R.Private.readback_bytes sea = (2 * read) + payload);
          ignore (first.materialise ());
          assert (R.Private.readback_bytes sea = (2 * read) + payload);
          assert (R.Private.bounds_readback_bytes sea = 2 * read);
          first.release ~held:false;
          second.release ~held:false));
  assert (live () = 0 && !pipelines = 0);
  print_endline
    "GPU output lifetimes: failed completion/reallocation invalidation, float32 input bounds, \
     retired holders collected and reused unread, and creating-domain close passed"
