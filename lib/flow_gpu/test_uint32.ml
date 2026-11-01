module E = Flow.Eval
module R = Flow_gpu.Run
module Emit = Flow_shader.Emit

type Flow_ir.Gpu.integer_storage += Other_integer_storage

let ok = Test_program.ok

let program expression =
  Test_program.compile ("(map (fn [(i : int)] " ^ expression ^ ") (array/range 1025 :type int))")

let compaction gpu pipelines n limited =
  let module B = Ogpu.Backend in
  let get = function Ok x -> x | Error e -> failwith (Ogpu.Error.to_string e) in
  let queue = Renderer.gpu_queue gpu and buffers = ref [] in
  let buffer length =
    let b =
      get
        (B.create_buffer (Renderer.gpu_device gpu)
           {
             label = Some "uint32 compaction test";
             size = Int64.of_int (max 4 length);
             usage = [ Storage; Copy_src; Copy_dst ];
           })
    in
    buffers := b :: !buffers;
    b
  in
  Fun.protect
    ~finally:(fun () -> List.iter (fun b -> ignore (B.destroy_buffer b)) !buffers)
    (fun () ->
      let large = n > 1 lsl 24 in
      let indices =
        if large then [ n - 3; n - 2; n - 1 ]
        else List.init n Fun.id |> List.filter (fun i -> i mod 3 = 1)
      in
      let bytes = Bytes.make (max 4 (n * 4)) '\000' in
      List.iter (fun i -> Bytes.set_int32_le bytes (i * 4) 1l) indices;
      let mask = buffer (Bytes.length bytes)
      and kept = buffer (4 * List.length indices)
      and total = buffer 4
      and cell = buffer 4
      and blocks = buffer (R.compact_blocks n)
      and groups = buffer 12 in
      (match R.fill_indices queue pipelines kept ~count:(Int64.to_int (B.buffer_size kept) / 4 + 1) with
      | Error d -> assert (d.code = "E_GPU")
      | Ok () -> failwith "integer fill exceeded buffer capacity");
      get (B.write_buffer mask ~offset:0L bytes);
      let count = if limited then n / 2 else n in
      let word = Bytes.create 4 in
      Bytes.set_int32_le word 0 (Int32.of_int count);
      get (B.write_buffer cell ~offset:0L word);
      let commands = get (B.begin_commands queue) in
      ok
        (R.compact ~uint32:true commands pipelines ~mask ~count:n
           ~count_in:(if limited then Some cell else None)
           ~kept ~total ~blocks ~groups);
      let receipt = get (B.commit commands) in
      get (B.complete_through queue receipt.epoch);
      let expected = List.filter (fun i -> i < count) indices in
      assert (
        Int32.to_int (Bytes.get_int32_le (get (B.read_buffer total ~offset:0L ~length:4)) 0)
        = List.length expected);
      let result = get (B.read_buffer kept ~offset:0L ~length:(max 4 (4 * List.length expected))) in
      List.iteri
        (fun k i -> assert (Int32.to_int (Bytes.get_int32_le result (k * 4)) land 0xffffffff = i))
        expected)

let resident_values gpu =
  let host = Flow_gpu.Host.create ~clock:Unix.gettimeofday gpu in
  Fun.protect ~finally:(fun () -> Flow_gpu.Host.close host) (fun () ->
      List.iter
        (fun length ->
          let before = (Flow_gpu.Host.stats host).payload_readback_bytes in
          let scope = Flow_gpu.Host.resident_scope ~uint32:true ~fast:true host in
          let resident = scope.executor in
          Fun.protect ~finally:resident.close (fun () ->
              let range =
                Option.get (resident.range (Flow.Ty.Array Flow.Ty.Int) (E.Int length) ~div:1 ~length)
              in
              let kept = Option.get (resident.compact range) in
              let filled = Option.get (resident.fill (Flow.Ty.Array Flow.Ty.Int) ~length (E.Int 4294967295)) in
              assert (resident.fill (Flow.Ty.Array Flow.Ty.Int) ~length (E.Int (-1)) = None);
              resident.settle ();
              assert ((Flow_gpu.Host.stats host).payload_readback_bytes = before);
              assert (resident.count kept = Some (max 0 (length - 1)));
              let kept = Option.get (scope.retain_uint32 kept)
              and range = Option.get (scope.retain_uint32 range)
              and filled = Option.get (scope.retain_uint32 filled) in
              assert (kept != range && range != filled && kept != filled);
              resident.close ();
              assert ((Flow_gpu.Host.stats host).payload_readback_bytes = before);
              assert (kept.materialise () = Array.init (max 0 (length - 1)) succ);
              assert (filled.materialise () = Array.make length 4294967295);
              assert (range.materialise () = Array.init length Fun.id)))
        [ 0; 1; 1025; 2049 ];
      List.iter (fun small_first ->
          let before = (Flow_gpu.Host.stats host).payload_readback_bytes in
          let scope = Flow_gpu.Host.resident_scope ~uint32:true ~fast:true host in
          let resident = scope.executor in
          Fun.protect ~finally:resident.close (fun () ->
              let range count = Option.get (resident.range (Flow.Ty.Array Flow.Ty.Int)
                  (E.Int count) ~div:1 ~length:count) in
              let large_value = range 1025 in
              let small_value = range 17 in
              let first = Option.get (scope.retain_uint32 (if small_first then small_value else large_value)) in
              let second = Option.get (scope.retain_uint32 (if small_first then large_value else small_value)) in
              let small, large = if small_first then first, second else second, first in
              assert (small.count = 17 && large.count = 1025);
              assert (Option.get (scope.retain_uint32 small_value) == small);
              assert (resident.count small_value = Some 17 && resident.count large_value = Some 1025);
              let sb, sn, _, su = Option.get (small.pin ())
              and lb, ln, _, lu = Option.get (large.pin ()) in
              assert (sb == lb && sn = 17 && ln = 1025);
              su (); lu ();
              let prefix = small.materialise () in
              assert (prefix = Array.init 17 Fun.id && small.materialise () == prefix);
              small.release ~held:false;
              let copied = Option.get (resident.compact large_value) in
              assert (resident.count copied = Some 1024);
              let copied = Option.get (scope.retain_uint32 copied) in
              resident.close ();
              assert (large.materialise () = Array.init 1025 Fun.id);
              assert ((Flow_gpu.Host.stats host).payload_readback_bytes - before = 1025 * 4);
              assert (copied.materialise () = Array.init 1024 succ)))
        [ false; true ]);
  List.iter
    (fun (body, no_read) ->
      let source =
        "(workspace w (graph g :context value (let* [tested (if (>= t 0.) " ^ body ^ " " ^ body
        ^ ")] 0.)))"
      in
      let workspace =
        match
          Flow.Workspace.check
            { Flow.Catalog.version = 1; kinds = [] }
            (ok (Flow.Syntax.parse source))
        with
        | Some w, [] -> w
        | _, ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))
      in
      let value =
        snd (List.hd (List.assoc [ "g"; "tested" ] (ok (E.static ~record:true workspace)).records))
      in
      let program = ok (Flow_ir.Executor.compile ~strict:true value) in
      let host = Flow_gpu.Host.create ~clock:Unix.gettimeofday gpu in
      Fun.protect
        ~finally:(fun () -> Flow_gpu.Host.close host)
        (fun () ->
          List.iter (fun time ->
          let before_run = Flow_gpu.Host.stats host in
          let scope = Option.get ((Flow_gpu.Host.backend host).residency ~fast:true) in
          let resident = scope.executor in
          Fun.protect ~finally:resident.close (fun () ->
              let live = Input.at_time time in
              let result =
                ok
                  (Flow_ir.Executor.force ~strict:true ~resident program ~live)
              in
              if no_read then assert ((Flow_gpu.Host.stats host).payload_readback_bytes = before_run.payload_readback_bytes);
              (match result with
              | E.Record fields when List.mem_assoc "vertex_points" fields ->
                  ok (Option.get (scope.validate_topology ~point_count:1026
                        ~vertices:(List.assoc "vertex_points" fields)
                        ~offsets:(List.assoc "primitive_offsets" fields)
                        ~kinds:(List.assoc "kinds" fields) ~max_polygon_size:3));
                  assert ((Flow_gpu.Host.stats host).validation_readback_bytes = before_run.validation_readback_bytes + 36)
              | _ -> ());
              let rec capture = function
                | E.Record fields ->
                    let fields = List.map (fun (k, v) -> (k, capture v)) fields in
                    fun () -> E.Record (List.map (fun (k, make) -> (k, make ())) fields)
                | E.Int_array _ as v ->
                    let holder = Option.get (scope.retain_uint32 v) in
                    assert (Option.get (scope.retain_uint32 v) == holder);
                    fun () -> E.Int_array (holder.materialise ())
                | (E.Float_array _ | E.Vec2_array _ | E.Vec3_array _ | E.Vec4_array _) as v ->
                    let holder = Option.get (scope.retain_float v) in
                    fun () ->
                      let planes = holder.materialise () in
                      let values = Array.init (holder.count * holder.width)
                          (fun i -> planes.(i mod holder.width).(i / holder.width)) in
                      (match v with E.Float_array _ -> E.Float_array values
                       | Vec2_array _ -> Vec2_array values | Vec3_array _ -> Vec3_array values
                       | _ -> Vec4_array values)
                | v -> fun () -> v
              in
              let materialise = capture result in
              resident.close ();
              if no_read then assert ((Flow_gpu.Host.stats host).payload_readback_bytes = before_run.payload_readback_bytes);
              let expected = ok (E.Host.force_reference value ~live) in
              let actuals = Parallel.run ~domains:4 (fun () ->
                  Parallel.init_array ~grain:1 8 (fun _ -> materialise ())) in
              Array.iter (fun actual -> assert (actual = expected)) actuals;
              let before = (Flow_gpu.Host.stats host).payload_readback_bytes in
              assert (materialise () = expected);
              assert ((Flow_gpu.Host.stats host).payload_readback_bytes = before))) [ 0.; 1. ]))
    [
      ( {|(let* [xs (map (fn [(i : int)] (int/add (int/add i 16777217) (int/mod (frame/index) 1)))
                           (array/range 1025 :type int))
                copied (map (fn [(i : int)] (int/add i (int/mod (frame/index) 1))) xs)
                gathered (map (fn [(i : int)] (array/nth xs i))
                             (array/range (array/count xs) :type int))]
          {:values xs :alias xs :copied copied :gathered gathered
           :positions (map (fn [(i : int)] [(+ (float i) t) 2. 3.])
                           (array/range 1025 :type int))})|},
        true );
      ( {|(let* [xs (map (fn [(i : int)] (int/add i (int/mod (frame/index) 1)))
                           (array/range 1025 :type int))]
          (map (fn [(x : int)] (+ (float x) t)) xs))|},
        false );
      ( {|{:vertex_points (map (fn [(i : int)] (int/add i (int/mod (frame/index) 1)))
                               (array/range 1026 :type int))
           :primitive_offsets (map (fn [(i : int)] (int/add (int/mul i 3) (int/mod (frame/index) 1)))
                                   (array/range 343 :type int))
           :kinds (map (fn [(i : int)] (int/mod (frame/index) 1)) (array/range 342 :type int))
           :positions (map (fn [(i : int)] [(+ (float i) t) 0. 0.]) (array/range 1026 :type int))}|},
        true );
    ]

let holders gpu pipelines =
  let packed = program "(int/add (if (int/eq (int/mod i 2) 0) 4294967295 16777217) (int/mod (frame/index) 1))" in
  let expected = Array.init 1025 (fun i -> if i mod 2 = 0 then 4294967295 else 16777217) in
  let runner = R.create gpu pipelines (ok (Emit.kernel ~fast:true ~uint32_outputs:[ 0 ] packed)) in
  Fun.protect ~finally:(fun () -> R.close runner) (fun () ->
      let dispatch () =
        let inputs = ok (Flow_ir.Packed.Host.prepare ~fast:true packed ~live:(Input.at_time 0.)) in
        ok (R.keep_uint32 (ok (R.dispatch ~wait:false runner inputs)))
      in
      let first = dispatch () in
      let neutral = R.integers first in
      assert (neutral.count = first.count && neutral.materialise == first.materialise);
      assert (R.pin_integers neutral = None);
      ok (first.settle ());
      assert (R.Private.readback_bytes runner = 0);
      let _, count, valid, unpin = Option.get (R.pin_integers neutral) in
      assert (count = 1025 && valid ());
      let foreign = Domain.spawn (fun () -> R.pin_integers neutral = None) in
      assert (Domain.join foreign);
      let second = dispatch () in
      ok (second.settle ());
      assert (R.Private.readback_bytes runner = 0);
      List.iter (fun domains ->
          let arrays = Parallel.run ~domains (fun () ->
              Parallel.init_array ~grain:1 8 (fun _ -> first.materialise ())) in
          Array.iter (fun a ->
              assert (a == arrays.(0));
              assert (a = expected)) arrays)
        [ 4; 1 ];
      assert (R.Private.readback_bytes runner = 1025 * 4);
      assert (R.Private.materialisations runner = 1);
      assert (valid ());
      unpin (); unpin ();
      assert (not (valid ()));
      assert (first.pin () = None);
      assert (R.pin_integers neutral = None);
      assert (R.pin_integers { neutral with storage = None } = None);
      assert (R.pin_integers { neutral with storage = Some Other_integer_storage } = None);
      let freed = dispatch () in
      ok (freed.settle ());
      freed.release ~held:false;
      assert (freed.pin () = None);
      (try ignore (freed.materialise ()); failwith "freed uint32 holder read"
       with Invalid_argument _ -> ());
      second.release ~held:true;
      R.close runner;
      assert (second.materialise () = expected);
      assert (R.Private.readback_bytes runner = 2 * 1025 * 4));
  let bad = program "(int/add -1 (int/mod (frame/index) 1))" in
  let runner = R.create gpu pipelines (ok (Emit.kernel ~fast:true ~uint32_outputs:[ 0 ] bad)) in
  Fun.protect ~finally:(fun () -> R.close ~discard:true runner) (fun () ->
      let holder = ok (R.keep_uint32 (ok (R.dispatch ~wait:false runner
                        (ok (Flow_ir.Packed.Host.prepare ~fast:true bad ~live:(Input.at_time 0.)))))) in
      (try ignore (holder.materialise ()); failwith "invalid uint32 holder published"
       with Diagnostic.Error d -> assert (d.code = "E_GPU_UINT32_RANGE"));
      assert (R.Private.readback_bytes runner = 0))

let topology gpu pipelines =
  let module B = Ogpu.Backend in
  let get = function Ok x -> x | Error e -> failwith (Ogpu.Error.to_string e) in
  let buffers = ref [] in
  let host = Flow_gpu.Host.create ~clock:Unix.gettimeofday gpu in
  let buffer words =
    let b = get (B.create_buffer (Renderer.gpu_device gpu)
                   { label = Some "topology validator test";
                     size = Int64.of_int (max 4 (Array.length words * 4));
                     usage = [ Storage; Copy_src; Copy_dst ] }) in
    buffers := b :: !buffers;
    let bytes = Bytes.create (Array.length words * 4) in
    Array.iteri (fun i n -> Bytes.set_int32_le bytes (4 * i) (Int32.of_int n)) words;
    if Bytes.length bytes > 0 then get (B.write_buffer b ~offset:0L bytes);
    b
  in
  Fun.protect
    ~finally:(fun () ->
      Flow_gpu.Host.close host;
      List.iter (fun b -> ignore (B.destroy_buffer b)) !buffers)
    (fun () ->
      let status = buffer (Array.make 9 (-1)) in
      let check ?(points = 4) ?(limit = 4294967295) vertices offsets kinds expected =
        let vertices = (buffer vertices, Array.length vertices)
        and offsets = (buffer offsets, Array.length offsets)
        and kinds = (buffer kinds, Array.length kinds) in
        let result =
          R.validate_topology (Renderer.gpu_queue gpu) pipelines
            ~vertices ~offsets ~kinds ~status ~point_count:points
            ~max_polygon_size:limit
        in
        let before = Flow_gpu.Host.stats host in
        let hosted = Flow_gpu.Host.validate_topology host ~vertices ~offsets ~kinds
                       ~point_count:points ~max_polygon_size:limit in
        assert (hosted = result);
        let after = Flow_gpu.Host.stats host in
        assert (after.validation_readback_bytes = before.validation_readback_bytes + 36);
        assert (after.payload_readback_bytes = 0);
        assert (after.readback_bytes = 0);
        assert (after.buffer_creations = 1);
        match result, expected with
        | Ok (), None -> ()
        | Error d, Some (code, message) ->
            assert (d.code = code);
            assert (d.message = message)
        | Error d, None -> failwith (Diagnostic.to_string d)
        | Ok (), Some _ -> failwith "invalid topology accepted"
      in
      let error message = Some ("E_TOPOLOGY", "Topology: " ^ message) in
      let quad = [| 0; 1; 2; 3 |] in
      check quad [| 0; 4 |] [| 0 |] None;
      check [||] [| 0 |] [||] None;
      check [||] [||] [||] (error "primitive offsets must contain the initial zero");
      check quad [| 1; 4 |] [| 0 |] (error "first primitive offset must be zero");
      check quad [| 0; 3 |] [| 0 |] (error "final primitive offset must equal vertex count");
      check quad [| 0; 4 |] [||] (error "primitive kind count must match primitive count");
      List.iter (fun bad -> check [| 0; bad; 2; 3 |] [| 0; 4 |] [| 0 |]
                              (error "vertex references an invalid point")) [ -1; 4; 16777217 ];
      check quad [| 0; 3; 2; 4 |] [| 0; 0; 0 |]
        (error "primitive offsets are not ordered");
      List.iter (fun bad -> check quad [| 0; 4 |] [| bad |]
                              (Some ("E_TOPOLOGY_KIND", "Primitive kinds are 0 (polygon), 1 (open polyline) or 2 (closed polyline).")))
        [ -1; 3 ];
      check [| 0; 1 |] [| 0; 2 |] [| 0 |] (error "primitive has too few vertices for its kind");
      check [| 0 |] [| 0; 1 |] [| 1 |] (error "primitive has too few vertices for its kind");
      check [| 0; 1 |] [| 0; 2 |] [| 2 |] (error "primitive has too few vertices for its kind");
      check [| 0; 1 |] [| 0; 2 |] [| 1 |] None;
      check ~limit:3 quad [| 0; 4 |] [| 0 |] (error "polygon exceeds the supported vertex count");
      check ~points:16777218 [| 16777217; 1; 2 |] [| 0; 3 |] [| 0 |] None;
      check ~points:4294967295 [| 4294967294; 1; 2 |] [| 0; 3 |] [| 0 |] None;
      let n = 1025 in
      check ~points:n (Array.init (3 * n) (fun i -> i mod n))
        (Array.init (n + 1) (fun i -> i * 3)) (Array.make n 0) None;
      check quad [| 0; 4 |] [| 0 |] None;
      let small = buffer [| 0 |] in
      List.iter
        (fun (expression, valid) ->
          let packed = program expression in
          let runner = R.create gpu pipelines (ok (Emit.kernel ~fast:true ~uint32_outputs:[ 0 ] packed)) in
          Fun.protect ~finally:(fun () -> R.close runner) (fun () ->
              let output = ok (R.dispatch runner (ok (Flow_ir.Packed.Host.prepare ~fast:true packed
                                                       ~live:(Input.at_time 0.)))) in
              let result = Flow_gpu.Host.validate_topology host
                  ~vertices:(Option.get (R.buffer output), R.count output)
                  ~offsets:(buffer [| 0; 1025 |], 2) ~kinds:(small, 1)
                  ~point_count:1025 ~max_polygon_size:4294967295 in
              assert (R.Private.readback_bytes runner = 0);
              match result, valid with
              | Ok (), true -> ()
              | Error d, false ->
                  assert (d.code = "E_TOPOLOGY");
                  assert (d.message = "Topology: vertex references an invalid point")
              | Error d, true -> failwith (Diagnostic.to_string d)
              | Ok (), false -> failwith "GPU-origin invalid point accepted"))
        [ ("(int/add i (int/mod (frame/index) 1))", true);
          ("(int/add (if (int/eq i 1024) 1025 i) (int/mod (frame/index) 1))", false) ];
      let before = Flow_gpu.Host.stats host in
      (match Flow_gpu.Host.validate_topology host ~vertices:(small, 2) ~offsets:(small, 1)
               ~kinds:(small, 0) ~point_count:1 ~max_polygon_size:4 with
      | Error d -> assert (d.code = "E_TOPOLOGY")
      | Ok () -> failwith "topology count exceeds storage");
      assert ((Flow_gpu.Host.stats host).validation_readback_bytes = before.validation_readback_bytes))

let () =
  let baseline = Ogpu.Impl.live_handles () in
  let gpu =
    match Renderer.acquire_gpu () with
    | Ok gpu -> gpu
    | Error e -> failwith (Format.asprintf "%a" Renderer.pp_error e)
  in
  Fun.protect
    ~finally:(fun () -> Renderer.release_gpu gpu)
    (fun () ->
      resident_values gpu;
      let pipelines =
        Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday (Renderer.gpu_device gpu)
      in
      Fun.protect
        ~finally:(fun () -> Flow_gpu.Pipelines.close pipelines)
        (fun () ->
          topology gpu pipelines;
          holders gpu pipelines;
          let live = Input.at_time 0. in
          List.iter
            (fun expression ->
              let packed = program expression in
              let msl = ok (Emit.kernel ~fast:true ~uint32_outputs:[ 0 ] packed) in
              assert (msl.output_uint32 = [| true |]);
              let runner = R.create gpu pipelines msl in
              Fun.protect
                ~finally:(fun () -> R.close runner)
                (fun () ->
                  let inputs = ok (Flow_ir.Packed.Host.prepare ~fast:true packed ~live) in
                  let output = ok (R.dispatch runner inputs) in
                  let before = R.Private.readback_bytes runner in
                  (match R.keep output with
                  | Error d -> assert (d.code = "E_GPU_UINT32_KEEP")
                  | Ok _ -> failwith "uint32 buffer escaped as float32 storage");
                  assert (R.Private.readback_bytes runner = before);
                  let expected = ok (Flow_ir.Packed.force ~strict:true packed ~live) in
                  let source = Option.get (R.buffer output) in
                  let consume packed msl ~bound ~bound_table =
                    let consumer = R.create gpu pipelines msl in
                    Fun.protect
                      ~finally:(fun () -> R.close consumer)
                      (fun () ->
                        let values = ok (Flow_ir.Packed.Host.prepare ~fast:true packed ~live) in
                        (match R.dispatch consumer values with
                        | Error d -> assert (d.code = "E_GPU_UINT32_BINDING")
                        | Ok _ -> failwith "uint32 input silently uploaded float storage");
                        let result = ok (R.dispatch ~bound ~bound_table consumer values) in
                        assert (ok (R.readback result) = expected);
                        let host_result = ok (R.dispatch ~uint32_cpu:true consumer values) in
                        assert (
                          ok (R.readback host_result)
                          = ok (Flow_ir.Packed.force ~strict:true packed ~live));
                        if Array.exists Fun.id msl.Emit.input_uint32 then begin
                          let exact =
                            { values with arrays = [| [| 16777217.; 4294967295. |] |]; count = 2 }
                          in
                          let output = ok (R.dispatch ~uint32_cpu:true consumer exact) in
                          assert (ok (R.readback output) = E.Int_array [| 16777217; 4294967295 |]);
                          List.iter
                            (fun invalid ->
                              let bad = { values with arrays = [| [| invalid |] |]; count = 1 } in
                              match R.dispatch ~uint32_cpu:true consumer bad with
                              | Error d -> assert (d.code = "E_GPU_UINT32_RANGE")
                              | Ok _ -> failwith "invalid uint32 host input was uploaded")
                            [ -1.; 4294967296.; 1.5; Float.nan ];
                          let retry = ok (R.dispatch ~uint32_cpu:true consumer values) in
                          assert (
                            ok (R.readback retry)
                            = ok (Flow_ir.Packed.force ~strict:true packed ~live))
                        end)
                  in
                  let copy = program "(+ i (int/mod (int t) 1))" in
                  consume copy
                    (ok (Emit.kernel ~fast:true ~uint32_inputs:[ 0 ] ~uint32_outputs:[ 0 ] copy))
                    ~bound:(fun _ -> Some source)
                    ~bound_table:(fun _ -> None);
                  let gather =
                    Test_program.compile
                      {|(let* [table (array/range 1025 :type int)]
                       (map (fn [(i : int)] (+ (array/nth table i) (int/mod (int t) 1)))
                         (array/range 1025 :type int)))|}
                  in
                  let layout = ok (Emit.kernel ~fast:true gather) in
                  let u = fst layout.gathers.(0) in
                  consume gather
                    (ok (Emit.kernel ~fast:true ~uint32_tables:[ u ] ~uint32_outputs:[ 0 ] gather))
                    ~bound:(fun _ -> None)
                    ~bound_table:(fun index ->
                      if index = u then Some (source, inputs.count) else None);
                  assert (R.Private.readback_bytes runner = before);
                  assert (ok (R.readback output) = expected);
                  assert (R.Private.readback_bytes runner = before + (inputs.count * 4));
                  let planes = ok (R.planes output) in
                  (match expected with
                  | E.Int_array values -> assert (planes = [| Array.map float_of_int values |])
                  | _ -> failwith "expected integer reference output");
                  assert (R.Private.status_reads runner = 1);
                  let empty = ok (R.dispatch runner { inputs with count = 0 }) in
                  assert (ok (R.readback empty) = E.Int_array [||]);
                  assert (ok (R.planes empty) = [| [||] |])))
            [
              "(+ (+ i 16777217) (int t))";
              "(+ 4294967295 (int/mod (int t) 1))";
              "(int/mod (+ i (int t)) 2)";
            ];
          List.iter
            (fun packed ->
              let live = { (Input.at_time 16777217.) with frame = 16777217; tick = 16777217 } in
              let msl =
                ok (Emit.kernel ~fast:true ~uint32_scalars:true ~uint32_outputs:[ 0 ] packed)
              in
              assert (msl.uint32_uniforms <> []);
              let runner = R.create gpu pipelines msl in
              Fun.protect
                ~finally:(fun () -> R.close runner)
                (fun () ->
                  let values = ok (Flow_ir.Packed.Host.prepare ~fast:true packed ~live) in
                  let output = ok (R.dispatch runner values) in
                  assert (
                    ok (R.readback output) = ok (Flow_ir.Packed.force ~strict:true packed ~live))))
            [
              program "(int/add i (frame/index))";
              Test_program.compile
                {|(let* [n (int t)]
                (map (fn [(i : int)] (int/add i n)) (array/range 1025 :type int)))|};
            ];
          let integer = program "(+ (+ i 16777217) (int t))"
          and floats = program "(+ (float i) t)" in
          let merged = Option.get (Flow_ir.Core_fuse.merge [ integer; floats ]) in
          let msl = ok (Emit.kernel ~fast:true ~uint32_outputs:[ 0 ] merged) in
          assert (msl.output_uint32 = [| true; false |]);
          let runner = R.create gpu pipelines msl in
          Fun.protect
            ~finally:(fun () -> R.close runner)
            (fun () ->
              let inputs = ok (Flow_ir.Packed.Host.prepare ~fast:true merged ~live) in
              let output = ok (R.dispatch runner inputs) in
              let expected = ok (Flow_ir.Packed.force_all ~strict:true merged ~live) in
              List.iteri
                (fun index expected -> assert (ok (R.readback ~index output) = expected))
                expected);
          List.iter
            (fun expression ->
              let packed = program expression in
              let runner =
                R.create gpu pipelines (ok (Emit.kernel ~fast:true ~uint32_outputs:[ 0 ] packed))
              in
              Fun.protect
                ~finally:(fun () -> R.close runner)
                (fun () ->
                  let inputs = ok (Flow_ir.Packed.Host.prepare ~fast:true packed ~live) in
                  match R.dispatch runner inputs with
                  | Error d -> assert (d.code = "E_GPU_UINT32_RANGE")
                  | Ok _ -> failwith "out-of-range uint32 output was published"))
            [ "(+ -1 (int/mod (int t) 1))"; "(+ 4294967296 (int/mod (int t) 1))" ];
          List.iter
            (fun n -> List.iter (compaction gpu pipelines n) [ false; true ])
            [ 0; 1; 1023; 1024; 1025 ];
          compaction gpu pipelines ((1 lsl 24) + 3) false));
  assert (Ogpu.Impl.live_handles () = baseline);
  print_endline
    "Uint32 outputs/compaction: exact above 2^24, uint32 maximum, mixed/empty outputs, block \
     tails, counts and range status"
