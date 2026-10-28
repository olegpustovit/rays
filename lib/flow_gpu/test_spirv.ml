let cases () =
  [
    ( "arithmetic",
      Test_program.compile
        "(map (fn [x] [(/ x (- x 3.)) (pow (abs (- x 4.)) 0.5) (mod x 7.) (+ t (abs x))]) \
         (array/range 16))" );
    ( "guarded",
      Test_program.compile "(map (fn [x] (if (> x t) (+ x 1.) (exp (* x 1000.)))) (array/range 16))"
    );
    ( "intrinsics",
      Test_program.compile
        "(map (fn [x] [(sin (+ x t)) (cos x) (fma x t 0.25) (log (+ (abs x) 1.))]) (array/range \
         16))" );
    ("two_outputs", Test_program.two_outputs ());
    ( "noise",
      Test_program.compile
        "(map (fn [x] (noise3 [(- x 0.5) t 0.25] :seed 31 :octaves 3)) (array/range 16))" );
    ( "noise_seeds",
      Test_program.compile
        "(map (fn [x] (if (> x t) (noise3 [x t -1.25] :seed 73 :octaves 32) (noise3 [(- 0. x) t \
         0.5] :seed 31))) (array/range 16))" );
    ( "checked",
      Test_program.compile
        "(map (fn [x] [(get (float/mul_checked x t) :value) (if (get (float/fma_checked x t 1.) \
         :valid) x 0.)]) (array/range 16))" );
    ( "operators",
      Test_program.compile
        "(map (fn [x] (+ (atan2 (sign (- x t)) (sqrt (+ x t))) (if (and (< x t) (not (= x 2.))) (+ \
         (tan x) (asin (* x 0.001))) (+ (acos (* x 0.001)) (max (min x t) (- x t)))))) \
         (array/range 16))" );
  ]
  @ Test_program.vector_fixtures 16

let fast_cases () =
  List.map
    (fun (name, source) -> (name, Test_program.compile source))
    [
      ("fast_float", "(map (fn [x] (+ (sin x) t)) (array/range 16))");
      ("fast_input", "(map (fn [x] (+ (float x) t)) (array/range 16 :type \"int\"))");
      ("fast_int63", "(map (fn [x] " ^ Test_program.int63_checks ^ ") (array/range 16))");
      ( "fast_arithmetic",
        "(let* [n (frame/index)] (map (fn [x] [(float (int/div (int/sub x n) (int/sub n 3))) \
         (float (int/mod (int/sub x n) (int/sub n 3))) (float (int/mul (int/add x 4294967297) \
         4294967295)) (if (int/le x n) t (float (int/xor x 63)))]) (array/range 16 :type \
         \"int\")))" );
      ( "fast_bits",
        "(map (fn [x] [(float (int/and (int/shr (int/shl (int/add x 1) 40) 13) 4294967295)) (float \
         (int/or (int/xor x 255) 65536)) (float (abs (int/sub (frame/index) x))) (if (int/ge x \
         (frame/index)) t (+ t 1.))]) (array/range 16 :type \"int\"))" );
      ( "fast_convert",
        "(map (fn [x] [(float (floor (- (+ x t) 0.5))) (float (ceil (- t x))) (float (round (- x \
         t))) (float (int (+ x t)))]) (array/range 16))" );
      ( "fast_trace",
        "(map (fn [i] (let* [h (scene/trace 1 [(float i) 0. 5.] [0. 0. -1.] 0. 100. t)] [h.t \
         (float h.prim) (+ h.u h.normal.z) (if h.hit 1. 0.)])) (array/range 16 :type \"int\"))" );
      ( "fast_trace_instanced",
        "(map (fn [i] (let* [h (scene/trace 1 [(float i) 0. 5.] [0. 0. -1.] 0. 100. t)] [h.t \
         (float h.instance) (+ h.u h.normal.x) (if h.hit 1. 0.)])) (array/range 16 :type \"int\"))"
      );
      ( "fast_trace_curves",
        "(map (fn [i] (let* [h (scene/trace 1 [(float i) 0. 5.] [0. 0. -1.] 0. 100. t)] [h.t \
         (float h.prim) (+ h.u h.normal.y) (if h.hit 1. 0.)])) (array/range 16 :type \"int\"))" );
      ( "fast_gather",
        "(let* [table (array/float 7 0.25)] (map (fn [i] (+ t (array/nth table (int/mod i 7)))) \
         (array/range 16 :type \"int\")))" );
      ( "fast_vec_gather",
        "(let* [table (map (fn [x] [x (+ x 1.) (- x 1.)]) (array/range 7))] (map (fn [i] (+ \
         (array/nth table (int/mod i 7)) [t 0. 0.])) (array/range 16 :type \"int\")))" );
      ( "fast_int_gather",
        "(let* [table (array/int 7 12345)] (map (fn [i] (+ t (float (array/nth table (int/mod i \
         7))))) (array/range 16 :type \"int\")))" );
      ( "fast_whole",
        "(map (fn [x] (let* [n (int/add x 4294967297) floating (float n)] [(float (int (- floating \
         4294967296.))) (+ floating 1.) (float (int (mod floating 7.))) t])) (array/range 16 :type \
         \"int\"))" );
      ( "fast_guarded_whole",
        "(map (fn [x] (let* [n (float (int/add x 4294967297)) v (if (int/lt x 7) (+ n 1.) (- n \
         1.))] [(float (int v)) v t])) (array/range 16 :type \"int\"))" );
      ( "fast_invalid_shift",
        "(map (fn [(x : int)] (float (int/shr x (int/add 63 (int/mod (frame/index) 1))))) \
         (array/range 16 :type int))" );
      ( "fast_negative_shift",
        "(map (fn [(x : int)] (float (int/shr x (int/sub (frame/index) (int/add (frame/index) \
         1))))) (array/range 16 :type int))" );
      ( "fast_divide_by_zero",
        "(map (fn [(x : int)] (float (int/div x (int/sub (frame/index) (frame/index))))) \
         (array/range 16 :type int))" );
      ( "fast_modulo_by_zero",
        "(map (fn [(x : int)] (float (int/mod x (int/sub (frame/index) (frame/index))))) \
         (array/range 16 :type int))" );
    ]
  @ Test_program.carried_fixtures 16

let () =
  let writing = Array.to_list Sys.argv = [ Sys.argv.(0); "--write-goldens" ] in
  let validator = Sys.command "command -v spirv-val >/dev/null 2>&1" = 0 in
  let golden = Buffer.create 512 in
  List.iter
    (fun (name, fast, program) ->
      let instanced = String.ends_with ~suffix:"instanced" name
      and curves = String.ends_with ~suffix:"curves" name in
      let emitted = Flow_gpu.Spirv.kernel ~fast ~instanced ~curves program |> Test_program.ok in
      let again = Flow_gpu.Spirv.kernel ~fast ~instanced ~curves program |> Test_program.ok in
      assert (emitted.bytes = again.bytes);
      assert (Bytes.get_int32_le emitted.bytes 0 = 0x07230203l);
      assert (
        Bytes.get_int32_le emitted.bytes 4
        = if emitted.layout.scene_binding = None then 0x00010300l else 0x00010400l);
      assert (Bytes.length emitted.bytes mod 4 = 0);
      let rec has_int64 at =
        if at = Bytes.length emitted.bytes then false
        else
          let instruction = Bytes.get_int32_le emitted.bytes at |> Int32.to_int in
          (instruction land 0xffff = 17 && Bytes.get_int32_le emitted.bytes (at + 4) = 11l)
          || has_int64 (at + ((instruction lsr 16) * 4))
      in
      if fast then assert (has_int64 20 = (name <> "fast_float"));
      assert (Ogpu.Shader.validate_bindings emitted.interface = Ok ());
      assert (emitted.uniform_bytes = Flow_ir.Kernel_layout.uniform_words emitted.layout * 16);
      let metal =
        Flow_gpu.Emit.kernel ~fast ~instanced ?curves:(if curves then Some `Loop else None) program
        |> Test_program.ok
      in
      assert (emitted.interface = metal.interface);
      assert (emitted.layout.uniform_layout = metal.uniform_layout);
      assert (emitted.layout.table_seeds = metal.table_seeds);
      assert (Array.of_list (List.map snd emitted.layout.gathers) = metal.gathers);
      assert (emitted.layout.counts = metal.counts);
      if validator then begin
        let file = Filename.temp_file "rays-spirv-" ".spv" in
        Fun.protect
          ~finally:(fun () -> Sys.remove file)
          (fun () ->
            Out_channel.with_open_bin file (fun out -> output_bytes out emitted.bytes);
            let code =
              Sys.command
                ((if emitted.layout.scene_binding = None then "spirv-val --target-env vulkan1.1 "
                  else "spirv-val --target-env vulkan1.2 ")
                ^ Filename.quote file)
            in
            if code <> 0 then failwith ("spirv-val refused " ^ name))
      end;
      Printf.bprintf golden "(%s %d %s)\n" name (Bytes.length emitted.bytes)
        (Digest.to_hex (Digest.bytes emitted.bytes)))
    (List.map (fun (name, program) -> (name, false, program)) (cases ())
    @ List.map (fun (name, program) -> (name, true, program)) (fast_cases ()));
  (* a moving scene has no SPIR-V ray-query form: a typed refusal, never a static kernel *)
  (match
     Flow_gpu.Spirv.kernel ~fast:true ~motion:true (List.assoc "fast_trace" (fast_cases ()))
   with
  | Error d -> assert (d.code = "E_GPU_FORM")
  | Ok _ -> assert false);
  let expected = "goldens/spirv.sexp" in
  if writing then
    Out_channel.with_open_bin expected (fun out -> output_string out (Buffer.contents golden))
  else assert (In_channel.with_open_bin expected In_channel.input_all = Buffer.contents golden);
  print_endline
    (if validator then "SPIR-V: deterministic binaries, shared bindings and Vulkan validation pass"
     else "SPIR-V: deterministic binaries and shared bindings pass; spirv-val unavailable")
