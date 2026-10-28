let () =
  let contains source fragment =
    let n=String.length fragment in
    let rec at i=i+n<=String.length source &&
      (String.sub source i n=fragment || at(i+1))in at 0 in
  List.iter(fun(source,flag)->
    let program=Test_program.compile source in
    let msl=Test_program.ok(Flow_gpu.Emit.kernel ~fast:true program)
    and wgsl=Test_program.ok(Flow_web.Wgsl.kernel_fast program)in
    assert(contains msl.source ("?"^flag^"u:0u;"));
    assert(contains wgsl.source ("sb |= "^flag^"u;")))[
      ("(map (fn [(x : int)] (float (int/div x (int/sub (frame/index) (frame/index))))) (array/range 8 :type int))","2");
      ("(map (fn [(x : int)] (float (int/shr x (int/add 63 (int/mod (frame/index) 1))))) (array/range 8 :type int))","4")]

let () =
  List.iter(fun(_,program)->
    let layout=Flow_ir.Kernel_layout.plan ~fast:true program in
    let metal=Test_program.ok(Flow_gpu.Emit.kernel ~fast:true program)
    and wgsl=Test_program.ok(Flow_web.Wgsl.kernel_fast program)in
    assert(metal.uniform_layout=layout.uniform_layout && wgsl.uniform_layout=layout.uniform_layout);
    let loops=ref 0 in
    Flow_ir.Kernel_layout.iter layout ~instruction:(fun _ _->())
      ~loop:(fun _ _ ~next:_ ~stop:_ ~body->incr loops;body());
    assert(!loops>0)) (Test_program.carried_fixtures 65)

let () =
  List.iter (fun source ->
    let program = Test_program.compile source in
    let layout = Flow_ir.Kernel_layout.plan ~fast:true program in
    assert (layout.input_used = [|true|]);
    Array.iteri (fun slot -> function
      | Flow_ir.Packed.Int_frame _ -> assert (layout.frames.(slot) > 0)
      | _ -> ()) layout.view.code;
    let metal = Test_program.ok (Flow_gpu.Emit.kernel ~fast:true program)
    and wgsl = Test_program.ok (Flow_web.Wgsl.kernel_fast program) in
    assert (metal.uniform_layout = layout.uniform_layout);
    assert (wgsl.uniform_layout = layout.uniform_layout);
    assert (List.exists (fun (binding : Flow_web.Wgsl.binding) ->
      binding.binding = 0 && binding.kind = Storage_read) wgsl.interface))
    ["(map (fn [x] (+ (float x) t)) (array/range 16 :type \"int\"))";
     "(map (fn [x] (+ x (float (frame/index)))) (array/range 16))"]

let () =
  let program=Test_program.compile
    "(let* [table (array/float 7 0.25)] (map (fn [i] (+ t (array/nth table (int/mod (int i) 7)))) (array/range 32769)))" in
  (match Flow_ir.Packed.gpu_refusals program,Flow_gpu.Emit.kernel program with
   |d::_,Error emitted->assert(d.code="E_GPU_FORM" && emitted=d);
     assert(d.message="Indexed array reads stay on the CPU.")
   |_->assert false)

let () =
  List.iter (fun source ->
    let checked=match Flow.Workspace.check Flow.Check.{version=1;kinds=[]}
        (Flow.Syntax.parse source |> Test_program.ok)with
      |Some workspace,[]->workspace
      |_,ds->failwith(String.concat "\n"(List.map Diagnostic.to_string ds))in
    let audited=ref Flow.Workspace.Paths.empty in
    let qualified,_=Flow_ir.qualify_workspace ~observe:(fun producer residual->
      let view=Flow.Eval.Residual.residual_view residual in
      Option.iter(fun path->
        List.iter(fun fusion->
          let packed=Flow_ir.Packed.compile_result ~fusion residual view.term |> Result.get_ok in
          ignore(Flow_gpu.Emit.kernel packed |> Test_program.ok)) [true;false];
        audited:=Flow.Workspace.Paths.add path !audited)producer)checked |> Test_program.ok in
    assert(not(Flow.Workspace.Paths.is_empty !audited));
    assert(Flow.Workspace.Paths.subset !audited qualified.approx))
    ["(workspace pixels (graph img :context image (image/map (fn [uv] [uv.x uv.y t 1]))))";
     "(workspace pixels (defn render :context image [(bias : float)]
        (image/map (fn [uv] [(+ uv.x bias) uv.y (floor 0.5) 1])))
        (graph img :context image (render 0.125)))"]

let () =
  let source="(map (fn [x] (if (> x t) (+ x 1.0) (exp (* x 1000.0)))) (array/range 16))"in
  let program=Test_program.compile source in
  let view=Flow_ir.Packed.Private.view program in
  let msl=Test_program.ok(Flow_gpu.Emit.kernel program)in
  let lines=String.split_on_char '\n' msl.source in
  let guarded=ref false in
  Array.iteri(fun slot->function
    |Flow_ir.Packed.Guarded(guards,Flow_ir.Packed.Unary(Exp,_))->
        guarded:=true;
        assert(guards<>[]);
        let assignment=Printf.sprintf "    r%d="slot in
        let rec check=function
          |line::next::_ when String.starts_with ~prefix:assignment line->
              assert(next=Printf.sprintf "    sb|=!isfinite(r%d);"slot)
          |_::rest->check rest|[]->assert false in check lines
    |_->())view.code;
  assert !guarded;
  let nested=Test_program.compile
    "(map (fn [x] (if (> x t) (noise3 [(pow (+ x t) 2.0) t 0.0] :seed 31) (+ x t))) (array/range 16))"in
  let nested=Test_program.ok(Flow_gpu.Emit.kernel nested)in
  assert(nested.table_seeds=[|31|]);
  assert(List.exists(fun(_,_,width)->width=1)nested.uniform_layout);
  assert(Ogpu.Shader.validate_bindings nested.interface=Ok());
  List.iter(fun source->
    let program=try Test_program.compile source with Invalid_argument message->failwith(source^": "^message)in
    match Flow_gpu.Emit.kernel program with Error d->assert(d.code="E_GPU_FORM")|Ok _->assert false)
    ["(map (fn [x] (+ (float x) t)) (array/range 16 :type \"int\"))";
     "(let* [offset (frame/index)] (map (fn [x] (+ x (float offset))) (array/range 16)))"]

let () =
  let qualifies source =
    let program = Test_program.compile source in
    assert (Flow_ir.Packed.gpu_refusals program = []);
    ignore (Test_program.ok (Flow_gpu.Emit.kernel program)) in
  qualifies "(for [x (array/range 1024)] (+ x t))";
  List.iter qualifies
    ["(map (fn [x] (+ x t)) (array/range 1024))";
     "(map (fn [x] [x t]) (array/range 1024))";
     "(map (fn [x] [x t 2 3]) (array/range 1024))";
     "(map (fn [x] (noise3 [x t 0] :octaves 32)) (array/range 1024))";
     "(map (fn [x] (let* [unused 1e39] (+ x t))) (array/range 1024))"];
  let overflowing = Test_program.compile
      "(map (fn [x] (+ x (+ t 1e39))) (array/range 1024))" in
  (match Flow_ir.Packed.gpu_refusals overflowing, Flow_gpu.Emit.kernel overflowing with
   | reason :: _, Error emitted ->
       assert (reason.code = "E_GPU_FORM" && emitted = reason)
   | _ -> assert false);
  List.iter (fun (width, body) ->
    let program = Test_program.compile ~fusion:false body in
    let msl = Test_program.ok (Flow_gpu.Emit.kernel program) in
    assert (msl.output_widths = [|width|]);
    let prepared = Test_program.ok (Flow_ir.Packed.Private.prepare program ~live:(Input.at_time 1.)) in
    assert (prepared.count = 1024);
    assert (Marshal.to_string (Flow_ir.Packed.force program ~live:(Input.at_time 1.)) [Marshal.No_sharing]
      = Marshal.to_string (Flow_ir.Packed.reference program ~live:(Input.at_time 1.)) [Marshal.No_sharing]))
    [2, "(map (fn [x] [x t]) (array/range 1024))";
     4, "(map (fn [x] [x t (+ x t) 1]) (array/range 1024))";
     4, "(map (fn [(uv : vec2)] [uv.x uv.y t 1]) (map (fn [x] [x 1]) (array/range 1024)))";
     2, "(let* [offset [1 2 3 4]] (map (fn [(p : vec4)] [(+ p.x offset.w) (+ p.w t)])
       (map (fn [x] [x 1 2 3]) (array/range 1024))))"];
  List.iter (fun name ->
    assert (Flow.Op.find name Flow.Context.value<>None)) Flow_gpu.Emit.names;
  assert (Flow_gpu.Emit.names=Flow.Packed_ops.names);
  List.iter (fun body ->
    let program = Test_program.compile ("(map (fn [x] " ^ body ^ ") (array/range 1024))") in
    ignore (Test_program.ok (Flow_gpu.Emit.kernel program));
    List.iter (fun domains -> Parallel.run ~domains (fun () ->
      List.iter (fun time ->
        let live = Input.at_time time in
        assert (Marshal.to_string (Flow_ir.Packed.force program ~live) [Marshal.No_sharing]
          = Marshal.to_string (Flow_ir.Packed.reference program ~live) [Marshal.No_sharing]))
        [0.; 0.125; 1.25; 7.])) [1;8])
    ["(cond (< x t) (+ x 1) (> x 5) (- x 1) :else (* x t))";
     "(case x 0 t 1 (+ t 1) :else (+ x t))";
     "(case x false t true (+ x t) :else 0)";
     "(case (> x t) 0 t 2 (+ x t) :else 0)"];
  List.iter (fun name ->
    let body=match name with
      | name when Flow.Packed_ops.integer name <> None ->
          "(if (> t 0.0) (" ^ name ^ " x 1) x)"
      | name when Flow.Packed_ops.comparison name <> None ->
          "(if (" ^ name ^ " x 1) (+ t 1.0) t)"
      | "noise3" -> "(noise3 [x t 0])"
      | "fma" -> "(fma x t 0.5)"
      | "rand/at" -> "(rand/at (int t) x)"
      | "length" -> "(length [x t 0])"
      | "dot" -> "(dot [x t 0] [1 2 3])"
      | "cross" -> "(cross [x t 0] [1 2 3])"
      | "normalize" -> "(normalize [x t 0])"
      | "asin" | "acos" -> "(" ^ name ^ " (* (+ x t) 0.0001))"
      | "log" -> "(log (+ (+ x t) 1.0))"
      | "and" | "or" -> "(if (" ^ name ^ " (> x t) (< x 8)) x t)"
      | "not" -> "(if (not (> x t)) x t)"
      | "<" | "<=" | ">" | ">=" | "=" -> "(if (" ^ name ^ " x t) x t)"
      | name when Flow.Packed_ops.unary name<>None -> "(" ^ name ^ " (+ x t))"
      | name -> "(" ^ name ^ " x t)" in
    let integer = Flow.Packed_ops.integer name <> None || Flow.Packed_ops.comparison name <> None || name = "rand/at" in
    let array = if integer then "(array/range 1024 :type \"int\")" else "(array/range 1024)" in
    let program = Test_program.compile ("(map (fn [x] " ^ body ^ ") " ^ array ^ ")") in
    if integer || name = "hypot" then
      (match Flow_gpu.Emit.kernel program with
       | Error d -> assert (d.code = "E_GPU_FORM")
       | Ok _ -> failwith ("Metal must refuse " ^ name))
    else ignore (Test_program.ok (Flow_gpu.Emit.kernel program)))
    Flow_gpu.Emit.names;
  let writing=Array.to_list Sys.argv=[Sys.argv.(0);"--write-goldens"] in
  List.iter (fun (name,program) ->
    let msl=Test_program.ok (Flow_gpu.Emit.kernel program) in
    let path="goldens/" ^ name ^ ".metal" in
    if writing then Out_channel.with_open_bin path (fun channel -> output_string channel msl.source)
    else assert (In_channel.with_open_bin path In_channel.input_all=msl.source);
    assert (List.length msl.interface=Array.length msl.input_widths+Array.length msl.output_widths+2+
      (if msl.table_seeds=[||] then 0 else 1));
    assert (Ogpu.Shader.validate_bindings msl.interface=Ok ());
    let inputs=Test_program.ok (Flow_ir.Packed.Private.prepare program ~live:(Input.at_time 1.)) in
    assert (inputs.count=1024);
    assert (Array.length inputs.frame=Array.length (Flow_ir.Packed.Private.view program).code))
    (Test_program.fixtures 1024 @ ["two_outputs",Test_program.two_outputs ()]);
  List.iter (fun source ->
    let program = Test_program.compile source in
    match Flow_ir.Packed.gpu_refusals program, Flow_gpu.Emit.kernel program with
    | reason :: _, Error emitted ->
        assert (reason.code = emitted.code && reason.message = emitted.message)
    | _ -> assert false)
    ["(sum [x (array/range 1024)] (+ x t))";
     "(for [x (array/range 32) y (array/range 32)] (+ (+ x y) t))";
     "(for [x (array/range 1024)] :skip [0] (+ x t))"];
  (* 32-bit fused and checked arithmetic is emitted for placed producers. *)
  List.iter (fun (source, fragment) ->
    let msl = Test_program.ok (Flow_gpu.Emit.kernel (Test_program.compile source)) in
    let contains sub s = let n = String.length sub in
      let rec at i = i + n <= String.length s && (String.sub s i n = sub || at (i + 1)) in at 0 in
    assert (contains fragment msl.source))
    ["(map (fn [x] (get (float/mul_checked x t) :value)) (array/range 1024))", "isfinite((";
     "(map (fn [x] (if (get (float/fma_checked x t 1.0) :valid) x 0.0)) (array/range 1024))", "?1.0f:0.0f)";
     "(map (fn [x] (get (float/fma_checked x t 1.0) :value)) (array/range 1024))", "fma("];
  let uniform=Test_program.ok(Flow_gpu.Emit.kernel(Test_program.compile "(map (fn [x] t) (array/range 1024))"))in
  assert(List.length uniform.interface=3 && not(List.exists(fun(binding:Ogpu.Shader.binding)->binding.binding=0)uniform.interface));
  let program=Test_program.compile
    "(map (fn [x] (if (> x t) 0.0 (min (* x x) 1.0))) (array/range 1024))" in
  let hidden=Test_program.ok(Flow_gpu.Emit.kernel program)in
  let checks=Flow_ir.Packed.Private.finite_checks program in
  let lines=String.split_on_char '\n' hidden.source in
  List.iteri(fun index line->if String.starts_with ~prefix:"  float r" line then
    let slot=Scanf.sscanf line "  float r%d=" Fun.id in
    let guard=Printf.sprintf "  sb|=!isfinite(r%d);" slot in
    if not (match (Flow_ir.Packed.Private.view program).code.(slot)with Guarded _->true|_->false)then
      assert((List.nth lines(index+1)=guard)=checks.(slot)))lines;
  print_endline "GPU emitter: shared operations, golden arithmetic/select/seeded fBm and typed ordered-loop refusals passed"

let () =
  let program = Test_program.compile
    "(let* [table (map (fn [i] [i (* i 2.) (- 0. i)]) (array/range 64))] (map (fn [x] (let* [p (array/nth table (int/mod (int x) 64))] (+ (+ p.x (* p.y 0.5)) p.z))) (array/range (int/add 4097 (frame/index)))))" in
  let layout = Flow_ir.Kernel_layout.plan ~fast:true program in
  let msl = Test_program.ok (Flow_gpu.Emit.kernel ~fast:true program) in
  let wgsl = Test_program.ok (Flow_web.Wgsl.kernel_fast program) in
  assert (msl.uniform_layout = layout.uniform_layout && wgsl.uniform_layout = layout.uniform_layout);
  assert (msl.table_seeds = layout.table_seeds && wgsl.table_seeds = layout.table_seeds);
  assert (msl.gathers = Array.of_list (List.map snd layout.gathers));
  assert (wgsl.gathers = Array.of_list (List.map snd layout.gathers));
  assert (msl.counts = layout.counts && wgsl.counts = layout.counts);
  assert (msl.uniform_bytes = layout.uniform_bytes);
  assert (wgsl.uniform_bytes = Flow_ir.Kernel_layout.uniform_words layout * 16);
  let msl_bindings = List.map (fun (binding : Ogpu.Shader.binding) -> binding.binding) msl.interface in
  let wgsl_bindings = List.map (fun (binding : Flow_web.Wgsl.binding) -> binding.binding) wgsl.interface in
  assert (msl_bindings = wgsl_bindings);
  let program = Test_program.compile "(map (fn [x] (+ x t)) (array/range 16))" in
  let layout = Flow_ir.Kernel_layout.plan program in
  let msl = Test_program.ok (Flow_gpu.Emit.kernel program) in
  let wgsl = Test_program.ok (Flow_web.Wgsl.kernel program) in
  assert (msl.uniform_layout = layout.uniform_layout && wgsl.uniform_layout = layout.uniform_layout);
  assert (msl.table_seeds = layout.table_seeds && wgsl.table_seeds = layout.table_seeds);
  assert (msl.uniform_bytes = layout.uniform_bytes);
  assert (wgsl.uniform_bytes = Flow_ir.Kernel_layout.uniform_words layout * 16)
