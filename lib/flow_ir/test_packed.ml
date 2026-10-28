module E = Flow.Eval
let () = Region_snapshot.start ()
module I = Flow_ir
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let checked ?(ops = []) body =
  let text = "(workspace w (graph g :context value (let* [tested " ^ body ^ "] 0.0)))" in
  let forms = ok (Flow.Syntax.parse text) in
  match Flow.Workspace.check ~ops {Flow.Catalog.version = 1; kinds = []} forms with
    | Some w, [] -> w
    | _, ds -> failwith (text ^ "\n" ^ String.concat "; " (List.map Diagnostic.to_string ds))
let recorded ?ops body =
  let evaluation = ok (E.static ~record:true (checked ?ops body)) in
  List.assoc ["g"; "tested"] evaluation.records |> List.hd |> snd
let same a b = match a, b with
  | Ok a, Ok b -> Marshal.to_string a [Marshal.No_sharing] = Marshal.to_string b [Marshal.No_sharing]
  | Error a, Error b -> Diagnostic.to_string a = Diagnostic.to_string b
  | _ -> false
let () =
  let trace = !I.Packed.Private.trace in
  Fun.protect ~finally:(fun () -> I.Packed.Private.trace := trace) (fun () ->
    I.Packed.Private.trace := ignore;
    let packed = Test_program.compile "(map (fn [index]
      (let* [x (+ (float index) t)]
        (if (< (mod x 2053.0) 1025.0)
          (let* [a (float/mul_checked x 0.5)]
            (if (< (mod x 7.0) 3.0)
              [a.value (float a.valid) (+ x 1.0) (- x 1.0)]
              [a.value (float a.valid) (+ x 2.0) (- x 2.0)]))
          (if (int/lt index 0)
            (error \"E_UNTAKEN\" \"Inactive guarded instruction ran.\")
            [-0.0 0.0 x (- 0.0 x)])))) (array/range 32769 :type int))" in
    let region = packed in
    let nested = ok (Result.map_error List.hd (I.Core.Region.rebuild region ~code:
      (Array.map (function
        | I.Core.Region.Guarded (first :: (_ :: _ as rest), body) ->
            I.Core.Region.Guarded ([first], Guarded (rest, body))
        | instruction -> instruction) region.code) ())) in
    List.iter (fun time ->
      let live = Input.at_time time in
      let expected = I.Packed.reference packed ~live in
      List.iter (fun domains -> Parallel.run ~domains (fun () ->
        List.iter (fun program -> assert (same
          (I.Packed.force ~strict:true program ~live) expected)) [packed; nested]))
        [1;4;8]) [0.;0.25;1024.5];
    print_endline "Guard reuse: changing blocks, nested/truncated spans, checked projections and untaken failure match reference at one/four/eight domains")
let has_cpu p = Option.fold ~none:false ~some:(fun packed ->
  I.Cost.packed ~count:(Option.value ~default:0 (I.Packed.static_count packed)) = I.Cpu_kernel)
  (I.Executor.packed_root p)
let stages p = Option.fold ~none:0 ~some:I.Packed.stage_count (I.Executor.packed_root p)
let compile ?fusion residual term =
  let legacy = I.Packed.compile ?fusion residual term in
  (match legacy, I.Packed.compile_result ?fusion residual term with
   | None, Error [_] -> ()
   | Some a, Ok b ->
       assert (Marshal.to_string (I.Packed.Private.view a) [Marshal.No_sharing]
         = Marshal.to_string (I.Packed.Private.view b) [Marshal.No_sharing]);
       assert (I.Packed.stage_count a = I.Packed.stage_count b
         && I.Packed.provenance a = I.Packed.provenance b
         && I.Packed.static_count a = I.Packed.static_count b)
   | _ -> assert false);
  legacy
let () =
  if Array.exists (fun arg -> arg="--bench-product" || arg="--bench-product-row" || arg="--bench-float-product" || arg="--bench-int-scale") Sys.argv then begin
    I.Packed.Private.trace := ignore;
    let count = 1_000_000 and width = 16 in
    let integer = Array.exists ((=) "--bench-int-scale") Sys.argv in
    let repeated_row = Array.exists ((=) "--bench-product-row") Sys.argv in
    let float_product = Array.exists ((=) "--bench-float-product") Sys.argv in
    let packed = Test_program.compile ~fusion:false (if integer then Printf.sprintf
      "(map (fn [index] (int/mul index %d)) (array/range (int/add %d (int t)) :type int))" width count else Printf.sprintf
      (if repeated_row then
         "(for [index (array/range %d :type int) component (map (fn [x] (+ x t)) (array/range %d))] component)"
       else if float_product then
         "(for [index (array/range %d) component (array/range %d)] (+ component t))"
       else "(for [index (array/range %d :type int) component (array/range %d)] (+ component t))")
      count width) in
    if float_product then begin
      let region=packed in
      assert(region.iteration=I.Core.Region.Product);
      assert(Array.for_all(fun(_,term)->term.Flow.Workspace.ty=Flow.Ty.Array Flow.Ty.Float)region.sources);
      assert(Array.for_all(function
        |I.Core.Region.Input _|Const _|Frame _|Binary _->true|_->false)region.code)
    end;
    let live = Input.at_time (-0.25) in
    let row = Array.init width (fun i -> float i -. 0.25) in
    let native () =
      if integer then E.Int_array(Array.init count(fun i->i*width)) else
      let data = Array.make (count * width) 0. in
      for i = 0 to count - 1 do Array.blit row 0 data (i * width) width done;
      E.Float_array data in
    List.iter (fun domains -> Parallel.run ~domains (fun () ->
      let actual = ok (I.Packed.force ~strict:true packed ~live) in
      assert (same (Ok actual) (Ok (native ())));
      let measure f =
        let trials = Array.init 3 (fun _ ->
          Gc.full_major ();
          let allocated = Gc.allocated_bytes () and started = Unix.gettimeofday () in
          ignore (f ());
          Unix.gettimeofday () -. started, Gc.allocated_bytes () -. allocated) in
        Array.sort (fun (a, _) (b, _) -> Float.compare a b) trials;
        trials.(1) in
      let native_seconds, native_bytes = measure native in
      let seconds, allocated = measure (fun () -> ok (I.Packed.force ~strict:true packed ~live)) in
      Printf.printf "%s count=%d width=%d registers=%d domains=%d native_seconds=%.6f packed_seconds=%.6f ratio=%.3f native_allocated_bytes=%.0f packed_allocated_bytes=%.0f\n%!"
        (if integer then "integer_scale" else if float_product then "product_float_bank" else "product_float") count width (Array.length (I.Packed.Private.view packed).code) domains
        native_seconds seconds (seconds /. native_seconds) native_bytes allocated)) [1;4];
    exit 0
  end
let () =
  if Array.exists ((=) "--bench-guarded") Sys.argv then begin
    I.Packed.Private.trace := ignore;
    let count = 1_000_000 in
    let packed = Test_program.compile (Printf.sprintf
      "(map (fn [x] (if (< (mod x 2.0) 1.0)\n\
         (if (< (mod x 4.0) 1.0) (+ (sin x) t) (- (cos x) t))\n\
         (+ (* x 0.5) t))) (array/range %d))" count) in
    List.iter (fun domains -> Parallel.run ~domains (fun () ->
      let live = Input.at_time 0.25 in
      ignore (ok (I.Packed.force ~strict:true packed ~live));
      let trials = Array.init 3 (fun _ ->
        Gc.full_major ();
        let allocated = Gc.allocated_bytes () and start = Unix.gettimeofday () in
        ignore (ok (I.Packed.force ~strict:true packed ~live));
        Unix.gettimeofday () -. start, Gc.allocated_bytes () -. allocated) in
      Array.sort (fun (a, _) (b, _) -> Float.compare a b) trials;
      let seconds, allocated = trials.(1) in
      Printf.printf "guarded_float count=%d registers=%d domains=%d median_seconds=%.6f allocated_bytes=%.0f\n%!"
        count (Array.length (I.Packed.Private.view packed).code) domains seconds allocated)) [1;4];
    exit 0
  end

let () =
  let trace= !I.Packed.Private.trace in
  I.Packed.Private.trace:=ignore;
  let count=32769 in
  let source=Printf.sprintf "(array/range %d :type int)" count in
  let cases=[
    "(map (fn [x] (int/add (int/xor x (frame/index)) 9007199254740993)) "^source^")";
    "(map (fn [x] (if (int/lt x 0) (int/div x 0) (int/add x (frame/index)))) "^source^")";
    "(map (fn [x] [(- (float x) t) (float (int/shr x 10))]) "^source^")";
    "(for [x "^source^" y (array/float 2 t)] y)";
    "(for [x "^source^" y (array/vec3 2 [t -0.0 2])] y)";
    "(for [x "^source^" y (array/vec4 2 [t -0.0 2 3])] y)";
    "(for [x "^source^" y (array/range 3 :type int)] :skip [0 1024 16384] (int/add (int/mul x 17) (int/add y (frame/index))))";
    "(array/sum (map (fn [x] (int/add x (frame/index))) "^source^"))";
    "(scan [a (frame/index)] [x "^source^"] (int/xor (int/mul a 2177342782468422677) x))";
    "(fold [a (frame/index)] [x "^source^"] (int/xor (int/mul a 2177342782468422677) x))";
    "(map (fn [x] (if (int/eq x 32768) (int/shr x 63) (int/add x (frame/index)))) "^source^")";
    "(map (fn [x] (if (int/eq x 32768) (int/div x 0) (int/add x (frame/index)))) "^source^")"] in
  List.iter(fun body->
    let value=recorded body in
    let residual=match value with E.Residual residual->residual|_->assert false in
    let packed=Option.get(I.Packed.compile residual (E.Residual.residual_view residual).term) in
    List.iter(fun frame->
      let live={(Input.at_time 0.25)with frame} in
      let expected=E.Private.force_reference value ~live in
      List.iter(fun domains->Parallel.run ~domains(fun()->
        if not(same expected (I.Packed.force ~strict:true packed ~live))then
          failwith("Typed block parity: "^body))) [1;4;8]) [0;42]) cases;
  I.Packed.Private.trace:=trace;
  print_endline "Typed blocks: exact integers, guards, mixed scalar/vector outputs, skipped products, wrapping ordered folds/scans and selected errors agree at one/four/eight domains"

let () =
  let trace = !I.Packed.Private.trace in
  I.Packed.Private.trace := ignore;
  let base=Test_program.compile ~fusion:false
    "(for [index (array/range 32769 :type int) component (map (fn [x] (+ x t)) (array/range 7))] component)" in
  let region=base in
  assert(region.code=[|I.Core.Region.Int_input 0;Input(1,1,0)|]);
  let make outer row =
    let sources=Array.mapi(fun i (residual,term)->
      let name=if i=0 then "repeat_outer" else "repeat_row" in
      let term={term with Flow.Workspace.node=Ref_binding(name,[])} in
      E.Residual.with_bindings residual ~term [name,(if i=0 then outer else E.Float_array row)],term)region.sources in
    let region=Result.get_ok(I.Core.Region.rebuild region ~sources ()) in
    let ordinary=Result.get_ok(I.Core.Region.rebuild region
      ~code:(Array.append region.code [|I.Core.Region.Unary(Abs,1)|]) ()) in
    let program region=match I.Core.Region.rebuild region () with Ok p->p
      |Error ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds)) in
    program region,program ordinary in
  let live=Input.at_time 0.25 in
  List.iter(fun row->List.iter(fun count->
    let outer=E.Int_array(Array.init count(fun i->max_int-i)) in
    let packed,ordinary=make outer row in
    let expected=Array.make (count*Array.length row) 0. in
    for i=0 to count-1 do Array.blit row 0 expected (i*Array.length row) (Array.length row) done;
    List.iter(fun domains->Parallel.run ~domains(fun()->
      assert(same(I.Packed.force ~strict:true packed ~live)(Ok(E.Float_array expected)));
      assert(same(I.Packed.force ~strict:true packed ~live)(I.Packed.force ~strict:true ordinary ~live));
      let data=Array.make (Array.length expected) 0. in
      ignore(ok(I.Packed.force_into ~strict:true ~column_source:(fun _ _->Ok None)
        ~count:(Array.length expected) ~outputs:[|[|Some data|]|] packed ~live));
      assert(same(Ok(E.Float_array data))(Ok(E.Float_array expected))))) [1;4;8]) [0;1;32769])
    [[||];[|-0.;0.;Float.next_after 1. infinity;Float.ldexp 1. (-1074)|]];
  List.iter(fun bad->List.iter(fun count->
    let packed,ordinary=make (E.Int_array(Array.make count 0)) [|0.;bad|] in
    let actual=I.Packed.force ~strict:true packed ~live in
    assert(same actual(I.Packed.force ~strict:true ordinary ~live));
    let discarded=I.Packed.force_into ~strict:true ~column_source:(fun _ _->Ok None)
      ~count:(2*count) ~outputs:[|[|None|]|] packed ~live in
    if count=0 then assert(Result.is_ok actual && Result.is_ok discarded)
    else match actual,discarded with
      |Error a,Error b->assert(a.code="E_NONFINITE" && a.code=b.code && a.message=b.message)
      |_->assert false) [0;1;32769]) [nan;infinity;neg_infinity];
  let packed,ordinary=make (E.Float_array [|infinity|]) [|0.|] in
  assert(same(I.Packed.force ~strict:true packed ~live)(I.Packed.force ~strict:true ordinary ~live));
  (match I.Packed.force ~strict:true packed ~live with Error d->assert(d.code="E_ARRAY_TYPE")|_->assert false);
  I.Packed.Private.trace := trace;
  print_endline "Product row projection: repeated bits, empty sources, selected errors, sinks and one/four/eight domains passed"

let () =
  let trace = !I.Packed.Private.trace in
  I.Packed.Private.trace := ignore;
  List.iter(fun body->let packed=Test_program.compile ~fusion:false body in
    List.iter(fun time->let live=Input.at_time time in
      List.iter(fun domains->Parallel.run ~domains(fun()->
        assert(same(I.Packed.force ~strict:true packed ~live)(I.Packed.reference packed ~live))))[1;4;8])
      [0.;1.;2.])
    ["(map (fn [x] (int/add x (int t))) (array/range (int/add 32769 (int t)) :type int))";
     "(let* [n (int/add 32769 (int t)) xs (array/range n :type int)] (map (fn [x] (int/sub x (int t))) xs))";
     "(for [x (array/range (int/add 1025 (int t)) :type int) y (array/range 3 :type int)] (int/add (int/mul x 3) y))";
     "(map (fn [x] (if (< x 2) (int/sub 0 x) (int/add x 7))) (array/range (int t) :type int))";
     "(map (fn [x] (int/add x 1)) (array/range (int/add -1 (int t)) :type int))";
     Printf.sprintf "(map (fn [x] (int/add x 1)) (array/range (int/add %d (int t)) :type int))" (Sys.max_array_length+1)];
  I.Packed.Private.trace:=trace;
  print_endline "Integer range views: captured counts, Cartesian/guarded indices, empty/changing frames and range diagnostics match reference at one/four/eight domains"

let () =
  let trace = !I.Packed.Private.trace in
  I.Packed.Private.trace := ignore;
  List.iter(fun count->List.iter(fun factor->List.iter(fun reverse->
    let operation=Printf.sprintf (if reverse then "(int/mul %d x)" else "(int/mul x %d)") factor in
    let packed=Test_program.compile(Printf.sprintf
      "(map (fn [x] %s) (array/range (int/add %d (int t)) :type int))" operation count) in
    let live=Input.at_time 0.25 in
    let expected=E.Int_array(Array.init count(fun i->i*factor)) in
    List.iter(fun domains->Parallel.run ~domains(fun()->
      assert(same(I.Packed.force ~strict:true packed ~live)(Ok expected));
      assert(same(I.Packed.force ~strict:true packed ~live)(I.Packed.reference packed ~live))))[1;4;8];
    if count>1 then begin
      let region=packed in
      let skipped=I.Core.Region.rebuild region ~skip:[|1|] () |>function
        |Ok value->value|Error _->assert false in
      assert(same(I.Packed.force ~strict:true skipped ~live)
        (Ok(E.Int_array(Array.init(count-1)(fun i->(if i<1 then i else i+1)*factor)))))
    end)[false;true])[0;1;-3;max_int;min_int])[0;1;32769];
  I.Packed.Private.trace:=trace;
  print_endline "Integer scaling: exact wrapping constants, empty/multi-block/reference/domain bytes and skipped rows pass"

let () =
  let trace = !I.Packed.Private.trace in
  I.Packed.Private.trace := ignore;
  let count=32769 in
  let fn=match recorded
    "(fn [(mask : int) (p : vec4)] (if (int/eq mask 1) [t 2 3 4] p))" with E.Fn f->f|_->assert false in
  let mask=Array.init count(fun i->i mod 2) in
  let marker=E.Source("test/borrow_columns",Flow.Ty.Array Flow.Ty.Vec4,[]) in
  let value=ok(E.Private.map_function ~signature:Flow.Ty.{params=[Int;Vec4];result=Vec4}
    fn [E.Int_array mask;E.Vec4_array(Array.make(count*4)0.)]) in
  let packed=match value with E.Residual r->Option.get(compile r(E.Residual.residual_view r).term)|_->assert false in
  let region=packed in
  let sources=Array.map(fun(residual,(term:Flow.Workspace.term))->
    if term.ty<>Flow.Ty.Array Flow.Ty.Vec4 then residual,term else
      let term={term with node=Ref_binding("typed_borrowed_source",[])} in
      E.Residual.with_bindings residual ~term ["typed_borrowed_source",marker],term)region.sources in
  let packed=match I.Core.Region.rebuild region ~sources ()with Ok p->p
    |Error ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds)) in
  let data=Array.init(count*5)(fun i->if i mod 5=4 then 99. else if i=0 then -0. else float i) in
  let before=Marshal.to_string data [] in
  let columns=Array.init 4(fun offset->I.Packed.{data;offset;stride=5}) in
  let resolutions=ref 0 in
  let resolve=function E.Source("test/borrow_columns",_,_)->incr resolutions;Ok(E.Vec4_array(Array.make(count*4)0.))
    |value->Ok value in
  let source _ (term:Flow.Workspace.term)=Ok(if term.ty=Flow.Ty.Array Flow.Ty.Vec4 then Some columns else None) in
  let live=Input.at_time 0.25 in
  let run columns outputs=I.Packed.force_into ~strict:true ~resolve
    ~column_source:(fun r term->match source r term with Ok(Some _)->Ok(Some columns)|other->other)
    ~count ~outputs packed ~live in
  List.iter(fun domains->Parallel.run ~domains(fun()->
    let planes=Array.init 4(fun _->Array.make count 0.) in
    ignore(ok(run columns[|Array.map Option.some planes|]));
    Array.iteri(fun c plane->Array.iteri(fun i x->
      let expected=if mask.(i)=1 then (if c=0 then 0.25 else float(c+1)) else data.(i*5+c) in
      assert(Int64.bits_of_float x=Int64.bits_of_float expected))plane)planes;
    ignore(ok(run columns[|[|None;Some planes.(1);None;None|]|])))) [1;4;8];
  assert(!resolutions=0 && before=Marshal.to_string data []);
  let refusal code columns=match run columns[|[|None;None;None;None|]|]with
    |Error d->assert(d.code=code)|Ok _->assert false in
  refusal "E_ARRAY_TYPE" [|columns.(0)|];
  refusal "E_ARRAY_RANGE" [|{columns.(0)with stride=0};columns.(1);columns.(2);columns.(3)|];
  refusal "E_ARRAY_COUNT" [|{columns.(0)with data=[||];offset=0};columns.(1);columns.(2);columns.(3)|];
  let bad=Array.copy data in bad.(0)<-nan;
  refusal "E_NONFINITE" (Array.map(fun(c:I.Packed.column)->{c with data=bad})columns);
  I.Packed.Private.trace := trace;
  print_endline "Typed borrowed columns: mixed Int/Vec4 inputs, exact planes, no source materialization and selected errors passed"

let () =
  let trace = !I.Packed.Private.trace in
  I.Packed.Private.trace := ignore;
  let count = 32769 in
  let bodies =
    ["(if (< (mod x 2.0) 1.0) (if (< (mod x 4.0) 1.0) (+ (sin x) t) (- (cos x) t)) (+ x t))";
     "(if (< (mod x 2.0) 1.0) (+ x t) (exp (* -1000.0 x)))";
     "(if (< (mod x 2.0) 1.0) (noise3 (* [(* x 0.01) t 0.25] 0.5) :seed 31 :octaves 3) t)"] in
  List.iter (fun body ->
    let packed = Test_program.compile (Printf.sprintf
      "(map (fn [x] %s) (array/range %d))" body count) in
    let data = Array.init (count * 2) (fun i -> if i mod 2 = 0 then -99. else float (i / 2)) in
    let source _ _ = Ok (Some [|I.Packed.{data; offset=1; stride=2}|]) in
    List.iter (fun time ->
      let live = Input.at_time time in
      let reference = I.Packed.reference packed ~live in
      List.iter (fun domains -> Parallel.run ~domains (fun () ->
        assert (same (I.Packed.force ~strict:true packed ~live) reference);
        let output = Array.make count 0. in
        ignore (ok (I.Packed.force_into ~strict:true ~column_source:source ~count
          ~outputs:[|[|Some output|]|] packed ~live));
        assert (same (Ok (E.Float_array output)) reference);
        ignore (ok (I.Packed.force_into ~strict:true ~column_source:source ~count
          ~outputs:[|[|None|]|] packed ~live)))) [1;8]) [0.;0.25;2.]) bodies;
  List.iter (fun body ->
    let packed = Test_program.compile (Printf.sprintf
      "(map (fn [x] %s) (array/range %d))" body count) in
    let live = Input.at_time 0.25 in
    let nonfinite = function Error (d : Diagnostic.t) -> assert (d.code="E_NONFINITE")
      | Ok _ -> failwith "Selected or unconditional computation hid overflow" in
    nonfinite (I.Packed.reference packed ~live);
    List.iter (fun domains -> Parallel.run ~domains (fun () ->
      nonfinite (I.Packed.force ~strict:true packed ~live);
      nonfinite (I.Packed.force_into ~strict:true ~column_source:(fun _ _ -> Ok None) ~count
        ~outputs:[|[|None|]|] packed ~live))) [1;8])
    ["(if (< x 1.0) t (exp (* x 1000.0)))";
     "(let* [q (* [x x x] 1.7976931348623157e308)] (if (< x -1.0) (noise3 q :seed 31) t))";
     "(if (> x 1.0) (let* [q (* [x x x] 1.7976931348623157e308)] (if (< x -1.0) (noise3 q :seed 31) t)) t)"];
  I.Packed.Private.trace := trace;
  print_endline "Guarded float blocks: selected spans, nested branches, noise, borrowed columns and skipped sinks match reference at one/eight domains"

let () =
  let snapshot_trace= !I.Packed.Private.trace in
  I.Packed.Private.trace:=ignore;
  let fn=match recorded
    "(fn [(p : vec3) (n : vec3)] (+ p (* n t)))" with E.Fn f->f|_->assert false in
  let count=32769 in
  let special=[|0.;-0.;1.;-1.;Float.next_after 1. infinity;1e-200;-1e-200|] in
  let normals=Array.init (count*3) (fun i->if i<Array.length special then special.(i)
    else sin (float i *. 0.73) *. Float.ldexp 1. (i mod 100-50)) in
  let positions=Array.map (fun n-> -.n) normals in
  let value=ok (E.Private.map_function ~signature:Flow.Ty.{params=[Vec3;Vec3];result=Vec3}
    fn [E.Vec3_array positions;E.Vec3_array normals]) in
  let packed=match value with E.Residual r->Option.get (compile r (E.Residual.residual_view r).term)
    |_->assert false in
  List.iter (fun time->
    let live=Input.at_time time in
    let expected=E.Private.force_reference value ~live in
    List.iter (fun domains->Parallel.run ~domains (fun()->
      assert (same (I.Packed.force ~strict:true packed ~live) expected))) [1;8])
    [0.;-0.;1.;Float.next_after 1. infinity;0.16];
  let fn=match recorded ~ops:[]
    "(fn [(p : vec3) (n : vec3)] (+ p (* n (* t (noise3 (* p 0.16) :seed 4611686018427387903)))))"
    with E.Fn f->f|_->assert false in
  let value=ok (E.Private.map_function ~signature:Flow.Ty.{params=[Vec3;Vec3];result=Vec3}
    fn [E.Vec3_array positions;E.Vec3_array normals]) in
  let packed=match value with E.Residual r->Option.get (compile r (E.Residual.residual_view r).term)
    |_->assert false in
  let region=packed in
  let ordinary=I.Core.Region.rebuild region ~code:(Array.append region.code [|I.Core.Region.Unary (Sin,0)|]) ()
    |> (function Ok p->p|Error _->assert false) in
  List.iter (fun time->
    let live=Input.at_time time in
    let expected=E.Private.force_reference value ~live in
    List.iter (fun domains->Parallel.run ~domains (fun()->
      let actual=I.Packed.force ~strict:true packed ~live in
      assert (same actual expected);
      assert (same actual (I.Packed.force ~strict:true ordinary ~live)))) [1;8])
    [0.;-0.;1.;2.;Float.next_after 1. infinity;0.16];
  let fn=match recorded ~ops:[]
    "(fn [(p : vec3)] [p.z (+ p.y (* t (- (* (noise3 [(* p.x 0.16) (* p.z 0.16) 0.0] :seed -17) 2.0) 1.0))) p.x])"
    with E.Fn f->f|_->assert false in
  let value=ok (E.Private.map_function ~signature:Flow.Ty.{params=[Vec3];result=Vec3}
    fn [E.Vec3_array positions]) in
  let packed=match value with E.Residual r->Option.get (compile r (E.Residual.residual_view r).term)
    |_->assert false in
  let region=packed in
  let ordinary=I.Core.Region.rebuild region ~code:(Array.append region.code [|I.Core.Region.Unary (Sin,0)|]) ()
    |> (function Ok p->p|Error _->assert false) in
  List.iter (fun time->let live=Input.at_time time in
    let expected=E.Private.force_reference value ~live in
    List.iter (fun domains->Parallel.run ~domains (fun()->
      let actual=I.Packed.force ~strict:true packed ~live in
      assert (same actual expected);
      assert (same actual (I.Packed.force ~strict:true ordinary ~live)))) [1;8])
    [0.;-0.;1.;-1.;0.16;Float.next_after 1. infinity];
  let strided=Array.init (count*4) (fun i->if i mod 4<3 then positions.(i/4*3+i mod 4) else 99.) in
  let borrowed=Array.init 3 (fun offset->I.Packed.{data=strided;offset;stride=4}) in
  let source _ _=Ok (Some borrowed) in
  let before=Marshal.to_string strided [] in
  List.iter (fun domains->Parallel.run ~domains (fun()->
    let live=Input.at_time 0.16 in
    let expected=match ok (I.Packed.force ~strict:true packed ~live) with E.Vec3_array a->a|_->assert false in
    List.iter (fun packed->
      let planes=Array.init 3 (fun _->Array.make count 0.) in
      ignore (ok (I.Packed.force_into ~strict:true ~column_source:source ~count
        ~outputs:[|Array.map Option.some planes|] packed ~live));
      Array.iteri (fun component plane->Array.iteri (fun i x->
        assert (Int64.bits_of_float x=Int64.bits_of_float expected.(i*3+component))) plane) planes;
      ignore (ok (I.Packed.force_into ~strict:true ~column_source:source ~count
        ~outputs:[|[|None;Some planes.(1);None|]|] packed ~live))) [packed;ordinary])) [1;8];
  assert (before=Marshal.to_string strided []);
  let live=Input.at_time 0.16 in
  let refusal code columns count outputs=match I.Packed.force_into ~strict:true
    ~column_source:(fun _ _->Ok (Some columns)) ~count ~outputs packed ~live with
    |Error d->assert (d.code=code)|Ok _->assert false in
  refusal "E_ARRAY_COUNT" borrowed (count-1) [|[|None;None;None|]|];
  refusal "E_ARRAY_COUNT" borrowed count [|[|None;Some [||];None|]|];
  refusal "E_ARRAY_RANGE" [|{borrowed.(0) with stride=0};borrowed.(1);borrowed.(2)|]
    count [|[|None;None;None|]|];
  refusal "E_ARRAY_RANGE" [|{borrowed.(0) with offset=max_int};borrowed.(1);borrowed.(2)|]
    count [|[|None;None;None|]|];
  refusal "E_ARRAY_TYPE" [|borrowed.(0)|] count [|[|None;None;None|]|];
  refusal "E_ARRAY_COUNT" [|{borrowed.(0) with data=[||];offset=0};borrowed.(1);borrowed.(2)|]
    count [|[|None;None;None|]|];
  ignore (ok (I.Packed.force_into ~strict:true ~column_source:(fun _ _->Ok (Some
    (Array.init 3 (fun _->I.Packed.{data=[||];offset=0;stride=1}))))
    ~count:0 ~outputs:[|[|None;None;None|]|] packed ~live));
  let fn=match recorded "(fn [(p : vec3)] [p.x p.y (+ p.z t)])" with E.Fn f->f|_->assert false in
  let value=ok (E.Private.map_function ~signature:Flow.Ty.{params=[Vec3];result=Vec3}
    fn [E.Vec3_array positions]) in
  let identity=match value with E.Residual r->Option.get (compile r (E.Residual.residual_view r).term)
    |_->assert false in
  let bad=Array.copy strided in bad.(0)<-nan;
  let bad_columns=Array.map (fun (c:I.Packed.column)->{c with data=bad}) borrowed in
  (match I.Packed.force_into ~strict:true ~column_source:(fun _ _->Ok (Some bad_columns))
    ~count ~outputs:[|[|None;None;Some (Array.make count 0.)|]|] identity ~live with
    |Error d->assert (d.code="E_NONFINITE")|Ok _->failwith "Skipped output hid nonfinite borrowed lane");
  let planes=Array.init 3 (fun _->Array.make count 0.) in
  ignore (ok (I.Packed.force_into ~strict:true ~column_source:(fun _ _->Ok None)
    ~count ~outputs:[|Array.map Option.some planes|] identity ~live));
  let expected=match ok (I.Packed.force ~strict:true identity ~live) with E.Vec3_array a->a|_->assert false in
  Array.iteri (fun component plane->Array.iteri (fun i x->assert
    (Int64.bits_of_float x=Int64.bits_of_float expected.(i*3+component))) plane) planes;
  I.Packed.Private.trace:=snapshot_trace;
  print_endline "Packed output products: stored rounding, cancellation, signed zero and one/eight domains match reference"

let () =
  List.iter (fun (body,expected) ->
    let value=recorded body in
    let packed=match value with E.Residual r ->
      Option.get (compile r (E.Residual.residual_view r).term) | _ -> assert false in
    List.iter (fun time ->
      let live=Input.at_time time in
      let output=I.Packed.force packed ~live in
      assert (same output (E.Private.force_reference value ~live));
      assert (same output (Ok (expected time)))) [0.;0.37;2.])
    ["(map (fn [x] ^:bypass (+ (+ x t) (exp 10000.))) (array/range 4))",
       (fun time -> E.Float_array (Array.init 4 (fun i -> float i +. time)));
     "(map (fn [x] ^:bypass (+ (+ x t) [5. 5.])) (array/range 4))",
       (fun time -> E.Vec2_array (Array.init 8 (fun i -> float (i/2) +. time)))];
  let rounded={(Option.get (Flow.Op.find "float" Flow.Context.value)) with
    name="test/int_result";out=(fun _ -> Flow.Ty.Int);arithmetic=None;packed_extension=None} in
  let value=recorded ~ops:[rounded]
    "(map (fn [x] (float ^:bypass (test/int_result (+ x t)))) (array/range 4))" in
  let r=match value with E.Residual r -> r | _ -> assert false in
  assert (compile r (E.Residual.residual_view r).term=None);
  let program=ok (I.Executor.compile value) in
  let live=Input.at_time 0.7 in
  assert (same (I.Executor.force program ~live) (E.Private.force_reference value ~live));
  assert (same (I.Executor.force program ~live) (Ok (E.Float_array [|1.;2.;3.;4.|])))

let () =
  List.iter (fun (x,y,z) ->
    let expression = Printf.sprintf "(length [%.17g %.17g %.17g])" x y z in
    let expected = E.Float (sqrt ((x *. x +. y *. y) +. z *. z)) in
    assert (Marshal.to_string (recorded expression) [Marshal.No_sharing]
      = Marshal.to_string expected [Marshal.No_sharing]))
    [(-3.,4.,-12.); (0.,0.,0.); (-0.,0.,-0.); (1e-200,-1e-200,1e-200)];
  let value = recorded "(map (fn [p] (if (> t 0) (min (length p) 1.0) 0.0))
    (array/vec3 16385 [1e200 0 0]))" in
  let packed = match value with E.Residual residual ->
    compile residual (E.Residual.residual_view residual).term |> Option.get
    | _ -> failwith "length kernel did not defer" in
  List.iter (fun time ->
    let live = Input.at_time time in
    let reference = E.Private.force_reference value ~live in
    (match time, reference with
     | 0., Ok (E.Float_array values) -> assert (Array.for_all ((=) 0.) values)
     | 1., Error d -> assert (d.code = "E_NONFINITE")
     | _ -> assert false);
    List.iter (fun domains -> Parallel.run ~domains (fun () ->
      assert (same (I.Packed.force packed ~live) reference))) [1;8]) [0.;1.]
let () =
  List.iter (fun count ->
    let source = Printf.sprintf "(array/range %d)" count in
    let vectors = Printf.sprintf "(array/vec3 %d [0.25 -0.5 1])" count in
    let cases = [
      "(map (fn [x] (+ (+ x t) (floor 1.75))) " ^ source ^ ")";
      "(map (fn [x] [x t]) " ^ source ^ ")";
      "(map (fn [x] [x t (+ x t) -0.0]) " ^ source ^ ")";
      "(map (fn [(p : vec2)] (+ p t)) (map (fn [x] [x 1]) " ^ source ^ "))";
      "(map (fn [(p : vec4)] [p.w p.z p.y (+ p.x t)]) (map (fn [x] [x 1 2 3]) " ^ source ^ "))";
      "(let* [offset [t 1 2 3]] (map (fn [x] (+ [x x x x] offset)) " ^ source ^ "))";
      "(for [x " ^ source ^ "] [x t])";
      "(for [x " ^ source ^ "] :skip [0 1024 16384] [x t 2 3])";
      "(sum [x " ^ source ^ "] [x t])";
      "(array/sum (map (fn [x] [x t 2 3]) " ^ source ^ "))";
      "(scan [a [0.0 0.0]] [x " ^ source ^ "] (+ (* a 0.99) [x t]))";
      "(fold [a [0.0 0.0 0.0 0.0]] [x " ^ source ^ "] (+ (* a 0.99) [x t 2 3]))";
      "(for [x " ^ source ^ "] (sin (+ x t)))";
      "(for [x " ^ source ^ "] :skip [0 1024 16384] (sin (+ x t)))";
      "(for [x " ^ source ^ " y (array/range 3)] (+ (* x 0.25) (+ y t)))";
      "(sum [x " ^ source ^ "] (sin (+ x t)))";
      "(fold [a 0] [x " ^ source ^ "] (- (* a 0.99) (* (+ x t) 0.0001)))";
      "(scan [a 0.0] [x " ^ source ^ "] (- (* a 0.99) (* (+ x t) 0.0001)))";
      "(scan [a [0 0 0]] [x " ^ vectors ^ "] (+ (* a 0.99) (* x t)))";
      "(fold [a [1 2 3]] [x " ^ source ^ "] (+ x t))";
      "(reduce (fn [a x] (+ (* a 0.99) (* x t))) 0.0 " ^ source ^ ")";
      "(let* [f (fn [a x] (+ (* a 0.99) (* x t)))] (reduce f 0.0 " ^ source ^ "))";
      "(array/sum (map (fn [x] (sin (+ x t))) " ^ source ^ "))";
      "(sum [x (array/float " ^ string_of_int count ^ " -0.0)] (* x (+ t 1)))";
      "(array/sum (array/vec3 " ^ string_of_int count ^ " [t -0.0 1]))";
      "(sum [x " ^ source ^ "] (pow (+ x t) 1000000))";
      "(fold [a 0.0] [x " ^ source ^ "] (pow (+ a t) 1000000))"
    ] in
    List.iter (fun body ->
      let value = recorded body in
      let program = ok (I.Executor.compile value) in
      if count >= 1024 && not (has_cpu program) then
        failwith ("No CPU kernel (" ^ Flow.Ty.to_string (Flow.Value.ty_of value) ^ "): " ^ body);
      List.iter (fun time ->
        let live = Input.at_time time in
        let reference = E.Private.force_reference value ~live in
        List.iter (fun domains -> Parallel.run ~domains (fun () ->
          let output = I.Executor.force program ~live in
          if not (same output reference) then failwith (Printf.sprintf "Parity failed (%d domains, t=%g): %s" domains time body);
          (* Exercise the register executor even below the placement cutoff. *)
          match value with
          | E.Residual r -> (match compile r (E.Residual.residual_view r).term with
              | Some p ->
                  if not (same (I.Packed.force p ~live) reference) then
                    failwith (Printf.sprintf "Direct kernel parity failed (%d domains, t=%g): %s" domains time body)
              | None -> ())
          | _ -> ())) [1; 8]) [0.; 0.125; 1.25; 7.]) cases)
    [0; 1; 1023; 1024; 1025; 2051; 16385];
  let correlated = recorded "(for [x (array/range 3) y (array/float (+ x 1))] (+ y t))" in
  let program = ok (I.Executor.compile correlated) in
  assert (not (has_cpu program));
  assert (same (I.Executor.force program ~live:(Input.at_time 0.125))
    (E.Private.force_reference correlated ~live:(Input.at_time 0.125)));
  List.iter (fun body ->
    let value = recorded body in
    let program = ok (I.Executor.compile value) in
    if stages program < 2 then failwith ("Map chain did not fuse: " ^ body);
    let unfused = match value with E.Residual r ->
      compile ~fusion:false r (E.Residual.residual_view r).term |> Option.get
      | _ -> failwith "live map chain did not defer" in
    assert (I.Packed.stage_count unfused = 1);
    List.iter (fun time ->
      let live = Input.at_time time in
      let reference = E.Private.force_reference value ~live in
      List.iter (fun domains -> Parallel.run ~domains (fun () ->
        assert (same (I.Executor.force program ~live) reference);
        assert (same (I.Packed.force unfused ~live) reference))) [1;8]) [0.;0.125;1.25;7.])
    ["(map (fn [y] (* y y)) (map (fn [x] (sin (+ x t))) (array/range 16385)))";
     "(let* [a (map (fn [x] (sin (+ x t))) (array/range 16385))] (map (fn [y] (* y y)) a))";
     "(array/sum (map (fn [y] (* y y)) (map (fn [x] (sin (+ x t))) (array/range 16385))))";
     "(fold [a 0.0] [x (map (fn [x] (sin (+ x t))) (array/range 16385))] (+ a x))";
     "(map (fn [p n] (+ p n)) (map (fn [x] (* x t)) (array/vec3 16385 [1 2 3])) (array/vec3 16385 [0 1 0]))";
     "(map (fn [x] (* x t)) (map (fn [x] (+ x t)) (array/range (frame/index))))"];
  (* A shortened zip must still evaluate the producer's unused tail and report its failure. *)
  let value = recorded "(map (fn [x y] (+ x y)) (map (fn [x] (pow (+ x t) 1000000)) (array/range 2051)) (array/float 1))" in
  let program = ok (I.Executor.compile value) in
  assert (stages program = 1);
  assert (same (I.Executor.force program ~live:(Input.at_time 0.))
    (E.Private.force_reference value ~live:(Input.at_time 0.)));
  let ticks = ref 0. in
  let profile = I.Profile.create ~clock:(fun () -> ticks := !ticks +. 0.001; !ticks) in
  let value = recorded "(map (fn [y] (* y y)) (map (fn [x] (sin (+ x t))) (array/range 2051)))" in
  let program = ok (I.Executor.compile ~profile value) in
  ignore (ok (I.Executor.force program ~live:(Input.at_time 1.25)));
  let reports = I.Profile.executions profile in
  assert (List.length reports = 1);
  let report = List.hd reports in
  assert (report.tier = Cpu_kernel && report.seconds > 0.);
  assert (fst report.owner = ["g";"tested"] && List.length report.sites >= 2);
  ignore (ok (I.Executor.force program ~live:(Input.at_time 2.)));
  assert (List.length (I.Profile.executions profile) = 1);
  ignore (ok (I.Executor.force ~reference:true program ~live:(Input.at_time 2.)));
  assert ((List.hd (I.Profile.executions profile)).tier = Interp);
  print_endline "Packed map/for/sum/fold/scan/reduce parity, group timings and one/eight domains"

let () =
  List.iter (fun body ->
    let value = recorded body in
    let packed = match value with E.Residual r ->
      compile r (E.Residual.residual_view r).term | _ -> assert false in
    assert (packed = None);
    let program = ok (I.Executor.compile value) in
    List.iter (fun time ->
      let live = Input.at_time time in
      assert (same (I.Executor.force program ~live)
        (E.Private.force_reference value ~live))) [0.;0.25;2.])
    ["(fold [a false] [x (array/range 1024)] (+ x t))";
     "(scan [a false] [x (array/range 1024)] (+ x t))"];
  let fn = match recorded "(fn [(uv : vec2)] [uv.x uv.y t 1])" with
    | E.Fn fn -> fn | _ -> assert false in
  let grid = Array.init (2 * 32769) (fun i -> float (i mod 17) /. 16.) in
  let original = Array.copy grid in
  let value = ok (E.Private.map_function
    ~signature:Flow.Ty.{params = [Vec2]; result = Vec4} fn [E.Vec2_array grid]) in
  let packed = match value with E.Residual r ->
    compile r (E.Residual.residual_view r).term |> Option.get | _ -> assert false in
  assert (I.Packed.static_count packed = Some 32769);
  List.iter (fun time ->
    let live = Input.at_time time in
    let expected = E.Private.force_reference value ~live in
    List.iter (fun domains -> Parallel.run ~domains (fun () ->
      let output = I.Packed.force packed ~live in
      assert (same output expected);
      match ok output with E.Vec4_array xs ->
        assert (Array.length xs = 4 * 32769);
        assert (xs.(0) = grid.(0) && xs.(1) = grid.(1) && xs.(2) = time && xs.(3) = 1.)
      | _ -> assert false)) [1;8]) [0.;0.25;2.];
  assert (grid = original)

let () =
  let prepare ?ops body = match recorded ?ops body with
    | E.Residual residual -> residual, (E.Residual.residual_view residual).term
    | _ -> failwith ("diagnostic fixture did not defer: " ^ body) in
  let refused body code =
    let residual, term = prepare body in
    assert (compile residual term = None);
    match I.Packed.compile_result residual term with
    | Error [d] ->
        assert (d.code = code && d.message <> "");
        assert (Option.fold ~none:false ~some:(fun (span : Diagnostic.span) ->
          span.finish > span.start) d.span);
        d
    | _ -> failwith ("expected " ^ code ^ ": " ^ body) in
  List.iter (fun (body, code) -> ignore (refused body code))
    ["(let* [s (state [a 0.0] (+ a (frame/dt)))] (map (fn [x] (+ x s)) (array/range 3)))", "E_PACKED_STATE";
     "(for [x (array/range 3) y (array/float (+ x 1))] (+ y t))", "E_PACKED_FORM";
     "(let* [bad (list 1 2)] (map (fn [x] (+ (+ x t) (first bad))) (array/range 3)))", "E_PACKED_CAPTURE";
     "(map (fn [x] (+ t (first (list x 1)))) (array/range 3))", "E_PACKED_FORM";
     "(map sin (array/float 3 t))", "E_PACKED_FUNCTION";
     "(map (fn [x] (+ x (value/rand t 1 2))) (array/range 3))", "E_PACKED_OPERATOR";
     "(fold [a false] [x (array/range 3)] (+ x t))", "E_PACKED_TYPE"];
  let residual, term = prepare ~ops:[]
      "(map (fn [p] (noise3 p :seed (frame/index))) (array/vec3 3 [0 1 2]))" in
  assert (compile residual term = None);
  (match I.Packed.compile_result residual term with
   | Error [d] -> assert (d.code = "E_PACKED_CONSTANT") | _ -> assert false);
  let sins n name = List.init n (fun _ -> "(sin ") |> String.concat ""
    |> fun prefix -> prefix ^ name ^ String.make n ')' in
  let kernel n = "(map (fn [x] (+ t " ^ sins n "x" ^ ")) (array/range 3))" in
  let residual, term = prepare (kernel 61) in
  let packed = Option.get (compile residual term) in
  assert (Array.length (I.Packed.Private.view packed).code = Flow.Packed_ops.register_limit);
  ignore (refused (kernel 62) "E_PACKED_LIMIT");
  let constant = "(map (fn [x] (+ (+ x t) (pow 1e200 2.0))) (array/range 3))" in
  (* Ordinary static evaluation also rejects this constant. Compile its
     checked term in the compatible empty scope before materialization. *)
  let bad = match (List.hd (checked constant).graphs).body.node with
    | Flow.Workspace.Let ([_, term], _) -> term | _ -> assert false in
  let residual, _ = prepare "(map (fn [x] (+ x t)) (array/range 3))" in
  assert (compile residual bad = None);
  let d = match I.Packed.compile_result residual bad with
    | Error [d] -> assert (d.code = "E_NONFINITE"); d | _ -> assert false in
  assert (Option.fold ~none:false ~some:(fun (span : Diagnostic.span) ->
    span.finish - span.start = String.length "(pow 1e200 2.0)") d.span);
  let parity value packed = List.iter (fun time ->
    let live = Input.at_time time in
    let reference = E.Private.force_reference value ~live in
    List.iter (fun domains -> Parallel.run ~domains (fun () ->
      assert (same (I.Packed.force packed ~live) reference))) [1;8]) [0.;0.125;1.25;7.] in
  List.iter (fun vector ->
    let value = recorded ("(let* [r {:vector " ^ vector ^ "}] (map (fn [x] (+ x r.vector.x)) (array/range 2051)))") in
    match value with
    | E.Residual residual ->
        let packed = Option.get (compile residual (E.Residual.residual_view residual).term) in
        parity value packed
    | _ -> assert false) ["[t 1]"; "[t 1 2]"; "[t 1 2 3]"];
  let value = recorded ("(map (fn [y] (- " ^ sins 40 "y" ^ " t)) (map (fn [x] (+ t "
    ^ sins 40 "x" ^ ")) (array/range 2051)))") in
  (match value with
   | E.Residual residual ->
       let term = (E.Residual.residual_view residual).term in
       let child = match term.node with Flow.Workspace.Hof (`Map, [_; child]) -> child | _ -> assert false in
       let child = Option.get (compile ~fusion:false residual child) in
       let packed = Option.get (compile residual term) in
       assert (Array.length (I.Packed.Private.view packed).code
         + Array.length (I.Packed.Private.view child).code > Flow.Packed_ops.register_limit);
       assert (I.Packed.stage_count packed = 1);
       parity value packed
   | _ -> assert false);
  print_endline "Packed diagnostics: refusal reasons/spans, 64-register boundary, captures and unfused fallback passed"

let () = Region_snapshot.finish "test_packed"

(* Several outputs per region, outside the trace above: the snapshots stay single-output. *)
let () =
  let program = Test_program.two_outputs () in
  let live = Input.at_time 1.25 in
  let results = List.map (fun domains ->
    let result = ref [] in
    Parallel.run ~domains (fun () -> result := ok (I.Packed.force_all program ~live));
    !result) [1; 8] in
  (match results with
   | [one; eight] ->
       assert (Marshal.to_string one [Marshal.No_sharing] = Marshal.to_string eight [Marshal.No_sharing]);
       (match one with
        | [E.Float_array a; E.Vec2_array b] ->
            assert (Array.length a = 1024 && Array.length b = 2048);
            Array.iteri (fun i x ->
              assert (x = float_of_int i +. 1.25 && b.(2 * i) = float_of_int i && b.(2 * i + 1) = x)) a
        | _ -> assert false)
   | _ -> assert false);
  assert (Result.is_error (I.Packed.force program ~live));
  print_endline "Packed outputs: two outputs match at 1 and 8 domains"

let () =
  let rec repeated = function
    | 0 -> "(+ x t)"
    | n -> let body = repeated (n - 1) in "(+ " ^ body ^ " " ^ body ^ ")" in
  List.iter (fun fusion ->
    let packed = Test_program.compile ~fusion
      ("(map (fn [x] " ^ repeated 5 ^ ") (array/range 16385))") in
    assert (Array.length (I.Packed.Private.view packed).code = 8);
    List.iter (fun time ->
      let live = Input.at_time time in
      let reference = I.Packed.reference packed ~live in
      List.iter (fun domains -> Parallel.run ~domains (fun () ->
        assert (same (I.Packed.force packed ~live) reference))) [1;8]) [0.;0.37;2.]) [false;true];
  let signed = Test_program.compile
    "(map (fn [x] [(+ x t) 0.0 -0.0 (+ x t)]) (array/range 4))" in
  let view = I.Packed.Private.view signed in
  assert (view.outputs.(0).(0) = view.outputs.(0).(3));
  assert (view.outputs.(0).(1) <> view.outputs.(0).(2));
  List.iter (fun domains -> Parallel.run ~domains (fun () ->
    let live = Input.at_time 0.37 in
    assert (same (I.Packed.force signed ~live) (I.Packed.reference signed ~live)))) [1;8];
  let guards = Test_program.compile
    "(map (fn [x] (if (and (< x t) (not (= x 0)))
      (max (min (sqrt (abs (* x x))) 1) 0) (sin (cos (/ (pow (mod x 7) 2) t)))))
      (array/range 4))" in
  assert (I.Packed.gpu_refusals guards = []);
  assert (Array.fold_left (fun n check -> if check then n + 1 else n) 0
    (I.Packed.Private.finite_checks guards) = 9);
  print_endline "Packed sharing: repeated expressions fit the register budget, signed zero and domain parity preserved"

let () =
  let live = Input.at_time 0.25 in
  let profile = I.Profile.create ~clock:(fun () -> 1.) in
  let tiny = recorded "(map (fn [x] (+ x t)) (array/range 1))" in
  let program = ok (I.Executor.compile ~strict:true ~profile tiny) in
  assert (same (I.Executor.force ~strict:true program ~live)
    (E.Private.force_reference tiny ~live));
  assert (List.exists (fun (run : I.execution) -> run.tier = Cpu_kernel)
    (I.Profile.executions profile));
  List.iter (fun body ->
    match I.Executor.compile ~strict:true (recorded body) with
    | Error diagnostic -> assert (diagnostic.code = "E_KERNEL_FORM")
    | Ok _ -> failwith ("strict numeric body accepted interpreter fallback: " ^ body))
    ["(map (fn [x] (value/rand x t)) (array/range 4))";
     "(map (fn [x] (+ x t)) (map (fn [x] (value/rand x t)) (array/range 4)))"];
  let overflow = Test_program.compile
    "(map (fn [x] (* (+ x t) 1e308)) (array/range 4))" in
  let references = ref [] in
  let measure = (fun () -> 1.), (fun _ ~seconds:_ ~reference -> references := reference :: !references) in
  assert (Result.is_error (I.Packed.force ~strict:true ~measure overflow ~live));
  assert (not (List.mem true !references));
  ignore (I.Packed.force ~measure overflow ~live);
  assert (List.mem true !references);
  print_endline "Strict numeric bodies: tiny packed execution, early refusal and no error fallback passed"

let () =
  let trace= !I.Packed.Private.trace in
  Fun.protect ~finally:(fun()->I.Packed.Private.trace:=trace)(fun()->
    I.Packed.Private.trace:=ignore;
    let producer=Test_program.compile ~fusion:false
      "(for [x (array/float (int/sub (frame/index) 1)) y (array/float (if (> t 0.) -1 2))] (+ x y))" in
    let captured=Test_program.compile ~fusion:false
      "(let* [gain (exp (* t 100000.))] (for [x (array/float (int/sub (frame/index) 1)) y (array/float 2)] (+ x gain)))" in
    let float_bank (packed:I.Core.Region.t)=
      let region=packed in
      assert(region.iteration=I.Core.Region.Product);
      assert(Array.for_all(fun(_,term)->term.Flow.Workspace.ty=Flow.Ty.Array Flow.Ty.Float)region.sources);
      assert(Array.for_all(function
        |I.Core.Region.Input _|Const _|Uniform _|Frame _|Binary _|Unary _->true|_->false)region.code) in
    let equal expected actual=match expected,actual with
      |Ok(E.Float_array expected),Ok(E.Float_array actual)->
          (* Empty Array.sub and Array.make have different OCaml block tags.
             Compare the language's element bits and cardinality directly. *)
          assert(Array.length expected=Array.length actual);
          Array.iteri(fun i x->assert(Int64.bits_of_float x=Int64.bits_of_float actual.(i)))expected
      |Error (expected:Diagnostic.t),Error (actual:Diagnostic.t)->
          assert(expected.code=actual.code && expected.message=actual.message && expected.cause=actual.cause)
      |_->failwith "Float Product forcing differs from reference" in
    List.iter(fun packed->float_bank packed;
      List.iter(fun(frame,time)->let live={(Input.at_time time)with frame} in
        let expected=I.Packed.reference packed ~live in
        List.iter(fun domains->Parallel.run ~domains(fun()->
          try equal expected(I.Packed.force ~strict:true packed ~live)
          with exn->failwith(Printf.sprintf "Float Product frame=%d time=%g domains=%d: %s"
            frame time domains(Printexc.to_string exn)))) [1;4;8])
        [0,0.25;1,0.25;2,0.25;32770,(-0.25)]) [producer;captured];
      let region : I.Core.Region.t = producer in
    List.iter(fun iteration->
      let packed=ok(Result.map_error List.hd(I.Core.Region.rebuild region ~iteration ())) in
      let calls=ref 0 in
      let source _ _=incr calls;if !calls=1 then
        Ok(Some[|I.Packed.{data=[||];offset=0;stride=1}|])
        else Error(Diagnostic.error ~code:"E_LATER_SOURCE" "Later column source was reached.") in
      let actual=I.Packed.force_into ~strict:true ~column_source:source ~count:0
        ~outputs:[|[|Some[||]|]|] packed ~live:(Input.at_time 0.25) in
      match iteration,actual with
      |I.Core.Region.Product,Ok _->assert(!calls=1)
      |Zip,Error d->assert(!calls=2 && d.code="E_LATER_SOURCE")
      |_->assert false) [I.Core.Region.Product;Zip];
    let malformed _ _=Ok(Some[||]) in
    (match I.Packed.force_into ~strict:true ~column_source:malformed ~count:0
        ~outputs:[|[|Some[||]|]|] producer ~live:(Input.at_time 0.25) with
     |Error d->assert(d.code="E_ARRAY_TYPE")|_->assert false);
    print_endline "Float Product: empty first input suppresses later producers, borrowed sources and captures; reached errors and Zip forcing remain exact")

let () =
  (* A map read by a minimum, a maximum and the map that scales by both runs once per outermost run. *)
  let live = Input.at_time 0.25 in
  let body = "(let* [h (map (fn [x] (* x (+ t 2.0))) (array/range 4096))
                      lo (array/min h) hi (array/max h)]
                 (map (fn [y] (/ (- y lo) (- hi lo))) h))" in
  let program = Test_program.compile body in
  let runs = ref 0 in
  let measure = (fun () -> 1.), (fun _ ~seconds:_ ~reference:_ -> incr runs) in
  let value = ok (I.Packed.force ~strict:true ~measure program ~live) in
  assert (!runs = 2);
  assert (same (Ok value) (E.Private.force_reference (recorded body) ~live));
  print_endline "Packed scope: a map read by three captures of one run is evaluated once, bytes equal to the walker"
