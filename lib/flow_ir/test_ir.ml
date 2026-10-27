open Flow_ir
module E = Flow.Eval

let () =
  let module W = Flow.Workspace in
  let check source = match W.check Flow.Check.{version=1;kinds=[]}
      (Flow.Syntax.parse source |> Result.get_ok) with
    | Some workspace, [] -> workspace
    | _, ds -> failwith (String.concat "\n" (List.map Flow.Diagnostic.to_string ds)) in
  let qualify ?inputs workspace =
    let observed = ref [] in
    let qualified,evaluation = qualify_workspace ~record:true ?inputs
      ~observe:(fun producer residual ->
        let view=E.Private.residual_view residual in
        if Option.is_some producer && view.term.ty=Flow.Ty.Array Flow.Ty.Vec4 then begin
          List.iter (fun fusion ->
            match Packed.compile_result ~fusion residual view.term with
            | Ok packed when Packed.gpu_refusals packed=[] ->
                assert (Packed.site packed=(view.instance,Option.get producer,view.iter))
            | _ -> ()) [true;false];
          observed:=(producer,view.site):: !observed
        end) workspace |> Result.get_ok in
    qualified,evaluation,!observed in
  let path=["img";"picture"] in
  let image body="(workspace pixels (graph img :context image (let* [picture
    (image/map (fn [uv] "^body^"))] picture)))" in
  List.iter (fun (body,accepted,code) ->
    let qualified,_,_=qualify (check(image body)) in
    assert (W.Paths.mem path qualified.approx=accepted);
    if not accepted then assert (List.exists (fun (d:Flow.Diagnostic.t)->d.code=code)
      (List.assoc path qualified.approx_reasons)))
    ["[uv.x uv.y t 1]",true,"";
     "[(floor 2.5) uv.y t 1]",true,"";
     "[(floor uv.x) uv.y t 1]",false,"E_GPU_FORM";
     "[(+ uv.x 1e39) uv.y t 1]",false,"E_GPU_FORM"];
  let shared=check "(workspace pixels (graph img :context image
    (let* [f (fn [uv] [uv.x uv.y t 1]) held (fn [uv] [uv.x uv.y t 1])
      a (image/map f) b (image/map f)] b)))" in
  let qualified,_,seen=qualify shared in
  assert (W.Paths.mem ["img";"a"] qualified.approx && W.Paths.mem ["img";"b"] qualified.approx);
  assert (not(W.Paths.mem ["img";"held"] qualified.approx));
  assert (List.mem_assoc (Some ["img";"a"]) seen && List.mem_assoc (Some ["img";"b"]) seen);
  let captures=check "(workspace pixels
    (graph img :context image [(offset : float 2.0)]
      (let* [picture (image/map (fn [uv] [(+ uv.x offset) uv.y t 1]))] picture))
    (graph host :context host [(one : float 2.0) (two : float 3.0)]
      (list (ref img :offset one) (ref img :offset two))))" in
  List.iter (fun (one,two,accepted)->
    let qualified,_,_=qualify ~inputs:["host",["one",E.Float one;"two",E.Float two]] captures in
    assert(W.Paths.mem path qualified.approx=accepted);
    if not accepted then assert(List.exists(fun(d:Flow.Diagnostic.t)->d.code="E_GPU_FORM")
      (List.assoc path qualified.approx_reasons))) [1e39,7.,false;7.,1e39,false;2.,7.,true];
  let state=check "(workspace pixels (graph img :context image
    (let* [bias (state [n 0.0] (+ n 0.1)) picture (image/map (fn [uv] [uv.x uv.y bias 1]))] picture)))" in
  let qualified,_,_=qualify state in
  assert (not(W.Paths.mem path qualified.approx));
  assert(List.exists(fun(d:Flow.Diagnostic.t)->d.code="E_PACKED_STATE")
    (List.assoc path qualified.approx_reasons));
  let counterfeit=Option.get(Flow.Op.find "image/map" Flow.Context.image) in
  let counterfeit={counterfeit with category="Counterfeit image map"} in
  let qualified,_,_=qualify {shared with ops=counterfeit::shared.ops} in
  assert(not(W.Paths.mem ["img";"a"] qualified.approx));
  assert(List.exists(fun(d:Flow.Diagnostic.t)->d.code="E_GPU_FORM")
    (List.assoc ["img";"a"] qualified.approx_reasons));
  print_endline "image qualification: authored sites, shared functions, folded/unsupported forms, capture requalification, state and declaration identity pass"

(* External value declarations cannot be swept in Flow's tests: Flow does not
   depend on Flow_ir. Exercise every declared argument and choice here. *)
let () =
  let rec sample = function
    |Flow.Ty.Int->Flow.Value.Int 2
    |Float->Float 0.25
    |Vec3->Vec3(0.3,0.7,0.2)
    |Color|Text->Text "#123456"
    |List ty->List [|sample ty;sample ty|]
    |_->assert false in
  List.iter(fun (op:Flow.Op.t)->
    assert(Flow.Op.validate [op]=None);
    assert(Option.get(Flow.Op.find ~extra:Operators.all op.name op.ctx)==op);
    let signature=op.signature in
    let args entries=List.map(fun(name,ty)->name,sample ty)entries in
    let run args=
      op.check args;
      let result=op.body ~live:(Frame_input.at_time 0.)
        ~node:(fun _ _->failwith "value operator created a deferred node")args in
      assert(Flow.Ty.fits(Flow.Value.ty_of result)
        (op.out(List.map(fun(_,value)->Flow.Value.ty_of value)args)))in
    let base=args signature.pos in
    List.iter(fun base->
      run base;
      Option.iter(fun(name,ty)->run(base@[name,sample ty;name,sample ty]))signature.rest;
      List.iter(fun(name,ty)->
        let values=match List.assoc_opt name op.choices with
          |Some choices->List.map(fun text->Flow.Value.Text text)choices
          |None->[sample ty]in
        List.iter(fun value->run(base@[name,value]))values)signature.kw;
      run(base@args signature.kw)) [base;base@args signature.opt];
    List.iter(fun(name,choices)->assert(choices<>[]);
      assert(List.mem_assoc name(signature.pos@signature.opt@signature.kw));
      List.iter(fun choice->run(List.map(fun(key,value)->
        key,if key=name then Flow.Value.Text choice else value)
        (base@args signature.opt@args signature.kw)))choices)op.choices)Operators.all

let node ?(ty = Flow.Ty.Float) ?(count = Count.Static 1) ?(rate = Static)
    ?(precision = Exact) ?(args = []) ?(scope = []) ?(invariant = false) name kind =
  let id = [name], scope in
  {id; ty; count; rate; precision; kind; args; scope; invariant; provenance = [id]; tier = Interp}
let edge name node = {name; node}
let constant name v = node name (Source (Constant (E.Float v)))
let op ?count ?rate ?precision ?scope ?invariant name args =
  node ?count ?rate ?precision ?scope ?invariant ~args name
    (Kernel {body = Operation "+"; elementwise = true; requires_exact = false})
let ir nodes roots = {nodes = Array.of_list nodes; roots = Array.of_list roots; groups = [||]}
let ok = function Ok x -> x | Error d -> failwith (Flow.Diagnostic.to_string d)
let approx_error ir = match place ir with
  | Error d -> assert (d.Flow.Diagnostic.code = "E_APPROX_SINK")
  | Ok _ -> failwith "accepted an approximate exact-only sink"

let () =
  List.iter (fun (value, count) ->
    let program = ok (Executor.compile value) in
    let graph = Executor.graph program in
    assert (graph.nodes.(graph.roots.(0)).count = Count.Static count))
    [E.Int_array [|0; max_int; min_int|], 3;
     E.Text_array [|""; "猫"|], 2;
     E.Text_array [||], 0]

let () =
  let shared = share (ir [constant "a" 1.; constant "b" 1.;
    op "sum" [edge "a" 0; edge "b" 1]] [2]) in
  assert (Array.length shared.nodes = 2);
  assert (shared.nodes.(0).provenance = [(["a"], []); (["b"], [])]);
  assert (shared.nodes.(1).args = [edge "a" 0; edge "b" 0]);
  let bits = share (ir [constant "zero" 0.; constant "negative_zero" (-0.)] [0; 1]) in
  assert (Array.length bits.nodes = 2);
  let phases = share (ir [constant "a" 1.;
    op ~precision:Exact "exact" [edge "a" 0];
    op ~precision:Approx "approx" [edge "a" 0];
    op ~rate:Frame "frame" [edge "a" 0]] [1; 2; 3]) in
  assert (Array.length phases.nodes = 4);
  (* Large array constants are shared by physical identity, never by a key two different arrays could share. *)
  let big_constant name a = node ~ty:(Flow.Ty.Array Flow.Ty.Float) ~count:(Count.Static (Array.length a)) name (Source (Constant (E.Float_array a))) in
  let a = Array.init 1000 float_of_int in
  let different = Array.copy a in
  different.(999) <- -1.;
  assert (Array.length (share (ir [big_constant "a" a; big_constant "same" a] [0; 1])).nodes = 1);
  assert (Array.length (share (ir [big_constant "a" a; big_constant "different" different] [0; 1])).nodes = 2);
  assert (Array.length (share (ir [big_constant "a" a; big_constant "copy" (Array.copy a)] [0; 1])).nodes = 2);
  let small = [|1.; 2.|] in
  assert (Array.length (share (ir [big_constant "a" small; big_constant "copy" (Array.copy small)] [0; 1])).nodes = 1);
  let opaque = node "catalog" (Opaque ("sop/box", None)) in
  assert (Array.length (share (ir [opaque; {opaque with id = ["other"], []}] [0; 1])).nodes = 2);
  let hoisted = hoist (ir [constant "a" 1.;
    op ~scope:[4] ~invariant:true "uniform" [edge "a" 0];
    op ~scope:[4] "varying" [edge "a" 0];
    op ~scope:[4] ~rate:Frame ~invariant:true "live" [edge "a" 0]] [1; 2; 3]) in
  assert (hoisted.nodes.(1).scope = []);
  assert (hoisted.nodes.(1).id = (["uniform"], [4]));
  assert (hoisted.nodes.(2).scope = [4] && hoisted.nodes.(3).scope = [4]);
  let empty = prune (ir [] []) in
  assert (empty.nodes = [||] && empty.roots = [||]);
  let pruned = prune (ir [constant "unused" 5.; constant "a" 2.;
    op "sum" [edge "a" 1; edge "b" 1]] [2]) in
  assert (Array.length pruned.nodes = 2 && pruned.roots = [|1|]);
  assert (pruned.nodes.(1).args = [edge "a" 0; edge "b" 0]);
  let chain count_a count_b = ir [constant "a" 2.;
    op ~count:count_a "first" [edge "a" 0];
    op ~count:count_b "second" [edge "a" 1]] [2] in
  let group_sizes ir = Array.map Array.length (fuse ir).groups in
  assert (group_sizes (chain (Count.Static 2048) (Count.Static 2048)) = [|1; 2|]);
  assert (group_sizes (chain Count.Unknown Count.Unknown) = [|1; 1; 1|]);
  let origin = ["g"; "P"], [] in
  assert (group_sizes (chain (Count.Data origin) (Count.Data origin)) = [|1; 2|]);
  assert (group_sizes (chain (Count.Data origin) (Count.Data (["g"; "N"], []))) = [|1; 1; 1|]);
  let barrier = fuse (ir [constant "a" 1.; op "a" [edge "x" 0];
    node ~args:[edge "x" 1] "display" (Sink (Display "draw/point"));
    op "b" [edge "x" 1]] [3]) in
  assert (Array.for_all (fun g -> Array.length g = 1) barrier.groups);
  let branching = fuse (ir [constant "a" 1.; op "a" [edge "x" 0];
    op "b" [edge "x" 1]; op "c" [edge "x" 1]] [2; 3]) in
  assert (Array.for_all (fun g -> Array.length g = 1) branching.groups);
  let source = {(constant "gpu" 1.) with precision = Approx} in
  let forwarded = op "forwarded" [edge "x" 0] in
  let display = node ~args:[edge "x" 1] "display" (Sink (Display "draw/point")) in
  let placed = ok (place (ir [source; forwarded; display] [2])) in
  assert (placed.nodes.(1).precision = Approx && placed.nodes.(2).precision = Approx);
  List.iter (fun sink -> approx_error (ir [source; forwarded;
    node ~args:[edge "x" 1] "sink" (Sink sink)] [2])) [Export; State_seed; Cache_key];
  assert ((ok (place (ir [source; forwarded; node ~args:[edge "x" 1] "sink" (Sink Sop_input)] [2]))).nodes.(2).precision = Exact);
  approx_error (ir [source; node ~args:[edge "x" 0] "catalog" (Opaque ("sop/transform", None))] [1]);
  let readback = node ~args:[edge "value" 0] "exact"
    (Kernel {body = Readback; elementwise = true; requires_exact = false}) in
  let exact = ok (place (ir [source; readback;
    node ~args:[edge "x" 1] "export" (Sink Export)] [2])) in
  assert (exact.nodes.(1).precision = Exact && exact.nodes.(2).precision = Exact);
  (try ignore (share (ir [op "cycle" [edge "x" 0]] [0])); failwith "accepted a cycle"
   with Invalid_argument _ -> ())

let check ?(ops = []) ?(catalog = {Flow.Check.version = 1; kinds = []}) text =
  let forms = ok (Flow.Syntax.parse text) in
  match Flow.Workspace.check ~ops catalog forms with
  | Some w, ds when not (List.exists (fun (d : Flow.Diagnostic.t) -> d.severity = Error) ds) -> w
  | _, ds -> failwith (String.concat "; " (List.map Flow.Diagnostic.to_string ds))
let value ?ops body =
  let ws = check ?ops ("(workspace w (graph g :context value " ^ body ^ "))") in
  let evaluated = ok (E.static ~record:true ws) in
  List.assoc "g" evaluated.results
let rec same_bits a b = match a, b with
  | E.Float a, E.Float b -> Int64.bits_of_float a = Int64.bits_of_float b
  | Vec3 (a, b, c), Vec3 (x, y, z) ->
      List.for_all2 (fun a b -> Int64.bits_of_float a = Int64.bits_of_float b) [a; b; c] [x; y; z]
  | Float_array a, Float_array b | Vec3_array a, Vec3_array b ->
      Marshal.to_string a [] = Marshal.to_string b []
  | List a, List b -> Array.length a = Array.length b && Array.for_all2 same_bits a b
  | Record a, Record b | Struct (_, _, a), Struct (_, _, b) ->
      List.length a = List.length b && List.for_all2 (fun (n, a) (m, b) -> n = m && same_bits a b) a b
  | Fn _, Fn _ -> true
  | a, b -> a = b
let same_result a b = match a, b with
  | Ok a, Ok b -> same_bits a b
  | Error a, Error b -> Flow.Diagnostic.to_string a = Flow.Diagnostic.to_string b
  | _ -> false

let () =
  let mapped = value "(array/sum (map (fn [x] (sin (+ x t))) (array/range 2048)))" in
  let compiled = Executor.graph (ok (Executor.compile mapped)) in
  let packed = Array.find_opt (function {kind=Kernel {body=Packed_map _;_};_}->true | _->false) compiled.nodes |> Option.get in
  List.iter (fun count ->
    let placed = ok (place (ir [{packed with count=Count.Static count;args=[]}] [0])) in
    assert (placed.nodes.(0).tier = Cost.cheapest ~legal:[Interp;Cpu_kernel] ~count);
    assert (Cost.packed ~count = placed.nodes.(0).tier)) [0;16;512;1024;65_536];
  let placed = ok (place (ir [{packed with count=Count.Data (["source"],[]);args=[]}] [0])) in
  assert (placed.nodes.(0).tier = Cpu_kernel);
  let live = Frame_input.at_time 0.125 in
  List.iter (fun count ->
    let value = value (Printf.sprintf "(array/sum (map (fn [x] (sin (+ x t))) (array/range %d)))" count) in
    assert (same_result (Executor.force (ok (Executor.compile value)) ~live)
      (E.Private.force_reference value ~live))) [0;16;33;34;512;1024]

let () =
  let array = E.Struct ("sop/attr", Flow.Ty.Array Flow.Ty.Vec3, []) in
  let array = (Executor.graph (ok (Executor.compile array))).nodes.(0) in
  assert (match array.count with Count.Data _ -> true | _ -> false);
  let record = value "(let* [r {:a (map (fn [x] (+ x t)) (array/range 2048))}] (array/sum r.a))" in
  let record = Executor.graph (ok (Executor.compile record)) in
  assert (Array.for_all (fun (n : node) -> match n.ty, n.count with
    | Flow.Ty.Array _, Count.Static 1 -> false | _ -> true) record.nodes);
  let profile = Profile.create ~clock:(fun () -> 1.) in
  for i = 0 to 512 do
    let ws = check (Printf.sprintf "(workspace w (graph g%d :context value (+ t 1)))" i) in
    let evaluated = ok (E.static ws) in
    let program = ok (Executor.compile ~profile (snd (List.hd evaluated.results))) in
    ignore (ok (Executor.force program ~live:(Frame_input.at_time 0.)))
  done;
  let reports = Profile.executions profile in
  assert (List.length reports = 512);
  assert (List.for_all (fun report -> List.for_all (fun (instance,_,_) -> instance = 0) report.sites) reports);
  assert ((List.hd reports).seconds = 0. && (List.hd reports).tier = Closure);
  List.iter (fun body ->
    let v = value body in
    let p = ok (Executor.compile v) in
    List.iter (fun time ->
      let live = Frame_input.at_time time in
      let reference = E.Private.force_reference v ~live in
      List.iter (fun domains -> Rays_math.Parallel.run ~domains (fun () ->
        assert (same_result (Executor.force p ~live) reference);
        assert (same_result (Executor.force ~reference:true p ~live) reference))) [1; 8])
      [0.; 0.125; 1.25; 7.])
    ["(+ (* t 2.0) (sin t))";
     "(exact (+ (* t 2.0) (sin t)))";
     "(let* [a t b (* a 3)] (+ b a))";
     "(let* [p [(sin t) (* t 2) (+ t 1)]] (+ p.x p.z))";
     "(let* [r {:x t :nested (list [(sin t) 0 0] [t 0 0])}] r.x)";
     "(if (> t 1) (sqrt t) (sin t))";
     "(sum [x (range 3)] (* t x))";
     "(array/sum (map (fn [x] (+ x t)) (array/range 128)))";
     "(let* [f (fn [x] (* x t))] (sum [x (map f (list 1 2 3))] x))";
     "(sqrt (pow t 1000000))"];
  let scalar = ok (Executor.compile (value "(+ (* t 2) (sin t))")) |> Executor.graph in
  assert (Array.exists (fun (n : node) -> n.tier = Closure) scalar.nodes);
  let conditional = ok (Executor.compile (value "(if (> t 0) (/ t 0) (sin t))")) |> Executor.graph in
  assert (Array.exists (function {kind = Kernel {body = Reference _; _}; _} -> true | _ -> false) conditional.nodes);
  let bad_live = {(Frame_input.at_time 0.) with dt = nan} in
  let v = value "(+ t 1)" in
  assert (same_result (Executor.force (ok (Executor.compile v)) ~live:bad_live)
    (E.Private.force_reference v ~live:bad_live));
  let v = value "(let* [a (state [p 0.0] (+ p (frame/dt))) b (* a 2)] (+ a b))" in
  let p = ok (Executor.compile v) in
  let optimized = E.create_state () and reference = E.create_state () in
  List.iter (fun frame ->
    let live = {(Frame_input.at_time (float frame)) with frame; dt = 0.125} in
    assert (same_result (Executor.force ~state:optimized p ~live)
      (E.Private.force_reference ~state:reference v ~live))) [0; 1; 2; 2; 3; 1];
  assert (E.state_stamp optimized = E.state_stamp reference)

let () =
  List.iter (fun body ->
    let v = value body in
    let program = ok (Executor.compile v) in
    assert (Array.exists (fun (n : node) -> n.tier = Cpu_kernel) (Executor.graph program).nodes);
    List.iter (fun time ->
      let live = Frame_input.at_time time in
      let reference = E.Private.force_reference v ~live in
      let sequential = Rays_math.Parallel.run ~domains:1 (fun () -> Executor.force program ~live) in
      let parallel = Rays_math.Parallel.run ~domains:8 (fun () -> Executor.force program ~live) in
      assert (same_result sequential reference && same_result parallel reference)) [0.; 0.125; 1.25; 7.])
    ["(array/sum (map (fn [x] (sin (+ (* x 0.25) t))) (array/range 2051)))";
     "(array/sum (map (fn [x] (/ (+ x t) (- x 1))) (array/range 2051)))";
     "(array/sum (map (fn [x] (mod (- t x) 0.7)) (array/range 2051)))";
     "(let* [p (array/nth (map (fn [p n] (+ p (* n (* t 0.8)))) (array/vec3 2051 [1 2 3]) (array/vec3 2053 [0 1 0])) 2050)] p.y)";
     "(array/sum (map (fn [x] (let* [a (* x t) b (sqrt a)] (+ (cos b) b))) (array/range 2051)))";
     "(array/sum (map (fn [x] (min (+ x t) (max x 100.0))) (array/range 2051)))";
     "(array/sum (map (fn [x] (+ (pow t 1000000) (* x 0))) (array/range 2051)))"];
  let v = value "(let* [unused (pow t 1000000) mapped (map (fn [x] (+ x t)) (array/range 2051))] (array/sum mapped))" in
  let program = ok (Executor.compile v) in
  List.iter (fun time ->
    let live = Frame_input.at_time time in
    assert (same_result (Executor.force program ~live) (E.Private.force_reference v ~live))) [0.; 0.125; 7.];
  let v = value "(array/sum (map (fn [x] (+ x t)) (array/float 0)))" in
  assert (Executor.force (ok (Executor.compile v)) ~live:(Frame_input.at_time 1.) = Ok (E.Float 0.));
  let v = value "(array/sum (map (fn [x] (+ x t)) (array/range (frame/index))))" in
  let program = ok (Executor.compile v) in
  List.iter (fun count ->
    let live = {(Frame_input.at_time 1.25) with frame = count} in
    assert (same_result (Executor.force program ~live) (E.Private.force_reference v ~live))) [0; 1023; 1024; 2051]

let read path = In_channel.with_open_text path In_channel.input_all
let () =
  let ops = Flow_ir.Operators.all in
  let mapped ~ops body =
    let ws = check ~ops ("(workspace noise (graph g :context value (let* [mapped " ^ body ^ "] (array/sum mapped))))") in
    let evaluated = ok (E.static ~record:true ws) in
    List.assoc ["g"; "mapped"] evaluated.records |> List.hd |> snd in
  let body = "(map (fn [p] (exact (noise3 (+ p [t 0 0])))) (array/vec3 2051 [0.3 0.7 -0.2]))" in
  let v = mapped ~ops body in
  let program = ok (Executor.compile v) in
  assert (Array.exists (fun (n : node) -> n.tier = Cpu_kernel) (Executor.graph program).nodes);
  List.iter (fun time ->
    let live = Frame_input.at_time time in
    let reference = ok (E.Private.force_reference v ~live) in
    List.iter (fun domains -> Rays_math.Parallel.run ~domains (fun () ->
      let result = ok (Executor.force program ~live) in
      assert (same_bits result reference);
      match result with
      | E.Float_array xs ->
          let expected = Rays_math.Noise.sample3 (Rays_math.Noise.create 0)
            ~x:(0.3 +. time) ~y:0.7 ~z:(-0.2) in
          assert (Array.length xs = 2051);
          Array.iter (fun x -> assert (Int64.bits_of_float x = Int64.bits_of_float expected)) xs
      | _ -> assert false)) [1; 8]) [0.; 0.125; 1.25; 7.];
  let v = mapped ~ops "(map (fn [p] (noise3 (* p (pow t 1000000)))) (array/vec3 2051 [1 2 3]))" in
  let program = ok (Executor.compile v) in
  let live = Frame_input.at_time 7. in
  assert (same_result (Executor.force program ~live) (E.Private.force_reference v ~live));
  let custom = {Flow_ir.Operators.noise3 with packed_extension = None;
    body = (fun ~live:_ ~node:_ _ -> E.Float 0.125)} in
  let v = mapped ~ops:[custom] body in
  let program = ok (Executor.compile v) in
  assert (Array.for_all (fun (n : node) -> n.tier <> Cpu_kernel) (Executor.graph program).nodes);
  assert (same_result (Executor.force program ~live) (E.Private.force_reference v ~live));
  match ok (Executor.force program ~live) with
  | E.Float_array xs -> assert (Array.for_all (( = ) 0.125) xs)
  | _ -> assert false

let () =
  let ops=Flow_ir.Operators.all in
  List.iter(fun seed->List.iter(fun octaves->
    let ws=check ~ops (Printf.sprintf "(workspace n (graph g :context value (let* [mapped (map (fn [p] (noise3 (+ p [t 0 0]) :seed %d :octaves %d)) (array/vec3 2051 [0.31 0.73 -0.29]))] (array/sum mapped))))" seed octaves)in
    let evaluated=ok(E.static ~record:true ws)in
    let value=List.assoc ["g";"mapped"] evaluated.records |> List.hd |> snd in
    let program=ok(Executor.compile value)in
    assert(Array.exists(fun(n:node)->n.tier=Cpu_kernel)(Executor.graph program).nodes);
    List.iter(fun time->let live=Frame_input.at_time time in
      let reference=ok(E.Private.force_reference value ~live)in
      List.iter(fun domains->let actual=Rays_math.Parallel.run ~domains(fun()->ok(Executor.force program ~live))in
        assert(same_bits actual reference)) [1;8]) [0.;0.125;1.25;7.]) [1;5;32]) [0;2026;-451];
  List.iter(fun expression->let ws=check ~ops ("(workspace c (graph g :context value (let* [result "^expression^"] 0)))")in
    let evaluated=ok(E.static ws)in
    ignore(ok(Executor.force (ok(Executor.compile(List.assoc "g" evaluated.results))) ~live:(Frame_input.at_time 0.))))
    ["(color/hsl 220 0.7 0.4)";"(color/hsla 451 0.5 0.6 0.25)";"(color/gradient (list \"#101426\" \"#f26444\") 0.3)"]

let () =
  (* Compare all million elements with the independent tree walker. *)
  let ws = check "(workspace large (graph g :context value (let* [xs (array/range 1000000) mapped (map (fn [x] (sin (+ (* x 0.25) t))) xs)] (array/sum mapped))))" in
  let evaluated = ok (E.static ~record:true ws) in
  let v = List.assoc ["g"; "mapped"] evaluated.records |> List.hd |> snd in
  let program = ok (Executor.compile v) in
  let live = Frame_input.at_time 1.25 in
  let one = Rays_math.Parallel.run ~domains:1 (fun () -> ok (Executor.force program ~live)) in
  let eight = Rays_math.Parallel.run ~domains:8 (fun () -> ok (Executor.force program ~live)) in
  let reference = ok (E.Private.force_reference v ~live) in
  let reference_eight = Rays_math.Parallel.run ~domains:8 (fun () -> ok (E.Private.force_reference v ~live)) in
  assert (same_bits one reference);
  assert (same_bits eight reference_eight);
  assert (same_bits one eight);
  (match one with
   | E.Float_array xs -> assert (Array.length xs = 1_000_000);
       Array.iteri (fun i x -> assert (Int64.bits_of_float x = Int64.bits_of_float (sin (float i *. 0.25 +. 1.25)))) xs
   | _ -> failwith "CPU map did not return a packed array")
let () =
  let catalog, _ = ok (Flow.Check.catalog_of_manifest (read "../sop_catalog/flow_manifest.sexp")) in
  List.iter (fun name ->
    let ws = check ~catalog (read ("../../specification/workspace/cases/" ^ name ^ ".lisp")) in
    let evaluated = ok (E.static ~record:true ws) in
    let dataflow = of_evaluation ws catalog evaluated |> optimize |> ok in
    assert (Array.length dataflow.roots >= List.length evaluated.results);
    let opaque_ids = Array.to_list dataflow.nodes |> List.filter_map (fun n -> match n.kind with
      | Opaque _ -> Some n.id | _ -> None) in
    assert (List.length opaque_ids = List.length (List.sort_uniq compare opaque_ids));
    let values = List.map snd evaluated.results @ evaluated.states
      @ Array.fold_right (fun (n : E.node) acc -> List.map snd n.args @ acc) evaluated.plan.nodes []
      @ Array.fold_right (fun (i : E.instance) acc -> i.result :: List.map snd i.inputs @ acc) evaluated.plan.instances []
      @ List.concat_map (fun (_, records) -> List.map snd records) evaluated.records in
    let prepared = List.map (fun v -> v, ok (Executor.compile v)) values in
    List.iter (fun time ->
      let live = Frame_input.at_time time in
      List.iter (fun domains -> Rays_math.Parallel.run ~domains (fun () ->
        let reference_state = E.create_state () and ir_state = E.create_state () in
        List.iter (fun (v, p) ->
          assert (same_result (Executor.force ~state:ir_state p ~live)
            (E.Private.force_reference ~state:reference_state v ~live))) prepared)) [1; 8])
      [0.; 0.125; 1.25; 7.])
    ["bloom"; "facade"; "garland"; "kit"; "orrery"; "rosette";
     "sunflower"; "tiles"; "tree"; "tunnel"; "variations"; "wave"];
  print_endline "Flow IR passes, precision, scalar/fallback execution and twelve-fixture parity passed"

let ()=
  let ws=check "(workspace gpu (graph g :context value (let* [mapped (map (fn [x] (+ x t)) (array/range 1024))] 0.0)))"in
  let ws,evaluation=ok(qualify_workspace ~record:true ws)in
  let value=List.assoc ["g";"mapped"] evaluation.records |>List.hd|>snd in
  let profile=Profile.create ~clock:(fun()->0.)in
  let program=ok(Executor.compile ~profile ~approx:ws.approx ~sink:(Display "circles") value)in
  let packed=Array.find_map(function {kind=Kernel{body=Packed_map packed;_};_}->Some packed|_->None)
    (Executor.graph program).nodes |>Option.get in
  let root=node ~ty:(Flow.Ty.Array Flow.Ty.Float) ~count:(Count.Static 1024) "mapped"
    (Kernel{body=Packed_map packed;elementwise=true;requires_exact=true})in
  let approximable=Flow.Workspace.Paths.singleton ["mapped"]in
  let placed=ok(place ~approx:approximable ~gpu_cost:(fun _ ~count:_->Some 0.)
    (ir [root;node "display" ~args:[edge "positions" 0](Sink(Display "circles"))][1]))in
  assert(placed.nodes.(0).tier=Gpu && placed.nodes.(0).precision=Approx);
  List.iter(fun kind->assert(match place ~approx:approximable ~gpu_cost:(fun _ ~count:_->Some 0.)
      (ir[root;node "sink" ~args:[edge "value" 0]kind][1])with
    |Error d->d.Flow.Diagnostic.code="E_APPROX_SINK"|Ok _->false))
    [Sink State_seed;Sink Cache_key;Sink Export;Opaque("catalog",None)];
  let sink=ok(place ~approx:approximable ~gpu_cost:(fun _ ~count:_->Some 0.)
    (ir[root;node "sink" ~args:[edge "value" 0](Sink Sop_input)][1]))in
  assert(sink.nodes.(0).tier=Gpu && sink.nodes.(1).precision=Exact);
  assert((ok(place ~approx:approximable (ir[root][0]))).nodes.(0).tier=Cpu_kernel);
  assert((ok(place ~approx:approximable ~gpu_cost:(fun _ ~count:_->Some 1.) (ir[root][0]))).nodes.(0).tier=Cpu_kernel);
  let exact=node "exact" ~args:[edge "value" 0](Kernel{body=Readback;elementwise=true;requires_exact=false})in
  let placed=ok(place ~approx:approximable ~gpu_cost:(fun _ ~count:_->Some 0.)
    (ir[root;exact;node "sop" ~args:[edge "value" 1](Sink Sop_input)][2]))in
  assert(placed.nodes.(1).precision=Exact && placed.nodes.(1).tier=Gpu_readback);
  (* This backend exercises callback ownership/selection only; it does not claim
     GPU execution or numerical conformance. Native tests run the emitted shader. *)
  let prepared=ref 0 and dispatched=ref 0 and gpu_seconds=ref(Some 0.002)in
  let backend:Gpu.backend={cost=(fun _ ~count:_->None);prepare=(fun _->incr prepared;
    Ok{run=(fun inputs->incr dispatched;Ok Gpu.{identity=1;count=inputs.count;width=1;stamp=Int64.of_int !dispatched;
      gpu_seconds= !gpu_seconds});
      readback=(fun _->Ok(E.Float_array[||]))})}in
  let overflow_ws = check "(workspace gpu (graph g :context value (let* [mapped
      (map (fn [x] (+ x (+ t 1e39))) (array/range 1024))] 0.0)))" in
  assert (List.mem_assoc ["g";"mapped"] overflow_ws.packed);
  let overflow_ws, overflow_evaluation = ok (qualify_workspace ~record:true overflow_ws) in
  assert (not (Flow.Workspace.Paths.mem ["g";"mapped"] overflow_ws.approx));
  assert (List.exists (fun (d : Flow.Diagnostic.t) -> d.code = "E_GPU_FORM")
    (List.assoc ["g";"mapped"] overflow_ws.approx_reasons));
  let overflow_value = List.assoc ["g";"mapped"] overflow_evaluation.records |> List.hd |> snd in
  let overflow_program = ok (Executor.compile ~approx:overflow_ws.approx
      ~sink:(Display "circles") overflow_value) in
  let overflow_packed = Array.find_map (function
      | {kind=Kernel {body=Packed_map packed;_};_} -> Some packed | _ -> None)
      (Executor.graph overflow_program).nodes |> Option.get in
  let overflow_root = {root with kind=Kernel {body=Packed_map overflow_packed;
      elementwise=true;requires_exact=true}} in
  assert ((ok (place ~approx:approximable ~gpu_cost:(fun _ ~count:_ -> Some 0.)
      (ir [overflow_root] [0]))).nodes.(0).tier = Cpu_kernel);
  Gpu.with_backend backend(fun()->
    assert (ok (Executor.try_display ~policy:Gpu.Qualification overflow_program
      ~live:(Frame_input.at_time 1.)) = None);
    assert (!prepared = 0 && !dispatched = 0);
    assert(match ok(Executor.force_display program ~live:(Frame_input.at_time 1.))with Executor.Cpu _->true|_->false);
    assert(!prepared=0 && !dispatched=0);
    for i=1 to 2 do assert(match ok(Executor.force_display ~policy:Gpu.Qualification program
      ~live:(Frame_input.at_time(float i)))with Executor.Gpu value->value.count=1024|_->false)done;
    assert(!prepared=1 && !dispatched=2);
    assert(List.exists(fun report->report.tier=Gpu && report.seconds=0.002)(Profile.executions profile));
    List.iter(fun seconds->gpu_seconds:=seconds;
      ignore(ok(Executor.force_display ~policy:Gpu.Qualification program ~live:(Frame_input.at_time 3.)));
      assert((List.find(fun report->report.tier=Gpu)(Profile.executions profile)).seconds=0.))
      [None;Some nan;Some(-1.)];
    assert(match ok(Executor.force_display ~reference:true ~policy:Gpu.Qualification program
      ~live:(Frame_input.at_time 1.))with Executor.Cpu _->true|_->false);
    (* The SOP input sink is an implicit readback; a CPU or export sink still refuses a GPU array. *)
    let sop_input=ok(Executor.compile ~approx:ws.approx ~sink:Sop_input value)in
    assert(match Executor.force_display ~policy:Gpu.Qualification sop_input ~live:(Frame_input.at_time 1.)with
      |Ok(Executor.Cpu _)->true|_->false);
    let illegal=ok(Executor.compile ~approx:ws.approx ~sink:Export value)in
    assert(match Executor.force_display ~policy:Gpu.Qualification illegal ~live:(Frame_input.at_time 1.)with
      |Error d->d.Flow.Diagnostic.code="E_APPROX_SINK"|Ok _->false));
  assert(List.exists(fun report->report.tier=Gpu_compile)(Profile.executions profile));
  assert(List.exists(fun report->report.tier=Gpu)(Profile.executions profile));
  assert(match ok(Executor.force_display ~policy:Gpu.Qualification program ~live:(Frame_input.at_time 1.))with
    Executor.Cpu _->true|_->false);
  (* Measured policy: a finite GPU estimate plus its display sink competes with
     the CPU kernel plus instance building; an expensive estimate keeps CPU. *)
  List.iter(fun(seconds,expect_gpu)->
    Gpu.with_backend {backend with cost=(fun _ ~count:_->Some seconds)}(fun()->
      assert(match ok(Executor.force_display program ~live:(Frame_input.at_time 1.))with
        |Executor.Gpu _->expect_gpu|Executor.Cpu _->not expect_gpu)))
    [0.,true;Cost.estimate Cpu_kernel ~count:1024,true;1.,false];
  print_endline "GPU neutral callbacks: measured placement, illegal exact sinks, qualification scope and phase profiles passed"

let () =
  let module W = Flow.Workspace in
  let path = ["g";"mapped"] in
  let text body = "(workspace qualification (graph g :context host " ^ body ^ "))" in
  let mapped body = text ("(let* [mapped " ^ body ^ " alias mapped] alias)") in
  let fingerprint (evaluation : E.t) =
    let key = Flow.Value.key_of ~residual:E.Private.residual_id in
    List.map (fun (name,v) -> name,key v) evaluation.results,
    List.map (fun (path,vs) -> path,List.map (fun (it,v) -> it,key v) vs) evaluation.records,
    Array.map (fun (n : E.node) -> n.id,n.kind,n.site,n.iter,n.inst,
      List.map (fun (name,v) -> name,key v) n.args) evaluation.plan.nodes in
  let qualify ?inputs ws =
    assert (W.Paths.is_empty ws.W.approx);
    let qualified,evaluation = ok (qualify_workspace ~record:true ?inputs ws) in
    assert (qualified.packed == ws.packed && qualified.packed_roots == ws.packed_roots);
    let reference = ok (E.static ~record:true ?inputs ws) in
    assert (fingerprint reference = fingerprint evaluation);
    List.iter (fun domains -> Rays_math.Parallel.run ~domains (fun () ->
      List.iter (fun time ->
        let live = Frame_input.at_time time in
        let actual_state = E.create_state () and reference_state = E.create_state () in
        List.iter2 (fun (_,a) (_,b) ->
          assert (same_result (E.force ~state:actual_state a ~live)
            (E.force ~state:reference_state b ~live))) evaluation.results reference.results)
        [0.;0.125;1.25;7.])) [1;8];
    qualified in
  let expect ?(ops = []) source accepted code =
    let ws = check ~ops source in
    let qualified = qualify ws in
    if W.Paths.mem path qualified.approx <> accepted then
      failwith ("qualification mismatch: " ^ source ^ "\n" ^ String.concat "\n"
        (List.concat_map (fun (p,ds) -> List.map (fun d -> String.concat "/" p ^ ": " ^
          Flow.Diagnostic.to_string d) ds) qualified.approx_reasons));
    if not accepted then assert (List.exists (fun (d : Flow.Diagnostic.t) -> d.code = code)
      (List.assoc path qualified.approx_reasons));
    qualified in
  List.iter (fun body -> ignore (expect (mapped body) true ""))
    ["(map (fn [x] (+ x t)) (array/range 4))";
     "(map (fn [x] (+ x 2.0)) (array/float 0))";
     "(for [x (array/float 4)] (* x 2.0))";
     "(map (fn [x] [x t]) (array/float 4))";
     "(map (fn [x] [x t 0 1]) (array/float 4))";
     "(map (fn [uv] [uv.x uv.y t 1]) (map (fn [x] [x 2]) (array/float 4)))";
     "(map (fn [x] (floor 2.5)) (array/float 4))"];
  List.iter (fun (body,code) -> ignore (expect (mapped body) false code))
    ["(map (fn [x] (+ x (floor t))) (array/float 4))", "E_GPU_FORM";
     "(for [x (array/float 4)] :skip [0] (+ x t))", "E_GPU_FORM";
     "(for [x (array/float 4) y (array/float 4)] (+ x y))", "E_GPU_FORM";
     "(map sin (array/float 4 t))", "E_PACKED_FUNCTION";
     "(map (fn [x] (+ x (+ t (pow 1e20 2.0)))) (array/float 4))", "E_GPU_FORM"];
  ignore (expect ~ops:Operators.all
    (mapped "(map (fn [p] (noise3 p :seed 7 :octaves 3)) (array/vec3 4))") true "");
  ignore (expect ~ops:Operators.all
    (mapped "(map (fn [p] (noise3 p :octaves 99)) (array/vec3 0))") false "E_PACKED_CONSTANT");
  let counterfeit = {(Option.get (Flow.Op.find "sin" Flow.Context.value)) with name = "noise3"} in
  ignore (expect ~ops:[counterfeit] (mapped "(map (fn [x] (noise3 (+ x t))) (array/float 4))")
    false "E_PACKED_OPERATOR");
  (* a chain of n sines, flat in a let* so it stays within the reader's nesting limit *)
  let sins n = "(let* [s0 x " ^ String.concat " " (List.init n (fun i -> Printf.sprintf "s%d (sin s%d)" (i + 1) i)) ^ Printf.sprintf "] s%d)" n in
  List.iter (fun (n,accepted) -> ignore (expect (mapped
    ("(map (fn [x] (+ t " ^ sins n ^ ")) (array/range 3))")) accepted "E_PACKED_LIMIT"))
    (let l = Flow.Packed_ops.register_limit in [l - 3,true;l - 2,false]);
  let ws = check "(workspace qualification (graph g :context value [(offset : float 2.0)]
    (let* [mapped (map (fn [x] (+ x (+ offset t))) (array/range 4))] 0.0)))" in
  List.iter (fun (offset,accepted) ->
    let qualified = qualify ~inputs:["g",["offset",E.Float offset]] ws in
    assert (W.Paths.mem path qualified.approx = accepted);
    if not accepted then assert (List.exists (fun (d : Flow.Diagnostic.t) -> d.code = "E_GPU_FORM")
      (List.assoc path qualified.approx_reasons))) [2.,true;1e39,false;7.,true];
  let ws = check "(workspace qualification
    (graph g :context value [(offset : float 2.0)]
      (let* [mapped (map (fn [x] (+ x (+ offset t))) (array/range 4))] 0.0))
    (graph h :context host [(one : float 2.0) (two : float 3.0)]
      (list (ref g :offset one) (ref g :offset two))))" in
  List.iter (fun (one,two,accepted) ->
    let qualified = qualify ~inputs:["h",["one",E.Float one;"two",E.Float two]] ws in
    assert (W.Paths.mem path qualified.approx = accepted))
    [1e39,7.,false;7.,1e39,false;2.,7.,true];
  ignore (expect (text "(let* [s (state [a 0.0] (+ a (frame/dt)))
    mapped (map (fn [x] (+ x s)) (array/range 4))] 0.0)") false "E_PACKED_STATE");
  let independent = check (text "(state [s 0.0] (let* [mapped
    (map (fn [x] (+ x t)) (array/range 4))] (+ s (array/sum (exact mapped)))))") in
  let qualified = qualify independent in
  let roots = W.Paths.of_list (List.map snd independent.packed_roots) in
  let maps = W.Paths.filter (fun path -> match List.assoc_opt path independent.packed with
    | Some (W.Candidate _) -> true | _ -> false) roots in
  assert (not (W.Paths.is_empty maps) && W.Paths.subset maps qualified.approx);
  let bypass = check (mapped "^:bypass (exact (map (fn [x] (+ x t)) (array/range 4)))") in
  let qualified = qualify bypass in
  assert (not (W.Paths.is_empty qualified.approx));
  let conditional = check (mapped "(if true (map (fn [x] (+ x t)) (array/range 4))
    (map (fn [x] (+ x t)) (array/range 4)))") in
  let qualified = qualify conditional in
  assert (not (W.Paths.mem path qualified.approx));
  assert (List.exists (fun (d : Flow.Diagnostic.t) -> d.code = "E_PACKED_PENDING")
    (List.assoc path qualified.approx_reasons));
  let boundary = check (text "(let* [blocked (map (fn [x] (floor (+ x t))) (array/float 4))
    mapped (map (fn [x] (+ x t)) blocked)] 0.0)") in
  let seen = ref false in
  let qualified,_ = ok (qualify_workspace ~observe:(fun producer residual ->
    if producer = Some path then begin
      seen := true;
      List.iter (fun fusion -> match Packed.compile_result ~fusion residual (E.Private.residual_view residual).term with
        | Ok packed -> assert (Packed.gpu_refusals packed <> [])
        | Error _ -> assert false) [true;false]
    end) boundary) in
  assert (!seen && not (W.Paths.mem path qualified.approx)
    && not (W.Paths.mem ["g";"blocked"] qualified.approx));
  let shared = check (text "(let* [f (fn [x] (sin (+ x t))) good (map f (array/float 4))
    bad (map f (list 1.0 2.0))] 0.0)") in
  let qualified = qualify shared in
  assert (W.Paths.mem ["g";"good"] qualified.approx && not (W.Paths.mem ["g";"bad"] qualified.approx)
    && not (W.Paths.mem ["g";"f"] qualified.approx));
  (* A known physical root with an unresolved owner must invalidate another
     successful observation of that producer, in either traversal order. *)
  let ws = check (text "(let* [a (map (fn [x] (+ x t)) (array/range 4))
    b (map (fn [x] (+ x t)) (array/range 4))] b)") in
  let a = ["g";"a"] and b = ["g";"b"] in
  let form p = List.find (fun (_,path) -> path = p) ws.packed_roots |> fst in
  List.iter (fun (known,ambiguous) ->
    let fake = ["missing"] in
    let modified = {ws with packed_roots = (form ambiguous, known) :: (form ambiguous,fake) ::
      List.filter (fun (_,path) -> path <> ambiguous) ws.packed_roots} in
    let qualified,_ = ok (qualify_workspace modified) in
    assert (not (W.Paths.mem known qualified.approx));
    assert (List.exists (fun (d : Flow.Diagnostic.t) -> d.code = "E_PACKED_PENDING")
      (List.assoc known qualified.approx_reasons))) [a,b;b,a];
  List.iter (fun source ->
    let ws = check source in
    let observed = ref W.Paths.empty in
    let qualified,_ = ok (qualify_workspace ~observe:(fun producer _ ->
      Option.iter (fun path -> observed := W.Paths.add path !observed) producer) ws) in
    let expected = W.Paths.of_list (List.map snd ws.packed_roots) in
    assert (not (W.Paths.is_empty expected));
    if not (W.Paths.subset expected !observed) then failwith ("unmatched callable root: " ^ source);
    let candidates = W.Paths.filter (fun path -> match List.assoc_opt path ws.packed with
      | Some (W.Candidate _) -> true | _ -> false) expected in
    assert (not (W.Paths.is_empty candidates) && W.Paths.subset candidates qualified.approx))
    [text "(let* [f (fn [offset] (let* [unused 0] (+ (array/sum (exact (map (fn [x] (+ x offset)) (array/float 4)))) 1))) alias f] (alias 2.0))";
     "(workspace qualification (defn f :context host [(offset : float)]
        (let* [unused 0] (map (fn [x] (+ x offset)) (array/float 4))))
        (defn invoke :context host [(alias : fn)] (alias 2.0))
        (graph g :context host (invoke :alias f)))"];
  let checked_forms forms = match W.check {Flow.Check.version = 1; kinds = []} forms with
    | Some ws,_ -> ws | _,ds -> failwith (String.concat "\n" (List.map Flow.Diagnostic.to_string ds)) in
  let graph name = ok (Flow.Syntax.parse ("(graph " ^ name ^ " :context host
    (let* [mapped (map (fn [x] (+ x t)) (array/range 4))] mapped))")) |> List.hd in
  let forms = [Flow.Syntax.make (List [Flow.Syntax.make (Sym "workspace");
    Flow.Syntax.make (Sym "identity"); graph "g"; graph "h"])] in
  let rec zero (form : Flow.Syntax.t) =
    let node = match form.node with
      | List xs -> Flow.Syntax.List (List.map zero xs)
      | Vec xs -> Vec (List.map zero xs) | Map xs -> Map (List.map zero xs)
      | Quote (kind,x) -> Quote (kind,zero x) | node -> node in
    {form with id = 0; node} in
  List.iter (fun forms ->
    let qualified = qualify (checked_forms forms) in
    assert (W.Paths.mem ["g";"mapped"] qualified.approx && W.Paths.mem ["h";"mapped"] qualified.approx))
    [forms;List.map zero forms];
  print_endline "Workspace qualification: actual captures, empty/vector/loop/noise kernels, limits, ambiguity, callable ownership and repeated/zero IDs passed"

(* P4.6: a mark set is one node that names its attribute edges. *)
let () =
  let marks = Marks {set = "draw/circles"; attributes = [
    {role = Position; edge = "positions"; output = 0}; {role = Size; edge = "radius"; output = 0};
    {role = Fill; edge = "fill"; output = 0}]} in
  let set = node ~args:[edge "positions" 1; edge "radius" 2; edge "fill" 3] "circles" marks in
  let graph = ir [constant "unused" 9.; op "pos" [edge "x" 0]; constant "r" 1.; constant "r2" 1.; set] [4] in
  let graph = {graph with nodes = Array.mapi (fun i n -> if i = 1 then {n with args = []} else n) graph.nodes} in
  (* Every pass keeps the node, its attribute list and its provenance. *)
  let shared = share graph in
  assert (Array.length shared.nodes = 4);
  let kept (g : t) = let n = g.nodes.(g.roots.(0)) in
    n.kind = marks && n.provenance = [(["circles"], [])] in
  assert (kept shared && kept (hoist shared) && kept (prune shared));
  let pruned = prune shared in
  assert (Array.length pruned.nodes = 3);
  let set = pruned.nodes.(pruned.roots.(0)) in
  assert (List.map (fun (a, i) -> a.role, pruned.nodes.(i).id) (attribute_edges pruned set)
    = [Position, (["pos"], []); Size, (["r"], []); Fill, (["r"], [])]);
  let placed = ok (place pruned) and fused = fuse pruned in
  assert (kept placed && kept fused);
  assert (attribute_edges placed placed.nodes.(1) = []);
  (* The drawing call's nodes become mark sets; other draw calls stay display sinks. *)
  let ws = check "(workspace w (graph g :context draw (draw/merge (draw/circle [1 2 0] 3)
    (let* [p (map (fn [i] [i 0 0]) (array/range 2048)) r (map (fn [i] (* i 0.5)) (array/range 2048))]
      (draw/circles p :radius r :stroke \"#fff\")))))" in
  let evaluated = ok (E.static ~record:true ws) in
  let catalog = Flow.Check.{version = 1; kinds = []} in
  let optimized = ok (optimize (of_evaluation ws catalog evaluated)) in
  let sets = Array.to_list optimized.nodes |> List.filter (fun n -> match n.kind with Marks _ -> true | _ -> false) in
  (match sets with
   | [n] -> (match n.kind with
       | Marks m -> assert (m.set = "draw/circles");
           assert (List.map (fun a -> a.role, a.edge) m.attributes
             = [Position, "positions"; Size, "radius"; Stroke, "stroke"])
       | _ -> assert false);
       assert (List.length (attribute_edges optimized n) = 3)
   | _ -> failwith "expected one mark set");
  assert (Array.exists (fun n -> n.kind = Sink (Display "draw/circle")) optimized.nodes)

(* P4.7 and P4.8 helpers: a circles draw graph whose attributes are packed maps. *)
let circles_graph ?(count = 2048) ~positions ~fills () =
  let ws = check (Printf.sprintf "(workspace w (graph g :context draw
    (let* [p (map (fn [i] %s) (array/range %d)) c (map (fn [i] %s) (array/range %d))]
      (draw/circles p :fill c))))" positions count fills count) in
  let evaluated = ok (E.static ~record:true ws) in
  let record name = List.assoc ["g"; name] evaluated.records |> List.hd |> snd in
  ws, evaluated, record "p", record "c"
let marks_node (g : t) = Array.to_list g.nodes |> List.find (fun n -> match n.kind with Marks _ -> true | _ -> false)
let catalog0 = Flow.Check.{version = 1; kinds = []}
let packed_of (g : t) i = match g.nodes.(i).kind with
  | Kernel {body = Packed_map p; _} -> p | _ -> failwith "not a region"

let () =
  let moving = "[(* i t) (sin (+ i t)) 0]" and colour = "[(sin t) (/ i 2048) (cos (* i t))]" in
  let ws, evaluated, p, c = circles_graph ~positions:moving ~fills:colour () in
  let graph = ok (optimize (of_evaluation ws catalog0 evaluated)) in
  let set = marks_node graph in
  let edges = attribute_edges graph set in
  (match edges with
   | [(a, i); (b, j)] ->
       assert (i = j && a.output = 0 && b.output = 1 && a.role = Position && b.role = Fill);
       let fused = packed_of graph i in
       assert (Packed.Private.((view fused).outputs |> Array.length) = 2);
       assert (graph.nodes.(i).rate = Frame && set.tier = Cpu_kernel);
       (* The fused result is the separate kernels' result byte for byte, at 1 and 8 domains. *)
       List.iter (fun time -> List.iter (fun domains ->
         let live = Frame_input.at_time time in
         Rays_math.Parallel.run ~domains (fun () ->
           match ok (Packed.force_all fused ~live) with
           | [fp; fc] ->
               assert (same_result (Ok fp) (Executor.force (ok (Executor.compile p)) ~live));
               assert (same_result (Ok fc) (Executor.force (ok (Executor.compile c)) ~live))
           | _ -> assert false)) [1; 8]) [0.; 0.5; 3.25]
   | _ -> failwith "expected the two attributes to share one fused node");
  (* Different counts do not fuse. *)
  let ws = check "(workspace w (graph g :context draw
    (let* [p (map (fn [i] [i 0 0]) (array/range 2048)) c (map (fn [i] [i 0 0]) (array/range 2049))]
      (draw/circles p :fill c))))" in
  let graph = ok (optimize (of_evaluation ws catalog0 (ok (E.static ~record:true ws)))) in
  (match attribute_edges graph (marks_node graph) with
   | [(_, i); (_, j)] -> assert (i <> j)
   | _ -> assert false);
  (* A static region and a frame-rate region do not fuse: the static one must not rerun. *)
  let ws, evaluated, _, _ = circles_graph ~positions:"[i 0 0]" ~fills:colour () in
  let graph = ok (optimize (of_evaluation ws catalog0 evaluated)) in
  (match attribute_edges graph (marks_node graph) with
   | [(_, i); (_, j)] -> assert (i <> j && graph.nodes.(i).rate = Static && graph.nodes.(j).rate = Frame)
   | _ -> assert false);
  (* The register limit declines and never fails. *)
  let sum salt = List.fold_left (fun acc k -> Printf.sprintf "(+ %s (sin (* i %d.%d)))" acc (k + 1) salt)
    "(sin t)" (List.init (Flow.Packed_ops.register_limit / 5) Fun.id) in
  let ws, evaluated, _, _ = circles_graph ~positions:("[" ^ sum 1 ^ " 0 0]") ~fills:("[" ^ sum 7 ^ " 0 0]") () in
  let graph = ok (optimize (of_evaluation ws catalog0 evaluated)) in
  (match attribute_edges graph (marks_node graph) with
   | [(a, i); (b, j)] ->
       assert (i <> j && a.output = 0 && b.output = 0);
       assert (Array.length Packed.Private.((view (packed_of graph i)).code) < Flow.Packed_ops.register_limit)
   | _ -> assert false);
  print_endline "Attribute fusion: one region with several outputs, byte-equal at 1 and 8 domains; declines on count, rate and the register limit"

(* P4.8: static positions with animated colour run the position region once over ten frames. *)
let () =
  let run_frames positions =
    let ticks = ref 0 in
    let profile = Profile.create ~clock:(fun () -> incr ticks; float (!ticks * !ticks)) in
    let ws, evaluated, p, c = circles_graph ~count:4096 ~positions ~fills:"[(sin t) (/ i 4096) (cos (* i t))]" () in
    ignore ws;
    let marks = ok (Executor.compile_marks ~profile "draw/circles" ["positions", p; "fill", c]) in
    ignore evaluated;
    let graph = Executor.marks_graph marks in
    let rates = List.map (fun (_, i) -> graph.nodes.(i).rate) (attribute_edges graph (marks_node graph)) in
    let seen = Hashtbl.create 4 in
    let state = E.create_state () in
    for frame = 0 to 9 do
      let live = Frame_input.at_time (float frame *. 0.25) in
      (match ok (Executor.force_marks ~state marks ~live) with
       | ["positions", pv; "fill", cv] ->
           assert (same_result (Ok pv) (E.Private.force_reference p ~live));
           assert (same_result (Ok cv) (E.Private.force_reference c ~live))
       | _ -> assert false);
      List.iter (fun (e : execution) ->
        let known = Option.value ~default:[] (Hashtbl.find_opt seen e.owner) in
        if not (List.mem e.seconds known) then Hashtbl.replace seen e.owner (e.seconds :: known))
        (Profile.executions profile)
    done;
    rates, Hashtbl.fold (fun _ runs acc -> List.length runs :: acc) seen [] |> List.sort compare in
  (* Static positions: one run for the position region, ten for the colours. *)
  assert (run_frames "[i 0 0]" = ([Static; Frame], [1; 10]));
  (* Both animated and fused: one run per frame for the pair. *)
  assert (snd (run_frames "[(* i t) 0 0]") = [10]);
  print_endline "Rate split: a static attribute runs once over ten frames, a frame-rate one every frame"

(* P4.12: placement against a target; mark sets are placed like kernels. *)
let () =
  let ws = check "(workspace gpu (graph g :context value (let* [mapped (map (fn [x] (+ x t)) (array/range 1024))] 0.0)))" in
  let ws, evaluation = ok (qualify_workspace ~record:true ws) in
  let value = List.assoc ["g"; "mapped"] evaluation.records |> List.hd |> snd in
  let program = ok (Executor.compile ~approx:ws.approx value) in
  let packed = Array.find_map (function {kind = Kernel {body = Packed_map packed; _}; _} -> Some packed | _ -> None)
    (Executor.graph program).nodes |> Option.get in
  let region name count = node ~ty:(Flow.Ty.Array Flow.Ty.Vec3) ~count:(Count.Static count) name
    (Kernel {body = Packed_map packed; elementwise = true; requires_exact = true}) in
  let approximable = Flow.Workspace.Paths.of_list [["big"]; ["small"]; ["other"]] in
  let gpu_cost _ ~count:_ = Some 0. in
  let set args = node ~args "circles" (Marks {set = "draw/circles"; attributes = [
    {role = Position; edge = "positions"; output = 0}; {role = Fill; edge = "fill"; output = 0}]}) in
  let tiers ?target nodes root =
    (ok (place ~approx:approximable ~gpu_cost ?target (ir nodes [root]))).nodes |> Array.map (fun (n : node) -> n.tier) in
  (* Native: a mark set is on the GPU when its regions are, wherever a constant stands in. *)
  let colour = constant "colour" 1. in
  assert (tiers [region "big" 1024; colour; set [edge "positions" 0; edge "fill" 1]] 2 = [|Gpu; Interp; Gpu|]);
  assert ((tiers [region "big" 1024; region "other" 1024; set [edge "positions" 0; edge "fill" 1]] 2).(2) = Gpu);
  (* A CPU region keeps the set on the CPU; the GPU region stays (native readback is legal). *)
  let mixed = [region "big" 1024; region "small" 16; set [edge "positions" 0; edge "fill" 1]] in
  let native_mixed = tiers mixed 2 in
  assert (native_mixed.(0) = Gpu && native_mixed.(1) <> Gpu && native_mixed.(2) = Cpu_kernel);
  assert ((tiers [colour; node ~args:[edge "positions" 0; edge "fill" 0] "circles"
    (Marks {set = "draw/circles"; attributes = [{role = Position; edge = "positions"; output = 0}]})] 1).(1) = Interp);
  (* A target that forbids readback: the region that would have to come back stays on the CPU. *)
  let no_readback = {native with legal = (fun n tier -> tier <> Gpu_readback && native.legal n tier)} in
  let mixed_no_readback = tiers ~target:no_readback mixed 2 in
  assert (mixed_no_readback.(0) <> Gpu && mixed_no_readback.(2) = Cpu_kernel);
  assert ((tiers ~target:no_readback [region "big" 1024; region "other" 1024;
    set [edge "positions" 0; edge "fill" 1]] 2).(2) = Gpu);
  let exact = node "exact" ~args:[edge "value" 0] (Kernel {body = Readback; elementwise = true; requires_exact = false}) in
  let sop = node "sop" ~args:[edge "value" 1] (Sink Sop_input) in
  assert (tiers [region "big" 1024; exact; sop] 2 = [|Gpu; Gpu_readback; Interp|]);
  let local = tiers ~target:no_readback [region "big" 1024; exact; sop] 2 in
  assert (local.(0) = Cpu_kernel && local.(1) <> Gpu_readback);
  (* A target that forbids the GPU outright: everything runs on the CPU. *)
  let cpu_only = {native with legal = (fun n tier -> tier <> Gpu && tier <> Gpu_readback && native.legal n tier)} in
  assert (Array.for_all (fun tier -> tier <> Gpu) (tiers ~target:cpu_only mixed 2));
  (* A dearer CPU makes the GPU win where the native table would not. *)
  let slow_cpu = {native with cost = (fun tier ~count -> native.cost tier ~count *.
    (match tier with Interp | Cpu_kernel -> 1e6 | _ -> 1.))} in
  assert ((ok (place ~approx:approximable ~gpu_cost:(fun _ ~count:_ -> Some 0.01)
    ~target:slow_cpu (ir [region "big" 1024] [0]))).nodes.(0).tier = Gpu);
  assert ((ok (place ~approx:approximable ~gpu_cost:(fun _ ~count:_ -> Some 0.01)
    (ir [region "big" 1024] [0]))).nodes.(0).tier = Cpu_kernel);
  print_endline "Placement targets: mark sets placed like kernels; no-readback and CPU-only targets; custom cost table"
