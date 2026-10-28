open Flow_ir
module E = Flow.Eval

let () =
  let module W = Flow.Workspace in
  let check source = match W.check Flow.Catalog.{version=1;kinds=[]}
      (Flow.Syntax.parse source |> Result.get_ok) with
    | Some workspace, [] -> workspace
    | _, ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds)) in
  let qualify ?inputs ?(gpu=Gpu.default) workspace =
    let observed = ref [] in
    let qualified,evaluation = qualify_workspace ~gpu ~record:true ?inputs
      ~observe:(fun producer residual ->
        let view=E.Residual.residual_view residual in
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
    if not accepted then assert (List.exists (fun (d:Diagnostic.t)->d.code=code)
      (List.assoc path qualified.approx_reasons)))
    ["[uv.x uv.y t 1]",true,"";
     "[(floor 2.5) uv.y t 1]",true,"";
     "[(floor uv.x) uv.y t 1]",false,"E_GPU_FORM";
     "[(+ uv.x 1e39) uv.y t 1]",false,"E_GPU_FORM"];
  let fast_gpu={Gpu.default with fast=true} in
    List.iter(fun body->
      let qualified,_,_=qualify ~gpu:fast_gpu(check(image body))in
      if not(W.Paths.mem path qualified.approx)then
        failwith(String.concat "\n"(List.map Diagnostic.to_string(List.assoc path qualified.approx_reasons))))
      ["[(fold [a uv.x] [i (range 3)] (+ a 0.25)) uv.y t 1]";
       "[(iterate 4611686018427387903 uv.x (fn [a] (+ a 1.))
          :until (fn [prev next] (>= next (+ uv.x 3.)))) uv.y t 1]" ];
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
    if not accepted then assert(List.exists(fun(d:Diagnostic.t)->d.code="E_GPU_FORM")
      (List.assoc path qualified.approx_reasons))) [1e39,7.,false;7.,1e39,false;2.,7.,true];
  let state=check "(workspace pixels (graph img :context image
    (let* [bias (state [n 0.0] (+ n 0.1)) picture (image/map (fn [uv] [uv.x uv.y bias 1]))] picture)))" in
  let qualified,_,_=qualify state in
  assert (not(W.Paths.mem path qualified.approx));
  assert(List.exists(fun(d:Diagnostic.t)->d.code="E_PACKED_STATE")
    (List.assoc path qualified.approx_reasons));
  let counterfeit=Option.get(Flow.Op.find "image/map" Flow.Context.image) in
  let counterfeit={counterfeit with category="Counterfeit image map"} in
  let qualified,_,_=qualify {shared with ops=counterfeit::shared.ops} in
  assert(not(W.Paths.mem ["img";"a"] qualified.approx));
  assert(List.exists(fun(d:Diagnostic.t)->d.code="E_GPU_FORM")
    (List.assoc ["img";"a"] qualified.approx_reasons));
  print_endline "image qualification: authored sites, shared functions, folded/unsupported forms, capture requalification, state and declaration identity pass"

let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)

let check ?(ops = []) ?(catalog = {Flow.Catalog.version = 1; kinds = []}) text =
  let forms = ok (Flow.Syntax.parse text) in
  match Flow.Workspace.check ~ops catalog forms with
  | Some w, ds when not (List.exists (fun (d : Diagnostic.t) -> d.severity = Diagnostic.Error_severity) ds) -> w
  | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
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
  | Error a, Error b -> Diagnostic.to_string a = Diagnostic.to_string b
  | _ -> false
let rec has_region = function
  | E.Residual residual ->
      let view = E.Residual.residual_view residual in
      (match Core_lower.lower residual with Ok core -> Array.length core.Core.regions > 0 | Error _ -> false)
      || packed_child_region residual view.term
      || has_fields view.bindings
  | E.List values -> Array.exists has_region values
  | E.Record fields | E.Struct (_, _, fields) | E.Source (_, _, fields) -> has_fields fields
  | E.Fn fn -> has_fields (E.Residual.function_bindings fn)
  | _ -> false
and packed_child_region residual term =
  Option.is_some (Packed.compile residual term)
  || List.exists (packed_child_region residual) (Flow.Workspace.children term)
and has_fields = function
  | [] -> false
  | (_, value) :: rest -> has_region value || has_fields rest
let () =
  let live = Input.at_time 0.125 in
  List.iter (fun count ->
    let value = value (Printf.sprintf "(array/sum (map (fn [x] (sin (+ x t))) (array/range %d)))" count) in
    assert (same_result (Executor.force (ok (Executor.compile value)) ~live)
      (E.Private.force_reference value ~live))) [0;16;33;34;512;1024]

let () =
  let profile = Profile.create ~clock:(fun () -> 1.) in
  for i = 0 to 512 do
    let ws = check (Printf.sprintf "(workspace w (graph g%d :context value (+ t 1)))" i) in
    let evaluated = ok (E.static ws) in
    let program = ok (Executor.compile ~profile (snd (List.hd evaluated.results))) in
    ignore (ok (Executor.force program ~live:(Input.at_time 0.)))
  done;
  let reports = Profile.executions profile in
  assert (List.length reports = 512);
  assert (List.for_all (fun report -> List.for_all (fun (instance,_,_) -> instance = 0) report.sites) reports);
  assert ((List.hd reports).seconds = 0. && (List.hd reports).tier = Core);
  List.iter (fun body ->
    let v = value body in
    let p = ok (Executor.compile v) in
    List.iter (fun time ->
      let live = Input.at_time time in
      let reference = E.Private.force_reference v ~live in
      List.iter (fun domains -> Parallel.run ~domains (fun () ->
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
  let bad_live = {(Input.at_time 0.) with dt = nan} in
  let v = value "(+ t 1)" in
  assert (same_result (Executor.force (ok (Executor.compile v)) ~live:bad_live)
    (E.Private.force_reference v ~live:bad_live));
  let v = value "(let* [a (state [p 0.0] (+ p (frame/dt))) b (* a 2)] (+ a b))" in
  let p = ok (Executor.compile v) in
  let optimized = E.create_state () and reference = E.create_state () in
  List.iter (fun frame ->
    let live = {(Input.at_time (float frame)) with frame; dt = 0.125} in
    assert (same_result (Executor.force ~state:optimized p ~live)
      (E.Private.force_reference ~state:reference v ~live))) [0; 1; 2; 2; 3; 1];
  assert (List.length (E.Private.state_values optimized)=List.length (E.Private.state_values reference)
    && Array.for_all2 same_bits (Array.of_list(E.Private.state_values optimized))
      (Array.of_list(E.Private.state_values reference)))

let () =
  List.iter (fun body ->
    let v = value body in
    let program = ok (Executor.compile v) in
    if not (has_region v) then failwith ("packed Core region missing for: " ^ body);
    List.iter (fun time ->
      let live = Input.at_time time in
      let reference = E.Private.force_reference v ~live in
      let sequential = Parallel.run ~domains:1 (fun () -> Executor.force program ~live) in
      let parallel = Parallel.run ~domains:8 (fun () -> Executor.force program ~live) in
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
    let live = Input.at_time time in
    assert (same_result (Executor.force program ~live) (E.Private.force_reference v ~live))) [0.; 0.125; 7.];
  let v = value "(array/sum (map (fn [x] (+ x t)) (array/float 0)))" in
  assert (Executor.force (ok (Executor.compile v)) ~live:(Input.at_time 1.) = Ok (E.Float 0.));
  let v = value "(array/sum (map (fn [x] (+ x t)) (array/range (frame/index))))" in
  let program = ok (Executor.compile v) in
  List.iter (fun count ->
    let live = {(Input.at_time 1.25) with frame = count} in
    assert (same_result (Executor.force program ~live) (E.Private.force_reference v ~live))) [0; 1023; 1024; 2051]

let read path = In_channel.with_open_text path In_channel.input_all
let () =
  let ops = [] in
  let mapped ~ops body =
    let ws = check ~ops ("(workspace noise (graph g :context value (let* [mapped " ^ body ^ "] (array/sum mapped))))") in
    let evaluated = ok (E.static ~record:true ws) in
    List.assoc ["g"; "mapped"] evaluated.records |> List.hd |> snd in
  let body = "(map (fn [p] (exact (noise3 (+ p [t 0 0])))) (array/vec3 2051 [0.3 0.7 -0.2]))" in
  let v = mapped ~ops body in
  let program = ok (Executor.compile v) in
  assert (Option.is_some (Executor.packed_region program));
  List.iter (fun time ->
    let live = Input.at_time time in
    let reference = ok (E.Private.force_reference v ~live) in
    List.iter (fun domains -> Parallel.run ~domains (fun () ->
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
  let live = Input.at_time 7. in
  assert (same_result (Executor.force program ~live) (E.Private.force_reference v ~live));
  let custom = {Flow.Op.noise3 with name = "toy_noise3"; packed_extension = None;
    body = (fun ~live:_ ~node:_ _ -> E.Float 0.125)} in
  let v = mapped ~ops:[custom] "(map (fn [p] (toy_noise3 p)) (array/vec3 2051 [1 2 3]))" in
  let program = ok (Executor.compile v) in
  assert (Executor.packed_region program = None);
  assert (same_result (Executor.force program ~live) (E.Private.force_reference v ~live));
  match ok (Executor.force program ~live) with
  | E.Float_array xs -> assert (Array.for_all (( = ) 0.125) xs)
  | _ -> assert false

let () =
  let ops=[] in
  List.iter(fun seed->List.iter(fun octaves->
    let ws=check ~ops (Printf.sprintf "(workspace n (graph g :context value (let* [mapped (map (fn [p] (noise3 (+ p [t 0 0]) :seed %d :octaves %d)) (array/vec3 2051 [0.31 0.73 -0.29]))] (array/sum mapped))))" seed octaves)in
    let evaluated=ok(E.static ~record:true ws)in
    let value=List.assoc ["g";"mapped"] evaluated.records |> List.hd |> snd in
    let program=ok(Executor.compile value)in
    assert(Option.is_some(Executor.packed_region program));
    List.iter(fun time->let live=Input.at_time time in
      let reference=ok(E.Private.force_reference value ~live)in
      List.iter(fun domains->let actual=Parallel.run ~domains(fun()->ok(Executor.force program ~live))in
        assert(same_bits actual reference)) [1;8]) [0.;0.125;1.25;7.]) [1;5;32]) [0;2026;-451];
  List.iter(fun expression->let ws=check ~ops ("(workspace c (graph g :context value (let* [result "^expression^"] 0)))")in
    let evaluated=ok(E.static ws)in
    ignore(ok(Executor.force (ok(Executor.compile(List.assoc "g" evaluated.results))) ~live:(Input.at_time 0.))))
    ["(color/hsl 220 0.7 0.4)";"(color/hsla 451 0.5 0.6 0.25)";"(color/gradient (list \"#101426\" \"#f26444\") 0.3)"]

let () =
  (* Compare all million elements with the independent tree walker. *)
  let ws = check "(workspace large (graph g :context value (let* [xs (array/range 1000000) mapped (map (fn [x] (sin (+ (* x 0.25) t))) xs)] (array/sum mapped))))" in
  let evaluated = ok (E.static ~record:true ws) in
  let v = List.assoc ["g"; "mapped"] evaluated.records |> List.hd |> snd in
  let program = ok (Executor.compile v) in
  let live = Input.at_time 1.25 in
  let one = Parallel.run ~domains:1 (fun () -> ok (Executor.force program ~live)) in
  let eight = Parallel.run ~domains:8 (fun () -> ok (Executor.force program ~live)) in
  let reference = ok (E.Private.force_reference v ~live) in
  let reference_eight = Parallel.run ~domains:8 (fun () -> ok (E.Private.force_reference v ~live)) in
  assert (same_bits one reference);
  assert (same_bits eight reference_eight);
  assert (same_bits one eight);
  (match one with
   | E.Float_array xs -> assert (Array.length xs = 1_000_000);
       Array.iteri (fun i x -> assert (Int64.bits_of_float x = Int64.bits_of_float (sin (float i *. 0.25 +. 1.25)))) xs
   | _ -> failwith "CPU map did not return a packed array")
let () =
  let catalog, _ = ok (Flow.Catalog.catalog_of_manifest (read "../flow_sop/flow_manifest.sexp")) in
  List.iter (fun name ->
    let ws = check ~catalog (read ("../../specification/workspace/cases/" ^ name ^ ".lisp")) in
    let evaluated = ok (E.static ~record:true ws) in
    let values = List.map snd evaluated.results @ evaluated.states
      @ Array.fold_right (fun (n : E.node) acc -> List.map snd n.args @ acc) evaluated.plan.nodes []
      @ Array.fold_right (fun (i : E.instance) acc -> i.result :: List.map snd i.inputs @ acc) evaluated.plan.instances []
      @ List.concat_map (fun (_, records) -> List.map snd records) evaluated.records in
    let prepared = List.map (fun v -> v, ok (Executor.compile v)) values in
    List.iter (fun time ->
      let live = Input.at_time time in
      List.iter (fun domains -> Parallel.run ~domains (fun () ->
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
  (* This backend exercises callback ownership/selection only; it does not claim
     GPU execution or numerical conformance. Native tests run the emitted shader. *)
  let prepared=ref 0 and dispatched=ref 0 and gpu_seconds=ref(Some 0.002)in
  let backend:Gpu.backend={cost=(fun ~fast:_ _ ~count:_->None);prepare=(fun ~fast:_ _->incr prepared;
    Ok{run=(fun inputs->incr dispatched;Ok Gpu.{identity=1;output=0;count=inputs.count;width=1;stamp=Int64.of_int !dispatched;
      gpu_seconds= !gpu_seconds});
      readback=(fun _->Ok(E.Float_array[||]))})}in
  let overflow_ws = check "(workspace gpu (graph g :context value (let* [mapped
      (map (fn [x] (+ x (+ t 1e39))) (array/range 1024))] 0.0)))" in
  assert (List.mem_assoc ["g";"mapped"] overflow_ws.packed);
  let overflow_ws, overflow_evaluation = ok (qualify_workspace ~record:true overflow_ws) in
  assert (not (Flow.Workspace.Paths.mem ["g";"mapped"] overflow_ws.approx));
  assert (List.exists (fun (d : Diagnostic.t) -> d.code = "E_GPU_FORM")
    (List.assoc ["g";"mapped"] overflow_ws.approx_reasons));
  let overflow_value = List.assoc ["g";"mapped"] overflow_evaluation.records |> List.hd |> snd in
  let overflow_program = ok (Executor.compile ~approx:overflow_ws.approx
      ~sink:(Display "circles") overflow_value) in
  let overflow_packed = Option.get (Executor.packed_region overflow_program) in
  assert (Packed.gpu_refusals ~fused:true overflow_packed <> []);
  let qualification_gpu={Gpu.default with backend=Some backend;policy=Gpu.Qualification} in
  let measured_gpu={Gpu.default with backend=Some backend} in
    assert (ok (Executor.try_display ~gpu:qualification_gpu overflow_program
      ~live:(Input.at_time 1.)) = None);
    assert (!prepared = 0 && !dispatched = 0);
    assert(match ok(Executor.force_display program ~live:(Input.at_time 1.))with Executor.Cpu _->true|_->false);
    assert(!prepared=0 && !dispatched=0);
    for i=1 to 2 do assert(match ok(Executor.force_display ~gpu:qualification_gpu program
      ~live:(Input.at_time(float i)))with Executor.Gpu value->value.count=1024|_->false)done;
    assert(!prepared=1 && !dispatched=2);
    assert(List.exists(fun report->report.tier=Gpu && report.seconds=0.002)(Profile.executions profile));
    List.iter(fun seconds->gpu_seconds:=seconds;
      ignore(ok(Executor.force_display ~gpu:qualification_gpu program ~live:(Input.at_time 3.)));
      assert((List.find(fun report->report.tier=Gpu)(Profile.executions profile)).seconds=0.))
      [None;Some nan;Some(-1.)];
    assert(match ok(Executor.force_display ~reference:true ~gpu:qualification_gpu program
      ~live:(Input.at_time 1.))with Executor.Cpu _->true|_->false);
    (* The SOP input sink is an implicit readback; a CPU or export sink still refuses a GPU array. *)
    let sop_input=ok(Executor.compile ~approx:ws.approx ~sink:Sop_input value)in
    assert(match Executor.force_display ~gpu:qualification_gpu sop_input ~live:(Input.at_time 1.)with
      |Ok(Executor.Cpu _)->true|_->false);
    let illegal=ok(Executor.compile ~approx:ws.approx value)in
    assert(match Executor.force_display ~gpu:qualification_gpu illegal ~live:(Input.at_time 1.)with
      |Error d->d.Diagnostic.code="E_APPROX_SINK"|Ok _->false);
  assert(List.exists(fun report->report.tier=Gpu_compile)(Profile.executions profile));
  assert(List.exists(fun report->report.tier=Gpu)(Profile.executions profile));
  assert(match ok(Executor.force_display program ~live:(Input.at_time 1.))with
    Executor.Cpu _->true|_->false);
  (* Measured policy: a finite GPU estimate plus its display sink competes with
     the CPU kernel plus instance building; an expensive estimate keeps CPU. *)
  List.iter(fun(seconds,expect_gpu)->
    let gpu={measured_gpu with backend=Some {backend with cost=(fun ~fast:_ _ ~count:_->Some seconds)}} in
      assert(match ok(Executor.force_display ~gpu program ~live:(Input.at_time 1.))with
        |Executor.Gpu _->expect_gpu|Executor.Cpu _->not expect_gpu))
    [0.,true;Cost.estimate Cpu_kernel ~count:1024,true;1.,false];
  let fast={qualification_gpu with fast=true} and exact=qualification_gpu in
  ignore(ok(Executor.force_display ~gpu:fast program ~live:(Input.at_time 2.)));
  let before_precision_switch= !prepared in
  ignore(ok(Executor.force_display ~gpu:exact program ~live:(Input.at_time 2.)));
  assert(!prepared=before_precision_switch+1);
  print_endline "GPU neutral callbacks: measured placement, illegal exact sinks, qualification scope and phase profiles passed"

let () =
  let module W = Flow.Workspace in
  let path = ["g";"mapped"] in
  let text body = "(workspace qualification (graph g :context host " ^ body ^ "))" in
  let mapped body = text ("(let* [mapped " ^ body ^ " alias mapped] alias)") in
  let fingerprint (evaluation : E.t) =
    let key = Flow.Value.key_of ~residual:E.Residual.residual_id in
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
    List.iter (fun domains -> Parallel.run ~domains (fun () ->
      List.iter (fun time ->
        let live = Input.at_time time in
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
          Diagnostic.to_string d) ds) qualified.approx_reasons));
    if not accepted then begin
      let reasons=List.assoc path qualified.approx_reasons in
      if not(List.exists (fun (d : Diagnostic.t) -> d.code = code) reasons) then
        failwith (Printf.sprintf "qualification for %s expected %s, got %s" source code
          (String.concat "; " (List.map Diagnostic.to_string reasons)))
    end;
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
  ignore (expect ~ops:[]
    (mapped "(map (fn [p] (noise3 p :seed 7 :octaves 3)) (array/vec3 4))") true "");
  ignore (expect ~ops:[]
    (mapped "(map (fn [p] (noise3 p :octaves 99)) (array/vec3 0))") false "E_PACKED_CONSTANT");
  let unsupported = {Flow.Op.noise3 with name = "toy_noise3"; packed_extension = None} in
  ignore (expect ~ops:[unsupported] (mapped "(map (fn [x] (toy_noise3 [x t 0])) (array/float 4))")
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
    if not accepted then assert (List.exists (fun (d : Diagnostic.t) -> d.code = "E_GPU_FORM")
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
  assert (List.exists (fun (d : Diagnostic.t) -> d.code = "E_PACKED_PENDING")
    (List.assoc path qualified.approx_reasons));
  let boundary = check (text "(let* [blocked (map (fn [x] (floor (+ x t))) (array/float 4))
    mapped (map (fn [x] (+ x t)) blocked)] 0.0)") in
  let seen = ref false in
  let qualified,_ = ok (qualify_workspace ~observe:(fun producer residual ->
    if producer = Some path then begin
      seen := true;
      List.iter (fun fusion -> match Packed.compile_result ~fusion residual (E.Residual.residual_view residual).term with
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
    assert (List.exists (fun (d : Diagnostic.t) -> d.code = "E_PACKED_PENDING")
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
  let checked_forms forms = match W.check {Flow.Catalog.version = 1; kinds = []} forms with
    | Some ws,_ -> ws | _,ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds)) in
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
