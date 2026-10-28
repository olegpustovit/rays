module E=Flow.Eval
module P=Flow_ir.Packed
let ok=function Ok value->value|Error d->failwith(Diagnostic.to_string d)
let bytes value=Marshal.to_string value [Marshal.No_sharing]
let same a b=match a,b with
  |Ok a,Ok b->bytes a=bytes b
  |Error a,Error b->Diagnostic.to_string a=Diagnostic.to_string b
  |_->false
let components=function E.Float x->[|x|]|Vec2(x,y)->[|x;y|]
  |Vec3(x,y,z)->[|x;y;z|]|Vec4(x,y,z,w)->[|x;y;z;w|]|_->assert false
let width=function Flow.Ty.Float->1|Vec2->2|Vec3->3|Vec4->4|_->assert false
let source ty : Flow.Op.t={Flow.Op.noise3 with
  name="probe/data";packed_extension=None;
  signature={pos=[];opt=[];rest=None;kw=[]};out=(fun _->Flow.Ty.Array ty);
  shape=Flow.Op.Struct{splice=false};check=(fun _->());
  body=(fun ~live:_ ~node:_ _->Flow.Value.Struct("probe/data",Flow.Ty.Array ty,[]))}
let workspace ?(bindings="") ops body=
  let forms=ok(Flow.Syntax.parse("(workspace w (graph g :context value (let* ["^bindings^
    " tested "^body^"] 0.0)))"))in
  match Flow.Workspace.check ~ops {Flow.Catalog.version=1;kinds=[]} forms with
  |Some workspace,[]->workspace
  |_,ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds))
let evaluate workspace=List.assoc ["g";"tested"](ok(E.static ~record:true workspace)).records
  |> List.hd |> snd
let fixture ?override ?seed ty reducer=
  let width=width ty in
  let seed=Option.value ~default:(if width=1 then "-0.0" else
    "["^String.concat " "(List.init width(fun _->"-0.0"))^"]") seed in
  let body=match width with 1->"[s uv.x uv.y t]"|2->"[s.x s.y uv.x t]"
    |3->"[s.x s.y s.z t]"|_->"[s.x s.y s.z s.w]"in
  let workspace=workspace ~bindings:("data (probe/data) s (reduce "^reducer^" "^seed^" data)")
    [source ty]("(fn [(uv : vec2)] "^body^")")in
  (* Exercise the compiler's independent declaration guard even for an unchecked extension. *)
  let workspace=Option.fold ~none:workspace ~some:(fun op->{workspace with ops=op::workspace.ops})override in
  let fn=match evaluate workspace with E.Fn fn->fn|_->assert false in
  let sum=List.assoc "s"(E.Residual.function_bindings fn)in
  let uv=E.Vec2_array(Array.init(32769*2)(fun i->float(i mod 17)/.16.))in
  let value=ok(E.Host.map_function ~signature:Flow.Ty.{params=[Vec2];result=Vec4} fn [uv])in
  let packed=match value with E.Residual r->
    (match P.compile_result r(E.Residual.residual_view r).term with
      |Ok packed->packed|Error ds->failwith(Printf.sprintf "uniform compile %s reducer=%s seed=%s: %s"
          (Flow.Ty.to_string ty)reducer seed (String.concat "; "(List.map Diagnostic.to_string ds))))
    |_->assert false in
  value,sum,packed
let observed ?fast counter=Some((fun()->0.),(fun packed ~seconds:_ ~reference->
  if not reference && not(P.Private.view packed).collecting then begin
    incr counter;Option.iter(fun expected->if P.Private.ordered_add packed<>expected then
      failwith(Printf.sprintf "uniform ordered-add expected=%b actual=%b result=%s body=%s"
        expected(P.Private.ordered_add packed)
        (Flow.Ty.to_string packed.term.ty)
        (Flow.Ty.to_string packed.body.ty)))fast
  end))
let cancellation i=match i mod 5 with 0->1e16|1->1.|2-> -1e16|3->1.|_-> -0.
let data ty count time=
  let width=width ty in
  let values=Array.init(count*width)(fun i->if width=1 then
    (if i=count-1 then time else cancellation i)
    else match i mod width with
    |0->cancellation(i/width)|1->time|2-> -0.|_->cancellation(i/width)+.time)in
  match width with 1->E.Float_array values|2->Vec2_array values
    |3->Vec3_array values|_->Vec4_array values
let resolve_source current=function
  |E.Struct("probe/data",_,_)->Ok !current
  |_->Error(Diagnostic.error ~code:"E_DATA_SOURCE" "Unexpected source.")
let () =
  let fn = match evaluate (workspace []
    "(fn [(x : float)] [(* (- x 6.0) 50.0) t 0.0])") with
    | E.Fn fn -> fn | _ -> assert false in
  let call = ok (E.Host.map_function ~signature:Flow.Ty.{params=[Float];result=Vec3}
    fn [E.Float_array (Array.init 13 float)]) in
  (* Recorded map calls are nested in this record. Tiny maps use the ordinary
     interpreter placement; strict numeric bodies still require packed execution. *)
  let value = E.Record ["offset", E.Int 0; "call", call] in
  List.iter (fun strict ->
    let program = ok (Flow_ir.Executor.compile ~strict value) in
    List.iter (fun time ->
      let live = Input.at_time time in
      let expected = E.Host.force_reference value ~live in
      List.iter (fun domains -> Parallel.run ~domains (fun () ->
        let actual = Flow_ir.Executor.force ~strict program ~live in
        if not (same actual expected) then failwith (Printf.sprintf
          "recorded map mismatch: strict=%b t=%g domains=%d expected=%s actual=%s"
          strict time domains
          (match expected with Ok value -> E.show value | Error d -> Diagnostic.to_string d)
          (match actual with Ok value -> E.show value | Error d -> Diagnostic.to_string d)))) [1;8]) [0.;0.25]) [false;true];
  print_endline "Tiny maps in recorded-call records preserve reference results and strict packed execution at one/eight domains"; flush stdout
let () =
  let fn = match evaluate (workspace [] "(fn [(x : float)] [x t 0.0])") with
    | E.Fn fn -> fn | _ -> assert false in
  let residuals = Array.init 129 (fun i ->
    ok (E.Host.map_function ~signature:Flow.Ty.{params=[Float];result=Vec3} fn
      [E.Float_array [|float i|]])) in
  let value = E.List (Array.append residuals [|residuals.(0)|]) in
  let program = ok (Flow_ir.Executor.compile value) in
  let live = Input.at_time 0.25 in
  let actual, attempts = Flow_ir.Executor.Private.force_with_lower_attempts program ~live in
  assert (same actual (E.Host.force_reference value ~live));
  assert (attempts = 129);
  print_endline "One force lowers 129 distinct residuals once and reuses the first shared value"; flush stdout
let () =
  List.iter (fun reducer -> List.iter (fun wrapped -> List.iter (fun integer ->
    let reduction = "(reduce " ^ reducer ^ " 0.0 data)" in
    let uniform = if wrapped then "(+ t " ^ reduction ^ ")" else reduction in
    let body = if integer then "(int (+ x u))" else "(+ x u)" in
    let fn = match evaluate (workspace ~bindings:("data (probe/data) u " ^ uniform)
      [source Flow.Ty.Float] ("(fn [(x : float)] " ^ body ^ ")")) with
      | E.Fn fn -> fn | _ -> assert false in
    let value = ok (E.Host.map_function
      ~signature:Flow.Ty.{params=[Float];result=(if integer then Int else Float)} fn
      [E.Float_array (Array.init 32769 (fun i -> float (i mod 11)))]) in
    let packed = match value with E.Residual residual ->
      Option.get (P.compile residual (E.Residual.residual_view residual).term)
      | _ -> assert false in
    let current = ref (E.Float_array [|1.;2.;3.;4.|]) in
    let resolve = resolve_source current and live = Input.at_time 0.25 in
    let reference = E.Host.force_reference ~resolve value ~live in
    List.iter (fun domains -> Parallel.run ~domains (fun () ->
      let actual = P.force ~strict:true ~resolve packed ~live in
      let compiled = Flow_ir.Executor.compile ~strict:true value in
      if reducer = "+" then begin
        if not (same actual reference) then failwith (Printf.sprintf "strict uniform packed mismatch reducer=%s actual=%s expected=%s"
          reducer (match actual with Ok value -> E.show value | Error d -> Diagnostic.to_string d)
          (match reference with Ok value -> E.show value | Error d -> Diagnostic.to_string d));
        assert (same (Flow_ir.Executor.force ~strict:true ~resolve (ok compiled) ~live) reference)
      end else begin
        let refused = function Error (d : Diagnostic.t) -> assert (d.code="E_KERNEL_FORM")
          | Ok _ -> failwith "Strict captured reduction used the interpreter" in
        refused actual; refused compiled
      end)) [1;8]) [false;true]) [false;true]) ["+";"min"];
  print_endline "Strict uniforms: direct/wrapped unsupported reductions refuse; Float/Int outputs and supported add match reference at one/eight domains"; flush stdout
let ()=
  let addition=Option.get(Flow.Op.find "+" Flow.Context.value)in
  let custom={addition with arithmetic=None;
    body=(fun ~live:_ ~node:_ args->match List.map snd args with
      |[a;b]->Flow.Value.Float(Flow.Value.num a-.Flow.Value.num b)|_->assert false)}in
  List.iter(fun(ty,reducer,override,seed,packed_expected,fast)->
    let value,sum,packed=fixture ?override ?seed ty reducer in
    let counts=if ty=Flow.Ty.Vec3 && reducer="+"then
      [0;1;1023;1024;1025;16383;16384;16385;32769]else[0;32769]in
    List.iter(fun count->List.iter(fun time->
      let current=ref(data ty count time)in let original=bytes !current in
      let live={(Input.at_time time)with frame=int_of_float(time*.4.)}in
      let expected=E.Host.force_reference ~resolve:(resolve_source current) value ~live in
      let scalar=ok(E.Host.force_reference ~resolve:(resolve_source current) sum ~live)in
      if seed=Some "0" then (match count,scalar with
        |0,E.Int 0->()|n,E.Float _ when n>0->()|_->assert false);
      let uniform=match scalar with E.Int n->[|float n|]|_->components scalar in
      List.iter(fun domains->Parallel.run ~domains(fun()->
        let cpu=ref 0 and gpu=ref 0 in
        assert(same(P.force ~resolve:(resolve_source current) ?measure:(observed ~fast cpu) packed ~live)expected);
        let actual=Flow_ir.Executor.force ~resolve:(resolve_source current)
          (ok(Flow_ir.Executor.compile sum)) ~live in
        if not(same actual (Ok scalar))then
          failwith(Printf.sprintf "uniform executor %s reducer=%s seed=%s count=%d time=%g domains=%d expected=%s actual=%s"
            (Flow.Ty.to_string ty)reducer(Option.value ~default:"default" seed)count time domains
            (E.show scalar)(match actual with Ok value->E.show value|Error d->Diagnostic.to_string d));
        let prepared=ok(P.Private.prepare ~resolve:(resolve_source current) ?measure:(observed ~fast gpu) packed ~live)in
        assert(Array.exists(fun actual->bytes actual=bytes uniform)prepared.uniforms);
        if (!cpu>0)<>packed_expected || (!gpu>0)<>packed_expected then
          failwith(Printf.sprintf "uniform %s reducer=%s seed=%s count=%d time=%g domains=%d CPU=%d GPU=%d expected=%b"
            (Flow.Ty.to_string ty)reducer(Option.value ~default:"default" seed)count time domains !cpu !gpu packed_expected);
        assert(bytes !current=original))) [1;8]) [0.;0.25;0.5]) counts)
    [Flow.Ty.Float,"+",None,None,true,true;
     Float,"(fn [a x] (+ a x))",None,None,true,true;
     Vec2,"+",None,None,true,true;
     Vec3,"+",None,None,true,true;
     Vec4,"+",None,None,true,true;
     Vec3,"(fn [a x] (+ a x))",None,None,true,true;
     Float,"(fn [a x] (+ (* a 0.99) x))",None,None,true,false;
     Vec3,"(fn [a x] (+ x a))",None,None,true,false;
     Float,"min",None,None,false,false;
     Float,"+",Some custom,None,false,false;
     Float,"+",None,Some "0",true,true];
  List.iter(fun body->
    let value=evaluate(workspace [] body)in
    let packed=match value with E.Residual r->P.compile r(E.Residual.residual_view r).term
      |> Option.get|_->assert false in
    assert(not(P.Private.ordered_add packed));
    List.iter(fun domains->Parallel.run ~domains(fun()->
      let live=Input.at_time 0. in
      assert(same(P.force packed ~live)(E.Host.force_reference value ~live))))[1;8])
    ["(scan [a [t 0.0]] [x (map (fn [x] [x -0.0]) (array/range 32769))] (+ a x))";
     "(fold [a [t 0.0]] [x (array/range 32769)] (+ a x))"];
  let value=evaluate(workspace []
    "(fold [a [t 0.0 0.0]] [x (array/vec3 32769 [0.25 -0.0 1.0])] (+ a x))")in
  let packed=match value with E.Residual r->P.compile r(E.Residual.residual_view r).term
    |> Option.get|_->assert false in
  (* Same instructions as a supported reduction, but the fold is a Product loop. *)
  assert((P.Private.view packed).code=[|P.Input(0,3,0);Input(0,3,1);Input(0,3,2);
    Accumulator 0;Accumulator 1;Accumulator 2;
    Binary(Add,3,0);Binary(Add,4,1);Binary(Add,5,2)|]);
  assert(not(P.Private.ordered_add packed));
  List.iter(fun domains->Parallel.run ~domains(fun()->
    let live=Input.at_time 0.25 in
    assert(same(P.force packed ~live)(E.Host.force_reference value ~live))))[1;8];
  (* A resolver may advance a fold before a later reduction error; both callers roll it back. *)
  let fold=evaluate(workspace [] "(state [a 0.0] (+ a 1.0))")in
  let live=Input.at_time 0. in
  List.iter(fun(ty,failed_component)->
  let value,sum,packed=fixture ty "+"in
  let width=width ty in
  List.iter(fun domains->Parallel.run ~domains(fun()->
    List.iter(fun count->List.iter(fun gpu->
      let state=E.create_state()in let before=E.state_stamp state in
      let values=Array.init(count*width)(fun i->
        if i mod width=failed_component && i/width>=count-2 then 1e308 else 0.)in
      let current=ref(if width=1 then E.Float_array values else E.Vec3_array values)in
      let original=bytes !current in
      let resolve value=ignore(ok(E.Host.force_reference ~state fold ~live));resolve_source current value in
      let reference_state=E.create_state()in let reference_before=E.state_stamp reference_state in
      let reference_resolve value=ignore(ok(E.Host.force_reference ~state:reference_state fold ~live));
        resolve_source current value in
      let expected=E.Host.force_reference ~state:reference_state ~resolve:reference_resolve value ~live in
      assert(Result.is_error expected && E.state_stamp reference_state=reference_before);
      let result=if gpu then Result.map(fun _->E.Int 0)(P.Private.prepare ~state ~resolve packed ~live)
        else P.force ~state ~resolve packed ~live in
      assert(same result expected && E.state_stamp state=before);
      assert(bytes !current=original);
      current:=(if width=1 then E.Float_array[|1.;2.;3.|]
        else E.Vec3_array[|1.;2.;3.;4.;5.;6.|]);
      let expected=E.Host.force_reference ~resolve:(resolve_source current) value ~live in
      if gpu then begin
        let prepared=ok(P.Private.prepare ~state ~resolve packed ~live)in
        let expected_uniform=ok(E.Host.force_reference ~resolve:(resolve_source current) sum ~live)
          |> components in
        assert(Array.exists(fun actual->bytes actual=bytes expected_uniform)prepared.uniforms)
      end
      else assert(same(P.force ~state ~resolve packed ~live)expected);
      assert(ok(E.Host.force_reference ~state fold ~live)=E.Float 1.)) [false;true]) [2;16385])) [1;8])
    [Flow.Ty.Float,0;Vec3,0;Vec3,1;Vec3,2];
  print_endline "Captured uniform packed/reference parity, ordered named reductions, changing inputs, errors and rollback passed"; flush stdout
