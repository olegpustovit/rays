module E = Flow.Eval
module C = Flow_ir.Core
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let checked body =
  let forms = ok (Flow.Syntax.parse ("(workspace w (graph g :context value " ^ body ^ "))")) in
  match Flow.Workspace.check {Flow.Check.version=1;kinds=[]} forms with
  | Some ws,[] -> ws | _,ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
let expressions = Core_fixtures.lower_expressions
let dump () =
  let buffer = Buffer.create 4096 in
  let fmt = Format.formatter_of_buffer buffer in
  List.iteri (fun i body ->
    Format.fprintf fmt "expression %d: %s\n" i body;
    let evaluation = ok (E.static (checked body)) in
    let rec value : E.value -> unit = function
      | Residual r ->
          (match Flow_ir.Core_lower.lower r with
          | Ok program ->
              let printed = Format.asprintf "%a" C.pp program in
              assert (printed = Format.asprintf "%a" C.pp (Result.get_ok (Flow_ir.Core_lower.lower r)));
              Format.fprintf fmt "%s" printed
          | Error name ->
              Format.fprintf fmt "refused %s\n" name)
      | List xs -> Array.iter value xs
      | Record xs -> List.iter (fun (_,v) -> value v) xs
      | v -> Format.fprintf fmt "constant %s\n" (E.show v) in
    value (List.assoc "g" evaluation.results)) expressions;
  Format.pp_print_flush fmt ();
  Buffer.contents buffer
let () =
  let actual = dump () in
  (if Array.length Sys.argv=2 && Sys.argv.(1)="--print" then print_string actual
  else begin
    let expected=In_channel.with_open_bin "core_lower.expected" In_channel.input_all in
    if actual <> expected then failwith ("Core lowering changed:\n" ^ actual)
  end);
  List.iter (fun (body,name) ->
    match List.assoc "g" (ok (E.static (checked body))).results with
    | E.Residual r -> assert (Flow_ir.Core_lower.lower r = Error name)
    | _ -> assert false)
    ["(let* [a0 t " ^ String.concat " " (List.init 1100 (fun i -> Printf.sprintf "a%d (+ a%d t)" (i+1) i)) ^ "] a1100)","budget"];
  (* a larger [~budget] admits the same program (cooked geometry folds are interpreted at cook time) *)
  (match List.assoc "g" (ok (E.static (checked ("(let* [a0 t " ^ String.concat " " (List.init 1100 (fun i -> Printf.sprintf "a%d (+ a%d t)" (i+1) i)) ^ "] a1100)")))).results with
   | E.Residual r -> assert (Result.is_ok (Flow_ir.Core_lower.lower ~budget:32768 r))
   | _ -> assert false);
  let observed=ref false in
  ignore (ok (E.Residual.handle ~observe:(fun r ->
    match (E.Residual.residual_view r).term.node with
    | Flow.Workspace.Loop {kind=`Fold;_} ->
        observed:=true;
        let program=Result.get_ok (Flow_ir.Core_lower.lower r) in
        List.iter (fun time ->
          let live=Input.at_time time in
          assert (Flow_ir.Core_exec.run program ~live = E.Private.force_reference (E.Residual r) ~live))
          [0.;0.37;2.]
    | _ -> ()) (fun () -> E.Private.static_with_requests
      (checked "(fold [s 0.] [i (range 2)] (+ s (+ t i)))"))));
  assert !observed;
  assert (C.literal_of_value (E.Residual (match List.assoc "g" (ok (E.static (checked "(+ t 1)"))).results with
    E.Residual r -> r | _ -> assert false)) = None)

let () =
  let geometry=Flow.Ty.geometry and image=Flow.Ty.image in
  let marker name ty id = Flow.Op.{(Option.get(Flow.Op.find "frame/dt" Flow.Context.value)) with
    name;signature={pos=[];opt=[];rest=None;kw=[]};out=(fun _->ty);out_args=None;
    live=false;arithmetic=None;packed_extension=None;
    body=(fun ~live:_ ~node:_ _->Flow.Value.Deferred(ty,id))} in
  let ops=[marker "handle_geometry" geometry 11;marker "handle_image" image 11] in
  let compiled body =
    let forms=ok(Flow.Syntax.parse ("(workspace handles (graph g :context value " ^
      "[(selected : bool false) (geo : geometry (handle_geometry)) (img : image (handle_image)) (bad : geometry (handle_geometry))] " ^
      "(let* [tested (if selected " ^ body ^ " " ^ body ^ ")] 0.)))")) in
    let workspace=match Flow.Workspace.check ~ops {Flow.Check.version=1;kinds=[]} forms with
      |Some workspace,[]->workspace|_,ds->failwith(String.concat "; " (List.map Diagnostic.to_string ds)) in
    let inputs=["g",["selected",E.Source("test/selected",Flow.Ty.Bool,[]);
      "geo",E.Deferred(geometry,11);"img",E.Deferred(image,11);"bad",E.Deferred(geometry,99)]] in
    let value=snd(List.hd(List.assoc["g";"tested"](ok(E.static ~record:true ~inputs workspace)).records)) in
    let residual=match value with E.Residual r->r|_->failwith("Expected cooked residual: "^body^" = "^E.show value) in
    Flow_ir.Core_exec.prepare(Result.get_ok(Flow_ir.Core_lower.lower ~strict:true residual)) in
  let reason=Diagnostic.error ~cause:"unknown handle cause" ~code:"E_TEST_HANDLE" "Unknown cooked handle." in
  let run prepared time allow_bad =
    let calls=ref [] in
    let resolve=function
      |E.Source("test/selected",Flow.Ty.Bool,[])->
          calls:=(Flow.Ty.Bool,0)::!calls;Ok(E.Bool(time>0.))
      |E.Source("$core/cooked",ty,["id",E.Int id])->
          calls:=(ty,id)::!calls;
          if (ty=geometry || ty=image) && (id=11 || allow_bad && id=99) then Ok(E.Deferred(ty,id))
          else Error reason
      |_->assert false in
    let result=Flow_ir.Core_exec.execute ~resolve prepared ~live:(Input.at_time time) in
    result,List.rev !calls in
  let mixed=compiled "(if selected {:a geo :b geo :picture img} {:a geo :b geo :picture img})" in
  List.iter(fun time->let result,calls=run mixed time false in
    assert(result=Ok(E.Record["a",E.Deferred(geometry,11);"b",E.Deferred(geometry,11);"picture",E.Deferred(image,11)]));
    assert(List.filter((=)(geometry,11))calls|>List.length=1);
    assert(List.filter((=)(image,11))calls|>List.length=1);
    assert(List.filter((=)(Flow.Ty.Bool,0))calls|>List.length=2)) [-1.;1.;-1.;1.];
  let lazy_bad=compiled "(if selected {:a bad :b bad} {:a geo :b geo})" in
  List.iter(fun time->let result,calls=run lazy_bad time false in
    if time>0. then match result with Error d->assert(d.code=reason.code && d.message=reason.message && d.cause=reason.cause)
      |Ok _->assert false
    else (assert(result=Ok(E.Record["a",E.Deferred(geometry,11);"b",E.Deferred(geometry,11)]));
      assert(not(List.mem(geometry,99)calls)))) [-1.;1.;-1.;1.];
  let crossing=compiled "(let* [chosen (if selected geo bad)] {:first chosen :after geo})" in
  List.iter(fun time->let result,calls=run crossing time true in
    assert(result=Ok(E.Record["first",E.Deferred(geometry,(if time>0. then 11 else 99));"after",E.Deferred(geometry,11)]));
    assert(List.mem(geometry,11)calls)) [1.;-1.;1.;-1.];
  print_endline "Core cooked handles: exact type, lazy failures, changing live branches and fresh execution validation"

let () =
  let storage=Flow.Op.{(Option.get(Flow.Op.find "frame/dt" Flow.Context.value))with
    name="test/storage";signature={pos=["values",Flow.Ty.Array Flow.Ty.Vec4];opt=[];rest=None;kw=["later",Flow.Ty.Float]};
    out=(fun _->Flow.Ty.geometry);out_args=None;live=false;arithmetic=None;packed_extension=None;
    body=(fun ~live:_ ~node:_ args->Flow.Value.Source("test/storage",Flow.Ty.geometry,args))} in
  let program body=
    let forms=ok(Flow.Syntax.parse("(workspace constants (graph g :context value (let* [tested "^body^"] 0.)))")) in
    let ws=match Flow.Workspace.check ~ops:[storage] {Flow.Check.version=1;kinds=[]}forms with
      |Some ws,[]->ws|_,ds->failwith(String.concat";"(List.map Diagnostic.to_string ds)) in
    let value=snd(List.hd(List.assoc["g";"tested"](ok(E.static ~record:true ws)).records)) in
    match value with E.Residual r->Result.get_ok(Flow_ir.Core_lower.lower ~strict:true r)
      |_->failwith("Expected constructor residual: "^E.show value) in
  let count_descriptors program=
    let count=ref 0 in
    C.iter (fun (statement:C.statement)->match statement.node with
      |Assign(_,Source_descriptor{name;_}) when String.starts_with ~prefix:"$core/constant-array/" name->incr count
      |_->()) program.C.body;
    !count in
  let run program time=
    let received=ref None in
    let resolve=function
      |E.Source("$core/operator",_,args)->received:=List.assoc_opt "values" args;Ok(E.Deferred(Flow.Ty.geometry,1))
      |_->assert false in
    let result=Flow_ir.Core_exec.run ~resolve program ~live:(Input.at_time time) in
    result, !received in
  let only=program "(let* [colors (array/vec4 (int t) [-0.0 1.0 2.0 3.0]) alias colors] (test/storage (exact alias)))" in
  assert(count_descriptors only=1);
  List.iter(fun time->let result,received=run only time in
    assert(result=Ok(E.Deferred(Flow.Ty.geometry,1)));
    match received with Some(E.Source(name,Flow.Ty.Array Flow.Ty.Vec4,["count",E.Int n;"value",_]))->
      assert(name="$core/constant-array/array/vec4" && n=int_of_float time)
      |value->failwith("Unexpected constant descriptor: "^Option.fold ~none:"missing" ~some:E.show value^"\n"^Format.asprintf "%a" C.pp only)) [0.;1.;32769.];
  let mixed=program "(let* [colors (array/vec4 (int t) [-0.0 1.0 2.0 3.0])] (test/storage colors :later (float (array/count colors))))" in
  assert(count_descriptors mixed=0);
  (match snd(run mixed 3.)with Some(E.Vec4_array data)->assert(Array.length data=12)|_->assert false);
  let ambiguous=program "(let* [colors (array/vec4 (int t) [-0.0 1.0 2.0 3.0]) alias (if (> t 1.) colors colors)] (test/storage alias))" in
  assert(count_descriptors ambiguous=0);
  let early=program "(test/storage (array/vec4 (int t) [-0.0 1.0 2.0 3.0]) :later (if (> t -100.) (error \"E_LATER\" \"later argument\") 0.))" in
  (match fst(run early (-1.))with Error d->assert(d.code="E_ARRAY_RANGE")|_->assert false);
  (match fst(run early 1.)with Error d->assert(d.code="E_LATER")|_->assert false);
  print_endline "Core constant arrays: original-site validation, cooked storage aliases, mixed-use dense path and empty/large metadata"

let () =
  List.iter(fun(body,expected_time)->
    let value=List.assoc "g"(ok(E.static(checked body))).results in
    let residual=match value with E.Residual r->r|_->failwith("Expected live frame facts: "^body)in
    let program=Result.get_ok(Flow_ir.Core_lower.lower residual)in
    let times=ref 0 in
    C.iter(fun(statement:C.statement)->match statement.node with
      |Assign(_,Frame Time)->incr times|_->())program.body;
    assert(!times=expected_time);
    let prepared=Flow_ir.Core_exec.prepare program in
    List.iter(fun(time,frame)->
      let live={(Input.at_time time)with frame;dt=0.125;pointer=(3.,4.)}in
      let expected=E.Private.force_reference value ~live in
      let actual=Flow_ir.Core_exec.execute prepared ~live in
      assert(Marshal.to_string actual[Marshal.No_sharing]=Marshal.to_string expected[Marshal.No_sharing]))
      [-1.,0;0.,1;0.37,2;2.,0])[
    "(+ (* (sin t) 0.5) (* t t))",1;
    "(+ (+ t t) (+ (+ (frame/dt) (frame/dt)) (+ (pointer/x) (pointer/x))))",1;
    "(+ (if (> (frame/index) 0) t 0.) t)",2;
    "(let* [total (fold [a 0.] [i (range (frame/index))] (+ a t))] (+ total t))",2;
  ];
  print_endline "Core frame facts: scoped sharing, changing inputs, branch dominance and empty loops agree"
