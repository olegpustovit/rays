module E=Flow.Eval
module I=Flow_ir
let ok=function Ok v->v|Error d->failwith(Diagnostic.to_string d)
let catalog=Flow.Check.{version=1;kinds=[]}
let source body=ok(Flow.Syntax.parse("(workspace capacity (graph g :context host "^body^"))"))
let checked body=match Flow.Workspace.check catalog(source body)with
  |Some w,[]->w|_,ds->failwith(String.concat ";"(List.map Diagnostic.to_string ds))
let value body=List.assoc "g"(ok(E.static(checked body))).results
let limits=["int",Sys.max_array_length;"text",Sys.max_array_length;
  "float",Sys.max_floatarray_length;"vec2",Sys.max_floatarray_length/2;
  "vec3",Sys.max_floatarray_length/3;"vec4",Sys.max_floatarray_length/4]
let literal kind="(array/max_length :type "^kind^")"
let failure code f=try ignore(f());assert false with
  |Diagnostic.Error d -> assert(d.code=code)
let ()=
  let op=Option.get(Flow.Op.find "array/max_length" Flow.Context.value)in
  let direct args=op.body ~live:(Input.at_time 0.) ~node:(fun _ _->assert false)args in
  assert(op.signature.pos=[] && op.signature.kw=["type",Flow.Ty.Text]);
  assert(direct []=E.Int Sys.max_floatarray_length);
  assert(value "(array/max_length)"=E.Int Sys.max_floatarray_length);
  assert(Flow.Op.result_type op []=Flow.Ty.Int);
  List.iter(fun(kind,limit)->
    assert(direct["type",E.Text kind]=E.Int limit);
    assert(value(literal kind)=E.Int limit);
    assert(Flow.Op.result_type op["type",Flow.Ty.Text,Some(Param.Text_value kind)]=Flow.Ty.Int);
    let body="(if (> t 0.) "^literal kind^" (int/sub "^literal kind^" 1))"in
    let v=value body in let r=match v with E.Residual r->r|_->assert false in
    let core=match I.Core_lower.lower ~strict:true r with Ok c->c|Error form->failwith form in
    let compiled=ok(I.Executor.compile ~strict:true v)in
    List.iter(fun time->let live=Input.at_time time in
      let expected=ok(E.Private.force_reference v ~live)in
      assert(expected=E.Int(if time>0. then limit else limit-1));
      assert(ok(Test_program.run ~root:r ~strict:true core ~live)=expected);
      List.iter(fun domains->Parallel.run ~domains(fun()->
        assert(ok(I.Executor.force ~strict:true compiled ~live)=expected))) [1;4;8]) [-1.;1.];
    (* Symbolic cap-minus-extra validation; no constructor or huge allocation. *)
    List.iter(fun extra->let maximum=limit-extra in
      let guard count=Printf.sprintf
        "(let* [maximum (int/sub %s %d)] (if (int/le %d maximum) \"ok\" (error \"E_CAPACITY\" \"count exceeds element capacity\")))"
        (literal kind)extra count in
      assert(value(guard maximum)=E.Text "ok");
      (match E.static(checked(guard(maximum+1)))with
       |Error d->assert(d.code="E_CAPACITY" && d.message="count exceeds element capacity")
       |Ok _->assert false)) [0;1];
    let ty=Option.get(Flow.Ty.of_string kind)in
    failure "E_ARRAY_RANGE"(fun()->Flow.Value.array_init ty (limit+1)(fun _->assert false))) limits;
  List.iter(fun arg->failure "E_ARRAY_TYPE"(fun()->direct["type",arg]))
    [E.Text "unknown";E.Int 1;E.Bool true;E.Text_array[|"float"|]];
  failure "E_ARRAY_TYPE"(fun()->Flow.Op.result_type op["type",Flow.Ty.Text,None]);
  List.iter(fun(body,code)->match Flow.Workspace.check catalog(source body)with
    |None,ds->assert(List.exists(fun(d:Diagnostic.t)->d.code=code)ds)
    |_->assert false)
    ["(array/max_length :type \"unknown\")","E_ARRAY_TYPE";
     "(array/max_length :type 1)","E_TYPE";
     "(array/max_length :type (if (> t 0.) \"int\" \"float\"))","E_ARRAY_TYPE";
     "(array/max_length 1)","E_ARITY"];
  (* Exact Int kernels retain ordinary GPU refusal, even when Int work feeds Float. *)
  List.iter(fun body->let v=value body in let r=match v with E.Residual r->r|_->assert false in
    let packed=match I.Packed.compile_result r(E.Residual.residual_view r).term with
      |Ok p->p|Error ds->failwith(String.concat ";"(List.map Diagnostic.to_string ds))in
    assert(I.Packed.gpu_refusals packed<>[]);
    let compiled=ok(I.Executor.compile ~strict:true v)in
    List.iter(fun domains->Parallel.run ~domains(fun()->
      let live={(Input.at_time 1.)with frame=3}in
      assert(ok(I.Executor.force ~strict:true compiled ~live)=ok(E.Private.force_reference v ~live)))) [1;4;8])
    ["(map (fn [(i : int)] (int/sub (array/max_length :type int) (int/add i (frame/index)))) (array/range 5001 :type int))";
     "(map (fn [(i : int)] (float (int/sub (array/max_length :type int) (int/add i (frame/index))))) (array/range 5001 :type int))"];
  (* An explicit, statically folded Float conversion has no retained Int bank. *)
  let v=value "(map (fn [x] (+ (+ x t) (float (array/max_length)))) (array/range 5001))"in
  let r=match v with E.Residual r->r|_->assert false in
  let p=match I.Packed.compile_result r(E.Residual.residual_view r).term with
    |Ok p->p|Error ds->failwith(String.concat ";"(List.map Diagnostic.to_string ds))in
  assert(I.Packed.gpu_refusals p=[]);
  let compiled=ok(I.Executor.compile ~strict:true v)in
  List.iter(fun domains->Parallel.run ~domains(fun()->
    let live=Input.at_time 1. in
    assert(ok(I.Executor.force ~strict:true compiled ~live)=ok(E.Private.force_reference v ~live)))) [1;4;8];
  print_endline "array/max_length: exact native element bounds, checked literal selectors, symbolic guards, reference/Core/CPU and ordinary Int-bank refusals; no giant allocations"
