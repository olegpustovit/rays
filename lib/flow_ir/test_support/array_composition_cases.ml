module E=Flow.Eval
module I=Flow_ir
let ok=function Ok value->value|Error d->failwith(Diagnostic.to_string d)
let checked ?(declarations="") body=
  let text="(workspace composition (graph g :context host "^declarations^" "^body^"))" in
  match Flow.Workspace.check Flow.Catalog.{version=1;kinds=[]}(ok(Flow.Syntax.parse text))with
  |Some w,[]->w|_,ds->failwith(body^": "^String.concat "; "(List.map Diagnostic.to_string ds))
let live_body body="(if (> t -1000.) "^body^" "^body^")"
let value body=List.assoc "g"(ok(E.static(checked(live_body body)))).results
let core value=match value with
  |E.Residual r->(match I.Core_lower.lower ~strict:true r with
      |Ok p->r,p|Error form->failwith("Composition Core refusal: "^form))
  |_->failwith "Composition needs a live residual"
let literal bytes=fst(Flow.Lisp.print[Flow.Syntax.make(Flow.Syntax.Str bytes)])
let all_bytes=String.init 256 Char.chr
let types=[Flow.Ty.Int;Float;Vec2;Vec3;Vec4;Text]
let element=function
  |Flow.Ty.Int->"(int/add i 9007199254740993)"
  |Float->"(if (int/eq i 0) -0. (float i))"
  |Vec2->"[(float i) -0.]"
  |Vec3->"[(float i) -0. t]"
  |Vec4->"[(float i) -0. t 0.125]"
  |Text->"(if (int/eq (int/mod i 2) 0) "^literal all_bytes^" \"é日本😀\")"
  |_->assert false
let producer ty count="(map (fn [(i : int)] "^element ty^") (array/range "^count^" :type int))"
let constant ty count=match ty with
  |Flow.Ty.Int->"(array/int "^count^" 9007199254740993)"
  |Float->"(array/float "^count^" -0.)"
  |Vec2->"(map (fn [x] [x -0.]) (array/float "^count^" 1.))"
  |Vec3->"(array/vec3 "^count^" [1. -0. 0.125])"
  |Vec4->"(array/vec4 "^count^" [1. -0. 0.125 2.])"
  |Text->"(array/text "^count^" "^literal all_bytes^")"
  |_->assert false
let gather table="(map (fn [(i : int)] (array/nth "^table^" (int/mod i 13))) (array/range (frame/index) :type int))"
let cases=List.concat_map(fun ty->let name=Flow.Ty.to_string ty in
  let bind count body="(let* [table "^producer ty count^"] "^body^")" in
  [name^"/concat",bind "(frame/index)" "(array/concat table table)";
   name^"/slice",bind "(frame/index)"
     "(array/slice table (int/div (frame/index) 2) (int/sub (frame/index) (int/div (frame/index) 2)))";
   name^"/hoisted",bind "13" ("(let* [joined (array/concat table table)] "^gather "joined"^")");
   name^"/inline-concat",bind "13" (gather "(array/concat table table)");
   name^"/inline-slice",bind "13" (gather "(array/slice table 0 13)")])types
let error_cases=[
  "late-index", "(map (fn [(i : int)] (array/nth (array/concat (array/int 1) (array/int 1)) i)) (array/range (frame/index) :type int))";
  "selected-slice", "(map (fn [(i : int)] (array/nth (array/slice (array/int 1) 2 0) i)) (array/range (frame/index) :type int))";
  "untaken-slice", "(map (fn [(i : int)] (if (int/lt i 0) (array/nth (array/slice (array/int 1) 2 0) i) i)) (array/range (frame/index) :type int))";
  "table-cause", "(map (fn [(i : int)] (array/nth (array/concat (if (> t 0.) (error \"E_COMPOSE\" \"selected table\" :cause \"raw\255\000cause\") (array/int 1)) (array/int 1)) i)) (array/range (frame/index) :type int))";
  "argument-before-copy", "(map (fn [(i : int)] (array/nth (array/slice (array/int 1) (error \"E_FIRST\" \"slice first\" :cause \"first cause\") (error \"E_COUNT\" \"slice count\")) i)) (array/range (frame/index) :type int))";
  "empty-product", "(for [i (array/range (frame/index) :type int) j (array/range -1 :type int)] (array/nth (array/slice (array/int 0) 1 0) i))";
  "empty-zip", "(map (fn [(i : int) (j : int)] (array/nth (array/slice (array/int 0) 1 0) i)) (array/range (frame/index) :type int) (array/range -1 :type int))";
  "earliest-row", "(map (fn [(i : int)] (if (int/eq i 0) (error \"E_ROW_ZERO\" \"first row\" :cause \"zero cause\") (array/nth (array/slice (array/int 1) 2 0) i))) (array/range (frame/index) :type int))"]
let bytes value=Marshal.to_string value[Marshal.No_sharing]
let captured_helper body=
  let recorded source=ok(E.static ~record:true(checked source))in
  let callable evaluation=match snd(List.hd(List.assoc["g";"tested"]evaluation.E.records))with
    |E.Fn fn->fn|_->assert false in
  let helper=callable(recorded("(let* [data (array/int 9 9) tested (fn [(j : int)] "^body^")] 0.)"))in
  let template=callable(recorded
    "(let* [data (array/int 1 7) tested (fn [(helper : (fn [int] int)) (i : int)] (if (int/lt i 0) (array/nth data 0) (helper (int/mod i 2))))] 0.)")in
  let params,body=Option.get(E.Residual.function_body template)in
  let root=match value "t"with E.Residual r->r|_->assert false in
  let view=E.Residual.residual_view root in
  E.Residual.compiled_function root ~instance:view.instance ~prefix:[] ~zone:["g";"tested"]
    ~params:(List.tl params) ~body ~bindings:(("helper",E.Fn helper)::E.Residual.function_bindings template)
let helper_value body count=ok(E.Host.map_function
  ~signature:Flow.Ty.{params=[Int];result=Int}(captured_helper body)[E.Int_array(Array.init count Fun.id)])
let same a b=match a,b with
  |Ok a,Ok b->assert(bytes a=bytes b)
  |Error(a:Diagnostic.t),Error(b:Diagnostic.t)->
      assert(a.code=b.code && a.message=b.message && a.cause=b.cause)
  |_->failwith "Composition/reference result mismatch"
