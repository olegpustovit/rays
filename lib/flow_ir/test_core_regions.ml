module E = Flow.Eval
module I = Flow_ir
open Test_support
let bits x y = Int64.bits_of_float x = Int64.bits_of_float y
let rec same (a : E.value) (b : E.value) = match a,b with
  | Float x,Float y -> bits x y
  | Vec3 (a,b,c),Vec3 (x,y,z) -> bits a x && bits b y && bits c z
  | List a,List b -> Array.length a=Array.length b && Array.for_all2 same a b
  | (Float_array a,Float_array b) -> Array.length a=Array.length b && Array.for_all2 bits a b
  | (Vec2_array a,Vec2_array b) | (Vec3_array a,Vec3_array b) | (Vec4_array a,Vec4_array b) ->
      Array.length a=Array.length b && Array.for_all2 bits a b
  | a,b -> a=b
let same_result a b = match a,b with
  | Ok a,Ok b -> same a b
  | Error (a : Diagnostic.t),Error (b : Diagnostic.t) -> a.code=b.code | _ -> false
(* Primitives are lowered inline; region forms must be the residual's own term. *)
let primitives = Core_fixtures.primitives
(* Dense parameter sweeps: every colour sector and stop boundary, many roundings. *)
let sweeps = [
  "hsl-sectors","(color/hsl (* t 60.) 0.75 0.5)",List.init 161 (fun i -> float_of_int (i-40) *. 0.25);
  "hsl","(color/hsl (* t 37.31) (abs (sin (* t 1.7))) (abs (cos (* t 0.9))))",
    List.init 40000 (fun i -> float_of_int i *. 0.0137 -. 100.);
  "gradient","(color/gradient (list \"#101426\" \"#244a72\" \"#3ca6a6\" \"#f2d16b\" \"#f26444\") t)",
    List.init 2501 (fun i -> float_of_int (i-500) *. 0.001);
  "gradient-two","(color/gradient (list \"#000000\" \"#ffffff\") t)",List.init 2501 (fun i -> float_of_int i *. 0.0004);
  "gradient-one","(color/gradient (list \"#336699\") t)",[0.;0.5];
]
let regions = Core_fixtures.regions
let times = [-3.;-0.;0.;0.37;1.;1.9;3.]
let check ?(times=times) ~region (name,body) =
  let tested=if region then body else "(if (> t -1000.) " ^ body ^ " " ^ body ^ ")" in
  let source="(workspace w (graph g :context value (let* [tested " ^ tested ^ "] 0.)))" in
  let ws=match Flow.Workspace.check ~ops:[] {Flow.Catalog.version=1;kinds=[]} (ok (Flow.Syntax.parse source)) with
    | Some ws,[] -> ws | _,ds -> failwith (name ^ ": " ^ String.concat "; " (List.map Diagnostic.to_string ds)) in
  let value=snd (List.hd (List.assoc ["g";"tested"] (ok (E.static ~record:true ws)).records)) in
  let r=match value with E.Residual r -> r | _ -> failwith (name ^ ": expected residual") in
  let core=match I.Core_lower.lower r with Ok p -> p | Error n -> failwith (name ^ ": refused " ^ n) in
  let text=Format.asprintf "%a" I.Core.pp core in
  let has_region=Array.length core.regions>0 in
  if has_region<>region then failwith (name ^ ": region expectation\n" ^ text);
  let program=ok (I.Executor.compile value) in
  List.iter (fun time ->
    let live=Input.at_time time in
    let expected=E.Host.force_reference value ~live in
    let verify label actual = if not (same_result actual expected) then
      failwith (Printf.sprintf "%s at %g (%s)" name time label) in
    (* A failing region declines to the packed path, so only successes compare with the walker. *)
    (match Test_program.run core ~live,expected with
     | Ok a,Ok b -> if not (same a b) then failwith (Printf.sprintf "%s at %g (direct)" name time)
     | Error _,Error _ when not region -> ()
     | Error _,_ when region -> ()
     | _ -> failwith (Printf.sprintf "%s at %g (direct outcome)" name time));
    if not region || Result.is_ok expected then
      verify "core" (I.Executor.force program ~live)) times
(* A workspace operator that reuses a shipped expansion's name keeps its own body: core declines it. *)
let () =
  let custom={Flow.Op.noise3 with name="toy_noise3";packed_extension=None;body=(fun ~live:_ ~node:_ _ -> E.Float 0.125)} in
  let source="(workspace w (graph g :context value (let* [tested (toy_noise3 [t 0 0])] 0.)))" in
  let ws=match Flow.Workspace.check ~ops:[custom] {Flow.Catalog.version=1;kinds=[]} (ok (Flow.Syntax.parse source)) with
    | Some ws,[] -> ws | _,ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds)) in
  match snd (List.hd (List.assoc ["g";"tested"] (ok (E.static ~record:true ws)).records)) with
  | E.Residual r -> assert (I.Core_lower.lower r = Error "toy_noise3")
  | _ -> failwith "expected residual"
let () =
  List.iter (check ~region:false) primitives;
  List.iter (fun (name,body,times) -> check ~times ~region:false (name,body)) sweeps;
  List.iter (check ~region:true) regions;
  print_endline "Core regions: packed-array primitives and Region_ref forms match the walker"

let carried_loops () =
  let module R=I.Core.Region in
  let rec bare=function R.Guarded(_,body)->bare body|body->body in
  let verify name program =
    assert(Array.exists(fun instruction->match bare instruction with R.Loop_begin _->true|_->false)program.R.code);
    List.iter(fun time->
      let live=Input.at_time time in
      let expected=I.Packed.Host.reference program ~live in
      List.iter(fun domains->
        let actual=Parallel.run ~domains(fun()->I.Packed.force ~strict:true program ~live)in
        (match actual,expected with
         |Error actual,Error expected when expected.Diagnostic.code="E_ITER_BOUND"->
             assert(actual.span=expected.span && actual.message=expected.message)
         |_->());
        if not(same_result actual expected)then
          failwith(Printf.sprintf "carried %s at %g, %d domains" name time domains)) [1;8])[-1.;0.37;5.]
  in
  let cases=[
    "bounds","(fold [a t] [i (range -2 (int x))] (+ a (* i 0.5)))";
    "range-limit","(fold [a x] [i (range (+ 4097 (abs (int t))))] (+ a 1.))";
    "record-swap","(let* [s (fold [s {:a 1 :b 2 :v [t 1.] :flag false :text \"start\"}]
      [i (range (int x))] {:a s.b :b s.a :v [s.v.y (+ s.v.x 1.)]
       :flag (not s.flag) :text (if s.flag \"a\" \"b\")})]
      [(float s.a) (float s.b) s.v.x (if s.flag 1. 0.)])";
    "nested","(fold [a t] [i (range 3)] (fold [b a] [j (range i)] (+ b (+ x j))))";
    "empty","(fold [a t] [i (range 0)] (/ a 0.))";
    "empty-error","(fold [a t] [i (range 0)] (error \"E_INNER\" \"bad\"))";
    "scope","(let* [a (fold [s t] [i (range 2)] (+ s (+ x 1.)))] (+ a (+ x 1.)))";
    "guarded-error","(if (> x t) (fold [a 0.] [i (range 2)] (/ (+ a x) (- x x))) t)";
    "error-order","(fold [a t] [i (range (if (= x 0.) 1 5000))] (if (> t -100.) (error \"E_FIRST\" \"first row\") a))";
    "iterate-count","(iterate (- (int x) 2) t (fn [a] (+ a x)))";
    "iterate-empty-error","(iterate 0 t (fn [a] (error \"E_EMPTY\" \"untaken body\"))
      :until (fn [prev next] (error \"E_EMPTY\" \"untaken predicate\")))";
    "iterate-nested","(iterate 3 t (fn [a] (fold [b a] [j (range 2)] (+ b (+ x j)))))";
    "iterate-lazy-error","(iterate 4611686018427387903 (+ x t)
      (fn [a] (if (> a (+ (+ x t) 2.)) (error \"E_LATE\" \"past stop\") (+ a 1.)))
      :until (fn [prev next] (>= next (+ (+ x t) 3.))))";
    "iterate-predicate-error","(iterate 2 t (fn [a] (+ a x))
      :until (fn [prev next] (error \"E_PREDICATE\" \"selected predicate\")))";
    "iterate-int-wrap","(iterate (+ (abs (int t)) (int x)) 4611686018427387903 (fn [a] (int/add a 1)))";
    "iterate-text","(let* [s (iterate 4611686018427387903 {:flag true :text \"old\" :n t}
      (fn [a] {:flag false :text \"next\" :n (+ a.n x)}) :until (fn [prev next] prev.flag))] s.n)";
  ]in
  List.iter(fun(name,body)->
    try verify name (Test_program.compile ("(map (fn [x] "^body^") (array/range 7))"))
    with Failure message->failwith(name^": "^message))cases;
  verify "helper" (Test_program.compile
    "(let* [f (fn [a] (+ a t))] (map (fn [x] (fold [a x] [i (range 3)] (f a))) (array/range 7)))");
  verify "iterate-helper" (Test_program.compile
    "(let* [f (fn [a] (+ a t))] (map (fn [x] (iterate 3 x f)) (array/range 7)))");
  let pcg = Test_program.compile
    ~declarations:"(defn pcg :context value [(s : int)]
        (let* [state (int/and (int/add (int/mul s 747796405) 2891336453) 4294967295)
               word (int/and (int/mul (int/xor (int/shr state (int/add (int/shr state 28) 4)) state) 277803737) 4294967295)]
          {:state state :word (int/xor (int/shr word 22) word)}))
      (defn tint :context value [(c : vec3) (k : float)] [(* c.x k) (* c.y k) (+ c.z k)])"
    "(map (fn [x] (let* [r (fold [r {:state (int/and (int x) 4294967295) :acc [t 1. 0.]}]
        [i (range 3)] (let* [p (pcg r.state)]
          {:state p.state :acc (tint r.acc (/ (float (int/and p.word 65535)) 65536.))}))]
        (+ r.acc.x (+ r.acc.y r.acc.z)))) (array/range 65))" in
  verify "defn-pcg-vec3" pcg;
  assert (I.Packed.gpu_refusals ~fast:true ~fused:true pcg = []);
  verify "iterate-named-helper" (Test_program.compile
    ~declarations:"(defn step :context value [(a : float)] (+ a t))"
    "(map (fn [x] (iterate 3 x step)) (array/range 7))");
  verify "outer-accumulator" (Test_program.compile
    "(fold [a 1.] [x (array/range 4)] (fold [b a] [i (range 2)] (+ b (+ t x))))");
  verify "parallel" (Test_program.compile
    "(map (fn [x] (fold [a t] [i (range 3)] (+ a (* x 0.125)))) (array/range 32769))");
  verify "iterate-parallel" (Test_program.compile
    "(map (fn [x] (iterate 5 (+ x t) (fn [a] (+ a 1.))
      :until (fn [prev next] (>= next (+ (+ x t) 2.))))) (array/range 32769))");
  verify "uint32" (List.assoc "loop-uint32" (Test_program.carried_fixtures 65));
  List.iter(fun(name,program)->if String.starts_with ~prefix:"iterate-" name then verify name program)
    (Test_program.carried_fixtures 7);
  let program=Test_program.compile
    "(map (fn [x] (fold [a t] [i (range 2)] (+ a x))) (array/range 1))" in
  let header=Option.get(Array.find_index(fun instruction->match bare instruction with R.Loop_begin _->true|_->false)program.R.code)in
  let loop=match bare program.code.(header)with R.Loop_begin loop->loop|_->assert false in
  let result=Option.get(Array.find_index(fun instruction->match bare instruction with R.Loop_result _->true|_->false)program.code)in
  let rejected name code outputs = match R.rebuild program ~code ~outputs ()with
    | Error _->()|Ok _->failwith("accepted malformed loop: "^name)in
  let changed slot instruction=let code=Array.copy program.code in code.(slot)<-instruction;code in
  rejected "end outside program" (changed header (R.Loop_begin{loop with finish=Array.length program.code})) program.outputs;
  rejected "carry type" (changed (header+1) (R.Loop_carry(header,0,Flow.Ty.Text))) program.outputs;
  rejected "carry component" (changed (header+1) (R.Loop_carry(header,1,Flow.Ty.Float))) program.outputs;
  rejected "end owner" (changed loop.finish (R.Loop_end(header+1,[|header+1|],None))) program.outputs;
  rejected "integer stop" (changed loop.finish (R.Loop_end(header,[|header+1|],Some header))) program.outputs;
  rejected "future stop" (changed loop.finish (R.Loop_end(header,[|header+1|],Some result))) program.outputs;
  rejected "result component" (changed result (R.Loop_result(header,max_int,Flow.Ty.Float))) program.outputs;
  rejected "escaped carry" program.code [|[|header+1|]|];
  rejected "escaped body value" (changed result (R.Unary(Abs,loop.finish-1))) program.outputs;
  assert(Flow.Op.range_length max_int min_int=0);
  (try ignore(Flow.Op.range_length min_int max_int);failwith "range overflow accepted"
   with Diagnostic.Error error->assert(error.code="E_ITER_BOUND"));
  print_endline "Core carried loops: empty, dynamic, nested, record/helper, error and domain parity passed"

let ()=carried_loops ()

let () =
  let fused=List.filter(fun(name,_)->String.starts_with ~prefix:"iterate-fused-" name)
    (Test_program.carried_fixtures 65)in
  List.iter(fun(name,program)->
    if List.length program.I.Core.Region.stages<>2 then failwith(name^": expected two fused stages");
    List.iter(fun time->
      let live=Input.at_time time in
      let expected=I.Packed.Host.reference program ~live in
      List.iter(fun domains->Parallel.run ~domains(fun()->
        if not(same_result(I.Packed.force ~strict:true program ~live)expected)then
          failwith(name^": fused loop parity")))[1;8])[-1.;0.37;5.])fused;
  let siblings=List.take 2 fused in
  let merged=Option.get(I.Core_fuse.merge(List.map snd siblings))in
  List.iter(fun time->
    let live=Input.at_time time in
    let expected=List.map(fun(_,program)->ok(I.Packed.Host.reference program ~live))siblings in
    List.iter(fun domains->Parallel.run ~domains(fun()->
      let actual=ok(I.Packed.force_all ~strict:true merged ~live)in
      assert(List.for_all2 same actual expected)))[1;8])[-1.;0.37;5.];
  let text=Test_program.compile
    "(map (fn [s] (if (> t 0.) s \"other\"))
      (map (fn [x] (iterate 2 \"old\" (fn [s] (if (> x t) \"next\" s)))) (array/range 7)))"in
  assert(List.length text.I.Core.Region.stages=2);
  let independent=Test_program.compile
    "(map (fn [x] [(fold [a t] [i (range 2)] (+ a x))
                   (fold [a t] [i (range 2)] (+ a x))]) (array/range 7))"in
  let rec bare=function I.Core.Region.Guarded(_,body)->bare body|body->body in
  assert(Array.fold_left(fun n instruction->match bare instruction with
    |I.Core.Region.Loop_begin _->n+1|_->n)0 independent.code=2);
  let reduction=Test_program.compile
    "(fold [total 1.] [x (map (fn [x] (iterate 3 (+ x t) (fn [a] (+ a 1.))))
      (array/range 7))] (+ (* total 0.5) x))"in
  assert(List.length reduction.I.Core.Region.stages=2);
  List.iter(fun program->List.iter(fun time->
    let live=Input.at_time time in
    assert(same_result(I.Packed.force ~strict:true program ~live)(I.Packed.Host.reference program ~live)))
    [-1.;0.37;5.])[text;independent;reduction];
  let parallel=List.assoc "iterate-fused-both"(Test_program.carried_fixtures 32769)in
  let live=Input.at_time 0.37 in
  let expected=I.Packed.Host.reference parallel ~live in
  List.iter(fun domains->Parallel.run ~domains(fun()->
    assert(same_result(I.Packed.force ~strict:true parallel ~live)expected)))[1;8];
  let shortened=Test_program.compile
    "(map (fn [x y] (+ x y))
      (map (fn [x] (iterate 1 t (fn [a] (if (> x 1.)
        (error \"E_TAIL\" \"producer tail\") a)))) (array/range 3)) (array/float 1))"in
  assert(List.length shortened.I.Core.Region.stages=1);
  assert(same_result(I.Packed.force ~strict:true shortened ~live)(I.Packed.Host.reference shortened ~live));
  print_endline "Loop fusion: producer/consumer scopes, guards, integer/text aliases and sibling outputs agree"

let () =
  List.iter(fun(name,body)->check ~times:[0.37] ~region:false
    (name,"(map (fn [x] "^body^") (array/range 1))"))[
    "iterate-carried-widen","(iterate 1 9007199254740993
      (fn [a] (+ (float (int/sub a 9007199254740992)) (* t 0.))))";
    "fold-carried-widen","(fold [a 9007199254740993] [i (range 1)]
      (+ (float (int/sub a 9007199254740992)) (* t 0.)))";
    "record-carried-widen","(let* [s (iterate 1 {:n 9007199254740993}
      (fn [a] {:n (+ (float (int/sub a.n 9007199254740992)) (* t 0.))}))] s.n)";
  ];
  print_endline "Carried Int/Float widening retains exact initial bits through structured Core"
