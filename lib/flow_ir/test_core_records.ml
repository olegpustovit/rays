module E = Flow.Eval
module I = Flow_ir
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let residual ?(inputs="") body =
  let source="(workspace w (graph g :context value " ^ inputs ^ " (let* [tested " ^ body ^ "] 0.)))" in
  let ws=match Flow.Workspace.check {Flow.Check.version=1;kinds=[]} (ok (Flow.Syntax.parse source)) with
    | Some ws,[] -> ws | _,ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds)) in
  match snd (List.hd (List.assoc ["g";"tested"] (ok (E.static ~record:true ws)).records)) with
  | E.Residual r -> r | _ -> failwith ("Expected a residual: " ^ body)
let () =
  if Array.length Sys.argv=2 && Sys.argv.(1)="--bench-records" then begin
    (* The Slice C record fold (lib/flow/test_workspace_eval.ml), prepared and forced through Executor's core path. *)
    let source="(workspace w (graph g :context value (let* [a (fold [a {:n 0.}] [x (array/range 4096)] (assoc a :n (+ a.n t)))] a.n)))" in
    let ws=match Flow.Workspace.check {Flow.Check.version=1;kinds=[]} (ok (Flow.Syntax.parse source)) with
      | Some ws,[] -> ws | _,ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds)) in
    let v=List.assoc "g" (ok (E.static ws)).results in
    let program=ok (I.Executor.compile v) in
    let force time=ignore (ok (I.Executor.force program ~live:(Input.at_time time))) in
    force 0.1;
    let samples=List.init 3 (fun _ ->
      Gc.full_major ();
      let bytes=Gc.allocated_bytes () and start=Unix.gettimeofday () in
      for i=1 to 30 do force (float i /. 100.) done;
      Unix.gettimeofday () -. start,Gc.allocated_bytes () -. bytes) in
    let seconds,bytes=List.nth (List.sort (fun (a,_) (b,_) -> Float.compare a b) samples) 1 in
    Printf.printf "Core record fold: %.6fs, %.0f allocated bytes; 30 frames x 4096 items; domains=1; median of 3\n" seconds bytes;
    exit 0
  end
let bits x y = Int64.bits_of_float x=Int64.bits_of_float y
let () =
  let value=E.Record ["values",E.Int_array [|1;0;1|];
    "nested",E.List [|E.Float(-0.);E.Text "raw\000é"|]] in
  List.iter(fun reference->
    let ticks=ref 0 in
    let profile=I.Profile.create ~clock:(fun()->incr ticks;float !ticks) in
    let program=ok(I.Executor.compile ~strict:true ~profile value) in
    let state=E.create_state() in let stamp=E.state_stamp state in
    let resolutions=ref 0 in
    let resolve _=incr resolutions;Error(Diagnostic.error ~code:"E_UNUSED" "unused resolver") in
    let result=I.Executor.force ~strict:true ~reference ~state ~resolve program
      ~live:(Input.at_time nan) |>ok in
    assert(result==value && E.state_stamp state=stamp && !resolutions=0 && !ticks=2);
    match I.Profile.executions profile with
    |[execution]->
      assert(execution.seconds=1. && execution.tier=(if reference then I.Interp else I.Core))
    |_->failwith "Concrete execution lost its profile entry") [false;true]
let rec same (a : E.value) (b : E.value) = match a,b with
  | Float a,Float b -> bits a b
  | Vec2 (a,b),Vec2 (x,y) -> bits a x && bits b y
  | Vec3 (a,b,c),Vec3 (x,y,z) -> bits a x && bits b y && bits c z
  | Vec4 (a,b,c,d),Vec4 (x,y,z,w) -> bits a x && bits b y && bits c z && bits d w
  | Record a,Record b -> List.length a=List.length b && List.for_all2 (fun (n,a) (m,b) -> n=m && same a b) a b
  | List a,List b -> Array.length a=Array.length b && Array.for_all2 same a b
  | a,b -> a=b
let cases = [
  "record",residual "(if (> t 0.) {:a t :b (+ t 1.)} {:a (- 0. t) :b 2.})";
  "nested",residual "(if (> t 0.) {:v {:x t :y [t 2.]} :label \"yes\"} {:v {:x (- 0. t) :y [0. t]} :label \"no\"})";
  "get",residual "(if (> t 0.) (get {:a t :b (* t 2.)} :b) (get {:a (- 0. t) :b 3.} :a))";
  "dotted",residual "(if (> t 0.) (let* [r {:inner {:v [t 2. 3. 4.]}}] r.inner.v.w) t)";
  "keys",residual "(if (> t 0.) (let* [{:keys [a b]} {:a t :b (* t 2.)}] (+ a b)) t)";
  "assoc-order",residual "(if (> t 0.) (assoc {:a t :b 2} :a 1.9 :c t :b (+ t 0.7)) {:a t :b 2 :c t})";
  "assoc-nested",residual "(if (> t 0.) (assoc {:r {:i 0 :v [1. 2.]} :text \"old\"} :r {:i t :v t :extra true} :text (str t)) {:r {:i 0 :v [0. 0.] :extra false} :text \"none\"})";
  "assoc-tag",residual "(let* [r (if (> t 0.) {:n 1} {:n 1.})] (assoc r :n 1.7))";
  "assoc-captured",residual "(let* [r {:n (int t)}] (assoc r :n 1.7))";
  "assoc-captured-branch",residual "(let* [r {:n (int t)} chosen (if (> t 0.) r r)] (assoc chosen :n 1.7))";
  "assoc-captured-nested",residual "(let* [r {:v {:n (int t)}}] (assoc r :v {:n 1.7}))";
  "assoc-captured-lazy",residual "(let* [r {:v {:n (int t)} :unused (exp (* t 100000.))} updated (assoc r :v {:n 1.7})] updated.v.n)";
  "branch-order",residual "(if (> t 0.) {:a t :b 2.} {:b 2. :a t})";
  "branch-order-show",residual "(str (if (> t 0.) {:a t :b 2.} {:b 2. :a t}))";
  "branch-order-assoc",residual "(let* [r (if (> t 0.) {:a 1 :b 2.} {:b 2. :a 1})] (assoc r :a t :c 3))";
  "branch-order-nested",residual "(if (> t -10.) (let* [r {:v {:a 0 :b 2.}} nv (if (> t 0.) {:a t :b 3} {:b 3 :a t})] (assoc r :v nv)) {:v {:a 0 :b 2.}})";
  "branch-order-get",residual "(let* [r (if (> t 0.) {:a t :b 2.} {:b 2. :a t})] (get r :a))";
  "text",residual "(if (> t 0.) \"a\\n\\\"\\\\é🙂\" \"b\")";
  "str",residual "(if (> t 0.) (str \"t=\" t \"/\" (int t) \"/\" (> t 1.) \"/\" [t -0.]) (str \"negative=\" t))";
  "str-record",residual "(if (> t 0.) (str {:a t :nested {:b [t 2.] :c \"text\"}}) (str {:a (- 0. t) :nested {:b [0. t] :c \"text\"}}))";
  "str-list",residual ~inputs:"[(empty : (list float) (list))]"
    "(if (> t 0.) (str (for [i (range 3)] (+ t i))) (str empty))";
  "typed-branch",residual "(if (> t -10.) (let* [f (fn [(x : float)] (equal? x 1.))] (f (if (> t 0.) 1 1.))) false)";
  "equal-record",residual "(if (> t 0.) (equal? {:a t :b {:text \"yes\"}} {:a t :b {:text \"yes\"}}) (equal? {:a t :b {:text \"yes\"}} {:b {:text \"yes\"} :a t}))";
  "equal-captures",residual "(let* [a {:v t} b {:v t}] (equal? a b))";
  "equal-capture-fresh",residual "(let* [a {:v {:x t}}] (if (> t 0.) (equal? a {:v {:x t}}) false))";
  "assoc-error",residual "(if (> t 0.) (assoc {:n 0} :n 10000000000000000000.) {:n 0})";
  "equal-tags",residual "(equal? (if (> t 0.) 1 1.) 1)";
  "equal-text",residual "(equal? (if (> t 0.) \"same\" \"other\") \"same\")";
  "captured",residual "(let* [r {:safe t :bad (exp (* t 100000.))}] (if (> t 0.) (get r :safe) t))";
  "captured-str",residual "(let* [r {:n t :v [t 2.]}] (if (> t 0.) (str r) \"none\"))";
]
let dump () = String.concat "" (List.map (fun (name,r) ->
  let core=match I.Core_lower.lower r with Ok p -> p | Error n -> failwith (name ^ ": Core refused " ^ n) in
  let program=ok (I.Executor.compile (E.Residual r)) in
  let printed=Format.asprintf "%a" I.Core.pp core in
  List.iter (fun time ->
    let live=Input.at_time time in
    let expected=E.Private.force_reference (E.Residual r) ~live in
    let equal_result (a : (E.value,Diagnostic.t) result) (b : (E.value,Diagnostic.t) result) = match a,b with
      | Ok a,Ok b -> same a b | Error a,Error b -> a.code=b.code | _ -> false in
    assert (equal_result (E.force (E.Residual r) ~live) expected);
    (let actual=I.Executor.force program ~live in
     if not (equal_result actual expected) then
       let show = function Ok v -> E.show v | Error d -> Diagnostic.to_string d in
       failwith (Printf.sprintf "%s at %g: %s <> %s" name time (show actual) (show expected)));
    match Test_program.run core ~live,expected with
    | Ok actual,Ok expected -> if not (same actual expected) then
        failwith (Printf.sprintf "%s at %.17g: %s <> %s" name time (E.show actual) (E.show expected))
    | Error a,Error b -> assert (a.code=b.code)
    | _ -> failwith (name ^ ": result mismatch"))
    [-3.;-2.5;-0.;0.;0.00005;0.00015;0.37;1.;1.9;100.25];
  name ^ "\n" ^ printed) cases)
let () =
  List.iter (fun body ->
    let r=residual body in
    let core=match I.Core_lower.lower r with Ok p->p|Error name->failwith name in
    let program=ok(I.Executor.compile(E.Residual r)) in
    List.iter (fun time ->
      let live=Input.at_time time in
      let expected=E.Private.force_reference(E.Residual r) ~live in
      List.iter (fun actual -> match actual,expected with
        | Ok a,Ok b->assert(same a b)
        | Error a,Error b->assert(a.Diagnostic.code=b.code && a.message=b.message && a.cause=b.cause)
        | _->failwith "Lazy record accumulator error changed")
        [Test_program.run core ~live;I.Executor.force program ~live]) [0.;1.])
    [ {|(fold [state {:n 0}] [i (range 3)]
          (if (> t 0.) (error "E_RECORD" "selected record error" :cause "record cause")
            (assoc state :n (int/add state.n 1))))|};
      {|(fold [state {:n 0}] [i (range 3)]
          (assoc (if (> t 0.) (error "E_RECORD" "selected record error" :cause "record cause") state)
            :n (int/add state.n 1)))|} ]
let () =
  let actual=dump () in
  if Array.length Sys.argv=2 && Sys.argv.(1)="--print" then print_string actual
  else begin
    let expected=In_channel.with_open_bin "core_records.expected" In_channel.input_all in
    if actual<>expected then failwith ("Core record/text print changed:\n" ^ actual);
    print_endline "Core records/text: ordered fields, nesting, coercion, runtime tags, display/equality and lazy captures passed"
  end
