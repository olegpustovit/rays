module E = Flow.Eval
module I = Flow_ir
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let read path = In_channel.with_open_bin path In_channel.input_all
let catalog = fst (ok (Flow.Check.catalog_of_manifest (read "../sop_catalog/flow_manifest.sexp")))
let check source = match Flow.Workspace.check catalog (ok (Flow.Syntax.parse source)) with
  | Some ws,ds when not (List.exists (fun (d : Diagnostic.t) -> d.severity=Diagnostic.Error_severity) ds) -> ws
  | _,ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
let value body = List.assoc "g" (ok (E.static (check ("(workspace w (graph g :context value "^body^"))")))).results
let bits x y = Int64.bits_of_float x = Int64.bits_of_float y
let rec same_bits (a : E.value) (b : E.value) = match a,b with
  | Float x,Float y -> bits x y
  | Vec2 (a,b),Vec2 (x,y) -> bits a x && bits b y
  | Vec3 (a,b,c),Vec3 (x,y,z) -> bits a x && bits b y && bits c z
  | Vec4 (a,b,c,d),Vec4 (x,y,z,w) -> bits a x && bits b y && bits c z && bits d w
  | List xs,List ys -> Array.length xs=Array.length ys && Array.for_all2 same_bits xs ys
  | Float_array xs,Float_array ys | Vec2_array xs,Vec2_array ys
  | Vec3_array xs,Vec3_array ys | Vec4_array xs,Vec4_array ys ->
      Array.length xs=Array.length ys && Array.for_all2 bits xs ys
  | Record fs,Record gs | Struct (_,_,fs),Struct (_,_,gs) ->
      List.length fs=List.length gs && List.for_all2 (fun (n,a) (m,b) -> n=m && same_bits a b) fs gs
  | Fn _,Fn _ -> true | a,b -> a=b
let same a b = match a,b with
  | Ok a,Ok b -> same_bits a b | Error a,Error b -> a=b | _ -> false
let direct=ref 0
let compare value =
  let program=ok (I.Executor.compile value) in
  List.iter (fun time ->
    let live=Input.at_time time in
    let reference=E.Private.force_reference value ~live in
    assert (same (I.Executor.force program ~live) reference);
    match value with
    | E.Residual r -> (match I.Core_lower.lower r with
        | Ok core -> ((match Test_program.run core ~live,reference with
            | Ok actual,Ok expected -> incr direct;assert (same_bits actual expected)
            | Error _,Error _ -> () | _ -> assert false);
            ())
        | Error _ -> ())
    | _ -> ()) [0.;0.37;1.9;6.28;100.25; -3.]
let () =
  List.iter (fun name ->
    let evaluation=ok (E.static (check (read ("../../specification/workspace/cases/"^name^".lisp")))) in
    List.iter (fun (_,v) -> compare v) evaluation.results;
    Array.iter (fun (node : E.node) -> List.iter (fun (_,v) -> compare v) node.args) evaluation.plan.nodes)
    ["wave";"orrery";"sunflower";"bloom";"tiles"];
  List.iter (fun body -> compare (value body)) [
    "(sum [k (range 6)] (/ (sin (* (+ t k) (+ (* 2 k) 1))) (+ (* 2 k) 1)))";
    "(let* [a 3 b (* a 2)] (+ (* t b) (mod t 0.7)))";
    "(if (> (sin t) 0.2) (pow t 2) (- 0 t))";
    "(let* [v [(* t 2) (+ t 1) (min t 3)]] (+ v.x v.y))";
    "(nth (for [i (range 4)] (* i t)) 2)";
    "(sum [x (for [i (range 4)] (+ (* i t) 1))] (* x 2))";
    "(sum [i (range 5)] (sum [j (range 3)] (* (+ t i) j)))";
    "(let* [a t a1 (+ a a) a2 (+ a1 a1) a3 (+ a2 a2)] (* a3 0.5))";
    "(let* [c (value/lerp [1 2 3] [t 0 1] 0.25)] c.z)";
    "(+ (* (sin t) 0.5) (* t t))";"(- (sqrt t) (cos (* t 3)))";"(* t 2)";"(- (+ (sin t) t) t)";"(+ (* 3 4) t)";"(* 2 (- t 1.5))";
    "(let* [a (* t 100000000000.) b (* a a) c (* b b) d (* c c) e (* d d) f (* e e)] (- f (sin t)))";
    "(sum [i (range 0)] t)";"(/ t 0)";
    "(sqrt (/ (- t 2) 0))";"(exp (* t 100000.))";"(int (* t 10000000000000000000.))"];
  assert (!direct>0);
  (* Repeated forces use the stateless shortcut, but ownership, invalid frames
     and a later arithmetic failure still take their original diagnostics. *)
  (let v=value "(exp t)" in
    let a=ok (I.Executor.compile v) and b=ok (I.Executor.compile v) in
    List.iter (fun time ->
      let live=Input.at_time time in
      let expected=E.Private.force_reference v ~live in
      assert (same (I.Executor.force a ~live) expected);
      assert (same (I.Executor.force a ~live) expected);
      assert (same (I.Executor.force b ~live) expected)) [0.;1.;10000.;2.];
    let live={(Input.at_time 1.) with dt=nan} in
  assert (same (I.Executor.force a ~live) (E.Private.force_reference v ~live)));
  Printf.printf "Core parity: fixtures, scalar expressions, direct execution (%d cases), and diagnostics passed\n" !direct
