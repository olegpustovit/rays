module E = Flow.Eval
module I = Flow_ir
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let bits x y = Int64.bits_of_float x = Int64.bits_of_float y
let rec same (a : E.value) (b : E.value) = match a,b with
  | Float x,Float y -> bits x y
  | Vec3 (a,b,c),Vec3 (x,y,z) -> bits a x && bits b y && bits c z
  | List a,List b -> Array.length a=Array.length b && Array.for_all2 same a b
  | a,b -> a=b
(* Every value-context scalar operator the closure compiler accepts, with at least one
   live expression. The coverage check below fails when an operator is added without a case. *)
let cases = [
  "array/max_length","(int/sub (array/max_length :type int) (frame/index))";

  "text/repr","(text/repr (str \"é日本😀\\t\\n\\\\\\\"\" t))";
  "fma","(fma t 0.9999999999999998 -1.)";
  "float/next_after","(float/next_after t (- t 1.0))";
  "float/mul_checked","(float/mul_checked t 1.7976931348623157e308)";
  "float/fma_checked","(float/fma_checked t 1.7976931348623157e308 -1.7976931348623157e308)";
  "array/vec4","(array/nth (array/vec4 4 [t 1 2 3]) 2)";
  "text/table_decode","(text/table_decode (if (> t 0.) \"a\\t猫\\n\" \"\"))";
  "text/split","(array/nth (text/split (str \"a,é,\" t) \",\") 1)";
  "text/tokens","(array/nth (text/tokens (str \"a, é\\t\" t \"\\n\")) 1)";
  "text/to_float","(text/to_float (str \" -1.5e1\" (int t)))";
  "text/to_int","(text/to_int (str \" 4\" (int t)))";
  "text/table_encode","(text/table_encode (list (list (if (> t 0.) \"猫\\n\" \"a\\t\") \"\\\\\")))";
  "text/blank?","(text/blank? (if (> t 0.) \" \\t\\r\\n\" \" \"))";
  "array/grade","(array/nth (array/grade (array/float 13 t) :descending true) 0)";
  "array/index_of","(array/nth (array/index_of (array/int 13 (frame/index)) (array/int 13 3)) 0)";
  "array/compact", "(array/count (array/compact (array/int 13 (if (> t 0.) 1 0))))";
  "array/scan_sum", "(array/nth (array/scan_sum (array/int 13 (frame/index))) 13)";
  "array/scatter_add","(array/nth (array/scatter_add (array/int 7 0) (array/vec3 7 [t 1 2]) 3) 0)";
  "array/min","(array/min (array/float 7 t))";
  "array/max","(array/max (array/int 7 (frame/index)))";
  "array/segment_sum","(array/nth (array/segment_sum (array/vec3 5 [t 1 2]) (map (fn [x] (int x)) (array/range 6))) 3)";
  "array/segment_min","(array/nth (array/segment_min (array/float 5 t) (map (fn [x] (int x)) (array/range 6))) 3)";
  "array/segment_max","(array/nth (array/segment_max (array/int 5 (frame/index)) (map (fn [x] (int x)) (array/range 6))) 3)";
  "array/segment_argmax","(array/nth (array/segment_argmax (array/int 5 (frame/index)) (map (fn [x] (int x)) (array/range 6))) 3)";
  "tan","(tan t)"; "atan2","(atan2 t -0.3)";
  "asin","(asin (* t 0.1))"; "acos","(acos (* t 0.1))";
  "log","(log (+ t 4.0))"; "sign","(sign t)"; "hypot","(hypot t 3.0)";
  "dot","(dot [t 0.3 -0.2] [0.7 -0.2 1.1])";
  "cross","(cross [t 0.3 -0.2] [0.7 -0.2 1.1])";
  "normalize","(normalize [t 0.3 -0.2])";
  "frame/input","(count (get (frame/input) :keys))";
  "frame/dt","(+ (frame/dt) t)"; "frame/index","(* t (frame/index))";
  "frame/width","(* t (frame/width))"; "frame/height","(* t (frame/height))";
  "pointer/x","(+ t (pointer/x))"; "pointer/y","(+ t (pointer/y))";
  "pointer/down","(if (pointer/down) t 0.)"; "pointer/down","(if (pointer/down \"right\") t 0.)";
  "pointer/down","(if (pointer/down \"x1\") t 0.)"; "pointer/down","(if (pointer/down \"nope\") t 0.)";
  "key/down","(if (key/down \"space\") t 0.)"; "key/down","(if (key/down \"zzz\") t 0.)";
  "array/range","(array/nth (array/range 8) (+ 1 (int t)))";
  "array/float","(array/nth (array/float 4 t) 1)";
  "array/int","(array/nth (array/int 4 (frame/index)) 1)";
  "array/text","(array/nth (array/text 4 (if (> t 0.) \"猫\" \"😀\")) 1)";
  "array/text","(array/nth (array/text 4 (if (> t 0.) \"a\" \"b\")) 1)";
  "array/text","(+ t (array/count (array/text 0)))";
  "array/vec3","(array/nth (array/vec3 4 [t 1 2]) 2)";
  "array/count","(array/count (array/float (+ 3 (int t))))";
  "array/nth","(array/nth (array/range 8) (+ 2 (int t)))";
  "array/slice","(array/count (array/slice (array/range 10) (int t) 4))";
  "array/concat","(array/count (array/concat (array/range (+ 2 (int t))) (array/float 2)))";
  "array/sum","(array/sum (array/float 5 t))"; "array/sum","(array/sum (array/vec3 3 [t 1 2]))";
  "array/sum","(+ t (array/sum (array/float 0)))";
  "int/mul","(int/mul (int t) 3)"; "int/div","(int/div (int t) 2)"; "int/mod","(int/mod (int (* t 7)) 3)";
  "int/and","(int/and (int t) 6)"; "int/xor","(int/xor (int t) 5)";
  "int/add","(int/add (frame/index) 9007199254740993)";
  "int/sub","(int/sub (frame/index) 9007199254740993)";
  "int/or","(int/or (frame/index) 9007199254740993)";
  "int/shl","(int/shl (frame/index) 31)";
  "int/shr","(int/shr (int/sub (frame/index) 9007199254740993) 31)";
  "int/lt","(int/lt (frame/index) 9007199254740993)";
  "int/le","(int/le (frame/index) 9007199254740993)";
  "int/gt","(int/gt (frame/index) 9007199254740993)";
  "int/ge","(int/ge (frame/index) 9007199254740993)";
  "int/eq","(int/eq (frame/index) 9007199254740993)";
  "length","(length [t 1 2])"; "equal?","(equal? t 1.)"; "exp","(exp t)"; "exact","(exact (+ t 1))";
  "+","(+ t 1)"; "-","(- t 1)"; "*","(* t 3)"; "/","(/ t 3)"; "mod","(mod t 0.7)"; "pow","(pow t 2)";
  "min","(min t 1)"; "max","(max t 1)"; "sin","(sin t)"; "cos","(cos t)"; "sqrt","(sqrt (+ t 1))";
  "floor","(floor t)"; "ceil","(ceil t)"; "round","(round t)"; "int","(int (* t 10))";
  "float","(float (int t))"; "abs","(abs t)";
  "<","(if (< t 1) 1. 0.)"; ">","(if (> t 1) 1. 0.)"; "<=","(if (<= t 1) 1. 0.)";
  ">=","(if (>= t 1) 1. 0.)"; "=","(if (= t 1) 1. 0.)"; "and","(if (and (> t 0) (< t 2)) 1. 0.)";
  "or","(if (or (> t 2) (< t 0)) 1. 0.)"; "not","(if (not (> t 1)) 1. 0.)";
  "value/rand","(value/rand t 1 2)";
  "rand/at","(rand/at (frame/index) (int t))";
  "noise3","(noise3 [t 0.3 -0.2])";
  "color/hsl","(color/hsl (* t 60.) 0.75 0.5)";
  "color/gradient","(color/gradient (list \"#101426\" \"#f26444\") t)";
  "text/trim","(text/trim (str \" \\t éÉ日本 \" t \" \\n\"))";
  "text/lower","(text/lower (str \"ABZéÉΣİ日本😀\" t))";
  "error","(if (> t 0.) (error (str \"E_EXACT_\" (int t)) (str \"é日本😀 \" t)) 7.)";
  "value/hsv","(value/hsv (* t 0.37) 0.5 0.9)"; "value/hsv","(value/hsv (- 0 t) 1. 0.25)";
  "value/hsv","(value/hsv (* t 0.166) 0.7 0.8)"; "value/hsv","(value/hsv (+ t 5.) 0.3 1.)";
  "value/lerp","(value/lerp t 1 0.25)"; "value/lerp","(value/lerp (* t 1.3) 0.7 0.3)";
  "value/lerp","(value/lerp [t 1 2] [3 (* t 1.1) 5] (/ t 7))"; "value/polar","(value/polar t 2)";
  "range","(range (int (+ t 3)))"; "linspace","(linspace 0 t 5)"; "linspace","(linspace 0 (+ 1 t) 1)";
  "linspace","(linspace 1 t 0)"; "linspace","(linspace 0 t (int (* t 3)))";
  "count","(+ t (count (list 1 2 3)))"; "first","(first (list t 1 2))"; "first","(first (take (int t) (list t 1)))";
  "last","(+ t (last (list 1 2 3)))"; "last","(last (take (int t) (list t 1)))";
  "rest","(sum [x (rest (list t 1 2))] (* x t))"; "nth","(nth (list t 1 2) (int t))"; "reverse","(sum [x (reverse (list t 1 2))] (* x t))";
  "take","(sum [x (take 2 (list t 1 2))] (* x t))"; "drop","(sum [x (drop 1 (list t 1 2))] (* x t))";
]
let times = [-3.;-0.;0.;0.37;1.;1.9;3.;7.5]
let lives = List.concat_map (fun time ->
  let base=Input.at_time time in
  [base; {base with buttons=[Input.LeftButton; Input.MouseX1]; keys=[Input.Space; Input.KeyChar 'a']; pointer=(12.,37.); size=(640,480); frame=3; dt=0.125}]) times
let () =
  let covered=List.sort_uniq compare (List.map fst cases) in
  List.iter (fun (o : Flow.Op.t) ->
    if o.ctx=Flow.Context.value && o.shape=Flow.Op.Scalar
      && not (List.mem o.name ["color/hsla"; "text/float"; "scene/trace"; "gpu/fwidth"; "gpu/sample"; "gpu/texture-size"])
      && not (List.mem o.name covered) then
      failwith ("No core operator case for " ^ o.name)) (Flow.Op.of_context Flow.Context.value);
  let n=ref 0 and static=ref [] in
  List.iter (fun (name,body) ->
    let source="(workspace w (graph g :context value (let* [tested " ^ body ^ "] 0.)))" in
    let ws=match Flow.Workspace.check {Flow.Catalog.version=1;kinds=[]} (ok (Flow.Syntax.parse source)) with
      | Some ws,[] -> ws | _,ds -> failwith (body ^ ": " ^ String.concat "; " (List.map Diagnostic.to_string ds)) in
    let value=snd (List.hd (List.assoc ["g";"tested"] (ok (E.static ~record:true ws)).records)) in
    match value with E.Residual r -> begin
    let core=match I.Core_lower.lower r with Ok p -> p | Error n -> failwith (name ^ " " ^ body ^ ": refused " ^ n) in
    List.iter (fun live ->
      incr n;
      match Test_program.run core ~live,E.Host.force_reference value ~live with
      | Ok a,Ok b -> if not (same a b) then failwith (Printf.sprintf "%s at %g: %s <> %s" body live.t (match a,b with E.Vec3 (x,y,z),E.Vec3 (p,q,r) -> Printf.sprintf "%h %h %h / %h %h %h" x y z p q r | _ -> E.show a) (E.show b))
      | Error (a : Diagnostic.t),Error (b : Diagnostic.t) ->
          if a.code<>b.code then failwith (Printf.sprintf "%s at %g: %s <> %s" body live.t a.code b.code)
      | Ok a,Error b -> failwith (Printf.sprintf "%s at %g: %s but reference %s" body live.t (E.show a) b.code)
      | Error a,Ok b -> failwith (Printf.sprintf "%s at %g: %s but reference %s" body live.t a.code (E.show b))) lives end
    | _ -> static := body :: !static) cases;
  if !static<>[] then failwith ("Not live: " ^ String.concat " | " !static);
  Printf.printf "Core operators: every scalar value operator lowers and matches the walker bit for bit (%d cases)\n" !n

let () =
  let live_value body =
    let source="(workspace text_arrays (graph g :context host (if (> t -1000.) " ^ body ^ " " ^ body ^ ")))" in
    let ws=match Flow.Workspace.check {Flow.Catalog.version=1;kinds=[]} (ok(Flow.Syntax.parse source)) with
      | Some ws,[]->ws | _,ds->failwith(String.concat "\n"(List.map Diagnostic.to_string ds)) in
    List.assoc "g" (ok(E.static ws)).results in
  List.iter (fun body ->
    let value=live_value body in
    let residual=match value with E.Residual r->r|_->failwith "live text result expected" in
    let core=match I.Core_lower.lower ~strict:true residual with
      |Ok core->core|Error form->failwith(body^": strict Core refused "^form) in
    (* Text leaves may collect in a region; Text transformations and scans
       retain the structured Core path. Both execute without a walker retry. *)
    let program=ok(I.Executor.compile ~strict:true value) in
    List.iter(fun time->
      let live=Input.at_time time in
      let expected=ok(E.Host.force_reference value ~live) in
      assert(ok(Test_program.run core ~live)=expected);
      List.iter(fun domains->Parallel.run ~domains(fun()->
        assert(ok(I.Executor.force ~strict:true program ~live)=expected))) [1;8]) [-1.;0.;1.])
    ["(map (fn [x] (if (> t 0.) x \"😀\")) (array/text 5001 \"猫\"))";
     "(map (fn [x] (text/trim (if (> t 0.) x \"　A \"))) (array/text 5001 \" \\tABéÉ日本😀\\r\\n\"))";
     "(map (fn [x] (text/lower (if (> t 0.) x \"ÉΣİZ\"))) (array/text 5001 \"ABéÉ日本😀\"))";
     "(map (fn [x] (text/repr (if (> t 0.) x \"ÉΣİZ\"))) (array/text 5001 \"ABéÉ日本😀\"))";
     "(for [x (array/text 5001 \"猫\")] (if (> t 0.) x \"😀\"))";
     "(scan [a \"\"] [x (array/text 5001 \"猫\")] (if (> t 0.) x \"😀\"))";
     "(for [x (array/text 2 \"猫\") y (array/text 2501 \"😀\")] :skip [5001] (if (> t 0.) x y))";
     "(scan [a \"seed\"] [x (array/text 2 \"猫\") y (array/text 2501 \"😀\")] (if (> t 0.) x y))";
     "(for [x (array/text 0) y (array/text -1)] (if (> t 0.) x y))";
     "(for [x (array/text 2) y (array/text 0)] (if (> t 0.) x y))";
     "(scan [a \"seed\"] [x (array/text 0) y (array/text -1)] (if (> t 0.) x y))";
     "(fold [a 0] [x (array/text 5001)] (int/add a (if (> t 0.) 1 2)))";
     "(map (fn [x] (if (> t 0.) x \"😀\")) (array/text 0))"];
  let dependent=live_value
    "(for [x (array/text 2 \"猫\") y (array/text (if (equal? x \"猫\") 2501 0) \"😀\")] (if (> t 0.) x y))" in
  let residual=match dependent with E.Residual r->r|_->assert false in
  assert(I.Core_lower.lower ~strict:true residual=Error "text_collect_dependent_clause");
  assert(Result.is_error(I.Executor.compile ~strict:true dependent));
  assert(ok(E.Host.force_reference dependent ~live:(Input.at_time 1.))
    =E.Text_array(Array.make 5002 "猫"));
  let nested_numeric=live_value
    "(map (fn [x] (text/trim (text/lower (str (array/sum (map (fn [y] (value/rand y t)) (array/float 2))))))) (array/text 2))" in
  let residual=match nested_numeric with E.Residual r->r|_->assert false in
  (match I.Core_lower.lower ~strict:true residual with
   | Ok program->assert(Array.length program.regions>0)
   | Error form->failwith("Nested numeric Text body refused as "^form));
  (match I.Executor.compile ~strict:true nested_numeric with
   | Error diagnostic->assert(diagnostic.Diagnostic.code="E_KERNEL_FORM")
   | Ok _->failwith "Unsupported numeric producer inside Text map qualified");
  assert(match ok(E.Host.force_reference nested_numeric ~live:(Input.at_time 1.)) with
    |E.Text_array values->Array.length values=2 |_->false);
  print_endline "Text arrays: strict structured Core and Executor match reference beyond 4,096 at one/eight domains; dependent collecting clauses refuse honestly"
