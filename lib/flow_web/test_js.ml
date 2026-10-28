(* Goldens for [Js.region], and a run of every emitted region function under
   node against native [Packed.force_all]. Regenerate goldens with
   [test_js.exe --write-goldens] from lib/flow_web. *)
module P = Flow_ir.Packed
module R = Flow_ir.Core.Region

let node = Sys.command "command -v node >/dev/null 2>&1" = 0
let numbers values = String.concat "," (List.map ((fun value -> Param.float_text value)) (Array.to_list values))

let flatten = function
  | Flow.Eval.Float_array a | Vec2_array a | Vec3_array a | Vec4_array a -> a
  | Float x -> [|x|] | Vec2 (x, y) -> [|x; y|] | Vec3 (x, y, z) -> [|x; y; z|]
  | Vec4 (x, y, z, w) -> [|x; y; z; w|]
  | _ -> failwith "unexpected value"

let close a b = a = b || Float.abs (a -. b) <= 1e-12 *. Float.max (Float.abs a) (Float.abs b)

(* Runs the region in two ranges (to exercise the carried state) and returns
   the status and one array per output line. *)
let run_node_raw ?(fast=false) program =
  let live = {(Input.at_time 1.) with frame=3} in
  let region = program and view = P.Host.view program in
  let inputs = Test_program.ok (P.Host.prepare_values program ~live) in
  let count = if region.iteration = R.Product then
      Array.fold_left ( * ) 1 (Array.map Flow.Value.array_length inputs.arrays) else inputs.count in
  let valid = List.sort_uniq compare (List.filter (fun i -> i >= 0 && i < count) (Array.to_list region.skip)) in
  let kept = count - List.length valid in
  let collecting = match region.result with R.Collect | R.Accumulate (_, true) -> true | _ -> false in
  let width = Array.length view.outputs.(0) in
  let outputs = (if collecting then Array.to_list (Array.map (fun o -> kept * Array.length o) view.outputs) else [])
    @ (if region.result = R.Collect then [] else [width + 1]) in
  let script = Buffer.create 65536 in
  Printf.bprintf script "import {noise3} from %S;\n"
    ("file://" ^ Filename.concat (Sys.getcwd ()) "../web_export/runtime/prims.js");
  Buffer.add_string script (Flow_web.Js.region ~fast program);
  let array values = "Float64Array.from([" ^ numbers values ^ "])" in
  let value = function Flow.Eval.Int n -> string_of_int n ^ "n"
    | Float f -> Param.float_text f | _ -> failwith "scalar frame fact expected" in
  let packed = function Flow.Eval.Int_array values ->
      "BigInt64Array.from([" ^ String.concat "," (Array.to_list (Array.map (fun n -> string_of_int n ^ "n") values)) ^ "])"
    | v -> array (flatten v) in
  let uniform index v =
    let rec integer = function P.Int_uniform n -> n=index | Guarded (_,body) -> integer body | _ -> false in
    if Array.exists integer view.code then "[" ^ value v ^ "]"
    else match v with Flow.Eval.Int_array _ -> packed v
      | _ -> array (match v with Flow.Eval.Int n -> [|float n|] | _ -> flatten v) in
  Printf.bprintf script "const inputs = [%s];\n" (String.concat "," (List.map packed (Array.to_list inputs.arrays)));
  Printf.bprintf script "const uniforms = [%s];\n"
    (String.concat "," (Array.to_list (Array.mapi uniform inputs.uniforms)
      @ ["[" ^ String.concat "," (Array.to_list (Array.map value inputs.frame)) ^ "]"]));
  Printf.bprintf script "const outputs = [%s];\n"
    (String.concat "," (List.mapi (fun k length ->
      let integer=R.integer_register view.code.(view.outputs.(min k (Array.length view.outputs-1)).(0)) in
      if integer && region.result<>R.Collect && k=List.length outputs-1 then Printf.sprintf "Array(%d).fill(0n)" length else Printf.sprintf "new %s(%d)"
        (if integer then "BigInt64Array" else "Float64Array") length) outputs));
  (match region.result with
   | R.Sum false -> Printf.bprintf script "outputs[outputs.length - 1][%d] = 0;\n" width
   | R.Sum zero -> if zero then Printf.bprintf script "outputs[outputs.length - 1][%d] = 1;\n" width
   | R.Accumulate (seed,_) ->
       let seed=Test_program.ok (Flow.Eval.Residual.eval_term region.residual seed ~live) in
       let values=match seed with Flow.Eval.Int _ -> [|value seed|]
         | _ -> Array.map ((fun value -> Param.float_text value)) (flatten seed) in
       Array.iteri (fun i seed -> Printf.bprintf script "outputs[outputs.length - 1][%d] = %s;\n" i seed) values;
       Printf.bprintf script "outputs[outputs.length - 1][%d] = 1;\n" width
   | R.Collect -> ());
  Printf.bprintf script "let status = region(inputs, uniforms, outputs, 0, %d);\n" (count / 2);
  Printf.bprintf script "if (status === 0) status = region(inputs, uniforms, outputs, %d, %d);\n" (count / 2) count;
  Buffer.add_string script "const str = (x) => Object.is(x, -0) ? '-0' : String(x);\nconsole.log(status);\n";
  Buffer.add_string script "for (const o of outputs) console.log(Array.from(o).map(str).join(' '));\n";
  let file = Filename.temp_file "region" ".mjs" and out = Filename.temp_file "region" ".out" in
  Out_channel.with_open_bin file (fun c -> output_string c (Buffer.contents script));
  assert (Sys.command (Printf.sprintf "node %s > %s" (Filename.quote file) (Filename.quote out)) = 0);
  let lines = String.split_on_char '\n' (In_channel.with_open_bin out In_channel.input_all) in
  Sys.remove file; Sys.remove out;
  match lines with
  | status :: rest ->
      int_of_string status,
      List.take (List.length outputs) rest
      |> List.map (fun l -> String.split_on_char ' ' l |> List.filter ((<>) "") |> Array.of_list)
  | [] -> assert false

let run_node ?fast program = let status,outputs=run_node_raw ?fast program in
  status,List.map (Array.map float_of_string) outputs

let compare_integer ?fast source =
  let program=try Test_program.compile source with Failure message -> failwith (source ^ ": " ^ message) in
  let live={(Input.at_time 1.) with frame=3} in
  let status,produced=run_node_raw ?fast program in
  assert (status=0);
  let expected=Test_program.ok (P.force_all ~strict:true program ~live) in
  let output=match produced with output::_ -> output | _ -> assert false in
  List.iter (fun value -> let values=match value with
    | Flow.Eval.Int_array values -> values | Int n -> [|n|] | _ -> failwith "Integer result expected" in
    assert (Array.length output>=Array.length values);
    Array.iteri (fun i n -> if output.(i)<>string_of_int n then
      failwith (Printf.sprintf "Integer JS mismatch at %d: %s <> %d" i output.(i) n)) values) expected;
  let reference=P.Host.reference program ~live |> Test_program.ok in
  assert (expected=[reference])

let compare_native ?fast name program =
  let status, produced = run_node ?fast program in
  assert (status = 0);
  let live = {(Input.at_time 1.) with frame=3} in
  let expected = Test_program.ok (P.force_all program ~live) |> List.map flatten in
  let region = program in
  let compared = ref 0 and exact = ref 0 in
  let check actual wanted =
    Array.iteri (fun i w ->
      incr compared;
      if actual.(i) = w then incr exact;
      if not (close actual.(i) w) then
        failwith (Printf.sprintf "%s: element %d is %.17g, native %.17g" name i actual.(i) w)) wanted in
  (match region.result with
   | R.Collect | R.Accumulate (_, true) ->
       List.iteri (fun k wanted -> assert (Array.length (List.nth produced k) >= Array.length wanted); check (List.nth produced k) wanted) expected
   | _ -> check (List.nth produced 0) (List.hd expected));
  Printf.printf "  %-14s %d values, %d bit-equal\n" name !compared !exact

let programs =
  let range = "(array/range 1024)" in
  let map body = "(map (fn [x] " ^ body ^ ") " ^ range ^ ")" in
  Test_program.fixtures 1024
  @ ["two_outputs", Test_program.two_outputs ()]
  @ List.map (fun (name, source) -> name, (try Test_program.compile source with Failure m -> failwith (name ^ ": " ^ m)))
    (List.map (fun name -> "op " ^ name, map ("(" ^ name ^ " x t)")) ["/"; "mod"; "pow"; "min"; "max"]
     @ List.map (fun name -> "op " ^ name, map ("(" ^ name ^ " (+ x t))")) ["sin"; "cos"; "sqrt"; "abs"]
     @ ["logic", map "(if (and (> x t) (or (< x 8) (not (>= x 500)))) x (- 0 x))";
        "cond", map "(cond (< x t) (+ x 1) (> x 5) (- x 1) :else (* x t))";
        "vec3", map "[x t (/ x 3)]";
        "noise octaves", map "(noise3 [(* x 0.37) (* t 1.3) (- 0.0 4.2)] :seed 5 :octaves 4)";
        "noise neg", map "(noise3 [(* x (- 0.0 0.37)) (- t 3.0) (- 0.0 40.5)] :seed 7)";
        "sum", "(sum [x " ^ range ^ "] (sin (+ x t)))";
        "sum vec", "(sum [x " ^ range ^ "] [x t])";
        "fold", "(fold [a 0.0] [x " ^ range ^ "] (- (* a 0.99) (* (+ x t) 0.0001)))";
        "scan", "(scan [a 0.0] [x " ^ range ^ "] (- (* a 0.99) (* (+ x t) 0.0001)))";
        "scan vec", "(scan [a [0.0 0.0]] [x " ^ range ^ "] (+ (* a 0.99) [x t]))";
        "skip", "(for [x " ^ range ^ "] :skip [0 5 1023] (sin (+ x t)))";
        "skip two", "(for [x " ^ range ^ "] :skip [0 700] (* x t))";
        "product", "(for [x (array/range 32) y (array/range 32)] (+ (* x y) t))"])

let carried = List.map(fun(name,body)->name,Test_program.compile
  ("(map (fn [x] "^body^") (array/range 65))"))[
  "carried dynamic","(fold [a t] [i (range -2 (int/mod (int x) 5))] (+ a (* i 0.5)))";
  "carried empty","(fold [a t] [i (range 0)] (error \"E_UNTAKEN\" \"empty loop\"))";
  "carried nested","(fold [a t] [i (range 3)] (fold [b a] [j (range i)] (+ b (+ x j))))";
  "carried record","(let* [s (fold [s {:a 1 :b 2 :v [t 1.] :flag false :text \"start\"}]
    [i (range (int/mod (int x) 5))] {:a s.b :b s.a :v [s.v.y (+ s.v.x 1.)]
      :flag (not s.flag) :text (if s.flag \"a\" \"b\")})]
    [(float s.a) (float s.b) s.v.x (if s.flag 1. 0.)])";
  "carried guarded","(if (> x t) (fold [a t] [i (range 2)] (+ a x)) x)";
  "carried scope","(let* [a (fold [s t] [i (range 2)] (+ s (+ x 1.)))] (+ a (+ x 1.)))";
  "iterate-empty-error","(iterate -1 t (fn [a] (error \"E_EMPTY\" \"untaken body\"))
    :until (fn [prev next] (error \"E_EMPTY\" \"untaken predicate\")))";
] @ List.filter(fun(name,_)->String.starts_with ~prefix:"iterate-" name)(Test_program.carried_fixtures 65)

let () =
  let writing = Array.to_list Sys.argv = [Sys.argv.(0); "--write-goldens"] in
  List.iter (fun name ->
    let program = List.assoc name programs in
    let source = Flow_web.Js.region program in
    let path = "goldens/" ^ name ^ ".js" in
    if writing then Out_channel.with_open_bin path (fun channel -> output_string channel source)
    else assert (In_channel.with_open_bin path In_channel.input_all = source);
    assert (source = Flow_web.Js.region program))
    ["arithmetic"; "noise"; "select"; "two_outputs"];
  if not node then print_endline "JS regions: goldens passed; node is absent, so the native comparison was SKIPPED"
  else begin
    let caused=Test_program.compile "(map (fn [x] (if (> x t) (error \"E_CAUSE\" \"selected\" :cause \"reason\") x)) (array/range 2))" in
    let file=Filename.temp_file "region-cause" ".js" in
    Fun.protect ~finally:(fun()->Sys.remove file)(fun()->
      Out_channel.with_open_bin file(fun channel->
        output_string channel(Flow_web.Js.region caused);
        Printf.fprintf channel "const inputs = [Float64Array.from([0,1])], uniforms = [new Float64Array(%d).fill(0.5)], outputs = [new Float64Array(2)];\n" (Array.length(P.Host.view caused).code);
        output_string channel {|
if (region(inputs, uniforms, outputs, 0, 0) !== 0 || region(inputs, uniforms, outputs, 0, 1) !== 0) throw new Error('untaken cause raised');
try { region(inputs, uniforms, outputs, 1, 2); throw new Error('selected cause did not raise'); }
catch (e) { if (e.code !== 'E_CAUSE' || e.message !== 'selected' || e.cause !== 'reason') throw e; }
|});
      if Sys.command("node "^Filename.quote file)<>0 then failwith(In_channel.with_open_bin file In_channel.input_all));
    print_endline "JS regions under node against native Packed.force_all:";
    List.iter (fun (name, program) -> compare_native name program) programs;
    List.iter(fun(name,program)->compare_native name program;compare_native ~fast:true (name^" fast")program)carried;
    let uint32=List.assoc "loop-uint32" (Test_program.carried_fixtures 65)in
    compare_native "uint32" uint32;compare_native ~fast:true "uint32 fast" uint32;
    List.iter(fun fast->compare_integer ~fast
      "(map (fn [x] (let* [lo (int/add 9007199254740993 (frame/index))]
        (fold [a (frame/index)] [i (range lo (int/add lo 3))] (int/add a i)))) (array/range 65))") [false;true];
    List.iter compare_integer [
      "(map (fn [x] (int/add x (frame/index))) (array/int 32769 4611686018427387903))";
      "(map (fn [x] (int/sub x (frame/index))) (array/int 32769 -4611686018427387904))";
      "(map (fn [x] (int/xor x (int/add 9007199254740993 (frame/index)))) (array/range 32769 :type int))";
      "(map (fn [x] (int/shr (int/add x (int/sub (frame/index) 9007199254740993)) 31)) (array/range 1025 :type int))";
      "(map (fn [x] (floor (+ x t))) (array/range 1025))";
      "(array/sum (map (fn [x] (int/add x (frame/index))) (array/int 1025 9007199254740993)))";
      "(scan [a (frame/index)] [x (array/range 1025 :type int)] (int/add a x))";
      "(fold [a (frame/index)] [x (array/range 1025 :type int)] (int/add a x))"];
    (* Fast precision's 64-bit register pairs agree with native wherever native's 63 bits do not wrap,
       far past 2^53 where plain numbers would not *)
    List.iter (compare_integer ~fast:true) [
      "(map (fn [(x : int)] (int/and (int/xor (int/mul x 73856093) (int/mul (int/mul x x) 19349663)) 4611686018427387903)) (array/range (int/add 70000 (frame/index)) :type int))";
      "(map (fn [(x : int)] (int/mod (int/mul (int/add x 1103515245) 2654435761) 1000003)) (array/range (int/add 4097 (frame/index)) :type int))";
      "(map (fn [(x : int)] (int/div (int/mul (int/add x (frame/index)) 3037000493) 7)) (array/range (int/add 4097 (frame/index)) :type int))";
      "(map (fn [(x : int)] (int/mod (int/sub 0 (int/mul x 2654435761)) 1000)) (array/range (int/add 4097 (frame/index)) :type int))";
      "(map (fn [(x : int)] (int/div (int/sub 0 (int/mul x 2654435761)) 1000)) (array/range (int/add 4097 (frame/index)) :type int))";
      "(map (fn [(x : int)] (int/shr (int/mul x 2654435761) 7)) (array/range (int/add 4097 (frame/index)) :type int))";
      "(map (fn [(x : int)] (int/shl x 40)) (array/range (int/add 4097 (frame/index)) :type int))";
      "(map (fn [(x : int)] (if (int/lt (int/mul x 2654435761) 5000000000000) (int/mul x 3) (int/sub 0 x))) (array/range (int/add 4097 (frame/index)) :type int))";
      "(map (fn [x] (int (* x 1.5))) (array/range (int/add 4097 (frame/index))))";
      "(map (fn [(x : int)] (int/add (array/nth (array/int 4 9007199254740993) (int/mod x 4)) x)) (array/range (int/add 4097 (frame/index)) :type int))"];
    (* A non-finite value is a status, not an exception, and native refuses it too. *)
    let overflow = Test_program.compile "(map (fn [x] (pow (+ x t) 1000000)) (array/range 1024))" in
    assert (fst (run_node overflow) = 1);
    assert (Result.is_error (P.force overflow ~live:(Input.at_time 1.)));
    print_endline "JS regions: goldens, node/native comparison and nonfinite status passed"
  end
