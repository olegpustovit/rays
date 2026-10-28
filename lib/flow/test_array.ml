open Flow
let check ?(context = "value") body =
  let forms = Result.get_ok (Syntax.parse ("(workspace w (graph g :context " ^ context ^ " " ^ body ^ "))")) in
  match Workspace.check {Catalog.version = 1; kinds = []} forms with
  | Some w, [] -> w
  | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
let value ?context body = List.assoc "g" (Result.get_ok (Eval.run ~time:0. (check ?context body))).results
let () =
  let integers=[|min_int;min_int+1;-(1 lsl 53)-1;0;(1 lsl 53)+1;max_int|] in
  let packed=Value.array_init Ty.Int (Array.length integers) (fun i->Value.Int integers.(i)) in
  assert (packed=Value.Int_array integers);
  assert (Value.ty_of packed=Ty.Array Ty.Int);
  assert (Value.array_length packed=Array.length integers);
  Array.iteri (fun i n->assert (Value.array_get packed i=Value.Int n)) integers;
  Value.validate packed;
  assert (Value.show_with Fun.id packed="6 × int");
  assert (Value.key_of ~residual:Fun.id packed<>
    Value.key_of ~residual:Fun.id (Value.Float_array (Array.map float integers)));
  assert (Value.array_init Ty.Int 0 (fun _->assert false)=Value.Int_array [||]);
  List.iter (fun count->try ignore (Value.array_init Ty.Int count (fun _->assert false));assert false
    with Diagnostic.Error d -> assert(d.code="E_ARRAY_RANGE")) [-1;Sys.max_array_length+1];
  List.iter (fun index->try ignore (Value.array_get packed index);assert false
    with Diagnostic.Error d -> assert(d.code="E_ARRAY_RANGE")) [-1;Array.length integers];
  let source=Value.Source("typed",Ty.Array Ty.Int,["values",packed]) in
  assert (Value.ty_of source=Ty.Array Ty.Int && Value.coerce_to Ty.Float source=source);
  Value.validate source;
  assert (Value.key_of ~residual:Fun.id source<>
    Value.key_of ~residual:Fun.id (Value.Struct("typed",Ty.Array Ty.Int,["values",packed])))

let () =
  let rejects packed code message =
    let before = Marshal.to_string packed [Marshal.No_sharing] in
    (try Value.validate packed; failwith "invalid packed input accepted"
     with Diagnostic.Error d ->
       assert (d.code = code && d.message = message && d.span = None));
    assert (Marshal.to_string packed [Marshal.No_sharing] = before) in
  List.iter (fun (width, wrap) ->
    List.iter (fun values ->
      let before = Marshal.to_string values [Marshal.No_sharing] in
      Value.validate (wrap values);
      assert (Marshal.to_string values [Marshal.No_sharing] = before))
      [ [||]; Array.init (6 * width) (fun i ->
          [|0.; -0.; Float.max_float; -.Float.max_float;
            Int64.float_of_bits 1L; -.Int64.float_of_bits 1L|].(i mod 6)) ];
    let values = Array.init (width * 16384) (fun i -> if i mod 2 = 0 then -0. else float i *. 0.125) in
    let before = Marshal.to_string values [Marshal.No_sharing] in
    Value.validate (wrap values);
    assert (Marshal.to_string values [Marshal.No_sharing] = before);
    List.iter (fun invalid ->
      let values = Array.make (3 * width) 0.25 in
      (* Every coordinate, including the final one, must be checked. *)
      for i = 0 to Array.length values - 1 do
        values.(i) <- invalid;
        rejects (wrap values) "E_NONFINITE" "array input produced a nonfinite value.";
        values.(i) <- 0.25
      done) [nan; infinity; neg_infinity])
    [1, (fun xs -> Value.Float_array xs); 2, (fun xs -> Value.Vec2_array xs);
     3, (fun xs -> Value.Vec3_array xs); 4, (fun xs -> Value.Vec4_array xs)];
  List.iter (fun (packed, message) -> rejects packed "E_ARRAY_TYPE" message)
    [Value.Vec2_array [|nan|], "Packed vec2 storage has two coordinates per element.";
     Value.Vec3_array [|infinity;0.|], "Packed vec3 storage has three coordinates per element.";
     Value.Vec4_array [|neg_infinity;0.;0.|], "Packed vec4 storage has four coordinates per element."];
  let small = Value.Vec3_array (Array.make 3 0.25)
  and large = Value.Vec3_array (Array.make (3 * 16384) 0.25) in
  let allocation packed =
    let before = Gc.allocated_bytes () in
    Value.validate packed;
    Gc.allocated_bytes () -. before in
  let growth = allocation large -. allocation small in
  if growth >= 4096. then
    failwith (Printf.sprintf "packed validation allocation grows with input: %.0f bytes" growth)

let () =
  List.iter (fun (ty, value, count) ->
    let packed = Value.array_init ty count (fun _ -> value) in
    assert (Value.ty_of packed = Ty.Array ty);
    assert (Value.array_length packed = count);
    Value.validate packed;
    if count > 0 then assert (Value.array_get packed (count - 1) = value))
    [Ty.Vec2, Value.Vec2 (1.,-0.), 0; Ty.Vec2, Value.Vec2 (1.,-0.), 3;
     Ty.Vec4, Value.Vec4 (1.,2.,3.,4.), 0; Ty.Vec4, Value.Vec4 (1.,2.,3.,4.), 3];
  List.iter (fun packed ->
    try Value.validate packed; assert false with Diagnostic.Error d ->
      assert (d.code = "E_ARRAY_TYPE" || d.code = "E_NONFINITE"))
    [Value.Vec2_array [|1.|]; Value.Vec4_array [|1.;2.;3.|];
     Value.Vec2_array [|nan;0.|]; Value.Vec4_array [|0.;0.;infinity;1.|]];
  assert (value ~context:"host" "(array/nth (map (fn [x] [x (+ x 1)]) (array/range 3)) 2)" = Eval.Vec2 (2.,3.));
  assert (value ~context:"host" "(let* [a (map (fn [x] [x 2 3 4]) (array/range 3))]
    (array/nth (array/concat (array/slice a 2 1) (array/slice a 0 1)) 1))" = Eval.Vec4 (0.,2.,3.,4.));
  assert (value ~context:"host" "(array/sum (map (fn [x] [x 1]) (array/range 3)))" = Eval.Vec2 (3.,3.));
  assert (value ~context:"host" "(array/sum (map (fn [x] [x 1 2 3]) (array/range 3)))" = Eval.Vec4 (3.,3.,6.,9.));
  assert (value ~context:"host" "(array/sum (map (fn [x] [x 1 2 3]) (array/range 0)))" = Eval.Vec4 (0.,0.,0.,0.));
  assert (value "(array/count (array/float 10000))" = Eval.Int 10000);
  assert (value "(array/sum (array/float 10000 2))" = Eval.Float 20000.);
  assert (value "(let* [p (array/nth (array/vec3 10000 [1 2 3]) 9999)] p.x)" = Eval.Float 1.);
  assert (value "(array/sum (map (fn [x] (+ x 1)) (array/range 10000)))" = Eval.Float 50005000.);
  assert (value "(fold [s 0.0] [x (array/float 10000 2)] (+ s x))" = Eval.Float 20000.);
  assert (value "(sum [x (array/float 10000 2)] x)" = Eval.Float 20000.);
  assert (value "(array/count (for [x (array/range 10000)] [x 0 0]))" = Eval.Int 10000);
  assert (value "(array/count (filter (fn [x] (> x 4999)) (array/range 10000)))" = Eval.Int 5000);
  assert (value "(array/nth (sort-by (fn [x] (- 0 x)) (array/range 10)) 0)" = Eval.Float 9.);
  let w = check "(let* [p (state [prev (array/float 10000)] (map (fn [x] (+ x (frame/dt))) prev))] (array/sum p))" in
  let export () =
    let s = Eval.create_state () in
    Array.init 5 (fun frame -> List.assoc "g" (Result.get_ok (Eval.run ~state:s
      ~live:{(Input.at_time (float frame)) with frame; dt = 0.125} ~time:(float frame) w)).results) in
  assert (export () = [|Eval.Float 1250.; Float 2500.; Float 3750.; Float 5000.; Float 6250.|]);
  assert (Marshal.to_string (export ()) [] = Marshal.to_string (export ()) []);
  assert (Ty.of_string "array:vec3" = Some (Ty.Array Ty.Vec3));
  assert (Ty.of_string "array:text" = Some (Ty.Array Ty.Text));
  assert (Result.is_error (Eval.static (check "(array/count (array/float -1))")));
  assert (Result.is_error (Eval.static (check "(array/nth (array/float 10) 10)")));
  assert (Result.is_error (Eval.run ~time:0. (check "(sum [x (array/float 2 1e308)] x)")));
  let typed = check "(array/count (array/float (frame/index)))" in
  assert (List.assoc "g" (Result.get_ok (Eval.run ~live:{(Input.at_time 0.) with frame = 10000} ~time:0. typed)).results = Eval.Int 10000)

let () =
  (* Packed live maps/for/scan defer instead of trying to store residual boxes. *)
  assert (value "(array/sum (map (fn [x] (+ x t)) (array/float 10 2)))" = Eval.Float 20.);
  let live_value body = List.assoc "g" (Result.get_ok (Eval.run ~time:1.25 (check body))).results in
  assert (live_value "(array/sum (map (fn [x] (+ x t)) (array/float 10 2)))" = Eval.Float 32.5);
  assert (live_value "(array/sum (for [x (array/float 10 2)] (+ x t)))" = Eval.Float 32.5);
  assert (live_value "(array/sum (scan [s 0.0] [x (array/float 3 2)] (+ s (+ x t))))" = Eval.Float 19.5);
  assert (live_value "(let* [p (array/nth (map (fn [p] (+ p [t 0 0])) (array/vec3 3 [1 2 3])) 2)] p.x)" = Eval.Float 2.25);
  let bounded = check "(array/sum (map (fn [x] (sum [i (range 4096)] (+ x (+ i t)))) (array/float 1024)))" in
  (match Eval.run ~time:1. bounded with
   | Error d -> assert (d.Diagnostic.code = "E_EVAL_BUDGET")
   | Ok _ -> failwith "packed block bypassed the evaluation budget");
  assert (live_value "(array/sum (map (fn [x] (+ x t)) (array/float 100000 2)))" = Eval.Float 325000.);
  assert (live_value "(sum [x (array/float 100000 2)] (+ x t))" = Eval.Float 325000.);
  assert (live_value "(fold [s 0.0] [x (array/float 100000 2)] (+ s (+ x t)))" = Eval.Float 325000.);
  let forms = Result.get_ok (Syntax.parse
    "(workspace w (graph g :context value (map + (list 1 2) (array/float 2))))") in
  let _, ds = Workspace.check {Catalog.version = 1; kinds = []} forms in
  assert (List.exists (fun (d : Diagnostic.t) -> d.code = "E_ARRAY_TYPE") ds)

let () =
  let strings=[|"";"猫";"a\000b";"é";"😀";"quote\"\\\n"|] in
  let array=Value.array_init Ty.Text (Array.length strings) (fun i->Value.Text strings.(i)) in
  assert (array=Value.Text_array strings && Value.ty_of array=Ty.Array Ty.Text);
  assert (Value.array_length array=Array.length strings);
  Array.iteri (fun i text->assert (Value.array_get array i=Value.Text text)) strings;
  Value.validate array;
  assert (Value.show_with Fun.id array="6 × text");
  assert (Ty.of_syntax (Syntax.make (List [Syntax.make (Sym "array");Syntax.make (Sym "text")]))
    =Some(Ty.Array Ty.Text));
  assert (Value.array_init Ty.Text 0 (fun _->assert false)=Value.Text_array[||]);
  let source=Value.Source("texts",Ty.Array Ty.Text,["values",array]) in
  assert (Value.ty_of source=Ty.Array Ty.Text);
  assert (Value.key_of ~residual:Fun.id source<>Value.key_of ~residual:Fun.id array);
  let text=Syntax.make (Str "猫😀é") |> Lisp.flat in
  let expected=Eval.Text_array (Array.make 5001 "猫😀é") in
  assert (value ~context:"host" "(array/text 3)"=Eval.Text_array[|"";"";""|]);
  List.iter (fun body->assert(value ~context:"host" body=expected))
    ["(array/text 5001 "^text^")";
     "(map (fn [x] x) (array/text 5001 "^text^"))";
     "(for [x (array/text 5001 "^text^")] x)";
     "(scan [a \"\"] [x (array/text 5001 "^text^")] x)"];
  assert (value "(array/count (array/text 5001))"=Eval.Int 5001);
  assert (value ~context:"host" "(for [x (array/text 2 \"猫\") y (array/text 2501 \"😀\")] :skip [5001] y)"
    =Eval.Text_array(Array.make 5001 "😀"));
  assert (value ~context:"host" "(scan [a \"seed\"] [x (array/text 2 \"猫\") y (array/text 2501 \"😀\")] y)"
    =Eval.Text_array(Array.make 5002 "😀"));
  assert (value ~context:"host" ("(array/concat (array/text 2 "^text^") (array/text 1 \"other\"))")
    =Eval.Text_array[|"猫😀é";"猫😀é";"other"|]);
  assert (value ~context:"host" ("(array/slice (array/text 5001 "^text^") 4999 2)")
    =Eval.Text_array[|"猫😀é";"猫😀é"|]);
  assert (value "(fold [a 0] [x (array/text 5001)] (int/add a 1))"=Eval.Int 5001);
  assert (value "(reduce (fn [a x] (int/add a 1)) 0 (array/text 5001))"=Eval.Int 5001);
  List.iter (fun body->assert(value ~context:"host" body=Eval.Text_array[||]))
    ["(map (fn [x] x) (array/text 0))";"(for [x (array/text 0)] x)";
     "(scan [a \"seed\"] [x (array/text 0)] x)";"(array/slice (array/text 1) 1 0)";
     "(for [x (array/text 0) y (array/text -1)] y)";
     "(for [x (array/text 2) y (array/text 0)] y)";
     "(scan [a \"seed\"] [x (array/text 0) y (array/text -1)] y)";
     "(array/concat (array/text 0) (array/text 0))"];
  assert (value ~context:"host" "(fold [a \"seed\"] [x (array/text 0)] x)"=Eval.Text "seed");
  let bulk_calls=ref 0 in
  ignore (Eval.Residual.handle ~bulk:(fun _->incr bulk_calls;None) (fun () ->
    Result.get_ok(Eval.Private.static_with_requests
      (check ~context:"host" "(map (fn [x] x) (array/text 5001 \"猫\"))"))));
  assert(!bulk_calls=0);
  List.iter (fun body->assert(Result.is_error(Eval.static(check ~context:"host" body))))
    ["(array/text -1)";"(array/nth (array/text 0) 0)";
     "(array/slice (array/text 1) 0 2)";"(array/concat (array/text 1) (array/int 1))";
     "(array/sum (array/text 0))"];
  print_endline "Text arrays: exact UTF-8 values, empty operations and interpreter map/for/scan/fold/reduce beyond 4,096 pass"
