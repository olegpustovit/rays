let ok = function Ok value -> value | Error error -> failwith (Diagnostic.to_string error)

let compile ?trace ?(fusion = true) ?(declarations = "") body =
  let source =
    "(workspace w " ^ declarations ^ " (graph g :context value (let* [tested " ^ body ^ "] 0.0)))"
  in
  let forms = ok (Flow.Syntax.parse source) in
  let workspace =
    match Flow.Workspace.check ~ops:[] { Flow.Catalog.version = 1; kinds = [] } forms with
    | Some workspace, [] -> workspace
    | _, errors -> failwith (String.concat "\n" (List.map Diagnostic.to_string errors))
  in
  let evaluated = ok (Flow.Eval.static ~record:true workspace) in
  match List.assoc [ "g"; "tested" ] evaluated.records |> List.hd |> snd with
  | Flow.Eval.Residual residual -> (
      match
        Flow_ir.Packed.compile_result ?trace ~fusion residual
          (Flow.Eval.Residual.residual_view residual).term
      with
      | Ok program -> program
      | Error errors -> failwith (String.concat "\n" (List.map Diagnostic.to_string errors)))
  | _ -> failwith "expected live packed program"

let fixtures ?trace count =
  let array = "(array/range " ^ string_of_int count ^ ")" in
  [
    ("arithmetic", compile ?trace ("(map (fn [x] (+ (* x 2) t)) " ^ array ^ ")"));
    ("select", compile ?trace ("(map (fn [x] (if (< x t) (+ x 1) (- x 1))) " ^ array ^ ")"));
    ( "noise",
      compile ?trace
        ("(map (fn [x] (noise3 [(* x 0.02) t 0.25] :seed 31 :octaves 3)) " ^ array ^ ")") );
  ]

let vector_fixtures ?trace count =
  let array = "(array/range " ^ string_of_int count ^ ")" in
  [
    ("vec2", compile ?trace ("(map (fn [x] [x (+ x t)]) " ^ array ^ ")"));
    ( "vec4",
      compile ?trace ~fusion:false
        ("(map (fn [(p : vec2)] [p.y p.x t 1])\n     (map (fn [x] [x (+ x 1)]) " ^ array ^ "))") );
    ( "vec4-input",
      compile ?trace ~fusion:false
        ("(let* [offset [t 1 2 3]]\n\
         \     (map (fn [(p : vec4)] (+ p offset)) (map (fn [x] [x 1 2 3]) " ^ array ^ ")))") );
  ]

let int63_checks =
  "(let* [n (int/add (int x) (frame/index))\n\
  \  d (int/add (int/mod n 4) 1) even (int/mul d 2)\n\
  \  low (int/shl 1 (int/add 62 (int/mod n 1)))]\n\
  \  [(if (int/lt (int/add 4611686018427387903 d) 0) 1. 0.)\n\
  \   (if (int/eq (int/mul 4611686018427387903 even) (int/sub 0 even)) 1. 0.)\n\
  \   (if (int/eq (int/shr (int/sub 0 d) 62) 1) 1. 0.)\n\
  \   (if (and (int/lt (abs low) 0) (int/eq (int/div low -1) low)) 1. 0.)])"

let carried_fixtures count =
  List.map
    (fun (name, body) ->
      (name, compile ("(map (fn [x] " ^ body ^ ") (array/range " ^ string_of_int count ^ "))")))
    [
      ("loop-bounds", "(fold [a t] [i (range -2 (int/mod (int x) 5))] (+ a (* i 0.5)))");
      ("loop-empty", "(fold [a t] [i (range 0)] (+ a x))");
      ("loop-nested", "(fold [a t] [i (range 3)] (fold [b a] [j (range i)] (+ b (+ x j))))");
      ( "loop-record",
        "(let* [s (fold [s {:a 1 :b 2 :v [t 1.] :flag false}]\n\
        \      [i (range (int/mod (int x) 5))] {:a s.b :b s.a :v [s.v.y (+ s.v.x 1.)] :flag (not \
         s.flag)})]\n\
        \      [(float s.a) (float s.b) s.v.x (if s.flag 1. 0.)])" );
      ("loop-guard", "(if (> x t) (fold [a t] [i (range 2)] (+ a x)) x)");
      ("iterate-count", "(iterate (- (int/mod (int x) 7) 2) t (fn [a] (+ a x)))");
      ( "iterate-until",
        "(iterate 4611686018427387903 (+ x t) (fn [a] (+ a 1.))\n\
        \      :until (fn [prev next] (>= next (+ (+ x t) 3.))))" );
      ( "iterate-old-state",
        "(let* [s (iterate 4611686018427387903 {:flag true :n t}\n\
        \      (fn [a] {:flag false :n (+ a.n x)}) :until (fn [prev next] prev.flag))] s.n)" );
      ("iterate-int63", "(iterate 1 [t 0. 0. 0.] (fn [unused] " ^ int63_checks ^ "))");
      ( "loop-uint32",
        "(let* [s (fold [s {:state (int/and (int x) 4294967295) :value 0}]\n\
        \      [i (range 3)] (let* [state (int/and (int/add (int/mul s.state 747796405) \
         2891336453) 4294967295)\n\
        \        word (int/and (int/mul (int/xor (int/shr state (int/add (int/shr state 28) 4)) \
         state) 277803737) 4294967295)\n\
        \        value (int/and (int/xor (int/shr word 22) word) 65535)] {:state state :value \
         value}))]\n\
        \      (+ (float s.value) t))" );
    ]
  @ [
      ( "iterate-captured-function",
        compile
          ("(let* [offset t f (fn [a] (+ a offset))]\n\
           \    (map (fn [x] (iterate 3 x f)) (array/range " ^ string_of_int count ^ ")))") );
    ]
  @ List.map
      (fun (name, body) -> (name, compile (Printf.sprintf body count)))
      [
        ( "iterate-fused-producer",
          "(map (fn [y] (if (> y t) (* y 2.) t))\n\
          \      (map (fn [x] (iterate 3 (+ x t) (fn [a] (+ a 1.)))) (array/range %d)))" );
        ( "iterate-fused-consumer",
          "(map (fn [y] (iterate 3 y (fn [a] (+ a y))))\n\
          \      (map (fn [x] (+ x t)) (array/range %d)))" );
        ( "iterate-fused-both",
          "(map (fn [y] (if (> y t)\n\
          \        (iterate 4611686018427387903 y (fn [a] (+ a 1.))\n\
          \          :until (fn [prev next] (>= next (+ y 2.)))) t))\n\
          \      (map (fn [x] (if (> x t) (fold [a t] [i (range 2)] (+ a x)) x)) (array/range %d)))"
        );
        ( "iterate-fused-integer",
          "(map (fn [y] (float (iterate 3 y\n\
          \        (fn [a] (int/xor a (frame/index))))))\n\
          \      (map (fn [x] (iterate 2 (int/add (int x) (frame/index))\n\
          \        (fn [a] (int/add a 4611686018427387903)))) (array/range %d)))" );
      ]

(* Attribute fusion will produce several outputs; no surface does yet, so the
   region is built by hand: a scalar and a vec2 from one register program. *)
let two_outputs () =
  let region = compile "(map (fn [x] [x (+ x t)]) (array/range 1024))" in
  let o = region.Flow_ir.Core.Region.outputs.(0) in
  match Flow_ir.Core.Region.rebuild region ~outputs:[| [| o.(1) |]; [| o.(0); o.(1) |] |] () with
  | Ok program -> program
  | Error ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))

let run ?root ?strict ?force ?resolve program ~live =
  Flow_ir.Core_exec.(execute ?root ?strict ?force ?resolve (prepare program) ~live)

(* A Core program but for its regions is plain data, with no closures or residuals: it marshals
   without [Closures] and reads back structurally equal. *)
let plain_roundtrip (p : Flow_ir.Core.program) =
  let plain = (p.constants, p.locals, p.body, p.results, p.state_slots, p.call_slots) in
  if compare (Marshal.from_string (Marshal.to_string plain []) 0) plain <> 0 then
    failwith "Core program does not round-trip as plain data"
