open Flow

let read path = In_channel.with_open_bin path In_channel.input_all

let catalog =
  match Catalog.catalog_of_manifest (read "../flow_sop/flow_manifest.sexp") with
  | Ok (catalog, _) -> catalog
  | Error d -> failwith (Diagnostic.to_string d)

let cases = "../../specification/workspace/cases"

let parse source =
  match Syntax.parse source with Ok forms -> forms | Error d -> failwith (Diagnostic.to_string d)

let run source = Workspace.check catalog (parse source)
let errors ds = List.filter (fun (d : Diagnostic.t) -> d.severity = Diagnostic.Error_severity) ds
let show ds = String.concat "; " (List.map Diagnostic.to_string ds)
let sop b = "(workspace w (graph g :context sop " ^ b ^ "))"
let value b = "(workspace w (graph g :context value " ^ b ^ "))"

(* accepted without errors (warnings allowed) *)
let good source =
  match run source with
  | Some ws, ds when errors ds = [] -> ws
  | _, ds -> failwith (source ^ "\n  rejected: " ^ show ds)

(* rejected with an error of this code whose message contains [text] *)
let bad ?(text = "") source code =
  let has_text (d : Diagnostic.t) =
    let n = String.length text and m = String.length d.message in
    let rec at i = i + n <= m && (String.sub d.message i n = text || at (i + 1)) in
    at 0
  in
  match run source with
  | None, ds
    when List.exists
           (fun (d : Diagnostic.t) ->
             d.code = code && d.severity = Diagnostic.Error_severity && has_text d)
           ds ->
      ()
  | _, ds -> failwith (Printf.sprintf "%s\n  expected %s %S, got: %s" source code text (show ds))

let warns source code =
  match run source with
  | Some _, ds ->
      assert (
        List.exists (fun (d : Diagnostic.t) -> d.code = code && d.severity = Diagnostic.Warning) ds)
  | _, ds -> failwith (source ^ show ds)

let quiet source =
  match run source with Some _, ds -> assert (ds = []) | _, ds -> failwith (source ^ show ds)

let live ws path = Workspace.Paths.mem path ws.Workspace.live
let invariant ws path = Workspace.Paths.mem path ws.Workspace.invariant

let () =
  let ws =
    good
      "(workspace vectors\n\
      \    (defn shift :context value [(p : vec2) (q : vec4)] {:p (+ p 2) :q (* 2 q)})\n\
      \    (defn count2 :context value [(a : (array vec2))] (array/count a))\n\
      \    (defn count4 :context value [(a : (array vec4))] (array/count a))\n\
      \    (graph g :context host\n\
      \      (let* [pair [t 2] quad [1 2 3 t]\n\
      \             moved (shift pair quad) [px py] pair [qx qy qz qw] quad]\n\
      \        {:pair pair :quad quad :moved moved :fields (+ px (+ py qw))})))"
  in
  let result = List.assoc "g" (Result.get_ok (Eval.static ws)).results in
  List.iter
    (fun time ->
      let live = Input.at_time time in
      let actual = Result.get_ok (Eval.force ~live result)
      and reference = Result.get_ok (Eval.Host.force_reference ~live result) in
      let expected =
        Eval.Record
          [
            ("pair", Vec2 (time, 2.));
            ("quad", Vec4 (1., 2., 3., time));
            ("moved", Record [ ("p", Vec2 (time +. 2., 4.)); ("q", Vec4 (2., 4., 6., 2. *. time)) ]);
            ("fields", Float (time +. 2. +. time));
          ]
      in
      assert (actual = expected && reference = expected))
    [ 0.; 0.25; 2. ];
  List.iter
    (fun expression -> bad (value expression) "E_TYPE" ~text:"matching vector widths")
    [ "(+ [1 2] [1 2 3])"; "(* [1 2 3 4] [1 2])"; "(- [1 2 3] [1 2 3 4])" ];
  bad (value "(let* [pair [1 2]] pair.z)") "E_FIELD";
  bad (value "(let* [[a b c] [1 2]] a)") "E_PATTERN" ~text:"a vec2 has 2";
  bad (value "[1 2 3 4 5]") "E_VECTOR";
  List.iter
    (fun expression ->
      let ws = good (value ("(let* [v " ^ expression ^ "] 0)")) in
      assert (Workspace.Paths.is_empty ws.approx);
      assert (not (List.mem_assoc [ "g"; "v" ] ws.packed)))
    [ "[t 2]"; "[1 2 3 t]" ];
  List.iter
    (fun (source, expected) ->
      let ws = good ("(workspace vectors (graph g :context host " ^ source ^ "))") in
      assert (List.assoc "g" (Result.get_ok (Eval.run ~time:0. ws)).results = expected))
    [
      ("(+ [1 2] true)", Eval.Vec2 (2., 3.));
      ("(let* [f (fn [(p : vec4)] p)] (f 2))", Eval.Vec4 (2., 2., 2., 2.));
      ("(let* [f (fn [(p : vec2)] p)] (f 3))", Eval.Vec2 (3., 3.));
    ]

let () =
  (* Function ports instantiate bodies, and keep each caller's captures. *)
  let field =
    Catalog.
      {
        name = "field";
        label = "Field";
        fields = [];
        folder = [];
        primary = true;
        unit = None;
        ty = Some (Port_type.Fn Ty.{ params = [ Float ]; result = Float });
      }
  in
  let kind =
    Catalog.
      {
        qualified = "sop/test_field";
        context = Context.sop;
        slots = [];
        parameters = [ field ];
        outputs = [ ("geo", Port_type.Geometry) ];
        facts = None;
      }
  in
  let run_with signature source =
    let parameter = { field with ty = Some (Port_type.Fn signature) } in
    Workspace.check
      { catalog with kinds = { kind with parameters = [ parameter ] } :: catalog.kinds }
      (parse source)
  in
  let signature = Ty.{ params = [ Record [ ("p", Vec3) ] ]; result = Float } in
  let accepted signature source =
    match run_with signature source with
    | Some ws, ds when errors ds = [] -> ws
    | _, ds -> failwith (source ^ "\n" ^ show ds)
  in
  let ws = accepted signature (sop "(sop/test_field :field (fn [r] r.p.x))") in
  (match (List.hd ws.graphs).body.node with
  | Workspace.Call
      {
        args =
          [
            ( "field",
              {
                ty = Ty.Fn (Some certified);
                node = Workspace.Fn { params = [ (_, Some input) ]; body; _ };
                _;
              } );
          ];
        _;
      } ->
      assert (certified = signature && input = List.hd signature.params && body.ty = Ty.Float)
  | _ -> failwith "function port did not retain its checked body");
  List.iter
    (fun (source, code) ->
      match run_with signature (sop source) with
      | None, ds when List.exists (fun (d : Diagnostic.t) -> d.code = code) ds -> ()
      | _, ds -> failwith (source ^ ": expected " ^ code ^ ", got " ^ show ds))
    [
      ("(sop/test_field :field 1.0)", "E_TYPE");
      ("(sop/test_field :field (fn [r s] r.p.x))", "E_ARITY");
      ("(sop/test_field :field (fn [r] (image/noise :width 1 :height 1)))", "E_TYPE");
      ("(let* [stored {:f (fn [r] r.p.x)}] (sop/box))", "E_FN_ESCAPES");
    ];
  let ws =
    accepted
      Ty.{ params = [ Float ]; result = Float }
      "(workspace w (defn consume :context sop [(f : fn)] (sop/test_field :field f)) (graph g \
       :context sop (let* [offset 7.0 f7 (fn [x] (+ x offset)) alias f7 f3 (fn [x] (* x 3.0))] \
       (sop/merge (list (consume alias) (consume f3))))))"
  in
  let evaluated =
    match Eval.static ws with Ok v -> v | Error d -> failwith (Diagnostic.to_string d)
  in
  let functions =
    Array.to_list evaluated.plan.nodes
    |> List.filter_map (fun (n : Eval.node) ->
        if n.kind = "sop/test_field" then
          match List.assoc "field" n.args with
          | Eval.Fn fn -> Some fn
          | _ -> failwith "function port lost its callable"
        else None)
  in
  assert (List.length functions = 2);
  List.iteri
    (fun i fn ->
      let _, body = Option.get (Eval.Residual.function_body fn) in
      assert (body.ty = Ty.Float);
      if i = 0 then
        assert (List.assoc "offset" (Eval.Residual.function_bindings fn) = Eval.Float 7.))
    functions

let () =
  (* A canvas accepts an image through the ordinary drawing operator. *)
  let ws =
    good
      "(workspace w (graph picture :context image (image/noise :width 2 :height 2)) (graph editor \
       :context editor (ui/workspace (ui/canvas (ref picture)))))"
  in
  let editor = List.find (fun (g : Workspace.graph) -> g.name = "editor") ws.graphs in
  (match editor.body.node with
  | Workspace.Op
      {
        op = "ui/workspace";
        args =
          [
            ( _,
              {
                node =
                  Workspace.Op
                    {
                      op = "ui/canvas";
                      args =
                        [
                          ( "drawing",
                            {
                              ty;
                              node =
                                Workspace.Op { op = "draw/image"; args = [ ("image", image) ]; _ };
                              _;
                            } );
                        ];
                      _;
                    };
                _;
              } );
          ];
        _;
      } ->
      assert (ty = Ty.drawing && image.ty = Ty.image)
  | _ -> failwith "canvas image did not lower to draw/image");
  let evaluated =
    match Eval.static ws with Ok v -> v | Error d -> failwith (Diagnostic.to_string d)
  in
  (match List.assoc "editor" evaluated.results with
  | Eval.Struct
      ( "ui/workspace",
        _,
        [ ("root", Eval.Struct ("ui/canvas", _, [ ("drawing", Eval.Deferred (ty, id)) ])) ] ) ->
      let drawing = evaluated.plan.nodes.(id) in
      assert (ty = Ty.drawing && drawing.kind = "draw/image");
      assert (List.assoc "image" drawing.args = List.assoc "picture" evaluated.results)
  | _ -> failwith "canvas image did not evaluate as a drawing");
  bad "(workspace w (graph editor :context editor (ui/workspace (ui/canvas 1))))" "E_TYPE"

let () =
  List.iter
    (fun context ->
      ignore
        (good
           ("(workspace w (graph g :context " ^ context
          ^ " (let* [img (image/noise :width 1 :height 1)] "
           ^ (match context with
             | "value" -> "0"
             | "sop" -> "(sop/box)"
             | "draw" -> "(draw/background \"#000000\")"
             | _ -> "img")
           ^ ")))")))
    [ "value"; "sop"; "draw"; "image" ]

let () =
  (* the 12 fixtures check with no diagnostics at all against the real catalog *)
  let files =
    Sys.readdir cases |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".lisp")
    |> List.sort compare
  in
  assert (List.length files = 12);
  let failed =
    List.filter_map
      (fun file ->
        let ws, ds = run (read (Filename.concat cases file)) in
        if ws = None || ds <> [] then Some (file ^ ": " ^ show ds) else None)
      files
  in
  if failed <> [] then failwith (String.concat "\n" failed)

let () =
  (* structure and the typed IR *)
  let ws =
    good
      "; note\n\
       (workspace w (defn f :context value [(x : float 2.0)] (* x 2)) (defmacro m [a] `(+ ~a ~a)) \
       (graph g :context value [(n : int 3)] (let* [a (f :x n)] (+ a (m 1)))))"
  in
  assert (
    ws.name = "w"
    && List.length ws.graphs = 1
    && List.length ws.defs = 1
    && List.length ws.macros = 1);
  let g = List.hd ws.graphs in
  assert (g.context = Context.value && List.length g.inputs = 1);
  (match g.body.node with
  | Workspace.Let
      ( [
          ( Workspace.Name "a",
            { node = Workspace.Call_fn { fn = "f"; args = [ _ ]; _ }; path = Some [ "g"; "a" ]; _ }
          );
        ],
        r ) -> (
      assert (r.path = Some [ "g"; "@result" ]);
      match r.node with
      | Workspace.Op
          { op = "+"; args = [ _; (_, { node = Workspace.Expanded { macro = "m"; _ }; _ }) ]; _ } ->
          ()
      | _ -> failwith "result is not (+ a expansion)")
  | _ -> failwith "unexpected IR of g");
  bad "" "E_NO_WORKSPACE";
  bad "(graph g :context sop nil)" "E_WORKSPACE";
  bad "(workspace w) (workspace v)" "E_ONE_WORKSPACE";
  bad "(workspace w (defn f :context value [(x : float)] x))" "E_NO_GRAPH";
  bad "(workspace w (thing g) (graph g :context value 1))" "E_FORM";
  bad "(workspace w (graph g :context value 1) (graph g :context value 2))" "E_NAME";
  bad "(workspace w (graph let* :context value 1))" "E_NAME";
  bad "(workspace w (graph g :context nowhere 1))" "E_CONTEXT_UNKNOWN";
  bad "(workspace w (graph g :context value [(n : bogus 1)] 1))" "E_PARAM";
  bad "(workspace w (graph g :context value [(n : int 1) (n : int 2)] 1))" "E_PARAM";
  bad "(workspace w (defn f :context value 1) (graph g :context value 1))" "E_SHAPE";
  quiet (value "1")

let () =
  (* E_INPUT_DEFAULT (L14) *)
  bad "(workspace w (graph a :context value [(n : int)] n))" "E_INPUT_DEFAULT"
    ~text:"needs a default";
  ignore (good "(workspace w (graph a :context value [(n : int 2)] n))");
  ignore
    (good "(workspace w (defn f :context value [(n : int)] n) (graph a :context value (f :n 1)))");
  bad "(workspace w (defn f :context value [(n : int)] n) (graph a :context value (f)))" "E_ARGS"
    ~text:"needs :n"

let () =
  (* E_SHADOW (L11) *)
  bad (sop "(let* [a 1 b (for [a (range 3)] (sop/box))] (sop/merge b))") "E_SHADOW" ~text:"shadows";
  bad (value "(let* [a 1 f (fn [a] a)] 1)") "E_SHADOW";
  bad (value "(let* [x 1 y (let* [x 2] x)] y)") "E_SHADOW";
  bad (value "(fold [a 0] [a (range 3)] 1)") "E_DUPLICATE" ~text:"bound twice";
  bad (value "(let* [1 2] 3)") "E_PATTERN";
  ignore (good (value "(let* [a 1 b (for [i (range 3)] i)] a)"))

let () =
  (* t is reserved (T1) *)
  bad (sop "(let* [t 1] (sop/box))") "E_BINDING" ~text:"context time";
  bad (value "(let* [f (fn [t] t)] 1)") "E_BINDING" ~text:"context time";
  bad (value "(for [t (range 3)] t)") "E_BINDING";
  bad "(workspace w (graph g :context value [(t : int 1)] 1))" "E_PARAM"

let () =
  (* E_FN_ESCAPES (F1) *)
  let msg = "cannot be stored or returned" in
  bad (value "(let* [f (fn [x] x)] f)") "E_FN_ESCAPES" ~text:msg;
  bad (value "(let* [f (fn [x] x) l (list f)] 1)") "E_FN_ESCAPES" ~text:msg;
  bad (value "(let* [f (fn [x] x) r {:f f}] 1)") "E_FN_ESCAPES" ~text:msg;
  bad (value "(let* [f (fn [x] x)] (fold [a f] [i (range 2)] 1))") "E_FN_ESCAPES" ~text:msg;
  bad (value "(count (map (fn [x] (fn [y] y)) (list 1)))") "E_FN_ESCAPES" ~text:msg;
  bad
    "(workspace w (graph a :context value [(n : int 2)] n) (graph g :context value (let* [f (fn \
     [x] x)] (ref a :n f))))"
    "E_FN_ESCAPES" ~text:msg;
  bad "(workspace w (defn h :context value [(k : fn)] k) (graph g :context value 1))" "E_FN_ESCAPES"
    ~text:msg;
  bad "(workspace w (graph g :context value [(k : fn)] 1))" "E_FN_ESCAPES" ~text:msg;
  (* accepted: bound, called, passed to map / filter / reduce / a fn-typed defn input *)
  ignore (good (value "(let* [f (fn [a b] (+ a b))] (f 1 2))"));
  ignore (good (value "(reduce + 0 (map (fn [x] (* x 2)) (list 1 2 3)))"));
  ignore
    (good
       "(workspace w (defn ring :context sop [(n : int 8) (make : fn)] (sop/merge (map make (range \
        n)))) (graph g :context sop (ring :n 3 :make (fn [i] (sop/box)))))");
  bad (value "(let* [f (fn [x] (f x))] (f 1))") "E_UNKNOWN_KIND" ~text:"Unknown operator";
  bad
    "(workspace w (defmacro dbl [x] `(+ ~x ~x)) (graph g :context value (count (map dbl (list \
     1)))))"
    "E_MACRO_AS_VALUE"

let () =
  (* E_ACC_TYPE (L6) *)
  bad (sop "(fold [g (sop/box)] [i (range 3)] 1.0)") "E_ACC_TYPE" ~text:"accumulator type";
  bad (value "(scan [a 1] [i (range 3)] \"s\")") "E_ACC_TYPE";
  ignore (good (sop "(fold [g (sop/box)] [i (range 3)] (sop/merge g (sop/box)))"));
  ignore
    (good
       (value "(let* [s (fold [{:keys [a b]} {:a 0 :b 1}] [i (range 10)] {:a b :b (+ a b)})] s.a)"));
  bad (value "(fold [a 0 b 1] [i (range 3)] a)") "E_ZONE" ~text:"exactly one accumulator"

let () =
  (* E_ITER_BOUND (L3): literal counts only, at check time *)
  bad (sop "(sop/merge (for [i (range 5000)] (sop/box)))") "E_ITER_BOUND" ~text:"4,096";
  bad (value "(count (range 10 5000))") "E_ITER_BOUND";
  bad (value "(count (linspace 0 1 5000))") "E_ITER_BOUND";
  bad (sop "(sop/merge (for [x (range 100) y (range 100)] (sop/box)))") "E_ITER_BOUND" ~text:"4,096";
  ignore (good (sop "(sop/merge (for [x (range 64) y (range 64)] (sop/box)))"));
  ignore (good (sop "(sop/merge (for [i (range 4096)] (sop/box)))"));
  (* a driven count is not checked here: Eval bounds it when it runs *)
  ignore
    (good
       "(workspace w (graph g :context sop [(n : int 9999)] (sop/merge (for [i (range n)] \
        (sop/box)))))");
  bad (value "(let* [z (for [i 3] i)] 1)") "E_TYPE" ~text:"iterates a list"

let () =
  (* E_NO_ELSE (C1), cond and case *)
  bad (value "(cond (< 2 1) 1)") "E_NO_ELSE" ~text:"final :else";
  bad (value "(case 1 1 2)") "E_NO_ELSE";
  bad (value "(if true 1)") "E_NO_ELSE";
  ignore (good (value "(cond (< 2 1) 1 (> 2 1) 2 :else 3)"));
  ignore (good (value "(case 2 1 10 2 20 :else 0)"));
  bad (value "(let* [k 1] (case 1 k 2 :else 0))") "E_CASE" ~text:"literal";
  bad (value "(cond true 1 :else \"a\")") "E_TYPE" ~text:"one type";
  bad (sop "(if (< 1 2) (sop/box) 3)") "E_TYPE" ~text:"Both branches";
  (* both branches are typed even when one is never taken *)
  bad (value "(if true 1 (+ 1 \"x\"))") "E_TYPE"

let () =
  (* E_PATTERN (D3) *)
  bad (value "(let* [[a b c] (list 1 2)] a)") "E_PATTERN" ~text:"needs 3 elements; the list has 2";
  bad (value "(let* [[a b c d] [1 2 3]] a)") "E_PATTERN" ~text:"a vec3 has 3";
  bad (value "(let* [{:keys [a c]} {:a 1}] a)") "E_FIELD" ~text:"no field c";
  bad (value "(let* [{:keys [a]} 1] a)") "E_PATTERN" ~text:"destructures a record";
  bad (value "(let* [[a b] 1] a)") "E_PATTERN" ~text:"destructures a list or vec3";
  bad (value "(let* [[] (list 1)] 1)") "E_PATTERN";
  bad (value "(let* [map 1] map)") "E_BINDING" ~text:"Invalid binding name map";
  bad (value "(let* [[a list] (list 1 2)] a)") "E_BINDING";
  ignore (good (value "(let* [[x y z] [1 2 3] {:keys [a b]} {:a 10 :b 20}] (+ (* x a) (* z b)))"));
  ignore
    (good
       (value
          "(let* [f (fn [[a b]] (* a b)) s (sum [[a b] (list (list 1 2) (list 3 4))] (+ (f (list a \
           b)) 0))] s)"))

let () =
  (* other static diagnostics of the study *)
  bad (value "(let* [a 1] b)") "E_UNBOUND" ~text:"not bound";
  bad
    (sop "(let* [a (for [i (range 2)] (sop/box))] (sop/transform (sop/merge a) :rotate [0 0 i]))")
    "E_UNBOUND";
  bad (value "(+ 1)") "E_ARITY" ~text:"takes 2 positional";
  bad (value "(let* [f (fn [a b] a)] (f 1))") "E_ARITY" ~text:"takes 2 arguments; got 1";
  bad (sop "(sop/box :sizes 1)") "E_UNKNOWN_PARAM" ~text:"Did you mean";
  bad (sop "(sop/box :size 1 :size 2)") "E_DUPLICATE_PARAM";
  bad (sop "(sop/box :size [1 2])") "E_TYPE" ~text:"takes vec3, but this is vec2";
  bad (sop "(sop/box :x_divisions \"a\")") "E_TYPE";
  bad (sop "(sop/box :x_divisions 0)") "E_HARD_RANGE";
  warns (sop "(sop/box :x_divisions 99)") "W_SOFT_RANGE";
  ignore (good (sop "(sop/transform)"));
  ignore (good (sop "(sop/exploded_view)"));
  ignore (good (sop "(sop/exploded_view nil)"));
  bad (sop "(sop/box (sop/box))") "E_EXTRA_POSITIONAL";
  bad (sop "(sop/nothing)") "E_UNKNOWN_KIND" ~text:"Unknown operator";
  bad (value "(sop/box)") "E_WRONG_CONTEXT";
  bad "(workspace w (graph g :context scene (scene/geometry (sop/box))))" "E_WRONG_CONTEXT";
  bad (sop "(scene/geometry (sop/box))") "E_WRONG_CONTEXT" ~text:"scene node";
  bad "(workspace w (defn f :context sop [(x : float)] (f x)) (graph g :context sop (f 1)))"
    "E_RECURSION" ~text:"Use fold";
  bad
    "(workspace w (defn app :context value [(f : fn) (x : float)] (f f x)) (graph g :context value \
     (app app 1)))"
    "E_RECURSION";
  bad "(workspace w (graph a :context value [(n : int 2)] n) (graph b :context value (ref a :m 1)))"
    "E_UNKNOWN_PARAM" ~text:"no input :m";
  bad (value "(ref nope)") "E_UNKNOWN_GRAPH";
  bad "(workspace w (defn f :context value [(x : float)] (ref g)) (graph g :context value 1))"
    "E_REF_IN_DEF";
  bad "(workspace w (graph a :context value (ref b)) (graph b :context value (ref a)))"
    "E_GRAPH_CYCLE" ~text:"a → b → a"

let () =
  (* liveness (T1): what depends on t *)
  let ws =
    good
      (sop
         "(let* [a (sop/box) b (* t 2) c (+ b 1) d (sop/transform a :translate [c 0 0]) e \
          (sop/transform d)] e)")
  in
  assert (not (live ws [ "g"; "a" ]));
  assert (live ws [ "g"; "b" ] && live ws [ "g"; "c" ]);
  assert (live ws [ "g"; "d" ] && live ws [ "g"; "e" ] && live ws [ "g"; "@result" ]);
  (* through a loop: the body is live, the collection is not, and the capture is *)
  let ws =
    good
      (sop
         "(let* [k (sin t) ring (for [i (range 3)] (let* [s (* i 2) w (+ s k)] (sop/box :size \
          w)))] (sop/merge ring))")
  in
  assert (live ws [ "g"; "k" ] && live ws [ "g"; "ring" ] && live ws [ "g"; "ring"; "w" ]);
  assert ((not (live ws [ "g"; "ring"; "s" ])) && not (live ws [ "g"; "ring"; ":i" ]));
  (* a fold accumulator: static initial value, live body; the accumulator is live inside *)
  let ws = good (value "(let* [f (fold [a 0] [i (range 3)] (let* [b (* a 2)] (+ b t)))] f)") in
  assert (live ws [ "g"; "f" ] && live ws [ "g"; "f"; "b" ] && live ws [ "g"; "f"; ":a" ]);
  let ws = good (value "(let* [f (fold [a t] [i (range 3)] (let* [b (* a 2)] b))] f)") in
  assert (live ws [ "g"; "f"; "b" ]);
  let ws = good (value "(let* [f (fold [a 1] [i (range 3)] (let* [b (* a 2)] b))] f)") in
  assert ((not (live ws [ "g"; "f" ])) && not (live ws [ "g"; "f"; "b" ]));
  (* through functions: a local fn and a defn whose body mentions t, and one that only receives it *)
  let ws =
    good
      (value
         "(let* [f (fn [x] (+ x t)) g2 (fn [x] (* x 2)) a (f 1) b (g2 1) c (g2 t)] (+ a (+ b c)))")
  in
  assert (live ws [ "g"; "a" ] && (not (live ws [ "g"; "b" ])) && live ws [ "g"; "c" ]);
  let ws =
    good
      "(workspace w (defn pulse :context value [(x : float)] (* x t)) (defn twice :context value \
       [(x : float)] (* x 2)) (graph g :context value (let* [a (pulse 1) b (twice 2) c (twice a)] \
       (+ a (+ b c)))))"
  in
  assert (live ws [ "g"; "a" ] && (not (live ws [ "g"; "b" ])) && live ws [ "g"; "c" ]);
  (* through map: a live function makes the list live; a live element makes the results live *)
  let ws =
    good
      (value
         "(let* [xs (map (fn [x] (* x t)) (list 1 2)) ys (map (fn [x] x) (list 1 2)) zs (map (fn \
          [x] x) (list t))] (count xs))")
  in
  assert (live ws [ "g"; "xs" ] && (not (live ws [ "g"; "ys" ])) && live ws [ "g"; "zs" ]);
  (* through graph refs and overrides *)
  let ws =
    good
      "(workspace w (graph a :context value [(n : int 2)] (* n t)) (graph b :context value [(n : \
       int 2)] (* n 2)) (graph c :context value (+ (ref a) (+ (ref b) (ref b :n t)))))"
  in
  let c = List.nth ws.graphs 2 in
  assert (live ws [ "c"; "@result" ]);
  ignore c;
  let ws =
    good
      "(workspace w (graph a :context value [(n : int 2)] (* n t)) (graph b :context value [(n : \
       int 2)] (* n 2)) (graph c :context value (let* [x (ref a) y (ref b) z (ref b :n 5) w (ref b \
       :n t)] (+ x (+ y (+ z w))))))"
  in
  assert (
    live ws [ "c"; "x" ]
    && (not (live ws [ "c"; "y" ]))
    && (not (live ws [ "c"; "z" ]))
    && live ws [ "c"; "w" ]);
  assert (live ws [ "a"; "@result" ] && not (live ws [ "b"; "@result" ]));
  (* a macro's fresh name t# is t__1, not t *)
  let m = "(defmacro add1 [a] `(let* [t# ~a] (+ t# 1)))" in
  let ws =
    good ("(workspace w " ^ m ^ " (graph g :context value (let* [x (add1 1) y (add1 t)] (+ x y))))")
  in
  assert ((not (live ws [ "g"; "x" ])) && live ws [ "g"; "y" ]);
  (* a static program marks nothing *)
  let ws = good (sop "(let* [a (sop/box) b (sop/transform a)] b)") in
  assert (Workspace.Paths.is_empty ws.live)

let () =
  (* loop invariance (L7) *)
  let ws =
    good
      (sop
         "(let* [ring (for [i (range 4)] (let* [up [0 1 0] r (* i 0.1) mv (sop/transform (sop/box) \
          :translate up) bg (sop/transform mv :uniform_scale r)] bg))] (sop/merge ring))")
  in
  assert (invariant ws [ "g"; "ring"; "up" ] && invariant ws [ "g"; "ring"; "mv" ]);
  assert ((not (invariant ws [ "g"; "ring"; "r" ])) && not (invariant ws [ "g"; "ring"; "bg" ]));
  (* invariance is relative to the innermost zone *)
  let ws =
    good (value "(sum [i (range 2)] (sum [j (range 3)] (let* [a (* i 2) b (* j 2)] (+ a b))))")
  in
  ignore ws

let () =
  (* E_TIME_COUNT (T2) *)
  bad (sop "(sop/merge (for [i (range (+ 3 (floor (* 2 (sin t)))))] (sop/box)))") "E_TIME_COUNT";
  bad (sop "(fold [s (sop/box)] [i (range (floor t))] s)") "E_TIME_COUNT";
  bad
    (sop "(sop/merge (filter (fn [g] (> t 1)) (list (sop/box) (sop/box))))")
    "E_TIME_COUNT" ~text:"changes length";
  bad
    (sop "(let* [k (floor t) xs (take k (list (sop/box) (sop/box)))] (sop/merge xs))")
    "E_TIME_COUNT";
  (* value-only lists may have a live length, and a live body is not a live count *)
  ignore (good (value "(sum [x (filter (fn [s] (> s t)) (list 1 2 3))] x)"));
  ignore (good (value "(count (range (floor t)))"));
  ignore (good (value "(fold [total 0] [x (range (floor t))] (+ total x))"));
  ignore (good (value "(count (scan [total 0] [x (range (floor t))] (+ total x)))"));
  ignore (good (sop "(sop/merge (for [i (range 3)] (sop/box :size (+ 1 (sin t)))))"));
  ignore (good (sop "(sop/merge (map (fn [i] (sop/box :size t)) (range 3)))"))

let () =
  (* E_TIME_BRANCH (T3) *)
  bad (sop "(if (> (sin t) 0.5) (sop/box) (sop/uv_sphere))") "E_TIME_BRANCH" ~text:"pick a value";
  bad (sop "(let* [k (sin t)] (cond (> k 0.5) (sop/box) :else (sop/uv_sphere)))") "E_TIME_BRANCH";
  bad (sop "(case (floor t) 0 (sop/box) :else (sop/uv_sphere))") "E_TIME_BRANCH";
  bad (sop "(sop/merge (if (> t 1) (list (sop/box)) (list (sop/box) (sop/box))))") "E_TIME_BRANCH";
  ignore (good (sop "(sop/box :size (if (> (sin t) 0.5) 1 0.5))"));
  ignore (good (sop "(sop/set_color (sop/box) :color (if (> (sin t) 0) \"#d69f61\" \"#6fa6a1\"))"));
  (* a static test may choose shapes *)
  ignore
    (good
       "(workspace w (graph g :context sop [(k : int 1)] (if (> k 0) (sop/box) (sop/uv_sphere))))")

let () =
  (* Unknown groups and computed names. *)
  warns (sop "(sop/blast (sop/box) :group \"nobody\")") "W_UNKNOWN_GROUP";
  quiet (sop "(sop/blast (sop/group_bounds (sop/box) :name \"attic\") :group \"attic\")");
  quiet
    (sop
       "(let* [a (sop/group_random (sop/box) :name \"lit\") b (sop/transform a) c (sop/merge b \
        (sop/box))] (sop/set_color c :group \"lit\" :color \"#f5cf4f\"))");
  quiet
    (sop
       "(sop/blast (sop/group_bounds (sop/box) :name (str \"floor_\" 2)) :group (str \"floor_\" 2))");
  warns (sop "(sop/blast (sop/group_bounds (sop/box) :name \"a\") :group \"b\")") "W_UNKNOWN_GROUP";
  (* an empty group means all elements *)
  quiet (sop "(sop/set_color (sop/box) :group \"\" :color [1 0 0])")

let () =
  (* colours, vec3 widening, bypass *)
  quiet (sop "(sop/set_color (sop/box) :color \"#b0680f\")");
  quiet (sop "(sop/set_color (sop/box) :color (value/hsv 0.1 0.5 0.9))");
  bad (sop "(sop/set_color (sop/box) :color \"red\")") "E_TYPE" ~text:"colour";
  bad (sop "(sop/set_color (sop/box) :color 3)") "E_TYPE";
  quiet (sop "(sop/uv_sphere :radius 0.1)");
  quiet (sop "(sop/uv_sphere :radius [0.1 0.2 0.3])");
  let ws = good (sop "(let* [a (sop/box) b ^:bypass (sop/transform a :translate [5 0 0])] b)") in
  (match (List.hd ws.graphs).body.node with
  | Workspace.Let
      ( [
          _;
          ( _,
            {
              node = Workspace.Bypass { node = Workspace.Call { kind = "sop/transform"; _ }; _ };
              _;
            } );
        ],
        _ ) ->
      ()
  | _ -> failwith "bypass is not wrapped");
  bad (sop "^:mute (sop/box)") "E_META" ~text:"only metadata is ^:bypass";
  bad (sop "^:bypass (sop/box :size 2)") "E_BYPASS" ~text:"no positional input";
  bad (sop "^:bypass (let* [a 1] (sop/box))") "E_BYPASS" ~text:"only operator and function calls"

let () =
  (* records, lists, str, hofs: types flow *)
  let ws =
    good
      (value
         "(let* [r {:a 1 :b {:c [1 2 3]}} s (assoc r :a 5 :d 2) n (+ r.a r.b.c.y) l (list 1 2.5) e \
          (list) t2 (str \"a\" 1 [1 2 3])] (+ n s.d))")
  in
  let ty name =
    match (List.hd ws.graphs).body.node with
    | Workspace.Let (bs, _) -> (
        match List.find_opt (fun (p, _) -> p = Workspace.Name name) bs with
        | Some (_, t) -> Ty.to_string t.ty
        | None -> failwith name)
    | _ -> failwith "no let"
  in
  assert (ty "r" = "rec{a:int,b:rec{c:vec3}}");
  assert (ty "s" = "rec{a:int,b:rec{c:vec3},d:int}");
  assert (ty "l" = "list:float" && ty "e" = "list:any" && ty "t2" = "text" && ty "n" = "float");
  bad (value "(let* [r {:a 1}] r.b)") "E_FIELD" ~text:"no field b. Fields: a";
  bad (value "(let* [r {:a 1} s (assoc r :a \"x\")] 1)") "E_TYPE" ~text:"assoc :a is int; got text";
  bad (value "(count (list 1 \"a\"))") "E_TYPE" ~text:"share one type";
  bad (value "(count (filter (fn [x] \"no\") (list 1)))") "E_TYPE" ~text:"predicate returns bool";
  bad (value "(reduce (fn [a x] \"s\") 0 (list 1))") "E_TYPE" ~text:"accumulator type";
  bad (value "(sort-by (fn [x] \"k\") (list 1))") "E_TYPE" ~text:"key returns a number";
  bad
    (value "(let* [f (fn [(a : float)] a)] (if false (f \"x\") 1))")
    "E_TYPE" ~text:"expected float, got text";
  bad (value "(map (fn [a] a))") "E_ARITY"

let () =
  (* macros through the workspace (M1, M2) *)
  let radial =
    "(defmacro radial [i n body] `(sop/merge (for [~i (range ~n)] (sop/transform ~body :rotate [0 \
     (* (/ ~i ~n) 6.2832) 0]))))"
  in
  let ws =
    good
      ("(workspace w " ^ radial
     ^ " (graph g :context sop (let* [ring (radial k 12 (sop/box :size (+ 0.1 (* k 0.01))))] \
        ring)))")
  in
  (match (List.hd ws.graphs).body.node with
  | Workspace.Let
      ( [
          ( _,
            {
              node =
                Workspace.Expanded
                  { macro = "radial"; body = { node = Workspace.Op _ | Workspace.Call _; _ } };
              _;
            } );
        ],
        _ ) ->
      ()
  | _ -> failwith "radial did not expand");
  (* the caller's index is a binder hole: the loop inside the expansion is a zone with that name *)
  assert (Workspace.Paths.is_empty ws.live && List.length ws.macros = 1);
  bad "(workspace w (defmacro m [x] `(+ ~x y)) (graph g :context value (let* [y 1] (m 2))))"
    "E_MACRO_CAPTURE" ~text:"would capture";
  bad "(workspace w (defmacro m [x] `(+ ~x ~(+ 1 2))) (graph g :context value (m 2)))"
    "E_MACRO_UNQUOTE";
  bad "(workspace w (defmacro m [x] `(m ~x)) (graph g :context value (m 2)))" "E_MACRO_DEPTH"
    ~text:"32";
  bad
    ("(workspace w (defmacro d [x] `(+ ~x ~x)) (graph g :context value "
    ^ String.concat "" (List.init 12 (fun _ -> "(d "))
    ^ "1" ^ String.make 12 ')' ^ "))")
    "E_MACRO_SIZE" ~text:"5,000";
  bad ("(workspace w " ^ radial ^ " (graph g :context sop (radial k 2)))") "E_MACRO_ARITY";
  bad
    "(workspace w (defmacro m [x] `(+ ~x 1)) (defn m :context value [(x : int)] x) (graph g \
     :context value 1))"
    "E_NAME";
  (* an expansion's shadowing is reported at the call *)
  bad
    ("(workspace w " ^ radial ^ " (graph g :context sop (let* [k 1 r (radial k 2 (sop/box))] r)))")
    "E_SHADOW";
  ignore (good "(workspace w (defmacro twice [x] `(+ ~x ~x)) (graph g :context value (twice 3)))");
  (* a template may call any global: kinds, defns, operators, other macros *)
  ignore
    (good
       "(workspace w (defn f :context value [(x : float)] x) (defmacro a [x] `(f ~x)) (defmacro b \
        [x] `(a ~x)) (graph g :context value (b 1)))")

let () =
  (* the other contexts: scene, world, settings, editor *)
  ignore
    (good
       "(workspace w (graph a :context sop (sop/box)) (graph s :context scene (scene/merge \
        (scene/geometry (ref a)) (scene/geometry (ref a) :translate [1 0 0] :name \"b\") \
        (scene/camera :eye [0 2 6]) (scene/light :color \"#ffcc88\"))) (graph x :context world \
        (world/sun (world/sky) :name \"L\")) (graph c :context settings (settings/config :title \
        \"t\" :fps 30)))");
  (* the kinds are the schemas' kinds: an unknown kind, keyword or type is reported per context *)
  let scene b =
    "(workspace w (graph a :context sop (sop/box)) (graph s :context scene " ^ b ^ "))"
  in
  let world b = "(workspace w (graph x :context world " ^ b ^ "))" in
  let settings b = "(workspace w (graph c :context settings " ^ b ^ "))" in
  bad (scene "(scene/nothing)") "E_UNKNOWN_KIND";
  ignore (good (scene "(scene/geometry)"));
  ignore (good (scene "(scene/geometry nil)"));
  bad (scene "(scene/geometry 3)") "E_TYPE" ~text:"takes geometry";
  bad (scene "(scene/geometry (ref a) :nothing 1)") "E_UNKNOWN_PARAM";
  bad (scene "(scene/geometry (ref a) :translate \"a\")") "E_TYPE";
  bad (scene "(scene/geometry (ref a) :visible \"x\")") "E_TYPE";
  bad (scene "(scene/light :shape \"Cube\")") "E_TYPE";
  bad (scene "(scene/camera :fov 200)") "E_HARD_RANGE";
  bad (scene "(scene/merge (ref a))") "E_TYPE";
  bad (scene "(world/sky)") "E_WRONG_CONTEXT";
  bad (world "(world/sky (sop/box))") "E_WRONG_CONTEXT";
  bad (world "(world/sun 3)") "E_TYPE" ~text:"takes world";
  bad (world "(world/sky :turbidity -1)") "E_HARD_RANGE";
  bad (world "(scene/light)") "E_WRONG_CONTEXT";
  bad (world "(sop/box)") "E_WRONG_CONTEXT";
  bad (settings "(settings/config :fps 500)") "E_HARD_RANGE";
  bad (settings "(settings/config :nothing 1)") "E_UNKNOWN_PARAM";
  bad (settings "(settings/config :title 3)") "E_TYPE";
  bad (settings "(world/sky)") "E_WRONG_CONTEXT";
  (* a graph's result has its context's type *)
  bad (scene "(world/sky)") "E_WRONG_CONTEXT";
  bad "(workspace w (graph a :context sop (sop/box)) (graph s :context scene (ref a)))" "E_TYPE";
  bad (world "(scene/merge)") "E_WRONG_CONTEXT";
  bad
    "(workspace w (graph e :context editor (ui/workspace (ui/split \"diagonal\" (ui/graph) \
     (ui/lisp)))))"
    "E_RANGE" ~text:"axis";
  bad
    "(workspace w (graph e :context editor (ui/workspace (ui/split-at \"vertical\" 0.95 (ui/graph) \
     (ui/lisp)))))"
    "E_RANGE" ~text:"ratio";
  bad "(workspace w (graph e :context editor (ui/workspace (ui/graph :wires \"straight\"))))"
    "E_UNKNOWN_PARAM" ~text:"wires"

let () =
  (* IR shapes, notes and reporting *)
  let ws =
    good
      "(workspace w\n\
      \  ; a helper\n\
      \  (defn f :context value [(x : float 2.0) (y : float 1.0)] (+ x y))\n\
      \  (graph g :context value [(n : int 3)]\n\
      \    (let* [; loop\n\
      \           s (sum [i (range n)] (f :y i))\n\
      \           l (map (fn [k] (* k 2)) (range 3))\n\
      \           c (cond (< s 1) 1 :else 2)\n\
      \           k (case n 1 \"a\" :else \"b\")]\n\
      \      (+ s (+ (count l) c)))))"
  in
  assert ((List.hd ws.defs).form.notes = [ "a helper" ]);
  (match (List.hd ws.graphs).body.node with
  | Workspace.Let ([ (_, s); (_, l); (_, c); (_, k) ], _) ->
      (match s.node with
      | Workspace.Loop
          {
            kind = `Sum;
            accs = [];
            clauses = [ (Workspace.Name "i", _) ];
            zone = [ "g"; "s" ];
            body = { node = Workspace.Call_fn { fn = "f"; args = [ _; _ ]; _ }; _ };
            _;
          } ->
          ()
      | _ -> failwith "sum is not a Loop");
      (match l.node with
      | Workspace.Hof
          ( `Map,
            [
              {
                node =
                  Workspace.Fn { zone = [ "g"; "l#0" ]; params = [ (Workspace.Name "k", None) ]; _ };
                _;
              };
              _;
            ] ) ->
          ()
      | _ -> failwith "map is not a Hof");
      (match c.node with Workspace.Cond ([ _ ], _) -> () | _ -> failwith "cond");
      (match k.node with Workspace.Case (_, [ _ ], _) -> () | _ -> failwith "case");
      assert (l.ty = Ty.List Ty.Int && k.ty = Ty.Text && s.ty = Ty.Float)
  | _ -> failwith "unexpected IR");
  (* a defn body is typed once per call site, but each problem is reported once *)
  (match
     run
       "(workspace w (defn f :context value [(x : float)] (+ x \"a\")) (graph g :context value (+ \
        (f 1) (+ (f 2) (f 3)))))"
   with
  | None, ds -> assert (List.length (errors ds) = 1)
  | Some _, _ -> failwith "accepted");
  (* every diagnostic carries a span in the source *)
  (match run (value "(let* [a 1] b)") with
  | _, [ { Diagnostic.span = Some { start; finish }; _ } ] -> assert (start > 0 && finish > start)
  | _ -> failwith "no span");
  (* the workspace is only returned without errors; warnings do not block it *)
  match run (sop "(sop/blast (sop/box) :group \"nobody\")") with
  | Some _, [ { severity = Diagnostic.Warning; _ } ] -> ()
  | _ -> failwith "warning blocked the workspace"

(* ---- the check.cjs suite, static half: one [t] per study test ---- *)

let ported = ref 0
and problems = ref []

let t name f =
  match f () with
  | () -> incr ported
  | exception e -> problems := (name, Printexc.to_string e) :: !problems

let printed forms = fst (Lisp.print forms)
let one source = match parse source with [ f ] -> f | _ -> failwith "expected one form"

let workspace_children source =
  match (one source).Syntax.node with
  | Syntax.List (_ :: _ :: children) -> children
  | _ -> failwith "not a workspace"

let head_is name (x : Syntax.t) =
  match x.node with Syntax.List ({ node = Syntax.Sym h; _ } :: _) -> h = name | _ -> false

let macros_of source = List.filter (head_is "defmacro") (workspace_children source)
let get_ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let trim s = String.trim s

let () =
  (* the 12 case studies: check, print canonically, round-trip *)
  let files =
    Sys.readdir cases |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".lisp")
    |> List.sort compare
  in
  List.iter
    (fun file ->
      t
        ("case " ^ file ^ " checks and round-trips")
        (fun () ->
          let source = read (Filename.concat cases file) in
          let text = printed (parse source) in
          ignore (good text);
          if printed (parse text) <> text then failwith "canonical print is not stable"))
    files

let () =
  (* the first block *)
  t "shadowing is an error" (fun () ->
      bad
        (sop "(let* [a 1 b (for [a (range 3)] (sop/box))] (sop/merge b))")
        "E_SHADOW" ~text:"shadows");
  t "fold body must match the accumulator" (fun () ->
      bad (sop "(fold [g (sop/box)] [i (range 3)] 1.0)") "E_ACC_TYPE" ~text:"accumulator type");
  t "for iterates lists only" (fun () ->
      bad (sop "(sop/merge (for [i 3] (sop/box)))") "E_TYPE" ~text:"iterates a list");
  t "iteration bound names the zone" (fun () ->
      bad (sop "(sop/merge (for [i (range 5000)] (sop/box)))") "E_ITER_BOUND" ~text:"4,096");
  t "recursion is rejected, fold suggested" (fun () ->
      bad "(workspace w (defn f :context sop [(x : float)] (f x)) (graph g :context sop (f 1)))"
        "E_RECURSION" ~text:"Use fold");
  t "both if branches are typed" (fun () ->
      bad (sop "(if (< 1 2) (sop/box) 3)") "E_TYPE" ~text:"Both branches");
  t "values cannot leave through an unbound name" (fun () ->
      bad
        (sop
           "(let* [a (for [i (range 2)] (sop/box))] (sop/transform (sop/merge a) :rotate [0 0 i]))")
        "E_UNBOUND" ~text:"not bound");
  t "unknown ref input" (fun () ->
      bad
        "(workspace w (graph a :context value [(n : int 2)] n) (graph b :context value (ref a :m \
         1)))"
        "E_UNKNOWN_PARAM" ~text:"no input :m");
  t "graph inputs need defaults" (fun () ->
      bad "(workspace w (graph a :context value [(n : int)] n))" "E_INPUT_DEFAULT"
        ~text:"needs a default");
  t "2.0 stays a float" (fun () ->
      let text = printed (parse "(workspace w (graph g :context value 2.0))") in
      let has s sub =
        let n = String.length sub in
        let rec at i = i + n <= String.length s && (String.sub s i n = sub || at (i + 1)) in
        at 0
      in
      assert (has text "2.0"))

let () =
  (* 1. function values *)
  t "fn: arity is checked" (fun () ->
      bad (value "(let* [f (fn [a b] a)] (f 1))") "E_ARITY" ~text:"takes 2 arguments; got 1");
  t "fn: annotated type is checked statically" (fun () ->
      bad
        (value "(let* [f (fn [(a : float)] a)] (if false (f \"x\") 1))")
        "E_TYPE" ~text:"expected float, got text");
  t "fn: a function cannot see its own name" (fun () ->
      bad (value "(let* [f (fn [x] (f x))] (f 1))") "E_UNKNOWN_KIND" ~text:"Unknown operator");
  let escapes = "cannot be stored or returned" in
  t "fn: escaping as a graph result is an error" (fun () ->
      bad (value "(let* [f (fn [x] x)] f)") "E_FN_ESCAPES" ~text:escapes);
  t "fn: escaping into a list is an error" (fun () ->
      bad (value "(let* [f (fn [x] x) l (list f)] 1)") "E_FN_ESCAPES" ~text:escapes);
  t "fn: escaping into a record is an error" (fun () ->
      bad (value "(let* [f (fn [x] x) r {:f f}] 1)") "E_FN_ESCAPES" ~text:escapes);
  t "fn: escaping as a fold accumulator is an error" (fun () ->
      bad (value "(let* [f (fn [x] x)] (fold [a f] [i (range 2)] 1))") "E_FN_ESCAPES" ~text:escapes);
  t "fn: escaping from a function body or HOF is an error" (fun () ->
      bad (value "(count (map (fn [x] (fn [y] y)) (list 1)))") "E_FN_ESCAPES" ~text:escapes);
  t "fn: escaping through a ref input is an error" (fun () ->
      bad
        "(workspace w (graph a :context value [(n : int 2)] n) (graph g :context value (let* [f \
         (fn [x] x)] (ref a :n f))))"
        "E_FN_ESCAPES" ~text:escapes);
  t "fn: escaping as a defn result is an error" (fun () ->
      bad "(workspace w (defn h :context value [(k : fn)] k) (graph g :context value 1))"
        "E_FN_ESCAPES" ~text:escapes);
  t "fn: graph inputs cannot be functions" (fun () ->
      bad "(workspace w (graph g :context value [(k : fn)] 1))" "E_FN_ESCAPES" ~text:escapes);
  t "fn: a macro is not a function value" (fun () ->
      bad
        "(workspace w (defmacro dbl [x] `(+ ~x ~x)) (graph g :context value (count (map dbl (list \
         1)))))"
        "E_MACRO_AS_VALUE" ~text:"macro is not a function value");
  t "fn: defn recursion through a function value is rejected" (fun () ->
      bad
        "(workspace w (defn app :context value [(f : fn) (x : float)] (f f x)) (graph g :context \
         value (app app 1)))"
        "E_RECURSION" ~text:"Recursive call");
  t "hof: filter predicate must return bool" (fun () ->
      bad
        (value "(count (filter (fn [x] \"no\") (list 1)))")
        "E_TYPE" ~text:"predicate returns bool");
  t "hof: reduce checks the accumulator type" (fun () ->
      bad (value "(reduce (fn [a x] \"s\") 0 (list 1))") "E_TYPE" ~text:"accumulator type")

let () =
  (* 2. lists, patterns; 3. records; 4. branches *)
  t "list: elements share one type" (fun () ->
      bad (value "(count (list 1 \"a\"))") "E_TYPE" ~text:"share one type");
  t "pattern: too few elements is an error" (fun () ->
      bad
        (value "(let* [[a b c] (list 1 2)] a)")
        "E_PATTERN" ~text:"needs 3 elements; the list has 2");
  t "pattern: a missing record field is a static error" (fun () ->
      bad (value "(let* [{:keys [a c]} {:a 1}] a)") "E_FIELD" ~text:"no field c");
  t "pattern: names must not be reserved words" (fun () ->
      bad (value "(let* [map 1] map)") "E_BINDING" ~text:"Invalid binding name map";
      bad (value "(let* [[a list] (list 1 2)] a)") "E_BINDING" ~text:"Invalid binding name list");
  t "record: assoc keeps the field type" (fun () ->
      bad
        (value "(let* [r {:a 1} s (assoc r :a \"x\")] 1)")
        "E_TYPE" ~text:"assoc :a is int; got text");
  t "record: unknown field" (fun () ->
      bad (value "(let* [r {:a 1}] r.b)") "E_FIELD" ~text:"no field b. Fields: a");
  t "record: fits is structural and field order does not matter" (fun () ->
      let ty s = Option.get (Ty.of_string s) in
      assert (Ty.fits (ty "rec{a:int,b:vec3}") (ty "rec{b:vec3}"));
      assert (not (Ty.fits (ty "rec{b:vec3}") (ty "rec{a:int,b:vec3}")));
      assert (Ty.fits (ty "list:rec{pos:vec3,size:int}") (ty "list:rec{size:float}")));
  t "record: type strings nest" (fun () ->
      let s = "list:rec{pos:vec3,kids:list:rec{n:int}}" in
      assert (Ty.to_string (Option.get (Ty.of_string s)) = s);
      assert (
        Ty.of_string "rec{a:list:rec{x:float},b:int}"
        = Some (Ty.Record [ ("a", Ty.List (Ty.Record [ ("x", Ty.Float) ])); ("b", Ty.Int) ])));
  t "record: reader makes flagged arrays" (fun () ->
      (match (one "{:a 1 :b [1 2 3]}").node with
      | Syntax.Map [ _; _; _; { node = Syntax.Vec [ _; _; _ ]; _ } ] -> ()
      | _ -> failwith "not a map");
      assert (trim (printed (parse "{:a 1 :b [1 2 3]}")) = "{:a 1 :b [1 2 3]}"));
  t "cond needs :else" (fun () -> bad (value "(cond (< 2 1) 1)") "E_NO_ELSE" ~text:"final :else");
  t "cond arms must agree" (fun () ->
      bad (value "(cond true 1 :else \"a\")") "E_TYPE" ~text:"one type");
  t "case needs :else and literal tests" (fun () ->
      bad (value "(case 1 1 2)") "E_NO_ELSE" ~text:"final :else";
      bad (value "(let* [k 1] (case 1 k 2 :else 0))") "E_CASE" ~text:"literal")

let () =
  (* 5. macros *)
  let radial =
    "(defmacro radial [i n body] `(sop/merge (for [~i (range ~n)] (sop/transform ~body :rotate (* \
     (/ ~i ~n) 360)))))"
  in
  let swap = "(defmacro add1 [a] `(let* [t# ~a] (+ t# 1)))" in
  t "macro: expand is deterministic and fresh names are distinct" (fun () ->
      let ms = macros_of ("(workspace w " ^ swap ^ " (graph g :context value 1))") in
      let call = one "(+ (add1 1) (add1 (add1 2)))" in
      let a = printed [ get_ok (Macro.expand ms call) ] in
      assert (a = printed [ get_ok (Macro.expand ms call) ]);
      assert (
        trim a
        = "(+ (let* [t__1 1] (+ t__1 1)) (let* [t__2 (let* [t__3 2] (+ t__3 1))] (+ t__2 1)))"));
  t "macro: expandOnce steps leftmost-outermost" (fun () ->
      let ms = macros_of ("(workspace w " ^ swap ^ " " ^ radial ^ " (graph g :context value 1))") in
      let st = Macro.state () in
      let x = one "(radial k 2 (add1 1))" in
      let s1 = get_ok (Macro.expand_once ~state:st ms x) in
      assert (
        trim (printed [ s1 ])
        = "(sop/merge (for [k (range 2)] (sop/transform (add1 1) :rotate (* (/ k 2) 360))))");
      let s2 = get_ok (Macro.expand_once ~state:st ms s1) in
      assert (get_ok (Macro.expand_once ~state:st ms s2) == s2);
      assert (printed [ s2 ] = printed [ get_ok (Macro.expand ms x) ]));
  t "macro: free template names would capture" (fun () ->
      bad "(workspace w (defmacro m [x] `(+ ~x y)) (graph g :context value (let* [y 1] (m 2))))"
        "E_MACRO_CAPTURE" ~text:"would capture a name from the call site");
  t "macro: template bindings must be fresh" (fun () ->
      bad "(workspace w (defmacro m [x] `(let* [tmp ~x] tmp)) (graph g :context value (m 2)))"
        "E_MACRO_CAPTURE" ~text:"would capture");
  t "macro: only parameters are unquoted" (fun () ->
      bad "(workspace w (defmacro m [x] `(+ ~x ~(+ 1 2))) (graph g :context value (m 2)))"
        "E_MACRO_UNQUOTE" ~text:"macros unquote only their parameters");
  t "macro: expansion depth is limited" (fun () ->
      bad "(workspace w (defmacro m [x] `(m ~x)) (graph g :context value (m 2)))" "E_MACRO_DEPTH"
        ~text:"32 nested expansions");
  t "macro: expansion size is limited" (fun () ->
      bad
        ("(workspace w (defmacro d [x] `(+ ~x ~x)) (graph g :context value "
        ^ String.concat "" (List.init 12 (fun _ -> "(d "))
        ^ "1" ^ String.make 12 ')' ^ "))")
        "E_MACRO_SIZE" ~text:"5,000 forms");
  t "macro: arity is checked" (fun () ->
      bad
        ("(workspace w " ^ radial ^ " (graph g :context sop (radial k 2)))")
        "E_MACRO_ARITY" ~text:"expects 3 arguments; got 2")

(* not ported: "freeSymbols understands fn, patterns, records and quoting" tests a JS helper the
   canvas uses; the OCaml checker computes liveness and captures inside Workspace instead. *)
let () =
  (* 6. comments and metadata *)
  let noted =
    "; lead\n\
     (workspace w\n\
    \  ; a helper\n\
    \  (defn f :context value [(x : float)] (* x 2))\n\
    \  (graph g :context value\n\
    \    (let* [; first\n\
    \           a 1\n\
    \           ; the pattern\n\
    \           [b c] (list 1 2)\n\
    \           q (f a)]\n\
    \      ; result note\n\
    \      (+ a (+ b (* c q)))))\n\
    \  ; tail\n\
    \  )"
  in
  t "notes attach to the next element by name" (fun () ->
      let ws = one noted in
      assert (ws.notes = [ "lead" ] && ws.tail = [ "tail" ]);
      let defn = List.find (head_is "defn") (workspace_children noted) in
      assert (defn.notes = [ "a helper" ]);
      let graph = List.find (head_is "graph") (workspace_children noted) in
      match graph.node with
      | Syntax.List
          [
            _;
            _;
            _;
            _;
            { node = Syntax.List [ _; { node = Syntax.Vec (a :: _ :: bc :: _); _ }; result ]; _ };
          ] ->
          assert (
            a.notes = [ "first" ] && bc.notes = [ "the pattern" ]
            && result.notes = [ "result note" ])
      | _ -> failwith "graph shape");
  t "notes survive clone and print stably" (fun () ->
      let text = printed (parse noted) in
      let has sub =
        let n = String.length sub in
        let rec at i = i + n <= String.length text && (String.sub text i n = sub || at (i + 1)) in
        at 0
      in
      assert (has "  ; a helper\n  (defn f" && has "; the pattern\n           [b c]");
      assert (printed (parse text) = text));
  t "setNote edits and removes notes" (fun () ->
      let x = one (value "(let* [a 1 b 2] b)") in
      let rec set_notes key notes (y : Syntax.t) =
        let y = if y.node = Syntax.Sym key && y.span.start > 0 then y else y in
        let map = List.map (set_notes key notes) in
        let node =
          match y.node with
          | Syntax.List l -> Syntax.List (map l)
          | Syntax.Vec l -> Syntax.Vec (map l)
          | Syntax.Map l -> Syntax.Map (map l)
          | n -> n
        in
        {
          y with
          node;
          notes = (match y.node with Syntax.Sym k when k = key -> notes | _ -> y.notes);
        }
      in
      let has s sub =
        let n = String.length sub in
        let rec at i = i + n <= String.length s && (String.sub s i n = sub || at (i + 1)) in
        at 0
      in
      let edited = printed [ set_notes "b" [ "two"; "lines" ] x ] in
      assert (has edited "; two\n" && has edited "; lines\n");
      assert (not (has (printed [ set_notes "b" [] (set_notes "b" [ "two" ] x) ]) ";")));
  t "bypass prints and clones" (fun () ->
      let x = one (sop "(let* [a (sop/box) b ^:bypass (sop/subdivide a)] b)") in
      let y, _ = Syntax.renumber 1000 x in
      let text = printed [ y ] in
      let has sub =
        let n = String.length sub in
        let rec at i = i + n <= String.length text && (String.sub text i n = sub || at (i + 1)) in
        at 0
      in
      assert (has "b ^:bypass (sop/subdivide a)"));
  t "bypass needs a fitting input" (fun () ->
      bad
        (sop "(sop/points ^:bypass (sop/point_list (sop/box)))")
        "E_BYPASS" ~text:"can't bypass sop/point_list";
      bad (sop "^:bypass (sop/box :size 2)") "E_BYPASS" ~text:"can't bypass");
  t "unknown metadata lists ^:bypass" (fun () ->
      bad (sop "^:mute (sop/box)") "E_META" ~text:"only metadata is ^:bypass")

let () =
  (* 7. reserved names, and t *)
  t "SPECIAL and RESERVED name the new forms" (fun () ->
      List.iter
        (fun k ->
          bad (value (Printf.sprintf "(let* [%s 1] 1)" k)) "E_BINDING" ~text:"Invalid binding name")
        [
          "fn";
          "cond";
          "case";
          "list";
          "values";
          "quote";
          "quasiquote";
          "unquote";
          "get";
          "assoc";
          "str";
          "map";
          "filter";
          "reduce";
          "sort-by";
          "concat";
        ]);
  t "E_TIME_COUNT: a loop count that depends on t is rejected" (fun () ->
      bad (sop "(sop/merge (for [i (range (+ 3 (floor (* 2 (sin t)))))] (sop/box)))") "E_TIME_COUNT");
  t "E_TIME_BRANCH: t choosing between shapes is rejected; t choosing a value is fine" (fun () ->
      bad (sop "(if (> (sin t) 0.5) (sop/box) (sop/uv_sphere))") "E_TIME_BRANCH";
      ignore (good (sop "(sop/box :size (if (> (sin t) 0.5) 1 0.5))")));
  t "t is reserved: it cannot be bound" (fun () ->
      bad (sop "(let* [t 1] (sop/box))") "E_BINDING" ~text:"context time")

let () =
  (* register L16: :skip *)
  t "skip: a for and a scene/merge take it" (fun () ->
      ignore (good (value "(let* [z (for [i (range 4)] :skip [1 [3]] i)] 1)"));
      ignore (good (value "(let* [z (for [i (range 2) j (range 2)] :skip [[0 1] [1 0]] i)] 1)"));
      ignore
        (good
           "(workspace w (graph s :context scene (scene/merge (scene/light) (scene/light) :skip \
            [[1]])))"));
  t "skip: a list of tuples of non-negative integers" (fun () ->
      bad (value "(let* [z (for [i (range 4)] :skip 3 i)] 1)") "E_SKIP";
      bad (value "(let* [z (for [i (range 4)] :skip [-1] i)] 1)") "E_SKIP";
      bad (value "(let* [z (for [i (range 4)] :skip [1.5] i)] 1)") "E_SKIP";
      bad (value "(let* [z (for [i (range 4)] :skip [[0 x]] i)] 1)") "E_SKIP";
      bad "(workspace w (graph s :context scene (scene/merge (scene/light) :skip [a])))" "E_SKIP");
  t "skip: only a for has it" (fun () ->
      bad (value "(sum [i (range 4)] :skip [1] i)") "E_ZONE";
      bad (value "(fold [a 0] [i (range 3)] :skip [1] a)") "E_ZONE");
  t "skip: a static list; a count that depends on t is still rejected" (fun () ->
      bad
        (sop "(sop/merge (for [i (range (+ 3 (floor (* 2 (sin t)))))] :skip [0] (sop/box)))")
        "E_TIME_COUNT";
      let ws = good (sop "(sop/merge (for [i (range 4)] :skip [2] (sop/box)))") in
      assert (Workspace.Paths.is_empty ws.live))

let () =
  if !problems <> [] then begin
    List.iter (fun (n, e) -> prerr_endline ("FAIL " ^ n ^ ": " ^ e)) (List.rev !problems);
    exit 1
  end;
  Printf.printf
    "Flow workspace: 12 fixtures, register rules, liveness and macros pass (%d check.cjs cases \
     ported here)\n"
    !ported

(* The audit's language cases (report.md L1, L5, L7, L8, L15, L17, L20). *)
let () =
  (* two inline zones under one binding have paths of their own, so their compiled ids differ *)
  let ws =
    good
      (sop
         "(sop/merge (sop/merge (for [i (range 2)] (sop/box))) (sop/merge (for [j (range 3)] \
          (sop/box))))")
  in
  let zones = ref [] in
  let rec walk (t : Workspace.term) =
    match t.node with
    | Workspace.Loop { zone; body; _ } ->
        zones := zone :: !zones;
        walk body
    | Call { args; _ } | Op { args; _ } -> List.iter (fun (_, a) -> walk a) args
    | _ -> ()
  in
  walk (List.hd ws.graphs).body;
  assert (List.length !zones = 2 && List.length (List.sort_uniq compare !zones) = 2);
  (* a positional argument may follow a keyword; an input given twice is an error *)
  ignore (good (sop "(let* [a (sop/box)] (sop/transform :translate [1 2 3] a))"));
  bad
    (sop "(sop/boolean :left (sop/box) (sop/torus))")
    "E_DUPLICATE_PARAM" ~text:"left is given twice";
  (* if has the type both branches fit *)
  bad (value "(count (range (if false 3 (list 1 2 3))))") "E_TYPE";
  (* an operator a caller can bind is not free as an argument of a template *)
  bad "(workspace w (defmacro m [x] `(+ count ~x)) (graph g :context value (let* [count 2] (m 1))))"
    "E_MACRO_CAPTURE";
  ignore
    (good "(workspace w (defmacro m [xs] `(count ~xs)) (graph g :context value (m (list 1 2))))");
  (* metadata outside an expression is reported *)
  warns "^:nonsense (workspace w (graph g :context value 1))" "W_UNKNOWN_META";
  warns "(workspace w ^:nonsense (graph g :context value 1))" "W_UNKNOWN_META";
  quiet "^:allow-warnings (workspace w (graph g :context value 1))";
  (* the static bound does not overflow *)
  bad
    (value
       ("(count (for ["
       ^ String.concat " " (List.init 6 (fun i -> Printf.sprintf "i%d (range 4096)" i))
       ^ "] 1))"))
    "E_ITER_BOUND";
  (* a constant has no fields; messages keep UTF-8 as written *)
  bad (value "t.x") "E_FIELD" ~text:"t is a constant";
  bad (sop "(sop/material (sop/box) :color \"h\xc3\xa9\")") "E_TYPE" ~text:"\"h\xc3\xa9\"";
  bad (sop "(sop/merge (for [i (range 2)] :skip [[0 x]] (sop/box)))") "E_SKIP"

let () =
  let candidate source = value ("(let* [candidate " ^ source ^ "] 0)") in
  let is_candidate ws path =
    match List.assoc_opt path ws.Workspace.packed with
    | Some (Workspace.Candidate producers) -> not (Workspace.Paths.is_empty producers)
    | _ -> false
  in
  let approximable source =
    let ws = good (candidate source) in
    assert (Workspace.Paths.is_empty ws.approx);
    is_candidate ws [ "g"; "candidate" ]
  in
  assert (approximable "(map (fn [(x : float)] (if (> x 0) (sin x) (cos x))) (array/float 4))");
  assert (approximable "(for [x (array/float 4)] (* x 2.0))");
  assert (approximable "(map (fn [x] (+ x (frame/dt))) (array/float 4))");
  assert (not (approximable "(for [x (array/float 4) y (array/float 4)] (+ x y))"));
  assert (approximable "(for [x (array/float 4)] :skip [0] (+ x 1.0))");
  assert (not (approximable "(fold [a 0.0] [x (array/float 4)] (+ a x))"));
  assert (not (approximable "(map (fn [x] (sin x)) (list 1 2 3))"));
  assert (not (approximable "(filter (fn [x] (> x 0)) (array/float 4))"));
  assert (approximable "(map (fn [x] (floor x)) (array/float 4))");
  assert (approximable "(map (fn [p] (length p)) (array/vec3 4))");
  bad (candidate "(map (fn [x] (length x)) (array/float 4))") "E_TYPE" ~text:"length needs a vec3";
  assert (not (approximable "(map (fn [(x : int)] (+ x 1)) (array/float 4))"));
  assert (approximable "(let* [f (fn [(x : float)] (sqrt (abs x))) xs (map f (array/float 4))] xs)");
  assert (not (approximable "(let* [xs (map sin (array/float 4))] (exact xs))"));
  let ws =
    good
      "(workspace w (defn wave :context value [(x : float)] (sin x))\n\
      \    (graph g :context value (let* [candidate (map wave (array/float 4))] 0)))"
  in
  assert (is_candidate ws [ "g"; "candidate" ]);
  let opaque =
    Catalog.
      {
        qualified = "value/opaque";
        context = Context.value;
        slots = [];
        parameters =
          [
            {
              name = "value";
              label = "Value";
              ty = Some Port_type.Float;
              fields =
                [
                  ( "value",
                    Param.Floating_view
                      Param.{ soft_min = 0.; soft_max = 1.; hard_min = None; hard_max = None },
                    Param.Float_value 0. );
                ];
              folder = [];
              primary = false;
              unit = None;
            };
          ];
        outputs = [ ("out", Port_type.Float) ];
        facts = None;
      }
  in
  let catalog = { catalog with kinds = opaque :: catalog.kinds } in
  let checked source =
    match Workspace.check ~ops:[] catalog (parse source) with
    | Some ws, _ -> ws
    | None, ds -> failwith (show ds)
  in
  let is source = is_candidate (checked source) [ "g"; "candidate" ] in
  assert (is (candidate "(map (fn [(p : vec3)] (noise3 p :seed 7 :octaves 3)) (array/vec3 4))"));
  assert (
    is
      "(workspace w (graph g :context value [(seed : int 7)]\n\
      \    (let* [candidate (map (fn [(p : vec3)] (noise3 p :seed seed)) (array/vec3 4))] 0)))");
  assert (is (candidate "(map (fn [(p : vec3)] (noise3 p :octaves 99)) (array/vec3 4))"));
  assert (is (candidate "(map (fn [x] (value/opaque :value x)) (array/float 4))"));
  List.iter
    (fun name ->
      assert (Option.is_some (Op.find name Context.value) || List.mem name Packed_ops.noise_names))
    Packed_ops.names;
  assert (not (Packed_ops.supports "exact"));
  print_endline
    "workspace candidates: raw approximation empty; maps/collect, aliases, exact, structural \
     refusals and uncertain captures passed"

let () =
  let producer = "(map (fn [x] (+ x 1.0)) (array/float 4))" in
  let checks body = "(let* [producer " ^ producer ^ " alias producer] " ^ body ^ ")" in
  List.iter
    (fun source -> bad ~text:"Producer: g/producer; sink: g/@result" source "E_APPROX_SINK")
    [
      sop (checks "(sop/box :size (array/sum alias))");
      value (checks "(state [s alias] s)");
      "(workspace w (graph consumer :context value [(xs : float 0.0)] 0)\n\
      \       (graph g :context value "
      ^ checks "(ref consumer :xs (array/sum alias))"
      ^ "))";
    ];
  List.iter
    (fun (context, body) ->
      bad ~text:"Producer: g/producer; sink: g/@result"
        ("(workspace w (graph g :context " ^ context ^ " " ^ checks body ^ "))")
        "E_APPROX_SINK")
    [
      ("settings", "(settings/config :width (count alias))");
      ("scene", "(scene/camera :fov (array/sum alias))");
    ];
  ignore (good (sop (checks "(sop/box :size (array/sum (exact alias)))")));
  let marked form =
    match
      List.assoc_opt [ "g"; "producer" ]
        (good
           ("(workspace w (graph g :context draw (let* [producer " ^ form
          ^ "] (draw/circles producer))))"))
          .packed
    with
    | Some (Workspace.Candidate _) -> true
    | _ -> false
  in
  let vectors = "(map (fn [x] [x 0 0]) (array/float 4))" in
  assert (marked ("(cond true " ^ vectors ^ " :else " ^ vectors ^ ")"));
  assert (marked ("(case 2 1 " ^ vectors ^ " :else " ^ vectors ^ ")"));
  assert (marked "(map (fn [x] (cond (> x 0) [x 0 0] :else [0 0 0])) (array/float 4))");
  assert (marked "(map (fn [x] (case x 0 [0 0 0] :else [x 0 0])) (array/float 4))");
  assert (
    Op.is_display_kind "draw/circles" && Op.is_display_kind "ui/canvas"
    && not (Op.is_display_kind "scene/camera"));
  let kind =
    Catalog.
      {
        qualified = "value/test_sink";
        context = Context.value;
        slots =
          [ { name = "source"; required = true; rest = false; ty = Some (Ty.Array Ty.Float) } ];
        parameters = [];
        outputs = [ ("out", Port_type.Float) ];
        facts = None;
      }
  in
  let checked body =
    Workspace.check { catalog with kinds = kind :: catalog.kinds } (parse (value (checks body)))
  in
  (match checked "(value/test_sink alias)" with
  | None, ds -> assert (List.exists (fun (d : Diagnostic.t) -> d.code = "E_APPROX_SINK") ds)
  | _ -> assert false);
  (match checked "(value/test_sink (exact alias))" with
  | Some _, ds -> assert (errors ds = [])
  | _ -> assert false);
  print_endline
    "workspace precision: producer/sink paths, aliases, exact, cond/case and display consumers \
     passed"
