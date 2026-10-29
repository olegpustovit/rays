open Flow
open Value

let nominal_samples =
  [
    (Ty.panel, Struct ("ui/outline", Ty.panel, []));
    (Ty.scene, Struct ("scene/merge", Ty.scene, []));
    (Ty.world, Struct ("world/none", Ty.world, []));
    (Ty.settings, Struct ("settings/config", Ty.settings, []));
    (Ty.editor, Struct ("ui/workspace", Ty.editor, []));
    (Ty.material, Struct ("material/standard", Ty.material, []));
    (Ty.geometry, No_geo);
  ]

let rec sample name = function
  | Ty.Int -> Int (if name = "index" || name = "active" || name = "first" then 0 else 2)
  | Float -> Float (if name = "ratio" || name = "gain" then 0.5 else 1.5)
  | Bool -> Bool true
  | Vec3 -> Vec3 (1., 2., 3.)
  | Vec2 -> Vec2 (1., 2.)
  | Vec4 -> Vec4 (1., 2., 3., 4.)
  | Text -> Text (if name = "axis" then "horizontal" else if name = "button" then "left" else "a")
  | Color -> Text "#336699"
  | List e -> List [| sample "" e; sample "" e |]
  | Array Ty.Int when name = "offsets" -> Int_array [| 0; 2 |]
  | Array e -> Value.array_init e 2 (fun _ -> sample "" e)
  | Named _ as ty -> Option.value ~default:(Deferred (ty, 0)) (List.assoc_opt ty nominal_samples)
  | Any -> Float 1.5
  | Record fs -> Record (List.map (fun (n, t) -> (n, sample n t)) fs)
  | Fn _ -> Fn ()

let check (o : Op.t) args =
  if o.name = "error" then (
    try
      ignore (o.body ~live:(Input.at_time 0.) ~node:(fun _ _ -> assert false) args);
      failwith "error returned"
    with Diagnostic.Error d ->
      assert (d.code = "a" && d.message = "a");
      No_geo)
  else
    try
      let made = ref [] in
      let result_type args =
        Op.result_type o
          (List.map
             (fun (name, v) ->
               let literal =
                 match v with
                 | Int n -> Some (Param.Int_value n)
                 | Float n -> Some (Param.Float_value n)
                 | Bool b -> Some (Param.Bool_value b)
                 | Text s -> Some (Param.Text_value s)
                 | _ -> None
               in
               (name, Value.ty_of v, literal))
             args)
      in
      let node name args =
        let id = List.length !made in
        made := (name, args) :: !made;
        Deferred (result_type args, id)
      in
      let result = o.body ~live:(Input.at_time 0.) ~node args in
      assert (Ty.fits (Value.ty_of result) (result_type args));
      (match o.shape with
      | Op.Struct _ ->
          assert (
            match result with Struct _ -> true | Deferred (ty, _) -> Ty.is_cooked ty | _ -> false)
      | Scalar -> ());
      result
    with Diagnostic.Error d -> failwith (o.name ^ ": " ^ d.code ^ ": " ^ d.message)

let () =
  assert (List.length Op.all = 198);
  for id = 0 to 9 do
    Ray_host.register id (fun _ -> Ray_host.miss)
  done;
  let repr = Option.get (Op.find "text/repr" Context.value) in
  List.iter
    (fun text -> assert (check repr [ ("text", Text text) ] = Text (Printf.sprintf "%S" text)))
    [ String.init 256 Char.chr; ""; "é日本😀\000\b\t\n\r\\\"" ];
  List.iter
    (fun (o : Op.t) ->
      assert (Option.get (Op.find o.name o.ctx) == o);
      assert (Option.is_some (Op.packed_kind o) = (o.ctx = Context.value && o.shape = Op.Scalar));
      let s = o.signature in
      let base =
        List.map
          (fun (n, t) ->
            ( n,
              match (o.name, n) with
              | "array/scatter_add", "indices" -> Int_array [| 0; 1 |]
              | "array/compact", "mask" -> Int_array [| 0; 1 |]
              | "array/index_of", "keys" -> Int_array [| 1; 2 |]
              | "array/index_of", "queries" -> Int_array [| 2; 3 |]
              | ("asin" | "acos"), _ -> Float 0.5
              | _ -> sample n t ))
          s.pos
      in
      let optional = [ base; base @ List.map (fun (n, t) -> (n, sample n t)) s.opt ] in
      List.iter
        (fun args ->
          ignore (check o args);
          (match s.rest with
          | None -> ()
          | Some (n, t) -> ignore (check o (args @ [ (n, sample n t); (n, sample n t) ])));
          List.iter
            (fun (n, t) ->
              let v =
                match List.assoc_opt n o.choices with Some (c :: _) -> Text c | _ -> sample n t
              in
              ignore (check o (args @ [ (n, v) ])))
            s.kw)
        optional;
      let valid = match s.rest with Some (n, t) -> base @ [ (n, sample n t) ] | None -> base in
      o.check valid;
      List.iter
        (fun (n, t) ->
          let v =
            match List.assoc_opt n o.choices with Some (c :: _) -> Text c | _ -> sample n t
          in
          o.check (valid @ [ (n, v) ]))
        s.kw;
      List.iter
        (fun (n, choices) ->
          assert (List.mem_assoc n (s.pos @ s.opt @ s.kw));
          assert (choices <> []))
        o.choices;
      match Op.arith o.name with
      | None -> ()
      | Some binary ->
          List.iter
            (fun (a, b) -> assert (binary.apply a b = check o [ ("a", a); ("b", b) ]))
            [
              (Int 2, Int 3);
              (Float 1.5, Int 2);
              (Vec3 (1., 2., 3.), Float 2.);
              (Float 2., Vec3 (1., 2., 3.));
            ])
    (List.filter (fun (o : Op.t) -> not (String.starts_with ~prefix:"gpu/" o.name)) Op.all);
  (* raster-stage intrinsics have no CPU value: every evaluation refuses them *)
  List.iter
    (fun name ->
      let o = Option.get (Op.find name Context.value) in
      match
        o.body ~live:(Input.at_time 0.)
          ~node:(fun _ _ -> assert false)
          (List.map (fun (n, t) -> (n, sample n t)) o.signature.pos)
      with
      | exception Diagnostic.Error d -> assert (d.code = "E_GPU_STAGE")
      | _ -> assert false)
    [
      "gpu/fwidth";
      "gpu/sample";
      "gpu/sample-level";
      "gpu/read";
      "gpu/texture-size";
      "gpu/instance-id";
      "gpu/front-facing";
      "gpu/depth";
      "gpu/fragment-x";
      "gpu/fragment-y";
      "gpu/trace-triangles";
      "gpu/trace-any";
      "gpu/trace-curves";
      "gpu/trace-curves-any";
      "gpu/scene-vertex";
      "gpu/scene-vertex-end";
      "gpu/scene-instance";
      "gpu/scene-sphere";
      "gpu/scene-strand";
      "gpu/scene-spheres";
      "gpu/scene-strands";
      "gpu/group-scan";
      "gpu/store";
      "gpu/store-int";
    ];
  List.iter
    (fun ctx ->
      List.iter
        (fun (o : Op.t) -> assert (o.ctx = ctx || o.ctx = Context.value))
        (Op.of_context ctx))
    (Context.all ());
  assert ((Option.get (Op.find "rand" Context.scene)).name = "value/rand");
  let sine = Option.get (Op.find "sin" Context.value) in
  assert (Op.packed_kind { sine with category = "Copied" } = None);
  let rejects name args =
    try
      (Option.get (Op.find name Context.editor)).check args;
      assert false
    with Diagnostic.Error d when d.code = "E_RANGE" -> ()
  in
  rejects "ui/graph" [ ("view", Text "invalid") ];
  rejects "ui/lisp" [ ("tab", Text "invalid") ];
  rejects "ui/split" [ ("axis", Text "invalid") ];
  rejects "ui/split-at" [ ("ratio", Float 1.) ];
  rejects "ui/tile" [];
  rejects "ui/switch" [ ("panel", sample "" Ty.panel); ("active", Int 1) ];
  (Option.get (Op.find "ui/graph" Context.editor)).check [ ("view", Residual ()) ];
  let empty = { Catalog.version = 1; kinds = [] } in
  let forms =
    Result.get_ok (Syntax.parse "(workspace w (graph g :context material (material/standard)))")
  in
  let ws, ds = Workspace.check empty forms in
  assert (
    ws <> None
    && not (List.exists (fun (d : Diagnostic.t) -> d.severity = Diagnostic.Error_severity) ds));
  let reserved, ds =
    Workspace.check empty
      (Result.get_ok
         (Syntax.parse "(workspace w (defmacro m [] `(standard)) (graph g :context material (m)))"))
  in
  assert (reserved <> None && ds = []);
  let known, ds =
    Workspace.check empty
      (Result.get_ok (Syntax.parse "(workspace w (graph g :context value (material/standard)))"))
  in
  assert (known = None && List.exists (fun (d : Diagnostic.t) -> d.code = "E_WRONG_CONTEXT") ds);
  (* Context survives short-name and function-value calls; the prefix has no semantic role. *)
  let k : Catalog.kind =
    {
      qualified = "custom/root";
      context = Context.scene;
      slots = [];
      parameters = [];
      outputs = [];
      facts = None;
    }
  in
  let ws, ds =
    Workspace.check { empty with kinds = [ k ] }
      (Result.get_ok (Syntax.parse "(workspace w (graph g :context scene (root)))"))
  in
  assert (ds = []);
  let evaluated = Result.get_ok (Eval.run ~time:0. (Option.get ws)) in
  assert (List.assoc "g" evaluated.results = Eval.Struct ("custom/root", Ty.scene, []));
  assert (Array.length evaluated.plan.nodes = 0);
  let k =
    { k with slots = [ { Catalog.name = "child"; required = true; rest = false; ty = None } ] }
  in
  let ws, ds =
    Workspace.check { empty with kinds = [ k ] }
      (Result.get_ok
         (Syntax.parse "(workspace w (graph g :context scene (first (map root (list nil)))))"))
  in
  assert (ds = []);
  let evaluated = Result.get_ok (Eval.run ~time:0. (Option.get ws)) in
  assert (
    List.assoc "g" evaluated.results
    = Eval.Struct ("custom/root", Ty.scene, [ ("child", Eval.No_geo) ]));
  assert (Array.length evaluated.plan.nodes = 0)

let () =
  let apply name args = check (Option.get (Op.find name Context.value)) args in
  let product = 131071 * 131071 * 19349663 in
  assert (apply "int/mul" [ ("a", Int (131071 * 131071)); ("b", Int 19349663) ] = Int product);
  assert (apply "int/mod" [ ("a", Int product); ("b", Int 97) ] = Int (product mod 97));
  assert (apply "int/and" [ ("a", Int product); ("b", Int max_int) ] = Int product);
  let values = Vec3_array [| 1.; 2.; 3.; 4.; 5.; 6. |] in
  let tail = apply "array/slice" [ ("array", values); ("first", Int 1); ("count", Int 1) ] in
  assert (tail = Vec3_array [| 4.; 5.; 6. |]);
  assert (
    apply "array/concat" [ ("first", tail); ("second", tail) ]
    = Vec3_array [| 4.; 5.; 6.; 4.; 5.; 6. |]);
  assert (apply "equal?" [ ("a", Text "key-pressed"); ("b", Text "key-pressed") ] = Bool true)

let () =
  let catalog = { Catalog.version = 1; kinds = [] } in
  let forms body =
    Result.get_ok (Syntax.parse ("(workspace w (graph g :context host " ^ body ^ "))"))
  in
  let checked ?(ops = []) body =
    match Workspace.check ~ops catalog (forms body) with
    | Some workspace, [] -> workspace
    | _, diagnostics -> failwith (String.concat "; " (List.map Diagnostic.to_string diagnostics))
  in
  let value body = List.assoc "g" (Result.get_ok (Eval.run ~time:0. (checked body))).results in
  let high = (1 lsl 53) + 1 in
  let text = string_of_int high in
  assert (Value.int_of (Int high) = high && Value.int_of (Int max_int) = max_int);
  assert (
    value (Printf.sprintf "(range %d %d)" high (high + 3))
    = Eval.List [| Int high; Int (high + 1); Int (high + 2) |]);
  assert (value ("(array/int 3 " ^ text ^ ")") = Eval.Int_array [| high; high; high |]);
  assert (value "(array/range 3 :type int)" = Eval.Int_array [| 0; 1; 2 |]);
  assert (value "(array/range 3)" = Eval.Float_array [| 0.; 1.; 2. |]);
  assert (
    value ("(map (fn [x] (int/add x 1)) (array/int 3 " ^ text ^ "))")
    = Eval.Int_array [| high + 1; high + 1; high + 1 |]);
  assert (value ("(for [x (array/int 65 " ^ text ^ ")] x)") = Eval.Int_array (Array.make 65 high));
  assert (value "(scan [a 0] [x (array/int 3 2)] (int/add a x))" = Eval.Int_array [| 2; 4; 6 |]);
  assert (value "(scan [a 7] [x (array/int 0)] (int/add a x))" = Eval.Int_array [||]);
  assert (value "(fold [a 7] [x (array/int 0)] (int/add a x))" = Eval.Int 7);
  assert (value "(reduce int/add 7 (array/int 0))" = Eval.Int 7);
  assert (value "(array/sum (array/int 0))" = Eval.Int 0);
  assert (value (Printf.sprintf "(array/sum (array/int 2 %d))" max_int) = Eval.Int (-2));
  assert (
    value
      ("(array/nth (array/concat (array/slice (array/int 3 " ^ text ^ ") 1 1) (array/int 1 0)) 0)")
    = Eval.Int high);
  let workspace = checked ("(map (fn [x] (int/add x 1)) (array/int 4097 " ^ text ^ "))") in
  let expected = Array.make 4097 (high + 1) in
  let run bulk =
    List.assoc "g"
      (Result.get_ok
         (Eval.Residual.handle ~bulk (fun () -> Eval.Host.static_with_requests workspace)))
        .results
  in
  assert (run (fun _ -> Some (Eval.Int_array expected)) = Eval.Int_array expected);
  (* A rounded prefix or wrong kind cannot replace exact integer evaluation. *)
  assert (run (fun _ -> Some (Eval.Int_array (Array.make 4097 high))) = Eval.Int_array expected);
  assert (
    run (fun _ -> Some (Eval.Float_array (Array.make 4097 (float (high + 1)))))
    = Eval.Int_array expected);
  let typed_result args =
    match List.find_opt (fun (name, _, _) -> name = "type") args with
    | None | Some (_, _, Some (Param.Text_value "float")) -> Ty.Array Ty.Float
    | Some (_, _, Some (Param.Text_value "vec3")) -> Ty.Array Ty.Vec3
    | _ -> Value.fail "E_TEST_TYPE" "The type must be literal."
  in
  let base = Option.get (Op.find "array/float" Context.value) in
  let operator =
    {
      base with
      name = "test/typed";
      signature = { pos = []; opt = []; rest = None; kw = [ ("type", Ty.Text) ] };
      choices = [];
      out = (fun _ -> Ty.Array Ty.Float);
      out_args = Some typed_result;
      body =
        (fun ~live:_ ~node:_ args ->
          match List.assoc_opt "type" args with
          | Some (Text "vec3") -> Vec3_array [| 1.; 2.; 3. |]
          | _ -> Float_array [| 1. |]);
    }
  in
  List.iter
    (fun (body, expected, ty) ->
      let workspace = checked ~ops:[ operator ] body in
      assert ((List.hd workspace.graphs).body.ty = ty);
      assert (List.assoc "g" (Result.get_ok (Eval.static workspace)).results = expected))
    [
      ("(test/typed)", Eval.Float_array [| 1. |], Ty.Array Ty.Float);
      ("(test/typed :type \"vec3\")", Eval.Vec3_array [| 1.; 2.; 3. |], Ty.Array Ty.Vec3);
    ];
  let workspace, diagnostics =
    Workspace.check ~ops:[ operator ] catalog
      (forms "(let* [kind \"vec3\"] (test/typed :type kind))")
  in
  assert (
    workspace = None
    && List.exists
         (fun (d : Diagnostic.t) -> d.code = "E_TEST_TYPE" && Option.is_some d.span)
         diagnostics);
  let source =
    {
      base with
      name = "test/present";
      signature = { pos = []; opt = []; rest = None; kw = [] };
      choices = [];
      out_args = None;
      out = (fun _ -> Ty.Bool);
      body = (fun ~live:_ ~node:_ _ -> Source ("test/present", Ty.Bool, []));
    }
  in
  let static body =
    List.assoc "g" (Result.get_ok (Eval.static (checked ~ops:[ source ] body))).results
  in
  let resolve = function
    | Eval.Source ("test/present", Ty.Bool, []) -> Ok (Eval.Bool false)
    | _ -> assert false
  in
  let marker = static "(test/present)" in
  assert (Eval.is_live marker && not (Eval.frame_dependent marker));
  assert (Eval.Host.force_reference ~resolve marker ~live:(Input.at_time 0.) = Ok (Eval.Bool false));
  assert (
    Eval.Host.force_reference ~resolve
      (static "(if (test/present) (sqrt -1) 7)")
      ~live:(Input.at_time 0.)
    = Ok (Eval.Int 7));
  (match Eval.Host.force_reference marker ~live:(Input.at_time 0.) with
  | Error d -> assert (d.code = "E_DATA_SOURCE")
  | Ok _ -> assert false);
  List.iter
    (fun resolved ->
      match
        Eval.Host.force_reference ~resolve:(fun _ -> Ok resolved) marker ~live:(Input.at_time 0.)
      with
      | Error d -> assert (d.code = "E_DATA_SOURCE")
      | Ok _ -> assert false)
    [ marker; Eval.Float 1. ]

let () =
  let live = Input.at_time 2. in
  let state = Eval.create_state () in
  let marker = Eval.Source ("test/array", Ty.Array Ty.Float, [ ("count", Eval.Int 2) ]) in
  let legacy = Eval.Struct ("test/array", Ty.Array Ty.Float, [ ("count", Eval.Int 2) ]) in
  let data = Eval.Float_array [| 0.; -0. |] in
  List.iter
    (fun marker ->
      List.iter
        (fun supplied ->
          let before = Eval.state_stamp state and calls = ref 0 in
          let resolve _ =
            incr calls;
            Ok data
          in
          let actual =
            Result.get_ok (Eval.Host.force_reference ?state:supplied ~resolve marker ~live)
          in
          assert (actual == data && !calls = 1 && Eval.state_stamp state = before);
          List.iter
            (fun (resolved, code) ->
              match
                Eval.Host.force_reference ?state:supplied
                  ~resolve:(fun _ -> Ok resolved)
                  marker ~live
              with
              | Error d -> assert (d.code = code)
              | Ok _ -> assert false)
            [
              (marker, "E_DATA_SOURCE");
              (Eval.Int_array [| 0; 1 |], "E_DATA_SOURCE");
              (Eval.Float_array [| 0.; nan |], "E_NONFINITE");
            ];
          let reason = Diagnostic.error ~cause:"host cause" ~code:"E_HOST" "host failure" in
          assert (
            Eval.Host.force_reference ?state:supplied ~resolve:(fun _ -> Error reason) marker ~live
            = Error reason);
          calls := 0;
          match
            Eval.Host.force_reference ?state:supplied ~resolve marker ~live:{ live with dt = nan }
          with
          | Error d -> assert (d.code = "E_FRAME" && !calls = 0)
          | Ok _ -> assert false)
        [ None; Some state ])
    [ marker; legacy ];
  let forms =
    Result.get_ok (Syntax.parse "(workspace nested_source (graph g :context value (+ t 1.)))")
  in
  let workspace =
    match Workspace.check { Catalog.version = 1; kinds = [] } forms with
    | Some workspace, [] -> workspace
    | _ -> assert false
  in
  let dynamic = List.assoc "g" (Result.get_ok (Eval.static workspace)).results in
  let fields = [ ("value", dynamic) ] in
  let marker = Eval.Source ("test/nested", Ty.Any, []) in
  let calls = ref 0 in
  let resolve _ =
    incr calls;
    Ok (Eval.Record fields)
  in
  assert (
    Eval.Host.force_reference ~resolve marker ~live = Ok (Eval.Record [ ("value", Eval.Float 3.) ]));
  assert (!calls = 1);
  let marker = Eval.Source ("test/live_argument", Ty.Float, fields) in
  let resolve actual =
    assert (actual == marker);
    Ok (Eval.Float 7.)
  in
  assert (Eval.Host.force_reference ~resolve marker ~live = Ok (Eval.Float 7.));
  let descriptor =
    Eval.Source
      ( "test/borrowed",
        Ty.Array Ty.Float,
        [ ("geometry", Eval.Deferred (Ty.geometry, 17)); ("owner", Eval.Text "point") ] )
  and legacy = Eval.Struct ("test/borrowed", Ty.Array Ty.Int, [ ("count", Eval.Int 2) ]) in
  let nested =
    Eval.Source
      ( "test/write",
        Ty.Array Ty.Float,
        [ ("values", Eval.Record [ ("offsets", legacy); ("values", descriptor) ]) ] )
  in
  List.iter
    (fun supplied ->
      let calls = ref 0 in
      let resolve actual =
        assert (actual == nested);
        incr calls;
        Ok data
      in
      assert (
        Result.get_ok (Eval.Host.force_reference ?state:supplied ~resolve nested ~live) == data);
      assert (!calls = 1);
      (match
         Eval.Host.force_reference ?state:supplied
           ~resolve:(fun actual ->
             assert (actual == nested);
             Ok (Eval.Int 0))
           nested ~live
       with
      | Error d -> assert (d.code = "E_DATA_SOURCE")
      | Ok _ -> assert false);
      let reason =
        Diagnostic.error ~cause:"nested host cause" ~code:"E_HOST" "nested host failure"
      in
      assert (
        Eval.Host.force_reference ?state:supplied
          ~resolve:(fun actual ->
            assert (actual == nested);
            Error reason)
          nested ~live
        = Error reason);
      calls := 0;
      match
        Eval.Host.force_reference ?state:supplied ~resolve nested ~live:{ live with dt = nan }
      with
      | Error d -> assert (d.code = "E_FRAME" && !calls = 0)
      | Ok _ -> assert false)
    [ None; Some state ];
  let forms =
    Result.get_ok
      (Syntax.parse
         "(workspace source_state (graph g :context value (state [previous 0.0] (+ previous 1.0))))")
  in
  let workspace =
    match Workspace.check { Catalog.version = 1; kinds = [] } forms with
    | Some workspace, [] -> workspace
    | _ -> assert false
  in
  let fold = List.assoc "g" (Result.get_ok (Eval.static workspace)).results in
  let before = Eval.state_stamp state in
  let reason = Diagnostic.error ~cause:"rollback cause" ~code:"E_HOST" "stateful host failure" in
  let resolve _ =
    ignore (Result.get_ok (Eval.force ~state fold ~live));
    assert (Eval.state_stamp state <> before);
    Error reason
  in
  assert (Eval.Host.force_reference ~state ~resolve marker ~live = Error reason);
  assert (Eval.state_stamp state = before);
  print_endline
    "Host forcing: concrete/legacy sources, validation/cause/frame failures and live nested \
     fallback agree with supplied state"

let () =
  List.iter
    (fun (name, ty, fill) ->
      let op = Option.get (Op.find name Context.value) in
      List.iter
        (fun count ->
          let actual =
            op.body ~live:(Input.at_time 0.)
              ~node:(fun _ _ -> assert false)
              [ ("count", Int count); ("value", fill) ]
          and expected = Value.array_init ty count (fun _ -> fill) in
          let data = function Vec3_array data | Vec4_array data -> data | _ -> assert false in
          assert (
            Array.for_all2
              (fun a b -> Int64.bits_of_float a = Int64.bits_of_float b)
              (data actual) (data expected)))
        [ 0; 1; 2; 3; 5; 17; 4097; 32769 ])
    [
      ("array/vec3", Ty.Vec3, Vec3 (-0., 1., -2.));
      ("array/vec4", Ty.Vec4, Vec4 (-0., 1., -2., Float.next_after 0. 1.));
    ];
  print_endline
    "Constant vector constructor: empty/single/power tails/multi-block signed-zero bits match \
     element builder"

let () =
  let capture f = try Ok (f ()) with Diagnostic.Error d -> Error (d.code, d.message) in
  List.iter
    (fun (name, fill) ->
      let op = Option.get (Op.find name Context.value) in
      List.iter
        (fun count ->
          let args = [ ("count", Int count); ("value", fill) ] in
          let actual = capture (fun () -> Op.constant_array_descriptor name args) in
          let expected =
            capture (fun () ->
                op.body ~live:(Input.at_time 0.) ~node:(fun _ _ -> assert false) args)
          in
          match (actual, expected) with
          | Error a, Error b -> assert (a = b)
          | Ok (Source (_, ty, [ ("count", Int actual); ("value", row) ])), Ok data ->
              assert (actual = count && ty = Value.ty_of data);
              if count > 0 then assert (row = Value.array_get data 0)
          | _ -> assert false)
        [ 0; 1; 7; -1 ];
      List.iter
        (fun fill ->
          let args = [ ("count", Int 1); ("value", fill) ] in
          assert (
            capture (fun () -> Op.constant_array_descriptor name args)
            =
            match
              capture (fun () ->
                  op.body ~live:(Input.at_time 0.) ~node:(fun _ _ -> assert false) args)
            with
            | Error e -> Error e
            | Ok _ -> assert false))
        [ Text "wrong"; Float nan ])
    [
      ("array/float", Float (-0.));
      ("array/vec3", Vec3 (-0., 1., 2.));
      ("array/vec4", Vec4 (-0., 1., 2., 3.));
    ];
  List.iter
    (fun count ->
      let args = [ ("count", Int count); ("value", Text "unused") ] in
      let op = Option.get (Op.find "array/vec4" Context.value) in
      assert (
        capture (fun () -> Op.constant_array_descriptor "array/vec4" args)
        =
        match
          capture (fun () -> op.body ~live:(Input.at_time 0.) ~node:(fun _ _ -> assert false) args)
        with
        | Error e -> Error e
        | Ok _ -> assert false))
    [ (Sys.max_floatarray_length / 4) + 1 ];
  (match
     Op.constant_array_descriptor "array/vec4" [ ("count", Int 0); ("value", Text "unused") ]
   with
  | Source (_, _, [ ("count", Int 0); ("value", Vec4 (0., 0., 0., 0.)) ]) -> ()
  | _ -> assert false);
  List.iter
    (fun args ->
      match capture (fun () -> Op.constant_array_descriptor "array/vec4" args) with
      | Error ("E_ARRAY_TYPE", _) -> ()
      | _ -> assert false)
    [ []; [ ("count", Int 1); ("count", Int 1) ]; [ ("count", Int 1); ("extra", Int 0) ] ];
  print_endline
    "Constant array descriptors: canonical count/bounds/empty/fill checks match dense constructor \
     errors"

let () =
  let apply name args = check (Option.get (Op.find name Context.value)) args in
  List.iter
    (fun name ->
      let op = Option.get (Op.find name Context.value) in
      assert (op.shape = Op.Scalar && op.ctx = Context.value))
    [ "array/scatter_add"; "array/compact"; "array/scan_sum"; "array/grade"; "array/index_of" ];
  let grade = Option.get (Op.find "array/grade" Context.value) in
  assert (
    grade.signature.pos = [ ("keys", Ty.Array Ty.Any) ]
    && grade.signature.kw = [ ("descending", Ty.Bool) ]);
  assert (apply "array/grade" [ ("keys", Int_array [| 2; 1; 2 |]) ] = Int_array [| 1; 0; 2 |]);
  assert (
    apply "array/grade" [ ("keys", Int_array [| 2; 1; 2 |]); ("descending", Bool false) ]
    = Int_array [| 1; 0; 2 |]);
  assert (
    apply "array/grade" [ ("keys", Int_array [| 2; 1; 2 |]); ("descending", Bool true) ]
    = Int_array [| 0; 2; 1 |]);
  assert (
    apply "array/index_of"
      [
        ("keys", Int_array [| 9007199254740993; 2; 2 |]);
        ("queries", Int_array [| 2; 7; 9007199254740993; 2; 7 |]);
      ]
    = Int_array [| 1; -1; 0; 1; -1 |]);
  assert (
    apply "array/index_of"
      [
        ("keys", Text_array [| "\255\000"; "é"; "é" |]);
        ("queries", Text_array [| "é"; "missing"; "\255\000" |]);
      ]
    = Int_array [| 1; -1; 0 |]);
  assert (apply "array/compact" [ ("mask", Int_array [| 1; 0; 1 |]) ] = Int_array [| 0; 2 |]);
  assert (apply "float/next_after" [ ("x", Float 1.); ("y", Float 2.) ] = Float (Float.succ 1.));
  assert (apply "float/next_after" [ ("x", Float 1.); ("y", Float 0.) ] = Float (Float.pred 1.));
  assert (apply "float/next_after" [ ("x", Float 3.); ("y", Float 3.) ] = Float 3.);
  assert (
    apply "float/next_after" [ ("x", Float 0.); ("y", Float 1.) ] = Float (Int64.float_of_bits 1L));
  assert (
    apply "array/segment_argmax"
      [ ("values", Int_array [| 2; 5; 5; 1; 5 |]); ("offsets", Int_array [| 0; 3; 3; 5 |]) ]
    = Int_array [| 1; -1; 4 |]);
  assert (
    apply "array/segment_argmax"
      [ ("values", Float_array [| 0.; -0.; 0. |]); ("offsets", Int_array [| 0; 3 |]) ]
    = Int_array [| 0 |]);
  assert (
    apply "array/segment_argmax" [ ("values", Float_array [||]); ("offsets", Int_array [| 0 |]) ]
    = Int_array [||]);
  assert (
    apply "array/scan_sum" [ ("array", Int_array [| max_int; 1; -1 |]) ]
    = Int_array [| 0; max_int; min_int; max_int |]);
  assert (
    apply "array/scatter_add"
      [
        ("indices", Int_array [| 0; 0; 1 |]);
        ("values", Float_array [| 1.; 2.; 3. |]);
        ("size", Int 3);
      ]
    = Float_array [| 3.; 3.; 0. |]);
  assert (
    apply "array/scatter_add"
      [
        ("indices", Int_array [| 0; 0; 1 |]);
        ("values", Int_array [| max_int; 1; 2 |]);
        ("size", Int 3);
      ]
    = Int_array [| min_int; 2; 0 |]);
  assert (
    apply "array/scatter_add"
      [
        ("indices", Int_array [| 0; 0; 1 |]);
        ("values", Vec3_array [| 1.; 2.; 3.; 4.; 5.; 6.; 1.; 1.; 1. |]);
        ("size", Int 2);
      ]
    = Vec3_array [| 5.; 7.; 9.; 1.; 1.; 1. |]);
  List.iter
    (fun (name, args, expected) -> assert (apply name args = expected))
    [
      ("array/grade", [ ("keys", Float_array [||]) ], Int_array [||]);
      ( "array/index_of",
        [ ("keys", Int_array [||]); ("queries", Int_array [| 1; 2 |]) ],
        Int_array [| -1; -1 |] );
      ( "array/index_of",
        [ ("keys", Text_array [| "a" |]); ("queries", Text_array [||]) ],
        Int_array [||] );
      ("array/compact", [ ("mask", Int_array [||]) ], Int_array [||]);
      ("array/scan_sum", [ ("array", Int_array [||]) ], Int_array [| 0 |]);
      ( "array/scatter_add",
        [ ("indices", Int_array [||]); ("values", Float_array [||]); ("size", Int 2) ],
        Float_array [| 0.; 0. |] );
    ];
  List.iter
    (fun (name, args) ->
      let before = Marshal.to_string args [ Marshal.No_sharing ] in
      let result = apply name args in
      (match result with
      | Int_array a -> if Array.length a > 0 then a.(0) <- 999
      | Float_array a -> if Array.length a > 0 then a.(0) <- 999.
      | _ -> assert false);
      assert (Marshal.to_string args [ Marshal.No_sharing ] = before))
    [
      ("array/grade", [ ("keys", Int_array [| 1; 0; 1 |]) ]);
      ("array/index_of", [ ("keys", Int_array [| 1; 2 |]); ("queries", Int_array [| 2; 1 |]) ]);
      ("array/compact", [ ("mask", Int_array [| 1; 0; 1 |]) ]);
      ("array/scan_sum", [ ("array", Int_array [| 1; 2 |]) ]);
      ( "array/scatter_add",
        [ ("indices", Int_array [| 0; 0 |]); ("values", Float_array [| 1.; 2. |]); ("size", Int 1) ]
      );
    ];
  let reject code name args =
    let op = Option.get (Op.find name Context.value) in
    try
      ignore (op.body ~live:(Input.at_time 0.) ~node:(fun _ _ -> assert false) args);
      failwith ("accepted " ^ name)
    with Diagnostic.Error d -> assert (d.code = code)
  in
  List.iter
    (fun (code, name, args) -> reject code name args)
    [
      ("E_ARRAY_MASK", "array/compact", [ ("mask", Int_array [| 2 |]) ]);
      ( "E_ARRAY_OFFSETS",
        "array/segment_argmax",
        [ ("values", Int_array [| 1 |]); ("offsets", Int_array [| 0; 2 |]) ] );
      ( "E_ARRAY_TYPE",
        "array/segment_argmax",
        [ ("values", Vec3_array [| 0.; 0.; 0. |]); ("offsets", Int_array [| 0; 1 |]) ] );
      ( "E_NONFINITE",
        "array/segment_argmax",
        [ ("values", Float_array [| nan |]); ("offsets", Int_array [| 0; 1 |]) ] );
      ("E_NONFINITE", "float/next_after", [ ("x", Float nan); ("y", Float 1.) ]);
      ("E_ARRAY_TYPE", "array/scan_sum", [ ("array", Float_array [| 1. |]) ]);
      ("E_NONFINITE", "array/grade", [ ("keys", Float_array [| nan |]) ]);
      ("E_ARRAY_TYPE", "array/index_of", [ ("keys", Int_array [||]); ("queries", Text_array [||]) ]);
      ( "E_ARRAY_RANGE",
        "array/scatter_add",
        [ ("indices", Int_array [| 1 |]); ("values", Int_array [| 2 |]); ("size", Int 1) ] );
      ( "E_ARRAY_COUNT",
        "array/scatter_add",
        [ ("indices", Int_array [||]); ("values", Float_array [| 1. |]); ("size", Int 1) ] );
    ];
  print_endline
    "Whole-array primitive census: canonical shapes, grade default, counts, exact outputs, \
     ownership and refusals"
