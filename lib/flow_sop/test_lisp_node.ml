(* These checks are the Lisp node's own protocol (its factory node, as the inspector and node menu
   build it: parameters, rebuilds, cook identity), so this module lowers calls as that node. *)
let ok = function Ok value -> value | Error message -> failwith message
let check = Test_support.check

let parse text =
  match Flow.Syntax.parse text with
  | Ok forms -> forms
  | Error d -> failwith (Diagnostic.to_string d)

let lower ?(reference = false) text =
  match Flow_sop.Lower.workspace ~reference (parse text) with
  | Error d -> failwith (Diagnostic.to_string d)
  | Ok lowered ->
      let graph = List.find (fun (graph : Flow_sop.Lower.graph) -> graph.default) lowered.graphs in
      ok (Lisp_sop.root lowered graph)

(* 128 entries: a test cooks many graphs in one session, and their working set must fit the cache
   (a grid is three cached nodes) for a recook to hit *)
let with_session fn =
  let session = ok (Sop.Session.create ~max_entries:128 ~max_payload_bytes:10_000_000) in
  Fun.protect ~finally:(fun () -> Sop.Session.close session) (fun () -> fn session)

let cook session ?(domains = 1) ?(grain = 97) ?(seed = 0L) node =
  match
    Sop.Session.cook session ~context:(ok (Sop.Context.create ~domains ~grain ~seed ())) node
  with
  | Ok output -> (
      match Sop.Payload.geometry output.payload with
      | Ok geometry -> geometry
      | Error d -> failwith (Diagnostic.error_to_string d))
  | Error d -> failwith (Diagnostic.error_to_string d)

let source =
  {|(workspace test
  (defnode sop/lisp_shift :label "Lisp shift" :category ["Deform"]
    :facts {:elementwise points :reads ["P"] :writes ["P"] :topology preserved :exact true}
    [(geo : geometry) (distance : float 0.5 :min -10 :max 10 :label "Distance")]
    (sop/with_attr geo :P (exact (map (fn [p] (+ p [distance 0.0 0.0]))
      (sop/attr geo :P)))))
  (graph g :context sop (sop/lisp_shift (sop/ext_src) :distance 2.0)))|}

let () =
  let geometry = Rdk.Line_geometry.points [| (0., 0., 0.) |] in
  let source =
    {|(workspace select
    (defnode sop/select_write [(geo : geometry)]
      (if (= (sop/count geo :points) 1)
        (let* [first (sop/with_attr geo "stage" (array/int 1 7) :type int)]
          (sop/with_attr first "seen" (array/int 1 (sop/count first :points)) :owner detail :type int))
        (error "E_UNTAKEN" "Untaken cooked branch")))
    (graph g :context sop (sop/select_write (sop/ext_src))))|}
  in
  let factory =
    Sop.Edit_graph.factory ~key:"ext_src" ~label:"Source" ~category:[ "Test" ] ~arity:0 (fun _ ->
        Ok (Lisp_sop.snapshot geometry))
  in
  let compile reference =
    match
      Flow_sop.Lower.workspace ~reference ~factories:(factory :: Sop.Nodes.factories) (parse source)
    with
    | Error d -> failwith (Diagnostic.to_string d)
    | Ok lowered ->
        let graph = List.hd lowered.graphs in
        ok (Lisp_sop.root lowered graph)
  in
  let expected = with_session (fun session -> cook session (compile false)) in
  List.iter
    (fun reference ->
      List.iter
        (fun domains ->
          with_session (fun session ->
              let actual = cook session ~domains (compile reference) in
              check
                (Rdk_test_support.geometry_bytes actual = Rdk_test_support.geometry_bytes expected)
                "lazy cooked selection disconnected a chained write or changed domain bytes";
              List.iter
                (fun (owner, name, value) ->
                  match Rdk.Geometry.find_attribute ~owner name actual with
                  | Some attribute -> (
                      match Rdk.Attribute.storage attribute with
                      | Rdk.Attribute.Int values ->
                          check (values = [| value |]) "chained cooked write has wrong values"
                      | _ -> failwith "chained cooked write has wrong type")
                  | None -> failwith "chained cooked write lost an attribute")
                [ (Rdk.Attribute.Point, "stage", 7); (Rdk.Attribute.Detail, "seen", 1) ]))
        [ 1; 8 ])
    [ false; true ]

let () =
  with_session (fun session ->
      let empty = Rdk.Line_geometry.points [||] in
      let node = Lisp_sop.workspace_node ~with_:[ ("src", Lisp_sop.snapshot empty) ] source in
      check
        (Rdk_test_support.geometry_bytes (cook session node) = Rdk_test_support.geometry_bytes empty)
        "empty geometry changed through a Lisp map")

let () =
  let source body =
    Printf.sprintf
      {|(workspace body_error
    (defnode sop/body_error [(geo : geometry) (bad : bool true)] %s)
    (graph g :context sop (sop/body_error (sop/line :points 2))))|}
      body
  in
  let expect_error session domains node =
    match Sop.Session.cook session ~context:(ok (Sop.Context.create ~domains ())) node with
    | Error d ->
        check
          (d.code = "E_BODY" && d.message = "body failure" && d.cause = Some "authored cause")
          ("authored body error changed during cooking: " ^ Diagnostic.error_to_string d)
    | Ok _ -> failwith "authored body error succeeded"
  in
  List.iter
    (fun reference ->
      List.iter
        (fun body ->
          let node = lower ~reference (source body) in
          with_session (fun session ->
              List.iter (fun domains -> expect_error session domains node) [ 1; 8 ]);
          let healthy, _ =
            ok (Sop.Node.apply_parameters node [ ("bad", Param.Bool_value false) ])
          in
          if String.starts_with ~prefix:"(if" body then begin
            with_session (fun session -> ignore (cook session healthy));
            let broken, _ =
              ok (Sop.Node.apply_parameters healthy [ ("bad", Param.Bool_value true) ])
            in
            with_session (fun session -> expect_error session 1 broken)
          end)
        [
          "(if bad (error \"E_BODY\" \"body failure\" :cause \"authored cause\") geo)";
          "(let* [ignored (error \"E_BODY\" \"body failure\" :cause \"authored cause\")] geo)";
        ])
    [ false; true ];
  let healthy = lower (source "(if false (error \"E_BODY\" \"body failure\") geo)") in
  with_session (fun session -> ignore (cook session healthy));
  let seed = Flow_sop.Operators.seed_for ~seed:0L ~namespace:"body_error" ~label:"body_error" () in
  let seeded =
    lower
      (source
         (Printf.sprintf
            "(if (int/eq (sop/context_seed \"body_error\") %d) (error \"E_BODY\" \"body failure\" \
             :cause \"authored cause\") geo)"
            seed))
  in
  check
    (Sop.Context.Dependencies.mem Seed (Sop.Node.dependencies seeded))
    "deferred body error lost its context seed dependency";
  with_session (fun session ->
      expect_error session 1 seeded;
      ignore (cook session ~seed:123456789L seeded);
      expect_error session 1 seeded);
  let salt = -1582480459898762999 in
  let salted_seed =
    Flow_sop.Operators.seed_for ~salt ~seed:0L ~namespace:"body_error" ~label:"body_error" ()
  in
  check
    (salted_seed <> seed
    && Flow_sop.Operators.seed_for ~salt:0 ~seed:0L ~namespace:"body_error" ~label:"body_error" ()
       = seed)
    "context seed salt did not change the mix or changed the unsalted one";
  (* the same salt as native's 0x6a09e667f3bcc909 xor: its low 63 bits *)
  check
    (Int64.logand (Int64.of_int salt) Int64.max_int = 0x6a09e667f3bcc909L)
    "salt literal is not native's constant";
  let saltedcall =
    lower
      (source
         (Printf.sprintf
            "(if (int/eq (sop/context_seed \"body_error\" :salt %d) %d) (error \"E_BODY\" \"body \
             failure\" :cause \"authored cause\") geo)"
            salt salted_seed))
  in
  with_session (fun session -> expect_error session 1 saltedcall);
  print_endline
    "Authored body errors cook lazily; parameter rebuilds, causes and context seed changes \
     preserve timing"

let () =
  List.iter
    (fun expression ->
      List.iter
        (fun reference ->
          let node =
            lower ~reference
              (Printf.sprintf
                 {|(workspace live_body
      (defnode sop/live_body [(geo : geometry)]
        (sop/transform geo :translate [%s 0.0 0.0]))
      (graph g :context sop (sop/live_body (sop/line :points 2))))|}
                 expression)
          in
          with_session (fun session ->
              List.iter
                (fun (time, dt) ->
                  let live =
                    {
                      (Input.at_time time) with
                      dt;
                      frame = int_of_float time;
                      tick = int_of_float time;
                    }
                  in
                  let context =
                    ok (Sop.Context.create ~time ~frame:(Int64.of_int live.frame) ~input:live ())
                  in
                  let actual =
                    Sop.Session.cook session ~context node
                    |> Result.fold ~ok:Fun.id ~error:(fun d ->
                        failwith (Diagnostic.error_to_string d))
                    |> fun (output : Sop.Session.output) ->
                    Result.get_ok (Sop.Payload.geometry output.payload)
                  in
                  let offset = if expression = "t" then time else dt in
                  let expected =
                    Lisp_sop.node
                      (Printf.sprintf
                         "(sop/transform (sop/line :points 2) :translate [%.17g 0.0 0.0])" offset)
                    |> cook session
                  in
                  check
                    (Rdk_test_support.geometry_bytes actual
                    = Rdk_test_support.geometry_bytes expected)
                    (Printf.sprintf "Prepared body reused stale geometry for %s at t=%g dt=%g"
                       expression time dt);
                  let before = Sop.Session.stats session in
                  ignore (Sop.Session.cook session ~context node |> Result.get_ok);
                  check
                    ((Sop.Session.stats session).misses = before.misses)
                    "An unchanged prepared body missed its warm cache")
                [ (0., 0.25); (1., 0.5); (2., 0.125) ]))
        [ false; true ])
    [ "t"; "(state [previous 0.0] (+ previous (frame/dt)))" ];
  print_endline
    "Prepared bodies reuse unchanged graphs; live drives/state changes recompile and warm contexts \
     retain cache hits"

let () =
  let original = Rdk.Line_geometry.points [| (0., -0., 2.); (1., 3., 4.) |] in
  let before = Rdk_test_support.geometry_bytes original in
  let transforms =
    [|
      Rays_math.Mat4.translation (Rays_math.Vec3.create 1. 0. 0.);
      Rays_math.Mat4.translation (Rays_math.Vec3.create 3. 0. 0.);
    |]
  in
  let input =
    Sop.Node.Build.make_geometry ~operation:"test.materialized_input" ~version:1 ~parameters:""
      ~cook_mode:Sop.Node.Generator ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
      (fun ~node_id:_ _ _ -> Ok Sop.Node.Build.{ geometry = original; instances = Some transforms })
  in
  let body =
    {|(let* [data (exact (map (fn [p] (+ p [0.5 -0.0 0.0])) (sop/attr geo :P)))
                      copied (sop/with_attr geo :copied data)
                      rows {:offsets (array/range (int/add (sop/count copied :points) 1) :type int)
                            :values (array/float (sop/count copied :points) 0.25)}]
                 (sop/with_attr copied :rows rows :type float_array))|}
  in
  let node body =
    Lisp_sop.workspace_node
      ~with_:[ ("src", input) ]
      (Printf.sprintf
         "(workspace direct_write (defnode sop/direct_write [(geo : geometry)] (if (int/gt \
          (sop/count geo :points) 0) %s geo)) (graph g :context sop (sop/direct_write \
          (sop/ext_src))))"
         body)
  in
  let actual = node body in
  let expected = Rdk.Instance_copy.materialize_instances ~transforms original |> Result.get_ok in
  with_session (fun session ->
      List.iter
        (fun domains ->
          Sop.Session.clear session;
          let context = ok (Sop.Context.create ~domains ~grain:97 ()) in
          let output = Sop.Session.cook session ~context actual |> Result.get_ok in
          let geometry = Sop.Payload.geometry output.payload |> Result.get_ok in
          check (output.instances = None) "Materialized write retained already-expanded instances";
          check
            (Rdk.Geometry.point_count geometry = Rdk.Geometry.point_count expected)
            "Materialized write lost source instance cardinality";
          let storage name =
            Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point name geometry
            |> Option.get |> Rdk.Attribute.View.storage
          in
          (match storage "copied" with
          | Rdk.Attribute.Float3 data ->
              check
                (data != Rdk.Geometry.positions geometry)
                "Computed write borrowed its input storage";
              for i = 0 to Rdk.Geometry.point_count geometry - 1 do
                let x, y, z = Rdk.Packed.Float3.get (Rdk.Geometry.positions expected) i in
                check
                  (Rdk.Packed.Float3.get data i = (x +. 0.5, y +. -0., z +. 0.))
                  "Computed write changed numeric values"
              done
          | _ -> failwith "Materialized write lost tuple storage");
          (match storage "rows" with
          | Rdk.Attribute.Float_array data ->
              let data = Rdk.Packed.Float_array.View.view data in
              check
                (data.offsets = Array.init 5 Fun.id && data.values = Array.make 4 0.25)
                "Materialized CSR write changed row layout"
          | _ -> failwith "Materialized write lost CSR storage");
          List.iter
            (fun (body, code) ->
              match Sop.Session.cook session ~context (node body) with
              | Error error ->
                  check (error.code = code)
                    "Materialized write changed producer/count error precedence"
              | Ok _ -> failwith "Malformed materialized write succeeded")
            [
              ( "(sop/with_attr geo :bad (array/float (int/add (sop/count geo :points) 1) 0.5) \
                 :type float)",
                "E_ATTR_COUNT" );
              ( "(sop/with_attr geo :bad (array/float (int/add (sop/count geo :points) 1) (exp \
                 1000.0)) :type float)",
                "E_NONFINITE" );
            ];
          let token = Sop.Context.Cancel.create () in
          Sop.Context.Cancel.cancel token;
          let cancelled = ok (Sop.Context.create ~domains ~cancel:token ()) in
          match Sop.Session.cook session ~context:cancelled actual with
          | Error error ->
              check
                (error.code = "cancelled" || error.code = "E_CANCELLED")
                "Materialized write lost cancellation"
          | Ok _ -> failwith "Cancelled materialized write succeeded")
        [ 1; 8 ]);
  check
    (before = Rdk_test_support.geometry_bytes original)
    "Materialized writes mutated their input";
  print_endline
    "Cooked materialized writes: owned tuple/CSR storage, expanded instances, producer/count \
     errors and cancellation"

let () =
  with_session (fun session ->
      let node =
        lower
          {|(workspace selected_error
    (defnode sop/selected_error [(geo : geometry)]
      (if (= (sop/count geo :points) 1)
        (error "E_SELECTED" "selected failure" :cause (str "point count " (sop/count geo :points)))
        geo))
    (graph g :context sop (sop/selected_error (sop/line :kind "Points" :points 1))))|}
      in
      List.iter
        (fun domains ->
          match Sop.Session.cook session ~context:(ok (Sop.Context.create ~domains ())) node with
          | Error d ->
              check
                (d.code = "E_SELECTED" && d.message = "selected failure"
               && d.cause = Some "point count 1")
                ("compiled cooked selection diagnostic differs: " ^ Diagnostic.error_to_string d)
          | Ok _ -> failwith "selected cooked error succeeded")
        [ 1; 8 ])

let () =
  let original = Option.get (Flow.Op.find "frame/dt" Flow.Context.value) in
  List.iter
    (fun (id, message) ->
      let probe =
        Flow.Op.
          {
            original with
            name = "bad_handle";
            signature = { pos = []; opt = []; rest = None; kw = [] };
            out = (fun _ -> Flow.Ty.Bool);
            out_args = None;
            live = false;
            arithmetic = None;
            packed_extension = None;
            body =
              (fun ~live:_ ~node:_ _ ->
                Flow.Value.Source ("$core/cooked", Flow.Ty.Bool, [ ("id", Flow.Value.Int id) ]));
          }
      in
      List.iter
        (fun count ->
          let geometry = Rdk.Line_geometry.points (Array.make count (0., 0., 0.)) in
          let factory =
            Sop.Edit_graph.factory ~key:"handle_source" ~label:"Source" ~category:[ "Test" ]
              ~arity:0 (fun _ -> Ok (Lisp_sop.snapshot geometry))
          in
          let factories = factory :: Sop.Nodes.factories in
          let catalog =
            match Flow_sop.Catalog.of_factories ~version:1 factories with
            | Ok catalog -> catalog
            | Error d -> failwith (Diagnostic.to_string d)
          in
          let forms =
            parse
              {|(workspace handles
        (defnode sop/handle_probe [(geo : geometry)]
          (if (int/gt (sop/count geo :points) 0) (if (bad_handle) geo geo) geo))
        (graph g :context sop (sop/handle_probe (sop/handle_source))))|}
          in
          let checked =
            match
              Flow_sop.Catalog.check_workspace ~ops:(probe :: Flow_sop.Operators.all) catalog forms
            with
            | Some checked, _ -> checked
            | _, diagnostics ->
                failwith (String.concat "; " (List.map Diagnostic.to_string diagnostics))
          in
          let lowered =
            match Flow_sop.Lower.of_checked ~factories checked with
            | Ok lowered -> lowered
            | Error d -> failwith (Diagnostic.to_string d)
          in
          let graph = List.hd lowered.graphs in
          let node = ok (Lisp_sop.root lowered graph) in
          List.iter
            (fun domains ->
              with_session (fun session ->
                  let result =
                    Sop.Session.cook session ~context:(ok (Sop.Context.create ~domains ())) node
                  in
                  if count = 0 then
                    match result with
                    | Ok output ->
                        check
                          (Rdk_test_support.geometry_bytes
                             (Result.get_ok (Sop.Payload.geometry output.payload))
                          = Rdk_test_support.geometry_bytes geometry)
                          "untaken malformed handle changed geometry"
                    | Error d -> failwith (Diagnostic.error_to_string d)
                  else
                    match result with
                    | Error d ->
                        check
                          (d.code = "E_DATA_SOURCE" && d.message = message)
                          "malformed cooked handle bypassed its typed guard"
                    | Ok _ -> failwith "malformed cooked handle succeeded"))
            [ 1; 8 ])
        [ 0; 1 ])
    [
      (999, "A cooked handle is missing from this execution.");
      (0, "A cooked handle has the wrong declared type.");
    ]

let () =
  List.iter
    (fun predicate ->
      let node =
        lower
          (Printf.sprintf
             {|(workspace parameter_error
      (defnode sop/parameter_error :validate %s :code "E_PARAM" :message "count must be positive"
        [(geo : geometry) (count : int 1)] geo)
      (graph g :context sop (sop/parameter_error (sop/line))))|}
             predicate)
      in
      let previous = Sop.Node.parameter_key node in
      (match Sop.Node.apply_parameters node [ ("count", Param.Int_value 0) ] with
      | Error message ->
          check (message = "count must be positive") "parameter validation error formatting differs"
      | Ok _ -> failwith "invalid parameter edit succeeded");
      check (Sop.Node.parameter_key node = previous) "rejected validation changed the node")
    [
      "(> count 0)";
      "(if (> count 0) true (error \"E_PARAM\" \"count must be positive\" :cause \"validation \
       reason\"))";
    ]

let () =
  let open Rdk in
  let positions =
    ok (Packed.Float3.of_owned ~x:[| -1.; 0.; 1. |] ~y:[| -0.; 0.; 0. |] ~z:[| 2.; 3.; 4. |])
  in
  let topology =
    ok
      (Topology.polygons_owned ~point_count:3 ~vertex_points:[| 0; 1; 2 |]
         ~primitive_offsets:[| 0; 3 |])
  in
  let attributes =
    List.map
      (fun (owner, name, storage) -> ok (Attribute.create_owned ~owner ~name storage))
      [
        (Attribute.Point, "weight", Attribute.Float [| 0.25; 0.5; 0.75 |]);
        (Attribute.Point, "id", Attribute.Int [| 2; 1; 0 |]);
        (Attribute.Point, "tag", Attribute.Text [| "one"; "two"; "three" |]);
        ( Attribute.Point,
          "weights",
          Attribute.Float_array
            (ok
               (Packed.Float_array.create_owned ~offsets:[| 0; 0; 2; 3 |]
                  ~values:[| 0.25; 0.5; 0.75 |])) );
        (Attribute.Vertex, "uv", Attribute.Float3 positions);
        (Attribute.Primitive, "material", Attribute.Int [| 7 |]);
      ]
  in
  let groups = [ ok (Group.ordered ~owner:Group.Point ~name:"ordered" ~length:3 [| 2; 0 |]) ] in
  let edge_groups =
    [
      Edge_group.init ~topology ~index:(Topology_index.create topology) ~name:"selected_edges"
        (fun i -> i = 0);
    ]
  in
  let original = ok (Geometry.create ~positions ~topology ~attributes ~groups ~edge_groups ()) in
  let before = Rdk_test_support.geometry_bytes original in
  let expected =
    (let p = Packed.Float3.View.view (Geometry.positions original) in
     Geometry.with_positions
       (Packed.Float3.View.of_owned_exn
          ~x:(Array.map (fun x -> x +. 2.) p.x)
          ~y:(Array.map (fun y -> y +. 0.) p.y)
          ~z:(Array.map (fun z -> z +. 0.) p.z))
       original
     |> Result.get_ok)
    |> Rdk_test_support.geometry_bytes
  in
  let node = Lisp_sop.workspace_node ~with_:[ ("src", Lisp_sop.snapshot original) ] source in
  List.iter
    (fun domains ->
      List.iter
        (fun grain ->
          with_session (fun session ->
              check
                (Rdk_test_support.geometry_bytes (cook session ~domains ~grain node) = expected)
                "Lisp body changed topology, attributes or ordered groups";
              check
                (Rdk_test_support.geometry_bytes original = before)
                "Lisp body mutated its immutable input"))
        [ 1; 97 ])
    [ 1; 4 ]

let () =
  let original = Lisp_sop.node "(sop/line :points 3)" in
  let node = Lisp_sop.workspace_node ~with_:[ ("src", original) ] source in
  check (Sop.Node.operation node = "lisp_shift") "defnode must keep its operation";
  check (List.length (Sop.Node.inputs node) = 1) "body leaked into public inputs";
  check
    (List.map (fun (f : Param.field_view) -> f.name) (Sop.Node.parameter_fields node)
    = [ "distance" ])
    "parameter schema was lost";
  let session = ok (Sop.Session.create ~max_entries:32 ~max_payload_bytes:1_000_000) in
  Fun.protect
    ~finally:(fun () -> Sop.Session.close session)
    (fun () ->
      let context = ok (Sop.Context.create ()) in
      let cook node =
        match Sop.Session.cook session ~context node with
        | Ok output -> (
            match Sop.Payload.geometry output.payload with
            | Ok geometry -> geometry
            | Error error -> failwith (Diagnostic.error_to_string error))
        | Error error -> failwith (Diagnostic.error_to_string error)
      in
      let input = cook original and first = cook node in
      let ix, iy, iz = Rdk.Packed.Float3.get (Rdk.Geometry.positions input) 0 in
      let x, y, z = Rdk.Packed.Float3.get (Rdk.Geometry.positions first) 0 in
      check (x = ix +. 2. && y = iy && z = iz) "body did not use authored parameters";
      check (cook node == first) "unchanged Lisp node missed the cache";
      let changed, _ = ok (Sop.Node.apply_parameters node [ ("distance", Param.Float_value 3.) ]) in
      check (Sop.Node.id changed = Sop.Node.id node) "parameter edit changed logical identity";
      let result = cook changed in
      check (result != first) "changed parameter hit old cache";
      let x, _, _ = Rdk.Packed.Float3.get (Rdk.Geometry.positions result) 0 in
      check (x = ix +. 3.) "parameter edit did not rebuild the Lisp body")

let controls =
  {|(workspace controls
  (defnode sop/all_controls
    [(geo : geometry)
     (enabled : bool true)
     (count : int 2 :min 1 :max 10 :hard_min 1)
     (amount : float 0.5 :min -10 :max 10 :hard_min -20 :hard_max 20)
     (tag : text "original")
     (mode : text "A" :kind choice :choices ["A" "B"])
     (offset : vec3 [1 2 3] :min -10 :max 10)]
    (let* [distance (+ (+ (+ (+ amount count)
      (if enabled 1.0 0.0)) (if (equal? tag "original") 0.0 10.0))
      (if (equal? mode "A") 0.0 20.0))]
      (sop/with_attr geo :P (exact
        (map (fn [p] (+ (+ p offset) [distance 0.0 0.0])) (sop/attr geo :P))))))
  (graph g :context sop (sop/all_controls (sop/line :kind "Points" :points 1))))|}

let () =
  with_session (fun session ->
      let node = lower controls in
      let first = cook session node in
      let position geometry = Rdk.Packed.Float3.get (Rdk.Geometry.positions geometry) 0 in
      check (position first = (4.5, 2., 3.)) "scalar and vector defaults did not reach the body";
      let before = Sop.Session.stats session in
      check (cook session node == first) "all-controls unchanged node missed cache";
      check
        ((Sop.Session.stats session).misses = before.misses)
        "unchanged body caused hidden public misses";
      let fields = Sop.Node.parameter_fields node in
      check
        (List.map (fun (f : Param.field_view) -> f.name) fields
        = [ "enabled"; "count"; "amount"; "tag"; "mode"; "offset_x"; "offset_y"; "offset_z" ])
        "parameter kinds lost declaration ordering";
      List.iter
        (fun (name, value, expected) ->
          let changed, effects = ok (Sop.Node.apply_parameters node [ (name, value) ]) in
          check
            (effects.cook && Sop.Node.id changed = Sop.Node.id node)
            (name ^ ": parameter edit lost identity");
          let before = Sop.Session.stats session in
          let result = cook session changed in
          check (position result = expected) (name ^ ": body ignored updated parameter");
          check
            ((Sop.Session.stats session).misses = before.misses + 1)
            (name ^ ": changed Lisp parameter must cause exactly one public miss");
          let before = Sop.Session.stats session in
          check
            (cook session changed == result && (Sop.Session.stats session).misses = before.misses)
            (name ^ ": repeated changed value missed cache"))
        [
          ("enabled", Param.Bool_value false, (3.5, 2., 3.));
          ("count", Param.Int_value 3, (5.5, 2., 3.));
          ("amount", Param.Float_value 1.5, (5.5, 2., 3.));
          ("tag", Param.Text_value "changed", (14.5, 2., 3.));
          ("mode", Param.Choice_value "B", (24.5, 2., 3.));
          ("offset_x", Param.Float_value 4., (7.5, 2., 3.));
          ("offset_y", Param.Float_value 4., (4.5, 4., 3.));
          ("offset_z", Param.Float_value 4., (4.5, 2., 4.));
        ];
      List.iter
        (fun ((name, _) as change) ->
          check
            (Result.is_error (Sop.Node.apply_parameters node [ change ]))
            (name ^ ": parameter normalization accepted an invalid value"))
        [
          ("amount", Param.Float_value nan);
          ("amount", Param.Float_value infinity);
          ("mode", Param.Choice_value "missing");
        ];
      (* Inspector writes use the native Param clamp; authored Lisp is checked. *)
      let clamped, _ = ok (Sop.Node.apply_parameters node [ ("count", Param.Int_value 0) ]) in
      let count =
        List.find
          (fun (f : Param.field_view) -> f.name = "count")
          (Sop.Node.parameter_fields clamped)
      in
      check
        (count.current = Param.Int_value 1)
        "integer hard bound differs from native inspector normalization")

let () =
  (* The body would fail its node's hard range if evaluated before validation. *)
  let source =
    {|(workspace validate
    (defnode sop/validated
      :validate (> count 0) :code "E_POSITIVE" :message "count must be positive"
      [(count : int 1 :min -10 :max 10)]
      (sop/line :kind "Points" :points count))
    (graph g :context sop (sop/validated)))|}
  in
  let node = lower source in
  match Sop.Node.apply_parameters node [ ("count", Param.Int_value 0) ] with
  | Error message ->
      check (Rdk_test_support.contains message "E_POSITIVE") "body ran before node validation"
  | Ok _ -> failwith "node validation did not reject edited value"

let () =
  with_session (fun session ->
      let optional =
        {|(workspace optional
    (defnode sop/optional [(geo : geometry :required false)]
      (if (equal? geo nil) (sop/line :kind "Points" :points 1) geo))
    (graph g :context sop (sop/optional)))|}
      in
      check
        (Rdk.Geometry.point_count (cook session (lower optional)) = 1)
        "absent optional slot did not become nil";
      List.iter
        (fun (slot, call, points) ->
          let text =
            Printf.sprintf
              {|(workspace rest
      (defnode sop/repeated [(geos : geometry %s :rest true)]
        (if (> (count geos) 0) (first geos) (sop/line :kind "Points" :points 1)))
      (graph g :context sop %s))|}
              slot call
          in
          let node = lower text in
          check
            (Rdk.Geometry.point_count (cook session node) = points)
            "rest-slot topology passthrough failed")
        [
          ("", "(sop/repeated (sop/line :points 2) (sop/line :points 3))", 2);
          (":required false", "(sop/repeated)", 1);
          (":required false", "(sop/repeated (sop/line :points 3))", 3);
          (":required false", "(sop/repeated nil)", 1);
        ])

let () =
  let source =
    {|(workspace nested
    (defnode sop/inner [(geo : geometry) (distance : float 0.5)]
      (sop/with_attr geo :P (exact (map (fn [p] (+ p [distance 0.0 0.0])) (sop/attr geo :P)))))
    (defnode sop/outer [(geo : geometry) (distance : float 1.5)]
      (sop/inner geo :distance distance))
    (graph g :context sop (sop/outer (sop/line :kind "Points" :points 1))))|}
  in
  with_session (fun session ->
      let node = lower source in
      let geometry = cook session node in
      check
        (Rdk.Packed.Float3.get (Rdk.Geometry.positions geometry) 0 = (1.5, 0., 0.))
        "nested Lisp call lost its parameters";
      let before = Sop.Session.stats session in
      let node, _ = ok (Sop.Node.apply_parameters node [ ("distance", Param.Float_value 2.5) ]) in
      let geometry = cook session node in
      check
        (Rdk.Packed.Float3.get (Rdk.Geometry.positions geometry) 0 = (2.5, 0., 0.))
        "nested Lisp parameter edit did not recook";
      check
        ((Sop.Session.stats session).misses = before.misses + 1)
        "nested bodies leaked private cache nodes")

let () =
  let source =
    {|(workspace exact
    (defnode sop/shift [(geo : geometry) (distance : float 0.125)]
      (sop/with_attr geo :P (exact (map (fn [p] (+ p [distance -0.0 0.0])) (sop/attr geo :P)))))
    (graph g :context sop (sop/shift (sop/grid :columns 64 :rows 64))))|}
  in
  let baseline =
    with_session (fun session -> Rdk_test_support.geometry_bytes (cook session (lower source)))
  in
  List.iter
    (fun reference ->
      let node = lower ~reference source in
      List.iter
        (fun domains ->
          List.iter
            (fun grain ->
              with_session (fun session ->
                  let geometry = cook session ~domains ~grain node in
                  check
                    (Rdk.Geometry.point_count geometry = 4_225)
                    "exactness fixture cardinality changed";
                  check
                    (Rdk_test_support.geometry_bytes geometry = baseline)
                    "Lisp geometry bytes differ by interpreter/kernel/domain/grain"))
            [ 1; 97; 4096 ])
        [ 1; 4 ])
    [ false; true ]

let () =
  List.iter
    (fun (keyword, call) ->
      with_session (fun session ->
          let source =
            Printf.sprintf
              {|(workspace typed_function
    (defnode sop/function_slot [(field : (fn [vec3] float) %s)]
      (sop/iso_surface :field field :resolution [4 4 4]))
    (graph g :context sop
      (let* [radius 0.7 field (fn [p] (- (length p) radius))]
        %s)))|}
              keyword call
          in
          let geometry = cook session (lower source) in
          let native =
            cook session
              (Lisp_sop.node
                 "(sop/iso_surface :field (fn [p] (- (length p) 0.7)) :resolution [4 4 4])")
          in
          check
            (Rdk_test_support.geometry_bytes geometry = Rdk_test_support.geometry_bytes native)
            "typed function slot lost its callable or captures"))
    [ ("", "(sop/function_slot field)"); (":keyword true", "(sop/function_slot :field field)") ]

let () =
  List.iter
    (fun (keyword, call) ->
      with_session (fun session ->
          let source =
            Printf.sprintf
              {|(workspace typed_image
    (defnode sop/image_slot [(geo : geometry) (picture : image %s)]
      (sop/attr_from_image geo :image picture))
    (graph g :context sop %s))|}
              keyword call
          in
          let geometry = cook session (lower source) in
          let native =
            cook session
              (Lisp_sop.node
                 {|(sop/attr_from_image (sop/grid :columns 2 :rows 2 :uv_attribute "uv") :image (image/noise :width 2 :height 2))|})
          in
          check
            (Rdk_test_support.geometry_bytes geometry = Rdk_test_support.geometry_bytes native)
            "typed image slot lost its cooked pixels"))
    [
      ( "",
        {|(sop/image_slot (sop/grid :columns 2 :rows 2 :uv_attribute "uv") (image/noise :width 2 :height 2))|}
      );
      ( ":keyword true",
        {|(sop/image_slot (sop/grid :columns 2 :rows 2 :uv_attribute "uv") :picture (image/noise :width 2 :height 2))|}
      );
    ]

let () =
  with_session (fun session ->
      let body =
        {|(let* [local (fn [p] (- (length (+ p [0.25 0.0 0.0])) 0.45))]
    (sop/merge
      (sop/iso_surface :field field :resolution [4 4 4])
      (sop/iso_surface :field local :resolution [4 4 4])))|}
      in
      let source =
        Printf.sprintf
          {|(workspace distinct_functions
    (defnode sop/two_fields [(field : (fn [vec3] float))] %s)
    (graph g :context sop (sop/two_fields (fn [p] (- (length p) 0.7)))))|}
          body
      in
      let native = Printf.sprintf {|(let* [field (fn [p] (- (length p) 0.7))] %s)|} body in
      check
        (Rdk_test_support.geometry_bytes (cook session (lower source))
        = Rdk_test_support.geometry_bytes (cook session (Lisp_sop.node native)))
        "outer function forwarding replaced a distinct local function")

let () =
  with_session (fun session ->
      let body call =
        Printf.sprintf
          {|(let* [base (sop/line :kind "Points" :points 1 :origin [0.7 0.0 0.0])
         sample {:radius (array/sum (sop/attr base :P))}
         field (fn [p] (- (length p) sample.radius.x))] %s)|}
          call
      in
      let source =
        Printf.sprintf
          {|(workspace cooked_capture
    (defnode sop/function_capture [(field : (fn [vec3] float))]
      (sop/iso_surface :field field :resolution [4 4 4]))
    (graph g :context sop %s))|}
          (body "(sop/function_capture field)")
      in
      let geometry = cook session (lower source) in
      let native =
        cook session (Lisp_sop.node (body "(sop/iso_surface :field field :resolution [4 4 4])"))
      in
      check
        (Rdk_test_support.geometry_bytes geometry = Rdk_test_support.geometry_bytes native)
        "function slot lost its outer cooked-geometry capture")

let () =
  with_session (fun session ->
      let body call =
        Printf.sprintf
          {|(let* [base (sop/line :kind "Points" :points 1 :origin [0.7 0.2 -0.1])
      sample {:offset (array/sum (sop/attr base :P))}
      field (fn [p] (+ p sample.offset))
      geo (sop/grid :columns 2 :rows 2)] %s)|}
          call
      in
      let native =
        cook session
          (Lisp_sop.node (body "(sop/with_attr geo :P (exact (map field (sop/attr geo :P))))"))
      in
      List.iter
        (fun call ->
          let source =
            Printf.sprintf
              {|(workspace map_capture
      (defnode sop/map_field [(geo : geometry) (field : (fn [vec3] vec3))]
        (sop/with_attr geo :P (exact (map field (sop/attr geo :P)))))
      (defnode sop/map_outer [(geo : geometry) (field : (fn [vec3] vec3))]
        (sop/map_field geo field))
      (graph g :context sop %s))|}
              (body call)
          in
          List.iter
            (fun reference ->
              let node = lower ~reference source in
              check
                (List.length (Sop.Node.inputs node) = 2)
                "forwarded function geometry leaked into public input slots";
              List.iter
                (fun domains ->
                  with_session (fun session ->
                      let geometry = cook session ~domains node in
                      check
                        (Rdk_test_support.geometry_bytes geometry
                        = Rdk_test_support.geometry_bytes native)
                        "map function slot confused its separate outer geometry capture with the \
                         input geometry";
                      let before = Sop.Session.stats session in
                      check
                        (cook session ~domains node == geometry
                        && (Sop.Session.stats session).misses = before.misses)
                        "unchanged forwarded geometry capture missed the public cache"))
                [ 1; 4 ])
            [ false; true ])
        [ "(sop/map_field geo field)"; "(sop/map_outer geo field)" ])

let () =
  let source =
    {|(workspace refused
    (defnode sop/unsupported_map [(geo : geometry)]
      (sop/with_attr geo :P (exact
        (map (fn [p] (if (equal? p [0.0 0.0 0.0]) p (+ p [1.0 0.0 0.0])))
          (sop/attr geo :P)))))
    (graph g :context sop (sop/unsupported_map (sop/grid :columns 2 :rows 2))))|}
  in
  (* Projection and graph compilation retain the body; cooking is where strict kernel preparation must refuse it. *)
  match Flow_sop.Lower.workspace (parse source) with
  | Error diagnostic ->
      check
        (diagnostic.code = "E_KERNEL_FORM"
        && diagnostic.message = "Numeric node bodies must compile to packed kernels."
        && diagnostic.span <> None)
        ("numeric Lisp body had an unrelated refusal: " ^ Diagnostic.to_string diagnostic)
  | Ok lowered -> (
      let graph = List.find (fun (graph : Flow_sop.Lower.graph) -> graph.default) lowered.graphs in
      match Lisp_sop.root lowered graph with
      | Error message ->
          check
            (List.mem "[E_KERNEL_FORM]" (String.split_on_char ' ' message))
            ("numeric Lisp body had an unrelated graph refusal: " ^ message)
      | Ok node ->
          with_session (fun session ->
              match
                Sop.Session.cook session ~context:(ok (Sop.Context.create ~domains:1 ())) node
              with
              | Error diagnostic ->
                  check
                    (diagnostic.code = "E_KERNEL_FORM"
                    && diagnostic.message = "Numeric node bodies must compile to packed kernels."
                    && diagnostic.span <> None)
                    ("numeric Lisp body had an unrelated cook refusal: "
                    ^ Diagnostic.error_to_string diagnostic)
              | Ok _ -> failwith "numeric Lisp body silently accepted an interpreter fallback"))

let () =
  with_session (fun session ->
      let prefix call =
        Printf.sprintf
          {|(let* [base (sop/line :kind "Points" :points 1 :origin [0.7 0.2 -0.1])
    sample {:offset (array/sum (sop/attr base :P))}
    field (fn [p] (+ p sample.offset))
    other (fn [p] (- p sample.offset))
    geo (sop/grid :columns 2 :rows 2)] %s)|}
          call
      in
      let unchanged = cook session (Lisp_sop.node (prefix "geo")) in
      let mapped =
        cook session
          (Lisp_sop.node (prefix "(sop/with_attr geo :P (exact (map field (sop/attr geo :P))))"))
      in
      let twice =
        cook session
          (Lisp_sop.node
             (prefix "(sop/with_attr geo :P (exact (map field (map field (sop/attr geo :P)))))"))
      in
      let different =
        cook session
          (Lisp_sop.node
             (prefix "(sop/with_attr geo :P (exact (map other (map field (sop/attr geo :P)))))"))
      in
      let cases =
        [
          ( {|[(geo : geometry) (field : (fn [vec3] vec3) :required false)]|},
            {|(if (equal? field nil) geo (sop/with_attr geo :P (exact (map field (sop/attr geo :P)))))|},
            [ ("(sop/slots geo)", unchanged); ("(sop/slots geo field)", mapped) ] );
          ( {|[(geo : geometry) (fields : (fn [vec3] vec3) :rest true :required false)]|},
            {|(if (= (count fields) 0) geo
      (sop/with_attr geo :P (exact (map (first fields) (sop/attr geo :P)))))|},
            [
              ("(sop/slots geo)", unchanged);
              ("(sop/slots geo field field)", mapped);
              ("(sop/slots geo (list field field))", mapped);
            ] );
          ( {|[(geo : geometry) (fields : (fn [vec3] vec3) :rest true)]|},
            {|(let* [field (first fields) other (first (rest fields))]
      (sop/with_attr geo :P (exact (map other (map field (sop/attr geo :P))))))|},
            [ ("(sop/slots geo field field)", twice); ("(sop/slots geo field other)", different) ]
          );
          ( {|[(geo : geometry :required false) (field : (fn [vec3] vec3))]|},
            {|(let* [target (if (equal? geo nil) (sop/grid :columns 2 :rows 2) geo)]
      (sop/with_attr target :P (exact (map field (sop/attr target :P)))))|},
            [ ("(sop/slots nil field)", mapped) ] );
          ( {|[(field : (fn [vec3] vec3)) (geos : geometry :rest true :required false)]|},
            {|(let* [target (if (= (count geos) 0) (sop/grid :columns 2 :rows 2) (first geos))]
      (sop/with_attr target :P (exact (map field (sop/attr target :P)))))|},
            [ ("(sop/slots field)", mapped); ("(sop/slots field geo geo)", mapped) ] );
        ]
      in
      List.iter
        (fun (inputs, body, calls) ->
          List.iter
            (fun (call, expected) ->
              let source =
                Printf.sprintf
                  {|(workspace repeated_functions
      (defnode sop/slots %s %s) (graph g :context sop %s))|}
                  inputs body (prefix call)
              in
              let node = lower source in
              let geometry = cook session node in
              check
                (Rdk_test_support.geometry_bytes geometry = Rdk_test_support.geometry_bytes expected)
                ("typed callable slot changed bytes: " ^ call);
              let before = Sop.Session.stats session in
              check
                (cook session node == geometry && (Sop.Session.stats session).misses = before.misses)
                "unchanged optional/rest callable missed its cache")
            calls)
        cases)

let () =
  with_session (fun session ->
      let prefix call =
        Printf.sprintf
          {|(let* [geo (sop/grid :columns 2 :rows 2 :uv_attribute "uv")
    picture (image/noise :width 2 :height 2)] %s)|}
          call
      in
      let unchanged = cook session (Lisp_sop.node (prefix "geo")) in
      let textured =
        cook session
          (Lisp_sop.node (prefix {|(sop/attr_from_image geo :image picture :attribute "shade")|}))
      in
      List.iter
        (fun (slot, body, calls) ->
          List.iter
            (fun (call, expected) ->
              let source =
                Printf.sprintf
                  {|(workspace repeated_images
      (defnode sop/images [(geo : geometry) %s] %s)
      (graph g :context sop %s))|}
                  slot body (prefix call)
              in
              check
                (Rdk_test_support.geometry_bytes (cook session (lower source))
                = Rdk_test_support.geometry_bytes expected)
                ("typed image slot changed bytes: " ^ call))
            calls)
        [
          ( {|(picture : image :required false)|},
            {|(if (equal? picture nil) geo
      (sop/attr_from_image geo :image picture :attribute "shade"))|},
            [ ("(sop/images geo)", unchanged); ("(sop/images geo picture)", textured) ] );
          ( {|(pictures : image :rest true :required false)|},
            {|(if (= (count pictures) 0) geo
      (sop/attr_from_image geo :image (first pictures) :attribute "shade"))|},
            [
              ("(sop/images geo)", unchanged);
              ("(sop/images geo picture picture)", textured);
              ("(sop/images geo (list picture picture))", textured);
            ] );
        ])

let () =
  let source amount helper =
    Printf.sprintf
      {|(workspace stable_body
    (defn shifted :context value [(p : vec3)] (+ p [%s 0.0 0.0]))
    (defnode sop/stable_body [(geo : geometry) (amount : float 0.0)]
      (sop/with_attr geo :P (exact (map (fn [(p : vec3)]
        (+ (shifted p) [amount 0.0 0.0])) (sop/attr geo :P)))))
    (graph g :context sop (let* [upstream (sop/line :kind "Points" :points 3)
      edited (sop/stable_body upstream :amount %s)
      sibling (sop/stable_body upstream :amount 0.0)] (sop/merge edited sibling))))|}
      helper amount
  in
  let lower ?previous text =
    let compiled_ids, sites =
      match previous with
      | None -> (None, None)
      | Some (lowered : Flow_sop.Lower.t) -> (Some lowered.compiled_ids, Some lowered.sites)
    in
    match
      Flow_sop.Lower.workspace ?compiled_ids ?sites ~factories:Sop.Nodes.factories (parse text)
    with
    | Ok lowered ->
        let graph = List.hd lowered.graphs in
        (lowered, ok (Lisp_sop.root lowered graph))
    | Error d -> failwith (Diagnostic.to_string d)
  in
  List.iter
    (fun domains ->
      with_session (fun session ->
          let first, node = lower (source "1.0" "2.0") in
          ignore (cook session ~domains node);
          let before = Sop.Session.stats session in
          let edited, node = lower ~previous:first (source "3.0" "2.0") in
          ignore (cook session ~domains node);
          let after = Sop.Session.stats session in
          (* the edited card's body, its card and the merge recook; the line and the sibling's body
             and card hit *)
          check
            (after.misses - before.misses = 3 && after.hits - before.hits = 4)
            "A call-site parameter edit recooked an unchanged Lisp sibling";
          let _, node = lower ~previous:edited (source "3.0" "4.0") in
          let changed = cook session ~domains node in
          let after_helper = Sop.Session.stats session in
          check
            (after_helper.misses - after.misses = 3 && after_helper.hits - after.hits = 4)
            "A helper body edit failed to invalidate both dependent Lisp bodies";
          let _, oracle = lower (source "3.0" "4.0") in
          with_session (fun fresh ->
              check
                (Rdk_test_support.geometry_bytes changed
                = Rdk_test_support.geometry_bytes (cook fresh ~domains oracle))
                "Helper invalidation returned stale cached geometry")))
    [ 1; 8 ];
  print_endline
    "Lisp declaration cache keys preserve unchanged siblings and invalidate edited helpers at \
     one/eight domains"

let () =
  let source ?(extra = "") values =
    Printf.sprintf
      {|(workspace declaration_cones
    %s
    (defn unrelated :context value [] %s)
    (defn leaf :context value [(p : vec3)] (+ p [%s 0.0 0.0]))
    (defmacro unrelated_adjusted [p] `(+ ~p [%s 0.0 0.0]))
    (defmacro adjusted_inner [p] `(+ ~p [%s 0.0 0.0]))
    (defmacro adjusted [p] `(adjusted_inner ~p))
    (defmacro dispatch [callee arg] `(~callee ~arg))
    (defmacro dispatched_inner [p] `(let* [held# ~p] (+ held# [%s 0.0 0.0])))
    (defn through :context value [(p : vec3)] (leaf (dispatch dispatched_inner (adjusted p))))
    (defn allowed :context value [(amount : float)] (>= amount %s))
    (defn canonical_owner :context value [(raw : text)] %S)
    (defn owner :context value [(raw : text)] (canonical_owner raw))
    (defnode sop/cone_child :validate (allowed amount)
      [(geo : geometry) (amount : float %s)]
      (let* [moved (exact (map through (sop/attr geo :P)))]
        (sop/with_attr geo :P (exact (map (fn [(p : vec3)]
          (+ p [(+ amount %s) 0.0 0.0])) moved)))))
    (defnode sop/cone_parent
      [(geo : geometry) (delta : float 0.0)
       (rules : text " POINT " :kind encoded :normalize (fn [(raw : text)] (owner raw)))]
      (let* [child (sop/cone_child geo)]
        (sop/with_attr child :P (exact (map (fn [(p : vec3)]
          (+ p [delta 0.0 0.0])) (sop/attr child :P))))))
    (graph g :context sop (let* [upstream (sop/line :kind "Points" :points 3)
      edited (sop/cone_parent upstream :delta 1.0)
      sibling (sop/cone_parent upstream :delta 0.0)] (sop/merge edited sibling))))|}
      extra values.(0) values.(1) values.(7) values.(6) values.(8) values.(4) values.(5) values.(2)
      values.(3)
  in
  let initial = [| "0.0"; "2.0"; "0.0"; "0.0"; "-10.0"; "point"; "0.0"; "0.0"; "0.0" |] in
  let lower ?previous ?extra values =
    let compiled_ids, sites =
      match previous with
      | None -> (None, None)
      | Some (lowered : Flow_sop.Lower.t) -> (Some lowered.compiled_ids, Some lowered.sites)
    in
    let lowered =
      match Flow_sop.Lower.workspace ?compiled_ids ?sites (parse (source ?extra values)) with
      | Ok lowered -> lowered
      | Error d -> failwith (Diagnostic.to_string d)
    in
    let graph = List.hd lowered.graphs in
    (lowered, ok (Lisp_sop.root lowered graph))
  in
  List.iter
    (fun domains ->
      List.iter
        (fun (index, value, label) ->
          with_session (fun session ->
              let first, node = lower initial in
              let before_bytes = Rdk_test_support.geometry_bytes (cook session ~domains node) in
              let before = Sop.Session.stats session in
              let changed = Array.copy initial in
              changed.(index) <- value;
              let _, node = lower ~previous:first changed in
              let output = cook session ~domains node in
              let after = Sop.Session.stats session in
              if index = 0 || index = 7 then begin
                check
                  (after.misses = before.misses && after.hits > before.hits)
                  "An unrelated helper edit recooked a declaration outside its dependency cone";
                check
                  (Rdk_test_support.geometry_bytes output = before_bytes)
                  "An unrelated helper edit changed cached geometry"
              end
              else
                (* the cards' fused bodies recook (the oracle below checks their bytes), the line hits *)
                check
                  (after.misses > before.misses && after.hits > before.hits)
                  (label
                 ^ ": a transitive declaration edit must invalidate both consumers and keep Line \
                    warm");
              let _, oracle = lower changed in
              with_session (fun fresh ->
                  check
                    (Rdk_test_support.geometry_bytes output
                    = Rdk_test_support.geometry_bytes (cook fresh ~domains oracle))
                    (label ^ ": declaration edit returned stale geometry"));
              let before = Sop.Session.stats session in
              ignore (cook session ~domains node);
              check
                ((Sop.Session.stats session).misses = before.misses)
                (label ^ ": unchanged declaration cone missed its warm cache")))
        [
          (0, "9.0", "unrelated helper");
          (1, "4.0", "transitive helper/Fn_ref");
          (2, "0.5", "child default");
          (3, "0.75", "child body");
          (4, "-9.0", "child validation helper");
          (5, "detail", "transitive normalizer helper");
          (6, "1.25", "nested macro");
          (7, "2.0", "unrelated macro");
          (8, "0.25", "parameterized macro head");
        ])
    [ 1; 8 ];
  let extra =
    {|(defmacro irrelevant_bump [x] `(let* [owned# ~x] (+ owned# 1.0)))
    (defn extra_unrelated :context value [] (irrelevant_bump 2.0))|}
  in
  List.iter
    (fun domains ->
      with_session (fun session ->
          let first, node = lower initial in
          let bytes = Rdk_test_support.geometry_bytes (cook session ~domains node) in
          let before = Sop.Session.stats session in
          let _, node = lower ~previous:first ~extra initial in
          check
            (Rdk_test_support.geometry_bytes (cook session ~domains node) = bytes)
            "Unrelated structural macro/helper edits changed cached geometry";
          let after = Sop.Session.stats session in
          check
            (after.misses = before.misses && after.hits > before.hits)
            "Unrelated macro expansion changed a reachable hygienic source key"))
    [ 1; 8 ];
  print_endline
    "Declaration cones: unrelated helpers retain hits; Fn_ref/helper, child \
     body/default/validator, normalizer and macro edits invalidate at one/eight domains"

(* Deferred child cooking stays inside its authored error boundary. *)
let () =
  let calls = ref 0 in
  let retained =
    [
      Diagnostic.{ node_id = 88; label = "retained child"; operation = "native.retained" };
      Diagnostic.{ node_id = 89; label = "retained input"; operation = "native.input" };
    ]
  in
  let child_error =
    {
      (Diagnostic.error ~cause:"\255 raw 日本😀" ~hints:[ "retained hint" ] ~code:"E_CHILD"
         "child cook failed")
      with
      trace = retained;
    }
  in
  let factory =
    Sop.Edit_graph.factory ~key:"failing_child" ~label:"Failing child" ~category:[ "Test" ] ~arity:1
      (fun inputs ->
        Result.ok
        @@ Sop.Node.Build.make ~operation:"native.failing_child" ~version:1 ~parameters:""
             ~cook_mode:Sop.Node.Generic ~inputs:(Array.of_list inputs)
             ~dependencies:Sop.Context.Dependencies.static (fun ~node_id:_ _context _inputs ->
               incr calls;
               Error child_error))
  in
  let source =
    {|(workspace boundary
    (defnode sop/error_boundary [(geo : geometry) (enabled : bool true)]
      (if enabled
        (with_error (sop/failing_child geo)
          (fn [failure] (assoc failure :code "E_WRAPPED" :message "authored wrapping")))
        (with_error geo (fn [failure] (error "E_MAPPER" "unused mapper")))))
    (graph g :context sop (sop/error_boundary (sop/line :points 2))))|}
  in
  let unused_source =
    {|(workspace boundary
    (defnode sop/error_boundary [(geo : geometry) (enabled : bool true)]
      (let* [unused (if enabled
        (with_error (sop/failing_child geo)
          (fn [failure] (assoc failure :code "E_WRAPPED" :message "authored wrapping")))
        nil)] geo))
    (graph g :context sop (sop/error_boundary (sop/line :points 2))))|}
  in
  let compile reference text =
    match
      Flow_sop.Lower.workspace ~reference ~factories:(factory :: Sop.Nodes.factories) (parse text)
    with
    | Error d -> failwith (Diagnostic.to_string d)
    | Ok lowered ->
        let graph = List.hd lowered.graphs in
        ok (Lisp_sop.root lowered graph)
  in
  List.iter
    (fun source ->
      List.iter
        (fun reference ->
          let node = compile reference source in
          List.iter
            (fun domains ->
              List.iter
                (fun grain ->
                  with_session (fun session ->
                      let before = !calls in
                      match
                        Sop.Session.cook session
                          ~context:(ok (Sop.Context.create ~domains ~grain ()))
                          node
                      with
                      | Ok _ -> failwith "protected child failure was lost"
                      | Error actual ->
                          check
                            (actual.code = "E_WRAPPED"
                            && actual.message = "authored wrapping"
                            && actual.cause = child_error.cause)
                            "deferred diagnostic mapping changed its authored payload";
                          check
                            (actual.hints = child_error.hints
                            && List.filter (fun frame -> List.mem frame retained) actual.trace
                               = retained)
                            "deferred mapping lost native hints or trace";
                          check
                            (!calls = before + 1)
                            "protected child cooked twice or outside the boundary"))
                [ 1; 97; 4096 ])
            [ 1; 4; 8 ];
          let forms = parse source in
          let rec disable (form : Flow.Syntax.t) =
            let node =
              match form.node with
              | Flow.Syntax.List (({ node = Sym "sop/error_boundary"; _ } as head) :: args) ->
                  Flow.Syntax.List
                    ((head :: args)
                    @ [ Flow.Syntax.make (Kw "enabled"); Flow.Syntax.make (Sym "false") ])
              | List children -> List (List.map disable children)
              | Vec children -> Vec (List.map disable children)
              | Map children -> Map (List.map disable children)
              | node -> node
            in
            { form with node }
          in
          let disabled = fst (Flow.Lisp.print (List.map disable forms)) in
          let before = !calls in
          let original =
            with_session (fun session ->
                cook session
                  (lower "(workspace original (graph g :context sop (sop/line :points 2)))"))
          in
          let actual = with_session (fun session -> cook session (compile reference disabled)) in
          check
            (!calls = before
            && Rdk_test_support.geometry_bytes actual = Rdk_test_support.geometry_bytes original)
            "untaken child/mapper executed or altered successful geometry")
        [ false; true ])
    [ source; unused_source ];
  print_endline
    "Diagnostic boundaries: deferred and unused protected children cook once; raw causes, hints, \
     trace, untaken arms and one/four/eight domains agree"

(* sop/context_frame: the cook frame as a float, a Frame cache dependency, rebuilt per frame. *)
let () =
  let source body =
    Printf.sprintf
      {|(workspace frame_body
    (defnode sop/frame_probe [(geo : geometry)]
      (sop/with_attr geo "f" (array/float (sop/count geo :points) %s) :owner "point" :type float))
    (graph g :context sop (sop/frame_probe (sop/line :points 3))))|}
      body
  in
  let cook_at session frame node =
    match Sop.Session.cook session ~context:(ok (Sop.Context.create ~frame ())) node with
    | Ok output -> (
        match Sop.Payload.geometry output.payload with
        | Ok g -> Rdk_test_support.geometry_bytes g
        | Error d -> failwith (Diagnostic.error_to_string d))
    | Error d -> failwith (Diagnostic.error_to_string d)
  in
  List.iter
    (fun reference ->
      let node = lower ~reference (source "(sop/context_frame)") in
      check
        (Sop.Context.Dependencies.mem Frame (Sop.Node.dependencies node))
        "context frame lost its Frame cache dependency";
      check
        (not (Sop.Context.Dependencies.mem Seed (Sop.Node.dependencies node)))
        "context frame added a seed dependency";
      with_session (fun session ->
          List.iter
            (fun frame ->
              let expected =
                cook_at session frame (lower ~reference (source (Printf.sprintf "%Ld.0" frame)))
              in
              check
                (cook_at session frame node = expected)
                "context frame changed with the cook frame";
              check (cook_at session frame node = expected) "context frame cook is not repeatable")
            [ 0L; 7L; 3L; 7L ]);
      check
        (not
           (Sop.Context.Dependencies.mem Frame
              (Sop.Node.dependencies (lower ~reference (source "1.0")))))
        "a constant body gained a Frame dependency")
    [ false; true ];
  (match
     Flow_sop.Lower.workspace
       (parse "(workspace w (graph g :context sop (sop/line :points (int (sop/context_frame)))))")
   with
  | Error d -> check (d.code = "E_NODE_CONTEXT") "context frame outside a body has the wrong code"
  | Ok _ -> failwith "context frame outside a defnode body was accepted");
  print_endline "Context frame: float cook frame, Frame dependency, rebuilt per frame, body only"

(* sop/context_time: the cook time in seconds, a Time cache dependency (not Frame), body only. *)
let () =
  let source body =
    Printf.sprintf
      {|(workspace time_body
    (defnode sop/time_probe [(geo : geometry)]
      (sop/with_attr geo "t" (array/float (sop/count geo :points) %s) :owner "point" :type float))
    (graph g :context sop (sop/time_probe (sop/line :points 3))))|}
      body
  in
  let cook_at session time node =
    match Sop.Session.cook session ~context:(ok (Sop.Context.create ~time ())) node with
    | Ok output -> (
        match Sop.Payload.geometry output.payload with
        | Ok g -> Rdk_test_support.geometry_bytes g
        | Error d -> failwith (Diagnostic.error_to_string d))
    | Error d -> failwith (Diagnostic.error_to_string d)
  in
  List.iter
    (fun reference ->
      let node = lower ~reference (source "(sop/context_time)") in
      check
        (Sop.Context.Dependencies.mem Time (Sop.Node.dependencies node))
        "context time lost its Time cache dependency";
      check
        (not (Sop.Context.Dependencies.mem Frame (Sop.Node.dependencies node)))
        "context time added a Frame dependency";
      with_session (fun session ->
          List.iter
            (fun time ->
              let expected =
                cook_at session time (lower ~reference (source (Printf.sprintf "%F" time)))
              in
              check (cook_at session time node = expected) "context time changed with the cook time")
            [ 0.; 1.25; 0.5; 1.25 ]);
      check
        (not
           (Sop.Context.Dependencies.mem Time
              (Sop.Node.dependencies (lower ~reference (source "1.0")))))
        "a constant body gained a Time dependency")
    [ false; true ];
  (match
     Flow_sop.Lower.workspace
       (parse "(workspace w (graph g :context sop (sop/line :points (int (sop/context_time)))))")
   with
  | Error d -> check (d.code = "E_NODE_CONTEXT") "context time outside a body has the wrong code"
  | Ok _ -> failwith "context time outside a defnode body was accepted");
  print_endline "Context time: float cook time, Time dependency, rebuilt per time, body only"
