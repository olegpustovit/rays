let ok = function Ok value -> value | Error diagnostic -> failwith (Diagnostic.to_string diagnostic)
let parse text = Flow.Syntax.parse text |> ok
let checked = function Some workspace, [] -> workspace
  | _, diagnostics -> failwith (String.concat "\n" (List.map Diagnostic.to_string diagnostics))

let () =
  let factories = [Sop.Nodes.Line.factory] in
  let forms = parse {|(workspace checked_field_defaults
    (defnode sop/iso_surface
      [(field : (fn [vec3] float) :keyword true)
       (resolution_x : float 1.0 :vec3 ["resolution" 0])
       (resolution_y : float 1.0 :vec3 ["resolution" 1])
       (resolution_z : float 1.0 :vec3 ["resolution" 2])
       (min_x : float -3.0 :vec3 ["min" 0])
       (min_y : float -4.0 :vec3 ["min" 1])
       (min_z : float -5.0 :vec3 ["min" 2])
       (max_x : float 7.0 :vec3 ["max" 0])
       (max_y : float 8.0 :vec3 ["max" 1])
       (max_z : float 9.0 :vec3 ["max" 2])]
      (sop/line :kind "Points" :points 2))
    (graph g :context sop
      (let* [field (fn [p] (+ p.x (+ p.y p.z)))]
        (sop/iso_surface :field field))))|} in
  let catalog = Flow_sop.Catalog.of_factories ~version:1 ~nodes:[] factories |> ok in
  let workspace = Flow_sop.Catalog.check_workspace ~sources:[]
      ~ops:Flow_sop.Operators.all catalog forms |> checked in
  let lowered = Flow_sop.Lower.of_checked ~factories workspace |> ok in
  let calls = Flow_sop.Lower.field_calls ~live:(Input.at_time 0.)
      ~resolve:(fun value -> Ok value) lowered in
  match calls ["g"; "field"] [] with
  | [(0, 8, at)] ->
      let samples k = List.assoc ["g"; "field"; "@result"] (ok (at k)) |> List.map snd in
      assert (samples 0 = [Flow.Eval.Float (-12.)]);
      assert (samples 7 = [Flow.Eval.Float 24.])
  | _ -> failwith "Field probe ignored the checked declaration's defaults"

let () =
  let catalog = Flow.Check.{version=1;kinds=[]} in
  let authored = parse "(workspace narrow (graph value :context value 0.0))" in
  let workspace = Flow_sop.Catalog.check_workspace catalog authored |> checked in
  assert (workspace.nodes=[] && workspace.ops=[]);
  let sources = parse "(defnode sop/selected_fixture [(geo : geometry)] geo)" in
  (* An explicit library is pruned to what the workspace reaches; ~library:true checks all of it. *)
  let workspace = Flow_sop.Catalog.check_workspace ~sources catalog authored |> checked in
  assert (workspace.nodes=[] && workspace.source=authored);
  let workspace = Flow_sop.Catalog.check_workspace ~library:true ~sources catalog authored |> checked in
  assert (List.map (fun (node : Flow.Workspace.node_definition) -> node.kind.qualified)
    workspace.nodes=["sop/selected_fixture"]);
  assert (workspace.source=authored)

let () =
  (* The embedded immutable source list uses the prepared index; a fresh list spine with
     the same forms takes the uncached path. Both must select the same definitions and
     preserve identical checker diagnostics. *)
  let embedded = ok (Flow_sop.Catalog.sources ()) in
  let copied = List.map Fun.id embedded in
  let nodes = ok (Flow_sop.Catalog.node_definitions ()) in
  let catalog = Flow_sop.Catalog.of_factories
      ~version:Flow_sop.Manifest.version ~nodes [] |> ok in
  let describe = function
    | None, diagnostics ->
        `Error (List.map Diagnostic.to_string diagnostics)
    | Some (workspace : Flow.Workspace.t), diagnostics ->
        `Ok (List.map Diagnostic.to_string diagnostics,
          List.map (fun (node : Flow.Workspace.node_definition) -> node.kind.qualified)
            workspace.nodes,
          List.map Flow.Syntax.head workspace.source)
  in
  let check sources text =
    let forms = parse text in
    let result = Flow_sop.Catalog.check_workspace ~sources
        ~ops:Flow_sop.Operators.all catalog forms in
    (forms, describe result)
  in
  let valid = {|(workspace indexed (graph g :context sop (sop/points :points 2)))|} in
  let invalid = {|(workspace indexed (graph g :context sop (sop/points :not_a_parameter 2)))|} in
  List.iter (fun text ->
    let fast_forms, fast = check embedded text in
    let fallback_forms, fallback = check copied text in
    assert (fast = fallback);
    assert (List.map Flow.Syntax.head fast_forms = List.map Flow.Syntax.head fallback_forms))
    [valid; invalid]

let () =
  let factories = [Sop.Nodes.Line.factory] in
  let source = parse {|(workspace helper_seed
    (defn helper_seed :context value [(enabled : bool)]
      (let* [selected enabled] (if selected (sop/context_seed "helper_fixture") 0)))
    (defn forward_seed :context value [(enabled : bool)] (helper_seed enabled))
    (defnode sop/helper_seed [(geo : geometry) (enabled : bool false)]
      (let* [shift (forward_seed (not enabled))]
        (sop/with_attr geo :P (exact
          (map (fn [p] (+ p [shift 0.0 0.0])) (sop/attr geo :P))))))
    (graph g :context sop (sop/helper_seed (sop/line :kind "Points" :points 2))))|} in
  let catalog = Flow_sop.Catalog.of_factories ~version:1 ~nodes:[] factories |> ok in
  let workspace = Flow_sop.Catalog.check_workspace ~sources:[]
    ~ops:Flow_sop.Operators.all catalog source |> checked in
  let lowered = Flow_sop.Lower.of_checked ~factories workspace |> ok in
  let graph = List.hd lowered.graphs in
  let node = Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)
    |> Result.get_ok in
  assert (Sop.Context.Dependencies.mem Seed (Sop.Node.dependencies node));
  let inactive, _ = Sop.Node.apply_parameters node ["enabled", Param.Bool_value true]
    |> Result.get_ok in
  assert (not (Sop.Context.Dependencies.mem Seed (Sop.Node.dependencies inactive)));
  let session = Sop.Session.create ~max_entries:16 ~max_payload_bytes:1_000_000 |> Result.get_ok in
  Fun.protect ~finally:(fun () -> Sop.Session.close session) (fun () ->
    let cook node seed =
      let context = Sop.Context.create ~seed () |> Result.get_ok in
      let output = Sop.Session.cook session ~context node |> Result.get_ok in
      let geometry = Sop.Payload.geometry output.payload |> Result.get_ok in
      Rdk.Packed.Float3.get (Rdk.Geometry.positions geometry) 0 in
    let first = cook node 3L in
    let before = Sop.Session.stats session in
    assert (cook node 0x7000000000000000L <> first);
    assert ((Sop.Session.stats session).misses = before.misses + 1);
    assert (cook inactive 3L = (0., 0., 0.));
    let before = Sop.Session.stats session in
    assert (cook inactive 0x7000000000000000L = (0., 0., 0.));
    assert ((Sop.Session.stats session).misses = before.misses))

let () =
  let source = parse {|(workspace fn_seed_scope
    (defn fn_seed_helper :context value []
      (first (map (fn [enabled] (if enabled (sop/context_seed "fn_fixture") 0))
        (list true))))
    (defnode sop/fn_seed_scope [(geo : geometry) (enabled : bool false)]
      (let* [shift (fn_seed_helper)] geo))
    (graph g :context sop (sop/fn_seed_scope (sop/line :points 2))))|} in
  let catalog = Flow_sop.Catalog.of_factories ~version:1 ~nodes:[] [Sop.Nodes.Line.factory] |> ok in
  let workspace = Flow_sop.Catalog.check_workspace ~sources:[]
    ~ops:Flow_sop.Operators.all catalog source |> checked in
  let definition = List.find (fun (node:Flow.Workspace.node_definition) ->
    node.kind.qualified="sop/fn_seed_scope") workspace.Flow.Workspace.nodes in
  assert (Flow_sop.Context_kernel.depends ~inputs:["enabled", Flow.Eval.Bool false]
    ~defs:workspace.defs definition.body)

let () =
  ignore (ok (Flow_sop.Catalog.sources ()));
  let forms = parse {|(defnode sop/embedded_fixture :label "Embedded fixture" :category ["Test"]
    :facts {:elementwise points :reads ["P"] :writes ["P"] :topology preserved :exact true}
    [(geo : geometry) (distance : float 0.5 :min -10 :max 10 :primary true)]
    (sop/null geo))|} in
  let node = Flow.Defnode.parse (List.hd forms) |> ok in
  let catalog = Flow_sop.Catalog.of_factories ~version:Flow_sop.Manifest.version
      ~nodes:[node] [] |> ok in
  let authored = parse {|(workspace w (graph g :context sop
    (sop/embedded_fixture (sop/line) :distance 2.0)))|} in
  (* The body delegates to a real native kind; the custom fixture itself is Lisp. *)
  let native = Flow_sop.Catalog.of_factories ~version:Flow_sop.Manifest.version
      ~nodes:[] [Sop.Nodes.Null.factory; Sop.Nodes.Line.factory] |> ok in
  let catalog = {catalog with Flow.Check.kinds = native.kinds @ catalog.kinds} in
  let workspace = Flow_sop.Catalog.check_workspace ~sources:forms catalog authored |> checked in
  assert (workspace.source == authored);
  assert (List.length workspace.Flow.Workspace.nodes = 1);
  let graph = List.hd workspace.graphs in
  let source_graph = List.hd (Flow.Syntax.children (List.hd authored) |> List.rev) in
  assert (graph.form.id = source_graph.id);
  assert ((List.hd workspace.nodes).body.form.id > source_graph.id);
  let edited, checked_edit = Flow_graph.Flow_edit.apply_checked
    ~checker:(Flow_sop.Catalog.check_workspace ~sources:forms) catalog authored
    (Flow_graph.Flow_edit.Set_note {node=["g"; "@result"]; text="kept note"}) |> ok in
  assert (checked_edit.source == edited);
  assert (List.length checked_edit.nodes = 1);
  let saved = fst (Flow.Lisp.print edited) in
  assert (List.for_all (fun form -> Flow.Syntax.head form <> Some "defnode")
    (Flow.Syntax.children (List.hd edited)));
  assert (List.length (Flow_sop.Catalog.check_workspace ~sources:forms catalog (parse saved)
    |> checked).nodes = 1);
  let manifest, digest = Flow_sop.Manifest.generate ~nodes:[node]
      [Sop.Nodes.Null.factory; Sop.Nodes.Line.factory] |> ok in
  let reread, read_digest = Flow.Check.catalog_of_manifest manifest |> ok in
  assert (digest = read_digest && reread.kinds = catalog.kinds);
  let tree = parse manifest |> List.hd in
  let rec has name text form =
    (Flow.Syntax.head form = Some name && List.exists (fun child ->
      match child.Flow.Syntax.node with Str value | Sym value -> value = text | _ -> false)
      (Flow.Syntax.children form)) || List.exists (has name text) (Flow.Syntax.children form) in
  assert (has "body" "lisp" tree);
  assert (has "calls" "sop/null" tree);
  let keyword_node = parse {|(defnode sop/keyword_fixture
    [(geo : geometry) (image : image :keyword true)
     (field : (fn [vec3] float) :keyword true)] geo)|}
    |> List.hd |> Flow.Defnode.parse |> ok in
  let keyword_manifest, _ = Flow_sop.Manifest.generate ~nodes:[keyword_node] [] |> ok in
  let keyword_catalog, _ = Flow.Check.catalog_of_manifest keyword_manifest |> ok in
  assert (keyword_catalog.kinds = [keyword_node.kind]);
  let keyword_tree = parse keyword_manifest |> List.hd in
  assert (has "keyword-inputs" "image" keyword_tree);
  assert (has "slot" "field" keyword_tree);
  assert (Result.is_error (Flow_sop.Catalog.of_factories ~version:1
    ~nodes:[{node with kind = {node.kind with qualified = "sop/null"}}] [Sop.Nodes.Null.factory]))

let () =
  let library distance = parse (Printf.sprintf {|(defnode sop/body_fixture [(geo : geometry)]
    (sop/with_attr geo :P (exact
      (map (fn [p] (+ p [%g 0.0 0.0])) (sop/attr geo :P)))))|} distance) in
  let first_library = library 1. and second_library = library 2. in
  let node = Flow.Defnode.parse (List.hd first_library) |> ok in
  let factories = [Sop.Nodes.Line.factory] in
  let catalog = Flow_sop.Catalog.of_factories ~version:1 ~nodes:[node] factories |> ok in
  let authored = parse {|(workspace body_edit (graph g :context sop
    (sop/body_fixture (sop/line :kind "Points" :points 2))))|} in
  let check sources = Flow_sop.Catalog.check_workspace ~sources
      ~ops:Flow_sop.Operators.all catalog authored |> checked in
  let first = Flow_sop.Lower.of_checked ~factories (check first_library) |> ok in
  let second = Flow_sop.Lower.of_checked ~factories ~compiled_ids:first.compiled_ids
      ~sites:first.sites (check second_library) |> ok in
  let macro_library distance = parse (Printf.sprintf {|(defnode sop/body_fixture [(geo : geometry)]
    (sop/with_attr geo :P (exact (map (fn [p] (move_point p)) (sop/attr geo :P)))))
    (defmacro move_point [p] `(+ ~p [%g 0.0 0.0]))|} distance) in
  let third = Flow_sop.Lower.of_checked ~factories ~compiled_ids:second.compiled_ids
      ~sites:second.sites (check (macro_library 3.)) |> ok in
  let fourth = Flow_sop.Lower.of_checked ~factories ~compiled_ids:third.compiled_ids
      ~sites:third.sites (check (macro_library 4.)) |> ok in
  let root (lowered : Flow_sop.Lower.t) =
    let graph = List.hd lowered.graphs in
    Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)
    |> Result.get_ok in
  let first = root first and second = root second and third = root third and fourth = root fourth in
  assert (Sop.Node.id first = Sop.Node.id second);
  let session = Sop.Session.create ~max_entries:16 ~max_payload_bytes:1_000_000 |> Result.get_ok in
  Fun.protect ~finally:(fun () -> Sop.Session.close session) (fun () ->
    let context = Sop.Context.create () |> Result.get_ok in
    let cook node =
      let output = Sop.Session.cook session ~context node |> Result.get_ok in
      let geometry = Sop.Payload.geometry output.payload |> Result.get_ok in
      Rdk.Packed.Float3.get (Rdk.Geometry.positions geometry) 0 in
    assert (cook first = (1., 0., 0.));
    List.iter (fun (node,x) ->
      assert (Sop.Node.id node = Sop.Node.id first);
      let before = Sop.Session.stats session in
      assert (cook node = (x, 0., 0.));
      assert ((Sop.Session.stats session).misses = before.misses + 1))
      [second,2.;third,3.;fourth,4.])

let () =
  let sources = parse {|(defnode sop/function_library
    [(field : (fn [vec3] float) :keyword true)]
    (sop/iso_surface :field field :resolution [4 4 4]))|} in
  let node = Flow.Defnode.parse (List.hd sources) |> ok in
  let factories = [Sop.Nodes.Iso_surface.factory] in
  let catalog = Flow_sop.Catalog.of_factories ~version:1 ~nodes:[node] factories |> ok in
  let workspace = Flow_sop.Catalog.check_workspace ~library:true ~sources
    ~ops:Flow_sop.Operators.all catalog (parse "(workspace function_library)") |> checked in
  let lowered = Flow_sop.Lower.of_checked ~factories workspace |> ok in
  ignore (Flow_sop.Catalog.of_factories ~version:1 ~nodes:[] lowered.factories |> ok);
  let sources = parse {|(defnode sop/map_function_library
    [(geo : geometry) (field : (fn [vec3] vec3) :keyword true)]
    (sop/with_attr geo :P (exact (map field (sop/attr geo :P)))))|} in
  let node = Flow.Defnode.parse (List.hd sources) |> ok in
  let map_factories = [Sop.Nodes.Line.factory] in
  let catalog = Flow_sop.Catalog.of_factories ~version:1 ~nodes:[node] map_factories |> ok in
  let workspace = Flow_sop.Catalog.check_workspace ~library:true ~sources
    ~ops:Flow_sop.Operators.all catalog (parse "(workspace map_function_library)") |> checked in
  let lowered = Flow_sop.Lower.of_checked ~factories:map_factories workspace |> ok in
  ignore (Flow_sop.Catalog.of_factories ~version:1 ~nodes:[] lowered.factories |> ok);
  let factory = List.hd lowered.factories in
  let disconnected = Sop.Edit_graph.instantiate_optional factory [None;None] |> Result.get_ok in
  assert (Sop.Node.operation disconnected = "map_function_library");
  assert (Sop.Node.parameter_fields disconnected = node.fields);
  let line = Sop.Edit_graph.instantiate Sop.Nodes.Line.factory [] |> Result.get_ok in
  let unbound = Sop.Edit_graph.instantiate factory [line;line] |> Result.get_ok in
  let workspace = Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog
    (parse {|(workspace actual_function (graph g :context sop
      (sop/map_function_library (sop/line :kind "Points" :points 2)
        :field (fn [p] (+ p [1.0 0.0 0.0])))))|}) |> checked in
  let lowered = Flow_sop.Lower.of_checked ~factories:map_factories workspace |> ok in
  let graph = List.hd lowered.graphs in
  let root = Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)
    |> Result.get_ok in
  let session = Sop.Session.create ~max_entries:16 ~max_payload_bytes:1_000_000 |> Result.get_ok in
  Fun.protect ~finally:(fun () -> Sop.Session.close session) (fun () ->
    let context = Sop.Context.create () |> Result.get_ok in
    (match Sop.Session.cook session ~context unbound with
     | Error diagnostic -> assert (diagnostic.code = "disconnected_input")
     | Ok _ -> failwith "Unbound function factory unexpectedly cooked");
    let output = Sop.Session.cook session ~context root |> Result.get_ok in
    let geometry = Sop.Payload.geometry output.payload |> Result.get_ok in
    assert (Rdk.Packed.Float3.get (Rdk.Geometry.positions geometry) 0 = (1., 0., 0.)))
