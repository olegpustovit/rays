let ok = function
  | Ok value -> value
  | Error diagnostic -> failwith (Diagnostic.to_string diagnostic)

let parse text = Flow.Syntax.parse text |> ok
let null_factory = List.find (fun f -> Sop.Edit_graph.factory_key f = "null") Sop.Nodes.factories

let line_nodes () =
  List.filter
    (fun (n : Flow.Defnode.t) -> n.kind.qualified = "sop/line")
    (ok (Flow_sop.Catalog.node_definitions ()))

let checked = function
  | Some workspace, [] -> workspace
  | _, diagnostics -> failwith (String.concat "\n" (List.map Diagnostic.to_string diagnostics))

let () =
  let catalog = Flow.Catalog.{ version = 1; kinds = [] } in
  let authored = parse "(workspace narrow (graph value :context value 0.0))" in
  let workspace = Flow_sop.Catalog.check_workspace catalog authored |> checked in
  assert (workspace.nodes = [] && workspace.ops = []);
  let sources = parse "(defnode sop/selected_fixture [(geo : geometry)] geo)" in
  (* An explicit library is pruned to what the workspace reaches; ~library:true checks all of it. *)
  let workspace = Flow_sop.Catalog.check_workspace ~sources catalog authored |> checked in
  assert (workspace.nodes = [] && workspace.source = authored);
  let workspace =
    Flow_sop.Catalog.check_workspace ~library:true ~sources catalog authored |> checked
  in
  assert (
    List.map (fun (node : Flow.Workspace.node_definition) -> node.kind.qualified) workspace.nodes
    = [ "sop/selected_fixture" ]);
  assert (workspace.source = authored)

let () =
  (* The embedded immutable source list uses the prepared index; a fresh list spine with
     the same forms takes the uncached path. Both must select the same definitions and
     preserve identical checker diagnostics. *)
  let embedded = ok (Flow_sop.Catalog.sources ()) in
  let copied = List.map Fun.id embedded in
  let nodes = ok (Flow_sop.Catalog.node_definitions ()) in
  let catalog = Flow_sop.Catalog.of_factories ~version:Flow_sop.Manifest.version ~nodes [] |> ok in
  let describe = function
    | None, diagnostics -> `Error (List.map Diagnostic.to_string diagnostics)
    | Some (workspace : Flow.Workspace.t), diagnostics ->
        `Ok
          ( List.map Diagnostic.to_string diagnostics,
            List.map
              (fun (node : Flow.Workspace.node_definition) -> node.kind.qualified)
              workspace.nodes,
            List.map Flow.Syntax.head workspace.source )
  in
  let check sources text =
    let forms = parse text in
    let result =
      Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms
    in
    (forms, describe result)
  in
  let valid = {|(workspace indexed (graph g :context sop (sop/points :points 2)))|} in
  let invalid = {|(workspace indexed (graph g :context sop (sop/points :not_a_parameter 2)))|} in
  List.iter
    (fun text ->
      let fast_forms, fast = check embedded text in
      let fallback_forms, fallback = check copied text in
      assert (fast = fallback);
      assert (List.map Flow.Syntax.head fast_forms = List.map Flow.Syntax.head fallback_forms))
    [ valid; invalid ]

let () =
  let factories = [] in
  (* a parameter change is an edit of the call's text *)
  let lower enabled =
    let source =
      parse
        ({|(workspace helper_seed
    (defn helper_seed :context value [(enabled : bool)]
      (let* [selected enabled] (if selected (sop/context_seed "helper_fixture") 0)))
    (defn forward_seed :context value [(enabled : bool)] (helper_seed enabled))
    (defnode sop/helper_seed [(geo : geometry) (enabled : bool false)]
      (let* [shift (forward_seed (not enabled))]
        (sop/with_attr geo :P (exact
          (map (fn [p] (+ p [shift 0.0 0.0])) (sop/attr geo :P))))))
    (graph g :context sop (sop/helper_seed (sop/line :kind "Points" :points 2) :enabled |}
       ^ enabled ^ ")))")
    in
    let catalog = Flow_sop.Catalog.of_factories ~version:1 ~nodes:(line_nodes ()) factories |> ok in
    let workspace =
      Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all catalog source |> checked
    in
    let lowered = Flow_sop.Lower.of_checked ~factories workspace |> ok in
    let graph = List.hd lowered.graphs in
    Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)
    |> Result.get_ok
  in
  let node = lower "false" in
  assert (Sop.Context.Dependencies.mem Seed (Sop.Node.dependencies node));
  let inactive = lower "true" in
  assert (not (Sop.Context.Dependencies.mem Seed (Sop.Node.dependencies inactive)));
  let session = Sop.Session.create ~max_entries:16 ~max_payload_bytes:1_000_000 |> Result.get_ok in
  Fun.protect
    ~finally:(fun () -> Sop.Session.close session)
    (fun () ->
      let cook node seed =
        let context = Sop.Context.create ~seed () |> Result.get_ok in
        let output =
          match Sop.Session.cook session ~context node with
          | Ok output -> output
          | Error d -> failwith (Diagnostic.error_to_string d)
        in
        let geometry = Sop.Payload.geometry output.payload |> Result.get_ok in
        Rdk.Packed.Float3.get (Rdk.Geometry.positions geometry) 0
      in
      let first = cook node 3L in
      let before = Sop.Session.stats session in
      assert (cook node 0x7000000000000000L <> first);
      assert ((Sop.Session.stats session).misses = before.misses + 1);
      assert (cook inactive 3L = (0., 0., 0.));
      let before = Sop.Session.stats session in
      assert (cook inactive 0x7000000000000000L = (0., 0., 0.));
      assert ((Sop.Session.stats session).misses = before.misses))

let () =
  let source =
    parse
      {|(workspace fn_seed_scope
    (defn fn_seed_helper :context value []
      (first (map (fn [enabled] (if enabled (sop/context_seed "fn_fixture") 0))
        (list true))))
    (defnode sop/fn_seed_scope [(geo : geometry) (enabled : bool false)]
      (let* [shift (fn_seed_helper)] geo))
    (graph g :context sop (sop/fn_seed_scope (sop/line :points 2))))|}
  in
  let catalog = Flow_sop.Catalog.of_factories ~version:1 ~nodes:(line_nodes ()) [] |> ok in
  let workspace =
    Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all catalog source |> checked
  in
  let definition =
    List.find
      (fun (node : Flow.Workspace.node_definition) -> node.kind.qualified = "sop/fn_seed_scope")
      workspace.Flow.Workspace.nodes
  in
  assert (
    Flow_sop.Context_kernel.depends
      ~inputs:[ ("enabled", Flow.Eval.Bool false) ]
      ~defs:workspace.defs definition.body)

let () =
  ignore (ok (Flow_sop.Catalog.sources ()));
  let forms =
    parse
      {|(defnode sop/embedded_fixture :label "Embedded fixture" :category ["Test"]
    :facts {:elementwise points :reads ["P"] :writes ["P"] :topology preserved :exact true}
    [(geo : geometry) (distance : float 0.5 :min -10 :max 10 :primary true)]
    (sop/null geo))|}
  in
  let node = Flow.Defnode.parse (List.hd forms) |> ok in
  let catalog =
    Flow_sop.Catalog.of_factories ~version:Flow_sop.Manifest.version ~nodes:[ node ] [] |> ok
  in
  let authored =
    parse
      {|(workspace w (graph g :context sop
    (sop/embedded_fixture (sop/line) :distance 2.0)))|}
  in
  (* The body delegates to a real native kind; the custom fixture itself is Lisp. *)
  let native =
    Flow_sop.Catalog.of_factories ~version:Flow_sop.Manifest.version ~nodes:(line_nodes ())
      [ null_factory ]
    |> ok
  in
  let catalog = { catalog with Flow.Catalog.kinds = native.kinds @ catalog.kinds } in
  let workspace = Flow_sop.Catalog.check_workspace ~sources:forms catalog authored |> checked in
  assert (workspace.source == authored);
  assert (List.length workspace.Flow.Workspace.nodes = 1);
  let graph = List.hd workspace.graphs in
  let source_graph = List.hd (Flow.Syntax.children (List.hd authored) |> List.rev) in
  assert (graph.form.id = source_graph.id);
  assert ((List.hd workspace.nodes).body.form.id > source_graph.id);
  let edited, checked_edit =
    Flow_graph.Flow_edit.apply_checked
      ~checker:(Flow_sop.Catalog.check_workspace ~sources:forms)
      catalog authored
      (Flow_graph.Flow_edit.Set_note { node = [ "g"; "@result" ]; text = "kept note" })
    |> ok
  in
  assert (checked_edit.source == edited);
  assert (List.length checked_edit.nodes = 1);
  let saved = fst (Flow.Lisp.print edited) in
  assert (
    List.for_all
      (fun form -> Flow.Syntax.head form <> Some "defnode")
      (Flow.Syntax.children (List.hd edited)));
  assert (
    List.length
      (Flow_sop.Catalog.check_workspace ~sources:forms catalog (parse saved) |> checked).nodes
    = 1);
  let manifest, digest =
    Flow_sop.Manifest.generate ~nodes:(node :: line_nodes ()) [ null_factory ] |> ok
  in
  let reread, read_digest = Flow.Catalog.catalog_of_manifest manifest |> ok in
  assert (digest = read_digest && List.sort compare reread.kinds = List.sort compare catalog.kinds);
  let tree = parse manifest |> List.hd in
  let rec has name text form =
    Flow.Syntax.head form = Some name
    && List.exists
         (fun child ->
           match child.Flow.Syntax.node with Str value | Sym value -> value = text | _ -> false)
         (Flow.Syntax.children form)
    || List.exists (has name text) (Flow.Syntax.children form)
  in
  assert (has "body" "lisp" tree);
  assert (has "calls" "sop/null" tree);
  let keyword_node =
    parse
      {|(defnode sop/keyword_fixture
    [(geo : geometry) (image : image :keyword true)
     (field : (fn [vec3] float) :keyword true)] geo)|}
    |> List.hd |> Flow.Defnode.parse |> ok
  in
  let keyword_manifest, _ = Flow_sop.Manifest.generate ~nodes:[ keyword_node ] [] |> ok in
  let keyword_catalog, _ = Flow.Catalog.catalog_of_manifest keyword_manifest |> ok in
  assert (keyword_catalog.kinds = [ keyword_node.kind ]);
  let keyword_tree = parse keyword_manifest |> List.hd in
  assert (has "keyword-inputs" "image" keyword_tree);
  assert (has "slot" "field" keyword_tree)
