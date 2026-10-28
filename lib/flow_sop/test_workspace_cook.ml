(* W2: every workspace fixture lowers and cooks, byte for byte the same at 1
   and 3 domains; node counts; provenance. *)
open Flow_sop
open Flow_graph

let fail message = failwith ("test_workspace_cook: " ^ message)
let check condition message = if not condition then fail message
let read path = In_channel.with_open_bin path In_channel.input_all
let cases = "../specification/workspace/cases"
let factories = Sop.Nodes.factories

let merge nodes =
  let with_ = List.mapi (fun i n -> (Printf.sprintf "i%d" i, n)) nodes in
  Lisp_sop.node ~with_
    ("(sop/merge " ^ String.concat " " (List.map (fun (k, _) -> "(sop/ext_" ^ k ^ ")") with_) ^ ")")

let () =
  let catalog = Result.get_ok (Editor_document.Kinds.catalog ~version:Manifest.version factories) in
  let forms =
    Result.get_ok
      (Flow.Syntax.parse
         "(workspace pixels (graph g :context image [] (let* [img (image/noise :width 17 :height 9 \
          :frequency 0.12 :seed 31)] img)))")
  in
  let checked =
    match Flow.Workspace.check catalog forms with
    | Some checked, [] -> checked
    | _, diagnostics -> fail (String.concat "\n" (List.map Diagnostic.to_string diagnostics))
  in
  let card =
    Projection.find (Projection.of_graph catalog checked "g") [ "g"; "img" ] |> Option.get
  in
  List.iter
    (fun name ->
      check
        (List.exists (fun (row : Projection.row) -> row.key = Flow_edit.Kw name) card.rows)
        ("image/noise projects " ^ name))
    [ "width"; "height"; "frequency"; "seed" ];
  let lowered = Lower.of_checked ~factories checked |> Result.get_ok in
  let graph = List.hd lowered.graphs in
  let node =
    Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)
    |> Result.get_ok
  in
  let session = Sop.Session.create ~max_entries:16 ~max_payload_bytes:65536 |> Result.get_ok in
  Fun.protect
    ~finally:(fun () -> Sop.Session.close session)
    (fun () ->
      let context = Sop.Context.create ~domains:8 ~grain:17 () |> Result.get_ok in
      let image =
        Sop.Session.cook session ~context node |> Result.get_ok |> fun output ->
        Sop.Payload.image output.payload |> Result.get_ok
      in
      check
        (Sop.Image.width image = 17 && Sop.Image.height image = 9)
        "Lisp image/noise cooked dimensions");
  let changed =
    Flow_edit.apply catalog forms
      (Flow_edit.Set_arg
         {
           node = [ "g"; "img" ];
           key = Flow_edit.Kw "width";
           sub = [];
           value = Flow.Syntax.parse "19" |> Result.get_ok |> List.hd;
         })
  in
  check (Result.is_ok changed) "image/noise width edits through the graph"

let lowered_cache = Hashtbl.create 8

let lower ?(edit = Fun.id) name =
  match Hashtbl.find_opt lowered_cache name with
  | Some l -> l
  | None ->
      let lowered =
        match Flow.Syntax.parse (edit (read (Filename.concat cases (name ^ ".lisp")))) with
        | Error d -> fail (Diagnostic.to_string d)
        | Ok forms -> (
            match Lower.workspace ~extra:Editor_document.Kinds.descriptors ~factories forms with
            | Ok lowered -> lowered
            | Error d -> fail (name ^ ": " ^ Diagnostic.to_string d))
      in
      Hashtbl.add lowered_cache name lowered;
      lowered

let geometry_bytes geometry =
  let open Rdk in
  let encode value = Marshal.to_string value [ Marshal.No_sharing ] in
  let attribute (attribute : Attribute.t) =
    let payload =
      match Attribute.storage attribute with
      | Float values -> encode values
      | Int values -> encode values
      | Text values -> encode values
      | Int_array values -> encode (Packed.Int_array.View.view values)
      | Float_array values -> encode (Packed.Float_array.View.view values)
      | Float2 values ->
          let v = Packed.Float2.View.view values in
          encode (v.x, v.y)
      | Float3 values ->
          let v = Packed.Float3.View.view values in
          encode (v.x, v.y, v.z)
      | Float4 values ->
          let v = Packed.Float4.View.view values in
          encode (v.x, v.y, v.z, v.w)
    in
    (Attribute.owner attribute, Attribute.name attribute, Attribute.kind_name attribute, payload)
  in
  let groups =
    List.map
      (fun group ->
        ( Group.owner group,
          Group.name group,
          Array.init (Group.length group) (fun index -> Group.mem index group),
          Group.ordered_elements group ))
      (Geometry.groups geometry)
  in
  let positions = Packed.Float3.View.view (Geometry.positions geometry) in
  encode
    ( positions.x,
      positions.y,
      positions.z,
      Topology.View.view (Geometry.topology geometry),
      List.map attribute (Geometry.attributes geometry),
      groups )

let cook ~domains (graph : Lower.graph) =
  match graph.root with
  | None -> None
  | Some root ->
      let compiled =
        match Sop.Edit_graph.compile_node graph.network.geometry ~node_id:root with
        | Ok compiled -> compiled
        | Error message -> fail message
      in
      let context =
        match Sop.Context.create ~domains ~grain:97 ~seed:42L () with
        | Ok context -> context
        | Error message -> fail message
      in
      let session =
        Sop.Session.create ~max_entries:512 ~max_payload_bytes:(256 * 1024 * 1024) |> Result.get_ok
      in
      Fun.protect
        ~finally:(fun () -> Sop.Session.close session)
        (fun () ->
          match Sop.Session.cook session ~context compiled with
          | Ok output -> Some (Result.get_ok (Sop.Payload.geometry output.payload))
          | Error error -> fail (Diagnostic.error_to_string error))

let slow = Sys.getenv_opt "RAYS_SLOW" = Some "1"
let fixtures = [ "kit"; "tree"; "tunnel"; "variations"; "wave" ]
let fixtures () = if slow then fixtures else [ "kit" ]
let nodes (graph : Lower.graph) = List.length (Sop.Edit_graph.inspect graph.network.geometry)

let operations (graph : Lower.graph) operation =
  List.length
    (List.filter
       (fun (n : Sop.Edit_graph.node_info) -> n.operation = operation)
       (Sop.Edit_graph.inspect graph.network.geometry))

(* register L16: a skipped iteration makes no node; the others keep compiled ids, plan keys and
   provenance, so the cook keeps hitting its cache *)
let skips () =
  let text skip =
    "(workspace w (graph g :context sop (sop/merge (for [i (range 4)] " ^ skip
    ^ "(sop/transform (sop/box) :translate [i 0 0])))))"
  in
  let lower_text ?previous source =
    match Flow.Syntax.parse source with
    | Error d -> fail (Diagnostic.to_string d)
    | Ok forms -> (
        let compiled_ids = Option.map (fun (l : Lower.t) -> l.compiled_ids) previous
        and sites = Option.map (fun (l : Lower.t) -> l.sites) previous in
        match
          Lower.workspace ~extra:Editor_document.Kinds.descriptors ~factories ?compiled_ids ?sites
            forms
        with
        | Ok lowered -> lowered
        | Error d -> fail (Diagnostic.to_string d))
  in
  let all = lower_text (text "") in
  let some = lower_text ~previous:all (text ":skip [2] ") in
  let graph (l : Lower.t) = List.find (fun (g : Lower.graph) -> g.name = "g") l.graphs in
  (* inlined, each Lisp card is a marker node over its body *)
  let cards l = operations (graph l) "flow.card" in
  check
    (cards some = cards all - 2)
    "a skipped iteration did not remove its box and transform cards";
  let two = lower_text ~previous:all (text ":skip [1 2] ") in
  check
    (nodes (graph all) - nodes (graph two) = 2 * (nodes (graph all) - nodes (graph some)))
    "a skipped iteration did not remove its own nodes";
  Lower.Path_map.iter
    (fun path id ->
      match Lower.Path_map.find_opt path all.compiled_ids with
      | Some before -> check (before = id) "a surviving node changed its compiled id"
      | None -> fail "a skip minted a compiled id")
    some.compiled_ids;
  (* provenance: what the merge took names the iteration that made it, not its position *)
  let iters (l : Lower.t) =
    List.sort_uniq compare
      (List.filter_map
         (fun (o : Lower.origin) ->
           if o.iter <> [] && List.length o.site > 0 then Some o.iter else None)
         (List.map snd (Network.Int_map.bindings l.provenance)))
  in
  check
    (iters all = [ [ 0 ]; [ 1 ]; [ 2 ]; [ 3 ] ] && iters some = [ [ 0 ]; [ 1 ]; [ 3 ] ])
    "the provenance of a skipped loop does not name the iterations that ran";
  let bytes (l : Lower.t) = geometry_bytes (Option.get (cook ~domains:1 (graph l))) in
  check
    (bytes some = geometry_bytes (Option.get (cook ~domains:3 (graph some))))
    "a skipped loop cooks differently at 1 and 3 domains";
  check (bytes some <> bytes all) "a skipped loop cooked like the whole"

let optional_rest () =
  let module Edit = Sop.Edit_graph in
  let factory =
    Edit.factory_slots ~key:"rest_fixture" ~operation:"merge" ~label:"Rest fixture"
      ~category:[ "Test" ] ~slots:[ "base"; "fixed"; "extras" ]
      ~inputs:[ Required; Optional; Optional_rest ] (fun inputs ->
        Ok (merge (List.filter_map Fun.id inputs)))
  in
  let factories = factory :: factories in
  let catalog = Catalog.of_factories ~version:1 factories |> Result.get_ok in
  let manifest, _ = Manifest.generate factories |> Result.get_ok in
  let manifest_catalog = Flow.Catalog.catalog_of_manifest manifest |> Result.get_ok |> fst in
  let fixture =
    List.find_opt
      (fun (kind : Flow.Catalog.kind) -> kind.qualified = "sop/rest_fixture")
      manifest_catalog.kinds
  in
  check
    (match fixture with
    | Some kind ->
        List.exists (fun (slot : Flow.Catalog.slot) -> slot.rest && not slot.required) kind.slots
    | None -> false)
    "optional rest survives manifest serialization";
  let source result =
    Printf.sprintf
      "(workspace w (graph g :context sop (let* [a (sop/box) b (sop/box :center [2 0 0]) r %s] r)))"
      result
  in
  let lower_text ?previous text =
    let forms = Flow.Syntax.parse text |> Result.get_ok in
    let compiled_ids, sites =
      match previous with
      | None -> (Lower.Path_map.empty, [])
      | Some (lowered : Lower.t) -> (lowered.compiled_ids, lowered.sites)
    in
    match Lower.workspace ~factories ~compiled_ids ~sites forms with
    | Ok lowered -> lowered
    | Error diagnostic -> fail (Diagnostic.to_string diagnostic)
  in
  let a = Lisp_sop.node {|(sop/box)|} and b = Lisp_sop.node {|(sop/box :center [2.0 0.0 0.0])|} in
  let cook_node nodes =
    let context = Sop.Context.create ~domains:1 ~seed:42L () |> Result.get_ok in
    let session =
      Sop.Session.create ~max_entries:16 ~max_payload_bytes:10_000_000 |> Result.get_ok
    in
    Fun.protect
      ~finally:(fun () -> Sop.Session.close session)
      (fun () ->
        match Sop.Session.cook session ~context (merge nodes) with
        | Ok output -> Result.get_ok (Sop.Payload.geometry output.payload)
        | Error error -> fail (Diagnostic.error_to_string error))
  in
  List.iter
    (fun (result, nodes) ->
      let lowered = lower_text (source result) in
      let graph = List.hd lowered.graphs in
      let expected = geometry_bytes (cook_node nodes) in
      List.iter
        (fun domains ->
          let geometry = Option.get (cook ~domains graph) in
          check
            (geometry_bytes geometry = expected)
            "fixed/optional/rest inputs retain order when lowered")
        [ 1; 3 ])
    [
      ("(sop/rest_fixture a)", [ a ]);
      ("(sop/rest_fixture a b)", [ a; b ]);
      ("(sop/rest_fixture a nil b a)", [ a; b; a ]);
      ("(sop/rest_fixture a :extras (list b a))", [ a; b; a ]);
      ("(sop/rest_fixture :extras (list b a) :base a)", [ a; b; a ]);
      ("(first (map sop/rest_fixture (list a) (list nil) (list (list b a))))", [ a; b; a ]);
    ];
  let bad_function_call =
    "(workspace w (defn caller :context sop [(make : fn) (a : geometry) (b : geometry)] (first \
     (map make (list a) (list b)))) (graph g :context sop (caller sop/null (sop/box) (sop/box))))"
  in
  (match Lower.workspace ~factories (Flow.Syntax.parse bad_function_call |> Result.get_ok) with
  | Error diagnostic ->
      check
        (diagnostic.code = "E_EXTRA_POSITIONAL")
        ("kind functions retain their input bound: " ^ Diagnostic.to_string diagnostic)
  | Ok _ -> fail "fixed kind function silently ignored an extra geometry input");
  let text = source "(sop/rest_fixture a)" in
  let forms = Flow.Syntax.parse text |> Result.get_ok in
  let workspace =
    match Flow.Workspace.check catalog forms with
    | Some workspace, _ -> workspace
    | _ -> fail "optional rest did not check"
  in
  let projected = Projection.of_graph catalog workspace "g" in
  let node = Projection.find projected [ "g"; "r" ] |> Option.get in
  check
    (List.exists
       (fun (row : Projection.row) -> row.kind = Projection.Add && row.key = Flow_edit.Pos 2)
       node.rows)
    "optional rest projects an add row after the fixed prefix";
  let before = lower_text text in
  let edited =
    Flow_edit.apply catalog forms
      (Flow_edit.Connect { node = [ "g"; "r" ]; key = Flow_edit.Pos 2; src = "b"; iter = false })
    |> Result.get_ok
  in
  let after = lower_text ~previous:before (fst (Flow.Lisp.print edited)) in
  check
    ((List.hd before.graphs).root = (List.hd after.graphs).root)
    "connecting optional rest preserves compiled identity";
  let geometry = Option.get (cook ~domains:1 (List.hd after.graphs)) in
  check
    (geometry_bytes geometry = geometry_bytes (cook_node [ a; b ]))
    "Flow_edit pads disconnected fixed slots before the rest connection";
  let edited =
    Flow_edit.apply catalog edited
      (Flow_edit.Disconnect { node = [ "g"; "r" ]; key = Flow_edit.Pos 2; fallback = None })
    |> Result.get_ok
  in
  let disconnected = lower_text ~previous:after (fst (Flow.Lisp.print edited)) in
  check
    (geometry_bytes (Option.get (cook ~domains:1 (List.hd disconnected.graphs)))
    = geometry_bytes (cook_node [ a ]))
    "Flow_edit disconnect restores zero extras";
  let workspace =
    match
      Flow.Workspace.check catalog
        (Flow.Syntax.parse (source "(sop/rest_fixture a :extras (list b a))") |> Result.get_ok)
    with
    | Some workspace, _ -> workspace
    | _ -> fail "named rest did not check"
  in
  let node =
    Projection.find (Projection.of_graph catalog workspace "g") [ "g"; "r" ] |> Option.get
  in
  check
    (List.exists
       (fun (row : Projection.row) -> row.key = Flow_edit.Kw "extras" && row.kind = Projection.Rest)
       node.rows)
    "named rest lists project as editable rows"

let attribute_composite () =
  let source result =
    Printf.sprintf
      "(workspace w (graph g :context sop (let* [a (sop/box) b (sop/box :center [2 0 0]) r %s] r)))"
      result
  in
  let a = Lisp_sop.node {|(sop/box)|} and b = Lisp_sop.node {|(sop/box :center [2.0 0.0 0.0])|} in
  let expected =
    Lisp_sop.node
      ~with_:[ ("a", a); ("b", b) ]
      {|(sop/attribute_composite (sop/ext_a) nil (sop/ext_b) nil nil (sop/ext_b) (sop/ext_a) :allow_position true :weight2 0.5 :weights "0.2\n0.3")|}
  in
  let context = Sop.Context.create ~domains:1 ~seed:42L () |> Result.get_ok in
  let session = Sop.Session.create ~max_entries:16 ~max_payload_bytes:10_000_000 |> Result.get_ok in
  let expected =
    Fun.protect
      ~finally:(fun () -> Sop.Session.close session)
      (fun () ->
        match Sop.Session.cook session ~context expected with
        | Ok output -> geometry_bytes (Result.get_ok (Sop.Payload.geometry output.payload))
        | Error error -> fail (Diagnostic.error_to_string error))
  in
  let catalog = Catalog.of_factories ~version:1 factories |> Result.get_ok in
  List.iter
    (fun result ->
      let forms = Flow.Syntax.parse (source result) |> Result.get_ok in
      let lowered =
        match Lower.workspace ~factories forms with
        | Ok lowered -> lowered
        | Error diagnostic -> fail (Diagnostic.to_string diagnostic)
      in
      List.iter
        (fun domains ->
          check
            (geometry_bytes (Option.get (cook ~domains (List.hd lowered.graphs))) = expected)
            "Attribute Composite Lisp fixed and repeated layer parity")
        [ 1; 4 ];
      let workspace =
        match Flow.Workspace.check catalog forms with
        | Some workspace, _ -> workspace
        | _ -> fail "Attribute Composite did not check"
      in
      let node =
        Projection.find (Projection.of_graph catalog workspace "g") [ "g"; "r" ] |> Option.get
      in
      check
        (List.exists (fun (row : Projection.row) -> row.key = Flow_edit.Kw "weights") node.rows)
        "Attribute Composite extra weights project as an editable field";
      let edited =
        Flow_edit.apply catalog forms
          (Flow_edit.Set_arg
             {
               node = [ "g"; "r" ];
               key = Flow_edit.Kw "weights";
               sub = [];
               value = Flow.Syntax.parse "\"0.4\\n0.3\"" |> Result.get_ok |> List.hd;
             })
        |> Result.get_ok
      in
      let after =
        match
          Lower.workspace ~factories ~compiled_ids:lowered.compiled_ids ~sites:lowered.sites edited
        with
        | Ok lowered -> lowered
        | Error diagnostic -> fail (Diagnostic.to_string diagnostic)
      in
      check
        ((List.hd after.graphs).root = (List.hd lowered.graphs).root)
        "Attribute Composite weight edit preserves node identity";
      check
        (geometry_bytes (Option.get (cook ~domains:1 (List.hd after.graphs))) <> expected)
        "Attribute Composite weight edit reaches the native cook")
    [
      "(sop/attribute_composite a nil b nil nil b a :allow_position true :weight2 0.5 :weights \
       \"0.2\\n0.3\")";
      "(sop/attribute_composite a :layer2 b :layers (list b a) :allow_position true :weight2 0.5 \
       :weights \"0.2\\n0.3\")";
    ]

let switch () =
  let source result =
    Printf.sprintf
      "(workspace w (graph g :context sop (let* [a (sop/box) b (sop/box :center [2 0 0]) c \
       (sop/box :center [4 0 0]) r %s] r)))"
      result
  in
  let catalog = Catalog.of_factories ~version:1 factories |> Result.get_ok in
  let cooked result =
    let forms = Flow.Syntax.parse (source result) |> Result.get_ok in
    let lowered =
      match Lower.workspace ~factories forms with
      | Ok lowered -> lowered
      | Error diagnostic -> fail (Diagnostic.to_string diagnostic)
    in
    let workspace =
      match Flow.Workspace.check catalog forms with
      | Some workspace, _ -> workspace
      | _ -> fail "Switch did not check"
    in
    let node =
      Projection.find (Projection.of_graph catalog workspace "g") [ "g"; "r" ] |> Option.get
    in
    check
      (result.[0] <> '('
      || List.exists
           (fun (row : Projection.row) -> row.kind = Projection.Add && row.label = "+ inputs")
           node.rows)
      "Switch offers a further branch as an add row";
    List.map
      (fun domains -> geometry_bytes (Option.get (cook ~domains (List.hd lowered.graphs))))
      [ 1; 4 ]
  in
  let third = cooked "c" in
  List.iter
    (fun result ->
      check (cooked result = third) ("Switch did not cook its third branch: " ^ result))
    [ "(sop/switch a b c :input 2)"; "(sop/switch a b :inputs (list c) :input 2)" ];
  check (cooked "(sop/switch a b c :input 1)" = cooked "b") "Switch did not cook its second branch"

let blend_shapes () =
  let source result =
    Printf.sprintf
      "(workspace w (graph g :context sop (let* [a (sop/box) b (sop/box :center [2 0 0]) c \
       (sop/box :center [4 0 0]) r %s] r)))"
      result
  in
  let a = Lisp_sop.node {|(sop/box)|}
  and b = Lisp_sop.node {|(sop/box :center [2.0 0.0 0.0])|}
  and c = Lisp_sop.node {|(sop/box :center [4.0 0.0 0.0])|} in
  let expected =
    Lisp_sop.node
      ~with_:[ ("a", a); ("b", b); ("c", c) ]
      {|(sop/blend_shapes (sop/ext_a) nil (sop/ext_b) nil nil (sop/ext_c) (sop/ext_a) :weight2 0.5 :weights "0.2\n0.3" :shape_masks "5\tother_mask\tshape")|}
  in
  let context = Sop.Context.create ~domains:1 ~seed:42L () |> Result.get_ok in
  let session = Sop.Session.create ~max_entries:16 ~max_payload_bytes:10_000_000 |> Result.get_ok in
  let expected =
    Fun.protect
      ~finally:(fun () -> Sop.Session.close session)
      (fun () ->
        match Sop.Session.cook session ~context expected with
        | Ok output -> geometry_bytes (Result.get_ok (Sop.Payload.geometry output.payload))
        | Error error -> fail (Diagnostic.error_to_string error))
  in
  let catalog = Catalog.of_factories ~version:1 factories |> Result.get_ok in
  List.iter
    (fun result ->
      let forms = Flow.Syntax.parse (source result) |> Result.get_ok in
      let lowered =
        match Lower.workspace ~factories forms with
        | Ok lowered -> lowered
        | Error diagnostic -> fail (Diagnostic.to_string diagnostic)
      in
      List.iter
        (fun domains ->
          check
            (geometry_bytes (Option.get (cook ~domains (List.hd lowered.graphs))) = expected)
            "Blend Shapes positional/named rest Lisp parity")
        [ 1; 4 ];
      let workspace =
        match Flow.Workspace.check catalog forms with
        | Some workspace, _ -> workspace
        | _ -> fail "Blend Shapes did not check"
      in
      let projected =
        Projection.find (Projection.of_graph catalog workspace "g") [ "g"; "r" ] |> Option.get
      in
      List.iter
        (fun name ->
          check
            (List.exists (fun (row : Projection.row) -> row.key = Flow_edit.Kw name) projected.rows)
            ("Blend Shapes projects " ^ name))
        [ "weights"; "shape_masks" ];
      let edited =
        Flow_edit.apply catalog forms
          (Flow_edit.Set_arg
             {
               node = [ "g"; "r" ];
               key = Flow_edit.Kw "weights";
               sub = [];
               value = Flow.Syntax.parse "\"0.4\\n0.3\"" |> Result.get_ok |> List.hd;
             })
        |> Result.get_ok
      in
      let edited =
        Flow_edit.apply catalog edited
          (Flow_edit.Set_arg
             {
               node = [ "g"; "r" ];
               key = Flow_edit.Kw "shape_masks";
               sub = [];
               value = Flow.Syntax.parse "\"5\\tmask\\tfirst\"" |> Result.get_ok |> List.hd;
             })
        |> Result.get_ok
      in
      let after =
        match
          Lower.workspace ~factories ~compiled_ids:lowered.compiled_ids ~sites:lowered.sites edited
        with
        | Ok lowered -> lowered
        | Error diagnostic -> fail (Diagnostic.to_string diagnostic)
      in
      check
        ((List.hd after.graphs).root = (List.hd lowered.graphs).root)
        "Blend Shapes parameter edit preserves identity";
      check
        (geometry_bytes (Option.get (cook ~domains:1 (List.hd after.graphs))) <> expected)
        "Blend Shapes weight edit reaches the native cook";
      let workspace =
        match Flow.Workspace.check catalog edited with
        | Some workspace, _ -> workspace
        | _ -> fail "Blend Shapes mask table edit did not check"
      in
      let projected =
        Projection.find (Projection.of_graph catalog workspace "g") [ "g"; "r" ] |> Option.get
      in
      check
        (List.exists
           (fun (row : Projection.row) -> row.key = Flow_edit.Kw "shape_masks" && row.expr <> None)
           projected.rows)
        "Blend Shapes mask table edits remain projected")
    [
      "(sop/blend_shapes a nil b nil nil c a :weight2 0.5 :weights \"0.2\\n0.3\" :shape_masks \
       \"5\\tother_mask\\tshape\")";
      "(sop/blend_shapes a :shape2 b :shapes (list c a) :weight2 0.5 :weights \"0.2\\n0.3\" \
       :shape_masks \"5\\tother_mask\\tshape\")";
    ]

(* Lowering costs 0.5-1 s per sphere-like node (12 petals: 8 s), so the default loop keeps the
   checks of the mechanism (switch, skips, the kit fixture at 1 and 3 domains) and RAYS_SLOW=1 adds
   the per-node parity sections and the other eleven fixtures with their provenance checks. *)
let run () =
  switch ();
  skips ();
  if slow then (
    blend_shapes ();
    attribute_composite ();
    optional_rest ());
  let cooked = ref 0 in
  List.iter
    (fun name ->
      let lowered = lower name in
      let catalog =
        Editor_document.Kinds.catalog ~version:Manifest.version factories |> Result.get_ok
      in
      let forms =
        Flow.Syntax.parse (read (Filename.concat cases (name ^ ".lisp"))) |> Result.get_ok
      in
      let checked =
        match Catalog.check_workspace ~ops:Operators.all catalog forms with
        | Some checked, _ -> checked
        | _ -> fail (name ^ " did not check")
      in
      let from_checked, phases =
        Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () ->
            Lower.of_checked ~factories checked |> Result.get_ok)
      in
      check
        (Flow.Phase_timer.calls phases "check" = 0
        && Flow.Phase_timer.calls phases "evaluate" = 1
        && Flow.Phase_timer.calls phases "lower" = 1)
        (name ^ ": checked lowering repeated a check or evaluation");
      check
        (from_checked.plan == from_checked.evaluated.plan
        && from_checked.states == from_checked.evaluated.states)
        (name ^ ": lowering did not retain its evaluation");
      let reference = Flow.Eval.static checked |> Result.get_ok in
      let rec same (a : Flow.Eval.value) (b : Flow.Eval.value) =
        match (a, b) with
        | Fn _, Fn _ -> true
        | List a, List b -> Array.length a = Array.length b && Array.for_all2 same a b
        | Record a, Record b | Struct (_, _, a), Struct (_, _, b) ->
            List.length a = List.length b
            && List.for_all2 (fun (n, a) (m, b) -> n = m && same a b) a b
        | _ ->
            Marshal.to_string a [ Marshal.No_sharing ] = Marshal.to_string b [ Marshal.No_sharing ]
      in
      let left_state = Flow.Eval.create_state () and right_state = Flow.Eval.create_state () in
      List.iteri
        (fun frame time ->
          let live = { (Input.at_time time) with frame } in
          let same_value a b =
            match
              (Flow.Eval.force ~state:left_state a ~live, Flow.Eval.force ~state:right_state b ~live)
            with
            | Ok a, Ok b -> same a b
            | Error a, Error b -> a = b
            | _ -> false
          in
          let pairs a b =
            List.length a = List.length b
            && List.for_all2 (fun (n, a) (m, b) -> n = m && same_value a b) a b
          in
          let a = lowered.evaluated and b = from_checked.evaluated in
          let same_evaluation (a : Flow.Eval.t) (b : Flow.Eval.t) =
            Array.length a.plan.nodes = Array.length b.plan.nodes
            && Array.for_all2
                 (fun (a : Flow.Eval.node) (b : Flow.Eval.node) ->
                   a.id = b.id && a.inst = b.inst && a.kind = b.kind && a.ty = b.ty
                   && a.site = b.site && a.iter = b.iter && pairs a.args b.args)
                 a.plan.nodes b.plan.nodes
            && Array.length a.plan.instances = Array.length b.plan.instances
            && Array.for_all2
                 (fun (a : Flow.Eval.instance) (b : Flow.Eval.instance) ->
                   a.graph = b.graph && a.default = b.default && pairs a.inputs b.inputs
                   && same_value a.result b.result)
                 a.plan.instances b.plan.instances
            && pairs a.results b.results
          in
          check
            (same_evaluation a b && same_evaluation reference b
            && List.length a.records = List.length b.records
            && List.for_all2 (fun (p, a) (q, b) -> p = q && pairs a b) a.records b.records)
            (Printf.sprintf "%s: checked/source lowering differ at %g" name time))
        [ 0.; 0.125; 1.25; 7. ];
      List.iter
        (fun (graph : Lower.graph) ->
          match
            try (cook ~domains:1 graph, cook ~domains:3 graph)
            with Failure m -> fail (name ^ "/" ^ graph.name ^ ": " ^ m)
          with
          | None, None -> ()
          | Some a, Some b -> (
              incr cooked;
              check
                (geometry_bytes a = geometry_bytes b)
                (Printf.sprintf "%s/%s differs at 1 and 3 domains" name graph.name);
              (* the source and checked lowerings give the same geometry (fresh ids do not leak) *)
              let again =
                List.find (fun (g : Lower.graph) -> g.instance = graph.instance) from_checked.graphs
              in
              match cook ~domains:1 again with
              | Some c ->
                  check
                    (geometry_bytes a = geometry_bytes c)
                    (name ^ "/" ^ graph.name ^ " is not deterministic")
              | None -> fail "second lowering lost its root")
          | _ -> fail "root differs")
        lowered.graphs)
    (fixtures ());
  if slow then begin
    check (!cooked >= 6) (Printf.sprintf "only %d graphs cooked" !cooked);
    (* Bloom (3 petals, the ref override 2): flower is 3 petals of (uv_sphere, transform, set_color,
     transform), a merge, the heart (uv_sphere, set_color) and the result
     merge; the ref override with 7 petals is its own network. *)
    let swap a b text = Str.global_replace (Str.regexp_string a) b text in
    let bloom =
      lower
        ~edit:(fun text ->
          swap ":petals 7" ":petals 2" (swap "(petals : int 12)" "(petals : int 3)" text))
        "bloom"
    in
    (match bloom.graphs with
    | [ flower; small ] ->
        check (nodes flower = (3 * 4) + 1 + 2 + 1) "bloom flower node count";
        check (operations flower "merge" = 2) "bloom flower merges";
        check (nodes small = (2 * 4) + 1 + 2 + 1) "bloom ref override node count";
        check (small.instance <> flower.instance) "distinct instances";
        (* provenance: the ring merge's input k is petal k's last transform *)
        let ring =
          List.find_map
            (fun (n : Sop.Edit_graph.node_info) ->
              let inputs = n.inputs in
              if n.operation = "merge" && Array.length inputs = 3 then Some n.id else None)
            (Sop.Edit_graph.inspect flower.network.geometry)
          |> Option.get
        in
        for k = 0 to 2 do
          let origin =
            snd
              (List.find
                 (fun (_, (o : Lower.origin)) -> o.merge = ring && o.input = k)
                 (Network.Int_map.bindings bloom.provenance))
          in
          check (origin.iter = [ k ]) "bloom provenance iteration";
          check
            (Some origin.source
            = (Option.get (Sop.Edit_graph.inputs flower.network.geometry ~node_id:ring)).(k))
            "bloom provenance source"
        done;
        check (Network.Int_map.cardinal bloom.provenance = 3 + 2 + 2 + 2) "bloom provenance size"
    | graphs -> fail (Printf.sprintf "bloom has %d sop networks" (List.length graphs)));
    (* every collecting merge tags its primitives with the input index *)
    let small =
      Lower.workspace ~extra:Editor_document.Kinds.descriptors ~factories
        (Flow.Syntax.parse
           "(workspace w (graph g :context sop (sop/merge (for [i (range 6)] (sop/uv_sphere \
            :center [i 0 0])))))"
        |> Result.get_ok)
      |> Result.get_ok
    in
    let graph = List.hd small.graphs in
    check (nodes graph = 7) "merge loop node count";
    let geometry = Option.get (cook ~domains:1 graph) in
    (match
       Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Primitive Lower.source_attribute geometry
     with
    | Some attribute -> (
        match Rdk.Attribute.View.storage attribute with
        | Rdk.Attribute.Int values ->
            check
              (Array.length values = Rdk.Geometry.primitive_count geometry
              && values.(0) = 0
              && values.(Array.length values - 1) = 5)
              "merge loop source attribute"
        | _ -> fail "source attribute is not int")
    | None -> fail "merge wrote no source attribute");
    (* Wave: the curve is a native node; t-dependent points are pending and
     the static cook uses t = 0 *)
    let wave = lower "wave" in
    check
      (List.length wave.pending >= 6
      && List.for_all (fun (p : Lower.pending) -> p.field = "points") wave.pending)
      "wave pending live parameters";
    check (Option.is_some (cook ~domains:1 (List.hd wave.graphs))) "wave cooks";
    (* ids are stable when the previous ids and sites are passed back *)
    let kit = lower "kit" in
    let again =
      match Flow.Syntax.parse (read (Filename.concat cases "kit.lisp")) with
      | Ok forms ->
          Lower.workspace ~extra:Editor_document.Kinds.descriptors ~factories
            ~compiled_ids:kit.compiled_ids ~sites:kit.sites forms
          |> Result.get_ok
      | Error _ -> fail "parse"
    in
    check
      (again.compiled_ids = kit.compiled_ids
      && Flow_sop.Network.Int_map.equal ( = ) again.compiled kit.compiled)
      "compiled ids are stable";
    (* the checker runs first *)
    match
      Lower.workspace ~extra:Editor_document.Kinds.descriptors ~factories
        (Flow.Syntax.parse "(workspace w (graph g :context sop (sop/nope)))" |> Result.get_ok)
    with
    | Error _ -> ()
    | Ok _ -> fail "lowering accepted an unchecked workspace"
  end;
  print_endline "workspace cook tests passed"
