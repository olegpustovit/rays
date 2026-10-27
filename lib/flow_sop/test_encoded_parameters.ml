module S = Flow.Syntax
module W = Flow.Workspace
let ok = function Ok value->value|Error message->failwith message
let flow_ok = function Ok value->value|Error d->failwith(Diagnostic.to_string d)
let parse text=flow_ok(S.parse text)
let catalog=flow_ok(Flow_sop.Catalog.of_factories ~version:1 Sop_catalog.Editor.factories)
let checker=Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all
let check forms=match checker catalog forms with
  |Some checked,_->checked|_,ds->failwith(String.concat "\n"(List.map Diagnostic.to_string ds))
let normalizer={|(fn [(raw : text)]
  (case (text/lower (text/trim raw)) "points" "point" "point" "point" "detail" "detail"
    :else (error "E_RULES" (str "Unknown owner: " raw))))|}
let declaration ?(hook=normalizer) ?(default=" POINTS ") ?(operation="test_encoded") ?version ()=
  "(defnode sop/test_encoded :label \"Encoded card\" :operation "^Flow.Lisp.flat(S.make(Str operation))^
  (Option.fold ~none:"" ~some:(fun version->" :version "^string_of_int version)version)^
  " [(geo : geometry) (rules : text "^Flow.Lisp.flat(S.make(Str default))^
  " :kind encoded :normalize "^hook^")]\n"^
  "(sop/with_attr geo :tag (array/text (sop/count geo :points) rules) :type text))"
let source ?hook ?default ?operation ?version ?(argument="") ()=
  "(workspace encoded "^declaration ?hook ?default ?operation ?version ()^
  " (graph g :context sop (let* [card (sop/test_encoded (sop/line :points 3) "^argument^")] card)))"
let lower forms=flow_ok(Flow_sop.Lower.workspace ~factories:Sop_catalog.Editor.factories forms)
let root lowered=
  let graph=List.find(fun(g:Flow_sop.Lower.graph)->g.default)lowered.Flow_sop.Lower.graphs in
  ok(Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root))
let field node=List.hd(Sop.Node.parameter_fields node)
let cook session domains node=match Sop.Session.cook session
  ~context:(ok(Sop.Context.create ~domains ()))node with
  |Ok output->Result.get_ok(Sop.Payload.geometry output.payload)
  |Error d->failwith(Diagnostic.error_to_string d)
let tag geometry=match Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "tag" geometry with
  |Some attr->(match Rdk.Attribute.Private.storage attr with Text values->values|_->assert false)
  |None->assert false
let refused code text=match checker catalog(parse text) with
  |None,ds when List.exists(fun(d:Diagnostic.t)->d.code=code)ds->()
  |_,ds->failwith("Expected "^code^": "^String.concat "; "(List.map Diagnostic.to_string ds))

let native_schema=
  let decode raw=match String.lowercase_ascii(String.trim raw) with
    |"points"|"point"->Ok "point"|"detail"->Ok "detail"|_->Error("Unknown owner: "^raw) in
  Param.schema ~name:"parameters" ~default:"point" [Param.field ~name:"rules"
    ~kind:(Param.encoded ~equal:String.equal ~encode:Fun.id ~decode)
    ~default:"point" ~get:Fun.id ~set:(fun value _->value)()]

let ()=
  let forms=parse(source()) in
  let checked=check forms in
  let definition=List.find(fun(n:W.node_definition)->n.kind.qualified="sop/test_encoded")checked.nodes in
  assert((List.hd definition.fields).default=Param.Text_value "point");
  assert(Flow.Lisp.flat(List.hd checked.source)=Flow.Lisp.flat(List.hd forms));
  let header=flow_ok(Flow.Defnode.parse(List.hd(parse(declaration())))) in
  let manifest,_=flow_ok(Flow_sop.Manifest.generate ~nodes:[header] Sop_catalog.Editor.factories) in
  let restored,_=flow_ok(Flow.Check.catalog_of_manifest manifest) in
  let restored_kind=List.find(fun(k:Flow.Check.kind)->k.qualified="sop/test_encoded")restored.kinds in
  assert(restored_kind.parameters=definition.kind.parameters);
  let node=root(lower forms) in
  let explicit=root(lower(parse(source ~operation:"native_encoded"()))) in
  assert(Sop.Node.operation explicit="native_encoded" && Sop.Node.label explicit="test_encoded");
  assert(Sop.Node.parameter_key explicit=Sop.Node.parameter_key node);
  assert(Sop.Node.parameter_fields explicit=Sop.Node.parameter_fields node);
  let (edited,_),phases=Flow.Phase_timer.sample ~clock:Unix.gettimeofday(fun()->
    ok(Sop.Node.apply_parameters explicit["rules",Param.Text_value " DETAIL "])) in
  assert(Flow.Phase_timer.calls phases "evaluate"=0);
  assert(Sop.Node.operation edited="native_encoded" && Sop.Node.id edited=Sop.Node.id explicit);
  assert(Sop.Node.parameter_fields node=Param.view native_schema "point");
  assert(Sop.Node.parameter_key node=Param.cook_key native_schema "point");
  let session=ok(Sop.Session.create ~max_entries:32 ~max_payload_bytes:1_000_000) in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    List.iter(fun domains->let geometry=cook session domains node in
      assert(tag geometry=[|"point";"point";"point"|]);
      assert(cook session domains node==geometry)) [1;8];
    let equivalent,effects=ok(Sop.Node.apply_parameters node["rules",Param.Text_value " POINT "]) in
    assert(equivalent==node && not effects.cook);
    let changed,effects=ok(Sop.Node.apply_parameters node["rules",Param.Text_value " DETAIL "]) in
    assert(effects.cook && Sop.Node.id changed=Sop.Node.id node);
    assert((field changed).current=Param.Text_value "detail");
    assert(Sop.Node.parameter_key changed=Param.cook_key native_schema "detail");
    List.iter(fun domains->assert(tag(cook session domains changed)=[|"detail";"detail";"detail"|])) [1;8];
    assert(Sop.Node.apply_parameters changed["rules",Param.Text_value "bad"]=Error "Unknown owner: bad");
    assert((field changed).current=Param.Text_value "detail");
    assert(tag(cook session 1 changed)=[|"detail";"detail";"detail"|]));
  (* Direct checked-workspace integrations use the same trust boundary. *)
  let raw=match W.check ~ops:Flow_sop.Operators.all catalog forms with Some w,_->w|_->assert false in
  let direct=flow_ok(Flow_sop.Lower.of_checked
    ~factories:Sop_catalog.Editor.factories raw) |>root in
  assert((field direct).default=Param.Text_value "point");
  let pane=Flow_graph.Projection.of_graph catalog checked "g" in
  let card=Option.get(Flow_graph.Projection.find pane["g";"card"]) in
  assert(definition.label="Encoded card");
  let row=List.find(fun(r:Flow_graph.Projection.row)->r.label="rules")card.rows in
  assert(row.default=Some "point");
  let edit text=Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=card.path;key=Kw "rules";sub=[];value=S.make(Str text)}) in
  let authored,changed=flow_ok(edit " DETAIL ") in
  assert(String.contains(Flow.Lisp.flat(List.hd authored)) 'D');
  let changed_node=root(flow_ok(Flow_sop.Lower.of_checked ~factories:Sop_catalog.Editor.factories changed)) in
  assert((field changed_node).current=Param.Text_value "detail");
  assert(Result.is_error(edit "bad"));
  assert((field node).current=Param.Text_value "point");
  refused "E_RULES" (source ~default:"bad"());
  refused "E_RULES" (source ~argument:":rules \"bad\""());
  refused "E_PARAM_NORMALIZE" (source ~hook:"(fn [(raw : text)] (str raw \"x\"))"());
  refused "E_SHAPE" (source ~operation:""());
  print_endline "Encoded parameters: canonical defaults/edits/cook keys, atomic refusal, authored source and projection"

let ()=
  List.iter(fun(hook,code)->refused code(source ~hook ())) [
    "(fn [(raw : int)] raw)","E_TYPE";
    "(fn [(raw : text)] 1)","E_TYPE";
    "(fn [(raw : text)] (str raw t))","E_PARAM_NORMALIZE";
    "(fn [(raw : text)] (let* [img (image/noise)] raw))","E_PARAM_NORMALIZE" ];
  refused "E_SHAPE" "(workspace bad (defnode sop/bad [(geo : geometry) (rules : text \"x\" :kind encoded)] geo))";
  refused "E_SHAPE" "(workspace bad (defnode sop/bad [(geo : geometry) (rules : text \"x\" :normalize (fn [(raw : text)] raw))] geo))";
  refused "E_PARAM_NORMALIZE" ("(workspace bad (defn impure :context value [(raw : text)] (str raw t)) "^
    declaration ~hook:"impure"()^" (graph g :context sop (sop/test_encoded nil)))");
  let pure="(workspace pure (defn canonical :context value [(raw : text)] (text/lower (text/trim raw))) "^
    declaration ~hook:"canonical"()^" (graph g :context sop (sop/test_encoded nil)))" in
  ignore(check(parse pure));
  (* The error primitive is an ordinary projected call and supplies either typed branch. *)
  ignore(check(parse "(workspace errors (graph g :context sop (if true (sop/line) (error \"E_TEST\" \"bad\"))))"));
  let forms=parse "(workspace errors (graph g :context value (let* [failure (error \"E_TEST\" \"bad\")] failure)))" in
  let checked=check forms in
  assert(Option.is_some(Flow_graph.Projection.find(Flow_graph.Projection.of_graph catalog checked "g")["g";"failure"]));
  ignore(flow_ok(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"failure"];key=Pos 1;sub=[];value=S.make(Str "changed")})));
  ignore(flow_ok(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"failure"];key=Kw "cause";sub=[];value=S.make(Str "exact cause")})));
  let forms=parse "(workspace math (graph g :context value (let* [fused (fma t 2.0 3.0)] fused)))" in
  let checked=check forms in
  assert(Option.is_some(Flow_graph.Projection.find(Flow_graph.Projection.of_graph catalog checked "g")["g";"fused"]));
  ignore(flow_ok(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"fused"];key=Pos 2;sub=[];value=S.make(Num "4.0")})));
  let forms=parse "(workspace quote (graph g :context value (let* [quoted (text/repr \"original\")] (if (equal? quoted \"\") 0.0 1.0))))" in
  let checked=check forms in
  assert(Option.is_some(Flow_graph.Projection.find(Flow_graph.Projection.of_graph catalog checked "g")["g";"quoted"]));
  ignore(flow_ok(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"quoted"];key=Pos 0;sub=[];value=S.make(Str "changed\n\195\169")})));
  let forms=parse "(workspace format (graph g :context value (let* [formatted (text/float 3.141592653589793)] 0.0)))" in
  let checked=check forms in
  let formatted ws=let result=flow_ok(Flow.Eval.static ~record:true ws) in
    snd(List.hd(List.assoc ["g";"formatted"] result.records)) in
  assert(formatted checked=Flow.Eval.Text "3.141592653589793");
  assert(Option.is_some(Flow_graph.Projection.find(Flow_graph.Projection.of_graph catalog checked "g")["g";"formatted"]));
  let _,edited=flow_ok(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"formatted"];key=Pos 0;sub=[];value=S.make(Num "0.1")})) in
  assert(formatted edited=Flow.Eval.Text "0.1");
  let rules=Param.String_table.encode [["point";"point";"*";"out";"false";"false";"boundary";
    "3.141592653589793";"false";"false";"false";""]] in
  let checked=check(parse ("(workspace tolerance (graph g :context sop (sop/group_promotions nil :rules "^
    Flow.Lisp.flat(S.make(Str rules))^")))")) in
  let evaluated=flow_ok(Flow.Eval.static checked) in
  let node=Array.find_opt(fun(n:Flow.Eval.node)->n.kind="sop/group_promotions")evaluated.plan.nodes |> Option.get in
  assert(List.assoc "rules" node.args=Flow.Eval.Text rules);
  print_endline "Encoded parameters: typed hooks and transitive pure helpers; generic error projection/edit"

let ()=
  let forms=parse(source ~argument:":rules (if (< t 0.5) \" POINT \" (if (< t 1.0) \"DETAIL\" \"bad\"))"()) in
  let lowered=lower forms in
  let graph=List.find(fun(g:Flow_sop.Lower.graph)->g.default)lowered.graphs in
  let lane=Flow_sop.Value_lane.create() in
  let resolve time=Flow_sop.Value_lane.resolve lane ~time graph.network in
  let node resolved=ok(Sop.Edit_graph.compile_node resolved.Flow_sop.Value_lane.geometry ~node_id:(Option.get graph.root)) in
  let first=flow_ok(resolve 0.0) |>node and changed=flow_ok(resolve 0.75) |>node in
  assert((field first).current=Param.Text_value "point");
  assert((field changed).current=Param.Text_value "detail");
  assert(Result.is_error(resolve 1.0));
  assert((field changed).current=Param.Text_value "detail");
  let restored=flow_ok(resolve 0.0) |>node in
  assert(Sop.Node.parameter_key restored=Sop.Node.parameter_key first);
  print_endline "Encoded parameters: live text drives canonicalize and malformed drives retain previous node"

let ()=
  let hook={|(fn [(raw : text)] (text/table_encode
    (map (fn [row] (map (fn [cell] (text/lower (text/trim cell))) row))
      (text/table_decode raw))))|} in
  let node=root(lower(parse(source ~hook ~default:" POINT \t DETAIL "()))) in
  List.iter(fun count->
    let rows=List.init count(fun _->[" POINT ";" DETAIL "]) in
    let raw=Param.String_table.encode rows in
    let expected=Param.String_table.encode(List.map(List.map(fun cell->String.lowercase_ascii(String.trim cell)))rows) in
    let node,_=ok(Sop.Node.apply_parameters node["rules",Param.Text_value raw]) in
    assert((field node).current=Param.Text_value expected)) [5001;100001];
  print_endline "Encoded parameters: native table codec parity beyond 4096 rows without a new input-size ceiling"

let () =
  let forms=parse(source ~version:2 ()) in
  let checked=check forms in
  let version (checked:W.t)=(List.find(fun(n:W.node_definition)->
    n.kind.qualified="sop/test_encoded")checked.nodes).version in
  let definition=List.find(fun(n:W.node_definition)->n.kind.qualified="sop/test_encoded")checked.nodes in
  assert(definition.version=2);
  let node=root(lower forms) and default=root(lower(parse(source()))) in
  assert(Sop.Node.version node=2 && Sop.Node.version default=1);
  assert(Sop.Node.parameter_fields node=Sop.Node.parameter_fields default);
  assert(Sop.Node.parameter_key node=Sop.Node.parameter_key default);
  let changed,_=ok(Sop.Node.apply_parameters node ["rules",Param.Text_value "detail"]) in
  assert(Sop.Node.version changed=2 && Sop.Node.id changed=Sop.Node.id node);
  assert(Sop.Node.version(Sop.Node.relabel "Visible label" changed)=2);
  let pane=Flow_graph.Projection.of_graph catalog checked "g" in
  let card=Option.get(Flow_graph.Projection.find pane["g";"card"]) in
  let saved,rechecked=flow_ok(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=card.path;key=Kw "rules";sub=[];value=S.make(Str " DETAIL ")})) in
  assert(version rechecked=2);
  let restored=check(parse(Flow.Lisp.flat(List.hd saved))) in
  assert(version restored=2);
  let edited=root(flow_ok(Flow_sop.Lower.of_checked ~factories:Sop_catalog.Editor.factories restored)) in
  assert(Sop.Node.version edited=2 && (field edited).current=Param.Text_value "detail");
  let header version=flow_ok(Flow.Defnode.parse(List.hd(parse(declaration ~version ())))) in
  let manifest version=fst(flow_ok(Flow_sop.Manifest.generate ~nodes:[header version] Sop_catalog.Editor.factories)) in
  assert(manifest 1=manifest 2); (* Runtime version is intentionally not a manifest row. *)
  let session=ok(Sop.Session.create ~max_entries:32 ~max_payload_bytes:1_000_000) in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    let geometry=cook session 1 node in
    let before=Sop.Session.stats session in
    assert(cook session 8 node==geometry && (Sop.Session.stats session).misses=before.misses);
    assert(tag(cook session 1 default)=tag geometry);
    assert(tag(cook session 8 changed)=[|"detail";"detail";"detail"|]));
  print_endline "Defnode version: runtime/rebuild/cook identity, unchanged schema/manifest, actual card gesture and saved source"
