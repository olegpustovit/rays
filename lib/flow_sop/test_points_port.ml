(* Whole bounded sop/points oracle; never delegates its authored body to Point_generate. *)
open Rdk
open Rdk_test_support
module N=Sop.Node
module P=Param
module S=Flow.Syntax
module D=Diagnostic
let checked=function Ok value->value|Error d->fail(D.to_string d)
let sources=Flow_sop.Catalog.sources() |>checked
let ()=check(not(List.exists(fun f->Sop.Edit_graph.factory_key f="points")Sop_catalog.Editor.factories))
  "Points remains registered as an OCaml body"
let factories=List.filter(fun f->Sop.Edit_graph.factory_key f<>"points")Sop_catalog.Editor.factories
let native_factories=Sop.Nodes.Point_generate.factory::factories
let catalog fs=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version fs |>checked
let checker forms=Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all(catalog factories)forms
let native_checker forms=Flow.Workspace.check ~ops:Flow_sop.Operators.all(catalog native_factories)forms
let errors ds=List.filter(fun(d:D.t)->d.severity=Diagnostic.Error_severity)ds
let workspace=function Some w,ds when errors ds=[]->w
  |_,ds->fail(String.concat "\n"(List.map D.to_string ds))
let compile ?(reference=false) w=
  let lowered=Flow_sop.Lower.of_checked ~reference ~factories w |>checked in
  let graph=List.hd lowered.graphs in
  lowered,Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |>get_string_ok
let forms keywords=S.parse("(workspace points_port (graph g :context sop (sop/points "^keywords^")))") |>checked
let node ?(reference=false) keywords=
  let lowered,node=compile ~reference(workspace(checker(forms keywords)))in
  if keywords=""then check(Flow_sop.Catalog.descriptor Sop.Nodes.Point_generate.factory=
    Flow_sop.Catalog.descriptor(List.find(fun f->Sop.Edit_graph.factory_key f="points")lowered.factories))
    "Points complete four-field descriptor";
  node
let native_initial()=Sop.Edit_graph.instantiate Sop.Nodes.Point_generate.factory[] |>get_string_ok
let native changes=fst(N.apply_parameters(native_initial())changes |>get_string_ok)
let session fn=
  let s=Sop.Session.create ~max_entries:32 ~max_payload_bytes:512_000_000 |>get_string_ok in
  Fun.protect ~finally:(fun()->Sop.Session.close s)(fun()->fn s)
let result ?(seed=0L) ?(time=0.) ?cancel s domains grain node=
  let context=Sop.Context.create ~seed ~time ?cancel ~domains ~grain() |>get_string_ok in
  Result.bind(Sop.Session.cook s ~context node)(fun cooked->
    check(cooked.instances=None)"Points acquired an instance payload";
    Sop.Payload.geometry cooked.payload)
let cooked=function Ok value->value|Error d->fail(Diagnostic.error_to_string d)
let cook s domains grain node=result s domains grain node |>cooked
let scalar=function P.Text_value text->Printf.sprintf "%S" text|Int_value n->string_of_int n|_->assert false
let keywords changes=N.parameter_fields(native changes) |>List.map(fun(field:P.field_view)->
  ":"^field.name^" "^scalar field.current) |>String.concat " "
let count n="points",P.Int_value n
let text name value=name,P.Text_value value
let group_modes=["empty","";"blank"," \t\r\n";"trim_named"," \t madeé猫 \n"]
let metadata_modes=["default",[];"custom",
  [text "source_point_attribute" " pointé ";text "source_index_attribute" " index猫 "]]
let field_text name node=match(List.find(fun(f:P.field_view)->f.name=name)(N.parameter_fields node)).current with
  |P.Text_value value->value|_->assert false
let payload label expected geometry=
  let count=match(List.find(fun(f:P.field_view)->f.name="points")(N.parameter_fields expected)).current with
    |P.Int_value n->n|_->assert false in
  check(Geometry.point_count geometry=count && Geometry.vertex_count geometry=0
    && Geometry.primitive_count geometry=0)("Points isolated-point cardinality "^label);
  let topology=Topology.Private.view(Geometry.topology geometry)in
  check(topology.vertex_points=[||] && topology.primitive_offsets=[|0|]
    && Bytes.length topology.primitive_kinds=0)("Points exact empty topology "^label);
  let positions=Packed.Float3.Private.view(Geometry.positions geometry)in
  List.iter(fun plane->Array.iter(fun value->check(Int64.bits_of_float value=0L)
    ("Points positive-zero position bits "^label))plane)[positions.x;positions.y;positions.z];
  let point_name=field_text "source_point_attribute" expected
  and index_name=field_text "source_index_attribute" expected in
  let attrs=Geometry.attributes geometry in
  check(List.map Attribute.name attrs=[point_name;index_name])("Points metadata insertion/raw names "^label);
  List.iteri(fun index attr->check(Attribute.owner attr=Attribute.Point)("Points provenance owner "^label);
    match Attribute.storage attr with
    |Attribute.Int values->check(values=Array.init count(fun point->if index=0 then -1 else point))
      ("Points exact provenance values "^label)
    |_->fail("Points provenance lost Int storage "^label))attrs;
  let group_name=String.trim(field_text "generated_group" expected)in
  (match group_name,Geometry.groups geometry with
   |"",[]->()
   |name,[group]->check(Group.owner group=Point && Group.name group=name
       && Group.length group=count && Group.cardinality group=count && not(Group.is_ordered group))
       ("Points trimmed unordered generated group "^label)
   |_->fail("Points generated-group inventory "^label));
  check(Geometry.edge_groups geometry=[])("Points unexpected edge groups "^label)
let verify_case label changes=
  let words=keywords changes in let actual=node words and walked=node ~reference:true words
  and expected=native changes in
  check(N.label actual=N.label expected && N.operation actual=N.operation expected && N.version actual=N.version expected)
    ("Points runtime identity "^label);
  check(N.parameter_fields actual=N.parameter_fields expected && N.parameter_key actual=N.parameter_key expected)
    ("Points parameter fields/key "^label);
  check(N.inputs actual=[] && N.dependencies actual=N.dependencies expected)"Points acquired an input/live dependency";
  session(fun s->let baseline=cook s 1 97 expected in payload label expected baseline;
    let bytes=geometry_bytes baseline in
    List.iter(fun domains->List.iter(fun grain->Sop.Session.clear s;
      let geometry=cook s domains grain actual in
      check(geometry_bytes geometry=bytes)("Points exact native/domain/grain "^label);
      payload label expected geometry;
      Sop.Session.clear s;check(geometry_bytes(cook s domains grain walked)=bytes)
        ("Points exact reference/domain/grain "^label)) [1;97;4096]) [1;4;8])
let verify()=
  List.iter(fun n->List.iter(fun(group_label,group)->List.iter(fun(names_label,names)->
    verify_case(Printf.sprintf "count=%d/%s/%s" n group_label names_label)
      (count n::text "generated_group" group::names))metadata_modes)group_modes)(List.init 50((+)1));
  List.iter(fun(label,changes)->verify_case label(count 7::changes))
    ["literal_wildcard_group",[text "generated_group" " *?[ab] "];
     "group_equals_attribute",[text "generated_group" "sourcepoint"];
     "group_P_is_valid",[text "generated_group" " P "];
     "raw_spaced_P_metadata",[text "source_point_attribute" " P ";text "source_index_attribute" "p"];
     "trim_equivalent_distinct_metadata",[text "source_point_attribute" "same";
       text "source_index_attribute" " same ";text "generated_group" " same "];
     "unicode_space_is_not_ascii_blank",[text "source_point_attribute" "\194\160";
       text "source_index_attribute" "\226\128\131";text "generated_group" "\194\160"];
     "swapped_metadata",[text "source_point_attribute" "sourceindex";text "source_index_attribute" "sourcepoint"];
     "escaped_utf8_metadata",[text "source_point_attribute" " a\\\"é猫\n ";text "source_index_attribute" "\t b\000 "];
     "nul_group",[text "generated_group" "\000"];
     "vertical_tab_actual_trim",[text "generated_group" "\011"];
     "form_feed_blank",[text "generated_group" "\012"]];
  ignore(node "");
  print_endline "Points all50 counts/group/name branches, exact raw metadata order and positive-zero/native/reference bytes atdomains1/4/8 grains1/97/4096"
let diagnostic_equal actual expected=actual.D.code=expected.D.code && actual.message=expected.message
  && actual.cause=expected.cause
let node_result native words=
  let parsed=forms words in let checked_workspace=(if native then native_checker else checker)parsed in
  match checked_workspace with
  |_,ds when errors ds<>[]->Error(List.hd(errors ds))
  |None,ds->Error(List.hd ds)
  |Some w,_->Result.bind(Flow_sop.Lower.of_checked ~factories:(if native then native_factories else factories)w)
      (fun lowered->let graph=List.hd lowered.graphs in
        Result.map_error(D.error ~code:"E_GRAPH")
          (Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)))
let refusals()=
  let cases=[":points 0";":points 51";":points "^string_of_int min_int;":points "^string_of_int max_int;
    ":points \"wrong\"";":points 1.5";":generated_group 1";
    ":source_point_attribute 1";":source_index_attribute false";":source_point_attribute \" \"";
    ":source_index_attribute \"\t\"";":source_point_attribute \"P\"";":source_index_attribute \"P\"";
    ":source_point_attribute \"same\" :source_index_attribute \"same\"";
    ":source_point_attribute \"P\" :source_index_attribute \"P\"";
    ":source_point_attribute \" \" :source_index_attribute \" \"";
    ":points 0 :source_point_attribute \"P\"";":unknown 1"]in
  List.iter(fun words->match node_result false words,node_result true words with
    |Error actual,Error expected->check(diagnostic_equal actual expected)("Points exact source refusal "^words)
    |_->fail("Points invalid source was accepted "^words))cases;
  List.iter(fun n->match node_result false(":points "^string_of_int n)with
    |Error d->check(d.code="E_HARD_RANGE")"Points literal hard-bound checker code"
    |Ok _->fail "Points literal hard-bound checker accepted count") [min_int;0;51;max_int];
  let actual=node "" and expected=native_initial()in
  List.iter(fun changes->let key=N.parameter_key actual and fields=N.parameter_fields actual in
    match N.apply_parameters actual changes,N.apply_parameters expected changes with
    |Error a,Error b->check(a=b)"Points exact constructor/edit name precedence";
        check(N.parameter_key actual=key && N.parameter_fields actual=fields)"Points failed edit mutated prior node"
    |_->fail "Points invalid metadata edit accepted")
    [[text "source_point_attribute" " "];[text "source_index_attribute" "P"];
     [text "source_point_attribute" "same";text "source_index_attribute" "same"];
     [text "source_point_attribute" "P";text "source_index_attribute" "P"];
     [text "source_point_attribute" "same";text "source_index_attribute" " same ";
       text "source_point_attribute" " P ";text "source_index_attribute" "P"];
     ["points",P.Text_value "wrong"];["generated_group",P.Int_value 1];
     ["source_point_attribute",P.Int_value 1];["source_index_attribute",P.Bool_value false];
     [text "source_point_attribute" "ok";text "source_point_attribute" " "]];
  (match N.apply_parameters actual["unknown",P.Text_value "ignored"],
    N.apply_parameters expected["unknown",P.Text_value "ignored"]with
   |Ok(a,ae),Ok(b,be)->check(N.parameter_key a=N.parameter_key actual
       && N.parameter_fields a=N.parameter_fields actual
       && N.parameter_key b=N.parameter_key expected
       && N.parameter_fields b=N.parameter_fields expected
       && not(P.has_effects ae) && not(P.has_effects be))"Points native unknown edit is a no-op"
   |_->fail "Points unknown editor field changed native no-op contract");
  List.iter(fun n->let changes=[count n]in
    let a=fst(N.apply_parameters actual changes |>get_string_ok)
    and b=fst(N.apply_parameters expected changes |>get_string_ok)in
    check(N.parameter_fields a=N.parameter_fields b && N.parameter_key a=N.parameter_key b)
      "Points native editor hard-bound normalization";
    session(fun s->check(geometry_bytes(cook s 1 97 a)=geometry_bytes(cook s 1 97 b))
      "Points normalized count exact native output")) [min_int;-1;0;51;max_int];
  List.iter(fun reference->session(fun s->let cancel=Sop.Context.Cancel.create()in Sop.Context.Cancel.cancel cancel;
    match result ~cancel s 1 97(node ~reference ""),result ~cancel s 1 97 expected with
    |Error a,Error b->check(a.code=b.code && a.message=b.message && a.cause=b.cause)
        "Points pre-cancelled cold cook exact native diagnostic"
    |_->fail "Points pre-cancelled cold cook succeeded")) [false;true];
  print_endline "Points literal/checker refusal vs editor clamping, exact constructor order/atomicity and pre-cancelled cold cook only"
let edit_value(field:P.field_view)=match field.current with
  |P.Int_value _->P.Int_value 7|Text_value _->P.Text_value(" edited_"^field.name^"é ")|_->assert false
let edits()=
  let actual=node "" and expected=native_initial()in
  check(List.length(N.parameter_fields actual)=4)"Points physical field count";
  List.iter(fun(field:P.field_view)->let value=edit_value field in
    let edited=fst(N.apply_parameters actual[field.name,value] |>get_string_ok)
    and oracle=fst(N.apply_parameters expected[field.name,value] |>get_string_ok)in
    check(N.parameter_key edited=N.parameter_key oracle)("Points edited key "^field.name);
    session(fun s->
      check(geometry_bytes(cook s 1 97 edited)=geometry_bytes(cook s 1 97 oracle))
        ("Points edited native bytes "^field.name);
      (* The independent oracle cook must not inflate the public miss assertion. *)
      Sop.Session.clear s;ignore(cook s 1 97 actual);let before=Sop.Session.stats s in
      ignore(cook s 1 97 edited);let after=Sop.Session.stats s in
      check(after.misses=before.misses+1)("Points exactly one public miss "^field.name);
      ignore(cook s 1 97 edited);let warm=Sop.Session.stats s in
      check(warm.misses=after.misses && warm.hits>after.hits)("Points warm hit "^field.name))) (N.parameter_fields actual);
  session(fun s->let bytes=geometry_bytes(cook s 1 97 actual)in let before=Sop.Session.stats s in
    check(geometry_bytes(result ~seed:0x7000000000000000L ~time:9. s 4 4096 actual |>cooked)=bytes)
      "Points static context changed output";
    check((Sop.Session.stats s).misses=before.misses)"Points static context invalidated cache";
    check(geometry_bytes(cook s 1 97(N.relabel "Display only ✓" actual))=bytes)"Points relabel changed output";
    check((Sop.Session.stats s).misses=before.misses)"Points relabel invalidated cache");
  let blank=fst(N.apply_parameters actual[text "generated_group" " \t "] |>get_string_ok)in
  check(N.parameter_key blank<>N.parameter_key actual)"Points ignored raw group spelling disappeared from cook key";
  let native_blank=native[text "generated_group" " \t "]in
  check(N.parameter_key blank=N.parameter_key native_blank)"Points blank-group native raw cook key";
  session(fun s->let bytes=geometry_bytes(cook s 1 97 actual)in let before=Sop.Session.stats s in
    check(geometry_bytes(cook s 1 97 blank)=bytes)"Points blank-group edit changed geometry";
    let after=Sop.Session.stats s in
    check(after.misses=before.misses+1)"Points ignored blank-group edit did not cause one public miss";
    ignore(cook s 1 97 blank);let warm=Sop.Session.stats s in
    check(warm.misses=after.misses && warm.hits>after.hits)"Points blank-group warm cache miss";
    check(geometry_bytes(cook s 1 97 native_blank)=bytes)"Points blank-group edited native bytes");
  print_endline "Points all4 physical edits/one-public-miss/warm hit, static context/relabel cache and retained blank-group key"
let projection()=
  let module Projection=Flow_graph.Projection in let module Edit=Flow_graph.Flow_edit in
  let c=catalog factories and native_c=catalog native_factories in
  let source=S.parse{|(workspace points_card (graph g :context sop (let* [card (sop/points)] card)))|} |>checked in
  let actual=workspace(checker source)and expected=workspace(native_checker source)in
  let card c w=Option.get(Projection.find(Projection.of_graph c w "g")["g";"card"])in
  let normalize(card:Projection.node)={card with rows=List.map(fun(row:Projection.row)->
    {row with expr=Option.map(fun expr->List.hd(S.parse(Flow.Lisp.flat expr)|>checked))row.expr})card.rows}in
  let actual_card=card c actual in
  check(actual.Flow.Workspace.source=source)"Points embedded helper changed authored source";
  check(normalize actual_card=normalize(card native_c expected))"Points exact native graph card";
  check(Projection.lines Full(normalize actual_card)=Projection.lines Full(normalize(card native_c expected)))
    "Points exact native inspector";
  let printed=fst(Flow.Lisp.print source)in
  let _,reread=compile(workspace(checker(S.parse printed |>checked)))in
  session(fun s->check(geometry_bytes(cook s 1 97 reread)=geometry_bytes(cook s 1 97(native_initial())))
    "Points authored save/read bytes");
  List.iter(fun(field:P.field_view)->let parameter=edit_value field in
    let value=match parameter with P.Int_value n->S.Num(string_of_int n)|Text_value s->S.Str s|_->assert false in
    let _,changed=Edit.apply_checked ~checker:(fun _ forms->checker forms)c source
      (Set_arg{node=actual_card.path;key=Kw field.name;sub=[];value=S.make value}) |>checked in
    let _,edited=compile changed in let oracle=native[field.name,parameter]in
    check(N.parameter_key edited=N.parameter_key oracle)("Points graph edit key "^field.name);
    session(fun s->check(geometry_bytes(cook s 1 97 edited)=geometry_bytes(cook s 1 97 oracle))
      ("Points graph edit exact cook "^field.name))) (N.parameter_fields(native_initial()));
  let bypass=Edit.Toggle_bypass{node=actual_card.path}in
  (match Edit.apply_checked ~checker:(fun _ forms->checker forms)c source bypass,
    Edit.apply_checked ~checker:(fun _ forms->native_checker forms)native_c source bypass with
   |Error a,Error b->check(a.code="E_BYPASS" && diagnostic_equal a b)"Points native zero-input bypass refusal"
   |_->fail "Points zero-input bypass accepted");
  (match Edit.apply_checked ~checker:(fun _ forms->checker forms)c source
    (Set_arg{node=actual_card.path;key=Kw "points";sub=[];value=S.make(Num "51")})with
   |Error d->check(d.code="E_HARD_RANGE")"Points graph hard-bound error code"
   |Ok _->fail "Points out-of-range graph edit accepted");
  check(fst(Flow.Lisp.print source)=printed)"Points failed gesture mutated source";
  let _,added=Edit.apply_checked ~checker:(fun _ forms->checker forms)c source
    (Add_node{scope=["g"];name="new_points";expr=List.hd(S.parse "(sop/points)" |>checked)}) |>checked in
  check(Option.is_some(Projection.find(Projection.of_graph c added "g")["g";"new_points"]))
    "Points actual Add_node/default menu starter";
  (match node_result false "(sop/points)",node_result true "(sop/points)"with
   |Error a,Error b->check(diagnostic_equal a b)"Points extra input refusal"|_->fail "Points acquired an input");
  print_endline "Points full native descriptor/card/inspector, all4 actual field gestures, roundtrip/menu/bypass and hard-bound refusals"
let live_parameters()=
  List.iter(fun count_expression->List.iter(fun reference->
    let source=S.parse(Printf.sprintf {|(workspace live_points (graph g :context sop
      (sop/points :points %s :generated_group (str " made_" (int t) " ")
        :source_point_attribute (str "source_" (int t)) :source_index_attribute (str "index_" (int t)))))|}
      count_expression) |>checked in
    let lowered=Flow_sop.Lower.of_checked ~reference ~factories(workspace(checker source)) |>checked
    and native_lowered=Flow_sop.Lower.of_checked ~reference ~factories:native_factories
      (workspace(native_checker source)) |>checked in
    let graph=List.hd lowered.graphs and native_graph=List.hd native_lowered.graphs
    and state=Flow.Eval.create_state()and native_state=Flow.Eval.create_state()in
    let lane=Flow_sop.Value_lane.create ~state()and native_lane=Flow_sop.Value_lane.create ~state:native_state()in
    session(fun s->List.iter(fun(time,dt)->let live={(Input.at_time time)with dt;frame=int_of_float time;tick=int_of_float time}in
      let network=Flow_sop.Value_lane.resolve ~live lane ~time graph.network |>checked
      and native_network=Flow_sop.Value_lane.resolve ~live native_lane ~time native_graph.network |>checked in
      let actual=Sop.Edit_graph.compile_node network.geometry ~node_id:(Option.get graph.root) |>get_string_ok
      and expected=Sop.Edit_graph.compile_node native_network.geometry ~node_id:(Option.get native_graph.root) |>get_string_ok in
      check(N.parameter_key actual=N.parameter_key expected && N.parameter_fields actual=N.parameter_fields expected)
        "Points Int/Text live/state parameter key";
      let stamp=Flow.Eval.state_stamp state and native_stamp=Flow.Eval.state_stamp native_state in
      let context=Sop.Context.create ~domains:8 ~grain:97 ~time ~input:live() |>get_string_ok in
      let cook node=Sop.Session.cook s ~context node |>cooked |>fun output->Sop.Payload.geometry output.payload |>cooked in
      let got=cook actual in check(geometry_bytes got=geometry_bytes(cook expected))"Points live/state native bytes";
      payload "live" expected got;
      check(Flow.Eval.state_stamp state=stamp && Flow.Eval.state_stamp native_state=native_stamp)
        "Points cook advanced caller state";
      ignore(Flow_sop.Value_lane.resolve ~live lane ~time graph.network |>checked);
      check(Flow.Eval.state_stamp state=stamp)"Points repeated frame advanced state") [0.,0.25;1.,0.5;2.,0.125])) [false;true])
    ["(int/add 7 (int t))";"(int/add -1 (int/mul 30 (int t)))";
     "(int/add 7 (state [previous 0] (int/add previous 1)))"];
  print_endline "Points ordinary Int/Text drives and retained-state value lane preserve native schema/output and caller state"
let benchmark_rows counts batch=List.concat_map(fun per_cook->List.concat_map(fun(group_label,group)->
  List.map(fun(names_label,names)->Printf.sprintf "count=%d/%s/%s" per_cook group_label names_label,
    per_cook,batch,count per_cook::text "generated_group" group::names)metadata_modes)group_modes)counts
let benchmark ~aggregate rows=
  let failures=ref [] and measured=ref 0 in
  Printf.printf "Points benchmark OCaml=%s word_size=%d table=%s rows=%d grain=4096 domains=1,4 median=3 fresh_cold_sessions native_cap=50\n%!"
    Sys.ocaml_version Sys.word_size(if aggregate then "aggregate_1M_total"else "micro") (2*List.length rows);
  List.iter(fun(label,per_cook,batch,changes)->let words=keywords changes in
    let actual=node words and expected=native changes and walked=node ~reference:true words in
    if aggregate then check(per_cook=50 && batch=20_000 && per_cook*batch=1_000_000)
      "Points aggregate applicability changed native50 cap or1M total";
    let baseline=ref None in
    List.iter(fun domains->session(fun s->let got=cook s domains 4096 actual in payload label expected got;
      let bytes=geometry_bytes got in
      check(bytes=geometry_bytes(cook s domains 4096 expected))("Points benchmark native bytes "^label);
      check(bytes=geometry_bytes(cook s domains 4096 walked))("Points benchmark reference bytes "^label);
      (match !baseline with None->baseline:=Some bytes|Some before->check(bytes=before)"Points benchmark exact domains"));
      let sample node=List.init 3(fun _->
        let sessions=Array.init batch(fun _->Sop.Session.create ~max_entries:32 ~max_payload_bytes:512_000_000 |>get_string_ok)in
        Fun.protect ~finally:(fun()->Array.iter Sop.Session.close sessions)(fun()->
          Gc.full_major();let before=Gc.quick_stat()and allocated=Gc.allocated_bytes()and started=Unix.gettimeofday()in
          Array.iter(fun s->ignore(cook s domains 4096 node))sessions;
          let elapsed=Unix.gettimeofday()-.started and after=Gc.quick_stat()in
          elapsed,Gc.allocated_bytes()-.allocated,after.promoted_words-.before.promoted_words,
          after.major_words-.before.major_words))
        |>List.sort(fun(a,_,_,_)(b,_,_,_)->Float.compare a b) |>fun samples->List.nth samples 1 in
      let nt,nb,np,nm=sample expected and lt,lb,lp,lm=sample actual in let ratio=lt/.nt in incr measured;
      Printf.printf "Points table=%s per_cook_points=%d aggregate_points=%d cold_sessions=%d mode=%s domains=%d grain=4096 native_batch_seconds=%.12f lisp_batch_seconds=%.12f native_cook_seconds=%.12f lisp_cook_seconds=%.12f ratio=%.17g native_cook_bytes=%.3f lisp_cook_bytes=%.3f native_promoted_words=%.3f lisp_promoted_words=%.3f native_major_words=%.3f lisp_major_words=%.3f gate=%s\n%!"
        (if aggregate then "aggregate_1M_total"else "micro") per_cook(per_cook*batch)batch label domains nt lt
        (nt/.float batch)(lt/.float batch)ratio(nb/.float batch)(lb/.float batch)
        (np/.float batch)(lp/.float batch)(nm/.float batch)(lm/.float batch)(if ratio<=2. then "PASS"else "FAIL");
      if not(ratio<=2.) then failures:=(label,domains,ratio)::!failures) [1;4])rows;
  let required=if aggregate then 12 else 36 in
  check(!measured=required)"Points predetermined complete performance row count";
  Printf.printf "Points benchmark terminal table=%s rows=%d failures=%d gate=%s\n%!"
    (if aggregate then "aggregate_1M_total"else "micro")!measured(List.length !failures)
    (if !failures=[]then "PASS"else "FAIL");
  if !failures<>[]then fail "Points unchanged2x complete performance gate failed"
let ()=if Array.exists((=)"--bench-points-aggregate")Sys.argv then benchmark ~aggregate:true(benchmark_rows[50]20_000)
  else if Array.exists((=)"--bench-points-micro")Sys.argv then benchmark ~aggregate:false(benchmark_rows[1;7;50]1000)
  else(verify();refusals();edits();projection();live_parameters())
