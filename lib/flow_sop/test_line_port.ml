open Rdk
open Rdk_test_support
module N = Sop.Node
module P = Param
module S = Flow.Syntax
let checked = function Ok value->value|Error d->fail(Diagnostic.to_string d)
let sources = Flow_sop.Catalog.sources() |>checked
let factories=List.filter(fun f->Sop.Edit_graph.factory_key f<>"line")Sop_catalog.Editor.factories
let catalog fs=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version fs |>checked
let checker forms=Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all (catalog factories) forms
let workspace=function Some w,ds when not(List.exists(fun(d:Diagnostic.t)->d.severity=Diagnostic.Error_severity)ds)->w
  |_,ds->fail(String.concat "\n"(List.map Diagnostic.to_string ds))
let compile ?(reference=false) w=
  let lowered=Flow_sop.Lower.of_checked ~reference ~factories w |>checked in
  let graph=List.hd lowered.graphs in
  lowered,Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |>get_string_ok
let node ?(reference=false) keywords=
  let forms=S.parse("(workspace line_port (graph g :context sop (sop/line "^keywords^")))") |>checked in
  let lowered,node=compile ~reference(workspace(checker forms)) in
  if keywords="" then check(Flow_sop.Catalog.descriptor Sop.Nodes.Line.factory=
    Flow_sop.Catalog.descriptor(List.find(fun f->Sop.Edit_graph.factory_key f="line")lowered.factories))
    "Line complete factory descriptor";
  node
let native changes=
  let initial=Sop.Edit_graph.instantiate Sop.Nodes.Line.factory [] |>get_string_ok in
  fst(N.apply_parameters initial changes |>get_string_ok)
let session f=
  let session=Sop.Session.create ~max_entries:64 ~max_payload_bytes:512_000_000 |>get_string_ok in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->f session)
let result ?(seed=0L) ?(time=0.) ?cancel session domains grain node=
  let context=Sop.Context.create ~seed ~time ?cancel ~domains ~grain () |>get_string_ok in
  Result.bind(Sop.Session.cook session ~context node)(fun cooked->Sop.Payload.geometry cooked.payload)
let cook session domains grain node=match result session domains grain node with
  |Ok geo->geo|Error error->fail(Diagnostic.error_to_string error)
let cooked=function Ok value->value|Error error->fail(Diagnostic.error_to_string error)
let keywords changes=
  let fields=N.parameter_fields(native changes) in
  let scalar=function P.Choice_value s|Text_value s->Printf.sprintf "%S" s
    |Int_value i->string_of_int i|Float_value f->Flow.Lisp.float f|Bool_value _->assert false in
  let field name=List.find(fun(f:P.field_view)->f.name=name)fields in
  let vector name=List.init 3(fun index->
    let f=List.find(fun(f:P.field_view)->f.vec3=Some(name,index))fields in scalar f.current)
    |>String.concat " " in
  Printf.sprintf ":kind %s :points %s :origin [%s] :direction [%s] :length %s"
    (scalar(field "kind").current)(scalar(field "points").current)
    (vector "origin")(vector "direction")(scalar(field "length").current)
let f name x=name,P.Float_value x
let i n="points",P.Int_value n
let kind name="kind",P.Choice_value name
let xyz prefix (x,y,z)=[f(prefix^"_x")x;f(prefix^"_y")y;f(prefix^"_z")z]
let cases=
  ["defaults",[];"nondefault",i 11::f "length" 3.75::xyz "origin"(-2.5,1.25,-0.75)@xyz "direction"(0.25,-0.5,0.75);
   "negative_length_clamped",[f "length"(-2.)];"negative_points_clamped",[kind "Points";i(-10)];
   "zero_length",[f "length" 0.];"negative_zero_length",[f "length"(-0.)];
   "subnormal_direction",i 17::xyz "direction"(Float.next_after 0. 1.,0.,-0.);
   "huge_direction",i 100001::f "length"(sqrt 2.)::xyz "origin"(1.,2.,3.)@xyz "direction"(Float.max_float,Float.max_float,0.);
   "mixed_exponents",i 32769::xyz "direction"(Float.min_float,1.,Float.max_float);
   "signed_zero_origin",i 17::xyz "origin"(-0.,-0.,-0.)@xyz "direction"(-1.,-0.,0.);
   "finite_endpoint_cancellation",i 17::f "length"Float.max_float::xyz "origin"(Float.max_float,0.,0.)@xyz "direction"(-1.,0.,0.);
   "max_endpoint",i 17::xyz "origin"(Float.max_float,0.,0.)@xyz "direction"(0.,1.,0.);
   "tiny_length",i 17::f "length"(Float.next_after 0. 1.)::xyz "direction"(1.,1.,1.)]
  @List.concat_map(fun mode->List.map(fun n->mode^"_"^string_of_int n,[kind mode;i n])
    (if mode="Points" then [1;2;6;4225;32769] else [2;6;4225;32769]))["Polygon curve";"Points"]
let verify ()=
  List.iter(fun(label,changes)->let text=keywords changes in
    let actual=node text and walked=node ~reference:true text and expected=native changes in
    check(N.label actual=N.label expected && N.operation actual=N.operation expected && N.version actual=N.version expected)
      ("Line runtime identity "^label);
    check(N.parameter_fields actual=N.parameter_fields expected && N.parameter_key actual=N.parameter_key expected)
      ("Line schema/cook key "^label);
    check(N.inputs actual=[] && N.dependencies actual=N.dependencies expected)"Line generator acquired an input or live dependency";
    session(fun s->let baseline=geometry_bytes(cook s 1 97 expected) in
      List.iter(fun domains->List.iter(fun grain->
        Sop.Session.clear s;
        check(geometry_bytes(cook s domains grain actual)=baseline)("Line native bytes "^label);
        Sop.Session.clear s;
        check(geometry_bytes(cook s domains grain walked)=baseline)("Line reference bytes "^label))
        [1;97;4096])[1;4;8]))cases;
  ignore(node "");
  print_endline "Line both modes, one/4225/32769/100001 points, scaled direction extremes, exact native/reference/domain/grain bytes"
let errors ()=
  let zero=xyz "direction"(0.,-0.,0.) in
  let overflowing=xyz "origin"(Float.max_float,0.,0.)@xyz "direction"(1.,0.,0.)@[f "length"Float.max_float] in
  (* Every oversized native array rejects before allocating, including the
     curve's integer vertex array if OCaml evaluates that binding first. *)
  let over_capacity=max Sys.max_floatarray_length Sys.max_array_length+1 in
  let cases=["curve_minimum",[i 1];"curve_minimum_before_zero",i 1::zero;
    "zero_direction",zero;"zero_direction_points_one",kind "Points"::i 1::zero;
    "endpoint",overflowing;"unused_endpoint_points_one",kind "Points"::i 1::overflowing;
    "zero_before_endpoint",zero@xyz "origin"(Float.max_float,0.,0.)@[f "length"Float.max_float]]
    @List.concat_map(fun mode->List.map(fun(label,changes)->mode^"_"^label,kind mode::changes)
      ["over_capacity",[i over_capacity];"max_int",[i max_int];
       "zero_before_capacity",i over_capacity::zero;
       "endpoint_before_capacity",i over_capacity::overflowing;
       "zero_before_endpoint_and_capacity",i over_capacity::zero@
         xyz "origin"(Float.max_float,0.,0.)@[f "length"Float.max_float]])
      ["Polygon curve";"Points"] in
  List.iter(fun(label,changes)->session(fun s->let expected=match result s 1 97(native changes)with
    |Error error->error|Ok _->fail("Line expected native error "^label)in
    List.iter(fun reference->let actual=node ~reference(keywords changes)in
      List.iter(fun domains->List.iter(fun grain->match result s domains grain actual with
        |Error error->check(error.code=expected.code && error.message=expected.message && error.cause=expected.cause)
            ("Line exact cook diagnostic "^label)
        |Ok _->fail("Line cook error lost "^label))[1;97;4096])[1;4;8])[false;true]))cases;
  List.iter(fun changes->List.iter(fun reference->session(fun s->
    let cancel=Sop.Context.Cancel.create()in Sop.Context.Cancel.cancel cancel;
    let expected=result ~cancel s 1 97(native changes)
    and actual=result ~cancel s 1 97(node ~reference(keywords changes))in
    match actual,expected with
    |Error actual,Error expected->check(actual.code=expected.code && actual.message=expected.message && actual.cause=expected.cause)
        "Line cancellation native diagnostic"
    |_->fail "Line cancelled cook succeeded"))[false;true])
    [[i 32769];[i over_capacity];[kind "Points";i over_capacity]];
  print_endline "Line minimum/direction/endpoint/capacity precedence and cancellation preserve native code/message/cause at cook"
let edits ()=
  let initial=node "" and oracle=native []in
  List.iter(fun(field:P.field_view)->let value=match field.current with
    |P.Choice_value _->P.Choice_value "Points"|Int_value _->P.Int_value 11
    |Float_value x->P.Float_value(x+.0.25)|_->assert false in
    let edited=fst(N.apply_parameters initial[field.name,value] |>get_string_ok)
    and expected=fst(N.apply_parameters oracle[field.name,value] |>get_string_ok)in
    check(N.parameter_key edited=N.parameter_key expected)("Line edit key "^field.name);
    session(fun s->ignore(cook s 1 97 initial);let before=Sop.Session.stats s in
      let got=cook s 1 97 edited in let after=Sop.Session.stats s in
      check(after.misses=before.misses+1)("Line one public miss "^field.name);
      ignore(cook s 1 97 edited);let warm=Sop.Session.stats s in
      check(warm.misses=after.misses && warm.hits>after.hits)("Line warm hit "^field.name);
      check(geometry_bytes got=geometry_bytes(cook s 1 97 expected))("Line edited native bytes "^field.name)))
    (N.parameter_fields initial);
  List.iter(fun(field:P.field_view)->match field.kind with P.Floating_view _->
    List.iter(fun bad->let key=N.parameter_key initial and fields=N.parameter_fields initial in
      let changes=[field.name,P.Float_value 0.25;field.name,P.Float_value bad]in
      (match N.apply_parameters initial changes,N.apply_parameters oracle changes with
      |Error actual,Error expected->check(actual=expected)("Line nonfinite parameter error "^field.name)
      |_->fail("Line nonfinite parameter accepted "^field.name));
      check(N.parameter_key initial=key && N.parameter_fields initial=fields)"Line failed edit was not atomic")
      [nan;infinity;neg_infinity]|_->())(N.parameter_fields initial);
  (match N.apply_parameters initial["kind",P.Choice_value "invalid"],N.apply_parameters oracle["kind",P.Choice_value "invalid"]with
    |Error a,Error b->check(a=b)"Line invalid choice error"|_->fail "Line invalid choice accepted");
  session(fun s->let first=cook s 1 97 initial in let before=Sop.Session.stats s in
    let after=result ~seed:0x7000000000000000L ~time:9. s 4 4096 initial |>cooked in
    check(geometry_bytes first=geometry_bytes after)"Line static context changed output";
    check((Sop.Session.stats s).misses=before.misses)"Line static context invalidated cache";
    let relabeled=N.relabel "Display only ✓" initial in
    check(geometry_bytes(cook s 1 97 relabeled)=geometry_bytes first)"Line relabel changed output";
    check((Sop.Session.stats s).misses=before.misses)"Line display relabel invalidated cache");
  print_endline "Line every physical field, hard clamping, atomic nonfinite/choice edits and static/relabel cache identity"
let projection ()=
  let module Projection=Flow_graph.Projection in let module Edit=Flow_graph.Flow_edit in
  let c=catalog factories and native_c=catalog(Sop.Nodes.Line.factory::factories)in
  let forms=S.parse{|(workspace line_card (graph g :context sop (let* [card (sop/line)] card)))|} |>checked in
  let actual=workspace(checker forms)and expected=workspace(Flow.Workspace.check ~ops:Flow_sop.Operators.all native_c forms)in
  let card c w=Option.get(Projection.find(Projection.of_graph c w "g")["g";"card"])in
  let normalize(card:Projection.node)={card with rows=List.map(fun(row:Projection.row)->
    {row with expr=Option.map(fun expr->List.hd(S.parse(Flow.Lisp.flat expr)|>checked))row.expr})card.rows}in
  let actual_card=card c actual in
  check(actual.Flow.Workspace.source=forms)"Line checking embedded library definitions into authored source";
  let printed=fst(Flow.Lisp.print forms)in
  let reread=S.parse printed |>checked |>checker |>workspace |>compile |>snd in
  session(fun s->check(geometry_bytes(cook s 1 97 reread)=geometry_bytes(cook s 1 97(native [])))
    "Line source save/read changes geometry");
  check(normalize actual_card=normalize(card native_c expected))"Line native graph card";
  check(Projection.lines Full(normalize actual_card)=Projection.lines Full(normalize(card native_c expected)))"Line native inspector";
  let oracle=native []in
  List.iter(fun(field:P.field_view)->let parameter,value=match field.current with
    |P.Choice_value _->P.Choice_value "Points",S.Str "Points"|Int_value _->P.Int_value 7,S.Num "7"
    |Float_value x->P.Float_value(x+.0.25),S.Num(Flow.Lisp.float(x+.0.25))|_->assert false in
    let name,value=match field.vec3 with None->field.name,value|Some(name,index)->
      name,S.Vec(List.init 3(fun component->if component=index then S.make value else
        let unchanged=List.find(fun(f:P.field_view)->f.vec3=Some(name,component))(N.parameter_fields oracle)in
        match unchanged.current with P.Float_value x->S.make(S.Num(Flow.Lisp.float x))|_->assert false))in
    let _,changed=Edit.apply_checked ~checker:(fun _ forms->checker forms)c forms
      (Set_arg{node=actual_card.path;key=Kw name;sub=[];value=S.make value}) |>checked in
    let _,edited=compile changed in let expected=fst(N.apply_parameters oracle[field.name,parameter] |>get_string_ok)in
    check(N.parameter_key edited=N.parameter_key expected)("Line graph edit key "^field.name);
    session(fun s->check(geometry_bytes(cook s 1 97 edited)=geometry_bytes(cook s 1 97 expected))
      ("Line graph edit cook "^field.name)))(N.parameter_fields oracle);
  let bypass=Edit.Toggle_bypass{node=actual_card.path} in
  (match Edit.apply_checked ~checker:(fun _ forms->checker forms)c forms bypass,
     Edit.apply_checked ~checker:(fun _ forms->Flow.Workspace.check ~ops:Flow_sop.Operators.all native_c forms)
       native_c forms bypass with
   |Error actual,Error expected->
      check(actual.code="E_BYPASS" && actual.code=expected.code && actual.message=expected.message
        && actual.cause=expected.cause)"Line native bypass refusal"
   |_->fail "Line zero-input generator bypass was accepted");
  (match Edit.apply_checked ~checker:(fun _ forms->checker forms)c forms
    (Set_arg{node=actual_card.path;key=Kw "kind";sub=[];value=S.make(Str "invalid")})with
   |Error _->()|Ok _->fail "Line invalid graph edit accepted");
  check(fst(Flow.Lisp.print forms)=printed)"Line malformed graph edit mutated prior source";
  let starter=S.parse "(sop/line)" |>checked |>List.hd in
  let _,added=Edit.apply_checked ~checker:(fun _ forms->checker forms)c forms
    (Add_node{scope=["g"];name="new_line";expr=starter}) |>checked in
  check(Option.is_some(Projection.find(Projection.of_graph c added "g")["g";"new_line"]))
    "Line actual Add_node/menu starter missing card";
  List.iter(fun expression->let forms=S.parse("(workspace bad (graph g :context sop "^expression^"))") |>checked in
    let _,errors=checker forms in
    check(List.exists(fun(d:Diagnostic.t)->d.severity=Diagnostic.Error_severity)errors)"Line wrong input accepted")
    ["(sop/line (sop/line))";"(sop/line :origin \"wrong\")";"(sop/line :kind \"wrong\")"];
  print_endline "Line native card/inspector, every physical Flow_edit gesture, bypass and bad input/choice projection"
let live_parameters ()=
  List.iter(fun expression->List.iter(fun reference->
    let forms=S.parse(Printf.sprintf {|(workspace live_line (graph g :context sop
      (sop/line :kind "Points" :points 7 :length (+ 1.0 %s))))|}expression) |>checked in
    let lowered=Flow_sop.Lower.of_checked ~reference ~factories(workspace(checker forms)) |>checked in
    let graph=List.hd lowered.graphs in let state=Flow.Eval.create_state()in
    let lane=Flow_sop.Value_lane.create ~state ()in
    let native_factories=Sop.Nodes.Line.factory::factories in
    let native_workspace=Flow.Workspace.check ~ops:Flow_sop.Operators.all (catalog native_factories) forms |>workspace in
    let native_lowered=Flow_sop.Lower.of_checked ~reference ~factories:native_factories native_workspace |>checked in
    let native_graph=List.hd native_lowered.graphs and native_state=Flow.Eval.create_state()in
    let native_lane=Flow_sop.Value_lane.create ~state:native_state ()and accumulated=ref 0. in
    session(fun s->List.iter(fun(time,dt)->
      let live={(Input.at_time time)with dt;frame=int_of_float time;tick=int_of_float time}in
      let network=Flow_sop.Value_lane.resolve ~live lane ~time graph.network |>checked in
      let actual=Sop.Edit_graph.compile_node network.geometry ~node_id:(Option.get graph.root) |>get_string_ok in
      let stamp=Flow.Eval.state_stamp state in
      let native_network=Flow_sop.Value_lane.resolve ~live native_lane ~time native_graph.network |>checked in
      let native_node=Sop.Edit_graph.compile_node native_network.geometry ~node_id:(Option.get native_graph.root) |>get_string_ok in
      let native_stamp=Flow.Eval.state_stamp native_state in
      check(N.parameter_key actual=N.parameter_key native_node)"Line live/state native parameter key";
      accumulated:= !accumulated+.dt;
      let expected=1.+.(if expression="t"then time else !accumulated)in
      let context=Sop.Context.create ~domains:8 ~time ~input:live () |>get_string_ok in
      let actual=Sop.Session.cook s ~context actual |>cooked |>fun output->Sop.Payload.geometry output.payload |>cooked in
      let native_actual=Sop.Session.cook s ~context native_node |>cooked |>fun output->Sop.Payload.geometry output.payload |>cooked in
      check(geometry_bytes actual=geometry_bytes native_actual)"Line live/state native value-lane geometry";
      check(geometry_bytes actual=geometry_bytes(cook s 8 97(native[kind "Points";i 7;f "length"expected])))
        (Printf.sprintf "Line live/state parameter geometry differs expression=%s reference=%b time=%.17g dt=%.17g expected_length=%.17g endpoint_y=%.17g"
          expression reference time dt expected
          (let _,y,_=Packed.Float3.get(Geometry.positions actual)6 in y));
      check(Flow.Eval.state_stamp state=stamp && Flow.Eval.state_stamp native_state=native_stamp)
        "Line cook changed caller state";
      ignore(Flow_sop.Value_lane.resolve ~live lane ~time graph.network |>checked);
      check(Flow.Eval.state_stamp state=stamp)"Line repeated frame advanced caller state")
      [0.,0.25;1.,0.5;2.,0.125]))[false;true])
    ["t";"(state [previous 0.0] (+ previous (frame/dt)))"];
  print_endline "Line source-driven and retained-state parameters rebuild through the ordinary value lane"
let benchmark ()=
  let rows=["curve_default_direction",[i 1_000_000];
    "points_default_direction",[kind "Points";i 1_000_000];
    "curve_oblique",i 1_000_000::f "length" 3.75::xyz "origin"(-2.5,1.25,-0.75)@xyz "direction"(0.25,-0.5,0.75);
    "points_oblique",kind "Points"::i 1_000_000::f "length" 3.75::xyz "origin"(-2.5,1.25,-0.75)@xyz "direction"(0.25,-0.5,0.75)]in
  let failures=ref []in
  List.iter(fun(label,changes)->let actual=node(keywords changes)and expected=native changes
    and walked=node ~reference:true(keywords changes)in
    let baseline=ref None in
    List.iter(fun domains->session(fun s->let got=cook s domains 4096 actual in
      let bytes=geometry_bytes got in
      check(bytes=geometry_bytes(cook s domains 4096 expected))("Line million native bytes "^label);
      check(bytes=geometry_bytes(cook s domains 4096 walked))("Line million reference bytes "^label);
      (match !baseline with None->baseline:=Some bytes|Some before->check(bytes=before)"Line million domain bytes");
      check(Geometry.point_count got=1_000_000)"Line million count");
      let sample node=List.init 3(fun _->session(fun s->Gc.full_major();
        let before=Gc.quick_stat()and allocated=Gc.allocated_bytes()and started=Unix.gettimeofday()in
        ignore(cook s domains 4096 node);
        let elapsed=Unix.gettimeofday()-.started and after=Gc.quick_stat()in
        elapsed,Gc.allocated_bytes()-.allocated,
        after.promoted_words-.before.promoted_words,after.major_words-.before.major_words))
        |>List.sort(fun(a,_,_,_)(b,_,_,_)->Float.compare a b) |>fun xs->List.nth xs 1 in
      let nt,nb,np,nm=sample expected and lt,lb,lp,lm=sample actual in let ratio=lt/.nt in
      Printf.printf "Line points=1000000 mode=%s domains=%d grain=4096 native=%.9f lisp=%.9f ratio=%.9f native_bytes=%.0f lisp_bytes=%.0f native_promoted_words=%.0f lisp_promoted_words=%.0f native_major_words=%.0f lisp_major_words=%.0f gate=%s\n%!"
        label domains nt lt ratio nb lb np lp nm lm(if ratio<=2. then "PASS" else "FAIL");
      if ratio>2. then failures:=(label,domains,ratio)::!failures)[1;4])rows;
  if !failures<>[]then fail "Line full fixed eight-row performance gate failed"
let ()=if Array.exists((=)"--bench-line")Sys.argv then benchmark()
  else(verify();errors();edits();projection();live_parameters())
