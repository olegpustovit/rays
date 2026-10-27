open Rdk
open Rdk_test_support
module N = Sop.Node
module P = Param
let checked=function Ok x->x|Error d->fail(Diagnostic.to_string d)
let sources=Flow_sop.Catalog.sources() |>checked
let node ?observe ~reference input keywords=
  let factory=Sop.Edit_graph.factory ~key:"color_input" ~label:"Color input" ~category:["Test"] ~arity:0(fun _->input) in
  let factories=factory::List.filter(fun f->Sop.Edit_graph.factory_key f<>"set_color")Sop_catalog.Editor.factories in
  let catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories |>checked in
  let forms=Flow.Syntax.parse("(workspace color (graph g :context sop (sop/set_color (sop/color_input) "^keywords^")))") |>checked in
  let ws=match Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms with
    |Some ws,ds when not(List.exists(fun(d:Diagnostic.t)->d.severity=Diagnostic.Error_severity)ds)->ws
    |_,ds->fail(String.concat "\n"(List.map Diagnostic.to_string ds)) in
  let lowered=Flow_sop.Lower.of_checked ~reference ~factories ws |>checked in
  Option.iter(fun observe->observe lowered)observe;
  if keywords="" then begin
    let factory=List.find(fun factory->Sop.Edit_graph.factory_key factory="set_color")lowered.factories in
    check(Flow_sop.Catalog.descriptor factory=Flow_sop.Catalog.descriptor Sop.Nodes.Set_color.factory)
      "Set Color factory descriptor operation/schema differs"
  end;
  let graph=List.hd lowered.graphs in
  Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |>get_string_ok
let native input changes=
  let initial=Sop.Edit_graph.instantiate Sop.Nodes.Set_color.factory[input] |>get_string_ok in
  fst(N.apply_parameters initial changes |>get_string_ok)
let result session domains grain node=
  Sop.Session.cook session ~context:(Sop.Context.create ~domains ~grain() |>get_string_ok) node
let cook session domains grain node=match result session domains grain node with
  |Ok output->(match Sop.Payload.geometry output.payload with Ok geometry->geometry|Error e->fail e.message)
  |Error e->fail(Diagnostic.error_to_string e)
let with_session f=
  let session=Sop.Session.create ~max_entries:64 ~max_payload_bytes:512_000_000 |>get_string_ok in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->f session)
let points count=Kernel.generate_point_ranges count(fun ~first ~last ~x ~y ~z->
  for i=first to last-1 do x.(i)<-float i;y.(i)<-0.25;z.(i)<- -0.5 done)
let owners=["Point",Attribute.Point,Group.Point;"Vertex",Attribute.Vertex,Group.Vertex;
  "Primitive",Attribute.Primitive,Group.Primitive]
let count geo=function Attribute.Point->Geometry.point_count geo|Vertex->Geometry.vertex_count geo
  |Primitive->Geometry.primitive_count geo|Detail->1
let lines point_count primitive_count=
  let topology=Topology.create_owned ~point_count
    ~vertex_points:(Array.init(primitive_count*2)(fun i->i mod point_count))
    ~primitive_offsets:(Array.init(primitive_count+1)(fun i->i*2))
    ~primitive_kinds:(Array.make primitive_count Topology.Open_polyline) |>get_string_ok in
  Geometry.create ~positions:(Geometry.positions(points point_count)) ~topology() |>get_string_ok
let selection_groups geo=
  List.fold_left(fun geo(_,owner,group_owner)->List.fold_left(fun geo(name,select)->
    Geometry.with_group(Group.init ~owner:group_owner ~name(count geo owner)select)geo |>get_string_ok)
    geo ["selected",(fun i->i mod 2=0);"empty",(fun _->false);"full",(fun _->true)])geo owners
let fixture ()=
  let topology=Topology.create_owned ~point_count:6 ~vertex_points:[|0;1;2;3;1;4;5|]
    ~primitive_offsets:[|0;4;7|] ~primitive_kinds:[|Topology.Polygon;Open_polyline|] |>get_string_ok in
  let geo=Geometry.create ~positions:(Geometry.positions(points 6)) ~topology() |>get_string_ok in
  let geo=List.fold_left(fun geo(_,owner,group_owner)->
    let n=count geo owner in
    let geo=Geometry.with_group(Group.ordered ~owner:group_owner ~name:"ordered" ~length:n
      (Array.init((n+1)/2)(fun i->((n-1)/2-i)*2)) |>get_string_ok)geo |>get_string_ok in
    List.fold_left(fun geo(name,select)->Geometry.with_group
      (Group.init ~owner:group_owner ~name n select)geo |>get_string_ok) geo
      ["selected",(fun i->i mod 2=0);"empty",(fun _->false);"full",(fun _->true);"é猫\"",(fun i->i=0)]) geo owners in
  let geo=Geometry.with_group(Group.init ~owner:Group.Point ~name:"point_only" 6(fun _->true))geo |>get_string_ok in
  Geometry.with_attribute(Attribute.create_owned ~owner:Attribute.Detail ~name:"tag"
    (Attribute.Text[|"untouched ✓"|]) |>get_string_ok)geo |>get_string_ok
let with_cd geo storage=
  List.fold_left(fun geo owner->Geometry.with_attribute
    (Attribute.create_owned ~owner ~name:"Cd"(storage(count geo owner)) |>get_string_ok)geo |>get_string_ok)
    geo [Attribute.Point;Vertex;Primitive;Detail]
let existing n=Attribute.Float4(Packed.Float4.of_owned
  ~x:(Array.init n(fun i->float i *.0.125)) ~y:(Array.make n (-0.))
  ~z:(Array.make n 0.75) ~w:(Array.make n 0.5) |>get_string_ok)
let changes owner group=["owner",P.Choice_value owner;"group",P.Text_value group;
  "color_r",P.Float_value 0.2;"color_g",P.Float_value 0.4;"color_b",P.Float_value 0.6;"alpha",P.Float_value 0.8]
let quoted text=Flow.Lisp.flat(Flow.Syntax.make(Str text))
let keywords owner group=Printf.sprintf ":owner %s :group %s :color [0.2 0.4 0.6] :alpha 0.8" (quoted owner) (quoted group)
let verify ()=
  let base=fixture() in
  let grid=with_session(fun session->cook session 1 97(Lisp_sop.node "(sop/grid :columns 64 :rows 64)")) in
  check(Geometry.point_count grid=4225 && Geometry.primitive_count grid>0)"Set Color grid fixture topology";
  let large=selection_groups(lines 32769 32769) in
  let fixtures=[base;points 0;points 1;grid;points 32769;with_cd base existing;
    large;with_cd large existing;with_cd large(fun n->Attribute.Float(Array.make n 0.1));
    with_cd base(fun n->Attribute.Float(Array.make n 0.1));
    with_cd base(fun n->Attribute.Int(Array.make n 17));
    with_cd base(fun n->Attribute.Text(Array.make n "wrong"));
    with_cd base(fun n->Attribute.Float2(Packed.Float2.of_owned
      ~x:(Array.make n 1.) ~y:(Array.make n 2.) |>get_string_ok));
    with_cd base(fun n->Attribute.Float_array(Packed.Float_array.create_owned
      ~offsets:(Array.init(n+1)Fun.id) ~values:(Array.make n 1.) |>get_string_ok));
    with_cd base(fun n->Attribute.Int_array(Packed.Int_array.create_owned
      ~offsets:(Array.init(n+1)Fun.id) ~values:(Array.make n 1) |>get_string_ok));
    with_cd base(fun n->Attribute.Float3(Packed.Float3.of_owned
      ~x:(Array.make n 1.) ~y:(Array.make n 2.) ~z:(Array.make n 3.) |>get_string_ok))] in
  List.iter(fun geo->let input=Lisp_sop.snapshot geo and before=geometry_bytes geo in
    let cases=List.concat_map(fun owner->List.map(fun group->owner,group)["";" \t\r\n "])["Point";"Vertex";"Primitive";"Detail"]
      @List.concat_map(fun(owner,_,group_owner)->List.filter_map(fun group->
        if Option.is_some(Geometry.find_group ~owner:group_owner(String.trim group)geo)
        then Some(owner,group)else None)["selected";"ordered";"empty";"full";" é猫\" "])owners in
    List.iter(fun(owner,group)->
      let actual=node ~reference:false input(keywords owner group)
      and reference=node ~reference:true input(keywords owner group)
      and oracle=native input(changes owner group) in
      check(N.operation actual=N.operation oracle && N.version actual=N.version oracle)"Set Color runtime identity";
      check(N.parameter_fields actual=N.parameter_fields oracle)"Set Color parameter schema";
      check(N.parameter_key actual=N.parameter_key oracle)"Set Color parameter key";
      with_session(fun session->
        let baseline=cook session 1 97 actual in
        check(close_geometry ~ulps:4 baseline(cook session 1 97 oracle))"Set Color native geometry";
        check(geometry_bytes baseline=geometry_bytes(cook session 1 97 reference))"Set Color reference bytes";
        List.iter(fun domains->List.iter(fun grain->Sop.Session.clear session;
          check(geometry_bytes baseline=geometry_bytes(cook session domains grain actual))"Set Color domains/grain") [1;97;4096]) [1;4;8]);
      check(before=geometry_bytes geo)"Set Color mutated input")cases)fixtures;
  print_endline"Set Color draft: all owners/group modes/Cd storage, schema, native4ULP/reference/domain bytes"
let edits ()=
  let input=Lisp_sop.snapshot(fixture()) in
  let initial=node ~reference:false input "" and oracle=native input[] in
  List.iter(fun(field:P.field_view)->
    let value=match field.current with P.Choice_value _->P.Choice_value"Vertex"
      |Text_value _->P.Text_value"selected"|Float_value _->P.Float_value 0.25|_->assert false in
    let actual=fst(N.apply_parameters initial[field.name,value] |>get_string_ok)
    and expected=fst(N.apply_parameters oracle[field.name,value] |>get_string_ok) in
    check(N.parameter_key actual=N.parameter_key expected)("Set Color edited key "^field.name);
    with_session(fun session->ignore(cook session 1 97 initial);let before=Sop.Session.stats session in
      let geometry=cook session 1 97 actual in let after=Sop.Session.stats session in
      check(after.misses=before.misses+1)("Set Color public miss "^field.name);
      ignore(cook session 1 97 actual);let warm=Sop.Session.stats session in
      check(warm.misses=after.misses && warm.hits>after.hits)("Set Color warm "^field.name);
      check(close_geometry ~ulps:4 geometry(cook session 1 97 expected))("Set Color edited native "^field.name)))
    (N.parameter_fields initial);
  print_endline"Set Color draft: every physical field native key/edit + one public miss/warm hit"
let errors ()=
  let input=Lisp_sop.snapshot(fixture()) in
  List.iter(fun(owner,group)->
  let native_error=native input["owner",P.Choice_value owner;"group",P.Text_value group]
  and authored_error=node ~reference:false input (Printf.sprintf ":owner %s :group %s" (quoted owner) (quoted group)) in
  with_session(fun session->match result session 1 97 native_error,result session 1 97 authored_error with
    |Error expected,Error actual->
        check(expected.code=actual.code && expected.message=actual.message)"Set Color missing group code/message";
        check(expected.cause=Some(Printf.sprintf"could not find group %S"group))"Set Color native cause";
        check(actual.cause=expected.cause)"Set Color missing group cause"
    |_->fail"Set Color missing group accepted")) ["Point","missingé";"Vertex","point_only";"Primitive","missing"];
  let initial=node ~reference:false input "" and oracle=native input[] in
  let bad=["owner",P.Choice_value"Detail";"group",P.Text_value"selected"] in
  check(Result.is_error(N.apply_parameters initial bad) && Result.is_error(N.apply_parameters oracle bad))"Set Color detail group validation";
  check(Result.is_error(N.apply_parameters initial["owner",P.Choice_value"Edge"])
    && Result.is_error(N.apply_parameters oracle["owner",P.Choice_value"Edge"]))"Set Color owner validation";
  List.iter(fun(field:P.field_view)->if field.name<>"owner" && field.name<>"group" then
    List.iter(fun x->let changes=[field.name,P.Float_value x] in
      match N.apply_parameters initial changes,N.apply_parameters oracle changes with
      |Ok(actual,_),Ok(expected,_)->check(N.parameter_key actual=N.parameter_key expected)"Set Color hard clamp"
      |Error actual,Error expected->check(actual=expected)"Set Color channel validation diagnostic"
      |_->fail"Set Color channel validation mismatch")[-1.;2.;nan;infinity;neg_infinity]) (N.parameter_fields initial);
  List.iter(fun text->try ignore(node ~reference:false input text);fail"Set Color initial nonfinite accepted"
    with Failure message->check(not(String.starts_with ~prefix:"Set Color initial nonfinite accepted" message))
      "Set Color initial nonfinite accepted") [":color_r 1e309";":alpha -1e309"]
let projection ()=
  let module S=Flow.Syntax in
  let module Projection=Flow_graph.Projection in
  let module Edit=Flow_graph.Flow_edit in
  let input=Lisp_sop.snapshot(fixture()) in
  let factory=Sop.Edit_graph.factory ~key:"color_input" ~label:"Color input" ~category:["Test"] ~arity:0(fun _->input) in
  let factories=factory::List.filter(fun factory->Sop.Edit_graph.factory_key factory<>"set_color")Sop_catalog.Editor.factories in
  let native_factories=Sop.Nodes.Set_color.factory::factories in
  let catalog factories=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories |>checked in
  let catalog=catalog factories and native_catalog=catalog native_factories in
  let checker catalog forms=Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms in
  let workspace=function Some workspace,diagnostics when not(List.exists(fun(d:Diagnostic.t)->d.severity=Diagnostic.Error_severity)diagnostics)->workspace
    |_,diagnostics->fail(String.concat"\n"(List.map Diagnostic.to_string diagnostics)) in
  let authored=Flow.Syntax.parse{|(workspace color (graph g :context sop
    (let* [src (sop/color_input)
           card (sop/set_color src :owner "Point" :group "" :color [0.2 0.4 0.6] :alpha 0.8)] card)))|} |>checked in
  let actual=workspace(checker catalog authored)
  and expected=workspace(Flow.Workspace.check ~ops:Flow_sop.Operators.all native_catalog authored) in
  let card catalog workspace=Option.get(Projection.find(Projection.of_graph catalog workspace "g")["g";"card"]) in
  let normalize(card:Projection.node)={card with rows=List.map(fun(row:Projection.row)->
    {row with expr=Option.map(fun expr->List.hd(Flow.Syntax.parse(Flow.Lisp.flat expr)|>checked))row.expr})card.rows} in
  let actual_card=card catalog actual and expected_card=card native_catalog expected in
  check(normalize actual_card=normalize expected_card)"Set Color native/Lisp graph card metadata";
  check(Projection.lines Full(normalize actual_card)=Projection.lines Full(normalize expected_card))"Set Color inspector rows";
  let root workspace=let lowered=Flow_sop.Lower.of_checked ~factories workspace |>checked in
    let graph=List.hd lowered.graphs in
    Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)|>get_string_ok in
  List.iter(fun(key,sub,value,field,parameter)->
    let _,changed=Edit.apply_checked ~checker catalog authored
      (Set_arg{node=actual_card.path;key=Kw key;sub;value=S.make value}) |>checked in
    let row=List.find(fun(row:Projection.row)->row.key=Edit.Kw key)(card catalog changed).rows in
    check(Option.is_some row.expr)("Set Color edited row "^field);
    let actual=root changed in
    let changes=List.map(fun(name,old)->name,if name=field then parameter else old)(changes "Point" "") in
    let expected=native input changes in
    check(N.parameter_key actual=N.parameter_key expected)("Set Color gesture key "^field);
    with_session(fun session->check(close_geometry ~ulps:4(cook session 1 97 actual)(cook session 1 97 expected))
      ("Set Color gesture cook "^field)))
    ["group",[],S.Str "é猫\"","group",P.Text_value "é猫\"";
     "owner",[],S.Str "Vertex","owner",P.Choice_value "Vertex";
     "color",[0],S.Num "0.25","color_r",P.Float_value 0.25;
     "color",[1],S.Num "0.25","color_g",P.Float_value 0.25;
     "color",[2],S.Num "0.25","color_b",P.Float_value 0.25;
     "alpha",[],S.Num "0.25","alpha",P.Float_value 0.25];
  check(Result.is_error(Edit.apply_checked ~checker catalog authored
    (Set_arg{node=actual_card.path;key=Kw "alpha";sub=[];value=S.make(Num "1.5")})))"Set Color hard bound gesture";
  let _,bypassed=Edit.apply_checked ~checker catalog authored(Toggle_bypass{node=actual_card.path})|>checked in
  check((card catalog bypassed).bypass)"Set Color bypass gesture";
  print_endline"Set Color draft: native card/inspector parity and every field direct graph gesture"
let raw_cd_audit ()=
  let cases=ref 0 in
  List.iter(fun(owner,attribute_owner,_)->List.iter(fun channel->List.iter(fun index->
    List.iter(fun value->
      let geo=fixture() in
      let plane component=Array.init(count geo attribute_owner)(fun i->if component=channel && i=index then value else 0.5) in
      let data=Packed.Float4.of_owned ~x:(plane 0) ~y:(plane 1) ~z:(plane 2) ~w:(plane 3) |>get_string_ok in
      let geo=Geometry.with_attribute(Attribute.create_owned ~owner:attribute_owner ~name:"Cd"(Attribute.Float4 data)|>get_string_ok)geo |>get_string_ok in
      let input=Lisp_sop.snapshot geo in
      with_session(fun session->
        let expected=cook session 1 97(native input(changes owner "selected")) in
        let data=Geometry.find_attribute ~owner:attribute_owner "Cd" expected |>Option.get
          |>Attribute.get(Attribute.color ~owner:attribute_owner)|>Option.get|>Packed.Float4.Private.view in
        let actual=(match channel with 0->data.x|1->data.y|2->data.z|_->data.w).(index) in
        let replacement=List.nth[0.2;0.4;0.6;0.8]channel in
        check(Int64.bits_of_float actual=Int64.bits_of_float(if index=0 then replacement else value))
          "Set Color native selected/unselected raw Cd bits";
        let before=geometry_bytes geo in
        List.iter(fun reference->let authored=node ~reference input(keywords owner "selected") in
          List.iter(fun domains->List.iter(fun grain->Sop.Session.clear session;
            check(geometry_bytes(cook session domains grain authored)=geometry_bytes expected)
              "Set Color selected/unselected raw Cd native bytes") [1;97;4096]) [1;4;8]) [false;true];
        check(geometry_bytes geo=before)"Set Color raw Cd mutated input");
      incr cases)[Int64.float_of_bits 0x7ff8000000000123L;infinity;neg_infinity]) [0;1]) [0;1;2;3])owners;
  Printf.printf"Set Color raw Cd: %d selected/unselected channel cases native/reference/strict byte exact at domains1/4/8 grains1/97/4096\n%!" !cases
let trace_grouped ()=
  let previous= !(Flow_ir.Packed.Private.trace) and seen=Hashtbl.create 16 in
  let profile=ref None in
  let rec describe depth name value=
    let key=Flow.Value.key_of ~residual:Flow.Eval.Residual.residual_id value in
    Printf.printf"capture name=%s type=%s key_bytes=%d key=%s\n%!" name
      (Flow.Ty.to_string(Flow.Value.ty_of value))(String.length key)
      (String.sub key 0(min 240(String.length key)));
    if depth>0 then match value with
      |Flow.Value.Residual residual->let view=Flow.Eval.Residual.residual_view residual in
        Printf.printf"residual name=%s term=%s\n%!"name(Flow.Lisp.flat view.term.form);
        List.iter(fun(binding,value)->describe(depth-1)(name^"/"^binding)value)view.bindings
      |Struct(kind,_,fields)|Source(kind,_,fields)->
        Printf.printf"descriptor name=%s kind=%s\n%!"name kind;
        List.iter(fun(field,value)->describe(depth-1)(name^"/"^field)value)fields
      |Record fields->List.iter(fun(field,value)->describe(depth-1)(name^"/"^field)value)fields
      |_->() in
  let trace=function
    |Error diagnostics->List.iter(fun(d:Diagnostic.t)->
      let key=d.code^":"^d.message in if not(Hashtbl.mem seen key)then begin
        Hashtbl.add seen key();Printf.printf"packed refusal %s\n%!"(Diagnostic.to_string d)end)diagnostics
    |Ok (packed:Flow_ir.Packed.t)->let region=packed in
      let key=Flow.Lisp.flat region.term.form in
      if not(Hashtbl.mem seen key)then begin Hashtbl.add seen key();
        Printf.printf"packed form=%s type=%s registers=%d outputs=%d sources=[%s]\n%!"
          key(Flow.Ty.to_string region.term.ty)(Array.length region.code)(Array.length region.outputs)
          (String.concat";"(Array.to_list(Array.map(fun(_,term)->Flow.Ty.to_string term.Flow.Workspace.ty)region.sources)))end in
  Flow_ir.Packed.Private.trace:=trace;
  Fun.protect ~finally:(fun()->Flow_ir.Packed.Private.trace:=previous)(fun()->
    let geo=with_cd(selection_groups(lines 32769 32769))existing in
    let measure name f=Gc.full_major();
      let bytes=Gc.allocated_bytes() and started=Unix.gettimeofday() in
      let result=f() in
      Printf.printf"stage=%s vertex_elements=%d seconds=%.6f bytes=%.0f\n%!"
        name(Geometry.vertex_count geo)(Unix.gettimeofday()-.started)(Gc.allocated_bytes()-.bytes);
      result in
    let constructor=Option.get(Flow.Op.find "array/vec4" Flow.Context.value) in
    let values=measure "typed_constructor"(fun()->constructor.body ~live:(Input.at_time 0.)
      ~node:(fun _ _->assert false)["count",Flow.Value.Int(Geometry.vertex_count geo);
        "value",Flow.Value.Vec4(0.2,0.4,0.6,0.8)]) in
    let selection=measure "group_source"(fun()->Flow_sop.Attribute_kernel.resolve
      ~geometry:(fun _->Some geo)(Flow.Value.Source("sop/group",Array Int,
        ["geometry",Deferred(Flow.Ty.geometry,0);"name",Text "selected";"owner",Text "vertex"]))|>checked) in
    ignore(measure "selected_writer"(fun()->Flow_sop.Attribute_kernel.write_selected
      ~grain:4096 ~owner:Attribute.Vertex ~kind:"vec4" "Cd" ~values ~selection
      ~default:(Flow.Value.Vec4(1.,1.,1.,1.))geo|>checked));
    let authored=node ~observe:(fun(lowered:Flow_sop.Lower.t)->profile:=Some lowered.profile;
      Array.iter(fun(node:Flow.Eval.node)->
        Printf.printf"plan node=%d kind=%s\n%!"node.id node.kind;
        if node.kind="sop/with_attr" then
          List.iter(fun(name,value)->describe 4 name value)node.args)lowered.plan.nodes)
      ~reference:false(Lisp_sop.snapshot geo)(keywords"Vertex""selected") in
    with_session(fun session->ignore(measure "whole_cold_cook"(fun()->cook session 1 4096 authored)));
    Option.iter(fun profile->List.iter(fun(execution:Flow_ir.execution)->
      Printf.printf"execution tier=%s seconds=%.6f sites=[%s]\n%!"
        (match execution.tier with Cpu_kernel->"cpu_kernel"|Interp->"interp"|Gpu->"gpu"
          |Gpu_readback->"gpu_readback"|Core->"core"|Gpu_compile->"gpu_compile")
        execution.seconds(String.concat";"(List.map(fun(_,path,_)->String.concat"/"path)execution.sites)))
      (Flow_ir.Profile.executions profile))!profile)
let benchmark_mask ()=
  let geo=with_cd(selection_groups(lines 1_000_000 500_000))existing in
  let old=Geometry.find_attribute ~owner:Attribute.Vertex "Cd" geo |>Option.get
    |>Attribute.storage in
  let planes=match old with Attribute.Float4 data->let view=Packed.Float4.Private.view data in
    [|view.x;view.y;view.z;view.w|]|_->assert false in
  let resolve name=Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some geo)
    (Flow.Value.Source("sop/group",Array Int,["geometry",Deferred(Flow.Ty.geometry,0);
      "name",Text name;"owner",Text "vertex"])) |>checked in
  let values=Flow.Op.Private.constant_array_descriptor "array/vec4"
    ["count",Flow.Value.Int(Geometry.vertex_count geo);"value",Vec4(0.2,0.4,0.6,0.8)] in
  List.iter(fun group->let selection=resolve group in
    List.iter(fun domains->
      List.iter(fun(stage,run)->
        let samples=List.init 3(fun _->Gc.full_major();
          let bytes=Gc.allocated_bytes() and start=Unix.gettimeofday() in
          Parallel.run ~domains run;
          Unix.gettimeofday()-.start,Gc.allocated_bytes()-.bytes) in
        let seconds,bytes=List.nth(List.sort(fun(a,_)(b,_)->Float.compare a b)samples)1 in
        Printf.printf"Color_mask elements=%d group=%s domains=%d stage=%s seconds=%.6f bytes=%.0f\n%!"
          (Geometry.vertex_count geo)group domains stage seconds bytes)
        ["group_source",(fun()->ignore(resolve group));
         "selection_validation",(fun()->match selection with
           |Flow.Value.Int_array data->let length=Array.length data and i=ref 0 in
               while !i<length && (let value=data.(!i) in value=0 || value=1) do incr i done;
               check(!i=length)"selection validation"
           |_->assert false);
         "owned_plane_copy",(fun()->ignore(Array.map Array.copy planes));
         "descriptor_selected_writer",(fun()->ignore(Flow_sop.Attribute_kernel.write_selected
           ~grain:4096 ~owner:Attribute.Vertex ~kind:"vec4" "Cd" ~values ~selection
           ~default:(Flow.Value.Vec4(1.,1.,1.,1.))geo |>checked))]) [1;4])
    ["selected";"full";"empty"]
let benchmark ?(detail_only=false) ()=
  let measured=ref 0 and failures=ref [] in
  let median xs=List.sort(fun(a,_)(b,_)->Float.compare a b)xs |>fun xs->List.nth xs 1 in
  List.iter(fun(owner,attribute_owner)->
    let geo=selection_groups(match attribute_owner with
      |Attribute.Vertex->lines 1_000_000 500_000|Primitive->lines 1_000_000 1_000_000
      |Point|Detail->points 1_000_000) in
    List.iter(fun(storage,make)->let geo=match make with None->geo|Some make->
      Geometry.with_attribute(Attribute.create_owned ~owner:attribute_owner ~name:"Cd"(make(count geo attribute_owner))|>get_string_ok)geo |>get_string_ok in
    let input=Lisp_sop.snapshot geo in
    List.iter(fun group->let actual=node ~reference:false input(keywords owner group)
      and walked=node ~reference:true input(keywords owner group)
      and native=native input(changes owner group) in
      List.iter(fun domains->
        with_session(fun session->let expected=geometry_bytes(cook session domains 4096 native) in
          check(geometry_bytes(cook session domains 4096 actual)=expected &&
            geometry_bytes(cook session domains 4096 walked)=expected)
            "Set Color million-row native/reference bytes");
        let batch=if attribute_owner=Attribute.Detail then 1000 else 1 in
        let sample node=List.init 3(fun _->
          let sessions=Array.init batch(fun _->Sop.Session.create ~max_entries:64
            ~max_payload_bytes:512_000_000|>get_string_ok) in
          Fun.protect ~finally:(fun()->Array.iter Sop.Session.close sessions)(fun()->
            Gc.full_major();let bytes=Gc.allocated_bytes() and start=Unix.gettimeofday() in
            Array.iter(fun session->ignore(cook session domains 4096 node))sessions;
            (Unix.gettimeofday()-.start)/.float batch,(Gc.allocated_bytes()-.bytes)/.float batch)) |>median in
        let native_time,native_bytes=sample native and time,bytes=sample actual in
        incr measured;
        if time>2.*.native_time then failures:=(owner,storage,group,domains,time/.native_time)::!failures;
        Printf.printf"Set Color points=1000000 owner=%s elements=%d Cd=%s group=%S domains=%d grain=4096 cold_sessions=%d native=%.17g lisp=%.17g ratio=%.17g gate=%s native_bytes=%.0f lisp_bytes=%.0f\n%!"
          owner(count geo attribute_owner)storage group domains batch native_time time(time/.native_time)
          (if time<=2.*.native_time then "pass" else "fail")native_bytes bytes) [1;4])
      (if attribute_owner=Attribute.Detail then [""]else ["";"selected";"full";"empty"]))
    ["absent",None;"vec4",Some existing;"wrong_float",Some(fun n->Attribute.Float(Array.make n 0.1))])
    (if detail_only then ["Detail",Attribute.Detail] else
      ["Point",Attribute.Point;"Vertex",Attribute.Vertex;"Primitive",Attribute.Primitive;"Detail",Attribute.Detail]);
  check(!measured=(if detail_only then 6 else 78))"Set Color predetermined performance row count";
  Printf.printf"set_color_gate rows=%d failures=%d\n%!" !measured(List.length !failures);
  match List.rev !failures with []->()|(owner,storage,group,domains,ratio)::_->
    fail(Printf.sprintf"Set Color2x gate: %d failed rows; first owner=%s Cd=%s group=%S domains=%d ratio=%.17g"
      (List.length !failures)owner storage group domains ratio)
let benchmark_detail_batch ()=benchmark ~detail_only:true ()
let trace_detail ()=
  let geo=Geometry.with_attribute(Attribute.create_owned ~owner:Attribute.Detail ~name:"Cd"
    (Attribute.Float[|0.1|])|>get_string_ok)(points 3)|>get_string_ok in
  let actual=node ~reference:false(Lisp_sop.snapshot geo)(keywords "Detail" "") in
  with_session(fun session->ignore(cook session 1 4096 actual))
let benchmark_one_row_writer ()=
  let geo=selection_groups(points 1_000_000) and values=Flow.Value.Vec4_array[|0.2;0.4;0.6;0.8|] in
  List.iter(fun(storage,make)->let geo=match make with None->geo|Some make->
    Geometry.with_attribute(Attribute.create_owned ~owner:Attribute.Detail ~name:"Cd"(make 1)|>get_string_ok)geo|>get_string_ok in
    List.iter(fun domains->
      let samples=List.init 3(fun _->Parallel.run ~domains(fun()->
        Gc.full_major();let bytes=Gc.allocated_bytes() and start=Unix.gettimeofday() in
        for _=1 to 1000 do ignore(Flow_sop.Attribute_kernel.write ~grain:4096
          ~owner:Attribute.Detail ~kind:"vec4" "Cd" values geo|>checked) done;
        (Unix.gettimeofday()-.start)/.1000.,(Gc.allocated_bytes()-.bytes)/.1000.))
        |>List.sort(fun(a,_)(b,_)->Float.compare a b) in
      let seconds,bytes=List.nth samples 1 in
      Printf.printf"Color_one_row_writer points=1000000 owner=Detail Cd=%s domains=%d grain=4096 calls=1000 seconds=%.9f bytes=%.0f\n%!"
        storage domains seconds bytes)[1;4])
    ["absent",None;"vec4",Some existing;"wrong_float",Some(fun n->Attribute.Float(Array.make n 0.1))]
let benchmark_domain_defaults ()=
  List.iter(fun(label,domains)->
    List.iter(fun(name,operation)->
      operation();
      let samples=List.init 3(fun _->
        Gc.full_major();let bytes=Gc.allocated_bytes() and start=Unix.gettimeofday() in
        for _=1 to 10000 do operation() done;
        (Unix.gettimeofday()-.start)/.10000.,(Gc.allocated_bytes()-.bytes)/.10000.)
        |>List.sort(fun(a,_)(b,_)->Float.compare a b) in
      let seconds,bytes=List.nth samples 1 in
      Printf.printf "Domain_default operation=%s domains=%s calls=10000 seconds=%.12f bytes=%.0f\n%!"
        name label seconds bytes)
      ["Context.create",(fun()->ignore(Sop.Context.create ?domains() |>get_string_ok));
       "Parallel.run",(fun()->Parallel.run ?domains(fun()->()))])
    ["omitted",None;"1",Some 1;"4",Some 4]
let profile_detail ()=
  let input=Lisp_sop.snapshot(selection_groups(points 1_000_000)) in
  let actual=node ~reference:false input(keywords "Detail" "") in
  Printf.printf "Set Color Detail profile ready pid=%d: absent Cd, 1M points, domains=1 grain=4096, fresh cold Sessions\n%!"
    (Unix.getpid());
  let until=Unix.gettimeofday()+.8. and count=ref 0 in
  while Unix.gettimeofday()<until do
    with_session(fun session->ignore(cook session 1 4096 actual));
    incr count
  done;
  Printf.printf "Set Color Detail profile: %d fresh-session cooks (Session create/close included in sampled process)\n%!" !count
let ()=if Array.exists((=)"--bench-domain-defaults")Sys.argv then benchmark_domain_defaults()
  else if Array.exists((=)"--profile-detail")Sys.argv then profile_detail()
  else if Array.exists((=)"--bench")Sys.argv then benchmark()
  else if Array.exists((=)"--bench-detail-batch")Sys.argv then benchmark_detail_batch()
  else if Array.exists((=)"--trace-detail")Sys.argv then trace_detail()
  else if Array.exists((=)"--bench-one-row-writer")Sys.argv then benchmark_one_row_writer()
  else if Array.exists((=)"--bench-mask")Sys.argv then benchmark_mask()
  else if Array.exists((=)"--audit-raw-cd")Sys.argv then raw_cd_audit()
  else if Array.exists((=)"--trace-grouped")Sys.argv then trace_grouped()
  else(verify();raw_cd_audit();edits();errors();projection())
