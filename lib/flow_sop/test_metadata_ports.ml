(* Independent native factories are the oracle for metadata recipes and edge ports. *)
open Rdk
open Rdk_test_support
module N = Sop.Node
module P = Param
let flow_ok = function Ok value->value|Error diagnostic->fail(Diagnostic.to_string diagnostic)
let quoted value=Flow.Lisp.flat(Flow.Syntax.make(Str value))
let sources=Flow_sop.Catalog.sources()|>flow_ok
let keys=["rename_attributes";"swap_attributes";"delete_attributes";"group_rename";"group_delete";"delete_edge_group";"rename_edge_group"]
let factories input reference =
  let snapshot key node=Sop.Edit_graph.factory ~key ~label:key ~category:["Test"] ~arity:0(fun _->node) in
  snapshot "metadata_input" input::snapshot "metadata_reference" reference::
  List.filter(fun factory->not(List.mem(Sop.Edit_graph.factory_key factory)keys))Sop_catalog.Editor.factories
let node ?(reference_node=None) ~reference input key keywords =
  let reference_input=Option.value ~default:input reference_node in
  let factories=factories input reference_input in
  let catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories |>flow_ok in
  let forms=Printf.sprintf "(workspace metadata (graph g :context sop (sop/%s (sop/metadata_input) %s)))" key keywords
    |>Flow.Syntax.parse |>flow_ok in
  let workspace=match Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms with
    |Some workspace,diagnostics when not(List.exists(fun(d:Diagnostic.t)->d.severity=Diagnostic.Error_severity)diagnostics)->workspace
    |_,diagnostics->fail(key^": "^String.concat "\n"(List.map Diagnostic.to_string diagnostics)) in
  let lowered=Flow_sop.Lower.of_checked ~reference ~factories workspace |>flow_ok in
  let graph=List.hd lowered.graphs in
  Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)|>get_string_ok
let factory = function
  |"rename_attributes"->Sop.Nodes.Rename_attributes.factory
  |"swap_attributes"->Sop.Nodes.Swap_attributes.factory
  |"delete_attributes"->Sop.Nodes.Delete_attributes.factory
  |"group_rename"->Sop.Nodes.Group_rename.factory
  |"group_delete"->Sop.Nodes.Group_delete.factory
  |"delete_edge_group"->Sop.Nodes.Delete_edge_group.factory
  |"rename_edge_group"->Sop.Nodes.Rename_edge_group.factory
  |_ ->assert false
let native ?reference input key changes =
  let inputs=input::Option.to_list reference in
  let node=(if key="delete_attributes"then Sop.Edit_graph.instantiate_optional(factory key)[Some input;reference]
    else Sop.Edit_graph.instantiate(factory key)inputs)|>get_string_ok in
  fst(N.apply_parameters node changes |>get_string_ok)
let cook session domains grain node =
  let context=Sop.Context.create ~domains ~grain ()|>get_string_ok in
  match Sop.Session.cook session ~context node with
  |Ok output->Result.map_error(fun error->error.Diagnostic.code,error.message,error.cause)(Sop.Payload.geometry output.payload)
  |Error error->Error(error.Diagnostic.code,error.message,error.cause)
let same a b=match a,b with
  |Ok a,Ok b->geometry_bytes a=geometry_bytes b
  |Error a,Error b->a=b
  |_->false
let describe=function
  |Ok geometry->"geometry:"^Digest.to_hex(Digest.string(geometry_bytes geometry))
  |Error(code,message,cause)->code^": "^message^Option.fold ~none:"" ~some:(fun cause->" cause: "^cause)cause
let points count=Kernel.generate_point_ranges count(fun ~first ~last ~x ~y ~z->
  for index=first to last-1 do x.(index)<-float index;y.(index)<- -0.;z.(index)<-0.5 done)
let grid ()=
  let size=65 in
  let vertex_points=Array.init(64*64*4)(fun index->
    let cell=index/4 and corner=index mod 4 in
    let x=cell mod 64 and y=cell/64 in
    (y+(if corner>=2 then 1 else 0))*size+x+(if corner=1||corner=2 then 1 else 0)) in
  let topology=Topology.polygons_owned ~point_count:(size*size)~vertex_points
    ~primitive_offsets:(Array.init(64*64+1)(fun index->index*4))|>get_string_ok in
  Geometry.create ~positions:(Geometry.positions(points(size*size)))~topology()|>get_string_ok
let fixture ()=
  let topology=Topology.create_owned ~point_count:6 ~vertex_points:[|0;1;2;3;1;4;5|]
    ~primitive_offsets:[|0;4;7|] ~primitive_kinds:[|Topology.Polygon;Open_polyline|]|>get_string_ok in
  let geometry=Geometry.create ~positions:(Geometry.positions(points 6))~topology()|>get_string_ok in
  let add owner count geometry =
    let array value=Array.make count value in
    let csr=Packed.Float_array.create_owned ~offsets:(Array.init(count+1)(fun i->i*2)) ~values:(Array.make(count*2)(-0.))|>get_string_ok in
    let int_csr=Packed.Int_array.create_owned ~offsets:(Array.init(count+1)(fun i->i*2)) ~values:(Array.make(count*2)max_int)|>get_string_ok in
    let float3=Packed.Float3.of_owned ~x:(array 2.) ~y:(array(-0.)) ~z:(array 0.5)|>get_string_ok in
    List.fold_left(fun geometry(name,storage)->Geometry.with_attribute
      (Attribute.create_owned ~owner ~name storage|>get_string_ok)geometry|>get_string_ok)geometry
      ["old_float",Attribute.Float(array(-0.));"old_int",Int(array max_int);
       "old_vec2",Float2(Packed.Float2.of_owned ~x:(array(-0.))~y:(array 2.)|>get_string_ok);"old_vec3",Float3 float3;
       "old_vec4",Float4(Packed.Float4.of_owned ~x:(array(-0.))~y:(array 2.)~z:(array 3.)~w:(array 4.)|>get_string_ok);"old_text",Text(array "猫\t😀");
       "old_float_array",Float_array csr;"old_int_array",Int_array int_csr] in
  let geometry=List.fold_left(fun geometry(owner,count)->add owner count geometry)geometry
    [Attribute.Point,6;Vertex,7;Primitive,2;Detail,1] in
  let group owner count name indices=Group.ordered ~owner ~length:count ~name indices|>get_string_ok in
  let geometry=List.fold_left(fun geometry group->Geometry.with_group group geometry|>get_string_ok)geometry
    [group Group.Point 6 "old_points"[|4;1;3|];group Point 6 "target"[|2;4|];group Point 6 "unused"[||];
     group Vertex 7 "old_vertices"[|6;0|];group Primitive 2 "old_primitives"[|1|]] in
  let index=Topology_index.create(Geometry.topology geometry) in
  let edges=Edge_group.init ~topology:(Geometry.topology geometry)~index ~name:"edges"(fun edge->edge<2) in
  let geometry=Geometry.with_edge_group edges geometry|>get_string_ok in
  Geometry.with_edge_group(Edge_group.with_name "old_edges" edges)geometry|>get_string_ok
let table=Param.String_table.encode
let rules rows=":rules "^quoted(table rows),["rules",P.Text_value(table rows)]
let cases =
  List.map(fun key->key,"",[])keys
  @ List.map(fun policy->let keywords,changes=rules[[" ANY ";"old_*";"new_*";String.uppercase_ascii policy]] in
      "rename_attributes",keywords,changes)["skip";"error";"overwrite"]
  @ List.concat_map(fun owner->List.map(fun method_->let keywords,changes=rules[[owner;"old_*";"new_*";method_]] in
      "swap_attributes",keywords,changes)["copy";"move";"swap"])["POINTS";"vertices";"primitive";"detail"]
  @ List.map(fun policy->let keywords,changes=rules[["*";"old_*";"new_*";policy]] in
      "group_rename",keywords,changes)["skip";"error";"overwrite";"union"]
  @ [(let keywords,changes=rules[["points";"old_points";"target";"union"]]in "group_rename",keywords,changes);
     (let keywords,changes=rules[["any";"old_*";"mid_*";"skip"];["any";"mid_*";"new_*";"overwrite"]]in "rename_attributes",keywords,changes);
     (let keywords,changes=rules[["point";"old_float";"old_int";"swap"];["point";"old_int";"old_vec2";"swap"]]in "swap_attributes",keywords,changes);
     (let keywords,changes=rules[["any";"old_*";"mid_*";"union"];["any";"mid_*";"new_*";"overwrite"]]in "group_rename",keywords,changes);
     (let keywords,changes=rules[["point";"old_*"];["vertex";"old_*"];["edge";"edges"]]in "group_delete",keywords,changes);
     (let keywords,changes=rules[["any";"old_*"]]in "group_delete",keywords^" :delete_unused true",changes@["delete_unused",P.Bool_value true]);
     "group_delete",":delete_unused true",["delete_unused",P.Bool_value true];
     "delete_attributes",":point_pattern \"old_*\" :detail_pattern \"old_text\"",["point_pattern",P.Text_value "old_*";"detail_pattern",P.Text_value "old_text"];
     "delete_attributes",":delete_non_selected true :vertex_pattern \"old_int\"",["delete_non_selected",P.Bool_value true;"vertex_pattern",P.Text_value "old_int"];
     "delete_attributes",":point_pattern \"old_int\\\\ \"",["point_pattern",P.Text_value "old_int\\ "];
     "delete_attributes",":point_pattern \"\\\\ old_int\"",["point_pattern",P.Text_value "\\ old_int"];
     (let keywords,changes=rules[["point";"[";"x";"skip"]]in "group_rename",keywords,changes);
     (let keywords,changes=rules[["point";"["]]in "group_delete",keywords,changes);
     "rename_edge_group",":from \"missing\" :into \"missing\"",["from",P.Text_value "missing";"into",P.Text_value "missing"];
     "rename_edge_group",":into \"\"",["into",P.Text_value ""]]
let verify ()=
  List.iter(fun geometry->
    let before=geometry_bytes geometry and input=Lisp_sop.snapshot geometry in
    List.iter(fun(key,keywords,changes)->
      let actual=node ~reference:false input key keywords and reference=node ~reference:true input key keywords in
      let oracle=native input key changes in
      check(N.operation actual=N.operation oracle && N.version actual=N.version oracle)(key^": operation/version differs");
      check(N.label actual=N.label oracle)(key^": runtime label differs");
      check(N.parameter_fields actual=N.parameter_fields oracle)(key^": physical schema differs");
      check(N.parameter_key actual=N.parameter_key oracle)(key^": canonical encoded key differs");
      let session=Sop.Session.create ~max_entries:64 ~max_payload_bytes:64_000_000|>get_string_ok in
      Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
        let baseline=cook session 1 97 actual in
        let expected=cook session 1 97 oracle in
        check(same baseline expected)(Printf.sprintf "%s %s points=%d: native bytes or error differs\nNative %s\nLisp %s"
          key keywords (Geometry.point_count geometry)(describe expected)(describe baseline));
        check(same baseline(cook session 1 97 reference))(key^": strict/reference differs");
        List.iter(fun domains->List.iter(fun grain->Sop.Session.clear session;
          check(same baseline(cook session domains grain actual))(key^": worker/grain differs"))[1;97;4096])[1;4;8]);
      check(before=geometry_bytes geometry)(key^": input mutated"))cases)
    [points 0;points 1;fixture();grid();points 32769];
  print_endline "metadata ports: native schema/keys/operations, all owners/storages/policies and strict/reference/domain results passed"
let edits ()=
  let input=Lisp_sop.snapshot(fixture()) in
  List.iter(fun key->let actual=node ~reference:false input key "" and oracle=native input key [] in
    List.iter(fun(field:P.field_view)->
      let value=match field.current with
        |P.Bool_value value->P.Bool_value(not value)
        |Text_value _->Text_value(if field.name="rules"then
            table(if key="group_delete"then[[" POINTS ";"old_*"]]
              else[[" POINTS ";"old_*";"new_*";if key="swap_attributes"then" COPY "else" SKIP "]])
          else if field.name="from"then"old_edges"else if field.name="into"then"new_edges"else"old_*")
        |_->assert false in
      let edited=fst(N.apply_parameters actual[field.name,value]|>get_string_ok)
      and expected=fst(N.apply_parameters oracle[field.name,value]|>get_string_ok) in
      check(N.parameter_fields edited=N.parameter_fields expected && N.parameter_key edited=N.parameter_key expected)
        (key^": edited physical schema/key differs");
      let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:16_000_000|>get_string_ok in
      Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
        ignore(cook session 1 97 actual);let before=Sop.Session.stats session in
        let result=cook session 1 97 edited in let after=Sop.Session.stats session in
        check(after.misses=before.misses+1)(key^": parameter edit must miss once");
        check(same result(cook session 1 97 expected))(key^": edited native result differs");
        let before_warm=Sop.Session.stats session in
        ignore(cook session 1 97 edited);let warm=Sop.Session.stats session in
        check(warm.misses=before_warm.misses && warm.hits>before_warm.hits)(key^": edited warm cache missed")))
      (N.parameter_fields actual))keys;
  print_endline "metadata ports: every physical edit has native canonical schema/key and one public miss"
let declaration_metadata ()=
  let definitions=List.filter_map(fun form->if Flow.Syntax.head form=Some "defnode"
    then let definition=Flow.Defnode.parse form|>flow_ok in
      if List.mem(Flow.Check.short definition.kind.qualified)keys then Some definition else None
    else None)sources in
  check(List.length definitions=List.length keys)"Every metadata kind has one embedded Lisp declaration";
  List.iter(fun(definition:Flow.Defnode.t)->
    let key=Flow.Check.short definition.kind.qualified in
    let native_factory=factory key in
    let catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:1[native_factory]|>flow_ok in
    let kind=List.hd catalog.kinds in
    check(definition.kind.slots=kind.slots && definition.kind.parameters=kind.parameters && definition.kind.outputs=kind.outputs)
      (key^": declaration slots, full field metadata or output differs");
    check(definition.operation=Sop.Edit_graph.factory_operation native_factory && definition.label=Sop.Edit_graph.factory_label native_factory
      && definition.category=Sop.Edit_graph.factory_category native_factory)(key^": public factory identity/menu differs"))definitions
let invalid_parameters ()=
  let input=Lisp_sop.snapshot(fixture()) in
  List.iter(fun key->
    let actual=node ~reference:false input key "" and oracle=native input key [] in
    let rejected raw=
      let apply node=match N.apply_parameters node["rules",P.Text_value raw]with
        |Error message->Error message|Ok _->Ok() in
      let expected=apply oracle and observed=apply actual in
      check(expected=observed && Result.is_error observed)(key^": malformed encoded parameter diagnostic differs: "^quoted raw) in
    List.iter rejected ["point";"point\twrong\\q";"point\twrong\\";
      table(if key="group_delete"then[["WHAT 猫";"*"]]else[["WHAT 猫";"*";"*";"skip"]])];
    if key<>"group_delete"then rejected(table[["point";"*";"*";"WHAT \"猫\""]]);
    if key="rename_attributes"||key="swap_attributes"then rejected(table[["point";"[";"*";if key="swap_attributes"then"swap"else"skip"]]))
    ["rename_attributes";"swap_attributes";"group_rename";"group_delete"];
  print_endline "metadata ports: malformed encoded rows, owner/policy aliases and rebuild glob diagnostics match native"
let reference_input ()=
  let geometry=fixture() and reference_geometry=points 6 in
  let input=Lisp_sop.snapshot geometry and reference=Lisp_sop.snapshot reference_geometry in
  let keywords=":reference (sop/metadata_reference) :delete_non_selected true :point_pattern \"old_*\"" in
  let actual=node ~reference_node:(Some reference)~reference:false input "delete_attributes"keywords
  and oracle=native ~reference input "delete_attributes"["delete_non_selected",P.Bool_value true;"point_pattern",P.Text_value "old_*"] in
  let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:16_000_000|>get_string_ok in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    let expected=cook session 1 97 oracle in
    List.iter(fun domains->List.iter(fun grain->Sop.Session.clear session;
      check(same expected(cook session domains grain actual))"Delete Attributes optional reference semantics differ") [1;97;4096])[1;4;8]);
  print_endline "metadata ports: Delete Attributes optional reference and inverse results match native"
let large_rules count=
  let input=Lisp_sop.snapshot(points 1) in
  List.iter(fun key->
    let row=if key="group_delete"then[" POINTS ";"missing"]
      else[" POINTS ";"missing";"replacement";if key="swap_attributes"then" COPY "else" SKIP "] in
    let raw=table(List.init count(fun _->row)) in
    let actual=node ~reference:false input key "" and oracle=native input key [] in
    let edited=fst(N.apply_parameters actual["rules",P.Text_value raw]|>get_string_ok)
    and expected=fst(N.apply_parameters oracle["rules",P.Text_value raw]|>get_string_ok) in
    check(N.parameter_fields edited=N.parameter_fields expected && N.parameter_key edited=N.parameter_key expected)
      (key^": large canonical schema/key differs");
    let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:16_000_000|>get_string_ok in
    Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
      check(same(cook session 1 97 edited)(cook session 1 97 expected))(key^": large native output differs"));
    Printf.printf "metadata ports: %s %d-row normalize/rebuild/cook matches native\n%!" key count)
    ["rename_attributes";"swap_attributes";"group_rename";"group_delete"]
let edge_cases key =
  let names=["edges";"old_edges";" edges ";"*";"é猫\"";"[";"\\";"";"absent"] in
  if key="delete_edge_group" then List.map(fun name->":name "^quoted name,["name",P.Text_value name])names
  else List.concat_map(fun from->List.map(fun into->
    ":from "^quoted from^" :into "^quoted into,["from",P.Text_value from;"into",P.Text_value into])
    ["renamed";from;"old_edges";"";" ";"é猫\""])names

let edge_exact key =
  let geometry=fixture() in
  let edge=Option.get(Geometry.find_edge_group "edges" geometry) in
  let geometry=List.fold_left(fun geometry name->
    Geometry.with_edge_group(Edge_group.with_name name edge)geometry|>get_string_ok)
    geometry [" edges ";"*";"é猫\"";"[";"\\"] in
  let old=Packed.Float3.Private.view(Geometry.positions geometry) in
  let x=Array.copy old.x in
  x.(0)<-Int64.float_of_bits 0x7ff8000000000123L;x.(1)<-infinity;x.(2)<-neg_infinity;
  let raw=Geometry.with_positions(Packed.Float3.of_owned ~x ~y:(Array.copy old.y)
    ~z:(Array.copy old.z)|>get_string_ok)geometry|>get_string_ok in
  List.iter(fun geometry->let before=geometry_bytes geometry and input=Lisp_sop.snapshot geometry in
    List.iter(fun(keywords,changes)->
      let expected=native input key changes in
      List.iter(fun reference->let actual=node ~reference input key keywords in
        let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:64_000_000|>get_string_ok in
        Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
          let expected=cook session 1 97 expected in
          List.iter(fun domains->List.iter(fun grain->Sop.Session.clear session;
            check(same expected(cook session domains grain actual))(key^": literal/native bytes/errors"))
            [1;97;4096])[1;4;8]))[false;true])
      (edge_cases key);
    check(before=geometry_bytes geometry)"Delete Edge Group input mutation")
    [geometry;raw;points 0;points 1;grid();points 32769];
  print_endline(key^": exact literal names, missing/empty/conflict, all payloads and reference/domain/grain parity")

let metadata_projection key changes =
  let module S=Flow.Syntax in
  let module Projection=Flow_graph.Projection in
  let module Edit=Flow_graph.Flow_edit in
  let input=Lisp_sop.snapshot(fixture()) in
  let factories=factories input input in
  let catalog factories=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories|>flow_ok in
  let catalog=catalog factories and native_catalog=catalog(factory key::factories) in
  let checker catalog forms=Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms in
  let workspace=function Some workspace,ds when not(List.exists(fun(d:Diagnostic.t)->d.severity=Diagnostic.Error_severity)ds)->workspace
    |_,ds->fail(String.concat "\n"(List.map Diagnostic.to_string ds)) in
  let forms=S.parse(Printf.sprintf "(workspace edges (graph g :context sop (let* [src (sop/metadata_input) card (sop/%s src)] card)))" key)|>flow_ok in
  let actual=workspace(checker catalog forms)
  and expected=workspace(Flow.Workspace.check ~ops:Flow_sop.Operators.all native_catalog forms) in
  let card catalog workspace=Option.get(Projection.find(Projection.of_graph catalog workspace "g")["g";"card"]) in
  let actual_card=card catalog actual and expected_card=card native_catalog expected in
  check(actual_card=expected_card)(key^": native card metadata");
  check(Projection.lines Full actual_card=Projection.lines Full expected_card)(key^": native inspector");
  let _,edited=List.fold_left(fun(forms,_) (field,value)->
    let value=S.make(match value with P.Text_value text->S.Str text
      |P.Bool_value value->S.Sym(string_of_bool value)|_->assert false) in
    Edit.apply_checked ~checker catalog forms
      (Set_arg{node=actual_card.path;key=Kw field;sub=[];value})|>flow_ok)
    (forms,actual)changes in
  let lowered=Flow_sop.Lower.of_checked ~factories edited|>flow_ok in
  let declared=List.find(fun factory->Sop.Edit_graph.factory_key factory=key)lowered.factories in
  check(Flow_sop.Catalog.descriptor declared=Flow_sop.Catalog.descriptor(factory key))
    (key^": complete factory descriptor");
  let graph=List.hd lowered.graphs in
  let edited=Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)|>get_string_ok in
  let expected=native input key changes in
  check(N.parameter_key edited=N.parameter_key expected)"Delete Edge Group graph parameter key";
  let _,bypassed=Edit.apply_checked ~checker catalog forms(Toggle_bypass{node=actual_card.path})|>flow_ok in
  check((card catalog bypassed).bypass)"Delete Edge Group graph bypass";
  let lowered=Flow_sop.Lower.of_checked ~factories bypassed|>flow_ok in
  let graph=List.hd lowered.graphs in
  let bypassed=Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)|>get_string_ok in
  let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:64_000_000|>get_string_ok in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    check(same(cook session 1 97 edited)(cook session 1 97 expected))"Delete Edge Group graph edit cook";
    check(same(cook session 1 97 bypassed)(cook session 1 97 input))"Delete Edge Group bypass preserves input bytes");
  print_endline(key^": native card/inspector parity, direct parameter edits and bypass gestures")

let edge_projection key =
  metadata_projection key (if key="delete_edge_group" then ["name",P.Text_value "old_edges"]
    else ["from",P.Text_value "old_edges";"into",P.Text_value "é猫\""])

let group_delete_projection () =
  metadata_projection "group_delete"
    ["rules",P.Text_value(table[[" POINTS ";"old_*"]]);"delete_unused",P.Bool_value true]

let group_delete_exact () =
  let key="group_delete" in
  let base=fixture() in
  let raw=Geometry.with_attribute(Attribute.create_owned ~owner:Point ~name:"raw"
    (Float[|nan;infinity;neg_infinity;-0.;0.;1.|])|>get_string_ok)base|>get_string_ok in
  let positions=Packed.Float3.Private.view(Geometry.positions raw) in
  let x=Array.copy positions.x in x.(0)<-nan;x.(1)<-infinity;x.(2)<-neg_infinity;
  let raw=Geometry.with_positions(Packed.Float3.of_owned ~x ~y:(Array.copy positions.y)
    ~z:(Array.copy positions.z)|>get_string_ok)raw|>get_string_ok in
  let raw=List.fold_left(fun geometry name->Geometry.with_group
    (Group.ordered ~owner:Point ~length:6 ~name[|4;1;3|]|>get_string_ok)geometry|>get_string_ok)
    raw ["é猫";"a b";"*";"[x]";"back\\slash"] in
  let configs=[]::List.concat_map(fun owner->List.map(fun pattern->[[owner;pattern]])
    ["old_*";"*";"* ^target";"missing";" \t ";"[";"\\"])
      [" POINTS ";"vertices";"primitives";"edges";"any";"*"]@
    [[["point";"old_*"];["vertex";"old_*"];["edge";"old_*"]];
     [["any";"old_*"];["any";"*"]];[["point";"["];["edge";"\\"]];
     [["edge";"\\"];["point";"["]];[["point";"é猫"]];[["point";"a\\ b"]];
     [["point";"\\*"]];[["point";"\\[x\\]"]];[["point";"back\\\\slash"]]] in
  List.iter(fun geometry->let before=geometry_bytes geometry and input=Lisp_sop.snapshot geometry in
    List.iter(fun rows->List.iter(fun unused->
      let keywords,changes=rules rows in
      let keywords=keywords^" :delete_unused "^string_of_bool unused
      and changes=changes@["delete_unused",P.Bool_value unused] in
      let expected=native input key changes in
      let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:64_000_000|>get_string_ok in
      Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
        let expected=cook session 1 97 expected in
        List.iter(fun reference->let actual=node ~reference input key keywords in
          List.iter(fun domains->List.iter(fun grain->Sop.Session.clear session;
            check(same expected(cook session domains grain actual))
              (Printf.sprintf "Group Delete native bytes/error cause: rules=%S unused=%b"(table rows)unused))
            [1;97;4096])[1;4;8])[false;true]))[false;true])configs;
    check(before=geometry_bytes geometry)"Group Delete input mutation")
    [points 0;points 1;base;raw;grid();points 32769];
  let input=Lisp_sop.snapshot base in
  let actual=node ~reference:false input key "" and expected=native input key [] in
  List.iter(fun encoded->
    let apply node=match N.apply_parameters node ["rules",P.Text_value encoded] with
      |Error message->Error message|Ok _->Ok() in
    let observed=apply actual in
    check(Result.is_error observed && observed=apply expected)
      "Group Delete encoded constructor diagnostics")
    ["point";"point\twrong\\q";"point\twrong\\";table[["WHAT 猫";"*"]];
      table[["point";"["];["WHAT 猫";"*"]]];
  print_endline "Group Delete: all owners, patterns, unused/order/raw payload and first-error parity"

let rename_attributes_cases =
  [] :: List.concat_map(fun owner->List.concat_map(fun policy->
    List.map(fun(pattern,replacement)->[[owner;pattern;replacement;policy]])
      ["old_*","new_*";"old_*","old_*";"missing","new";
       "old_float","old_int";"old_float","\\ ";"old_float","\\\t";
       "old_float","P";"P","renamed_P";"old_?nt","new_?nt";
       "old_[fi]*","new_[fi]*"])
    ["skip";"error";"overwrite"])["point";"vertex";"primitive";"detail";" ANY ";"*"]
  @ [[["any";"old_*";"mid_*";"skip"];["any";"mid_*";"new_*";"overwrite"]];
     [["point";"old_float";"old_int";"overwrite"];["point";"old_int";"old_vec2";"overwrite"]];
     [["point";"old_int";"old_float";"overwrite"];["point";"old_float";"new_float";"skip"]];
     [["points";"old_float";"é猫";" SKIP "];["vertices";"old_float";"é猫";" ERROR "]]]

let rename_attributes_exact () =
  let key="rename_attributes" in
  let base=fixture()|>fun geometry->Geometry.with_attribute
    (Attribute.create_owned ~owner:Vertex ~name:"P"(Float(Array.make 7(-0.)))|>get_string_ok)geometry|>get_string_ok in
  let raw=Geometry.with_attribute(Attribute.create_owned ~owner:Point ~name:"raw"
    (Float [|nan;infinity;neg_infinity;-0.;0.;1.|])|>get_string_ok)base|>get_string_ok in
  let raw=List.fold_left(fun geometry name->Geometry.with_attribute
    (Attribute.create_owned ~owner:Detail ~name (Text[|"猫\000\255"|])|>get_string_ok)geometry|>get_string_ok)
    raw ["é猫";"a-b";"back\\slash";"a b";"[x]";"*"] in
  let positions=Packed.Float3.Private.view(Geometry.positions raw) in
  let x=Array.copy positions.x in x.(0)<-nan;x.(1)<-infinity;x.(2)<-neg_infinity;
  let raw=Geometry.with_positions(Packed.Float3.of_owned ~x ~y:(Array.copy positions.y)
    ~z:(Array.copy positions.z)|>get_string_ok)raw|>get_string_ok in
  let literal_cases=List.map(fun pattern->[["detail";pattern;"renamed";"skip"]])
    ["é猫";"a-b";"back\\\\slash";"a\\ b";"\\[x\\]";"\\*"] in
  List.iter(fun geometry->let before=geometry_bytes geometry and input=Lisp_sop.snapshot geometry in
    List.iter(fun rows->let keywords,changes=rules rows in
      let actual=node ~reference:false input key keywords and walked=node ~reference:true input key keywords
      and expected=native input key changes in
      check(N.operation actual=N.operation expected && N.version actual=N.version expected
        && N.label actual=N.label expected && N.parameter_fields actual=N.parameter_fields expected
        && N.parameter_key actual=N.parameter_key expected)"Rename Attributes identity/schema/canonical key";
      let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:64_000_000|>get_string_ok in
      Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
        let baseline=cook session 1 97 expected in
        List.iter(fun domains->List.iter(fun grain->Sop.Session.clear session;
          let observed=cook session domains grain actual in
          check(same baseline observed)("Rename Attributes native ordered payload/error mismatch "^keywords^
            "\nNative "^describe baseline^"\nLisp "^describe observed);
          (match observed with Error _->()|Ok output->
            check(Packed.Float3.data_id(Geometry.positions output)=Packed.Float3.data_id(Geometry.positions geometry)
              && Topology.data_id(Geometry.topology output)=Topology.data_id(Geometry.topology geometry))
              "Rename Attributes borrowed positions/topology ownership");
          check(same baseline(cook session domains grain walked))"Rename Attributes reference bytes/errors")
          [1;97;4096])[1;4;8]))(rename_attributes_cases@literal_cases);
    check(before=geometry_bytes geometry)"Rename Attributes input mutation")
    [points 0;points 1;grid();base;raw;points 32769];
  let input=Lisp_sop.snapshot base in
  let actual=node ~reference:false input key "" and expected=native input key [] in
  List.iter(fun raw->let apply node=match N.apply_parameters node["rules",P.Text_value raw]with
      |Error message->Error message|Ok _->Ok() in
    check(apply actual=apply expected && Result.is_error(apply actual))
      ("Rename Attributes rebuild refusal "^quoted raw))
    ["point";"point\twrong\\q";"point\twrong\\";
     table[["BADé猫\"";"*";"*";"skip"]];table[["point";"*";"*";"BADé猫\""]];
     table[["point";"[";"x";"skip"]];table[["point";"\\";"x";"skip"]];
     table[["point";"*";"x";"skip"]];table[["point";"old_float";"*";"skip"]];
     table[["point";"old_float";"";"skip"]];table[["point";"old_* ^old_int";"new_*";"skip"]];
     table[["point";"old_float";"old_int";"error"];["point";"[";"new";"skip"]]];
  List.iter(fun value->let apply node=match N.apply_parameters node["rules",value]with
      |Error message->Error message|Ok _->Ok() in
    check(apply actual=apply expected && Result.is_error(apply actual))"Rename Attributes wrong parameter refusal")
    [P.Bool_value true;P.Int_value 17;P.Float_value nan;P.Float_value infinity];
  let native_catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version
    (factory key::factories input input)|>flow_ok
  and catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version
    (factories input input)|>flow_ok in
  List.iter(fun expression->let forms=Flow.Syntax.parse(Printf.sprintf
      "(workspace invalid (graph g :context sop (sop/rename_attributes %s)))"expression)|>flow_ok in
    let errors=function None,ds->List.filter_map(fun(d:Diagnostic.t)->if d.severity=Diagnostic.Error_severity then Some d.code else None)ds
      |_->fail "Rename Attributes wrong input accepted" in
    let expected=errors(Flow.Workspace.check ~ops:Flow_sop.Operators.all native_catalog forms)
    and actual=errors(Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms) in
    check(actual=expected && List.mem "E_TYPE" actual)"Rename Attributes wrong input error code")
    ["17";"(image/noise :width 1 :height 1)"];
  let edited_changes=["rules",P.Text_value(table[[" POINTS ";"old_*";"new_*";" SKIP "]])] in
  let edited=fst(N.apply_parameters actual edited_changes|>get_string_ok)
  and oracle=fst(N.apply_parameters expected edited_changes|>get_string_ok) in
  check(N.parameter_fields edited=N.parameter_fields oracle && N.parameter_key edited=N.parameter_key oracle)
    "Rename Attributes edited canonical fields/key";
  let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:64_000_000|>get_string_ok in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    ignore(cook session 1 97 actual);let before=Sop.Session.stats session in
    let result=cook session 1 97 edited in let after=Sop.Session.stats session in
    check(after.misses=before.misses+1 && same result(cook session 1 97 oracle))
      "Rename Attributes edit one public miss/native bytes";
    let before=Sop.Session.stats session in ignore(cook session 1 97 edited);let after=Sop.Session.stats session in
    check(after.misses=before.misses && after.hits>before.hits)"Rename Attributes warm cache";
    List.iter(fun label->let relabeled=N.relabel label edited in
      check(N.id relabeled=N.id edited && N.parameter_key relabeled=N.parameter_key edited
        && same result(cook session 1 97 relabeled))"Rename Attributes relabel identity/cache") ["Renamed";"é猫"]);
  print_endline "Rename Attributes: all owners/policies/storage/order/raw bytes, errors, edits and domain/grain proof"

let rename_attributes_projection () =
  metadata_projection "rename_attributes"
    ["rules",P.Text_value(table[[" ANY ";"old_*";"new_*";" OVERWRITE "]])]

let group_rename_fixture () =
  let geometry=fixture() in
  let geometry=List.fold_left(fun geometry(owner,count,source,selected,target)->
    Geometry.with_group(Group.ordered ~owner ~name:"old" ~length:count source|>get_string_ok)geometry|>get_string_ok
    |>fun geometry->Geometry.with_group(Group.ordered ~owner ~name:"target" ~length:count target|>get_string_ok)geometry|>get_string_ok
    |>fun geometry->Geometry.with_group(Group.init ~owner ~name:"unordered" count selected)geometry|>get_string_ok)
    geometry [Group.Point,6,[|4;1;3|],(fun i->i mod 2=0),[|2;4|];
      Vertex,7,[|6;0|],(fun i->i mod 2=0),[|0;4|];
      Primitive,2,[|1|],(fun i->i=1),[|0|]] in
  let topology=Geometry.topology geometry in let index=Topology_index.create topology in
  let geometry=List.fold_left(fun geometry(name,selected)->Geometry.with_edge_group
    (Edge_group.init ~topology ~index ~name selected)geometry|>get_string_ok)
    geometry ["old",(fun i->i<2);"target",(fun i->i=1||i=2)] in
  List.fold_left(fun geometry name->
    Geometry.with_group(Group.ordered ~owner:Point ~name ~length:6[|4;1|]|>get_string_ok)geometry|>get_string_ok)
    geometry ["é猫";"a-b";"back\\slash";"a b";"[x]";"*"]

let group_rename_cases =
  []::List.concat_map(fun owner->List.concat_map(fun policy->
    List.map(fun(pattern,replacement)->[[owner;pattern;replacement;policy]])
      ["old_*","new_*";"old","target";"unordered","target";"old","old";
       "missing","new";"old","\\ ";"[","new";"\\","new";
       "old*","new";"old","new*"])
    ["skip";"error";"overwrite";"union"])["point";"vertex";"primitive";"edge";" ANY ";"*"]@
    [[["any";"old_*";"mid_*";"union"];["any";"mid_*";"new_*";"overwrite"]];
     [["any";"old";"target";"union"];["any";"unordered";"target";"union"]];
     [["any";"old";"target";"overwrite"];["any";"target";"new";"overwrite"]];
     [["point";"old";"target";"error"];["edge";"[";"new";"skip"]];
     [["edge";"[";"new";"skip"];["point";"old";"target";"error"]]]@
    List.map(fun pattern->[["point";pattern;"renamed";"skip"]])
      ["é猫";"a-b";"back\\\\slash";"a\\ b";"\\[x\\]";"\\*"]

let group_rename_exact () =
  let key="group_rename" and base=group_rename_fixture() in
  let positions=Packed.Float3.Private.view(Geometry.positions base) in
  let x=Array.copy positions.x in x.(0)<-nan;x.(1)<-infinity;x.(2)<-neg_infinity;
  let raw=Geometry.with_positions(Packed.Float3.of_owned ~x ~y:(Array.copy positions.y)
    ~z:(Array.copy positions.z)|>get_string_ok)base|>get_string_ok in
  let raw=Geometry.with_attribute(Attribute.create_owned ~owner:Point ~name:"raw"
    (Float[|nan;infinity;neg_infinity;-0.;0.;1.|])|>get_string_ok)raw|>get_string_ok in
  List.iter(fun geometry->let before=geometry_bytes geometry and input=Lisp_sop.snapshot geometry in
    List.iter(fun rows->let keywords,changes=rules rows in
      let actual=node ~reference:false input key keywords and walked=node ~reference:true input key keywords
      and expected=native input key changes in
      check(N.operation actual=N.operation expected && N.version actual=N.version expected
        && N.label actual=N.label expected && N.parameter_fields actual=N.parameter_fields expected
        && N.parameter_key actual=N.parameter_key expected)"Group Rename native identity/schema/key";
      let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:64_000_000|>get_string_ok in
      Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
        let baseline=cook session 1 97 expected in
        List.iter(fun domains->List.iter(fun grain->Sop.Session.clear session;
          let observed=cook session domains grain actual in
          check(same baseline observed)("Group Rename ordered native bytes/cause "^keywords^
            "\nNative "^describe baseline^"\nLisp "^describe observed);
          check(same baseline(cook session domains grain walked))"Group Rename strict/reference bytes/cause")
          [1;97;4096])[1;4;8]))group_rename_cases;
    check(before=geometry_bytes geometry)"Group Rename input mutation")
    [points 0;points 1;grid();base;raw;points 32769];
  let input=Lisp_sop.snapshot base in
  let actual=node ~reference:false input key "" and expected=native input key [] in
  List.iter(fun raw->let apply node=match N.apply_parameters node["rules",P.Text_value raw]with
      |Error message->Error message|Ok _->Ok() in
    check(apply actual=apply expected && Result.is_error(apply actual))"Group Rename encoded constructor refusal")
    ["point";"point\twrong\\q";"point\twrong\\";
      table[["BADé猫\"";"*";"*";"union"]];table[["point";"*";"*";"BADé猫\""]];
      table[["point";"old";"target";"error"];["BADé猫\"";"old";"new";"skip"]]];
  List.iter(fun value->let apply node=match N.apply_parameters node["rules",value]with
      |Error message->Error message|Ok _->Ok() in
    check(apply actual=apply expected && Result.is_error(apply actual))"Group Rename wrong parameter refusal")
    [P.Bool_value true;P.Int_value 17;P.Float_value nan;P.Float_value infinity];
  let native_catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version
    (factory key::factories input input)|>flow_ok
  and catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version
    (factories input input)|>flow_ok in
  List.iter(fun expression->let forms=Flow.Syntax.parse(Printf.sprintf
      "(workspace invalid (graph g :context sop (sop/group_rename %s)))"expression)|>flow_ok in
    let errors=function None,ds->List.filter_map(fun(d:Diagnostic.t)->if d.severity=Diagnostic.Error_severity then Some d.code else None)ds
      |_->fail "Group Rename wrong input accepted" in
    let expected=errors(Flow.Workspace.check ~ops:Flow_sop.Operators.all native_catalog forms)
    and actual=errors(Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms) in
    check(actual=expected && List.mem "E_TYPE" actual)"Group Rename wrong input error code")
    ["17";"(image/noise :width 1 :height 1)"];
  List.iter(fun rows->let value=P.Text_value(table rows) in
    check(Result.is_ok(N.apply_parameters actual["rules",value])
      && Result.is_ok(N.apply_parameters expected["rules",value]))
      "Group Rename glob failure must remain a cook refusal")
    [[["point";"[";"new";"union"]];[["edge";"\\";"new";"error"]]];
  let changes=["rules",P.Text_value(table[[" ANY ";"old";"target";" UNION "]])] in
  let edited=fst(N.apply_parameters actual changes|>get_string_ok)
  and oracle=fst(N.apply_parameters expected changes|>get_string_ok) in
  check(N.parameter_fields edited=N.parameter_fields oracle && N.parameter_key edited=N.parameter_key oracle)
    "Group Rename canonical edit schema/key";
  let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:64_000_000|>get_string_ok in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    ignore(cook session 1 97 actual);let before=Sop.Session.stats session in
    let observed=cook session 1 97 edited in let after=Sop.Session.stats session in
    check(after.misses=before.misses+1 && same observed(cook session 1 97 oracle))
      "Group Rename one public edit miss and native bytes";
    (match observed with Error _->assert false|Ok geometry->
      check(Group.ordered_elements(Option.get(Geometry.find_group ~owner:Point "target" geometry))=Some[|2;4;1;3|])
        "Group Rename union uses destination-first explicit order");
    let before=Sop.Session.stats session in ignore(cook session 1 97 edited);let after=Sop.Session.stats session in
    check(after.misses=before.misses && after.hits>before.hits)"Group Rename warm hit";
    List.iter(fun label->let relabeled=N.relabel label edited in
      check(N.id relabeled=N.id edited && N.parameter_key relabeled=N.parameter_key edited
        && same observed(cook session 1 97 relabeled))"Group Rename relabel identity/cook") ["Renamed";"é猫"]);
  print_endline "Group Rename all owner/policy/ordered union/edge/raw/error/schema/cache/domain proof"

let group_rename_projection () =
  metadata_projection "group_rename"
    ["rules",P.Text_value(table[[" POINTS ";"old_points";"target";" UNION "]])]

let delete_attributes_projection () =
  let module S=Flow.Syntax in
  let module Projection=Flow_graph.Projection in
  let module Edit=Flow_graph.Flow_edit in
  let input=Lisp_sop.snapshot(fixture()) in
  let factories=factories input input in
  let catalog factories=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories|>flow_ok in
  let catalog=catalog factories and native_catalog=catalog(factory "delete_attributes"::factories) in
  let checker catalog forms=Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms in
  let workspace=function Some workspace,ds when not(List.exists(fun(d:Diagnostic.t)->d.severity=Diagnostic.Error_severity)ds)->workspace
    |_,ds->fail(String.concat "\n"(List.map Diagnostic.to_string ds)) in
  let forms=S.parse {|(workspace deletion (graph g :context sop
    (let* [src (sop/metadata_input) reference_source (sop/metadata_reference)
      card (sop/delete_attributes src)] card)))|}|>flow_ok in
  let actual=workspace(checker catalog forms)
  and expected=workspace(Flow.Workspace.check ~ops:Flow_sop.Operators.all native_catalog forms) in
  let card catalog workspace=Option.get(Projection.find(Projection.of_graph catalog workspace "g")["g";"card"]) in
  let actual_card=card catalog actual and expected_card=card native_catalog expected in
  check(actual_card=expected_card && Projection.lines Full actual_card=Projection.lines Full expected_card)
    "Delete Attributes native card/inspector parity";
  let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:64_000_000|>get_string_ok in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    let compile workspace=let lowered=Flow_sop.Lower.of_checked ~factories workspace|>flow_ok in
      let graph=List.hd lowered.graphs in
      Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)|>get_string_ok in
    let edited_forms,edited=List.fold_left(fun(forms,_) (field,value)->
      Edit.apply_checked ~checker catalog forms
        (Set_arg{node=actual_card.path;key=Kw field;sub=[];value})|>flow_ok)
      (forms,actual)["delete_non_selected",S.make(Sym "true");
        "point_pattern",S.make(Str "old_int");"vertex_pattern",S.make(Str "old_vec*");
        "primitive_pattern",S.make(Str "old_text");"detail_pattern",S.make(Str "old_float*")] in
    let changes=["delete_non_selected",P.Bool_value true;"point_pattern",P.Text_value "old_int";
      "vertex_pattern",P.Text_value "old_vec*";"primitive_pattern",P.Text_value "old_text";
      "detail_pattern",P.Text_value "old_float*"] in
    let observed=compile edited in
    let expected=native input "delete_attributes" changes in
    check(N.parameter_key observed=N.parameter_key expected && same(cook session 1 97 observed)(cook session 1 97 expected))
      "Delete Attributes every field edit cook/key";
    let connected_forms,connected=Edit.apply_checked ~checker catalog edited_forms
      (Connect{node=actual_card.path;key=Kw "reference";src="reference_source";iter=false})|>flow_ok in
    let connected_node=compile connected in
    check(same(cook session 1 97 connected_node)(cook session 1 97(native ~reference:input input "delete_attributes" changes)))
      "Delete Attributes optional reference connection cook";
    let _,disconnected=Edit.apply_checked ~checker catalog connected_forms
      (Disconnect{node=actual_card.path;key=Kw "reference";fallback=None})|>flow_ok in
    check(same(cook session 1 97(compile disconnected))(cook session 1 97 expected))
      "Delete Attributes optional reference disconnection cook";
    let _,bypassed=Edit.apply_checked ~checker catalog forms(Toggle_bypass{node=actual_card.path})|>flow_ok in
    check(same(cook session 1 97(compile bypassed))(cook session 1 97 input))"Delete Attributes actual bypass cook");
  print_endline "Delete Attributes card/inspector, all field gestures, optional connect/disconnect and bypass cooks"

let cold_sample ~batch ~domains node =
  List.init 3(fun _->
    let sessions=Array.init batch(fun _->Sop.Session.create ~max_entries:32 ~max_payload_bytes:512_000_000|>get_string_ok) in
    Fun.protect ~finally:(fun()->Array.iter Sop.Session.close sessions)(fun()->
      Gc.full_major();let bytes=Gc.allocated_bytes() and started=Unix.gettimeofday() in
      Array.iter(fun session->ignore(cook session domains 4096 node))sessions;
      (Unix.gettimeofday()-.started)/.float batch,(Gc.allocated_bytes()-.bytes)/.float batch))
    |>List.sort(fun(a,_)(b,_)->Float.compare a b)|>fun samples->List.nth samples 1

let swap_attributes_cases =
  let methods=["swap";"move";"copy"] in
  let kinds=["float";"int";"vec2";"vec3";"vec4";"text";"float_array";"int_array"] in
  []::List.concat_map(fun owner->List.concat_map(fun method_->
    List.map(fun(source,destination)->[[owner;source;destination;method_]])
      (["old_*","new_*";"old_*","old_*";"missing","old_float";
        "old_float","missing";"missing","missing2";"old_float","old_int";
        "old_float","old_float";"absent_*","old_*";"old_?nt","new_?nt";
        "old_[fi]*","new_[fi]*";"old_float","\\ "]@
       List.map(fun kind->"old_"^kind,"new_"^kind)kinds))methods)
    [" POINTS ";"vertices";"primitives";"detail"]@
  List.concat_map(fun method_->List.map(fun(source,destination)->[["point";source;destination;method_]])
    (["P","P";"P","new_P";"missing","P";"P","old_vec3";"old_vec3","P";
      "P","old_float";"old_float","P"]@
     List.concat_map(fun kind->["old_"^kind,"P";"P","old_"^kind])kinds))methods@
  [[ ["point";"old_float";"old_int";"swap"];["point";"old_int";"old_vec2";"swap"] ];
   [ ["point";"old_*";"mid_*";"copy"];["point";"mid_*";"new_*";"move"] ];
   [ ["point";"old_vec3";"P";"move"];["point";"P";"new_P";"copy"] ];
   [ ["vertex";"P";"new_P";"swap"] ]]

let swap_attributes_fixture () =
  let geometry=fixture() in
  let geometry=Geometry.with_attribute(Attribute.create_owned ~owner:Vertex ~name:"P"
    (Float(Array.make(Geometry.vertex_count geometry)(-0.)))|>get_string_ok)geometry|>get_string_ok in
  List.fold_left(fun geometry name->Geometry.with_attribute
    (Attribute.create_owned ~owner:Point ~name (Text(Array.make 6 "猫\t😀"))|>get_string_ok)
    geometry|>get_string_ok)geometry ["é猫";"a-b";"back\\slash";"a b";"[x]";"*"]

let swap_attributes_exact () =
  let key="swap_attributes" and base=swap_attributes_fixture() in
  let positions=Packed.Float3.Private.view(Geometry.positions base) in
  let x=Array.copy positions.x in x.(0)<-nan;x.(1)<-infinity;x.(2)<-neg_infinity;
  let raw=Geometry.with_positions(Packed.Float3.of_owned ~x ~y:(Array.copy positions.y)
    ~z:(Array.copy positions.z)|>get_string_ok)base|>get_string_ok in
  let raw=Geometry.with_attribute(Attribute.create_owned ~owner:Point ~name:"raw_float"
    (Float[|nan;infinity;neg_infinity;-0.;0.;1.|])|>get_string_ok)raw|>get_string_ok in
  let cases=swap_attributes_cases@List.concat_map(fun method_->List.map(fun pattern->
    [["point";pattern;"renamed";method_]])
    ["é猫";"a-b";"back\\\\slash";"a\\ b";"\\[x\\]";"\\*"])["swap";"move";"copy"] in
  List.iter(fun geometry->let before=geometry_bytes geometry and input=Lisp_sop.snapshot geometry in
    List.iter(fun rows->let keywords,changes=rules rows in
      let actual=node ~reference:false input key keywords and walked=node ~reference:true input key keywords
      and expected=native input key changes in
      check(N.operation actual=N.operation expected && N.version actual=N.version expected
        && N.label actual=N.label expected && N.parameter_fields actual=N.parameter_fields expected
        && N.parameter_key actual=N.parameter_key expected)"Swap Attributes native identity/schema/key";
      let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:64_000_000|>get_string_ok in
      Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
        let baseline=cook session 1 97 expected in
        List.iter(fun domains->List.iter(fun grain->Sop.Session.clear session;
          let observed=cook session domains grain actual in
          check(same baseline observed)("Swap Attributes native bytes/cause "^keywords^
            "\nNative "^describe baseline^"\nLisp "^describe observed);
          check(same baseline(cook session domains grain walked))"Swap Attributes strict/reference bytes/cause")
          [1;97;4096])[1;4;8]))cases;
    check(before=geometry_bytes geometry)"Swap Attributes input mutation")
    [points 0;points 1;grid();base;raw;points 32769];
  let input=Lisp_sop.snapshot base in
  let actual=node ~reference:false input key "" and expected=native input key [] in
  List.iter(fun raw->let apply node=match N.apply_parameters node["rules",P.Text_value raw]with
      |Error message->Error message|Ok _->Ok() in
    check(apply actual=apply expected && Result.is_error(apply actual))"Swap Attributes encoded/validation constructor refusal")
    ["point";"point\twrong\\q";"point\twrong\\";
      table[["any";"old_float";"new_float";"copy"]];table[["*";"old_float";"new_float";"copy"]];
      table[["BADé猫\"";"old_float";"new_float";"copy"]];table[["point";"old_float";"new_float";"BADé猫\""]];
      table[["point";"[";"new";"copy"]];table[["point";"old";"new*";"move"]];
      table[["point";"old*";"new";"swap"]];table[["point";"\\";"new";"copy"]];
      table[["point";"old_float";"P";"move"];["vertex";"[";"new";"copy"]];
      table[["point";"old_float";"P";"move"];["BADé猫\"";"old";"new";"copy"]]];
  List.iter(fun value->let apply node=match N.apply_parameters node["rules",value]with
      |Error message->Error message|Ok _->Ok() in
    check(apply actual=apply expected && Result.is_error(apply actual))"Swap Attributes wrong parameter refusal")
    [P.Bool_value true;P.Int_value 17;P.Float_value nan;P.Float_value infinity];
  let changes=["rules",P.Text_value(table[[" POINTS ";"old_float";"old_int";" SWAP "]])] in
  let edited=fst(N.apply_parameters actual changes|>get_string_ok)
  and oracle=fst(N.apply_parameters expected changes|>get_string_ok) in
  check(N.parameter_fields edited=N.parameter_fields oracle && N.parameter_key edited=N.parameter_key oracle)
    "Swap Attributes canonical edit schema/key";
  let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:64_000_000|>get_string_ok in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    ignore(cook session 1 97 actual);let before=Sop.Session.stats session in
    let observed=cook session 1 97 edited in let after=Sop.Session.stats session in
    check(after.misses=before.misses+1 && same observed(cook session 1 97 oracle))
      "Swap Attributes one public edit miss and native bytes";
    let before=Sop.Session.stats session in ignore(cook session 1 97 edited);let after=Sop.Session.stats session in
    check(after.misses=before.misses && after.hits>before.hits)"Swap Attributes warm hit";
    List.iter(fun label->let relabeled=N.relabel label edited in
      check(N.id relabeled=N.id edited && N.parameter_key relabeled=N.parameter_key edited
        && same observed(cook session 1 97 relabeled))"Swap Attributes relabel identity/cook") ["Swapped";"é猫"]);
  print_endline "Swap Attributes all owner/method/storage/P/order/raw/error/schema/cache/domain proof"

let swap_attributes_projection () =
  metadata_projection "swap_attributes"
    ["rules",P.Text_value(table[[" POINTS ";"old_float";"old_int";" SWAP "]])]

let swap_attributes_benchmark ?(p_only=false) () =
  let measured=ref 0 and failures=ref [] in
  let geometry=Plane_generators.grid ~connectivity:Grid_quads ~columns:999 ~rows:999 ~size:8.()|>get_ok in
  check(Geometry.point_count geometry=1_000_000)"Swap Attributes million-point quad grid";
  let geometry=List.fold_left(fun geometry(owner,count)->
    List.fold_left(fun geometry name->Geometry.with_attribute
      (Attribute.create_owned ~owner ~name(Float(Array.make count(-0.)))|>get_string_ok)geometry|>get_string_ok)
      geometry ["old_a";"old_b";"target"])geometry
    [Attribute.Point,Geometry.point_count geometry;Vertex,Geometry.vertex_count geometry;
      Primitive,Geometry.primitive_count geometry;Detail,1] in
  let alternate_positions=Packed.Float3.of_owned
    ~x:(Array.make(Geometry.point_count geometry)1.)
    ~y:(Array.make(Geometry.point_count geometry)(-0.))
    ~z:(Array.make(Geometry.point_count geometry)3.)|>get_string_ok in
  check(Packed.Float3.data_id alternate_positions<>Packed.Float3.data_id(Geometry.positions geometry))
    "Swap Attributes benchmark must change canonical P, not reuse its buffer";
  let geometry=Geometry.with_attribute(Attribute.create_owned ~owner:Point ~name:"old_vec3"
    (Float3 alternate_positions)|>get_string_ok)geometry|>get_string_ok in
  let input=Lisp_sop.snapshot geometry in
  let configs=["default",[],1000]@
    List.concat_map(fun owner->List.concat_map(fun method_->
      List.map(fun(label,source,destination)->owner^"_"^method_^"_"^label,
        [[owner;source;destination;method_]],1000)
        ["present","old_a","target";"new","old_a","new";"inverse","missing","target";
         "missing","missing","missing2";"same","old_a","old_a";"wildcard","old_*","new_*"]@
      [owner^"_"^method_^"_successive",[[owner;"old_*";"mid_*";method_];[owner;"mid_*";"new_*";method_]],1000])
      ["swap";"move";"copy"])["point";"vertex";"primitive";"detail"]@
    List.concat_map(fun method_->List.map(fun(label,source,destination)->"P_"^method_^"_"^label,
      [["point";source;destination;method_]],1)
      ["same","P","P";"from","P","new_P";"from_existing","P","old_vec3";
       "into","old_vec3","P";"wrong_into","old_a","P";"wrong_from","P","old_a";
       "inverse","missing","P"])["swap";"move";"copy"] in
  let configs=if p_only then List.filter_map(fun(label,rows,_)->
    if String.starts_with ~prefix:"P_" label then Some(label,rows,1000)else None)configs else configs in
  List.iter(fun(label,rows,batch)->let keywords,changes=rules rows in
    let actual=node ~reference:false input "swap_attributes" keywords
    and walked=node ~reference:true input "swap_attributes" keywords
    and expected=native input "swap_attributes" changes in
    List.iter(fun domains->let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:512_000_000|>get_string_ok in
      Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
        let baseline=cook session domains 4096 expected in
        check(same baseline(cook session domains 4096 actual) && same baseline(cook session domains 4096 walked))
          ("Swap Attributes million-point native/reference preflight "^label));
      let native_time,native_bytes=cold_sample ~batch ~domains expected
      and time,bytes=cold_sample ~batch ~domains actual in
      incr measured;
      let ratio=time/.native_time in
      if time>2.*.native_time then failures:=(label,domains,ratio)::!failures;
      Printf.printf "swap_attributes points=1000000 quads=998001 float_attrs=12 vec3_attrs=1 mode=%s domains=%d grain=4096 cold_sessions=%d native=%.17g lisp=%.17g ratio=%.17g native_bytes=%.0f lisp_bytes=%.0f\n%!"
        label domains batch native_time time ratio native_bytes bytes)[1;4])configs;
  check(!measured=(if p_only then 42 else 212))"Swap Attributes must retain all predetermined performance rows";
  Printf.printf "swap_attributes_gate rows=%d failures=%d p_batch=%b\n%!" !measured(List.length !failures)p_only;
  match List.rev !failures with []->()| (mode,domains,ratio)::_->fail(Printf.sprintf
    "Swap Attributes2x gate: %d failed rows; first %s domains=%d ratio=%.17g"
    (List.length !failures)mode domains ratio)

let rename_attributes_benchmark () =
  let geometry=Plane_generators.grid ~connectivity:Grid_quads ~columns:999 ~rows:999 ~size:8.()|>get_ok in
  check(Geometry.point_count geometry=1_000_000)"Rename Attributes million-point grid";
  let geometry=List.fold_left(fun geometry(owner,count)->
    List.fold_left(fun geometry name->Geometry.with_attribute
      (Attribute.create_owned ~owner ~name(Float(Array.make count(-0.)))|>get_string_ok)geometry|>get_string_ok)
      geometry ["old_a";"old_b";"target"])
    geometry [Attribute.Point,Geometry.point_count geometry;Vertex,Geometry.vertex_count geometry;
      Primitive,Geometry.primitive_count geometry;Detail,1] in
  let input=Lisp_sop.snapshot geometry in
  let configs=["default",[];"missing",[["any";"missing";"new";"skip"]];
    "same",[["any";"old_*";"old_*";"skip"]]]@
    List.concat_map(fun owner->List.map(fun policy->owner^"_"^policy,[[owner;"old_a";"target";policy]])
      ["skip";"error";"overwrite"])["point";"vertex";"primitive";"detail";"any"]@
    List.map(fun owner->owner^"_wildcard",[[owner;"old_*";"new_*";"skip"]])
      ["point";"vertex";"primitive";"detail";"any"]@
    ["successive",[["any";"old_*";"mid_*";"skip"];["any";"mid_*";"new_*";"overwrite"]];
     "overwrite_order",[["any";"old_a";"old_b";"overwrite"];["any";"old_b";"target";"overwrite"]]] in
  List.iter(fun(label,rows)->let keywords,changes=rules rows in
    let actual=node ~reference:false input "rename_attributes" keywords
    and walked=node ~reference:true input "rename_attributes" keywords
    and expected=native input "rename_attributes" changes in
    List.iter(fun domains->let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:512_000_000|>get_string_ok in
      Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
        let baseline=cook session domains 4096 expected in
        check(same baseline(cook session domains 4096 actual) && same baseline(cook session domains 4096 walked))
          ("Rename Attributes million-point strict/reference/native preflight "^label));
      let native_time,native_bytes=cold_sample ~batch:1000 ~domains expected
      and time,bytes=cold_sample ~batch:1000 ~domains actual in
      Printf.printf "rename_attributes points=1000000 quads=998001 attributes=12 mode=%s domains=%d grain=4096 cold_sessions=1000 native=%.9f lisp=%.9f ratio=%.3f native_bytes=%.0f lisp_bytes=%.0f\n%!"
        label domains native_time time(time/.native_time)native_bytes bytes)[1;4])configs

let group_rename_benchmark () =
  let geometry=Plane_generators.grid ~connectivity:Grid_quads ~columns:999 ~rows:999 ~size:8.()|>get_ok in
  check(Geometry.point_count geometry=1_000_000)"Group Rename million-point quad grid";
  let geometry=List.fold_left(fun geometry(owner,count)->
    List.fold_left(fun geometry(name,phase)->Geometry.with_group
      (Group.init ~owner ~name count(fun i->i mod 3=phase))geometry|>get_string_ok)
      geometry ["old_a",0;"old_b",1;"target",2])geometry
    [Group.Point,Geometry.point_count geometry;Vertex,Geometry.vertex_count geometry;Primitive,Geometry.primitive_count geometry] in
  let geometry=List.fold_left(fun geometry(owner,count)->
    List.fold_left(fun geometry(name,phase)->let length=max 0 ((count+2-phase)/3) in
      let order=Array.init length(fun i->phase+3*(length-1-i)) in
      Geometry.with_group(Group.ordered ~owner ~name ~length:count order|>get_string_ok)geometry|>get_string_ok)
      geometry ["ordered_a",0;"ordered_target",2])geometry
    [Group.Point,Geometry.point_count geometry;Vertex,Geometry.vertex_count geometry;Primitive,Geometry.primitive_count geometry] in
  let topology=Geometry.topology geometry in let index=Topology_index.create topology in
  let geometry=List.fold_left(fun geometry(name,phase)->Geometry.with_edge_group
    (Edge_group.init ~topology ~index ~name(fun i->i mod 3=phase))geometry|>get_string_ok)
    geometry ["old_a",0;"old_b",1;"target",2] in
  let input=Lisp_sop.snapshot geometry in
  let configs=["default",[];"missing",[["any";"missing";"new";"skip"]];
    "same",[["any";"old_*";"old_*";"union"]]]@
    List.concat_map(fun owner->List.map(fun policy->owner^"_"^policy,[[owner;"old_a";"target";policy]])
      ["skip";"error";"overwrite";"union"])["point";"vertex";"primitive";"edge";"any"]@
    List.map(fun owner->owner^"_wildcard",[[owner;"old_*";"new_*";"skip"]])
      ["point";"vertex";"primitive";"edge";"any"]@
    ["successive",[["any";"old_*";"mid_*";"skip"];["any";"mid_*";"new_*";"overwrite"]];
     "union_chain",[["any";"old_a";"target";"union"];["any";"old_b";"target";"union"]]]@
    List.map(fun owner->owner^"_ordered_union",[[owner;"ordered_a";"ordered_target";"union"]])
      ["point";"vertex";"primitive";"any"] in
  List.iter(fun(label,rows)->let keywords,changes=rules rows in
    let actual=node ~reference:false input "group_rename" keywords
    and walked=node ~reference:true input "group_rename" keywords
    and expected=native input "group_rename" changes in
    List.iter(fun domains->let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:512_000_000|>get_string_ok in
      Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
        let baseline=cook session domains 4096 expected in
        check(same baseline(cook session domains 4096 actual) && same baseline(cook session domains 4096 walked))
          ("Group Rename million-point native/reference preflight "^label));
      let batch=if List.exists(function [_;pattern;replacement;"union"]->pattern<>replacement|_->false)rows
        then 1 else 1000 in
      let native_time,native_bytes=cold_sample ~batch ~domains expected
      and time,bytes=cold_sample ~batch ~domains actual in
      Printf.printf "group_rename points=1000000 quads=998001 groups=18 mode=%s domains=%d grain=4096 cold_sessions=%d native=%.9f lisp=%.9f ratio=%.3f native_bytes=%.0f lisp_bytes=%.0f\n%!"
        label domains batch native_time time(time/.native_time)native_bytes bytes)[1;4])configs

let delete_attributes_benchmark () =
  let geometry=Plane_generators.grid ~connectivity:Grid_quads ~columns:999 ~rows:999 ~size:8.()|>get_ok in
  check(Geometry.point_count geometry=1_000_000)"Delete Attributes million-point grid";
  let geometry=List.fold_left(fun geometry(owner,count)->List.fold_left(fun geometry(name,storage)->
    Geometry.with_attribute(Attribute.create_owned ~owner ~name storage|>get_string_ok)geometry|>get_string_ok)
    geometry ["keep_float",Attribute.Float(Array.make count(-0.));"drop_int",Int(Array.make count max_int)])
    geometry [Attribute.Point,Geometry.point_count geometry;Vertex,Geometry.vertex_count geometry;
      Primitive,Geometry.primitive_count geometry;Detail,1] in
  let input=Lisp_sop.snapshot geometry in
  let reference=List.fold_left(fun geometry owner->Geometry.without_attribute ~owner "drop_int" geometry)
    geometry [Attribute.Point;Vertex;Primitive;Detail]
    |>Geometry.without_attribute ~owner:Attribute.Vertex "keep_float" in
  let patterns=["point_pattern";"vertex_pattern";"primitive_pattern";"detail_pattern"] in
  let configs=["default",[];"inverse",["delete_non_selected",P.Bool_value true]]@
    List.map(fun field->field,[field,P.Text_value "keep_*"])patterns@
    List.concat_map(fun(pattern,label)->List.map(fun inverse->label^(if inverse then "_inverse" else ""),
      ("delete_non_selected",P.Bool_value inverse)::List.map(fun field->field,P.Text_value pattern)patterns)[false;true])
      ["*","all";"* ^keep_*","exclusions"] in
  List.iter(fun(reference_name,reference)->
    List.iter(fun(label,changes)->
      let keywords=String.concat " "(List.map(fun(field,value)->":"^field^" "^(match value with
        |P.Bool_value value->string_of_bool value|P.Text_value value->quoted value|_->assert false))changes)
        ^(if Option.is_some reference then " :reference (sop/metadata_reference)" else "") in
      let actual=node ~reference_node:reference ~reference:false input "delete_attributes" keywords
      and expected=native ?reference input "delete_attributes" changes in
      List.iter(fun domains->
        let preflight=Sop.Session.create ~max_entries:32 ~max_payload_bytes:512_000_000|>get_string_ok in
        Fun.protect ~finally:(fun()->Sop.Session.close preflight)(fun()->
          check(same(cook preflight domains 4096 actual)(cook preflight domains 4096 expected))
            "Delete Attributes million-point native preflight");
        let native_time,native_bytes=cold_sample ~batch:1000 ~domains expected
        and time,bytes=cold_sample ~batch:1000 ~domains actual in
        Printf.printf "delete_attributes points=1000000 quads=998001 mode=%s reference=%s domains=%d grain=4096 cold_sessions=1000 native=%.6f lisp=%.6f ratio=%.3f native_bytes=%.0f lisp_bytes=%.0f\n%!"
          label reference_name domains native_time time(time/.native_time)native_bytes bytes)[1;4])configs)
    ["absent",None;"empty",Some(Lisp_sop.snapshot(points 0));"selected",Some(Lisp_sop.snapshot reference)]

let edge_benchmark ?(batch=1) key =
  let geometry=Plane_generators.grid ~connectivity:Grid_quads ~columns:999 ~rows:999 ~size:8.()|>get_ok in
  check(Geometry.point_count geometry=1_000_000)"Delete Edge Group million-point grid";
  let topology=Geometry.topology geometry in
  let edge=Edge_group.init ~topology ~index:(Topology_index.create topology) ~name:"edges"(fun i->i mod 2=0) in
  List.iter(fun group_count->
    let geometry=List.init group_count(fun i->if i=0 then "edges" else "edge_"^string_of_int i)
      |>List.fold_left(fun geometry name->Geometry.with_edge_group(Edge_group.with_name name edge)geometry|>get_string_ok)geometry in
    let input=Lisp_sop.snapshot geometry in
    List.iter(fun name->
      let keywords,changes=if key="delete_edge_group" then ":name "^quoted name,["name",P.Text_value name]
        else ":from "^quoted name^" :into \"renamed\"",["from",P.Text_value name;"into",P.Text_value "renamed"] in
      let actual=node ~reference:false input key keywords
      and expected=native input key changes in
      List.iter(fun domains->
        let preflight=Sop.Session.create ~max_entries:32 ~max_payload_bytes:512_000_000|>get_string_ok in
        Fun.protect ~finally:(fun()->Sop.Session.close preflight)(fun()->
          check(same(cook preflight domains 4096 actual)(cook preflight domains 4096 expected))
            "Delete Edge Group million-point native preflight");
        let sample node=List.init 3(fun _->
          let sessions=Array.init batch(fun _->Sop.Session.create ~max_entries:32 ~max_payload_bytes:512_000_000|>get_string_ok) in
          Fun.protect ~finally:(fun()->Array.iter Sop.Session.close sessions)(fun()->
            Gc.full_major();let bytes=Gc.allocated_bytes() and started=Unix.gettimeofday() in
            Array.iter(fun session->ignore(cook session domains 4096 node))sessions;
            (Unix.gettimeofday()-.started)/.float batch,(Gc.allocated_bytes()-.bytes)/.float batch))
          |>List.sort(fun(a,_)(b,_)->Float.compare a b)|>fun samples->List.nth samples 1 in
        let native_time,native_bytes=sample expected and time,bytes=sample actual in
        Printf.printf "%s points=1000000 quads=998001 groups=%d name=%S domains=%d grain=4096 cold_sessions=%d native=%.6f lisp=%.6f ratio=%.3f native_bytes=%.0f lisp_bytes=%.0f\n%!"
          key group_count name domains batch native_time time(time/.native_time)native_bytes bytes)[1;4])
      ["edges";"absent"])[0;1;8]

let group_delete_benchmark () =
  let geometry=Plane_generators.grid ~connectivity:Grid_quads ~columns:999 ~rows:999 ~size:8.()|>get_ok in
  check(Geometry.point_count geometry=1_000_000)"Group Delete million-point grid";
  let geometry=List.fold_left(fun geometry(owner,count)->
    List.fold_left(fun geometry(name,empty)->
      Geometry.with_group(Group.init ~owner ~name count(fun i->not empty && i mod 3=0))geometry|>get_string_ok)
      geometry ["old_selected",false;"keep_selected",false;"unused",true])geometry
    [Group.Point,Geometry.point_count geometry;Vertex,Geometry.vertex_count geometry;
      Primitive,Geometry.primitive_count geometry] in
  let topology=Geometry.topology geometry and index=Topology_index.create(Geometry.topology geometry) in
  let geometry=List.fold_left(fun geometry(name,empty)->
    Geometry.with_edge_group(Edge_group.init ~topology ~index ~name(fun i->not empty && i mod 3=0))geometry|>get_string_ok)
    geometry ["old_selected",false;"keep_selected",false;"unused",true] in
  let input=Lisp_sop.snapshot geometry in
  let configs=["default",[];"unused",["delete_unused",P.Bool_value true]]@
    List.map(fun owner->owner,["rules",P.Text_value(table[[owner;"old_*"]])])
      ["point";"vertex";"primitive";"edge";"any"]@
    List.map(fun(label,rows,unused)->label,["rules",P.Text_value(table rows);"delete_unused",P.Bool_value unused])
      ["all",[["any";"*"]],false;"exclusions",[["any";"* ^keep_*"]],false;
       "blank",[["any";" \t "]],false;"missing",[["any";"missing"]],false;
       "selected_unused",[["any";"old_*"]],true;
       "successive",[["points";"old_*"];["vertices";"old_*"];["primitives";"old_*"];["edges";"old_*"]],true] in
  List.iter(fun(label,changes)->
    let keywords=String.concat " "(List.map(fun(field,value)->":"^field^" "^(match value with
      |P.Bool_value value->string_of_bool value|P.Text_value value->quoted value|_->assert false))changes) in
    let actual=node ~reference:false input "group_delete" keywords and expected=native input "group_delete" changes in
    List.iter(fun domains->
      let preflight=Sop.Session.create ~max_entries:32 ~max_payload_bytes:512_000_000|>get_string_ok in
      Fun.protect ~finally:(fun()->Sop.Session.close preflight)(fun()->
        check(same(cook preflight domains 4096 actual)(cook preflight domains 4096 expected))
          "Group Delete million-point native preflight");
      let native_time,native_bytes=cold_sample ~batch:1000 ~domains expected
      and time,bytes=cold_sample ~batch:1000 ~domains actual in
      Printf.printf "group_delete points=1000000 quads=998001 groups=12 mode=%s domains=%d grain=4096 cold_sessions=1000 native=%.6f lisp=%.6f ratio=%.3f native_bytes=%.0f lisp_bytes=%.0f\n%!"
        label domains native_time time(time/.native_time)native_bytes bytes)[1;4])configs

let edges ()=List.iter(fun key->edge_exact key;edge_projection key)["delete_edge_group";"rename_edge_group"]
let ()=if Array.exists((=)"--bench-edge")Sys.argv then edge_benchmark "delete_edge_group"
  else if Array.exists((=)"--bench-delete-attributes")Sys.argv then delete_attributes_benchmark()
  else if Array.exists((=)"--bench-group-delete")Sys.argv then group_delete_benchmark()
  else if Array.exists((=)"--bench-swap-attributes-p-batch")Sys.argv then swap_attributes_benchmark ~p_only:true()
  else if Array.exists((=)"--bench-swap-attributes")Sys.argv then swap_attributes_benchmark()
  else if Array.exists((=)"--swap-attributes")Sys.argv then (swap_attributes_exact();swap_attributes_projection())
  else if Array.exists((=)"--bench-group-rename")Sys.argv then group_rename_benchmark()
  else if Array.exists((=)"--group-rename")Sys.argv then (group_rename_exact();group_rename_projection())
  else if Array.exists((=)"--group-delete")Sys.argv then (group_delete_exact();group_delete_projection())
  else if Array.exists((=)"--bench-rename-attributes")Sys.argv then rename_attributes_benchmark()
  else if Array.exists((=)"--rename-attributes")Sys.argv then (rename_attributes_exact();rename_attributes_projection())
  else if Array.exists((=)"--bench-rename-edge")Sys.argv then edge_benchmark "rename_edge_group"
  else if Array.exists((=)"--bench-rename-edge-batch")Sys.argv then edge_benchmark ~batch:1000 "rename_edge_group"
  else if Array.exists((=)"--edge")Sys.argv then (delete_attributes_projection();rename_attributes_projection();group_delete_projection();group_rename_projection();swap_attributes_projection();edges())
  else if Array.exists((=)"--huge")Sys.argv then large_rules 100001
  else if Array.exists((=)"--large")Sys.argv then large_rules 5001
  else(declaration_metadata();verify();edits();invalid_parameters();reference_input();delete_attributes_projection();edges())
