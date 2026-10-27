(* Whole setter checks for embedded ports. *)
open Rdk
open Rdk_test_support
module N = Sop.Node
module P = Param
let checked = function Ok value -> value | Error diagnostic -> fail (Diagnostic.to_string diagnostic)
let sources = Flow_sop.Catalog.sources () |> checked
let keys = ["set_float";"set_int";"set_vector";"set_orient";"set_transform"]
let native_factory = function
  |"set_float" -> Sop.Nodes.Set_float.factory |"set_int" -> Sop.Nodes.Set_int.factory
  |"set_vector" -> Sop.Nodes.Set_vector.factory |"set_orient" -> Sop.Nodes.Set_orient.factory
  |"set_transform" -> Sop.Nodes.Set_transform.factory | _ -> assert false
let node ~reference input key keywords =
  let input_factory = Sop.Edit_graph.factory ~key:"setter_input" ~label:"Setter input" ~category:["Test"] ~arity:0 (fun _ -> input) in
  let factories = input_factory :: List.filter (fun factory -> not (List.mem (Sop.Edit_graph.factory_key factory) keys)) Sop_catalog.Editor.factories in
  let catalog = Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories |> checked in
  let forms = Printf.sprintf "(workspace setters (graph g :context sop (sop/%s (sop/setter_input) %s)))" key keywords |> Flow.Syntax.parse |> checked in
  let workspace = match Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms with
    |Some workspace, diagnostics when not (List.exists (fun (diagnostic:Diagnostic.t)->diagnostic.severity=Diagnostic.Error_severity) diagnostics) -> workspace
    |_, diagnostics -> fail (String.concat "\n" (List.map Diagnostic.to_string diagnostics)) in
  let lowered = match Flow_sop.Lower.of_checked ~reference ~factories workspace with
    |Ok lowered->lowered|Error diagnostic->fail(key^" "^keywords^": "^Diagnostic.to_string diagnostic) in
  if keywords="" then begin
    let actual=List.find(fun factory->Sop.Edit_graph.factory_key factory=key)lowered.factories
    and expected=native_factory key in
    check(Flow_sop.Catalog.descriptor actual=Flow_sop.Catalog.descriptor expected)
      (key^": full native factory descriptor differs")
  end;
  let graph = List.hd lowered.graphs in
  Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |> get_string_ok
let native input key changes =
  let node = Sop.Edit_graph.instantiate (native_factory key) [input] |> get_string_ok in
  fst (N.apply_parameters node changes |> get_string_ok)
let cook session domains grain node =
  let context = Sop.Context.create ~domains ~grain () |> get_string_ok in
  let result = Sop.Session.cook session ~context node in
  match result with Ok value -> Sop.Payload.geometry value.payload |> (function Ok geometry->geometry |Error error->fail error.message)
    |Error error->fail error.message
let points count = Kernel.generate_point_ranges count (fun ~first ~last ~x ~y ~z ->
  for index=first to last-1 do x.(index)<-float index; y.(index)<-0.25;z.(index)<- -0.5 done)
let mixed () =
  let topology = Topology.create_owned ~point_count:6 ~vertex_points:[|0;1;2;3;1;4;5|]
    ~primitive_offsets:[|0;4;7|] ~primitive_kinds:[|Topology.Polygon;Open_polyline|] |> get_string_ok in
  let geometry = Geometry.create ~positions:(Geometry.positions (points 6)) ~topology () |> get_string_ok in
  let tag = Attribute.create_owned ~name:"tag" ~owner:Attribute.Detail (Attribute.Text [|"untouched ✓"|]) |> get_string_ok in
  Geometry.with_attribute tag geometry |> get_string_ok
let owners = ["Point";"Vertex";"Primitive";"Detail"]
let grid ()=
  let size=65 in
  let geometry=points(size*size) in
  let vertex_points=Array.init(64*64*4)(fun index->
    let cell=index/4 and corner=index mod 4 in
    let x=cell mod 64 and y=cell/64 in
    (y+(if corner>=2 then 1 else 0))*size+x+(if corner=1||corner=2 then 1 else 0)) in
  let topology=Topology.polygons_owned ~point_count:(size*size)~vertex_points
    ~primitive_offsets:(Array.init(64*64+1)(fun index->index*4))|>get_string_ok in
  Geometry.create ~positions:(Geometry.positions geometry)~topology()|>get_string_ok
let cases =
  List.concat_map (fun owner ->
    let keyword = Printf.sprintf ":owner %S " owner and change = "owner",P.Choice_value owner in
    ["set_float",keyword^":name \"new float\" :value -2.5",[change;"name",P.Text_value "new float";"value",P.Float_value (-2.5)];
     "set_int",keyword^":name \"id\" :value 4611686018427387903",[change;"name",P.Text_value "id";"value",P.Int_value max_int];
     "set_vector",keyword^":name \"v\" :value [2 -3 0.5]",[change;"name",P.Text_value "v";"x",P.Float_value 2.;"y",P.Float_value (-3.);"z",P.Float_value 0.5]]) owners
  @ List.map (fun key -> key,"",[]) keys
  @ ["set_orient",":x 0 :y 0 :z 0 :w 0",["w",P.Float_value 0.];
     "set_orient",":x 1 :y 2 :z 3 :w -4",["x",P.Float_value 1.;"y",P.Float_value 2.;"z",P.Float_value 3.;"w",P.Float_value (-4.)];
     "set_orient",":x 1e308",["x",P.Float_value 1e308];
     "set_transform",":m01 -2 :m03 4 :m30 1e-12 :m33 1.0000000000005",["m01",P.Float_value (-2.);"m03",P.Float_value 4.;"m30",P.Float_value 1e-12;"m33",P.Float_value 1.0000000000005]]
let verify () =
  List.iter (fun geometry ->
    let input=Lisp_sop.snapshot geometry and before=geometry_bytes geometry in
    List.iter (fun (key,keywords,changes) ->
      let actual=node ~reference:false input key keywords and reference=node ~reference:true input key keywords in
      let oracle=native input key changes in
      check (N.operation actual=N.operation oracle && N.version actual=N.version oracle)
        (key^": runtime operation/version differs");
      check (N.label actual=N.label oracle) (key^": initial cook label differs");
      check (N.parameter_fields actual=N.parameter_fields oracle) (key^": parameter metadata differs");
      check (N.parameter_key actual=N.parameter_key oracle) (key^": parameter key differs");
      let session=Sop.Session.create ~max_entries:64 ~max_payload_bytes:256_000_000 |> get_string_ok in
      Fun.protect ~finally:(fun ()->Sop.Session.close session) (fun () ->
        let baseline=cook session 1 97 actual in
        check (close_geometry ~ulps:4 baseline (cook session 1 97 oracle)) (key^": native oracle differs");
        check (geometry_bytes baseline=geometry_bytes (cook session 1 97 reference)) (key^": strict/reference bytes differ");
        List.iter (fun domains -> List.iter (fun grain ->
          Sop.Session.clear session;
          check (geometry_bytes baseline=geometry_bytes (cook session domains grain actual)) (key^": worker/grain bytes differ")) [1;97;4096]) [1;4;8]);
      check (before=geometry_bytes geometry) (key^": input mutated")) cases)
    [points 0;points 1;mixed ();grid();points 32769];
  print_endline "setter draft: all owners, schema, native/reference and worker bytes passed"

let parameter_edits () =
  let input=Lisp_sop.snapshot(mixed()) in
  List.iter(fun key->
    let initial=node ~reference:false input key "" and oracle=native input key [] in
    List.iter(fun(field:P.field_view)->
      let value=match field.current with
        |P.Choice_value _->P.Choice_value "Vertex"
        |Text_value _->Text_value "edited"
        |Int_value value->Int_value(value+7)
        |Float_value value->Float_value(value+.(if List.mem field.name ["m30";"m31";"m32";"m33"]then 1e-13 else 0.25))
        |Bool_value _->assert false in
      let edited=fst(N.apply_parameters initial[field.name,value] |>get_string_ok)
      and expected=fst(N.apply_parameters oracle[field.name,value] |>get_string_ok) in
      check(N.parameter_key edited=N.parameter_key expected)(key^" edited native key differs: "^field.name);
      let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:16_000_000 |>get_string_ok in
      Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
        ignore(cook session 1 97 initial);let before=Sop.Session.stats session in
        let actual=cook session 1 97 edited in let after=Sop.Session.stats session in
        check(after.misses=before.misses+1)(key^" edit must have one public miss: "^field.name);
        ignore(cook session 1 97 edited);let warm=Sop.Session.stats session in
        check(warm.misses=after.misses && warm.hits>after.hits)(key^" edited warm cook missed: "^field.name);
        check(close_geometry ~ulps:4 actual(cook session 1 97 expected))(key^" edited oracle differs: "^field.name)))
      (N.parameter_fields initial)) keys;
  print_endline "setter draft: every physical parameter edit has native keys/oracle and one public miss"

let invalid_parameters ()=
  let input=Lisp_sop.snapshot(mixed()) in
  List.iter(fun(key,changes)->
    let actual=node ~reference:false input key "" and oracle=native input key [] in
    let apply node=match N.apply_parameters node changes with Error message->Error message|Ok _->Ok() in
    let expected=apply oracle and observed=apply actual in
    check(expected=observed && Result.is_error observed)(key^": rejected parameter diagnostic differs");
    check(N.parameter_key actual=N.parameter_key oracle)(key^": failed edit changed initial parameter key"))
    (List.concat_map(fun key->[key,["name",P.Text_value " \t\n"];key,["name",P.Text_value "P"]])
       ["set_float";"set_int";"set_vector"]
     @List.concat_map(fun name->List.map(fun value->"set_transform",[name,P.Float_value value])
         [Float.next_after 1e-12 infinity;Float.next_after (-1e-12) neg_infinity]) ["m30";"m31";"m32"]
     @["set_transform",["m33",P.Float_value(Float.next_after (1.+.1e-12) infinity)];
       "set_transform",["m33",P.Float_value(Float.next_after (1.-.1e-12) neg_infinity)];
       "set_transform",["m32",P.Float_value 1.];
       "set_transform",["m33",P.Float_value 1.01];
       "set_orient",["x",P.Float_value infinity]]
     @List.concat_map(fun(field:P.field_view)->List.map(fun value->
         "set_transform",[field.name,P.Float_value value]) [nan;infinity;neg_infinity])
       (N.parameter_fields(native input "set_transform" [])));
  print_endline "setter draft: empty/P names, affine boundaries and nonfinite parameter errors match native atomically"

let benchmark ?only () =
  let count=1_000_000 in
  let input=Lisp_sop.snapshot(points count) in
  let median samples=List.sort(fun(a,_)(b,_)->Float.compare a b)samples |>fun samples->List.nth samples 1 in
  List.iter(fun(key,keywords,changes)->
    let authored=node ~reference:false input key keywords and oracle=native input key changes in
    List.iter(fun domains->
      let sample node=List.init 3(fun _->
        let session=Sop.Session.create ~max_entries:64 ~max_payload_bytes:512_000_000 |>get_string_ok in
        Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
          Gc.full_major();let allocated=Gc.allocated_bytes() and started=Unix.gettimeofday() in
          ignore(cook session domains 4096 node);
          Unix.gettimeofday()-.started,Gc.allocated_bytes()-.allocated)) |>median in
      let native_seconds,native_allocated=sample oracle and lisp_seconds,lisp_allocated=sample authored in
      Printf.printf "%s points=%d domains=%d grain=4096 native_seconds=%.6f lisp_seconds=%.6f ratio=%.3f native_allocated_bytes=%.0f lisp_allocated_bytes=%.0f\n%!"
        key count domains native_seconds lisp_seconds (lisp_seconds/.native_seconds) native_allocated lisp_allocated)
      [1;4])
    (List.filter(fun(key,_,_)->Option.fold ~none:true ~some:((=)key)only)
    ["set_float",":value 2.5",["value",P.Float_value 2.5];
     "set_int",":value 4611686018427387903",["value",P.Int_value max_int];
     "set_vector",":value [2 -3 0.5]",["x",P.Float_value 2.;"y",P.Float_value (-3.);"z",P.Float_value 0.5];
     "set_orient",":x 1 :y 2 :z 3 :w -4",["x",P.Float_value 1.;"y",P.Float_value 2.;"z",P.Float_value 3.;"w",P.Float_value (-4.)];
     "set_transform",":m01 -2 :m03 4",["m01",P.Float_value (-2.);"m03",P.Float_value 4.]])

let trace_transform ()=
  let programs=ref [] and previous= !(Flow_ir.Packed.Private.trace) in
  let seen=Hashtbl.create 32 in
  let trace result=match result with
    |Error diagnostics->List.iter(fun(d:Diagnostic.t)->
      let key=d.code^":"^d.message in if not(Hashtbl.mem seen key)then begin
        Hashtbl.add seen key();Printf.printf "packed refusal %s\n%!"(Diagnostic.to_string d)end)diagnostics
    |Ok (packed:Flow_ir.Packed.t)->let region=packed in
      let key=Flow.Lisp.flat region.term.form in
      if not(Hashtbl.mem seen key)then begin Hashtbl.add seen key();programs:=packed:: !programs;
        Printf.printf "packed success form=%s result=%s iteration=%s sources=[%s] registers=%d outputs=%d\n%!"
          key(Flow.Ty.to_string region.term.ty)
          (match region.iteration with Zip->"zip"|Product->"product")
          (String.concat ";"(Array.to_list(Array.map(fun(_,term)->Flow.Ty.to_string term.Flow.Workspace.ty)region.sources)))
          (Array.length region.code)(Array.length region.outputs)end in
  let geometry=mixed() in
  Flow_ir.Packed.Private.trace:=trace;
  Fun.protect ~finally:(fun()->Flow_ir.Packed.Private.trace:=previous)(fun()->
    let tested=node ~reference:false(Lisp_sop.snapshot geometry)"set_transform"":m01 -2 :m03 4" in
    let session=Sop.Session.create ~max_entries:64 ~max_payload_bytes:16_000_000|>get_string_ok in
    Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->ignore(cook session 1 4096 tested)));
  let resolve=Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some geometry) in
  List.iter(fun (packed:Flow_ir.Packed.t)->let region=packed in
    if region.iteration=Product then begin
      (match Flow_ir.Packed.Private.prepare_values ~resolve packed ~live:(Input.at_time 0.)with
       |Ok inputs->Printf.printf "product concrete count=%d sources=[%s]\n%!"inputs.count
           (String.concat ";"(Array.to_list(Array.map(fun value->Flow.Ty.to_string(Flow.Value.ty_of value))inputs.arrays)))
       |Error diagnostic->Printf.printf "product input failure %s\n%!"(Diagnostic.to_string diagnostic));
      match Flow_ir.Packed.force ~strict:true ~resolve packed ~live:(Input.at_time 0.)with
      |Ok value->Printf.printf "product strict force=%s\n%!"(Flow.Ty.to_string(Flow.Value.ty_of value))
      |Error diagnostic->Printf.printf "product strict failure %s\n%!"(Diagnostic.to_string diagnostic)
    end) !programs

let projection ()=
  let module S=Flow.Syntax in
  let module Projection=Flow_graph.Projection in
  let module Edit=Flow_graph.Flow_edit in
  let input=Lisp_sop.snapshot(mixed()) in
  let factory=Sop.Edit_graph.factory ~key:"setter_input" ~label:"Setter input" ~category:["Test"] ~arity:0(fun _->input) in
  let factories=factory::List.filter(fun factory->not(List.mem(Sop.Edit_graph.factory_key factory)keys))Sop_catalog.Editor.factories in
  let native_factories=List.map native_factory keys @ factories in
  let catalog factories=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories |>checked in
  let catalog=catalog factories and native_catalog=catalog native_factories in
  let checker catalog forms=Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms in
  let workspace=function Some workspace,diagnostics when not(List.exists(fun(d:Diagnostic.t)->d.severity=Diagnostic.Error_severity)diagnostics)->workspace
    |_,diagnostics->fail(String.concat"\n"(List.map Diagnostic.to_string diagnostics)) in
  let card catalog workspace=Option.get(Projection.find(Projection.of_graph catalog workspace "g")["g";"card"]) in
  let normalize(card:Projection.node)={card with rows=List.map(fun(row:Projection.row)->
    {row with expr=Option.map(fun expr->List.hd(Flow.Syntax.parse(Flow.Lisp.flat expr)|>checked))row.expr})card.rows} in
  let root workspace=let lowered=Flow_sop.Lower.of_checked ~factories workspace |>checked in
    let graph=List.hd lowered.graphs in
    Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)|>get_string_ok in
  List.iter(fun key->
    let authored=Printf.sprintf{|(workspace setters (graph g :context sop
      (let* [src (sop/setter_input) card (sop/%s src)] card)))|}key |>Flow.Syntax.parse |>checked in
    let actual=workspace(checker catalog authored)
    and expected=workspace(Flow.Workspace.check ~ops:Flow_sop.Operators.all native_catalog authored) in
    let actual_card=card catalog actual and expected_card=card native_catalog expected in
    check(normalize actual_card=normalize expected_card)(key^": native/Lisp graph card metadata");
    check(Projection.lines Full(normalize actual_card)=Projection.lines Full(normalize expected_card))
      (key^": inspector graph rows");
    let oracle=native input key [] in
    List.iter(fun(field:P.field_view)->
      let parameter,value=match field.current with
        |P.Choice_value _->P.Choice_value "Vertex",S.Str "Vertex"
        |Text_value _->P.Text_value "edited",S.Str "edited"
        |Int_value value->P.Int_value(value+7),S.Num(string_of_int(value+7))
        |Float_value value->
            let value=value+.(if List.mem field.name ["m30";"m31";"m32";"m33"]then 1e-13 else 0.25) in
            P.Float_value value,S.Num(Printf.sprintf"%.17g"value)
        |Bool_value _->assert false in
      (* Inspector vector edits write the complete value, including the two
         untouched defaults, when that keyword is absent from the source. *)
      let name,value=match field.vec3 with
        |None->field.name,value
        |Some(name,index)->name,S.Vec(List.init 3(fun component->
            if component=index then S.make value else
            let unchanged=List.find(fun(f:P.field_view)->f.vec3=Some(name,component))(N.parameter_fields oracle) in
            match unchanged.current with P.Float_value x->S.make(S.Num(Flow.Lisp.float x))|_->assert false)) in
      let _,changed=Edit.apply_checked ~checker catalog authored
        (Set_arg{node=actual_card.path;key=Kw name;sub=[];value=S.make value})|>checked in
      let row=List.find(fun(row:Projection.row)->row.key=Edit.Kw name)(card catalog changed).rows in
      check(Option.is_some row.expr)(key^": edited graph row "^field.name);
      let actual=root changed and expected=fst(N.apply_parameters oracle[field.name,parameter]|>get_string_ok) in
      check(N.parameter_key actual=N.parameter_key expected)(key^": graph gesture parameter key "^field.name);
      let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:16_000_000|>get_string_ok in
      Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
        check(close_geometry ~ulps:4(cook session 1 97 actual)(cook session 1 97 expected))
          (key^": graph gesture cook "^field.name))) (N.parameter_fields oracle);
    let _,bypassed=Edit.apply_checked ~checker catalog authored(Toggle_bypass{node=actual_card.path})|>checked in
    check((card catalog bypassed).bypass)(key^": bypass graph gesture"))
    keys;
  print_endline"setter draft: five setters native card/inspector parity and every physical field direct graph gesture"

let ()=if Array.exists((=)"--bench-transform")Sys.argv then benchmark ~only:"set_transform"()
  else if Array.exists((=)"--bench-float")Sys.argv then benchmark ~only:"set_float"()
  else if Array.exists((=)"--bench-int")Sys.argv then benchmark ~only:"set_int"()
  else if Array.exists((=)"--bench-vector")Sys.argv then benchmark ~only:"set_vector"()
  else if Array.exists((=)"--bench-orient")Sys.argv then benchmark ~only:"set_orient"()
  else if Array.exists((=)"--bench")Sys.argv then benchmark()
  else if Array.exists((=)"--trace")Sys.argv then trace_transform() else (verify();parameter_edits();invalid_parameters();projection())
