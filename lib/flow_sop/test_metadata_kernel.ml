(* Authored metadata operations preserve native owner, storage and ordering semantics. *)
open Rdk
open Rdk_test_support
module M = struct
  let quoted value=Flow.Lisp.flat(Flow.Syntax.make(Str value))
  let cook input expression =
    let node=Lisp_sop.node ~with_:["source",Lisp_sop.snapshot input] expression in
    let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:16_000_000 |> get_string_ok in
    Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
      let run domains grain =
        let context=Sop.Context.create ~domains ~grain () |> get_string_ok in
        match Sop.Session.cook session ~context node with
      |Ok output->Result.map_error Diagnostic.error_to_string (Sop.Payload.geometry output.payload)
      |Error error->Error(Diagnostic.error_to_string error) in
      let expected=run 1 97 in
      List.iter(fun domains->List.iter(fun grain->Sop.Session.clear session;
        let actual=run domains grain in
        check (match expected,actual with Ok a,Ok b->geometry_bytes a=geometry_bytes b
          |Error a,Error b->a=b|_->false) "Metadata mutation domain/grain results differ") [97;4096]) [1;8];
      expected)
  let rename_attr ~owner ~pattern ~replacement ~conflict input = cook input
    (Printf.sprintf "(sop/rename_attr (sop/ext_source) %s %s :owner %s :conflict %s)"
      (quoted pattern)(quoted replacement)(quoted owner)(quoted conflict))
  let rename_group ~owner ~pattern ~replacement ~conflict input = cook input
    (Printf.sprintf "(sop/rename_group (sop/ext_source) %s %s :owner %s :conflict %s)"
      (quoted pattern)(quoted replacement)(quoted owner)(quoted conflict))
  let delete_group ~owner ~pattern ~unused input = cook input
    (Printf.sprintf "(sop/delete_group (sop/ext_source) %s :owner %s :unused %b)"(quoted pattern)(quoted owner)unused)
  let rename_edge ~from ~into input = cook input
    (Printf.sprintf "(sop/rename_edge (sop/ext_source) %s %s)"(quoted from)(quoted into))
  let swap ~owner ~source ~destination ~method_ input = cook input
    (Printf.sprintf "(sop/swap_attr (sop/ext_source) %s %s :owner %s :method %s)"
      (quoted source)(quoted destination)(quoted owner)(quoted method_))
  let query owner name pattern input =
    let owner=match owner with Attribute.Point->"point"|Vertex->"vertex"|Primitive->"primitive"|Detail->"detail" in
    let flow_ok=function Ok value->value|Error d->fail(Diagnostic.to_string d) in
    let forms=Printf.sprintf "(workspace metadata (graph g :context sop [(geo : geometry nil)] (let* [tested (%s geo %s :owner %s)] geo)))"
      name(quoted pattern)(quoted owner)|>Flow.Syntax.parse|>flow_ok in
    let workspace=match Flow.Workspace.check ~ops:Flow_sop.Operators.all Flow.Check.{version=1;kinds=[]}forms with
      |Some workspace,[]->workspace|_,ds->fail(String.concat "; "(List.map Diagnostic.to_string ds)) in
    let evaluation=Flow.Eval.static ~record:true ~inputs:["g",["geo",Flow.Eval.Deferred(Flow.Ty.geometry,7)]]workspace|>flow_ok in
    let value=List.assoc["g";"tested"]evaluation.records|>List.hd|>snd in
    let resolve=Flow_sop.Attribute_kernel.resolve ~geometry:(fun id->if id=7 then Some input else None) in
    let program=Flow_ir.Executor.compile ~strict:true value|>flow_ok in
    let live=Input.at_time 0. in
    let expected=Flow.Eval.Private.force_reference ~resolve value ~live in
    List.iter(fun domains->Parallel.run ~domains(fun()->
      let actual=Flow_ir.Executor.force ~strict:true ~resolve program ~live in
      check(actual=expected) "Metadata query strict/reference/domain results differ")) [1;8];
    expected
  let attr_type ~owner ~name input = query owner "sop/attr_type" name input
  let attr_names ~owner ~pattern input = query owner "sop/attr_names" pattern input
end

let string_ok = get_string_ok
let rdk_ok = get_string_ok
let add owner name value geometry =
  Geometry.with_attribute (Attribute.create_owned ~owner ~name (Attribute.Float [|value|]) |> string_ok) geometry |> string_ok
let fixture () =
  let geometry = Kernel.generate_point_ranges 1 (fun ~first:_ ~last:_ ~x ~y ~z->x.(0)<-0.;y.(0)<-0.;z.(0)<-0.) in
  geometry |> add Attribute.Point "source" 2. |> add Attribute.Point "target" 7.
    |> add Attribute.Detail "source" 11.
let names geometry = Geometry.Private.attributes geometry |> Array.map (fun attribute->Attribute.owner attribute,Attribute.name attribute)
let value owner name geometry = match Geometry.find_attribute ~owner name geometry with
  |Some attribute -> (match Attribute.Private.storage attribute with Attribute.Float values->values.(0)|_->assert false)
  |None -> assert false
let () =
  let input=fixture () in
  let original=geometry_bytes input in
  let skip=M.rename_attr ~owner:"point" ~pattern:"source" ~replacement:"target" ~conflict:"skip" input |> rdk_ok in
  check (skip==input) "skip conflict must retain exact unchanged geometry";
  check (match M.rename_attr ~owner:"point" ~pattern:"source" ~replacement:"target" ~conflict:"error" input with Error _->true|_->false) "error conflict must fail";
  check (original=geometry_bytes input) "error conflict mutated input";
  let overwrite=M.rename_attr ~owner:"point" ~pattern:"source" ~replacement:"target" ~conflict:"overwrite" input |> rdk_ok in
  check (value Attribute.Point "target" overwrite=2. && value Attribute.Detail "source" overwrite=11.) "overwrite crossed owner or retained destination payload";
  check (names overwrite=[|Attribute.Point,"target";Attribute.Detail,"source"|]) "overwrite changed stable source slot";
  check (match M.rename_attr ~owner:"point" ~pattern:"source" ~replacement:"P" ~conflict:"overwrite" input with Error _->true|_->false) "rename must reserve canonical P";
  let missed=M.rename_attr ~owner:"any" ~pattern:"not_found" ~replacement:"renamed" ~conflict:"error" input |> rdk_ok in
  check (missed==input) "pattern no-match must be a no-op";
  check (M.attr_type ~owner:Attribute.Point ~name:"P" input=Ok (Flow.Eval.Text "vec3")) "canonical P type missing";
  check (match M.attr_type ~owner:Attribute.Point ~name:"absent" input with Error diagnostic->diagnostic.code="E_ATTR_TYPE"|_->false) "type query fabricated missing storage";
  let grouped=Kernel.generate_point_ranges 4 (fun ~first ~last ~x ~y ~z->
    for i=first to last-1 do x.(i)<-float i;y.(i)<-0.;z.(i)<-0. done) in
  let ordered name elements = Group.ordered ~owner:Group.Point ~name ~length:4 elements |> string_ok in
  let grouped=grouped |> Geometry.with_group (ordered "source" [|1;2|]) |> string_ok
    |> Geometry.with_group (ordered "target" [|3;1|]) |> string_ok
    |> Geometry.with_group (ordered "unused" [||]) |> string_ok in
  let united=M.rename_group ~owner:"point" ~pattern:"source" ~replacement:"target" ~conflict:"union" grouped |> rdk_ok in
  check (Geometry.find_group ~owner:Group.Point "source" united=None) "union did not remove source";
  let target=Geometry.find_group ~owner:Group.Point "target" united |> Option.get in
  check (Group.ordered_elements target=Some [|3;1;2|]) "union lost destination-first preferred order";
  let deleted=M.delete_group ~owner:"point" ~pattern:"" ~unused:true grouped |> rdk_ok in
  check (Geometry.find_group ~owner:Group.Point "source" deleted<>None
    && Geometry.find_group ~owner:Group.Point "target" deleted<>None
    && Geometry.find_group ~owner:Group.Point "unused" deleted=None) "blank rules/delete_unused semantics changed";
  check (match M.rename_edge ~from:"missing" ~into:"missing" grouped with Error _->true|_->false) "equal missing exact edge name must fail";
  print_endline "metadata draft focused semantics passed"

let () =
  let positions=Packed.Float3.Private.of_owned_exn ~x:[|0.;1.;0.;2.|]
    ~y:[|0.;0.;1.;2.|] ~z:[|0.;0.;0.;0.|] in
  let topology=Topology.polygons_owned ~point_count:4 ~vertex_points:[|0;1;2|]
    ~primitive_offsets:[|0;3|] |> get_string_ok in
  let geometry=Geometry.create ~positions ~topology () |> get_string_ok in
  let owners=[Attribute.Point,4;Attribute.Vertex,3;Attribute.Primitive,1;Attribute.Detail,1] in
  let storages count =
    let plane value=Array.make count value in
    let offsets=Array.init (count+1) (fun i->i*16) in
    ["float",Attribute.Float (plane (-0.));
     "int",Attribute.Int (Array.make count max_int);
     "vec2",Attribute.Float2 (Packed.Float2.of_owned ~x:(plane (-0.)) ~y:(plane 2.) |> string_ok);
     "vec3",Attribute.Float3 (Packed.Float3.Private.of_owned_exn ~x:(plane 1.) ~y:(plane 2.) ~z:(plane 3.));
     "vec4",Attribute.Float4 (Packed.Float4.of_owned ~x:(plane 1.) ~y:(plane 2.) ~z:(plane 3.) ~w:(plane 4.) |> string_ok);
     "text",Attribute.Text (Array.make count "preserved ✓\n");
     "float_array",Attribute.Float_array (Packed.Float_array.create_owned ~offsets ~values:(Array.make (count*16) 0.25) |> string_ok);
     "int_array",Attribute.Int_array (Packed.Int_array.create_owned ~offsets ~values:(Array.make (count*16) min_int) |> string_ok)] in
  let geometry=List.fold_left (fun geometry (owner,count)->
    List.fold_left (fun geometry (kind,storage)->
      Geometry.with_attribute (Attribute.create_owned ~name:("a_"^kind) ~owner storage |> string_ok) geometry |> string_ok)
      geometry (storages count)) geometry owners in
  let before=geometry_bytes geometry in
  let renamed=M.rename_attr ~owner:"any" ~pattern:"a_*" ~replacement:"b_*" ~conflict:"error" geometry |> rdk_ok in
  List.iter (fun (owner,_) -> List.iter (fun kind ->
    let source=Geometry.find_attribute ~owner ("a_"^kind) geometry |> Option.get in
    let result=Geometry.find_attribute ~owner ("b_"^kind) renamed |> Option.get in
    check (Attribute.Private.storage source==Attribute.Private.storage result) "rename copied or changed attribute storage";
    check (M.attr_type ~owner ~name:("b_"^kind) renamed=Ok (Flow.Eval.Text kind)) "renamed type query differs")
    ["float";"int";"vec2";"vec3";"vec4";"text";"float_array";"int_array"]) owners;
  check (geometry_bytes geometry=before) "all-storage rename mutated source";
  (match M.attr_names ~owner:Attribute.Point ~pattern:"b_*" renamed |> (function Ok value->value|Error d->fail(Diagnostic.to_string d)) with
   |Flow.Eval.Text_array names -> names.(0)<-"mutated copy";
     check (Geometry.find_attribute ~owner:Attribute.Point "b_float" renamed<>None) "name query shared mutable geometry state"
   |_->assert false);
  print_endline "metadata draft: all owners/storage, copied queries and canonical P passed"

let () =
  let geometry=fixture () in
  List.iter(fun(method_,source_exists,source_value,target_value)->
    let result=M.swap ~owner:"point" ~source:"source" ~destination:"target" ~method_ geometry |> string_ok in
    check ((Geometry.find_attribute ~owner:Attribute.Point "source" result<>None)=source_exists)
      (method_^": source presence differs");
    Option.iter(fun expected->check(value Attribute.Point "source" result=expected)(method_^": source payload differs"))source_value;
    check(value Attribute.Point "target" result=target_value)(method_^": destination payload differs");
    check(value Attribute.Detail "source" result=11.)(method_^": crossed attribute owner"))
    ["copy",true,Some 2.,2.;"move",false,None,2.;"swap",true,Some 7.,2.];
  let copied=M.swap ~owner:"point" ~source:"P" ~destination:"rest" ~method_:"move" geometry |> string_ok in
  check (Geometry.positions copied==Geometry.positions geometry) "moving canonical P changed positions";
  check (Geometry.find_attribute ~owner:Attribute.Point "rest" copied<>None) "moving P must copy it";
  check (match M.swap ~owner:"point" ~source:"source" ~destination:"P" ~method_:"copy" geometry with
    Error _->true|Ok _->false) "scalar destination P must fail";
  check (geometry_bytes geometry=geometry_bytes(fixture())) "metadata mutated original input";
  print_endline "Metadata copy/move/swap preserve owner payloads and canonical P"

let () =
  let input=fixture () in
  let texts owner component=match Flow_sop.Attribute_metadata.entries ~owner ~component input with
    |Ok(Flow.Eval.Text_array values)->Array.to_list values|_->fail "attr_entries column is not text" in
  check(texts None "name"=["source";"target";"source"]) "attr_entries names must keep native attribute order";
  check(texts None "owner"=["point";"point";"detail"]) "attr_entries owners must follow the names";
  check(texts None "type"=["float";"float";"float"]) "attr_entries types must follow the names";
  check(texts (Some Attribute.Point) "name"=["source";"target"]) "attr_entries owner filter lost an attribute";
  check(texts (Some Attribute.Primitive) "name"=[]) "attr_entries must be empty for an absent owner";
  check(Result.is_error(Flow_sop.Attribute_metadata.entries ~owner:None ~component:"bogus" input)) "attr_entries accepted a bad column";
  print_endline "attr_entries: parallel name/type/owner columns keep native order and owner filter"

let () =
  let flow_ok=function Ok value->value|Error d->fail(Diagnostic.to_string d) in
  let catalog=Flow_sop.Catalog.of_factories ~version:1 Sop.Nodes.factories |>flow_ok in
  let checker=Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all in
  let forms=Flow.Syntax.parse {|(workspace metadata_projection (graph g :context sop
    (let* [geo (sop/line) renamed (sop/rename_attr geo "N" "normal" :owner point :conflict skip)
      swapped (sop/swap_attr renamed "normal" "backup" :owner vertex :method copy)
      names (sop/attr_names geo "*" :owner primitive) kind (sop/attr_type geo :P)
      entries (sop/attr_entries geo :owner primitive)
      grouped (sop/rename_group swapped "old*" "new*" :owner edge :conflict union)] grouped)))|} |>flow_ok in
  let checked=match checker catalog forms with Some checked,[]->checked
    |_,diagnostics->fail(String.concat "; "(List.map Diagnostic.to_string diagnostics)) in
  let pane=Flow_graph.Projection.of_graph catalog checked "g" in
  List.iter(fun(binding,ty)->let node=Flow_graph.Projection.find pane["g";binding] |>Option.get in
    check(node.ty=ty) "Metadata graph output type differs";
    check(List.exists(fun(row:Flow_graph.Projection.row)->row.label="owner")node.rows) "Metadata owner row missing")
    ["renamed",Flow.Ty.geometry;"swapped",Flow.Ty.geometry;"names",Flow.Ty.Array Text;"entries",Flow.Ty.Record["name",Array Text;"type",Array Text;"owner",Array Text];"kind",Flow.Ty.Text;"grouped",Flow.Ty.geometry];
  List.iter(fun(binding,key,value)->
    let _,checked=Flow_graph.Flow_edit.apply_checked ~checker catalog forms
      (Set_arg{node=["g";binding];key=Kw key;sub=[];value=Flow.Syntax.make(Sym value)}) |>flow_ok in
    let pane=Flow_graph.Projection.of_graph catalog checked "g" in
    let node=Flow_graph.Projection.find pane["g";binding] |>Option.get in
    let row=List.find(fun(row:Flow_graph.Projection.row)->row.label=key)node.rows in
    check(Flow.Lisp.flat(Option.get row.expr)=Printf.sprintf "%S" value) "Metadata gesture lost enum normalization")
    ["renamed","conflict","overwrite";"swapped","method","move";"grouped","owner","point"];
  print_endline "Metadata graph projection and enum gestures remain checked"
