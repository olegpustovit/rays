open Rdk
open Rdk_test_support
module E = Flow.Eval
module L = Flow_sop.Lower
module Edit = Sop.Edit_graph
let flow = function Ok value->value|Error diagnostic->failwith(Diagnostic.to_string diagnostic)
let parse source=Flow.Syntax.parse source |>flow
let factories=Sop_catalog.Editor.factories
let with_session fn=
  let session=Sop.Session.create ~max_entries:64 ~max_payload_bytes:64_000_000 |>get_string_ok in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->fn session)
let compile (lowered:L.t)=
  let graph=List.hd lowered.graphs in
  graph,Edit.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |>get_string_ok
let cook session ?(domains=1) ?(grain=97) ?(time=0.) node=
  let context=Sop.Context.create ~domains ~grain ~time () |>get_string_ok in
  Sop.Session.cook session ~context node
let geometry=function
  |Ok(output:Sop.Session.output)->Result.get_ok(Sop.Payload.geometry output.payload)
  |Error diagnostic->failwith(Diagnostic.error_to_string diagnostic)
let fields geometry=
  let topology=Topology.Private.view(Geometry.topology geometry)
  and positions=Packed.Float3.Private.view(Geometry.positions geometry)in
  ["points",E.Vec3_array(Array.init(Array.length positions.x*3)(fun i->
      match i mod 3 with 0->positions.x.(i/3)|1->positions.y.(i/3)|_->positions.z.(i/3)));
   "vertex_points",E.Int_array(Array.copy topology.vertex_points);
   "primitive_offsets",E.Int_array(Array.copy topology.primitive_offsets);
   "kinds",E.Int_array(Array.init(Bytes.length topology.primitive_kinds)(fun i->Char.code(Bytes.get topology.primitive_kinds i)))]
let fixtures ()=
  let make count vertices offsets kinds=
    let topology=Topology.create_owned ~point_count:count ~vertex_points:vertices
      ~primitive_offsets:offsets ~primitive_kinds:kinds |>get_string_ok in
    let positions=Packed.Float3.Private.of_owned_exn ~x:(Array.init count float)
      ~y:(Array.init count(fun i->if i mod 2=0 then -0. else 0.25)) ~z:(Array.make count 2.)in
    Geometry.create ~positions ~topology () |>get_string_ok in
  [make 0 [||][|0|][||];make 5 [|0;1;2;2;1;3|][|0;3;6|][|Topology.Polygon;Polygon|];
   make 3 [|0;1;1;2;1|][|0;5|][|Topology.Open_polyline|];
   make 4 [|3;2;1;0|][|0;4|][|Topology.Closed_polyline|];
   make 6 [|0;1;2;0;1;3;1;0;4;0;1;5|][|0;3;6;9;12|]
     [|Topology.Polygon;Open_polyline;Closed_polyline;Polygon|]]
let arrays_source={|(workspace arrays
  (graph g :context sop [(points : (array vec3) (array/vec3 0))
    (vertex_points : (array int) (array/int 0))
    (primitive_offsets : (array int) (array/int 1)) (kinds : (array int) (array/int 0))]
    (sop/mesh :points points :vertex_points vertex_points
      :primitive_offsets primitive_offsets :kinds kinds)))|}
let arrays ?(reference=false) values=
  L.workspace ~reference ~factories ~inputs:["g",values](parse arrays_source) |>flow |>compile |>snd
let rebuild={|(let* [topology (sop/topology geo)]
  (sop/mesh :points (sop/attr geo :P) :vertex_points topology.vertex_points
    :primitive_offsets topology.primitive_offsets :kinds topology.kinds))|}
let lower ?(reference=false) source input=
  let factory=Edit.factory ~key:"ext_src" ~label:"Source" ~category:["Test"] ~arity:0(fun _->input)in
  L.workspace ~reference ~factories:(factory::factories)(parse source) |>flow
let ()=
  List.iter(fun expected->let input=fields expected and before=geometry_bytes expected in
    List.iter(fun reference->let node=arrays ~reference input in
      List.iter(fun domains->List.iter(fun grain->with_session(fun session->
        let actual=geometry(cook session ~domains ~grain node)in
        check(geometry_bytes actual=before)"C1 own-array mesh differs from native fixture";
        let warm=Sop.Session.stats session in
        ignore(cook session ~domains ~grain node);
        check((Sop.Session.stats session).misses=warm.misses)"C1 unchanged mesh cache missed")) [1;97;4096]) [1;4;8]) [false;true];
    List.iter(fun reference->
      let source=Printf.sprintf {|(workspace authored
        (defnode sop/rebuild [(geo : geometry)] %s)
        (graph g :context sop (sop/rebuild (sop/ext_src))))|} rebuild in
      let _,node=lower ~reference source (Lisp_sop.snapshot expected) |>compile in
      List.iter(fun domains->with_session(fun session->
        check(geometry_bytes(geometry(cook session ~domains node))=before)
          "C1 strict/reference authored topology/P reconstruction differs")) [1;4;8]) [false;true];
    (* Geometry construction owns native planes, even when Flow input arrays
       remain referenced by the caller after the successful cook. *)
    with_session(fun session->let actual=geometry(cook session(arrays input))in
      List.iter(function _,E.Int_array values when Array.length values>0->values.(0)<-99
        |_,E.Vec3_array values when Array.length values>0->values.(0)<-99.|_->())input;
      check(geometry_bytes actual=before)"C1 mesh retained a mutable Flow argument")) (fixtures());
  print_endline "C1 five plain geometries rebuild from owned arrays and authored topology/P at one/four/eight domains"

let ()=
  let n=32769 in
  let expected=Line_geometry.points(Array.init n(fun i->
    float i,(if i mod 2=0 then -0. else Float.next_after 0. 1.),-2.)) in
  let source={|(workspace packed_mesh
    (defnode sop/packed_mesh [(geo : geometry)]
      (let* [topology (sop/topology geo)]
        (sop/mesh :points (exact (map (fn [(position : vec3)] position) (sop/attr geo :P)))
          :vertex_points topology.vertex_points
          :primitive_offsets topology.primitive_offsets :kinds topology.kinds)))
    (graph g :context sop (sop/packed_mesh (sop/ext_src))))|} in
  List.iter(fun reference->let _,node=lower ~reference source(Lisp_sop.snapshot expected)|>compile in
    List.iter(fun domains->List.iter(fun grain->with_session(fun session->
      check(geometry_bytes(geometry(cook session ~domains ~grain node))=geometry_bytes expected)
        "C1 multi-block packed point reconstruction differs")) [1;97;4096]) [1;4;8]) [false;true];
  print_endline "C1 32769-point authored packed mesh preserves signed-zero/subnormal bytes across domains and grains"

let ()=
  let expected=List.nth(fixtures())1 in
  let source={|(workspace mesh_capture
    (defn topology_of :context sop [(geo : geometry)] (sop/topology geo))
    (defnode sop/mesh_capture [(geo : geometry) (field : (fn [vec3] vec3))]
      (let* [topology (topology_of geo)]
        (sop/mesh :points (exact (map field (sop/attr geo :P)))
          :vertex_points topology.vertex_points
          :primitive_offsets topology.primitive_offsets :kinds topology.kinds)))
    (graph g :context sop
      (sop/mesh_capture (sop/ext_src) (fn [(p : vec3)] p))))|} in
  List.iter(fun reference->let _,node=lower ~reference source(Lisp_sop.snapshot expected) |>compile in
    List.iter(fun domains->with_session(fun session->
      check(geometry_bytes(geometry(cook session ~domains node))=geometry_bytes expected)
        "C1 helper/record/typed function capture changed reconstructed geometry")) [1;8]) [false;true]

let ()=
  let original=List.nth(fixtures())1 in
  let evaluate expression=
    let source="(workspace query (graph g :context sop [(geo : geometry nil)] (let* [tested "^expression^"] geo)))"in
    let catalog=Flow_sop.Catalog.of_factories ~version:1 factories |>flow in
    let checked=match Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all catalog(parse source)with
      |Some checked,[]->checked|_,diagnostics->failwith(String.concat ";"(List.map Diagnostic.to_string diagnostics))in
    let evaluation=E.static ~record:true ~inputs:["g",["geo",E.Deferred(Flow.Ty.geometry,7)]] checked |>flow in
    List.assoc["g";"tested"]evaluation.records |>List.hd |>snd in
  let resolver=Flow_sop.Attribute_kernel.resolve ~geometry:(fun id->if id=7 then Some original else None)in
  let queried=evaluate "(sop/topology geo)"in
  let live=Input.at_time 0. in
  let expected=E.Record(List.tl(fields original))in
  let result=E.Private.force_reference ~resolve:resolver queried ~live |>flow in
  check(result=expected)"C1 topology kind codes or plane order differ";
  (match result with E.Record fields->List.iter(function _,E.Int_array a->Array.fill a 0(Array.length a)99|_->assert false)fields|_->assert false);
  check(E.Private.force_reference ~resolve:resolver queried ~live |>flow = expected)"C1 topology query mutated native storage";
  let source=Lisp_sop.snapshot original in
  let origins=List.map(fun component->
    let captured="(let* [topology (sop/topology geo)] (map (fn [(i : int)] i) topology."^component^"))" in
    let direct="(map (fn [(i : int)] i) (get (sop/topology geo) :"^component^"))" in
    let origin expression=
    let values=evaluate expression in
    check(Flow_sop.Attribute_kernel.sources values=[7])"C1 topology capture lost its geometry dependency";
    let program=Flow_sop.Attribute_kernel.prepare ~strict:true ~sources:[7][source]values |>flow in
    let packed=Option.get(Flow_ir.Executor.packed_region program)in
    match Flow_ir.Packed.count_origin packed with Some origin->origin
    |None->fail("C1 topology count origin missing for "^expression) in
    let captured=origin captured and direct=origin direct in
    check(captured=direct)"C1 direct topology Get and captured field have different origins";
    captured)
    ["vertex_points";"primitive_offsets";"kinds"]in
  check(List.length(List.sort_uniq compare origins)=3)"C1 distinct topology plane counts share a false origin";
  List.iter(fun domains->Parallel.run ~domains(fun()->
    let program=Flow_ir.Executor.compile ~strict:true queried |>flow in
    check(Flow_ir.Executor.force ~strict:true ~resolve:resolver program ~live |>flow=expected)
      "C1 compiled topology differs from reference")) [1;8];
  (* A real compiled host source is refused by 2D export, rather than silently
     inventing browser geometry or routing through a native callback. *)
  let dynamic=evaluate "(if (= (frame/index) 0) (get (sop/topology geo) :kinds) (array/int 0))"in
  (match dynamic with E.Residual residual->
    let core=Flow_ir.Core_lower.lower ~strict:true residual |>get_string_ok in
    (match Flow_web.Js.frame core with Error d->check(d.code="E_WEB_SOURCE")"C1 cooked source changed web refusal"
      |Ok _->fail("C1 unexpectedly exported cooked geometry: "^Format.asprintf "%a" Flow_ir.Core.pp core))
    |_->fail "C1 dynamic test did not retain its source");
  print_endline "C1 topology copies, captured count origins and actual 2D host-source refusal are explicit"

let ()=
  let input=List.nth(fixtures())1 in
  let input=Geometry.with_attribute(Attribute.create_owned ~name:"unrelated" ~owner:Attribute.Point
    (Float(Array.make(Geometry.point_count input)0.)) |>get_string_ok)input |>get_string_ok in
  let values={|(workspace topology_cache (graph g :context sop [(geo : geometry nil)]
    (let* [tested (array/int 1 (array/count (get (sop/topology geo) :kinds)))] geo)))|} in
  let checked=match Flow.Workspace.check ~ops:Flow_sop.Operators.all
      {Flow.Check.version=1;kinds=[]}(parse values)with
    |Some checked,[]->checked|_,diagnostics->fail(String.concat ";"
        (List.map Diagnostic.to_string diagnostics)) in
  let value=E.static ~record:true ~inputs:["g",["geo",E.Deferred(Flow.Ty.geometry,7)]] checked |>flow
    |>fun evaluation->List.assoc["g";"tested"]evaluation.records |>List.hd |>snd in
  let node=Flow_sop.Attribute_kernel.node ~strict:true ~owner:Attribute.Detail ~kind:"int"
    ~source:"C1 topology" ~name:"primitive_count" ~values:value ~sources:[7] [Lisp_sop.snapshot input] in
  check((Sop.Node.facts node).reads=[])"C1 topology-only capture reads unrelated attributes";
  with_session(fun session->
    ignore(geometry(cook session node));
    let positions=Packed.Float3.Private.of_owned_exn ~x:(Array.make(Geometry.point_count input)8.)
      ~y:(Array.make(Geometry.point_count input)4.) ~z:(Array.make(Geometry.point_count input)2.) in
    let changed=Geometry.with_positions positions input |>get_string_ok in
    let changed=Geometry.with_attribute(Attribute.create_owned ~name:"unrelated" ~owner:Point
      (Float(Array.make(Geometry.point_count input)1.)) |>get_string_ok)changed |>get_string_ok in
    let source=Lisp_sop.snapshot changed in ignore(geometry(cook session source));
    let before=Sop.Session.stats session in
    let rebuilt source=Sop.Node.Private.adopt_identity ~source:node
      (Sop.Node.Private.rebuild_with_inputs node[|source|]) in
    let output=geometry(cook session(rebuilt source)) in
    check((Sop.Session.stats session).misses=before.misses)"C1 topology capture missed for position/attribute-only change";
    check(Packed.Float3.data_id(Geometry.positions output)=Packed.Float3.data_id positions)
      "C1 cached topology capture failed to carry fresh positions";
    (match Geometry.find_attribute ~owner:Point "unrelated" output |>Option.get |>Attribute.storage with
     |Float values->check(values=Array.make(Geometry.point_count input)1.)
         "C1 cached topology capture failed to carry fresh unread attribute values"|_->assert false);
    let changed=Geometry.with_attribute(Attribute.create_owned ~name:"added" ~owner:Point
      (Float(Array.make(Geometry.point_count input)2.)) |>get_string_ok)changed |>get_string_ok in
    let source=Lisp_sop.snapshot changed in ignore(geometry(cook session source));
    let before=Sop.Session.stats session in
    ignore(geometry(cook session(rebuilt source)));
    check((Sop.Session.stats session).misses=before.misses+1)
      "C1 changed attribute layout failed to invalidate the ordinary write cache";
    let changed=List.nth(fixtures())2 in
    let before=Sop.Session.stats session in
    let output=geometry(cook session(rebuilt(Lisp_sop.snapshot changed))) in
    check((Sop.Session.stats session).misses>before.misses)"C1 topology change did not invalidate query";
    match Geometry.find_attribute ~owner:Detail "primitive_count" output |>Option.get |>Attribute.storage with
    |Int values->check(values=[|1|])"C1 topology query carried stale count"|_->assert false)

let ()=
  let base=fields(List.nth(fixtures())1)in
  let replace key value=List.map(fun(name,old)->name,if name=key then value else old)base in
  let cases=["primitive_offsets",E.Int_array[||],"Topology: primitive offsets must contain the initial zero";
    "primitive_offsets",E.Int_array[|1;3;6|],"Topology: first primitive offset must be zero";
    "primitive_offsets",E.Int_array[|0;3;5|],"Topology: final primitive offset must equal vertex count";
    "kinds",E.Int_array[|0|],"Topology: primitive kind count must match primitive count";
    "vertex_points",E.Int_array[|0;1;2;2;1;9007199254740993|],"Topology: vertex references an invalid point";
    "primitive_offsets",E.Int_array[|0;7;6|],"Topology: primitive offsets are not ordered";
    "primitive_offsets",E.Int_array[|0;2;6|],"Topology: primitive has too few vertices for its kind"]in
  List.iter(fun(key,value,message)->List.iter(fun reference->
    let node=arrays ~reference(replace key value)in
    List.iter(fun domains->with_session(fun session->match cook session ~domains node with
      |Error d->check(d.code="E_TOPOLOGY" && d.message=message && d.cause=None)"C1 native topology error changed"
      |Ok _->fail "C1 malformed topology succeeded")) [1;8]) [false;true])cases;
  List.iter(fun kind->with_session(fun session->match cook session(arrays(replace "kinds"(E.Int_array[|kind;0|])))with
    |Error d->check(d.code="E_TOPOLOGY_KIND")"C1 invalid kind code diagnostic"
    |Ok _->fail "C1 invalid kind code succeeded")) [-1;3;9007199254740993];
  let resolver=Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->None) in
  List.iter(fun geometry->match resolver(E.Source("sop/topology",Flow.Ty.Array Int,
      ["geometry",geometry;"component",E.Text"vertex_points"]))with
    |Error d->check(d.code="E_DATA_SOURCE")"C1 missing/wrong geometry source changed diagnostic"
    |Ok _->fail "C1 unavailable geometry succeeded")
    [E.No_geo;E.Deferred(Flow.Ty.geometry,7);E.Deferred(Flow.Ty.Float,7)];
  List.iter(fun domains->with_session(fun session->
    let cancel=Sop.Context.Cancel.create() in Sop.Context.Cancel.cancel cancel;
    let context=Sop.Context.create ~domains ~cancel () |>get_string_ok in
    match Sop.Session.cook session ~context(arrays base)with
    |Error d->check(d.code="cancelled" || d.code="E_CANCELLED")"C1 cancelled constructor changed diagnostic"
    |Ok _->fail "C1 cancelled mesh succeeded")) [1;8];
  let source body=Printf.sprintf {|(workspace lazy_mesh
    (defnode sop/lazy_mesh [(geo : geometry)] %s)
    (graph g :context sop (sop/lazy_mesh (sop/ext_src))))|} body in
  let original=List.nth(fixtures())1 in
  List.iter(fun reference->
    let _,healthy=lower ~reference(source("(if false (sop/mesh :points (sop/attr geo \"missing\") :vertex_points (array/int 0) :primitive_offsets (array/int 0) :kinds (array/int 0)) "^rebuild^")"))(Lisp_sop.snapshot original) |>compile in
    with_session(fun session->check(geometry_bytes(geometry(cook session healthy))=geometry_bytes original)"C1 untaken mesh forced bad input");
    let _,bad=lower ~reference(source {|(sop/mesh :points (error "E_POINTS" "selected points" :cause "point cause")
      :vertex_points (array/int 0) :primitive_offsets (array/int 0) :kinds (array/int 0))|})(Lisp_sop.snapshot original) |>compile in
    with_session(fun session->match cook session bad with
      |Error d->check(d.code="E_POINTS" && d.message="selected points" && d.cause=Some"point cause")"C1 point error order/cause changed"
      |Ok _->fail "C1 selected point failure succeeded")) [false;true];
  print_endline "C1 native topology errors, exact Int validation, lazy untaken mesh and selected causes retain cook timing"

let ()=
  let catalog=Flow_sop.Catalog.of_factories ~version:1 factories |>flow in
  let checker=Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all in
  let forms=parse {|(workspace cards (graph g :context sop
    (let* [src (sop/line :points 2) topology (sop/topology src)
      pts (array/vec3 0) vp (array/int 0) po (array/int 1) ks (array/int 0)
      mesh (sop/mesh :points pts :vertex_points vp :primitive_offsets po :kinds ks)] mesh)))|}in
  let checked=match checker catalog forms with Some checked,[]->checked|_->fail "C1 projection fixture did not check"in
  let projection=Flow_graph.Projection.of_graph catalog checked "g"in
  let mesh=Flow_graph.Projection.find projection["g";"mesh"] |>Option.get
  and topology=Flow_graph.Projection.find projection["g";"topology"] |>Option.get in
  check(List.map(fun(row:Flow_graph.Projection.row)->row.label)mesh.rows=["points";"vertex_points";"primitive_offsets";"kinds"])"C1 mesh card rows";
  check(topology.outputs=["vertex_points",Flow.Ty.Array Int;"primitive_offsets",Array Int;"kinds",Array Int])"C1 topology card outputs";
  List.iter(fun key->let value=parse(if key="points"then"(array/vec3 0)"else if key="primitive_offsets"then"(array/int 1)"else"(array/int 0)") |>List.hd in
    ignore(flow(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
      (Set_arg{node=["g";"mesh"];key=Kw key;sub=[];value})))) ["points";"vertex_points";"primitive_offsets";"kinds"];
  ignore(flow(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Connect{node=["g";"mesh"];key=Kw"vertex_points";src="vp";iter=false})));
  let before=fst(Flow.Lisp.print forms)in
  check(Result.is_error(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"mesh"];key=Kw"kinds";sub=[];value=parse"(array/text 0)" |>List.hd})))"C1 malformed edit accepted";
  check(fst(Flow.Lisp.print forms)=before)"C1 malformed edit changed prior source";
  check(Option.is_some(Flow_graph.Flow_edit.default_for(Flow.Ty.Array Int)"value"))"C1 Int array socket has no typed default";
  let starter=List.hd(parse "(sop/mesh :points (array/vec3 0) :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0))") in
  ignore(flow(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Add_node{scope=["g"];name="empty_mesh";expr=starter})));
  let node=Lisp_sop.node(Flow.Lisp.flat starter) in
  with_session(fun session->check(geometry_bytes(geometry(cook session node))=geometry_bytes(List.hd(fixtures())))
    "C1 menu starter failed to cook the native empty mesh");
  List.iter(fun source->let _,ds=checker catalog(parse source)in
    check(ds<>[])"C1 missing/wrong keyword checked")
    ["(workspace bad (graph g :context sop (sop/mesh :points (array/vec3 0))))";
     "(workspace bad (graph g :context sop (sop/topology 3)))"];
  print_endline "C1 ordinary cards, typed defaults and checked keyword/connection edits retain prior source on refusal"

let ()=
  List.iter(fun expression->List.iter(fun reference->
    let source=Printf.sprintf {|(workspace animated (graph g :context sop
      (let* [coordinate %s]
        (sop/mesh :points (exact (map (fn [(i : int)] [coordinate 0.0 0.0]) (array/range 1 :type int)))
          :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0)))))|} expression in
    let lowered=L.workspace ~reference ~factories(parse source) |>flow in
    let graph=List.hd lowered.graphs in
    let state=E.create_state()in
    let lane=Flow_sop.Value_lane.create ~state ()in
    with_session(fun session->List.iter(fun(time,dt)->
      let live={(Input.at_time time)with dt;frame=int_of_float time;tick=int_of_float time}in
      let resolved=Flow_sop.Value_lane.resolve ~live lane ~time graph.network |>flow in
      let node=Edit.compile_node resolved.geometry ~node_id:(Option.get graph.root) |>get_string_ok in
      let stamp=E.state_stamp state in
      let context=Sop.Context.create ~domains:8 ~time ~input:live () |>get_string_ok in
      let actual=geometry(Sop.Session.cook session ~context node)in
      let expected=if expression="t"then time else dt in
      check(Packed.Float3.get(Geometry.positions actual)0=(expected,0.,0.))"C1 stale frame/state array";
      check(E.state_stamp state=stamp)"C1 cooked snapshot leaked mutable host state") [0.,0.25;1.,0.5;2.,0.125])) [false;true])
    ["t";"(state [previous 0.0] (+ previous (frame/dt)))"];
  let source={|(workspace mesh_zone (graph g :context sop
    (sop/merge (for [p (sop/point_list (sop/ext_src))]
      (sop/mesh :points (array/vec3 1 p) :vertex_points (array/int 0)
        :primitive_offsets (array/int 1) :kinds (array/int 0))))))|}in
  let expected=Line_geometry.points[|0.,0.,0.;2.,0.,0.;4.,0.,0.|]in
  List.iter(fun reference->let _,node=lower ~reference source(Lisp_sop.snapshot expected) |>compile in
    List.iter(fun domains->with_session(fun session->let actual=geometry(cook session ~domains node)in
      check(fields actual=fields expected)"C1 zone mesh lost element/state capture")) [1;8]) [false;true];
  print_endline "C1 retained live/state arrays and zone element captures stay byte-exact"

let ()=
  let source={|(workspace rollback (graph g :context sop
    (let* [step (state [previous 0.0] (+ previous (frame/dt)))]
      (sop/mesh :points (exact (map (fn [(i : int)]
        (if (> step 0.0) (error "E_MESH_STATE" "selected state" :cause "state cause") [0.0 0.0 0.0]))
        (array/range 1 :type int))) :vertex_points (array/int 0)
        :primitive_offsets (array/int 1) :kinds (array/int 0)))))|} in
  List.iter(fun reference->
    let lowered=L.workspace ~reference ~factories(parse source) |>flow in
    let graph=List.hd lowered.graphs and state=E.create_state() in
    let lane=Flow_sop.Value_lane.create ~state () in
    let live={(Input.at_time 1.)with dt=0.5;frame=1;tick=1} in
    let network=Flow_sop.Value_lane.resolve ~live lane ~time:1. graph.network |>flow in
    let node=Edit.compile_node network.geometry ~node_id:(Option.get graph.root) |>get_string_ok in
    let stamp=E.state_stamp state in
    with_session(fun session->List.iter(fun domains->
      let context=Sop.Context.create ~domains ~time:1. ~input:live () |>get_string_ok in
      match Sop.Session.cook session ~context node with
      |Error d->check(d.code="E_MESH_STATE" && d.cause=Some"state cause")"C1 selected state cause changed";
          check(E.state_stamp state=stamp)"C1 failed mesh advanced supplied host state"
      |Ok _->fail "C1 selected state error succeeded") [1;8])) [false;true]

(* Separate typed sockets avoid changing Any/error or polymorphic with_attr
   rules. Both public forms reach the same immutable topology boundary. *)
let plane_type=Flow.Ty.Record(List.map(fun name->name,Flow.Ty.Array Flow.Ty.Float)["x";"y";"z"])
let plane_fields geometry=
  let p=Packed.Float3.Private.view(Geometry.positions geometry)in
  ("points",E.Record["x",E.Float_array(Array.copy p.x);"y",E.Float_array(Array.copy p.y);"z",E.Float_array(Array.copy p.z)])
    ::List.tl(fields geometry)
let plane_arrays_source={|(workspace planes
  (graph g :context sop [(points : {:x (array float) :y (array float) :z (array float)}
    {:x (array/float 0) :y (array/float 0) :z (array/float 0)})
    (vertex_points : (array int) (array/int 0))
    (primitive_offsets : (array int) (array/int 1)) (kinds : (array int) (array/int 0))]
    (sop/mesh_planes :points points :vertex_points vertex_points
      :primitive_offsets primitive_offsets :kinds kinds)))|}
let plane_arrays ?(reference=false) values=
  L.workspace ~reference ~factories ~inputs:["g",values](parse plane_arrays_source)|>flow|>compile|>snd
let plane_rebuild={|(let* [topology (sop/topology geo) positions (sop/attr geo :P)]
  (sop/mesh_planes :points
    {:x (exact (map (fn [(p : vec3)] p.x) positions))
     :y (exact (map (fn [(p : vec3)] p.y) positions))
     :z (exact (map (fn [(p : vec3)] p.z) positions))}
    :vertex_points topology.vertex_points :primitive_offsets topology.primitive_offsets :kinds topology.kinds))|}
let ()=
  let large=Line_geometry.points(Array.init 65537(fun i->
    float i,(if i mod 2=0 then -0. else Float.next_after 0. 1.),-2.))in
  List.iter(fun expected->let before=geometry_bytes expected in
    List.iter(fun reference->
      let args=plane_fields expected in
      let node=plane_arrays ~reference args in
      let source=Printf.sprintf {|(workspace rebuilt_planes
        (defnode sop/rebuilt_planes [(geo : geometry)] %s)
        (graph g :context sop (sop/rebuilt_planes (sop/ext_src))))|}plane_rebuild in
      let _,rebuilt=lower ~reference source(Lisp_sop.snapshot expected)|>compile in
      List.iter(fun domains->List.iter(fun grain->with_session(fun session->
        List.iter(fun node->check(geometry_bytes(geometry(cook session ~domains ~grain node))=before)
          "Mesh planes native/reference/multi-block bytes differ") [node;rebuilt];
        let misses=(Sop.Session.stats session).misses in
        ignore(cook session ~domains ~grain node);
        check((Sop.Session.stats session).misses=misses)"Mesh planes warm cache missed")) [1;97;4096]) [1;4;8];
      with_session(fun session->let actual=geometry(cook session node)in
        (match List.assoc "points" args with E.Record planes->List.iter(function
          |_,E.Float_array values->Array.fill values 0(Array.length values)99.|_->assert false)planes
          |_->assert false);
        List.iter(function _,E.Int_array values->Array.fill values 0(Array.length values)99|_->())args;
        check(geometry_bytes actual=before)"Mesh planes borrowed mutable caller buffers")) [false;true])
    (fixtures()@[large]);
  print_endline "Mesh planes all five plain topologies and 65537-point authored maps own exact planes at 1/4/8 domains"
let ()=
  let base=plane_fields(List.nth(fixtures())1)in
  let replace key value=List.map(fun(name,old)->name,if key=name then value else old)base in
  let bad=["points",E.Record["x",E.Float_array[||];"y",E.Float_array[|0.|];"z",E.Float_array[||]],"E_ARRAY_COUNT";
    "primitive_offsets",E.Int_array[||],"E_TOPOLOGY";
    "vertex_points",E.Int_array[|9007199254740993;1;2;2;1;3|],"E_TOPOLOGY";
    "kinds",E.Int_array[|3;0|],"E_TOPOLOGY_KIND"]in
  List.iter(fun(key,value,code)->List.iter(fun reference->List.iter(fun domains->with_session(fun session->
    match cook session ~domains(plane_arrays ~reference(replace key value))with
    |Error d->check(d.code=code)"Mesh planes malformed storage changed error code"
    |Ok _->fail "Mesh planes malformed storage succeeded")) [1;4;8]) [false;true])bad;
  List.iter(fun axis->List.iter(fun bad->let p=Packed.Float3.Private.view(Geometry.positions(List.nth(fixtures())1))in
    let point_fields=List.map(fun(name,values)->let values=Array.copy values in
      if name=axis then values.(0)<-bad;name,E.Float_array values)["x",p.x;"y",p.y;"z",p.z]in
    List.iter(fun reference->match L.workspace ~reference ~factories
      ~inputs:["g",replace "points"(E.Record point_fields)](parse plane_arrays_source)with
      |Error d->check(d.code="E_NONFINITE")"Mesh planes input validation changed nonfinite refusal"
      |Ok _->fail "Mesh planes accepted nonfinite input arrays") [false;true]) [nan;infinity;neg_infinity]) ["x";"y";"z"];
  List.iter(fun reference->let original=List.nth(fixtures())1 in
    let source body=Printf.sprintf {|(workspace lazy_planes
      (defnode sop/lazy_planes [(geo : geometry)] %s)
      (graph g :context sop (sop/lazy_planes (sop/ext_src))))|}body in
    let _,node=lower ~reference(source("(if false (sop/mesh_planes :points (error \"E_UNTAKEN\" \"untaken\") :vertex_points (array/int 0) :primitive_offsets (array/int 0) :kinds (array/int 0)) "^plane_rebuild^")"))(Lisp_sop.snapshot original)|>compile in
    with_session(fun session->check(geometry_bytes(geometry(cook session node))=geometry_bytes original)
      "Mesh planes forced untaken argument");
    let _,node=lower ~reference(source {|(sop/mesh_planes :points
      {:x (error "E_X" "selected x" :cause "x cause") :y (array/float 0) :z (array/float 0)}
      :vertex_points (error "E_VP" "later vertices") :primitive_offsets (array/int 1) :kinds (array/int 0))|})
      (Lisp_sop.snapshot original)|>compile in
    with_session(fun session->match cook session node with
      |Error d->check(d.code="E_X" && d.message="selected x" && d.cause=Some"x cause")
        "Mesh planes selected error cause or field forcing order changed"
      |Ok _->fail "Mesh planes selected error succeeded")) [false;true];
  (* The storage proof is separate from allocation: scalar collectors retain
     the existing per-plane bound, without allocating an impossible huge test. *)
  let n=Sys.max_floatarray_length/3+1 in
  check(n<=Sys.max_floatarray_length && n*3>Sys.max_floatarray_length)
    "Mesh planes failed to remove the interleaved representation ceiling";
  check(Flow.Op.Private.array_count "array/float"(E.Int Sys.max_floatarray_length)=Sys.max_floatarray_length)
    "Mesh planes narrowed native scalar capacity";
  (try ignore(Flow.Op.Private.array_count "array/float"(E.Int(Sys.max_floatarray_length+1)));
      fail "Scalar point plane exceeded native array capacity"
   with Diagnostic.Error d -> check(d.code="E_ARRAY_RANGE")"Scalar plane capacity error changed");
  print_endline "Mesh planes finite/count/topology errors, lazy causes and exact scalar capacity retain native boundaries"
let ()=
  let catalog=Flow_sop.Catalog.of_factories ~version:1 factories|>flow in
  let checker=Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all in
  let source={|(workspace planes_cards (graph g :context sop
    (let* [coords {:x (array/float 0) :y (array/float 0) :z (array/float 0)}
      built (sop/mesh_planes :points {:x (array/float 0) :y (array/float 0) :z (array/float 0)} :vertex_points (array/int 0)
        :primitive_offsets (array/int 1) :kinds (array/int 0))] built)))|}in
  let forms=parse source in
  let checked=match checker catalog forms with Some checked,[]->checked|_->fail "Mesh planes card source did not check"in
  let card=Flow_graph.Projection.of_graph catalog checked "g"|>fun p->Flow_graph.Projection.find p["g";"built"]|>Option.get in
  check(List.map(fun(row:Flow_graph.Projection.row)->row.label)card.rows=["points";"vertex_points";"primitive_offsets";"kinds"])"Mesh planes required card rows changed";
  let row=List.find(fun(row:Flow_graph.Projection.row)->row.label="points")card.rows in
  check(row.ty=Some plane_type && row.socket)"Mesh planes card has an unconstrained points socket";
  check(Option.is_some(Flow_graph.Flow_edit.default_for plane_type "points"))"Mesh planes record socket has no typed default";
  let edit expression=Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"built"];key=Kw"points";sub=[];value=parse expression|>List.hd})in
  ignore(flow(edit "{:x (array/float 1) :y (array/float 1) :z (array/float 1)}"));
  ignore(flow(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"built"];key=Kw"points";sub=[1];value=parse"(array/float 0)"|>List.hd})));
  ignore(flow(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Connect{node=["g";"built"];key=Kw"points";src="coords";iter=false})));
  List.iter(fun key->ignore(flow(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"built"];key=Kw key;sub=[];
      value=parse(if key="primitive_offsets"then"(array/int 1)"else"(array/int 0)")|>List.hd}))))
    ["vertex_points";"primitive_offsets";"kinds"];
  let printed=fst(Flow.Lisp.print forms)in
  List.iter(fun expression->check(Result.is_error(edit expression))"Mesh planes malformed points edit accepted")
    ["(array/vec3 0)";"{:x (array/float 0) :y (array/float 0)}";
     "{:x (array/int 0) :y (array/float 0) :z (array/float 0)}";
     "{:x (array/float 0) :y (array/float 0) :z (array/float 0) :extra (array/float 0)}"];
  check(fst(Flow.Lisp.print forms)=printed)"Mesh planes malformed edit changed prior source";
  let saved=parse printed in
  ignore(L.workspace ~factories saved|>flow);
  let saved_checked=match checker catalog saved with Some checked,[]->checked|_->fail "Mesh planes saved source did not check"in
  check(saved_checked.source=saved)"Mesh planes source save embedded implementation definitions";
  let starter=parse "(sop/mesh_planes :points {:x (array/float 0) :y (array/float 0) :z (array/float 0)} :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0))"|>List.hd in
  ignore(flow(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Add_node{scope=["g"];name="empty_planes";expr=starter})));
  with_session(fun session->check(geometry_bytes(geometry(cook session(Lisp_sop.node(Flow.Lisp.flat starter))))=geometry_bytes(List.hd(fixtures())))
    "Mesh planes insertion starter does not construct empty geometry");
  print_endline "Mesh planes typed record cards, defaults, insertion, source save and malformed edit rollback are ordinary gestures"
let ()=
  List.iter(fun expression->List.iter(fun reference->
    let source=Printf.sprintf {|(workspace animated_planes (graph g :context sop
      (let* [coordinate %s]
        (sop/mesh_planes :points
          {:x (exact (map (fn [(i : int)] coordinate) (array/range 1 :type int)))
           :y (array/float 1) :z (array/float 1)}
          :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0)))))|}expression in
    let lowered=L.workspace ~reference ~factories(parse source)|>flow in
    let graph=List.hd lowered.graphs and state=E.create_state()in
    let lane=Flow_sop.Value_lane.create ~state ()in
    with_session(fun session->List.iter(fun(time,dt)->
      let live={(Input.at_time time)with dt;frame=int_of_float time;tick=int_of_float time}in
      let network=Flow_sop.Value_lane.resolve ~live lane ~time graph.network|>flow in
      let node=Edit.compile_node network.geometry ~node_id:(Option.get graph.root)|>get_string_ok in
      let stamp=E.state_stamp state in
      let context=Sop.Context.create ~domains:8 ~time ~input:live ()|>get_string_ok in
      let actual=geometry(Sop.Session.cook session ~context node)in
      check(Packed.Float3.get(Geometry.positions actual)0=((if expression="t"then time else dt),0.,0.))
        "Mesh planes retained live/state coordinate became stale";
      check(E.state_stamp state=stamp)"Mesh planes cook mutated retained state") [0.,0.25;1.,0.5;2.,0.125])) [false;true])
    ["t";"(state [previous 0.0] (+ previous (frame/dt)))"];
  let source={|(workspace plane_zone (graph g :context sop
    (sop/merge (for [p (sop/point_list (sop/ext_src))]
      (sop/mesh_planes :points {:x (array/float 1 p.x) :y (array/float 1 p.y) :z (array/float 1 p.z)}
        :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0))))))|}in
  let expected=Line_geometry.points[|0.,0.,0.;2.,0.,0.;4.,0.,0.|]in
  List.iter(fun reference->let _,node=lower ~reference source(Lisp_sop.snapshot expected)|>compile in
    List.iter(fun domains->with_session(fun session->check(fields(geometry(cook session ~domains node))=fields expected)
      "Mesh planes lost retained zone element")) [1;8]) [false;true];
  print_endline "Mesh planes retained live/state and zone element captures preserve constructor behavior"
let ()=
  let original=List.nth(fixtures())1 in
  let captured={|(workspace captured_planes
    (defn topology_planes :context sop [(geo : geometry)] (sop/topology geo))
    (defnode sop/captured_planes [(geo : geometry) (field : (fn [vec3] vec3))]
      (let* [topology (topology_planes geo) p (sop/attr geo :P)]
        (sop/mesh_planes :points
          {:x (exact (map (fn [(v : vec3)] (let* [mapped (field v)] mapped.x)) p))
           :y (exact (map (fn [(v : vec3)] (let* [mapped (field v)] mapped.y)) p))
           :z (exact (map (fn [(v : vec3)] (let* [mapped (field v)] mapped.z)) p))}
          :vertex_points topology.vertex_points :primitive_offsets topology.primitive_offsets :kinds topology.kinds)))
    (graph g :context sop (sop/captured_planes (sop/ext_src) (fn [(v : vec3)] v))))|}in
  List.iter(fun reference->let _,node=lower ~reference captured(Lisp_sop.snapshot original)|>compile in
    List.iter(fun domains->with_session(fun session->check(geometry_bytes(geometry(cook session ~domains node))=geometry_bytes original)
      "Mesh planes lost typed Fn/helper/source capture")) [1;8]) [false;true];
  List.iter(fun domains->with_session(fun session->
    let cancel=Sop.Context.Cancel.create()in Sop.Context.Cancel.cancel cancel;
    let context=Sop.Context.create ~domains ~cancel ()|>get_string_ok in
    match Sop.Session.cook session ~context(plane_arrays(plane_fields original))with
    |Error d->check(d.code="cancelled" || d.code="E_CANCELLED")"Mesh planes cancellation changed"
    |Ok _->fail "Cancelled mesh planes succeeded")) [1;8];
  let source={|(workspace plane_rollback (graph g :context sop
    (let* [step (state [previous 0.0] (+ previous (frame/dt)))]
      (sop/mesh_planes :points
        {:x (exact (map (fn [(i : int)] (if (> step 0.0)
          (error "E_PLANE_STATE" "selected plane state" :cause "plane state cause") 0.0)) (array/range 1 :type int)))
         :y (array/float 1) :z (array/float 1)}
        :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0)))))|}in
  List.iter(fun reference->let lowered=L.workspace ~reference ~factories(parse source)|>flow in
    let graph=List.hd lowered.graphs and state=E.create_state()in
    let lane=Flow_sop.Value_lane.create ~state ()in
    let live={(Input.at_time 1.)with dt=0.5;frame=1;tick=1}in
    let network=Flow_sop.Value_lane.resolve ~live lane ~time:1. graph.network|>flow in
    let node=Edit.compile_node network.geometry ~node_id:(Option.get graph.root)|>get_string_ok in
    let stamp=E.state_stamp state in
    with_session(fun session->List.iter(fun domains->
      let context=Sop.Context.create ~domains ~time:1. ~input:live ()|>get_string_ok in
      match Sop.Session.cook session ~context node with
      |Error d->check(d.code="E_PLANE_STATE" && d.cause=Some"plane state cause")"Mesh planes state error lost cause";
        check(E.state_stamp state=stamp)"Mesh planes failure advanced retained host state"
      |Ok _->fail "Mesh planes selected state error succeeded") [1;8])) [false;true];
  print_endline "Mesh planes helper/Fn captures, cancellation and failed cooked state preserve the existing boundary"
let ()=
  let source={|(workspace plane_origins (graph g :context sop [(geo : geometry nil)]
    (let* [p (sop/attr geo :P) coords
      {:x (exact (map (fn [(point : vec3)] point.x) p))
       :y (exact (map (fn [(point : vec3)] point.y) p))
       :z (exact (map (fn [(point : vec3)] point.z) p))}] geo)))|}in
  let catalog=Flow_sop.Catalog.of_factories ~version:1 factories|>flow in
  let checked=match Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all catalog(parse source)with
    |Some checked,[]->checked|_,ds->fail(String.concat ";"(List.map Diagnostic.to_string ds))in
  let evaluation=E.static ~record:true ~inputs:["g",["geo",E.Deferred(Flow.Ty.geometry,7)]]checked|>flow in
  let coordinates=List.assoc["g";"coords"]evaluation.records|>List.hd|>snd in
  check(Flow_sop.Attribute_kernel.sources coordinates=[7])"Mesh plane record capture lost geometry dependency";
  check(Flow_sop.Attribute_kernel.reads coordinates=["P"])"Mesh plane capture lost precise position read facts";
  let input=Lisp_sop.snapshot(List.nth(fixtures())1)in
  let origins=match coordinates with E.Record planes->List.map(fun(_,value)->
    let program=Flow_sop.Attribute_kernel.prepare ~strict:true ~sources:[7]
      [input]value|>flow in
    check(Flow_ir.Executor.packed_region program=None)
      "Exact coordinate unexpectedly became a direct packed root";
    check(Flow_ir.Executor.root_is_packed_or_readback program)
      "Exact coordinate lost its packed readback root";
    let packed=Option.get(Flow_ir.Executor.packed_root program)in
    Flow_ir.Packed.count_origin packed|>Option.get)planes|_->assert false in
  check(List.length(List.sort_uniq compare origins)=1)"Coordinate fields acquired false independent owner counts";
  print_endline "Mesh planes keep precise P read facts, source ownership and a shared point-owner count through captured records"

let ()=
  (* Count-then-fill offsets use both the initial zero and terminal total.
     Compaction remains an ordinary index array used by generic gather maps. *)
  let source={|(workspace compact_mesh
    (defnode sop/compact_mesh []
      (let* [points (array/vec3 6 [1 -0. 2])
        mask (exact (map (fn [(i : int)] (if (int/eq (int/mod i 2) 0) 1 0)) (array/range 6 :type int)))
        kept (array/compact mask)
        selected (exact (map (fn [(i : int)] (array/nth points i)) kept))
        counts (array/int 1 3)]
        (sop/mesh :points selected :vertex_points (array/range 3 :type int)
          :primitive_offsets (array/scan_sum counts) :kinds (array/int 1 0))))
    (graph g :context sop (sop/compact_mesh)))|}in
  let topology=Topology.create_owned ~point_count:3 ~vertex_points:[|0;1;2|]
    ~primitive_offsets:[|0;3|] ~primitive_kinds:[|Topology.Polygon|]|>get_string_ok in
  let positions=Packed.Float3.Private.of_owned_exn ~x:[|1.;1.;1.|] ~y:[|-0.;-0.;-0.|] ~z:[|2.;2.;2.|]in
  let expected=Geometry.create ~positions ~topology ()|>get_string_ok|>geometry_bytes in
  List.iter(fun reference->let _,node=L.workspace ~reference ~factories(parse source)|>flow|>compile in
    List.iter(fun domains->List.iter(fun grain->with_session(fun session->
      check(geometry_bytes(geometry(cook session ~domains ~grain node))=expected)
        "C4 authored compact/gather and scan/mesh composition differs")) [1;97;4096]) [1;4;8]) [false;true];
  print_endline "C4 authored compact→gather and exclusive scan→mesh offsets preserve native geometry bytes"

let ()=
  let invoke name args=(Option.get(Flow.Op.find name Flow.Context.value)).body
    ~live:(Input.at_time 0.) ~node:(fun _ _->assert false)args in
  let int_attribute name geometry=match Geometry.find_attribute ~owner:Attribute.Point name geometry with
    |Some attribute->(match Attribute.Private.storage attribute with Attribute.Int a->a|_->assert false)
    |None->assert false in
  let texts=[|"b";"a";"b";"\255\128";"x\000y";"é日本😀";"\240\144\128\128";"\238\128\128"|]in
  List.iter(fun(storage,keys)->let count=match keys with
    |E.Int_array a->Array.length a|E.Float_array a->Array.length a|E.Text_array a->Array.length a|_->assert false in
    let source=Line_geometry.points(Array.init count(fun i->float i,0.,0.))in
    let attribute=Attribute.create_owned ~owner:Attribute.Point ~name:"key" storage|>get_string_ok in
    let source=Geometry.with_attribute attribute source|>get_string_ok in
    let before=geometry_bytes source in
    List.iter(fun domains->Parallel.run ~domains(fun()->List.iter(fun descending->
      let permutation=match invoke "array/grade"["keys",keys;"descending",E.Bool descending]with
        |E.Int_array a->a|_->assert false in
      let ranks=Array.make count 0 in Array.iteri(fun rank source->ranks.(source)<-rank)permutation;
      let native=Ordering.sort ~grain:1 ~owner:Ordering.Points
        ~key:(Ordering.Attribute_component{name="key";component=0}) ~descending
        ~output_indices:"rank" source|>get_rdk in
      check(ranks=int_attribute "rank" native)"Grade stable ranks differ from native Ordering.sort";
      check(geometry_bytes source=before)"Grade changed native key geometry") [false;true]))[1;4;8];
    (match keys with
     |E.Int_array _|E.Text_array _->
       let first=invoke "array/index_of"["keys",keys;"queries",keys]in
       let first_indices=match first with E.Int_array a->a|_->assert false in
       let mask=Array.mapi(fun i position->if i=position then 1 else 0)first_indices in
       let compact=invoke "array/compact"["mask",E.Int_array mask]in
       let dense=invoke "array/index_of"["keys",compact;"queries",first]in
       List.iter(fun domains->Parallel.run ~domains(fun()->
         let native=Attribute_ops.enumerate ~grain:1 ~piece_attribute:"key"
           ~mode:Attribute_ops.Enumerate_pieces ~owner:Attribute.Point ~name:"dense" source|>get_rdk in
         check(dense=E.Int_array(int_attribute "dense" native))
           "Index_of first occurrence/compact composition differs from native piece IDs"))[1;4;8]
     |_->()))
    [Attribute.Int[|max_int;min_int;max_int;9007199254740993;min_int|],
       E.Int_array[|max_int;min_int;max_int;9007199254740993;min_int|];
     Attribute.Float[|0.;-0.;1.;1.;Float.next_after 0. 1.|],
       E.Float_array[|0.;-0.;1.;1.;Float.next_after 0. 1.|];
     Attribute.Text texts,E.Text_array texts];
  print_endline "C5 native consumer oracles: stable scalar ranks and first-occurrence piece IDs, without full SOP mode claims"
