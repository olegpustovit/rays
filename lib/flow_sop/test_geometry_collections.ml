open Rdk
open Rdk_test_support
let ok=function Ok value->value|Error message->failwith message
let flow=function Ok value->value|Error d->failwith(Diagnostic.to_string d)
let source x=Geometry.create ~positions:(Packed.Float3.View.of_owned_exn
  ~x:[|x|] ~y:[|-0.|] ~z:[|Float.next_after 0. 1.|]) ~topology:(Topology.empty ~point_count:1)()|>ok
let factory counter key geometry=Sop.Edit_graph.factory ~key ~label:key ~category:["Test"] ~arity:0(fun _->
  Sop.Node.Private.make_geometry ~operation:key ~version:1 ~parameters:(geometry_bytes geometry)
    ~cook_mode:Sop.Node.Generator ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
    (fun ~node_id:_ _ _->incr counter;Ok Sop.Node.Private.{geometry;instances=None}))
let ()=
  let count=5001 in
  let a=source 1. and b=source 2. and a_calls=ref 0 and b_calls=ref 0 in
  let factories=factory a_calls "collection_a" a::factory b_calls "collection_b" b::Sop.Nodes.factories in
  let many=String.concat " "(List.init count(fun i->if i mod 2=0 then "a" else "b")) in
  (* [parts] is a real checked geometry rest slot. A frame-dependent index
     keeps map live while preserving the fixed geometry network shape. *)
  List.iter (fun (case,body)->
  let text=Printf.sprintf {|(workspace geometry_collections
    (defnode sop/live_collection [(parts : geometry :rest true)]
      %s)
    (graph g :context sop (let* [a(sop/collection_a) b(sop/collection_b)]
      (sop/live_collection %s))))|} body many in
  let forms=Flow.Syntax.parse text|>flow in
  let expected=Mesh_merge.merge ~source_attribute:"__flow_src"
    (List.init count(fun i->if i mod 2=0 then a else b))|>ok in
  List.iter(fun reference->let lowered=Flow_sop.Lower.workspace ~reference ~factories forms|>flow in
    let graph=List.hd lowered.graphs in
    check(Flow_sop.Port.Map.for_all(fun port _->port.Flow_sop.Port.path<>"input")graph.network.drives)
      "A live geometry rest slot became a parameter drive";
    let node=Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)|>ok in
    List.iter(fun domains->List.iter(fun grain->
      let session=Sop.Session.create ~max_entries:64 ~max_payload_bytes:8_000_000|>ok in
      Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
        List.iter(fun time->Sop.Session.clear session;a_calls:=0;b_calls:=0;
          let context=Sop.Context.create ~domains ~grain ~time ~input:(Input.at_time time)()|>ok in
          let output=Sop.Session.cook session ~context node|>Result.map_error Diagnostic.error_to_string|>ok in
          let actual=Sop.Payload.geometry output.payload|>Result.map_error Diagnostic.error_to_string|>ok in
          check(geometry_bytes actual=geometry_bytes expected)
            (case^": live 5001 geometry collection truncated or reordered native payload blocks");
          check(!a_calls=1 && !b_calls=1)"repeated geometry list handles recooked their child";
          check(Packed.Float3.get(Geometry.positions a)0=(1.,-0.,Float.next_after 0. 1.))"source payload mutated")
          [0.;1.])) [1;4096]) [1;8]) [false;true])
    ["filter-map", "(let* [picked (filter (fn [(geo : geometry)] true) parts)] (sop/merge (map (fn [(geo : geometry)] (nth (list geo) (int/mod (frame/index) 1))) picked)))"];
  List.iter(fun(body,code)->
    let text=Printf.sprintf "(workspace forbidden (defnode sop/forbidden [(parts : geometry :rest true)] %s) (graph g :context sop (sop/forbidden (sop/line))))"body in
    let _,diagnostics=Flow_sop.Catalog.check_workspace ~sources:(Flow_sop.Catalog.sources ()|>flow) ~ops:Flow_sop.Operators.all
      (Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories|>flow)
      (Flow.Syntax.parse text|>flow)in
    check(List.exists(fun(d:Diagnostic.t)->d.code=code)diagnostics)
      "Geometry collection capacity relaxed the fixed-network time rules")
    ["(sop/merge (map (fn [(geo : geometry)] (if (< t 0.5) geo nil)) parts))","E_TIME_BRANCH";
     "(sop/merge (filter (fn [(geo : geometry)] (< t 0.5)) parts))","E_TIME_COUNT"];
  print_endline "Live geometry collections: 5001 rest/captured map and static-filter blocks, native bytes, single child cooks and unchanged time-shape refusals"
