module E = Flow.Eval
module G = Rdk.Geometry
module T = Rdk.Topology
let ok = function Ok value -> value | Error message -> failwith message
let flow_ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)

let fixtures () =
  let topology points vertex_points primitive_offsets primitive_kinds =
    T.create_owned ~point_count:points ~vertex_points ~primitive_offsets ~primitive_kinds |> ok in
  ["empty", T.empty ~point_count:0;
   "shared-polygons", topology 5 [|0;1;2;2;1;3|] [|0;3;6|] [|T.Polygon;Polygon|];
   "open-repeated", topology 3 [|0;1;1;2;1|] [|0;5|] [|T.Open_polyline|];
   "closed", topology 4 [|3;2;1;0|] [|0;4|] [|T.Closed_polyline|];
   "mixed-nonmanifold", topology 6 [|0;1;2;0;1;3;1;0;4;0;1;5|]
     [|0;3;6;9;12|] [|T.Polygon;Open_polyline;Closed_polyline;Polygon|]]
  |> List.map (fun (name, topology) ->
    let count = T.point_count topology in
    let positions = Rdk.Packed.Float3.Private.of_owned_exn
      ~x:(Array.init count float) ~y:(Array.make count 0.) ~z:(Array.make count 0.) in
    name, (G.create ~positions ~topology () |> ok))

let checked body =
  let source = "(workspace topology (graph g :context sop [(geo : geometry nil)] (let* [tested " ^ body ^ "] geo)))" in
  let forms = Flow.Syntax.parse source |> flow_ok in
  match Flow.Workspace.check ~ops:Flow_sop.Operators.all {Flow.Check.version=1;kinds=[]} forms with
  | Some checked, [] -> checked
  | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))

let evaluate geometry body =
  let value = Flow.Eval.static ~record:true ~inputs:["g",["geo",E.Deferred (Flow.Ty.geometry,7)]] (checked body)
    |> flow_ok |> fun evaluation -> List.assoc ["g";"tested"] evaluation.records |> List.hd |> snd in
  let resolve = Flow_sop.Attribute_kernel.resolve ~geometry:(fun id -> if id=7 then Some geometry else None) in
  value, resolve

let () =
  List.iter (fun (name, geometry) ->
    let index = Rdk.Topology_index.create (G.topology geometry) in
    let view = Rdk.Topology_index.Private.view index in
    List.iter (fun (element, expected) ->
      let value, resolve = evaluate geometry ("(sop/count geo :" ^ element ^ ")") in
      let live=Input.at_time 0. in
      let program=Flow_ir.Executor.compile ~strict:true value |> flow_ok in
      List.iter(fun domains->Parallel.run ~domains(fun()->
        assert(flow_ok(E.Private.force_reference ~resolve value ~live)=E.Int expected);
        assert(flow_ok(Flow_ir.Executor.force ~strict:true ~resolve program ~live)=E.Int expected))) [1;8])
      ["points",G.point_count geometry;"vertices",G.vertex_count geometry;
       "primitives",G.primitive_count geometry;"edges",Rdk.Topology_index.edge_count index];
    let value, resolve = evaluate geometry "(sop/edges geo)" in
    let expected = E.Record ["a",E.Int_array (Array.copy view.edge_a);"b",E.Int_array (Array.copy view.edge_b)] in
    List.iter (fun domains -> Parallel.run ~domains (fun () ->
      let live = Input.at_time 0.25 in
      let result = flow_ok (E.Private.force_reference ~resolve value ~live) in
      assert (result=expected);
      let core = Flow_ir.Executor.compile ~strict:true value |> flow_ok in
      assert (flow_ok (Flow_ir.Executor.force ~strict:true ~resolve core ~live)=expected);
      (match result with E.Record [_,E.Int_array a;_,E.Int_array b] ->
        if Array.length a>0 then begin a.(0)<-99;b.(0)<-99 end | _ -> assert false);
      assert (flow_ok (E.Private.force_reference ~resolve value ~live)=expected))) [1;8];
    let value, resolve = evaluate geometry
      "(map (fn [(x : int)] (int/add x (frame/index))) (get (sop/edges geo) :a))" in
    let core = Flow_ir.Executor.compile ~strict:true value |> flow_ok in
    let live = {(Input.at_time 0.25) with frame=3} in
    let expected = E.Int_array (Array.map (fun x -> x+3) view.edge_a) in
    List.iter (fun domains -> Parallel.run ~domains (fun () ->
      assert (flow_ok (E.Private.force_reference ~resolve value ~live)=expected);
      assert (flow_ok (Flow_ir.Executor.force ~strict:true ~resolve core ~live)=expected))) [1;8];
    let value, resolve = evaluate geometry
      "(let* [n (sop/count geo :points)] (array/int n 9007199254740993))" in
    let core = Flow_ir.Executor.compile ~strict:true value |> flow_ok in
    let expected = E.Int_array (Array.make (G.point_count geometry) 9007199254740993) in
    assert (flow_ok (Flow_ir.Executor.force ~strict:true ~resolve core ~live)=expected);
    Printf.printf "Topology %s: counts and stable unique-edge order match RDK at one/eight domains\n" name)
    (fixtures ())

let () =
  List.iter(fun(_,geometry)->List.iter(fun(owner,element,native_owner,count)->
    let text=Printf.sprintf {|(workspace topology_node
      (defnode sop/topology_node [(geo : geometry)]
        (sop/with_attr geo :indices (array/int (sop/count geo :%s) 9007199254740993) :owner %s :type int))
      (graph g :context sop (sop/topology_node (sop/ext_source))))|} element owner in
    let node=Lisp_sop.workspace_node ~with_:["source",Lisp_sop.snapshot geometry] text in
    let expected=G.with_attribute(Rdk.Attribute.create_owned ~owner:native_owner ~name:"indices"
      (Int(Array.make(count geometry)9007199254740993)) |>ok)geometry |>ok in
    List.iter(fun domains->
      let session=Sop.Session.create ~max_entries:16 ~max_payload_bytes:1_000_000 |>ok in
      Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
        let context=Sop.Context.create ~domains () |>ok in
        match Sop.Session.cook session ~context node with
        | Ok output -> assert(Rdk_test_support.geometry_bytes(Result.get_ok(Sop.Payload.geometry output.payload))
            =Rdk_test_support.geometry_bytes expected)
        | Error diagnostic -> failwith(Diagnostic.error_to_string diagnostic))) [1;8])
    ["point","points",Rdk.Attribute.Point,G.point_count;
     "vertex","vertices",Vertex,G.vertex_count;
     "primitive","primitives",Primitive,G.primitive_count]) (fixtures());
  print_endline "Strict authored topology counts build exact integer attributes for every element owner at one/eight domains"

let () =
  let catalog=Flow_sop.Catalog.of_factories ~version:1 Sop_catalog.Editor.factories |> flow_ok in
  let checker=Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all in
  let forms=Flow.Syntax.parse {|(workspace topology_projection (graph g :context sop
    (let* [geo (sop/line) n (sop/count geo :points) edges (sop/edges geo)] geo)))|} |> flow_ok in
  let checked=match checker catalog forms with Some checked,[]->checked | _->assert false in
  let projection=Flow_graph.Projection.of_graph catalog checked "g" in
  let node=Flow_graph.Projection.find projection ["g";"n"] |> Option.get in
  assert(node.ty=Flow.Ty.Int);
  assert(List.map(fun(row:Flow_graph.Projection.row)->row.label)node.rows=["geometry";"element"]);
  let edges=Flow_graph.Projection.find projection ["g";"edges"] |> Option.get in
  assert(edges.ty=Flow.Ty.Record["a",Array Int;"b",Array Int]);
  assert(edges.outputs=["a",Flow.Ty.Array Int;"b",Flow.Ty.Array Int]);
  let edited,checked=Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"n"];key=Pos 1;sub=[];value=Flow.Syntax.make(Kw "edges")}) |> flow_ok in
  let text=fst(Flow.Lisp.print edited) in
  assert(String.contains text ':');
  let evaluation=E.static ~record:true checked |> flow_ok in
  let source=List.assoc ["g";"n"] evaluation.records |> List.hd |> snd in
  (match source with E.Source("sop/count",Flow.Ty.Int,args)->assert(List.assoc "element" args=E.Text "edges")
    |_->assert false);
  List.iter(fun text->let _,diagnostics=checker catalog (Flow.Syntax.parse text |> flow_ok) in
    assert(List.exists(fun(d:Diagnostic.t)->d.severity=Diagnostic.Error_severity)diagnostics))
    ["(workspace bad (graph g :context sop (sop/count nil :detail)))";
     "(workspace bad (graph g :context sop (sop/count nil 2)))";
     "(workspace bad (graph g :context sop (sop/edges 2)))"];
  let refused code = function Error(d:Diagnostic.t)->assert(d.code=code)|Ok _->assert false in
  let missing=Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->None) in
  refused "E_DATA_SOURCE" (missing(E.Source("sop/count",Flow.Ty.Int,
    ["geometry",E.Deferred(Flow.Ty.geometry,7);"element",E.Text"points"])));
  refused "E_DATA_SOURCE" (missing(E.Source("sop/edges",Flow.Ty.Array Int,
    ["geometry",E.No_geo;"component",E.Text"a"])));
  let geometry=List.assoc "shared-polygons" (fixtures()) in
  let resolve=Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some geometry) in
  refused "E_TOPOLOGY_COUNT" (resolve(E.Source("sop/count",Flow.Ty.Int,
    ["geometry",E.Deferred(Flow.Ty.geometry,7);"element",E.Text"detail"])));
  refused "E_DATA_SOURCE" (resolve(E.Source("sop/edges",Flow.Ty.Array Int,
    ["geometry",E.Deferred(Flow.Ty.geometry,7);"component",E.Text"unknown"])));
  print_endline "Topology cards expose exact Int/edge fields; direct keyword edits recheck and invalid selectors/sources refuse"

let () =
  let geometry=List.assoc "shared-polygons" (fixtures()) in
  let geometry=G.with_attribute(Rdk.Attribute.create_owned ~name:"unrelated" ~owner:Rdk.Attribute.Point
    (Float(Array.make(G.point_count geometry)0.)) |>ok)geometry |>ok in
  let values,_=evaluate geometry "(array/int 1 (sop/count geo :edges))" in
  assert(Flow_sop.Attribute_kernel.sources values=[7]);
  let source=Lisp_sop.snapshot geometry in
  let node=Flow_sop.Attribute_kernel.node ~strict:true ~owner:Rdk.Attribute.Detail ~kind:"int"
    ~source:"topology-count" ~name:"edge_count" ~values ~sources:[7] [source] in
  assert((Sop.Node.facts node).reads=[]);
  let cooked session node = match Sop.Session.cook session ~context:(Sop.Context.create() |> ok) node with
    | Ok output -> Sop.Payload.geometry output.payload |> Result.get_ok
    | Error d -> failwith(Diagnostic.error_to_string d) in
  let edge_count geometry=Rdk.Topology_index.edge_count(Rdk.Topology_index.create(G.topology geometry)) in
  let check output expected =
    let values=G.find_attribute ~owner:Rdk.Attribute.Detail "edge_count" output |> Option.get
      |>Rdk.Attribute.storage |>function Rdk.Attribute.Int values->values |_->assert false in
    assert(values=[|edge_count expected|]) in
  let session=Sop.Session.create ~max_entries:16 ~max_payload_bytes:1_000_000 |>ok in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    check(cooked session node)geometry;
    let positions=Rdk.Packed.Float3.Private.of_owned_exn ~x:(Array.make(G.point_count geometry)2.)
      ~y:(Array.make(G.point_count geometry)3.) ~z:(Array.make(G.point_count geometry)4.) in
    let changed=G.with_positions positions geometry |>ok in
    let changed=G.with_attribute(Rdk.Attribute.create_owned ~name:"unrelated" ~owner:Rdk.Attribute.Point
      (Float(Array.make(G.point_count geometry)1.)) |>ok)changed |>ok in
    let source=Lisp_sop.snapshot changed in
    ignore(cooked session source);
    let before=Sop.Session.stats session in
    let rebuilt source=Sop.Node.Private.adopt_identity ~source:node
      (Sop.Node.Private.rebuild_with_inputs node [|source|]) in
    let output=cooked session(rebuilt source) in
    let after=Sop.Session.stats session in
    assert(after.misses=before.misses && after.hits=before.hits+2);
    check output changed;
    assert(Rdk.Packed.Float3.data_id(G.positions output)=Rdk.Packed.Float3.data_id positions);
    assert(G.find_attribute ~owner:Point "unrelated" output<>None);
    let changed=List.assoc "open-repeated" (fixtures()) in
    let before=Sop.Session.stats session in
    let output=cooked session(rebuilt(Lisp_sop.snapshot changed)) in
    assert((Sop.Session.stats session).misses>before.misses);
    check output changed);
  print_endline "Topology count captures ignore unrelated attributes/positions, carry fresh components, and invalidate on topology changes"
