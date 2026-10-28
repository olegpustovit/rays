module E = Flow.Eval
module G = Rdk.Geometry
module Group = Rdk.Group
module Edge = Rdk.Edge_group
let ok = function Ok x -> x | Error message -> failwith message
let flow_ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let bytes = Rdk_test_support.geometry_bytes
let owners = ["point",Some Group.Point;"vertex",Some Vertex;"primitive",Some Primitive;"edge",None]
let count geometry = function
  | Some Group.Point -> G.point_count geometry | Some Vertex -> G.vertex_count geometry
  | Some Primitive -> G.primitive_count geometry
  | None -> Rdk.Topology_index.edge_count(Rdk.Topology_index.create(G.topology geometry))
let fixtures () =
  [Rdk.Topology.empty ~point_count:0;
   Rdk.Topology.create_owned ~point_count:4 ~vertex_points:[|0;1;2;2;1;0|]
     ~primitive_offsets:[|0;3;6|] ~primitive_kinds:[|Rdk.Topology.Polygon;Open_polyline|] |> ok]
  |>List.map(fun topology->let n=Rdk.Topology.point_count topology in
    G.create ~positions:(Rdk.Packed.Float3.View.of_owned_exn ~x:(Array.init n float)
      ~y:(Array.make n 0.) ~z:(Array.make n 0.)) ~topology () |>ok)
let native_write ?order ~owner ~name mask geometry = match owner with
  | None -> assert(order=None);let topology=G.topology geometry in
      G.with_edge_group(Edge.init ~topology ~index:(Rdk.Topology_index.create topology) ~name
        (fun i->mask.(i)=1))geometry |>ok
  | Some owner ->let group=match order with
      | None->Group.init ~owner ~name (Array.length mask)(fun i->mask.(i)=1)
      | Some order->Group.ordered ~owner ~name ~length:(Array.length mask) order |>ok in
      G.with_group group geometry |>ok
let query geometry owner name component =
  Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some geometry)
    (E.Source((if component=None then "sop/group" else "sop/group_order"),
      Flow.Ty.Any,["geometry",E.Deferred(Flow.Ty.geometry,7);"owner",E.Text owner;"name",E.Text name]
        @Option.fold ~none:[] ~some:(fun component->["component",E.Text component])component)) |>flow_ok
let checked body =
  let forms=Flow.Syntax.parse("(workspace groups (graph g :context sop [(geo : geometry nil)] (let* [tested "^body^"] geo)))") |>flow_ok in
  match Flow.Workspace.check ~ops:Flow_sop.Operators.all {Flow.Catalog.version=1;kinds=[]} forms with
  | Some checked,[]->checked |_,ds->failwith(String.concat "; " (List.map Diagnostic.to_string ds))
let evaluated geometry body =
  let evaluated=E.static ~record:true ~inputs:["g",["geo",E.Deferred(Flow.Ty.geometry,7)]](checked body) |>flow_ok in
  let value=List.assoc["g";"tested"]evaluated.records |>List.hd |>snd in
  value,Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some geometry)
let exact geometry body expected =
  let value,resolve=evaluated geometry body in
  let program=Flow_ir.Executor.compile ~strict:true value |>flow_ok in
  List.iter(fun domains->Parallel.run ~domains(fun()->
    let live=Input.at_time 0.25 in
    assert(flow_ok(E.Host.force_reference ~resolve value ~live)=expected);
    assert(flow_ok(Flow_ir.Executor.force ~strict:true ~resolve program ~live)=expected))) [1;8]
let create_session ()=Sop.Session.create ~max_entries:32 ~max_payload_bytes:1_000_000 |>ok
let cook session ?(time=0.) domains node =
  match Sop.Session.cook session ~context:(Sop.Context.create ~domains ~time () |>ok)node with
  | Ok output->Sop.Payload.geometry output.payload |>Result.get_ok
  | Error d->failwith(Diagnostic.error_to_string d)
let int_array data=Array.fold_left(fun array value->
  "(array/concat "^array^" (array/int 1 "^string_of_int value^"))") "(array/int 0 0)" data
let write_node geometry owner mask ?order () =
  Lisp_sop.node ~with_:["source",Lisp_sop.snapshot geometry]
    ("(sop/with_group (sop/ext_source) "^int_array mask^" :name \"selected\" :owner "^owner^
      Option.fold ~none:"" ~some:(fun order->" :order "^order)order^")")
let () =
  List.iter(fun geometry->List.iter(fun(owner,native_owner)->
    let n=count geometry native_owner in
    let mask=Array.init n(fun i->if i mod 2=0 then 1 else 0) in
    let expected=native_write ~owner:native_owner ~name:"selected" mask geometry in
    let node=write_node geometry owner mask () in
    assert((Sop.Node.facts node).cook_mode=Generic);
    assert((Sop.Node.facts node).reads=["*"] && (Sop.Node.facts node).writes=["*"]);
    let session=create_session() in
    Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
      List.iter(fun domains->let output=cook session domains node in
        assert(bytes output=bytes expected);
        exact output ("(sop/group geo :name \"selected\" :owner "^owner^")") (E.Int_array mask);
        exact output ("(sop/group_order geo :name \"selected\" :owner "^owner^")")
          (E.Record["ordered",E.Bool false;"indices",E.Int_array[||]]);
        let copy=Lisp_sop.workspace_node ~with_:["source",Lisp_sop.snapshot output]
          ("(workspace groups (defnode sop/test_mask_roundtrip [(geo : geometry)] (sop/with_group geo "^
            "(exact (map (fn [(value : int)] value) (sop/group geo :name \"selected\" :owner "^owner^"))) "^
            ":name \"selected\" :owner "^owner^" :order (sop/group_order geo :name \"selected\" :owner "^owner^"))) "^
            "(graph g :context sop (sop/test_mask_roundtrip (sop/ext_source))))") in
        assert(bytes(cook session domains copy)=bytes output)) [1;8]);
    (match native_owner with None->() |Some _->
      let selected=Array.to_list(Array.mapi(fun i value->i,value)mask) |>List.filter_map(fun(i,value)->if value=1 then Some i else None)
        |>List.rev |>Array.of_list in
      List.iter(fun(mask,order)->
        let expected=native_write ~owner:native_owner ~name:"selected" ~order mask geometry in
        let node=write_node geometry owner mask ~order:(int_array order)() in
        let session=create_session() in
        Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
          List.iter(fun domains->let output=cook session domains node in
            assert(bytes output=bytes expected);
            exact output ("(sop/group_order geo :name \"selected\" :owner "^owner^")")
              (E.Record["ordered",E.Bool true;"indices",E.Int_array order]);
            let copy=Lisp_sop.node ~with_:["source",Lisp_sop.snapshot output]
              ("(let* [geo (sop/ext_source)] (sop/with_group geo (sop/group geo :name \"selected\" :owner "^owner^") "^
                ":name \"selected\" :owner "^owner^" :order (sop/group_order geo :name \"selected\" :owner "^owner^")))") in
            assert(bytes(cook session domains copy)=bytes output)) [1;8]))
        [mask,selected;Array.make n 0,[||]]);
    assert(query expected owner "selected" None=E.Int_array mask);
    let owned=query expected owner "selected" None in
    (match owned with E.Int_array data->Array.fill data 0 n 0|_->assert false);
    assert(query expected owner "selected" None=E.Int_array mask)) owners)(fixtures());
  print_endline "Groups: native owner masks, stable/empty order and unordered round trips, strict queries, owned arrays and one/eight-domain bytes"

let () =
  let geometry=List.nth(fixtures())1 in
  let names=["wall_a";"wall_b";"wall_1";"floor";"literal*";"žarek"] in
  List.iter(fun(owner,native_owner)->
    let geometry=List.fold_left(fun g name->native_write ~owner:native_owner ~name
      (Array.make(count geometry native_owner)1)g)geometry names in
    List.iter(fun pattern->let compiled=Rdk.Attribute_pattern.compile pattern |>ok in
      exact geometry ("(sop/group_names geo "^fst(Flow.Lisp.print[Flow.Syntax.make(Str pattern)])^
        " :owner "^owner^")")
        (E.Text_array(Array.of_list(List.filter(Rdk.Attribute_pattern.matches compiled)names))))
      ["";"*";"wall*";"wall?";"wall_[a-b]";"wall_[!0-9]";"* ^wall*";"^wall* wall_b";"literal\\*";"ž*"])
    owners;
  let session=create_session() in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    let refused code node=match Sop.Session.cook session ~context:(Sop.Context.create() |>ok)node with
      |Error d->assert(d.code=code)|Ok _->assert false in
    List.iter(fun(owner,native_owner)->let n=count geometry native_owner in
      refused "E_GROUP_COUNT" (write_node geometry owner (Array.make(n+1)0)());
      refused "E_GROUP_MASK" (write_node geometry owner (Array.make n 2)());
      (match native_owner with
       |None->refused "E_GROUP_OWNER" (write_node geometry owner (Array.make n 0)~order:"(array/int 0 0)"())
       |Some _->List.iter(fun order->refused "E_GROUP_ORDER"
          (write_node geometry owner (Array.make n 1)~order:(int_array order)())) [[|n|];[|0;0|];[||]])) owners);
  let missing geometry=Flow_sop.Attribute_kernel.resolve ~geometry in
  let source name args=E.Source(name,Flow.Ty.Any,("geometry",E.Deferred(Flow.Ty.geometry,7))::args) in
  let refused code=function Error(d:Diagnostic.t)->assert(d.code=code)|Ok _->assert false in
  refused "E_DATA_SOURCE"(missing(fun _->None)(source "sop/group"["name",E.Text "missing"]));
  refused "E_GROUP_MISSING"(missing(fun _->Some geometry)(source "sop/group"["name",E.Text "missing"]));
  refused "E_GROUP_OWNER"(missing(fun _->Some geometry)(source "sop/group"["name",E.Text "missing";"owner",E.Text "detail"]));
  List.iter(fun pattern->refused "E_GROUP_PATTERN"
    (missing(fun _->Some geometry)(source "sop/group_names"["pattern",E.Text pattern]))) ["[";"\\";"[z-a]"];
  print_endline "Groups: native wildcard/escape/exclusion/name order and count/mask/order/owner/source refusals"

let () =
  let catalog=Flow_sop.Catalog.of_factories ~version:1 Sop.Nodes.factories |>flow_ok in
  let checker=Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all in
  let forms=Flow.Syntax.parse {|(workspace group_projection (graph g :context sop
    (let* [geo (sop/line) mask (sop/group geo :name "selected" :owner point)
      names (sop/group_names geo "wall*" :owner vertex)
      order (sop/group_order geo :name "selected" :owner primitive)
      result (sop/with_group geo mask :name "copy" :owner point)] result)))|} |>flow_ok in
  let checked=match checker catalog forms with Some checked,[]->checked|_,ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds)) in
  let pane=Flow_graph.Projection.of_graph catalog checked "g" in
  List.iter(fun(binding,ty)->let node=Flow_graph.Projection.find pane["g";binding] |>Option.get in
    assert(node.ty=ty);assert(List.exists(fun(row:Flow_graph.Projection.row)->row.label="owner")node.rows))
    ["mask",Flow.Ty.Array Int;"names",Array Text;"order",Record["ordered",Bool;"indices",Array Int];"result",Flow.Ty.geometry];
  List.iter(fun binding->
    let _,checked=Flow_graph.Flow_edit.apply_checked ~checker catalog forms
      (Set_arg{node=["g";binding];key=Kw "owner";sub=[];value=Flow.Syntax.make(Sym "edge")}) |>flow_ok in
    let pane=Flow_graph.Projection.of_graph catalog checked "g" in
    let node=Flow_graph.Projection.find pane["g";binding] |>Option.get in
    let row=List.find(fun(row:Flow_graph.Projection.row)->row.label="owner")node.rows in
    assert(Flow.Lisp.flat(Option.get row.expr)="\"edge\"")) ["mask";"names";"order";"result"];
  let forms=Flow.Syntax.parse {|(workspace group_wire (graph g :context sop
    (let* [geo (sop/line) mask (sop/group geo :name "selected" :owner point)
      result (sop/with_group geo (array/int 0 0) :name "copy" :owner point)] result)))|} |>flow_ok in
  ignore(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"result"];key=Pos 1;sub=[];value=Flow.Syntax.make(Sym "mask")}) |>flow_ok);
  List.iter(fun text->let _,diagnostics=checker catalog(Flow.Syntax.parse text |>flow_ok) in
    assert(List.exists(fun(d:Diagnostic.t)->d.severity=Diagnostic.Error_severity)diagnostics))
    ["(workspace bad (graph g :context sop (sop/with_group nil (array/int 0 0) :name \"x\" :order 2)))";
     "(workspace bad (graph g :context sop (sop/group nil :name \"\")))";
     "(workspace bad (graph g :context sop (sop/group nil :name \"x\" :owner detail)))"];
  print_endline "Group cards expose typed masks/name arrays/order records; owner and mask gestures recheck directly"

let () =
  let geometry=List.nth(fixtures())1 in
  let n=G.point_count geometry in
  let source mask=native_write ~owner:(Some Group.Point) ~name:"selected" mask geometry in
  let first=source(Array.make n 1) and second=source(Array.make n 0) in
  let node=Lisp_sop.node ~with_:["target",Lisp_sop.snapshot geometry;"source",Lisp_sop.snapshot first]
    "(sop/with_group (sop/ext_target) (sop/group (sop/ext_source) :name \"selected\") :name \"copy\")" in
  assert(List.length(Sop.Node.inputs node)=2);
  let expected=native_write ~owner:(Some Group.Point) ~name:"copy"(Array.make n 1)geometry in
  let session=create_session() in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    assert(bytes(cook session 1 node)=bytes expected);
    let before=Sop.Session.stats session in
    assert(bytes(cook session 8 node)=bytes expected);
    assert((Sop.Session.stats session).misses=before.misses);
    let inputs=Array.of_list(Sop.Node.inputs node) in
    inputs.(1)<-Lisp_sop.snapshot second;
    let changed=Sop.Node.Private.adopt_identity ~source:node(Sop.Node.Private.rebuild_with_inputs node inputs) in
    let expected=native_write ~owner:(Some Group.Point) ~name:"copy"(Array.make n 0)geometry in
    assert(bytes(cook session 8 changed)=bytes expected);
    assert((Sop.Session.stats session).misses>before.misses));
  let values,_=evaluated first "(sop/group geo :name \"selected\")" in
  let read=Flow_sop.Attribute_kernel.node ~owner:Rdk.Attribute.Point ~kind:"int" ~source:"group-query"
    ~name:"mark" ~values ~sources:[7] [Lisp_sop.snapshot first] in
  assert((Sop.Node.facts read).reads=[]);
  let session=create_session() in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    let assert_mask output mask=match G.find_attribute ~owner:Point "mark"output |>Option.get |>Rdk.Attribute.storage with
      |Int data->assert(data=mask)|_->assert false in
    assert_mask(cook session 1 read)(Array.make n 1);
    let before=Sop.Session.stats session in
    let changed=Sop.Node.Private.adopt_identity ~source:read
      (Sop.Node.Private.rebuild_with_inputs read [|Lisp_sop.snapshot second|]) in
    assert_mask(cook session 8 changed)(Array.make n 0);
    assert((Sop.Session.stats session).misses>before.misses));
  let n=5001 in
  let geometry=G.create ~positions:(Rdk.Packed.Float3.View.of_owned_exn
    ~x:(Array.make n 0.) ~y:(Array.make n 0.) ~z:(Array.make n 0.))
    ~topology:(Rdk.Topology.empty ~point_count:n)() |>ok in
  let node=Lisp_sop.workspace_node ~with_:["source",Lisp_sop.snapshot geometry] {|(workspace live_groups
    (defnode sop/test_live_group [(geo : geometry)]
      (sop/with_group geo (exact (map (fn [(value : int)] (if (< t 0.5) value 0))
        (array/int (sop/count geo :points) 1))) :name "selected"))
    (graph g :context sop (sop/test_live_group (sop/ext_source))))|} in
  List.iter(fun domains->let session=create_session() in
    Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
      List.iter(fun(time,selected)->let output=cook session ~time domains node in
        assert(bytes output=bytes(native_write ~owner:(Some Group.Point) ~name:"selected"
          (Array.make n selected)geometry))) [0.,1;0.75,0;0.25,1])) [1;8];
  print_endline "Groups: captured geometry and group changes invalidate caches; warm hits and live strict multi-block masks preserve native bytes"

let () =
  let forms=Flow.Syntax.parse {|(workspace folded_group (graph g :context sop
    (let* [geo (sop/line) counter (state [previous 0] (int/add previous 1))]
      (sop/with_group geo (exact (map (fn [(value : int)]
        (if (int/eq (int/mod counter 2) 1) value 0)) (array/int (sop/count geo :points) 1)))
        :name "selected"))))|} |>flow_ok in
  List.iter(fun domains->
    let lowered=Flow_sop.Lower.workspace ~factories:Sop.Nodes.factories forms |>flow_ok in
    let graph=List.hd lowered.graphs and lane=Flow_sop.Value_lane.create() in
    let session=create_session() in
    Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
      List.iter(fun frame->let time=float frame*.0.25 in
        let live={(Input.at_time time)with frame} in
        let resolved=Flow_sop.Value_lane.resolve ~live lane ~time graph.network |>flow_ok in
        let node=Sop.Edit_graph.compile_node resolved.geometry ~node_id:(Option.get graph.root) |>ok in
        let context=Sop.Context.create ~input:live ~time ~frame:(Int64.of_int frame) ~domains() |>ok in
        let get node=match Sop.Session.cook session ~context node with
          |Ok output->Sop.Payload.geometry output.payload |>Result.get_ok
          |Error d->failwith(Diagnostic.error_to_string d) in
        let original=get(List.hd(Sop.Node.inputs node)) in
        let expected=native_write ~owner:(Some Group.Point) ~name:"selected"
          (Array.make(G.point_count original)((frame+1)mod 2))original in
        assert(bytes(get node)=bytes expected)) [0;1;2])) [1;8];
  print_endline "Group frame folds snapshot on the initial domain and recook exact masks at one/eight domains"

let () =
  let geometry=List.nth(fixtures())1 in
  exact geometry {|(map (fn [(value : text)] (text/glob_literal value)) (array/text 5001 "a*"))|}
    (E.Text_array(Array.make 5001 "a\\*"));
  exact geometry {|(map (fn [(value : text)] (text/glob_rewrite "a*" "b*" value)) (array/text 5001 "a猫"))|}
    (E.Text_array(Array.make 5001 "b猫"));
  let catalog=Flow.Catalog.{version=1;kinds=[]} in
  let checker=Flow.Workspace.check ~ops:Flow_sop.Operators.all in
  let forms=Flow.Syntax.parse {|(workspace glob_cards (graph g :context sop [(geo : geometry nil)]
    (let* [literal (text/glob_literal "a*") rewritten (text/glob_rewrite "a*" "b*" "abc")
      entries (sop/group_entries geo "*" :owner "any")] geo)))|} |>flow_ok in
  let checked=match checker catalog forms with Some checked,[]->checked
    |_,ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds)) in
  let pane=Flow_graph.Projection.of_graph catalog checked "g" in
  List.iter(fun(binding,ty)->assert((Flow_graph.Projection.find pane["g";binding] |>Option.get).ty=ty))
    ["literal",Flow.Ty.Text;"rewritten",Text;"entries",Record["owners",Array Text;"names",Array Text]];
  List.iter(fun(binding,key,value)->
    let _,changed=Flow_graph.Flow_edit.apply_checked ~checker catalog forms
      (Set_arg{node=["g";binding];key;sub=[];value=Flow.Syntax.make(Str value)}) |>flow_ok in
    let card=Flow_graph.Projection.find(Flow_graph.Projection.of_graph catalog changed "g")["g";binding] |>Option.get in
    let row=List.find(fun(row:Flow_graph.Projection.row)->row.key=key)card.rows in
    assert(Option.map Flow.Lisp.flat row.expr=Some(Flow.Lisp.flat(Flow.Syntax.make(Str value)))))
    ["literal",Flow_graph.Flow_edit.Pos 0,"é*";
     "rewritten",Pos 0,"x*";"rewritten",Pos 1,"y*";"rewritten",Pos 2,"xyz";
     "entries",Pos 1,"wall*";"entries",Kw "owner","edge"];
  print_endline "Glob literal/rewrite and ordered group entries: byte oracles, native snapshot conflicts, strict 5001-text loops and direct graph gestures"

let () =
  let catalog=Flow.Catalog.{version=1;kinds=[]} in
  let refuses body =
    let forms=Flow.Syntax.parse body |>flow_ok in
    let _,ds=Flow.Workspace.check ~ops:Flow_sop.Operators.all catalog forms in
    assert(List.exists(fun(d:Diagnostic.t)->d.code="E_ARRAY_TYPE")ds) in
  refuses {|(workspace public_fold (graph g :context sop [(geo : geometry nil)]
    (fold [previous geo] [i (array/int 3)] (sop/delete_group previous ""))))|};
  refuses {|(workspace public_any_fold (graph g :context sop [(geo : geometry nil)]
    (fold [previous geo] [i (array/int 3)] (error "E_PUBLIC_FOLD" "public geometry fold"))))|};
  List.iter(fun body->refuses("(workspace rejected (defnode sop/rejected [(geo : geometry)] "^body^
    ") (graph g :context sop (sop/rejected nil)))"))
    ["(for [i (array/int 3)] (sop/delete_group geo \"\"))";
     "(scan [previous geo] [i (array/int 3)] (sop/delete_group previous \"\"))";
     "(map (fn [(i : int)] (sop/delete_group geo \"\")) (array/int 3))"];
  let groups=List.init 5001(fun i->Group.init ~owner:Group.Point ~name:("entry_"^string_of_int i) 0(fun _->assert false)) in
  let geometry=G.create ~positions:(G.positions(List.hd(fixtures()))) ~topology:(Rdk.Topology.empty ~point_count:0) ~groups() |>ok in
  let node=Lisp_sop.workspace_node ~with_:["source",Lisp_sop.snapshot geometry] {|(workspace cooked_inventory
    (defnode sop/cooked_inventory [(geo : geometry)]
      (let* [entries (sop/group_entries geo "*" :owner "point")
             result (fold [state {:geometry geo :seen 0}]
               [name entries.names]
               (let* [seen (int/add state.seen 1)]
                 {:geometry (sop/with_attr state.geometry "iteration" (array/int 1 seen) :owner detail :type int)
                  :seen seen}))]
        result.geometry))
    (graph g :context sop (sop/cooked_inventory (sop/ext_source))))|} in
  let session=create_session() in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    List.iter(fun domains->let output=cook session domains node in
      assert(List.length(G.groups output)=5001);
      match G.find_attribute ~owner:Rdk.Attribute.Detail "iteration" output |>Option.get |>Rdk.Attribute.storage with
      |Int values->assert(values=[|5001|])|_->assert false) [1;8]);
  (* a node that reads the fold's geometry, and a nested fold over a list that depends on the outer step, are
     deferred with the fold to cook time *)
  let node=Lisp_sop.workspace_node ~with_:["source",Lisp_sop.snapshot geometry] {|(workspace cooked_consumer
    (defnode sop/cooked_consumer [(geo : geometry)]
      (let* [outer (fold [state {:geometry geo :seen 0}] [pattern (array/text 1 "entry_1*")]
               (let* [names (sop/group_entries state.geometry pattern :owner "point")
                      inner (fold [step {:geometry state.geometry :seen state.seen}] [name names.names]
                              {:geometry (sop/with_attr step.geometry "iteration" (array/int 1 1) :owner detail :type int)
                               :seen (int/add step.seen 1)})]
                 inner))
             marked (sop/with_attr outer.geometry "seen" (array/int 1 outer.seen) :owner detail :type int)]
        (sop/delete_attr marked "iteration" :owner "detail")))
    (graph g :context sop (sop/cooked_consumer (sop/ext_source))))|} in
  let session=create_session() in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    let output=cook session 1 node in
    assert(G.find_attribute ~owner:Rdk.Attribute.Detail "iteration" output=None);
    match G.find_attribute ~owner:Rdk.Attribute.Detail "seen" output |>Option.get |>Rdk.Attribute.storage with
    |Int values->assert(values=[|1111|])|_->assert false);
  print_endline "Cooked defnode Fold traverses 5001 inventory entries through geometry handles; public array-node folds and collecting forms remain refused"

let ()=
  List.iter(fun n->
    let topology=Rdk.Topology.create_owned ~point_count:n
      ~vertex_points:(Array.init(n*2)(fun i->i/2))
      ~primitive_offsets:(Array.init(n+1)(fun i->i*2))
      ~primitive_kinds:(Array.make n Rdk.Topology.Open_polyline) |>ok in
    let geometry=G.create ~positions:(Rdk.Packed.Float3.View.of_owned_exn
      ~x:(Array.make n 0.) ~y:(Array.make n 0.) ~z:(Array.make n 0.)) ~topology() |>ok in
    List.iter(fun(owner,native_owner)->match native_owner with None->()|Some native_owner->
      let length=count geometry (Some native_owner) in
      List.iter(fun ordered->
        let selected i=match (i lsr 3)mod 3 with
          |0->false|1->true|_->0xa5 land(1 lsl(i land 7))<>0 in
        let group=if ordered then
            let members=Array.init length(fun i->length-i-1) |>Array.to_list
              |>List.filter selected |>Array.of_list in
            Group.ordered ~owner:native_owner ~name:"bytes" ~length members |>ok
          else Group.init ~owner:native_owner ~name:"bytes" length selected in
        let geometry=G.with_group group geometry |>ok in
        let bits=Bytes.copy(Group.View.bits_view group)
        and order=Group.ordered_elements group and identity=Group.data_id group in
        let expected=Array.init length(fun i->if Group.mem i group then 1 else 0) in
        List.iter(fun domains->Parallel.run ~domains(fun()->
          match query geometry owner "bytes" None with
          |E.Int_array actual->assert(actual=expected);
              if length>0 then actual.(0)<-1-actual.(0)
          |_->assert false)) [1;4;8];
        assert(query geometry owner "bytes" None=E.Int_array expected);
        assert(Group.View.bits_view group=bits && Group.ordered_elements group=order
          && Group.data_id group=identity)) [false;true])owners)
    [0;1;7;8;9;63;64;65;32769];
  print_endline "Ordinary group byte masks preserve empty/tails/all owners/explicit order and immutable storage at one/four/eight domains"

let promoted_query geometry owner name =
  Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some geometry)
    (E.Source("sop/group",Flow.Ty.Array Flow.Ty.Int,
      ["geometry",E.Deferred(Flow.Ty.geometry,7);"owner",E.Text owner;
       "name",E.Text name;"as_owner",E.Text"point"]))|>flow_ok
let native_promoted geometry owner name grain =
  let selection=match owner with
    |"point"->Rdk_core.Element_selection.Selected_points(Option.get(G.find_group ~owner:Group.Point name geometry))
    |"vertex"->Selected_vertices(Option.get(G.find_group ~owner:Group.Vertex name geometry))
    |"primitive"->Selected_primitives(Option.get(G.find_group ~owner:Group.Primitive name geometry))
    |"edge"->Selected_edges(Option.get(G.find_edge_group name geometry))|_->assert false in
  let group=Rdk_core.Element_selection.promote ~grain ~destination:Group.Point selection(G.topology geometry)|>ok in
  E.Int_array(Array.init(G.point_count geometry)(fun i->if Group.mem i group then 1 else 0))
let promotion_fixture n =
  let topology=if n<6 then Rdk.Topology.empty ~point_count:n else
    Rdk.Topology.create_owned ~point_count:n ~vertex_points:[|0;1;2;2;1;3;4;4;0|]
      ~primitive_offsets:[|0;3;6;9|]
      ~primitive_kinds:[|Rdk.Topology.Polygon;Polygon;Open_polyline|]|>ok in
  G.create ~positions:(Rdk.Packed.Float3.View.of_owned_exn
    ~x:(Array.init n float) ~y:(Array.make n 0.) ~z:(Array.make n 0.)) ~topology()|>ok
let () =
  List.iter(fun n->let geometry=promotion_fixture n in
    List.iter(fun(owner,native_owner)->let length=count geometry native_owner in
      List.iter(fun selected->let mask=Array.init length(fun i->if selected i then 1 else 0) in
        List.iter(fun ordered->
          let order=if ordered && native_owner<>None then Some
            (Array.to_list(Array.mapi(fun i value->i,value)mask)|>List.filter_map(fun(i,value)->
              if value=1 then Some i else None)|>List.rev|>Array.of_list) else None in
          let geometry=native_write ?order ~owner:native_owner ~name:"promoted" mask geometry in
          let before=bytes geometry and original=query geometry owner "promoted" None in
          let expected=native_promoted geometry owner "promoted" 97 in
          List.iter(fun domains->Parallel.run ~domains(fun()->
            List.iter(fun grain->assert(native_promoted geometry owner "promoted" grain=expected)) [1;97;4096];
            assert(promoted_query geometry owner "promoted"=expected);
            (match promoted_query geometry owner "promoted" with E.Int_array values when n>0->values.(0)<-1-values.(0)|_->());
            assert(promoted_query geometry owner "promoted"=expected);
            assert(query geometry owner "promoted" None=original))) [1;4;8];
          exact geometry("(exact (map (fn [(value : int)] value) "^
            "(sop/group geo :name \"promoted\" :owner "^owner^" :as_owner point)))")expected;
          assert(bytes geometry=before)) [false;true])
        [(fun _->false);(fun _->true);(fun i->i mod 2=0);(fun i->i=length-1)])owners)
    [0;1;7;8;9;32769];
  let geometry=promotion_fixture 7 in
  List.iter(fun name->let geometry=native_write ~owner:(Some Group.Vertex) ~name
    (Array.make(G.vertex_count geometry)1)geometry in
    assert(promoted_query geometry "vertex" name=native_promoted geometry "vertex" name 97)) ["\194\160";"\227\128\128"];
  List.iter(fun(name,owner,target,code)->
    match Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some geometry)
      (E.Source("sop/group",Flow.Ty.Array Flow.Ty.Int,["geometry",E.Deferred(Flow.Ty.geometry,7);
        "name",E.Text name;"owner",E.Text owner;"as_owner",target])) with
    |Error d->assert(d.code=code)|Ok _->assert false)
    ["absent","vertex",E.Text"point","E_GROUP_MISSING";
     " ","vertex",E.Text"point","E_GROUP_NAME";
     "absent","detail",E.Text"point","E_GROUP_OWNER";
     "absent","vertex",E.Text"edge","E_GROUP_TARGET";
     "absent","vertex",E.Int 0,"E_GROUP_TYPE"];
  let point_only=native_write ~owner:(Some Group.Point) ~name:"other_owner" (Array.make 7 1)geometry in
  (match Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some point_only)
    (E.Source("sop/group",Flow.Ty.Array Flow.Ty.Int,["geometry",E.Deferred(Flow.Ty.geometry,7);
      "name",E.Text"other_owner";"owner",E.Text"vertex";"as_owner",E.Text"point"]))with
   |Error d->assert(d.code="E_GROUP_MISSING")|Ok _->assert false);
  let foreign=Rdk.Topology.empty ~point_count:7 in
  let edge=Edge.init ~topology:foreign ~index:(Rdk.Topology_index.create foreign) ~name:"foreign"(fun _->true) in
  assert(Result.is_error(G.with_edge_group edge geometry));
  print_endline "Group query promotion: native any-incidence masks, owner lookup, empty/tails/order/copy and exact domain/grain proof"

let () =
  let geometry=promotion_fixture 7 in
  List.iter(fun(owner,native_owner)->let geometry=native_write ~owner:native_owner ~name:"promoted"
    (Array.init(count geometry native_owner)(fun i->if i mod 2=0 then 1 else 0))geometry in
    let expected=native_promoted geometry owner "promoted" 97 in
    List.iter(fun captured->
      let body=if captured then "(let* [mask (sop/group geo :name \"promoted\" :owner "^owner^
        " :as_owner point)] (sop/with_attr geo \"mark\" (exact (map (fn [(p : vec3) (m : int)] m) "^
        "(sop/attr geo :P) mask)) :type int))" else
        "(sop/with_attr geo \"mark\" (exact (map (fn [(p : vec3) (m : int)] m) "^
        "(sop/attr geo :P) (sop/group geo :name \"promoted\" :owner "^owner^" :as_owner point))) :type int)" in
      let node=Lisp_sop.workspace_node ~with_:["source",Lisp_sop.snapshot geometry]
        ("(workspace promotion_origin (defnode sop/promoted [(geo : geometry)] "^body^
          ") (graph g :context sop (sop/promoted (sop/ext_source))))") in
      let session=create_session() in Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
        List.iter(fun domains->Sop.Session.clear session;
          let output=cook session domains node in
          assert((Sop.Node.facts node).reads=["P"]);
          match G.find_attribute ~owner:Rdk.Attribute.Point "mark" output |>Option.get |>Rdk.Attribute.storage,expected with
          |Rdk.Attribute.Int actual,E.Int_array expected->assert(actual=expected)|_->assert false) [1;4;8])) [false;true])owners;
  let catalog=Flow.Catalog.{version=1;kinds=[]} in
  let checker=Flow.Workspace.check ~ops:Flow_sop.Operators.all in
  let forms=Flow.Syntax.parse {|(workspace promotion_cards (graph g :context sop [(geo : geometry nil)]
    (let* [mask (sop/group geo :name "selected" :owner vertex)] geo)))|}|>flow_ok in
  let workspace=function Some checked,[]->checked|_,ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds)) in
  let initial=workspace(checker catalog forms) in
  let card workspace=Flow_graph.Projection.find(Flow_graph.Projection.of_graph catalog workspace "g")["g";"mask"]|>Option.get in
  let row workspace=List.find(fun(row:Flow_graph.Projection.row)->row.key=Flow_graph.Flow_edit.Kw"as_owner")(card workspace).rows in
  assert((row initial).expr=None);
  let changed_forms,changed=Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"mask"];key=Kw"as_owner";sub=[];value=Flow.Syntax.make(Str"point")})|>flow_ok in
  assert(Option.map Flow.Lisp.flat(row changed).expr=Some"\"point\"");
  let _,cleared=Flow_graph.Flow_edit.apply_checked ~checker catalog changed_forms
    (Disconnect{node=["g";"mask"];key=Kw"as_owner";fallback=None})|>flow_ok in
  assert((row cleared).expr=None);
  List.iter(fun op->let forms=Flow.Syntax.parse("(workspace invalid_target (graph g :context sop [(geo : geometry nil)] "^
    "(let* [mask ("^op^" geo :name \"selected\" :as_owner point)] geo)))")|>flow_ok in
    match checker catalog forms with None,_ ->()|_->assert false)["sop/group_order"];
  print_endline "Group promotion output count origins and optional-target graph set/disconnect gestures preserve source owner"

let () =
  let stages program=Option.fold ~none:0 ~some:Flow_ir.Packed.stage_count
    (Flow_ir.Executor.packed_root program)in
  let geometry=promotion_fixture 7 and catalog=Flow.Catalog.{version=1;kinds=[]} in
  let forms=Flow.Syntax.parse {|(workspace origins (graph g :context value
    [(p : (array vec3) (array/vec3 0)) (mask : (array int) (array/int 0 0))]
    (let* [a (map (fn [(position : vec3)] (+ position [t 0 0])) p)
           b (map (fn [(m : int)] [(float m) t 0]) mask)
           tested (exact (map (fn [(left : vec3) (right : vec3)] (+ left right)) a b))] 0.0)))|}|>flow_ok in
  let checked=match Flow.Workspace.check catalog forms with Some workspace,[]->workspace
    |_,ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds)) in
  List.iter(fun(owner,native_owner)->let geometry=native_write ~owner:native_owner ~name:"promoted"
    (Array.init(count geometry native_owner)(fun i->i mod 2))geometry in
    List.iter(fun promoted->
      let marker=E.Source("sop/group",Flow.Ty.Array Flow.Ty.Int,
        ["geometry",E.Deferred(Flow.Ty.geometry,7);"owner",E.Text owner;"name",E.Text"promoted"]@
        (if promoted then["as_owner",E.Text"point"]else[])) in
      let positions=E.Source("sop/attr",Flow.Ty.Array Flow.Ty.Vec3,
        ["geometry",E.Deferred(Flow.Ty.geometry,7);"attribute",E.Text"P"]) in
      let evaluation=E.static ~record:true ~inputs:["g",["p",positions;"mask",marker]]checked|>flow_ok in
      let values=List.assoc["g";"tested"]evaluation.records|>List.hd|>snd in
      let program=Flow_sop.Attribute_kernel.prepare ~strict:true ~sources:[7]
        [Lisp_sop.snapshot geometry]values|>flow_ok in
      assert(stages program=(if promoted || owner="point" then 3 else 1));
      let resolve=Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some geometry) in
      List.iter(fun domains->Parallel.run ~domains(fun()->let live=Input.at_time 0.25 in
        let expected=E.Host.force_reference ~resolve values ~live|>flow_ok in
        let actual=Flow_ir.Executor.force ~strict:true ~resolve program ~live|>flow_ok in
        assert(actual=expected))) [1;4;8]) [false;true])owners;
  print_endline "Promoted captured group descriptors share point fusion origins; original source owners remain distinct"

let () = if Array.mem "--bench-promotion" Sys.argv then begin
  let n=1_000_000 and primitives=333_333 in
  let topology=Rdk.Topology.create_owned ~point_count:n
    ~vertex_points:(Array.init(3*primitives)Fun.id)
    ~primitive_offsets:(Array.init(primitives+1)(fun i->3*i))
    ~primitive_kinds:(Array.make primitives Rdk.Topology.Polygon)|>ok in
  let geometry=G.create ~positions:(Rdk.Packed.Float3.View.of_owned_exn
    ~x:(Array.init n float) ~y:(Array.make n 0.) ~z:(Array.make n 0.)) ~topology()|>ok in
  let measure force =
    let samples=Array.init 3(fun _->
      Gc.full_major();
      let allocated=Gc.allocated_bytes() and started=Unix.gettimeofday() in
      let result=force() in
      let seconds=Unix.gettimeofday()-.started in
      let allocated=Gc.allocated_bytes()-.allocated in
      Sys.opaque_identity result |>ignore;
      seconds,allocated) in
    Array.sort(fun(a,_)(b,_)->Float.compare a b)samples;
    samples.(1) in
  print_endline "Group promotion: 1000000 points, 999999 vertices, 333333 triangles, one isolated point; warm indices; median3; initial-domain allocated bytes";
  List.iter(fun(owner,native_owner)->
    let geometry=native_write ~owner:native_owner ~name:"promoted"
      (Array.init(count geometry native_owner)(fun i->i mod 2))geometry in
    let expected=native_promoted geometry owner "promoted" 4096 in
    List.iter(fun domains->Parallel.run ~domains(fun()->
      assert(promoted_query geometry owner "promoted"=expected);
      let native,native_bytes=measure(fun()->native_promoted geometry owner "promoted" 4096) in
      let query,query_bytes=measure(fun()->promoted_query geometry owner "promoted") in
      assert(promoted_query geometry owner "promoted"=expected);
      Printf.printf "promotion owner=%s domains=%d native=%.17g query=%.17g ratio=%.17g native_bytes=%.0f query_bytes=%.0f\n%!"
        owner domains native query (query/.native) native_bytes query_bytes)) [1;4])owners
end
