(* Actual native Peak/Mountain selection is the independent row oracle. *)
module E=Flow.Eval
module D=Diagnostic
let ok=function Ok value->value|Error message->failwith message
let parse text=match Flow.Syntax.parse text with Ok forms->forms|Error d->failwith(D.to_string d)
let count=32769
let fixture bad selected =
  let positions=Rdk.Packed.Float3.Private.of_owned_exn
    ~x:(Array.make count 0.) ~y:(Array.make count 0.) ~z:(Array.make count 0.) in
  let x=Array.init count(fun i->if List.mem i bad then Float.nan else 0.)
  and y=Array.make count 0. and z=Array.make count 0. in
  let directions=Rdk.Packed.Float3.Private.of_owned_exn ~x ~y ~z in
  let topology=Rdk.Topology.empty ~point_count:count in
  let geometry=ok(Rdk.Geometry.create ~positions ~topology())in
  let geometry=ok(Rdk.Geometry.with_attribute
    (ok(Rdk.Attribute.create_owned ~owner:Point ~name:"bad"(Float3 directions)))geometry)in
  let failures=Rdk.Group.init ~owner:Point ~name:"failures" count(fun i->List.mem i bad && selected i)in
  let selected=Rdk.Group.init ~owner:Point ~name:"selected" count selected in
  let geometry=ok(Rdk.Geometry.with_group failures geometry)in
  ok(Rdk.Geometry.with_group selected geometry),selected
let native ~mountain ~grain geometry selected =
  let selection=Rdk.Deform.Selected_points selected in
  if mountain then Rdk.Deform.mountain ~grain ~selection ~direction_attribute:"bad" ~height:0. geometry
  else Rdk.Deform.peak ~grain ~selection ~direction_attribute:"bad" ~distance:1. geometry
let source mountain=
  let operation=if mountain then "mountain" else "peak"in
  let reason=if mountain then "non-finite direction, noise, mask, or output"else "non-finite direction, mask, or output"in
  Printf.sprintf {|(workspace rows
    (defnode sop/row_check [(geo : geometry)]
      (let* [unused (error_rows (exact (map (fn [flag] flag) (sop/group geo :name "failures")))
        (fn [index] {:code "invalid_deformation"
          :message "%s could not produce valid geometry"
          :cause (list (str "%s [invalid_deformation]: Rdk_mesh.Deform.%s: %s at point " index))})
        :policy "range-last")] geo))
    (graph g :context sop (sop/row_check (sop/ext_src))))|}operation operation operation reason
let compile reference geometry mountain=
  let factory=Sop.Edit_graph.factory ~key:"ext_src" ~label:"Source" ~category:["Test"] ~arity:0
    (fun _->Lisp_sop.snapshot geometry)in
  let lowered=match Flow_sop.Lower.workspace ~reference
    ~factories:(factory::Sop_catalog.Editor.factories)(parse(source mountain))with
    |Ok lowered->lowered|Error d->failwith(D.to_string d)in
  let graph=List.hd lowered.graphs in
  ok(Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root))
let context domains grain=ok(Sop.Context.create ~domains ~grain())
let with_session fn=
  let session=ok(Sop.Session.create ~max_entries:32 ~max_payload_bytes:10_000_000)in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->fn session)
let ()=
  List.iter(fun(bad,selected)->let geometry,selection=fixture bad selected in
    let before=Rdk_test_support.geometry_bytes geometry in
    List.iter(fun mountain->let nodes=List.map(fun reference->compile reference geometry mountain)[false;true]in
      List.iter(fun domains->List.iter(fun grain->
        let expected=Parallel.run ~domains(fun()->native ~mountain ~grain geometry selection)in
        List.iter(fun node->with_session(fun session->
          match expected,Sop.Session.cook session ~context:(context domains grain)node with
          |Ok _,Ok output->assert(Rdk_test_support.geometry_bytes(Result.get_ok(Sop.Payload.geometry output.payload))=before)
          |Error expected,Error actual->let operation=Rdk.Error.operation expected in
              assert(actual.code=Rdk.Error.code expected && actual.message=operation^" could not produce valid geometry"
                && actual.cause=Some(Rdk.Error.to_string expected) && actual.hints=Rdk.Error.hints expected)
          |Error expected,Ok _->failwith(Printf.sprintf "Native %s at domains%d grain%d became a successful Lisp cook"
              (Rdk.Error.to_string expected)domains grain)
          |Ok _,Error actual->failwith(Printf.sprintf "Native success at domains%d grain%d became Lisp %s: %s%s"
              domains grain actual.code actual.message
              (Option.fold ~none:"" ~some:(fun cause->" / "^cause)actual.cause))))nodes) [1;97;4096]) [1;4;8]) [false;true];
    assert(Rdk_test_support.geometry_bytes geometry=before))
    [[0;1;96;97;98],(fun _->true);[96;97;98],(fun _->true);
     [4095;4096;4097],(fun _->true);[32768],(fun _->true);
     [0;1;96;97;4096;32768],(fun i->i=1 || i=97 || i=32768);
     [0;1;4096],(fun _->false)];
  let geometry,_=fixture[] (fun _->true)in
  let node=compile false geometry false in
  with_session(fun session->
    ignore(Result.get_ok(Sop.Session.cook session ~context:(context 1 1)node));
    let before=Sop.Session.stats session in
    List.iter(fun domains->List.iter(fun grain->
      let output=Result.get_ok(Sop.Session.cook session ~context:(context domains grain)node)in
      assert(Rdk_test_support.geometry_bytes(Result.get_ok(Sop.Payload.geometry output.payload))=
        Rdk_test_support.geometry_bytes geometry)) [1;97;4096]) [1;4;8];
    let after=Sop.Session.stats session in assert(after.misses=before.misses && after.hits>before.hits))

let ()=print_endline "Diagnostic rows: native Peak/Mountain causes match unused compiled/reference validation at domains1/4/8 grains1/97/4096; successful cache keys ignore grain"
