open Sop
module S = Flow.Syntax
module W = Flow.Workspace
module P = Flow_graph.Projection
module E = Flow_graph.Flow_edit
let ok = Result.get_ok
let factories = Sop_catalog.Editor.factories
let catalog = ok (Editor_document.Kinds.catalog ~version:1 factories)
let source = {|(workspace sample
  (graph img :context image (image/noise :width 2 :height 2 :seed 9))
  (graph g :context sop
    (let* [picture (ref img)
           mesh (sop/grid :columns 1 :rows 1 :uv_attribute "uv")
           result (sop/attr_from_image mesh picture :attribute "sample" :channel "a")]
      result)))|}
let load text = match Editor_document.Workspace_doc.of_text catalog text with
  | Ok document -> document
  | Error diagnostics -> failwith (String.concat "\n" (List.map Diagnostic.to_string diagnostics))
let diagnostics text = snd (W.check catalog (ok (S.parse text)))
let () =
  let kind = List.find (fun (d : Flow_sop.Catalog.descriptor) -> d.key = "attr_from_image")
      (Flow_sop.Catalog.with_library []) in
  assert (kind.slot_types = ["geometry";"image"]);
  assert (List.map fst kind.slots = ["geometry";"image"]);
  List.iter (fun input_types ->
    assert (try ignore (Edit_graph.factory ~key:"typed" ~label:"Typed" ~category:["Test"]
      ~input_types ~arity:1 (fun inputs -> List.hd inputs)); false with Invalid_argument _ -> true))
    [[]; ["image";"image"]; ["bad type"]];
  let bad = Edit_graph.factory ~key:"bad_typed" ~label:"Bad Typed" ~category:["Test"]
    ~input_types:["missing_type"] ~arity:1 (fun inputs -> List.hd inputs) in
  assert (Result.is_error (Flow_sop.Catalog.of_factories ~version:1 [bad]));
  let manifest, _ = ok (Flow_sop.Manifest.generate ~extra:Editor_document.Kinds.descriptors factories) in
  let parsed, _ = ok (Flow.Check.catalog_of_manifest manifest) in
  let kind catalog = List.find (fun (kind : Flow.Check.kind) -> kind.qualified = "sop/attr_from_image") catalog.Flow.Check.kinds in
  assert ((kind parsed).slots = (kind catalog).slots);
  assert ((List.nth (kind parsed).slots 1).ty = Some Flow.Ty.image);
  assert (not (List.exists (fun (slot : Flow.Check.slot) -> slot.ty <> None)
    (List.find (fun (kind : Flow.Check.kind) -> kind.qualified = "sop/copy_to_points") parsed.kinds).slots));
  let document = load source in
  let scope = P.of_graph catalog document.checked "g" in
  let node = Option.get (P.find scope ["g";"result"]) in
  assert ((List.nth node.rows 0).ty = Some Flow.Ty.geometry && (List.nth node.rows 0).head);
  assert ((List.nth node.rows 1).ty = Some Flow.Ty.image && not (List.nth node.rows 1).head);
  let wrong = {|(workspace wrong
    (graph img :context image (image/noise :width 1 :height 1))
    (graph g :context sop (sop/attr_from_image (ref img) (sop/grid))))|} in
  assert (List.exists (fun (d : Diagnostic.t) -> d.code = "E_TYPE") (diagnostics wrong));
  assert (List.exists (fun (d : Diagnostic.t) -> d.code = "E_MISSING_INPUT")
    (diagnostics "(workspace missing (graph g :context sop (sop/attr_from_image (sop/grid))))"));
  List.iter (fun gesture -> ignore (ok (Editor_document.Workspace_doc.edit catalog document gesture)))
    [E.Connect {node=["g";"result"]; key=Pos 1; src="picture"; iter=false};
     E.Set_arg {node=["g";"result"]; key=Kw "channel"; sub=[]; value=S.make (Str "b")};
     E.Disconnect {node=["g";"result"]; key=Pos 1; fallback=Some (S.make (List [S.make (Sym "ref"); S.make (Sym "img")]))}];
  let disconnected = ok (Editor_document.Workspace_doc.edit catalog document
    (E.Disconnect {node=["g";"result"]; key=Pos 1;
      fallback=Some (List.hd (ok (S.parse (Option.get (List.nth node.rows 1).default))))})) in
  assert (Option.is_some (P.find (P.of_graph catalog disconnected.checked "g") ["g";"result#1"]));
  let lowered = ok (Flow_sop.Lower.workspace ~extra:Editor_document.Kinds.descriptors ~factories document.source) in
  let graph = List.find (fun (graph : Flow_sop.Lower.graph) -> graph.name = "g" && graph.default) lowered.graphs in
  let node = ok (Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)) in
  let session = ok (Session.create ~max_entries:16 ~max_payload_bytes:65536) in
  Fun.protect ~finally:(fun () -> Session.close session) (fun () ->
    let output = ok (Session.cook session ~context:(ok (Context.create ())) node) in
    let geometry = ok (Payload.geometry output.payload) in
    let attr = Option.get (Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "sample" geometry) in
    assert (Rdk.Geometry.point_count geometry = 4 && Rdk.Attribute.length attr = 4);
    Array.iter (fun value -> assert (value = 1.)) (Option.get (Rdk.Attribute.get
      (Rdk.Attribute.key ~name:"sample" ~owner:Rdk.Attribute.Point Rdk.Attribute.float) attr)));
  print_endline "mixed image SOP: single declaration, checked typed ports, old manifest shape, graph gestures and actual image cook pass"
