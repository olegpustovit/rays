(* A camera write in a workspace holding a [fn] closure takes the literal fast path: no lowering,
   no evaluation, and the document is the one a full lowering gives. *)
module Document = Editor_document.Document
module Lowering = Editor_document.Lowering
module Workspace_doc = Editor_document.Workspace_doc
module Sync = Editor_document.Scene_sync
module KG = Editor_document.Kind_graph
module E = Flow_graph.Flow_edit

let check condition message = if not condition then failwith message
let factories = Sop.Nodes.factories
let catalog = Editor_document.Kinds.catalog ~version:1 factories |> Result.get_ok

let text =
  {|(workspace sea
  (graph g :context sop [(height : float 0.5)]
    (let* [a (sop/uv_sphere :radius 0.5)
           l (sop/with_attr a :P (exact (map (fn [(p : vec3)] (+ p [0.0 height 0.0])) (sop/attr a :P))))
           b (sop/transform l :translate [1 0 0])]
      b))
  (graph scene :context scene
    (let* [body (scene/geometry (ref g) :name "body")
           cam (scene/camera :name "cam" :eye [7.0 4.0 7.0] :target [0 0 0] :follow_viewport true)]
      (scene/root (scene/merge body cam) :width 960 :height 640))))|}

let lower ?previous ws =
  match Lowering.of_workspace ~factories ?previous ws with
  | Ok doc -> doc
  | Error d -> failwith (Diagnostic.to_string d)

let source (doc : Document.t) = Workspace_doc.to_text doc.workspace.ws

(* lowering and evaluation calls made by [f] *)
let work f =
  let result, p = Flow.Phase_timer.sample ~clock:Unix.gettimeofday f in
  (result, Flow.Phase_timer.calls p "lower" + Flow.Phase_timer.calls p "evaluate")

let scene doc = Document.scene_graph doc

let same_as_full what (doc : Document.t) =
  let full = lower (Ws_fixture.of_text (source doc)) in
  let view (d : Document.t) =
    List.map
      (fun (n : KG.node) ->
        (n.label, n.kind, List.map (fun (f : Param.field_view) -> (f.name, f.current)) (KG.fields n)))
      (scene d)
  in
  check (view doc = view full) (what ^ ": differs from a full lowering")

let run () =
  let doc = lower (Ws_fixture.of_text text) in
  let cam = (List.find (fun (n : KG.node) -> n.label = "cam") (scene doc)).id in
  let moved =
    { doc with
      scene =
        fst
          (Result.get_ok
             (KG.set_fields (scene doc) cam
                [ ("eye_x", Param.Float_value 6.5); ("eye_y", Param.Float_value 3.25);
                  ("eye_z", Param.Float_value 7.125) ])) }
  in
  let saved, n = work (fun () -> Result.get_ok (Sync.reconcile ~factories ~adopt:false doc moved)) in
  check (n = 0) "a camera write lowered or evaluated";
  same_as_full "camera write" saved;
  (* a literal scrub of a card inside the closure-holding graph *)
  let op =
    E.Set_arg
      { node = [ "g"; "b" ]; key = Kw "translate"; sub = [ 0 ];
        value = List.hd (Result.get_ok (Flow.Syntax.parse "3")) }
  in
  let ws = Workspace_doc.edit catalog saved.workspace.ws op |> Result.get_ok in
  let scrubbed = lower ~previous:saved ws in
  same_as_full "literal scrub" scrubbed;
  (* the sketch the profile came from: a [fn] closure and a following camera *)
  let sea = In_channel.with_open_bin "../../../sketches/gpu_sea.rays" In_channel.input_all in
  let sdoc = lower (Ws_fixture.of_text sea) in
  let cam = (List.find (fun (n : KG.node) -> n.label = "camera") (scene sdoc)).id in
  let moved =
    { sdoc with
      scene = fst (Result.get_ok (KG.set_fields (scene sdoc) cam [ ("eye_x", Param.Float_value 6.5) ])) }
  in
  let saved, n = work (fun () -> Result.get_ok (Sync.reconcile ~factories ~adopt:false sdoc moved)) in
  check (n = 0) "gpu_sea camera write lowered or evaluated";
  same_as_full "gpu_sea camera write" saved;
  (* a second write finds the first one in every place the evaluation kept the camera *)
  let again = { saved with scene = fst (Result.get_ok (KG.set_fields (scene saved) cam [ ("eye_x", Param.Float_value 5.5) ])) } in
  let saved, n = work (fun () -> Result.get_ok (Sync.reconcile ~factories ~adopt:false saved again)) in
  check (n = 0) "the second camera write lowered or evaluated";
  same_as_full "second camera write" saved
