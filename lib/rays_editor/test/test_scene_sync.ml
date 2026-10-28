(* Edits of the derived scene and World are edits of the text: each kind of change
   (an object's field, an inline object, a rename, a reparent, a delete, the render camera, the
   World node, its layers, a preset, settings, and objects only the host made) goes through
   [Scene_sync.reconcile], and the text it leaves lowers to the same document (by labels). *)
open Rays
open Sop

module Document = Editor_document.Document
module Lowering = Editor_document.Lowering
module Kinds = Editor_document.Kinds
module Objects = Editor_document.Objects
module Layers = Editor_document.Layers
module Workspace_doc = Editor_document.Workspace_doc
module Sync = Editor_document.Scene_sync
module E = Flow.Eval
module KG = Editor_document.Kind_graph

let check condition message = if not condition then failwith message
let factories = Sop.Nodes.factories

let contains text piece =
  let n = String.length piece in
  let rec at i = i + n <= String.length text && (String.sub text i n = piece || at (i + 1)) in
  at 0

let text = {|(workspace sync
  (graph g :context sop (sop/box))
  (graph scene :context scene
    (let* [body (scene/geometry (ref g) :name "body")
           arm (scene/geometry (ref g) :name "arm" :translate [1 0 0])
           cam (scene/camera :name "cam" :eye [0 2 6])
           side (scene/camera :name "side" :eye [6 2 0])
           studio (scene/world (ref world) :name "studio" :exposure 0.5)
           all (scene/merge body arm cam side (scene/light :name "fill") studio)]
      all))
  (graph world :context world
    (world/sun (world/sky :name "sky") :name "sun"))
  (graph settings :context settings (settings/config :title "t" :width 640)))|}

let lower ?previous workspace =
  match Lowering.of_workspace ~factories ?previous workspace with
  | Ok doc -> doc | Error d -> failwith (Diagnostic.to_string d)

let open_text text = lower (Ws_fixture.of_text text)
let scene (doc : Document.t) = Document.scene_graph doc
let node_id doc label = (List.find (fun (i : KG.node) -> i.label = label)
  ((scene doc))).id

(* a document as labels say it: ids are allowed to differ *)
let snapshot (doc : Document.t) =
  let graph = scene doc in
  let fields node = List.map (fun (f : Param.field_view) -> f.name, f.current) (KG.fields node) in
  let label g id = Option.map (fun (n : KG.node) -> n.label) (KG.find g id) in
  let objects = List.sort compare (List.map (fun (i : KG.node) ->
    i.label, i.kind, Option.bind (Objects.parent graph i.id) (label graph), fields i)
    (graph)) in
  let layers = List.concat_map (fun wid ->
    match Document.Int_map.find_opt wid doc.layers with
    | None -> []
    | Some layers ->
        List.map (fun (node : KG.node) -> node.label, node.kind, fields node) (KG.stack layers))
    (Objects.ids "world" graph) in
  objects, layers, Option.bind doc.active_camera (label graph),
  List.map (fun (f : Param.field_view) -> f.name, f.current) (Editor_document.Settings.fields doc.settings)
  |> List.sort compare

let source (doc : Document.t) = Workspace_doc.to_text (doc.workspace.ws)

(* the text of [doc], opened again, says the same *)
let same_after_reload doc what =
  check (snapshot (open_text (source doc)) = snapshot doc) (what ^ ": the saved text is not the document")

let with_scene (doc : Document.t) graph = { doc with scene = graph }

let set doc label values =
  with_scene doc (fst (Result.get_ok (KG.set_fields (scene doc) (node_id doc label) values)))

let reconcile ?adopt before after =
  Sync.reconcile ~factories ?adopt before after

let calls = ref 0
let ok = function Ok doc -> incr calls; doc | Error message -> failwith (Printf.sprintf "reconcile #%d: %s" !calls message)
let float name x = name, Param.Float_value x

let world_network (doc : Document.t) =
  let wid = List.hd (Objects.ids "world" (scene doc)) in
  wid, Document.Int_map.find wid doc.layers

let with_world_network (doc : Document.t) wid _ graph = Document.with_layers doc wid graph

let layer_id doc label =
  let _, n = world_network doc in
  (List.find (fun (i : KG.node) -> i.label = label) n).id

(* an edit the list, a key or a panel asks of objects of the text *)
let write ?(level = Document.Scene) doc edit =
  Result.map (function Some (doc, _) -> doc | None -> failwith "the text did not take the edit")
    (Sync.write ~factories doc level edit)

(* ... which lowers to what the same edit of the derived objects is *)
let wrote what ?level doc edit after =
  let written = ok (write ?level doc edit) in
  check (snapshot written = snapshot after)
    (what ^ ": the text-first edit is not the edit of the derived document:\n" ^ source written);
  written

(* a name, a parent and a field are also what [reconcile] writes for a camera the host moves: the
   same text *)
let text_first what doc level edit after =
  match Sync.write ~factories doc level edit with
  | Ok (Some (text_first, _)) ->
      check (source text_first = source (ok (reconcile doc after)))
        (what ^ ": the text-first edit and the reconciled one wrote different text:\n" ^ source text_first)
  | Ok None -> failwith (what ^ ": the text did not take the edit")
  | Error message -> failwith (what ^ ": " ^ message)

let qualification_roots () =
  let qualification = Ws_fixture.of_text {|(workspace qualification
    (graph g :context value [(offset : float 2.0)]
      (let* [mapped (map (fn [x] (+ x (+ offset t))) (array/range 4))] 0.0))
    (graph geo :context sop (sop/box :size 1.0)))|} in
  let path = ["g";"mapped"] in
  check (Flow.Workspace.Paths.is_empty qualification.checked.approx) "raw checker published qualified paths";
  let first = lower qualification in
  let current = ref first in
  List.iter (fun (offset,accepted) ->
    let previous = !current in
    let workspace = {(previous.workspace.ws) with inputs = ["g",["offset",E.Float offset]]} in
    let doc = lower ~previous workspace in
    let qualified = doc.workspace.ws and lowered = doc.workspace.lowered in
    check (Flow.Workspace.Paths.mem path qualified.checked.approx = accepted)
      "input changes reused stale qualification";
    check (qualified.checked.approx == lowered.approx) "document did not publish lowering qualification";
    check (qualified.source == workspace.source && qualified.checked.source == workspace.checked.source)
      "qualification replaced the authored source";
    if not accepted then check (List.exists (fun (d : Diagnostic.t) -> d.code = "E_GPU_FORM")
      (List.assoc path qualified.checked.approx_reasons)) "capture overflow has no reason";
    current := doc) [2.,true;1e39,false;7.,true];
  let previous = !current in
  let catalog = Result.get_ok (Kinds.catalog ~version:Flow_sop.Manifest.version factories) in
  let edited = Workspace_doc.edit catalog (previous.workspace.ws)
    (Flow_graph.Flow_edit.Set_arg {node = ["geo";"@result"]; key = Kw "size"; sub = [];
      value = Flow.Syntax.make (Num "2.0")}) |> Result.get_ok in
  check (edited.literal <> None) "catalog literal did not exercise the fast patch";
  let doc = lower ~previous edited in
  check (Flow.Workspace.Paths.mem path (doc.workspace.ws).checked.approx)
    "patched authored forms lost their producer associations";
  check ((doc.workspace.ws).source == edited.source && contains (Workspace_doc.to_text (doc.workspace.ws)) ":size 2.0")
    "publishing metadata lost the literal edit";
  let unchanged = lower ~previous:doc (doc.workspace.ws) in
  check ((unchanged.workspace.ws).checked == (doc.workspace.ws).checked
    && unchanged.workspace.lowered == doc.workspace.lowered) "unchanged qualification broke literal reuse";
  let old_form = List.find (fun (_,p) -> p = path) qualification.checked.packed_roots |> fst in
  let generated = Flow.Syntax.make old_form.node in
  let generated_checked = {qualification.checked with
    packed_roots = List.map (fun (form,path) -> (if form == old_form then generated else form),path)
      qualification.checked.packed_roots;
    graphs = List.map (fun (graph : Flow.Workspace.graph) ->
      if graph.name <> "g" then graph else match graph.body.node with
      | Let (bindings,result) -> {graph with body = {graph.body with node = Let (
          List.map (fun (pattern,(term : Flow.Workspace.term)) -> pattern,
            if term.form == old_form then {term with form = generated} else term) bindings,result)}}
      | _ -> assert false) qualification.checked.graphs} in
  let generated_doc = lower {qualification with checked = generated_checked} in
  let edited = Workspace_doc.edit catalog (generated_doc.workspace.ws)
    (Flow_graph.Flow_edit.Set_arg {node = ["geo";"@result"]; key = Kw "size"; sub = [];
      value = Flow.Syntax.make (Num "2.0")}) |> Result.get_ok in
  check (List.exists (fun (form,_) -> form == generated) edited.checked.packed_roots)
    "literal patch replaced a generated ID-zero producer";
  let edited_doc = lower ~previous:generated_doc edited in
  check (Flow.Workspace.Paths.mem path (edited_doc.workspace.ws).checked.approx)
    "generated empty-span root stopped matching after literal patch"

let run () =
  qualification_roots ();
  let doc = open_text text in
  (* Following the viewport writes camera coordinates every frame. Rounding them
     makes the camera diverge far enough to flash its own frustum when zoomed out. *)
  List.iter (fun distance ->
    let after = set doc "cam" [float "eye_x" (distance *. 0.123456789012345);
      float "eye_y" (distance *. 0.987654321098765); float "eye_z" distance;
      float "target_x" 0.123456789012345] in
    let saved = ok (reconcile ~adopt:false doc after) in
    check (snapshot saved = snapshot after) "camera write-back rounded its coordinates";
    (* the cook and every prepared piece are keyed by the settings and the networks physically:
       a camera move that rebuilt either would recook and re-prepare the scene every frame *)
    check (saved.settings == doc.settings) "a camera write-back rebuilt the unchanged settings";
    check (Document.Int_map.for_all (fun id network ->
        Document.Int_map.find_opt id doc.networks == Some network
        || Document.Int_map.find id doc.networks == network) saved.networks)
      "a camera write-back rebuilt an unchanged network";
    same_after_reload saved "camera precision") [6.123456789012345; 10000.123456789; 1e7];
  let arm = node_id doc "arm" and fill = node_id doc "fill" in
  (* a field of a bound object *)
  let edited = ok (reconcile doc (set doc "arm" [ float "translate_x" 4.; float "translate_y" 2. ])) in
  check (contains (source edited) ":translate [4.0 2.0 0.0]") "a transform edit did not reach the text";
  check (node_id edited "arm" = arm) "an edit changed an object's id";
  check (edited.workspace.lowered != doc.workspace.lowered) "the edited text was not lowered again";
  same_after_reload edited "transform";
  (* the inspector's and the handles' route writes the fields text first (the derived object is
     never edited): the same text as the reconciled edit, ranges applied, ids kept *)
  let direct doc label values =
    match Sync.set_fields ~factories doc Document.Scene ~node:(node_id doc label) values with
    | Ok (Some (written, _, _)) -> written
    | Ok None -> failwith (label ^ " has no text") | Error message -> failwith message in
  List.iter (fun (label, values) ->
    let written = direct doc label values in
    check (source written = source (ok (reconcile doc (set doc label values))))
      ("fields written text first differ from the reconciled edit of " ^ label ^ ": " ^ source written);
    check (node_id written label = node_id doc label) ("fields written text first changed the id of " ^ label))
    [ "arm", [ float "translate_x" 4.; float "translate_y" 2. ]; "fill", [ float "intensity" 99. ];
      "cam", [ float "fov" 500. ] ];
  let hidden = [ "visible", Param.Bool_value false ] in
  text_first "flags" doc Document.Scene (Sync.Fields [ arm, hidden; fill, hidden ])
    (set (set doc "arm" hidden) "fill" hidden);
  (* an inline object is unfolded into a binding first *)
  let edited = ok (reconcile doc (set doc "fill" [ float "intensity" 99. ])) in
  check (contains (source edited) ":intensity 99.0") "an inline object's edit did not reach the text";
  check (node_id edited "fill" = fill) "an inline object lost its id when unfolded";
  check (List.exists (fun (_, h) -> match h with Document.Bound_at _ -> true | _ -> false)
           (List.filter (fun (id, _) -> id = fill) edited.homes.objects)) "the unfolded object has no binding";
  same_after_reload edited "inline object";
  (* rename keeps the id, and the children's parent names follow *)
  let renamed = with_scene doc (KG.update (scene doc) arm (fun n -> { n with label = "forearm" })) in
  let edited = ok (reconcile doc renamed) in
  text_first "rename" doc Document.Scene (Sync.Rename (arm, "forearm")) renamed;
  check (contains (source edited) ":name \"forearm\"" && node_id edited "forearm" = arm) "a rename did not keep the id";
  same_after_reload edited "rename";
  (* reparent keeping the world placement, rename the parent, then unparent *)
  let body = node_id doc "body" in
  let under = with_scene doc (KG.update (scene doc) arm (fun n -> { n with input = Some body })) in
  let edited = ok (reconcile doc under) in
  (* a list's reparent keeps the world placement: the transform is computed before the text is written *)
  let placed = open_text (Str.global_replace (Str.regexp_string ":name \"body\"") ":name \"body\" :translate [0 3 0] :rotate [0 90 0]" text) in
  (let arm = node_id placed "arm" and body = node_id placed "body" in
   let moved = with_scene placed (Objects.reparent (scene placed) [ arm ] (Some body)) in
   text_first "reparent" placed Document.Scene (Sync.Reparent ([ arm ], Some body)) moved;
   let saved = ok (reconcile placed moved) in
   check (contains (source saved) ":parent \"body\""
          && Objects.parent_world (scene saved) arm <> Objects.parent_world (scene placed) arm
          && not (contains (source saved) ":translate [1 0 0]")) ("a reparent kept the local transform: " ^ source saved);
   text_first "outdent" saved Document.Scene (Sync.Outdent [ arm ])
     (with_scene saved (Objects.outdent (scene saved) [ arm ])));
  check (contains (source edited) ":parent \"body\"") "a reparent did not reach the text";
  same_after_reload edited "reparent";
  let torso = with_scene edited (KG.update (scene edited) body (fun n -> { n with label = "torso" })) in
  let edited' = ok (reconcile edited torso) in
  text_first "parent rename" edited Document.Scene (Sync.Rename (body, "torso")) torso;
  (match Sync.write ~factories doc Document.Scene (Sync.Rename (arm, "body")) with
   | Error _ -> () | Ok _ -> failwith "a rename to a name another object has was written");
  check (contains (source edited') ":parent \"torso\"" && not (contains (source edited') ":parent \"body\""))
    "renaming a parent left its child pointing at the old name";
  same_after_reload edited' "parent rename";
  let free = with_scene edited (KG.update (scene edited) arm (fun n -> { n with input = None })) in
  let edited' = ok (reconcile edited free) in
  check (not (contains (source edited') ":parent")) "unparenting left the parent keyword";
  same_after_reload edited' "unparent";
  (* delete: a bound object leaves its binding and the merge; an inline one leaves the merge *)
  let gone = with_scene doc (KG.remove [ arm; fill ] (scene doc)) in
  let edited = wrote "delete" doc (Sync.Delete [ arm; fill ]) gone in
  check (not (contains (source edited) "\"arm\"") && not (contains (source edited) "\"fill\""))
    "a deleted object stayed in the text";
  check (contains (source edited) "scene/merge body cam side") "a deleted object stayed in the merge";
  same_after_reload edited "delete";
  (* the render camera *)
  let side = node_id doc "side" in
  let edited = wrote "render camera" doc (Sync.Camera (Some side)) { doc with active_camera = Some side } in
  check (contains (source edited) ":active true" && edited.active_camera = Some side) "the render camera did not reach the text";
  let reopened = open_text (source edited) in
  check (reopened.active_camera = Some (node_id reopened "side")) "the render camera did not survive a reload";
  (* the World node, a layer of an inline stack, a layer rename *)
  let wid, network = world_network doc in
  let sun = layer_id doc "sun" in
  let world_node graph values = fst (Result.get_ok (KG.set_fields graph wid values)) in
  let edited = wrote "World key" doc (Sync.Fields [ wid, [ float "exposure" (-1.) ] ])
    (with_scene doc (world_node (scene doc) [ float "exposure" (-1.) ])) in
  check (contains (source edited) ":exposure -1.0") "a World node edit did not reach the text";
  same_after_reload edited "world node";
  let layer_edit doc id values =
    let wid, n = world_network doc in
    with_world_network doc wid n (fst (Result.get_ok (KG.set_fields n id values))) in
  let edited = wrote "layer key" ~level:(Document.Inside wid) doc (Sync.Fields [ sun, [ float "intensity" 321. ] ])
    (layer_edit doc sun [ float "intensity" 321. ]) in
  check (contains (source edited) ":intensity 321.0") "a World layer edit did not reach the text";
  check (layer_id edited "sun" = sun) "a World layer edit changed its id";
  same_after_reload edited "world layer";
  (* drag a layer (azimuth, elevation) then undo-like: the document before is untouched *)
  check (snapshot doc = snapshot (open_text text)) "an edit changed its input";
  (* delete a layer: the stack closes over it *)
  let edited = ok (write ~level:(Document.Inside wid) doc (Sync.Delete [ sun ])) in
  check (not (contains (source edited) "world/sun")) "a deleted World layer stayed in the text";
  same_after_reload edited "world layer delete";
  (* reorder the stack: sky above sun *)
  let sky = layer_id doc "sky" in
  let g = network in
  let swapped = KG.update g sun (fun n -> { n with input = None })
    |> (fun g -> KG.update g sky (fun n -> { n with input = Some sun })) in
  let swapped = Document.with_layers doc wid swapped in
  let edited = wrote "restack" ~level:(Document.Inside wid) doc (Sync.Restack [ sun; sky ]) swapped in
  same_after_reload edited "world restack";
  check (List.map (fun (l, _, _) -> l) (let _, layers, _, _ = snapshot edited in layers) = [ "sky"; "sun" ])
    "the World stack was not rewritten";
  (* a preset replaces the layers: the World graph is written whole *)
  let world = List.assoc "white room" World.presets in
  let preset_network = Result.get_ok (Layers.graph_of_world world) in
  let values = snd (Layers.of_world world) in
  let preset = with_scene (Document.with_layers doc wid preset_network)
      (world_node (scene doc) values) in
  let edited = wrote "World preset" ~level:(Document.Inside wid) doc (Sync.Layers (preset_network, values)) preset in
  check (contains (source edited) "world/room") "a World preset did not reach the text";
  same_after_reload edited "world preset";
  (* deleting the World takes the member out of the scene; its graph stays *)
  let worldless = Document.prune (with_scene doc (KG.remove [ wid ] (scene doc))) in
  let edited = wrote "World delete" doc (Sync.Delete [ wid ]) worldless in
  check (not (contains (source edited) "(scene/world") && Objects.ids "world" (scene edited) = [])
    "a deleted World stayed in the scene";
  same_after_reload edited "world delete";
  check (Objects.ids "world" (scene (open_text (source edited))) = []) "a deleted World came back on reload";
  (* settings graph *)
  let settings = fst (Result.get_ok (Editor_document.Settings.apply doc.settings [ "width", Param.Int_value 800 ])) in
  let edited = wrote "settings" doc (Sync.Settings settings) { doc with settings } in
  (* a workspace with no settings graph keeps them itself *)
  (let plain = open_text "(workspace plain (graph g :context sop (sop/box)))" in
   match Sync.write ~factories plain Document.Scene (Sync.Settings settings) with
   | Ok (Some (kept, _)) ->
       check (kept.settings == settings && (kept.workspace.ws).settings == settings)
         "settings with no graph were not kept by the workspace"
   | _ -> failwith "settings with no graph were refused");
  check (contains (source edited) ":width 800") "a settings edit did not reach the text";
  same_after_reload edited "settings";
  (* a name two objects share is refused whole *)
  let twins = open_text {|(workspace twins
    (graph scene :context scene
      (scene/merge (scene/light :name "a") (scene/light :name "a") (scene/light :name "b"))))|} in
  let b = node_id twins "b" in
  let first = (List.hd (List.filter (fun (i : KG.node) -> i.label = "a") ((scene twins)))).id in
  (match reconcile twins (with_scene twins (KG.update (scene twins) b (fun n -> { n with input = Some first }))) with
   | Error _ -> ()
   | Ok _ -> failwith "a parent named like another object was written");
  (* objects only the host made: the first explicit edit writes them *)
  let bare = open_text "(workspace bare (graph g :context sop (sop/box)))" in
  let with_light (doc : Document.t) =
    let graph = KG.add (scene doc) (KG.instance ~label:"light1" Objects.Light.kind) in
    with_scene doc (KG.add graph (KG.instance ~label:"camera1" Objects.Camera.kind)) in
  let bare = with_light bare in
  check (not (contains (source bare) "scene/")) "the host's objects were written before any edit";
  check (Sync.set_fields ~factories bare Document.Scene ~node:(node_id bare "light1") [ float "intensity" 12. ] = Ok None
         && not (Sync.in_text bare Document.Scene (node_id bare "light1")))
    "an object only the host made was taken to have text";
  let edited = ok (reconcile bare (set bare "light1" [ float "intensity" 12. ])) in
  check (contains (source edited) "graph scene" && contains (source edited) "scene/light"
         && contains (source edited) ":intensity 12.0" && contains (source edited) "scene/geometry (ref g)"
         && contains (source edited) "scene/camera") "an edited host object was not written to a scene graph";
  same_after_reload edited "adoption";
  (* a camera that follows the viewport is not such an edit *)
  let followed = ok (reconcile ~adopt:false bare (set bare "camera1" [ float "eye_x" 3. ])) in
  check (not (contains (source followed) "scene/") && snapshot followed = snapshot (set bare "camera1" [ float "eye_x" 3. ]))
    "a follow-the-viewport move was written or lost";
  (* a scene graph is authoritative for cameras and lights: what
     it does not say is not there, and the host seeds nothing *)
  let names doc = List.sort compare (List.map (fun (i : KG.node) -> i.label) ((scene doc))) in
  let with_graphs extra = Ws_fixture.of_text ("(workspace bare (graph g :context sop (sop/box))" ^ extra ^ ")") in
  check (names (lower ~previous:bare (with_graphs "")) = [ "camera1"; "g"; "light1" ])
    "a workspace with no scene graph lost the host's camera and light";
  check (names (lower ~previous:bare (with_graphs " (graph scene :context scene (scene/merge (scene/geometry (ref g))))")) = [ "g" ])
    "a scene graph kept the host's camera or light";
  check (names (lower ~previous:bare (with_graphs " (graph scene :context scene (scene/merge))")) = [])
    "an empty scene graph kept an object";
  (* deleting a host-made camera or light adopts the other host objects into a scene graph and
     removes the one; the saved text has no seed to bring it back *)
  let without (doc : Document.t) label =
    Document.prune (with_scene doc (KG.remove [ node_id doc label ] (scene doc))) in
  let no_camera = ok (reconcile bare (without bare "camera1")) in
  check (contains (source no_camera) "graph scene" && contains (source no_camera) "scene/light"
         && not (contains (source no_camera) "scene/camera")) "a deleted host camera was not written as absent";
  check (names no_camera = [ "g"; "light1" ] && names (open_text (source no_camera)) = [ "g"; "light1" ])
    "a deleted host camera came back on reload";
  same_after_reload no_camera "host camera delete";
  let no_light = ok (reconcile bare (without bare "light1")) in
  check (not (contains (source no_light) "scene/light") && contains (source no_light) "scene/camera"
         && names (open_text (source no_light)) = [ "camera1"; "g" ]) "a deleted host light came back on reload";
  (* adopted, the others are objects of the text *)
  let nothing = ok (write no_camera (Sync.Delete [ node_id no_camera "light1" ])) in
  check (names (open_text (source nothing)) = [ "g" ]) "deleting the last host objects was not written";
  (* adding by key: an object joins the scene's merge, a layer goes on top of the stack; a document
     with no scene graph (or World graph) gets one, its host objects written first *)
  let add doc name expr = Result.get_ok (Flow_graph.Flow_edit.apply_checked
    (Result.get_ok (Kinds.catalog ~version:Flow_sop.Manifest.version factories)) (fst (doc : Workspace_doc.t * _)).source
    (Flow_graph.Flow_edit.Add_node { scope = [ "scene" ]; name; expr })) in
  ignore add;
  let call head args = Flow.Syntax.make (Flow.Syntax.List (Flow.Syntax.make (Flow.Syntax.Sym head) :: args)) in
  let catalog = Result.get_ok (Kinds.catalog ~version:Flow_sop.Manifest.version factories) in
  let edit (doc : Document.t) op = lower ~previous:doc
    (Result.get_ok (Workspace_doc.edit catalog (doc.workspace.ws) op)) in
  let added = edit doc (Flow_graph.Flow_edit.Add_node { scope = [ "scene" ]; name = "lamp";
    expr = call "scene/light" [ Flow.Syntax.make (Flow.Syntax.Kw "name"); Flow.Syntax.make (Flow.Syntax.Str "lamp") ] }) in
  check (contains (source added) "scene/merge body arm cam side (scene/light :name \"fill\") lamp"
         || contains (source added) "lamp)") "a new object did not join the scene's merge";
  check (List.exists (fun (i : KG.node) -> i.label = "lamp") ((scene added)))
    "a new object is not in the scene";
  let layered = edit doc (Flow_graph.Flow_edit.Add_node { scope = [ "world" ]; name = "haze";
    expr = call "world/gradient" [] }) in
  check (List.map (fun (l, _, _) -> l) (let _, layers, _, _ = snapshot layered in layers) = [ "Gradient"; "sun"; "sky" ])
    "a new World layer is not on top of the stack";
  same_after_reload layered "added layer";
  let twice = edit added (Flow_graph.Flow_edit.Delete_nodes { nodes = [ [ "scene"; "lamp" ] ] }) in
  check (snapshot twice = snapshot doc) "deleting what was added did not give the scene back";
  check (Result.is_ok (Workspace_doc.edit catalog (layered.workspace.ws) (Flow_graph.Flow_edit.Delete_nodes { nodes = [ [ "world"; "haze" ] ] })))
    "a World layer could not be deleted by its binding";
  (* through the editor: a workspace with no scene graph gets one when an object is added *)
  let env = Rays_editor.Editor.create ~ui_text_size:11 ~workspace:(Ws_fixture.of_text "(workspace bare (graph g :context sop (sop/box)))")
      ~prepare:Editor_prepare.surface
      ~draw:(fun _ surface -> Rdk_rays.Surface.scene3 surface.Rays_editor.Private.Cook.surface) () |> Result.get_ok in
  let env = match Rays_editor.Editor.edit env (Flow_graph.Flow_edit.Add_node { scope = [ "scene" ]; name = "lamp";
    expr = call "scene/light" [] }) with Ok env -> env | Error m -> failwith m in
  let saved = Workspace_doc.to_text (Rays_editor.Editor.workspace env) in
  check (contains saved "graph scene" && contains saved "scene/geometry (ref g)" && contains saved "scene/camera"
         && contains saved "lamp") "adding to a missing scene graph did not write it";
  check (Rays_editor.Editor.undo_label env = Some "Add node") "adding an object is not one entry";
  Rays_editor.Editor.close env;
  print_endline "scene sync: fields, inline, rename, reparent, delete, camera, World, settings, host objects, add by key ok"

(* ---- the copies of a loop are one template ---- *)

let lamps (doc : Document.t) =
  List.filter_map (fun (i : KG.node) -> if i.kind = "light" then Some i else None)
    ((scene doc))

let field (i : KG.node) name =
  List.find_map (fun (f : Param.field_view) -> if f.name = name then Some f.current else None)
    (KG.fields i)

let set_id (doc : Document.t) id values =
  with_scene doc (fst (Result.get_ok (KG.set_fields (scene doc) id values)))

let without_ids (doc : Document.t) ids =
  Document.prune (with_scene doc (KG.remove ids (scene doc)))

let refused = function Error message -> message | Ok _ -> failwith "an edit the loop cannot take was written"

let run_loops () =
  let body_loop = {|(workspace lamps
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (let* [body (scene/geometry (ref g) :name "body")
             row (for [i (range 4)] (scene/light :name "lamp" :intensity 30 :translate [i 2 0]))]
        (scene/merge body row))))|} in
  (* both spellings: the loop bound to a name, and written in the merge *)
  List.iter (fun (what, text) ->
    let doc = open_text text in
    check (List.length (lamps doc) = 4) (what ^ ": the loop did not make four lamps");
    check (List.length (List.filter (fun (_, h) -> Document.loop_of h <> None) doc.homes.objects) = 4)
      (what ^ ": the copies have no home in the loop");
    let third = (List.nth (lamps doc) 2).id in
    (* a literal field: the template is edited, every copy changes, the text says it once *)
    let edited = ok (reconcile doc (set_id doc third [ float "intensity" 77. ])) in
    check (List.for_all (fun i -> field i "intensity" = Some (Param.Float_value 77.)) (lamps edited))
      (what ^ ": an edit of one copy did not change them all");
    check (List.length (lamps edited) = 4 && contains (source edited) ":intensity 77.0"
           && not (contains (source edited) ":intensity 30"))
      (what ^ ": the template was not edited");
    check (snapshot (open_text (source edited)) = snapshot edited) (what ^ ": the saved text is not the document");
    let note = match Sync.write ~factories doc Document.Scene (Sync.Fields [ third, [ float "intensity" 77. ] ]) with
      | Ok (Some (_, note)) -> note | _ -> None in
    check (match note with Some n -> contains n "loop template" && contains n "4 copies" | None -> false)
      (what ^ ": the status does not say the loop template and the copies: " ^ Option.value ~default:"none" note);
    (* a field the loop computes from its variable has no value of its own *)
    let message = refused (reconcile doc (set_id doc third [ float "translate_x" 9. ])) in
    check (contains message "translate is computed by the loop" && contains message "(now i)")
      (what ^ ": a computed component was not refused with its expression: " ^ message);
    (match Sync.set_fields ~factories doc Document.Scene ~node:third [ float "translate_x" 9. ] with
     | Error direct -> check (direct = message) (what ^ ": the text-first route refuses otherwise: " ^ direct)
     | Ok _ -> failwith (what ^ ": the text-first route wrote a computed component"));
    (* a component the loop does not compute is the template's: every copy follows *)
    let lifted = ok (reconcile doc (set_id doc third [ float "translate_y" 5. ])) in
    check (contains (source lifted) ":translate [i 5.0 0]"
           && List.for_all (fun i -> field i "translate_y" = Some (Param.Float_value 5.)) (lamps lifted)
           && List.map (fun i -> field i "translate_x") (lamps lifted) = List.map (fun i -> field i "translate_x") (lamps doc))
      (what ^ ": a literal component of a computed vector was not edited in the template: " ^ source lifted);
    (* rename: the template's :name *)
    let renamed = with_scene doc (KG.update (scene doc) third (fun n -> { n with label = "spot" })) in
    let renamed = ok (reconcile doc renamed) in
    check (List.for_all (fun (i : KG.node) -> i.label = "spot") (lamps renamed)
           && contains (source renamed) ":name \"spot\"" && not (contains (source renamed) ":name \"lamp\""))
      (what ^ ": a rename did not reach the template :name: " ^ source renamed ^ String.concat "," (List.map (fun (i : KG.node) -> i.label) (lamps renamed)));
    (* delete one copy: the loop skips that iteration, the others keep their iteration, id and place *)
    let x doc = List.map (fun i -> field i "translate_x") (lamps doc) in
    let xs = List.map (fun v -> Some (Param.Float_value v)) in
    let ids doc = List.sort compare (List.map (fun (i : KG.node) -> i.id) (lamps doc)) in
    let gone = ok (write doc (Sync.Delete ([ third ]))) in
    check (contains (source gone) ":skip [2]" && not (contains (source gone) "take") && not (contains (source gone) "drop"))
      (what ^ ": copy 3 was not skipped: " ^ source gone);
    check (x (open_text (source gone)) = xs [ 0.; 1.; 3. ]) (what ^ ": the copies after a deleted one moved");
    check (ids gone = List.filter (( <> ) third) (ids doc)) (what ^ ": the other copies changed id");
    same_after_reload gone (what ^ " delete a copy");
    (* repeated deletes accumulate in one list; a literal edit still reaches every remaining copy *)
    let again = ok (write gone (Sync.Delete ([ (List.nth (lamps gone) 1).id ]))) in
    check (contains (source again) ":skip [1 2]" && x (open_text (source again)) = xs [ 0.; 3. ])
      (what ^ ": a second delete did not accumulate: " ^ source again);
    let edited_after = ok (reconcile again (set_id again (List.hd (lamps again)).id [ float "intensity" 5. ])) in
    check (List.length (lamps edited_after) = 2 && contains (source edited_after) ":skip [1 2]"
           && List.for_all (fun i -> field i "intensity" = Some (Param.Float_value 5.)) (lamps edited_after))
      (what ^ ": an edit after a delete lost the skip or a copy");
    same_after_reload again (what ^ " delete twice");
    let first = (List.hd (lamps doc)).id in
    let gone = ok (write doc (Sync.Delete ([ first ]))) in
    check (contains (source gone) ":skip [0]" && not (contains (source gone) "concat")) (what ^ ": deleting the first copy");
    let gone2 = ok (write doc (Sync.Delete ([ (List.nth (lamps doc) 1).id; (List.nth (lamps doc) 3).id ]))) in
    check (x (open_text (source gone2)) = xs [ 0.; 2. ]) (what ^ ": two copies deleted together")) [
    "bound", body_loop;
    "inline", {|(workspace lamps
      (graph g :context sop (sop/box))
      (graph scene :context scene
        (scene/merge (scene/geometry (ref g) :name "body")
                     (for [i (range 4)] (scene/light :name "lamp" :intensity 30 :translate [i 2 0])))))|} ];
  (* several clauses: one iteration of the product goes, the flat iteration index is what is listed *)
  let grid = open_text {|(workspace grid
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g) :name "body")
                   (for [i (range 2) j (range 3)] (scene/light :name "lamp" :translate [i j 0])))))|} in
  check (List.length (lamps grid) = 6) "the two-clause loop did not make six lamps";
  let ids doc = List.sort compare (List.map (fun (i : KG.node) -> i.id) (lamps doc)) in
  let xy doc = List.map (fun i -> field i "translate_x", field i "translate_y") (lamps doc) in
  let cell i j = Some (Param.Float_value i), Some (Param.Float_value j) in
  let victim = (List.nth (lamps grid) 4).id in
  let gone = ok (write grid (Sync.Delete ([ victim ]))) in
  check (contains (source gone) ":skip [4]") ("a product iteration was not skipped: " ^ source gone);
  check (xy (open_text (source gone)) = [ cell 0. 0.; cell 0. 1.; cell 0. 2.; cell 1. 0.; cell 1. 2. ])
    "a two-clause delete removed the wrong copy";
  check (ids gone = List.filter (( <> ) victim) (ids grid)) "a two-clause delete changed the other ids";
  same_after_reload gone "two-clause delete";
  (* a bound loop too *)
  let bound_grid = open_text {|(workspace grid
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (let* [body (scene/geometry (ref g) :name "body")
             lamps (for [i (range 2) j (range 3)] (scene/light :name "lamp" :translate [i j 0]))]
        (scene/merge body lamps))))|} in
  let gone = ok (write bound_grid (Sync.Delete ([ (List.nth (lamps bound_grid) 4).id ]))) in
  check (List.length (lamps gone) = 5 && contains (source gone) "lamps (for [i (range 2)"
         && contains (source gone) ":skip [4]") ("a bound two-clause delete: " ^ source gone);
  same_after_reload gone "bound two-clause delete";
  (* a copy that makes two objects: deleting one keeps its sibling, at any iteration *)
  let pairs = open_text {|(workspace pairs
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g) :name "body")
                   (for [i (range 3)]
                     (scene/merge (scene/light :name "a" :translate [i 0 0]) (scene/light :name "b" :translate [i 1 0]))))))|} in
  check (List.length (lamps pairs) = 6) "the loop of pairs did not make six lamps";
  let at doc label i = List.find_opt (fun (l : KG.node) -> l.label = label
    && field l "translate_x" = Some (Param.Float_value i)) (lamps doc) in
  let id_at doc label i = (Option.get (at doc label i)).id in
  let half = ok (write pairs (Sync.Delete ([ id_at pairs "a" 0. ]))) in
  check (contains (source half) ":skip [[0 0]]" && not (contains (source half) "for [i (range 3)] :skip"))
    ("half a copy: the merge did not skip its argument: " ^ source half);
  check (List.length (lamps half) = 5 && at half "a" 0. = None && at half "b" 0. <> None && at half "a" 1. <> None)
    "deleting one object of a copy took its sibling or another copy";
  check (snapshot (open_text (source half)) = snapshot half) "half a copy: the saved text is not the document";
  let ids_before = List.filter (( <> ) (id_at pairs "a" 0.)) (ids pairs) in
  check (ids half = ids_before) "half a copy: the other objects changed id";
  (* then the other half: its iteration has nothing left, so the loop skips it *)
  let other = ok (write half (Sync.Delete ([ id_at half "b" 0. ]))) in
  check (List.length (lamps other) = 4
         && contains (source other) ":skip [0]") ("the second half of a copy: " ^ source other);
  same_after_reload other "both halves";
  (* both at once, in another iteration *)
  let both = ok (write pairs (Sync.Delete ([ id_at pairs "a" 1.; id_at pairs "b" 1. ]))) in
  check (List.length (lamps both) = 4 && contains (source both) ":skip [1]" && not (contains (source both) "[[1 0]]"))
    ("both objects of a copy: " ^ source both);
  (* the siblings bound by name in a let* body *)
  let named = open_text {|(workspace pairs
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g) :name "body")
                   (for [i (range 3)]
                     (let* [a (scene/light :name "a" :translate [i 0 0])
                            b (scene/light :name "b" :translate [i 1 0])]
                       (scene/merge a b))))))|} in
  let half = ok (write named (Sync.Delete ([ id_at named "b" 2. ]))) in
  check (List.length (lamps half) = 5 && at half "b" 2. = None && at half "a" 2. <> None
         && contains (source half) ":skip [[2 1]]") ("a bound pair, one deleted: " ^ source half);
  same_after_reload half "bound pair half";
  (* a merge that loses an argument renumbers its skip (here b, which it skipped, and d, which it skips, move up) *)
  let catalog = Result.get_ok (Kinds.catalog ~version:Flow_sop.Manifest.version factories) in
  let renum = open_text {|(workspace r (graph scene :context scene
    (let* [a (scene/light :name "a") b (scene/light :name "b") c (scene/light :name "c") d (scene/light :name "d")]
      (scene/merge a b c d :skip [1 3]))))|} in
  check (List.length (lamps renum) = 2) "a merge :skip did not leave two lamps";
  let edit_text doc op = Workspace_doc.to_text (Result.get_ok (Workspace_doc.edit catalog (doc.Document.workspace.ws) op)) in
  let after = edit_text renum (Flow_graph.Flow_edit.Delete_nodes { nodes = [ [ "scene"; "b" ] ] }) in
  check (contains after ":skip [2]") ("deleting an argument did not renumber the skip: " ^ after);
  let after = edit_text renum (Flow_graph.Flow_edit.Delete_nodes { nodes = [ [ "scene"; "a" ] ] }) in
  check (contains after ":skip [0 2]") ("deleting an earlier argument did not shift the skip: " ^ after);
  print_endline "scene sync: loop copies are one template: edit, computed refusal, rename, exact delete, pairs ok"

(* a loop inside a loop: the copies nest, the rules are the same, and a delete is exact at any depth *)
let run_nested_loops () =
  let text = {|(workspace nest
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g) :name "body")
                   (for [i (range 3)]
                     (scene/merge (for [j (range 2)] (scene/light :name "lamp" :intensity 30 :translate [i j 0])))))))|} in
  let doc = open_text text in
  check (List.length (lamps doc) = 6) "the nested loop did not make six lamps";
  let pick n = (List.nth (lamps doc) n).id in
  let edited = ok (reconcile doc (set_id doc (pick 3) [ float "intensity" 77. ])) in
  check (List.for_all (fun i -> field i "intensity" = Some (Param.Float_value 77.)) (lamps edited)
         && contains (source edited) ":intensity 77.0" && not (contains (source edited) ":intensity 30"))
    "a literal edit did not reach the inner template";
  same_after_reload edited "nested edit";
  let message = refused (reconcile doc (set_id doc (pick 3) [ float "translate_x" 9. ])) in
  check (contains message "computed by the loop" && contains message "(now i)")
    ("a computed field of a nested copy was not refused with its expression: " ^ message);
  let renamed = ok (reconcile doc (with_scene doc (KG.update (scene doc) (pick 3) (fun n -> { n with label = "spot" })))) in
  check (List.for_all (fun (i : KG.node) -> i.label = "spot") (lamps renamed)
         && contains (source renamed) ":name \"spot\"" && not (contains (source renamed) ":name \"lamp\""))
    ("a nested rename did not reach the template: " ^ source renamed);
  same_after_reload renamed "nested rename";
  let ids doc = List.sort compare (List.map (fun (i : KG.node) -> i.id) (lamps doc)) in
  let ij doc = List.map (fun i -> field i "translate_x", field i "translate_y") (lamps doc) in
  let cell i j = Some (Param.Float_value i), Some (Param.Float_value j) in
  let all = [ 0, 0; 0, 1; 1, 0; 1, 1; 2, 0; 2, 1 ] in
  let cells without = List.filter_map (fun (i, j) -> if List.mem (i, j) without then None
    else Some (cell (float_of_int i) (float_of_int j))) all in
  (* one inner copy goes alone: only that object, every other outer copy untouched *)
  List.iter (fun n ->
    let i, j = List.nth all n in
    let gone = ok (write doc (Sync.Delete ([ pick n ]))) in
    check (contains (source gone) (Printf.sprintf ":skip [[%d %d]]" i j)) ("nested delete " ^ string_of_int n ^ ": " ^ source gone);
    check (ij gone = cells [ i, j ] && ij (open_text (source gone)) = cells [ i, j ])
      (Printf.sprintf "deleting inner copy (%d,%d) took another object" i j);
    check (ids gone = List.filter (( <> ) (pick n)) (ids doc)) "a nested delete changed the other ids";
    same_after_reload gone "nested delete") [ 0; 3; 5 ];
  (* repeated deletes accumulate in the inner loop's list *)
  let first = ok (write doc (Sync.Delete ([ pick 3 ]))) in
  let second = ok (write first (Sync.Delete ([ (List.nth (lamps first) 0).id ]))) in
  check (contains (source second) ":skip [[0 0] [1 1]]" && List.length (lamps second) = 4)
    ("nested deletes did not accumulate: " ^ source second);
  same_after_reload second "nested twice";
  (* every object of one outer copy: the outer iteration goes *)
  let outer = ok (write doc (Sync.Delete ([ pick 2; pick 3 ]))) in
  check (ij outer = cells [ 1, 0; 1, 1 ] && contains (source outer) ":skip [1]" && not (contains (source outer) "[[1"))
    ("an outer copy was not skipped: " ^ source outer);
  same_after_reload outer "outer copy";
  (* three levels, several clauses in the innermost: the innermost loop that makes the object is the one that skips *)
  let deep = open_text {|(workspace deep
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g) :name "body")
        (for [i (range 2)]
          (scene/merge
            (for [j (range 2)]
              (scene/merge
                (for [a (range 2) b (range 2)]
                  (scene/light :name "lamp" :translate [i j (+ (* 2 a) b)])))))))))|} in
  check (List.length (lamps deep) = 16) "the three-level loop did not make sixteen lamps";
  let z doc = List.map (fun i -> field i "translate_x", field i "translate_y", field i "translate_z") (lamps doc) in
  let pick3 n = (List.nth (lamps deep) n).id in
  (* lamp number 11 is i=1 j=0 a=1 b=1, the innermost iteration (1 0 3) *)
  let gone = ok (write deep (Sync.Delete ([ pick3 11 ]))) in
  check (contains (source gone) ":skip [[1 0 3]]" && List.length (lamps gone) = 15
         && z (open_text (source gone)) = z gone
         && not (List.mem (List.nth (z deep) 11) (z gone)))
    ("a three-level delete: " ^ source gone);
  check (ids gone = List.filter (( <> ) (pick3 11)) (ids deep)) "a three-level delete changed the other ids";
  same_after_reload gone "three-level delete";
  (* all of (1 0): the middle iteration goes, not four inner ones *)
  let middle = ok (write deep (Sync.Delete ((List.map pick3 [ 8; 9; 10; 11 ])))) in
  check (List.length (lamps middle) = 12 && contains (source middle) ":skip [[1 0]]" && not (contains (source middle) "[[1 0 "))
    ("a middle iteration was not skipped as one: " ^ source middle);
  same_after_reload middle "three-level middle";
  (* an empty loop stays a loop: every object deleted *)
  let none = ok (write doc (Sync.Delete ((List.map pick [ 0; 1; 2; 3; 4; 5 ])))) in
  check (lamps none = [] && lamps (open_text (source none)) = []) "deleting every copy left objects";
  same_after_reload none "all copies";
  print_endline "scene sync: nested loops: edit, computed refusal, rename, exact delete at 2 and 3 levels, accumulate ok"

(* ---- the root and the World as a merge member ---- *)

let diagnostic text = match Lowering.of_workspace ~factories (Ws_fixture.of_text text) with
  | Error d -> Some (d.Diagnostic.code, d.message)
  | Ok _ -> None

let refused code text =
  match diagnostic text with
  | Some (c, message) -> check (c = code) (Printf.sprintf "expected %s, got %s: %s" code c message); message
  | None -> failwith ("expected " ^ code ^ ", the text was accepted")

let root_text = {|(workspace studio
  (graph g :context sop (sop/box))
  (graph sky :context world (world/sun (world/sky :name "sky") :name "sun"))
  (graph scene :context scene
    (let* [body (scene/geometry (ref g) :name "body")
           cam (scene/camera :name "cam")
           side (scene/camera :name "side" :eye [6 2 0])
           world (scene/world (ref sky) :name "Bloom" :exposure -0.5)
           all (scene/merge body cam side world)]
      (scene/root all :camera side :renderer "Path traced" :width 800 :height 600 :max_spp 64))))|}

(* a text on one line: the printer breaks long calls *)
let flat text = String.concat " " (List.filter (( <> ) "")
  (String.split_on_char ' ' (String.map (function '\n' -> ' ' | c -> c) text)))

let run_root () =
  let module R = Objects.Root in
  (* a scene without a root renders as the defaults; a camera carries no render size *)
  let old = open_text text in
  check (old.root = R.default && old.homes.root = None) "a scene without a root did not get the default root";
  check (Result.is_error (Workspace_doc.of_text (Result.get_ok (Kinds.catalog ~version:1 factories)) {|(workspace old
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g)) (scene/camera :width 1600 :height 900 :max_spp 512))))|}))
    "a camera still takes a render size";
  (* every small sketch of the repository still loads (the large ones load in test_workspace_ir and the RAYS_SLOW sweeps); shattered_studio carries a root, written by the editor *)
  let sketches = "../../../sketches" in
  let loaded = List.fold_left (fun n name ->
    let file = Filename.concat sketches name in
    if not (Filename.check_suffix file ".rays") || (Unix.stat file).st_size > 1500 then n else begin
      let text = In_channel.with_open_bin file In_channel.input_all in
      (* a sketch that brings its own SOPs is not loaded here *)
      match Result.bind (Rays_editor.Source.read_imports ~file text)
        (fun imports -> Rays_editor.Workspace.load ~imports text) with
      | Error ds when List.exists (fun (d : Diagnostic.t) -> d.code = "E_UNKNOWN_KIND") ds -> n
      | Error ds -> failwith (file ^ ": " ^ String.concat "; " (List.map Diagnostic.to_string ds))
      | Ok workspace ->
          (match Lowering.of_workspace ~factories workspace with
           | Ok _ -> n + 1 | Error d -> failwith (file ^ ": " ^ Diagnostic.to_string d))
    end) 0 (Sys.readdir sketches |> Array.to_list |> List.sort compare |> List.filteri (fun i _ -> i mod 2 = 0)) in
  check (loaded >= 4) (Printf.sprintf "only %d sketches were found to load" loaded);
  let studio = lower (Result.get_ok (Rays_editor.Workspace.load
    (In_channel.with_open_bin "../../../sketches/shattered_studio.rays" In_channel.input_all))) in
  check (studio.root.width = 1600 && studio.root.height = 1600 && studio.root.max_spp = 512 && studio.homes.root <> None)
    "shattered_studio (saved once by the editor) does not carry its render size on a scene/root";
  (* a root and a World member *)
  let doc = open_text root_text in
  check (doc.root.renderer = R.Path_traced && doc.root.width = 800 && doc.root.height = 600
         && doc.root.max_spp = 64 && doc.homes.root <> None) "the root's settings were not read";
  check (doc.active_camera = Some (node_id doc "side")) "the root's :camera is not the active camera";
  let wid = List.hd (Objects.ids "world" (scene doc)) in
  check (List.mem_assoc wid doc.homes.objects && doc.homes.world_graph = Some "sky"
         && Document.Int_map.mem wid doc.layers && List.length doc.homes.layers = 2)
    "scene/world did not read its world graph's layers";
  check ((KG.find (scene doc) wid |> Option.get).label = "Bloom") "the World lost its name";
  same_after_reload doc "root and World";
  (* a root edit round-trips Save, one undo entry's worth of text *)
  let edited = ok (write doc (Sync.Root { doc.root with width = 1024; bounces = 8 })) in
  check (edited.root.width = 1024 && edited.root.bounces = 8
         && contains (flat (source edited)) ":renderer \"Path traced\" :width 1024 :height 600 :max_spp 64 :bounces 8")
    ("a root edit did not reach the text: " ^ source edited);
  check ((open_text (source edited)).root = edited.root) "a root edit did not round-trip Save";
  (* the printer keeps the camera, renderer, size, samples order *)
  let tidy = ok (write edited (Sync.Root { edited.root with round_samples = 2 })) in
  check (contains (flat (source tidy)) ":bounces 8 :round_samples 2") "the root printed its keywords out of order";
  (* the render camera is written on the root *)
  let cam = wrote "root camera" doc (Sync.Camera (Some (node_id doc "cam")))
    { doc with active_camera = Some (node_id doc "cam") } in
  check (contains (flat (source cam)) ":camera cam :renderer \"Path traced\" :width 800"
         && not (contains (source cam) ":camera side")
         && cam.active_camera = Some (node_id cam "cam")) ("the camera was not written on the root: " ^ source cam);
  (* a part gets a default root; the first edit of a root setting writes one over its result *)
  let part = open_text text in
  let written = ok (write part (Sync.Root { part.root with width = 640; renderer = R.Wireframe })) in
  check (written.homes.root <> None && written.root.width = 640 && contains (source written) "(scene/root"
         && contains (flat (source written)) ":renderer \"Wireframe\" :width 640"
         && (open_text (source written)).root = written.root) ("the root was not written: " ^ source written);
  (* the World is a merge member: its keywords edit like an object's, its layers its graph's *)
  let wid, network = world_network doc in
  let sun = layer_id doc "sun" and sky = layer_id doc "sky" in
  let world_node graph values = fst (Result.get_ok (KG.set_fields graph wid values)) in
  let edited = wrote "World member" doc (Sync.Fields [ wid, [ float "exposure" 1.; float "rotation" 20. ] ])
    (with_scene doc (world_node (scene doc) [ float "exposure" 1.; float "rotation" 20. ])) in
  check (contains (flat (source edited)) "(scene/world (ref sky) :name \"Bloom\" :exposure 1.0 :rotation 20.0)")
    ("a World member's edit did not reach its call: " ^ source edited);
  same_after_reload edited "World member";
  let layer_edit doc id values =
    let wid, n = world_network doc in
    with_world_network doc wid n (fst (Result.get_ok (KG.set_fields n id values))) in
  let edited = wrote "World member layer" ~level:(Document.Inside wid) doc (Sync.Fields [ sun, [ float "intensity" 321. ] ])
    (layer_edit doc sun [ float "intensity" 321. ]) in
  check (contains (source edited) ":intensity 321.0" && layer_id edited "sun" = sun) "a layer of the World's graph was not edited";
  same_after_reload edited "World member layer";
  (* a deleted top layer hands the graph's result to the layer below; the stack reorders *)
  let edited = ok (write ~level:(Document.Inside wid) doc (Sync.Delete [ sun ])) in
  check (not (contains (source edited) "world/sun") && contains (source edited) "world/sky"
         && Objects.ids "world" (scene edited) <> []) ("a deleted top layer: " ^ source edited);
  same_after_reload edited "World member layer delete";
  let g = network in
  let swapped = KG.update g sun (fun n -> { n with input = None })
    |> (fun g -> KG.update g sky (fun n -> { n with input = Some sun })) in
  let swapped = Document.with_layers doc wid swapped in
  let edited = wrote "World member restack" ~level:(Document.Inside wid) doc (Sync.Restack [ sun; sky ]) swapped in
  check (List.map (fun (l, _, _) -> l) (let _, layers, _, _ = snapshot edited in layers) = [ "sky"; "sun" ]
         && contains (source edited) "(scene/world (ref sky)") ("a restack: " ^ source edited);
  same_after_reload edited "World member restack";
  (let world = List.assoc "white room" World.presets in
   let network = Result.get_ok (Layers.graph_of_world world) and values = snd (Layers.of_world world) in
   ignore (wrote "World member preset" ~level:(Document.Inside wid) doc (Sync.Layers (network, values))
     (with_scene (Document.with_layers doc wid network) (world_node (scene doc) values))));
  (* deleting the World leaves its merge input, its binding and its graph (nothing else reads it) *)
  let worldless = Document.prune (with_scene doc (KG.remove [ wid ] (scene doc))) in
  let edited = wrote "World member delete" doc (Sync.Delete [ wid ]) worldless in
  check (Objects.ids "world" (scene edited) = [] && not (contains (source edited) "scene/world")
         && not (contains (source edited) "graph sky") && contains (source edited) "scene/root")
    ("a deleted World: " ^ source edited);
  same_after_reload edited "World member delete";
  (* what is refused *)
  ignore (refused "E_SCENE_ROOT" {|(workspace a
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/root (scene/merge (scene/root (scene/merge (scene/geometry (ref g))))))))|});
  ignore (refused "E_SCENE_ROOT" {|(workspace a
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g)) (scene/root (scene/merge)))))|});
  let message = refused "E_SCENE_WORLD" {|(workspace a
    (graph g :context sop (sop/box))
    (graph sky :context world (world/sky))
    (graph scene :context scene
      (let* [one (scene/world (ref sky) :name "one")
             two (scene/world (ref sky) :name "two")]
        (scene/root (scene/merge (scene/geometry (ref g)) one two)))))|} in
  check (contains message "one" && contains message "two") ("E_SCENE_WORLD did not name both bindings: " ^ message);
  ignore (refused "E_SCENE_CAMERA" {|(workspace a
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (let* [elsewhere (scene/camera :name "away")]
        (scene/root (scene/merge (scene/geometry (ref g)) (scene/camera)) :camera elsewhere))))|});
  print_endline "scene root: old files, root settings and camera, Save round trip, part root, E_SCENE_ROOT/WORLD/CAMERA ok"

(* the ops of a gesture, applied together: all or none *)
let apply_ops (doc : Document.t) ops =
  let catalog = Result.get_ok (Kinds.catalog ~version:Flow_sop.Manifest.version factories) in
  List.fold_left (fun doc op ->
    Result.bind doc (fun (doc : Document.t) ->
      match Workspace_doc.edit catalog (doc.workspace.ws) op with
      | Error d -> Error (Diagnostic.to_string d)
      | Ok workspace -> Lowering.of_workspace ~factories ~previous:doc workspace
          |> Result.map_error Diagnostic.to_string)) (Ok doc) ops

let okx = function Ok d -> d | Error m -> failwith m

let graph_names (doc : Document.t) =
  List.map (fun (g : Flow.Workspace.graph) -> g.name) (doc.workspace.ws).checked.graphs

let geometries doc = List.length (Objects.ids "geometry" (scene doc))

let run_compose () =
  (* the composition gestures: one op list each, one undo entry (the ops apply together) *)
  let doc = open_text root_text in
  let added = okx (apply_ops doc (Sync.add_geometry doc ~existing:None)) in
  check (geometries added = geometries doc + 1 && List.mem "shape" (graph_names added)
         && contains (flat (source added)) "(scene/geometry (ref shape))"
         && contains (flat (source added)) "(scene/merge body cam side world object)")
    ("add geometry: " ^ source added);
  same_after_reload added "add geometry";
  let again = okx (apply_ops added (Sync.add_geometry added ~existing:(Some "shape"))) in
  check (geometries again = geometries added + 1 && graph_names again = graph_names added
         && contains (flat (source again)) "(scene/geometry (ref shape) :name \"shape 2\")")
    ("add geometry of an existing graph: " ^ source again);
  (* a failing step leaves nothing behind *)
  check (Result.is_error (apply_ops doc [ List.hd (Sync.add_geometry doc ~existing:None);
    Flow_graph.Flow_edit.Remove_graph { name = "nope" } ])) "a failed gesture was applied";
  (* one World only *)
  check (Result.is_error (Sync.add_world doc)) "a second World was offered";
  let bare = open_text {|(workspace a (graph g :context sop (sop/box))
    (graph scene :context scene (scene/root (scene/merge (scene/geometry (ref g))))))|} in
  let worlded = okx (apply_ops bare (Result.get_ok (Sync.add_world bare))) in
  check (Objects.ids "world" (scene worlded) <> []
    && contains (flat (source worlded)) "(scene/world (ref sky))"
    && contains (flat (source worlded)) "(graph sky :context world"
    && contains (flat (source worlded)) "(world/sun (world/sky))")
    ("add World: " ^ source worlded);
  same_after_reload worlded "add World";
  let call head args = Flow.Syntax.make (Flow.Syntax.List (Flow.Syntax.make (Flow.Syntax.Sym head) :: args)) in
  let sym name = Flow.Syntax.make (Flow.Syntax.Sym name) in
  (match apply_ops worlded [ Flow_graph.Flow_edit.Add_node { scope = [ "scene" ]; name = "again";
      expr = call "scene/world" [ call "ref" [ sym "sky" ] ] } ] with
   | Error message -> check (contains message "E_SCENE_WORLD" || contains message "World") ("one-World add: " ^ message)
   | Ok _ -> failwith "a second World was added to one root");
  let delete_world doc = ok (write doc (Sync.Delete (Objects.ids "world" (scene doc)))) in
  let deleted = delete_world worlded in
  check (Objects.ids "world" (scene deleted) = [] && not (List.mem "sky" (graph_names deleted))
    && not (contains (source deleted) "world/none")) "deleting a World did not remove its orphan graph";
  same_after_reload deleted "delete orphan World";
  let saved = Flow.Syntax.parse "(graph saved :context world (ref sky))" |> Result.get_ok |> List.hd in
  let shared = okx (apply_ops worlded [Flow_graph.Flow_edit.Set_graph {name="saved"; form=saved}]) in
  let sky_text doc =
    let graph = List.find (fun (g : Flow.Workspace.graph) -> g.name = "sky")
        (doc.Document.workspace.ws).checked.graphs in
    fst (Flow.Lisp.print [graph.form]) in
  let retained = delete_world shared in
  check (Objects.ids "world" (scene retained) = [] && List.mem "saved" (graph_names retained)
    && sky_text retained = sky_text shared && not (contains (source retained) "world/none"))
    "deleting a World changed the graph another reference still uses";
  same_after_reload retained "delete shared World";
  (* take a wire out of the merge: the object stays as an unwired binding, and wires back *)
  let rock = open_text {|(workspace rocks
    (graph rock :context sop (sop/box))
    (graph scatter :context sop (sop/box))
    (graph scene :context scene
      (let* [left (scene/geometry (ref rock))
             right (scene/geometry (ref rock) :translate [3 0 0] :name "right")
             pebbles (scene/geometry (ref scatter))
             all (scene/merge left right pebbles)]
        (scene/root all))))|} in
  let out = okx (apply_ops rock [ Flow_graph.Flow_edit.Disconnect
    { node = [ "scene"; "all" ]; key = Flow_graph.Flow_edit.Pos 2; fallback = None } ]) in
  check (geometries out = 2 && contains (flat (source out)) "pebbles (scene/geometry (ref scatter))"
         && contains (flat (source out)) "(scene/merge left right)") ("take out of the scene: " ^ source out);
  let back = okx (apply_ops out [ Flow_graph.Flow_edit.Connect
    { node = [ "scene"; "all" ]; key = Flow_graph.Flow_edit.Pos 2; src = "pebbles"; iter = false } ]) in
  check (geometries back = 3) "a wire taken out did not wire back";
  (* delete an object: its SOP graph goes only when nothing else references it *)
  let remove (doc : Document.t) label = ok (write doc (Sync.Delete [ node_id doc label ])) in
  let no_pebbles = remove rock "scatter" in
  check (not (List.mem "scatter" (graph_names no_pebbles)) && List.mem "rock" (graph_names no_pebbles)
         && not (contains (source no_pebbles) "pebbles")) ("delete frees an unshared graph: " ^ source no_pebbles);
  same_after_reload no_pebbles "delete object";
  let no_left = remove rock "rock" in
  check (List.mem "rock" (graph_names no_left) && geometries no_left = 2)
    "deleting one of two objects of a graph removed the graph";
  let no_right = remove no_left "right" in
  check (not (List.mem "rock" (graph_names no_right)) && List.mem "scatter" (graph_names no_right))
    ("deleting the last object of a graph kept it: " ^ source no_right);
  (* group into a new merge: between the selection and the old one *)
  let grouped = okx (apply_ops rock [ Flow_graph.Flow_edit.Group_merge
    { nodes = [ [ "scene"; "left" ]; [ "scene"; "right" ] ]; name = "pair" } ]) in
  check (contains (flat (source grouped)) "pair (scene/merge left right)"
         && contains (flat (source grouped)) "all (scene/merge pair pebbles)" && geometries grouped = 3)
    ("group: " ^ source grouped);
  same_after_reload grouped "group";
  (* E9: deleting or moving one layer of a World member is an edit of that binding: the graph's
     input, its comment, the expression and the other names stay *)
  let skies = open_text {x|(workspace skies
    (graph g :context sop (sop/box))
    (graph sky :context world [(warmth : float 0.5)]
      ;; the stack, bottom first
      (let* [base (world/sky :name "base")
             haze (world/gradient base :name "haze")
             sun (world/sun haze :name "sun" :intensity (* warmth 4))]
        sun))
    (graph scene :context scene (scene/root (scene/merge (scene/geometry (ref g)) (scene/world (ref sky))))))|x} in
  let kept what (doc : Document.t) =
    let text = flat (source doc) in
    check (contains text "[(warmth : float 0.5)]" && contains text "; the stack, bottom first"
           && contains text ":intensity (* warmth 4)" && contains text "base (world/sky")
      (what ^ " rewrote the world graph: " ^ source doc) in
  let wid, network = world_network skies in
  let haze = layer_id skies "haze" and sun = layer_id skies "sun" and base = layer_id skies "base" in
  let g = network in
  let edited = ok (write ~level:(Document.Inside wid) skies (Sync.Delete [ haze ])) in
  kept "deleting a layer" edited;
  check (not (contains (source edited) "haze") && contains (flat (source edited)) "(world/sun base")
    ("a deleted layer of a World member: " ^ source edited);
  same_after_reload edited "member layer delete";
  let swapped = KG.update g haze (fun n -> { n with input = None })
    |> (fun g -> KG.update g sun (fun n -> { n with input = None }))
    |> (fun g -> KG.update g haze (fun n -> { n with input = Some sun }))
    |> (fun g -> KG.update g sun (fun n -> { n with input = Some base })) in
  let swapped = Document.with_layers skies wid swapped in
  let edited = wrote "member layer move" ~level:(Document.Inside wid) skies (Sync.Restack [ base; sun; haze ]) swapped in
  kept "moving a layer" edited;
  check (List.map (fun (l, _, _) -> l) (let _, layers, _, _ = snapshot edited in layers) = [ "haze"; "sun"; "base" ])
    ("a moved layer of a World member: " ^ source edited);
  same_after_reload edited "member layer move";
  (* the first edit of a render setting writes a root beside a binding already named root *)
  let rooted = open_text {x|(workspace r (graph g :context sop (sop/box))
    (graph scene :context scene (let* [root (scene/geometry (ref g))] (scene/merge root))))|x} in
  let written = ok (write rooted (Sync.Root { rooted.root with width = 640 })) in
  check (written.homes.root <> None && written.root.width = 640 && contains (flat (source written)) "(scene/root (scene/merge root) :width 640)")
    ("a scene that binds root refused its first render setting: " ^ source written);
  (* a rename to a name another object has is refused: [:parent] reads names *)
  let family = open_text {x|(workspace f
    (graph scene :context scene
      (scene/merge (scene/light :name "a") (scene/light :name "b") (scene/light :name "c" :parent "b"))))|x} in
  let a = node_id family "a" in
  (match reconcile family (with_scene family (KG.update (scene family) a (fun n -> { n with label = "b" }))) with
   | Error message -> check (contains message "named") message
   | Ok _ -> failwith "a rename to another object's name was written");
  (* an object the host made is still its graph's after another graph's edit lowered the text again *)
  let pair = open_text "(workspace pair (graph g :context sop (sop/box)) (graph h :context sop (sop/box)))" in
  let relowered = okx (apply_ops pair [ Flow_graph.Flow_edit.Set_arg { node = [ "h"; "@result" ];
    key = Flow_graph.Flow_edit.Kw "uniform_scale"; sub = []; value = Flow.Syntax.make (Flow.Syntax.Num "2.0") } ]) in
  let moved = ok (reconcile relowered (set relowered "g" [ float "translate_x" 1. ])) in
  check (contains (flat (source moved)) "(scene/geometry (ref g) :translate [1.0 0.0 0.0])"
         && contains (source moved) "(scene/geometry (ref h))")
    ("a host object lost its graph after a re-lowering: " ^ source moved);
  print_endline "scene compose: add geometry (new and existing), add World, one World, take out and wire back, delete refcount, group ok"

(* Several scene graphs, each with a root of its own: a viewport renders as the root of the scene it
   shows, with that scene's World *)
let instances_text = {|(workspace days
  (graph g :context sop (sop/box))
  (graph noon :context world (world/sun (world/sky :name "noon") :name "sun"))
  (graph dusk :context world (world/sun (world/sky :name "dusk") :name "sun"))
  (graph set :context scene
    (scene/merge (scene/geometry (ref g) :name "body") (scene/camera :name "hero")))
  (graph day :context scene
    (let* [cam (scene/camera :name "noon-cam" :eye [0 1 8])
           all (scene/merge (ref set) cam (scene/world (ref noon) :name "Noon"))]
      (scene/root all :camera cam :renderer "Raster" :width 800 :height 450)))
  (graph night :context scene
    (let* [cam (scene/camera :name "dusk-cam" :eye [4 2 6] :fov 30)
           all (scene/merge (ref set) cam (scene/world (ref dusk) :name "Dusk" :exposure -2))]
      (scene/root all :camera cam :renderer "Path traced" :max_spp 1024 :bounces 12
                  :width 1600 :height 900)))
  (graph editor :context editor
    (ui/workspace (ui/split-at "horizontal" 0.5 (ui/viewport (ref day)) (ui/viewport (ref night))))))|}

let run_instances () =
  let module R = Objects.Root in
  let doc = open_text instances_text in
  let shell = Option.get doc.shell in
  check (List.length shell.preview_sources = 2) "two viewports, two scene instances";
  let root_of (_, (source : Document.preview_source)) = Lowering.instance_root source.instance in
  (* each instance's root, its size and its own camera object: the two differ *)
  (match List.map root_of shell.preview_sources with
   | [ Some day; Some night ] ->
       check (day.params.renderer = R.Raster) "day renders raster";
       check (night.params.renderer = R.Path_traced && night.params.max_spp = 1024 && night.params.bounces = 12)
         (Printf.sprintf "night's root was not read: %d spp, %d bounces" night.params.max_spp night.params.bounces);
       check ((day.params.width, day.params.height) = (800, 450)
              && (night.params.width, night.params.height) = (1600, 900)) "each root's own resolution";
       (match day.camera, night.camera with
        | Some d, Some n ->
            check (Rays.Camera.position d = Rays_math.Vec3.create 0. 1. 8.) "day looks from its own camera";
            check (Rays.Camera.position n = Rays_math.Vec3.create 4. 2. 6.) "night looks from its own camera";
            check (Rays.Camera.projection n <> Rays.Camera.projection d) "and with its own lens"
        | _ -> failwith "each root names its camera")
   | _ -> failwith "each viewport's scene instance names its root");
  (* the document stores them per viewport, beside the Worlds *)
  (match doc.view_roots with
   | [ (_, day); (_, night) ] ->
       check (day.camera <> None && night.camera <> None
              && day.params.width = 800 && night.params.width = 1600)
         "the document holds each viewport's root and camera"
   | _ -> failwith "each viewport over a root has an entry in the document");
  check (Lowering.instance_root (E.Struct ("scene/merge", Flow.Ty.scene, [])) = None) "a part has no root of its own";
  (* each viewport draws its own World: the document's scene (a part) has none, the two roots do *)
  check (Objects.ids "world" (scene doc) = []) "the part holds no World";
  (match doc.view_worlds with
   | [ (_, Some day); (_, Some night) ] ->
       check (day.node.label = "Noon" && night.node.label = "Dusk")
         "each viewport's World node is its own";
       check (Layers.to_world day.node day.layers <> None && Layers.to_world night.node night.layers <> None)
         "both Worlds bake";
       check (Layers.to_world day.node day.layers <> Layers.to_world night.node night.layers)
         "and they differ (exposure, layers)"
   | _ -> failwith "each viewport over a root names its World");
  (* objects are plain data: an object parented under its own child is refused, in the derived
     scene and in the text *)
  let a = KG.instance ~label:"a" Objects.Geometry.kind in
  let b = { (KG.instance ~label:"b" Objects.Geometry.kind) with input = Some a.id } in
  check (List.map (fun (n : KG.node) -> n.input) (Objects.reparent [ a; b ] [ a.id ] (Some b.id)) = [ None; Some a.id ])
    "a reparent under a child made a cycle";
  check (Result.is_error (Lowering.of_workspace ~factories (Ws_fixture.of_text {|(workspace c
    (graph g :context sop (sop/box))
    (graph scene :context scene (scene/merge (scene/geometry (ref g) :name "a" :parent "b")
      (scene/geometry (ref g) :name "b" :parent "a"))))|}))) "two objects parented under each other lowered";
  print_endline "scene instances: each viewport has its root and its World ok"
