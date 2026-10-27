(* The workspace document (text, layout by path, settings), presets as
   s-expressions, and the editor opened on a workspace: history labels, one
   entry per gesture, live `t` recooked and static nodes cached. *)
open Rays
open Flow_graph
module E = Flow_edit
module S = Flow.Syntax
module Doc = Editor_document.Workspace_doc
module Layout = Editor_document.Layout_by_path
module Preset = Editor_document.Preset
module Document = Editor_document.Document

let fail message = failwith ("test_workspace_doc: " ^ message)
let check condition message = if not condition then fail message
let factories = Sop_catalog.Editor.factories
let catalog = Editor_document.Contexts.catalog ~version:1 factories |> Result.get_ok
let show ds = String.concat "; " (List.map Diagnostic.to_string ds)

open Test_text

let schema = Param.(schema ~name:"doc-settings" ~default:(0., "a")
  [ field ~name:"amount" ~label:"Amount" ~kind:(floating ~min:0. ~max:10. ()) ~default:0.
      ~get:fst ~set:(fun amount (_, mode) -> amount, mode) ();
    field ~name:"mode" ~label:"Mode" ~kind:Text ~default:"a"
      ~get:snd ~set:(fun mode (amount, _) -> amount, mode) () ])
let settings (amount, mode) = Editor_document.Settings.make schema (amount, mode)

let still = {|; a small study
(workspace study
  (graph g :context sop [(n : int 3)]
    (let* [a (sop/uv_sphere :radius 0.5)
           ; moved right
           b (sop/transform a :translate [1 0 0])]
      b)))
|}

let moving = {|(workspace moving
  (graph g :context sop
    (let* [a (sop/uv_sphere :radius 0.5)
           b (sop/transform a :translate [(sin t) 0 0])]
      b)))
|}

let of_text ?settings text = match Doc.of_text ?settings catalog text with
  | Ok doc -> doc
  | Error ds -> fail (show ds)

(* bloom with 3 petals and a 2-petal accent: these checks read objects, layers and the window, not the petal count *)
let small_bloom () = List.fold_left (fun text (from, by) -> Str.global_replace (Str.regexp_string from) by text) (case "bloom")
  ["(petals : int 12)", "(petals : int 3)"; ":petals 7", ":petals 2"]

let full_edit catalog (doc : Doc.t) operation =
  E.apply_checked ~ops:doc.checked.ops ~library:doc.fragment
    ~checker:(Flow_sop.Catalog.check_workspace ~ops:doc.checked.ops ~library:doc.fragment)
    catalog doc.source operation

let part_text () =
  let text = still ^ {|
(layout
  (node ["g" "a"] :at [120 48] :level "full" :pinned true :collapsed false :rows {:radius false})
  (node ["g" "b"] :at [300.5 40])
  (frame ["g"] "Legs" :at [0 0] :size [200 100]))

(settings :amount 2.5 :mode "b")
|} in
  let doc = of_text ~settings:(settings (0., "a")) text in
  check (Doc.name doc = "study") "name";
  check (Layout.Path_map.find [ "g"; "a" ] doc.layout.at = (120., 48.)) "layout at";
  check (Layout.Path_map.find [ "g"; "a" ] doc.layout.level = Flow_graph.Projection.Full
         && Layout.Path_map.find [ "g"; "a" ] doc.layout.pinned) "layout level and pin";
  check (Layout.Path_map.find [ "g"; "b" ] doc.layout.at = (300.5, 40.)) "layout float";
  check ((List.hd (Layout.Path_map.find [ "g" ] doc.layout.frames)).title = "Legs") "frames";
  check (Editor_document.Settings.get schema doc.settings = (2.5, "b")) "settings read";
  let out = Doc.to_text doc in
  check (has out "; a small study" && has out "; moved right") "comments survive to_text";
  let again = of_text ~settings:(settings (0., "a")) out in
  check (Doc.to_text again = out) "to_text is a fixed point";
  check (again.layout = doc.layout) "layout round trip";
  (* an open expression is keyed by the leaf of a call that is only a card while open *)
  let opened = of_text "(workspace w (graph g :context value (let* [a (max 1.0 (+ 2.0 3.0))] a)))\n(layout (node [\"g\" \"a#1\"] :collapsed false))" in
  check (Layout.Path_map.find_opt [ "g"; "a#1" ] opened.layout.collapsed = Some false) "an open expression survives loading";
  (* a default setting is not written *)
  let plain = of_text ~settings:(settings (0., "a")) still in
  check (not (has (Doc.to_text plain) "settings") && not (has (Doc.to_text plain) "layout")) "empty layout and defaults are omitted";
  (* errors *)
  let bad text code = match Doc.of_text ~settings:(settings (0., "a")) catalog text with
    | Error ds -> check (List.exists (fun (d : Diagnostic.t) -> d.code = code) ds) (code ^ ": " ^ show ds)
    | Ok _ -> fail ("accepted: " ^ code) in
  bad (still ^ "(layout (nope))") "E_LAYOUT";
  bad (still ^ "(layout (node [\"g\"] :at [1]))") "E_LAYOUT";
  bad (still ^ "(settings :nope 1)") "E_SETTINGS";
  bad (still ^ "(settings :amount \"x\")") "E_SETTINGS";
  bad "(workspace w (graph g :context sop (sop/uv_sphere :nope 1)))" "E_UNKNOWN_PARAM";
  bad "(nothing)" "E_WORKSPACE"

let part_edit () =
  let text = still ^ {|
(layout
  (node ["g" "a"] :at [120 40])
  (node ["g" "b"] :at [300 40]))
|} in
  let doc = of_text text in
  let renamed = match Doc.edit catalog doc (E.Rename { node = [ "g"; "a" ]; to_ = "ball" }) with
    | Ok d -> d | Error d -> fail (Diagnostic.to_string d) in
  check (Layout.Path_map.mem [ "g"; "ball" ] renamed.layout.at && not (Layout.Path_map.mem [ "g"; "a" ] renamed.layout.at))
    "rename rewrites layout keys in the same transaction";
  check (Layout.Path_map.mem [ "g"; "b" ] renamed.layout.at) "other layout keys stay";
  check (has (Doc.to_text renamed) "(sop/transform ball") "rename rewrites the source";
  check (renamed.checked.name = "study") "re-checked";
  (* atomic: a refused edit leaves the document as it was *)
  (match Doc.edit catalog doc (E.Rename { node = [ "g"; "a" ]; to_ = "b" }) with
   | Error _ -> ()
   | Ok _ -> fail "accepted a clashing rename");
  (match Doc.edit catalog doc (E.Set_arg { node = [ "g"; "a" ]; key = Kw "nope"; sub = []; value = S.make (S.Num "1") }) with
   | Error _ -> ()
   | Ok _ -> fail "accepted an unknown keyword");
  check (Doc.to_text doc = Doc.to_text (of_text text)) "the original document is untouched";
  let deleted = match Doc.edit catalog (of_text (still ^ "(layout (node [\"g\" \"a\"] :at [1 2]) (node [\"g\" \"b\"] :at [3 4]))"))
      (E.Add_node { scope = [ "g" ]; name = "spare"; expr = S.make (S.Num "1") }) with
    | Ok d -> d | Error d -> fail (Diagnostic.to_string d) in
  (match Doc.edit catalog deleted (E.Delete_nodes { nodes = [ [ "g"; "spare" ] ] }) with
   | Ok d -> check (Layout.Path_map.cardinal d.layout.at = 2) "delete keeps other keys"
   | Error d -> fail (Diagnostic.to_string d))

let frame ?(keys = []) events count = Test_editor_input.frame ~keys (450., 320.) events count
let key k = Input.Key_pressed k
let char c = key (Input.KeyChar c)

let editor ?presets text =
  let workspace = of_text text in
  Rays_editor.Editor.create ~ui_text_size:11 ~await:true ~workspace ?presets
    ~prepare:(fun _ output -> Ok (Result.get_ok (Sop.Payload.geometry output.Sop.Session.payload)))
    ~draw:(fun _ _ -> Scene3.create []) () |> function
  | Ok e -> e | Error m -> fail m

let first_x geometry =
  let view = Rdk.Packed.Float3.Private.view (Rdk.Geometry.positions geometry) in
  view.x.(0)

(* update frames until [ok] holds: the editor awaits each frame's cook, so a bounded number of
   frames, not a wait on the clock *)
let settle ?(from = 0) e ok =
  let rec go count e =
    let e = Rays_editor.Editor.update e (frame [] count) in
    if ok e then e, count
    else if count > from + 200 then fail "the editor did not settle"
    else go (count + 1) e in
  go from e

let part_editor () =
  let module E3 = Rays_editor.Editor in
  let e = editor still in
  let objects e = List.map (fun (i : Sop.Edit_graph.node_info) -> i.label, i.operation)
    (Sop.Edit_graph.inspect (E3.document e)) in
  check (List.mem ("g", "geometry") (objects e)) "one geometry object per sop graph";
  let e, count = settle e (fun e -> E3.prepared e <> None) in
  let before = Option.get (E3.prepared e) in
  (* static: idle frames keep the cooked value (no recook) *)
  let e, count = settle ~from:count e (fun _ -> true) in
  let e, count = List.fold_left (fun (e, c) _ -> E3.update e (frame [] c), c + 1) (e, count) (List.init 20 Fun.id) in
  check (E3.prepared e == Some before || Option.get (E3.prepared e) == before) "static nodes stay cached";
  check (E3.undo_label e = None) "no edit yet";
  (* one gesture is one history entry, named by the op *)
  let scrub v = E.Set_arg { node = [ "g"; "a" ]; key = Kw "radius"; sub = []; value = S.make (S.Num v) } in
  let e = match E3.edit e (scrub "0.6") with Ok e -> e | Error m -> fail m in
  let e = E3.edit e (scrub "0.7") |> Result.get_ok in
  check (E3.undo_label e = Some "Edit value") "scrub label";
  check (has (Doc.to_text (E3.workspace e)) ":radius 0.7") "scrub rewrote the source";
  let e = E3.edit e (E.Rename { node = [ "g"; "b" ]; to_ = "moved" }) |> Result.get_ok in
  check (E3.undo_label e = Some "Rename") "rename label";
  let e = E3.edit e (E.Wrap { nodes = [ [ "g"; "a" ] ]; loop = For }) |> Result.get_ok in
  check (E3.undo_label e = Some "Repeat") "repeat label";
  check (match E3.edit e (E.Rename { node = [ "g"; "a" ]; to_ = "moved" }) with Error _ -> true | Ok _ -> false)
    "a refused edit is an error";
  check (E3.undo_label e = Some "Repeat") "a refused edit records nothing";
  let e, count = settle ~from:(count + 1) e (fun e ->
    match E3.prepared e with Some g -> Rdk.Geometry.point_count g > Rdk.Geometry.point_count before | None -> false) in
  (* undo steps back through the labelled entries *)
  let undo e c = E3.update e (frame ~keys:[ Input.Meta ] [ char 'z' ] c) in
  let e = undo e count in
  check (E3.undo_label e = Some "Rename" && E3.redo_label e = Some "Repeat")
    (Printf.sprintf "undo Repeat: undo=%s redo=%s" (Option.value ~default:"-" (E3.undo_label e))
       (Option.value ~default:"-" (E3.redo_label e)));
  let e = undo e (count + 1) in
  check (E3.undo_label e = Some "Edit value" && E3.redo_label e = Some "Rename") "undo Rename";
  let e = undo e (count + 2) in
  check (E3.undo_label e = None && E3.redo_label e = Some "Edit value") "one undo reverts the merged scrub";
  check (Doc.to_text (E3.workspace e) = Doc.to_text (of_text still)) "undo restored the source";
  let e = E3.update e (frame ~keys:[ Input.Meta; Input.Shift ] [ char 'z' ] (count + 3)) in
  check (E3.undo_label e = Some "Edit value") "redo";
  E3.close e

let part_conditional_history () =
  let module E3 = Rays_editor.Editor in
  let e = editor {|(workspace history
    (graph g :context value
      (let* [a (+ 1 2)
             b (if true (let* [u (+ a 1)] u) (+ a 2))
             c (cond true (+ a 3) false (+ a 4) :else (+ a 5))]
        (+ b c))))|} in
  let before = Doc.to_text (E3.workspace e) in
  let e, _ = List.fold_left (fun (e, count) op ->
    let changed = E3.edit e op |> Result.get_ok in
    check (E3.undo_label changed = Some (E.label op)) "conditional gesture history label";
    let undone = E3.update changed (frame ~keys:[Input.Meta] [char 'z'] count) in
    check (Doc.to_text (E3.workspace undone) = before && E3.undo_label undone = None
      && E3.redo_label undone = Some (E.label op)) ("conditional gesture was not one undo step: " ^ E.label op);
    undone, count + 1) (e, 1)
    [ E.Wrap {nodes = [["g"; "a"]]; loop = If};
      Set_arg {node = ["g"; "c"]; key = Pos 0; sub = []; value = S.make (Sym "false")};
      Add_arm {node = ["g"; "c"]; after = 0};
      Delete_arm {node = ["g"; "c"]; index = 0};
      Unfold {node = ["g"; "b"]; key = Arm (-1); sub = []};
      Fold_into {node = ["g"; "b#then"; "u"]};
      Toggle_bypass {node = ["g"; "b"]} ] in
  E3.close e

let part_live () =
  let module E3 = Rays_editor.Editor in
  let e = editor moving in
  let e, count = settle e (fun e -> E3.prepared e <> None) in
  let xs = ref [] in
  let e = ref e and count = ref count in
  while List.length (List.sort_uniq compare !xs) < 4 && !count < 400 do
    e := E3.update !e (frame [] !count);
    incr count;
    Option.iter (fun g -> xs := first_x g :: !xs) (E3.prepared !e)
  done;
  check (List.length (List.sort_uniq compare !xs) >= 4) "a time-driven workspace animates in the editor";
  E3.close !e

let part_preset () =
  let module E3 = Rays_editor.Editor in
  let directory = Filename.temp_dir "rays-workspace-presets" "" in
  Fun.protect ~finally:(fun () ->
    let state = Filename.concat directory "state" in
    if Sys.file_exists state then begin
      Array.iter (fun name -> Sys.remove (Filename.concat state name)) (Sys.readdir state);
      Unix.rmdir state
    end;
    Array.iter (fun f -> Sys.remove (Filename.concat directory f)) (Sys.readdir directory);
    Unix.rmdir directory) (fun () ->
    let e = editor ~presets:directory still in
    let e, count = settle e (fun e -> E3.prepared e <> None) in
    let e = E3.edit e (E.Set_note { node = [ "g"; "a" ]; text = "kept" }) |> Result.get_ok in
    let e = E3.update e (frame [ key (Input.KeyChar '/'); char 's' ] (count + 1)) in
    let e = E3.update e (frame [ key Input.Enter ] (count + 2)) in
    let name = match Preset.list ~directory with [ (name, _) ] -> name | _ -> fail "/ s did not save one preset" in
    let path = Preset.path ~directory ~name in
    check (Filename.check_suffix path ".rays" && Sys.file_exists path) "/ s saved a .rays";
    let text = In_channel.with_open_bin path In_channel.input_all in
    check (has text "(workspace study" && has text "; kept" && has text "(view") "the preset is s-expressions with its comments";
    check (not (has text "{\"") && not (has text "\"version\"")) "no JSON";
    (* it loads: same source, same comments *)
    let loaded = Preset.load ~path ~factories ~settings:Editor_document.Settings.none () |> Result.get_ok in
    let ws = fst (loaded.doc.workspace) in
    check (Doc.to_text ws = Doc.to_text (E3.workspace e)) "save then load round trip";
    check (Preset.list ~directory |> List.map fst = [ name ]) "listed";
    (* load through / b: one undo entry *)
    let e = E3.edit e (E.Rename { node = [ "g"; "a" ]; to_ = "ball" }) |> Result.get_ok in
    let e = E3.update e (frame [ key (Input.KeyChar '/'); char 'b' ] (count + 3)) in
    let e = E3.update e (frame [ Input.Text_input name; key Input.Enter ] (count + 4)) in
    let e = E3.update e (frame [] (count + 5)) in
    check (has (Doc.to_text (E3.workspace e)) "(sop/transform a ") "/ b restored the saved source";
    check (E3.undo_label e = Some "Load preset") "load is one undo entry";
    (* errors never touch the document *)
    let corrupt = Preset.path ~directory ~name:"corrupt" in
    Out_channel.with_open_text corrupt (fun c -> output_string c "(workspace");
    check (Result.is_error (Preset.load ~path:corrupt ~factories ~settings:Editor_document.Settings.none ())) "corrupt text is an error";
    check (Result.is_error (Preset.save ~directory ~name:"x" ~doc:loaded.doc ~view:Flow.Syntax.(make (Map [ make (Kw "zoom"); make (Num "inf") ]))))
      "a nonfinite view is refused";
    check (Result.is_error (Preset.save ~directory ~name:"" ~doc:loaded.doc ~view:(Flow.Syntax.make (Flow.Syntax.Map [])))) "an empty name is refused";
    E3.close e)

(* the view round trips through s-expressions *)
let part_view () =
  let directory = Filename.temp_dir "rays-workspace-view" "" in
  Fun.protect ~finally:(fun () ->
    Array.iter (fun f -> Sys.remove (Filename.concat directory f)) (Sys.readdir directory);
    Unix.rmdir directory) (fun () ->
    let doc = match Editor_document.Contexts.of_workspace ~factories (of_text still) with
      | Ok d -> d | Error d -> fail (Diagnostic.to_string d) in
    let view = match Flow.Syntax.parse "{:eye [1.5 -2.0 3] :look_through true :name \"a \\\"b\\\"\"}" with
      | Ok [ v ] -> v | _ -> fail "view text" in
    let path = Preset.save ~directory ~name:"v" ~doc ~view |> Result.get_ok in
    let loaded = Preset.load ~path ~factories ~settings:Editor_document.Settings.none () |> Result.get_ok in
    let print v = fst (Flow.Lisp.print [ v ]) in
    check (print loaded.view = print view) "view round trip")


(* W4: the editor's layout gestures edit the layout keys only. *)
let part_pane_layout () =
  let module M = Layout.Path_map in
  let doc = of_text still in
  let opened = Editor_document.Contexts.of_workspace ~factories doc |> function Ok d -> d | Error m -> fail (Diagnostic.to_string m) in
  let ws = fst (opened.workspace) in
  let ws = { ws with layout = { ws.layout with at = M.add [ "g"; "a" ] (40., 60.) ws.layout.at;
                                               collapsed = M.add [ "g"; "b" ] true ws.layout.collapsed;
                                               level = M.add [ "g"; "gone" ] Flow_graph.Projection.Full ws.layout.level;
                                               pinned = M.add [ "nograph"; "a" ] true ws.layout.pinned } } in
  let again = of_text (Doc.to_text ws) in
  check (M.find [ "g"; "a" ] again.layout.at = (40., 60.) && M.mem [ "g"; "b" ] again.layout.collapsed)
    "moved items and collapsed zones round-trip through the s-expression";
  (* a key outlives nothing: entries of a binding or a graph that is not there are dropped on load *)
  check (M.is_empty again.layout.level && M.is_empty again.layout.pinned) "stale layout keys were kept";
  (* and by the edit that removes what they name *)
  let two = of_text "(workspace w (graph a :context sop (sop/box)) (graph b :context sop (let* [x (sop/box)] x)))\n(layout (node [\"b\" \"x\"] :at [10 20]))" in
  check (M.mem [ "b"; "x" ] two.layout.at) "a live key was dropped";
  let removed = Doc.edit catalog two (E.Remove_graph { name = "b" }) |> Result.get_ok in
  check (Layout.is_empty removed.layout) "the layout of a removed graph was kept";
  check (Result.is_ok (Doc.of_text catalog (Doc.to_text removed))) "the text after removing a graph does not load";
  (* comments between and after the root forms, and a view form, survive to_text *)
  let noted = of_text "(workspace w (graph a :context sop (let* [x (sop/box)] x)))\n; about the view\n(view {:eye [1 2 3]})\n; where things are\n(layout (node [\"a\" \"x\"] :at [10 20]))\n; the end\n" in
  let out = Doc.to_text noted in
  check (has out "; about the view" && has out "(view {:eye [1 2 3]})" && has out "; where things are" && has out "; the end")
    ("root forms and their comments: " ^ out);
  check (Doc.to_text (of_text out) = out) "to_text with root comments is a fixed point";
  (* a layout or settings error points at its form *)
  (match Doc.of_text catalog "(workspace w (graph a :context sop (sop/box)))\n(layout (nonsense))" with
   | Error [ { code = "E_LAYOUT"; span = Some { start; _ }; _ } ] -> check (start > 0) "E_LAYOUT span"
   | _ -> fail "E_LAYOUT has no span")

(* W10: scene, world and settings graphs become the document's objects, World and settings *)

let part_contexts () =
  let module Objects = Editor_document.Objects in
  let module Layers = Editor_document.Layers in
  let module Contexts = Editor_document.Contexts in
  let module Edit = Sop.Edit_graph in
  let build ?previous ws = match Contexts.of_workspace ~factories ?previous ws with
    | Ok d -> d | Error d -> fail (Diagnostic.to_string d) in
  let bloom = of_text (small_bloom ()) in
  let doc = build bloom in
  let scene = Document.scene_graph doc in
  let infos = Edit.inspect scene in
  let ops op = List.filter (fun (i : Edit.node_info) -> i.operation = op) infos in
  let label (i : Edit.node_info) = i.label in
  check (List.map label (ops "geometry") = [ "flower"; "accent" ]) "the scene graph's geometry objects, by :name";
  check (List.length (ops "camera") = 1 && List.length (ops "light") = 1 && List.length (ops "world") = 1)
    "camera, light and World come from the workspace";
  check (Document.validate doc = Ok ()) "the document validates";
  let accent = (List.nth (ops "geometry") 1).node in
  (match Objects.geometry accent with
   | Some p -> check (p.translate_x = 2.2 && p.translate_z = -1.2 && p.scale_x = 0.55 && p.scale_y = 0.55
                      && p.rotate_y = 0.) "a vec3 keyword sets its three fields"
   | None -> fail "accent is not a geometry object");
  (match Objects.Camera.of_node (List.hd (ops "camera")).node with
   | Some (camera, _) -> check (Rays.Camera.position camera = Rays_math.Vec3.create 0.5 1.8 6.) "camera eye"
   | None -> fail "no camera");
  (match Objects.light (List.hd (ops "light")).node with
   | Some p -> check (p.intensity = 60. && p.translate_x = 4.) "light fields"
   | None -> fail "no light");
  check (doc.active_camera = Some (List.hd (ops "camera")).id) "the workspace's camera is the active one";
  (* each geometry object owns the lowered network of the sop graph it refers to *)
  let flower = (List.hd (ops "geometry")).id and accent_id = (List.nth (ops "geometry") 1).id in
  let network id = Option.get (Document.Int_map.find_opt id doc.networks) in
  check ((network flower).displayed <> None && (network accent_id).displayed <> None
         && (network flower).graph != (network accent_id).graph)
    "two refs of one graph with different inputs are two networks";
  (* the World: a node and its layer stack, bottom first *)
  let world_id = (List.hd (ops "world")).id in
  let world_node = (List.hd (ops "world")).node in
  check (Sop.Node.label world_node = "Bloom study") "the World's name";
  (match Layers.to_world world_node (network world_id) with
   | Some w ->
       check (w.exposure = -0.5) "World exposure";
       check (match w.layers with
         | [ { layer = Rays.World.Sky s; _ }; { layer = Rays.World.Sun u; _ } ] ->
             s.turbidity = 3. && u.intensity = 1500.
         | _ -> false) "World layers, bottom first"
   | None -> fail "the World does not read");
  (* settings: the window the host opens *)
  let window = match Contexts.window bloom with Ok w -> w | Error d -> fail (Diagnostic.to_string d) in
  check (window = { Contexts.title = "Bloom study"; width = 1400; height = 800; fps = 60; seed = 42; fast = true })
    "settings graph: window title, size, fps (a macro call) and seed";
  (match Contexts.window (of_text (case "tree")) with
   | Ok w -> check (w.title = "Rays" && w.fps = 60 && w.fast) "no settings graph: defaults, fast precision"
   | Error d -> fail (Diagnostic.to_string d));
  (* :precision "exact" keeps the CPU; Contexts.fast reads it from a document every frame *)
  let exact = of_text "(workspace e (graph g :context value 1.0) (graph s :context settings (settings/config :precision \"exact\")))" in
  (match Contexts.window exact with
   | Ok w -> check (not w.fast) "settings :precision \"exact\" is not fast"
   | Error d -> fail (Diagnostic.to_string d));
  check (Contexts.fast doc) "a document without :precision is fast";
  check (List.exists (fun (f : Param.field_view) -> f.name = "title" && f.current = Text_value "Bloom study")
    (Editor_document.Settings.fields doc.settings)) "the settings graph is the document's settings";
  (* determinism, print/parse and preset round trips *)
  (* ids are fresh per build; everything else is a function of the source *)
  let shape (d : Document.t) =
    List.map (fun (i : Edit.node_info) -> i.operation, i.label, i.parameters) (Edit.inspect (Document.scene_graph d)),
    List.filter_map (fun (i : Edit.node_info) -> if i.operation <> "world" then None else
      Option.bind (Document.Int_map.find_opt i.id d.networks) (fun n -> Layers.to_world i.node n))
      (Edit.inspect (Document.scene_graph d)),
    Editor_document.Settings.fields d.settings in
  check (shape (build bloom) = shape doc) "the same source builds the same document twice";
  check (Document.dump (build ~previous:doc bloom) = Document.dump doc) "rebuilding over the previous document changes nothing";
  check (Doc.to_text (of_text (Doc.to_text bloom)) = Doc.to_text bloom) "print and parse are a fixed point";
  let directory = Filename.temp_dir "rays-contexts" "" in
  Fun.protect ~finally:(fun () ->
    Array.iter (fun f -> Sys.remove (Filename.concat directory f)) (Sys.readdir directory);
    Unix.rmdir directory) (fun () ->
    let path = Preset.save ~directory ~name:"bloom" ~doc ~view:(Flow.Syntax.make (Flow.Syntax.Map [])) |> Result.get_ok in
    let text = In_channel.with_open_bin path In_channel.input_all in
    check (not (has text "(settings :")) "settings owned by a graph are not written a second time";
    let loaded = Preset.load ~path ~factories ~settings:Editor_document.Settings.none () |> Result.get_ok in
    check (shape loaded.doc = shape doc) "the preset round trips scene, World and settings");
  (* a Flow_edit on the scene graph rebuilds it; ids, and viewport edits of unmentioned fields, stay *)
  let edited = match Doc.edit catalog bloom (E.Set_arg { node = [ "scene"; "accent" ]; key = Kw "translate";
      sub = []; value = Flow.Syntax.make (S.Vec (List.map (fun n -> S.make (S.Num n)) [ "4"; "0"; "1" ])) }) with
    | Ok d -> d | Error d -> fail (Diagnostic.to_string d) in
  let doc2 = build ~previous:doc edited in
  let accent2 = List.nth (List.filter (fun (i : Edit.node_info) -> i.operation = "geometry")
    (Edit.inspect (Document.scene_graph doc2))) 1 in
  check (accent2.id = accent_id) "an edit keeps the object's id";
  check ((Option.get (Objects.geometry accent2.node)).translate_x = 4.) "the edit reaches the object";
  check (has (Doc.to_text edited) ":translate [4 0 1]") "the source carries the edit";
  (* negative lowering: computed arguments outside a schema bound, and a wrong result *)
  let settings_of body = of_text ("(workspace w (graph c :context settings [(f : int 60)] " ^ body ^ "))") in
  check (Result.is_ok (Contexts.window (settings_of "(settings/config :fps f)"))) "a settings graph with an input";
  (* a literal past a hard bound is a check error; a computed one is clamped like any write *)
  check ((Result.get_ok (Contexts.window (settings_of "(settings/config :fps (* f 100))"))).fps = 240)
    "a computed fps above the hard bound is clamped when lowered";
  check (match Contexts.of_workspace ~factories (of_text "(workspace w (graph x :context world (world/sky)))") with
    | Ok doc -> Objects.ids "world" (Document.scene_graph doc) = [] | Error _ -> false)
    "a world graph no scene/world references is no World";
  (* the implicit scene: without a scene graph, one geometry object per sop graph *)
  let plain = build (of_text still) in
  check (List.map label (List.filter (fun (i : Edit.node_info) -> i.operation = "geometry")
    (Edit.inspect (Document.scene_graph plain))) = [ "g" ]) "no scene graph: one object per sop graph";
  (* Variations: one garden object, the graph opened for a viewport *)
  let variations = build (of_text (case "variations")) in
  check (List.map label (Edit.inspect (Document.scene_graph variations))
         = [ "garden"; "garden (v1.1.1)"; "garden (v1.1.2)"; "garden (v1.1.3)" ]) "variations scene";
  ()

(* W10: the scene's objects and the World come from the workspace *)
let part_editor_contexts () =
  let module E3 = Rays_editor.Editor in
  let e = ref (editor (small_bloom ())) in
  let count = ref 0 in
  let step events = incr count; e := E3.update !e (frame events !count) in
  for _ = 1 to 10 do step [] done;
  let dump () =
    let directory = Filename.temp_dir "rays-contexts-dump" "" in
    Fun.protect ~finally:(fun () ->
      Array.iter (fun f -> Sys.remove (Filename.concat directory f)) (Sys.readdir directory);
      Unix.rmdir directory) (fun () ->
      E3.crash_dump !e directory;
      In_channel.with_open_bin (Filename.concat directory "editor.txt") In_channel.input_all) in
  let objects () = List.map (fun (i : Sop.Edit_graph.node_info) -> i.label, i.operation)
    (Sop.Edit_graph.inspect (E3.document !e)) in
  check (List.mem ("accent", "geometry") (objects ()) && List.mem ("Bloom study", "world") (objects ()))
    "the editor opens the workspace's objects and World";
  check (List.length (List.filter (fun (_, op) -> op = "camera") (objects ())) = 1
         && List.length (List.filter (fun (_, op) -> op = "light") (objects ())) = 1)
    "a scene that declares a camera and a light gets no seeded ones";
  check (has (dump ()) "pane graph: scene\n") "the pane shows the scene graph at the scene level until an outline row or a named panel picks another";
  (* an edit on the scene graph reaches the objects, in one history entry *)
  let e' = E3.edit !e (E.Set_arg { node = [ "scene"; "accent" ]; key = Kw "scale"; sub = [];
    value = S.make (S.Vec (List.map (fun n -> S.make (S.Num n)) [ "1"; "1"; "1" ])) }) |> Result.get_ok in
  check (E3.undo_label e' = Some "Edit value") "one history entry";
  check (List.exists (fun (i : Sop.Edit_graph.node_info) -> i.label = "accent"
    && (Option.get (Editor_document.Objects.geometry i.node)).scale_x = 1.) (Sop.Edit_graph.inspect (E3.document e')))
    "the scene edit reached the object"

let part_literals () =
  let module L = Flow_sop.Lower in
  let text = {|(workspace w
    (defmacro identity [x] `(sop/transform ~x))
    (graph g :context sop
      (let* [a (sop/box :size [2 2 2] :consolidate_points true :normals "Point")
             b (sop/transform a :translate [1 0 0])
             c (sop/set_color b :color "#ff8800")]
        c)))|} in
  let doc = ref (of_text text) in
  let lowered = ref (L.of_checked ~factories (!doc).checked |> Result.get_ok) in
  let syntax text = Flow.Syntax.parse text |> Result.get_ok |> List.hd in
  let op path field sub text = E.Set_arg {node = path; key = Kw field; sub; value = syntax text} in
  List.iter (fun operation ->
    let old = !doc in
    let (source, checked) = full_edit catalog old operation |> Result.get_ok in
    let next, phases = Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () ->
      Doc.edit catalog old operation |> Result.get_ok) in
    List.iter (fun phase -> check (Flow.Phase_timer.calls phases phase = 0)
      ("literal repeated " ^ phase)) ["print"; "parse"; "check"; "project"];
    check (Doc.to_text next = Doc.to_text {next with source; checked}) "literal save differs from slow edit";
    let changes = Doc.literal_changes ~previous:old next |> Option.get in
    let patched, phases = Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () ->
      List.fold_left (fun lowered change -> Editor_document.Literal_edit.lower lowered change
        |> Option.get |> Result.get_ok) !lowered changes) in
    check (Flow.Phase_timer.calls phases "lower" = 0 && Flow.Phase_timer.calls phases "evaluate" = 0)
      "literal re-evaluated or re-lowered";
    let reference = L.of_checked ~factories ~compiled_ids:patched.compiled_ids ~sites:patched.sites checked
      |> Result.get_ok in
    let same a b = Marshal.to_string a [Marshal.No_sharing] = Marshal.to_string b [Marshal.No_sharing] in
    check (same patched.plan reference.plan && same patched.evaluated.records reference.evaluated.records)
      "literal plan or probe records differ from full evaluation";
    if has (Doc.to_text next) "#0088ff" then List.iter2 (fun (a : L.graph) (b : L.graph) ->
      List.iter (fun domains ->
        let a = Test_support.Workspace_cook.cook ~domains a |> Option.get
        and b = Test_support.Workspace_cook.cook ~domains b |> Option.get in
        check (Test_support.Workspace_cook.geometry_bytes a = Test_support.Workspace_cook.geometry_bytes b)
          "literal cook differs from full evaluation") [3]) patched.graphs reference.graphs;
    let scope = Projection.of_graph catalog old.checked "g" in
    let arguments = List.map (fun (change : Editor_document.Literal_edit.change) ->
      change.path, E.Kw change.field, change.expr) changes in
    let patched_scope = Projection.with_arguments arguments scope in
    let reference_scope = Projection.of_graph catalog checked "g" in
    let row (r : Projection.row) = r.label, r.ty, r.chip, Option.map Flow.Lisp.flat r.expr in
    check (List.map (fun (n : Projection.node) -> n.path, List.map row n.rows) patched_scope.nodes
      = List.map (fun (n : Projection.node) -> n.path, List.map row n.rows) reference_scope.nodes)
      "literal projected rows differ from a full projection";
    let module Pane = Pxui_graph.Scope in
    let pane = Pane.create () |> Pane.with_scope ~key:"g" scope |> Pane.select [["g";"b"]] in
    let updated = Pane.with_arguments arguments pane in
    check (Pane.selected updated = Pane.selected pane
      && List.for_all (fun (node : Projection.node) ->
        Pane.Private.box_of pane node.path = Pane.Private.box_of updated node.path
        && Pane.Private.ports pane node.path = Pane.Private.ports updated node.path) scope.nodes
      && Pane.Private.wire_count pane = Pane.Private.wire_count updated
      && List.for_all (fun i -> Pane.Private.wire_points pane i = Pane.Private.wire_points updated i)
        (List.init (Pane.Private.wire_count pane) Fun.id))
      "literal pane update changed selection, boxes, ports or wire routes";
    doc := next; lowered := patched)
    [op ["g";"a"] "size" [] "[3 4 5]";
     op ["g";"b"] "translate" [0] "17";
     op ["g";"c"] "color" [] "\"#0088ff\"";
     op ["g";"a"] "consolidate_points" [] "false"];
  List.iter (fun operation ->
    let slow = full_edit catalog !doc operation
    and fast = Doc.edit catalog !doc operation in
    check (match slow, fast with Error a, Error b -> a.code = b.code | _ -> false)
      "invalid literal did not refuse atomically with the checker's code")
    [op ["g";"a"] "normals" [] "\"unknown\""; op ["g";"c"] "color" [] "\"bad\"";
     op ["g";"a"] "size" [0] "true"];
  (* Source identity survives width changes and separate edits before lowering. *)
  let first = !doc in
  let next = Doc.edit catalog first (op ["g";"b"] "translate" [1] "88") |> Result.get_ok in
  let next = Doc.edit catalog next (op ["g";"a"] "size" [2] "27") |> Result.get_ok in
  check (List.length (Option.get (Doc.literal_changes ~previous:first next)) = 2)
    "two literals in one frame lost a change";
  let base = of_text still in
  let first = Doc.edit catalog base (op ["g";"a"] "radius" [] "0.6") |> Result.get_ok in
  let second = Doc.edit catalog first (op ["g";"b"] "translate" [0] "7") |> Result.get_ok in
  check (List.length (Option.get (Doc.literal_changes ~previous:second first)) = 1)
    "undo of another field lost the reverse literal change";
  let spelling = Doc.edit catalog first (op ["g";"a"] "radius" [] "0.60") |> Result.get_ok in
  check (List.length (Option.get (Doc.literal_changes ~previous:first spelling)) = 1)
    "a spelling-only edit left the pane's authored expression stale";
  List.iter (fun (text, operation, fallback) ->
    let old = of_text text in
    let next, phases = Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () ->
      Doc.edit catalog old operation |> Result.get_ok) in
    check (Flow.Phase_timer.calls phases "check" = 0 && Flow.Phase_timer.calls phases "parse" = 0)
      "literal with notes or in a loop repeated the workspace check";
    let source, checked = full_edit catalog old operation |> Result.get_ok in
    check (Doc.to_text next = Doc.to_text {next with source; checked}) "loop/comment literal text differs";
    let lowered = L.of_checked ~factories old.checked |> Result.get_ok in
    let patched, phases = Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () ->
      Editor_document.Literal_edit.lower lowered
        (List.hd (Option.get (Doc.literal_changes ~previous:old next)))) in
    if fallback then check (patched = None) "geometry template or retained function did not use the full lowering fallback"
    else begin
      check (Flow.Phase_timer.calls phases "lower" = 0 && Flow.Phase_timer.calls phases "evaluate" = 0)
        ("literal lowered or evaluated: " ^ text);
      let patched = patched |> Option.get |> Result.get_ok in
      let full = L.of_checked ~factories ~compiled_ids:patched.compiled_ids ~sites:patched.sites checked |> Result.get_ok in
      check (Marshal.to_string patched.plan [Marshal.No_sharing] = Marshal.to_string full.plan [Marshal.No_sharing])
        ("literal plan differs: " ^ text);
      if has text "(for " then List.iter2 (fun (a : L.graph) (b : L.graph) ->
        let a = Test_support.Workspace_cook.cook ~domains:1 a |> Option.get
        and b = Test_support.Workspace_cook.cook ~domains:3 b |> Option.get in
        check (Test_support.Workspace_cook.geometry_bytes a = Test_support.Workspace_cook.geometry_bytes b)
          "static loop literal cook differs") patched.graphs full.graphs
    end)
    ["(workspace w (graph g :context sop (let* [a (sop/uv_sphere :radius ; radius note\n0.5)] a)))",
      op ["g";"a"] "radius" [] "0.7", false;
     "(workspace w (graph g :context sop (let* [copies (for [n (range 3)] (let* [a (sop/box :size [1 1 1])] a))] (sop/merge copies))))",
      op ["g";"copies";"a"] "size" [0] "2", false;
     "(workspace w (graph g :context sop (let* [a (sop/transform (sop/box :size [1 1 1])) b (sop/box :size [1 1 1])] (sop/merge a b))))",
      op ["g";"a#0"] "size" [0] "2.5", false;
     "(workspace w (graph g :context sop (-> (sop/box :size [1 1 1]) (sop/transform) (sop/transform))))",
      op ["g";"@result#0#0"] "size" [] "[2 3 4]", false;
     "(workspace w (defn unit_box :context sop [] (sop/box :size [1 1 1])) (graph g :context sop (sop/merge (unit_box) (unit_box))))",
      op ["def:unit_box";"@result"] "size" [0] "2.5", false;
     "(workspace w (graph g :context sop [(count : int 2)] (sop/box :size [1 1 1])) (graph copies :context sop (sop/merge (ref g) (ref g :count 3))))",
      op ["g";"@result"] "size" [0] "2.5", false;
     "(workspace w (graph g :context sop (let* [a (sop/box :size [1 1 1]) make_box (fn [n] (sop/box)) copies (map make_box (range 3))] (sop/merge a copies))))",
      op ["g";"a"] "size" [0] "2.5", true];
  let module E3 = Rays_editor.Editor in
  let e = editor still in
  Fun.protect ~finally:(fun () -> E3.close e) (fun () ->
    let e = E3.update e (frame [] 0) in
    let operation = op ["g";"a"] "radius" [] "0.7" in
    let e, phases = Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () ->
      let e = E3.edit e operation |> Result.get_ok in E3.update e (frame [] 1)) in
    List.iter (fun phase -> check (Flow.Phase_timer.calls phases phase = 0)
      ("editor literal repeated " ^ phase)) ["print"; "parse"; "check"; "evaluate"; "lower"; "project"; "layout"];
    let source, checked = full_edit catalog (of_text still) operation |> Result.get_ok in
    let ws = E3.workspace e in
    check (Doc.to_text ws = Doc.to_text {ws with source; checked}) "editor literal save differs");
  print_endline "workspace literal edits: source, checked terms, plans, records, projected rows and cooked bytes match the full path"

(* Every checked-in [.rays] with a scrubbable literal (Step 1's file coverage): a literal
   edit writes the bytes the full path prints and refuses with its code; one that takes the
   literal path repeats no print, parse or check, and its patched lowering has the full
   lowering's plan.  Prints the coverage and the full path's cost per file. *)
let part_files () =
  let module L = Flow_sop.Lower in
  let catalog = Rays_editor.workspace_catalog () |> Result.get_ok in
  let rec rays dir = Array.to_list (Sys.readdir dir) |> List.concat_map (fun name ->
    let path = Filename.concat dir name in
    if Sys.is_directory path then rays path
    else if Filename.check_suffix name ".rays" then [path] else []) in
  let files = List.sort compare (rays "../../../sketches") @ ["../../../specification/pxui-kit/kit.rays"] in
  (* the seven files that cost 6-30 s each (a lowering of each, three per literal) run under RAYS_SLOW=1 only *)
  let heavy = ["flow_particles_gpu/"; "ws_sunflower/"; "ws_tiles/"; "sop_gallery/"; "flow_field/"; "ws_bloom/"; "ws_garland/"] in
  let files = if Sys.getenv_opt "RAYS_SLOW" = Some "1" then files
    else List.filter (fun f -> not (List.exists (fun h -> has f h) heavy)) files in
  let literal (form : S.t) = match form.node with
    | Num text -> (match int_of_string_opt text with
        | Some n -> Some (S.make (S.Num (string_of_int (n + 1))))
        | None -> Option.map (fun x -> S.make (S.Num (Flow.Lisp.float (x +. 0.5)))) (float_of_string_opt text))
    | Sym "true" -> Some (S.make (S.Sym "false")) | Sym "false" -> Some (S.make (S.Sym "true"))
    | _ -> None in
  let rec sites (scope : Projection.scope) = List.concat_map (fun (node : Projection.node) ->
    List.filter_map (fun (row : Projection.row) -> match row.kind, row.expr with
      | Arg, Some ({ node = Vec (first :: _); _ } as expr) ->
          Option.map (fun value -> node.path, row.key, [0], expr, value) (literal first)
      | Arg, Some expr -> Option.map (fun value -> node.path, row.key, [], expr, value) (literal expr)
      | _ -> None) node.rows
    @ Option.fold ~none:[] ~some:(fun (zone : Projection.zone) -> sites zone.scope) node.zone) scope.nodes in
  let covered = ref 0 and windows = ref 0 and seen = ref 0 in
  (* one file in six is opened and swept (RAYS_SLOW=1: all) *)
  List.iter (fun file ->
    incr seen;
    if !seen mod 6 = 1 || Sys.getenv_opt "RAYS_SLOW" = Some "1" then
    let text = In_channel.with_open_bin file In_channel.input_all in
    match Result.bind (Rays_editor.Source.read_imports ~file text)
      (fun imports -> Rays_editor.Workspace.load ~imports text) with
    | Error ds when List.exists (fun (d : Diagnostic.t) -> d.code = "E_UNKNOWN_KIND") ds -> ()
    | Error ds -> fail (file ^ ": " ^ show ds)
    | Ok doc ->
    (* a start reads its window from the document it opens: the same answer as the settings
       graph evaluated for the window alone, for every file *)
    (match Editor_document.Contexts.of_workspace ~factories doc with
     | Ok document ->
         incr windows;
         check (Editor_document.Contexts.document_window document = Editor_document.Contexts.window doc)
           (file ^ ": the opened document's window is not the workspace's")
     | Error _ -> ());
    let lowered = lazy (L.of_checked ~factories ~inputs:doc.inputs doc.checked |> Result.get_ok) in
    let scopes = (List.map (fun (g : Flow.Workspace.graph) -> Projection.of_graph catalog doc.checked g.name) doc.checked.graphs
      @ List.map (fun (g : Flow.Workspace.graph) -> Projection.of_graph catalog doc.checked ("def:" ^ g.name)) doc.checked.defs) in
    let fast = ref 0 and patched = ref 0 and refused = ref 0 and slow = ref [] in
    let edits = List.concat_map sites scopes |> List.filter (fun (path, _, _, _, _) -> Doc.imported_file doc path = None) in
    (* each literal costs a full lowering of the file: one per file, the middle one (RAYS_SLOW=1: all) *)
    let edits = if Sys.getenv_opt "RAYS_SLOW" = Some "1" then edits else
      let middle = List.length edits / 2 in List.filteri (fun i _ -> i = middle) edits in
    List.iter (fun (path, key, sub, _, value) ->
      let op = E.Set_arg { node = path; key; sub; value } in
      let where = Printf.sprintf "%s %s" file (String.concat "/" path) in
      let full = full_edit catalog doc op in
      let next, phases = Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () -> Doc.edit catalog doc op) in
      match full, next with
      | Error a, Error b -> check (a.code = b.code) (where ^ ": the literal path refused with another code"); incr refused
      | Ok _, Error d -> fail (where ^ ": the literal path refused an edit the full path accepts: " ^ Diagnostic.to_string d)
      | Error d, Ok _ -> fail (where ^ ": the literal path accepted an edit the full path refuses: " ^ Diagnostic.to_string d)
      | Ok (source, checked), Ok next ->
          check (Doc.to_text next = Doc.to_text { next with source; checked }) (where ^ ": saved bytes differ from the full path");
          if next.literal = None then begin
            (* the full path's cost (reported, not asserted): the edit again, then its lowering, each timed once *)
            let seconds run = (snd (Flow.Phase_timer.sample ~clock:Unix.gettimeofday run)).total in
            let edit = seconds (fun () -> Doc.edit catalog doc op) and lower = seconds (fun () ->
              L.of_checked ~factories ~compiled_ids:(Lazy.force lowered).compiled_ids ~sites:(Lazy.force lowered).sites ~inputs:doc.inputs checked) in
            slow := (edit +. lower) :: !slow
          end else begin
            incr fast;
            List.iter (fun phase -> check (Flow.Phase_timer.calls phases phase = 0)
              (where ^ ": the literal path repeated " ^ phase)) ["print"; "parse"; "check"; "project"];
            match Editor_document.Literal_edit.lower (Lazy.force lowered) (List.hd (Option.get (Doc.literal_changes ~previous:doc next))) with
            | None -> ()
            | Some (Error d) ->
                (* the checker accepts it and lowering refuses it (a cook-level parameter rule): so must the full path *)
                (match L.of_checked ~factories ~compiled_ids:(Lazy.force lowered).compiled_ids ~sites:(Lazy.force lowered).sites ~inputs:doc.inputs checked with
                 | Error e -> check (e.code = d.code) (Printf.sprintf "%s: the lowering patch refused with another code: %s versus %s"
                     where (Diagnostic.to_string d) (Diagnostic.to_string e)); incr refused
                 | Ok _ -> fail (where ^ ": the lowering patch refused an edit the full lowering accepts: " ^ Diagnostic.to_string d))
            | Some (Ok patched_lowering) ->
                incr patched;
                let reference = L.of_checked ~factories ~compiled_ids:patched_lowering.compiled_ids
                    ~sites:patched_lowering.sites ~inputs:doc.inputs checked |> Result.get_ok in
                let shape (plan : Flow.Eval.plan) = Array.map (fun (n : Flow.Eval.node) ->
                  n.id, n.inst, n.site, n.iter, n.kind, n.ty, List.map fst n.args) plan.nodes in
                let same = try Marshal.to_string patched_lowering.plan [Marshal.No_sharing]
                    = Marshal.to_string reference.plan [Marshal.No_sharing]
                  with Invalid_argument _ -> shape patched_lowering.plan = shape reference.plan in
                check same (where ^ ": the patched plan differs from the full lowering")
          end) edits;
    if edits <> [] then begin
      covered := !covered + 1;
      let slow_ms = List.fold_left max 0. !slow *. 1000. in
      Printf.printf "  %-44s literals %3d  literal path %3d  lowering patched %3d  refused %2d  full path %3d (max %.1f ms)\n"
        (String.sub file 3 (String.length file - 3)) (List.length edits) !fast !patched !refused (List.length !slow) slow_ms
    end) files;
  check (!covered >= 3) (Printf.sprintf "only %d checked-in files have a scrubbable literal" !covered);
  Printf.printf "workspace start: the opened document of %d checked-in files says the window their settings graph says\n" !windows;
  print_endline "workspace literal edits: every checked-in .rays saves the full path's bytes with the full path's refusals"

let part_projection_reuse () =
  let text = {|(workspace w
    (graph g :context sop [(count : int 3)] (let* [a (sop/box)] a))
    (graph other :context sop (let* [b (sop/points :points 1)] b)))
    (layout (node ["g" "a"] :at [120 48])
            (node ["g" ":count"] :at [0 0])
            (node ["other" "b"] :at [240 96]))|} in
  let ws = of_text text in
  let renamed, phases = Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () ->
    Doc.edit catalog ws (E.Rename {node = ["other";"b"]; to_ = "moved"}) |> Result.get_ok) in
  check (Flow.Phase_timer.calls phases "project" = 0) "a rename projected every graph to prune layout";
  check (Projection.same_graph ws.checked renamed.checked "g"
    && not (Projection.same_graph ws.checked renamed.checked "other")) "projection invalidation ignored lexical graph content";
  check (Layout.Path_map.find ["g";"a"] renamed.layout.at = (120.,48.)
    && Layout.Path_map.mem ["g";":count"] renamed.layout.at
    && Layout.Path_map.find ["other";"moved"] renamed.layout.at = (240.,96.)
    && not (Layout.Path_map.mem ["other";"b"] renamed.layout.at)) "path pruning lost a surviving key or kept the renamed one";
  let removed, phases = Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () ->
    Doc.edit catalog renamed (E.Remove_graph {name = "other"}) |> Result.get_ok) in
  check (Flow.Phase_timer.calls phases "project" = 0
    && not (Layout.Path_map.mem ["other";"moved"] removed.layout.at)) "removing a graph projected or retained its layout";
  let typed = of_text "(workspace w (defn a :context value [] 1) (graph b :context value (let* [result (a)] 1)))" in
  let changed = of_text "(workspace w (defn a :context value [] [1 2 3]) (graph b :context value (let* [result (a)] 1)))" in
  check (not (Projection.same_graph typed.checked changed.checked "b")) "projection reuse hid a changed dependency type";
  let module E3 = Rays_editor.Editor in
  let e = editor text in
  Fun.protect ~finally:(fun () -> E3.close e) (fun () ->
    let e = E3.update e (frame [] 0) in
    let _, phases = Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () ->
      E3.edit e (E.Rename {node = ["other";"b"]; to_ = "moved"}) |> Result.get_ok
      |> fun e -> E3.update e (frame [] 1)) in
    check (Flow.Phase_timer.calls phases "project" = 0) "editing another graph re-projected the pane");
  print_endline "workspace projection: path pruning, unchanged graph reuse and dependency type invalidation passed"

let part_imports () =
  let library = ";; shared definitions\n(workspace library (defn twice :context value [(x : float)] (* x 2)) (graph shared :context value (twice 3)))\n" in
  let text = "(workspace local (graph result :context value (+ (ref shared) (twice 2))))\n;; library stays separate\n(import \"lib.rays\")\n" in
  let imports = ["lib.rays", library] in
  let doc = Doc.of_text ~imports catalog text |> Result.get_ok in
  check (List.length doc.checked.graphs = 2 &&
    List.length (List.filter (fun (g : Flow.Workspace.graph) -> g.name = "twice") doc.checked.defs) = 1)
    "import children were not checked together";
  check (Doc.import_texts doc = imports) "raw import text was lost (reload digest)";
  check (Doc.imported_file doc ["shared"; "@result"] = Some "lib.rays"
    && Doc.imported_file doc ["def:twice"] = Some "lib.rays") "import provenance was lost";
  let printed = Doc.to_text doc in
  check (has printed "(import \"lib.rays\")" && has printed "library stays separate"
    && not (has printed "(defn twice") && not (has printed "(graph shared")) "save inlined imported definitions";
  check (Doc.to_text (Doc.of_text ~imports catalog printed |> Result.get_ok) = printed) "imports did not round trip";
  let edit = E.Set_arg {node = ["shared"; "@result"]; key = Pos 0; sub = []; value = S.make (Num "4")} in
  check (match Doc.edit catalog doc edit with Error d -> d.code = "E_IMPORTED" && has d.message "lib.rays" | _ -> false)
    "imported expression remained writable";
  check (match Doc.edit catalog doc (E.Remove_graph {name = "shared"}) with Error d -> d.code = "E_IMPORTED" | _ -> false)
    "imported graph remained removable";
  List.iter (fun op -> check (match Doc.edit catalog doc op with
    | Error d -> d.code = "E_IMPORTED" | _ -> false) ("imported gesture remained writable: " ^ E.label op))
    [ E.Rename_graph {name = "shared"; to_ = "renamed"};
      Set_note {node = ["shared"]; text = "changed"};
      Wrap {nodes = [["shared"; "@result"]]; loop = If};
      Add_node {scope = ["shared"]; name = "extra"; expr = S.make (Num "1")};
      Set_input_default {form = "def:twice"; input = "x"; value = S.make (Num "1")};
      Set_graph {name = "shared"; form = S.parse "(graph shared :context value 1)" |> Result.get_ok |> List.hd} ];
  let dependent = Doc.of_text ~imports:["lib.rays", "(graph shared :context value (ref local))"] catalog
    "(workspace w (graph local :context value 1)) (import \"lib.rays\")" |> Result.get_ok in
  check (match Doc.edit catalog dependent (E.Rename_graph {name = "local"; to_ = "moved"}) with
    | Error d -> d.code = "E_IMPORTED" | _ -> false) "local rename rewrote an imported reference";
  let fragment = Doc.of_text catalog "(defn twice :context value [(x : float)] (* x 2))" |> Result.get_ok in
  let fragment = {fragment with layout = {fragment.layout with at = Layout.Path_map.singleton ["def:twice"; "@result"] (20., 40.)}} in
  check (not (has (Doc.to_text fragment) "(workspace") && not (has (Doc.to_text fragment) "(layout"))
    "editing a library fragment appended workspace metadata";
  let error imports text code = check (match Doc.of_text ~imports catalog text with
    | Error ds -> List.exists (fun (d : Diagnostic.t) -> d.code = code) ds | _ -> false) ("missing " ^ code) in
  error [] text "E_IMPORT";
  error ["lib.rays", "(import \"other.rays\")\n" ^ library] text "E_IMPORT";
  error imports "(workspace local (graph shared :context value 1)) (import \"lib.rays\")" "E_NAME";
  error [] "(workspace local (graph g :context value 1)) (import \"/absolute.rays\")" "E_IMPORT";
  let local = Doc.edit catalog doc (E.Set_graph {name = "result"; form = S.parse "(graph result :context value (twice 9))" |> Result.get_ok |> List.hd}) |> Result.get_ok in
  check (Doc.import_texts local = imports && not (has (Doc.to_text local) "(graph shared")) "local edit changed imported source";
  print_endline "workspace imports: splice, provenance, saves, edit refusal and diagnostics passed"

let run () = List.iter (fun f -> f ()) [ part_text; part_edit; part_view; part_editor; part_conditional_history; part_live; part_preset; part_pane_layout; part_contexts; part_editor_contexts; part_literals; part_files; part_projection_reuse; part_imports ]
