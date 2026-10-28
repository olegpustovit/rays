open Rays
open Sop

let get = Result.get_ok
(* pointer positions are the 11-point kit's *)
let source = {|(workspace materials
  (graph blue :context material
    (material/standard :name "blue" :color "#2670f5" :roughness 0.3))
  (graph white :context material
    (material/standard :name "white" :color "#eeeeee" :roughness 0.8))
  (graph geo :context sop
    (let* [base (sop/material (sop/box) :material (ref blue))
           group (sop/group_range base :owner "Primitives" :name "accent"
                   :start 0 :end_ 0)]
      (sop/material group :group "accent" :material (ref white)))))|}

let workspace text = match Rays_editor.Workspace.load text with
  | Ok w -> w | Error ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))

let cook domains node =
  let session = Session.create ~max_entries:32 ~max_payload_bytes:(1 lsl 24) |> get in
  Fun.protect ~finally:(fun () -> Session.close session) (fun () ->
    Session.cook session ~context:(Context.create ~domains () |> get) node
    |> Result.map_error Diagnostic.error_to_string |> get)

let surface (output : Sop.Session.output) =
  let geometry = Sop.Payload.geometry output.payload |> Result.map_error Diagnostic.to_string |> get in
  Rdk_rays.Surface.of_geometry ?instances:output.instances geometry |> get

let surface_scene node surface =
  let explosion = Rdk_rays.Packed_pieces.explosion
      ~operation:(Node.operation node) ~fields:(Node.parameter_fields node) in
  Rdk_rays.Surface.scene3 ?explosion surface

let attr name geometry =
  Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Primitive name geometry
  |> Option.get |> Rdk.Attribute.storage

let signature geometry =
  ["shop_materialpath"; "material_roughness"; "material_color"; "material_emission"]
  |> List.map (fun name -> match attr name geometry with
    | Rdk.Attribute.Text a -> `Text (Array.to_list a)
    | Float a -> `Float (Array.to_list a)
    | Float4 a -> `Tuple (List.init (Rdk.Packed.Float4.length a) (Rdk.Packed.Float4.get a))
    | _ -> failwith "unexpected material attribute")

let face_materials_preserve_explosion () =
  let piece x id = Lisp_sop.node (Printf.sprintf {|(-> (sop/box :normals "Auto" :connectivity "Triangles" :size [1.0 1.0 1.0])
    (sop/transform :mode "Matrix" :m03 %s :m13 0.0 :m23 0.0)
    (sop/set_int :owner "Primitive" :name "piece" :value %d))|} (Lisp_sop.float x) id) in
  let geometry = (Result.get_ok (Sop.Payload.geometry (cook 1 (Lisp_sop.node ~with_:["in2", (piece (-2.) 0); "in3", (piece 2. 1)] {|(sop/merge (sop/ext_in2) (sop/ext_in3))|})).payload)) in
  let group = Rdk.Group.init ~owner:Rdk.Group.Primitive ~name:"one_face"
      (Rdk.Geometry.primitive_count geometry) (( = ) 0) in
  let node = Lisp_sop.node ~with_:["in4", (Lisp_sop.snapshot (Rdk.Geometry.with_group group geometry |> get))] {|(-> (sop/material
       (sop/ext_in4)
       :material "blue"
       :color [0.15 0.43 0.96]
       :roughness 0.3
       :emission [0.0 0.0 0.0])
     (sop/material
       :group "one_face"
       :material "white"
       :color [1.0 1.0 1.0]
       :roughness 0.8
       :emission [0.0 0.0 0.0])
     (sop/exploded_view :amount 1.0))|} in
  let surface = cook 1 node |> surface in
  let drawings = surface_scene node surface |> Scene.D3.Private.drawings in
  assert (List.length drawings = 2);
  (* Centers ±2 move to ±4. Painting just one face must leave every face of
     each cube at exactly the same rigid displacement. *)
  List.iter (fun (d : Scene.D3.Private.drawing) ->
    let v = Mesh.Private.packed_view d.mesh in
    Array.iter (fun index ->
      let x = abs_float v.vertices.x.(index) in
      assert (x >= 3.5 && x <= 4.5);
      assert (abs_float v.vertices.y.(index) <= 0.5);
      assert (abs_float v.vertices.z.(index) <= 0.5)) v.indices) drawings

(* ---- the editor around materials: outline, follow and back, rename, assign, pick ---- *)
module E3 = Rays_editor.Editor
module N = Rays_editor.Private.Navigator
module Edit = Flow_graph.Flow_edit

let fail message = failwith ("test_materials: " ^ message)
let check condition message = if not condition then fail message
let has text sub =
  let n = String.length sub in
  let rec at i = i + n <= String.length text && (String.sub text i n = sub || at (i + 1)) in
  at 0

let follow_source = {|(workspace follow
  (graph cobalt :context material
    (material/standard :name "cobalt blue" :color "#2670f5" :roughness 0.3))
  (graph spare :context material (material/standard :name "spare" :color "#ff0000"))
  (graph shards :context sop
    (let* [m (sop/material (sop/box) :material (ref cobalt))] m))
  (graph scene :context scene
    (let* [obj (scene/geometry (ref shards) :name "shards")] (scene/merge obj))))|}

let frame ?(buttons = []) ?(keys = []) mouse events count : Frame.t = {
  input = { (Input.at_time (float count /. 60.)) with dt = 1. /. 60.;
    frame = count; tick = count; size = 900, 640; pointer = mouse; keys; buttons; events };
  pixel_scale = 1., 1.; fps = 60.; mouse_delta = 0., 0. }

let editor () =
  E3.create ~ui_text_size:11 ~await:true ~workspace:(workspace follow_source)
    ~prepare:(fun _ output -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.Session.payload))
      |> Result.map_error Rdk.Error.to_string)
    ~draw:(fun _ mesh -> Scene.D3.create [ Scene.D3.mesh mesh ]) ()
  |> function Ok e -> e | Error m -> fail m

let dump_line e key =
  let directory = Filename.temp_dir "rays-materials-dump" "" in
  Fun.protect ~finally:(fun () ->
    Array.iter (fun f -> Sys.remove (Filename.concat directory f)) (Sys.readdir directory);
    Sys.rmdir directory) (fun () ->
    E3.crash_dump e directory;
    let text = In_channel.with_open_bin (Filename.concat directory "editor.txt") In_channel.input_all in
    match List.find_opt (fun l -> String.starts_with ~prefix:(key ^ ": ") l) (String.split_on_char '\n' text) with
    | Some l -> String.sub l (String.length key + 2) (String.length l - String.length key - 2)
    | None -> fail ("no " ^ key ^ " in the dump"))

let text_of e = fst (Flow.Lisp.print (E3.workspace e).Editor_document.Workspace_doc.source)

let follow_tests () =
  let e = ref (editor ()) and count = ref 0 in
  let step ?(buttons = []) ?(keys = []) ?(mouse = (450., 300.)) events =
    incr count; e := E3.update !e (frame ~buttons ~keys mouse events !count) in
  let key k = Input.Key_pressed k and ch c = Input.Key_pressed (Input.KeyChar c) in
  let click ?(keys = []) (x, y) =
    step ~keys ~mouse:(x, y) [ Input.Pointer_moved (x, y) ];
    step ~buttons:[ Input.LeftButton ] ~keys ~mouse:(x, y) [ Input.Pointer_pressed (Input.LeftButton, (x, y)) ];
    step ~keys ~mouse:(x, y) [ Input.Pointer_released (Input.LeftButton, (x, y)) ] in
  let pane () = dump_line !e "pane graph" and route () = dump_line !e "route" in
  let jump name =
    step [ key (Input.KeyChar '/'); ch 'j' ]; step [ Input.Text_input name ]; step [ key Input.Enter ]; step [] in
  for _ = 1 to 12 do step [] done;
  check (pane () = "scene" && route () = "scene") ("opens on the scene: " ^ pane ());

  (* the outline: groups, chip, use count, unused *)
  let ws = (E3.workspace !e).Editor_document.Workspace_doc.checked in
  let chips = match Flow.Eval.static ws with
    | Ok ev -> N.chips ev
    | Error d -> fail (Diagnostic.to_string d) in
  let params : N.params = { workspace = ws; active = Some "scene"; scope = None;
    records = None; probes = (fun _ -> 0); selected = []; chips; objects = []; root_detail = ""; layouts = None; notes = [] } in
  let lines = Array.to_list (Array.map N.describe (N.rows N.initial params)) in
  let index text = match List.find_index (fun l -> has l text) lines with
    | Some i -> i | None -> fail (text ^ " is not in the outline: " ^ String.concat " | " lines) in
  check (index "SCENE" < index "GEOMETRY" && index "GEOMETRY" < index "MATERIALS")
    "the groups are in dependency order";
  check (index "MATERIALS" < index "cobalt" && index "shards" < index "MATERIALS") "a graph sits in its group";
  ignore (index "cobalt · ×1 · #2670f5");
  ignore (index "spare · unused · #ff0000");
  check (List.length (List.filter (fun l -> has l "unused") lines) = 1) "only the unread material is unused";
  check (Array.length (N.rows (N.with_query "cob" N.initial) params) = 2) "typing filters across the groups";

  (* / j, then follow with i and back with u *)
  jump "shards";
  check (pane () = "shards" && route () = "scene > shards") ("/ j jumps: " ^ route ());
  let bx, by, bw, bh = Option.get (E3.node_box !e [ "shards"; "m" ]) in
  click (float bx +. float bw /. 2., float by +. float bh -. 6.);
  step [ ch 'i' ]; step [];
  check (pane () = "cobalt" && route () = "scene > shards > cobalt")
    ("i follows the :material of the selected node: " ^ route ());
  step [ ch 'u' ]; step [];
  check (pane () = "shards" && route () = "scene > shards") ("u goes back one: " ^ route ());
  step [ ch 'u' ]; step [];
  check (pane () = "scene" && route () = "scene") ("and back to the scene: " ^ route ());
  step [ ch 'u' ]; step [];
  check (pane () = "scene") "with nothing to go back to u stays at the scene";

  (* a double-click on the node follows too *)
  jump "shards";
  let bx, by, bw, bh = Option.get (E3.node_box !e [ "shards"; "m" ]) in
  let body = float bx +. float bw /. 2., float by +. float bh -. 6. in
  click body; click body; step [];
  check (pane () = "cobalt") ("a double-click follows: " ^ pane () ^ " / " ^ route ());
  jump "scene";

  (* a pick in the viewport lands in the right graph: the surface says "cobalt blue", the graph is cobalt *)
  let vx, vy, vw, vh = (E3.panes !e (frame (0., 0.) [] !count)).Pxui_shell.Layout.view in
  click ~keys:[ Input.Alt ] (float vx +. float vw /. 2., float vy +. float vh /. 2.);
  step []; step [];
  check (pane () = "cobalt") ("Alt-click opens the material of the surface: " ^ pane () ^ " / " ^ route ());
  jump "scene";

  (* i in a viewport with nothing selected follows to the scene it shows *)
  jump "shards";
  click (float vx +. 4., float vy +. 4.);
  step [ ch 'i' ]; step [];
  check (pane () = "scene") ("i in a viewport follows to its scene: " ^ pane () ^ " / " ^ route ());

  (* rename: the graph and every (ref) to it, one undo entry *)
  let renamed = match E3.edit !e (Edit.Rename_graph { name = "cobalt"; to_ = "navy" }) with
    | Ok e -> e | Error m -> fail m in
  let text = text_of renamed in
  check (has text "(graph navy" && has text "(ref navy)" && not (has text "(ref cobalt)"))
    ("rename rewrote the graph and its reader: " ^ text);
  check (E3.undo_label renamed = Some "Rename graph") "one history entry";
  check (Result.is_error (E3.edit !e (Edit.Rename_graph { name = "cobalt"; to_ = "spare" }))) "a taken name is refused";
  e := renamed;
  step ~keys:[ Input.Meta ] [ ch 'z' ];
  check (has (text_of !e) "(graph cobalt" && has (text_of !e) "(ref cobalt)") "one undo restores both";

  (* a read material is not deleted, and the refusal names who reads it; an unread one goes *)
  (match E3.edit !e (Edit.Remove_graph { name = "cobalt" }) with
   | Ok _ -> fail "a read material was removed"
   | Error m -> check (has m "shards") ("the refusal names its reader: " ^ m));
  check (Result.is_ok (E3.edit !e (Edit.Remove_graph { name = "spare" }))) "an unread material can go";

  (* I peeks in a floating window *)
  jump "shards";
  let bx, by, bw, bh = Option.get (E3.node_box !e [ "shards"; "m" ]) in
  click (float bx +. float bw /. 2., float by +. float bh -. 6.);
  step ~keys:[ Input.Shift ] [ ch 'i' ]; step [];
  check (has (text_of !e) "(ui/floating (ui/graph \"cobalt\"))" || has (text_of !e) "ui/graph \"cobalt\"")
    ("I opens the material in a floating graph: " ^ text_of !e);
  (* / a: Material makes a graph and opens it; Material of... fills the reference *)
  jump "scene";
  step [ key (Input.KeyChar '/'); ch 'a' ]; step [ Input.Text_input "material" ]; step [ key Input.Enter ]; step [];
  check (has (text_of !e) "(graph material :context material") ("/ a made a material graph: " ^ text_of !e);
  check (pane () = "material" && E3.undo_label !e = Some "New material") "and opened it";
  step [ ch 'u' ]; step [];
  check (pane () = "scene") "u returns from it";
  jump "shards";
  let bx, by, bw, bh = Option.get (E3.node_box !e [ "shards"; "m" ]) in
  click (float bx +. float bw /. 2., float by +. float bh -. 6.);
  step [ key (Input.KeyChar '/'); ch 'a' ]; step [ Input.Text_input "spare" ]; step [ key Input.Enter ]; step [];
  check (has (text_of !e) ":material (ref spare)" && has (text_of !e) "(sop/material m")
    ("Material of... adds a sop/material after the selection: " ^ text_of !e);

  E3.close !e

(* ---- carry: a material or a SOP graph picked up by pointer or by keys, put as one entry ---- *)
let carry_source = {|(workspace carrying
  (graph cobalt :context material
    (material/standard :name "cobalt blue" :color "#2670f5" :roughness 0.3))
  (graph spare :context material (material/standard :name "spare" :color "#ff0000"))
  (graph shards :context sop
    (let* [m (sop/material (sop/box) :material (ref spare))] m))
  (graph plinth :context sop (sop/box))
  (graph scene :context scene
    (let* [obj (scene/geometry (ref shards) :name "shards")] (scene/merge obj))))|}

let carry_tests () =
  let module L = Pxui_shell.Layout in
  (* the panels [editor] below declares, for the outline rows' places *)
  let layout = L.Split { axis = `H; size = `Ratio 0.22; a = L.Leaf L.Outline;
    b = L.Split { axis = `H; size = `Ratio 0.45; a = L.Leaf (L.View "main");
      b = L.Split { axis = `H; size = `Ratio 0.62; a = L.Leaf L.Graph; b = L.Leaf L.Inspector } } } in
  let editor = {|
  (graph editor :context editor
    (ui/workspace (ui/split-at "horizontal" 0.22 (ui/outline)
      (ui/split-at "horizontal" 0.45 (ui/viewport (ref scene))
        (ui/split-at "horizontal" 0.62 (ui/graph) (ui/inspector))))))|} in
  let with_editor source = String.sub source 0 (String.length source - 1) ^ editor ^ ")" in
  (* an unbounded budget unless a check is about the budget: a shown preview never depends on how
     loaded the machine is (it was once held back under a full parallel suite) *)
  let make ?(source = carry_source) ?(carry_budget = Float.infinity) () =
    E3.create ~ui_text_size:11 ~await:true ~carry_budget ~workspace:(workspace (with_editor source))
      ~prepare:(fun _ _ -> Ok (Scene.D3.create []))
      ~draw:(fun _ scene -> scene) ()
    |> function Ok e -> e | Error m -> fail m in
  let e = ref (make ()) and count = ref 0 in
  let step ?(advance = 1) ?(buttons = []) ?(keys = []) ?(mouse = (450., 300.)) events =
    count := !count + advance; e := E3.update !e (frame ~buttons ~keys mouse events !count) in
  let timed ?buttons ?keys ?mouse events =
    let start = Unix.gettimeofday () in
    step ?buttons ?keys ?mouse events; (Unix.gettimeofday () -. start) *. 1000. in
  let key k = Input.Key_pressed k and ch c = Input.Key_pressed (Input.KeyChar c) in
  let jump name =
    step [ key (Input.KeyChar '/'); ch 'j' ];
    step [ Input.Text_input name; key Input.Enter ]; step [] in
  let pane () = dump_line !e "pane graph" in
  let line () = Option.value ~default:"-" (E3.carry_line !e) in
  let ws = ref (E3.workspace !e) in
  step [];
  (* y is a plain key nothing else uses *)
  check (List.length (List.filter (fun (c : Rays_editor.Private.Editor_commands.command) -> match c.trigger with
    | Some (Editor_core.Keymap.Chord (Input.KeyChar 'y', [])) -> true | _ -> false)
    Rays_editor.Private.Editor_commands.keymap) = 1) "y picks up and no other command takes it";
  (* the outline row of a graph, in screen points, as the Navigator draws it now *)
  let catalog = get (Rays_editor.workspace_catalog ()) in
  let row_of graph =
    let doc = (E3.workspace !e).Editor_document.Workspace_doc.checked in
    let chips = match Flow.Eval.static doc with Ok ev -> N.chips ev | Error d -> fail (Diagnostic.to_string d) in
    let active = pane () in
    let scope = try Some (Flow_graph.Projection.of_graph catalog doc active) with Invalid_argument _ -> None in
    let params : N.params = { workspace = doc; active = Some active; scope;
      records = None; probes = (fun _ -> 0); selected = []; chips;
      (* as many object rows as the panel draws: a row's place depends on them *)
      objects = (List.map (fun _ -> { N.depth = 0; letter = ""; name = ""; detail = ""; visible = None; render = None;
        lead = false; inert = false; chosen = false; home = None }) (E3.scene_document !e));
      root_detail = ""; layouts = None; notes = [] } in
    let geometry = L.geometry ~hidden:[ L.Timeline ] layout (frame (0., 0.) [] 0) in
    let bounds = (Option.get (L.find geometry L.Outline)).body in
    match List.find_map (fun (row, (x, y, w, h)) ->
      let text = N.describe row in
      let text = if String.starts_with ~prefix:"> " text then String.sub text 2 (String.length text - 2) else text in
      if String.starts_with ~prefix:(graph ^ " · ") text then Some (x +. w /. 2., y +. h /. 2.) else None)
      (Array.to_list (N.row_rects N.initial params ~bounds)) with
    | Some at -> at
    | None -> fail ("the outline has no row " ^ graph) in
  let tile path =
    let x, y, w, h = Option.get (E3.node_box !e path) in
    float x +. float w /. 2., float y +. float h -. 6. in
  (* the pointer carries cobalt from its outline row onto the node of shards that reads spare *)
  jump "shards";
  check (pane () = "shards") "the pane shows shards";
  ws := E3.workspace !e;
  let history = E3.undo_label !e in
  let drag_to target =
    let from = row_of "cobalt" in
    step ~mouse:from [ Input.Pointer_moved from ];
    step ~buttons:[ Input.LeftButton ] ~mouse:from [ Input.Pointer_pressed (Input.LeftButton, from) ];
    let fx, fy = from in
    step ~buttons:[ Input.LeftButton ] ~mouse:(fx +. 2., fy +. 1.) [ Input.Pointer_moved (fx +. 2., fy +. 1.) ];
    check (E3.carrying !e = None) "inside the dead zone nothing is carried";
    step ~buttons:[ Input.LeftButton ] ~mouse:(fx +. 12., fy +. 6.) [ Input.Pointer_moved (fx +. 12., fy +. 6.) ];
    step ~buttons:[ Input.LeftButton ] ~mouse:(fx +. 12., fy +. 6.) [];
    check (E3.carrying !e = Some ("material", "(ref cobalt)")) "past the dead zone the row is carried";
    let tx, ty = target in
    step ~buttons:[ Input.LeftButton ] ~mouse:target [ Input.Pointer_moved (tx, ty) ];
    step ~buttons:[ Input.LeftButton ] ~mouse:target [] in
  drag_to (tile [ "shards"; "m" ]);
  check (has (text_of !e) "(ref cobalt)" && not (has (text_of !e) "(ref spare)"))
    ("hovering a target shows the edit everywhere: " ^ text_of !e);
  check (E3.undo_label !e = history) "a preview writes nothing to the history";
  check (has (line ()) "Preview" && has (line ()) ":material (ref cobalt) on shards/m")
    ("the strip says what a release writes: " ^ line ());
  (* Escape restores the document that was there, physically *)
  step ~buttons:[ Input.LeftButton ] ~mouse:(tile [ "shards"; "m" ]) [ key Input.Escape ];
  step ~mouse:(tile [ "shards"; "m" ]) [ Input.Pointer_released (Input.LeftButton, tile [ "shards"; "m" ]) ];
  step [];
  check (E3.carrying !e = None && E3.workspace !e == !ws) "Escape leaves the document physically equal";
  check (E3.undo_label !e = history) "and there is nothing to undo";
  (* a release over nothing drops it too; a lost window focus as well *)
  drag_to (tile [ "shards"; "m" ]);
  step ~buttons:[ Input.LeftButton ] ~mouse:(2000., 2000.) [ Input.Pointer_moved (2000., 2000.) ];
  step ~mouse:(2000., 2000.) [ Input.Pointer_released (Input.LeftButton, (2000., 2000.)) ]; step [];
  check (E3.carrying !e = None && E3.workspace !e == !ws) "a release over nothing restores the document";
  drag_to (tile [ "shards"; "m" ]);
  step [ Input.Focus_lost ]; step []; step [];
  check (E3.carrying !e = None && E3.workspace !e == !ws) "a lost window focus restores the document";
  (* a release over the target is the put: one entry, labelled Put *)
  drag_to (tile [ "shards"; "m" ]);
  let at = tile [ "shards"; "m" ] in
  step ~mouse:at [ Input.Pointer_released (Input.LeftButton, at) ]; step []; step [];
  check (E3.carrying !e = None && E3.undo_label !e = Some "Put") ("a release puts it, as one entry: "
    ^ Option.value ~default:"-" (E3.undo_label !e));
  check (has (text_of !e) ":material (ref cobalt)" && not (has (text_of !e) "(ref spare)")) "it wrote :material (ref cobalt)";
  let by_pointer = text_of !e in
  step ~keys:[ Input.Meta ] [ ch 'z' ];
  check (E3.undo_label !e = history && E3.workspace !e == !ws) "one undo gives the document back";

  (* by keys: the same put, the same entry.  Open the material, y, a letter, Enter *)
  jump "cobalt";
  step [ ch 'y' ]; step [];
  check (E3.carrying !e = Some ("material", "(ref cobalt)")) "y picks up the open material";
  check (has (line ()) "a shards") ("every place that takes it has a letter: " ^ line ());
  step [ ch 'a' ]; step [];
  check (has (text_of !e) "(ref cobalt)" && E3.undo_label !e = history) "a target letter previews, writing nothing";
  step [ key Input.Enter ]; step [];
  check (E3.carrying !e = None && E3.undo_label !e = Some "Put") "Enter writes one entry named Put";
  check (text_of !e = by_pointer) "the keys wrote the same text as the pointer";
  step ~keys:[ Input.Meta ] [ ch 'z' ];
  check (E3.workspace !e == !ws) "and one undo gives it back";
  (* Escape drops a key carry; Enter before a letter says so *)
  step [ ch 'y' ]; step [];
  step [ key Input.Enter ]; step [];
  check (E3.carrying !e <> None && has (line ()) "Carrying") "Enter with no target chosen keeps the carry";
  step [ key Input.Escape ]; step [];
  check (E3.carrying !e = None && E3.workspace !e == !ws) "Escape drops a key carry";

  (* a SOP graph goes to the scene: one more object over the existing graph *)
  jump "shards";
  step [ ch 'y' ]; step [];
  check (E3.carrying !e = Some ("sop", "(ref shards)")) "y picks up the open SOP graph";
  jump "scene";
  check (pane () = "scene" && E3.carrying !e <> None) "the carry survives navigation";
  step [ ch 'a' ]; step [];
  check (has (line ()) "scene/geometry (ref shards)") ("the scene takes it: " ^ line ());
  step [ key Input.Enter ]; step [];
  check (E3.undo_label !e = Some "Put" && E3.carrying !e = None) "the put is one entry";
  let text = text_of !e in
  let count_of sub = let n = String.length sub in
    let rec go i acc = if i + n > String.length text then acc
      else go (i + 1) (if String.sub text i n = sub then acc + 1 else acc) in go 0 0 in
  check (count_of "(scene/geometry (ref shards)" = 2) ("two objects now show shards: " ^ text);
  step ~keys:[ Input.Meta ] [ ch 'z' ];
  check (E3.workspace !e == !ws) "undone";

  (* a place that does not take the payload refuses, with the checker's words: a merge has no
     material; releasing there writes nothing *)
  jump "scene";
  let hover_scene_node name =
    drag_to (tile [ "scene"; name ]) in
  hover_scene_node "@result";
  check (has (line ()) "Refused") ("a merge takes no material, and the strip says why: " ^ line ());
  check (E3.workspace !e == !ws) "a refused target shows nothing";
  let at = tile [ "scene"; "@result" ] in
  step ~mouse:at [ Input.Pointer_released (Input.LeftButton, at) ]; step []; step [];
  check (E3.carrying !e = None && E3.undo_label !e = history) "a release on a refusal writes nothing";
  (* a SOP graph put on an object re-points its (ref ...) *)
  let drag_graph graph target =
    let from = row_of graph in
    let fx, fy = from in
    step ~mouse:from [ Input.Pointer_moved from ];
    step ~buttons:[ Input.LeftButton ] ~mouse:from [ Input.Pointer_pressed (Input.LeftButton, from) ];
    step ~buttons:[ Input.LeftButton ] ~mouse:(fx +. 12., fy +. 6.) [ Input.Pointer_moved (fx +. 12., fy +. 6.) ];
    step ~buttons:[ Input.LeftButton ] ~mouse:(fx +. 12., fy +. 6.) [];
    step ~buttons:[ Input.LeftButton ] ~mouse:target [ Input.Pointer_moved target ];
    step ~buttons:[ Input.LeftButton ] ~mouse:target [];
    step ~mouse:target [ Input.Pointer_released (Input.LeftButton, target) ]; step []; step [] in
  drag_graph "plinth" (tile [ "scene"; "obj" ]);
  check (E3.undo_label !e = Some "Put" && has (text_of !e) "(scene/geometry (ref plinth)")
    ("a SOP graph on an object re-points it: " ^ text_of !e);
  step ~keys:[ Input.Meta ] [ ch 'z' ];
  check (E3.workspace !e == !ws) "and one undo gives it back";

  (* timings of a preview: apply on the first hot frame, restore on the cancel *)
  jump "shards";
  let apply = ref 0. and restore = ref 0. in
  let from = row_of "cobalt" in
  step ~mouse:from [ Input.Pointer_moved from ];
  step ~buttons:[ Input.LeftButton ] ~mouse:from [ Input.Pointer_pressed (Input.LeftButton, from) ];
  let fx, fy = from in
  step ~buttons:[ Input.LeftButton ] ~mouse:(fx +. 12., fy +. 6.) [ Input.Pointer_moved (fx +. 12., fy +. 6.) ];
  step ~buttons:[ Input.LeftButton ] ~mouse:(fx +. 12., fy +. 6.) [];
  let target = tile [ "shards"; "m" ] in
  step ~buttons:[ Input.LeftButton ] ~mouse:target [ Input.Pointer_moved target ];
  apply := timed ~buttons:[ Input.LeftButton ] ~mouse:target [];
  check (has (text_of !e) "(ref cobalt)") "the preview is on";
  restore := timed ~buttons:[ Input.LeftButton ] ~mouse:target [ key Input.Escape ];
  step ~mouse:target [ Input.Pointer_released (Input.LeftButton, target) ]; step [];
  check (E3.workspace !e == !ws) "restored";
  (* resting on a node enters what it references after 0.6 s: the carry survives, three graphs away *)
  jump "shards";
  let from = row_of "cobalt" in
  let fx, fy = from in
  step ~mouse:from [ Input.Pointer_moved from ];
  step ~buttons:[ Input.LeftButton ] ~mouse:from [ Input.Pointer_pressed (Input.LeftButton, from) ];
  step ~buttons:[ Input.LeftButton ] ~mouse:(fx +. 12., fy +. 6.) [ Input.Pointer_moved (fx +. 12., fy +. 6.) ];
  let target = tile [ "shards"; "m" ] in
  step ~buttons:[ Input.LeftButton ] ~mouse:target [ Input.Pointer_moved target ];
  step ~advance:35 ~buttons:[ Input.LeftButton ] ~mouse:target [];
  check (pane () = "shards") "the pointer has not rested long enough to enter";
  step ~advance:40 ~buttons:[ Input.LeftButton ] ~mouse:target [];
  check (pane () = "spare" && E3.carrying !e <> None) ("resting on a node entered the graph it reads, still carrying: " ^ pane ());
  step ~buttons:[ Input.LeftButton ] ~mouse:target [ key Input.Escape ];
  step ~mouse:target [ Input.Pointer_released (Input.LeftButton, target) ]; step [];
  check (E3.workspace !e == !ws && E3.carrying !e = None) "and Escape still gives the document back";
  step [ ch 'u' ]; step [];
  (* a viewport is a place: the surface under the pointer says which graph, and the material
     graph its primitive reads says which node *)
  let view_center () =
    let vx, vy, vw, vh = (E3.panes !e (frame (0., 0.) [] !count)).Pxui_shell.Layout.view in
    float vx +. float vw /. 2., float vy +. float vh /. 2. in
  let carry_from_row graph =
    let from = row_of graph in
    let fx, fy = from in
    step ~mouse:from [ Input.Pointer_moved from ];
    step ~buttons:[ Input.LeftButton ] ~mouse:from [ Input.Pointer_pressed (Input.LeftButton, from) ];
    step ~buttons:[ Input.LeftButton ] ~mouse:(fx +. 12., fy +. 6.) [ Input.Pointer_moved (fx +. 12., fy +. 6.) ] in
  jump "scene";
  carry_from_row "cobalt";
  let over = view_center () in
  step ~buttons:[ Input.LeftButton ] ~mouse:over [ Input.Pointer_moved over ];
  step ~buttons:[ Input.LeftButton ] ~mouse:over [];
  step ~buttons:[ Input.LeftButton ] ~mouse:over [];
  check (has (line ()) ":material (ref cobalt) on m in graph shards") ("a surface names its node: " ^ line ());
  step ~mouse:over [ Input.Pointer_released (Input.LeftButton, over) ]; step []; step [];
  check (E3.undo_label !e = Some "Put" && text_of !e = by_pointer) "a put on a surface is the same entry as on the node";
  step ~keys:[ Input.Meta ] [ ch 'z' ];
  check (E3.workspace !e == !ws) "undone";
  (* with no material node in the graph, the put adds one after the result, in the same entry *)
  E3.close !e;
  e := make ~source:{|(workspace bare
  (graph cobalt :context material (material/standard :name "cobalt blue" :color "#2670f5"))
  (graph plinth :context sop (let* [b (sop/box)] b))
  (graph scene :context scene
    (let* [obj (scene/geometry (ref plinth) :name "plinth")] (scene/merge obj))))|} ();
  step [];
  ws := E3.workspace !e;
  jump "scene";
  carry_from_row "cobalt";
  let over = view_center () in
  step ~buttons:[ Input.LeftButton ] ~mouse:over [ Input.Pointer_moved over ];
  step ~buttons:[ Input.LeftButton ] ~mouse:over [];
  step ~buttons:[ Input.LeftButton ] ~mouse:over [];
  check (has (line ()) "a sop/material node and :material (ref cobalt) in graph plinth") ("no material node yet: " ^ line ());
  step ~mouse:over [ Input.Pointer_released (Input.LeftButton, over) ]; step []; step [];
  check (E3.undo_label !e = Some "Put" && has (text_of !e) "(sop/material b :material (ref cobalt))")
    ("one entry added the node and its material: " ^ text_of !e);
  step ~keys:[ Input.Meta ] [ ch 'z' ];
  check (E3.workspace !e == !ws) "undone as one";
  (* over the budget: the target is lit, the strip says what a release writes and why there is no
     picture, and the document shown stays as it was; the release still writes it *)
  E3.close !e;
  e := make ~carry_budget:0. ();
  step [];
  ws := E3.workspace !e;
  jump "shards";
  drag_to (tile [ "shards"; "m" ]);
  check (has (line ()) "Would write :material (ref cobalt) on shards/m" && has (line ()) "no preview"
         && has (line ()) "applying takes")
    ("a slow put says what it would write and why it is not shown: " ^ line ());
  check (E3.workspace !e == !ws && has (text_of !e) "(ref spare)" && not (has (text_of !e) "(ref cobalt)"))
    "a held-back put shows no picture: the document shown is the original";
  check (E3.carrying !e <> None && E3.undo_label !e = history) "the carry goes on, nothing is written";
  let at = tile [ "shards"; "m" ] in
  step ~mouse:at [ Input.Pointer_released (Input.LeftButton, at) ]; step []; step [];
  check (E3.carrying !e = None && E3.undo_label !e = Some "Put" && has (text_of !e) ":material (ref cobalt)")
    "the release writes it, as one Put entry";
  step ~keys:[ Input.Meta ] [ ch 'z' ];
  check (E3.workspace !e == !ws) "and one undo gives it back";
  (* Enter before a letter keeps the carry and says so, ahead of the prompt *)
  jump "cobalt";
  step [ ch 'y' ]; step [];
  step [ key Input.Enter ]; step [];
  check (has (line ()) "Pick a target letter first" && has (line ()) "Carrying")
    ("the reminder is on the strip with the prompt: " ^ line ());
  step [ ch 'a' ]; step [];
  check (not (has (line ()) "Pick a target letter first")) "choosing a letter clears the reminder";
  step [ key Input.Escape ]; step [];
  check (!apply < 500. && !restore < 500.) "a preview applies and restores inside the 500 ms budget";
  E3.close !e

(* ---- carry onto a viewport panel: a scene graph re-points it, a camera is its root's :camera ---- *)
let viewport_source = {|(workspace views
  (graph shards :context sop (sop/box))
  (graph day :context scene
    (let* [obj (scene/geometry (ref shards) :name "shards")
           cam (scene/camera :name "cam")
           side (scene/camera :name "side" :eye [6 2 0])
           all (scene/merge obj cam side)]
      (scene/root all :camera cam)))
  (graph night :context scene
    (let* [obj (scene/geometry (ref shards) :name "shards")
           side (scene/camera :name "side" :eye [-6 2 0])
           all (scene/merge obj side)]
      all))
  (graph editor :context editor
    (let* [left (ui/viewport (ref day))
           right (ui/viewport (ref night))]
      (ui/workspace (ui/split-at "horizontal" 0.6 (ui/split-at "horizontal" 0.5 left right) (ui/graph))))))|}

let viewport_carry_tests () =
  let e = ref (E3.create ~ui_text_size:11 ~await:true ~workspace:(workspace viewport_source)
    ~prepare:(fun _ output -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.Session.payload))
      |> Result.map_error Rdk.Error.to_string)
    ~draw:(fun _ mesh -> Scene.D3.create [ Scene.D3.mesh mesh ]) ()
    |> function Ok e -> e | Error m -> fail m) and count = ref 0 in
  let step ?(keys = []) ?(mouse = (450., 300.)) events =
    incr count; e := E3.update !e (frame ~keys mouse events !count) in
  let key k = Input.Key_pressed k and ch c = Input.Key_pressed (Input.KeyChar c) in
  let jump name =
    step [ key (Input.KeyChar '/'); ch 'j' ]; step [ Input.Text_input name ]; step [ key Input.Enter ]; step [] in
  let line () = Option.value ~default:"-" (E3.carry_line !e) in
  for _ = 1 to 12 do step [] done;
  let ws = E3.workspace !e in
  let history = E3.undo_label !e in
  let undo () = step ~keys:[ Input.Meta ] [ ch 'z' ]; step [] in
  (* a scene graph open: y holds it, and the viewports are its places *)
  jump "night";
  step [ ch 'y' ]; step [];
  check (E3.carrying !e = Some ("scene", "(ref night)")) "y picks up the open scene graph";
  check (has (line ()) "a viewport 1" && not (has (line ()) "viewport 2"))
    ("the viewport that does not show it has a letter, the one that does has none: " ^ line ());
  step [ ch 'a' ]; step [];
  check (has (line ()) "(ui/viewport (ref night))" && has (text_of !e) "(ui/viewport (ref night))"
         && not (has (text_of !e) "(ui/viewport (ref day))"))
    ("a letter previews the panel re-pointed: " ^ line ());
  step [ key Input.Enter ]; step [];
  check (E3.carrying !e = None && E3.undo_label !e = Some "Put" && has (text_of !e) "left (ui/viewport (ref night))"
         && has (text_of !e) "right (ui/viewport (ref night))")
    ("Enter writes one entry: " ^ text_of !e);
  undo ();
  check (E3.workspace !e == ws && E3.undo_label !e = history) "one undo gives it back";
  (* a camera object selected: y holds its name; the root of the scene the panel shows takes it *)
  jump "day";
  let bx, by, bw, bh = Option.get (E3.node_box !e [ "day"; "side" ]) in
  step ~mouse:(float bx +. float bw /. 2., float by +. float bh -. 6.)
    [ Input.Pointer_moved (float bx +. float bw /. 2., float by +. float bh -. 6.) ];
  let at = float bx +. float bw /. 2., float by +. float bh -. 6. in
  step ~mouse:at [ Input.Pointer_pressed (Input.LeftButton, at); Input.Pointer_released (Input.LeftButton, at) ]; step [];
  step [ ch 'y' ]; step [];
  check (E3.carrying !e = Some ("camera", "side")) ("y picks up the selected camera: "
    ^ Option.fold ~none:"-" ~some:(fun (k, v) -> k ^ " " ^ v) (E3.carrying !e));
  step [ ch 'a' ]; step [];
  check (has (line ()) ":camera side on all in graph day" || has (line ()) ":camera side on")
    ("the root of day takes it: " ^ line ());
  step [ key Input.Enter ]; step [];
  check (E3.undo_label !e = Some "Put" && has (text_of !e) ":camera side") ("the root's :camera is written: " ^ text_of !e);
  undo ();
  check (E3.workspace !e == ws) "one undo gives it back";
  (* the second viewport shows a part, which has no root: one is written over its result *)
  step [ ch 'y' ]; step [];
  step [ ch 's' ]; step [];
  check (has (line ()) "a scene/root with :camera side in graph night") ("a part gets a root: " ^ line ());
  step [ key Input.Enter ]; step [];
  check (E3.undo_label !e = Some "Put" && has (text_of !e) "(scene/root" && has (text_of !e) ":camera side")
    ("the root is added in the same entry: " ^ text_of !e);
  undo ();
  check (E3.workspace !e == ws) "undone as one";
  (* a left press on the panel while a scene is held is the put (the pointer route to a viewport) *)
  jump "night";
  step [ ch 'y' ]; step [];
  let vx, vy, vw, vh = (E3.panes !e (frame (0., 0.) [] !count)).Pxui_shell.Layout.view in
  let over = float vx +. float vw /. 2., float vy +. float vh /. 2. in
  step ~mouse:over [ Input.Pointer_moved over ]; step ~mouse:over [];
  check (has (line ()) "(ui/viewport (ref night))") ("the pointer over the panel previews it: " ^ line ());
  step ~mouse:over [ Input.Pointer_pressed (Input.LeftButton, over); Input.Pointer_released (Input.LeftButton, over) ]; step [];
  check (E3.carrying !e = None && E3.undo_label !e = Some "Put" && has (text_of !e) "left (ui/viewport (ref night))")
    ("a press on the panel is the put: " ^ text_of !e);
  undo ();
  check (E3.workspace !e == ws) "undone";
  E3.close !e

let () =
  face_materials_preserve_explosion ();
  let graph = Rays_editor.Workspace.sop_graphs (workspace source) |> get
      |> List.assoc "geo" in
  let node = graph in
  let one = cook 1 node and four = cook 4 node in
  assert (signature (Result.get_ok (Sop.Payload.geometry one.payload)) = signature (Result.get_ok (Sop.Payload.geometry four.payload)));
  assert (Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry one.payload)) = 6);
  (match attr "shop_materialpath" (Result.get_ok (Sop.Payload.geometry one.payload)), attr "material_roughness" (Result.get_ok (Sop.Payload.geometry one.payload)) with
   | Rdk.Attribute.Text names, Float rough ->
       assert (names.(0) = "white" && rough.(0) = 0.8);
       assert (Array.sub names 1 5 = Array.make 5 "blue");
       assert (Array.sub rough 1 5 = Array.make 5 0.3)
   | _ -> assert false);
  let surface = surface one in
  let drawings = surface_scene node surface |> Scene.D3.Private.drawings in
  assert (List.length drawings = 2);
  assert (List.fold_left (fun n (d : Scene.D3.Private.drawing) -> n + Mesh.index_count d.mesh) 0 drawings = 36);
  assert (List.exists (fun (d : Scene.D3.Private.drawing) -> d.material.diffuse = Rays_math.Color.hex_exn "#2670f5") drawings);
  assert (signature (Result.get_ok (Sop.Payload.geometry one.payload)) = signature (Result.get_ok (Sop.Payload.geometry (cook 1 node).payload)));
  List.iter (fun text -> assert (Result.is_error (Rays_editor.Workspace.load text)))
    ["(workspace x (graph m :context material (material/standard :roughness 2)))";
     "(workspace x (graph m :context material (material/standard :color \"invalid\")))";
     "(workspace x (graph m :context sop (material/standard)))"];
  follow_tests ();
  carry_tests ();
  viewport_carry_tests ();
  print_endline "material refs, group preservation, render batches, cancellation and domain exactness passed"
