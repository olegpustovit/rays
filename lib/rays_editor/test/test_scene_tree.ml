(* The scene tree, window-free: the scene opens as a list; arrows move the
   focus, [i] and a double-click enter a geometry object or the World, [u]
   goes up; Tab reparents keeping its world position, [h] hides; scene edits
   never re-cook SOPs; [/ e] opens the World, which then bakes; [/ l] then
   [l], [t] or [g] shows the graph panel's list, text or graph (the workspace's scene graph). *)
open Rays
open Sop
open Test_support

let frame ?(mouse = (450, 320)) ?(events = []) ?(keys = []) ?(buttons = []) count : Frame.t =
  {
    input =
      {
        (Input.at_time (float_of_int count /. 60.)) with
        dt = 1. /. 60.;
        frame = count;
        tick = count;
        size = (900, 640);
        pointer =
          (let x, y = mouse in
           (float x, float y));
        keys;
        buttons;
        events;
      };
    pixel_scale = (1., 1.);
    fps = 60.;
    mouse_delta = (0., 0.);
  }

module E = Rays_editor.Editor
module KG = Editor_document.Kind_graph

(* geo1 (one box), two lights, a camera, a World of sky, softbox and constellation *)
let text =
  {|(workspace scene_tree
  (graph geo :context sop (sop/box))
  (graph scene :context scene
    (let* [body (scene/geometry (ref geo) :name "geo1")
           key (scene/light :name "key")
           fill (scene/light :name "fill" :translate [-3 4 2])
           cam (scene/camera :name "camera1" :aperture 0.3)
           world (scene/world (ref sky) :name "world")]
      (scene/merge body key fill cam world)))
  (graph sky :context world
    (world/scatter (world/shape (world/sky :name "sky") :name "softbox")
                   :name "constellation")))|}

let contains text piece =
  let n = String.length piece in
  let rec at i = i + n <= String.length text && (String.sub text i n = piece || at (i + 1)) in
  at 0

(* the workspace text is the truth of every scene edit: what the list, the inspector, the
   handles and the World keys did is in it, and it opens as the same scene *)
let source env = Rays_editor.Workspace_doc.to_text (E.workspace env)

let labelled env =
  let graph = E.scene_document env in
  List.sort compare
    (List.map
       (fun (info : KG.node) ->
         ( info.label,
           info.kind,
           Option.bind info.input (fun id ->
               Option.map (fun (n : KG.node) -> n.label) (KG.find graph id)),
           List.map (fun (f : Param.field_view) -> (f.name, f.current)) (KG.fields info) ))
       graph)

let preview_scene env =
  match Sys.getenv_opt "RAYS_UI_PREVIEW" with
  | None -> ()
  | Some directory ->
      (* window-free: the editor's scene into an offscreen canvas *)
      let canvas = Canvas.create_exn ~width:900 ~height:640 in
      Result.get_ok (Canvas.render canvas (E.scene env (frame 0)));
      ignore (Canvas.save_png canvas (Filename.concat directory "scene-inspector.png"));
      ignore (Canvas.destroy canvas)

let domains_agree () =
  (* Every object cooks the same bytes with one domain and with many. *)
  let digest domains =
    let workspace =
      Ws_fixture.of_text
        {|(workspace mountain
      (graph g :context sop
        (sop/mountain (sop/grid :columns 40 :rows 40 :size 4 :width 4 :height 4)
                      :seed 3 :height 0.4 :frequency [1 1 1] :octaves 3 :lacunarity 2
                      :roughness 0.5 :recompute_normals true)))|}
    in
    let env =
      E.create ~ui_text_size:11 ~await:true ~workspace ~domains ~grain:16 ~max_entries:4
        ~max_payload_bytes:(16 * 1024 * 1024)
        ~prepare:Editor_prepare.surface
        ~draw:(fun _ _ -> Scene.D3.create [])
        ()
      |> Result.get_ok
    in
    let env = E.update env (frame 0) in
    match E.prepared env with
    | Some geometry ->
        E.close env;
        let points = Rdk.Geometry.positions geometry in
        Digest.string
          (String.concat ","
             (List.init (Rdk.Packed.Float3.length points) (fun index ->
                  let x, y, z = Rdk.Packed.Float3.get points index in
                  Printf.sprintf "%h %h %h" x y z)))
    | None -> fail "the domain-count cook did not finish"
  in
  check (digest 1 = digest 4) "one domain and four domains cooked different geometry"

let run () =
  let cooks = Atomic.make 0 in
  let env =
    E.create ~ui_text_size:11 ~await:true ~workspace:(Ws_fixture.of_text text) ~max_entries:4
      ~max_payload_bytes:(16 * 1024 * 1024)
      ~prepare:(fun settings output ->
        Atomic.incr cooks;
        Editor_prepare.surface settings output)
      ~draw:(fun _ surface -> Rdk_rays.Surface.scene3 surface.Rays_editor__.Cook.surface)
      ()
    |> Result.get_ok
  in
  let count = ref 0 in
  let step ?mouse ?keys ?buttons env events =
    incr count;
    E.update env (frame ?mouse ?keys ?buttons ~events !count)
  in
  let key k = Input.Key_pressed k and char c = Input.Key_pressed (Input.KeyChar c) in
  (* the editor awaits the cook each frame submits: one frame cooks geo1 *)
  let settle env =
    let env = step env [] in
    if E.prepared env <> None && Atomic.get cooks > 0 then env
    else fail "the scene did not cook geo1"
  in
  let env = settle env in
  let gx, gy, _, _ = (E.panes env (frame 0)).graph in
  (* Focus the graph pane (the list), then walk the rows. *)
  let in_list = (gx + 40, gy + 400) in
  let env =
    step ~mouse:in_list env
      [
        Input.Pointer_pressed (Input.LeftButton, (40., 1.));
        Input.Pointer_released (Input.LeftButton, (40., 1.));
      ]
  in
  let env =
    step ~mouse:in_list env
      [
        Input.Pointer_pressed (Input.LeftButton, (float (fst in_list), float (snd in_list)));
        Input.Pointer_released (Input.LeftButton, (float (fst in_list), float (snd in_list)));
      ]
  in
  let label env = Option.map (fun (p : E.picked) -> p.label) (E.selected_node env) in
  let env = step env [ key Input.Home ] in
  let first = Option.map (fun (p : E.picked) -> p.id) (E.selected_node env) in
  let env = step env [ key Input.ArrowDown ] in
  check
    (Option.map (fun (p : E.picked) -> p.id) (E.selected_node env) <> first)
    "the list's down arrow did not move to the next row";
  let env = step env [ key Input.ArrowUp ] in
  check
    (Option.map (fun (p : E.picked) -> p.id) (E.selected_node env) = first)
    "the list's up arrow did not return to the previous row";
  let rec select name env tries =
    if label env = Some name then env
    else if tries = 0 then
      fail
        ("no list row named " ^ name ^ " among "
        ^ String.concat "," (List.map (fun (i : KG.node) -> i.label) (E.document env))
        ^ " at level "
        ^ Option.value ~default:"scene" (E.level env))
    else select name (step env [ key Input.ArrowDown ]) (tries - 1)
  in
  let env = select "camera1" env 6 in
  check
    ((Camera.lens (E.render_camera env)).aperture = 0.3)
    "the ACTIVE camera did not carry its text lens";
  let env = step env [ char 'i' ] in
  check
    (E.look_through env && E.view_camera env = E.render_camera env && E.level env = None)
    "entering the camera did not look through it at the scene level";
  let env = step env [ char 'i' ] in
  check (E.look_through env) "entering the camera again toggled look-through off";
  let env = step env [ key Input.Home ] in
  let env = select "geo1" env 6 in
  check (E.level env = None) "the editor did not open at the scene level";
  let env = step env [ char 'i' ] in
  check (E.level env = Some "geo1") "i did not enter the focused geometry object";
  check
    (Option.fold ~none:false
       ~some:(fun network ->
         Edit_graph.inspect network
         |> List.exists (fun (info : Edit_graph.node_info) -> info.label = "box"))
       (E.network env))
    "geo1's network is not the sketch's SOP graph";
  let env = step env [ key (Input.KeyChar '/'); char 'l'; char 'l' ] in
  let env = step env [] in
  let projection env =
    let directory = Filename.temp_dir "rays-flow-view" "" in
    Fun.protect
      ~finally:(fun () ->
        Array.iter (fun file -> Sys.remove (Filename.concat directory file)) (Sys.readdir directory);
        Unix.rmdir directory)
      (fun () ->
        E.crash_dump env directory;
        In_channel.with_open_text (Filename.concat directory "editor.txt") (fun channel ->
            ignore (input_line channel);
            input_line channel))
  in
  check (projection env = "projection: list") "/ l l did not reach Flow list";
  let gx, gy, _, _ = (E.panes env (frame 0)).graph in
  let row = (gx + 40, gy + 24 + 12) in
  let env = step ~mouse:row env [] in
  let env =
    step ~mouse:row env
      [
        Input.Pointer_pressed (Input.LeftButton, (float (fst row), float (snd row)));
        Input.Pointer_released (Input.LeftButton, (float (fst row), float (snd row)));
      ]
  in
  let env = step env [ key Input.Enter ] in
  check (projection env = "projection: graph") "list Enter did not open the selected node in graph";
  let env = step env [ char 'u' ] in
  check (E.level env = None) "u did not go back to the scene";
  let env = step env [] in
  (* the list is hit-tested from the next frame *)
  (* A double-click on a list row enters it too. *)
  let row_y index = gy + 24 + (24 * index) + 12 in
  let rec row_of name index env =
    if index > 5 then
      fail
        (name ^ " row not found among "
        ^ String.concat "," (List.map (fun (i : KG.node) -> i.label) (E.document env))
        ^ " at level "
        ^ Option.value ~default:"scene" (E.level env))
    else
      let point = (gx + 60, row_y index) in
      let click =
        [
          Input.Pointer_pressed (Input.LeftButton, (float (fst point), float (snd point)));
          Input.Pointer_released (Input.LeftButton, (float (fst point), float (snd point)));
        ]
      in
      let env = step ~mouse:point env click in
      if label env = Some name then (env, point, click) else row_of name (index + 1) env
  in
  let env, point, click = row_of "geo1" 0 env in
  let env = step ~mouse:point env click in
  check (E.level env = Some "geo1") "double-clicking the geo1 row did not enter it";
  let env = step env [ char 'u' ] in
  let lights_before = List.length (E.lights env) in
  (* Scene edits never re-cook: reparent the light under geo1 with Tab. *)
  let cooked = Atomic.get cooks in
  let position env =
    match E.lights env with
    | { Light.kind = Area { position; _ }; _ } :: _ -> position
    | _ -> fail "the first light is not an area light"
  in
  let before = position env in
  let env = select "key" env 6 in
  let env = step ~keys:[] env [ key Input.Tab ] in
  let parent env =
    E.scene_document env |> List.find (fun (info : KG.node) -> info.label = "key") |> fun info ->
    info.input
  in
  check (parent env <> None) "Tab did not reparent the light under the row above";
  check (contains (source env) ":parent \"geo1\"") "Tab did not write the parent to the text";
  check (E.undo_label env = Some "Reparent") "reparenting is not one 'Reparent' entry";
  check
    (Rays_math.Vec3.nearly_equal (position env) before ~eps:1e-6)
    "reparenting moved the light in the world";
  let env = step ~keys:[ Input.Shift ] env [ key Input.Tab ] in
  check (parent env = None) "Shift-Tab did not move the light back to the scene root";
  check (not (contains (source env) ":parent")) "Shift-Tab left the parent in the text";
  let env = step env [ char 'h' ] in
  check (List.length (E.lights env) = lights_before - 1) "h did not hide the light";
  check (contains (source env) ":visible false") "h did not write the hidden flag to the text";
  check (E.undo_label env = Some "Hide") "hiding is not one 'Hide' entry";
  let env = List.fold_left (fun env _ -> step env []) env [ 1; 2; 3; 4; 5 ] in
  check (Atomic.get cooks = cooked) "a scene-level edit re-cooked SOPs";
  (* Undo walks back through the scene edits. *)
  let undo env = step ~keys:[ Input.Meta ] env [ char 'z' ] in
  let env = undo env in
  check (List.length (E.lights env) = lights_before) "undo did not show the light again";
  check (not (contains (source env) ":visible false")) "undo left the hidden flag in the text";
  (* Nor does dragging an object transform slider in the inspector (undo itself re-cooks, so let
     that settle first).  A scene object is inspected over the projection of the graph that binds
     it, as any graph node is: at the test's 11 points its translate row's x field is 100 points
     in and 346 under the panel top. *)
  let env = List.fold_left (fun env _ -> step env []) env (List.init 40 Fun.id) in
  let cooked = Atomic.get cooks in
  let rec select_up name env tries =
    if label env = Some name then env
    else if tries = 0 then fail ("no list row named " ^ name)
    else select_up name (step env [ key Input.ArrowUp ]) (tries - 1)
  in
  let env = select_up "geo1" env 8 in
  let env = step env [] in
  preview_scene env;
  let ix, iy, _, _ = (E.panes env (frame 0)).inspector in
  let slider x = (ix + 100 + x, iy + 346) in
  let at (x, y) = (float x, float y) in
  let translate_x env =
    Option.bind (E.selected_node env) (fun node ->
        List.find_map
          (fun (field : Param.field_view) ->
            if field.name = "translate_x" then Some field.current else None)
          node.E.fields)
  in
  let before = translate_x env in
  let env =
    step ~mouse:(slider 0) ~buttons:[ Input.LeftButton ] env
      [ Input.Pointer_pressed (Input.LeftButton, at (slider 0)) ]
  in
  let env =
    List.fold_left
      (fun env x -> step ~mouse:(slider x) ~buttons:[ Input.LeftButton ] env [])
      env [ 4; 8; 12; 16 ]
  in
  let env =
    step ~mouse:(slider 20) env [ Input.Pointer_released (Input.LeftButton, at (slider 20)) ]
  in
  check
    (before <> None && translate_x env <> before)
    "dragging the Translate X slider did not edit geo1";
  check (contains (source env) ":translate [") "the Translate X drag did not reach the text";
  check (E.undo_label env = Some "Edit value") "the slider drag is not one \"Edit value\" entry";
  check
    (List.map fst (E.objects env) <> [ Rays_math.Mat4.identity ])
    "the transform did not reach objects";
  let env = List.fold_left (fun env _ -> step env []) env (List.init 20 Fun.id) in
  check (Atomic.get cooks = cooked) "an object transform edit re-cooked SOPs";
  let env =
    step ~mouse:in_list env
      [
        Input.Pointer_pressed (Input.LeftButton, (float (fst in_list), float (snd in_list)));
        Input.Pointer_released (Input.LeftButton, (float (fst in_list), float (snd in_list)));
      ]
  in
  (* / e opens the World, which bakes. *)
  let env = step env [ key (Input.KeyChar '/'); char 'e' ] in
  check (E.level env = Some "world") "/ e did not open the World";
  let env = step env [] in
  check (E.world env <> None) "the World did not bake";
  let env =
    step ~mouse:in_list env
      [
        Input.Pointer_pressed (Input.LeftButton, (float (fst in_list), float (snd in_list)));
        Input.Pointer_released (Input.LeftButton, (float (fst in_list), float (snd in_list)));
      ]
  in
  let env = select "softbox" env 16 in
  let selected = (E.selected_node env |> Option.get).id in
  let parameters env id =
    KG.find (E.document env) id
    |> Option.get |> KG.fields
    |> List.map (fun (field : Param.field_view) -> (field.name, field.current))
  in
  let original = parameters env selected in
  let others =
    E.document env
    |> List.filter (fun (info : KG.node) -> info.id <> selected)
    |> List.map (fun (info : KG.node) -> (info.id, parameters env info.id))
  in
  let press = (100., 300.) and release = (800., 1000.) in
  let camera = E.camera env in
  let env =
    step ~mouse:(100, 300) ~buttons:[ Input.LeftButton ] env
      [ Input.Pointer_pressed (Input.LeftButton, press) ]
  in
  let env =
    step ~mouse:(800, 1000) ~buttons:[ Input.LeftButton ] env [ Input.Pointer_moved release ]
  in
  let moved = parameters env selected in
  check
    (moved <> original && List.assoc "elevation" moved = Param.Float_value (-90.))
    "captured map-layer movement did not clamp outside the map";
  check (contains (source env) ":elevation -90.0") "the map-layer drag did not reach the text";
  check
    (List.for_all (fun (id, values) -> parameters env id = values) others)
    "a map-layer gesture edited another stable node";
  let env = step ~mouse:(800, 1000) env [ Input.Pointer_released (Input.LeftButton, release) ] in
  check
    (Camera.position (Easy_camera.camera (E.camera env))
    = Camera.position (Easy_camera.camera camera))
    "map-layer movement also navigated the 3D camera";
  let env = step ~keys:[ Input.Meta ] env [ char 'z' ] in
  check (parameters env selected = original) "map-layer drag did not undo in one entry";
  check
    (not (contains (source env) ":elevation -90.0"))
    "undoing the map-layer drag left the text edited";
  let env =
    step ~mouse:in_list env
      [
        Input.Pointer_pressed (Input.LeftButton, (float (fst in_list), float (snd in_list)));
        Input.Pointer_released (Input.LeftButton, (float (fst in_list), float (snd in_list)));
      ]
  in
  let env = step env [ key Input.Home ] in
  let env = select "softbox" env 16 in
  (* World keys: t flips the selected emitter to a real light, ] moves the
     time of day, 3 loads the white room preset. *)
  let env = step env [ char 't' ] in
  let env = step env [] in
  check
    (match E.world env with Some baked -> baked.World.lights <> [] | None -> false)
    "t did not promote the selected shape to a light";
  check (contains (source env) ":emit \"Light\"") "World t did not write the emitter to the text";
  let env = select "constellation" env 16 in
  let scatter = (E.selected_node env |> Option.get).id in
  let seed env = List.assoc "seed" (parameters env scatter) in
  let before = seed env in
  let env = step env [ char 'n' ] in
  check
    (match (before, seed env) with Int_value a, Int_value b -> b = a + 1 | _ -> false)
    "World n did not reseed the selected scatter layer";
  let env = step env [ char 'd' ] in
  let day_cycle env =
    List.find_map
      (fun (info : KG.node) -> if info.kind <> "world" then None else KG.field info "day_cycle")
      (E.scene_document env)
    |> Option.get
  in
  check (day_cycle env = Float_value 1.) "World d did not start its day cycle";
  check
    (contains (source env) ":day_cycle 1.0" && contains (source env) ":seed")
    "World n and d did not reach the text";
  let hours env =
    List.find_map
      (fun (info : KG.node) ->
        if info.kind <> "world" then None
        else
          List.find_map
            (fun (field : Param.field_view) ->
              match (field.name, field.current) with
              | "time_of_day", Param.Float_value hours -> Some hours
              | _ -> None)
            (KG.fields info))
      (E.scene_document env)
    |> Option.get
  in
  let before = hours env in
  let env = step env [ char ']' ] in
  check (Float.abs (hours env -. before -. 0.5) < 1e-9) "] did not step the time of day";
  let env = step env [ char '3' ] in
  check
    (List.exists (fun (info : KG.node) -> info.kind = "room") (E.document env))
    "3 did not load the white room preset";
  check
    (contains (source env) "world/room" && E.undo_label env = Some "Preset white room")
    "the white room preset did not replace the World graph in the text";
  let env = step env [ char 'u' ] in
  check (E.level env = None) "u did not leave the World";
  let saved = E.document env and baked = E.world env in
  let env = step env [ char 't'; char 'n'; char 'd' ] in
  (* the lazy products (sh9, specular) are closures until forced; the eager bake decides *)
  let eager =
    Option.map (fun (b : Rays.World.baked) ->
        (b.camera, b.lighting, b.cdf, b.lights, b.sun, b.background, b.exposure))
  in
  check
    (E.document env == saved && eager (E.world env) = eager baked)
    "World keys edited the scene level";
  (* The scene starts in list; the workspace has a scene graph, so / l t shows its text and
     / l g its graph, which is the pane's own scene graph (the outline names it). *)
  let projection_of env = projection env in
  let env = step env [ key (Input.KeyChar '/'); char 'l'; char 't' ] in
  let env = step env [] in
  check (projection_of env = "projection: text") "/ l t did not reach the scene graph's text";
  let env = step env [ key (Input.KeyChar '/'); char 'l'; char 'g' ] in
  let env = step env [] in
  check
    (projection_of env = "projection: graph" && E.level env = None)
    "/ l g did not reach the scene graph";
  (* Deleting a network's display node never leaves a dangling display:
     delete geo1 from the scene, flip the projection, and come back. *)
  let env = step env [ char 'u' ] in
  let env = step env [ key (Input.KeyChar '/'); char 'l'; char 'l' ] in
  let env = step env [] in
  let env = select "geo1" env 8 in
  let env = step env [ key Input.Delete ] in
  (* list -> text -> graph -> list *)
  let env = step env [ key (Input.KeyChar '/'); char 'l'; char 't' ] in
  let env = step env [ key (Input.KeyChar '/'); char 'l'; char 'g' ] in
  let env = step env [ key (Input.KeyChar '/'); char 'l'; char 'l' ] in
  let env = step env [ key (Input.KeyChar '/'); char 'e' ] in
  let env = step env [ char 'u' ] in
  check (E.level env = None) "the scene broke after deleting its display object";
  check (not (contains (source env) "(scene/geometry")) "deleting geo1 left it in the text";
  (* what was written opens as the same scene *)
  let reopened =
    E.create ~ui_text_size:11 ~await:true
      ~workspace:(Ws_fixture.of_text (source env))
      ~prepare:Editor_prepare.surface
      ~draw:(fun _ surface -> Rdk_rays.Surface.scene3 surface.Rays_editor__.Cook.surface)
      ()
    |> Result.get_ok
  in
  check (labelled reopened = labelled env) "the saved text does not open as the edited scene";
  E.close reopened;
  (* The list's right-click menu enters the row under the pointer. *)
  let env = step env [] in
  let env, point, _ = row_of "world" 0 env in
  let fx, fy = (float (fst point), float (snd point)) in
  let env =
    step ~mouse:point env
      [
        Input.Pointer_pressed (Input.RightButton, (fx, fy));
        Input.Pointer_released (Input.RightButton, (fx, fy));
      ]
  in
  let env = step ~mouse:point env [] in
  let item = (fst point + 20, snd point + 3 + 12) in
  let ix, iy = (float (fst item), float (snd item)) in
  let env = step ~mouse:item env [] in
  let env =
    step ~mouse:item env
      [
        Input.Pointer_pressed (Input.LeftButton, (ix, iy));
        Input.Pointer_released (Input.LeftButton, (ix, iy));
      ]
  in
  check (E.level env = Some "world") "the list's right-click Enter did not open the World";
  E.close env;
  domains_agree ();
  print_endline "scene tree: list keys, enter/up, lights, reparent, World, graph, domains ok"

(* The host camera is deleted like any object, and the deletion is in the saved text:
   a scene graph is authoritative, so a reload does not seed it again. *)
let run_host () =
  let open_text text =
    E.create ~ui_text_size:11 ~await:true ~workspace:(Ws_fixture.of_text text)
      ~prepare:Editor_prepare.surface
      ~draw:(fun _ surface -> Rdk_rays.Surface.scene3 surface.Rays_editor__.Cook.surface)
      ()
    |> Result.get_ok
  in
  let count = ref 0 in
  let step ?keys env events =
    incr count;
    E.update env (frame ?keys ~events !count)
  in
  let key k = Input.Key_pressed k in
  let names env =
    List.sort compare (List.map (fun (i : KG.node) -> i.label) (E.scene_document env))
  in
  let delete env name =
    let gx, gy, _, _ = (E.panes env (frame 0)).graph in
    let at = (float (gx + 40), float (gy + 400)) in
    let env =
      step env
        [
          Input.Pointer_pressed (Input.LeftButton, at); Input.Pointer_released (Input.LeftButton, at);
        ]
    in
    let rec select env tries =
      if Option.map (fun (p : E.picked) -> p.label) (E.selected_node env) = Some name then env
      else if tries = 0 then
        fail ("no list row named " ^ name ^ " among " ^ String.concat "," (names env))
      else select (step env [ key Input.ArrowDown ]) (tries - 1)
    in
    step (select (step env [ key Input.Home ]) 8) [ key Input.Delete ]
  in
  let source env = Rays_editor.Workspace_doc.to_text (E.workspace env) in
  let env = step (open_text "(workspace host (graph g :context sop (sop/box)))") [] in
  check
    (names env = List.sort compare [ "camera1"; "g" ])
    ("the host did not seed its camera: " ^ String.concat "," (names env));
  let reopen env what present absent =
    let reopened = step (open_text (source env)) [] in
    check
      (List.for_all (fun n -> List.mem n (names reopened)) present
      && not (List.exists (fun n -> List.mem n (names reopened)) absent))
      (what ^ " came back on reload");
    reopened
  in
  (* the camera *)
  let no_camera = delete env "camera1" in
  check (not (List.mem "camera1" (names no_camera))) "Delete did not remove the host camera";
  check
    (E.undo_label no_camera = Some "Delete")
    "deleting the host camera is not one 'Delete' entry";
  check
    (contains (source no_camera) "graph scene" && not (contains (source no_camera) "scene/camera"))
    "a deleted host camera is not in the text";
  E.close (reopen no_camera "the deleted camera" [ "g" ] [ "camera1" ]);
  let undone = step ~keys:[ Input.Meta ] no_camera [ key (Input.KeyChar 'z') ] in
  check
    (List.mem "camera1" (names undone) && not (contains (source undone) "graph scene"))
    "undo did not give the host camera back";
  List.iter E.close [ env; no_camera; undone ];
  print_endline "scene tree: host camera deletion survives Save and reload"
