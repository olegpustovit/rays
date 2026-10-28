open Rays

let check condition message = if not condition then failwith message
let key k = Input.Key_pressed k
let char c = key (Input.KeyChar c)
let palette = [key (Input.KeyChar '/'); char '/']
let wheel point = [Input.Pointer_moved point; Input.Scrolled (0., 1.)]

let frame ?(buttons = []) ?(keys = []) ?(delta = (0., 0.)) mouse events count : Frame.t = {
  input = { (Input.at_time (float count /. 60.)) with dt = 1. /. 60.;
    frame = count; tick = count; size = 900, 640; pointer = mouse; keys; buttons; events };
  pixel_scale = 1., 1.; fps = 60.; mouse_delta = delta }

let run () =
  let exercise ~name ~create ~update ~close ~camera ~dump ~panes =
    let current = ref (create ()) and count = ref 0 in
    let directory = Filename.temp_dir "rays-input-contract" "" in
    Fun.protect ~finally:(fun () -> close !current;
      Array.iter (fun file -> Sys.remove (Filename.concat directory file)) (Sys.readdir directory);
      Unix.rmdir directory) (fun () ->
    let step ?buttons ?keys ?delta ?(mouse = (100., 300.)) events =
      incr count;
      current := update !current (frame ?buttons ?keys ?delta mouse events !count) in
    let snapshot () =
      dump !current directory;
      In_channel.with_open_bin (Filename.concat directory "document.txt") In_channel.input_all in
    let still before message = check (snapshot () = before) (name ^ ": " ^ message) in
    let modal_gestures () =
      step ~mouse:(330., 300.) (wheel (330., 300.));
      List.iter (fun button ->
        step ~buttons:[button] ~mouse:(100., 300.) [Input.Pointer_pressed (button, (100., 300.))];
        step ~buttons:[button] ~keys:[Input.Shift] ~delta:(70., 20.) ~mouse:(170., 320.)
          [Input.Pointer_moved (170., 320.)];
        step ~mouse:(170., 320.) [Input.Pointer_released (button, (170., 320.))];
        step palette; step [])
        [Input.LeftButton; Input.RightButton; Input.MiddleButton] in
    step [];
    let before_wheel = camera !current in
    step ~mouse:(800., 300.) [Input.Pointer_moved (100., 300.);
      Input.Scrolled (0., 1.); Input.Pointer_moved (800., 300.)];
    check (camera !current <> before_wheel)
      (name ^ ": a later pane crossing discarded an owned wheel event");
    let after_wheel = camera !current in
    step ~mouse:(100., 300.) [Input.Pointer_moved (800., 300.);
      Input.Scrolled (0., 1.); Input.Pointer_moved (100., 300.)];
    check (camera !current = after_wheel)
      (name ^ ": an inspector-owned wheel reached the viewport after crossing");
    let original = snapshot () in
    step palette; step [];
    modal_gestures ();
    still original "modal wheel/pointer/Shift-World gestures changed document or camera";
    step [key Input.Escape]; step [];
    let original = snapshot () in
    (* Newly opened and dismissed popups own all remaining pointer events
       even when the whole transition happens in one frame. *)
    step ~mouse:(330., 300.) (palette @ wheel (330., 300.));
    still original "same-frame popup opening leaked a wheel";
    step ~mouse:(100., 300.) ([key Input.Escape;
      Input.Pointer_pressed (Input.RightButton, (100., 300.)); Input.Pointer_moved (170., 320.);
      Input.Pointer_released (Input.RightButton, (170., 320.))] @ wheel (100., 300.));
    still original "same-frame popup dismissal leaked a press/move/release/wheel";
    step [];
    let rotation () =
      let line = String.split_on_char '\n' (snapshot ())
        |> List.find (fun line -> String.length line > 0 && String.trim line <> ""
          && List.mem "world" (String.split_on_char ' ' line) && String.starts_with ~prefix:"  node" line) in
      String.split_on_char ' ' line |> List.find_map (fun word ->
        if String.starts_with ~prefix:"rotation=" word
        then Some (float_of_string (String.sub word 9 (String.length word - 9))) else None)
      |> Option.get in
    let original = snapshot () and before = camera !current in
    step ~mouse:(140., 300.) [key Input.Shift;
      Input.Pointer_pressed (Input.LeftButton, (100., 300.));
      Input.Pointer_moved (130., 300.); Input.Key_released Input.Shift;
      Input.Pointer_released (Input.LeftButton, (140., 300.))];
    check (rotation () = 20. && camera !current = before)
      (name ^ ": same-frame World drag lost ordered movement or also navigated");
    step ~keys:[Input.Meta] [char 'z'];
    still original "one same-frame World drag did not undo as one operation";
    step ~buttons:[Input.LeftButton] ~keys:[Input.Shift] ~mouse:(120., 300.)
      [Input.Pointer_pressed (Input.LeftButton, (100., 300.)); Input.Pointer_moved (120., 300.)];
    check (rotation () = 10.) (name ^ ": first-frame held World movement was lost");
    step ~buttons:[Input.LeftButton] ~mouse:(150., 300.)
      [Input.Key_released Input.Shift; Input.Pointer_moved (150., 300.)];
    check (rotation () = 25. && camera !current = before)
      (name ^ ": releasing Shift changed the operation during a World drag");
    step ~mouse:(160., 300.) [Input.Pointer_released (Input.LeftButton, (160., 300.))];
    check (rotation () = 30.) (name ^ ": World drag dropped its final release position");
    let first_drag = snapshot () in
    step ~mouse:(120., 300.) [key Input.Shift;
      Input.Pointer_pressed (Input.LeftButton, (100., 300.));
      Input.Pointer_released (Input.LeftButton, (120., 300.)); Input.Key_released Input.Shift];
    check (rotation () = 40.) (name ^ ": second World drag did not apply");
    step ~keys:[Input.Meta] [char 'z'];
    still first_drag "two separate World drags merged in history";
    step ~keys:[Input.Meta] [char 'z'];
    still original "release-frame movement split one World drag into multiple undo entries";
    step ~buttons:[Input.LeftButton] ~keys:[Input.Shift]
      [Input.Pointer_pressed (Input.LeftButton, (100., 300.))];
    step ~buttons:[Input.LeftButton] palette;
    step [key Input.Escape]; step [];
    step ~mouse:(200., 300.) [Input.Pointer_moved (200., 300.)];
    still original "a popup-cancelled World drag resumed on movement";
    List.iter (fun cancellation ->
      step ~buttons:[Input.LeftButton] ~keys:[Input.Shift]
        [Input.Pointer_pressed (Input.LeftButton, (100., 300.))];
      step [cancellation];
      step ~mouse:(220., 300.) [Input.Pointer_moved (220., 300.)];
      still original "a cancelled World drag resumed on movement")
      [Input.Focus_lost; Input.Pointer_cancelled Input.LeftButton];
    step ~buttons:[Input.LeftButton] ~keys:[Input.Shift] ~mouse:(360., 300.)
      [Input.Pointer_pressed (Input.LeftButton, (360., 300.))];
    step ~buttons:[Input.LeftButton] ~keys:[Input.Shift] ~mouse:(430., 300.)
      [Input.Pointer_moved (430., 300.)];
    check (rotation () = 35. && camera !current = before)
      (name ^ ": a World drag lost ownership at a pane crossing");
    step ~mouse:(430., 300.) [Input.Pointer_released (Input.LeftButton, (430., 300.))];
    step ~keys:[Input.Meta] [char 'z'];
    still original "cross-pane World drag did not undo as one operation";
    step ~mouse:(-2300., 300.) [key Input.Shift;
      Input.Pointer_pressed (Input.LeftButton, (100., 300.));
      Input.Pointer_released (Input.LeftButton, (-2300., 300.)); Input.Key_released Input.Shift];
    check (rotation () = -120.) (name ^ ": a large negative World drag did not wrap rotation");
    step ~keys:[Input.Meta] [char 'z'];
    still original "large negative World drag did not undo in one entry";
    step ~mouse:(140., 300.) [key Input.Shift;
      Input.Pointer_pressed (Input.LeftButton, (100., 300.));
      Input.Pointer_released (Input.LeftButton, (120., 300.));
      Input.Pointer_pressed (Input.LeftButton, (100., 300.));
      Input.Pointer_released (Input.LeftButton, (140., 300.)); Input.Key_released Input.Shift];
    check (rotation () = 30.) (name ^ ": batched World gestures lost an operation");
    step ~keys:[Input.Meta] [char 'z'];
    check (rotation () = 10.) (name ^ ": batched World gestures merged in history");
    step ~keys:[Input.Meta] [char 'z'];
    still original "batched World gesture undo did not restore the original";
    step ~keys:[Input.Shift] [];
    step ~mouse:(140., 300.) [Input.Pointer_pressed (Input.LeftButton, (100., 300.));
      Input.Pointer_moved (140., 300.); Input.Focus_lost];
    check (rotation () = 20.)
      (name ^ ": final focus loss erased Shift from earlier World events");
    step ~keys:[Input.Meta] [char 'z'];
    still original "focus-cancelled World drag did not undo in one entry";
    (* Start in the view, cross both other panes, and finish outside it. *)
    let before = camera !current in
    step ~buttons:[Input.RightButton] [Input.Pointer_pressed (Input.RightButton, (100., 300.))];
    step ~buttons:[Input.RightButton] ~mouse:(500., 320.) [Input.Pointer_moved (500., 320.)];
    let across_graph = camera !current in
    check (across_graph <> before) (name ^ ": viewport drag stopped at the graph pane");
    step ~buttons:[Input.RightButton] ~mouse:(800., 350.) [Input.Pointer_moved (800., 350.)];
    check (camera !current <> across_graph) (name ^ ": viewport drag stopped at the inspector pane");
    step ~mouse:(800., 350.) [Input.Pointer_released (Input.RightButton, (800., 350.))];
    let released = camera !current in
    step [Input.Pointer_moved (110., 300.)];
    check (camera !current = released) (name ^ ": released drag resumed on re-entry");
    step ~buttons:[Input.RightButton] [Input.Pointer_pressed (Input.RightButton, (100., 300.))];
    step [Input.Focus_lost];
    let cancelled = camera !current in
    step [Input.Pointer_moved (140., 330.)];
    check (camera !current = cancelled) (name ^ ": focus loss did not end the viewport drag");
    step ~buttons:[Input.RightButton] [Input.Pointer_pressed (Input.RightButton, (100., 300.))];
    step ~buttons:[Input.RightButton] palette;
    step [key Input.Escape]; step [];
    let cancelled = camera !current in
    step [Input.Pointer_moved (190., 350.)];
    check (camera !current = cancelled) (name ^ ": popup did not cancel an existing drag");
    (* the scene opens as a list: / l g in the graph pane shows the scene graph *)
    begin
      let gx, gy, _, _ = (panes !current (frame (0., 0.) [] 0)).Pxui_shell.Layout.graph in
      let point = float (gx + 25), float (gy + 300) in
      step ~mouse:point [Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point)];
      step [key (Input.KeyChar '/'); char 'l'; char 'g']
    end;
    step [];
    let gx, gy, _, _ = (panes !current (frame (0., 0.) [] 0)).Pxui_shell.Layout.graph in
    let point = float (gx + 25), float (gy + 300) in
    let before = camera !current in
    step ~buttons:[Input.MiddleButton] ~mouse:point [Input.Pointer_pressed (Input.MiddleButton, point)];
    step ~buttons:[Input.MiddleButton] [Input.Pointer_moved (100., 300.)];
    step [Input.Pointer_released (Input.MiddleButton, (100., 300.))];
    check (camera !current = before) (name ^ ": graph-owned pan began viewport navigation");
    (* a press in the graph pane that drags out over the view is the pane's, never navigation
       (the pane's own gestures, moving a node included, are tested through [Scope]) *)
    let gx, gy, _, gh = (panes !current (frame (0., 0.) [] 0)).Pxui_shell.Layout.graph in
    (* the framed graph leaves its corner empty: a press there is the canvas's, not a card's (a card
       dragged out over the view would start a carry, which the next click puts) *)
    let point = float (gx + 12), float (gy + gh - 12) in
    step ~mouse:point [key Input.Home];
    step [];
    step ~buttons:[Input.LeftButton] ~mouse:point [Input.Pointer_pressed (Input.LeftButton, point)];
    step ~buttons:[Input.LeftButton] [Input.Pointer_moved (100., 300.)];
    step [Input.Pointer_released (Input.LeftButton, (100., 300.))];
    check (camera !current = before) (name ^ ": a graph pane drag reached viewport navigation");
    count := !count + 30; (* past the double-click interval *)
    (* Tab adds in the graph; shared chrome traversal still works from View. *)
    let view_point = 100., 300. in
    step ~mouse:view_point [Input.Pointer_pressed (Input.LeftButton, view_point);
      Input.Pointer_released (Input.LeftButton, view_point)];
    let _, _, width, _ = (panes !current (frame (0., 0.) [] 0)).Pxui_shell.Layout.view in
    step [key Input.Tab; key Input.Space];
    let _, _, collapsed, _ = (panes !current (frame (0., 0.) [] 0)).Pxui_shell.Layout.view in
    check (collapsed < width) (name ^ ": same-frame Tab/Space did not activate the pane header")) in
  let workspace = Ws_fixture.of_text {|(workspace world
    (graph g :context sop (sop/box))
    (graph sky :context world (world/none))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g)) (scene/world (ref sky)))))|} in
  let module E3 = Rays_editor.Editor in
  exercise ~name:"Editor"
    ~create:(fun () -> E3.create ~ui_text_size:11 ~workspace ~camera:(Easy_camera.create ~inertia:false ())
      ~prepare:Editor_prepare.empty ~draw:(fun _ _ -> Scene.D3.empty) () |> Result.get_ok)
    ~update:E3.update ~close:E3.close ~dump:E3.crash_dump ~panes:E3.panes
    ~camera:(fun env -> let camera = E3.camera env in
      Camera.position (Easy_camera.camera camera), Camera.target (Easy_camera.camera camera),
      Easy_camera.distance camera);
  print_endline "editor input: the host shields popups/World edits and retain viewport/graph gesture owners"
