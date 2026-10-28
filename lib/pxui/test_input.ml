open Rays
module Ui = Pxui.Ui

let check condition message = if not condition then failwith message

let frame ?(buttons = []) ?(keys = []) events : Frame.t =
  {
    input =
      {
        (Input.at_time 0.) with
        frame = 0;
        tick = 0;
        size = (320, 240);
        pointer = (10., 10.);
        keys;
        buttons;
        events;
      };
    pixel_scale = (1., 1.);
    fps = 60.;
    mouse_delta = (0., 0.);
  }

let run () =
  let ui = Ui.create ~font_size:11 ?font_path:(Sys.getenv_opt "RAYS_UI_FONT") () in
  Fun.protect
    ~finally:(fun () -> Ui.destroy ui)
    (fun () ->
      let body ui =
        let view =
          Ui.box ui ~flags:Ui.clickable ~at:(0., 0.) ~w:(Ui.Px 160.) ~h:(Ui.Px 240.) "view"
        in
        let control =
          Ui.within ui view (fun () ->
              Ui.box ui
                ~flags:Ui.(clickable + focusable)
                ~at:(30., 30.) ~w:(Ui.Px 50.) ~h:(Ui.Px 50.) "control")
        in
        let graph =
          Ui.box ui
            ~flags:Ui.(clickable + scroll)
            ~at:(160., 0.) ~w:(Ui.Px 160.) ~h:(Ui.Px 240.) "graph"
        in
        (Ui.key view, Ui.signal ui control, Ui.signal ui graph)
      in
      let step ?(popup = false) frame =
        let owner, control, graph =
          Ui.frame ui frame (fun ui ->
              if popup then ignore (Ui.modal ui "modal" (fun () -> Ui.label ui "Popup"));
              body ui)
        in
        (Ui.input ~owner ui, control, graph)
      in
      ignore (step (frame []));
      let drag =
        [
          Input.Pointer_pressed (Input.RightButton, (10., 10.));
          Input.Pointer_moved (220., 20.);
          Input.Pointer_released (Input.RightButton, (220., 20.));
        ]
      in
      let input, _, graph = step (frame drag) in
      check
        (input.input.events = drag && not graph.pressed)
        "viewport capture did not survive a pane crossing/release";
      let drag =
        [
          Input.Pointer_pressed (Input.MiddleButton, (200., 10.));
          Input.Pointer_moved (10., 10.);
          Input.Pointer_released (Input.MiddleButton, (10., 10.));
        ]
      in
      let input, _, graph = step (frame drag) in
      check
        (input.input.events = [ Input.Pointer_cancelled Input.MiddleButton ] && graph.pressed)
        "graph-owned drag reached the viewport";
      let click =
        [
          Input.Pointer_pressed (Input.LeftButton, (40., 40.));
          Input.Pointer_released (Input.LeftButton, (40., 40.));
          Input.Key_pressed Input.Enter;
        ]
      in
      let input, control, _ = step (frame ~keys:[ Input.Shift ] click) in
      check
        (control.clicked
        && input.input.events = [ Input.Pointer_cancelled Input.LeftButton ]
        && input.input.keys = [])
        "child widget input escaped to its viewport root";
      let events = click @ [ Input.Scrolled (0., 1.) ] in
      let input, control, _ = step ~popup:true (frame events) in
      check
        ((not control.clicked) && input.input.buttons = []
        && input.mouse_delta = (0., 0.)
        && input.input.events = [ Input.Pointer_cancelled Input.LeftButton ])
        "new popup did not shield this frame's underlying controls";
      let input, control, _ = step ~popup:true (frame (Input.Key_pressed Input.Escape :: click)) in
      check
        ((not control.clicked) && input.input.events = [ Input.Pointer_cancelled Input.LeftButton ])
        "popup dismissal leaked its remaining press/release events";
      ignore (step (frame []));
      ignore
        (step
           (frame ~buttons:[ Input.RightButton ]
              [ Input.Pointer_pressed (Input.RightButton, (10., 10.)) ]));
      let input, _, _ = step ~popup:true (frame ~buttons:[ Input.RightButton ] []) in
      check
        (List.mem (Input.Pointer_cancelled Input.RightButton) input.input.events)
        "popup opening did not cancel the viewport's captured gesture";
      ignore (step (frame [ Input.Focus_lost ]));
      let input, _, _ =
        step (frame [ Input.Focus_lost; Input.Pointer_cancelled Input.RightButton ])
      in
      check
        (input.input.events = [ Input.Focus_lost; Input.Pointer_cancelled Input.RightButton ])
        "cancellation events were consumed";
      let only_view ui =
        Ui.key
          (Ui.box ui ~flags:Ui.clickable ~at:(0., 0.) ~w:(Ui.Px 160.) ~h:(Ui.Px 240.) "only-view")
      in
      ignore (Ui.frame ui (frame []) only_view);
      let unowned_click =
        [
          Input.Pointer_pressed (Input.LeftButton, (200., 10.));
          Input.Pointer_released (Input.LeftButton, (200., 10.));
        ]
      in
      let owner = Ui.frame ui (frame unowned_click) only_view in
      check
        ((Ui.input ~owner ui).input.events = [ Input.Pointer_cancelled Input.LeftButton ])
        "unowned pointer events escaped to an explicit viewport owner";
      check ((Ui.input ui).input.events = unowned_click) "default input lost unowned pointer events";
      let modal_y key =
        let row =
          Ui.frame ui (frame []) (fun ui ->
              Ui.modal ui key (fun () -> Ui.box ui ~h:(Ui.Px 24.) "row"))
          |> Option.get
        in
        let _, y, _, _ = Ui.rect ui row in
        y
      in
      ignore (modal_y "height-original");
      let centred = modal_y "height-original" in
      ignore (Ui.frame ui (frame []) (fun _ -> ()));
      ignore (Ui.frame ui (frame []) (fun _ -> ()));
      check (modal_y "height-original" = centred) "closed modal lost its remembered height";
      for index = 0 to 63 do
        let key = "height-" ^ string_of_int index in
        ignore (modal_y key);
        ignore (modal_y key)
      done;
      check
        (modal_y "height-original" <> centred)
        "dynamic modal keys did not evict old cached heights";
      let probe ui =
        let box = Ui.box ui ~flags:Ui.clickable ~w:(Ui.Px 100.) ~h:(Ui.Px 100.) "press-keys" in
        Ui.press_keys ui box
      in
      ignore (Ui.frame ui (frame []) probe);
      let pressed =
        Ui.frame ui
          (frame ~keys:[ Input.Shift ] ~buttons:[ Input.LeftButton ]
             [ Input.Key_pressed Input.Shift; Input.Pointer_pressed (Input.LeftButton, (10., 10.)) ])
          probe
      in
      let held =
        Ui.frame ui
          (frame ~buttons:[ Input.LeftButton ]
             [ Input.Key_released Input.Shift; Input.Pointer_moved (20., 10.) ])
          probe
      in
      let released =
        Ui.frame ui (frame [ Input.Pointer_released (Input.LeftButton, (20., 10.)) ]) probe
      in
      check
        (pressed = [ Input.Shift ] && held = pressed && released = pressed)
        "captured press keys changed during motion or release";
      (* capture_unclaimed: a blocking (non-clickable) child under the ancestor takes the press away
     from it only where the child is clickable *)
      let unclaimed ui =
        let outer =
          Ui.box ui
            ~flags:Ui.(capture_unclaimed + blocking)
            ~at:(0., 0.) ~w:(Ui.Px 200.) ~h:(Ui.Px 200.) "outer"
        in
        let taker =
          Ui.within ui outer (fun () ->
              Ui.box ui ~flags:Ui.clickable ~at:(100., 0.) ~w:(Ui.Px 50.) ~h:(Ui.Px 50.) "taker")
        in
        let plain =
          Ui.within ui outer (fun () ->
              Ui.box ui ~flags:Ui.scroll ~at:(0., 100.) ~w:(Ui.Px 50.) ~h:(Ui.Px 50.) "plain")
        in
        ignore plain;
        (Ui.signal ui outer, Ui.signal ui taker)
      in
      ignore (Ui.frame ui (frame []) unclaimed);
      let outer, taker =
        Ui.frame ui
          (frame ~buttons:[ Input.LeftButton ]
             [
               Input.Pointer_pressed (Input.LeftButton, (110., 10.)); Input.Pointer_moved (120., 10.);
             ])
          unclaimed
      in
      check
        (taker.pressed && (not outer.pressed) && not outer.held)
        "ancestor captured a consumed press";
      ignore
        (Ui.frame ui (frame [ Input.Pointer_released (Input.LeftButton, (120., 10.)) ]) unclaimed);
      let outer, _ =
        Ui.frame ui
          (frame ~buttons:[ Input.LeftButton ]
             [
               Input.Pointer_pressed (Input.LeftButton, (10., 110.)); Input.Pointer_moved (30., 90.);
             ])
          unclaimed
      in
      check
        (outer.pressed && outer.held && outer.dragging
        && outer.drag = (20., -20.)
        && outer.press_point = (10., 110.))
        "ancestor did not capture an unclaimed press";
      let outer, _ =
        Ui.frame ui (frame [ Input.Pointer_released (Input.LeftButton, (60., 90.)) ]) unclaimed
      in
      check
        (outer.released && (not outer.held) && outer.release_point = (60., 90.))
        "unclaimed capture did not release";
      print_endline
        "PXUI ordered input: viewport/child ownership, crossings, popup transitions and \
         cancellation passed")
