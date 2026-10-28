let () =
  let open Editor_core.History in
  let h = create ~capacity:3 0 in
  let h = record 1 h |> record 2 |> record ~merge:Repair 3 in
  assert (present h = 3 && depth h = 2);
  let h = Option.get (undo h) in
  assert (present h = 1 && can_redo h);
  let h = Option.get (redo h) in
  assert (present h = 3 && not (can_redo h));
  let h = record 4 h |> record 5 |> record 6 in
  assert (depth h = 3);
  let rec bottom h = match undo h with Some h -> bottom h | None -> h in
  assert (present (bottom h) = 3 && not (can_undo (bottom h)));
  let h = record ~merge:(Gesture "move/7") 7 h |> record ~merge:(Gesture "move/7") 8 in
  assert (present h = 8 && depth h = 3);
  let h = seal h |> record ~merge:(Gesture "move/7") 9 in
  assert (present (Option.get (undo h)) = 8);
  let burst at value h = record ~merge:(Burst { key = "view"; at; window = 0.25 }) value h in
  let h = create 0 |> burst 1. 1 |> burst 1.2 2 in
  assert (depth h = 1 && present (Option.get (undo h)) = 0);
  let h = burst 1.5 3 h in
  assert (depth h = 2 && present (Option.get (undo h)) = 2);
  let h = Option.get (undo h) in
  (* a repair is no edit: it leaves what redo restores (E12) *)
  let h = record ~merge:Repair 10 h in
  assert (present h = 10 && can_redo h && present (Option.get (redo h)) = 3);
  (* Labels name what undo reverts and what redo reapplies. *)
  let h = create 0 |> record ~label:"Connect" 1 |> record ~label:"Move" 2 in
  assert (label h = "Move");
  let h = Option.get (undo h) in
  assert (label h = "Connect" && redo_label h = Some "Move");
  print_endline "editor history: merge, seal, undo/redo, capacity, labels ok"

type scope = View | Graph

let bindings :
    ( scope,
      [ `Toggle | `Layout | `Undo | `Redo | `Delete | `Frame | `Palette | `Add_light ] )
    Editor_core.Command.t
    list =
  Editor_core.Keymap.
    [
      {
        id = "toggle";
        trigger = Some (Leader "g");
        label = "toggle graph";
        guide = [];
        scope = None;
        action = `Toggle;
      };
      {
        id = "layout";
        trigger = Some (Leader "l");
        label = "layout";
        guide = [];
        scope = Some Graph;
        action = `Layout;
      };
      {
        id = "undo";
        trigger = Some (Chord (Input.KeyChar 'z', [ Input.Meta ]));
        label = "undo";
        guide = [];
        scope = None;
        action = `Undo;
      };
      {
        id = "redo";
        trigger = Some (Chord (Input.KeyChar 'z', [ Input.Meta; Input.Shift ]));
        label = "redo";
        guide = [];
        scope = None;
        action = `Redo;
      };
      {
        id = "delete";
        trigger = Some (Chord (Input.Delete, []));
        label = "delete";
        guide = [];
        scope = Some Graph;
        action = `Delete;
      };
      {
        id = "frame";
        trigger = Some (Chord (Input.KeyChar 'f', []));
        label = "frame";
        guide = [];
        scope = Some Graph;
        action = `Frame;
      };
      {
        id = "add-light";
        trigger = Some (Leader "al");
        label = "add light";
        guide = [];
        scope = None;
        action = `Add_light;
      };
      (* No trigger: palette only, never routed from a key. *)
      Editor_core.Command.make ~id:"palette" ~label:"palette only" `Palette;
    ]

let frame events : Rays.Frame.t =
  {
    input = { (Input.at_time 0.) with frame = 0; tick = 0; size = (10, 10); events };
    pixel_scale = (1., 1.);
    fps = 0.;
    mouse_delta = (0., 0.);
  }

let () =
  let open Rays in
  let open Editor_core.Router in
  let alias ?scope trigger label = Editor_core.Command.make ~id:"alias" ~label ~trigger ?scope () in
  let meta = Editor_core.Keymap.Chord (Input.KeyChar 'c', [ Input.Meta ]) in
  let view = alias ~scope:View meta "view alias"
  and graph = alias ~scope:Graph meta "graph alias"
  and ctrl = alias (Chord (Input.KeyChar 'c', [ Input.Ctrl ])) "Ctrl alias"
  and qj = alias (Leader "qj") "first leader alias"
  and qq = alias (Leader "qq") "second leader alias" in
  let aliases = [ view; graph; ctrl; qj; qq ] in
  let _, matched, _ =
    Editor_core.Router.step aliases ~focus:Graph ~text_focus:false
      ~frame:
        (let frame = frame [ Input.Key_pressed (Input.KeyChar 'c') ] in
         { frame with input = { frame.input with keys = [ Input.Meta ] } })
      Idle
  in
  assert (matched = [ graph ]);
  let _, matched, _ =
    Editor_core.Router.step aliases ~focus:Graph ~text_focus:false
      ~frame:
        (let frame = frame [ Input.Key_pressed (Input.KeyChar 'c') ] in
         { frame with input = { frame.input with keys = [ Input.Ctrl ] } })
      Idle
  in
  assert (matched = [ ctrl ]);
  let _, matched, _ =
    Editor_core.Router.step aliases ~focus:View ~text_focus:false
      ~frame:
        (frame
           [
             Input.Key_pressed (Input.KeyChar '/');
             Input.Key_pressed (Input.KeyChar 'q');
             Input.Key_pressed (Input.KeyChar 'q');
           ])
      Idle
  in
  assert (matched = [ qq ]);
  let traversed, activated, remaining =
    Editor_core.Router.step bindings ~focus:View ~text_focus:false
      ~frame:(frame [ Input.Key_pressed Input.Tab; Input.Key_pressed (Input.KeyChar '/') ])
      Idle
  in
  assert (
    traversed = Idle && activated = []
    && remaining.input.events
       = [ Input.Key_pressed Input.Tab; Input.Key_pressed (Input.KeyChar '/') ]);
  let add =
    Editor_core.Command.make ~id:"graph.add" ~label:"add" ~scope:View
      ~trigger:(Editor_core.Keymap.Chord (Input.Tab, []))
      `Add
  in
  let state, actions, remaining =
    Editor_core.Router.step [ add ] ~focus:View ~text_focus:false
      ~frame:
        (frame
           [
             Input.Key_pressed Input.Tab;
             Input.Key_pressed (Input.KeyChar '/');
             Input.Text_input " ";
           ])
      (Pending "")
  in
  assert (
    state = Idle
    && List.map (fun c -> c.Editor_core.Command.action) actions = [ `Add ]
    && remaining.input.events = [ Input.Key_pressed (Input.KeyChar '/'); Input.Text_input " " ]);
  let state, actions, remaining =
    Editor_core.Router.step [ add ] ~focus:View ~text_focus:false
      ~frame:
        (frame
           [
             Input.Key_pressed Input.Shift;
             Input.Key_pressed Input.Tab;
             Input.Key_pressed (Input.KeyChar '/');
           ])
      Idle
  in
  assert (
    state = Idle
    && List.map (fun c -> c.Editor_core.Command.action) actions = []
    && List.mem (Input.Key_pressed Input.Tab) remaining.input.events
    && List.mem (Input.Key_pressed (Input.KeyChar '/')) remaining.input.events);
  let step ?(focus = View) ?(text_focus = false) state keys =
    Editor_core.Router.step bindings ~focus ~text_focus
      ~frame:(frame (List.map (fun key -> Input.Key_pressed key) keys))
      state
  in
  let state, actions, passed = step Idle [ Input.KeyChar '/'; Input.KeyChar 'g' ] in
  assert (
    state = Idle
    && List.map (fun c -> c.Editor_core.Command.action) actions = [ `Toggle ]
    && passed.input.events = []);
  let state, actions, _ =
    Editor_core.Router.step bindings ~focus:View ~text_focus:false
      ~frame:(frame [ Input.Key_pressed (Input.KeyChar '/'); Input.Text_input " " ])
      Idle
  in
  assert (state = Pending "" && List.map (fun c -> c.Editor_core.Command.action) actions = []);
  let state, actions, passed =
    Editor_core.Router.step bindings ~focus:View ~text_focus:false
      ~frame:(frame [ Input.Key_pressed (Input.KeyChar 'g'); Input.Text_input "g" ])
      state
  in
  assert (
    state = Idle
    && List.map (fun c -> c.Editor_core.Command.action) actions = [ `Toggle ]
    && passed.input.events = []);
  let state, actions, _ = step Idle [ Input.KeyChar '/' ] in
  assert (state = Pending "" && List.map (fun c -> c.Editor_core.Command.action) actions = []);
  let state, actions, passed = step state [ Input.Escape ] in
  assert (
    state = Idle
    && List.map (fun c -> c.Editor_core.Command.action) actions = []
    && passed.input.events = []);
  let _, actions, _ = step Idle [ Input.KeyChar '/'; Input.KeyChar 'l' ] in
  assert (List.map (fun c -> c.Editor_core.Command.action) actions = []);
  let _, actions, _ = step ~focus:Graph Idle [ Input.KeyChar '/'; Input.KeyChar 'l' ] in
  assert (List.map (fun c -> c.Editor_core.Command.action) actions = [ `Layout ]);
  (* Sequences: a proper prefix opens the next which-key page. *)
  let state, actions, _ = step Idle [ Input.KeyChar '/'; Input.KeyChar 'a' ] in
  assert (state = Pending "a" && List.map (fun c -> c.Editor_core.Command.action) actions = []);
  let state, actions, _ = step state [ Input.KeyChar 'L' ] in
  assert (state = Idle && List.map (fun c -> c.Editor_core.Command.action) actions = [ `Add_light ]);
  let state, actions, _ = step Idle [ Input.KeyChar '/'; Input.KeyChar 'a'; Input.KeyChar 'q' ] in
  assert (state = Idle && List.map (fun c -> c.Editor_core.Command.action) actions = []);
  let state, actions, passed = step ~text_focus:true Idle [ Input.KeyChar '/' ] in
  assert (
    state = Idle
    && List.map (fun c -> c.Editor_core.Command.action) actions = []
    && passed.input.events = [ Input.Key_pressed (Input.KeyChar '/') ]);
  let state, _, _ = step Idle [ Input.KeyChar '/' ] in
  let state, actions, passed = step ~text_focus:true state [ Input.KeyChar 'g' ] in
  assert (
    state = Idle
    && List.map (fun c -> c.Editor_core.Command.action) actions = []
    && passed.input.events = [ Input.Key_pressed (Input.KeyChar 'g') ]);
  let chord ?(focus = View) ?(text_focus = false) keys events =
    Editor_core.Router.step bindings ~focus ~text_focus
      ~frame:
        (let frame = frame events in
         { frame with input = { frame.input with keys } })
      Idle
  in
  let _, actions, passed =
    chord [ Input.Meta ] [ Input.Key_pressed (Input.KeyChar 'Z'); Input.Text_input "z" ]
  in
  assert (
    List.map (fun c -> c.Editor_core.Command.action) actions = [ `Undo ] && passed.input.events = []);
  let _, actions, _ = chord [ Input.Meta; Input.Shift ] [ Input.Key_pressed (Input.KeyChar 'z') ] in
  assert (List.map (fun c -> c.Editor_core.Command.action) actions = [ `Redo ]);
  let _, actions, _ =
    chord []
      [
        Input.Key_pressed Input.Meta;
        Input.Key_pressed (Input.KeyChar 'z');
        Input.Key_pressed Input.Shift;
        Input.Key_pressed (Input.KeyChar 'z');
        Input.Key_released Input.Shift;
        Input.Key_pressed (Input.KeyChar 'z');
        Input.Key_released Input.Meta;
      ]
  in
  assert (List.map (fun c -> c.Editor_core.Command.action) actions = [ `Undo; `Redo; `Undo ]);
  let _, actions, _ =
    chord [ Input.Meta ] [ Input.Key_pressed (Input.KeyChar 'z'); Input.Key_pressed Input.Meta ]
  in
  assert (List.map (fun c -> c.Editor_core.Command.action) actions = []);
  let _, actions, _ =
    Editor_core.Router.step ~previous_keys:[ Input.Meta ] bindings ~focus:View ~text_focus:false
      ~frame:
        (frame
           [
             Input.Key_pressed (Input.KeyChar 'z');
             Input.Focus_lost;
             Input.Key_pressed (Input.KeyChar 'z');
           ])
      Idle
  in
  assert (List.map (fun c -> c.Editor_core.Command.action) actions = [ `Undo ]);
  let _, actions, _ =
    Editor_core.Router.step ~previous_keys:[ Input.Meta ] bindings ~focus:View ~text_focus:false
      ~frame:
        (let frame =
           frame
             [
               Input.Key_pressed (Input.KeyChar 'z');
               Input.Key_pressed Input.Meta;
               Input.Key_pressed (Input.KeyChar 'z');
             ]
         in
         { frame with input = { frame.input with keys = [ Input.Meta ] } })
      Idle
  in
  assert (List.map (fun c -> c.Editor_core.Command.action) actions = [ `Undo; `Undo ]);
  let _, actions, _ =
    Editor_core.Router.step ~previous_keys:[] bindings ~focus:View ~text_focus:false
      ~frame:(frame [ Input.Key_pressed (Input.KeyChar 'z'); Input.Key_released Input.Meta ])
      Idle
  in
  assert (List.map (fun c -> c.Editor_core.Command.action) actions = []);
  let _, actions, _ = chord ~focus:Graph [] [ Input.Key_pressed Input.Delete ] in
  assert (List.map (fun c -> c.Editor_core.Command.action) actions = [ `Delete ]);
  let _, actions, _ = chord ~focus:Graph [] [ Input.Key_pressed (Input.KeyChar 'F') ] in
  assert (List.map (fun c -> c.Editor_core.Command.action) actions = [ `Frame ]);
  let _, actions, passed =
    chord ~focus:Graph [ Input.Meta ] [ Input.Key_pressed (Input.KeyChar 'f') ]
  in
  assert (
    List.map (fun c -> c.Editor_core.Command.action) actions = []
    && passed.input.events = [ Input.Key_pressed (Input.KeyChar 'f') ]);
  let _, actions, passed = chord [] [ Input.Key_pressed Input.Delete ] in
  assert (
    List.map (fun c -> c.Editor_core.Command.action) actions = []
    && passed.input.events = [ Input.Key_pressed Input.Delete ]);
  let _, actions, passed =
    chord ~text_focus:true [ Input.Meta ] [ Input.Key_pressed (Input.KeyChar 'z') ]
  in
  assert (
    List.map (fun c -> c.Editor_core.Command.action) actions = []
    && passed.input.events = [ Input.Key_pressed (Input.KeyChar 'z') ]);
  let pointer = Input.Pointer_moved (4., 5.) in
  let ended, passed = fly (frame [ Input.Key_pressed (Input.KeyChar 'w'); pointer ]) in
  assert ((not ended) && passed.input.events = [ pointer ]);
  let ended, passed =
    fly
      (frame
         [
           Input.Key_pressed (Input.KeyChar '/');
           Input.Key_pressed (Input.KeyChar 'g');
           Input.Text_input "g";
         ])
  in
  assert (ended && passed.input.events = [ Input.Key_pressed (Input.KeyChar '/') ]);
  let state, actions, passed =
    Editor_core.Router.step bindings ~focus:View ~text_focus:false ~frame:passed Idle
  in
  assert (
    state = Pending ""
    && List.map (fun c -> c.Editor_core.Command.action) actions = []
    && passed.input.events = []);
  let ended, passed = fly (frame [ Input.Key_pressed Input.Space ]) in
  assert (ended && passed.input.events = []);
  let ended, passed = fly (frame [ Input.Key_pressed Input.Escape ]) in
  assert (ended && passed.input.events = []);
  let ended, passed = fly (frame [ Input.Focus_lost ]) in
  assert (ended && passed.input.events = [ Input.Focus_lost ]);
  (* E10: a focused text field keeps its own Command chords (undo, above) and lets the others run,
     but only commands of every pane *)
  let save =
    Editor_core.Command.make ~id:"save" ~label:"save"
      ~trigger:(Editor_core.Keymap.Chord (Input.KeyChar 's', [ Input.Meta ]))
      `Palette
  and scoped =
    Editor_core.Command.make ~id:"dup" ~label:"duplicate" ~scope:View
      ~trigger:(Editor_core.Keymap.Chord (Input.KeyChar 'd', [ Input.Meta ]))
      `Frame
  in
  let typing events =
    Editor_core.Router.step (save :: scoped :: bindings) ~focus:View ~text_focus:true
      ~frame:
        (let frame = frame events in
         { frame with input = { frame.input with keys = [ Input.Meta ] } })
      Idle
  in
  let _, actions, passed = typing [ Input.Key_pressed (Input.KeyChar 's') ] in
  assert (actions = [ save ] && passed.input.events = []);
  let _, actions, passed = typing [ Input.Key_pressed (Input.KeyChar 'd') ] in
  assert (actions = [] && passed.input.events = [ Input.Key_pressed (Input.KeyChar 'd') ]);
  (* the leader is /: Space plays and pauses, Shift P resets, a bare / waits for the sequence *)
  let play =
    Editor_core.Command.make ~id:"play" ~label:"play"
      ~trigger:(Editor_core.Keymap.Chord (Input.Space, []))
      `Frame
  and reset =
    Editor_core.Command.make ~id:"reset" ~label:"reset"
      ~trigger:(Editor_core.Keymap.Chord (Input.KeyChar 'p', [ Input.Shift ]))
      `Layout
  in
  let run events =
    let state, actions, _ =
      Editor_core.Router.step [ play; reset ] ~focus:View ~text_focus:false ~frame:(frame events)
        Idle
    in
    (state, actions)
  in
  assert (run [ Input.Key_pressed Input.Space ] = (Idle, [ play ]));
  assert (
    run [ Input.Key_pressed Input.Shift; Input.Key_pressed (Input.KeyChar 'p') ] = (Idle, [ reset ]));
  assert (run [ Input.Key_pressed (Input.KeyChar '/') ] = (Pending "", []));
  (* E17: Shift and a symbol key is the symbol it types: / ? is not / /, and a chord
     bound to + is reached by Shift = *)
  let leader sequence action =
    Editor_core.Command.make ~id:sequence ~label:sequence
      ~trigger:(Editor_core.Keymap.Leader sequence) action
  in
  let keys = leader "?" `Layout
  and palette = leader "/" `Palette
  and plus =
    Editor_core.Command.make ~id:"plus" ~label:"larger"
      ~trigger:(Editor_core.Keymap.Chord (Input.KeyChar '+', [ Input.Meta ]))
      `Frame
  in
  let _, actions, _ =
    Editor_core.Router.step [ palette; keys ] ~focus:View ~text_focus:false
      ~frame:
        (frame
           [
             Input.Key_pressed (Input.KeyChar '/');
             Input.Key_pressed Input.Shift;
             Input.Key_pressed (Input.KeyChar '/');
           ])
      Idle
  in
  assert (actions = [ keys ]);
  let _, actions, _ =
    Editor_core.Router.step [ palette; keys ] ~focus:View ~text_focus:false
      ~frame:
        (frame [ Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar '/') ])
      Idle
  in
  assert (actions = [ palette ]);
  let _, actions, _ =
    Editor_core.Router.step [ plus ] ~focus:View ~text_focus:false
      ~frame:
        (let frame = frame [ Input.Key_pressed (Input.KeyChar '=') ] in
         { frame with input = { frame.input with keys = [ Input.Meta; Input.Shift ] } })
      Idle
  in
  assert (actions = [ plus ]);
  print_endline "editor router: leader/chord scope, text focus, event consumption ok"
