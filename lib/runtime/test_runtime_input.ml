module Source = Runtime_input

let get = function Ok value -> value | Error message -> failwith message

let key ?(modifiers = []) ?(down = true) ?(repeat = false) key =
  Sdl3.Event.Key { key; modifiers; down; repeat }

let window change = Sdl3.Event.Window change

let sdl_fixture () =
  [
    Sdl3.Event.Mouse_motion { x = 7.; y = 11.; dx = 7.; dy = 11. };
    Mouse_button { button = Left; down = true; x = 7.; y = 11. };
    Mouse_motion { x = 9.; y = 14.; dx = 2.; dy = 3. };
    Mouse_button { button = Left; down = false; x = 9.; y = 14. };
    Mouse_wheel { x = 1.; y = -2.; direction = Normal; mouse_x = 9.;
      mouse_y = 14.; integer_x = 1; integer_y = -2 };
    key (Char 'a');
    key (Char 'a') ~down:false;
    Text_input "a";
    Text_editing { text = "ab"; start = 1; length = 1 };
    window (Resized (20, 10));
    window Focus_lost;
  ]

let wheel ~x ~y direction = Sdl3.Event.Mouse_wheel { x; y; direction;
  mouse_x = 9.; mouse_y = 14.; integer_x = 0; integer_y = -1 }

let run () =
  let native = get (Source.create ~max_events:64
      ~logical_width:10 ~logical_height:10) in
  for _ = 1 to 3 do
    List.iter (fun event -> get (Runtime_input_sdl3.push native event))
      (sdl_fixture ());
  done;
  let native_trace = Source.drain native in
  if List.length native_trace <> 33 then
    failwith "native frozen 33-event trace differs";
  let native_facts = Source.snapshot native in
  if native_facts.pointer <> (9., 14.) then
    failwith "SDL logical coordinates were incorrectly Retina-scaled";
  (* Three rounds of 0,0 -> 7,11 -> 9,14: the absolute moves sum to 9,14. *)
  if native_facts.mouse_delta <> (9., 14.) then
    failwith "frame delta accumulation drift";
  Source.begin_frame native;
  if (Source.snapshot native).mouse_delta <> (0., 0.) then
    failwith "frame deltas did not reset";
  get (Runtime_input_sdl3.push native (wheel ~x:0.5 ~y:(-1.5) Flipped));
  (match Source.drain native with
   | [Scrolled (0.5, -1.5)] -> ()
   | _ -> failwith "flipped trackpad scroll was inverted a second time");
  Source.begin_frame native;
  get (Runtime_input_sdl3.push native (wheel ~x:0.5 ~y:(-1.5) Normal));
  (match Source.drain native with
   | [Scrolled (0.5, -1.5)] -> ()
   | _ -> failwith "ordinary wheel direction changed");
  Source.begin_frame native;
  get (Runtime_input_sdl3.push native (key Space));
  (match Source.drain native with
   | [Key_pressed Input.Space] -> ()
   | _ -> failwith "the space bar did not arrive as the named Space key");
  (* Relative mode sums SDL relative motion; the clamped absolute position
     at the window edge no longer contributes. *)
  Source.set_relative native true;
  let edge x dx = Sdl3.Event.Mouse_motion { x; y = 5.; dx; dy = -1. } in
  List.iter (fun event -> get (Runtime_input_sdl3.push native event))
    [edge 10. 25.; edge 10. 30.; edge 10. 5.];
  if (Source.snapshot native).mouse_delta <> (60., -3.) then
    failwith "relative mode did not accumulate SDL relative motion";
  Source.set_relative native false;
  Source.begin_frame native;
  ignore (Source.drain native);
  get (Source.push native (Pointer_pressed (Input.LeftButton, (1., 1.))));
  get (Source.push native Focus_lost);
  (match Source.drain native with
   | [Pointer_pressed (Input.LeftButton, _); Focus_lost] -> ()
   | _ -> failwith "focus loss synthesized a pointer cancellation event");
  get (Source.push native (Pointer_pressed (Input.LeftButton, (1., 1.))));
  get (Source.push native (Pointer_cancelled Input.LeftButton));
  ignore (Source.drain native);
  (* keys: held keys are variants, and a modifier list and repeat survive *)
  get (Runtime_input_sdl3.push native (key (Char 'a')));
  get (Runtime_input_sdl3.push native (key Control));
  get (Runtime_input_sdl3.push native (key (Char 'a') ~down:false));
  (match Source.drain native with
   | [Key_pressed (Input.KeyChar 'a'); Key_pressed Input.Ctrl;
      Key_released (Input.KeyChar 'a')] -> ()
   | _ -> failwith "keys are not the translated key variants in order");
  (match Runtime_input_sdl3.translate (key (Unknown 0x4000_0064)) with
   | Some (Key_pressed (Input.Unknown 0x4000_0064)) -> ()
   | _ -> failwith "an unknown key lost its code");
  List.iter (fun (change, expected) ->
    if Runtime_input_sdl3.translate (window change) <> expected then
      failwith "window authority mapping drift")
    Sdl3.Event.[
      Shown, Some (Visibility_changed true);
      Hidden, Some (Visibility_changed false);
      Minimized, Some (Visibility_changed false);
      Occluded, Some (Visibility_changed false);
      Restored, Some (Visibility_changed true);
      Focus_gained, Some Input.Focus_gained;
      Focus_lost, Some Input.Focus_lost;
      Close_requested, Some Input.Closed;
      Resized (640, 480), Some (Input.Resized (640, 480));
      Pixel_size_changed (1280, 960), Some (Input.Pixel_size_changed (1280, 960));
      (* a minimizing window can report no size: no event, no error *)
      Resized (0, 0), None;
      Pixel_size_changed (0, 0), None;
    ];
  if Runtime_input_sdl3.translate Sdl3.Event.Quit <> Some Input.Closed then
    failwith "quit mapping drift";
  (* visibility reaches the snapshot, and a pixel size changes nothing held *)
  let visibility = get (Source.create ~max_events:8 ~logical_width:4 ~logical_height:4) in
  if not (Source.snapshot visibility).visible then failwith "a new source starts hidden";
  get (Runtime_input_sdl3.push visibility (window Occluded));
  if (Source.snapshot visibility).visible then failwith "occlusion did not hide the source";
  get (Runtime_input_sdl3.push visibility (window Restored));
  if not (Source.snapshot visibility).visible then failwith "restore did not show the source";
  get (Runtime_input_sdl3.push visibility (window (Pixel_size_changed (800, 600))));
  (match Source.drain visibility with
   | [Visibility_changed false; Visibility_changed true; Pixel_size_changed (800, 600)] -> ()
   | _ -> failwith "window events did not arrive in order");
  List.iter (fun event -> get (Runtime_input_sdl3.push native event))
    Sdl3.Event.[
      Pinch { phase = Began; scale = 1.0 };
      Pinch { phase = Updated; scale = 1.25 };
      Pinch { phase = Ended; scale = 1.0 };
    ];
  (match Source.drain native with
   | [Input.Pinched 1.25] -> ()
   | _ -> failwith "a pinch gesture did not become one zoom factor");
  if Result.is_ok (Source.push native (Input.Pinched 0.)) then
    failwith "a zero pinch factor passed input validation";
  (* a phased trackpad scroll keeps its points, phase and clock, and only its wheel twin counts
     toward the frame's wheel delta *)
  get (Runtime_input_sdl3.push native
    (Sdl3.Event.Scroll { x = 0.; y = -12.; phase = Scroll_changed; seconds = 3.5 }));
  (match Source.drain native with
   | [Input.Trackpad_scrolled { delta = (0., -12.); phase = Input.Moved; time = 3.5 }] -> ()
   | _ -> failwith "a trackpad scroll was not translated as it came");
  if Result.is_ok (Source.push native
      (Input.Trackpad_scrolled { delta = (Float.nan, 0.); phase = Input.Lifted; time = 0. })) then
    failwith "a non-finite trackpad scroll passed input validation";
  (* a finished file dialog is one event: the paths, an empty cancel, or the failure *)
  List.iter (fun (outcome, expected) ->
    match Runtime_input_sdl3.translate (Sdl3.Event.Dialog { id = 7; outcome }) with
    | Some (File_dialog { id = 7; result }) when result = expected -> ()
    | _ -> failwith "a file dialog outcome did not translate")
    Sdl3.Event.[
      Chosen [ "/tmp/a.png"; "/tmp/b.png" ], Ok [ "/tmp/a.png"; "/tmp/b.png" ];
      Cancelled, Ok [];
      Failed "no portal", Error "no portal";
    ];
  (* files: a drag over the window has a position, the drop a path *)
  let drop="/definitely/missing/dir/drop.bin" in
  List.iter (fun event -> get (Runtime_input_sdl3.push native event))
    Sdl3.Event.[
      Drop { change = Drop_begin; x = 0.; y = 0. };
      Drop { change = Drop_position; x = 5.5; y = 6.5 };
      Drop { change = File drop; x = 5.5; y = 6.5 };
      Drop { change = Drop_complete; x = 0.; y = 0. };
    ];
  (match Source.drain native with
   | [File_drag_moved (5.5, 6.5); File_dropped name; File_drag_ended] when name = drop -> ()
   | _ -> failwith "native file drop must carry its drag position and the full path without reading it");
  if Result.is_ok (Source.push native (File_dropped "")) then
    failwith "empty file-drop path passed input validation";
  get (Source.push native (File_dropped drop));
  (match Source.drain native with
   | [File_dropped name] when name = drop -> ()
   | _ -> failwith "rejected file drop corrupted the next event");
  if Result.is_ok (Source.push native (Resized (0, 4))) then
    failwith "a zero-width resize passed input validation";
  (* the pump reports the first rejected event and still delivers the rest *)
  let batch = get (Source.create ~max_events:8 ~logical_width:4 ~logical_height:4) in
  (match Runtime_input_sdl3.push_all batch
      Sdl3.Event.[
        Drop { change = File ""; x = 0.; y = 0. };
        Text_input "after";
        Drop { change = File ""; x = 1.; y = 1. };
      ] with
   | Error "file-drop path is malformed" -> ()
   | Error message -> failwith ("pump returned another error: " ^ message)
   | Ok () -> failwith "the pump swallowed a rejected event");
  (match Source.drain batch with
   | [Text_input "after"] -> ()
   | _ -> failwith "a rejected event dropped the rest of its batch");
  let bounded = get (Source.create ~max_events:8
      ~logical_width:1 ~logical_height:1) in
  for index = 1 to 100_000 do
    get(Runtime_input_sdl3.push bounded(Sdl3.Event.Mouse_motion{
      x=float index;y=0.;dx=1.;dy=0.}))
  done;
  (* A full queue drops its oldest event: the newest eight remain. *)
  if Source.drain bounded
     <> List.init 8 (fun index -> Input.Pointer_moved (float (99_993 + index), 0.)) then
    failwith "input queue is not bounded";
  if (Source.snapshot bounded).pointer <> (100_000., 0.) then
    failwith "events dropped from a full queue did not update the pointer";
  print_endline
    "runtime input: native trace33, Retina, reset, capture, window, pinch, drop, bounded"
