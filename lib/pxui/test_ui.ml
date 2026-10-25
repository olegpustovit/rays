(* Interaction contract of the immediate-mode kit: press/drag/release,
   capture, focus loss, text entry, numeric editing, scrolling, inspector sections,
   and identical behaviour at 1x and 2x backing scales. *)
open Rays
module Ui = Pxui.Ui

let fail message = raise (Failure message)
let pointer (x, y) = float x, float y
let press point = Event.MousePressed (Input.LeftButton, pointer point)
let release point = Event.MouseReleased (Input.LeftButton, pointer point)
let move point = Event.MouseMoved (pointer point)

let frame ~scale ~time events : Frame.t =
  { width = 320; height = 240; size = 320, 240;

    pixel_scale = scale, scale; time; dt = 1. /. 60.; fps = 60.; count = 0;
    mouse = 0., 0.; mouse_delta = 0., 0.; keys = []; mouse_buttons = []; events }

(* A panel at the origin, 240 wide: rows start at y = 3 and are 24 tall; the
   value column starts at x = 120 (half the 234-point inner width) and
   toggles occupy x in [197, 237). *)
let row y = 3 + (y * 24) + 12

let with_scale scale =
  let time = ref 0. in
  let step ui events build =
    time := !time +. 0.5;
    Ui.frame ui (frame ~scale ~time:!time events) (fun ui ->
      Ui.panel ui ~x:0. ~y:0. ~width:240. "panel" (fun () -> build ui)) in
  let modified_step ui modifiers key build =
    time := !time +. 0.5;
    Ui.frame ui
      { (frame ~scale ~time:!time [Event.KeyPressed key])
        with keys = modifiers }
      (fun ui -> Ui.panel ui ~x:0. ~y:0. ~width:240. "panel"
        (fun () -> build ui)) in
  let command_step ui key build =
    modified_step ui [Input.Meta] (Input.KeyChar key) build in
  let settle ui build = ignore (step ui [] build) in
  let label = Printf.sprintf "%gx: %s" scale in

  let ui = Ui.create ~font_size:11 () in
  let roots events = Ui.frame ui (frame ~scale ~time:0.5 events) (fun ui ->
    let root = Ui.box ui ~flags:Ui.clickable ~w:(Ui.Px 100.)
      ~h:(Ui.Px 100.) "root" in
    let child = Ui.within ui root (fun () ->
      Ui.box ui ~flags:Ui.clickable ~w:(Ui.Px 30.) ~h:(Ui.Px 30.)
        ~at:(10., 10.) "child") in
    Ui.signal ui root, Ui.signal ui child, Ui.key root) in
  let _, _, root_key = roots [] in
  if Ui.last_press_within ui (frame ~scale ~time:0.5 [press (15, 15)])
      [root_key] <> Some root_key then
    fail (label "hit-tree preview missed a child press");
  let root, child, _ = roots [press (15, 15)] in
  if root.pressed || root.subtree_press <> Some 0
      || not child.pressed || child.subtree_press <> Some 0 then
    fail (label "child press did not reach its hit ancestor");
  let root, child, _ = roots [release (15, 15); press (80, 80)] in
  if not root.pressed || root.subtree_press <> Some 1
      || child.pressed then
    fail (label "blank root press did not select the root");
  Ui.destroy ui;

  (* Toggle commits on release inside, never on press alone. *)
  let ui = Ui.create ~font_size:11 () in
  let value = ref false in
  let build ui = value := Ui.toggle ui "Enabled" !value in
  settle ui build;
  step ui [press (210, row 0)] build;
  if !value then fail (label "toggle fired before release");
  step ui [release (210, row 0)] build;
  if not !value then fail (label "toggle did not react to a click");
  step ui [press (210, row 0); release (300, row 0)] build;
  if not !value then fail (label "toggle fired after release outside");
  step ui [press (237, row 0); release (236, row 0)] build;
  if not !value then fail (label "half-open bounds accepted a press on the right edge");

  (* Compact inspector sliders use the visible track's position, including
     on a simple click, and Option-click opens the numeric editor. *)
  let compact = ref "0" in
  let build ui =
    let value, _ = Ui.value_field ui ~at:(20., 20.) ~w:101. ~h:21.
      ~fraction:(Option.value ~default:0. (float_of_string_opt !compact))
      ~slide:(fun fraction -> Printf.sprintf "%.17g" fraction)
      ~valid:(fun text -> float_of_string_opt text <> None) "compact" !compact in
    compact := value in
  let ui = Ui.create ~font_size:11 () in
  ignore (Ui.frame ui (frame ~scale ~time:0. [] ) build);
  ignore (Ui.frame ui (frame ~scale ~time:0.5
    [press (70, 30); release (70, 30)]) build);
  if abs_float (Option.get (float_of_string_opt !compact) -. 0.5) > 0.02
      || Ui.text_input_focused ui then
    fail (label "compact slider click did not jump to the pointer");
  ignore (Ui.frame ui { (frame ~scale ~time:1. [press (90, 30); release (90, 30)])
    with keys = [Input.Alt] } build);
  if not (Ui.text_input_focused ui) then
    fail (label "Option-click did not open compact numeric entry");
  (* The track keeps the pointer from press to release, even outside the field; a lost window or a
     cancelled pointer ends the capture. *)
  compact := "0";
  let ui = Ui.create ~font_size:11 () in
  let tick time events = ignore (Ui.frame ui (frame ~scale ~time events) build) in
  tick 0. [];
  tick 0.5 [press (50, 30)];
  tick 1. [move (400, 30)];
  if float_of_string !compact <> 1. then fail (label "slide drag did not capture beyond the field");
  tick 1.5 [move (30, 30)];
  if float_of_string !compact >= 0.5 then fail (label "slide did not follow the captured pointer");
  tick 2. [release (30, 30)];
  tick 2.5 [press (50, 30)];
  let before = !compact in
  tick 3. [Event.WindowFocusLost; move (400, 30)];
  if !compact <> before then fail (label "window focus loss did not cancel capture");
  tick 3.5 [press (50, 30)];
  let before = !compact in
  tick 4. [Event.PointerCancelled Input.LeftButton; move (400, 30)];
  if !compact <> before then fail (label "pointer cancellation did not stop capture");

  (* Buttons fire on release inside only. *)
  let clicks = ref 0 in
  let build ui = if Ui.button ui "Apply" then incr clicks in
  let ui = Ui.create ~font_size:11 () in
  settle ui build;
  step ui [press (20, row 0); move (300, row 0); release (300, row 0)] build;
  if !clicks <> 0 then fail (label "button fired after release outside");
  step ui [press (20, row 0); release (20, row 0)] build;
  if !clicks <> 1 then fail (label "button did not fire on release inside");

  (* A collapsed inspector section builds none of its rows; a click on its header opens it. *)
  let built = ref false and opened = ref None in
  let build ui =
    built := false;
    ignore (Ui.inspector_section ui ~key:"Advanced" "Advanced" (fun () ->
      built := true; ignore (Ui.toggle ui "Nested" false)));
    opened := Ui.expanded ui "Advanced" in
  let ui = Ui.create ~font_size:11 () in
  settle ui build;
  if !built || !opened <> Some false then fail (label "a collapsed section built its rows");
  step ui [press (20, row 0); release (20, row 0)] build;
  settle ui build;
  if not !built || !opened <> Some true then fail (label "a click did not open its section");

  (* A numeric field edits as text: Enter commits, invalid text stays open, Escape cancels, a
     pasted line loses its break, the caret takes insertions and an outside press commits. *)
  let count = ref "2" in
  let build ui =
    let value, _ = Ui.value_field ui ~at:(20., 20.) ~w:101. ~h:21.
      ~valid:(fun text -> float_of_string_opt text <> None) "count" !count in
    count := value in
  let ui = Ui.create ~font_size:11 () and clock = ref 0. in
  let fstep ?(keys = []) ?(dt = 0.5) ui events build =
    clock := !clock +. dt;
    ignore (Ui.frame ui { (frame ~scale ~time:!clock events) with keys } build) in
  let ffast ui events build = fstep ~dt:0.1 ui events build in
  let fcommand ui key build =
    fstep ~keys:[Input.Meta] ui [Event.KeyPressed (Input.KeyChar key)] build in
  let open_field () =
    fstep ui [press (70, 30); release (70, 30)] build;
    ffast ui [press (70, 30); release (70, 30)] build in
  fstep ui [] build;
  open_field ();
  if not (Ui.text_input_focused ui) then fail (label "a click did not open the field");
  fstep ui [Event.TextInput "27"; Event.KeyPressed Input.Enter] build;
  if !count <> "27" || Ui.text_input_focused ui then
    fail (label ("typed number was not committed: " ^ !count));
  open_field ();
  fstep ui [Event.TextInput "x"; Event.KeyPressed Input.Enter] build;
  if !count <> "27" || not (Ui.text_input_focused ui) then
    fail (label "invalid number text was committed or dismissed");
  fstep ui [Event.KeyPressed Input.Escape; Event.TextInput "4"] build;
  if !count <> "27" || Ui.text_input_focused ui then
    fail (label "Escape did not cancel numeric editing");
  open_field ();
  let previous_clipboard = Clipboard.get_text () in
  Fun.protect ~finally:(fun () ->
    match previous_clipboard with
    | Ok text -> ignore (Clipboard.set_text text)
    | Error _ -> ()) (fun () ->
      (* a copied line comes with its break: a field of one line takes the line *)
      (match Clipboard.set_text "12\n" with Ok () -> ()
       | Error message -> fail message);
      fcommand ui 'v' build;
      fstep ui [Event.KeyPressed Input.Enter] build;
      if !count <> "12" then fail (label "numeric editor refused a pasted line for its break");
      open_field ();
      (match Clipboard.set_text "31" with Ok () -> ()
       | Error message -> fail message);
      fcommand ui 'v' build;
      fstep ui [Event.KeyPressed Input.Enter] build;
      if !count <> "31" then fail (label "numeric editor did not paste valid text"));
  open_field ();
  fstep ui [Event.KeyPressed Input.ArrowRight;
    Event.KeyPressed Input.ArrowLeft; Event.TextInput "2";
    Event.KeyPressed Input.Enter] build;
  if !count <> "321" then fail (label "numeric editor ignored its insertion caret");
  open_field ();
  fstep ui [Event.TextInput "2.5"] build;
  fstep ui [press (300, 200)] build;
  if !count <> "2.5" then fail (label "typed number was not committed by an outside press");

  (* A bounded panel scrolls by whole rows and routes hits to scrolled rows. *)
  let toggles = Array.make 4 false in
  let scrolled ui =
    Ui.panel ui ~x:0. ~y:0. ~width:240. ~max_height:80. "scroll" (fun () ->
      Array.iteri (fun index value ->
        toggles.(index) <- Ui.toggle ui (Printf.sprintf "Row %d" index) value)
        toggles) in
  let ui = Ui.create ~font_size:11 () in
  let scroll_step events =
    time := !time +. 0.5;
    Ui.frame ui (frame ~scale ~time:!time events) scrolled in
  scroll_step [];
  scroll_step [move (100, 40); Event.MouseScrolled (0., (-2.))];
  (* 102 points of rows in an 80-point panel scroll by at most 22. *)
  scroll_step [press (210, 40); release (210, 40)];
  if not toggles.(2) || toggles.(0) || toggles.(1) then
    fail (label "scrolled panel did not route the hit to the scrolled row");

  let fixed = Array.make 4 false in
  let ui = Ui.create ~font_size:11 () in
  let fixed_panel events =
    time := !time +. 0.5;
    Ui.frame ui (frame ~scale ~time:!time events) (fun ui ->
      Ui.panel ui ~x:0. ~y:0. ~width:240. ~height:80. "fixed" (fun () ->
        Array.iteri (fun index value ->
          fixed.(index) <- Ui.toggle ui (Printf.sprintf "Fixed %d" index) value)
          fixed)) in
  fixed_panel [];
  fixed_panel [move (100, 40); Event.MouseScrolled (0., -2.)];
  fixed_panel [press (210, 40); release (210, 40)];
  if not fixed.(2) || fixed.(0) || fixed.(1) then
    fail (label "fixed-height inspector panel did not scroll its rows");

  let ui = Ui.create ~font_size:11 () in
  let child_y = ref 0. in
  let rubber events =
    time := !time +. 0.5;
    let child = Ui.frame ui (frame ~scale ~time:!time events) (fun ui ->
      let parent = Ui.box ui ~flags:Ui.(scroll + clip) ~at:(0., 0.)
          ~w:(Ui.Px 100.) ~h:(Ui.Px 80.) ~scroll_step:24. "rubber" in
      Ui.within ui parent (fun () ->
        Ui.box ui ~w:(Ui.Px 100.) ~h:(Ui.Px 100.) "content")) in
    child_y := let _, y, _, _ = Ui.rect ui child in y in
  rubber [];
  rubber [move (50, 40); Event.MouseScrolled (0., 1.)];
  if !child_y <= 0. then fail (label "scroll did not stretch past the top edge");
  rubber [];
  if Float.abs !child_y > 0.25 then
    fail (label "rubber scroll did not spring back to the edge");

  let ui = Ui.create ~font_size:11 () in
  let scroll_dir events =
    time := !time +. 0.5;
    Ui.frame ui (frame ~scale ~time:!time events) (fun ui ->
      let parent = Ui.box ui ~flags:Ui.(scroll + clip)
          ~at:(0., 0.) ~w:(Ui.Px 100.) ~h:(Ui.Px 80.)
          ~scroll_step:24. "direction" in
      Ui.within ui parent (fun () ->
        ignore (Ui.box ui ~w:(Ui.Px 100.) ~h:(Ui.Px 240.) "long"));
      parent) in
  let parent = scroll_dir [] in
  Ui.set_scroll_offset ui parent 80.;
  let parent = scroll_dir [move (50, 40); Event.MouseScrolled (0., 1.)] in
  if Ui.scroll_offset ui parent <> 56. then
    fail (label "positive natural scroll did not move the view up");
  let parent = scroll_dir [Event.MouseScrolled (0., -1.)] in
  if Ui.scroll_offset ui parent <> 80. then
    fail (label "negative wheel scroll did not move the view down");

  (* A trackpad (Event.TrackpadScrolled, each motion with the wheel steps SDL makes of it): the
     fingers move the content point for point, a lift in motion coasts to rest, an edge is
     overshot and returned to, a stretch is held until the lift, and content that fits stays. *)
  let pad ?(content = 2000.) ?(start = 0.) ?(h = Ui.Px 80.) ~dt () =
    let ui = Ui.create ~font_size:11 () and clock = ref 100. in
    let step events =
      clock := !clock +. dt;
      Ui.frame ui (frame ~scale ~time:!clock events) (fun ui ->
        let parent = Ui.box ui ~flags:Ui.(scroll + clip) ~at:(0., 0.)
            ~w:(Ui.Px 100.) ~h ~scroll_step:24. "pad" in
        Ui.within ui parent (fun () ->
          ignore (Ui.box ui ~w:(Ui.Px 100.) ~h:(Ui.Px content) "long"));
        parent) in
    let parent = step [move (50, 40)] in
    Ui.set_scroll_offset ui parent start;
    ignore (step [Event.TrackpadScrolled { delta = (0., 0.); phase = Touched; time = 5. }]);
    ui, step in
  let finger time dy =
    [Event.TrackpadScrolled { delta = (0., dy); phase = Moved; time };
     Event.MouseScrolled (0., dy *. 0.1)] in
  let lifted = [Event.TrackpadScrolled { delta = (0., 0.); phase = Lifted; time = 5.04 }] in
  (* the same flick at two frame rates: 625 points a second at the lift *)
  let glide dt =
    let pad_ui, flick = pad ~dt () in
    ignore (flick (finger 5.016 (-10.)));
    let parent = flick (finger 5.032 (-10.)) in
    if Ui.scroll_offset pad_ui parent <> 20. then
      fail (label "trackpad fingers did not move the content point for point");
    let previous = ref 20. and stride = ref Float.infinity and parent = ref (flick lifted) in
    for _ = 1 to int_of_float (3. /. dt) do
      parent := flick [];
      let offset = Ui.scroll_offset pad_ui !parent in
      (* a pixel of slack: the coast is snapped to device pixels *)
      if offset < !previous || offset -. !previous > !stride +. 1. then
        fail (label "trackpad coast did not slow down monotonically");
      stride := offset -. !previous; previous := offset
    done;
    if !stride <> 0. || Ui.scroll_position pad_ui !parent <> !previous then
      fail (label "trackpad coast did not come to rest");
    !previous in
  let slow = glide (1. /. 60.) and fast = glide (1. /. 120.) in
  if slow < 150. || slow > 185. then fail (label "trackpad coast went the wrong distance");
  if Float.abs (slow -. fast) > 2. then fail (label "trackpad coast depends on the frame rate");
  (* a flick into the top edge overshoots it and returns to it exactly *)
  let pad_ui, flick = pad ~start:20. ~dt:(1. /. 60.) () in
  ignore (flick (finger 5.016 10.));
  ignore (flick (finger 5.032 10.));
  let parent = ref (flick lifted) and deepest = ref 0. in
  for _ = 1 to 120 do
    parent := flick [];
    deepest := Float.min !deepest (Ui.scroll_position pad_ui !parent)
  done;
  if !deepest > -1. || !deepest < -80. then fail (label "trackpad coast did not bounce off the edge");
  if Ui.scroll_position pad_ui !parent <> 0. || Ui.scroll_offset pad_ui !parent <> 0. then
    fail (label "trackpad bounce did not settle on the edge");
  (* a pull past the edge is held while the fingers rest and springs back when they lift *)
  let pad_ui, flick = pad ~dt:(1. /. 60.) () in
  ignore (flick (finger 5.016 200.));
  let parent = ref (flick []) in
  for _ = 1 to 60 do parent := flick [] done;
  if Ui.scroll_position pad_ui !parent >= -1. then fail (label "held trackpad stretch let go");
  parent := flick lifted;
  for _ = 1 to 60 do parent := flick [] done;
  if Ui.scroll_position pad_ui !parent <> 0. then fail (label "trackpad stretch did not spring back");
  (* a box whose height comes from its parent (a third of 240) stretches as a fixed one does *)
  let pad_ui, flick = pad ~h:(Ui.Pct (1. /. 3.)) ~dt:(1. /. 60.) () in
  ignore (flick (finger 5.016 200.));
  if Ui.scroll_position pad_ui (flick []) >= -1. then
    fail (label "a scroll box of relative height did not stretch");
  (* content shorter than the box neither scrolls, stretches nor coasts *)
  let pad_ui, flick = pad ~content:40. ~dt:(1. /. 60.) () in
  ignore (flick (finger 5.016 (-30.)));
  let parent = ref (flick (finger 5.032 30.)) in
  let still () = Ui.scroll_position pad_ui !parent = 0. in
  let ok = ref (still ()) in
  parent := flick lifted; ok := !ok && still ();
  parent := flick [Event.MouseScrolled (0., -3.)]; ok := !ok && still ();
  for _ = 1 to 30 do parent := flick []; ok := !ok && still () done;
  if not !ok then fail (label "content that fits moved under a scroll");

  (* A pinch goes where the wheel goes: to the scrollable box under the pointer,
     its factors multiplied within a frame, and nowhere else. *)
  let ui = Ui.create ~font_size:11 () in
  let pinched events =
    time := !time +. 0.5;
    let seen = ref 1. in
    ignore (Ui.frame ui (frame ~scale ~time:!time events) (fun ui ->
      let canvas = Ui.box ui ~flags:Ui.(scroll + clip) ~at:(0., 0.)
          ~w:(Ui.Px 100.) ~h:(Ui.Px 80.) "pinch" in
      seen := (Ui.signal ui canvas).pinch));
    !seen in
  ignore (pinched []);
  if pinched [move (50, 40)] <> 1. then fail (label "a frame without a pinch reported one");
  if pinched [move (50, 40); Event.MousePinched 1.25] <> 1.25 then
    fail (label "a pinch over the canvas did not reach its signal");
  if Float.abs (pinched [move (50, 40); Event.MousePinched 1.1; Event.MousePinched 0.5] -. 0.55) > 1e-9 then
    fail (label "two pinches in a frame were not multiplied");
  if pinched [move (300, 200); Event.MousePinched 1.25] <> 1. then
    fail (label "a pinch away from the canvas reached it");

  (* Text fields: UTF-8 entry, Backspace/Delete, focus kept on cancel. *)
  let title = ref "" in
  let build ui = title := Ui.text_field ui "Title" !title in
  let ui = Ui.create ~font_size:11 () in
  settle ui build;
  step ui [press (150, row 0); release (150, row 0);
    Event.TextEditing { text = "e"; start = 0; length = 1 };
    Event.TextInput "hé"; Event.KeyPressed Input.Backspace] build;
  if !title <> "h" then fail (label "text field did not accept UTF-8 input and backspace");
  let ime_cursor () = match Scene.Private.text_regions (Ui.scene ui) with
    | [(_, _, _, _, true, cursor)] -> cursor
    | _ -> fail (label "focused text field lost its IME region") in
  let cursor_after_h = ime_cursor () in
  if cursor_after_h <= 8 then fail (label "IME cursor stayed at the field origin");
  step ui [Event.PointerCancelled Input.LeftButton; Event.TextInput "!"] build;
  if !title <> "h!" then fail (label "pointer cancellation dismissed text focus");
  if ime_cursor () <= cursor_after_h then
    fail (label "IME cursor did not follow the inserted text");
  step ui [Event.TextInput " two words"; Event.KeyPressed Input.Delete;
    Event.TextInput "!"] build;
  if !title <> "h! two words!" || not (Ui.text_input_focused ui) then
    fail (label "focused text input lost spaces or changed at-end Delete");
  step ui [Event.KeyPressed Input.ArrowLeft; Event.KeyPressed Input.Backspace] build;
  if !title <> "h! two word!" then
    fail (label "Backspace ignored the moved caret");
  let cursor_before_end = ime_cursor () in
  step ui [Event.KeyPressed Input.End] build;
  if ime_cursor () <= cursor_before_end then
    fail (label "IME cursor ignored keyboard caret movement");
  let previous_clipboard = Clipboard.get_text () in
  Fun.protect ~finally:(fun () ->
    match previous_clipboard with
    | Ok text -> ignore (Clipboard.set_text text)
    | Error _ -> ()) (fun () ->
      (match Clipboard.set_text " pasted" with
       | Ok () -> () | Error message -> fail message);
      command_step ui 'v' build;
      if !title <> "h! two word! pasted" then
        fail (label "Command-V did not paste into the focused text field");
      command_step ui 'c' build;
      if Clipboard.get_text () <> Ok !title then
        fail (label "Command-C did not copy the focused text field");
      command_step ui 'x' build;
      if !title <> "" || Clipboard.get_text () <> Ok "h! two word! pasted" then
        fail (label "Command-X did not cut the focused text field");
      step ui [Event.TextInput "aé中z";
        Event.KeyPressed Input.ArrowLeft;
        Event.KeyPressed Input.ArrowLeft;
        Event.KeyPressed Input.Delete] build;
      if !title <> "aéz" then
        fail (label "Delete did not remove one UTF-8 character at the caret");
      modified_step ui [Input.Shift] Input.ArrowLeft build;
      command_step ui 'c' build;
      if Clipboard.get_text () <> Ok "é" then
        fail (label "Command-C ignored the selected UTF-8 character");
      command_step ui 'x' build;
      if !title <> "az" then fail (label "Command-X ignored selection bounds");
      command_step ui 'v' build;
      if !title <> "aéz" then fail (label "paste ignored the insertion caret"));
  step ui [press (125, row 0); release (125, row 0); Event.TextInput "!"] build;
  if !title <> "!aéz" then fail (label "mouse click did not place the text caret");
  step ui [press (300, 200)] build;
  if Ui.text_input_focused ui then fail (label "an outside press kept text focus");
  Ui.destroy ui;

  let short = ref "x" and long = ref "long text" in
  let build ui =
    short := Ui.text_field ui "Short" !short;
    long := Ui.text_field ui "Long" !long in
  let ui = Ui.create ~font_size:11 () in
  settle ui build;
  step ui [press (150, row 1); release (150, row 1)] build;
  step ui [] build;
  if !short <> "x" || !long <> "long text" then
    fail (label "unfocused short field reused the longer field caret");
  Ui.destroy ui;

  (* a scrub field (a number dragged sideways) opens for typing on a double-click *)
  let number = ref "240" and typing = ref false in
  let build ui =
    let value, open_ = Ui.value_field ui ~at:(20., 20.) ~w:100. ~h:21.
      ~scrub:(fun origin _ _ -> origin) ~valid:(fun _ -> true) "scrub-field" !number in
    number := value; typing := open_ in
  let ui = Ui.create ~font_size:11 () in
  let clock = ref 0. in
  let tick events = clock := !clock +. 0.05; ignore (Ui.frame ui (frame ~scale ~time:!clock events) build) in
  tick []; tick [move (60, 30)];
  tick [press (60, 30); release (60, 30)];
  if !typing then fail (label "one click on a scrub field opened typing");
  tick [press (60, 30); release (60, 30)]; tick [];
  if not !typing then fail (label "a double-click on a scrub field did not open typing");
  tick [Event.KeyPressed (Input.KeyChar 'a'); Event.TextInput "99"]; tick [Event.KeyPressed Input.Enter]; tick [];
  if !typing || not (String.ends_with ~suffix:"99" !number) then
    fail (label ("typing in a scrub field did not commit: " ^ !number));
  Ui.destroy ui;

  let long_value = "abcdefghijklmnopqrstuvwxyz0123456789" in
  let field = ref long_value in
  let build ui =
    let value, _ = Ui.value_field ui ~at:(20., 20.) ~w:100. ~h:21.
      ~valid:(fun _ -> true) "long-field" !field in
    field := value in
  let ui = Ui.create ~font_size:11 () in
  let field_time = ref 0. in
  let field_step ?(keys = []) events =
    field_time := !field_time +. 0.5;
    ignore (Ui.frame ui { (frame ~scale ~time:!field_time events) with keys }
      build) in
  field_step [];
  field_step [press (40, 30); release (40, 30)];
  let cursor () = match Scene.Private.text_regions (Ui.scene ui) with
    | [(_, _, _, _, true, cursor)] -> cursor
    | regions -> fail (label (Printf.sprintf "long field lost its IME region (%d)"
        (List.length regions))) in
  field_step ~keys:[Input.Meta] [Event.KeyPressed Input.ArrowLeft];
  if cursor () > 12 then fail (label "Command-Left did not reveal the start");
  field_step ~keys:[Input.Meta] [Event.KeyPressed Input.ArrowRight];
  if cursor () < 80 || cursor () > 100 then
    fail (label "Command-Right did not reveal the end");
  field_step ~keys:[Input.Meta] [Event.KeyPressed Input.ArrowLeft];
  field_step [press (21, 30)];
  field_step [move (220, 30)];
  for _ = 1 to 30 do field_step [] done;
  field_step [release (220, 30)];
  let previous_clipboard = Clipboard.get_text () in
  Fun.protect ~finally:(fun () ->
    match previous_clipboard with
    | Ok text -> ignore (Clipboard.set_text text)
    | Error _ -> ()) (fun () ->
      field_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'c')];
      if Clipboard.get_text () <> Ok long_value then
        fail (label "edge drag did not select through the end"));
  Ui.destroy ui;
  Ui.destroy ui

let run () =
  (match Sdl3.Init.init [Sdl3.Init.Video] with
   | Ok () -> () | Error error -> fail (Format.asprintf "%a" Sdl3.pp_error error));
  let ui = Ui.create ~font_size:11 () in
  Fun.protect ~finally:(fun () -> Ui.destroy ui) (fun () ->
    let build ?(key = "a") ?(skip = false) ui =
      let parent = Ui.box ui ~flags:Ui.clickable ~w:(Ui.Px 100.) ~h:(Ui.Px 100.) "hover-parent" in
      let child = Ui.within ui parent (fun () ->
        Ui.box ui ~flags:Ui.clickable ~w:(Ui.Px 40.) ~h:(Ui.Px 40.) "hover-child") in
      let ready = not skip && Ui.hovered_within ui parent && Ui.hover_delay ui ~key in
      if ready then Ui.tooltip ui ~key ~text:"A shared tooltip.";
      ready, Ui.hovered_within ui parent, (Ui.signal ui child).hovered in
    let step ?key ?skip time events = Ui.frame ui (frame ~scale:1. ~time events)
      (build ?key ?skip) in
    ignore (step 0. []);
    let ready, parent, child = step 0.1 [move (10,10)] in
    if ready || not parent || not child then fail "hover ancestry did not use the shared hit tree";
    let ready,_,_ = step 0.479 [] in
    if ready then fail "tooltip appeared before 380 ms";
    let ready,_,_ = step 0.481 [] in
    if not ready then fail "tooltip did not appear after 380 ms";
    let ready,_,_ = step 0.5 [move (11,10)] in
    if ready then fail "pointer motion did not restart hover rest";
    let ready,_,_ = step 0.881 [] in
    if not ready then fail "tooltip did not restart after movement";
    ignore (step 0.9 [Event.WindowFocusLost]);
    let ready,_,_ = step 0.91 [move (11,10)] in
    if ready then fail "focus loss kept hover rest";
    let ready,_,_ = step ~key:"b" 1.3 [] in
    if ready then fail "a different target inherited hover time";
    let ready,_,_ = step ~key:"b" 1.681 [] in
    if not ready then fail "changed target did not settle";
    ignore (step ~skip:true 1.7 []);
    let ready,_,_ = step ~key:"b" 2.1 [] in
    if ready then fail "leaving a target retained its timer";
    let ready,_,_ = step ~key:"b" 2.6 [press (11,10)] in
    if ready then fail "pointer capture displayed a tooltip";
    ignore (step 2.7 [release (11,10)]);
    let ready,_,_ = step 1. [] in
    if ready then fail "a backwards clock retained hover time";
    let ready = Ui.frame ui (frame ~scale:1. ~time:1.5 []) (fun ui ->
      ignore (Ui.modal ui "hover-modal" (fun () -> Ui.label ui "Modal"));
      Ui.hover_delay ui ~key:"a") in
    if ready then fail "a newly opened popup kept a background tooltip");
  with_scale 1.;
  with_scale 2.;
  (* Hover: the topmost control under the pointer, from last frame's rects. *)
  let ui = Ui.create ~font_size:11 () in
  let hovered = ref false in
  let build ui = Ui.panel ui ~x:0. ~y:0. ~width:240. "panel" (fun () ->
    let row = Ui.box ui ~flags:Ui.clickable ~w:Ui.Grow ~h:(Ui.Px 24.) "probe" in
    hovered := (Ui.signal ui row).hovered) in
  Ui.frame ui (frame ~scale:2. ~time:0. []) build;
  Ui.frame ui (frame ~scale:2. ~time:0.1 [move (40, 12)]) build;
  if not !hovered || not (Ui.wants_pointer ui) then fail "hover did not follow the pointer";
  Ui.frame ui (frame ~scale:2. ~time:0.2 [move (300, 200)]) build;
  if !hovered || Ui.wants_pointer ui then fail "hover outlived the pointer";
  (* text fields show the I-beam under the pointer, and only there *)
  let field_ui = Ui.create ~font_size:11 () in
  let title = ref "" in
  let field ui = title := Ui.text_field ui "Title" !title in
  Ui.frame field_ui (frame ~scale:1. ~time:0.6 []) field;
  Ui.frame field_ui (frame ~scale:1. ~time:0.7 [move (150, row 0)]) field;
  if Ui.cursor field_ui <> Some `Text then fail "a text field did not request the I-beam";
  Ui.frame field_ui (frame ~scale:1. ~time:0.8 [move (300, 200)]) field;
  if Ui.cursor field_ui <> None then fail "the I-beam outlived hover";
  let area_ui = Ui.create ~font_size:11 () in
  let area ui = ignore (Ui.text_area_submit ui ~at:(0., 0.) ~w:200. ~h:100. "area" "hello") in
  Ui.frame area_ui (frame ~scale:1. ~time:0.9 []) area;
  Ui.frame area_ui (frame ~scale:1. ~time:1.0 [move (50, 30)]) area;
  if Ui.cursor area_ui <> Some `Text then fail "a text area did not request the I-beam";
  Ui.destroy area_ui;
  Ui.destroy field_ui;
  Ui.destroy ui;
  (* Keys that come and go (150,000 of them, 500 a frame) leave the retained state the size it was. *)
  let ui = Ui.create ~font_size:11 () and next = ref 0 in
  let churn () = Ui.frame ui (frame ~scale:1. ~time:0. []) (fun ui ->
    for _ = 1 to 500 do
      incr next; ignore (Ui.box ui ~w:(Ui.Px 1.) ~h:(Ui.Px 1.) (string_of_int !next))
    done) in
  churn (); churn ();
  let words () = Obj.reachable_words (Obj.repr ui) in
  let settled = words () in
  for _ = 1 to 300 do churn () done;
  if words () > 2 * settled then fail "churned keys grew the UI's retained tables";
  Ui.destroy ui;
  (* A box with no extent of its own (Fit, its one child placed with ~at) shows the child. *)
  let ui = Ui.create ~font_size:11 () in
  let placed events = Ui.frame ui (frame ~scale:1. ~time:0. events) (fun ui ->
    let group = Ui.box ui ~w:Ui.Fit ~h:Ui.Fit "group" in
    let child = Ui.within ui group (fun () ->
      Ui.box ui ~flags:Ui.clickable ~at:(40., 40.) ~w:(Ui.Px 20.) ~h:(Ui.Px 20.) "placed") in
    (Ui.signal ui child).hovered) in
  ignore (placed []);
  if not (placed [move (50, 50)]) then fail "a Fit box of placed children culled them";
  Ui.destroy ui;
  (* One panel paints in a handful of batches, not one draw per label. *)
  let ui = Ui.create ~font_size:11 () in
  Ui.frame ui (frame ~scale:2. ~time:0. []) (fun ui ->
    Ui.panel ui "panel" (fun () ->
      Ui.label ui "PXUI";
      ignore (Ui.toggle ui "Animate" true);
      ignore (Ui.text_field ui "Caption" "Functional UI");
      ignore (Ui.button ui "Quit")));
  (match Scene.Private.stage_native ~width:320 ~height:240 (Ui.scene ui) with
   | Ok staged ->
       let batches = List.fold_left (fun total -> function
         | Scene.Private.Ui_layer (batch, _) ->
             total + Array.length (Scene_command.Ui_batch.batches batch)
         | _ -> total) 0 staged.layers in
       if batches = 0 || batches > 4 then
         fail (Printf.sprintf "panel painted in %d batches" batches)
   | Error message -> fail message);
  Ui.destroy ui;
  (* Picker: typing filters, arrows move the cursor, Enter picks an index of
     the filtered rows, Delete arms then deletes, Backspace on an empty query
     goes back, Escape cancels. *)
  let key k = Event.KeyPressed k in
  let items = [| "alpha", "1"; "beta", "2"; "gamma", "3"; "delta", "4" |] in
  let rows query = Array.of_list (List.filter
      (fun (label, _) -> Ui.fuzzy_match ~query label) (Array.to_list items)) in
  let ui = Ui.create ~font_size:11 () and query = ref "" and last = ref `None in
  let off = ref (fun _ -> false) in
  let pick ?(command=false) ?(shift=false) events =
    Ui.frame ui
      { (frame ~scale:1. ~time:0. events) with
        keys = (if command then [Input.Meta]
          else if shift then [Input.Shift] else []) } (fun ui ->
      Ui.panel ui ~x:0. ~y:0. ~width:240. "p" (fun () ->
        let edited, result = Ui.picker ui ~off:(fun index -> !off index) "Search" ~query:!query rows in
        query := edited; last := result)) in
  pick [];
  if not (Ui.text_input_focused ui) then fail "picker did not take keyboard focus";
  pick [key Input.ArrowDown; key Input.ArrowDown; key Input.Enter];
  if !last <> `Pick 2 then fail "picker arrows did not move the cursor";
  pick [Event.TextInput "dla"; key Input.Enter];
  if !query <> "dla" || !last <> `Pick 0 || fst (rows !query).(0) <> "delta" then
    fail "picker typing did not filter rows before Enter";
  let previous_clipboard = Clipboard.get_text () in
  Fun.protect ~finally:(fun () ->
    match previous_clipboard with
    | Ok text -> ignore (Clipboard.set_text text)
    | Error _ -> ()) (fun () ->
      query := "";
      (match Clipboard.set_text "del" with Ok () -> ()
       | Error message -> fail message);
      pick ~command:true [key (Input.KeyChar 'v')];
      if !query <> "del" then fail "picker did not paste the search query";
      pick ~command:true [key (Input.KeyChar 'c')];
      if Clipboard.get_text () <> Ok "del" then
        fail "picker did not copy the search query";
      pick ~command:true [key (Input.KeyChar 'x')];
      if !query <> "" then fail "picker did not cut the search query";
      query := "delta";
      pick [];
      pick [key Input.ArrowLeft];
      pick ~shift:true [key Input.ArrowLeft];
      pick ~command:true [key (Input.KeyChar 'c')];
      if Clipboard.get_text () <> Ok "t" then
        fail "picker did not copy the selected query character";
      pick ~command:true [key (Input.KeyChar 'x')];
      if !query <> "dela" then fail "picker did not cut the selection";
      pick ~command:true [key (Input.KeyChar 'v')];
      if !query <> "delta" then fail "picker pasted outside the caret");
  query := "dla";
  pick [key Input.Delete];
  if !last <> `None then fail "picker deleted on the first Delete";
  pick [key Input.Delete];
  if !last <> `Delete 0 then fail "picker did not delete on the second Delete";
  pick [Event.TextInput "zzz"; key Input.Enter];
  if !last <> `Submit then fail "picker Enter with no rows did not submit";
  query := "";
  pick [key Input.Backspace];
  if !last <> `Back then fail "picker Backspace on an empty query did not go back";
  pick [key Input.Escape];
  if !last <> `Cancel then fail "picker Escape did not cancel";
  (* The second row sits at y = 3 + 24 + 24 (search row, first row).  A row that cannot be
     picked leaves the keyboard with the search field. *)
  query := "";
  pick [];
  off := (fun index -> index = 1);
  pick [];
  pick [press (60, 3 + 48 + 12); release (60, 3 + 48 + 12)];
  pick [Event.TextInput "a"];
  if !last <> `None || !query <> "a" then fail "a click on an unavailable picker row took the keyboard";
  off := (fun _ -> false);
  query := "";
  pick [];
  pick [press (60, 3 + 48 + 12)];
  if !last <> `None then fail "picker row committed on press";
  pick [release (60, 3 + 48 + 12)];
  if !last <> `Pick 1 then fail "picker row did not commit on release inside";
  Ui.destroy ui;
  (* Context menu: rows commit on release inside, disabled rows are inert,
     a press outside dismisses; the host keeps it open on `Open. *)
  let ui = Ui.create ~font_size:11 () and result = ref `Open in
  let menu events =
    Ui.frame ui (frame ~scale:1. ~time:0. events) (fun ui ->
      result := Ui.context_menu ui ~at:(20., 20.) "ctx"
        ["One", true; "Two", false; "Three", true]) in
  let row index = 60, 20 + 3 + (index * 24) + 12 in
  menu [];
  menu [press (row 0)];
  if !result <> `Open then fail "context menu committed on press";
  menu [release (row 0)];
  if !result <> `Pick 0 then fail "context menu did not commit on release";
  menu [press (row 1); release (row 1)];
  if !result <> `Open then fail "disabled context menu row committed";
  menu [press (row 2); release (300, 200)];
  if !result <> `Open then fail "context menu committed after release outside";
  menu [press (300, 200)];
  if !result <> `Dismiss then fail "press outside did not dismiss the context menu";
  Ui.destroy ui;
  (* Text areas: the text_field edit and IME path over lines. *)
  let ui = Ui.create ~font_size:11 () in
  let submenu events = Ui.frame ui (frame ~scale:1. ~time:0. events) (fun ui ->
    Ui.context_menu ui ~at:(20., 20.) ~width:150.
      ~submenus:[Ui.{row = 0; rows = ["Draw", true; "Value", true]; keys = []; current = None}]
      "submenu" ["New graph", true; "Delete", true]) in
  ignore (submenu []);
  ignore (submenu [move (60, 38)]);
  ignore (submenu [move (190, 62)]);
  if submenu [press (190, 62)] <> `Open then fail "submenu committed on press";
  if submenu [] <> `Open then fail "submenu lost capture between frames";
  if submenu [release (190, 62)] <> `Pick 3 then fail "submenu did not commit on release";
  if submenu [press (300, 200)] <> `Dismiss then fail "submenu did not dismiss outside";
  Ui.destroy ui;
  let area = ref "" and errors = ref [] and wrapped = ref false
  and submitted = ref false and language = ref None and context = ref None and scrubs = ref []
  and scrub_edits = ref [] in
  let ui = Ui.create ~font_size:11 () and time = ref 0. in
  let selected = ref (0, 0) in
  let area_step ?(keys = []) ?reveal ?reveal_nonce ?select ?splice events =
    time := !time +. 0.5;
    ignore (Ui.frame ui { (frame ~scale:1. ~time:!time events) with keys } (fun ui ->
      let text, submit = Ui.text_area_submit ui ~at:(0., 0.) ~w:300. ~h:96.
        ?reveal ?reveal_nonce ?select ?splice ~on_select:(fun range -> selected := range)
        ~wrap:!wrapped ~errors:!errors ?language:!language ~on_context:(fun at -> context := Some at)
        ~on_scrub:(fun p -> scrubs := p :: !scrubs)
        ~on_scrub_edit:(fun range value -> scrub_edits := (range, value) :: !scrub_edits) "area" !area in
      area := text; submitted := submit)) in
  let region () = match Scene.Private.text_regions (Ui.scene ui) with
    | [(_, y, _, _, true, cursor)] -> y, cursor
    | _ -> fail "text area lost its IME region" in
  let expect what want = if !area <> want then
    fail (Printf.sprintf "text area %s: %S" what !area) in
  area_step [];
  area_step [press (150, 12); release (150, 12); Event.TextInput "ab"; Event.KeyPressed Input.Enter;
    Event.TextInput "cd"];
  expect "insert and Enter" "ab\ncd";
  area_step [Event.KeyPressed Input.ArrowUp; Event.TextInput "X"];
  expect "Up keeps the column" "abX\ncd";
  area_step [Event.KeyPressed Input.Home; Event.TextInput "_"];
  expect "Home is line-scoped" "_abX\ncd";
  area_step [Event.KeyPressed Input.ArrowDown; Event.KeyPressed Input.End; Event.KeyPressed Input.Backspace];
  expect "Down then End then Backspace" "_abX\nc";
  area_step [Event.KeyPressed Input.Delete];
  expect "Delete at the end" "_abX\nc";
  let cursor = snd (region ()) in
  area_step [Event.TextEditing { text = "e"; start = 0; length = 1 }];
  if snd (region ()) <> cursor then fail "composition moved the caret";
  area_step [Event.TextInput "é"];
  expect "IME commit" "_abX\ncé";
  let previous_clipboard = Clipboard.get_text () in
  Fun.protect ~finally:(fun () -> match previous_clipboard with
    | Ok text -> ignore (Clipboard.set_text text) | Error _ -> ()) (fun () ->
    area_step ~keys:[Input.Shift] [Event.KeyPressed Input.ArrowLeft];
    area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'c')];
    if Clipboard.get_text () <> Ok "é" then fail "text area copy ignored the selection";
    area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'a')];
    area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'x')];
    expect "cut all" "";
    if Clipboard.get_text () <> Ok "_abX\ncé" then fail "text area cut lost the text";
    area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'v')];
    expect "paste keeps the newline" "_abX\ncé";
    (* copy and cut with no selection take the caret's line: of an empty area, nothing *)
    area := "";
    area_step [press (150, 12); release (150, 12)];
    area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'c')];
    area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'x')];
    expect "copy and cut of an empty area" "";
    area := "ab\ncd";
    area_step [press (150, 12); release (150, 12)];
    area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'x')];
    expect "cut with no selection takes the caret's line" "cd");
  (* Tab keeps the focus and inserts two spaces, Shift-Tab takes them off, Command-Enter is the
     host's apply *)
  area := "ab";
  area_step [press (150, 12); release (150, 12)];
  area_step [Event.KeyPressed Input.Tab];
  expect "Tab" "ab  ";
  if not (Ui.text_input_focused ui) then fail "Tab moved the focus out of the text area";
  area_step [Event.KeyPressed Input.Home; Event.KeyPressed Input.Tab];
  expect "Tab at the start of the line" "  ab  ";
  area_step ~keys:[Input.Shift] [Event.KeyPressed Input.Tab];
  expect "Shift-Tab takes two spaces" "ab  ";
  area_step [Event.TextInput "x"];
  area_step ~keys:[Input.Shift] [Event.KeyPressed Input.Tab];
  expect "Shift-Tab on an unindented line" "xab  ";
  if !submitted then fail "an edit submitted the text area";
  area_step ~keys:[Input.Meta] [Event.KeyPressed Input.Enter];
  if not !submitted then fail "Command-Enter did not report an apply";
  expect "Command-Enter inserts nothing" "xab  ";
  area_step ~keys:[Input.Ctrl] [Event.KeyPressed Input.Enter];
  if not !submitted then fail "Control-Enter did not report an apply";
  (* soft wrapping: one long line is several rows, the caret and Up/Down follow the rows, and
     nothing scrolls sideways *)
  area := String.make 100 'a';
  wrapped := true;
  area_step [press (150, 60); release (150, 60)];
  area_step [Event.TextInput "X"];
  let x_at = String.index !area 'X' in
  if x_at < 60 || x_at > 99 then fail (Printf.sprintf "a click on the third row landed at column %d" x_at);
  (* a text line is one and a half times the 11-point text: 17 points *)
  let y_third, _ = region () in
  if y_third < 2 * 17 then fail (Printf.sprintf "a wrapped caret is on the first row (y %d)" y_third);
  area_step [Event.KeyPressed Input.ArrowUp; Event.TextInput "Y"];
  let y_at = String.index !area 'Y' in
  if y_at >= x_at || x_at - y_at > 60 then
    fail (Printf.sprintf "Up did not move to the row above (%d then %d)" x_at y_at);
  area_step [Event.KeyPressed Input.Home; Event.TextInput "H"];
  let h_at = String.index !area 'H' in
  if h_at = 0 || h_at > y_at then fail "Home on a wrapped row was not row-scoped";
  area_step [Event.KeyPressed Input.ArrowUp; Event.KeyPressed Input.ArrowUp; Event.KeyPressed Input.ArrowUp];
  area_step [Event.TextInput "T"];
  if String.index !area 'T' > 40 then fail "Up at the first row did not go to the start";
  wrapped := false;
  area_step [];
  (* scrolling: the caret line stays inside the 96-point area *)
  area := String.concat "\n" (List.init 40 string_of_int);
  errors := [ 3; 40 ];
  area_step [press (150, 12); release (150, 12)];
  area_step ~keys:[Input.Meta] [Event.KeyPressed Input.ArrowDown];
  for _ = 1 to 39 do area_step [Event.KeyPressed Input.ArrowDown] done;
  area_step [];
  let y, _ = region () in
  if y < 0 || y > 96 then fail (Printf.sprintf "caret line left the area (y %d)" y);
  area_step [Event.KeyPressed Input.Escape];
  if Ui.text_input_focused ui then fail "Escape did not leave the text area";
  (* a language: brackets in pairs, Enter indents, right-click reports the point *)
  errors := [];
  language := Some { Ui.colorize = (fun _ -> []); brackets = (fun _ -> []);
    indent = (fun text caret -> if String.contains (String.sub text 0 caret) '(' then "  " else "");
    pairs = [ '(', ')'; '"', '"' ]; complete = (fun _ _ -> []); describe = (fun _ _ -> None);
    number_at = (fun _ _ -> None);
    context_at = (fun text caret -> (* an odd number of quotes before the caret: inside a string *)
      let quotes = ref 0 in
      String.iteri (fun i c -> if i < caret && c = '"' then incr quotes) text;
      if !quotes mod 2 = 1 then `String else `Code);
    comment = ";"; rewrite = None };
  area := "";
  area_step [press (150, 12); release (150, 12); Event.TextInput "("];
  expect "an opener inserts its pair" "()";
  area_step [Event.TextInput "a"; Event.TextInput ")"];
  expect "a closer typed before itself steps over it" "(a)";
  area_step [Event.KeyPressed Input.ArrowLeft; Event.KeyPressed Input.Enter];
  expect "Enter indents as the language says" "(a\n  )";
  area_step [Event.TextInput "\""; Event.TextInput "\""];
  expect "a quote pairs, then steps over its twin" "(a\n  \"\")";
  area_step [Event.KeyPressed Input.ArrowLeft; Event.KeyPressed Input.Backspace];
  expect "Backspace between an empty pair takes both" "(a\n  )";
  area_step ~keys:[Input.Shift] [Event.KeyPressed Input.Home];
  area_step [Event.TextInput "("];
  expect "an opener wraps the selection" "(a\n(  ))";
  area_step [Event.MousePressed (Input.RightButton, (100., 20.)); Event.MouseReleased (Input.RightButton, (100., 20.))];
  if !context = None then fail "a right-click did not reach on_context";
  (* completions: typing opens the popup, Down moves, Tab accepts; Escape closes it before it
     leaves the editor *)
  let digit c = (c >= '0' && c <= '9') || c = '.' in
  language := Some { (Option.get !language) with
    complete = (fun text caret ->
      let rec back i = if i > 0 && text.[i - 1] <> ' ' && text.[i - 1] <> '(' then back (i - 1) else i in
      let start = back caret in
      let prefix = String.sub text start (caret - start) in
      if prefix = "" then [] else
        List.filter_map (fun w -> if String.starts_with ~prefix w
          then Some { Ui.replace = (start, caret); insert = w; label = w; detail = "word"; doc = w } else None)
          [ "zebra"; "zephyr"; "zoo" ]);
    number_at = (fun text byte ->
      if byte < String.length text && digit text.[byte] then
        let rec a i = if i > 0 && digit text.[i - 1] then a (i - 1) else i
        and b i = if i < String.length text && digit text.[i] then b (i + 1) else i in
        Some (a byte, b byte)
      else None) };
  area := "";
  area_step [press (150, 12); release (150, 12); Event.TextInput "z"; Event.TextInput "e"];
  expect "typing opens the popup without changing the text" "ze";
  area_step [Event.KeyPressed Input.ArrowDown];
  expect "Down moves in the popup, not in the text" "ze";
  area_step [Event.KeyPressed Input.Tab];
  expect "Tab accepts the chosen completion" "zephyr";
  area_step [Event.TextInput " "; Event.TextInput "z"];
  area_step [Event.KeyPressed Input.Escape];
  if not (Ui.text_input_focused ui) then fail "Escape closed the editor instead of the popup";
  area_step [Event.KeyPressed Input.Escape];
  if Ui.text_input_focused ui then fail "a second Escape did not leave the editor";
  (* Enter breaks the line unless Up or Down chose a row; Tab always accepts *)
  let saved = !language in
  area := "";
  area_step [press (150, 12); release (150, 12); Event.TextInput "z"; Event.TextInput "e"];
  area_step [Event.KeyPressed Input.Enter];
  expect "Enter after a typed word breaks the line, it does not take the popup" "ze\n";
  area_step [Event.TextInput " "; Event.TextInput "z"; Event.TextInput "e"];
  area_step [Event.KeyPressed Input.ArrowDown];
  area_step [Event.KeyPressed Input.Enter];
  expect "Enter takes the row an arrow chose" "ze\n zephyr";
  area_step [Event.KeyPressed Input.Escape; Event.KeyPressed Input.Escape];
  (* Tab and Shift-Tab with a selection indent and dedent every line it touches *)
  language := Some { (Option.get !language) with complete = (fun _ _ -> []) };
  area := "a\n\nb\nc";
  area_step [press (150, 12); release (150, 12)];
  area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'a')];
  area_step [Event.KeyPressed Input.Tab];
  expect "Tab with a selection indents every line and keeps the selection" "  a\n\n  b\n  c";
  if fst !selected <> 0 || snd !selected <> String.length !area then fail "the indented lines lost their selection";
  area_step ~keys:[Input.Shift] [Event.KeyPressed Input.Tab];
  expect "Shift-Tab with a selection dedents every line" "a\n\nb\nc";
  area_step [Event.KeyPressed Input.ArrowDown];
  area_step [Event.KeyPressed Input.Tab];
  expect "Tab without a selection still inserts two spaces" "a\n\nb\nc  ";
  (* brackets and quotes pair and step over only in code *)
  area := "";
  area_step [press (150, 12); release (150, 12); Event.TextInput "\""; Event.TextInput "("];
  expect "a bracket inside a string is typed as it is" "\"(\"";
  area_step [Event.TextInput "\""];
  expect "a quote before the closing quote steps over it" "\"(\"";
  area := "\"x\"";
  area_step [];
  area_step ~keys:[Input.Meta] [Event.KeyPressed Input.ArrowUp];
  area_step [Event.TextInput "\""];
  expect "a quote before a string's opening quote does not step over it" "\"\"\"x\"";
  (* Up and Down keep their column across a short line *)
  area := "abcdef\nab\nabcdef";
  area_step [];
  area_step ~keys:[Input.Meta] [Event.KeyPressed Input.ArrowUp];
  area_step (List.init 5 (fun _ -> Event.KeyPressed Input.ArrowRight));
  area_step [Event.KeyPressed Input.ArrowDown];
  area_step [Event.KeyPressed Input.ArrowDown];
  area_step [Event.TextInput "X"];
  expect "Down across a short line returns to the column it left" "abcdef\nab\nabcdeXf";
  (* [select] takes the keyboard and places the selection; [splice] is one undo step *)
  language := None;
  area := "one two one";
  Ui.unfocus ui;
  area_step ~select:(4, 7) [];
  if not (Ui.text_input_focused ui) then fail "a select request did not focus the area";
  area_step [Event.TextInput "2"];
  expect "typing replaces the requested selection" "one 2 one";
  area := "one two one";
  area_step ~splice:[ 0, 3, "1"; 8, 11, "111" ] ~select:(1, 1) [];
  expect "a splice replaces every range" "1 two 111";
  area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'z')];
  expect "Command-Z takes the whole splice back" "one two one";
  (* [reveal] fires again for a new nonce, not for the same one after the user scrolled away *)
  area := String.concat "\n" (List.init 40 string_of_int);
  let visible () = let y, _ = region () in y >= 0 && y < 96 in
  let back_to_end ?reveal ?reveal_nonce () =
    area_step ?reveal ?reveal_nonce ~keys:[Input.Meta] [Event.KeyPressed Input.ArrowUp];
    area_step ?reveal ?reveal_nonce ~keys:[Input.Meta] [Event.KeyPressed Input.ArrowDown] in
  area_step [press (150, 12); release (150, 12)];
  back_to_end ();
  let off = String.length (String.concat "\n" (List.init 30 string_of_int)) in
  if not (visible ()) then fail "the caret at the end was not in view";
  area_step ~reveal:off [];
  if visible () then fail "reveal did not scroll the offset into view";
  back_to_end ~reveal:off ();
  area_step ~reveal:off [];
  if not (visible ()) then fail "reveal fired twice for one request";
  area_step ~reveal:off ~reveal_nonce:1 [];
  if visible () then fail "reveal did not fire again for a new nonce";
  back_to_end ~reveal:off ~reveal_nonce:1 ();
  area_step [];
  if not (visible ()) then fail "the caret did not return";
  area_step ~reveal:off ~reveal_nonce:1 [];
  if visible () then fail "reveal did not fire again after an absent reveal";
  (* line and form edits: comment toggle, move and duplicate lines, enclosing form, bracket jump *)
  let pairs_of text =
    let stack = ref [] and out = ref [] in
    String.iteri (fun i c -> if c = '(' then stack := i :: !stack
      else if c = ')' then match !stack with o :: r -> out := (o, i) :: !out; stack := r | [] -> ()) text;
    List.rev !out in
  language := Some { (Option.get saved) with brackets = pairs_of; complete = (fun _ _ -> []) };
  let alt = [Input.Alt] and alt_shift = [Input.Alt; Input.Shift] in
  area := "a\n\n  b\nc";
  area_step [press (150, 12); release (150, 12)];
  area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'a')];
  area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar '/')];
  expect "Command-/ comments every non-blank line at the smallest indent" "; a\n\n;   b\n; c";
  area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar '/')];
  expect "Command-/ again takes the comments off" "a\n\n  b\nc";
  area := "x\n y";
  area_step [];
  area_step ~keys:[Input.Meta] [Event.KeyPressed Input.ArrowUp];
  area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar '/')];
  area_step [Event.TextInput "Q"];
  expect "the caret keeps its place in a commented line" "; Qx\n y";
  area := "one\ntwo\nthree";
  area_step [];
  area_step ~keys:[Input.Meta] [Event.KeyPressed Input.ArrowUp];
  area_step ~keys:[Input.Meta] [Event.KeyPressed Input.ArrowDown];
  area_step ~keys:[Input.Meta] [Event.KeyPressed Input.ArrowUp];
  area_step [Event.KeyPressed Input.End];
  area_step ~keys:alt [Event.KeyPressed Input.ArrowUp];
  expect "Option-Up on the first line does nothing" "one\ntwo\nthree";
  area_step ~keys:alt [Event.KeyPressed Input.ArrowDown];
  expect "Option-Down moves the line down" "two\none\nthree";
  area_step [Event.TextInput "X"];
  expect "the caret travels with the moved line" "two\noneX\nthree";
  area_step ~keys:alt [Event.KeyPressed Input.ArrowDown];
  area_step ~keys:alt [Event.KeyPressed Input.ArrowDown];
  expect "Option-Down on the last line does nothing" "two\nthree\noneX";
  area_step ~keys:alt [Event.KeyPressed Input.ArrowUp];
  expect "Option-Up moves the line up" "two\noneX\nthree";
  area := "ab";
  area_step [];
  area_step ~keys:[Input.Meta] [Event.KeyPressed Input.ArrowDown];
  area_step ~keys:alt_shift [Event.KeyPressed Input.ArrowDown];
  area_step [Event.TextInput "Z"];
  expect "Shift-Option-Down copies the line and keeps the caret on the copy" "ab\nabZ";
  area := "ab";
  area_step [];
  area_step ~keys:[Input.Meta] [Event.KeyPressed Input.ArrowDown];
  area_step ~keys:alt_shift [Event.KeyPressed Input.ArrowUp];
  area_step [Event.TextInput "Z"];
  expect "Shift-Option-Up copies the line above and keeps the caret on the original" "abZ\nab";
  area := "(a (b c) d)";
  area_step [];
  area_step [Event.KeyPressed Input.Home];
  area_step (List.init 5 (fun _ -> Event.KeyPressed Input.ArrowRight));
  area_step ~keys:[Input.Meta; Input.Alt] [Event.KeyPressed Input.ArrowUp];
  if !selected <> (3, 8) then fail "Command-Option-Up did not select the innermost form";
  area_step ~keys:[Input.Meta; Input.Alt] [Event.KeyPressed Input.ArrowUp];
  if !selected <> (0, 11) then fail "a second Command-Option-Up did not widen to the next form";
  area_step [Event.KeyPressed Input.Home];
  area_step (List.init 5 (fun _ -> Event.KeyPressed Input.ArrowRight));
  area_step ~keys:[Input.Meta; Input.Shift] [Event.KeyPressed (Input.KeyChar '\\')];
  if !selected <> (3, 3) then fail (Printf.sprintf "the bracket jump did not go to the opener of the pair around the caret %d %d" (fst !selected) (snd !selected));
  area_step ~keys:[Input.Meta; Input.Shift] [Event.KeyPressed (Input.KeyChar '\\')];
  if !selected <> (8, 8) then fail "the bracket jump did not cross to the far side of the pair";
  (* a text that comes back unchanged takes its own undo steps back *)
  area := "foo";
  area_step [press (150, 12); release (150, 12)];
  area_step [Event.TextInput "x"];
  expect "typing" "foox";
  area := "other";
  area_step [];
  area := "foox";
  area_step [];
  area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'z')];
  expect "Command-Z after the text returned undoes the typing" "foo";
  language := saved;
  (* a number dragged sideways follows the pointer: an integer one per five points, a float a
     tenth of its last place per point; the host hears Live frames and one Done *)
  area := "";
  area_step [press (150, 12); release (150, 12)];
  let origin = snd (region ()) in
  area := "10 x";
  area_step [press (150, 12); release (150, 12)];
  let char_w = (snd (region ()) - origin) / 4 in
  if char_w <= 0 then fail "no glyph width to aim a drag with";
  let nx = origin + (char_w / 2) in
  scrubs := [];
  area_step [press (nx, 18)];
  area_step [move (nx + 10, 18)];
  area_step [move (nx + 20, 18)];
  expect "a dragged integer moves one per five points" "14 x";
  if List.hd !scrub_edits <> ((0, 2), "14") then fail "a numeric drag lost its pre-edit token span";
  if not (List.mem `Live !scrubs) then fail "a drag did not report on_scrub `Live";
  area_step [release (nx + 20, 18)];
  if List.hd !scrubs <> `Done then fail "a drag's end did not report on_scrub `Done";
  expect "the release keeps the value" "14 x";
  area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'z')];
  expect "Command-Z takes the whole drag back" "10 x";
  area := "0.5 y";
  area_step [];
  area_step [press (nx, 18)];
  area_step [move (nx + 10, 18)];
  expect "a dragged float moves a tenth of its last place per point" "0.6 y";
  area_step [release (nx + 10, 18)];
  area_step [press (nx, 18)];
  area_step [move (nx, 32)];
  area_step [release (nx, 32)];
  expect "a vertical drag is a selection, not a scrub" "0.6 y";
  (* only the number's underline (the low part of its row) is a handle: a sideways drag that
     starts higher selects *)
  area_step [press (nx, 8)];
  area_step [move (nx + 20, 8)];
  area_step [release (nx + 20, 8)];
  expect "a sideways drag from above the underline selects, it does not scrub" "0.6 y";
  (* the language's rewrite runs after each edited frame and maps the caret *)
  language := Some { (Option.get !language) with rewrite = Some (fun text caret -> text ^ "!", caret) };
  area := "";
  area_step [press (150, 12); release (150, 12); Event.TextInput "a"];
  expect "the rewrite keeps the text as the language wants it" "a!";
  area_step [Event.TextInput "b"];
  expect "the caret stays where the rewrite put it" "ab!!";
  language := None;
  (* macOS editing: a double click selects the word and a triple the line, Option and
     Command keys move and delete by words and lines, Command-Z undoes inside the area *)
  area := "alpha beta\ngamma delta";
  (* a click past the end of the first row lands after "beta" *)
  let at = 290, 12 in
  area_step [press at; release at; press at; release at];
  area_step [Event.TextInput "X"];
  expect "a double click selects the word" "alpha X\ngamma delta";
  area_step [press at; release at; press at; release at; press at; release at];
  area_step [Event.TextInput "L"];
  expect "a triple click selects the line with its break" "Lgamma delta";
  area := "one two three";
  area_step [press (150, 12); release (150, 12)];
  area_step ~keys:[Input.Meta] [Event.KeyPressed Input.ArrowDown];
  area_step ~keys:[Input.Alt] [Event.KeyPressed Input.ArrowLeft];
  area_step [Event.TextInput "X"];
  expect "Option-Left moves to the start of the word" "one two Xthree";
  area_step ~keys:[Input.Alt] [Event.KeyPressed Input.Backspace];
  expect "Option-Backspace takes the word before the caret" "one two three";
  area_step ~keys:[Input.Alt; Input.Shift] [Event.KeyPressed Input.ArrowLeft];
  area_step [Event.TextInput "2"];
  expect "Shift-Option-Left extends the selection by a word" "one 2three";
  area_step ~keys:[Input.Meta] [Event.KeyPressed Input.Backspace];
  expect "Command-Backspace takes the line to the caret" "three";
  area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'z')];
  expect "Command-Z undoes the last change" "one 2three";
  area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'z')];
  area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'z')];
  expect "Command-Z steps back through the changes" "one two Xthree";
  area_step ~keys:[Input.Meta; Input.Shift] [Event.KeyPressed (Input.KeyChar 'z')];
  expect "Shift-Command-Z redoes" "one two three";
  area := "";
  area_step [press (150, 12); release (150, 12)];
  area_step [Event.TextInput "a"];
  area_step [Event.TextInput "b"];
  area_step [Event.TextInput "c"];
  area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'z')];
  expect "consecutive typing undoes as one step" "";
  area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'y')];
  expect "Command-Y redoes the typing" "abc";
  if Ui.passed_undo ui <> None then fail "a redo the area took was passed on";
  area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'y')];
  if Ui.passed_undo ui <> Some `Redo then fail "a redo with an empty stack was not passed on";
  area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'z')];
  area_step ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'z')];
  if Ui.passed_undo ui <> Some `Undo then fail "an undo with an empty stack was not passed on";
  expect "the passed undo leaves the text alone" "";
  area_step [];
  if Ui.passed_undo ui <> None then fail "the passed undo outlived its frame";
  (* a row label cut short of its right-hand detail: whole when it fits, else an ellipsis within the
     limit, never a split multi-byte character *)
  let cp s = 7. *. float (String.fold_left (fun n c -> if Char.code c land 0xC0 = 0x80 then n else n + 1) 0 s) in
  if Ui.ellipsis ~width:cp ~limit:200. "split side by side" <> "split side by side" then fail "a fitting label was cut";
  if Ui.ellipsis ~width:cp ~limit:70. "split side by side" <> "split sid…" then fail "a long label was not cut to the limit";
  if Ui.ellipsis ~width:cp ~limit:28. "ƒ petal" <> "ƒ p…" then fail "the cut split a multi-byte character";
  if Ui.ellipsis ~width:cp ~limit:7. "petal" <> "…" || Ui.ellipsis ~width:cp ~limit:6. "petal" <> "" then
    fail "a label with no room was not the ellipsis alone, then nothing";
  (* the cut measures what it shows, not the whole label *)
  let measured = ref 0 in
  let counted s = measured := !measured + String.length s; cp s in
  if Ui.ellipsis ~width:counted ~limit:70. (String.make 4000 'a') <> String.make 9 'a' ^ "…" || !measured > 100 then
    fail "the cut of a long label measured all of it";
  Ui.destroy ui;
  (* Modal: centered, and Escape or a press outside dismisses it. *)
  let ui = Ui.create ~font_size:11 () and shown = ref None in
  let modal events = Ui.frame ui (frame ~scale:1. ~time:0. events) (fun ui ->
    shown := Ui.modal ui ~width:200. "modal" (fun () -> Ui.label ui "Hello")) in
  modal [];
  modal [];
  if !shown <> Some () then fail "modal did not build its content";
  modal [press (160, 120); release (160, 120)];
  if !shown <> Some () then fail "a press inside dismissed the modal";
  modal [key Input.Escape];
  if !shown <> None then fail "Escape did not dismiss the modal";
  modal [];
  modal [press (5, 5)];
  if !shown <> None then fail "a press outside did not dismiss the modal";
  Ui.destroy ui;
  let ui = Ui.create ~font_size:11 () and shown = ref None in
  let popup events = Ui.frame ui (frame ~scale:1. ~time:0. events) (fun ui ->
    shown := Ui.popup ui ~at:(20., 20.) ~width:200. ~height:10. "popup"
      (fun () -> Ui.label ui "One"; Ui.label ui "Two")) in
  popup [];
  popup [];
  popup [press (50, 58)];
  if !shown <> Some () then fail "popup dismissed a press inside its laid-out rect";
  popup [press (300, 200)];
  if !shown <> None then fail "popup kept a press outside its laid-out rect";
  Ui.destroy ui;
  let ui = Ui.create ~font_size:11 () in
  let menu events = Ui.frame ui (frame ~scale:1. ~time:0. events) (fun ui ->
    Ui.context_menu ui ~at:(20., 20.) ~width:240. ~selected:1 "choices"
      ["First", true; "Second", true]) in
  ignore (menu []);
  ignore (menu []);
  if menu [press (240, 58); release (240, 58)] <> `Pick 1 then
    fail "dropdown rows did not extend to the requested control width";
  Ui.destroy ui;
  let ui = Ui.create ~font_size:11 () in
  let front_key = ref 0 in
  let raised events = Ui.frame ui (frame ~scale:1. ~time:0. events) (fun ui ->
    let front = Ui.box ui ~flags:Ui.clickable ~at:(0., 0.) ~w:(Ui.Px 100.) ~h:(Ui.Px 60.) "front" in
    front_key := Ui.key front;
    Ui.to_front ui front;
    let back = Ui.box ui ~flags:Ui.clickable ~at:(0., 0.) ~w:(Ui.Px 100.) ~h:(Ui.Px 60.) "back" in
    List.iter (fun box -> Ui.draw ui box (fun paint (x, y, w, h) ->
      Ui.Paint.fill paint ~x ~y ~w ~h Color.white)) [front; back];
    (Ui.signal ui front).clicked, (Ui.signal ui back).clicked) in
  ignore (raised []);
  if raised [press (20, 20); release (20, 20)] <> (true, false) then
    fail "a later body covered a raised floating control";
  let scene = Ui.scene ui ~under:(fun key -> if key = !front_key then
    [Scene.rect ~at:(0, 0) ~w:100 ~h:60 ~fill:Color.black ()] else []) in
  (match Scene.Private.stage_native ~width:320 ~height:240 scene with
   | Ok {layers = [Ui_layer _; Scene2_layer _; Ui_layer _]; _} -> ()
   | _ -> fail "native content was not between the background and floating controls");
  Ui.destroy ui;
  (* Two text widgets share nothing: a double click's word in one and its undo stack do not
     reach the numeric editor opened after it (a drag there stays inside its own text, and
     Command-Z with nothing typed is the host's). *)
  let ui = Ui.create ~font_size:11 () in
  let words = ref "alpha beta gamma" and number = ref "240" and clock = ref 0. in
  let tick ?(keys = []) ?(dt = 0.05) events =
    clock := !clock +. dt;
    ignore (Ui.frame ui { (frame ~scale:1. ~time:!clock events) with keys } (fun ui ->
      words := fst (Ui.value_field ui ~at:(20., 20.) ~w:100. ~h:21. ~valid:(fun _ -> true) "words" !words);
      number := fst (Ui.value_field ui ~at:(20., 60.) ~w:100. ~h:21. ~scrub:(fun origin _ _ -> origin)
        ~valid:(fun text -> int_of_string_opt text <> None) "number" !number))) in
  tick []; tick [press (100, 30); release (100, 30)];
  tick ~dt:1. [press (100, 30); release (100, 30); press (100, 30)]; tick [release (100, 30)];
  tick [Event.TextInput "X"];
  tick ~dt:1. [press (60, 70); release (60, 70); press (60, 70)];
  tick [move (61, 70)]; tick [release (61, 70)];
  if not (Ui.text_input_focused ui) then fail "a double click did not open the numeric editor";
  tick ~keys:[Input.Meta] [Event.KeyPressed (Input.KeyChar 'z')];
  if Ui.passed_undo ui <> Some `Undo then fail "a numeric editor undid another widget's edit";
  tick [Event.KeyPressed Input.Escape];
  if !number <> "240" then fail ("a numeric editor took another widget's text: " ^ !number);
  Ui.destroy ui;
  (* A carry: held once the pointer has left the 4-point dead zone of the press, seen as hover
     by a box that does not own the press, put once on release, and gone after a pointer
     cancellation or a focus loss.  A key starts one with no capture: a press is the put. *)
  let ui = Ui.create ~font_size:11 () in
  let clicked = ref false and popup = ref false in
  let carried events = Ui.frame ui (frame ~scale:1. ~time:0. events) (fun ui ->
    if !popup then ignore (Ui.popup ui ~at:(200., 0.) ~width:100. ~height:40. "over" (fun () -> ()));
    let source = Ui.box ui ~flags:Ui.clickable ~at:(0., 0.) ~w:(Ui.Px 100.) ~h:(Ui.Px 40.) "source" in
    let target = Ui.box ui ~flags:Ui.clickable ~at:(0., 100.) ~w:(Ui.Px 100.) ~h:(Ui.Px 40.) "target" in
    if (Ui.signal ui source).held then
      Ui.carry ui ~from:source ~kind:"material" ~value:"(ref cobalt)" ();
    clicked := !clicked || (Ui.signal ui target).clicked;
    (Ui.carrying ui, Ui.drop_target ui target, (Ui.signal ui target).hovered)) in
  let payload = { Ui.kind = "material"; value = "(ref cobalt)" } in
  ignore (carried []); ignore (carried []);
  (match carried [ press (10, 10) ] with
   | None, None, false -> () | _ -> fail "a press alone started a carry");
  (match carried [ move (12, 12) ] with
   | None, None, false -> () | _ -> fail "a carry started inside the dead zone");
  (match carried [ move (10, 120) ] with
   | Some held, Some (Ui.Hover over), true when held = payload && over = payload -> ()
   | _ -> fail "a box that does not own the press did not see the carry hover");
  (match carried [ move (10, 110) ] with
   | Some _, Some (Ui.Hover _), _ -> () | _ -> fail "the carry did not follow the pointer");
  (match carried [ release (10, 110) ] with
   | None, Some (Ui.Dropped dropped), _ when dropped = payload -> ()
   | _ -> fail "a release over a target did not deliver the payload");
  (match carried [] with
   | None, None, _ -> () | _ -> fail "the put was delivered twice");
  if !clicked then fail "the release of a carry clicked the target";
  (* a release over nothing delivers nothing, and the payload is gone *)
  ignore (carried [ press (10, 10) ]); ignore (carried [ move (10, 120) ]);
  (match carried [ move (200, 200); release (200, 200) ] with
   | None, None, _ -> () | _ -> fail "a release over nothing delivered a payload");
  (* cancellation and focus loss clear it, delivering nothing *)
  ignore (carried [ press (10, 10) ]); ignore (carried [ move (10, 120) ]);
  (match carried [ Event.PointerCancelled Input.LeftButton ] with
   | None, None, _ -> () | _ -> fail "a pointer cancellation left the payload held");
  (match carried [ release (10, 120) ] with
   | None, None, _ -> () | _ -> fail "a cancelled carry was put on release");
  ignore (carried [ press (10, 10) ]); ignore (carried [ move (10, 120) ]);
  (match carried [ Event.WindowFocusLost ] with
   | None, None, _ -> () | _ -> fail "a focus loss left the payload held");
  ignore (carried [ release (10, 120) ]);
  (* a popup that takes the capture takes the carry: the release after it puts nothing *)
  ignore (carried [ press (10, 10) ]); ignore (carried [ move (10, 120) ]);
  popup := true; ignore (carried []); popup := false;
  (match carried [ release (10, 120) ] with
   | None, None, _ -> () | _ -> fail "a carry outlived the capture a popup took");
  (* from a key: no capture, hover from the pointer, the next left press is the put *)
  Ui.carry ui ~kind:"sop" ~value:"(ref shards)" ();
  if Ui.carrying ui = None then fail "a key did not start a carry";
  (match carried [ move (10, 120) ] with
   | Some _, Some (Ui.Hover { kind = "sop"; _ }), true -> ()
   | _ -> fail "a key carry was not seen hovering");
  (match carried [ press (10, 120) ] with
   | None, Some (Ui.Dropped { value = "(ref shards)"; _ }), _ -> ()
   | _ -> fail "a press did not put a key carry");
  ignore (carried [ release (10, 120) ]);
  if !clicked then fail "the press that put a key carry also clicked";
  (* the host cancels (Escape) *)
  Ui.carry ui ~kind:"sop" ~value:"(ref shards)" ();
  Ui.cancel_carry ui;
  if Ui.carrying ui <> None then fail "cancel_carry kept the payload";
  (* an idle frame after a carry builds nothing new *)
  let scene_size () = List.length (Ui.scene ui) in
  ignore (carried []);
  let idle = scene_size () in
  Ui.carry ui ~kind:"sop" ~value:"(ref shards)" ();
  ignore (carried [ move (50, 50) ]);
  Ui.cancel_carry ui;
  ignore (carried []);
  if scene_size () <> idle then fail "an idle frame painted a ghost";
  Ui.destroy ui;
  (match Sdl3.Init.quit_subsystems [Sdl3.Init.Video] with
   | Ok () -> () | Error error -> fail (Format.asprintf "%a" Sdl3.pp_error error));
  print_endline "PXUI Ui interaction contract passed at 1x and 2x"
