open Sdl3

external push_event_trace : unit -> bool = "caml_sdl3_test_push_event_trace"
external push_resize_burst : unit -> bool = "caml_sdl3_test_push_resize_burst"
external mutate_event_sources : unit -> unit = "caml_sdl3_test_mutate_event_sources"

let fail message = failwith ("SDL3 event test: " ^ message)
let get = function Ok value -> value | Error error -> fail (Format.asprintf "%a" pp_error error)

external dialog_reserve : unit -> int = "caml_sdl3_test_dialog_reserve"
external dialog_abandon : int -> unit = "caml_sdl3_test_dialog_abandon"
external dialog_finish : int -> string list option -> bool = "caml_sdl3_test_dialog_finish"
external dialog_finish_huge : int -> bool = "caml_sdl3_test_dialog_finish_huge"

(* File dialogs: an outcome is copied into a native slot on whatever thread
   SDL calls back from, and arrives as one event on the initial domain. *)
let check_dialogs () =
  let outcomes () =
    List.filter_map
      (function Event.Dialog { id; outcome } -> Some (id, outcome) | _ -> None)
      (get (Event.poll_coalesced ()))
  in
  ignore (outcomes ());
  let first = dialog_reserve () in
  if first = 0 then fail "no dialog slot was free";
  if outcomes () <> [] then fail "a dialog that is still open produced an event";
  if not (dialog_finish first (Some [ "/tmp/žaba.png"; "/tmp/b c.png" ])) then
    fail "the callback was not run";
  (match outcomes () with
  | [ (id, Event.Chosen [ "/tmp/žaba.png"; "/tmp/b c.png" ]) ] when id = first -> ()
  | _ -> fail "a chosen file list did not arrive as one event with its paths");
  if outcomes () <> [] then fail "a dialog's event arrived twice";
  let second = dialog_reserve () in
  if second = first then fail "a finished dialog's id was reused at once";
  ignore (dialog_finish second (Some []));
  (match outcomes () with
  | [ (id, Event.Cancelled) ] when id = second -> ()
  | _ -> fail "a cancelled dialog did not arrive as Cancelled");
  let third = dialog_reserve () in
  ignore (dialog_finish third None);
  (match outcomes () with
  | [ (id, Event.Failed "boom") ] when id = third -> ()
  | _ -> fail "a failed dialog did not carry its error text");
  let huge = dialog_reserve () in
  ignore (dialog_finish_huge huge);
  (match outcomes () with
  | [ (id, Event.Failed message) ] when id = huge && message <> "" -> ()
  | _ -> fail "an unbounded selection was not refused");
  (* several finish before the next poll: oldest first, one event each *)
  let a = dialog_reserve () and b = dialog_reserve () in
  ignore (dialog_finish b (Some [ "/tmp/b" ]));
  ignore (dialog_finish a (Some [ "/tmp/a" ]));
  (match outcomes () with
  | [ (x, Event.Chosen [ "/tmp/a" ]); (y, Event.Chosen [ "/tmp/b" ]) ] when x = a && y = b -> ()
  | _ -> fail "simultaneous dialogs did not arrive oldest first");
  (* the queue is bounded: a ninth open dialog is refused with an error *)
  let held = List.init 8 (fun _ -> dialog_reserve ()) in
  if List.mem 0 held then fail "eight dialogs should fit";
  if dialog_reserve () <> 0 then fail "a ninth open dialog was accepted";
  List.iter dialog_abandon held;
  let again = dialog_reserve () in
  if again = 0 then fail "abandoned dialogs did not free their slots";
  dialog_abandon again

let key ?(modifiers = []) ?(down = true) ?(repeat = false) key =
  Event.Key { key; modifiers; down; repeat }

let window change = Event.Window change

let run () =
  get (Init.init [ Init.Events ]);
  ignore (get (Event.poll_coalesced ()));
  if not (push_event_trace ()) then fail "SDL rejected a trace event";
  let events = get (Event.poll_coalesced ()) in
  let drop_path =
    match
      List.find_map
        (function Event.Drop { change = Event.File path; _ } -> Some path | _ -> None)
        events
    with
    | Some path -> path
    | None -> fail "the dropped file did not arrive"
  in
  let text =
    match List.find_map (function Event.Text_input text -> Some text | _ -> None) events with
    | Some text -> text
    | None -> fail "the text input did not arrive"
  in
  let composition =
    match
      List.find_map (function Event.Text_editing { text; _ } -> Some text | _ -> None) events
    with
    | Some text -> text
    | None -> fail "the composition did not arrive"
  in
  (* The kinds Rays never reads (pen, touch, gamepad, audio, display,
     clipboard, user, a sixth button, a text drop, window moves) are absent;
     the rest keep their order, SDL's macros chose every constructor, and the
     motion pair became one event with summed relative motion. *)
  let expected =
    Key.
      [
        key (Char 'a') ~modifiers:[ Shift_held; Caps_lock ];
        key (Char 'a') ~down:false;
        key Arrow_up ~repeat:true;
        key Escape;
        key F5;
        key Shift ~modifiers:[ Shift_held ];
        key Meta ~modifiers:[ Meta_held ];
        key Delete;
        key Enter;
        key Space;
        key (Char 'z');
        key (Char 'a') ~modifiers:[ Meta_held ];
        key (Char '7');
        key (Unknown 0x444);
        Event.Text_input "žirafa";
        Event.Text_editing { text = "č"; start = 1; length = 2 };
        Event.Mouse_motion { x = 11.5; y = 22.5; dx = 7.; dy = 3. };
        Event.Mouse_button { button = Left; down = true; x = 30.; y = 40. };
        Event.Mouse_button { button = Middle; down = false; x = 31.; y = 41. };
        Event.Mouse_button { button = Right; down = true; x = 32.; y = 42. };
        Event.Mouse_button { button = X1; down = true; x = 33.; y = 43. };
        Event.Mouse_button { button = X2; down = false; x = 34.; y = 44. };
        Event.Mouse_wheel
          {
            x = 1.;
            y = -2.;
            direction = Flipped;
            mouse_x = 50.;
            mouse_y = 60.;
            integer_x = 1;
            integer_y = -2;
          };
        window Event.Shown;
        window Event.Hidden;
        window Event.Minimized;
        window Event.Restored;
        window Event.Occluded;
        window Event.Focus_gained;
        window Event.Focus_lost;
        window (Event.Resized (640, 480));
        window (Event.Pixel_size_changed (1280, 960));
        window Event.Close_requested;
        Event.Pinch { phase = Began; scale = 1.0 };
        Event.Pinch { phase = Updated; scale = 1.25 };
        Event.Pinch { phase = Ended; scale = 1.0 };
        Event.Drop { change = Drop_begin; x = 0.; y = 0. };
        Event.Drop { change = Drop_position; x = 5.5; y = 6.5 };
        Event.Drop { change = File "/tmp/žaba.png"; x = 30.; y = 40. };
        Event.Drop { change = Drop_complete; x = 0.; y = 0. };
        Event.Quit;
      ]
  in
  if events <> expected then begin
    let describe = function
      | Event.Quit -> "Quit"
      | Window _ -> "Window"
      | Key _ -> "Key"
      | Text_input _ -> "Text_input"
      | Text_editing _ -> "Text_editing"
      | Mouse_motion _ -> "Mouse_motion"
      | Mouse_button _ -> "Mouse_button"
      | Mouse_wheel _ -> "Mouse_wheel"
      | Pinch _ -> "Pinch"
      | Drop _ -> "Drop"
      | Dialog _ -> "Dialog"
      | Scroll _ -> "Scroll"
    in
    let first_difference =
      let rec go index a b =
        match (a, b) with
        | [], [] -> "none"
        | x :: a, y :: b when x = y -> go (index + 1) a b
        | x :: _, y :: _ ->
            Printf.sprintf "event %d is %s, expected %s" index (describe x) (describe y)
        | _ -> Printf.sprintf "lengths differ at event %d" index
      in
      go 0 events expected
    in
    fail
      (Printf.sprintf "the typed trace changed: %d events, expected %d; %s" (List.length events)
         (List.length expected) first_difference)
  end;
  check_dialogs ();
  mutate_event_sources ();
  if text <> "žirafa" || composition <> "č" || drop_path <> "/tmp/žaba.png" then
    fail "an SDL-borrowed pointer escaped without an OCaml copy";
  (* Coalescing keeps the latest event of each size kind, not one overall. *)
  if not (push_resize_burst ()) then fail "SDL rejected a resize event";
  (match get (Event.poll_coalesced ()) with
  | [ Event.Window (Event.Resized (800, 600)); Event.Window (Event.Pixel_size_changed (1280, 960)) ]
    ->
      ()
  | events ->
      fail (Printf.sprintf "coalesced resize burst lost a kind (%d events)" (List.length events)));
  get (Init.quit_subsystems [ Init.Events ]);
  Printf.printf "SDL3 copied typed event trace passed (%d events)\n%!" (List.length events)
