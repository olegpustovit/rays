(* The sketch's gate: Esc and q reach it only while a viewer or canvas has the focus and nothing
   else owns the keyboard; a key held while the keyboard was away stays hidden until released. *)
let fail message = failwith ("test_gate: " ^ message)

let input ?(keys = []) ?(buttons = []) ?(pointer = (10., 10.)) events : Frame_input.t =
  { t = 0.; dt = 1. /. 60.; frame = 0; tick = 0; size = (100, 100); pointer; buttons; keys; events }

(* a viewer has the focus, the pointer is on it, and nothing else is open *)
let open_ : Gate.t = {
  prompt = false; menu = false; leader = false; text_focus = false; focus_on_picture = true;
  pointer_on_picture = true; pressed = false; previous = input []; blocked = [] }

let presses key = input ~keys:[ key ] [ Key_pressed key; Text_input key ]
let sees (frame : Frame_input.t) key =
  List.mem key frame.keys || List.mem (Frame_input.Key_pressed key) frame.events

let () =
  (* Esc and q, whoever owns the keyboard *)
  List.iter (fun (what, g, reaches) ->
    List.iter (fun key ->
      let seen, _ = Gate.gate g (presses key) in
      if sees seen key <> reaches then
        fail (Printf.sprintf "%s: %s %s the sketch" what key (if reaches then "did not reach" else "reached"));
      if (List.exists (function Frame_input.Text_input _ -> true | _ -> false) seen.events) <> reaches then
        fail (what ^ ": typed text and the key disagree"))
      [ "escape"; "q" ])
    [ "a viewer or canvas in focus", open_, true;
      "the graph pane in focus", { open_ with focus_on_picture = false }, false;
      "the text pane in focus", { open_ with focus_on_picture = false; text_focus = true }, false;
      "a leader half typed", { open_ with leader = true }, false;
      "a prompt", { open_ with prompt = true }, false;
      "a menu", { open_ with menu = true }, false;
      "a text field in a canvas in focus", { open_ with text_focus = true }, false ];
  (* the Esc that closes a leader is still held when the keyboard returns: it stays hidden until released *)
  let closing = { open_ with leader = true } in
  let seen, blocked = Gate.gate closing (presses "escape") in
  if sees seen "escape" || blocked <> [ "escape" ] then fail "the leader's Esc reached the sketch or was not remembered";
  let held = input ~keys:[ "escape" ] [] in
  let seen, blocked = Gate.gate { open_ with blocked } held in
  if sees seen "escape" || blocked <> [ "escape" ] then fail "a held Esc leaked when the keyboard came back";
  let seen, blocked = Gate.gate { open_ with blocked } (input ~keys:[ "escape"; "q" ] [ Key_pressed "q" ]) in
  if sees seen "escape" || not (sees seen "q") || blocked <> [ "escape" ] then
    fail "another key was hidden with the held one";
  let seen, blocked = Gate.gate { open_ with blocked } (input [ Key_released "escape" ]) in
  if blocked <> [] || seen.keys <> [] then fail "a released key stayed hidden";
  let seen, _ = Gate.gate { open_ with blocked } (presses "escape") in
  if not (sees seen "escape") then fail "a fresh press after the release did not reach the sketch";
  (* the pointer: only over the picture, and a press that began there stays the sketch's *)
  let moved = input ~pointer:(50., 50.) [ Pointer_moved (50., 50.) ] in
  let away = { open_ with pointer_on_picture = false; previous = input ~pointer:(5., 5.) [] } in
  let seen, _ = Gate.gate away moved in
  if seen.pointer <> (5., 5.) || seen.events <> [] then fail "the pointer left the picture and the sketch followed";
  let seen, _ = Gate.gate { away with pointer_on_picture = true } moved in
  if seen.pointer <> (50., 50.) || seen.events = [] then fail "the pointer over the picture was hidden";
  let seen, _ = Gate.gate { away with prompt = true; pointer_on_picture = true } moved in
  if seen.pointer <> (5., 5.) then fail "a prompt did not hold the pointer";
  let drag = input ~pointer:(50., 50.) ~buttons:[ "left" ] [ Pointer_moved (50., 50.) ] in
  let seen, _ = Gate.gate { away with previous = input ~pointer:(5., 5.) ~buttons:[ "left" ] [] } drag in
  if seen.pointer <> (50., 50.) then fail "a drag that began on the picture was cut off";
  let seen, _ = Gate.gate away drag in
  if seen.buttons <> [] || seen.pointer <> (5., 5.) then fail "a drag that began elsewhere became the sketch's";
  let seen, _ = Gate.gate { away with pointer_on_picture = true; pressed = true } drag in
  if seen.buttons = [] then fail "a press on the picture was hidden";
  print_endline "Gate: Esc and q reach the sketch only with a viewer or canvas in focus; held keys wait for release; the pointer follows the picture"
