open Rays
open Ui_core
open Ui_kit

let previous_utf8 text index =
  let rec seek index =
    if index <= 0 then 0
    else if Char.code text.[index] land 0xc0 <> 0x80 then index
    else seek (index - 1) in
  if index <= 0 then 0 else seek (index - 1)

let next_utf8 text index =
  let length = String.length text in
  let rec seek index =
    if index >= length then length
    else if Char.code text.[index] land 0xc0 <> 0x80 then index
    else seek (index + 1) in
  if index >= length then length else seek (index + 1)

let text_caret_at ui ?size text x =
  match face ui size with
  | None -> String.length text
  | Some font ->
      let length = String.length text and density = float ui.density in
      let id = if length = 0 then 0 else font_id ui.atlas font in
      let rec seek index width =
        if index >= length then length else
          let decoded = String.get_utf_8_uchar text index in
          let code = Uchar.to_int (Uchar.utf_decode_uchar decoded) in
          let advance = match glyph ui.atlas font ~id ~density:ui.density code with
            | Some glyph -> float glyph.advance /. density
            | None -> 0. in
          if x < width +. (advance /. 2.) then index
          else seek (index + Uchar.utf_decode_length decoded) (width +. advance) in
      seek 0 0.

let numeric_character = function
  | '0' .. '9' | '+' | '-' | '.' | 'e' | 'E' -> true
  | _ -> false

let clipboard_command ~command = function
  | Input.Key_pressed (Input.KeyChar key) when command ->
      Some (Char.lowercase_ascii key)
  | _ -> None

type text_edit = { mutable text : string; mutable caret : int; mutable anchor : int }

(* A text starts being edited: the caret at its end, the selection from [anchor], and nothing of
   the text edited before it (scroll, undo stacks, a double click's selection unit). *)
let reset_edit ui key text ~anchor =
  ui.edit_focus <- key; ui.edit_value <- text;
  ui.edit_caret <- String.length text; ui.edit_anchor <- anchor;
  ui.edit_scroll_x <- 0.;
  ui.edit_undo <- []; ui.edit_redo <- []; ui.edit_group <- -1; ui.edit_unit <- None;
  ui.edit_column <- -1.

let load_text_edit ui key text =
  if ui.edit_focus <> key || ui.edit_value <> text then begin
    let left = ui.edit_focus, ui.edit_value, ui.edit_undo, ui.edit_redo in
    let back = List.find_opt (fun (k, v, _, _) -> k = key && v = text) ui.edit_shelf in
    let shelf = List.filter (fun entry -> Some entry <> back) ui.edit_shelf in
    ui.edit_shelf <- List.take 4
      (if ui.edit_undo = [] && ui.edit_redo = [] then shelf else left :: shelf);
    reset_edit ui key text ~anchor:(String.length text);
    Option.iter (fun (_, _, undo, redo) -> ui.edit_undo <- undo; ui.edit_redo <- redo) back
  end;
  { text; caret = ui.edit_caret; anchor = ui.edit_anchor }

let save_text_edit ui edit =
  ui.edit_value <- edit.text;
  ui.edit_caret <- edit.caret;
  ui.edit_anchor <- edit.anchor

let text_selection edit =
  min edit.caret edit.anchor, max edit.caret edit.anchor

let replace_text edit inserted =
  let start, stop = text_selection edit in
  edit.text <- String.sub edit.text 0 start ^ inserted ^
    String.sub edit.text stop (String.length edit.text - stop);
  edit.caret <- start + String.length inserted;
  edit.anchor <- edit.caret

(* Words as macOS counts them: letters, digits, underscores and every non-ASCII byte. *)
let word_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | c -> Char.code c >= 128

(* Option-Left: back over the separators, then over the word *)
let word_left text i =
  let i = ref i in
  while !i > 0 && not (word_char text.[!i - 1]) do decr i done;
  while !i > 0 && word_char text.[!i - 1] do decr i done;
  !i

(* Option-Right: over the separators, then to the end of the word *)
let word_right text i =
  let n = String.length text in
  let i = ref i in
  while !i < n && not (word_char text.[!i]) do incr i done;
  while !i < n && word_char text.[!i] do incr i done;
  !i

let line_start text i =
  let i = ref i in
  while !i > 0 && text.[!i - 1] <> '\n' do decr i done;
  !i

let line_end text i =
  let n = String.length text in
  let i = ref i in
  while !i < n && text.[!i] <> '\n' do incr i done;
  !i

(* ---- line and form edits of the text area ---- *)

(* the lines a selection touches, as the byte range [first, stop) without the last break *)
let line_block text a b =
  line_start text a, line_end text (if b > a && text.[b - 1] = '\n' then b - 1 else b)

(* the bytes [first, stop) of the edit's text replaced by [replacement]; a selection over the
   replacement afterwards keeps its direction *)
let replace_block edit first stop replacement =
  edit.text <- String.sub edit.text 0 first ^ replacement ^ String.sub edit.text stop (String.length edit.text - stop)
let keep_selection edit first last =
  if edit.caret >= edit.anchor then (edit.anchor <- first; edit.caret <- last)
  else (edit.anchor <- last; edit.caret <- first)

(* [block] with every line commented by [prefix], or with it taken off when each non-blank line
   has it (and the space after it); a comment goes in at the block's smallest indent *)
let toggle_comment prefix block =
  let lines = String.split_on_char '\n' block in
  let blank l = String.trim l = "" in
  let indent l = let i = ref 0 in while l.[!i] = ' ' || l.[!i] = '\t' do incr i done; !i in
  let body = List.filter (fun l -> not (blank l)) lines in
  let n = String.length prefix in
  let commented l = String.starts_with ~prefix (String.sub l (indent l) (String.length l - indent l)) in
  String.concat "\n" (List.map (fun l ->
    if blank l then l else
    let i = indent l in
    if body <> [] && List.for_all commented body then
      let n = if i + n < String.length l && l.[i + n] = ' ' then n + 1 else n in
      String.sub l 0 i ^ String.sub l (i + n) (String.length l - i - n)
    else
      let col = List.fold_left (fun m l -> min m (indent l)) max_int body in
      String.sub l 0 col ^ prefix ^ " " ^ String.sub l col (String.length l - col)) lines)

(* the innermost bracket pair that holds [a, b) and is larger than it, as (open, past the close) *)
let enclosing_pair pairs a b =
  List.fold_left (fun best (o, c) ->
    if o <= a && b <= c + 1 && (o, c + 1) <> (a, b) then
      match best with Some (bo, bc) when bc - bo <= c + 1 - o -> best | _ -> Some (o, c + 1)
    else best) None pairs

(* the caret across the bracket beside it, else out to the opener of the pair around it *)
let bracket_jump pairs caret =
  match List.find_map (fun (o, c) ->
      if o = caret then Some (c + 1) else if c + 1 = caret then Some o else None) pairs with
  | Some _ as jump -> jump
  | None -> Option.map fst (enclosing_pair pairs caret caret)

(* what a double click selects at [i]: the word there, else the run of blanks, else the one
   punctuation character; nothing on a line break *)
let word_at text i =
  let n = String.length text in
  let class_of c = if word_char c then 1 else if c = ' ' || c = '\t' then 2
    else if c = '\n' || c = '\r' then 0 else 3 in
  let pick = if i < n && class_of text.[i] <> 0 then Some i
    else if i > 0 && class_of text.[i - 1] <> 0 then Some (i - 1) else None in
  match pick with
  | None -> i, i
  | Some p when class_of text.[p] = 3 -> p, p + 1
  | Some p ->
      let k = class_of text.[p] in
      let a = ref p and b = ref (p + 1) in
      while !a > 0 && class_of text.[!a - 1] = k do decr a done;
      while !b < n && class_of text.[!b] = k do incr b done;
      !a, !b

(* ---- undo inside the focused text: snapshots before each change, consecutive typing as one *)

let rec bounded n = function x :: rest when n > 0 -> x :: bounded (n - 1) rest | _ -> []

let remember ui edit ~typing =
  if not (typing && edit.caret = edit.anchor && ui.edit_group = edit.caret) then begin
    ui.edit_undo <- bounded 200 ((edit.text, edit.caret, edit.anchor) :: ui.edit_undo);
    ui.edit_redo <- []
  end;
  ui.edit_group <- -1

let undo_text ui edit ~redo =
  match if redo then ui.edit_redo else ui.edit_undo with
  | [] -> false
  | (text, caret, anchor) :: rest ->
      let now = edit.text, edit.caret, edit.anchor in
      if redo then (ui.edit_redo <- rest; ui.edit_undo <- now :: ui.edit_undo)
      else (ui.edit_undo <- rest; ui.edit_redo <- now :: ui.edit_redo);
      edit.text <- text; edit.caret <- caret; edit.anchor <- anchor;
      ui.edit_group <- -1;
      true

(* an exhausted stack hands the key to the host's own undo ([passed_undo]) *)
let pass_undo ui edit ~redo =
  undo_text ui edit ~redo || (ui.passed_undo <- Some (if redo then `Redo else `Undo); false)

(* A press places the caret (Shift extends the selection), a double click selects the word
   there, a triple click the [line_of] range; the drag that follows extends by the same unit. *)
let press_select ui edit ~shift ~clicks ~line_of at =
  if clicks < 2 then begin
    edit.caret <- at;
    if not shift then edit.anchor <- at;
    ui.edit_unit <- None
  end else begin
    let a, b = if clicks = 2 then word_at edit.text at else line_of at in
    edit.anchor <- a; edit.caret <- b;
    ui.edit_unit <- Some (a, b, clicks)
  end

let drag_select ui edit ~line_of at =
  match ui.edit_unit with
  | None -> edit.caret <- at
  | Some (a, b, clicks) ->
      let c, d = if clicks = 2 then word_at edit.text at else line_of at in
      if c < a then (edit.anchor <- b; edit.caret <- c)
      else (edit.anchor <- a; edit.caret <- max b d)

let point_text_caret ui ?size edit signal ~shift ~x ~right =
  let at (px, _) = text_caret_at ui ?size edit.text
    (Float.max 0. (Float.min right px -. x +. ui.edit_scroll_x)) in
  let line_of _ = 0, String.length edit.text in
  if signal.pressed then
    press_select ui edit ~shift ~clicks:signal.clicks ~line_of (at signal.press_point);
  if signal.dragging || (signal.held && signal.pointer <> signal.press_point) then begin
    let px = fst signal.pointer in
    if px < x then ui.edit_scroll_x <- Float.max 0.
      (ui.edit_scroll_x -. Float.max 1. (Float.min 24. ((x -. px) *. 0.25)))
    else if px > right then ui.edit_scroll_x <- ui.edit_scroll_x +.
      Float.max 1. (Float.min 24. ((px -. right) *. 0.25));
    drag_select ui edit ~line_of (at signal.pointer)
  end

(* The keys every text widget shares, as macOS binds them: Command-A/C/X/V, Command-Z and
   Shift-Command-Z (or Command-Y) undo and redo, Option-arrows move by words, Command-arrows
   and Home/End by lines, Option-Backspace/Delete take a word, Command-Backspace/Delete the
   line to the caret.  Returns whether the text changed. *)
let edit_text_event ?(multiline = false) ui edit ~accept ~modifiers event =
  let command = command_modifiers modifiers
  and shift = List.mem Input.Shift modifiers
  and alt = List.mem Input.Alt modifiers in
  let selected () = edit.caret <> edit.anchor in
  let move target =
    edit.caret <- target;
    if not shift then edit.anchor <- target in
  (* the selection, else from the caret to [target] *)
  let delete_to target =
    if (not (selected ())) && target = edit.caret then false else begin
      remember ui edit ~typing:false;
      if not (selected ()) then edit.anchor <- target;
      replace_text edit ""; true
    end in
  match event with
  | event when clipboard_command ~command event = Some 'a' ->
      edit.anchor <- 0; edit.caret <- String.length edit.text; false
  | event when clipboard_command ~command event = Some 'c' ->
      let start, stop = text_selection edit in
      ignore (Clipboard.set_text (if selected () then
        String.sub edit.text start (stop - start) else edit.text)); false
  | event when clipboard_command ~command event = Some 'x' ->
      let start, stop = text_selection edit in
      let copied = if selected () then
        String.sub edit.text start (stop - start) else edit.text in
      if Clipboard.set_text copied = Ok () then begin
        remember ui edit ~typing:false;
        if selected () then replace_text edit "" else begin
          edit.text <- ""; edit.caret <- 0; edit.anchor <- 0
        end;
        true
      end else false
  | event when clipboard_command ~command event = Some 'v' ->
      (* a field of one line takes a copied line without its break *)
      let one_line text = if multiline then text
        else String.of_seq (Seq.filter (fun c -> c <> '\n' && c <> '\r') (String.to_seq text)) in
      (match Result.map one_line (Clipboard.get_text ()) with
       | Ok text when accept text -> remember ui edit ~typing:false; replace_text edit text; true
       | Ok _ | Error _ -> false)
  | event when clipboard_command ~command event = Some 'z' -> pass_undo ui edit ~redo:shift
  | event when clipboard_command ~command event = Some 'y' -> pass_undo ui edit ~redo:true
  | Input.Text_input text when accept text ->
      let typing = not (String.exists (function ' ' | '\t' | '\n' -> true | _ -> false) text) in
      remember ui edit ~typing;
      replace_text edit text;
      if typing then ui.edit_group <- edit.caret;
      true
  | Input.Key_pressed Input.Backspace ->
      delete_to (if command then line_start edit.text edit.caret
        else if alt then word_left edit.text edit.caret
        else previous_utf8 edit.text edit.caret)
  | Input.Key_pressed Input.Delete ->
      delete_to (if command then line_end edit.text edit.caret
        else if alt then word_right edit.text edit.caret
        else next_utf8 edit.text edit.caret)
  | Input.Key_pressed Input.ArrowLeft ->
      move (if command then line_start edit.text edit.caret
        else if alt then word_left edit.text edit.caret
        else if selected () && not shift then fst (text_selection edit)
        else previous_utf8 edit.text edit.caret);
      false
  | Input.Key_pressed Input.ArrowRight ->
      move (if command then line_end edit.text edit.caret
        else if alt then word_right edit.text edit.caret
        else if selected () && not shift then snd (text_selection edit)
        else next_utf8 edit.text edit.caret);
      false
  | Input.Key_pressed Input.Home -> move (line_start edit.text edit.caret); false
  | Input.Key_pressed Input.End -> move (line_end edit.text edit.caret); false
  | _ -> false

let paint_text_edit paint ?size ?(right = false) ?(inset = 2.) ?(caret = true) ~control:(cx, cy, cw, ch) ~y ~composition edit =
  let ui = paint.owner in
  let theme = ui.theme in
  let width text = Paint.text_width paint ?size text in
  let before = String.sub edit.text 0 edit.caret in
  let visible = float (max 1 (cw - 4)) in
  let scroll = Float.max 0. (Float.min ui.edit_scroll_x
    (Float.max 0. (width edit.text -. visible))) in
  let caret_width = width before in
  let scroll = if caret_width < scroll then caret_width
    else if caret_width -. scroll > visible then caret_width -. visible
    else scroll in
  ui.edit_scroll_x <- scroll;
  (* a number keeps its right edge while it is edited, until it outgrows the field *)
  let origin = if right && width edit.text <= visible
    then float (cx + cw - 2) -. width edit.text else float cx +. inset in
  let text_x = origin -. scroll in
  let caret_x = text_x +. caret_width in
  Paint.input_region paint ~x:(float cx) ~y:(float cy) ~w:(float cw)
    ~h:(float ch) ~focused:true ~cursor:(caret_x -. float cx) ();
  let previous_clip = paint.clip_rect in
  paint.clip_rect <- intersect previous_clip
    (float cx, float cy, float cw, float ch);
  if edit.caret <> edit.anchor then begin
    let start, stop = text_selection edit in
    Paint.fill paint
      ~x:(text_x +. width (String.sub edit.text 0 start))
      ~y:(float cy +. 0.5)
      ~w:(width (String.sub edit.text start (stop - start)))
      ~h:(float (max 1 (ch - 2))) (Theme.tint theme)
  end;
  if composition = "" then Paint.text paint ?size ~at:(text_x, float y) edit.text
  else begin
    Paint.text paint ?size ~at:(text_x, float y) before;
    Paint.text paint ?size ~at:(caret_x, float y) composition;
    Paint.text paint ?size ~at:(caret_x +. width composition, float y)
      (String.sub edit.text edit.caret
        (String.length edit.text - edit.caret))
  end;
  (* the caret is the line of the text it sits in: 14 of a 20-point field at body size *)
  let caret_y, caret_h = match size with
    | None -> float cy +. 2.5, 14.
    | Some size -> let h = float size *. 1.1 in float cy +. ((float ch -. h) /. 2.), h in
  if caret then Paint.fill paint ~x:caret_x ~y:caret_y ~w:1. ~h:caret_h theme.accent;
  paint.clip_rect <- previous_clip

(* Numeric label editing shared by float and integer sliders. The retained
   state records validity; [parse] validates the edit buffer. *)
let rec numeric_editor ?size ?control ?(click_to_edit = false)
    ?(edit_request = false) ?(alt_to_edit = false) ?(align_right = false)
    ?(accept = String.for_all numeric_character) ui row signal ~keys ~current ~parse =
  let enter = function
    | Input.Key_pressed Input.Enter, modifiers -> not (command_modifiers modifiers)
    | _ -> false in
  let bounds = ints (rect ui row) in
  let control_bounds = Option.value ~default:(value_control bounds) control in
  let control = control_bounds in
  let inside_control point = contains (floats control) point in
  let editing = text_state ui row in
  let state = state ui row ~default:1 in
  let set text ~valid =
    set_text_state ui row (Some text);
    set_state ui row (if valid then 1 else 0) in
  let finish () = set_text_state ui row None; set_state ui row 1 in
  match editing with
  | Some text when not (focused ui row) || (signal.pressed
      && not (inside_control signal.press_point)) ->
      (* Focus moved away: commit a valid value, drop an invalid one. *)
      finish ();
      if focused ui row then unfocus ui;
      (match parse text with Some value -> Some value | None -> None), false
  | Some text ->
      let edit = load_text_edit ui row.box_key text in
      let committed = ref None and state = ref state
      and cancelled = ref false in
      let (cx, _, cw, _) = control in
      let left = if align_right then
          Float.max (float (cx + 2)) (float (cx + cw - 2) -. text_width ui ?size edit.text)
        else float (cx + 2) in
      point_text_caret ui ?size edit signal ~shift:(press_shift ui row)
        ~x:left ~right:(float (cx + cw - 2));
      List.iter (fun ((event : Input.event), modifiers) -> match event with
        | Input.Key_pressed Input.Enter when not (command_modifiers modifiers) ->
            (match parse edit.text with
             | Some value -> committed := Some value
             | None -> state := 0)
        | Input.Key_pressed Input.Escape -> cancelled := true
        | event ->
            if edit_text_event ui edit ~modifiers
                ~accept event then
              state := if parse edit.text <> None then 1 else 0) keys;
      if !cancelled then (finish (); unfocus ui; None, false)
      else (match !committed with
        | Some value -> finish (); unfocus ui; Some value, false
        | None ->
            save_text_edit ui edit;
            set edit.text ~valid:(!state land 1 <> 0);
            None, true)
  | None ->
      let click = click_to_edit && signal.clicked
        && abs_float (fst signal.release_point -. fst signal.press_point) < 4.
        && abs_float (snd signal.release_point -. snd signal.press_point) < 4. in
      if edit_request || (alt_to_edit && signal.clicked
          && List.mem Input.Alt (press_keys ui row)) || click
          || (signal.double_clicked && not (inside_control signal.press_point))
          || (focused ui row && List.exists enter keys) then begin
        let text = current () in
        set text ~valid:true;
        focus ui row;
        reset_edit ui row.box_key text ~anchor:0;
        let rec after_enter = function
          | event :: rest when enter event -> rest
          | _ :: rest -> after_enter rest | [] -> [] in
        let keys = if signal.double_clicked || signal.clicked then keys else after_enter keys in
        numeric_editor ?size ~control:control_bounds ~click_to_edit ~align_right ~accept ui row
          { signal with keys = List.map fst keys; pressed = false;
            clicked = false; double_clicked = false } ~keys ~current ~parse
      end else None, false

let value_field ui ~at ~w ~h ?size ?display ?fraction ?slide ?scrub ?(left = false)
    ?(edit = false) ?lead ?trail ?line ?(bare = false) ?placeholder ?tracking ~valid label value =
  let box = box ui ~flags:(clickable lor tab_stop lor blocking lor clip)
    ~at ~w:(Px w) ~h:(Px h) label in
  let signal = signal ui box in
  let bounds = ints (rect ui box) in
  let slider = Option.is_some slide || Option.is_some scrub in
  let typed, editing = numeric_editor ?size ~control:bounds
    ~click_to_edit:(not slider) ~edit_request:(edit || (slider && signal.double_clicked)) ~alt_to_edit:slider
    ~accept:(fun _ -> true) ui box signal ~keys:(key_events ui box)
    ~current:(fun () -> value) ~parse:(fun text -> if valid text then Some text else None) in
  if editing then ui.b_flags.(box.index) <- clickable lor focusable lor blocking lor clip;
  let origin = if signal.pressed then begin
      ui.scrub_origin <- Some (box.box_key, value); value
    end else match ui.scrub_origin with
      | Some (key, origin) when key = box.box_key -> origin | _ -> value in
  let value = match typed with
    | Some text -> text
    | None when not editing && (signal.pressed || signal.held || signal.released)
        && signal.button = Some Input.LeftButton
        && not (List.mem Input.Alt (press_keys ui box)) ->
        let pointer = if signal.released then signal.release_point else signal.pointer in
        (match slide, scrub with
         | Some slide, _ -> slide (fraction_at bounds (fst pointer))
         | None, Some scrub -> scrub origin (fst pointer -. fst signal.press_point)
             (List.mem Input.Shift (press_keys ui box))
         | None, None -> value)
    | _ -> value in
  if not signal.held && Option.fold ~none:false
      ~some:(fun (key, _) -> key = box.box_key) ui.scrub_origin then ui.scrub_origin <- None;
  let buffer = text_state ui box in
  let edit = if editing && focused ui box then
      load_text_edit ui box.box_key (Option.value ~default:value buffer)
    else { text = value; caret = 0; anchor = 0 } in
  let composition = ui.composition and focus = focused ui box in
  let display = if typed <> None || signal.held || signal.released then value
    else Option.value ~default:value display in
  let invalid = state ui box ~default:1 land 1 = 0 in
  draw ui box (fun paint rect ->
    let (x, y, w, h) as bounds = ints rect in
    let previous_clip = paint.clip_rect in
    paint.clip_rect <- intersect previous_clip
      (float x, float y, float w, float h);
    if invalid || editing || not bare then
      underline paint bounds (if invalid then Theme.invalid
        else if editing then ui.theme.accent
        else Option.value line ~default:(Theme.edge ui.theme));
    (* the content is centred in the 19 points above the hairline: a body line starts half a point up *)
    let text_y = match size with
      | Some size -> float (y + max 0 ((h - size) / 2))
      | None -> float y +. (float (h - 1 - ui.font_size) /. 2.) -. 0.5 in
    if editing && focus then paint_text_edit paint ?size ~control:bounds ~y:(y + max 0 ((h - Option.value ~default:ui.font_size size) / 2)) ~composition edit
    else begin
      (* the 2-point line under the value is its position in the soft range *)
      Option.iter (fun f -> Paint.fill paint ~x:(float x) ~y:(float (y + h - 3))
        ~w:(float w *. Float.max 0. (Float.min 1. f)) ~h:2. (Theme.ink_2 ui.theme)) fraction;
      match lead, trail with
      | None, None when display = "" && placeholder <> None ->
          Paint.text paint ?size ~color:(Theme.ink_3 ui.theme)
            ~at:(float (x + 2), text_y) (Option.get placeholder)
      | None, None ->
          let width = Paint.text_width paint ?size display in
          Paint.text paint ?size ?tracking ~at:((if left then float (x + 2)
            else float (x + w - 2) -. width), text_y) display
      | _ ->
          (* a lead (the expression's ƒ) and a trail (its live value) frame the value: 6 between *)
          let draw_part ?(right = false) (text, color) =
            let width = Paint.text_width paint ?size text in
            Paint.text paint ?size ~color
              ~at:((if right then float (x + w - 2) -. width else float (x + 2)), text_y) text;
            width in
          let lead_w = Option.fold ~none:0. ~some:(fun l -> draw_part l +. 6.) lead in
          let trail_w = Option.fold ~none:0. ~some:(fun t -> draw_part ~right:true t +. 6.) trail in
          (* with no lead the value sits against its trail (a unit: [35 mm]) *)
          let shown = inspector_fit paint ~size:(Option.value ~default:ui.font_size size)
            ~width:(float w -. 4. -. lead_w -. trail_w) display in
          let value_x = if lead = None && not left
            then float (x + w - 2) -. trail_w -. Paint.text_width paint ?size shown
            else float (x + 2) +. lead_w in
          Paint.text paint ?size ~color:ui.theme.foreground ~at:(value_x, text_y) shown
    end;
    paint.clip_rect <- previous_clip);
  value, editing

type head_action = { caption : string; keycap : string; active : bool; usable : bool }
type head = { renamed : string; chosen : int option; reset_pressed : bool }

(* The head block of a panel, from the kit's inspector sheet: 12 above, a chips row (the kind in
   ink-2, a badge in the accent, an index at the right in ink-3), the name at the display size
   (edited in place when [rename] says which names are valid), one detail line in ink-2, a row of
   text buttons with their keys and a trailing [reset] button, 8 below and a line-2 hairline. *)
let inspector_header ui ~key ?kind ?badge ?index ?rename ?(actions = []) ?reset ?(reset_enabled = true) ~title ~detail () =
  let win = ui.kit_window in
  (* a window's head (windows.html): the 20-point title and one detail line, 8 above, no chips,
     buttons or hairline *)
  let display = ui.font_size * (if win then Theme.title_size else Theme.display_size) / Theme.font_size in
  let small = max 8 (ui.font_size - 2) in
  let line = float ui.kit_row_height and title_h = if win then float ui.kit_row_height else float (display + 8) in
  let width = inspector_width ui in
  let chips = not win && (kind <> None || badge <> None || index <> None) in
  let actions = if win then [] else actions and reset = if win then None else reset in
  let buttons = actions <> [] || reset <> None in
  let text_x = if win then 12. else 8. in
  let y_title = if win then 8. else if chips then 12. +. line +. 4. else 12. in
  let y_detail = y_title +. title_h +. (if win then 0. else 4.) in
  let y_buttons = y_detail +. line +. 8. in
  let height = if win then y_detail +. line
    else (if buttons then y_buttons +. 20. else y_detail +. line) +. 8. +. 1. in
  let header = box ui ~flags:clip ~w:Grow ~h:(Px height) key in
  let theme = ui.theme in
  let measure ?(size = ui.font_size) text = mono_width size text in
  let title', editing = match rename with
    | Some valid ->
        within ui header (fun () ->
          value_field ui ~at:(text_x -. 2., if win then y_title -. 1. else y_title -. 2.)
            ~w:(width -. (2. *. (text_x -. 2.))) ~h:title_h ~size:display
            ~left:true ~bare:true ~tracking:(-0.01 *. float display) ~valid (key ^ "-name") title)
    | None -> title, false in
  (* the buttons: a box each, in a row from 8 with 4 between; [reset] is bare at the right *)
  let pressed = ref None in
  let button index ~label ~hint ~on ~dim ~enabled ~x ~w ~pad =
    let b = within ui header (fun () ->
      box ui ~flags:(if enabled then clickable lor tab_stop else none) ~at:(x, y_buttons)
        ~w:(Px w) ~h:(Px 20.) (Printf.sprintf "%s-button-%d" key index)) in
    let signal = signal ui b in
    if enabled && signal.clicked then pressed := Some index;
    draw ui b (fun paint ((x, y, _, h) as rect) ->
      paint_button_ground paint theme ~held:(enabled && signal.held) ~hovered:(enabled && signal.hovered)
        ~on ~primary:false rect;
      Paint.text paint ~at:(x +. 1. +. pad, text_top ui y h)
        ~color:(if not enabled then Theme.ink_3 theme
                else if dim then Theme.ink_2 theme else theme.foreground) label;
      if hint <> "" then
        Paint.text paint ~size:small
          ~at:(x +. 1. +. pad +. mono_width ui.font_size label +. 6., text_top ui ~size:small y h)
          ~color:(Theme.ink_3 theme) hint) in
  let place = ref 8. in
  List.iteri (fun index (a : head_action) ->
    let w = 14. +. measure a.caption +. (if a.keycap = "" then 0. else 6. +. measure ~size:small a.keycap) in
    button index ~label:a.caption ~hint:a.keycap ~on:a.active ~dim:false ~enabled:a.usable ~x:!place ~w ~pad:6.;
    place := !place +. w +. 4.) actions;
  let reset_pressed = ref false in
  Option.iter (fun label ->
    let w = 10. +. measure label in
    let before = !pressed in
    button (-1) ~label ~hint:"" ~on:false ~dim:true ~enabled:reset_enabled ~x:(width -. 8. -. w) ~w ~pad:4.;
    if !pressed <> before then (pressed := before; reset_pressed := true)) reset;
  draw ui header (fun paint (x, y, w, h) ->
    if not win then Paint.fill paint ~x ~y:(y +. h -. 1.) ~w ~h:1. (Theme.edge theme);
    if chips then begin
      let at = y +. 12. +. text_top ui ~size:small 0. line in
      let cx = ref (x +. 8.) in
      (* the kind gives way when the index and badge need the room *)
      let per = (float small /. 2.) +. (0.08 *. float small) in
      let taken = Option.fold ~none:0. ~some:(fun t -> mono_width small t +. 6.) index
        +. Option.fold ~none:0. ~some:(fun t -> mono_width ~tracking:(0.08 *. float small) small t +. 6.) badge in
      Option.iter (fun text ->
        let room = int_of_float ((w -. 16. -. taken) /. per) in
        let text = if String.length text <= room then text
          else if room <= 1 then "" else String.sub text 0 (room - 1) ^ "." in
        Paint.cap paint ~at:(!cx, at) text;
        (* the glyphs run a little wider than the design's metrics: never closer than 6 *)
        cx := !cx +. Float.max (mono_width ~tracking:(0.08 *. float small) small text)
                (Paint.cap_width paint text) +. 6.) kind;
      Option.iter (fun text -> Paint.cap paint ~at:(!cx, at) ~color:theme.accent text) badge;
      (* the sheet's 0.04em tracking follows every letter, the last too, so it ends 8 from the edge *)
      Option.iter (fun text ->
        let tracking = 0.04 *. float small in
        let count = ref 0 in
        String.iter (fun c -> if Char.code c land 0xc0 <> 0x80 then incr count) text;
        Paint.text paint ~size:small ~tracking ~color:(Theme.ink_3 theme)
          ~at:(x +. w -. 8. -. Paint.text_width paint ~size:small text -. (tracking *. float !count), at) text) index
    end;
    if rename = None then
      Paint.text paint ~at:(x +. text_x,
          y +. y_title +. (if win then text_top ui ~size:display 0. title_h else 2.))
        ~size:display ~tracking:(-0.01 *. float display) ~color:theme.foreground
        (inspector_fit paint ~size:display ~width:(w -. (2. *. text_x)) title);
    Paint.text paint ~at:(x +. text_x, y +. y_detail +. text_top ui 0. line) ~color:(Theme.ink_2 theme)
      (inspector_fit paint ~size:ui.font_size ~width:(w -. (2. *. text_x)) detail));
  ignore editing;
  { renamed = title'; chosen = !pressed; reset_pressed = !reset_pressed }

let text_field ui text value =
  let row = kit_row ui ~flags:(clickable lor focusable lor blocking lor focus_mark)
      ~hit:(control_hit value_control) text in
  let signal = signal ui row in
  let focused = focused ui row in
  let edit = if focused then load_text_edit ui row.box_key value else
    let end_ = String.length value in
    { text = value; caret = end_; anchor = end_ } in
  if focused then begin
    let (cx, _, cw, _) = value_control (ints (rect ui row)) in
    point_text_caret ui edit signal ~shift:(press_shift ui row)
      ~x:(float (cx + 2)) ~right:(float (cx + cw - 2));
    List.iter (fun (event, modifiers) -> ignore (edit_text_event ui edit ~modifiers
      ~accept:(fun _ -> true) event)) (key_events ui row);
    save_text_edit ui edit
  end;
  let value = edit.text in
  let theme = ui.theme and shown = display text and composition = ui.composition in
  let hovered = signal.hovered in
  (* the I-beam over the value, as in any text field *)
  (let (px, py) = ui.pointer in
   let (cx, cy, cw, ch) = value_control (ints (rect ui row)) in
   if hovered && px >= float cx && px < float (cx + cw)
      && py >= float cy && py < float (cy + ch) then request_cursor ui `Text);
  draw ui row (fun paint rect ->
    let (x, y, _, h) as bounds = ints rect in
    let (cx, cy, cw, ch) as control = value_control bounds in
    if hovered then hover_row paint ui bounds;
    kit_text paint ~color:(Theme.ink_2 theme) (x + side) (label_y ui y h) shown;
    underline paint control (if focused then theme.accent else Theme.edge theme);
    if focused then paint_text_edit paint ~control ~y:(label_y ui y h)
      ~composition edit
    else begin
      Paint.input_region paint ~x:(float cx) ~y:(float cy) ~w:(float cw)
        ~h:(float ch) ~focused:false ();
      kit_text paint ~color:theme.foreground (cx + 2) (label_y ui y h) value
    end);
  value

(* A search field at a place: [text_field]'s editing, a hairline for the control and its
   [placeholder] while empty; Enter, Shift-Enter, Down and Up answer a step, Escape a close. *)
let number_field ui ~at ~w ~h ?size ?display ?edit ?left ?trail ~kind ?range ?valid key text =
  let module N = Param.Number in
  let valid = Option.value valid ~default:(N.valid kind) in
  fst (value_field ui ~at ~w ~h ?size ?display ?fraction:(Option.bind range (fun r -> N.fraction r text))
    ~scrub:(N.scrub kind ?range) ?edit ?left ?trail ~valid key text)

let line_field ui ~at ~w ~h ?(grab = false) ?(placeholder = "") label value =
  let field = box ui ~flags:(clickable lor focusable lor blocking lor focus_mark) ~at ~w:(Px w) ~h:(Px h) label in
  if grab then focus ui field;
  let signal = signal ui field in
  let focused = focused ui field in
  let edit = if focused then load_text_edit ui field.box_key value else
    { text = value; caret = String.length value; anchor = String.length value } in
  let step = ref `None in
  if focused then begin
    let (fx, _, fw, _) = ints (rect ui field) in
    point_text_caret ui edit signal ~shift:(press_shift ui field) ~x:(float (fx + 2)) ~right:(float (fx + fw - 2));
    List.iter (fun ((event : Input.event), modifiers) -> match event with
      | Input.Key_pressed Input.Enter when not (command_modifiers modifiers) ->
          step := if List.mem Input.Shift modifiers then `Previous else `Next
      | Input.Key_pressed Input.ArrowDown -> step := `Next
      | Input.Key_pressed Input.ArrowUp -> step := `Previous
      | Input.Key_pressed Input.Escape -> step := `Close
      | event -> ignore (edit_text_event ui edit ~modifiers ~accept:(fun _ -> true) event)) (key_events ui field);
    save_text_edit ui edit
  end;
  if signal.hovered then request_cursor ui `Text;
  let theme = ui.theme and composition = ui.composition in
  draw ui field (fun paint rect ->
    let (x, y, w, h) = ints rect in
    let control = x, y, w, h in
    underline paint control (if focused then theme.accent else Theme.edge theme);
    if focused then paint_text_edit paint ~control ~y:(label_y ui y h) ~composition edit
    else kit_text paint ~color:(if edit.text = "" then Theme.ink_3 theme else theme.foreground)
      (x + 2) (label_y ui y h) (if edit.text = "" then placeholder else edit.text));
  edit.text, !step

(* Multiline text: the same focus, IME composition, clipboard and edit
   events as [text_field] ([edit_text_event], the [ui.edit_*] retained
   state); added are Enter, Tab (two spaces), the vertical keys and row-scoped Home/End,
   and the pointer maps to (row, column).  With [wrap] a long line continues on the next row
   after a fixed number of Unicode code points.  ponytail: rows are
   found per frame (O(text)), only visible rows are drawn. *)

(* The rows of a text: (start, stop, logical line) with [stop] exclusive and before the newline.
   Without [cols] a row is a line; with it a line continues every [cols] characters. *)
let text_rows ?cols text =
  let n = String.length text in
  let rows = ref [] in
  let rec line start logical =
    let stop = match String.index_from_opt text start '\n' with Some i -> i | None -> n in
    let rec chunk from = match cols with
      | None -> rows := (from, stop, logical) :: !rows
      | Some cols ->
          let rec advance i k =
            if k = 0 || i >= stop then i
            else advance (i + Uchar.utf_decode_length (String.get_utf_8_uchar text i)) (k - 1) in
          let upto = advance from (max 1 cols) in
          if upto >= stop then rows := (from, stop, logical) :: !rows
          else (rows := (from, upto, logical) :: !rows; chunk upto) in
    chunk start;
    if stop < n then line (stop + 1) (logical + 1) in
  line 0 0;
  Array.of_list (List.rev !rows)

(* the row holding byte [index] *)
let row_at rows index =
  let start i = let s, _, _ = rows.(i) in s in
  let rec seek low high =
    if low >= high then low else
      let middle = (low + high + 1) / 2 in
      if start middle <= index then seek middle high else seek low (middle - 1) in
  seek 0 (Array.length rows - 1)

let context_clicked (signal : signal) =
  let (px, py), (rx, ry) = signal.press_point, signal.release_point in
  signal.released && signal.button = Some Input.RightButton
  && ((px -. rx) *. (px -. rx)) +. ((py -. ry) *. (py -. ry)) < 16.

let context_at ui box =
  Int_table.fold (fun target (value : accumulator) found ->
    if found = None && hit_within ui target (key box) && value.released && value.button = Some Input.RightButton
       && Float.hypot (fst value.press_point -. fst value.release_point)
            (snd value.press_point -. snd value.release_point) < 4.
    then Some value.release_point else found) ui.signals None

(* What a code editor knows about its text: colours by byte span, matching bracket pairs, the
   indentation of a new line, the brackets typed in pairs, ranked completions for the token at
   the caret, a description of the token under the pointer, and the numeric literals a drag
   changes.  A host supplies it; the widget stays language-free. *)
type completion = {
  replace : int * int;  (* the byte span [insert] replaces *)
  insert : string;
  label : string;  (* the row *)
  detail : string;  (* the row's right column: a type, a category *)
  doc : string;  (* one line under the rows while the row is selected *)
}

type language = {
  colorize : string -> (int * int * Rays_math.Color.t) list;  (* sorted, non-overlapping byte spans *)
  brackets : string -> (int * int) list;  (* (open, close) byte positions of matched pairs *)
  indent : string -> int -> string;  (* the indentation of a line broken at this byte *)
  pairs : (char * char) list;  (* typing the first inserts both; the second skips over itself *)
  complete : string -> int -> completion list;  (* ranked, for the token ending at the caret *)
  describe : string -> int -> (int * int * string) option;  (* the token at a byte and its doc *)
  number_at : string -> int -> (int * int) option;  (* the numeric literal at a byte *)
  context_at : string -> int -> [ `Code | `String | `Comment ];  (* what a caret at a byte is in *)
  comment : string;  (* what starts a line comment *)
  rewrite : (string -> int -> string * int) option;
  (* [rewrite text caret] after an edit: the text as the language keeps it (parinfer), and
     where [caret] lands in it *)
}

(* A box at the root, laid out and painted after every pane (like a tooltip), so a popup under
   a caret is never clipped by its pane. *)
let overlay_box ui ?flags ~at ~w ~h label =
  at_root ui (fun () ->
    let overlay = box ui ?flags ~w:(Px w) ~h:(Px h) ~at label in
    ui.overlays <- overlay.index :: ui.overlays;
    overlay)

(* A number literal dragged [dx] points: a float moves a tenth of its last decimal place per
   point (Shift: ten times that) and keeps one more decimal; an integer moves one per five
   points (Shift: one per point). *)
let scrubbed literal dx ~coarse =
  match String.index_opt literal '.', int_of_string_opt literal with
  | None, Some v -> string_of_int (v + int_of_float (Float.round (dx /. (if coarse then 1. else 5.))))
  | Some dot, _ ->
      let decimals = String.length literal - dot - 1 in
      let step = (10. ** float (- (decimals + 1))) *. (if coarse then 10. else 1.) in
      let v = (match float_of_string_opt literal with Some v -> v | None -> 0.) +. (dx *. step) in
      let s = Printf.sprintf "%.*f" (decimals + 1) v in
      let n = ref (String.length s) in
      while !n > 0 && s.[!n - 1] = '0' do decr n done;
      if !n > 0 && s.[!n - 1] = '.' then String.sub s 0 (!n + 1) else String.sub s 0 !n
  | None, None -> literal

(* the text area's painting: gutter, selection, runs, diagnostics, caret *)
let paint_text_area ~colors ~composition ~messages ~chips ~hovered_number ~scrubbing ~matched ~spans ~errors ~slot_of ~notes ~char_w ~final ~stop_of ~start_of ~focused ~edit ~rows ~count ~notes ~row ~pad ~offset ~horizontal ~gutter ~(theme : Theme.t) ~body ui =
  draw ui body (fun paint (bx, by, bw, bh) ->
    let width text = Paint.text_width paint text in
    Paint.fill paint ~x:bx ~y:by ~w:bw ~h:bh theme.input;
    let previous = paint.clip_rect in
    let clip x w = paint.clip_rect <- intersect previous
      (x, by, w, bh) in
    let text_x = bx +. gutter -. horizontal in
    Paint.fill paint ~x:(bx +. gutter) ~y:by ~w:1. ~h:bh (Theme.faint_border theme);
    let small = Paint.label_size paint in
    let first = max 0 (int_of_float (Float.floor ((offset -. pad) /. row)) - Array.length notes)
    and last = min (count - 1) (int_of_float (Float.floor ((offset +. bh) /. row))) in
    let caret_line = row_at rows edit.caret in
    let selected = focused && edit.caret <> edit.anchor in
    let s0, s1 = text_selection edit in
    let band line (start, stop) color y =
      let ls = start_of rows line and le = stop_of rows line in
      (* a selection runs on past the end of a row that ends a line *)
      let newline = le < String.length final && final.[le] = '\n' in
      let start = max start ls and stop = min stop (if newline then le + 1 else le) in
      if start < stop || (start <= le && stop > le) then
        let x0 = width (String.sub final ls (min start le - ls)) in
        let x1 = if stop > le then width (String.sub final ls (le - ls)) +. char_w
          else width (String.sub final ls (stop - ls)) in
        Paint.fill paint ~x:(text_x +. x0) ~y ~w:(Float.max 1. (x1 -. x0)) ~h:row color in
    for line = first to last do
      let y = by -. offset +. pad +. (float (slot_of notes line) *. row) in
      let text_y = y +. float (max 0 ((text_line_height ui - ui.font_size) / 2)) in
      let _, _, logical = rows.(line) in
      let starts_line = line = 0 || (let _, _, before = rows.(line - 1) in before <> logical) in
      clip bx bw;
      let wrong = List.mem (logical + 1) errors and current = focused && line = caret_line in
      if wrong then Paint.fill paint ~x:bx ~y ~w:bw ~h:row (Rays_math.Color.with_alpha Theme.invalid 18)
      else if current then Paint.fill paint ~x:bx ~y ~w:bw ~h:row (Theme.faint_border theme);
      if starts_line then begin
        let number = string_of_int (logical + 1) in
        let number_x = bx +. gutter -. 10. -. Paint.text_width paint ~size:small number in
        if wrong then Paint.fill paint ~x:(number_x -. 10.) ~y:(y +. (row /. 2.) -. 3.) ~w:6. ~h:6. Theme.invalid;
        Paint.text paint ~size:small ~at:(number_x, text_y +. 1.)
          ~color:(if wrong then Theme.invalid else if current then theme.foreground else Theme.ink_3 theme) number
      end;
      clip (bx +. gutter) (bw -. gutter);
      List.iter (fun span -> band line span (Theme.tint theme) y) spans;
      if selected then band line (s0, s1) (Theme.tint theme) y;
      (* the bracket pair at the caret: an accent outline *)
      Option.iter (fun (o, k) -> List.iter (fun b ->
        let ls = start_of rows line and le = stop_of rows line in
        if b >= ls && b < le then
          (* the glyph's box (6.5 x 14) with a 1-point outline outside it: 8.5 x 16 *)
          Paint.frame paint ~x:(text_x +. width (String.sub final ls (b - ls)) -. 1.) ~y:(y +. 2.)
            ~w:(char_w +. 2.) ~h:16. theme.accent) [ o; k ]) matched;
      (* a number under the pointer (or being dragged) wears an accent underline *)
      Option.iter (fun (a, b) ->
        let ls = start_of rows line and le = stop_of rows line in
        if a >= ls && a < le then begin
          let x0 = width (String.sub final ls (a - ls)) and x1 = width (String.sub final ls (min b le - ls)) in
          Paint.fill paint ~x:(text_x +. x0) ~y:(y +. row -. 4.) ~w:(Float.max 1. (x1 -. x0)) ~h:2.
            (if !scrubbing <> None then theme.accent else Rays_math.Color.with_alpha theme.accent 160)
        end) hovered_number;
      let ls = start_of rows line and le = stop_of rows line in
      (* a colour literal wears its colour as a bar under it *)
      List.iter (fun (a, b, color) ->
        if a >= ls && a < le && b <= String.length final then
          Paint.fill paint ~x:(text_x +. width (String.sub final ls (a - ls))) ~y:(y +. row -. 5.)
            ~w:(width (String.sub final a (min b le - a))) ~h:4. color) chips;
      (* the wrong span of a diagnostic is underlined in the error colour *)
      List.iter (fun (_, span, _) -> match span with
        | Some (a, b) when a < le && b > ls && b > a && b <= String.length final ->
            let a = max a ls and b = min b le in
            Paint.fill paint ~x:(text_x +. width (String.sub final ls (a - ls))) ~y:(y +. row -. 3.)
              ~w:(Float.max 1. (width (String.sub final a (b - a)))) ~h:1. Theme.invalid
        | _ -> ()) messages;
      let line_str = String.sub final ls (le - ls) in
      if focused && line = caret_line && composition <> "" then begin
        let before = String.sub final ls (edit.caret - ls) in
        let caret_x = text_x +. width before in
        Paint.text paint ~at:(text_x, text_y) ~color:theme.foreground before;
        Paint.text paint ~at:(caret_x, text_y) ~color:theme.foreground composition;
        Paint.text paint ~at:(caret_x +. width composition, text_y) ~color:theme.foreground
          (String.sub line_str (String.length before) (String.length line_str - String.length before))
      end else if colors = [] then Paint.text paint ~at:(text_x, text_y) ~color:theme.foreground line_str
      else begin
        (* the row in coloured runs, the gaps in the foreground colour; ponytail: every row scans
           the whole colour list (visible rows x tokens), fine for workspace-sized text *)
        let at a = text_x +. width (String.sub final ls (a - ls)) in
        let run a b color = if a < b then Paint.text paint ~at:(at a, text_y) ~color (String.sub final a (b - a)) in
        let pos = List.fold_left (fun pos (a, b, color) ->
          let a = max a ls and b = min b le in
          if a >= b || a < pos then pos else (run pos a theme.foreground; run a b color; b)) ls colors in
        run pos le theme.foreground
      end
    done;
    (* the message rows: the diagnostic at the label size, under the indent of its line *)
    clip (bx +. gutter) (bw -. gutter);
    Array.iteri (fun k (after, _, message) ->
      let y = by -. offset +. pad +. (float (after + 1 + k) *. row) in
      if y +. row > by && y < by +. bh then begin
        let ls = start_of rows after in
        let indent = ref 0 in
        while ls + !indent < String.length final && final.[ls + !indent] = ' ' do incr indent done;
        Paint.text paint ~size:small ~at:(text_x +. width (String.make !indent ' '), text_top ui ~size:small y row)
          ~color:Theme.invalid message
      end) notes;
    clip (bx +. gutter) (bw -. gutter);
    if focused then begin
      let y = by -. offset +. pad +. (float (slot_of notes caret_line) *. row) in
      let caret_x = text_x +. width (String.sub final (start_of rows caret_line)
        (edit.caret - start_of rows caret_line)) in
      Paint.input_region paint ~x:bx ~y ~w:bw ~h:row ~focused:true ~cursor:(caret_x -. bx) ();
      Paint.fill paint ~x:(Float.round caret_x) ~y:(y +. 2.) ~w:1. ~h:(row -. 4.) theme.accent
    end else Paint.input_region paint ~x:bx ~y:by ~w:bw ~h:bh ~focused:false ();
    paint.clip_rect <- previous);
  ()

(* the completion popup, under the caret's row (above it near the bottom of the view) *)
let paint_completions ~slot_of ~popup ~signal_of ~first ~row ~notes ~pad ~offset ~by ~selected ~items ~x_at ~edit ~rows ~(theme : Theme.t) ui =
  Option.iter (fun (container, rows_boxes, width, height) ->
    let caret_line = row_at rows edit.caret in
    let anchor = x_at (fst items.(!selected).replace) in
    let px = Float.max 8. (Float.min (anchor -. 8.) (ui.view_w -. width -. 8.)) in
    let below = by -. offset +. pad +. (float (slot_of notes caret_line + 1) *. row) in
    let py = if below +. height <= ui.view_h -. 8. then below
      else Float.max 8. (by -. offset +. pad +. (float (slot_of notes caret_line) *. row) -. height) in
    set_at ui container ~at:(px, py);
    let selected = !selected in
    draw ui container (fun paint (x, y, w, h) ->
      Paint.fill paint ~x ~y ~w ~h theme.input;
      Paint.frame paint ~x ~y ~w ~h (Theme.edge theme);
      let doc = items.(selected).doc in
      if doc <> "" then
        Paint.text paint ~color:(Theme.muted theme)
          ~at:(x +. 8., y +. h -. 18.) (display doc));
    List.iteri (fun i r ->
      let c = items.(first + i) and hovered = (signal_of ui r).hovered in
      draw ui r (fun paint rect ->
        let (rx, ry, rw, rh) as bounds = ints rect in
        if first + i = selected then fill paint bounds theme.control
        else if hovered then hover_row paint ui bounds;
        kit_text paint ~color:theme.foreground (rx + side) (label_y ui ry rh) c.label;
        let dw = int_of_float (text_width ui c.detail) in
        kit_text paint ~color:(Theme.muted theme) (rx + rw - dw - side) (label_y ui ry rh) c.detail)) rows_boxes)
    popup;
  ()

(* a dragged number: the literal at the press follows the pointer *)
let apply_scrub ~scrubbing ~on_scrub ~suggest ~on_scrub_edit ~edit ui =
  Option.iter (fun (start, literal, dx, first) ->
    if start <= String.length edit.text && literal <> "" then begin
      (* the whole drag is one step of the editor's undo *)
      if first then remember ui edit ~typing:false;
      let stop = ref start in
      while !stop < String.length edit.text && numeric_character edit.text.[!stop] do incr stop done;
      let coarse = match ui.input_frame with
        | Some (frame : Frame.t) -> List.mem Input.Shift frame.input.keys | None -> false in
      let next = scrubbed literal dx ~coarse in
      Option.iter (fun f -> f (start, !stop) next) on_scrub_edit;
      edit.text <- String.sub edit.text 0 start ^ next
        ^ String.sub edit.text !stop (String.length edit.text - !stop);
      edit.caret <- start + String.length next; edit.anchor <- edit.caret;
      set_state ui suggest 0;
      Option.iter (fun f -> f `Live) on_scrub
    end) !scrubbing;
  ()

(* the keys of a focused text area: completion, pairs, indent, line edits, movement, typing *)
let handle_keys ~body ~stop_of ~page ~typed ~language ~suggest ~open_ ~accept ~selected ~chosen ~items ~submitted ~changed ~closed ~shown ~row_text ~start_of ~edit ~rows_of ui =
  List.iter (fun ((event : Input.event), modifiers) ->
    let command = command_modifiers modifiers and shift = List.mem Input.Shift modifiers in
    let rows = rows_of edit.text in
    let count = Array.length rows in
    let x_of line index = text_width ui
      (String.sub edit.text (start_of rows line) (index - start_of rows line)) in
    let line = row_at rows edit.caret in
    let move target = edit.caret <- target; if not shift then edit.anchor <- target in
    (* Up and Down keep the column they started from across shorter rows *)
    let vertical step =
      let target = line + step in
      let x = if ui.edit_column >= 0. then ui.edit_column else x_of line edit.caret in
      ui.edit_column <- x;
      if target < 0 then move 0
      else if target >= count then move (String.length edit.text)
      else move (start_of rows target + text_caret_at ui (row_text edit.text rows target) x) in
    (match event with
     | Input.Key_pressed (Input.ArrowUp | ArrowDown) when not command -> ()
     | Input.Key_pressed (Input.PageUp | PageDown | Shift | Ctrl | Alt | Meta) -> ()
     | _ -> ui.edit_column <- -1.);
    let listing = shown > 0 && not !closed in
    let change () = changed := true; remember ui edit ~typing:false in
    (match event with
    | Input.Key_pressed Input.Enter when command -> submitted := true
    (* the completion popup takes Up, Down, Tab, Enter and Escape while it is open *)
    | Input.Key_pressed ((Input.ArrowUp | Input.ArrowDown) as key) when listing ->
        let n = Array.length items in
        chosen := true;
        selected := (!selected + (if key = Input.ArrowUp then n - 1 else 1)) mod n
    | Input.Key_pressed Input.Tab when listing && not command -> accept items.(!selected)
    | Input.Key_pressed Input.Enter when listing && !chosen -> accept items.(!selected)
    | Input.Key_pressed Input.Escape when open_ <> 0 && not !closed -> set_state ui suggest 0; closed := true
    | Input.Key_pressed Input.Escape -> unfocus ui
    | Input.Key_pressed Input.Enter ->
        begin
          let indent = match language with
            | Some l -> l.indent edit.text (fst (text_selection edit)) | None -> "" in
          change (); replace_text edit ("\n" ^ indent);
          if open_ <> 0 then (set_state ui suggest 0; closed := true)
        end
    (* brackets come in pairs: an opener wraps the selection or inserts both, a closer typed
       before itself steps over it, Backspace between an empty pair takes both *)
    | Input.Text_input s when language <> None && String.length s = 1 && not command ->
        let c = s.[0] and pairs = (Option.get language).pairs in
        let start, stop = text_selection edit in
        let next = if edit.caret < String.length edit.text then Some edit.text.[edit.caret] else None in
        (* a bracket is a bracket only in code: in a string only a quote steps over its closing
           quote, and a comment takes every character as typed; in code a quote before a quote
           is the next string's opening one *)
        let context = (Option.get language).context_at edit.text edit.caret in
        let quote = List.mem (c, c) pairs in
        let step_over = next = Some c && (match context with
          | `Code -> List.exists (fun (_, close) -> close = c) pairs && not quote
          | `String -> quote
          | `Comment -> false) in
        (match context, List.assoc_opt c pairs with
         | `Code, Some close when start <> stop ->
             change ();
             replace_text edit (String.make 1 c ^ String.sub edit.text start (stop - start) ^ String.make 1 close);
             edit.anchor <- start + 1; edit.caret <- stop + 1
         | _ when step_over ->
             edit.caret <- edit.caret + 1; edit.anchor <- edit.caret
         | `Code, Some close ->
             change ();
             replace_text edit (String.make 1 c ^ String.make 1 close);
             edit.caret <- edit.caret - 1; edit.anchor <- edit.caret
         | _ -> changed := edit_text_event ~multiline:true ui edit ~accept:(fun _ -> true) ~modifiers event || !changed);
        typed := true
    | Input.Key_pressed Input.Backspace when language <> None && not command
        && edit.caret = edit.anchor && edit.caret > 0 && edit.caret < String.length edit.text
        && List.mem (edit.text.[edit.caret - 1], edit.text.[edit.caret]) (Option.get language).pairs ->
        change (); edit.anchor <- edit.caret - 1; edit.caret <- edit.caret + 1; replace_text edit ""; typed := true
    (* Command-X / C with no selection take the caret's whole logical line, newline included *)
    | Input.Key_pressed (Input.KeyChar ('x' | 'X' | 'c' | 'C' as key)) when command && edit.caret = edit.anchor ->
        let s = line_start edit.text edit.caret in
        let e = min (String.length edit.text) (line_end edit.text edit.caret + 1) in
        if Clipboard.set_text (String.sub edit.text s (e - s)) = Ok ()
           && (key = 'x' || key = 'X') then begin
          change (); edit.anchor <- s; edit.caret <- e; replace_text edit ""; typed := true
        end
    | Input.Key_pressed Input.Tab when not command && edit.caret <> edit.anchor ->
        (* Tab and Shift-Tab with a selection indent and dedent every line it touches *)
        let a, b = text_selection edit in
        let first, stop = line_block edit.text a b in
        let block = String.sub edit.text first (stop - first) in
        let shifted = String.concat "\n" (List.map (fun l ->
          if not shift then (if l = "" then l else "  " ^ l)
          else if String.starts_with ~prefix:"  " l then String.sub l 2 (String.length l - 2)
          else if String.starts_with ~prefix:" " l then String.sub l 1 (String.length l - 1) else l)
          (String.split_on_char '\n' block)) in
        if shifted <> block then begin
          change (); replace_block edit first stop shifted;
          keep_selection edit first (first + String.length shifted)
        end
    | Input.Key_pressed Input.Tab when not command ->
        if not shift then (change (); replace_text edit "  ")
        else begin
          (* Shift-Tab: up to two spaces leave the start of the line *)
          let line_start = line_start edit.text edit.caret in
          let spaces = if line_start < String.length edit.text && edit.text.[line_start] = ' '
            then (if line_start + 1 < String.length edit.text && edit.text.[line_start + 1] = ' ' then 2 else 1)
            else 0 in
          if spaces > 0 then begin
            change ();
            edit.text <- String.sub edit.text 0 line_start
              ^ String.sub edit.text (line_start + spaces) (String.length edit.text - line_start - spaces);
            edit.caret <- max line_start (edit.caret - spaces); edit.anchor <- edit.caret
          end
        end
    (* line and form edits: Command-/ toggles comments, Option-Up and Down move the lines, with
       Shift duplicate them, Command-Option-Up selects the enclosing form, Command-Shift-\\ jumps
       to the matching bracket *)
    | Input.Key_pressed (Input.KeyChar '/') when command && language <> None ->
        let a, b = text_selection edit in
        let first, stop = line_block edit.text a b in
        let block = String.sub edit.text first (stop - first) in
        let toggled = toggle_comment (Option.get language).comment block in
        if toggled <> block then begin
          change (); replace_block edit first stop toggled;
          let last = first + String.length toggled in
          if a = b then (edit.caret <- max first (a + last - stop); edit.anchor <- edit.caret)
          else keep_selection edit first last
        end
    | Input.Key_pressed (Input.ArrowUp | Input.ArrowDown as key) when List.mem Input.Alt modifiers && not command ->
        let a, b = text_selection edit in
        let first, stop = line_block edit.text a b in
        let n = String.length edit.text in
        let block = String.sub edit.text first (stop - first) in
        let up = key = Input.ArrowUp in
        let shift_by d = edit.anchor <- edit.anchor + d; edit.caret <- edit.caret + d in
        if shift then begin
          (* a copy below keeps the selection on the copy, a copy above on the original *)
          change ();
          replace_block edit stop stop ("\n" ^ block);
          if not up then shift_by (String.length block + 1)
        end else if up && first > 0 then begin
          let before = line_start edit.text (first - 1) in
          change ();
          replace_block edit before stop (block ^ "\n" ^ String.sub edit.text before (first - 1 - before));
          shift_by (before - first)
        end else if not up && stop < n then begin
          let after = line_end edit.text (stop + 1) in
          change ();
          replace_block edit first after (String.sub edit.text (stop + 1) (after - stop - 1) ^ "\n" ^ block);
          shift_by (after - stop)
        end
    | Input.Key_pressed Input.ArrowUp when command && List.mem Input.Alt modifiers ->
        let a, b = text_selection edit in
        (match language with
         | Some l ->
             Option.iter (fun (o, c) -> edit.anchor <- o; edit.caret <- c) (enclosing_pair (l.brackets edit.text) a b)
         | None -> ())
    | Input.Key_pressed (Input.KeyChar '\\') when command && shift && language <> None ->
        Option.iter (fun at -> edit.caret <- at; edit.anchor <- at)
          (bracket_jump ((Option.get language).brackets edit.text) edit.caret)
    | Input.Key_pressed Input.ArrowUp when command -> move 0
    | Input.Key_pressed Input.ArrowDown when command -> move (String.length edit.text)
    | Input.Key_pressed Input.ArrowUp -> vertical (-1)
    | Input.Key_pressed Input.ArrowDown -> vertical 1
    | Input.Key_pressed Input.PageUp -> vertical (-page)
    | Input.Key_pressed Input.PageDown -> vertical page
    | Input.Key_pressed (Input.Home | Input.ArrowLeft) when command || event = Input.Key_pressed Input.Home ->
        move (start_of rows line)
    | Input.Key_pressed (Input.End | Input.ArrowRight) when command || event = Input.Key_pressed Input.End ->
        move (stop_of rows line)
    | event ->
        if edit_text_event ~multiline:true ui edit ~accept:(fun _ -> true) ~modifiers event then begin
          changed := true;
          (match event with
           | Input.Text_input _ | Input.Key_pressed (Input.Backspace | Input.Delete) -> typed := true
           | _ -> ())
        end))
    (key_events ui body);
  ()

(* a press on a number arms a scrub; three points sideways make it one *)
let arm_scrub ~scrubbing ~scrub_state ~on_scrub ~point_at ~text ~on_handle ~signal ~language ~scrub ui =
  (* a press on a number arms a scrub (its state is the start + 1, negated once dragging); three
     points sideways make it one, three points down a selection as before *)
  (match language with
   | Some l when signal.pressed && signal.button = Some Input.LeftButton ->
       (match (if on_handle signal.press_point then l.number_at text (point_at signal.press_point) else None) with
        | Some (a, b) -> set_state ui scrub (a + 1); set_text_state ui scrub (Some (String.sub text a (b - a)))
        | None -> set_state ui scrub 0)
   | Some _ when scrub_state <> 0 && signal.held ->
       let dx = fst signal.pointer -. fst signal.press_point
       and dy = snd signal.pointer -. snd signal.press_point in
       if scrub_state < 0 || (Float.abs dx >= 3. && Float.abs dx >= Float.abs dy) then begin
         let start = abs scrub_state - 1 in
         set_state ui scrub (- (start + 1));
         scrubbing := Some (start, Option.value ~default:"" (text_state ui scrub), dx, scrub_state > 0)
       end else if Float.abs dy >= 3. then set_state ui scrub 0
   | _ when scrub_state <> 0 && not signal.held ->
       set_state ui scrub 0;
       if scrub_state < 0 then Option.iter (fun f -> f `Done) on_scrub
   | _ -> ());
  ()

let text_area_submit ui ~at ~w ~h ?(wrap = false) ?(errors = []) ?(messages = []) ?(spans = [])
    ?reveal ?(reveal_nonce = 0) ?select ?(splice = []) ?language ?on_context ?on_scrub ?on_scrub_edit ?on_click ?on_caret
    ?on_caret_move ?on_select ?on_drop ?(chips = []) label text =
  (* a line of code is one and a half times its text, as code editors set it (17 points at 11,
     20 at 13): the 24-point row is a control's, twice the text *)
  let row = float (text_line_height ui) in
  let body = box ui
      ~flags:(clickable lor focusable lor blocking lor scroll lor clip
              lor keep_tab)
      ~at ~w:(Px w) ~h:(Px h) ~scroll_step:row label in
  (* a [select] request takes the keyboard for the area *)
  if select <> None then focus ui body;
  let char_w = text_width ui "0" in
  (* the sheet's code area: a 36-point gutter (wider only past 999 lines), the text at its edge,
     6 points of padding above the first row *)
  let gutter_of lines = 36. +. char_w *. float (max 0 (String.length (string_of_int lines) - 3)) in
  let pad = 6. in
  let columns_of count =
    if not wrap then None
    else
      let gutter = gutter_of count in
      Some (int_of_float (Float.floor (Float.max 1. (w -. gutter -. 16.) /. Float.max 1. char_w))) in
  (* the gutter width follows the number of logical lines, the wrap width follows the gutter *)
  let rows_of text =
    let logical = 1 + String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 text in
    text_rows ?cols:(columns_of logical) text in
  let rows = rows_of text in
  let count = Array.length rows in
  (* a diagnostic's message is a row of its own under the last row of its line: [note_rows] are
     (the display row it follows, the wrong span, the message), in order; a row's slot counts
     the message rows above it *)
  let note_rows rows = if messages = [] then [||] else begin
    let found = List.filter_map (fun (line, span, message) ->
      let last = ref (-1) in
      Array.iteri (fun i (_, _, logical) -> if logical = line - 1 then last := i) rows;
      if !last >= 0 then Some (!last, span, message) else None) messages in
    Array.of_list (List.stable_sort (fun (a, _, _) (b, _, _) -> Int.compare a b) found)
  end in
  let slot_of notes i = Array.fold_left (fun n (after, _, _) -> if after < i then n + 1 else n) i notes in
  let row_at_slot notes count k =
    if notes = [||] then max 0 (min (count - 1) k) else begin
      let i = ref 0 in
      while !i + 1 < count && slot_of notes (!i + 1) <= k do incr i done; !i
    end in
  let logical_count rows = let _, _, l = rows.(Array.length rows - 1) in l + 1 in
  let start_of rows i = let s, _, _ = rows.(i) in s in
  let stop_of rows i = let _, e, _ = rows.(i) in e in
  let row_text text rows i = String.sub text (start_of rows i) (stop_of rows i - start_of rows i) in
  (* the content box scrolls; two empty boxes keep the completion popup's and the number
     scrub's state between frames *)
  let content, suggest, scrub = within ui body (fun () ->
    let content = box ui ~w:(Px w)
        ~h:(Px ((float (count + Array.length (note_rows rows)) *. row) +. pad)) (label ^ "-content") in
    let suggest = box ui ~w:(Px 0.) ~h:(Px 0.) ~at:(0., 0.) (label ^ "-suggest") in
    let scrub = box ui ~w:(Px 0.) ~h:(Px 0.) ~at:(0., 0.) (label ^ "-scrub") in
    content, suggest, scrub) in
  let signal_of = signal in
  let signal = signal ui body in
  (* the I-beam over text; a number under the pointer asks for the scrub
     cursor further down, and the last request wins *)
  if signal.hovered then request_cursor ui `Text;
  let focused = focused ui body in
  let gutter = gutter_of (logical_count rows) in
  let bx, by, bw, bh = rect ui body in
  let horizontal = ref (float (state ui body ~default:0)) in
  (* the byte at a point; [strict] answers only over the row's glyphs *)
  let point_in ?(strict = false) text rows (px, py) =
    let count = Array.length rows in
    let line = row_at_slot (note_rows rows) count
      (int_of_float (Float.floor ((py -. by +. scroll_position ui body -. pad) /. row))) in
    let x = px -. bx -. gutter +. !horizontal in
    let line_text = row_text text rows line in
    if strict && (x < 0. || x > text_width ui line_text || py < by || py > by +. bh) then None
    else Some (start_of rows line + text_caret_at ui line_text (Float.max 0. x)) in
  let point_at point = Option.get (point_in text rows point) in
  (* a number is dragged by its underline: the lowest 9 points of its row, so a drag that selects
     text from a number does not rewrite it *)
  let on_handle (_, py) =
    let y = py -. by +. scroll_position ui body -. pad in
    y -. (row *. Float.floor (y /. row)) >= row -. 9. in
  let edit = if focused then load_text_edit ui body.box_key text
    else { text; caret = String.length text; anchor = String.length text } in
  let moved = ref false and submitted = ref false in
  (match on_context with
   | Some f when context_clicked signal -> f signal.release_point
   | _ -> ());
  let scrub_state = state ui scrub ~default:0 in
  let scrubbing = ref None in
  arm_scrub ~scrub ~language ~signal ~on_handle ~text ~point_at ~on_scrub ~scrub_state ~scrubbing ui;
  (* the completion popup: its state is (token start + 1) * 512 + 256 once an arrow has moved
     the selection + the selected row; the rows are built before the keys so a click on one is
     seen this frame *)
  let open_ = if focused then state ui suggest ~default:0 else (set_state ui suggest 0; 0) in
  let items = match language with
    | Some l when open_ <> 0 -> Array.of_list (l.complete edit.text edit.caret)
    | _ -> [||] in
  let shown = min 8 (Array.length items) in
  let selected = ref (min (open_ land 255) (max 0 (Array.length items - 1))) in
  (* Enter takes the row only after Up or Down chose it; Tab takes it always *)
  let chosen = ref (open_ land 256 <> 0) in
  let first = max 0 (!selected - shown + 1) in
  let closed = ref false in
  let accept (c : completion) =
    remember ui edit ~typing:false;
    let a, b = c.replace in
    let a = max 0 (min a (String.length edit.text)) and b = max 0 (min b (String.length edit.text)) in
    edit.text <- String.sub edit.text 0 a ^ c.insert ^ String.sub edit.text b (String.length edit.text - b);
    edit.caret <- a + String.length c.insert; edit.anchor <- edit.caret;
    set_state ui suggest 0; closed := true in
  let popup = if shown = 0 then None else begin
    let doc = items.(!selected).doc in
    let width = Array.fold_left (fun w (c : completion) ->
      Float.max w (text_width ui c.label +. text_width ui c.detail +. 44.)) 220. items in
    let width = Float.min width (Float.max 120. (ui.view_w -. 16.)) in
    let height = (float (shown * ui.kit_row_height)) +. (if doc = "" then 4. else 22.) in
    let container = overlay_box ui ~flags:blocking ~at:(0., 0.) ~w:width ~h:height (label ^ "-completions") in
    let boxes = within ui container (fun () ->
      List.init shown (fun i ->
        kit_row ui ~flags:(clickable lor blocking lor keep_focus) (Printf.sprintf "row-%d" i))) in
    List.iteri (fun i r -> if (signal_of ui r).clicked then accept items.(first + i)) boxes;
    Some (container, boxes, width, height)
  end in
  if focused then begin
    let caret0 = edit.caret in
    (* a triple click takes the logical line with its break *)
    let line_of at =
      let s = line_start edit.text at and e = line_end edit.text at in
      s, (if e < String.length edit.text then e + 1 else e) in
    if signal.pressed && signal.button = Some Input.LeftButton then
      begin
        ui.edit_column <- -1.;
        press_select ui edit ~shift:(press_shift ui body) ~clicks:signal.clicks ~line_of
          (point_at signal.press_point);
        (* a double click on true / false flips it, as dragging flips a number *)
        if signal.clicks = 2 && language <> None then begin
          let a, b = text_selection edit in
          let flipped = match String.sub edit.text a (b - a) with
            | "true" -> Some "false" | "false" -> Some "true" | _ -> None in
          Option.iter (fun word ->
            remember ui edit ~typing:false;
            replace_text edit word; edit.anchor <- a; edit.caret <- a + String.length word) flipped
        end
      end
    else if !scrubbing = None && scrub_state = 0
        && (signal.dragging || (signal.held && signal.pointer <> signal.press_point)) then
      drag_select ui edit ~line_of (point_at signal.pointer);
    let typed = ref false and changed = ref false in
    (* the host's replacements (one undo step, last first so the offsets hold), then its selection *)
    if splice <> [] then begin
      remember ui edit ~typing:false;
      List.iter (fun (a, b, by) ->
        if 0 <= a && a <= b && b <= String.length edit.text then
          edit.text <- String.sub edit.text 0 a ^ by ^ String.sub edit.text b (String.length edit.text - b))
        (List.sort (fun (a, _, _) (b, _, _) -> Int.compare b a) splice);
      changed := true
    end;
    Option.iter (fun (a, b) ->
      let n = String.length edit.text in
      edit.anchor <- max 0 (min n a); edit.caret <- max 0 (min n b)) select;
    let page = max 1 (int_of_float (bh /. row) - 1) in
    handle_keys ~rows_of ~edit ~start_of ~row_text ~shown ~closed ~changed ~submitted ~items ~chosen ~selected ~accept ~open_ ~suggest ~language ~typed ~page ~stop_of ~body ui;
    (* a dragged number: the literal at the press follows the pointer (the caret sits after it) *)
    apply_scrub ~edit ~on_scrub_edit ~suggest ~on_scrub ~scrubbing ui;
    (* typing opens the popup on the token at the caret (the selection starts over); a popup
       left open follows the caret and closes when the caret leaves its token *)
    (match language with
     | Some l when (!typed || (open_ <> 0 && not !closed)) ->
         (match l.complete edit.text edit.caret with
          | c :: _ when !typed -> set_state ui suggest ((fst c.replace + 1) * 512)
          | c :: _ when fst c.replace = (open_ lsr 9) - 1 ->
              set_state ui suggest ((open_ lsr 9) * 512 + (if !chosen then 256 else 0) + !selected)
          | _ -> set_state ui suggest 0)
     | _ -> ());
    (* the language's rewrite (parinfer) after this frame's edits; the anchor maps like the caret *)
    (match language with
     | Some { rewrite = Some rewrite; _ } when !changed ->
         let text', caret' = rewrite edit.text edit.caret in
         let anchor' = if edit.anchor = edit.caret then caret' else snd (rewrite edit.text edit.anchor) in
         edit.text <- text'; edit.caret <- caret'; edit.anchor <- anchor'
     | _ -> ());
    save_text_edit ui edit;
    moved := edit.caret <> caret0 || edit.text != text || signal.pressed
  end;
  (* scrolling: the wheel, then whatever keeps the caret (or [reveal]) in view *)
  let final = edit.text in
  if focused then Option.iter (fun f -> f edit.caret) on_caret;
  if focused then Option.iter (fun f -> f (text_selection edit)) on_select;
  if focused && !moved && (signal.pressed || signal.dragging || key_events ui body <> []) then
    Option.iter (fun f -> f edit.caret) on_caret_move;
  (match on_click with
   | Some f when signal.clicked && signal.button = Some Input.LeftButton && scrub_state >= 0 ->
       Option.iter (fun byte -> f byte (command_modifiers (press_keys ui body)))
         (point_in ~strict:true text rows signal.release_point)
   | _ -> ());
  (* a payload in flight over the area: the byte under the pointer, hovering or released *)
  (match on_drop with
   | Some f ->
       Option.iter (fun drop -> Option.iter (fun byte -> f byte drop) (point_in text rows signal.pointer))
         (drop_target ui body)
   | None -> ());
  let rows = if final == text then rows else rows_of final in
  let count = Array.length rows in
  let notes = note_rows rows in
  (* [reveal] scrolls once per (offset, length, nonce): the content box remembers it, and forgets
     it while there is nothing to reveal *)
  let revealed = match reveal with
    | Some index when bw > 0.
        && state ui content ~default:0 <> (index * 1_000_003) + String.length final + 1 + (reveal_nonce * 1_000_000_007) ->
        set_state ui content ((index * 1_000_003) + String.length final + 1 + (reveal_nonce * 1_000_000_007)); Some index
    | None -> set_state ui content 0; None
    | _ -> None in
  let target = if !moved then Some edit.caret
    else Option.map (fun index -> min (String.length final) index) revealed in
  let longest = ref 0 in
  Array.iteri (fun i _ -> longest := max !longest (stop_of rows i - start_of rows i)) rows;
  let visible = Float.max 1. (bw -. gutter -. 16.) in
  let max_x = if wrap then 0. else Float.max 0. (float !longest *. char_w -. visible) in
  horizontal := Float.max 0. (Float.min max_x (!horizontal +. fst signal.scroll *. row));
  let vertical = ref (Float.max 0. (Float.min (Float.max 0. ((float (count + Array.length notes) *. row) +. pad -. bh))
    (scroll_offset ui body))) in
  Option.iter (fun index ->
    let line = row_at rows index in
    let top = pad +. (float (slot_of notes line) *. row) in
    if top < !vertical then vertical := top
    else if top +. row > !vertical +. bh then vertical := top +. row -. bh;
    let x = text_width ui (String.sub final (start_of rows line) (min index (stop_of rows line) - start_of rows line)) in
    if x < !horizontal then horizontal := x
    else if x -. !horizontal > visible then horizontal := x -. visible) target;
  set_state ui body (int_of_float !horizontal);
  if !vertical <> scroll_offset ui body then set_scroll_offset ui body !vertical;
  (* painted where the shared elastic scroll puts the content, overshoot included *)
  let offset = scroll_position ui body and horizontal = !horizontal in
  let theme = ui.theme and composition = ui.composition in
  (* the language's colours and the bracket pair at the caret, once per frame *)
  let colors = match language with Some l -> l.colorize final | None -> [] in
  let matched = match language with
    | Some l when focused ->
        let c = edit.caret in
        List.find_opt (fun (o, k) -> o = c - 1 || k = c - 1 || o = c || k = c) (l.brackets final)
    | _ -> None in
  (* the pointer over a number invites a drag; over any other token it describes it after a rest *)
  let under_pointer = match language with
    | Some _ when signal.hovered && ui.active = None ->
        point_in ~strict:true final rows signal.pointer
    | _ -> None in
  let hovered_number = match language, under_pointer, !scrubbing with
    | _, _, Some (start, _, _, _) ->
        let stop = ref start in
        while !stop < String.length final && numeric_character final.[!stop] do incr stop done;
        Some (start, !stop)
    | Some l, Some byte, None when on_handle signal.pointer -> l.number_at final byte
    | _ -> None in
  if hovered_number <> None then request_cursor ui `Horizontal_resize;
  (match language, under_pointer with
   | Some l, Some byte when hovered_number = None && open_ = 0 ->
       Option.iter (fun (start, _, doc) ->
         if doc <> "" then tooltip ui ~key:(Printf.sprintf "%s#%d" label start) ~text:doc)
         (l.describe final byte)
   | _ -> ());
  (* the completion popup sits under the caret's row (above it near the bottom of the view) *)
  let text_x0 = bx +. gutter -. horizontal in
  let x_at index =
    let line = row_at rows index in
    text_x0 +. text_width ui (String.sub final (start_of rows line) (index - start_of rows line)) in
  paint_completions ~theme ~rows ~edit ~x_at ~items ~selected ~by ~offset ~pad ~notes ~row ~first ~signal_of ~popup ~slot_of ui;
  paint_text_area ~body ~theme ~gutter ~horizontal ~offset ~pad ~row ~notes ~count ~rows ~edit ~focused ~start_of ~stop_of ~final ~char_w ~notes ~slot_of ~errors ~spans ~matched ~scrubbing ~hovered_number ~chips ~messages ~composition ~colors ui;
  final, !submitted

type pick = [ `None | `Pick of int | `Delete of int | `Submit | `Back | `Cancel ]

(* ponytail: case-insensitive subsequence match, no scoring; swap for
   fzf-style ranking if lists grow long. *)
let fuzzy_match ~query text =
  let query = String.lowercase_ascii query and text = String.lowercase_ascii text in
  let length = String.length text in
  let rec walk at index =
    index = String.length query
    || (at < length && walk (at + 1) (if text.[at] = query.[index] then index + 1 else index)) in
  walk 0 0

(* A focused search row over a windowed list. The cursor (search box state)
   and an armed delete row (list box state) are retained; the host keeps the
   query and applies the result. *)
let picker ui ?(limit = 10) ?mark ?(off = fun _ -> false) ?(slash = true) ?(at_rest = false) label ~query rows_of =
  let rows = ref (rows_of query) in
  let count () = Array.length !rows in
  (* the search row: a 20-point field in 4 points of padding, a [/] before the query *)
  let search = box ui ~flags:(clickable lor focusable lor blocking lor focus_mark) ~w:Grow ~h:(Px 28.) label in
  if ui.focus = 0 || ui.rw.(search.box_slot) = 0. then focus ui search;
  let list = box_keyed ui ~w:Grow ~h:Fit ~axis:Column (int_key search.box_key 0) in
  let search_signal = signal ui search in
  if search_signal.hovered then request_cursor ui `Text;
  let keys = key_events ui search in
  let clamp cursor = if count () = 0 then 0 else max 0 (min (count () - 1) cursor) in
  let cursor = ref (clamp (state ui search ~default:0))
  and armed = ref (state ui list ~default:(-1)) and query = ref query
  and result = ref `None in
  let edit = load_text_edit ui search.box_key !query in
  let (sx, _, sw, _) = ints (rect ui search) in
  let prefix = if slash then 13 else 0 in
  (* the slash is 6.5 wide and 6 of gap: the query starts 14.5 in from the field's edge *)
  let inset = if slash then 14.5 -. float prefix else 2. in
  point_text_caret ui edit search_signal ~shift:(press_shift ui search)
    ~x:(float (sx + 4 + prefix) +. inset) ~right:(float (sx + sw - 4 - 2));
  let set_query text = query := text; rows := rows_of text; cursor := 0; armed := -1 in
  List.iter (fun ((event : Input.event), modifiers) -> let count = count () in
    if !result = `None then match event with
    | Input.Key_pressed Input.Backspace when edit.text = "" -> result := `Back
    | Input.Key_pressed Input.ArrowLeft when edit.text = "" -> result := `Back
    | Input.Key_pressed Input.ArrowDown when count > 0 ->
        cursor := (!cursor + 1) mod count; armed := -1
    | Input.Key_pressed Input.ArrowUp when count > 0 ->
        cursor := (!cursor + count - 1) mod count; armed := -1
    | Input.Key_pressed Input.Enter ->
        result := if count > 0 then (if off !cursor then `None else `Pick !cursor) else `Submit
    | Input.Key_pressed Input.Delete when count > 0 &&
        edit.caret = edit.anchor ->
        if !armed = !cursor then (result := `Delete !cursor; armed := -1)
        else armed := !cursor
    | Input.Key_pressed Input.Escape -> result := `Cancel
    | event ->
        ignore (edit_text_event ui edit ~modifiers ~accept:(fun _ -> true) event);
        if edit.text <> !query then set_query edit.text) keys;
  save_text_edit ui edit;
  let count = count () and rows = !rows in
  let length = min limit count in
  let start = if length = count then 0
    else max 0 (min (count - length) (!cursor - (length / 2))) in
  let theme = ui.theme and composition = ui.composition in
  let placeholder = display label in
  draw ui search (fun paint rect ->
    let (x, y, w, h) = ints rect in
    let (cx, cy, cw, ch) as field = x + 4, y + 4, max 1 (w - 8), max 1 (h - 8) in
    (* a field that has not been typed in yet can sit at rest: the hairline and no caret *)
    let resting = at_rest && edit.text = "" in
    underline paint field (if resting then Theme.edge theme else theme.accent);
    if slash then kit_text paint ~color:(Theme.ink_2 theme) (cx + 2) (label_y ui cy ch) "/";
    let control = cx + prefix, cy, max 1 (cw - prefix), ch in
    if edit.text = "" then
      Paint.text paint ~color:(Theme.ink_3 theme) ~at:(float (cx + prefix) +. inset, float (label_y ui cy ch))
        placeholder;
    paint_text_edit paint ~inset ~caret:(edit.text <> "") ~control ~y:(label_y ui cy ch) ~composition edit);
  within ui list (fun () ->
    for visible = 0 to length - 1 do
      let index = start + visible in
      (* a row that cannot be picked leaves the keyboard with the search field *)
      let row = box_keyed ui
          ~flags:(clickable lor focusable lor blocking lor (if off index then keep_focus else 0))
          ~w:Grow ~h:(Px (float ui.kit_row_height)) (int_key list.box_key visible) in
      let row_signal = signal ui row in
      if !result = `None && row_signal.clicked && not (off index) then result := `Pick index;
      let current = index = !cursor and hovered = row_signal.hovered in
      let doomed = index = !armed in
      let text, detail = rows.(index) in
      let swatch = Option.bind mark (fun mark -> mark index) in
      draw ui row (fun paint rect ->
        let (x, y, w, h) as bounds = ints rect in
        (* the keyboard cursor is the control fill; a row armed for deletion reads in the error colour *)
        if doomed then fill paint bounds (Theme.pressed_fill theme)
        else if current then fill paint bounds theme.control
        else if hovered then hover_row paint ui bounds;
        let text_x = if mark = None then x + side else x + side + 16 in
        let unavailable = off index in
        (* a label that would run into the detail is cut with an ellipsis *)
        let key_size = max 8 (ui.font_size - 2) in
        let key_text = "\xe2\x86\xb5" in
        let right = if current then
            float (x + w - side) -. Paint.text_width paint ~size:key_size key_text -. 8.
          else float (x + w - side) in
        let text = ellipsis ~width:(Paint.text_width paint)
            ~limit:(right -. Paint.text_width paint detail -. 12. -. float text_x) text in
        Option.iter (fun color -> fill paint (x + side, y + ((h - 8) / 2), 8, 8)
          (if unavailable then Theme.ink_3 theme else color)) swatch;
        (* the query's letters, matched in order, in the accent *)
        let query = String.lowercase_ascii !query and lower = String.lowercase_ascii text in
        let matched = Array.make (String.length text) false in
        let next = ref 0 and at = ref 0 in
        while !at < String.length text do
          let decoded = String.get_utf_8_uchar text !at in
          let length = Uchar.utf_decode_length decoded in
          if !next < String.length query && String.length lower > !at && lower.[!at] = query.[!next]
          then (matched.(!at) <- true; incr next);
          at := !at + length
        done;
        let pen = ref (float text_x) and start = ref 0 in
        while !start < String.length text do
          let flag = matched.(!start) in
          let stop = ref !start in
          while !stop < String.length text
                && (!stop = !start || matched.(!stop) = flag || Char.code text.[!stop] land 0xc0 = 0x80) do
            incr stop
          done;
          let run = String.sub text !start (!stop - !start) in
          Paint.text paint ~at:(!pen, float (label_y ui y h))
            ~color:(if doomed then Theme.invalid else if unavailable then Theme.ink_3 theme
              else if flag then theme.accent else theme.foreground) run;
          pen := !pen +. Paint.text_width paint run;
          start := !stop
        done;
        let right = if current then
            (let width = Paint.text_width paint ~size:key_size key_text in
             Paint.text paint ~size:key_size ~color:(Theme.ink_3 theme)
               ~at:(float (x + w - side) -. width, text_top ui ~size:key_size (float y) (float h)) key_text;
             float (x + w - side) -. width -. 8.)
          else float (x + w - side) in
        Paint.text paint ~color:(if unavailable then Theme.ink_3 theme else Theme.muted theme)
          ~at:(right -. Paint.text_width paint detail, float (label_y ui y h)) detail)
    done);
  set_state ui search !cursor; set_state ui list !armed;
  !query, !result

(* A floating kit menu at [at], kept inside the frame. The host holds whether
   it is open; rows commit on press and release inside, disabled rows are
   inert, and Escape, focus loss, or a press outside dismiss it. *)
type submenu = { row : int; rows : (string * bool) list; keys : string list; current : int option }

let context_menu ui ~at:(x, y) ?width ?selected ?(swatches = []) ?(keys = []) ?(danger = [])
    ?(submenus = []) ?(lead_from = 0) label items =
  let submenu_key = key_of (current_seed ui) (label ^ "-sub") in
  (* the width follows the longest row; an empty label is a separator line; [keys] are the
     shortcuts at the right of the rows, in ink-2 at the label size *)
  let row_height = float ui.kit_row_height and gap = 9. in
  let key_size = max 8 (ui.font_size - 2) in
  let key_of keys index = match List.nth_opt keys index with Some key when key <> "" -> Some key | _ -> None in
  let swatch_pad = if List.exists Option.is_some swatches then 14. else 0. in
  let measure ~lead ~keys ~chevrons base items =
    (* the 1-point edge, 12 points of padding, then the label, 24 points and the key or chevron *)
    fst (List.fold_left (fun (w, index) (text, _) ->
      Float.max w (text_width ui text +. 26. +. lead +. swatch_pad
        +. (match key_of keys index with
            | Some key -> 24. +. text_width ui ~size:key_size key
            | None -> if List.mem index chevrons then 32.5 else 0.)),
      index + 1) (base, 0) items) in
  let height_of items = List.fold_left (fun h (text, _) -> h +. (if text = "" then gap else row_height)) 14. items in
  let lead = if selected = None then 0. else 14. in
  let width = Float.min ui.view_w (measure ~lead ~keys ~chevrons:(List.map (fun sub -> sub.row) submenus)
    (Option.value ~default:150. width) items) in
  let height = height_of items in
  let x = Float.max 0. (Float.min x (ui.view_w -. width))
  and y = Float.max 0. (Float.min y (ui.view_h -. height)) in
  (* the submenu of a row: a second menu overlapping this one by a point, level with its row *)
  let row_top index = y +. 7. +. List.fold_left (fun top (text, _) -> top +. (if text = "" then gap else row_height))
      0. (List.take (max 0 index) items) in
  let sub_geometry sub =
    let sub_width = Float.min ui.view_w (measure ~lead:(if sub.current = None then 0. else 14.) ~keys:sub.keys
      ~chevrons:[] 0. sub.rows) in
    let sub_height = height_of sub.rows in
    let sx = if x +. width -. 1. +. sub_width <= ui.view_w then x +. width -. 1. else x -. sub_width +. 1. in
    let sy = Float.max 0. (Float.min (row_top sub.row -. 7.) (ui.view_h -. sub_height)) in
    sx, sy, sub_width, sub_height in
  (* the rows of the submenus are numbered after those of this menu, in order *)
  let bases = fst (List.fold_left (fun (acc, n) sub -> (sub, n) :: acc, n + List.length sub.rows)
    ([], List.length items) submenus) in
  let base_of sub = List.assq sub bases in
  let pad name = box ui ~flags:blocking ~w:Grow ~h:(Px 6.) name in
  (* the rows of one menu: the row picked, and the row the pointer is on *)
  let rows_of ~lead_from ~items ~keys ~selected ~swatches ~danger ~opened ~chevrons =
    let hovered = ref None in
    let lead_of index = if selected = None || index < lead_from then 0 else 14 in
    let picked = List.mapi (fun index (text, enabled) ->
      if text = "" then begin
        let line = box ui ~flags:blocking ~w:Grow ~h:(Px gap) (Printf.sprintf "separator-%d" index) in
        draw ui line (fun paint (x, y, w, _) ->
          Paint.fill paint ~x ~y:(y +. 4.) ~w ~h:1. (Theme.edge ui.theme));
        None
      end else begin
        let row = kit_row ui ~flags:(if enabled then clickable lor focusable lor blocking lor tab_only else blocking)
            text in
        let signal = signal ui row in
        if enabled && signal.hovered then hovered := Some index;
        let theme = ui.theme and shown = display text in
        let chevron = List.mem index chevrons in
        draw ui row (fun paint rect ->
          let (_, y, _, h) as bounds = ints rect in
          let rx, _, _, _ = bounds in
          if enabled && (signal.hovered || opened = Some index) then fill paint bounds theme.control;
          (* the current choice: a 6-point accent square before the label *)
          if selected = Some index then
            Paint.fill paint ~x:(float rx +. float side) ~y:(float y +. (float h /. 2.) -. 3.) ~w:6. ~h:6. theme.accent;
          Option.iter (fun color ->
            let sx = float (rx + side + lead_of index) and sy = float y +. float h /. 2. -. 4. in
            Paint.fill paint ~x:sx ~y:sy ~w:8. ~h:8. color;
            Paint.frame paint ~x:sx ~y:sy ~w:8. ~h:8. (Theme.edge theme))
            (Option.join (List.nth_opt swatches index));
          (* the destructive row reads in the error ink *)
          kit_text paint ~color:(if not enabled then Theme.ink_3 theme
              else if List.mem index danger then Theme.invalid else theme.foreground)
            (rx + side + lead_of index + int_of_float swatch_pad) (label_y ui y h) shown;
          let _, _, w, _ = bounds in
          if chevron then Paint.chevron paint ~at:(float (rx + w) -. 15.25, float y +. (float h /. 2.))
            `Right (Theme.ink_2 theme);
          Option.iter (fun key ->
            Paint.text paint ~size:key_size ~color:(Theme.ink_2 theme)
              ~at:(float (rx + w - side) -. Paint.text_width paint ~size:key_size key,
                   text_top ui ~size:key_size (float y) (float h)) key) (key_of keys index));
        if enabled && signal.clicked && not chevron then Some index else None
      end) items in
    List.find_map Fun.id picked, !hovered in
  let keep = List.map sub_geometry submenus in
  let stroke = Theme.edge ui.theme in
  let opened = ref None in
  let related = if submenus = [] then [] else [submenu_key] in
  match popup_with_related ui ~stroke ~keep ~related ~at:(x, y) ~width ~height label (fun () ->
      let top = pad "menu-top" in
      let open_row = state ui top ~default:(-1) in
      opened := if open_row >= 0 then Some open_row else None;
      let picked, hovered = rows_of ~lead_from ~items ~keys ~selected ~swatches ~danger ~opened:!opened
          ~chevrons:(List.map (fun sub -> sub.row) submenus) in
      (* the pointer opens the submenu of a row, or closes it on any other *)
      Option.iter (fun index ->
        let now = if List.exists (fun sub -> sub.row = index) submenus then index else -1 in
        if now <> open_row then set_state ui top now;
        opened := if now >= 0 then Some now else None) hovered;
      ignore (pad "menu-bottom");
      picked) with
  | None -> dismiss_popup ui; `Dismiss
  | Some (Some index) -> dismiss_popup ui; `Pick index
  | Some None ->
      (match Option.bind !opened (fun row -> List.find_opt (fun sub -> sub.row = row) submenus) with
       | None -> `Open
       | Some sub ->
           let sx, sy, sw, sh = sub_geometry sub in
           let result = popup ui ~stroke ~attached:true ~at:(sx, sy) ~width:sw ~height:sh
               (label ^ "-sub") (fun () ->
             ignore (pad "menu-top");
             let picked, _ = rows_of ~lead_from:0 ~items:sub.rows ~keys:sub.keys ~selected:sub.current
                 ~swatches:[] ~danger:[] ~opened:None ~chevrons:[] in
             ignore (pad "menu-bottom");
             picked) in
           (match result with
            | Some (Some index) -> dismiss_popup ui; `Pick (base_of sub + index)
            | _ -> `Open))

let expanded ui text =
  let key = key_of (current_seed ui) text in
  let slot = Table.find ui.table key in
  if slot < 0 || ui.state_values.(slot) = min_int then None
  else Some (ui.state_values.(slot) = 1)

let ( + ) left right = left lor right
