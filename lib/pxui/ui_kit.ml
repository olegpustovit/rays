open Rays
open Ui_core

(* --------------------------------------------------------- kit widgets *)

let kit_row ui ?(flags = clickable lor focusable lor blocking lor tab_only) ?hit label =
  box ui ~flags ~w:Grow ~h:(Px (float ui.kit_row_height)) ?hit label

let ints (x, y, w, h) = (int_of_float x, int_of_float y, int_of_float w, int_of_float h)
let floats (x, y, w, h) = (float x, float y, float w, float h)

(* Kit rev 3 geometry from a row rectangle: 12 points at each side, a label column of 112
   (half of a narrow row), then the 20-point control. *)
let side = 12

(* Text of [size] points centred in a row: its top is half of what the row has over the text
   size.  Measured against the kit's reference renders (specification/pxui-kit): 13-point text
   in a 24-point row has its ascenders at 7 and its descenders at 18. *)
let text_top ui ?size y h =
  y +. Float.floor ((h -. float (Option.value size ~default:ui.font_size)) /. 2.)

let table ui ~at ~w ~h ~headers ~rows ~cell label =
  if rows < 0 then invalid_arg "Ui.table: negative row count";
  let row_h = 24. in
  let widths = Array.map (fun header -> Float.max 72. (text_width ui header +. 24.)) headers in
  let stops = Array.make (Array.length headers + 1) 0. in
  Array.iteri (fun i width -> stops.(i + 1) <- stops.(i) +. width) widths;
  let root = box ui ~flags:clip ~at ~w:(Px w) ~h:(Px h) label in
  within ui root (fun () ->
      let header = box ui ~w:(Px w) ~h:(Px row_h) (label ^ "-header") in
      draw ui header (fun paint (x, y, _, _) ->
          Array.iteri
            (fun i title ->
              Paint.text paint ~color:(Theme.ink_2 ui.theme)
                ~at:(x +. stops.(i) +. 8., text_top ui y row_h)
                title)
            headers;
          Paint.line paint
            ~from_:(x, y +. row_h -. 0.5)
            ~to_:(x +. w, y +. row_h -. 0.5)
            (Theme.border ui.theme));
      let body_h = Float.max 0. (h -. row_h) in
      let body =
        box ui ~flags:(scroll + clip) ~w:(Px w) ~h:(Px body_h) ~scroll_step:row_h (label ^ "-body")
      in
      (* Both sizes are explicit: use the shared wheel/trackpad/coast state before
       choosing rows. Arrange then sees consumed input and a zero time delta. *)
      ui.l_h.(body.index) <- body_h;
      ui.l_content.(body.index) <- float rows *. row_h;
      Option.iter
        (fun (frame : Frame.t) -> apply_scroll ~consume:true ui frame.input.t body.index)
        ui.input_frame;
      let first = min rows (max 0 (int_of_float (Float.floor (scroll_offset ui body /. row_h)))) in
      let last = min rows (first + int_of_float (Float.ceil (body_h /. row_h)) + 2) in
      let clicked = ref None in
      within ui body (fun () ->
          let extent = box ui ~w:(Px w) ~h:(Px (float rows *. row_h)) (label ^ "-extent") in
          within ui extent (fun () ->
              for index = first to last - 1 do
                let row =
                  box ui ~flags:clickable
                    ~at:(0., float index *. row_h)
                    ~w:(Px w) ~h:(Px row_h)
                    (label ^ "-row-" ^ string_of_int index)
                in
                if (signal ui row).clicked then clicked := Some index;
                draw ui row (fun paint (x, y, _, _) ->
                    Array.iteri
                      (fun column _ ->
                        let value = cell index column in
                        let right = x +. stops.(column + 1) -. 8. in
                        Paint.text paint ~color:ui.theme.foreground
                          ~at:(right -. Paint.text_width paint value, text_top ui y row_h)
                          value)
                      headers;
                    Paint.line paint
                      ~from_:(x, y +. row_h -. 0.5)
                      ~to_:(x +. w, y +. row_h -. 0.5)
                      (Theme.faint_border ui.theme))
              done));
      (!clicked, signal ui body))

let label_y ui y h = y + ((h - ui.font_size) / 2)
let value_column w = min 112 (max 0 ((w - (2 * side) - 8) / 2))

let value_control (x, y, w, h) =
  let cx = x + side + value_column w + 8 in
  (cx, y + 2, max 1 (x + w - side - cx), max 1 (h - 4))

let toggle_control (x, y, w, h) = (x + w - side - 28, y + ((h - 14) / 2), 28, 14)
let control_hit shape rect = floats (shape (ints rect))

(* A value shown to three significant digits of its range (48.0 in 0..120, 0.20 in 0..1). *)

let fraction_at (x, _, w, _) pointer_x =
  Float.max 0. (Float.min 1. ((pointer_x -. float x) /. float (max 1 (w - 1))))

let fill paint (x, y, w, h) color =
  Paint.fill paint ~x:(float x) ~y:(float y) ~w:(float w) ~h:(float h) color

let kit_text paint ?color x y text = Paint.text paint ~at:(float x, float y) ?color text
let hover_row paint ui bounds = fill paint bounds (Theme.faint_border ui.theme)

(* a field is a value on a hairline: accent while it holds the keyboard *)
let underline paint (cx, cy, cw, ch) color = fill paint (cx, cy + ch - 1, cw, 1) color

let paint_switch paint (theme : Theme.t) ~x ~y value =
  Paint.fill paint ~x ~y ~w:28. ~h:14. (if value then theme.input else theme.track);
  Paint.stroke paint ~x:(x +. 0.5) ~y:(y +. 0.5) ~w:27. ~h:13. (Theme.border theme);
  Paint.fill paint
    ~x:(x +. if value then 17. else 3.)
    ~y:(y +. 3.) ~w:8. ~h:8.
    (if value then theme.accent else Theme.ink_3 theme)

let cap_y ui y h = text_top ui ~size:(max 8 (ui.font_size - 2)) y h

(* a section header: a label in ink-3 and a chevron, under 16 points of space *)
let paint_section paint ui (x, y, w, h) label ~open_ ~hovered =
  ignore hovered;
  (* the sheet has no hover state on a section header *)
  Paint.cap paint ~at:(x +. float side, cap_y ui y h) ~color:(Theme.ink_3 ui.theme) label;
  Paint.chevron paint
    ~at:(x +. w -. float side -. 3., y +. (h /. 2.))
    (if open_ then `Down else `Right)
    ui.theme.foreground

(* the space above a section header: 4 at the top of its parent (16 under a window's head), 16
   after anything else, none when this closed section directly follows a closed one *)
let section_gap ?(next_closed = true) ?last ui =
  let last = match last with Some last -> last | None -> ui.b_last.(current_parent ui) in
  if last < 0 then if ui.kit_window then 16 else 4
  else if last = ui.closed_section && next_closed then 0
  else 16

(* The ground of a button, the one painter of [button], of the inspector's and of the shell's positioned buttons: a fill
   on press and hover, the control fill when [on], a line-3 edge for the [primary]. *)
let paint_button_ground paint theme ~held ~hovered ~on ~primary (cx, cy, cw, ch) =
  if held then Paint.fill paint ~x:cx ~y:cy ~w:cw ~h:ch (Theme.pressed_fill theme)
  else if hovered then Paint.fill paint ~x:cx ~y:cy ~w:cw ~h:ch (Theme.hover_fill theme)
  else if on then Paint.fill paint ~x:cx ~y:cy ~w:cw ~h:ch theme.control;
  if primary then
    Paint.stroke paint ~x:(cx +. 0.5) ~y:(cy +. 0.5) ~w:(cw -. 1.) ~h:(ch -. 1.)
      (Theme.border theme)

let panel_with ?stroke ?(window = false) ui ?(x = 12.) ?(y = 12.) ?(width = 280.) ?height
    ?max_height ?(padding = 0) label f =
  let row_height = 24 in
  if padding < 0 then invalid_arg "Ui.panel: padding must be non-negative";
  let width = Float.max 180. width in
  let max_h =
    match max_height with
    | Some height -> Float.max height (float (add row_height (2 * padding)))
    | None -> Float.infinity
  in
  let panel =
    box ui
      ~flags:(scroll lor clip lor blocking)
      ~w:(Px width)
      ~h:(Option.fold ~none:Fit ~some:(fun height -> Px height) height)
      ~max_h ~axis:Column ~padding:(float padding) ~at:(x, y) ~scroll_step:(float row_height) label
  in
  let theme = ui.theme and index = panel.index in
  draw ui panel (fun paint (x, y, w, h) ->
      (* a floating sheet (a menu, a window) is the input white; a docked panel is the ground *)
      Paint.fill paint ~x ~y ~w ~h (if Option.is_some stroke then theme.input else theme.panel));
  draw_over ui panel (fun paint rect ->
      Option.iter
        (fun color ->
          let x, y, w, h = rect in
          Paint.frame paint ~x ~y ~w ~h color)
        stroke;
      let x, y, width, height = ints rect in
      let content = int_of_float ui.l_content.(index) in
      let maximum = max 0 (content - height) in
      if maximum > 0 then begin
        let track_x = x + width - 6
        and track_y = y + padding
        and track_height = max 1 (height - (2 * padding)) in
        let thumb_height = min track_height (max 20 (track_height * height / max 1 content)) in
        let travel = track_height - thumb_height in
        let scroll_y = int_of_float ui.scroll_y.(panel.box_slot) in
        let thumb_y = track_y + (scroll_y * travel / maximum) in
        (* no track, no arrows: a 4-point thumb *)
        Paint.fill paint ~x:(float track_x) ~y:(float thumb_y) ~w:4. ~h:(float thumb_height)
          (Theme.border theme)
      end);
  let previous_row = ui.kit_row_height and previous_window = ui.kit_window in
  ui.kit_row_height <- row_height;
  ui.kit_window <- window;
  Fun.protect
    ~finally:(fun () ->
      ui.kit_row_height <- previous_row;
      ui.kit_window <- previous_window)
    (fun () -> within ui panel f)

let panel ?window ui = panel_with ?window ui

let inspector_width ui =
  let rec find index =
    if index <= 0 then 280.
    else match ui.b_w.(index) with Px width -> width | _ -> find ui.b_parent.(index)
  in
  find (current_parent ui)

(* [ellipsis] at a text size; a text with no room at all is the ellipsis alone *)
let inspector_fit paint ~size ~width text =
  let shown = ellipsis ~width:(Paint.text_width paint ~size) ~limit:width text in
  if shown = "" && not (text = "" && width >= 0.) then "…" else shown

(* The kit's inspector row, from its CSS: 12 side padding, a 6-point slot for the pin dot, 8, the
   label column, 8, the control to 12 from the right edge.  The label column is 0.3 of the panel
   less 16 (98 at 380 wide, 80 at 320); a longer label ellipsizes in it, never stacks. *)
let inspector_label_width width = Float.max 56. (Float.floor (width *. 0.3) -. 16.)
let inspector_control_x width = 12. +. 6. +. 8. +. inspector_label_width width +. 8.

(* A window's rows (windows.html): the label at 12, a 88-point column and no pin slot. *)
let inspector_label_x ui = if ui.kit_window then 12. else 26.

(* The 4 points between a head's rule and the first element of the body, when it is not a section. *)
let lead_gap ui =
  if ui.b_last.(current_parent ui) < 0 then ignore (box ui ~w:Grow ~h:(Px 4.) "lead-gap")

let inspector_row ui ?width ?pin ~key ~label () =
  let width = Option.value width ~default:(inspector_width ui) in
  let label_x = inspector_label_x ui in
  let label_w = if ui.kit_window then 88. else inspector_label_width width in
  let control_x = if ui.kit_window then 12. +. 88. +. 8. else inspector_control_x width in
  let pin = if ui.kit_window then None else pin in
  lead_gap ui;
  let row = box ui ~flags:clickable ~w:Grow ~h:(Px (float ui.kit_row_height)) key in
  let theme = ui.theme and hovered = hovered_within ui row in
  draw ui row (fun paint (x, y, w, h) ->
      if hovered then Paint.fill paint ~x ~y ~w ~h (Theme.faint_border theme);
      (* the pin dot: filled ink when the row is on its card, an ink-3 ring when it is not *)
      Option.iter
        (fun on ->
          let at = (x +. 15., y +. (h /. 2.)) in
          if on then Paint.circle paint ~at ~radius:3. ~fill:theme.foreground ()
          else Paint.circle paint ~at ~radius:2.5 ~stroke:(Theme.ink_3 theme) ())
        pin;
      Paint.text paint
        ~at:(x +. label_x, text_top ui y h)
        ~color:(Theme.ink_2 theme)
        (inspector_fit paint ~size:ui.font_size ~width:label_w label));
  (row, control_x, 2., Float.max 40. (width -. control_x -. 12.))

let inspector_section ui ~key ?(expanded = false) ?set_expanded label f =
  let last = ui.b_last.(current_parent ui) in
  let title =
    box ui ~flags:(clickable lor tab_stop) ~w:Grow ~h:(Px (float ui.kit_row_height)) key
  in
  let open_ = state ui title ~default:(if expanded then 1 else 0) <> 0 in
  let open_ = Option.value set_expanded ~default:open_ in
  let signal = signal ui title in
  let open_ = if signal.clicked then not open_ else open_ in
  (* the gap above depends on this section's own state, known only now *)
  let gap = float (section_gap ~next_closed:(not open_) ~last ui) in
  ui.b_h.(title.index) <- Px (gap +. float ui.kit_row_height);
  set_state ui title (if open_ then 1 else 0);
  if not open_ then ui.closed_section <- title.index;
  draw ui title (fun paint (x, y, w, h) ->
      paint_section paint ui (x, y +. gap, w, h -. gap) label ~open_ ~hovered:signal.hovered);
  if open_ then Some (f ()) else None

let inspector_toggle_value ui ~key ~at:(x, y) value =
  let control = box ui ~flags:(clickable lor tab_stop) ~at:(x, y) ~w:(Px 28.) ~h:(Px 20.) key in
  let value = if (signal ui control).clicked then not value else value in
  draw ui control (fun paint (x, y, _, _) -> paint_switch paint ui.theme ~x ~y:(y +. 3.) value);
  value

(* A switch row: the switch sits at the start of the control column, not at the row's end. *)
let inspector_toggle ui ~key ~label value =
  let row, cx, cy, _ = inspector_row ui ~key ~label () in
  within ui row (fun () -> inspector_toggle_value ui ~key:(key ^ "-value") ~at:(cx, cy) value)

(* A text button on a row of its own. *)
let inspector_button ui ~key label =
  lead_gap ui;
  let row = box ui ~flags:(clickable lor tab_stop) ~w:Grow ~h:(Px (float ui.kit_row_height)) key in
  let signal = signal ui row in
  draw ui row (fun paint (x, y, w, h) ->
      let shown = inspector_fit paint ~size:ui.font_size ~width:(w -. 24.) label in
      paint_button_ground paint ui.theme ~held:signal.held ~hovered:signal.hovered ~on:false
        ~primary:false
        (x +. 6., y +. 2., Paint.text_width paint shown +. 12., h -. 4.);
      Paint.text paint ~at:(x +. float side, text_top ui y h) ~color:ui.theme.foreground shown);
  signal.clicked

(* A read-out: the label, then the value right-aligned in the control column; no pin dot. *)
let inspector_readout ui ?width ~key ~label value =
  let row, control_x, _, control_width = inspector_row ui ?width ~key ~label () in
  draw ui row (fun paint (x, y, _, h) ->
      let shown = inspector_fit paint ~size:ui.font_size ~width:control_width value in
      Paint.text paint
        ~at:(x +. control_x +. control_width -. Paint.text_width paint shown, text_top ui y h)
        ~color:ui.theme.foreground shown)

(* The scrolling part of an inspector under its head (the head stays): a column that takes the
   rest of the panel and scrolls, its 4-point line-3 thumb 2 from the right edge. *)
let inspector_body ui f =
  let body =
    box ui
      ~flags:(scroll lor clip lor over_thumb)
      ~w:Grow ~h:Grow ~axis:Column ~scroll_step:(float ui.kit_row_height) "inspector-body"
  in
  draw_over ui body (fun paint (x, y, w, h) ->
      let content = ui.l_content.(body.index) in
      if content > h then begin
        let track_y = y +. 6. and track_h = Float.max 1. (h -. 12.) in
        let thumb_h = Float.min track_h (Float.max 20. (track_h *. h /. content)) in
        let travel = track_h -. thumb_h and maximum = content -. h in
        let thumb_y = track_y +. (ui.scroll_y.(body.box_slot) *. travel /. maximum) in
        Paint.fill paint ~x:(x +. w -. 6.) ~y:thumb_y ~w:4. ~h:thumb_h (Theme.border ui.theme)
      end);
  within ui body f

(* The width the kit's CSS gives [text] of [size] points: the face is monospaced at half an em
   (plus [tracking] a character).  A head lays its buttons and chips out with it, as the sheet
   does, whatever advance the rasterised glyphs round to. *)
let mono_width ?(tracking = 0.) size text =
  let count = ref 0 in
  String.iter (fun c -> if Char.code c land 0xc0 <> 0x80 then incr count) text;
  float !count *. ((float size /. 2.) +. tracking)

(* The bar under an inspector (inspector.html): a line-2 hairline and a 24-point bar of hints, each a
   key in ink-3 at the label size and what it does in ink-2, and at the right a 6-point dot and a
   count as a label.  It sits below the body, which scrolls above it. *)
let inspector_bar ui ~hints ~count =
  let small = max 8 (ui.font_size - 2) in
  let line = float ui.kit_row_height in
  let bar = box ui ~w:Grow ~h:(Px (line +. 1.)) "inspector-bar" in
  draw ui bar (fun paint (x, y, w, _) ->
      let theme = ui.theme in
      Paint.fill paint ~x ~y ~w ~h:1. (Theme.edge theme);
      let y = y +. 1. in
      let at = ref (x +. 12.) in
      List.iter
        (fun (key, what) ->
          Paint.text paint ~size:small ~color:(Theme.ink_3 theme)
            ~at:(!at, text_top ui ~size:small y line)
            key;
          at := !at +. mono_width small key +. 8.;
          Paint.text paint ~color:(Theme.ink_2 theme) ~at:(!at, text_top ui y line) what;
          at := !at +. mono_width ui.font_size what +. 8.)
        hints;
      let label = Printf.sprintf "%d on card" count in
      (* the sheet's tracking follows the last letter too: the ink ends that much before the 12 *)
      let label_w = Paint.cap_width paint label +. Paint.cap_tracking paint in
      let left = x +. w -. 12. -. label_w in
      Paint.cap paint ~at:(left, cap_y ui y line) ~color:(Theme.ink_2 theme) label;
      Paint.circle paint ~at:(left -. 11., y +. (line /. 2.)) ~radius:3. ~fill:theme.foreground ())

let inspector_message ui ~key message =
  lead_gap ui;
  let row = box ui ~w:Grow ~h:(Px (float ui.kit_row_height)) key in
  draw ui row (fun paint (x, y, w, h) ->
      Paint.circle paint ~at:(x +. 15., y +. (h /. 2.)) ~radius:3. ~fill:ui.theme.accent ();
      Paint.text paint
        ~at:(x +. 26., y +. float (label_y ui 0 ui.kit_row_height))
        ~color:(Theme.ink_2 ui.theme)
        (inspector_fit paint ~size:ui.font_size ~width:(w -. 38.) message))

let popup_with_related ui ?stroke ?max_height ?(dismiss_initial = true) ?(attached = false)
    ?(keep = []) ?(related = []) ~at:(x, y) ~width ~height label f =
  let key = key_of (current_seed ui) label in
  (* Build a popup before the body it shields. Previous popups arbitrate in
     [route]; this also shields a newly opened/dismissed popup's frame.  An [attached] popup (a
     submenu, built after the modal one) shields nothing of its own: it shares the modal's
     events, and only the modal one is dismissed, by a press outside it and every [keep] rectangle. *)
  ui.modal_in_frame <- true;
  if not attached then begin
    let belongs target = hit_within ui target key || List.exists (hit_within ui target) related in
    Int_table.filter_map_inplace
      (fun target signal -> if belongs target then Some signal else None)
      ui.signals;
    if Option.fold ~none:false ~some:(fun active -> not (belongs active)) ui.active then begin
      ui.cancelled <- ui.active_button :: ui.cancelled;
      ui.active <- None
    end;
    if ui.focus <> 0 && not (belongs ui.focus) then unfocus ui
  end;
  let slot = Table.find ui.table key in
  let rect =
    if slot >= 0 then (ui.rx.(slot), ui.ry.(slot), ui.rw.(slot), ui.rh.(slot))
    else (x, y, width, height)
  in
  let dismissed =
    List.exists
      (function
        | Input.Key_pressed Input.Escape | Input.Focus_lost -> true
        | Input.Pointer_pressed (_, point) ->
            (slot >= 0 || dismiss_initial)
            && (not (contains rect point))
            && not (List.exists (fun r -> contains r point) keep)
        | _ -> false)
      ui.frame_events
  in
  if dismissed && not attached then None
  else begin
    (* Popups live at the root wherever they are built, so a pane's hit
       area never clips them, and are moved last so they paint on top. *)
    let parents = ui.parents in
    ui.parents <- [ 0 ];
    let index = ui.count in
    Fun.protect
      ~finally:(fun () -> ui.parents <- parents)
      (fun () ->
        (* a window's 1-point edge is part of its box: its content starts inside it *)
        let result =
          panel_with ?stroke ?max_height
            ~padding:(if stroke = None then 0 else 1)
            ui ~x ~y ~width label f
        in
        ui.overlays <- index :: ui.overlays;
        if attached then ui.modal_extra <- key :: ui.modal_extra else ui.modal_key <- Some key;
        Some result)
  end

let popup ui ?stroke ?max_height ?dismiss_initial ?attached ~at ~width ~height label f =
  popup_with_related ui ?stroke ?max_height ?dismiss_initial ?attached ~at ~width ~height label f

(* A centered panel from last frame's height. *)
let modal ui ?(width = 320.) label f =
  let key = key_of (current_seed ui) label in
  let slot = Table.find ui.table key in
  if slot >= 0 then begin
    if (not (Hashtbl.mem ui.modal_heights key)) && Hashtbl.length ui.modal_heights >= 32 then begin
      (* ponytail: scan at most 32 heights on insertion; use LRU if this
         small working set ever needs a larger capacity. *)
      let oldest =
        Hashtbl.fold
          (fun key (_, seen) oldest ->
            match oldest with
            | Some (_, previous) when previous <= seen -> oldest
            | _ -> Some (key, seen))
          ui.modal_heights None
      in
      Option.iter (fun (key, _) -> Hashtbl.remove ui.modal_heights key) oldest
    end;
    Hashtbl.replace ui.modal_heights key (ui.rh.(slot), ui.frame_number)
  end;
  let height = Option.fold ~none:0. ~some:fst (Hashtbl.find_opt ui.modal_heights key) in
  let x = Float.round (Float.max 0. ((ui.view_w -. width) /. 2.))
  and y = Float.round (Float.max 0. ((ui.view_h -. height) /. 2.)) in
  popup ui ~stroke:(Theme.border ui.theme) ~dismiss_initial:false ~at:(x, y) ~width ~height
    ~max_height:(Float.max 48. (ui.view_h -. 32.))
    label f

(* A label: ink-2 capitals 12 from the left (a window's title), 4 points under the top of its parent. *)
let label ui text =
  let gap = if ui.b_last.(current_parent ui) < 0 then 4. else 0. in
  let row = box ui ~flags:none ~w:Grow ~h:(Px (gap +. float ui.kit_row_height)) text in
  let theme = ui.theme and shown = display text in
  draw ui row (fun paint (x, y, _, h) ->
      Paint.cap paint
        ~at:(x +. float side, cap_y ui (y +. gap) (h -. gap))
        ~color:(Theme.ink_2 theme) shown)

(* A window's foot: a line-2 hairline under 4 points of space, then the keys in ink-3 each followed by
   what it does in ink-2, 8 points apart, and [right] (a key or a count) at the end. *)
let footer ui ?right hints =
  let row = box ui ~flags:none ~w:Grow ~h:(Px (5. +. float ui.kit_row_height)) "footer" in
  let theme = ui.theme and size = max 8 (ui.font_size - 2) in
  draw ui row (fun paint (x, y, w, h) ->
      Paint.fill paint ~x ~y:(y +. 4.) ~w ~h:1. (Theme.edge theme);
      let key_y = text_top ui ~size (y +. 5.) (h -. 5.)
      and hint_y = text_top ui (y +. 5.) (h -. 5.) in
      let pen =
        List.fold_left
          (fun pen (key, what) ->
            Paint.text paint ~size ~color:(Theme.ink_3 theme) ~at:(pen, key_y) key;
            let pen = pen +. Paint.text_width paint ~size key +. 8. in
            Paint.text paint ~color:(Theme.ink_2 theme) ~at:(pen, hint_y) what;
            pen +. Paint.text_width paint what +. 8.)
          (x +. float side)
          hints
      in
      ignore pen;
      Option.iter
        (fun text ->
          Paint.cap paint ~at:(x +. w -. float side -. Paint.cap_width paint text, key_y) text)
        right)

(* A message under a section: a 6-point dot (the accent for information, the error ink for [error]) and
   the text in ink-2 (or the error ink), wrapped to the width of the parent as it was laid out last
   frame, on lines of 20 points with 4 above and below: a message of one line is 28 high, of two 48. *)

(* A button is its text on a 20-point box (6 points of padding in a transparent 1-point edge):
   a fill on hover and press, an accent line inside the bottom edge with the keyboard.  The box
   starts 5 points in so that its text lines up with the labels at 12. *)
let button ui ?(bare = false) ?(icon = false) ?(at_end = false) ?ink text =
  let shown = display text in
  let measure = text_width ui in
  (* the box: a 1-point edge and 6 points of padding (4 when [bare]) round the text; an [icon] is the
     20-point control square with its glyph centred *)
  let width = if icon then 20. else measure shown +. if bare then 10. else 14. in
  let extent (x, y, w, h) =
    let bx = if at_end then x +. w -. 8. -. width else x +. 5. in
    (bx, y +. 2., Float.max 1. (Float.min width (w -. 10.)), h -. 4.)
  in
  let row =
    kit_row ui
      ~flags:(clickable lor focusable lor blocking lor tab_only lor focus_mark)
      ~hit:extent text
  in
  let signal = signal ui row in
  let theme = ui.theme and focus = focused ui row in
  draw ui row (fun paint rect ->
      let x, y, w, h = rect in
      let cx, cy, cw, ch = extent (x, y, w, h) in
      let ink = Option.value ink ~default:(if bare then Theme.ink_2 theme else theme.foreground) in
      paint_button_ground paint theme ~held:signal.held ~hovered:signal.hovered ~on:false
        ~primary:false (cx, cy, cw, ch);
      (* the keyboard's mark: the accent over the last row of the box, its whole width *)
      if focus then Paint.fill paint ~x:cx ~y:(cy +. ch -. 1.) ~w:cw ~h:1. theme.accent;
      let pad =
        if icon then Float.floor ((cw -. measure shown) /. 2.) else if bare then 5. else 7.
      in
      match (icon, shown) with
      | true, ("<" | ">" | "v" | "^") ->
          let direction =
            match shown with "<" -> `Left | ">" -> `Right | "v" -> `Down | _ -> `Up
          in
          Paint.chevron paint ~at:(cx +. (cw /. 2.), cy +. (ch /. 2.)) direction ink
      | _ -> Paint.text paint ~at:(cx +. pad, text_top ui y h) ~color:ink shown);
  signal.clicked

(* A switch on its row: the label and the 28 x 14 switch at the end. *)
let toggle ui text value =
  let row =
    kit_row ui
      ~flags:(clickable lor focusable lor blocking lor tab_only)
      ~hit:(control_hit toggle_control) text
  in
  let signal = signal ui row in
  let value = if signal.clicked then not value else value in
  let theme = ui.theme and shown = display text in
  let hovered = signal.hovered in
  draw ui row (fun paint rect ->
      let ((x, y, _, h) as bounds) = ints rect in
      let cx, cy, _, _ = toggle_control bounds in
      if hovered then hover_row paint ui bounds;
      kit_text paint ~color:theme.foreground (x + side) (label_y ui y h) shown;
      paint_switch paint theme ~x:(float cx) ~y:(float cy) value);
  value
