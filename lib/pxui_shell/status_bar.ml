type state = [ `Ok | `Busy | `Error ]

(* What every strip starts with: the file, a dot for the state (checked, cooking, refused) and
     the status line in ink-2, cut to [limit]; where the next thing goes. *)
(* where the lead's parts go: the file, the dot, the status cut to [limit], and the end *)
let lead_plan ui ~x ~file ~limit text =
  let module Ui = Pxui.Ui in
  let tx = x +. 12. in
  let file_x = tx in
  let tx = if file <> "" then tx +. Ui.text_width ui file +. 8. else tx in
  let dot_x = tx in
  let tx = tx +. 14. in
  let shown = Ui.ellipsis ~width:(Ui.text_width ui) ~limit:(Float.max 0. (limit -. tx)) text in
  (file_x, dot_x, tx, shown, tx +. Ui.text_width ui shown +. 8.)

let lead ui paint (x, y, h) ~file ~state ~limit text =
  let module Ui = Pxui.Ui in
  let theme = Ui.theme ui in
  let ty = Kit.text_y ui y h in
  let file_x, dot_x, text_x, shown, after = lead_plan ui ~x ~file ~limit text in
  if file <> "" then Ui.Paint.text paint ~at:(file_x, ty) ~color:theme.foreground file;
  Ui.Paint.circle paint
    ~at:(dot_x +. 3., y +. (h /. 2.))
    ~radius:3.
    ~fill:
      (match state with
      | `Ok -> (Pxui.Theme.ports theme).int
      | `Busy -> theme.accent
      | `Error -> Pxui.Theme.invalid)
    ();
  Ui.Paint.text paint ~at:(text_x, ty)
    ~color:(if state = `Error then Pxui.Theme.invalid else Pxui.Theme.ink_2 theme)
    shown;
  after

(* where the end's parts start (the layout, then the frame rate), 12 from the edge, 8 apart *)
let trail_start ui ~x ~w ?(notes = []) ?(readout = "") ~layout ~fps () =
  let right = x +. w -. 12. in
  let right =
    match fps with
    | Some fps -> right -. Kit.cap_width ui (Printf.sprintf "%d fps" fps) -. 8.
    | None -> right
  in
  let right = if layout <> "" then right -. Kit.cap_width ui layout -. 8. else right in
  let right =
    List.fold_left (fun right note -> right -. Kit.cap_width ui note -. 8.) right (List.rev notes)
  in
  if readout = "" then right else right -. Pxui.Ui.text_width ui readout -. 8.

(* the strip's end: the layout in use, then the frame rate in ink, 8 apart; where they start *)
let trail ui paint (x, y, w, h) ?(notes = []) ?(readout = "") ~layout ~fps () =
  let module Ui = Pxui.Ui in
  let theme = Ui.theme ui in
  let right = ref (x +. w -. 12.) in
  Option.iter
    (fun fps ->
      let fps = Printf.sprintf "%d fps" fps in
      right := !right -. Ui.Paint.cap_width paint fps;
      Ui.Paint.cap paint ~color:theme.foreground ~at:(!right, Kit.cap_y ui y h) fps;
      right := !right -. 8.)
    fps;
  if layout <> "" then begin
    right := !right -. Ui.Paint.cap_width paint layout;
    Ui.Paint.cap paint ~at:(!right, Kit.cap_y ui y h) layout;
    right := !right -. 8.
  end;
  (* notes ("3 floating", "3 graphs") stand left of the layout *)
  List.iter
    (fun note ->
      right := !right -. Ui.Paint.cap_width paint note;
      Ui.Paint.cap paint ~at:(!right, Kit.cap_y ui y h) note;
      right := !right -. 8.)
    (List.rev notes);
  (* a graph alone in the strip: its counts in ink-2 before the zoom *)
  if readout <> "" then begin
    right := !right -. Ui.Paint.text_width paint readout;
    Ui.Paint.text paint ~at:(!right, Kit.text_y ui y h) ~color:(Pxui.Theme.ink_2 theme) readout;
    right := !right -. 8.
  end;
  !right

(* the hairline above the bar; the bar is the 24 points under it *)
let ground ui paint (x, y, w, h) =
  let theme = Pxui.Ui.theme ui in
  Pxui.Ui.Paint.fill paint ~x ~y ~w ~h theme.panel;
  Pxui.Ui.Paint.fill paint ~x ~y ~w ~h:1. (Pxui.Theme.edge theme);
  (x, y +. 1., w, h -. 1.)

(* the rule, then the focused pane's kind (ink) and what is selected (ink-2), both labels;
     where the next thing goes *)
let focus_labels ui paint (_, y, h) ?(rule = true) ?(accent = false) ~after ?kind ?selection () =
  let module Ui = Pxui.Ui in
  let theme = Ui.theme ui in
  let tx = ref (after +. 17.) in
  if rule then
    Ui.Paint.fill paint ~x:(after +. 4.)
      ~y:(y +. ((h -. 12.) /. 2.))
      ~w:1. ~h:12. (Pxui.Theme.border theme);
  List.iter
    (fun (text, color) ->
      Ui.Paint.cap paint ~at:(!tx, Kit.cap_y ui y h) ~color text;
      tx := !tx +. Ui.Paint.cap_width paint text +. 8.)
    (List.filter_map Fun.id
       [
         Option.map (fun k -> (k, if accent then theme.accent else theme.foreground)) kind;
         Option.map (fun s -> (s, Pxui.Theme.ink_2 theme)) selection;
       ]);
  !tx

(* where the labels of [focus_labels] end, without painting *)
let focus_end ui ~after ?kind ?selection () =
  List.fold_left
    (fun tx text -> tx +. Kit.cap_width ui text +. 8.)
    (after +. 17.)
    (List.filter_map Fun.id [ kind; selection ])

let draw ui ~bounds:(x, y, width, height) ?(file = "") ?(state = `Ok) ?(layout = "") ?(notes = [])
    ?(readout = "") ?kind ?selection ~text ~fps () =
  if height > 0 then begin
    let module Ui = Pxui.Ui in
    let box =
      Ui.box ui ~flags:Ui.clip
        ~w:(Ui.Px (float_of_int width))
        ~h:(Ui.Px (float_of_int height))
        ~at:(float_of_int x, float_of_int y)
        "workspace-status"
    in
    Ui.draw ui box (fun paint bounds ->
        let x, y, w, h = ground ui paint bounds in
        let right = trail ui paint (x, y, w, h) ~notes ~readout ~layout ~fps () in
        let limit = if kind = None && selection = None then right else right -. 160. in
        let after = lead ui paint (x, y, h) ~file ~state ~limit text in
        if kind <> None || selection <> None then
          ignore (focus_labels ui paint (x, y, h) ~accent:false ~after ?kind ?selection ()))
  end

let guide ui ~bounds:(x, y, width, height) ?(file = "") ?(state = `Ok) ?(layout = "") ?(text = "")
    ?fps ?(notes = []) ?(readout = "") ?(accent = false) ?(extra = []) ?leader ?kind ?selection
    ~context () =
  let module Ui = Pxui.Ui in
  if height > 0 then
    match leader with
    | Some pending ->
        (* an open leader: the file and its state, a rule, the pending prefix in the accent and
           [waiting for a key], the frame rate at the end *)
        let box =
          Ui.box ui ~flags:Ui.clip
            ~w:(Ui.Px (float width))
            ~h:(Ui.Px (float height))
            ~at:(float x, float y)
            "workspace-guide"
        in
        Ui.draw ui box (fun paint bounds ->
            let x, y, w, h = ground ui paint bounds in
            let theme = Ui.theme ui in
            let right = trail ui paint (x, y, w, h) ~notes ~readout ~layout ~fps () in
            let after =
              lead ui paint (x, y, h) ~file ~state ~limit:(x +. Float.min 320. (w /. 4.)) text
            in
            ignore right;
            let tx = focus_labels ui paint (x, y, h) ~accent:true ~after ~kind:pending () in
            Ui.Paint.text paint
              ~at:(tx, Kit.text_y ui y h)
              ~color:(Pxui.Theme.ink_2 theme) "waiting for a key")
    | None ->
        let bar =
          Ui.box ui
            ~flags:Ui.(clickable + clip)
            ~w:(Ui.Px (float width))
            ~h:(Ui.Px (float height))
            ~at:(float x, float y)
            "workspace-guide"
        in
        let keys = extra in
        let title = Editor_core.Guide_context.name context in
        (* the kind and what is selected; a context that is no node's (the leader, a search) names itself *)
        let kind = Some (Option.value kind ~default:title) in
        let selection =
          match context with
          | Editor_core.Guide_context.Canvas | Node | Multi | List | Text -> selection
          | _ -> Some title
        in
        let theme = Ui.theme ui in
        let fx = float x and fw = float width in
        let limit = trail_start ui ~x:fx ~w:fw ~notes ~readout ~layout ~fps () in
        let has_lead = not (file = "" && text = "") in
        (* the lead (file, dot, status) takes what the labels and the key hints leave, at least a
           quarter of the strip (320 at most); its rule drags it to a width kept in the strip's box
           state, for the session *)
        let labels_w = focus_end ui ~after:0. ?kind ?selection () in
        let keys_w =
          List.fold_left
            (fun a (k, l) ->
              a +. Ui.text_width ui ~size:(Kit.cap_size ui) k +. Ui.text_width ui l +. 16.)
            0. keys
        in
        let max_w = Float.max 0. (limit -. labels_w -. 8. -. fx) in
        let natural_limit =
          Float.max (fx +. Float.min 320. (fw /. 4.)) (limit -. labels_w -. keys_w -. 8.)
        in
        let natural_end =
          if has_lead then
            let _, _, _, _, after = lead_plan ui ~x:fx ~file ~limit:natural_limit text in
            after
          else fx -. 5.
        in
        let stored () =
          Option.bind (Ui.text_state ui bar) float_of_string_opt
          |> Option.map (fun w -> Float.min w max_w)
        in
        let split =
          Ui.box ui
            ~flags:Ui.(clickable + blocking)
            ~w:(Ui.Px 7.)
            ~h:(Ui.Px (float height))
            ~at:((match stored () with Some w -> fx +. w | None -> natural_end) +. 1., float y)
            "workspace-guide-split"
        in
        let lead_limit, after_lead =
          if not has_lead then (natural_limit, natural_end)
          else begin
            let sg = Ui.signal ui split in
            let w = stored () in
            (* the hit box sits on the rule: it follows the stored width once there is one *)
            if sg.hovered || sg.held then Ui.request_cursor ui `Horizontal_resize;
            if sg.pressed && w = None then
              Ui.set_text_state ui bar (Some (string_of_float (natural_end -. fx)));
            if sg.held then
              Ui.set_text_state ui bar
                (Some
                   (string_of_float (Float.min max_w (Float.max 24. (fst sg.pointer -. 4. -. fx)))));
            match stored () with
            | Some w -> (fx +. w, fx +. w)
            | None -> (natural_limit, natural_end)
          end
        in
        let labels_end = focus_end ui ~after:after_lead ?kind ?selection () in
        let small = Kit.cap_size ui in
        let pairs =
          fst
            (List.fold_left
               (fun (acc, tx) (key, label) ->
                 let kw = Ui.text_width ui ~size:small key and lw = Ui.text_width ui label in
                 if tx +. kw +. 8. +. lw > limit -. 8. then (acc, infinity)
                 else ((tx, key, label, kw, lw) :: acc, tx +. kw +. 8. +. lw +. 8.))
               ([], labels_end) keys)
          |> List.rev
        in
        Ui.draw ui bar (fun paint bounds ->
            let x, y, w, h = ground ui paint bounds in
            let limit = trail ui paint (x, y, w, h) ~notes ~readout ~layout ~fps () in
            ignore limit;
            (* the file and its state, to the rule, then the labels *)
            if has_lead then begin
              ignore (lead ui paint (x, y, h) ~file ~state ~limit:lead_limit text);
              let after = after_lead in
              ignore (focus_labels ui paint (x, y, h) ~accent ~after ?kind ?selection ())
            end
            else
              ignore
                (focus_labels ui paint (x, y, h) ~rule:false ~accent ~after:(x -. 5.) ?kind
                   ?selection ());
            (* each key in ink-3 at the label size before what it does in ink-2, 8 apart *)
            List.iter
              (fun (tx, key, label, kw, _) ->
                Ui.Paint.text paint ~size:small
                  ~at:(tx, Kit.cap_y ui y h)
                  ~color:(Pxui.Theme.ink_3 theme) key;
                Ui.Paint.text paint
                  ~at:(tx +. kw +. 8., Kit.text_y ui y h)
                  ~color:(Pxui.Theme.ink_2 theme) label)
              pairs);
        if (Ui.signal ui bar).hovered then
          Ui.tooltip ui ~key:"guide-strip"
            ~text:
              (title ^ " \xc2\xb7 "
              ^ String.concat "  " (List.map fst keys)
              ^ " \xc2\xb7 / ?: all keys")

(* Echo, the sheet's [08]: tips stacked 4 apart in the pane's bottom-left corner, the last at the
     bottom.  A tip is 24 high on the sheet fill with a line-2 edge and 13-point text 7 in; information
     starts with a 6-point dot in the hint colour, a refusal reads in the error ink. *)
let tips ui ~bounds:(x, y, width, height) ?(avoid = []) tips =
  let module Ui = Pxui.Ui in
  if width > 0 && height > 0 && tips <> [] then begin
    let theme = Ui.theme ui in
    let count = List.length tips in
    List.iteri
      (fun index (text, kind) ->
        let w =
          Float.min
            (float (max 0 (width - 24)))
            (Ui.text_width ui text +. match kind with `Info -> 32. | `Refusal -> 14.)
        in
        let top = float (y + max 0 (height - 12)) -. (float (count - index) *. 28.) +. 4. in
        (* a tip that would sit on a rectangle of [avoid] (a graph card) moves up above it *)
        let left = float (x + 12) in
        let top =
          List.fold_left
            (fun top (ax, ay, aw, ah) ->
              if left < ax +. aw && ax < left +. w && top < ay +. ah && ay < top +. 24. then
                ay -. 28.
              else top)
            top
            (List.sort (fun (_, a, _, _) (_, b, _, _) -> compare b a) avoid)
        in
        let box =
          Ui.box ui ~flags:Ui.clip ~w:(Ui.Px w) ~h:(Ui.Px 24.)
            ~at:(float (x + 12), top)
            (Printf.sprintf "echo-tip-%d" index)
        in
        Ui.draw ui box (fun paint (x, y, w, h) ->
            Ui.Paint.fill paint ~x ~y ~w ~h theme.input;
            Ui.Paint.frame paint ~x ~y ~w ~h (Pxui.Theme.edge theme);
            match kind with
            | `Info ->
                Ui.Paint.circle paint
                  ~at:(x +. 10., y +. (h /. 2.))
                  ~radius:3. ~fill:(Pxui.Theme.ports theme).hint ();
                Ui.Paint.text paint ~at:(x +. 19., Kit.text_y ui y h) ~color:theme.foreground text
            | `Refusal ->
                Ui.Paint.text paint ~at:(x +. 7., Kit.text_y ui y h) ~color:Pxui.Theme.invalid text))
      tips
  end
