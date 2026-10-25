open Rays
open Scope_wires
open Scope_commands

(* -------------------------------------------------------------- painting *)

(* a port's colour is its type's ([Node_menu.port_color], the menu's squares too); an untyped one
   is the output's *)
let port_color theme (ty : Ty.t option) = match ty with
  | Some ty -> Node_menu.port_color theme ty
  | None -> (Pxui.Theme.ports theme).output
let colors t = Pxui.Theme.ports t.theme
let ty_color t ty = port_color t.theme ty

let fitted paint size width text =
  (* as before, the mark alone when not even it fits *)
  if Ui.Paint.text_width paint ~size text <= width then text else
  match Ui.ellipsis ~width:(Ui.Paint.text_width paint ~size) ~limit:width text with "" -> "…" | t -> t

let dashed_line paint ~width color a b = Ui.Paint.dashed paint ~from_:a ~to_:b ~width color

(* A 1-point frame inside the box, on whole points: a stroke is centred on its rectangle, so the
   box's own edge would paint half a point outside it (and blur at 1x). *)
let frame_in paint ~x ~y ~w ~h ?fill color =
  Option.iter (fun f -> Ui.Paint.fill paint ~x ~y ~w ~h f) fill;
  Ui.Paint.stroke paint ~x:(x +. 0.5) ~y:(y +. 0.5) ~w:(w -. 1.) ~h:(h -. 1.) color

(* the same, dashed (a drop target, a dashed zone) *)
let dashed_frame_in paint ~x ~y ~w ~h color =
  Ui.Paint.dashed_rect paint ~x:(x +. 0.5) ~y:(y +. 0.5) ~w:(w -. 1.) ~h:(h -. 1.) color

(* crisp 1-point lines on the pixel grid: a line of width 1 centred on a whole point blurs *)
let hline paint ~x ~y ~w color = Ui.Paint.fill paint ~x ~y ~w ~h:1. color

(* the kit's chevron scaled with the zoom (the pane draws everything at [z]) *)
let draw_chevron paint ~z ~at:(cx, cy) direction color =
  let a, b, tip = match direction with
    | `Down -> (-4., -2.), (4., -2.), (0., 2.) | `Up -> (-4., 2.), (4., 2.), (0., -2.)
    | `Right -> (-2., -4.), (-2., 4.), (2., 0.) | `Left -> (2., -4.), (2., 4.), (-2., 0.) in
  let p (dx, dy) = cx +. dx *. z, cy +. dy *. z in
  Ui.Paint.line paint ~from_:(p a) ~to_:(p tip) color; Ui.Paint.line paint ~from_:(p tip) ~to_:(p b) color

let zone_style theme = function
  | P.For -> Pxui.Theme.zone_for theme | Fold | Scan | State -> Pxui.Theme.zone_fold theme
  | Sum -> Pxui.Theme.zone_sum theme | Fn -> Pxui.Theme.zone_fn theme | Let -> Pxui.Theme.zone_let theme
  | Branch -> Pxui.Theme.zone_branch theme ~taken:false

(* the tinted rectangle of an expanded zone: fill and a 1-point edge inside the box; the brackets
   of a selected zone are the frame pass's, one set *)
let paint_zone ?(taken = false) paint theme kind (x, y, w, h) =
  let s = if kind = P.Branch then Pxui.Theme.zone_branch theme ~taken else zone_style theme kind in
  Ui.Paint.fill paint ~x ~y ~w ~h s.fill;
  if s.dashed then dashed_frame_in paint ~x ~y ~w ~h s.edge
  else Ui.Paint.stroke paint ~x:(x +. 0.5) ~y:(y +. 0.5) ~w:(w -. 1.) ~h:(h -. 1.) s.edge

let sx t gx = float t.x +. t.pan_x +. gx *. t.zoom
let sy t gy = float t.y +. t.pan_y +. gy *. t.zoom

(* Kit rev 3 (specification/pxui-kit/graph.html, kit.css [.node .nh .nr .port]), in points at zoom 1
   and relative to the card's border box.  The header overlaps the 1-point border, so its type
   square is at 9, 8 and the rows start at [P.body_top]; ports are 8-point circles centred on the
   edge (a row's on x = 0 and w, the header's on 1 and w - 1, y = 13). *)
let head_pad = 9.  (* 1 border + 8 padding *)
let label_x = 13.  (* 1 border + 12 padding *)
let field_w = 76. and vec_w = 112. and field_h = 16.
let switch_w = 28. and switch_h = 14.
let value_x w = w -. head_pad -. field_w
let port_y = 13.  (* the header's ports; a row's are centred on the row *)

(* a socket: an 8-point circle (a 1-point ring in the port colour, filled once wired), a record a
   10 by 6 pill, a function the same 8-point square turned 45 degrees; a list is a plain circle *)
let paint_socket paint theme (ty : Ty.t option) ~connected ~z (cx, cy) =
  let color = port_color theme ty in
  let r = 4. *. z in
  let fill = if connected then color else theme.Pxui.input in
  let ring (cx, cy) =
    Ui.Paint.circle paint ~at:(cx, cy) ~radius:r ~fill:color ();
    if not connected then Ui.Paint.circle paint ~at:(cx, cy) ~radius:(r -. 1.) ~fill:theme.input () in
  (* a diamond of half-diagonal [half], one point of height per scan row *)
  let diamond half colour =
    let n = int_of_float (Float.ceil half) in
    for k = - n to n - 1 do
      let hw = half -. Float.abs (float k +. 0.5) in
      if hw > 0. then Ui.Paint.fill paint ~x:(cx -. hw) ~y:(cy +. float k) ~w:(2. *. hw) ~h:1. colour
    done in
  match ty with
  | Some (Ty.Fn _) ->
      let half = 4. *. Float.sqrt 2. *. z in
      diamond half color;
      if not connected then diamond (half -. Float.sqrt 2.) theme.input
  | Some (Ty.Record _) ->
      frame_in paint ~x:(cx -. 5. *. z) ~y:(cy -. 3. *. z) ~w:(10. *. z) ~h:(6. *. z) ~fill color
  | _ -> ring (cx, cy)

(* kit text centred in a row: its top is [Ui.text_top], scaled by the zoom through [h] *)
let text_in paint ui ~size ~color ~x ~y ~h text =
  Ui.Paint.text paint ~at:(x, Ui.text_top ui ~size y h) ~size ~color text

(* a label: upper case, tracked by a point; returns its width *)
let cap_in paint ui ~size ~color ~x ~y ~h label =
  let label = String.uppercase_ascii label in
  let tracking = 0.08 *. float size in  (* kit: +0.08em *)
  Ui.Paint.text paint ~at:(x, Ui.text_top ui ~size y h) ~size ~tracking ~color label;
  Ui.Paint.text_width paint ~size label +. float (String.length label) *. tracking

let kind_label = function
  | P.For -> "for" | Fold -> "fold" | Scan -> "scan" | Sum -> "sum" | Let -> "let" | Fn -> "fn" | State -> "state" | Branch -> "branch"

let note_colors theme =
  if Pxui.Theme.dark theme then Color.hex_exn "#4a4220", Color.hex_exn "#f3e6a8"
  else Color.hex_exn "#f3e6a8", Color.hex_exn "#4a3f10"

(* the marks a node or zone carries: live time, loop-invariant, macro, loop order *)
let marks ?execution (n : P.node) =
  (if n.live then [ "t" ] else [])
  @ Option.fold ~none:[] ~some:(fun (e : Flow_graph.Probe.execution) -> [e.tier]) execution
  @ (if n.macro <> None then [ "◊" ] else [])
  @ (match n.zone with Some { order = Some o; _ } -> [ o ] | _ -> [])

let marks_width paint ~size ~gap marks =
  List.fold_left (fun a m -> a +. Ui.Paint.text_width paint ~size m +. gap) 0. marks

(* marks right-aligned at [right], accent, label size *)
let paint_marks paint ui theme ~size ~gap ~right ~y ~h marks =
  ignore (List.fold_left (fun x m ->
    let w = Ui.Paint.text_width paint ~size m in
    text_in paint ui ~size ~color:theme.Pxui.accent ~x:(x -. w) ~y ~h m;
    x -. w -. gap) right marks)

(* DepartureMono has no ⟲ ◆ ◷ ↥ ▸ ▾: the pane draws ↵ ◊ t ↑ ► ▼ in their place. *)
let shown_glyph = function "⟲" -> "↵" | "◆" -> "◊" | g -> g

let strip_parens s =
  let n = String.length s in
  if n >= 2 && s.[0] = '(' && s.[n - 1] = ')' then String.sub s 1 (n - 2) else s

(* a graph input's default, what a wired name reads as live *)
let live_value t name = match t.scope with
  | Some scope ->
      List.find_map (fun (i : P.input) ->
        if i.name = name then Option.map Flow.Lisp.flat i.default else None) scope.inputs
  | None -> None

let number_words text = (* "[0 1 0]" as its numbers *)
  let n = String.length text in
  if n >= 2 && text.[0] = '[' && text.[n - 1] = ']' then
    let parts = String.split_on_char ' ' (String.sub text 1 (n - 2)) in
    if List.for_all (fun s -> float_of_string_opt s <> None) parts then Some parts else None
  else None

let fraction_of (r : P.row) text = match r.control with
  | P.Range (lo, hi) -> Editor_core.Number.fraction (lo, hi) text
  | _ -> None

(* a field painted without the live editor: a value on a line-2 hairline, label-size text *)
let all_numbers elems = List.for_all (fun (e : S.t) -> match e.node with S.Num _ -> true | _ -> false) elems

(* a number or a vector of numbers at a zoom where the live editors are built: they paint
   themselves (see [update]) *)
let live_numbers ~z (r : P.row) = z >= 0.5 && (match r.chip, r.expr with
  | P.Const, Some { S.node = S.Num _; _ } -> true
  | P.Const, Some { S.node = S.Vec elems; _ } ->
      let k = List.length elems in k >= 2 && k <= 4 && all_numbers elems
  | _ -> false)

(* a field painted without the live editor: a value on a line-2 hairline, label-size text; the
   position line of a ranged number is 2 points above the hairline (rows 13 and 14 of 16) *)
let paint_field paint ui ~z ~ls ~theme ~x ~y ~w ?(left = false) ?fraction ?(chevron = false) ~color text =
  let h = field_h *. z in
  Ui.Paint.fill paint ~x ~y:(y +. h -. 1.) ~w ~h:1. (Pxui.Theme.edge theme);
  Option.iter (fun f ->
    Ui.Paint.fill paint ~x ~y:(y +. h -. 3.) ~w:(w *. f) ~h:2. (Pxui.Theme.ink_2 theme)) fraction;
  let room = w -. 4. *. z -. (if chevron then 10. *. z else 0.) in
  let text = fitted paint ls room text in
  let tw = Ui.Paint.text_width paint ~size:ls text in
  let tx = if left || chevron then x +. 2. *. z else x +. w -. 2. *. z -. tw in
  text_in paint ui ~size:ls ~color ~x:tx ~y ~h text;
  if chevron then draw_chevron paint ~z ~at:(x +. w -. 5. *. z, y +. h /. 2. -. 1. *. z) `Down color

let paint_switch paint theme ~z ~x ~y on =
  frame_in paint ~x ~y ~w:(switch_w *. z) ~h:(switch_h *. z)
    ~fill:(if on then theme.Pxui.input else theme.Pxui.track) (Pxui.Theme.border theme);
  Ui.Paint.fill paint ~x:(x +. (if on then 17. else 3.) *. z) ~y:(y +. 3. *. z) ~w:(8. *. z) ~h:(8. *. z)
    (if on then theme.accent else Pxui.Theme.ink_3 theme)

(* the cells of a vector: each element is a field, its number right-aligned, anything else the
   expression's text *)
let paint_vector paint ui ~z ~ls ~theme ~right ~y (elems : S.t list) =
  let k = List.length elems in
  let vw = vec_w *. z and gap = 8. *. z in
  let cw = (vw -. gap *. float (k - 1)) /. float k in
  List.iteri (fun c (e : S.t) ->
    let text, left = match e.node with
      | S.Num s -> s, false | S.Str s -> s, true | _ -> Flow.Lisp.flat e, true in
    paint_field paint ui ~z ~ls ~theme ~x:(right -. vw +. float c *. (cw +. gap)) ~y ~w:cw ~left
      ~color:theme.Pxui.foreground text) elems

(* the value of a row, at the right of it (kit [.nr]).  [live]: the live editor of a number or
   vector of numbers is built over it (in [update]), so it is not painted here. *)
(* a wired row, right-aligned: [ƒ] when its source can fold into it, [←], the source and its live
   value.  Returns where the arrow starts, the room the name has, the live value and its width;
   [update] puts the fold button's box where this paints the glyph. *)
let wired_name (r : P.row) s = match r.expr with
  | Some { S.node = S.List ({ S.node = S.Sym head; _ } :: _); _ } when E.nested s -> head
  | _ -> s
let fold_glyph = "\xc6\x92"
let wired_geo ~measure t ~z ~fs ~x ~w ~fold s =
  let right = x +. w -. head_pad *. z and gap = 6. *. z in
  let live = live_value t s in
  let aw = measure fs "\xe2\x86\x90" in
  let fw = if fold then measure fs fold_glyph +. gap else 0. in
  let live_w = match live with Some v -> measure fs v +. gap | None -> 0. in
  let room = Float.max 0. (right -. x -. (label_x +. 40.) *. z -. aw -. gap -. live_w -. fw) in
  let left = right -. live_w -. Float.min (measure fs s) room -. gap -. aw in
  left, fw, room, live, live_w

(* where the name of a row that reads a binding is: its left and its width, in the card's points *)
let ref_chip_geo t ~z ~fs ~w s =
  let left, _, room, _, _ = wired_geo ~measure:t.measure t ~z ~fs ~x:0. ~w ~fold:false s in
  left, t.measure fs "\xe2\x86\x90" +. 6. *. z +. Float.min (t.measure fs s) room

let paint_value paint ui t ~z ~fs ?size ?(fold = false) ~x ~y ~w ~live (r : P.row) =
  let theme = t.theme in
  let ls = match size with Some s -> s | None -> max 4 (fs - 2) in
  let rh = P.row_height *. z in
  let right = x +. w -. head_pad *. z in
  let fx = right -. field_w *. z and fy = y +. 4. *. z in
  let dim = Pxui.Theme.ink_3 theme in
  match r.chip with
  | P.No_value ->
      (match r.default with
       | Some ("true" | "false" as d) ->
           paint_switch paint theme ~z ~x:(right -. switch_w *. z) ~y:(y +. 5. *. z) (d = "true")
       | Some d ->
           (match number_words d with
            | Some parts ->
                let k = List.length parts in
                let vw = vec_w *. z and gap = 8. *. z in
                let cw = (vw -. gap *. float (k - 1)) /. float k in
                List.iteri (fun c s ->
                  paint_field paint ui ~z ~ls ~theme ~x:(right -. vw +. float c *. (cw +. gap)) ~y:fy ~w:cw ~color:dim s) parts
            | None ->
                let numeric = float_of_string_opt d <> None in
                paint_field paint ui ~z ~ls ~theme ~x:fx ~y:fy ~w:(field_w *. z) ~left:(not numeric && r.control <> P.Choice)
                  ?fraction:(fraction_of r d) ~chevron:(r.control = P.Choice) ~color:dim d)
       | None -> ())
  | Const -> (match r.expr with
      | Some { S.node = S.Num text; _ } ->
          if not live then
            paint_field paint ui ~z ~ls ~theme ~x:fx ~y:fy ~w:(field_w *. z) ?fraction:(fraction_of r text)
              ~color:theme.foreground text
      | Some { S.node = S.Vec elems; _ } ->
          if not live then paint_vector paint ui ~z ~ls ~theme ~right ~y:fy elems
      | Some { S.node = S.Sym (("true" | "false") as b); _ } ->
          paint_switch paint theme ~z ~x:(right -. switch_w *. z) ~y:(y +. 5. *. z) (b = "true")
      | Some e ->
          let text = match e.node with S.Str s -> s | _ -> Flow.Lisp.flat e in
          paint_field paint ui ~z ~ls ~theme ~x:fx ~y:fy ~w:(field_w *. z) ~left:(r.control <> P.Choice)
            ~chevron:(r.control = P.Choice) ~color:theme.foreground text
      | None -> ())
  | Name s ->
      (* a nested node is its own card: the row names its kind; a wired row reads [← source] and
         the live value at the right *)
      let s = wired_name r s in
      let gap = 6. *. z in
      let arrow = "\xe2\x86\x90" in
      let aw = Ui.Paint.text_width paint ~size:fs arrow in
      let left, fw, room, live, live_w =
        wired_geo ~measure:(fun size s -> Ui.Paint.text_width paint ~size s) t ~z ~fs ~x ~w ~fold s in
      let s = fitted paint fs room s in
      if fold then text_in paint ui ~size:fs ~color:theme.accent ~x:(left -. fw) ~y ~h:rh fold_glyph;
      text_in paint ui ~size:fs ~color:(if fold then Pxui.Theme.ink_3 theme else theme.accent) ~x:left ~y ~h:rh arrow;
      text_in paint ui ~size:fs ~color:(Pxui.Theme.ink_2 theme) ~x:(left +. aw +. gap) ~y ~h:rh s;
      Option.iter (fun v ->
        text_in paint ui ~size:fs ~color:theme.foreground ~x:(right -. live_w +. gap) ~y ~h:rh v) live
  | Inline { glyph; text } ->
      let glyph = shown_glyph glyph in
      let gw = Ui.Paint.text_width paint ~size:ls glyph +. 6. *. z in
      text_in paint ui ~size:ls ~color:theme.accent ~x:fx ~y ~h:rh glyph;
      text_in paint ui ~size:ls ~color:(Pxui.Theme.ink_2 theme) ~x:(fx +. gw) ~y ~h:rh
        (fitted paint ls (field_w *. z -. gw) text)

(* where a row's label must stop: the control (or the wired value) takes the right of the row *)
let value_room paint t ~z ~fs ~w ?(fold = false) (r : P.row) =
  let numbers = match r.chip, r.expr with
    | P.Const, Some { S.node = S.Vec _; _ } -> true
    | P.No_value, _ -> (match Option.bind r.default number_words with Some _ -> true | None -> false)
    | _ -> false in
  match r.chip with
  | _ when r.kind = P.Add -> w -. (label_x +. head_pad) *. z
  | P.No_value when r.default = None -> w -. (label_x +. head_pad) *. z
  | P.Const when (match r.expr with Some { S.node = S.Sym ("true" | "false"); _ } -> true | _ -> false) ->
      w -. (label_x +. head_pad +. switch_w +. 6.) *. z
  | P.No_value when (match r.default with Some ("true" | "false") -> true | _ -> false) ->
      w -. (label_x +. head_pad +. switch_w +. 6.) *. z
  | P.Name s ->
      let s = match r.expr with
        | Some { S.node = S.List ({ S.node = S.Sym head; _ } :: _); _ } when E.nested s -> head | _ -> s in
      let live = live_value t s in
      let aw = Ui.Paint.text_width paint ~size:fs "\xe2\x86\x90" in
      let sw = Float.min (Ui.Paint.text_width paint ~size:fs s) (w *. 0.4) in
      let lw = match live with Some v -> Ui.Paint.text_width paint ~size:fs v +. 6. *. z | None -> 0. in
      w -. (label_x +. head_pad) *. z -. aw -. sw -. lw -. 12. *. z -. (if fold then 14. *. z else 0.)
  | _ ->
      w -. (label_x +. head_pad) *. z -. (if numbers then vec_w else field_w) *. z -. 6. *. z

let paint_rows paint ui t ~z ~fs ~x ~y ~w ?(carry = false) (n : P.node) (lines : P.line array) ~selected_row =
  let theme = t.theme in
  let ls = max 4 (fs - 2) in
  let rh = P.row_height *. z in
  Array.iteri (fun k line ->
    let ry = y +. float k *. rh in
    match line with
    | P.Folder (_, name) ->
        (* a folder's label row: its name in the foreground, 8 points in *)
        ignore (cap_in paint ui ~size:ls ~color:theme.foreground ~x:(x +. 9. *. z) ~y:ry ~h:rh name)
    | Row (i, r) ->
        let fold = Hashtbl.mem t.folds (n.path, i) in
        let target = carry && selected_row = Some i in
        if selected_row = Some i then
          Ui.Paint.fill paint ~x:(x +. 1.) ~y:ry ~w:(w -. 2.) ~h:rh
            (if target then Pxui.Theme.hover_fill theme else Pxui.Theme.faint_border theme);
        (* a carried value over a row: the row takes the hover fill, an ink label and an accent underline *)
        if target then
          hline paint ~x:(x +. value_x w *. z) ~y:(ry +. 19. *. z) ~w:(field_w *. z) theme.accent;
        let add = r.kind = P.Add in
        let color = match r.kind with
          | _ when target -> theme.foreground
          | P.Add -> Pxui.Theme.ink_2 theme | Binder | Hole -> theme.accent | _ -> Pxui.Theme.ink_2 theme in
        let size = if add then ls else fs in
        text_in paint ui ~size ~color ~x:(x +. label_x *. z) ~y:ry ~h:rh
          (fitted paint size (Float.max 0. (value_room paint t ~z ~fs ~w ~fold r)) r.label);
        if r.socket then
          paint_socket paint theme r.ty ~connected:(wired r) ~z (x, ry +. 12. *. z);
        if not add then paint_value paint ui t ~z ~fs ~fold ~x ~y:ry ~w ~live:(live_numbers ~z r) r) lines

let paint_outputs paint ui t ~z ~fs ~x ~y ~w (n : P.node) ~nlines =
  let theme = t.theme in
  let ls = max 4 (fs - 2) in
  let rh = P.row_height *. z in
  List.iteri (fun j (name, ty) ->
    let ry = y +. float (nlines + j) *. rh in
    text_in paint ui ~size:fs ~color:(colors t).record ~x:(x +. label_x *. z) ~y:ry ~h:rh ("→ " ^ name);
    let label = Ty.to_string ty in
    let lw = Ui.Paint.text_width paint ~size:ls label in
    text_in paint ui ~size:ls ~color:(Pxui.Theme.ink_2 theme) ~x:(x +. w -. head_pad *. z -. lw) ~y:ry ~h:rh label;
    paint_socket paint theme (Some ty) ~connected:true ~z (x +. w, ry +. 12. *. z)) n.outputs

(* rail rows of a zone: name, role, and what feeds it.  Under an expanded zone's label row they
   are the accumulators, further variables and parameters (the port at the right is the
   variable's); a collapsed zone's card lists every row, its captures too. *)
let paint_rail paint ui t ~z ~fs ~x ~y ~w ~expanded (rail : P.rail_row list) =
  let theme = t.theme in
  let palette = colors t in
  let ls = max 4 (fs - 2) in
  let rh = P.row_height *. z in
  List.iteri (fun i (r : P.rail_row) ->
    let ry = y +. float i *. rh in
    let color = match r.role with
      | P.Var -> theme.accent | Acc -> palette.vec3 | Param -> palette.fn | Capture -> Pxui.Theme.muted theme in
    text_in paint ui ~size:fs ~color ~x:(x +. 13. *. z) ~y:ry ~h:rh (fitted paint fs (56. *. z) r.name);
    let role = match r.role with P.Var -> "in" | Acc -> "acc" | Param -> "param" | Capture -> "from" in
    text_in paint ui ~size:ls ~color:(Pxui.Theme.ink_3 theme) ~x:(x +. 13. *. z +. 58. *. z) ~y:ry ~h:rh role;
    (match r.expr with
     | Some e ->
         let sym = (match e.node with S.Sym _ -> true | _ -> false) in
         let shown = match e.node with S.Sym s -> s | _ -> Flow.Lisp.flat e in
         let ex = x +. 92. *. z in
         let room = Float.max 0. (x +. w -. (if expanded then 24. else 8.) *. z -. ex) in
         if sym then begin
           text_in paint ui ~size:ls ~color:theme.accent ~x:ex ~y:ry ~h:rh "\xe2\x86\x90";
           let aw = Ui.Paint.text_width paint ~size:ls "\xe2\x86\x90 " in
           text_in paint ui ~size:ls ~color:(Pxui.Theme.ink_2 theme) ~x:(ex +. aw) ~y:ry ~h:rh
             (fitted paint ls (Float.max 0. (room -. aw)) shown)
         end else
           text_in paint ui ~size:ls ~color:(Pxui.Theme.ink_2 theme) ~x:ex ~y:ry ~h:rh (fitted paint ls room shown)
     | None -> ());
    if r.key <> None || r.role = P.Capture then
      paint_socket paint theme r.ty ~connected:(r.role = P.Capture || r.expr <> None) ~z (x, ry +. 12. *. z);
    if expanded then
      paint_socket paint theme r.ty ~connected:true ~z (x +. w -. 12. *. z, ry +. 12. *. z)) rail

(* the zone's label row (kit [.zone] label): [FOR] in accent, the binder in ink, "in ..." in ink-2 *)
let zone_binder t (n : P.node) (zn : P.zone) =
  match P.label_row zn with
  | None when zn.kind = P.Fn && String.contains (List.hd (List.rev n.path)) '#' ->
      (* a function written in a row names its owner: [map -> positions], not [fn] *)
      let leaf = List.hd (List.rev n.path) in
      let holder = List.rev (List.tl (List.rev n.path)) @ [ String.sub leaf 0 (String.index leaf '#') ] in
      (match node_of t holder with
       | Some h -> h.head ^ " -> " ^ P.title h, None
       | None -> P.title n, None)
  | Some { name; expr = Some e; _ } -> name, Some (strip_parens (Flow.Lisp.flat e))
  | Some { name; _ } -> name, None
  | None -> P.title n, None

let paint_zone_label paint ui t ~z ~fs ~x ~y ~w ?execution (n : P.node) (zn : P.zone) =
  let theme = t.theme in
  let ls = max 4 (fs - 2) in
  let hh = P.head_height *. z in
  let gap = 8. *. z in
  let label = kind_label zn.kind ^ Option.fold ~none:"" ~some:(fun (e : Flow_graph.Probe.execution) -> " · " ^ e.tier) execution in
  let cw = cap_in paint ui ~size:ls ~color:theme.accent ~x ~y ~h:hh label in
  let name, in_ = zone_binder t n zn in
  let nx = x +. cw +. gap in
  let room = Float.max 0. (x +. w -. 9. *. z -. nx) in
  let name = fitted paint fs room name in
  text_in paint ui ~size:fs ~color:theme.foreground ~x:nx ~y ~h:hh name;
  match in_ with
  | Some e ->
      let ix = nx +. Ui.Paint.text_width paint ~size:fs name +. gap in
      text_in paint ui ~size:fs ~color:(Pxui.Theme.ink_2 theme) ~x:ix ~y ~h:hh
        (fitted paint fs (Float.max 0. (x +. w -. 9. *. z -. ix)) ("in " ^ e))
  | None -> ()

(* the iteration selector at the zone's top right: [‹ 2 / 5 ›] on the sheet fill, 9 points in
   from the zone's edge and 3 down (inside its 1-point border), 20 high; tile-relative points *)
let selector_geo ~measure ~z ~fs ~w ~count ~probe =
  let read = if count > 0 then Printf.sprintf "%d / %d" (probe + 1) count else "no runs" in
  let tw = measure fs read in
  let chev = 6. *. z and gap = 12. *. z in
  let total = chev +. gap +. tw +. gap +. chev in
  w -. head_pad *. z -. total, 3. *. z, total, 20. *. z, read

let paint_selector paint ui t ~z ~fs ~count ~probe (x, y, w) =
  let theme = t.theme in
  let sx, sy, sw, sh, read = selector_geo ~measure:(fun size s -> Ui.text_width ui ~size s) ~z ~fs ~w ~count ~probe in
  let sx = x +. sx and sy = y +. sy in
  Ui.Paint.fill paint ~x:sx ~y:sy ~w:sw ~h:sh theme.input;
  (* the sheet's [.seg] children are ink-3 *)
  let ink = Pxui.Theme.ink_3 theme in
  draw_chevron paint ~z ~at:(sx +. 3. *. z, sy +. sh /. 2.) `Left ink;
  draw_chevron paint ~z ~at:(sx +. sw -. 3. *. z, sy +. sh /. 2.) `Right ink;
  text_in paint ui ~size:fs ~color:ink ~x:(sx +. 18. *. z) ~y:sy ~h:sh read

(* the card's header: the in-port of the first geometry slot, the type square, the name, and at
   the right the view flag, the marks and the kind (a failed node says its code instead) *)
let paint_header paint ui t ~z ~fs ~x ~y ~w (n : P.node) ?execution ?lens_open ?failed ?head ?(shown = P.Card) () =
  let theme = t.theme in
  let ls = max 4 (fs - 2) in
  let hh = P.head_height *. z in
  let viewed = t.display = Some n.path in
  let ink_3 = Pxui.Theme.ink_3 theme in
  let invalid = Pxui.Theme.invalid in
  let square =
    if failed <> None then invalid
    else if n.bypass then ink_3
    else if viewed then theme.foreground
    else ty_color t (Some n.ty) in
  Ui.Paint.fill paint ~x:(x +. head_pad *. z) ~y:(y +. 8. *. z) ~w:(8. *. z) ~h:(8. *. z) square;
  Option.iter (fun (r : P.row) ->
    paint_socket paint theme r.ty ~connected:(wired r) ~z (x +. 1. *. z, y +. port_y *. z)) head;
  let name_x = x +. 23. *. z in
  let right = x +. w -. head_pad *. z in
  let right = match lens_open with
    | Some open_ ->
        draw_chevron paint ~z ~at:(x +. w -. 14. *. z, y +. 12. *. z) (if open_ then `Down else `Right)
          (Pxui.Theme.muted theme);
        x +. w -. 24. *. z
    | None -> right in
  (* the displayed node: the 12-point view flag, a line-3 frame and the accent square inside *)
  let right = if viewed then begin
      let fx = right -. 12. *. z and fy = y +. 6. *. z in
      frame_in paint ~x:fx ~y:fy ~w:(12. *. z) ~h:(12. *. z) ~fill:theme.input (Pxui.Theme.border theme);
      Ui.Paint.fill paint ~x:(fx +. 3. *. z) ~y:(fy +. 3. *. z) ~w:(6. *. z) ~h:(6. *. z) theme.accent;
      fx -. 6. *. z
    end else right in
  let m = marks ?execution n in
  let gap = 5. *. z in
  paint_marks paint ui theme ~size:ls ~gap ~right ~y ~h:hh m;
  let right = right -. marks_width paint ~size:ls ~gap m -. (if m = [] then 0. else 1. *. z) in
  let title = P.title n in
  let name_color =
    if failed <> None then invalid
    else if n.bypass then ink_3
    else if n.synthetic || P.anonymous n then Pxui.Theme.muted theme else theme.foreground in
  let name = fitted paint fs (Float.max 0. (right -. name_x)) title in
  text_in paint ui ~size:fs ~color:name_color ~x:name_x ~y ~h:hh name;
  let name_end = name_x +. Ui.Paint.text_width paint ~size:fs name in
  (* a chip: how many rows have something written, in the kind's place at the right *)
  let right = if shown = P.Chip && P.set_count n > 0 then begin
      let text = Printf.sprintf "+%d" (P.set_count n) in
      text_in paint ui ~size:ls ~color:(Pxui.Theme.ink_2 theme) ~x:(name_end +. 9. *. z) ~y ~h:hh text;
      right end else right in
  let name_end = if shown = P.Chip && P.set_count n > 0 then
      name_end +. 9. *. z +. Ui.Paint.text_width paint ~size:ls (Printf.sprintf "+%d" (P.set_count n)) else name_end in
  (* where the name and the kind meet, the kind yields; a value card has its field there instead *)
  let kind_text = match failed with Some code -> Some code | None ->
    match t.imported n.path with Some file -> Some file
    | None -> if n.zone = None && n.head <> n.name && not (P.value_card n) then Some n.head else None in
  Option.iter (fun text ->
    let room = right -. (name_end +. 6. *. z) in
    if room >= 3. *. Ui.Paint.text_width paint ~size:ls "m" then begin
      let kind = fitted paint ls room text in
      let kw = Ui.Paint.text_width paint ~size:ls kind in
      text_in paint ui ~size:ls ~color:(Pxui.Theme.ink_2 theme) ~x:(right -. kw) ~y ~h:hh kind
    end) kind_text

(* The footer row of a node card or a collapsed zone: the value at the probe, a sparkline across
   the innermost zone, the tags; label-size ink-2 over a line-1 hairline.
   The tags use ↑ for the study's ↥ and t for ◷ (DepartureMono lacks both). *)
let paint_footer paint ui t ~z ~fs (f : Flow_graph.Probe.footer) (ty : Ty.t) ~pad (x, y, w, h) =
  let theme = t.theme in
  let ls = max 4 (fs - 2) in
  let fh = P.foot_height *. z in
  let fy = y +. h -. pad *. z -. fh in
  Ui.Paint.fill paint ~x:(x +. 1.) ~y:fy ~w:(w -. 2.) ~h:1. (Pxui.Theme.faint_border theme);
  let ink_2 = Pxui.Theme.ink_2 theme in
  let ty0 = fy +. 1. and th = fh -. 1. in
  let right = ref (x +. w -. head_pad *. z) in
  let put_right color s =
    let tw = Ui.Paint.text_width paint ~size:ls s in
    text_in paint ui ~size:ls ~color ~x:(!right -. tw) ~y:ty0 ~h:th s;
    right := !right -. tw -. 6. *. z in
  if f.live then put_right theme.accent "t";
  Option.iter (fun (e : Flow_graph.Probe.execution) -> put_right ink_2 e.tier) f.execution;
  let tags = String.concat " · " (List.filter_map Fun.id [ f.branch; f.kept; Option.map (Printf.sprintf "×%d") f.runs;
    Option.bind f.execution (fun e -> Option.map (fun seconds -> Printf.sprintf "%.3f ms" (seconds *. 1000.)) e.seconds) ]) in
  if tags <> "" then
    put_right ink_2 (fitted paint ls (Float.max 0. (!right -. x) *. 0.5) tags);
  let left = x +. label_x *. z in
  (match f.spark with
   | Some (values, at) ->
       let sw = 36. *. z and sh = 10. *. z in
       let sx0 = Float.max (left +. 30. *. z) (!right -. sw) in
       let lo = Array.fold_left Float.min infinity values and hi = Array.fold_left Float.max neg_infinity values in
       let px i = sx0 +. float i /. float (Array.length values - 1) *. sw
       (* flat runs sit on a whole point plus a half, so a 1-point line is not blurred over two rows *)
       and py v = Float.floor (if hi = lo then fy +. 7. *. z +. sh /. 2. else fy +. 7. *. z +. sh -. (v -. lo) /. (hi -. lo) *. sh) +. 0.5 in
       (* at most 16 segments: a 240-iteration series draws as many as a 20-iteration one *)
       let n = Array.length values in
       let step = max 1 (n / 16) in
       let i = ref 0 in
       while !i < n - 1 do
         let j = min (n - 1) (!i + step) in
         Ui.Paint.line paint ~from_:(px !i, py values.(!i)) ~to_:(px j, py values.(j)) ~width:1.
           (ty_color t (Some ty));
         i := j
       done;
       Ui.Paint.fill paint ~x:(px at -. 1.5 *. z) ~y:(py values.(at) -. 1.5 *. z) ~w:(3. *. z) ~h:(3. *. z) theme.accent;
       right := sx0 -. 6. *. z
   | None -> ());
  text_in paint ui ~size:ls ~color:ink_2 ~x:left ~y:ty0 ~h:th
    (fitted paint ls (Float.max 0. (!right -. left)) f.value)

(* The expansion panel under a macro call, in graph units relative to its top: step
   buttons on the first row ("call", 1, 2, ...), the printed step, and the replace
   button with the reading on the last row. *)
let lens_button_box ~len i =
  if i = 0 then (8., 2.), (36., 20.)
  else if i >= len then (8. +. 40. +. float (max 0 (len - 1)) *. 28. +. 4., 2.), (64., 20.)  (* the template *)
  else (8. +. 40. +. float (i - 1) *. 28., 2.), (24., 20.)
let lens_replace_box lh = (8., lh -. P.row_height +. 2.), (196., 20.)  (* a control is 20 high *)
(* the panel's top inside a card of height [h]: above the padding *)
let lens_top h lh = h -. lh -. P.card_pad

let paint_lens paint ui t ~z ~fs (l : P.lens) ~step (x, y, w) ~lh =
  let theme = t.theme in
  let ls = max 4 (fs - 2) in
  Ui.Paint.fill paint ~x:(x +. 1.) ~y ~w:(w -. 2.) ~h:1. (Pxui.Theme.faint_border theme);
  let len = Array.length l.steps in
  for i = 0 to len do
    let (bx, by), (bw, bh) = lens_button_box ~len i in
    let on = i = step in
    if on then Ui.Paint.fill paint ~x:(x +. (bx +. 2.) *. z) ~y:(y +. (by +. bh) *. z -. 1.) ~w:((bw -. 4.) *. z) ~h:1. theme.foreground;
    let label = if i = 0 then "call" else if i = len then "Template" else string_of_int i in
    let tw = Ui.Paint.text_width paint ~size:ls label in
    text_in paint ui ~size:ls ~color:(if on then theme.foreground else Pxui.Theme.ink_3 theme)
      ~x:(x +. (bx +. bw /. 2.) *. z -. tw /. 2.) ~y:(y +. by *. z) ~h:(bh *. z) label
  done;
  let text = if step >= len then (if l.template = "" then "(no definition)" else l.template)
    else match l.error with
      | Some message when step >= len -> message
      | _ -> l.steps.(max 0 step) in
  let lines = String.split_on_char '\n' text in
  List.iteri (fun k line ->
    if k < 16 then
      Ui.Paint.text paint ~at:(x +. 10. *. z, y +. (P.row_height +. 4. +. float k *. 20.) *. z) ~size:fs
        ~color:theme.foreground (fitted paint fs (w -. 20. *. z) (if k = 15 && List.length lines > 16 then line ^ " …" else line))) lines;
  let (rx, ry), (rw, rh) = lens_replace_box lh in
  frame_in paint ~x:(x +. rx *. z) ~y:(y +. ry *. z) ~w:(rw *. z) ~h:(rh *. z) (Pxui.Theme.border theme);
  text_in paint ui ~size:ls ~color:theme.foreground ~x:(x +. (rx +. 8.) *. z) ~y:(y +. ry *. z) ~h:(rh *. z)
    "Replace call with expansion";
  let reading = match l.error with
    | _ when step >= len -> "the macro's template"
    | Some message when step >= len - 1 && step > 0 -> message
    | _ -> if step = 0 then "as written" else Printf.sprintf "after %d expansion step%s" step (if step > 1 then "s" else "") in
  let tw = Ui.Paint.text_width paint ~size:ls reading in
  text_in paint ui ~size:ls ~color:(Pxui.Theme.muted theme) ~x:(x +. w -. head_pad *. z -. tw) ~y:(y +. ry *. z) ~h:(rh *. z)
    (fitted paint ls (w -. (rx +. rw +. 24.) *. z) reading)

(* a card's hairline: line-2, line-3 when selected, composed over the card's own fill (the sheet's
   border sits on its background) *)
let card_edge t ~fill ~selected =
  Color.blend t.theme.foreground fill ~pct:(if selected then 0.70 else 0.85)

(* the card: its fill and a 1-point border inside its box (a stroke is centred on its rectangle) *)
let paint_card paint ~x ~y ~w ~h ~fill ~edge =
  Ui.Paint.fill paint ~x ~y ~w ~h fill;
  Ui.Paint.stroke paint ~x:(x +. 0.5) ~y:(y +. 0.5) ~w:(w -. 1.) ~h:(h -. 1.) edge

(* a point: the 14-point disc in the node's colour with its name beside it (a compound is a ring
   with a dot), brackets when selected; the displayed one wears its flag below the disc *)
let paint_point paint ui t ~z ~fs (p : P.placed) ?failed ~selected (x, y, _, _) =
  let theme = t.theme in
  let c = (x +. 7. *. z, y +. 7. *. z) in
  let ty, macro, bypass, title, viewed = match p.item with
    | P.Item n -> n.ty, n.macro <> None, n.bypass, P.title n, t.display = Some n.path
    | P.Input i -> i.ty, false, false, i.name, false
    | P.Return -> Flow.Ty.geometry, false, false, "return", false in
  let color = if failed <> None then Pxui.Theme.invalid else ty_color t (Some ty) in
  if macro then begin
    Ui.Paint.circle paint ~at:c ~radius:(7. *. z) ~fill:theme.Pxui.panel ~stroke:color ();
    Ui.Paint.circle paint ~at:c ~radius:(2. *. z) ~fill:color ()
  end else Ui.Paint.circle paint ~at:c ~radius:(7. *. z) ~fill:color ();
  let name_color = if failed <> None then Pxui.Theme.invalid else if bypass then Pxui.Theme.ink_3 theme
    else theme.foreground in
  text_in paint ui ~size:fs ~color:name_color ~x:(x +. 22. *. z) ~y:(y +. 7. *. z -. 12. *. z) ~h:(24. *. z) title;
  if viewed then begin
    let fy = y +. 21. *. z in
    frame_in paint ~x ~y:fy ~w:(12. *. z) ~h:(12. *. z) ~fill:theme.input (Pxui.Theme.border theme);
    Ui.Paint.fill paint ~x:(x +. 3. *. z) ~y:(fy +. 3. *. z) ~w:(6. *. z) ~h:(6. *. z) theme.accent
  end;
  if selected then
    Ui.Paint.brackets paint ~x ~y ~w:(14. *. z) ~h:(14. *. z) ~offset:5. ~length:6. theme.accent

let paint_node paint ui t ~z ~fs ?footer ?lens_step ?(hovered = false) ?(carry = false) ?(out_wired = true) ~shown ?failed
    (p : P.placed) (n : P.node) ~selected ~row_hover (x, y, w, h) =
  if shown = P.Point then paint_point paint ui t ~z ~fs p ?failed ~selected (x, y, w, h) else
  let theme = t.theme in
  let z_ = n.zone in
  let execution = Option.bind t.records (fun records ->
    let chain = Option.value ~default:[] (Hashtbl.find_opt t.chains n.path) in
    Flow_graph.Probe.execution records n.path ~probes:(List.map t.probe chain)) in
  let execution = match execution with Some _ -> execution | None -> Option.bind footer (fun f -> f.Flow_graph.Probe.execution) in
  let ls = max 4 (fs - 2) in
  let stacked = match z_ with Some _ -> p.collapsed | None -> false in
  if stacked then
    List.iter (fun d ->
      paint_card paint ~x:(x +. d *. z) ~y:(y +. d *. z) ~w ~h ~fill:theme.input
        ~edge:(card_edge t ~fill:theme.input ~selected:false)) [ 6.; 3. ];
  (* a chip is the header of a card that is laid out larger: the zoom, not the document, shrank it *)
  let h = if shown = P.Chip && z_ = None then P.head_height *. z else h in
  let fill = if hovered && not selected then Pxui.Theme.hover_fill theme else theme.input in
  let edge = match failed with
    | Some _ -> Pxui.Theme.invalid
    | None -> card_edge t ~fill ~selected in
  paint_card paint ~x ~y ~w ~h ~fill ~edge;
  let lens = match n.lens, lens_step with Some l, Some step -> Some (l, step) | _ -> None in
  let lh = match lens with Some (l, step) -> P.lens_height l ~step | None -> 0. in
  let head = List.find_opt (fun (r : P.row) -> r.head) n.rows in
  (match z_ with
   | Some zn ->
       (* a collapsed zone: its kind as a label, the card's name, the toggle chevron *)
       paint_zone_label paint ui t ~z ~fs ~x:(x +. head_pad *. z) ~y ~w:(w -. head_pad *. z) ?execution n zn
   | None ->
       paint_header paint ui t ~z ~fs ~x ~y ~w n ~shown ?failed ?head ?execution
         ?lens_open:(if n.lens <> None && shown <> P.Chip then Some (lens <> None) else None) ());
  (* the displayed result at the end of its chain has no out-port: the view flag takes the place *)
  if out_wired || t.display <> Some n.path then
    paint_socket paint theme (Some n.ty) ~connected:out_wired ~z (x +. w -. 1. *. z, y +. port_y *. z);
  (* a ghost value: where the footer is not drawn, the value at the pinned iteration beside the out-port *)
  (match footer with
   | Some f when shown <> P.Full && z_ = None && out_wired ->
       let ink_3 = Pxui.Theme.ink_3 theme in
       text_in paint ui ~size:ls ~color:ink_3 ~x:(x +. w +. 8. *. z) ~y:(y +. (port_y -. 8.) *. z) ~h:(16. *. z)
         (fitted paint ls (72. *. z) f.value)
   | _ -> ());
  (* a literal binding is the header alone: its value in the field at the right *)
  (match n.rows with
   | [ r ] when P.value_card n -> paint_value paint ui t ~z ~fs ~size:fs ~x ~y ~w ~live:(live_numbers ~z r) r
   | _ -> ());
  let body_y = y +. P.body_top *. z in
  if shown <> P.Chip then begin
  (match n.note with
   | Some note ->
       let bg, ink = note_colors theme in
       Ui.Paint.fill paint ~x:(x +. 1.) ~y:body_y ~w:(w -. 2.) ~h:(P.row_height *. z) bg;
       let first = match String.index_opt note '\n' with Some i -> String.sub note 0 i | None -> note in
       text_in paint ui ~size:ls ~color:ink ~x:(x +. label_x *. z) ~y:body_y ~h:(P.row_height *. z)
         (fitted paint ls (w -. (label_x +. head_pad) *. z) first)
   | None -> ());
  let rows_y = body_y +. (if n.note <> None then P.row_height *. z else 0.) in
  (match z_ with
   | None ->
       paint_rows paint ui t ~z ~fs ~x ~y:rows_y ~w ~carry n p.lines ~selected_row:row_hover;
       if n.head = "list" || n.head = "str" then
         Array.iteri (fun k -> function
           | P.Row (_, ({ kind = P.Rest; key = E.Pos j; _ } : P.row)) when j >= 1 ->
               text_in paint ui ~size:fs ~color:theme.accent ~x:(x +. w -. 20. *. z) ~y:(rows_y +. float k *. P.row_height *. z)
                 ~h:(P.row_height *. z) "↑"
           | _ -> ()) p.lines;
       paint_outputs paint ui t ~z ~fs ~x ~y:rows_y ~w n ~nlines:(Array.length p.lines)
   | Some zn when p.collapsed -> paint_rail paint ui t ~z ~fs ~x ~y:body_y ~w ~expanded:false zn.rail
   | Some _ -> ());
  if shown = P.Full then
    Option.iter (fun f -> paint_footer paint ui t ~z ~fs f n.ty ~pad:(P.card_pad +. lh) (x, y, w, h)) footer;
  Option.iter (fun (l, step) -> paint_lens paint ui t ~z ~fs l ~step (x, y +. h -. (lh +. P.card_pad) *. z, w) ~lh) lens
  end;
  if selected then Ui.Paint.brackets paint ~x ~y ~w ~h ~offset:3. theme.accent

(* does anything inside the zone read this name (a loop variable shows its port only then)? *)
let zone_uses (zn : P.zone) names =
  let reads name = List.mem (root_name name) (List.map root_name names) in
  let rec scope (s : P.scope) =
    List.exists (fun (n : P.node) ->
      List.exists (fun (r : P.row) -> List.exists reads (P.sources r)) n.rows
      || (match n.zone with
          | Some z -> List.exists (fun (r : P.rail_row) ->
              match r.expr with Some e -> List.exists reads (E.free_names e) | None -> false) z.rail
              || scope z.scope
          | None -> false)) s.nodes
    || (match s.result with P.Link l -> reads l | _ -> false) in
  scope zn.scope

(* the expanded zone's label row, selector, rail rows and ports, over its tint; the cards inside
   are wired directly across its edge *)
let paint_zone_frame paint ui t ~z ~fs ?footer (n : P.node) (zn : P.zone) ~selected ~out_wired (x, y, w, h) ~probe ~count =
  let theme = t.theme in
  let execution = Option.bind t.records (fun records ->
    let chain = Option.value ~default:[] (Hashtbl.find_opt t.chains n.path) in
    Flow_graph.Probe.execution records n.path ~probes:(List.map t.probe chain)) in
  (* the zone's own footer under its body *)
  let h = match footer with
    | Some f -> paint_footer paint ui t ~z ~fs f n.ty ~pad:0. (x, y, w, h); h -. P.foot_height *. z
    | None -> h in
  (* the label clears the collection's in-port when a name feeds it *)
  let plain = zn.kind = P.For in
  let in_wired = not plain && (match P.label_row zn with
    | Some { key = Some _; expr = Some e; _ } -> E.free_names e <> [] | _ -> false) in
  let ind = if in_wired then 8. *. z else 0. in
  paint_zone_label paint ui t ~z ~fs ~x:(x +. ind) ~y ~w:(w -. ind) ?execution n zn;
  (* a plain [for] has no ports of its own on its edge: the label reads as the sheet's, and the
     yielded card wires straight to its consumer *)
  if not plain then
    paint_socket paint theme (Some n.ty) ~connected:out_wired ~z (x +. w -. 1. *. z, y +. port_y *. z);
  (match P.label_row zn with
   | Some r ->
       (* the collection's in-port when a name feeds it; the variable's out-port once something reads it *)
       (match r.expr with
        | Some e when (not plain) && r.key <> None && E.free_names e <> [] ->
            paint_socket paint theme r.ty ~connected:true ~z (x +. 1. *. z, y +. port_y *. z)
        | _ -> ());
       if zone_uses zn r.names then
         paint_socket paint theme r.ty ~connected:true ~z (x +. 12. *. z, y +. 36. *. z)
   | None -> ());
  if zn.kind <> P.Let && zn.kind <> P.State && zn.kind <> P.Branch then paint_selector paint ui t ~z ~fs ~count ~probe (x, y, w);
  paint_rail paint ui t ~z ~fs ~x ~y:(y +. P.head_height *. z) ~w ~expanded:true (P.extra_rails zn);
  if selected then Ui.Paint.brackets paint ~x ~y ~w ~h ~offset:3. theme.accent

(* a graph input is a value node: a header-only card with its value in a field at the right *)
let paint_input paint ui t ~z ~fs (i : P.input) ~selected (x, y, w, h) =
  let theme = t.theme in
  let hh = P.head_height *. z in
  paint_card paint ~x ~y ~w ~h ~fill:theme.input ~edge:(card_edge t ~fill:theme.input ~selected);
  Ui.Paint.fill paint ~x:(x +. head_pad *. z) ~y:(y +. 8. *. z) ~w:(8. *. z) ~h:(8. *. z) (ty_color t (Some i.ty));
  let fx = x +. value_x P.node_width *. z in
  text_in paint ui ~size:fs ~color:theme.foreground ~x:(x +. 23. *. z) ~y ~h:hh
    (fitted paint fs (Float.max 0. (fx -. 6. *. z -. (x +. 23. *. z))) i.name);
  (* a number is the live editor's (built in [update] at 13 points, like the sheet's header value);
     anything else is painted here *)
  (match i.default with
   | Some { S.node = S.Num _; _ } when z >= 0.5 -> ()
   | Some d ->
       let text = match d.node with S.Str s -> s | _ -> Flow.Lisp.flat d in
       let numeric = (match d.node with S.Num _ -> true | _ -> false) in
       paint_field paint ui ~z ~ls:fs ~theme ~x:fx ~y:(y +. 4. *. z) ~w:(field_w *. z) ~left:(not numeric)
         ~color:theme.foreground text
   | None ->
       paint_field paint ui ~z ~ls:fs ~theme ~x:fx ~y:(y +. 4. *. z) ~w:(field_w *. z) ~left:true
         ~color:(Pxui.Theme.ink_3 theme) (Ty.to_string i.ty));
  paint_socket paint theme (Some i.ty) ~connected:true ~z (x +. w -. 1. *. z, y +. port_y *. z);
  if selected then Ui.Paint.brackets paint ~x ~y ~w ~h ~offset:3. theme.accent

let paint_return paint ui t ~z ~fs (s : P.scope) ~selected (x, y, w, h) =
  let theme = t.theme in
  let hh = P.head_height *. z and rh = P.row_height *. z in
  paint_card paint ~x ~y ~w ~h ~fill:theme.input ~edge:(card_edge t ~fill:theme.input ~selected);
  Ui.Paint.fill paint ~x:(x +. head_pad *. z) ~y:(y +. 8. *. z) ~w:(8. *. z) ~h:(8. *. z) theme.foreground;
  text_in paint ui ~size:fs ~color:theme.foreground ~x:(x +. 23. *. z) ~y ~h:hh "return";
  let ry = y +. P.body_top *. z in
  text_in paint ui ~size:fs ~color:(Pxui.Theme.ink_2 theme) ~x:(x +. label_x *. z) ~y:ry ~h:rh "result";
  let right = x +. w -. head_pad *. z in
  (match s.result with
   | P.Link l ->
       let arrow = "\xe2\x86\x90" in
       let aw = Ui.Paint.text_width paint ~size:fs arrow and gap = 6. *. z in
       let l = fitted paint fs (w *. 0.5) l in
       let lw = Ui.Paint.text_width paint ~size:fs l in
       text_in paint ui ~size:fs ~color:theme.accent ~x:(right -. lw -. gap -. aw) ~y:ry ~h:rh arrow;
       text_in paint ui ~size:fs ~color:(Pxui.Theme.ink_2 theme) ~x:(right -. lw) ~y:ry ~h:rh l
   | Node _ -> ()
   | Literal e ->
       let text = fitted paint fs (w *. 0.5) (Flow.Lisp.flat e) in
       let tw = Ui.Paint.text_width paint ~size:fs text in
       text_in paint ui ~size:fs ~color:theme.foreground ~x:(right -. tw) ~y:ry ~h:rh text);
  paint_socket paint theme None ~connected:(s.result <> P.Literal (S.make (S.Sym "nil"))) ~z (x, ry +. 12. *. z);
  if selected then Ui.Paint.brackets paint ~x ~y ~w ~h ~offset:3. theme.accent

let paint_polyline ~clip paint ~width color pts =
  let rec go = function
    | a :: (b :: _ as rest) ->
        (match clip_segment a b clip with
         | Some (a, b) -> Ui.Paint.line paint ~from_:a ~to_:b ~width color
         | None -> ());
        go rest
    | _ -> () in
  go pts

(* the dashes of a loop's feedback or a switch's inactive wire *)
let paint_dashed_polyline paint ~width color pts =
  let rec go = function
    | a :: (b :: _ as rest) -> dashed_line paint ~width color a b; go rest
    | _ -> () in
  go pts

(* a bend: the 5-point square of the wire's colour on the line (kit [.bend]) *)
let paint_bends paint ~z color pts =
  match pts with
  | _ :: (_ :: _ as rest) ->
      let rec go = function
        | (x, y) :: (_ :: _ as more) ->
            let s = Float.max 3. (5. *. z) in
            Ui.Paint.fill paint ~x:(x -. 2. *. z) ~y:(Float.floor (y -. 2.5 *. z)) ~w:s ~h:s color; go more
        | _ -> () in
      go rest
  | _ -> ()

let tag_name t (w : wire) = match w.from with
  | Some p ->
      let n = match node_of t p with Some n -> P.title n | None -> List.hd (List.rev p) in
      if String.length n > 14 then String.sub n 0 13 ^ "~" else n
  | None -> ""

let paint_background paint t ~viewport ~wires (zones : (P.node * P.zone * P.placed * float * float) list) =
  let theme = t.theme in
  let vx, vy, vw, vh = viewport in
  let z = t.zoom in
  (* a dot every 24 points, thinned by powers of two so the dots stay 24 screen points apart or more *)
  let thin = if z >= 1. then 1. else 2. ** Float.ceil (Float.log2 (1. /. z) -. 1e-9) in
  let spacing = 24. *. z *. thin in
  Ui.Paint.fill paint ~x:vx ~y:vy ~w:vw ~h:vh theme.panel;
  Ui.Paint.grid paint ~x:vx ~y:vy ~w:vw ~h:vh
    ~origin:(vx +. Float.rem t.pan_x spacing -. 1., vy +. Float.rem t.pan_y spacing -. 1.) ~spacing ~dot:2.
    (Color.with_alpha theme.foreground 66);
  if z >= 0.5 then begin
    let px = 480. *. z and py = 192. *. z in
    let rem v m = let r = Float.rem v m in if r < 0. then r +. m else r in
    let x0 = vx +. rem (t.pan_x +. (84. *. z)) px and y0 = vy +. rem (t.pan_y +. (84. *. z)) py in
    let line = Pxui.Theme.border theme in
    let cx = ref x0 in
    while !cx < vx +. vw do
      let cy = ref y0 in
      while !cy < vy +. vh do
        Ui.Paint.fill paint ~x:(!cx -. 4.) ~y:!cy ~w:9. ~h:1. line;
        Ui.Paint.fill paint ~x:!cx ~y:(!cy -. 4.) ~w:1. ~h:9. line;
        cy := !cy +. py
      done;
      cx := !cx +. px
    done
  end;
  List.iter (fun ((node : P.node), (zn : P.zone), (p : P.placed), ax, ay) ->
    let taken = if zn.kind <> P.Branch then false else
      match t.records, List.rev node.path with
      | Some records, leaf :: outer ->
          (match String.rindex_opt leaf '#' with
           | None -> false
           | Some split ->
               let parent = List.rev outer @ [String.sub leaf 0 split] in
               let arm = String.sub leaf (split + 1) (String.length leaf - split - 1) in
               (match node_of t parent with
                | None -> false
                | Some parent ->
                    let index = if arm = "else" then List.length (List.filter (fun (r : P.row) ->
                      match r.key with E.Arm i -> i >= 0 | _ -> false) parent.rows)
                      else if arm = "then" then 0 else int_of_string (String.sub arm 5 (String.length arm - 5)) - 1 in
                    let chain = Option.value ~default:[] (Hashtbl.find_opt t.chains parent.path) in
                    Flow_graph.Probe.taken_arm records parent ~probes:(List.map t.probe chain) = Some index))
      | _ -> false in
    paint_zone ~taken paint theme zn.kind (sx t ax, sy t ay, p.w *. z, p.h *. z)) zones;
  (* titled frames sit under the wires *)
  List.iter (fun (scope_path, (ox, oy)) ->
    List.iter (fun (title, (fx, fy), (fw, fh)) ->
      let x = sx t (ox +. fx) and y = sy t (oy +. fy) in
      frame_in paint ~x ~y ~w:(fw *. z) ~h:(fh *. z) ~fill:(Color.with_alpha theme.foreground 8)
        (Pxui.Theme.edge theme);
      Ui.Paint.text paint ~at:(x +. 6., y +. 4.) ~size:(max 7 (int_of_float (10. *. z)))
        ~color:(Pxui.Theme.muted theme) title;
      (* delete cross top right, resize grip bottom right *)
      Ui.Paint.text paint ~at:(x +. fw *. z -. 14., y +. 4.) ~size:(max 7 (int_of_float (10. *. z)))
        ~color:(Pxui.Theme.muted theme) "x";
      let ink = Pxui.Theme.ink_3 theme in
      Ui.Paint.fill paint ~x:(x +. fw *. z -. 4.) ~y:(y +. fh *. z -. 11.) ~w:1. ~h:8. ink;
      Ui.Paint.fill paint ~x:(x +. fw *. z -. 11.) ~y:(y +. fh *. z -. 4.) ~w:8. ~h:1. ink) (frame_list t scope_path)) t.geo.origins;
  let chain = chains_of t in
  let tag_size = max 5 (int_of_float (Float.round (11. *. z))) in
  List.iter (fun i ->
    let w = t.geo.wires.(i) in
    let pts = List.map (fun (x, y) -> sx t x, sy t y) w.pts in
    let is_selected = match t.selected_wire, w.target with
      | Some (sp, sk, _), Some (tp, tk, _) -> sp = tp && sk = tk
      | _ -> false in
    let is_highlighted = List.mem w t.highlighted in
    let lit = Option.map (fun c -> wire_lit c w) chain in
    let width = if is_highlighted || lit = Some true then Float.max 2. (2.4 *. z) else Float.max 1. (1.5 *. z) in
    (* a switch's active wire is solid and accented, the others dashed grey *)
    let layout = match w.target with
      | Some (p, E.Pos i, _) -> Option.map (fun a -> i = a) (List.assoc_opt p t.switches)
      | _ -> None in
    let color = if is_selected then theme.foreground else if layout = Some true then theme.accent
      else if layout = Some false then Pxui.Theme.muted theme else ty_color t w.ty in
    let color = if lit = Some false then Color.with_alpha color 40 else color in
    if tagged t w && not is_selected then begin
      (* a long wire reads as a stub and the source's name at the use site *)
      let (ax, ay) = sx t (fst w.a), sy t (snd w.a) and (bx, by) = sx t (fst w.b), sy t (snd w.b) in
      let stub = 14. *. z in
      Ui.Paint.line paint ~from_:(ax, ay) ~to_:(ax +. stub, ay) ~width color;
      Ui.Paint.line paint ~from_:(bx -. stub, by) ~to_:(bx, by) ~width color;
      let name = tag_name t w in
      let tw = Ui.Paint.text_width paint ~size:tag_size name in
      Ui.Paint.text paint ~at:(bx -. stub -. 4. -. tw, by -. float tag_size /. 2. -. 1.) ~size:tag_size
        ~color:(if lit = Some false then Color.with_alpha (Pxui.Theme.ink_2 theme) 60 else Pxui.Theme.ink_2 theme) name
    end else begin
      if layout = Some false || w.dashed then paint_dashed_polyline paint ~width color pts
      else paint_polyline ~clip:viewport paint ~width color pts;
      if t.wires = `Straight then paint_bends paint ~z color pts
    end) wires

(* ---------------------------------------------------------------- update *)
