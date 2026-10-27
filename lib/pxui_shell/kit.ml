open Rays

(* Kit rev 3 pieces the chrome shares: where text sits in a 24-point bar, and the text button. *)

  module Ui = Pxui.Ui
  let text_y ui y h = Ui.text_top ui y h
  let cap_y ui y h = Ui.text_top ui ~size:(max 8 (Ui.font_size ui - 2)) y h
  let cap_size ui = max 8 (Ui.font_size ui - 2)

  (* the width of a label ({!Ui.Paint.cap}): upper case, 0.08 em of tracking after each letter *)
  let cap_width ui text =
    let count = ref 0 in
    String.iter (fun c -> if Char.code c land 0xC0 <> 0x80 then incr count) text;
    Ui.text_width ui ~size:(cap_size ui) (String.uppercase_ascii text)
    +. (0.08 *. float (cap_size ui) *. float !count)

  (* the width of a text button, the kit's [.btn]: a transparent 1-point edge, 6, the label, 6 and
     the key, 6, the edge *)
  let button_width ui ?hint ?(icon = false) label =
    14. +. Ui.text_width ui label +. (if icon then 13. else 0.)
    +. (match hint with Some hint -> 6. +. Ui.text_width ui ~size:(cap_size ui) hint | None -> 0.)

  (* A button is its text and, in ink-3, its key: a fill on hover and press, the control fill
     while [active]; [primary] is the one outlined button of a panel.  [icon] draws a play
     triangle or a stop square before the label. *)
  let button ui ~key ~at:(bx, by) ~w ?(h = 20.) ?(enabled = true) ?(active = false) ?(primary = false)
      ?(centered = false) ?hint ?icon label =
    let theme = Ui.theme ui in
    let box = Ui.box ui ~flags:Ui.(clickable + blocking) ~w:(Ui.Px w) ~h:(Ui.Px h) ~at:(bx, by) key in
    let signal = Ui.signal ui box in
    Ui.draw ui box (fun paint (x, y, w, h) ->
      Ui.paint_button_ground paint theme ~held:(enabled && signal.held)
        ~hovered:(enabled && signal.hovered) ~on:active ~primary (x, y, w, h);
      let color = if enabled then theme.foreground else Pxui.Theme.ink_3 theme in
      let tx = match icon with
        | None when centered -> x +. Float.floor ((w -. Ui.Paint.text_width paint label) /. 2.)
        | None -> x +. 7.
        | Some shape ->
            let cy = y +. (h /. 2.) in
            (match shape with
             | `Play ->
                 (* the sheet's 7 x 8 triangle: half-point scanlines, so a Retina pixel row is one *)
                 for row = 0 to 15 do
                   let top = cy -. 4. +. (0.5 *. float row) in
                   let reach = 1. -. (Float.abs (top +. 0.25 -. cy) /. 4.) in
                   Ui.Paint.fill paint ~x:(x +. 7.) ~y:top ~w:(7. *. reach) ~h:0.5 color
                 done
             | `Stop -> Ui.Paint.fill paint ~x:(x +. 7.) ~y:(cy -. 4.) ~w:8. ~h:8. color);
            x +. 20. in
      Ui.Paint.text paint ~at:(tx, text_y ui y h) ~color label;
      Option.iter (fun hint ->
        Ui.Paint.text paint ~size:(cap_size ui) ~color:(Pxui.Theme.ink_3 theme)
          ~at:(tx +. Ui.Paint.text_width paint label +. 6., cap_y ui y h) hint) hint);
    signal.clicked && enabled

  (* A colour: a 20-point swatch, then its hex field to the right edge of the control column;
     the hex text typed (or [hex] unchanged).  [key] names the pair; [at] is the control column. *)
  let colour ui ~key ~at:(x, y) ~w ~swatch ~hex =
    let box = Ui.box ui ~at:(x, y) ~w:(Ui.Px 20.) ~h:(Ui.Px 20.) ("swatch-" ^ key) in
    Ui.draw ui box (fun paint (sx, sy, sw, sh) ->
      (* a 20-point square with its border inside *)
      Ui.Paint.fill paint ~x:sx ~y:sy ~w:sw ~h:sh swatch;
      Ui.Paint.stroke paint ~x:(sx +. 0.5) ~y:(sy +. 0.5) ~w:(sw -. 1.) ~h:(sh -. 1.)
        (Pxui.Theme.edge (Ui.theme ui)));
    fst (Ui.value_field ui ~at:(x +. 28., y) ~w:(w -. 28.) ~h:20.
      ~left:true ~valid:(fun t -> Result.is_ok (Rays_math.Color.hex t)) ("hex-" ^ key) hex)

  (* A vector: three cells in the control column, 8 between, each with its axis letter in ink-3
     drawn on [box] (the row); [cell index ~x ~w] makes the cell's own field (x, w relative to
     [box]) and returns what it asks for.  [reserve] keeps room at the right for a row's toggle. *)
  let number ui ~key ~at ~w ?(h = 20.) ?size ~kind ?range ?display ?edit ?left ?trail ?valid text =
    let module N = Editor_core.Number in
    let valid = Option.value valid ~default:(N.valid kind) in
    fst (Ui.value_field ui ~at ~w ~h ?size ?display ?fraction:(Option.bind range (fun r -> N.fraction r text))
      ~scrub:(N.scrub kind ?range) ?edit ?left ?trail ~valid key text)

  let vector ui box ~at:(cx, cy) ~w ?(reserve = 0.) ?(axes = [ "x"; "y"; "z" ]) cell =
    let n = float (List.length axes) in
    let cell_w = (w -. reserve -. (8. *. (n -. 1.))) /. n in
    List.concat (List.mapi (fun index axis ->
      (* the cells' edges land on whole points, as the sheet's do *)
      let start = cx +. float index *. (cell_w +. 8.) in
      let fx = Float.round start and fw = Float.round (start +. cell_w) -. Float.round start in
      Ui.draw ui box (fun paint (x, y, _, _) ->
        Ui.Paint.text paint ~at:(x +. fx +. 2., cap_y ui (y +. cy -. 0.5) 20.)
          ~size:(cap_size ui) ~color:(Pxui.Theme.ink_3 (Ui.theme ui)) axis);
      cell index ~x:fx ~w:fw) axes)

  (* The kit's switch: 28 x 14, a line-3 edge, an 8-point knob that is ink-3 on the track at the left
     and the accent on white at the right.  True on a click. *)
  let switch ui ~key ~at:(sx, sy) on =
    let box = Ui.box ui ~flags:Ui.(clickable + blocking) ~w:(Ui.Px 28.) ~h:(Ui.Px 14.) ~at:(sx, sy) key in
    let signal = Ui.signal ui box in
    Ui.draw ui box (fun paint (x, y, w, h) ->
      let theme = Ui.theme ui in
      Ui.Paint.fill paint ~x ~y ~w ~h (if on then theme.input else theme.track);
      Ui.Paint.stroke paint ~x:(x +. 0.5) ~y:(y +. 0.5) ~w:(w -. 1.) ~h:(h -. 1.) (Pxui.Theme.border theme);
      Ui.Paint.fill paint ~x:(x +. (if on then 17. else 3.)) ~y:(y +. 3.) ~w:8. ~h:8.
        (if on then theme.accent else Pxui.Theme.ink_3 theme));
    signal.clicked

  (* Text tabs: the one in use is ink with a 1-point underline, the others ink-3.  Laid out
     leftwards from [right]; the index clicked. *)
  let segments ui ~key ~right ~y labels active =
    let h = 20. in
    let theme = Ui.theme ui in
    let widths = List.map (fun label -> Ui.text_width ui label) labels in
    let total = List.fold_left ( +. ) (12. *. float (max 0 (List.length labels - 1))) widths in
    let x = ref (right -. total) and clicked = ref None in
    List.iteri (fun index label ->
      let w = List.nth widths index in
      let box = Ui.box ui ~flags:Ui.(clickable + blocking) ~w:(Ui.Px (w +. 8.)) ~h:(Ui.Px h)
          ~at:(!x -. 4., y) (Printf.sprintf "%s-%d" key index) in
      let signal = Ui.signal ui box in
      if signal.clicked then clicked := Some index;
      Ui.draw ui box (fun paint (x, y, w, h) ->
        let on = index = active in
        Ui.Paint.text paint ~at:(x +. 4., text_y ui y h)
          ~color:(if on || signal.hovered then theme.foreground else Pxui.Theme.ink_3 theme) label;
        if on then Ui.Paint.fill paint ~x:(x +. 4.) ~y:(y +. h -. 1.) ~w:(w -. 8.) ~h:1. theme.foreground);
      x := !x +. w +. 12.) labels;
    !clicked, right -. total
