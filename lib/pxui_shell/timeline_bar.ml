open Rays

type intent =
  | Pause_toggle
  | Stop_playback
  | Reset_playback
  | Seek_playback of int64
  | Set_end of int

(* The strip of the workspace sheet is one 24-point bar: Play, Stop, F, the frame field, the time and
     the ruler to the edge.  A taller panel (timeline.html) has the bar, Reset, the rule, Frame, Time and End,
     a hairline and the ruler under it.  [edge] draws the line-2 hairline above a strip that has
     no gutter over it. *)
let draw ui ~bounds:(x, y, width, height) ?(edge = false) ~playing ~frame ~time ~max_frame () =
  let module Ui = Pxui.Ui in
  let theme = Ui.theme ui in
  let fx = float x and fy = float y and fw = float width and fh = float height in
  let bar =
    Ui.box ui
      ~flags:Ui.(clip + clickable)
      ~w:(Ui.Px fw) ~h:(Ui.Px fh) ~at:(fx, fy) "workspace-timeline"
  in
  let top = if edge then 1. else 0. in
  Ui.draw ui bar (fun paint (x, y, w, h) ->
      Ui.Paint.fill paint ~x ~y ~w ~h theme.panel;
      if edge then Ui.Paint.fill paint ~x ~y ~w ~h:1. (Pxui.Theme.edge theme));
  let tall = fh -. top >= 56. in
  let bar_h = if tall then 24. else fh -. top in
  Ui.within ui bar (fun () ->
      let cy = top +. ((bar_h -. 20.) /. 2.) in
      let cx = ref 12. in
      let button key ?icon ?hint ?active ?enabled label =
        let w = Kit.button_width ui ~icon:(icon <> None) ?hint label in
        let clicked = Kit.button ui ~key ~at:(!cx, cy) ~w ?icon ?hint ?active ?enabled label in
        cx := !cx +. w +. 8.;
        clicked
      in
      let pause =
        button "timeline-play" ~icon:`Play ~active:playing
          ?hint:(if tall then Some "Space" else None)
          "Play"
      in
      let stop = button "timeline-stop" ~icon:`Stop "Stop" in
      let reset = tall && button "timeline-reset" "Reset" in
      (* the rule between the buttons and the fields: 4 points of margin each side *)
      let rule_x = !cx +. 4. in
      if tall then cx := !cx +. 4. +. 1. +. 4. +. 8.;
      let label text =
        let at = !cx in
        cx := !cx +. Kit.cap_width ui text +. 8.;
        at
      in
      let frame_label = label (if tall then "Frame" else "F") in
      let field_w = if tall then 64. else 48. in
      let field_x = !cx in
      let current = Int64.to_string frame in
      let typed =
        fst
          (Ui.value_field ui ~at:(field_x, cy) ~w:field_w ~h:20.
             ~valid:(fun text -> Int64.of_string_opt (String.trim text) <> None)
             "timeline-frame-field" current)
      in
      cx := !cx +. field_w +. 8.;
      let time_label = if tall then label "Time" else !cx in
      let readout = Printf.sprintf "%.2f s" time in
      let readout_x = !cx in
      cx := !cx +. Ui.text_width ui readout +. 8.;
      (* the last frame, typed, at the end of a tall panel's bar *)
      let last = string_of_int max_frame in
      let end_x = fw -. 12. -. 64. in
      let ended =
        if not tall then last
        else
          fst
            (Ui.value_field ui ~at:(end_x, cy) ~w:64. ~h:20.
               ~valid:(fun text ->
                 match int_of_string_opt (String.trim text) with Some n -> n >= 1 | None -> false)
               "timeline-end-field" last)
      in
      let ruler_x = if tall then 0. else !cx in
      let ruler_y = if tall then top +. bar_h +. 1. else top in
      let ruler_w = Float.max 0. (fw -. ruler_x) and ruler_h = fh -. ruler_y in
      (* the ruler's own line-2 edge is its first column; the ticks divide what is inside it *)
      let inner_x = if tall then 0. else 1. in
      let span = Float.max 1. (ruler_w -. inner_x) in
      let range = Float.max (Int64.to_float frame) (float_of_int max_frame) in
      let ruler =
        Ui.box ui
          ~flags:Ui.(clickable + blocking)
          ~at:(ruler_x, ruler_y) ~w:(Ui.Px ruler_w) ~h:(Ui.Px ruler_h) "timeline-scrub"
      in
      let signal = Ui.signal ui ruler in
      let scrub =
        if signal.button = Some Input.LeftButton && (signal.held || signal.released) && ruler_w > 1.
        then begin
          let px = fst (if signal.released then signal.release_point else signal.pointer) in
          let rx, _, _, _ = Ui.rect ui ruler in
          Some (Float.round (Float.max 0. (Float.min 1. ((px -. rx -. inner_x) /. span)) *. range))
        end
        else None
      in
      Ui.draw ui bar (fun paint (x, y, w, _) ->
          let y = y +. top in
          let h = bar_h in
          Ui.Paint.cap paint
            ~at:(x +. frame_label, Kit.cap_y ui y h)
            (if tall then "Frame" else "F");
          if tall then begin
            Ui.Paint.cap paint ~at:(x +. time_label, Kit.cap_y ui y h) "Time";
            Ui.Paint.cap paint
              ~at:(x +. end_x -. 8. -. Kit.cap_width ui "End", Kit.cap_y ui y h)
              "End";
            Ui.Paint.fill paint ~x:(x +. rule_x)
              ~y:(y +. ((h -. 12.) /. 2.))
              ~w:1. ~h:12. (Pxui.Theme.border theme);
            Ui.Paint.fill paint ~x ~y:(y +. bar_h) ~w ~h:1. (Pxui.Theme.edge theme)
          end;
          Ui.Paint.text paint
            ~at:(x +. readout_x, Kit.text_y ui y h)
            ~color:(if tall then theme.foreground else Pxui.Theme.ink_2 theme)
            readout);
      Ui.draw ui ruler (fun paint (x, y, w, h) ->
          Ui.Paint.fill paint ~x ~y ~w ~h theme.input;
          if not tall then Ui.Paint.fill paint ~x ~y ~w:1. ~h (Pxui.Theme.edge theme);
          let left = x +. inner_x in
          let at fraction = left +. Float.floor (fraction *. span) in
          let head = at (Int64.to_float frame /. Float.max 1. range) in
          Ui.Paint.fill paint ~x:left ~y ~w:(head -. left) ~h (Pxui.Theme.tint theme);
          (* a strip has fifty minor ticks and a major one in five; a taller ruler a tick a hundredth
           and a major one in ten, numbered *)
          let minors = if tall then 100 else 50 and every = if tall then 10 else 5 in
          let minor_h = if tall then 6. else 5. and major_h = if tall then 14. else 10. in
          for tick = 0 to minors - 1 do
            let tx = at (float tick /. float minors) in
            if tick mod every = 0 then begin
              Ui.Paint.fill paint ~x:tx
                ~y:(y +. h -. major_h)
                ~w:1. ~h:major_h (Pxui.Theme.border theme);
              if tall then
                Ui.Paint.text paint ~size:(Kit.cap_size ui) ~color:(Pxui.Theme.ink_3 theme)
                  ~at:(tx +. 4., Kit.cap_y ui (y +. 16.) 24.)
                  (string_of_int (int_of_float (Float.round (range *. float tick /. float minors))))
            end
            else
              Ui.Paint.fill paint ~x:tx
                ~y:(y +. h -. minor_h)
                ~w:1. ~h:minor_h (Pxui.Theme.edge theme)
          done;
          (* the 2-point accent playhead, and the frame as an accent label right of it *)
          let head = if tall then head -. 1. else head in
          Ui.Paint.fill paint ~x:head ~y ~w:2. ~h theme.accent;
          let tag_h = if tall then 24. else 14. in
          Ui.Paint.cap paint ~color:theme.accent
            ~at:(head +. 2., Kit.cap_y ui y tag_h)
            (Int64.to_string frame));
      List.filter_map Fun.id
        [
          (* a typed frame clamps to the timeline: 0 to the last frame *)
          (if typed <> current then
             Option.map
               (fun n -> Seek_playback (Int64.max 0L (Int64.min (Int64.of_int max_frame) n)))
               (Int64.of_string_opt (String.trim typed))
           else None);
          (if ended <> last then
             Option.map (fun n -> Set_end n) (int_of_string_opt (String.trim ended))
           else None);
          (if pause then Some Pause_toggle else None);
          (if stop then Some Stop_playback else None);
          (if reset then Some Reset_playback else None);
          (match scrub with
          | Some target when target <> Int64.to_float frame ->
              Some (Seek_playback (Int64.of_float target))
          | _ -> None);
        ])
