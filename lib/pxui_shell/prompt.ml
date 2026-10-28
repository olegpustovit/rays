module Ui = Pxui.Ui

(* The sheet's button row: 20 points, [Cancel esc] and the primary [accept] with its key, right-aligned
     8 from the edge of a window [width] wide (its 1-point edges inside), 4 apart, and 8 under it.  A click
     is the same as the key. *)
let buttons ui ~key ~width ~accept =
  let row = Ui.box ui ~w:Ui.Grow ~h:(Ui.Px 28.) (key ^ "-buttons") in
  Ui.within ui row (fun () ->
      let primary_w = Kit.button_width ui ~hint:"\xe2\x86\xb5" accept
      and cancel_w = Kit.button_width ui ~hint:"esc" "Cancel" in
      let primary_x = width -. 2. -. 8. -. primary_w in
      let cancel =
        Kit.button ui ~key:(key ^ "-cancel")
          ~at:(primary_x -. 4. -. cancel_w, 0.)
          ~w:cancel_w ~hint:"esc" "Cancel"
      in
      let submit =
        Kit.button ui ~key:(key ^ "-accept") ~at:(primary_x, 0.) ~w:primary_w ~primary:true
          ~hint:"\xe2\x86\xb5" accept
      in
      if cancel then `Cancel else if submit then `Submit else `None)

let spacer ui key height = ignore (Ui.box ui ~w:Ui.Grow ~h:(Ui.Px height) key)

(* A name prompt, the sheet's [06]: the title, what is asked in ink-2, the field and the buttons *)
let name ui ~key ~title ~description ~label ~query =
  Ui.modal ui ~width:320. key (fun () ->
      Ui.label ui title;
      spacer ui "prompt-above" 8.;
      let line = Ui.box ui ~w:Ui.Grow ~h:(Ui.Px 24.) "prompt-description" in
      Ui.draw ui line (fun paint (x, y, w, h) ->
          Ui.Paint.text paint
            ~at:(x +. 8., Kit.text_y ui y h)
            ~color:(Pxui.Theme.ink_2 (Ui.theme ui))
            (Ui.ellipsis ~width:(Ui.Paint.text_width paint) ~limit:(w -. 16.) description));
      let query, result =
        Ui.col ui ~padding:4. "prompt-field" (fun () ->
            Ui.picker ui ~slash:false label ~query (fun _ -> [||]))
      in
      let clicked = buttons ui ~key ~width:320. ~accept:"Save" in
      (query, match clicked with `None -> result | (`Cancel | `Submit) as pick -> pick))

(* A searchable prompt, the sheet's [01] window: the field, the rows in the picker's style (labels
     cut with an ellipsis), then the hairline and the hint bar with [N of M]; no buttons *)
let search ui ~key ~title ~label ~query ~rows =
  Ui.modal ui ~width:320. key (fun () ->
      Ui.label ui title;
      let query, result = Ui.picker ui label ~query rows in
      Ui.footer ui
        ~right:(Printf.sprintf "%d of %d" (Array.length (rows query)) (Array.length (rows "")))
        [ ("\xe2\x86\x91\xe2\x86\x93", "move"); ("\xe2\x86\xb5", "pick") ];
      (query, result))

type macro = { name : string; holes : (bool * string) array }

(* ponytail: the first 12 literals only; a template with more is rare and the
     rest stay copied into it *)
let macro ui ~key ~title ~literals ~free (m : macro) =
  Pxui.Ui.modal ui ~width:480. key (fun () ->
      (* Enter in one of its fields creates the macro, like the button *)
      let enter = Pxui.Ui.text_input_focused ui && Pxui.Ui.key_pressed ui Input.Enter in
      Pxui.Ui.label ui title;
      Pxui.Ui.inspector_message ui ~key:(key ^ "-hint")
        (match free with
        | [] -> "Tick the literals that become holes."
        | names -> "Holes: ticked literals and " ^ String.concat ", " names);
      let holes =
        Array.mapi
          (fun i (on, name) ->
            if i >= 12 || i >= Array.length literals then (on, name)
            else begin
              let on = Pxui.Ui.toggle ui (Printf.sprintf "%d  %s" (i + 1) literals.(i)) on in
              let name =
                if on then Pxui.Ui.text_field ui (Printf.sprintf "hole %d" (i + 1)) name else name
              in
              (on, name)
            end)
          m.holes
      in
      let name = Pxui.Ui.text_field ui "Macro name" m.name in
      let submit = Pxui.Ui.button ui "Create macro" in
      ({ name; holes }, if submit || enter then `Submit else `None))
