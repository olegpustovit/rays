open Rays

  module Ui = Pxui.Ui

  type 'a item = Field of 'a | Folder of string * 'a item list

  type flow_row = {
    path : string;
    fields : Param.field_view list;
    shown : bool;
    locked : bool;
    drive : string option;
    live : string option;
  }

  type flow_change = Edited of string * Param.value
    | Pinned of string * bool | Reset of string
    | Expression of string * string | Follow of string

  let rec insert path field items = match path with
    | [] -> items @ [Field field]
    | name :: rest ->
        let rec loop reversed = function
          | [] -> List.rev_append reversed [Folder (name, insert rest field [])]
          | Folder (candidate, children) :: tail when candidate = name ->
              List.rev_append reversed
                (Folder (candidate, insert rest field children) :: tail)
          | item :: tail -> loop (item :: reversed) tail
        in
        loop [] items

  (* Sections are one level deep: the top folder of a parameter, never the vector's own folder
     (its row is one row, in the section it belongs to). *)
  let top_folder (row : flow_row) =
    let folder = match row.fields with
      | (field : Param.field_view) :: _ -> field.folder | [] -> [] in
    let squash name = String.lowercase_ascii (String.concat "" (String.split_on_char ' ' name)) in
    let folder = match List.rev folder with
      | last :: rest when squash last = squash row.path -> List.rev rest
      | _ -> folder in
    match folder with first :: _ -> [ first ] | [] -> []

  let flow_fields ui ?(expanded = []) ?width ?(pins = false) ?(pin_click = false) ?(chips = [])
      ?kind_label ?(on_choice = fun _ _ -> ()) rows =
    (* the rows fill the panel they are built in *)
    let width = Option.value width ~default:(Ui.inspector_width ui) in
    let theme = Ui.theme ui in
    let ink_2 = Pxui.Theme.ink_2 theme and ink_3 = Pxui.Theme.ink_3 theme in
    let expression text = String.starts_with ~prefix:"=" text
      && String.length (String.trim text) > 1 in
    (* the dot of a row is drawn by the row ([pins]); with [pin_click] a click on it pins the row *)
    let pin_of shown = if pins then Some shown else None in
    let pinnable = pin_click in
    (* a click on the dot, or the s key over the row, asks to flip the row's pin *)
    let pin_change box path shown clicked edits =
      let key = pinnable && Ui.hovered_within ui box && not (Ui.text_input_focused ui)
        && Ui.key_pressed ui (Input.KeyChar 's') in
      (if clicked || key then [ Pinned (path, not shown) ] else []) @ edits in
    let action ui key label ~x ~y ~enabled ?(visible = true) () =
      let pin = label = "pin" in
      let x = if pin then 6. else x and w = if pin then 18. else 20. in
      let box = Ui.box ui ~flags:(if enabled then Ui.(clickable + tab_stop) else Ui.none)
          ~at:(x, y) ~w:(Ui.Px w) ~h:(Ui.Px 20.) key in
      let signal = Ui.signal ui box in
      let clicked = enabled && signal.clicked in
      if visible && not pin then Ui.draw ui box (fun paint (x, y, w, h) ->
        let color = if enabled && signal.hovered then theme.foreground else ink_3 in
        if label = "\xc3\x97" then begin
          let cx = x +. (w /. 2.) and cy = y +. (h /. 2.) in
          Ui.Paint.line paint ~from_:(cx -. 3.5, cy -. 3.5) ~to_:(cx +. 3.5, cy +. 3.5) color;
          Ui.Paint.line paint ~from_:(cx -. 3.5, cy +. 3.5) ~to_:(cx +. 3.5, cy -. 3.5) color
        end else Ui.Paint.text paint ~at:(x +. 1., Kit.cap_y ui y h) ~size:(Kit.cap_size ui) ~color label);
      clicked in
    let input ?(ranged = true) field path ~edit ~x ~y ~w =
      let key = "flow-value-" ^ path in
      let numeric kind ?range ?display text convert =
        (* the kit's number field: a drag changes the value from the one it had, by the step of its
           kind and soft range (a vector's cell has none), so a click without a drag writes nothing *)
        let range = if ranged then range else None in
        let changed = Ui.number_field ui ~at:(x, y) ~w ~h:20. ~kind ?range ?display ~edit ~left:(expression text)
            ?trail:(Option.map (fun unit -> unit, ink_3) field.Param.unit)
            ~valid:(fun text -> Param.Number.valid kind text || expression text) key text in
        if changed = text then [] else if expression changed then
          [Expression (path, changed)]
        else Option.fold ~none:[] ~some:(fun value -> [Edited (field.Param.name, value)])
            (convert changed) in
      match field.Param.kind, field.current with
      | Param.Integer_view range, Param.Int_value value ->
          numeric Param.Number.Int ~range:(float range.soft_min, float range.soft_max) (string_of_int value)
            (fun text -> Option.map (fun n -> Param.Int_value n) (int_of_string_opt text))
      | Param.Floating_view range, Param.Float_value value ->
          (* the text is the value in full, so a scrub starts from what is stored; it is shown short *)
          numeric Param.Number.Float ~range:(range.soft_min, range.soft_max)
            ~display:(Param.Number.show value) (Flow.Lisp.float value)
            (fun text -> Option.map (fun n -> Param.Float_value n) (float_of_string_opt text))
      | Param.Text_view, Param.Text_value value ->
          (* an empty group means every element of its owner *)
          let placeholder = match field.default with
            | Param.Text_value "" when field.name = "group" -> Some "all points"
            | Param.Text_value "" when String.ends_with ~suffix:"_group" field.name -> Some "all"
            | _ -> None in
          let text, _ = Ui.value_field ui ~at:(x, y) ~w ~h:20. ?placeholder
              ~left:true ~valid:(fun _ -> true) key value in
          if text = value then [] else [Edited (field.name, Param.Text_value text)]
      | Param.Choice_view choices, (Param.Choice_value _ | Param.Int_value _ as value) ->
          let box = Ui.box ui ~flags:Ui.(clickable + tab_stop + clip)
              ~at:(x, y) ~w:(Ui.Px w) ~h:(Ui.Px 20.) key in
          let index = match value with
            | Param.Int_value index -> index
            | Param.Choice_value label ->
                Option.value ~default:0 (Array.find_index (( = ) label) choices)
            | _ -> assert false in
          on_choice field.Param.name box;
          let just_opened = (Ui.signal ui box).clicked in
          let open_ = just_opened || Ui.state ui box ~default:0 = 1 in
          Ui.set_state ui box (if open_ then 1 else 0);
          Ui.draw ui box (fun paint (x, y, w, h) ->
            Ui.Paint.fill paint ~x ~y:(y +. h -. 1.) ~w ~h:1.
              (if open_ then theme.accent else Pxui.Theme.edge theme);
            let pad = match List.assoc_opt choices.(index) chips with
              | Some color ->
                  Ui.Paint.rect paint ~x:(x +. 2.) ~y:(y +. 6.) ~w:8. ~h:8. ~fill:color
                    ~stroke:(Pxui.Theme.edge theme) (); 14.
              | None -> 0. in
            Ui.Paint.text paint ~at:(x +. 2. +. pad, Kit.text_y ui (y -. 0.5) h)
              ~color:theme.foreground choices.(index);
            Ui.Paint.chevron paint ~at:(x +. w -. 5., y +. (h /. 2.))
              (if open_ then `Up else `Down) theme.foreground);
          if not open_ || just_opened then [] else
            let bx, by, bw, bh = Ui.rect ui box in
            (match Ui.context_menu ui ~at:(bx, by +. bh +. 1.) ~width:bw ~selected:index
                ~swatches:(Array.to_list (Array.map (fun choice -> List.assoc_opt choice chips) choices))
                (key ^ "-options")
                (Array.to_list (Array.map (fun choice -> choice, true) choices)) with
             | `Open -> []
             | `Dismiss -> Ui.set_state ui box 0; []
             | `Pick selected ->
                 Ui.set_state ui box 0;
                 if selected = index then [] else
                   [Edited (field.name, match value with
                     | Param.Int_value _ -> Param.Int_value selected
                     | _ -> Param.Choice_value choices.(selected))])
      | Param.Toggle_view, Param.Bool_value value ->
          let edited = Ui.inspector_toggle_value ui ~key ~at:(x, y) value in
          if edited = value then [] else
            [Edited (field.name, Param.Bool_value edited)]
      | _ -> [] in
    let driven path title source live shown =
      let box, control_x, control_y, control_w = Ui.inspector_row ui
          ~width ?pin:(pin_of shown) ~key:("flow-row-" ^ path) ~label:title () in
      (* the cross that removes a drive shows on the hovered row, in place of the live value *)
      let hovered = Ui.hovered_within ui box in
      Ui.within ui box (fun () ->
        let live = if hovered then None else live in
        let edits = if expression source then
          let text, _ = Ui.value_field ui ~at:(control_x, control_y) ~w:control_w ~h:20.
              ~display:(String.sub source 1 (String.length source - 1))
              ~lead:("\xc6\x92", ink_2)
              ?trail:(Option.map (fun value -> value, ink_2) live)
              ~line:(Pxui.Theme.ports theme).float
              ~valid:expression ("flow-expression-" ^ path) source in
          if text = source then [] else [Expression (path, text)]
        else (let link = Ui.box ui ~flags:Ui.(clickable + tab_stop_marked)
            ~at:(control_x, control_y) ~w:(Ui.Px (Float.max 1. (control_w -. 24.))) ~h:(Ui.Px 20.)
            ("flow-source-" ^ path) in
          let followed = (Ui.signal ui link).clicked in
          Ui.draw ui box (fun paint (x, y, _, _) ->
          let ty = Kit.text_y ui (y +. control_y -. 0.5) 20. in
          let left = x +. control_x +. 2. and right = x +. control_x +. control_w -. 2. in
          Ui.Paint.text paint ~at:(left, ty) ~color:theme.accent "\xe2\x86\x90";
          let source_x = left +. Ui.Paint.text_width paint "\xe2\x86\x90" +. 6. in
          let live_w = Option.fold ~none:0. ~some:(fun v -> Ui.Paint.text_width paint v +. 6.) live in
          Ui.Paint.text paint ~at:(source_x, ty) ~color:ink_2
            (Ui.ellipsis ~width:(Ui.Paint.text_width paint ~size:(Ui.font_size ui)) ~limit:(right -. source_x -. live_w) source);
          Option.iter (fun value ->
            Ui.Paint.text paint ~color:theme.foreground
              ~at:(right -. Ui.Paint.text_width paint value, ty) value) live);
          if followed then [Follow path] else []) in
        let reset = hovered && action ui ("reset-" ^ path) "\xc3\x97" ~x:(width -. 32.)
            ~y:control_y ~enabled:true () in
        let pin = pinnable && action ui ("pin-" ^ path) "pin" ~x:0. ~y:control_y ~enabled:true () in
        pin_change box path shown pin (if reset then Reset path :: edits else edits)) in
    let scalar path title field shown =
      let box, control_x, control_y, control_w = Ui.inspector_row ui
          ~width ?pin:(pin_of shown) ~key:("flow-row-" ^ path) ~label:title () in
      Ui.within ui box (fun () ->
        (* the label's own extent: its column *)
        let label_x = Ui.inspector_label_x ui in
        let label = Ui.box ui ~flags:Ui.clickable ~at:(label_x, 2.)
            ~w:(Ui.Px (Float.max 1. (control_x -. label_x -. 8.))) ~h:(Ui.Px 20.) "label-edit" in
        let edit = (Ui.signal ui label).double_clicked in
        let edits = input field path ~x:control_x ~y:control_y ~w:control_w ~edit in
        let pinned = pinnable && action ui ("pin-" ^ path) "pin" ~x:0. ~y:control_y ~enabled:true () in
        pin_change box path shown pinned edits) in
    let has_substr sub s =
      let len_s = String.length s and len_sub = String.length sub in
      let rec check i =
        if i + len_sub > len_s then false
        else if String.sub s i len_sub = sub then true
        else check (i + 1) in
      check 0 in
    let is_color_3 (row : flow_row) =
      List.length row.fields = 3 && (
        has_substr "color" row.path
        || List.for_all (fun (f : Param.field_view) -> has_substr "color" f.name) row.fields
      ) in
    let is_color_1 (field : Param.field_view) =
      match field.kind, field.current with
      | Param.Text_view, Param.Text_value text ->
          (String.starts_with ~prefix:"#" text || has_substr "color" field.name)
          && Result.is_ok (Rays_math.Color.hex text)
      | _ -> false in
    let swatch_and_hex ~path ~control_x ~control_y ~control_w ~swatch_color ~hex_str =
      Kit.colour ui ~key:path ~at:(control_x, control_y) ~w:control_w ~swatch:swatch_color ~hex:hex_str in
    let color_row_3 (row : flow_row) title fields shown =
      let path = row.path in
      let to_f = function
        | Param.Float_value x -> x
        | Int_value x -> float x
        | _ -> 0. in
      let r, g, b = match fields with
        | [ f0; f1; f2 ] -> to_f f0.Param.current, to_f f1.current, to_f f2.current
        | _ -> 0., 0., 0. in
      let swatch_color = Rays_math.Color.of_unit_floats r g b in
      let hex_str = Rays_math.Color.to_hex_string swatch_color in
      let box, control_x, control_y, control_w = Ui.inspector_row ui
          ~width ?pin:(pin_of shown) ~key:("flow-row-" ^ path) ~label:title () in
      Ui.within ui box (fun () ->
        let text = swatch_and_hex ~path ~control_x ~control_y ~control_w ~swatch_color ~hex_str in
        let hex_edits =
          if text = hex_str then [] else
          match Rays_math.Color.hex text with
          | Ok c ->
              let nr, ng, nb, _ = Rays_math.Color.to_floats c in
              (match fields with
               | [ f0; f1; f2 ] ->
                   [ Edited (f0.Param.name, Param.Float_value nr);
                     Edited (f1.name, Param.Float_value ng);
                     Edited (f2.name, Param.Float_value nb) ]
               | _ -> [])
          | Error _ -> [] in
        let pin = pinnable && action ui ("pin-" ^ path) "pin" ~x:0. ~y:control_y
            ~enabled:(not row.locked) () in
        hex_edits @ pin_change box path shown pin []) in
    let color_row_1 path title field shown =
      let text_val = match field.Param.current with Param.Text_value t -> t | _ -> "#ffffff" in
      let c = Result.value (Rays_math.Color.hex text_val) ~default:Rays_math.Color.white in
      let r, g, b, _ = Rays_math.Color.to_floats c in
      let swatch_color = Rays_math.Color.of_unit_floats r g b in
      let hex_str = Rays_math.Color.to_hex_string swatch_color in
      let box, control_x, control_y, control_w = Ui.inspector_row ui
          ~width ?pin:(pin_of shown) ~key:("flow-row-" ^ path) ~label:title () in
      Ui.within ui box (fun () ->
        let text = swatch_and_hex ~path ~control_x ~control_y ~control_w ~swatch_color ~hex_str in
        let hex_edits =
          if text = hex_str then [] else
          match Rays_math.Color.hex text with
          | Ok _ -> [ Edited (field.Param.name, Param.Text_value text) ]
          | Error _ -> [] in
        let pin = pinnable && action ui ("pin-" ^ path) "pin" ~x:0. ~y:control_y ~enabled:true () in
        hex_edits @ pin_change box path shown pin []) in
    let row_widget (row : flow_row) =
      (* a row is named by its argument, as the sheet and the card name it *)
      let title = match row.fields with
        | [field] when String.starts_with ~prefix:"@" row.path -> field.Param.label
        | _ -> row.path in
      match row.drive, row.fields with
      | Some source, _ -> driven row.path title source row.live row.shown
      | None, [field] when is_color_1 field -> color_row_1 row.path title field row.shown
      | None, [field] -> scalar row.path title field row.shown
      | None, fields when is_color_3 row -> color_row_3 row title fields row.shown
      | None, fields ->
          (* a vector: three fields in the control column, 8 between, each with its axis letter *)
          let box, control_x, control_y, control_w = Ui.inspector_row ui
              ~width ?pin:(pin_of row.shown) ~key:("flow-row-" ^ row.path) ~label:title () in
          Ui.within ui box (fun () ->
            let edits =
              Kit.vector ui box ~at:(control_x, control_y) ~w:control_w (fun index ~x ~w ->
                  let field = List.nth fields index in
                  input ~ranged:false field (row.path ^ "." ^ List.nth ["x"; "y"; "z"] index)
                    ~edit:false ~x ~y:control_y ~w) in
            let pin = pinnable && action ui ("pin-" ^ row.path) "pin" ~x:0. ~y:control_y
                ~enabled:(not row.locked) () in
            edits @ pin_change box row.path row.shown pin []) in
    let rec build path items = List.concat_map (function
      | Field row -> row_widget row
      | Folder (label, children) ->
          let path = path @ [label] in
          let key = String.concat "/" path in
          Option.value ~default:[]
            (Ui.inspector_section ui ~key:("flow-section-" ^ key)
              ~expanded:(List.mem key expanded || (path = [ label ] && Some label = kind_label)) label
              (fun () -> build path children))) items in
    if rows = [] then (Ui.inspector_message ui ~key:"no-parameters" "No parameters"; []) else
      (* a kind's own section first, for the arguments that have no folder *)
      let folder_of row = match top_folder row, kind_label with
        | [], Some label -> [ label ] | folder, _ -> folder in
      let own, others = List.partition (fun row -> top_folder row = []) rows in
      build [] (List.fold_left (fun items (row : flow_row) -> insert (folder_of row) row items) []
                  (own @ others))

  let fields ui ?expanded ?width views =
    let rows = List.map (fun (field : Param.field_view) ->
      { path = field.name; fields = [field]; shown = false; locked = true;
        drive = None; live = None }) views in
    flow_fields ui ?expanded ?width rows
    |> List.filter_map (function Edited (name, value) -> Some (name, value)
      | _ -> None)

  let record ui schema values =
    match fields ui (Param.view schema values) with
    | [] -> Ok (values, Param.no_effects)
    | changes -> Param.apply_all schema values changes
