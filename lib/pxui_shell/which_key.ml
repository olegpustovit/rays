open Rays

  open Editor_core.Keymap
  open Editor_core.Command

  let first_word label = List.hd (String.split_on_char ' ' label)

  (* One page per typed prefix: a key that continues into longer sequences
     shows as a "+group" row named by the first word of its first command. *)
  let page keymap ~prefix scope =
    List.fold_left (fun rows command -> match command.trigger with
      | Some (Leader sequence) when command.scope = scope
          && String.length sequence > String.length prefix
          && String.starts_with ~prefix sequence ->
          let key = String.make 1 sequence.[String.length prefix] in
          if List.mem_assoc key rows then rows
          else rows @ [key, if String.length sequence = String.length prefix + 1
            then command.label
            else "+" ^ first_word command.label]
      | Some (Chord (key, modifiers)) when prefix = "" && command.scope = scope ->
          rows @ [Editor_core.Keymap.label (Chord (key, modifiers)), command.label]
      | _ -> rows) [] keymap

  (* The sheet's order of the leader's sections; a category it does not know follows them. *)
  let category_order = [ "Add"; "Panel"; "Layout"; "Go"; "Time"; "File" ]

  (* The leader's page by category: one-letter continuations only, no chords.  A key that
     continues into longer sequences is "+" and a name from [describe], else the first word of its
     first command. *)
  let by_category keymap ~prefix ~focus ~category ~describe ~order =
    let rows = List.fold_left (fun rows command -> match command.trigger with
      | Some (Leader sequence) when (command.scope = None || command.scope = Some focus)
          && String.length sequence > String.length prefix
          && String.starts_with ~prefix sequence ->
          let key = String.make 1 sequence.[String.length prefix] and title = category command in
          if List.exists (fun (t, k, _) -> t = title && k = key) rows then rows
          else rows @ [ title, key,
            if String.length sequence = String.length prefix + 1 then command.label
            else "+" ^ Option.value (describe (prefix ^ key)) ~default:(first_word command.label) ]
      | _ -> rows) [] keymap in
    let titles = List.fold_left (fun acc (t, _, _) -> if List.mem t acc then acc else acc @ [ t ]) [] rows in
    let ordered = List.filter (fun t -> List.mem t titles) category_order
      @ List.filter (fun t -> not (List.mem t category_order)) titles in
    (* the host's order of a section's keys; keys it does not name keep the keymap's order after them *)
    let rank title key =
      let rec find i = function [] -> max_int | k :: rest -> if k = key then i else find (i + 1) rest in
      find 0 (order title) in
    List.map (fun title -> title,
      List.stable_sort (fun (a, _) (b, _) -> compare (rank title a) (rank title b))
        (List.filter_map (fun (t, k, l) -> if t = title then Some (k, l) else None) rows)) ordered

  (* Sections of key rows, one to a column, painted by one box.  A section is a label in ink-3
     ([gap] points above it) over rows of 24: the key in ink-3 at the label size, 8 points on,
     what it does, and a chevron when the key continues into more keys.  [cols] columns share [w]. *)
  let columns ui paint (x, y, w) ~cols ~gap ~label_ink sections =
    let module Ui = Pxui.Ui in
    let theme = Ui.theme ui in
    let row = float (Ui.row_height ui) in
    let col_w = w /. float (max 1 cols) in
    List.iteri (fun column (title, rows) ->
      let cx = x +. (float column *. col_w) in
      Ui.Paint.cap paint ~at:(cx +. 12., Kit.cap_y ui (y +. gap) row) ~color:(Pxui.Theme.ink_3 theme) title;
      List.iteri (fun k (key, label) ->
        let rx = cx and ry = y +. gap +. row +. (float k *. row) in
        let group = String.starts_with ~prefix:"+" label in
        let label = if group then String.sub label 1 (String.length label - 1) else label in
        let key_width = Ui.Paint.text_width paint ~size:(Kit.cap_size ui) key in
        Ui.Paint.text paint ~size:(Kit.cap_size ui) ~at:(rx +. 12., Kit.cap_y ui ry row)
          ~color:(Pxui.Theme.ink_3 theme) key;
        let lx = rx +. 12. +. key_width +. 8. in
        Ui.Paint.text paint ~at:(lx, Kit.text_y ui ry row) ~color:label_ink
          (Ui.ellipsis ~width:(Ui.Paint.text_width paint) ~limit:(rx +. col_w -. lx -. (if group then 20. else 12.)) label);
        (* a key that continues into more keys *)
        if group then Ui.Paint.chevron paint ~at:(rx +. col_w -. 17., ry +. (row /. 2.)) `Right
          (Pxui.Theme.ink_3 theme)) rows) sections

  let tallest sections = List.fold_left (fun most (_, rows) -> max most (List.length rows)) 0 sections

  (* The leader: a sheet over the status strip, the [/] key and its name over six columns.  Hosts
     that give a [category] get the sections of the sheet (the keys of the leader only); without it the
     sections are the commands everywhere and those of the focused pane, with the key chords. *)
  let panel ui ?category ?(describe = fun _ -> None) ?(order = fun _ -> []) keymap ~prefix ~focus ~focus_name =
    let module Ui = Pxui.Ui in
    let theme = Ui.theme ui in
    let sections = match category with
      | Some category -> by_category keymap ~prefix ~focus ~category ~describe ~order
      | None -> List.filter (fun (_, rows) -> rows <> [])
          [ "Global", page keymap ~prefix None; focus_name, page keymap ~prefix (Some focus) ] in
    let view_w, view_h = Ui.view_size ui in
    let row = float (Ui.row_height ui) in
    let cols = max 6 (List.length sections) in
    (* the edge, 8, the head row, a section label under 4, its rows, 8; never under the sheet's 183 *)
    let height = Float.max 183. (1. +. 8. +. row +. 4. +. row +. (float (tallest sections) *. row) +. 8.) in
    let y = Float.max 0. (view_h -. float Layout.status_height -. height) in
    let leader = if prefix = "" then "/" else "/ " ^ prefix in
    (* build before the body it shields; the host closes it on any key *)
    ignore (Ui.popup ui ~dismiss_initial:false ~at:(0., y) ~width:view_w ~height "leader" (fun () ->
      let box = Ui.box ui ~w:Ui.Grow ~h:(Ui.Px height) "leader-sheet" in
      Ui.draw ui box (fun paint (x, y, w, _) ->
        Ui.Paint.fill paint ~x ~y ~w ~h:1. (Pxui.Theme.border theme);
        let y = y +. 9. in
        Ui.Paint.text paint ~size:(Kit.cap_size ui) ~at:(x +. 12., Kit.cap_y ui y row)
          ~color:(Pxui.Theme.ink_3 theme) leader;
        Ui.Paint.cap paint ~at:(x +. 20. +. Ui.Paint.text_width paint ~size:(Kit.cap_size ui) leader, Kit.cap_y ui y row)
          "Leader";
        let hint = "a key continues \xc2\xb7 esc closes" in
        Ui.Paint.text paint ~size:(Kit.cap_size ui) ~color:(Pxui.Theme.ink_2 theme)
          ~at:(x +. w -. 12. -. Ui.Paint.text_width paint ~size:(Kit.cap_size ui) hint, Kit.cap_y ui y row) hint;
        columns ui paint (x +. 16., y +. row, w -. 32.) ~cols ~gap:4.
          ~label_ink:theme.foreground sections)))

  (* The key sheet: a title row ([Keys], what has the focus, a close button), the filter, and the
     commands in three columns, one section each. *)
  (* The key sheet's sections: every command a key reaches, once per id with all its keys (the
     Ctrl twins of the Command chords left out), under the section [category] gives it, the
     leader sheet's sections first. *)
  let sheet_sections ?(category = fun _ -> "Keys") ?(text_keys = []) keymap =
    let commands = List.fold_left (fun seen command ->
      if command.trigger = None || List.exists (fun previous -> previous.id = command.id) seen then seen
      else command :: seen) [] keymap |> List.rev in
    let titles = List.fold_left (fun acc command ->
      let title = category command in if List.mem title acc then acc else acc @ [ title ]) [] commands in
    let sections = List.map (fun title -> title, List.filter_map (fun command ->
      if category command <> title then None else
      let keys = List.filter_map (fun alias -> if alias.id <> command.id then None
        else match alias.trigger with
          | Some (Chord (_, modifiers)) when List.mem Input.Ctrl modifiers -> None
          | Some trigger -> Some (Editor_core.Keymap.label trigger) | None -> None) keymap
        |> List.sort_uniq String.compare |> String.concat " / " in
      Some (keys, command.label)) commands)
      (List.filter (fun t -> List.mem t titles) category_order
       @ List.filter (fun t -> not (List.mem t category_order)) titles) in
    match text_keys with
    | [] -> sections
    | keys -> sections @ ["Text", List.map (fun (label, trigger) ->
        Editor_core.Keymap.label trigger, label) keys]

  let sheet ui ?(context = "") ?category ?text_keys keymap =
    let module Ui = Pxui.Ui in
    let sections = sheet_sections ?category ?text_keys keymap in
    match Ui.modal ui ~width:572. "guide-keys" (fun () ->
      let theme = Ui.theme ui in
      let closed = ref false in
      (* the title row: 4 above, then 24: the label, what has the focus (ink-2 at 70 percent, as
         written) 8 before the close button, which ends 8 from the edge *)
      let title = Ui.box ui ~w:Ui.Grow ~h:(Ui.Px 28.) "keys-title" in
      Ui.draw ui title (fun paint (x, y, w, _) ->
        Ui.Paint.cap paint ~at:(x +. 12., Kit.cap_y ui (y +. 4.) 24.) ~color:(Pxui.Theme.ink_2 theme) "Keys";
        if context <> "" then
          Ui.Paint.text paint ~size:(Kit.cap_size ui)
            ~color:(Rays_math.Color.with_alpha (Pxui.Theme.ink_2 theme) 179)
            ~at:(x +. w -. 36. -. Ui.Paint.text_width paint ~size:(Kit.cap_size ui) context,
                 Kit.cap_y ui (y +. 4.) 24.) context);
      Ui.within ui title (fun () ->
        let holder = Ui.box ui ~w:(Ui.Px 40.) ~h:(Ui.Px 24.) ~at:(530., 4.) "keys-close-holder" in
        Ui.within ui holder (fun () ->
          if Ui.button ui ~icon:true ~bare:true ~at_end:true ~ink:(Pxui.Theme.ink_2 theme) "\xc3\x97###guide-close"
          then closed := true));
      (* the filter: what is typed narrows the rows to the commands and keys that match; the
         query lives with its box, Escape closes the sheet *)
      let memory = Ui.box ui ~w:Ui.Grow ~h:(Ui.Px 0.) "keys-filter-memory" in
      let query, pick = Ui.picker ui ~at_rest:true "filter commands"
          ~query:(Option.value ~default:"" (Ui.text_state ui memory)) (fun _ -> [||]) in
      Ui.set_text_state ui memory (Some query);
      let sections = if query = "" then sections else List.filter_map (fun (title, rows) ->
        match List.filter (fun (keys, label) -> Ui.fuzzy_match ~query label || Ui.fuzzy_match ~query keys) rows with
        | [] -> None | rows -> Some (title, rows)) sections in
      (* three columns: each section goes under the shortest column so far *)
      let rows_of (_, rows) = 1 + List.length rows in
      let stacks = List.fold_left (fun stacks section ->
        let height stack = List.fold_left (fun n s -> n + rows_of s) 0 stack in
        let shortest = List.fold_left (fun best i ->
          if height (List.nth stacks i) < height (List.nth stacks best) then i else best) 0 [ 1; 2 ] in
        List.mapi (fun i stack -> if i = shortest then stack @ [ section ] else stack) stacks)
        [ []; []; [] ] sections in
      let tallest = List.fold_left (fun most stack ->
        max most (List.fold_left (fun n s -> n + rows_of s) 0 stack)) 0 stacks in
      let row = float (Ui.row_height ui) in
      let box = Ui.box ui ~w:Ui.Grow ~h:(Ui.Px (float tallest *. row +. 8.)) "keys-sheet" in
      Ui.draw ui box (fun paint (x, y, w, _) ->
        List.iteri (fun column stack ->
          ignore (List.fold_left (fun top section ->
            columns ui paint (x +. (float column *. w /. 3.), top, w /. 3.) ~cols:1 ~gap:0.
              ~label_ink:(Pxui.Theme.ink_2 theme) [ section ];
            top +. (float (rows_of section) *. row)) y stack)) stacks);
      pick <> `Cancel && not !closed) with
    | Some open_ -> open_ | None -> false
