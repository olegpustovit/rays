module Ui = Pxui.Ui
module Ids = Set.Make (Int)

type row = {
  id : int;
  depth : int;
  label : string;
  detail : string;
  badge : string * Rays_math.Color.t;
  link : bool;
  ghost : bool;
  flags : bool list;
}

type drop = Before | Inside | After

type intent =
  | Select of int list
  | Flag of { ids : int list; column : int; value : bool }
  | Move of { ids : int list; target : int; drop : drop }
  | Indent of int list
  | Outdent of int list
  | Reorder of { ids : int list; delta : int }
  | Rename of int * string
  | Activate of int
  | Delete of int list

type command =
  | Up
  | Down
  | Extend_up
  | Extend_down
  | Collapse
  | Expand
  | First
  | Last
  | Indent_rows
  | Outdent_rows
  | Move_up
  | Move_down
  | Rename_row
  | Delete_rows
  | Filter
  | Hide
  | Activate_row

type drag =
  | Rows of { ids : int list; moved : bool }
  | Paint of { column : int; value : bool; painted : int list }

type t = {
  focus : int option;
  anchor : int option;
  folded : Ids.t;
  filter : string option;
  renaming : (int * string) option;
  drag : drag option;
  reveal : bool;
  context : (float * float * int * int list) option;
  shown : (row array * Ids.t * string option * int array) option;
}

let create () =
  {
    focus = None;
    anchor = None;
    folded = Ids.empty;
    filter = None;
    renaming = None;
    drag = None;
    reveal = false;
    context = None;
    shown = None;
  }

let reveal t = { t with reveal = true }
let focused t = t.focus
let editing t = t.renaming <> None || t.filter <> None

let summary t =
  Printf.sprintf "focus %s, %d folded, filter %s"
    (Option.fold ~none:"-" ~some:string_of_int t.focus)
    (Ids.cardinal t.folded)
    (Option.fold ~none:"-" ~some:(Printf.sprintf "%S") t.filter)

let bindings =
  let open Editor_core.Keymap in
  let key ?(modifiers = []) id label key action =
    Editor_core.Command.make ~id:("list." ^ id) ~label ~trigger:(Chord (key, modifiers)) action
  in
  let open Input in
  [
    key "up" "previous row" ArrowUp Up;
    key "down" "next row" ArrowDown Down;
    key "up" "previous row" (KeyChar 'k') Up;
    key "down" "next row" (KeyChar 'j') Down;
    key "extend-up" "extend selection up" ArrowUp Extend_up ~modifiers:[ Shift ];
    key "extend-down" "extend selection down" ArrowDown Extend_down ~modifiers:[ Shift ];
    key "collapse" "fold / parent" ArrowLeft Collapse;
    key "expand" "unfold / first child" ArrowRight Expand;
    key "first" "first row" Home First;
    key "last" "last row" End Last;
    key "indent" "reparent under previous" Tab Indent_rows;
    key "outdent" "reparent up a level" Tab Outdent_rows ~modifiers:[ Shift ];
    key "move-up" "move up" ArrowUp Move_up ~modifiers:[ Alt ];
    key "move-down" "move down" ArrowDown Move_down ~modifiers:[ Alt ];
    key "rename" "rename" F2 Rename_row;
    key "delete" "delete" Delete Delete_rows;
    key "delete" "delete" Backspace Delete_rows;
    key "filter" "filter" (KeyChar 's') Filter;
    key "hide" "hide / show" (KeyChar 'h') Hide;
    key "activate" "open selected in graph" Enter Activate_row;
  ]

let has_children rows index =
  index + 1 < Array.length rows && rows.(index + 1).depth > rows.(index).depth

(* Indices of the shown rows: a filter keeps matches and their ancestors
     (and ignores folds); a folded row hides its descendants. *)
let visible t rows =
  let count = Array.length rows in
  let filtering = match t.filter with None | Some "" -> false | Some _ -> true in
  let keep =
    if not filtering then Array.make count true
    else begin
      let query = Option.get t.filter in
      let keep = Array.make count false and stack = Array.make (count + 1) 0 in
      Array.iteri
        (fun index row ->
          let depth = min row.depth count in
          stack.(depth) <- index;
          if Ui.fuzzy_match ~query row.label then
            for level = 0 to depth do
              keep.(stack.(level)) <- true
            done)
        rows;
      keep
    end
  in
  let shown = ref [] and hidden_below = ref max_int in
  Array.iteri
    (fun index row ->
      if row.depth <= !hidden_below then hidden_below := max_int;
      if !hidden_below = max_int && keep.(index) then begin
        shown := index :: !shown;
        if (not filtering) && Ids.mem row.id t.folded && has_children rows index then
          hidden_below := row.depth
      end)
    rows;
  Array.of_list (List.rev !shown)

(* The shown rows, recomputed only when the rows, folds, or filter change. *)
let shown t rows =
  match t.shown with
  | Some (source, folded, filter, shown)
    when source == rows && folded == t.folded && filter = t.filter ->
      (t, shown)
  | _ ->
      let shown = visible t rows in
      ({ t with shown = Some (rows, t.folded, t.filter, shown) }, shown)

let position rows shown id =
  let found = ref None in
  Array.iteri (fun k index -> if !found = None && rows.(index).id = id then found := Some k) shown;
  !found

let span rows shown a b =
  List.init (abs (b - a) + 1) (fun offset -> rows.(shown.(min a b + offset)).id)

let run_command t rows ~selected command =
  let t, shown = shown t rows in
  let count = Array.length shown in
  let at k = rows.(shown.(k)) in
  let current = Option.bind t.focus (position rows shown) in
  let move k =
    let id = (at (max 0 (min (count - 1) k))).id in
    ({ t with focus = Some id; anchor = Some id; reveal = true }, [ Select [ id ] ])
  in
  let extend k =
    let k = max 0 (min (count - 1) k) in
    let anchor = Option.value ~default:k (Option.bind t.anchor (position rows shown)) in
    ( { t with focus = Some (at k).id; reveal = true },
      [ Select ((at k).id :: List.filter (( <> ) (at k).id) (span rows shown anchor k)) ] )
  in
  let targets = if selected = [] then Option.to_list t.focus else selected in
  if count = 0 then (t, [])
  else
    match (command, current) with
    | Up, Some k -> move (k - 1)
    | Down, Some k -> move (k + 1)
    | (Up | Extend_up), None -> move (count - 1)
    | (Down | Extend_down), None -> move 0
    | First, _ -> move 0
    | Last, _ -> move (count - 1)
    | Extend_up, Some k -> extend (k - 1)
    | Extend_down, Some k -> extend (k + 1)
    | Collapse, Some k -> (
        let index = shown.(k) in
        if has_children rows index && not (Ids.mem rows.(index).id t.folded) then
          ({ t with folded = Ids.add rows.(index).id t.folded }, [])
        else
          let parent = ref None in
          for candidate = k - 1 downto 0 do
            if !parent = None && (at candidate).depth < (at k).depth then parent := Some candidate
          done;
          match !parent with Some parent -> move parent | None -> (t, []))
    | Expand, Some k ->
        let index = shown.(k) in
        if Ids.mem rows.(index).id t.folded then
          ({ t with folded = Ids.remove rows.(index).id t.folded }, [])
        else if has_children rows index then move (k + 1)
        else (t, [])
    | Indent_rows, _ -> (t, [ Indent targets ])
    | Outdent_rows, _ -> (t, [ Outdent targets ])
    | Delete_rows, _ -> (t, [ Delete targets ])
    | Move_up, _ -> (t, [ Reorder { ids = targets; delta = -1 } ])
    | Move_down, _ -> (t, [ Reorder { ids = targets; delta = 1 } ])
    | Filter, _ -> ({ t with filter = Some (Option.value t.filter ~default:"") }, [])
    | Rename_row, Some k when not (at k).ghost ->
        ({ t with renaming = Some ((at k).id, (at k).label) }, [])
    | Hide, Some k ->
        let value = match (at k).flags with shown :: _ -> not shown | [] -> true in
        (t, [ Flag { ids = targets; column = 0; value } ])
    | Activate_row, Some k -> (t, [ Activate (at k).id ])
    | (Collapse | Expand | Rename_row | Hide | Activate_row), _ -> (t, [])

let flag_width = 20. (* a 12-point flag and its 8-point gap *)
let indent_step = 12.

(* The row list, its toggle columns, and the filter and rename prompts,
     built inside [Ui.frame]. One box takes every pointer gesture; rows are
     found from the pointer, so only the visible slice is painted. *)
let update t ui (_frame : Rays.Frame.t) ~bounds:(x, y, w, h) ?(title = "") ~columns rows ~selected =
  let theme = Ui.theme ui in
  let height = float_of_int (Ui.row_height ui) in
  let x = float_of_int x and y = float_of_int y and w = float_of_int w and h = float_of_int h in
  let shown = visible t rows in
  let count = Array.length shown in
  let at k = rows.(shown.(k)) in
  let top = y +. height in
  let body = Float.max height (h -. height) in
  let box =
    Ui.box ui
      ~flags:Ui.(clickable + scroll + clip + blocking)
      ~w:(Ui.Px w) ~h:(Ui.Px h) ~at:(x, y) ~scroll_step:height "tree"
  in
  let signal = Ui.signal ui box in
  let scroll = Ui.scroll_offset ui box in
  let scroll =
    match (t.reveal, Option.bind t.focus (position rows shown)) with
    | true, Some k ->
        let row_top = float_of_int k *. height in
        if row_top < scroll then row_top
        else if row_top +. height > scroll +. body then row_top +. height -. body
        else scroll
    | _ -> scroll
  in
  let scroll =
    Float.max 0. (Float.min scroll (Float.max 0. ((float_of_int count *. height) -. body)))
  in
  if t.reveal then Ui.set_scroll_offset ui box scroll;
  ignore
    (Ui.within ui box (fun () ->
         Ui.box ui ~w:(Ui.Px w) ~h:(Ui.Px (float_of_int (count + 1) *. height)) "tree-content"));
  let ancestors k =
    let chain = ref [] and depth = ref (at k).depth in
    for candidate = k - 1 downto 0 do
      if (at candidate).depth < !depth then begin
        chain := candidate :: !chain;
        depth := (at candidate).depth
      end
    done;
    !chain
  in
  (* the ancestors of the first row in view stay at the top while the list is scrolled *)
  let sticky_of scroll =
    if count = 0 || scroll <= 0. then []
    else
      List.take 3
        (ancestors (max 0 (min (count - 1) (int_of_float (Float.floor (scroll /. height))))))
  in
  let row_at (_, py) =
    (* a sticky row is the row under the pointer, not the one scrolled beneath it *)
    let slot = int_of_float (Float.floor ((py -. top) /. height)) in
    match if py < top then None else List.nth_opt (sticky_of scroll) slot with
    | Some k -> Some k
    | None ->
        let k = int_of_float (Float.floor ((py -. top +. scroll) /. height)) in
        if py < top || k < 0 || k >= count then None else Some k
  in
  (* the last flag ends 12 from the edge, a flag column is a 12-point flag and an 8-point gap *)
  let columns_x = x +. w -. 24. -. (flag_width *. float_of_int (max 0 (List.length columns - 1))) in
  let column_at (px, _) =
    if px < columns_x -. 4. then None else Some (int_of_float ((px -. columns_x) /. flag_width))
  in
  let flag k column =
    match List.nth_opt (at k).flags column with Some value -> value | None -> false
  in
  let is_selected id = List.mem id selected in
  let left = signal.button = Some Input.LeftButton in
  let modifier key = List.mem key (Ui.press_keys ui box) in
  (* Presses: select, start a row drag or a toggle paint. *)
  let t, intents =
    if not (signal.pressed && left) then (t, [])
    else
      match row_at signal.press_point with
      | None -> ({ t with drag = None }, [ Select [] ])
      | Some k -> (
          let row = at k in
          let px, _ = signal.press_point in
          let chevron_x = x +. 12. +. (float_of_int row.depth *. indent_step) in
          if has_children rows shown.(k) && px >= chevron_x -. 6. && px < chevron_x +. 10. then
            ( {
                t with
                folded =
                  (if Ids.mem row.id t.folded then Ids.remove row.id t.folded
                   else Ids.add row.id t.folded);
                drag = None;
              },
              [] )
          else
            match column_at signal.press_point with
            | Some column when column < List.length row.flags && not row.ghost ->
                let value = not (flag k column) in
                let ids = if is_selected row.id then selected else [ row.id ] in
                ( { t with drag = Some (Paint { column; value; painted = ids }) },
                  [ Flag { ids; column; value } ] )
            | _ ->
                let selection =
                  if modifier Input.Shift then
                    let anchor =
                      Option.value ~default:k (Option.bind t.anchor (position rows shown))
                    in
                    row.id :: List.filter (( <> ) row.id) (span rows shown anchor k)
                  else if modifier Input.Meta || modifier Input.Ctrl then
                    if is_selected row.id then List.filter (( <> ) row.id) selected
                    else row.id :: selected
                  else if is_selected row.id then row.id :: List.filter (( <> ) row.id) selected
                  else [ row.id ]
                in
                ( {
                    t with
                    focus = Some row.id;
                    anchor = (if modifier Input.Shift then t.anchor else Some row.id);
                    drag =
                      (if row.ghost then None else Some (Rows { ids = selection; moved = false }));
                  },
                  [ Select selection ] ))
  in
  (* Right-click: a menu on the row, or on the selection when the row is
       in it; it never changes the selection. *)
  let t =
    if not (Ui.context_clicked signal) then t
    else
      match row_at signal.release_point with
      | Some k when not (at k).ghost ->
          let id = (at k).id in
          let x, y = signal.release_point in
          { t with context = Some (x, y, id, if is_selected id then selected else [ id ]) }
      | Some _ | None -> t
  in
  let t, intents =
    match t.context with
    | None -> (t, intents)
    | Some (x, y, id, ids) -> (
        let shown =
          match List.find_opt (fun (row : row) -> row.id = id) (Array.to_list rows) with
          | Some row -> ( match row.flags with value :: _ -> Some value | [] -> None)
          | None -> None
        in
        match
          Ui.context_menu ui ~at:(x, y) "tree-context"
            [
              ("Enter", true);
              ("Rename", true);
              ((if shown = Some false then "Show" else "Hide"), shown <> None);
              ("Delete", true);
            ]
        with
        | `Open -> (t, intents)
        | `Dismiss -> ({ t with context = None }, intents)
        | `Pick 0 -> ({ t with context = None }, intents @ [ Activate id ])
        | `Pick 1 ->
            ( {
                t with
                context = None;
                renaming =
                  Some
                    ( id,
                      Option.fold ~none:""
                        ~some:(fun (row : row) -> row.label)
                        (List.find_opt (fun (row : row) -> row.id = id) (Array.to_list rows)) );
              },
              intents )
        | `Pick 2 ->
            ( { t with context = None },
              intents @ [ Flag { ids; column = 0; value = shown = Some false } ] )
        | `Pick _ -> ({ t with context = None }, intents @ [ Delete ids ]))
  in
  let intents =
    if signal.double_clicked && left then
      match row_at signal.pointer with
      | Some k -> intents @ [ Activate (at k).id ]
      | None -> intents
    else intents
  in
  (* Held gestures: a 4-point dead zone starts a row drag; paint applies
       the toggle value to each row the pointer passes. *)
  let t, intents =
    match t.drag with
    | Some (Rows { ids; moved }) when signal.held || signal.released ->
        let px, py = signal.pointer and sx, sy = signal.press_point in
        let moved = moved || Float.hypot (px -. sx) (py -. sy) >= 4. in
        if not signal.released then ({ t with drag = Some (Rows { ids; moved }) }, intents)
        else
          let drop =
            match row_at signal.pointer with
            | Some k when moved && (not (List.mem (at k).id ids)) && not (at k).ghost ->
                let offset = ((py -. top +. scroll) /. height) -. float_of_int k in
                [
                  Move
                    {
                      ids;
                      target = (at k).id;
                      drop =
                        (if offset < 0.25 then Before else if offset > 0.75 then After else Inside);
                    };
                ]
            | _ -> []
          in
          ({ t with drag = None }, intents @ drop)
    | Some (Paint { column; value; painted }) when signal.held || signal.released ->
        let fresh =
          match row_at signal.pointer with
          | Some k
            when (not (List.mem (at k).id painted))
                 && (not (at k).ghost)
                 && column < List.length (at k).flags
                 && flag k column <> value ->
              [ (at k).id ]
          | _ -> []
        in
        ( {
            t with
            drag =
              (if signal.released then None
               else Some (Paint { column; value; painted = fresh @ painted }));
          },
          if fresh = [] then intents else intents @ [ Flag { ids = fresh; column; value } ] )
    | Some _ -> ({ t with drag = None }, intents)
    | None -> (t, intents)
  in
  let drop_hint =
    match t.drag with
    | Some (Rows { moved = true; ids }) -> (
        match row_at signal.pointer with
        | Some k when not (List.mem (at k).id ids) ->
            let offset = ((snd signal.pointer -. top +. scroll) /. height) -. float_of_int k in
            Some (k, if offset < 0.25 then Before else if offset > 0.75 then After else Inside)
        | _ -> None)
    | _ -> None
  in
  Ui.draw ui box (fun paint _ ->
      let scroll = Ui.scroll_position ui box in
      let first = max 0 (int_of_float (Float.floor (scroll /. height))) in
      let last = min (count - 1) (int_of_float (Float.ceil ((scroll +. body) /. height))) in
      let sticky = sticky_of scroll in
      let text ?(color = theme.foreground) at label = Ui.Paint.text paint ~at ~color label in
      Ui.Paint.fill paint ~x ~y ~w ~h theme.panel;
      let ink_2 = Pxui.Theme.ink_2 theme and ink_3 = Pxui.Theme.ink_3 theme in
      let hover = if signal.hovered && t.drag = None then row_at signal.pointer else None in
      let draw_row k row_y =
        let row = at k in
        let index = shown.(k) in
        let selected = is_selected row.id in
        (* current (the keyboard cursor) and selected are the control fill; selected adds the
           accent brackets, drawn once the rows are down *)
        Ui.Paint.fill paint ~x ~y:row_y ~w ~h:height theme.panel;
        if selected then
          Ui.Paint.fill paint ~x:(x +. 6.) ~y:row_y ~w:(w -. 12.) ~h:height theme.control
        else if t.focus = Some row.id then
          Ui.Paint.fill paint ~x ~y:row_y ~w ~h:height theme.control
        else if hover = Some k then
          Ui.Paint.fill paint ~x ~y:row_y ~w ~h:height (Pxui.Theme.faint_border theme);
        let indent level = x +. 12. +. (float_of_int level *. indent_step) in
        let text_y = Kit.text_y ui row_y height in
        if has_children rows index then
          Ui.Paint.chevron paint
            ~at:(indent row.depth +. 3., row_y +. (height /. 2.))
            (if Ids.mem row.id t.folded && t.filter = None then `Right else `Down)
            ink_2;
        (* the kind: a letter in the label style, in ink once selected *)
        let badge_x = indent row.depth +. 14. in
        let letter = if row.link then "\xe2\x86\xb3" else fst row.badge in
        Ui.Paint.cap paint
          ~at:(badge_x, Kit.cap_y ui row_y height)
          ~color:(if selected || t.focus = Some row.id then theme.foreground else ink_2)
          letter;
        let label_x = badge_x +. Ui.Paint.cap_width paint letter +. 8. in
        let hidden = match row.flags with shown :: _ -> not shown | [] -> false in
        let color = if row.ghost || row.link || hidden then ink_3 else theme.foreground in
        text ~color (label_x, text_y) row.label;
        if row.detail <> "" then begin
          (* the detail sits before the flags, where the label leaves room *)
          let dw = Ui.Paint.text_width paint row.detail in
          let dx = columns_x -. 8. -. dw in
          if dx > label_x +. Ui.Paint.text_width paint row.label +. 8. then
            text ~color:ink_2 (dx, text_y) row.detail
        end;
        (* Flags: a square for the first column (visible), a round one for the others. *)
        List.iteri
          (fun column value ->
            let cx = columns_x +. (float_of_int column *. flag_width) +. 6.
            and cy = row_y +. (height /. 2.) in
            Ui.Paint.flag paint ~at:(cx, cy) ~round:(column > 0) value)
          row.flags
      in
      for k = first to last do
        draw_row k (top +. (float_of_int k *. height) -. scroll)
      done;
      (* the sheet's brackets: 4 points out of the fill, which is 6 in from the list's edge; after
         every row, since they reach into the rows above and below, whose ground would cover them *)
      let mark k row_y =
        if is_selected (at k).id then
          Ui.Paint.brackets paint ~x:(x +. 6.) ~y:row_y ~w:(w -. 12.) ~h:height ~offset:4.
            ~length:8. theme.accent
      in
      for k = first to last do
        mark k (top +. (float_of_int k *. height) -. scroll)
      done;
      List.iteri (fun slot k -> draw_row k (top +. (float_of_int slot *. height))) sticky;
      List.iteri (fun slot k -> mark k (top +. (float_of_int slot *. height))) sticky;
      if sticky <> [] then
        Ui.Paint.fill paint ~x
          ~y:(top +. (float_of_int (List.length sticky) *. height))
          ~w ~h:1. (Pxui.Theme.edge theme);
      (match drop_hint with
      | Some (k, drop) -> (
          let row_y = top +. (float_of_int k *. height) -. scroll in
          match drop with
          | Inside -> Ui.Paint.frame paint ~x ~y:row_y ~w ~h:height ~width:2. theme.accent
          | Before | After ->
              let line_y = if drop = Before then row_y else row_y +. height in
              Ui.Paint.line paint ~from_:(x, line_y) ~to_:(x +. w, line_y) ~width:2. theme.accent)
      | None -> ());
      (* Header: the level as a section label and the flag columns, or the filter under a prompt. *)
      Ui.Paint.fill paint ~x ~y ~w ~h:height theme.panel;
      if t.filter = None then begin
        Ui.Paint.cap paint ~at:(x +. 12., Kit.cap_y ui y height) ~color:ink_3 title;
        List.iteri
          (fun column name ->
            Ui.Paint.cap paint ~color:ink_2
              ~at:
                ( columns_x
                  +. (float_of_int column *. flag_width)
                  +. 6.
                  -. (Ui.Paint.cap_width paint name /. 2.),
                  Kit.cap_y ui y height )
              name)
          columns
      end);
  (* The filter field sits in the header; Escape clears it. *)
  let t =
    match t.filter with
    | None -> t
    | Some query -> (
        match
          Ui.panel ui ~x ~y ~width:w ~max_height:(height +. 16.) "tree-filter" (fun () ->
              Ui.picker ui "Filter rows" ~query (fun _ -> [||]))
        with
        | _, `Cancel ->
            Ui.unfocus ui;
            { t with filter = None }
        | query, (`Submit | `Pick _) ->
            Ui.unfocus ui;
            { t with filter = Some query }
        | query, _ -> { t with filter = Some query })
  in
  let t, intents =
    match t.renaming with
    | None -> (t, intents)
    | Some (id, name) -> (
        match
          Prompt.name ui ~key:"tree-rename" ~title:"Rename" ~label:"Name"
            ~description:"A new name for the row" ~query:name
        with
        | None | Some (_, `Cancel) ->
            Ui.dismiss_popup ui;
            ({ t with renaming = None }, intents)
        | Some (name, `Submit) ->
            Ui.dismiss_popup ui;
            ( { t with renaming = None },
              if String.trim name = "" then intents else intents @ [ Rename (id, String.trim name) ]
            )
        | Some (name, _) -> ({ t with renaming = Some (id, name) }, intents))
  in
  ({ t with reveal = false }, intents)
