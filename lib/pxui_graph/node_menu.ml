module Ui = Pxui.Ui

type entry = {
  key : string;
  label : string;
  category : string list;
  arity : int;
  context : string; (* the kind's graph: "sop", "value", "scene" ... *)
  output : Flow.Ty.t; (* what the node makes: the colour of its square *)
  off : string option; (* why the kind cannot be placed in this graph, if it cannot *)
}

let of_ops ?extra context =
  Flow.Op.of_context ?extra context
  |> List.filter_map (fun (op : Flow.Op.t) ->
      if op.ctx <> context then None
      else
        Some
          {
            key = "=" ^ op.name;
            label = op.name;
            category = [ op.category ];
            arity = 0;
            context = Flow.Context.name context;
            output = op.out (List.map snd op.signature.pos);
            off = None;
          })

type item = { entry : entry; lower_key : string; lower_label : string; lower_category : string }

type t = {
  items : item array;
  x : int;
  y : int;
  query : string;
  after : string option; (* the node the new one goes after: the title says so *)
}

let menu_width = 320
let position menu = (menu.x, menu.y)
let menu_limit = 10

let create ?after ~x ~y entries =
  let seen = Hashtbl.create (List.length entries) in
  let items =
    List.filter
      (fun entry ->
        if
          String.trim entry.key = ""
          || String.trim entry.label = ""
          || entry.category = []
          || List.exists (fun part -> String.trim part = "") entry.category
          || entry.arity < 0 || Hashtbl.mem seen entry.key
        then false
        else begin
          Hashtbl.add seen entry.key ();
          true
        end)
      entries
    |> List.map (fun entry ->
        {
          entry;
          lower_key = String.lowercase_ascii entry.key;
          lower_label = String.lowercase_ascii entry.label;
          lower_category = String.lowercase_ascii (String.concat " / " entry.category);
        })
  in
  { items = Array.of_list items; x; y; query = ""; after }

let search_rank query item =
  let contains text =
    let rec at index =
      index + String.length query <= String.length text
      && (String.sub text index (String.length query) = query || at (index + 1))
    in
    at 0
  in
  if String.starts_with ~prefix:query item.lower_label then 0
  else if List.exists (String.starts_with ~prefix:query) (String.split_on_char ' ' item.lower_label)
  then 1
  else if contains item.lower_label || contains item.lower_key then 2
  else if Ui.fuzzy_match ~query item.lower_label || Ui.fuzzy_match ~query item.lower_key then 3
  else 4

(* The rows: every match of the query, or, before anything is typed, every kind that can be placed
   here: the ones that take an input first when the menu is after a node (what is likely to follow it),
   then the rest, each by name.  A category is reached by typing its name. *)
let rows menu =
  let items = Array.to_list menu.items in
  if menu.query <> "" then begin
    let query = String.lowercase_ascii menu.query in
    (* the kinds that cannot be placed here follow the ones that can *)
    items
    |> List.filter (fun item ->
        Ui.fuzzy_match ~query item.lower_label
        || Ui.fuzzy_match ~query item.lower_category
        || Ui.fuzzy_match ~query item.lower_key)
    |> List.sort (fun left right ->
        let off item = if item.entry.off = None then 0 else 1 in
        let order = Int.compare (off left) (off right) in
        let order =
          if order <> 0 then order
          else Int.compare (search_rank query left) (search_rank query right)
        in
        if order <> 0 then order else String.compare left.entry.label right.entry.label)
    |> List.map (fun item -> item.entry)
    |> Array.of_list
  end
  else
    let fits entry = if menu.after <> None && entry.arity = 0 then 1 else 0 in
    (* the host lists the graph's own kinds first: a graph's kinds keep that order, by name within *)
    let contexts =
      List.fold_left
        (fun seen item ->
          if List.mem item.entry.context seen then seen else seen @ [ item.entry.context ])
        [] items
    in
    let context entry =
      let rec find i = function
        | [] -> i
        | c :: rest -> if c = entry.context then i else find (i + 1) rest
      in
      find 0 contexts
    in
    items
    |> List.filter (fun item -> item.entry.off = None)
    |> List.map (fun item -> item.entry)
    |> List.sort (fun a b ->
        let order = Int.compare (fits a) (fits b) in
        let order = if order <> 0 then order else Int.compare (context a) (context b) in
        if order <> 0 then order else String.compare a.label b.label)
    |> Array.of_list

(* what a row says on the right: the graph the kind is for and its category ([sop / points]), or why
   it cannot go here *)
let entry_detail entry =
  match entry.off with
  | Some reason -> reason
  | None -> (
      entry.context
      ^
      match List.rev entry.category with
      | last :: _ -> " / " ^ String.lowercase_ascii last
      | [] -> "")

(* a row of the results; an unplaceable kind is named by its graph and key *)
let entry_label entry =
  match entry.off with Some _ -> entry.context ^ "/" ^ entry.key | None -> entry.label

(* the colour of a type's port: a kind's square here, the ports, squares and wires of the graph pane.
   A list is its elements' colour; what has no colour of its own is the output's *)
let color theme (role : Flow.Ty.color) =
  let ports = Pxui.Theme.ports theme in
  match role with
  | `Geometry -> ports.geometry
  | `Float -> ports.float
  | `Int -> ports.int
  | `Bool -> ports.bool
  | `Vec3 -> ports.vec3
  | `Text -> ports.text
  | `Fn -> ports.fn
  | `Record -> ports.record
  | `Output -> ports.output
  | `Compound -> ports.compound

let rec port_color theme (ty : Flow.Ty.t) =
  let ports = Pxui.Theme.ports theme in
  match ty with
  | Named _ -> color theme (Flow.Ty.color ty)
  | Float -> ports.float
  | Int -> ports.int
  | Vec2 | Vec3 | Vec4 -> ports.vec3
  | Bool -> ports.bool
  | Text | Color -> ports.text
  | Fn _ -> ports.fn
  | Record _ -> ports.record
  | List e | Array e -> port_color theme e
  | Any -> ports.output

let picker_rows menu query =
  rows { menu with query } |> Array.map (fun entry -> (entry_label entry, entry_detail entry))

(* The node menu, the sheet's [01]: a search field over the kinds, the likeliest first, the first
   row current (Enter places it); typing narrows them, by name, key or category.  A press outside or
   Escape closes it.  Returns the menu while it stays open and the key of the entry picked this
   frame. *)
let update menu ui ~bounds:(bx, by, bw, bh) =
  let row = Ui.row_height ui in
  let shown = Array.length (rows { menu with query = "" }) in
  (* the edge, the title (4 above its row), the search row, the rows and the foot (a hairline under 4,
     then a bar) *)
  let height = 2 + (4 + row) + 28 + (min menu_limit shown * row) + (5 + row) in
  let width = menu_width in
  let x = max bx (min menu.x (bx + bw - width)) and y = max by (min menu.y (by + bh - height)) in
  let result =
    Ui.popup ui
      ~stroke:(Pxui.Theme.border (Ui.theme ui))
      ~at:(float_of_int x, float_of_int y)
      ~width:(float_of_int width) ~height:(float_of_int height) "pxui-graph-menu"
      (fun () ->
        let theme = Ui.theme ui in
        (* the title row: the label at 12 and, right-aligned 8 from the edge in ink-2 at the label size,
         the node the new one goes after *)
        let title =
          Ui.box ui ~w:Ui.Grow ~h:(Ui.Px (float_of_int (4 + row))) "pxui-graph-menu-title"
        in
        Ui.draw ui title (fun paint (x, y, w, h) ->
            let size = Ui.Paint.label_size paint in
            let top = Ui.text_top ui ~size (y +. 4.) (h -. 4.) in
            Ui.Paint.cap paint ~at:(x +. 12., top) "Add node";
            Option.iter
              (fun name ->
                let text = "after " ^ name in
                Ui.Paint.text paint ~size ~color:(Pxui.Theme.ink_2 theme)
                  ~at:(x +. w -. 8. -. Ui.Paint.text_width paint ~size text, top)
                  text)
              menu.after);
        (* the rows, kept so that a row's square and unavailability are known *)
        let latest = ref [||] in
        let query, pick =
          Ui.picker ui ~limit:menu_limit "type to search" ~query:menu.query
            ~mark:(fun index ->
              if index < Array.length !latest then Some (port_color theme !latest.(index).output)
              else None)
            ~off:(fun index -> index < Array.length !latest && !latest.(index).off <> None)
            (fun query ->
              latest := rows { menu with query };
              picker_rows menu query)
        in
        (* a cross at the right of the search row clears it *)
        let clear =
          Ui.box ui
            ~flags:Ui.(clickable + blocking)
            ~w:(Ui.Px 20.) ~h:(Ui.Px 20.)
            ~at:(float_of_int (x + width - 25), float_of_int (y + 1 + row + 4 + 4))
            "pxui-graph-menu-clear"
        in
        if query <> "" then
          Ui.draw ui clear (fun paint (cx, cy, _, _) ->
              Ui.Paint.text paint
                ~at:(cx +. 6.5, Ui.text_top ui cy 20.)
                ~color:(Pxui.Theme.muted (Ui.theme ui))
                "\xc3\x97");
        let query = if (Ui.signal ui clear).clicked then "" else query in
        Ui.footer ui
          ~right:
            (Printf.sprintf "%d of %d"
               (Array.length (rows { menu with query }))
               (Array.length menu.items))
          [ ("\xe2\x86\x91\xe2\x86\x93", "move"); ("\xe2\x86\xb5", "place") ];
        (query, pick))
  in
  let menu, picked =
    match result with
    | None -> (None, None)
    | Some (query, pick) -> (
        let menu = { menu with query } in
        match pick with
        | `Cancel -> (None, None)
        | `Pick index -> (None, Some (rows menu).(index).key)
        | _ -> (Some menu, None))
  in
  if menu = None then Ui.dismiss_popup ui;
  (menu, picked)

module Private = struct
  let keys menu ~query =
    rows { menu with query } |> Array.to_list |> List.map (fun entry -> entry.key)
end
