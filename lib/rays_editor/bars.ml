(* The host bars of the workspace: the graph panel's toolbar (Add, Repeat, Iterate, fn, macro, defn: graph.html's list;
  the narrower workspace.html header drops macro).  Buttons only report a
   click: [Editor_frame] maps it to the one command or edit it means. *)
module Ui = Pxui.Ui

type tool = Add | Repeat | Iterate | Fn | Macro | Defn | Outline

let tools =
  [
    (Add, "Add", "A");
    (Repeat, "Repeat", "R");
    (Iterate, "Iterate", "\xe2\x87\xa7R");
    (Fn, "fn", "L");
    (Macro, "macro", "M");
    (Defn, "defn", "D");
  ]

(* The toolbar sits in the graph panel's header, after its title, breadcrumb and the 1 x 12 rule
   ([Pxui_shell.Chrome.tools_start]): text buttons 20 high and 8 apart, as wide as their text
   ([Pxui_shell.Kit.button_width]); a tool that would run into the views and the collapse button
   is left out. *)
let views = [ "Graph"; "List"; "Text" ]

let tool_rects ?(outline = false) ui ~header:(hx, hy, hw, hh) ~from =
  let hx = float hx and hy = float hy and hw = float hw and hh = float hh in
  let x = ref (hx +. from) in
  (* the views are 12 apart and end 36 points from the edge (8, the 20-point button, 8) *)
  let views_w = List.fold_left (fun w v -> w +. Pxui.Ui.text_width ui v) 24. views in
  let limit = hx +. hw -. 36. -. views_w -. 8. in
  let place tools =
    x := hx +. from;
    List.filter_map
      (fun (tool, label, hint) ->
        let w = Pxui_shell.Kit.button_width ui ~hint label in
        let at = !x in
        x := !x +. w +. 8.;
        if at +. w <= limit then Some (tool, label, hint, (at, hy +. ((hh -. 20.) /. 2.), w, 20.))
        else None)
      tools
  in
  (* macro goes first when the tools do not all fit *)
  let placed =
    match place tools with
    | all when List.length all = List.length tools -> all
    | _ -> place (List.filter (fun (tool, _, _) -> tool <> Macro) tools)
  in
  (* Outline, after the edit tools while the window has none: it splits one in at the right *)
  let w = Pxui_shell.Kit.button_width ui "Outline" in
  if outline && !x +. w <= limit then
    placed @ [ (Outline, "Outline", "", (!x, hy +. ((hh -. 20.) /. 2.), w, 20.)) ]
  else placed

let tool_rect ?outline ui ~header ~from tool =
  List.find_map
    (fun (t, _, _, r) -> if t = tool then Some r else None)
    (tool_rects ?outline ui ~header ~from)

(* the 1 x 12 rule that parts a header's title from its tools, [from] being where they start *)
let rule ui ~key ~header:(hx, hy, _, hh) ~from =
  Ui.draw ui
    (Ui.box ui ~w:(Ui.Px 1.) ~h:(Ui.Px 12.)
       ~at:(float hx +. from -. 13., float hy +. (float (hh - 12) /. 2.))
       key)
    (fun paint (x, y, w, h) -> Ui.Paint.fill paint ~x ~y ~w ~h (Pxui.Theme.border (Ui.theme ui)))

let graph_tools ?outline ui ~header ~from ~enabled =
  let rects = tool_rects ?outline ui ~header ~from in
  if rects <> [] then rule ui ~key:"workspace-tool-rule" ~header ~from;
  List.fold_left
    (fun clicked (tool, label, hint, (x, y, w, h)) ->
      if
        Pxui_shell.Kit.button ui ~key:("workspace-tool-" ^ label) ~at:(x, y) ~w ~h
          ~enabled:(enabled tool)
          ?hint:(if hint = "" then None else Some hint)
          label
      then Some tool
      else clicked)
    None rects

(* A panel tree as an editor graph: leaves and splits are bindings, so every panel edit finds
   its binding; [ponytail:] names are kind_n, not the study's prose names. *)
let tree_text ~name ~scene tree =
  let used = Hashtbl.create 8 and bindings = ref [] and names = ref [] in
  let bind base form =
    let n = 1 + Option.value ~default:0 (Hashtbl.find_opt used base) in
    Hashtbl.replace used base n;
    let name = if n = 1 then base else Printf.sprintf "%s_%d" base n in
    bindings := (name, form) :: !bindings;
    name
  in
  let rec go path tree =
    let name = node path tree in
    names := (List.rev path, name) :: !names;
    name
  and node path : Editor_core.Panels.t -> string = function
    | Leaf p ->
        let base, form =
          match p with
          | Canvas key -> ("canvas", Printf.sprintf "(ui/canvas (ref %s))" key)
          | View _ -> ("preview", Printf.sprintf "(ui/viewport (ref %s))" scene)
          | Graph -> ("network", "(ui/graph)")
          | List -> ("list", "(ui/list)")
          | Lisp -> ("code", "(ui/lisp)")
          | Inspector -> ("inspector", "(ui/inspector)")
          | Outline -> ("outline", "(ui/outline)")
          | Spreadsheet -> ("spreadsheet", "(ui/spreadsheet)")
          | Timeline -> ("timeline", "(ui/timeline)")
        in
        bind base form
    | Split { axis; size; a; b } ->
        let a = go (0 :: path) a in
        let b = go (1 :: path) b in
        let axis = if axis = `H then "horizontal" else "vertical" in
        bind "split"
          (match size with
          | `Ratio ratio ->
              Printf.sprintf "(ui/split-at %S %s %s %s)" axis
                (Flow.Lisp.float (Float.round (ratio *. 1e4) /. 1e4))
                a b
          | `First n -> Printf.sprintf "(ui/split %S %s %s :first_size %d)" axis a b n
          | `Second n -> Printf.sprintf "(ui/split %S %s %s :second_size %d)" axis a b n)
    | Tile cells ->
        bind "tile"
          ("(ui/tile " ^ String.concat " " (List.mapi (fun i c -> go (i :: path) c) cells) ^ ")")
    | Float t -> bind "floating" ("(ui/floating " ^ go (0 :: path) t ^ ")")
  in
  let root = go [] tree in
  ( Printf.sprintf "(graph %s :context editor\n  (let* [%s]\n    (ui/workspace %s)))" name
      (String.concat "\n         " (List.rev_map (fun (n, f) -> n ^ " " ^ f) !bindings))
      root,
    fun path -> List.assoc_opt path !names )
