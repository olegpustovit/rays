(* The Navigator panel: see navigator.mli. *)
module Ui = Pxui.Ui
module W = Flow.Workspace
module P = Flow_graph.Projection
module S = Flow.Syntax

type path = W.path

type intent =
  | Open of { graph : string; node : path option }
  | Set_default of { graph : string; input : string; value : float; integer : bool }
  | Macro of string
  | Rename of { graph : string; to_ : string }
  | Remove of string
  | Layout of int
  | New_graph of string
  | Add
  | Flag of { node : path; name : string; value : bool }

(* [rename]: the graph whose name the field holds, and whether the field has opened yet *)
type state = { query : string; typing : bool; rename : (string * bool) option;
               scene_closed : bool;  (* the scene's root row is folded: its objects are hidden *)
               opened : string list;  (* graphs whose node rows are unfolded under their row *)
               context : (float * float * string option) option
                 (* the right-click menu: its point and the graph row it was opened on *) }

(* the contexts a graph can be created in from the UI *)
let new_graph_contexts = [ "sop"; "scene"; "draw"; "image"; "value"; "material" ]

let initial = { query = ""; typing = false; rename = None; scene_closed = false; opened = []; context = None }
let open_graph graph s = if List.mem graph s.opened then s else { s with opened = graph :: s.opened }
let with_query query s = { s with query }
let query s = s.query

(* a scene object: its kind's letter, what it is, its visible and render flags (where it has
   them), whether it is the render camera, and the binding that places it *)
type obj = { depth : int; letter : string; name : string; detail : string;
             visible : bool option; render : bool option; lead : bool; inert : bool; chosen : bool;
             home : path option }

type params = {
  workspace : W.t;
  active : string option;
  scope : P.scope option;
  records : Flow_graph.Probe.t option;
  probes : path -> int;
  selected : path list;
  chips : (string * Rays_math.Color.t) list;  (** the evaluated colour of each material graph *)
  objects : obj list;
  root_detail : string;  (** what the scene's root row says: its samples *)
  layouts : (string list * int) option;
  notes : (string * string) list;
}

type row =
  | Head of string * string  (* a section's label and what its right column is *)
  | Object_row of obj
  | Root_row of { graph : string; detail : string; folded : bool; active : bool }
  | Layout_row of { index : int; label : string; active : bool }
  | Input_row of { graph : string; name : string; integer : bool; value : float }
  | Graph_row of { graph : string; label : string; context : W.context option; detail : string;
                   active : bool; chip : Rays_math.Color.t option; dim : bool; used : int option }
  | Node_row of { graph : string; path : path; depth : int; label : string; detail : string;
                  zone : string option; ty : Flow.Ty.t; result : bool; selected : bool }
  | Macro_row of string * int
  | Link_row of { label : string; graph : string; outgoing : bool }
  | Empty of string

(* ---- reading the source ---- *)

let rec walk f (form : S.t) = f form; List.iter (walk f) (S.children form)

let head = S.head

let count_where pred form =
  let n = ref 0 in walk (fun f -> if pred f then incr n) form; !n

let loops (form : S.t) =
  count_where (fun f -> match head f with Some ("for" | "fold" | "scan" | "sum") -> true | _ -> false) form

(* the graphs a graph form reads with (ref name ...) *)
let reads (form : S.t) =
  let names = ref [] in
  walk (fun f -> match f.node with
    | S.List [ { S.node = S.Sym "ref"; _ }; { S.node = S.Sym n; _ } ]
    | S.List ({ S.node = S.Sym "ref"; _ } :: { S.node = S.Sym n; _ } :: _) ->
        if not (List.mem n !names) then names := n :: !names
    | _ -> ()) form;
  List.rev !names

(* the evaluated colour of each material graph: a vector, or the hex text the graph wrote *)
let chips (ev : Flow.Eval.t) =
  let byte f = int_of_float (Float.round (255. *. Float.min 1. (Float.max 0. f))) in
  List.filter_map (fun (name, v) -> match v with
    | Flow.Eval.Struct ("material/standard", _, fields) ->
        let colour = match List.assoc_opt "color" fields with
          | Some (Flow.Eval.Vec3 (r, g, b)) -> Some (Rays_math.Color.rgb (byte r) (byte g) (byte b))
          | Some (Flow.Eval.Text hex) -> Result.to_option (Rays_math.Color.hex hex)
          | None -> Some Rays_math.Color.white
          | Some _ -> None in
        Option.map (fun c -> name, c) colour
    | _ -> None) ev.results

(* what a material graph says of itself beside its swatch: its roughness *)
let notes (ev : Flow.Eval.t) =
  List.filter_map (fun (name, v) -> match v with
    | Flow.Eval.Struct ("material/standard", _, fields) ->
        (match List.assoc_opt "roughness" fields with
         | Some (Flow.Eval.Float r) -> Some (name, Printf.sprintf "rough %.2g" r)
         | Some (Flow.Eval.Int r) -> Some (name, Printf.sprintf "rough %d" r)
         | _ -> None)
    | _ -> None) ev.results

(* The outline's groups, in dependency order. *)
let group_of context = (Flow.Context.descriptor context).group

let group_order () = List.fold_left (fun groups context ->
  let group = (Flow.Context.descriptor context).group in
  if List.mem group groups then groups else groups @ [group]) [] (Flow.Context.all ())

let grouped (ws : W.t) =
  List.filter_map (fun group ->
    match List.filter (fun (g : W.graph) -> group_of g.context = group) ws.graphs with
    | [] -> None | graphs -> Some (group, graphs)) (group_order ())

let jump_rows (ws : W.t) =
  List.concat_map (fun (group, graphs) -> List.map (fun (g : W.graph) -> g.name, group) graphs) (grouped ws)

(* how many other graphs read [name] *)
let readers (ws : W.t) name =
  List.length (List.filter (fun (g : W.graph) -> g.name <> name && List.mem name (reads g.form)) ws.graphs)

let calls (ws : W.t) name =
  let forms = List.map (fun (g : W.graph) -> g.form)
    (ws.graphs @ List.filter (fun (d : W.graph) -> d.name <> name) ws.defs) @ ws.macros in
  List.fold_left (fun n form -> n + count_where (fun f -> head f = Some name) form) 0 forms

let macro_name (form : S.t) = match form.node with
  | S.List ({ S.node = S.Sym "defmacro"; _ } :: { S.node = S.Sym n; _ } :: _) -> Some n
  | _ -> None

let macro_uses (ws : W.t) name =
  calls ws name

let plural n what = Printf.sprintf "%d %s%s" n what (if n = 1 then "" else "s")

let zone_glyph : P.zone_kind -> string = function
  | For -> "for" | Fold -> "fold" | Scan -> "scan" | Sum -> "sum" | Let -> "let*" | Fn -> "λ" | State -> "state" | Branch -> "?"

let number (form : S.t) = match form.node with
  | S.Num text -> float_of_string_opt text
  | _ -> None

(* ---- rows ---- *)

let rec node_rows ~graph ~depth p (scope : P.scope) chains counts acc =
  List.iter (fun (n : P.node) ->
    let chain = Option.value ~default:[] (Hashtbl.find_opt chains n.path) in
    let detail = match n.zone, p.records with
      | Some z, _ when z.kind = Let -> "scope"
      | Some _, _ ->
          (match List.assoc_opt n.path counts with Some c -> string_of_int c ^ "×" | None -> "")
      | None, Some records ->
          (Flow_graph.Probe.footer records n ~probes:(List.map p.probes chain)).value
      | None, None -> "" in
    acc := Node_row { graph; path = n.path; depth; detail; ty = n.ty; result = n.synthetic;
                      label = P.title n;
                      zone = Option.map (fun (z : P.zone) -> zone_glyph z.kind) n.zone;
                      selected = List.mem n.path p.selected } :: !acc;
    Option.iter (fun (z : P.zone) -> node_rows ~graph ~depth:(depth + 1) p z.scope chains counts acc)
      n.zone) scope.nodes

let contains ~query text =
  let q = String.lowercase_ascii query and t = String.lowercase_ascii text in
  let n = String.length q and m = String.length t in
  let rec at i = i + n <= m && (String.sub t i n = q || at (i + 1)) in
  n = 0 || at 0

let graph_row p (g : W.graph) detail =
  let used = readers p.workspace g.name in
  (* the scene's own reference counts: its World object reads the graph *)
  let used = if g.context <> Flow.Context.world then used else
      max used (List.length (List.filter (fun o -> o.letter = "W" && o.detail = "ref " ^ g.name) p.objects)) in
  let unused = used = 0 && (g.context = Flow.Context.material || g.context = Flow.Context.sop) in
  Graph_row { graph = g.name; label = g.name; context = Some g.context; active = p.active = Some g.name;
              chip = (if g.context = Flow.Context.material then List.assoc_opt g.name p.chips else None);
              dim = unused;
              (* how many graphs read it, in the section's right column *)
              used = (if g.context = Flow.Context.material || g.context = Flow.Context.sop || g.context = Flow.Context.world then
                Some used else None);
              detail = (if unused then "unused" else if g.context = Flow.Context.material
                        then Option.value ~default:"" (List.assoc_opt g.name p.notes) else detail) }

let search state p chains counts =
  let q = String.trim state.query in
  let acc = ref [] in
  let ws = p.workspace in
  let hits = ref 0 in
  let add row = incr hits; acc := row :: !acc in
  List.iter (fun (g : W.graph) ->
    if contains ~query:q g.name then add (graph_row p g (group_of g.context))) ws.graphs;
  List.iter (fun (d : W.graph) ->
    if contains ~query:q d.name then
      add (Graph_row { graph = "def:" ^ d.name; label = d.name; context = None;
                       detail = "function"; active = p.active = Some ("def:" ^ d.name);
                       chip = None; dim = false; used = None })) ws.defs;
  (match p.scope, p.active with
   | Some scope, Some graph ->
       let inner = ref [] in
       node_rows ~graph ~depth:0 p scope chains counts inner;
       List.iter (function
         | Node_row n as row when contains ~query:q n.label -> add row
         | _ -> ()) (List.rev !inner)
   | _ -> ());
  Head (Printf.sprintf "%s · click opens" (plural !hits "match"), "") :: List.rev !acc
  |> fun rows -> if !hits = 0 then rows @ [ Empty "nothing matches" ] else rows

let rows ?(wide = true) state p =
  let p = if p.workspace.defs=[] && p.workspace.macros=[] then p else
    let authored = Hashtbl.create 16 in
    List.iter (fun (form : S.t) -> match form.node with
      | S.List ({node=S.Sym "workspace";_}::_::forms) ->
          List.iter (fun (form : S.t) -> Hashtbl.replace authored form.id ()) forms
      | _ -> Hashtbl.replace authored form.id ()) p.workspace.source;
    let workspace = {p.workspace with
      W.defs=List.filter (fun (definition : W.graph) -> Hashtbl.mem authored definition.form.id) p.workspace.defs;
      macros=List.filter (fun (form : S.t) -> Hashtbl.mem authored form.id) p.workspace.macros} in
    {p with workspace} in
  let ws = p.workspace in
  let chains = match p.scope with Some s -> Flow_graph.Probe.chains s | None -> Hashtbl.create 1 in
  let counts = match p.scope, p.records with
    | Some scope, Some records -> Flow_graph.Probe.counts records scope ~probe:p.probes
    | _ -> [] in
  if String.trim state.query <> "" then Array.of_list (search state p chains counts) else begin
    let acc = ref [] in
    let add row = acc := row :: !acc in
    let active_tree graph =
      match p.scope with
      | Some scope when wide && p.active = Some graph && List.mem graph state.opened -> node_rows ~graph ~depth:1 p scope chains counts acc
      | _ -> () in
    let nodes (form : S.t) = count_where (fun f -> match head f with
      | Some h -> String.contains h '/' | None -> false) form in
    let defs_rows () =
      List.iter (fun (d : W.graph) ->
        let n = calls ws d.name in
        add (Graph_row { graph = "def:" ^ d.name; label = d.name; context = None;
                         detail = (if d.inputs = [] then "defn"
                                   else "defn \xc2\xb7 " ^ String.concat " " (List.map (fun (name, _, _) -> name) d.inputs));
                         active = p.active = Some ("def:" ^ d.name);
                         chip = None; dim = false; used = Some n });
        active_tree ("def:" ^ d.name)) ws.defs;
      List.iter (fun m -> Option.iter (fun name ->
        add (Macro_row (name, macro_uses ws name))) (macro_name m)) ws.macros in
    let groups = grouped ws in
    (* the narrow sheet has no Layout section, and so none of the editor graphs *)
    let groups = if wide then groups else List.filter (fun (g, _) -> g <> "Layout") groups in
    (* the scene's first graph is the root row; its objects are its tree *)
    let root_graph = List.find_opt (fun (g : W.graph) -> g.context = Flow.Context.scene) ws.graphs in
    List.iter (fun (group, graphs) ->
      add (Head (group, match group with
        | "Scene" -> "v  r" | "Geometry" | "Materials" -> "used" | "Layout" -> "/ [" | _ -> ""));
      let graph_rows graphs = List.iter (fun (g : W.graph) ->
        let n = loops g.form in
        add (graph_row p g (match g.context with
          | context when context = Flow.Context.sop -> plural (nodes g.form) "node" ^ (if n > 0 then " \xc2\xb7 " ^ plural n "loop" else "")
          | _ -> if n > 0 then plural n "loop" else W.context_name g.context));
        active_tree g.name) graphs in
      if group = "Scene" then begin
        (match root_graph with
         | Some g ->
             add (Root_row { graph = g.name; detail = p.root_detail; folded = state.scene_closed;
                             active = p.active = Some g.name });
             if not state.scene_closed then List.iter (fun o -> add (Object_row o)) p.objects;
             graph_rows (List.filter (fun (x : W.graph) -> x != g) graphs)
         | None -> graph_rows graphs; List.iter (fun o -> add (Object_row o)) p.objects)
      end
      else if group = "Layout" then begin
        (* the numbered layouts, then the editor graphs that hold them *)
        Option.iter (fun (labels, active) ->
          List.iteri (fun index label -> add (Layout_row { index; label; active = index = active })) labels)
          p.layouts;
        graph_rows graphs
      end
      else graph_rows graphs) groups;
    if ws.defs <> [] || ws.macros <> [] then begin
      add (Head ("Definitions", "used")); defs_rows ()
    end;
    (* the active graph's inputs *)
    (match p.scope, p.active with
     | Some scope, Some graph when scope.inputs <> [] ->
         add (Head ("Inputs", if not wide then "" else if String.length graph > 4 && String.sub graph 0 4 = "def:"
                                then String.sub graph 4 (String.length graph - 4) else graph));
         List.iter (fun (i : P.input) ->
           match Option.bind i.default number with
           | Some value when i.ty = Flow.Ty.Int || i.ty = Flow.Ty.Float ->
               add (Input_row { graph; name = i.name; integer = i.ty = Flow.Ty.Int; value })
           | _ -> ()) scope.inputs
     | _ -> ());
    (match p.active with
     | Some graph when wide && not (String.length graph > 4 && String.sub graph 0 4 = "def:") ->
         let out = match List.find_opt (fun (g : W.graph) -> g.name = graph) ws.graphs with
           | Some g -> List.filter (fun n -> List.exists (fun (x : W.graph) -> x.name = n) ws.graphs) (reads g.form)
           | None -> [] in
         let by = List.filter_map (fun (g : W.graph) ->
           if g.name <> graph && List.mem graph (reads g.form) then Some g.name else None) ws.graphs in
         add (Head ("Data flow", graph));
         List.iter (fun n -> add (Link_row { label = "reads"; graph = n; outgoing = true })) out;
         List.iter (fun n -> add (Link_row { label = "read by"; graph = n; outgoing = false })) by;
         if out = [] && by = [] then add (Empty "no other graph reads or is read")
     | _ -> ());
    Array.of_list (List.rev !acc)
  end

let describe = function
  | Head (s, right) -> if right = "" then String.uppercase_ascii s else String.uppercase_ascii s ^ " · " ^ right
  | Object_row o -> Printf.sprintf "%s%s %s · %s%s%s" (String.make (2 * o.depth) ' ') o.letter o.name o.detail
      (match o.visible with Some true -> " · v" | _ -> "") (match o.render with Some true -> " · r" | _ -> "")
  | Root_row { graph; detail; folded; _ } ->
      Printf.sprintf "R root (%s) · %s%s" graph detail (if folded then " · folded" else "")
  | Layout_row { index; label; active } -> Printf.sprintf "layout %d %s%s" index label (if active then " *" else "")
  | Input_row { name; value; _ } -> Printf.sprintf "input %s = %g" name value
  | Graph_row { label; detail; active; chip; used; _ } ->
      (* an unused graph says so in its detail, any other how often it is read *)
      String.concat " · " (List.filter (( <> ) "") [
        (if active then "> " else "") ^ label; detail;
        (match used with Some n when detail <> "unused" -> Printf.sprintf "×%d" n | _ -> "");
        (match chip with
         | Some c -> Rays_math.Color.to_hex_string { c with a = 255 }
         | None -> "") ])
  | Node_row { label; detail; depth; zone; _ } ->
      Printf.sprintf "%s%s%s · %s" (String.make (2 * depth) ' ')
        (match zone with Some z -> z ^ " " | None -> "") label detail
  | Macro_row (name, n) -> Printf.sprintf "λ %s · %s" name (plural n "use")
  | Link_row { label; graph; _ } -> label ^ " " ^ graph
  | Empty s -> s

(* ---- drawing ---- *)

(* a graph's square wears a port colour of the kit: geometry for a SOP graph, record for a World *)
let context_color theme context =
  let context = Option.value ~default:Flow.Context.sop context in
  Pxui_graph.Node_menu.color theme (Flow.Context.descriptor context).color

let type_color theme (ty : Flow.Ty.t) =
  let ports = Pxui.Theme.ports theme in
  match ty with
  | Named _ -> Pxui_graph.Node_menu.color theme
      (match Flow.Ty.color ty with `Output -> `Compound | color -> color)
  | Float -> ports.float | Int -> ports.int | Bool -> ports.bool
  | Vec2 | Vec3 | Vec4 -> ports.vec3 | Text -> ports.text | Fn _ -> ports.fn | Record _ -> ports.record
  | List _ | Array _ | Color | Any -> ports.compound

let height_of ~rh = function
  | Head _ -> rh +. 16.
  | _ -> rh

(* the top of every row below the search field, and the total *)
let tops ~rh rows =
  let tops = Array.make (Array.length rows + 1) 0. in
  (* a section has 16 points of space above it, the first one 4 *)
  Array.iteri (fun i r -> tops.(i + 1) <- tops.(i) +. height_of ~rh r
    -. (match i, r with 0, Head _ -> 12. | _ -> 0.)) rows;
  tops

(* the search row: 4 points of margin over a 24-point row *)
let search_height = 28.

let row_rects state p ~bounds:(x, y, w, _) =
  let rh = 24. and x = float x and y = float y and w = float w in
  let rows = rows ~wide:(w >= 300.) state p in
  let t = tops ~rh rows in
  Array.mapi (fun i r -> r, (x, y +. search_height +. t.(i), w, t.(i + 1) -. t.(i))) rows

(* the list's rows, painted in the content child so the list's clip holds them *)
let paint_rows ~rows ~state ~p ~label_w ~wide ~muted ~hovered ~rh ~tops ~(theme : Pxui.Theme.t) ~body ~w ~top ~x ~box ~content ui =
  Ui.draw ui content (fun paint _ ->
    let scroll = Ui.scroll_position ui box in
    Ui.Paint.fill paint ~x ~y:top ~w ~h:body theme.panel;
    Array.iteri (fun k row ->
      let ry = top +. tops.(k) -. scroll and rhh = tops.(k + 1) -. tops.(k) in
      if ry +. rhh > top && ry < top +. body then begin
        let ink_3 = Pxui.Theme.ink_3 theme in
        let ty = Pxui_shell.Kit.text_y ui ry rh in
        let text ?(color = theme.foreground) at label =
          Ui.Paint.text paint ~at:(fst at, ty) ~color label in
        (* a zone's kind: an accent label, no fill *)
        let tag ~at:tx glyph =
          Ui.Paint.cap paint ~at:(tx, Pxui_shell.Kit.cap_y ui ry rh) ~color:theme.accent glyph;
          tx +. Ui.Paint.cap_width paint glyph +. 8. in
        (* hover is the faintest line, the current row the control fill, the selected one adds
           the accent brackets *)
        let shade ?(selected = false) current =
          if selected then begin
            Ui.Paint.fill paint ~x:(x +. 4.) ~y:ry ~w:(w -. 8.) ~h:rhh theme.control;
            Ui.Paint.brackets paint ~x:(x +. 4.) ~y:ry ~w:(w -. 8.) ~h:rhh ~offset:3. ~length:8. ~width:2. theme.accent
          end else if current then Ui.Paint.fill paint ~x ~y:ry ~w ~h:rhh theme.control
          else if hovered = Some k then
            Ui.Paint.fill paint ~x ~y:ry ~w ~h:rhh (Pxui.Theme.faint_border theme) in
        (* the detail at the row's end takes at most half the row; the label keeps the rest *)
        let right_text label =
          let label = Ui.ellipsis ~width:(Ui.Paint.text_width paint) ~limit:((w -. 24.) /. 2.) label in
          let tw = Ui.Paint.text_width paint label in
          Ui.Paint.text paint ~at:(x +. w -. tw -. 12., ty) ~color:muted label;
          x +. w -. tw -. 20. in
        (* a label with a detail at the right stops short of it instead of running under it *)
        let labelled ?color from label detail =
          let edge = right_text detail in
          text ?color (from, 0.) (Ui.ellipsis ~width:(Ui.Paint.text_width paint) ~limit:(edge -. from) label) in
        let square ~at:sx color = Ui.Paint.fill paint ~x:sx ~y:(ry +. (rh /. 2.) -. 4.) ~w:8. ~h:8. color in
        (* the right columns: a 20-point count, or the two 12-point flags 8 apart *)
        let flag_x column = x +. w -. 12. -. 12. -. (float (1 - column) *. 20.) in
        match row with
        | Head (s, right) ->
            let cy = Pxui_shell.Kit.cap_y ui (ry +. rhh -. rh) rh in
            Ui.Paint.cap paint ~at:(x +. 12., cy) ~color:ink_3 s;
            (* the right column is a label in ink-3 too: two 12-point columns over the flags, a
               word, or the leader keys as the kit's key text *)
            if right = "v  r" then List.iteri (fun column name ->
              Ui.Paint.cap paint ~at:(flag_x column +. 6. -. (Ui.Paint.cap_width paint name /. 2.), cy) ~color:ink_3 name)
              [ "v"; "r" ]
            else if right = "/ [" then begin
              let small = Pxui_shell.Kit.cap_size ui in
              let kw = Ui.Paint.text_width paint ~size:small right in
              Ui.Paint.text paint ~size:small ~at:(x +. w -. 12. -. kw, cy)
                ~color:ink_3 right
            end else if right <> "" && (wide || right <> "used") then
              Ui.Paint.cap paint ~at:(x +. w -. 12. -. Ui.Paint.cap_width paint right, cy) ~color:ink_3 right
        | Object_row o ->
            shade ~selected:o.chosen false;
            (* the objects are the children of the root row (its letter at 26 wide, 12 narrow), each level 8 points in; wide,
              the root's chevron takes the first 14; the selected row sits in a 4-point wrapper with its own
               padding (15 and 3), so its letter is a point left and its flags 5 points right *)
            let ox = if o.chosen && wide then x +. 26.5
              else x +. (if wide then 38. else 20.) +. 8. *. float o.depth -. (if o.chosen then 1. else 0.) in
            (* the selected object stands in the root's indent with a chevron of its own *)
            if o.chosen && wide then Ui.Paint.chevron paint ~at:(x +. 15., ry +. (rh /. 2.)) `Down theme.foreground;
            let right = x +. w -. (if o.chosen then 7. else 12.) in
            let flag_x column = right -. 12. -. (float (1 - column) *. 20.) in
            Ui.Paint.cap paint ~at:(ox, Pxui_shell.Kit.cap_y ui ry rh)
              ~color:(if o.chosen then theme.foreground else muted) o.letter;
            let cy = ry +. (rh /. 2.) in
            (* a flag is a 12-point box with its 1-point line and a 6-point mark inset 2 inside it: the render camera's is the accent *)
            Option.iter (fun on ->
              let fx = flag_x 0 in
              (* a stroke is centred on its edge: inset half a point to keep the box 12 points *)
              Ui.Paint.fill paint ~x:fx ~y:(cy -. 6.) ~w:12. ~h:12. theme.input;
              Ui.Paint.stroke paint ~x:(fx +. 0.5) ~y:(cy -. 5.5) ~w:11. ~h:11. (Pxui.Theme.border theme);
              if on then Ui.Paint.fill paint ~x:(fx +. 3.) ~y:(cy -. 3.) ~w:6. ~h:6. muted) o.visible;
            Option.iter (fun on ->
              let fx = flag_x 1 +. 6. in
              Ui.Paint.circle paint ~at:(fx, cy) ~radius:5.5 ~fill:theme.input ~stroke:(Pxui.Theme.border theme) ();
              if on then Ui.Paint.circle paint ~at:(fx, cy) ~radius:3. ~fill:(if o.lead then theme.accent else muted) ()) o.render;
            let edge = flag_x 0 -. 8. in
            let dw = Ui.Paint.text_width paint o.detail in
            let name_x = ox +. Ui.Paint.cap_width paint o.letter +. 8. in
            let name = Ui.ellipsis ~width:(Ui.Paint.text_width paint) ~limit:(edge -. name_x) o.name in
            text (name_x, 0.) name;
            if wide && name_x +. Ui.Paint.text_width paint name +. 8. < edge -. dw then
              Ui.Paint.text paint ~at:(edge -. dw, ty) ~color:muted o.detail
        | Root_row { detail; folded; active; _ } ->
            shade active;
            (* the chevron opens or folds the objects; wide only, where the sheet draws it *)
            if wide then Ui.Paint.chevron paint ~at:(x +. 15., ry +. (rh /. 2.)) (if folded then `Right else `Down) theme.foreground;
            let ox = x +. (if wide then 26. else 12.) in
            Ui.Paint.cap paint ~at:(ox, Pxui_shell.Kit.cap_y ui ry rh) ~color:muted "R";
            let name_x = ox +. Ui.Paint.cap_width paint "R" +. 8. in
            text (name_x, 0.) "root";
            if wide then begin
              let edge = flag_x 0 -. 8. in
              let dw = Ui.Paint.text_width paint detail in
              Ui.Paint.text paint ~at:(edge -. dw, ty) ~color:muted detail
            end
        | Layout_row { index; label; active } ->
            shade false;
            (* its key as the kit's key text, then the name 8 points on *)
            let small = Pxui_shell.Kit.cap_size ui in
            let key = string_of_int index in
            Ui.Paint.text paint ~size:small ~at:(x +. 12., Pxui_shell.Kit.cap_y ui ry rh) ~color:(Pxui.Theme.ink_3 theme) key;
            let name_x = x +. 12. +. Ui.Paint.text_width paint ~size:small key +. 8. in
            text (name_x, 0.) (Ui.ellipsis ~width:(Ui.Paint.text_width paint) ~limit:(x +. w -. 38. -. name_x) label);
            if active then Ui.Paint.fill paint ~x:(x +. w -. 18.) ~y:(ry +. (rh /. 2.) -. 3.) ~w:6. ~h:6. theme.accent
        | Input_row { name; _ } ->
            text (x +. 12., 0.) ~color:muted (Ui.ellipsis ~width:(Ui.Paint.text_width paint) ~limit:label_w name)
        | Graph_row { label; context; detail; active; chip; dim; used; graph } ->
            shade active;
            if wide && active && p.scope <> None then
              Ui.Paint.chevron paint ~at:(x +. 5., ry +. (rh /. 2.)) (if List.mem graph state.opened then `Down else `Right) ink_3;
            (match chip with
             | Some c ->
                 Ui.Paint.fill paint ~x:(x +. 12.) ~y:(ry +. (rh /. 2.) -. 6.) ~w:12. ~h:12. c;
                 Ui.Paint.stroke paint ~x:(x +. 12.5) ~y:(ry +. (rh /. 2.) -. 5.5) ~w:11. ~h:11. (Pxui.Theme.edge theme)
             | None when String.starts_with ~prefix:"def:" graph ->
                 (* a function: the diamond of its port, a 7-point square turned 45 degrees (10 across),
                    in half-point rows *)
                 let cx = x +. 16. and cy = ry +. (rh /. 2.) in
                 for k = 0 to 19 do
                   let off = (float k +. 0.5) *. 0.5 -. 5. in
                   let half = 5. -. Float.abs off in
                   Ui.Paint.fill paint ~x:(cx -. half) ~y:(cy +. off -. 0.25) ~w:(2. *. half) ~h:0.5
                     (Pxui.Theme.ports theme).fn
                 done
             | None -> square ~at:(x +. 12.) (context_color theme context));
            let from = x +. (if chip <> None then 32. else if String.starts_with ~prefix:"def:" graph then 27. else 28.) in
            let color = if dim then ink_3 else theme.foreground in
            (match used with
             | Some n ->
                 let count = string_of_int n in
                 (* a count is ink on the wide sheet, ink-2 on the narrow one *)
                 Ui.Paint.text paint ~at:(x +. w -. 12. -. Ui.Paint.text_width paint count, ty)
                   ~color:(if wide then color else if dim then ink_3 else muted) count;
                 let edge = x +. w -. 40. in
                 let dw = Ui.Paint.text_width paint detail in
                 let name = Ui.ellipsis ~width:(Ui.Paint.text_width paint) ~limit:(edge -. from) label in
                 text ~color (from, 0.) name;
                 if wide && from +. Ui.Paint.text_width paint name +. 8. < edge -. dw then
                   Ui.Paint.text paint ~at:(edge -. dw, ty) ~color:(if dim then ink_3 else muted) detail
             | None -> labelled ~color from label (if wide then detail else ""))
        | Node_row { depth; label; detail; zone; ty; result; selected; _ } ->
            shade ~selected false;
            let nx = x +. 28. +. 12. *. float depth in
            (match zone with
             | Some glyph -> labelled (tag ~at:nx glyph) label detail
             | None ->
                 square ~at:nx (type_color theme ty);
                 labelled ~color:(if result then muted else theme.foreground) (nx +. 16.) label detail)
        | Macro_row (name, uses) ->
            shade false;
            text (x +. 12., 0.) ~color:muted "\xce\xbb";
            labelled (x +. 28.) name (plural uses "use")
        | Link_row { label; graph; _ } ->
            shade false;
            text (x +. 12., 0.) ~color:muted label;
            text (x +. 20. +. label_w, 0.) ~color:theme.foreground graph
        | Empty s -> text (x +. 12., 0.) ~color:ink_3 s
      end) rows);
  ()

(* the wide sheet's bar of its keys and the graph count *)
let paint_footer ui ~(theme : Pxui.Theme.t) ~muted ~rh ~x ~y ~w ~h ~footer p =
  if footer > 0. then
    Ui.draw ui (Ui.box ui ~w:(Ui.Px w) ~h:(Ui.Px footer) ~at:(x, y +. h -. footer) "navigator-footer")
      (fun paint (fx, fy, fw, _) ->
        Ui.Paint.fill paint ~x:fx ~y:fy ~w:fw ~h:footer theme.panel;
        Ui.Paint.fill paint ~x:fx ~y:fy ~w:fw ~h:1. (Pxui.Theme.edge theme);
        let small = Pxui_shell.Kit.cap_size ui in
        let tx = ref (fx +. 12.) in
        (* each key in ink-3 at the label size, what it does in ink-2, 8 apart *)
        List.iter (fun (key, what) ->
          Ui.Paint.text paint ~size:small ~at:(!tx, Pxui_shell.Kit.cap_y ui (fy +. 1.) rh) ~color:(Pxui.Theme.ink_3 theme) key;
          tx := !tx +. Ui.Paint.text_width paint ~size:small key +. 8.;
          Ui.Paint.text paint ~at:(!tx, Pxui_shell.Kit.text_y ui (fy +. 1.) rh) ~color:muted what;
          tx := !tx +. Ui.Paint.text_width paint what +. 8.)
          [ "s", "filter"; "i", "enter"; "/ j", "jump" ];
        let count = plural (List.length p.workspace.graphs) "graph" in
        Ui.Paint.cap paint ~at:(fx +. fw -. 12. -. Ui.Paint.cap_width paint count, Pxui_shell.Kit.cap_y ui (fy +. 1.) rh) count)

let paint_thumb ui ~theme ~total ~rh box =
  (* the scroll thumb: 4 points wide, 2 from the edge, line-3, over a track 6 points in from both ends *)
  Ui.draw_over ui box (fun paint (bx, by, bw, bh) ->
    let content = total +. rh in
    if content > bh then begin
      let track = Float.max 1. (bh -. 12.) in
      let thumb = Float.min track (Float.max 20. (track *. bh /. content)) in
      let scroll = Ui.scroll_position ui box in
      let y = by +. 6. +. ((track -. thumb) *. Float.min 1. (Float.max 0. (scroll /. Float.max 1. (content -. bh)))) in
      Ui.Paint.fill paint ~x:(bx +. bw -. 6.) ~y ~w:4. ~h:thumb (Pxui.Theme.border theme)
    end)

let input_sliders ui ~emit ~rows ~tops ~top ~scroll ~rh ~body ~x ~w ~label_w ~(signal : Ui.signal) ~row_at =
  (* the sliders of the inputs, over their rows: a drag scrubs and Option-click types, as every
     numeric field of the kit; a double click on the row's name types too, as an inspector row's
     label does (the field itself keeps its drag) *)
  let typed_row = if signal.double_clicked then row_at signal.press_point else None in
  Array.iteri (fun k row -> match row with
    | Input_row { graph; name; integer; value } ->
        let ry = top +. tops.(k) -. scroll in
        if ry >= top && ry +. rh <= top +. body then begin
          let show v = if integer then string_of_int (int_of_float (Float.round v)) else Printf.sprintf "%.3g" v in
          (* a scrub from the value at the press, like a card's field: 1 per 6 points for an
             integer, a hundredth of the magnitude per point for a float, Shift is finer *)
          let scrub origin dx shift = match float_of_string_opt origin with
            | None -> origin
            | Some o when integer -> show (o +. Float.round (dx /. (if shift then 24. else 6.)))
            | Some o -> show (o +. dx *. 0.01 *. Float.max 1. (Float.abs o) *. (if shift then 0.1 else 1.)) in
          let text = show value in
          let changed, _ = Ui.value_field ui ~at:(x +. 20. +. label_w, ry +. 2.) ~w:(w -. 32. -. label_w) ~h:20.
              ~scrub ~edit:(typed_row = Some k)
              ~valid:(fun t -> match float_of_string_opt t with
                | Some v -> Float.is_finite v && (not integer || Float.is_integer v) | None -> false)
              ("navigator-input-" ^ graph ^ "-" ^ name) text in
          if changed <> text then
            Option.iter (fun v -> emit (Set_default { graph; input = name; value = v; integer }))
              (float_of_string_opt changed)
        end
    | _ -> ()) rows

let view state ui ~bounds:(x, y, w, h) p =
  let theme = Ui.theme ui in
  let rh = float (Ui.row_height ui) in
  let x = float x and y = float y and w = float w and h = float h in
  let muted = Pxui.Theme.muted theme in
  (* the search field above the list *)
  let was_typing = state.typing in
  (* the wide sheet's field leaves room for the add button; the narrow one has no button *)
  let field_w = if w >= 300. then w -. 40. else w -. 16. in
  (* F2 puts the graph's name in this field instead of the search *)
  let state, renamed = match state.rename with
    | Some (graph, seen) ->
        let text, open_ = Ui.value_field ui ~at:(x +. 8., y +. 6.) ~w:field_w ~h:20.
            ~left:true ~edit:(not seen) ~valid:Flow.Symbol.valid_name "navigator-rename" graph in
        if text <> graph then { state with rename = None }, [ Rename { graph; to_ = text } ]
        else if open_ then { state with rename = Some (graph, true) }, []
        else { state with rename = (if seen then None else state.rename) }, []
    | None ->
        let query, typing = Ui.value_field ui ~at:(x +. 8., y +. 6.) ~w:field_w ~h:20.
            ~left:true ~valid:(fun _ -> true) "navigator-search" state.query in
        { state with query; typing }, [] in
  let query = state.query and typing = state.typing in
  if query = "" && not typing && state.rename = None then
    Ui.draw ui (Ui.box ui ~flags:Ui.clip ~w:(Ui.Px (field_w -. 4.)) ~h:(Ui.Px 20.) ~at:(x +. 10., y +. 6.) "navigator-placeholder")
      (fun paint (px, py, pw, h) ->
        let ty = Pxui_shell.Kit.text_y ui py h in
        Ui.Paint.text paint ~at:(px, ty) ~color:muted "/";
        Ui.Paint.text paint ~at:(px +. 12.5, ty) ~color:(Pxui.Theme.ink_3 theme)
          (Ui.ellipsis ~width:(Ui.Paint.text_width paint) ~limit:(pw -. 12.5)
             (if w >= 300. then "go to node, graph, function" else "go to")));
  let rows = rows ~wide:(w >= 300.) state p in
  let tops = tops ~rh rows in
  let total = tops.(Array.length rows) in
  let top = y +. search_height in
  (* the wide sheet closes with a hairline and a 24-point bar of its keys and the graph count *)
  let footer = if w >= 300. then rh +. 1. else 0. in
  let body = Float.max rh (h -. search_height -. footer) in
  let box = Ui.box ui ~flags:Ui.(clickable + scroll + clip + blocking)
      ~w:(Ui.Px w) ~h:(Ui.Px body) ~at:(x, top) ~scroll_step:rh "navigator-list" in
  let signal = Ui.signal ui box in
  (* the painted position, elastic edge movement included: a click lands on the row it sees *)
  let scroll = Ui.scroll_position ui box in
  (* the rows paint in the child so the list's clip holds them: a box's own painter is not clipped *)
  let content = Ui.within ui box (fun () ->
    Ui.box ui ~w:(Ui.Px w) ~h:(Ui.Px (total +. rh)) "navigator-content") in
  let row_at (_, py) =
    let off = py -. top +. scroll in
    if py < top || off >= total then None
    else begin
      let k = ref 0 in
      while tops.(!k + 1) <= off do incr k done;
      Some !k
    end in
  let intents = ref (List.rev renamed) in
  let emit i = intents := i :: !intents in
  (* the two sheets of the kit: a column under 300 points (the workspace) drops the details and
     keeps 56 points for an input's label, a wide one (the outline sheet) shows them and keeps 112 *)
  let wide = w >= 300. in
  let label_w = if wide then 112. else 56. in
  (* add an object, a graph, a node: the add menu *)
  if w >= 300. && Pxui_shell.Kit.button ui ~key:"navigator-add" ~at:(x +. w -. 28., y +. 6.) ~w:20. ~centered:true "+" then emit Add;
  let fold = ref false in
  let toggle = ref None in
  let begin_rename = ref None in
  (* a material or a SOP graph is a source: pressed and moved 4 points it is carried, as the
     Flow value that reads it *)
  if signal.held then
    (match Option.map (fun k -> rows.(k)) (row_at signal.press_point) with
     | Some (Root_row { graph; _ }) ->
         Ui.carry ui ~from:box ~kind:"scene" ~value:("(ref " ^ graph ^ ")") ()
     | Some (Graph_row { graph; context = Some context; _ }) ->
         Ui.carry ui ~from:box ~kind:(Flow.Context.name context)
           ~value:("(ref " ^ graph ^ ")") ()
     | _ -> ());
  let put = match Ui.drop_target ui box with Some (Ui.Dropped _) -> true | _ -> false in
  (if signal.clicked && not put then match Option.map (fun k -> rows.(k)) (row_at signal.release_point) with
   | Some (Graph_row { graph; active; _ }) ->
       (* the chevron of the open graph's row folds its node rows; the rest of the row opens it *)
       if wide && active && p.scope <> None && fst signal.release_point < x +. 10. then toggle := Some graph
       else emit (Open { graph; node = None })
   | Some (Root_row { graph; _ }) ->
       (* the chevron folds the scene's objects; the rest of the row opens the scene graph *)
       if wide && fst signal.release_point < x +. 24. then fold := true else emit (Open { graph; node = None })
   | Some (Node_row { graph; path; _ }) -> emit (Open { graph; node = Some path })
   | Some (Link_row { graph; _ }) -> emit (Open { graph; node = None })
   | Some (Macro_row (name, _)) -> emit (Macro name)
   | Some (Object_row ({ home = Some (graph :: _ as path); _ } as o)) ->
       (* a press on a flag toggles it in the text; anywhere else on the row opens the object *)
       let px = fst signal.release_point in
       let flag column = let fx = x +. w -. 12. -. 12. -. (float (1 - column) *. 20.) in px >= fx -. 4. && px < fx +. 16. in
       (match o.visible, o.render with
        | Some on, _ when flag 0 && not o.inert -> emit (Flag { node = path; name = "visible"; value = not on })
        | _, Some on when flag 1 && not o.inert -> emit (Flag { node = path; name = "render"; value = not on })
        | _ -> emit (Open { graph; node = Some path }))
   | Some (Layout_row { index; _ }) -> emit (Layout index)
   | _ -> ());
  if (typing || was_typing) && Ui.key_pressed ui Input.Enter then
    (match List.find_map (function
       | Graph_row { graph; _ } -> Some (Open { graph; node = None })
       | Node_row { graph; path; _ } -> Some (Open { graph; node = Some path })
       | _ -> None) (Array.to_list rows) with
     | Some intent -> emit intent | None -> ());
  let hovered = if signal.hovered then row_at signal.pointer else None in
  (* F2 renames and Delete removes the graph under the pointer (a refused removal says who reads it) *)
  (match Option.map (fun k -> rows.(k)) hovered with
   | Some (Graph_row { graph; _ } | Root_row { graph; _ }) when not (String.starts_with ~prefix:"def:" graph)
       && not typing && state.rename = None ->
       if Ui.key_pressed ui Input.F2 then begin_rename := Some (graph, false)
       else if Ui.key_pressed ui Input.Delete then emit (Remove graph)
   | _ -> ());
  (* a right-click opens the context menu: New graph (a context each), and on a graph row Rename
     and Delete.  The row's own graph is kept with the menu; a function row has neither. *)
  let context = ref state.context in
  if Ui.context_clicked signal then begin
    let cx, cy = signal.release_point in
    let graph = match Option.map (fun k -> rows.(k)) (row_at signal.release_point) with
      | Some (Graph_row { graph; _ } | Root_row { graph; _ }) -> Some graph
      | _ -> None in
    context := Some (cx, cy, graph)
  end;
  (match !context with
   | None -> ()
   | Some (cx, cy, graph) ->
       let editable = match graph with Some g -> not (String.starts_with ~prefix:"def:" g) | None -> false in
       let items = [ "New graph", true; "Rename", editable; "Delete", editable ] in
       let submenu = Ui.{ row = 0; rows = List.map (fun c -> c, true) new_graph_contexts; keys = []; current = None } in
       (match Ui.context_menu ui ~at:(cx, cy) ~danger:[ 2 ] ~submenus:[ submenu ] "navigator-context" items with
        | `Open -> ()
        | `Dismiss -> context := None
        | `Pick n ->
            context := None;
            (match n, graph with
             | 1, Some g -> begin_rename := Some (g, false)
             | 2, Some g -> emit (Remove g)
             | n, _ when n >= 3 -> emit (New_graph (List.nth new_graph_contexts (n - 3)))
             | _ -> ())));
  paint_rows ~content ~box ~x ~top ~w ~body ~theme ~tops ~rh ~hovered ~muted ~wide ~label_w ~p ~state ~rows ui;
  paint_footer ui ~theme ~muted ~rh ~x ~y ~w ~h ~footer p;
  paint_thumb ui ~theme ~total ~rh box;
  input_sliders ui ~emit ~rows ~tops ~top ~scroll ~rh ~body ~x ~w ~label_w ~signal ~row_at;
  let state = match !toggle with
    | Some graph -> { state with opened = (if List.mem graph state.opened
                                           then List.filter (( <> ) graph) state.opened else graph :: state.opened) }
    | None -> state in
  { state with context = !context; rename = (if !begin_rename <> None then !begin_rename else state.rename);
               scene_closed = (if !fold then not state.scene_closed else state.scene_closed) }, List.rev !intents
