open Core_model
open Sop
open Editor_document
open Core_shell

(* ---- levels ---- *)

let scene value = Document.scene_graph value.doc

(* Membership is shared by drawing, lighting, picking and framing. *)
let view_wants value view id =
  let views = match value.doc.Document.shell with Some s -> s.views | None -> [] in
  match view with
    | `All -> true
    | `Primary -> not (List.exists (fun (_, ids) -> List.mem id ids) views)
    | `Only key -> (match List.assoc_opt key views with Some ids -> List.mem id ids | None -> false)

let kind value id = Option.map Node.operation (Edit_graph.find (scene value) ~node_id:id)

let level_key = function
  | Document.Scene -> "scene"
  | Inside id -> "object:" ^ string_of_int id

(* The kinds the node menu offers where the pane shows a graph of [context]. *)
let catalog value context =
  if context = Flow.Context.scene then Objects.catalog
  else if context = Flow.Context.world then Layers.catalog
  else if context = Flow.Context.sop then value.factories @ (snd value.doc.workspace).factories else []

(* [catalog] as descriptors, plus the library nodes this document does not use yet: a workspace
   lowers only the library definitions it reaches. *)
let kinds value context =
  let present = List.map Flow_sop.Catalog.descriptor (catalog value context) in
  if context = Flow.Context.sop then Flow_sop.Catalog.with_library present else present

(* the open level's network; the scene's if the level is gone (every path resolves the level first) *)
let network value =
  Option.value ~default:value.doc.Document.scene (Document.network value.doc value.level)
let document value = (network value).graph.geometry

let viewed_path value (scope : Flow_graph.Projection.scope) =
  let valid (preview : preview) = match preview.path, scope.path with
    | graph :: _, root :: _ when graph = root -> Some preview.path
    | _ -> None in
  let preview = match value.level with
    | Inside id when Document.object_graph value.doc id = Some (List.hd scope.path) ->
        Option.bind (Document.Int_map.find_opt id value.previews) valid
    | _ -> Document.Int_map.fold (fun _ preview found ->
        match found with Some _ -> found | None -> valid preview) value.previews None in
  let preview = match preview, value.canvas_preview with
    | None, Some canvas -> valid canvas
    | _ -> preview in
  match preview with
  | Some _ -> preview
  | None -> match scope.result with
    | Link target -> Some [ List.hd scope.path; target ]
    | Node path -> Some path
    | Literal _ -> None

(* A named pane or Navigator can show a SOP graph while the scene list is open. Its
   compiled nodes belong to an object's network, independent of the list's level. *)
let node_owner value node_id =
  let owns (network : Document.network) = Edit_graph.find network.graph.geometry ~node_id <> None in
  match value.level with
  | Inside id when Option.fold ~none:false ~some:owns (Document.Int_map.find_opt id value.doc.networks) -> Some id
  | _ ->
      (* ponytail: scan owners on this fallback; add an ownership index if large scenes need it. *)
      Document.Int_map.fold (fun id network found ->
        match found with Some _ -> found | None -> if owns network then Some id else None)
        value.doc.networks None

let compiled_node value node_id =
  let owned = Option.bind (node_owner value node_id) (fun id ->
    let graph = match Cook.applied value.cook id with
      | Some resolved -> resolved.Flow_sop.Value_lane.geometry
      | None -> (Document.Int_map.find id value.doc.networks).graph.geometry in
    Edit_graph.find graph ~node_id) in
  match owned with
  | Some _ -> owned
  | None -> List.find_map (fun (g : Flow_sop.Lower.graph) -> Edit_graph.find g.network.geometry ~node_id)
      (snd value.doc.workspace).graphs

(* A new material graph: a free name and its Set_graph ([Material] in the add menu, "new material"
   in the inspector). *)
let new_material value =
  let ws, _ = value.doc.Document.workspace in
  let taken n = List.exists (fun (g : Flow.Workspace.graph) -> g.name = n) (ws.checked.graphs @ ws.checked.defs) in
  let rec pick i = let n = if i = 1 then "material" else "material_" ^ string_of_int i in
    if taken n then pick (i + 1) else n in
  let name = pick 1 in
  let module S = Flow.Syntax in
  let list items = S.make (S.List items) and sym s = S.make (S.Sym s) and kw k = S.make (S.Kw k) in
  (* (graph name :context material (material/standard :name "name" :color "#cccccc" :roughness 0.4)) *)
  let form = list [ sym "graph"; sym name; kw "context"; sym "material";
    list [ sym "material/standard"; kw "name"; S.make (S.Str name); kw "color"; S.make (S.Str "#cccccc");
           kw "roughness"; S.make (S.Num "0.4") ] ] in
  name, Flow_graph.Flow_edit.Set_graph { name; form }

(* A new graph of a context, from the Navigator's context menu or the add menu: a fresh name
   (the context's, then [_2] ...) and a body that checks and cooks.  A scene reads the first SOP
   graph, if any, and has a camera and a light, so it renders in a viewport. *)
let new_graph value context =
  let ws, _ = value.doc.Document.workspace in
  let taken n = List.exists (fun (g : Flow.Workspace.graph) -> g.name = n) (ws.checked.graphs @ ws.checked.defs) in
  let rec pick i = let n = if i = 1 then context else context ^ "_" ^ string_of_int i in
    if taken n then pick (i + 1) else n in
  let name = pick 1 in
  let module S = Flow.Syntax in
  let list items = S.make (S.List items) and sym s = S.make (S.Sym s) and kw k = S.make (S.Kw k)
  and num n = S.make (S.Num n) and str s = S.make (S.Str s) in
  let vec items = S.make (S.Vec (List.map num items)) in
  let call head args = list (sym head :: args) in
  let body = match context with
    | "sop" -> call "sop/box" []
    | "draw" -> call "draw/background" [ str "#111318" ]
    | "image" -> call "image/noise" [ kw "width"; num "256"; kw "height"; num "256";
                                      kw "frequency"; num "0.03"; kw "seed"; num "1" ]
    | "value" -> num "1"
    | "material" -> call "material/standard" [ kw "name"; str name; kw "color"; str "#cccccc";
                                              kw "roughness"; num "0.4" ]
    | _ ->
        let geometry = List.find_map (fun (g : Flow.Workspace.graph) ->
          if g.context = Flow.Context.sop then Some g.name else None) ws.checked.graphs in
        let bindings = (match geometry with
          | Some g -> [ "geometry", call "scene/geometry" [ list [ sym "ref"; sym g ] ] ] | None -> [])
          @ [ "camera", call "scene/camera" [ kw "eye"; vec [ "4"; "5"; "5" ]; kw "target"; vec [ "0"; "0"; "0" ] ];
              "light", call "scene/light" [ kw "translate"; vec [ "0"; "5"; "3" ]; kw "intensity"; num "50" ] ] in
        list [ sym "let*"; S.make (S.Vec (List.concat_map (fun (n, e) -> [ sym n; e ]) bindings));
               call "scene/root" [ call "scene/merge" (List.map (fun (n, _) -> sym n) bindings);
                                   kw "camera"; sym "camera" ] ] in
  name, Flow_graph.Flow_edit.Set_graph { name; form = list [ sym "graph"; sym name; kw "context"; sym context; body ] }

(* "=(* 2 t)" typed in a row: the expression after the "=", any Lisp expression *)
let expression_text text =
  match Flow.Syntax.parse (String.sub text 1 (String.length text - 1)) with
  | Ok [ expr ] -> Ok expr
  | Ok _ -> Error "Write one expression after the ="
  | Error d -> Error ("Expression: " ^ d.Diagnostic.message)

(* 1204 as "1 204" *)
let group_digits n =
  let text = string_of_int (abs n) in
  let len = String.length text in
  let b = Buffer.create (len + 4) in
  if n < 0 then Buffer.add_char b '-';
  String.iteri (fun i c -> if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char b ' '; Buffer.add_char b c) text;
  Buffer.contents b

(* the section of a kind's arguments that have no folder: its own name ("sop/copy_to_points" is Copy to points) *)
let kind_label head =
  let leaf = match String.rindex_opt head '/' with
    | Some i -> String.sub head (i + 1) (String.length head - i - 1) | None -> head in
  String.capitalize_ascii (String.map (fun c -> if c = '_' then ' ' else c) leaf)

(* The arguments of a kind the editor has no compiled node for (a scene or material node), as the
   fields a node would give: the checked kind's parameters, valued by what is written in [authored]. *)
let kind_fields value graph head authored =
  let module S = Flow.Syntax in
  let ws, _ = value.doc.Document.workspace in
  let context = Option.map (fun (g : Flow.Workspace.graph) -> g.context)
    (List.find_opt (fun (g : Flow.Workspace.graph) -> g.name = graph)
      (ws.checked.graphs @ ws.checked.defs)) in
  let declaration = List.find_opt (fun (node : Flow.Workspace.node_definition) ->
    node.kind.qualified = head) ws.checked.nodes in
  match Lazy.force value.flow_catalog, context with
  | _ when head = "material/standard" ->
      (* a built-in of the workspace, not a catalog kind: its four keywords *)
      let number (e : S.t) = match e.node with S.Num t -> float_of_string_opt t | _ -> None in
      let field name default view =
        let current = match authored name, default with
          | Some { S.node = S.Str t; _ }, Param.Text_value _ -> Param.Text_value t
          | Some e, Param.Float_value _ -> (match number e with Some f -> Param.Float_value f | None -> default)
          | _ -> default in
        { Param.name; label = name; description = None; folder = []; impact = Param.Cook; primary = false;
          unit = None; vec3 = None; kind = view; default; current } in
      Some [ field "name" (Param.Text_value "") Param.Text_view;
             field "color" (Param.Text_value "#cccccc") Param.Text_view;
             field "roughness" (Param.Float_value 0.5)
               (Param.Floating_view { soft_min = 0.; soft_max = 1.; hard_min = Some 0.; hard_max = Some 1. });
             field "emission" (Param.Text_value "#000000") Param.Text_view ]
  | Some catalog, Some context ->
      (match (match declaration with
         | Some node when node.kind.context = context -> Ok node.kind
         | _ -> Flow.Check.resolve_kind catalog context head) with
       | Error _ -> None
       | Ok (k : Flow.Check.kind) ->
           let number (e : S.t) = match e.node with S.Num t -> float_of_string_opt t | _ -> None in
           Some (List.concat_map (fun (p : Flow.Check.parameter) ->
             let written = authored p.name in
             let three = List.length p.fields = 3 && p.ty = Some Flow.Port_type.Vec3 in
             List.mapi (fun i (name, view, default) ->
               let current = match written, default with
                 | Some { S.node = S.Vec [ a; b; c ]; _ }, Param.Float_value _ when three ->
                     (match number (List.nth [ a; b; c ] i) with Some f -> Param.Float_value f | None -> default)
                 | Some e, Param.Float_value _ when not three ->
                     (match number e with Some f -> Param.Float_value f | None -> default)
                 | Some { S.node = S.Num t; _ }, Param.Int_value _ ->
                     (match int_of_string_opt t with Some n -> Param.Int_value n | None -> default)
                 | Some { S.node = S.Str t; _ }, Param.Choice_value _ -> Param.Choice_value t
                 | Some { S.node = S.Str t; _ }, Param.Text_value _ -> Param.Text_value t
                 | Some { S.node = S.Sym ("true" | "false" as b); _ }, Param.Bool_value _ ->
                     Param.Bool_value (b = "true")
                 | _ -> default in
               let field = { Param.name; label = (if three then name else p.label); description = None; folder = p.folder;
                 impact = Param.Cook; primary = false; unit = (if three then None else p.unit);
                 vec3 = (if three then Some (p.name, i) else None); kind = view; default; current } in
               match Option.bind declaration (fun node ->
                 List.find_opt (fun (field : Param.field_view) -> field.name = name) node.fields) with
               | Some field -> {field with current}
               | None -> field)
               p.fields) k.parameters))
  | _ -> None

(* The iterations the zones around a node of the pane's graph probe, outermost first, and the
   compiled node it lowers to there.  [chains] is {!Flow_graph.Probe.chains} of the scope, made once
   by the caller (it walks the whole scope). *)
let probes_of value chains path =
  List.map (fun zone -> Option.value ~default:0 (Layout_by_path.Path_map.find_opt zone value.probes))
    (Option.value ~default:[] (Hashtbl.find_opt chains path))

let compiled_at value chains records path =
  Option.bind (Flow_graph.Probe.plan_node records path ~probes:(probes_of value chains path)) (fun plan ->
    Flow_sop.Network.Int_map.find_opt plan (snd value.doc.Document.workspace).compiled)

(* A nested operator call in a wired row (the call `* 0.2 (sin ...)` in row b): one sub-row per leaf of its
   tree, addressed by [Set_arg]'s [sub] (child indices; the head is child 0).  Depth 4, 12 rows.  They are
   the body of the inspector of a node under its row, and of the row itself (a click on its chip). *)
let arg_prefix = "@arg:"

let operator_call (e : Flow.Syntax.t) =
  let module S = Flow.Syntax in
  match e.node with
  | S.List ({ S.node = S.Sym h; _ } :: _ :: _) -> not (String.contains h '/') | _ -> false

let sub_entries ~shown (r : Flow_graph.Projection.row) =
  let module S = Flow.Syntax in
  match r.expr with
  | Some e when operator_call e ->
      let count = ref 0 in
      let rec walk chain depth sub (e : S.t) = match e.node with
        | S.List ({ S.node = S.Sym h; _ } :: _) when depth < 4 ->
            List.concat (List.mapi (fun i (c : S.t) ->
              if i = 0 then [] else
              let sub = sub @ [ i ] in
              match c.node with
              | S.List ({ S.node = S.Sym h'; _ } :: _ :: _) when not (String.contains h' '/') ->
                  walk (chain @ [ h ]) (depth + 1) sub c
              | _ when !count >= 12 -> []
              | _ ->
                  incr count;
                  let label = r.label ^ "." ^ String.concat "." (List.map (fun k -> string_of_int (k - 1)) sub) in
                  let base = Printf.sprintf "%s%s@%s" arg_prefix r.label (String.concat "." (List.map string_of_int sub)) in
                  let field ?vec3 ?(suffix = "") kind current =
                    { Param.name = base ^ suffix; label = (if vec3 = None then label else suffix);
                      description = None; folder = []; impact = Param.Cook; primary = false;
                      unit = None; vec3; kind; default = current; current } in
                  let range f = let m = Float.max 1. (Float.abs f *. 2.) in
                    { Param.soft_min = -. m; soft_max = m; hard_min = None; hard_max = None } in
                  let make fields drive =
                    [ base, r, sub, { Pxui_shell.Inspector.path = base; fields; shown;
                                      locked = false; drive; live = None } ] in
                  (match c.node with
                   | S.Num t ->
                       (match int_of_string_opt t, float_of_string_opt t with
                        | Some i, _ ->
                            make [ field (Param.Integer_view { Param.soft_min = min 0 (2 * i);
                              soft_max = max 10 (2 * i); hard_min = None; hard_max = None }) (Param.Int_value i) ] None
                        | _, Some f -> make [ field (Param.Floating_view (range f)) (Param.Float_value f) ] None
                        | _ -> [])
                   | S.Str t -> make [ field Param.Text_view (Param.Text_value t) ] None
                   | S.Sym ("true" | "false" as b) -> make [ field Param.Toggle_view (Param.Bool_value (b = "true")) ] None
                   | S.Vec l when List.length l >= 2 && List.length l <= 4 && List.for_all (fun (x : S.t) -> match x.node with
                       | S.Num t -> float_of_string_opt t <> None | _ -> false) l ->
                       make (List.mapi (fun i (x : S.t) ->
                         let f = match x.node with S.Num t -> float_of_string t | _ -> 0. in
                         field ~vec3:(base, i) ~suffix:(List.nth [ ".x"; ".y"; ".z"; ".w" ] i)
                           (Param.Floating_view (range f)) (Param.Float_value f)) l) None
                   | S.Sym s -> make [ field Param.Text_view (Param.Text_value "") ] (Some s)
                   | _ -> make [ field Param.Text_view (Param.Text_value "") ] (Some ("=" ^ Flow.Lisp.flat c)))) (S.children e))
        | _ -> [] in
      walk [] 0 [] e
  | _ -> []

(* The inspector's subject among the pane's selected paths: a card selected together with one of its
   rows (the pane selects both on a click on an expression chip) is the row alone. *)
let inspector_paths paths =
  let holder p = match List.rev p with
    | last :: (_ :: _ as rest) when String.starts_with ~prefix:":" last -> Some (List.rev rest) | _ -> None in
  match paths with
  | [ a; b ] when holder b = Some a -> [ b ]
  | [ a; b ] when holder a = Some b -> [ a ]
  | paths -> paths

(* the row and sub-path of a field named [name] among [entries] (a vector's cell is its row's [.x] ...) *)
let arg_row_in entries name =
  let base = List.fold_left (fun b suffix ->
    if String.ends_with ~suffix b then String.sub b 0 (String.length b - 2) else b) name [ ".x"; ".y"; ".z"; ".w" ] in
  List.find_map (fun (b, (r : Flow_graph.Projection.row), sub, _) -> if b = base then Some (r, sub) else None) entries

(* the request of an edit of a sub-row or of an operator row ([arg_row] finds the row and sub-path of a field) *)
let follow_row source (n : Flow_graph.Projection.node) r =
  List.find_map (fun name -> Option.map (fun path -> Follow_source path)
    (Text_pane.reference_target source ~from:n.path name)) (Flow_graph.Projection.sources r)

let arg_request ~source (n : Flow_graph.Projection.node) ~arg_row = function
  | Pxui_shell.Inspector.Follow path ->
      Option.bind (arg_row path) (fun ((r : Flow_graph.Projection.row), sub) ->
        let expr = List.fold_left (fun expr i -> Option.bind expr (fun expr -> List.nth_opt (Flow.Syntax.children expr) i)) r.expr sub in
        follow_row source n {r with expr; chip = (if sub = [] then r.chip else Const)})
  | Pxui_shell.Inspector.Edited (name, edited) ->
      let module S = Flow.Syntax in
      Option.bind (arg_row name) (fun ((r : Flow_graph.Projection.row), sub) ->
        let component = if String.ends_with ~suffix:".x" name then Some 0
          else if String.ends_with ~suffix:".y" name then Some 1
          else if String.ends_with ~suffix:".z" name then Some 2 else None in
        let syntax = match edited with
          | Param.Float_value f -> Some (S.make (S.Num (Flow.Lisp.float f)))
          | Int_value i -> Some (S.make (S.Num (string_of_int i)))
          | Bool_value b -> Some (S.make (S.Sym (string_of_bool b)))
          | Text_value t | Choice_value t -> Some (S.make (S.Str t)) in
        Option.map (fun value ->
          Syntax_edit (Flow_graph.Flow_edit.Set_arg { node = n.path; key = r.key;
            sub = (match component with Some i -> sub @ [ i ] | None -> sub); value })) syntax)
  | Pxui_shell.Inspector.Expression (path, text) ->
      Option.bind (arg_row path) (fun ((r : Flow_graph.Projection.row), sub) ->
        match expression_text text with
        | Ok value -> Some (Syntax_edit (Flow_graph.Flow_edit.Set_arg { node = n.path; key = r.key; sub; value }))
        | Error message -> Some (Declined message))
  | _ -> None

(* The inspector of the node selected in the workspace pane: its
   value at the probe, whether it recooks every frame, the list of its
   iterations (a click moves the zone's probe), and the catalog parameters of
   the lowered node at that iteration.  An edit is a [Set_arg] on the authored
   argument; an argument that is not a literal shows its expression and is
   locked.  Returns the graph requests and the probe moves. *)
let spreadsheet_source value =
  match value.scope_key, Pxui_graph.Scope.selected value.scope_view with
  | Some {scope; records = Some records; _}, path :: _ ->
      let chains = Flow_graph.Probe.chains scope in
      Option.bind (compiled_at value chains records path) (fun node_id ->
        Option.bind (node_owner value node_id) (fun object_id -> Cook.source value.cook ~object_id ~node_id))
  | _ -> None

(* the iterations of the loop the node sits in, one button each *)
let iteration_picks ~(footer : Flow_graph.Probe.footer) ~chain ~probes ~(n : Flow_graph.Projection.node) ~records ui =
  let iterations = Flow_graph.Probe.iterations records n ~probes in
  let zone = List.nth_opt (List.rev chain) 0 in
  let picks = match zone with
    | Some z when Array.length iterations > 0 ->
        Pxui.Ui.inspector_message ui ~key:"ws-runs"
          (Printf.sprintf "Inside %s: runs %d times. %s" (List.nth z (List.length z - 1))
             (Option.value ~default:(Array.length iterations) footer.runs)
             (if n.invariant then "The same every time, so it can move out of the loop."
              else "It changes with the loop variable."));
        let current = List.nth probes (List.length probes - 1) in
        List.concat (List.init (min 64 (Array.length iterations)) (fun k ->
          if Pxui.Ui.inspector_button ui ~key:(Printf.sprintf "ws-iter-%d" k)
              (Printf.sprintf "%s%d  %s" (if k = current then "► " else "  ") (k + 1) iterations.(k))
          then [ Pxui_graph.Scope.Probe_set { zone = z; index = k } ] else []))
    | _ -> [] in
  picks

(* the note above the binding in the Lisp *)
let note_section ~width ~(n : Flow_graph.Projection.node) ui =
  (* the note above the binding in the Lisp: one line here, typing is one history entry;
     the last section, closed *)
  let note = if n.synthetic || Flow_graph.Projection.anonymous n then [] else begin
    let current = Option.value n.note ~default:"" in
    Option.value ~default:[] (Pxui.Ui.inspector_section ui ~key:"ws-note-section" "Note" (fun () ->
      if String.contains current '\n' then begin
        Pxui.Ui.inspector_message ui ~key:"ws-note-lines" "A note of several lines is edited in the text pane.";
        []
      end else begin
        let box, control_x, control_y, control_w = Pxui.Ui.inspector_row ui ~width ~key:"ws-note" ~label:"note" () in
        let text = Pxui.Ui.within ui box (fun () ->
          fst (Pxui.Ui.value_field ui ~at:(control_x, control_y) ~w:control_w ~h:20.
            ~left:true ~valid:(fun _ -> true) "ws-note-field" current)) in
        if text = current then []
        else [ Syntax_edit (Flow_graph.Flow_edit.Set_note { node = n.path; text }) ]
      end)) end in
  note

(* what the cook made of a geometry node *)
let output_section ~width ~geo ui =
  (* what the cook made of a geometry node: read-out rows, as the sheet's Output section *)
  (match geo with
   | Some (g : Flow_graph.Probe.geometry) ->
       ignore (Pxui.Ui.inspector_section ui ~key:"ws-output" ~expanded:true "Output" (fun () ->
         Pxui.Ui.inspector_readout ui ~width ~key:"ws-output-points" ~label:"points"
           (group_digits g.points);
         Pxui.Ui.inspector_readout ui ~width ~key:"ws-output-prims" ~label:"prims"
           (group_digits g.prims);
         Option.iter (fun (x, y, z) ->
           Pxui.Ui.inspector_readout ui ~width ~key:"ws-output-bounds" ~label:"bounds"
             (Printf.sprintf "%.1f \xc3\x97 %.1f \xc3\x97 %.1f" x y z)) g.extent;
         if g.groups <> [] then
           Pxui.Ui.inspector_readout ui ~width ~key:"ws-output-groups" ~label:"groups"
             (String.concat ", " g.groups);
         List.iteri (fun i (owner, name, kind, length) ->
           Pxui.Ui.inspector_readout ui ~width ~key:("ws-output-attr-" ^ string_of_int i)
             ~label:(owner ^ " " ^ name) (Printf.sprintf "%s · %s" kind (group_digits length))) g.attributes))
   | None -> ());
  ()

(* the intents the rows' widgets returned, as document changes *)
let field_edits ~on_choice ~width ~expanded ~parameters ~ref_rows ~row_of ~arg_row ~value ~size_names ~(n : Flow_graph.Projection.node) ~resized ~graph ~layout_names ~rows ui =
  let num f =
Flow.Syntax.make (Flow.Syntax.Num (Flow.Lisp.float f)) in
  let edits = if rows = [] then [] else
    Pxui_shell.Inspector.flow_fields ui ~expanded ~width ~pins:true ~pin_click:true ~on_choice
      ~kind_label:(kind_label n.head)
      ~chips:(match value.scope_key with
        | Some { evaluated = Some ev; _ } -> Navigator.chips ev
        | _ -> []) rows
    |> List.filter_map (function
      | Pxui_shell.Inspector.Edited ("@layout", Param.Choice_value chosen) ->
          Option.bind layout_names (fun (names, _) ->
            Option.map (fun index -> Syntax_edit (Flow_graph.Flow_edit.Set_layout { graph; index }))
              (Array.find_index (( = ) chosen) names))
      | Pxui_shell.Inspector.Edited ("@size", Param.Choice_value chosen) ->
          Option.map (fun index ->
            let how = List.nth [ `Ratio; `First; `Second ] index in
            (* a split that is not on screen has no sizes to keep: half, or 240 points *)
            let size = Option.value (resized n.path how) ~default:(match how with
              | `Ratio -> `Ratio 0.5 | `First -> `First 240 | `Second -> `Second 240) in
            Syntax_edit (Flow_graph.Flow_edit.Set_layout_size { node = n.path; size }))
            (Array.find_index (( = ) chosen) size_names)
      | (Pxui_shell.Inspector.Edited (name, _) | Pxui_shell.Inspector.Expression (name, _) | Pxui_shell.Inspector.Follow name) as ev
        when String.starts_with ~prefix:arg_prefix name -> arg_request ~source:(fst value.doc.workspace).source n ~arg_row ev
      | Pxui_shell.Inspector.Follow path ->
          let row = match row_of path with Some _ as row -> row | None ->
            List.find_map (fun suffix ->
              if String.ends_with ~suffix path then row_of (String.sub path 0 (String.length path - 2))
              else None) [".x"; ".y"; ".z"; ".w"] in
          Option.bind row (follow_row (fst value.doc.workspace).source n)
      | Pxui_shell.Inspector.Reset path when String.starts_with ~prefix:arg_prefix path -> None
      | Pxui_shell.Inspector.Edited (name, edited) ->
          (match List.find_opt (fun (r, _) -> ("@ref:" ^ r.Flow_graph.Projection.label) = name) ref_rows with
           | Some (r, _) ->
               (match edited with
                | Param.Choice_value "new material" ->
                    let name, make = new_material value in
                    Some (Syntax_batch ("New material", [ make; Flow_graph.Flow_edit.Set_arg
                      { node = n.path; key = r.key; sub = [];
                        value = Flow.Syntax.make (Flow.Syntax.List [ Flow.Syntax.make (Flow.Syntax.Sym "ref"); Flow.Syntax.make (Flow.Syntax.Sym name) ]) } ]))
                | Param.Choice_value chosen when chosen <> "(none)" && chosen <> "" ->
                    let syntax = Flow.Syntax.make (Flow.Syntax.List [ Flow.Syntax.make (Flow.Syntax.Sym "ref"); Flow.Syntax.make (Flow.Syntax.Sym chosen) ]) in
                    Some (Syntax_edit (Flow_graph.Flow_edit.Set_arg
                      { node = n.path; key = r.key; sub = []; value = syntax }))
                | _ -> None)
           | None ->
               List.find_map (fun (parameter : Flow_sop.Port.parameter) ->
                 match List.find_index (fun (f : Param.field_view) -> f.name = name) parameter.fields with
                 | None -> None
                 | Some index ->
                     let syntax = match edited with
                       | Param.Float_value f -> num f
                       | Int_value i -> Flow.Syntax.make (Flow.Syntax.Num (string_of_int i))
                       | Bool_value b -> Flow.Syntax.make (Flow.Syntax.Sym (string_of_bool b))
                       | Text_value t | Choice_value t -> Flow.Syntax.make (Flow.Syntax.Str t) in
                     let syntax = if List.length parameter.fields <> 3 then syntax else
                       Flow.Syntax.make (Flow.Syntax.Vec (List.mapi (fun i (f : Param.field_view) ->
                         if i = index then syntax else match f.current with
                           | Param.Float_value x -> num x
                           | Int_value x -> num (float x) | _ -> Flow.Syntax.make (Flow.Syntax.Num "0.0")) parameter.fields)) in
                     Some (Syntax_edit (Flow_graph.Flow_edit.Set_arg
                       { node = n.path; key = Flow_graph.Flow_edit.Kw parameter.path; sub = []; value = syntax })))
                 parameters)
      | Pxui_shell.Inspector.Pinned (path, want) ->
          (* the dot: on the card or not; the default rule's own answer is stored as nothing *)
          Option.map (fun (r : Flow_graph.Projection.row) ->
            Pin_row { node = n.path; label = r.label;
                      pin = if want = Flow_graph.Projection.row_shown r then None else Some want }) (row_of path)
      | Pxui_shell.Inspector.Expression (path, text) ->
          let key, sub = match String.index_opt path '.' with
            | Some i -> String.sub path 0 i, List.filter_map Fun.id [ List.assoc_opt
                (String.sub path (i + 1) (String.length path - i - 1)) [ "x", 0; "y", 1; "z", 2 ] ]
            | None -> path, [] in
          (match expression_text text with
           | Ok value ->
               Some (Syntax_edit (Flow_graph.Flow_edit.Set_arg { node = n.path;
                 key = Flow_graph.Flow_edit.Kw key; sub; value }))
           | Error message -> Some (Declined message))
      | Pxui_shell.Inspector.Reset path ->
          (match List.find_opt (fun (r, _) -> ("@ref:" ^ r.Flow_graph.Projection.label) = path) ref_rows with
           | Some (r, _) ->
               Some (Syntax_edit (Flow_graph.Flow_edit.Set_arg
                 { node = n.path; key = r.key; sub = []; value = Flow.Syntax.make (Flow.Syntax.Sym "nil") }))
           | None ->
               Some (Syntax_edit (Flow_graph.Flow_edit.Disconnect { node = n.path;
                 key = Flow_graph.Flow_edit.Kw (List.hd (String.split_on_char '.' path)); fallback = None })))) in
  edits

(* a value operator's inputs as rows *)
let operator_rows ~(n : Flow_graph.Projection.node) ~pin_of ~size_row ~layout_row ~ref_rows ~parameters () =
  (* a value operator (+, *, sin ...) or a nested call has no catalog parameters: its inputs are
     the card's rows, so the inspector shows those.  A literal is a field, a wire or an
     expression a readout that typing replaces; edits are [Set_arg] on the row's own key. *)
  let op_rows = if parameters <> [] || ref_rows <> [] || layout_row <> None || size_row <> None then [] else
    List.filter_map (fun (r : Flow_graph.Projection.row) ->
      match r.kind, r.expr with
      | (Flow_graph.Projection.Arg | Flow_graph.Projection.Rest | Flow_graph.Projection.Hole), Some e when not r.head ->
          let name = arg_prefix ^ r.label in
          let field ?vec3 ?(suffix = "") kind current =
            { Param.name = name ^ suffix; label = (if vec3 = None then r.label else suffix);
              description = None; folder = []; impact = Param.Cook; primary = false;
              unit = None; vec3; kind; default = current; current } in
          let range lo hi = { Param.soft_min = lo; soft_max = hi; hard_min = None; hard_max = None } in
          let float_range f = match r.control with
            | Flow_graph.Projection.Range (lo, hi) -> range lo hi
            | _ -> let m = Float.max 1. (Float.abs f *. 2.) in range (-. m) m in
          let row fields drive live =
            Some (r, { Pxui_shell.Inspector.path = name; fields; shown = Flow_graph.Projection.row_shown ?pin:(pin_of r) r;
                       locked = false; drive; live }) in
          let wired () = row [ field Param.Text_view (Param.Text_value "") ]
            (Some (match e with { Flow.Syntax.node = Flow.Syntax.Sym s; _ } -> s | e -> "=" ^ Flow.Lisp.flat e)) None in
          (match e.node with
           | Flow.Syntax.Num t ->
               (match r.ty, float_of_string_opt t with
                | Some Flow.Ty.Int, _ when int_of_string_opt t <> None ->
                    let i = int_of_string t in
                    row [ field (Param.Integer_view { Param.soft_min = min 0 (2 * i); soft_max = max 10 (2 * i);
                        hard_min = None; hard_max = None }) (Param.Int_value i) ] None None
                | _, Some f -> row [ field (Param.Floating_view (float_range f)) (Param.Float_value f) ] None None
                | _ -> wired ())
           | Flow.Syntax.Str t -> row [ field Param.Text_view (Param.Text_value t) ] None None
           | Flow.Syntax.Sym ("true" | "false" as b) -> row [ field Param.Toggle_view (Param.Bool_value (b = "true")) ] None None
           | Flow.Syntax.Vec ([ _; _; _ ] as l) when List.for_all (fun (x : Flow.Syntax.t) -> match x.node with
               | Flow.Syntax.Num t -> float_of_string_opt t <> None | _ -> false) l ->
               row (List.mapi (fun i (x : Flow.Syntax.t) ->
                 let f = match x.node with Flow.Syntax.Num t -> float_of_string t | _ -> 0. in
                 field ~vec3:(name, i) ~suffix:(List.nth [ ".x"; ".y"; ".z" ] i)
                   (Param.Floating_view (float_range f)) (Param.Float_value f)) l) None None
           | _ -> wired ())
      | _ -> None) n.rows in
  op_rows

(* a switch's layouts and a split's size, as choice rows *)
let layout_and_size_rows ~value ~(n : Flow_graph.Projection.node) () =
  (* a switch's :active is a choice among its layouts, named from their panels *)
  let layout_names = match n.head, value.doc.Document.shell with
    | "ui/switch", Some { switch = Some sw; _ }
      when List.length sw.layouts
           = List.length (List.filter (fun (r : Flow_graph.Projection.row) -> match r.key with Flow_graph.Flow_edit.Pos _ -> r.kind <> Flow_graph.Projection.Add | _ -> false) n.rows) ->
        Some (Array.of_list (Editor_core.Panels.labels sw.layouts), sw.active)
    | _ -> None in
  let layout_row = Option.map (fun (names, active) ->
    let field = { Param.name = "@layout"; label = "Layout"; description = Some "The layout shown";
      folder = []; impact = Param.View; primary = true; unit = None; vec3 = None; kind = Param.Choice_view names;
      default = Param.Choice_value names.(0); current = Param.Choice_value names.(active) } in
    { Pxui_shell.Inspector.path = "@layout"; fields = [ field ]; shown = true; locked = false;
      drive = None; live = None }) layout_names in
  (* a split is sized by ratio or with one side fixed: the choice converts it, keeping
     the sizes it shows (flow.md 11.11) *)
  let size_names = [| "By ratio"; "First side fixed"; "Second side fixed" |] in
  let size_row = if n.head <> "ui/split" && n.head <> "ui/split-at" then None else begin
    let given key = List.exists (fun (r : Flow_graph.Projection.row) -> r.key = Flow_graph.Flow_edit.Kw key && r.expr <> None) n.rows in
    let now = if given "first_size" then 1 else if given "second_size" then 2 else 0 in
    let field = { Param.name = "@size"; label = "Size"; description = Some "What the split is sized by";
      folder = []; impact = Param.View; primary = true; unit = None; vec3 = None; kind = Param.Choice_view size_names;
      default = Param.Choice_value size_names.(0); current = Param.Choice_value size_names.(now) } in
    Some { Pxui_shell.Inspector.path = "@size"; fields = [ field ]; shown = true; locked = false;
           drive = None; live = None }
  end in
  layout_names, layout_row, size_names, size_row

(* the rows that name a graph: a choice among the graphs *)
let reference_rows ~value ~(n : Flow_graph.Projection.node) () =
  let is_ref_slot (r : Flow_graph.Projection.row) =
    (match r.expr with
     | Some { Flow.Syntax.node = Flow.Syntax.List [ { Flow.Syntax.node = Flow.Syntax.Sym "ref"; _ }; { Flow.Syntax.node = Flow.Syntax.Sym _; _ } ]; _ } -> true
     | _ -> false)
    || r.ty = Some Flow.Ty.material
    || (r.ty = Some Flow.Ty.geometry && String.starts_with ~prefix:"scene/" n.head) in
  let doc, _ = value.doc.Document.workspace in
  let sop_graphs = List.filter_map (fun (g : Flow.Workspace.graph) ->
    if g.context = Flow.Context.sop then Some g.name else None) doc.checked.graphs in
  let material_graphs = List.filter_map (fun (g : Flow.Workspace.graph) ->
    if g.context = Flow.Context.material then Some g.name else None) doc.checked.graphs in
  let ref_rows = List.filter_map (fun (r : Flow_graph.Projection.row) ->
    if not (is_ref_slot r) then None else
    let is_material = r.ty = Some Flow.Ty.material in
    let target = match r.expr with
      | Some { Flow.Syntax.node = Flow.Syntax.List [ { Flow.Syntax.node = Flow.Syntax.Sym "ref"; _ }; { Flow.Syntax.node = Flow.Syntax.Sym name; _ } ]; _ } -> name
      | _ -> "" in
    let choices = match (if is_material then material_graphs else sop_graphs) with
      | [] when not is_material -> [| "(none)" |]
      | gs ->
          let list = if target <> "" && not (List.mem target gs) then target :: gs else gs in
          Array.of_list (if is_material then list @ [ "new material" ] else list) in
    let field_name = "@ref:" ^ r.label in
    let current_val = if target <> "" then target else choices.(0) in
    let field = {
      Param.name = field_name;
      label = String.capitalize_ascii r.label;
      description = Some (if is_material then "Material graph reference" else "SOP graph reference");
      folder = [];
      impact = Param.Cook;
      primary = false;
      unit = None;
      vec3 = None;
      kind = Param.Choice_view choices;
      default = Param.Choice_value choices.(0);
      current = Param.Choice_value current_val;
    } in
    Some (r, { Pxui_shell.Inspector.path = field_name; fields = [ field ]; shown = true; locked = false;
               drive = None; live = None }))
    n.rows in
  ref_rows

(* the readouts a node shows under its head *)
let readout_rows ~(n : Flow_graph.Projection.node) ~value ~(footer : Flow_graph.Probe.footer) ~readouts ~width ~zone_text ui =
  (* the head's detail line says it for a node; a zone keeps its readout rows *)
  if zone_text <> None then
    List.iter (fun (label, text) ->
      Pxui.Ui.inspector_readout ui ~width ~key:("ws-" ^ label) ~label text) readouts;
  if zone_text = None && Option.fold ~none:false ~some:(fun (e : Flow_graph.Probe.execution) -> e.tier <> "Cooked") footer.execution then
    List.iter (fun (label, text) ->
      Pxui.Ui.inspector_readout ui ~width ~key:("ws-" ^ label) ~label text)
      (List.filter (fun (label, _) -> List.mem label ["tier"; "execution group"; "group time"]) readouts);
  let checked = (fst value.doc.Document.workspace).checked in
  (if Flow.Workspace.Paths.mem n.path checked.approx then
    Pxui.Ui.inspector_readout ui ~width ~key:"ws-precision" ~label:"precision" "approximable"
  else match List.assoc_opt n.path checked.approx_reasons with
    | Some reasons -> List.iteri (fun i (reason : Diagnostic.t) ->
        Pxui.Ui.inspector_readout ui ~width ~key:("ws-precision-" ^ string_of_int i)
          ~label:"precision" reason.message) reasons
    | None -> ());
  ()

(* a loop or scope says what it is, then the rows of its rail *)
let zone_says ~width ~zone_text ~(n : Flow_graph.Projection.node) ui =
  (match n.zone, zone_text with
   | Some z, Some (_, sentence) ->
       let vars = List.filter_map (fun (r : Flow_graph.Projection.rail_row) -> if r.role = Flow_graph.Projection.Var then Some r.name else None) z.rail in
       Pxui.Ui.inspector_message ui ~key:"ws-zone-says"
         (sentence (match vars with v :: _ -> v | [] -> "item"));
       List.iter (fun (r : Flow_graph.Projection.rail_row) ->
         let label = match r.role with Flow_graph.Projection.Var -> r.name ^ " in" | Acc -> r.name ^ " (next)"
           | Param -> r.name ^ " (input)" | Capture -> r.name in
         Pxui.Ui.inspector_readout ui ~width ~key:("ws-rail-" ^ r.name) ~label
           (match r.expr with Some e -> Flow.Lisp.flat e | None -> "same for all")) z.rail
   | _ -> ());
  ()

(* a graph input: its type, and its default as one Lisp form *)
let input_inspector ~graph ~width ~(scope : Flow_graph.Projection.scope) ~path ui =
  (match List.find_opt (fun (i : Flow_graph.Projection.input) -> i.path = path) scope.inputs with
   | None -> [], []
   | Some input ->
       ignore (Pxui.Ui.inspector_header ui ~key:"ws-input" ~kind:"input" ~title:input.name
         ~detail:(Flow.Ty.to_string input.ty) ());
       let shown = match input.default with Some d -> Flow.Lisp.flat d | None -> "" in
       let text = Pxui.Ui.inspector_body ui (fun () ->
         let box, cx, cy, cw = Pxui.Ui.inspector_row ui ~width ~key:"ws-input-default" ~label:"default" () in
         Pxui.Ui.within ui box (fun () ->
           match input.default with
           | Some { Flow.Syntax.node = Flow.Syntax.Num _; _ } ->
               (* a number: the pane's own field, so it drags here as it does on the card *)
               Option.value ~default:shown
                 (Pxui_graph.Scope.num_field ui ~at:(cx, cy) ~w:cw ~h:20.
                    (* the input's type decides the step: a float written [1] scrubs by fractions *)
                    ?kind:(match input.ty with
                      | Flow.Ty.Int -> Some Editor_core.Number.Int
                      | Flow.Ty.Float -> Some Editor_core.Number.Float | _ -> None)
                    "ws-input-default-field" shown)
           | _ ->
               fst (Pxui.Ui.value_field ui ~at:(cx, cy) ~w:cw ~h:20.
                 ~left:(float_of_string_opt shown = None)
                 ~valid:(fun t -> String.trim t <> "") "ws-input-default-field" shown))) in
       (match (if text = shown then Ok [] else Flow.Syntax.parse text) with
        | Ok [ form ] ->
            [ Syntax_edit (Flow_graph.Flow_edit.Set_input_default { form = graph; input = input.name; value = form }) ], []
        | Ok [] -> [], []
        | Ok _ -> [ Declined "A default is one Lisp form" ], []
        | Error d -> [ Declined ("Default: " ^ d.Diagnostic.message) ], []))

(* an expression chip clicked on a card: the row is the subject *)
let expression_inspector ~width ~value ~row_subject ui =
  let h, (r : Flow_graph.Projection.row) = Option.get row_subject in
  let e = Option.get r.expr in
  let op = match e.node with Flow.Syntax.List ({ Flow.Syntax.node = Flow.Syntax.Sym op; _ } :: _) -> op | _ -> "" in
  ignore (Pxui.Ui.inspector_header ui ~key:"ws-header" ~kind:op
    ~title:(Flow_graph.Projection.title h ^ " \xc2\xb7 " ^ r.label) ~detail:(Flow.Lisp.flat e) ());
  let entries = sub_entries ~shown:true r in
  let requests = Pxui.Ui.inspector_body ui (fun () ->
    let unfold =
      if Pxui.Ui.inspector_button ui ~key:("ws-unfold-" ^ r.label) ("Unfold " ^ r.label)
      then [ Syntax_edit (Flow_graph.Flow_edit.Unfold { node = h.path; key = r.key; sub = [] }) ] else [] in
    unfold @ (if entries = [] then [] else
      Pxui_shell.Inspector.flow_fields ui ~width ~kind_label:(kind_label op)
        (List.map (fun (_, _, _, ir) -> ir) entries)
      |> List.filter_map (arg_request ~source:(fst value.doc.workspace).source h ~arg_row:(arg_row_in entries)))) in
  requests, []

let workspace_inspector ?(image=fun _->None) ?(window = false) ?(on_choice = fun _ _ -> ()) ?(resized = fun _ _ -> None) ?(follows = fun _ -> None) value ui ~width path =
  let module P = Flow_graph.Projection in
  let module Probe = Flow_graph.Probe in
  let module S = Flow.Syntax in
  match value.scope_key, value.doc.Document.workspace with
  | Some { scope; records = Some records; graph; _ }, _ ->
      (* [holder @ [":" ^ label]]: a row of a node that is an operator call (the pane's expression chip) *)
      let row_subject = match List.rev path with
        | last :: (_ :: _ as rev_holder) when String.starts_with ~prefix:":" last
                                             && not (List.exists (fun (i : P.input) -> i.path = path) scope.inputs) ->
            let label = String.sub last 1 (String.length last - 1) in
            Option.bind (P.find scope (List.rev rev_holder)) (fun (h : P.node) ->
              Option.map (fun r -> h, r) (List.find_opt (fun (r : P.row) ->
                r.label = label && (match r.expr with Some e -> operator_call e | None -> false)) h.rows))
        | _ -> None in
      (match P.find scope path with
       | None when row_subject <> None ->
           (* an expression chip clicked on a card: the row is the subject, its body the sub-rows the
              holder's own inspector builds for it, and the Unfold button *)
           expression_inspector ~row_subject ~value ~width ui
       | None ->
           (* a graph input: its type, and its default as one Lisp form (the pane's field, here) *)
           input_inspector ~path ~scope ~width ~graph ui
       | Some n ->
           let chains = Probe.chains scope in
           let chain = Option.value ~default:[] (Hashtbl.find_opt chains n.path) in
           let probes = probes_of value chains n.path in
           let footer = Probe.footer records n ~probes in
           let preview=match Probe.at records n.path ~probes with
             |Some(Probe.Value(Flow.Eval.Deferred(Flow.Ty.Named "image",id)))->image id
             |Some(Probe.Image preview)->image preview.node
             |_->None in
           Option.iter(fun image->
             let w,h=Rays.Image.Private.dimensions image in
             Pxui.Ui.inspector_readout ui ~width ~key:"image-size" ~label:"image"
               (Printf.sprintf "%d × %d" w h);
             let height=min 160.(width *. float h /. float w)in
             let box=Pxui.Ui.box ui ~w:(Px width) ~h:(Px height) "image-thumbnail"in
             Pxui.Ui.draw ui box(fun paint(x,y,width,height)->
               let scale=min(width /. float w)(height /. float h)in
               Pxui.Ui.Paint.image paint ~x ~y ~w:(float w *. scale) ~h:(float h *. scale)image))preview;
           (* a loop or scope says what it is, then the rows of its rail (what it runs over, what it feeds back,
              what it only reads), as the study's inspector does *)
           let zone_text = Option.map (fun (z : P.zone) -> match z.kind with
             | P.For -> "repeat · collect a list", Printf.sprintf "Runs its body once for every %s and collects the results into a list."
             | Fold -> "iterate · feed back", Printf.sprintf "Runs its body for every %s and hands the result to the next run."
             | Scan -> "scan · keep every step", Printf.sprintf "Runs its body for every %s and keeps each step's result in a list."
             | Sum -> "sum · add up", Printf.sprintf "Runs its body for every %s and adds the results."
             | Let -> "scope · names for its result", (fun _ -> "Names shared by its result; it runs once.")
             | State -> "state · fold frames", (fun _ -> "Reads the previous frame's value and stores this frame's result.")
             | Fn -> "function · runs per call", (fun _ -> "A function: its body runs each time it is called.")
             | Branch -> "conditional arm", (fun _ -> "Runs when its condition is selected.")) n.zone in
           (* a bypassed node passes its input through: the plan node is the upstream one, so it
              has no number, cook or arguments of its own to show here *)
           let node_id = if n.bypass then None else compiled_at value chains records n.path in
           let node = Option.bind node_id (compiled_node value) in
           let literal = function
             | { S.node = S.Num _ | S.Str _ | S.Sym ("true" | "false"); _ } -> true
             | { S.node = S.Vec l; _ } -> List.for_all (fun (e : S.t) -> match e.node with S.Num _ -> true | _ -> false) l
             | _ -> false in
           let authored_row key =
             Option.bind (List.find_opt (fun (r : P.row) -> r.key = Flow_graph.Flow_edit.Kw key) n.rows)
               (fun (r : P.row) -> r.expr) in
           let authored (parameter : Flow_sop.Port.parameter) = authored_row parameter.path in
           (* the arguments: a compiled node's own fields, else (a scene or material node, which
              has none) the checked kind's parameters, valued by what is written *)
           let fields = match node with
             | Some node -> Node.parameter_fields node
             | None -> Option.value ~default:[] (kind_fields value graph n.head authored_row) in
           let parameters = Result.value ~default:[] (Flow_sop.Port.parameters fields) in
           (* the docked column under ~340 points (workspace.html) has no number and no bar; the
              window follows windows.html (no bar); the 380 column (inspector.html) has both *)
           let narrow = width < 340. in
           let doc_layout = (fst value.doc.Document.workspace).layout in
           let pin_of (r : P.row) = Option.bind (Layout_by_path.Path_map.find_opt n.path doc_layout.rows)
             (Layout_by_path.String_map.find_opt r.label) in
           let row_of path = List.find_opt (fun (r : P.row) -> r.key = Flow_graph.Flow_edit.Kw path) n.rows in
           let on_card_row path = match row_of path with
             | Some r -> P.row_shown ?pin:(pin_of r) r | None -> false in
           let geo = Option.bind node_id (fun id -> Option.bind (node_owner value id) (fun object_id ->
             Cook.geometry value.cook ~object_id ~node_id:id)) in
           let displayed = viewed_path value scope in
           let readouts = Probe.readouts records n ~probes in
           (* the head: the kind, VIEW while displayed, the node's number; the name edited in
              place; what the probe and the cook know; the flags the card has as buttons *)
           let follow = follows n.path in
           (* the active camera: the one the scene's root names as its :camera *)
           let root_node = List.find_opt (fun (m : P.node) -> m.head = "scene/root") scope.nodes in
           let active_camera = n.head = "scene/camera" && (match root_node with
             | Some root -> List.exists (fun (r : P.row) -> r.key = Flow_graph.Flow_edit.Kw "camera" && (match r.expr with
                 | Some { S.node = S.Sym name; _ } -> name = List.nth n.path (List.length n.path - 1)
                 | _ -> false)) root.rows
             | None -> false) in
           (* a scene or material node says its state: the active camera is `active`, the rest how it cooks *)
           let scene_state = if not (String.starts_with ~prefix:"scene/" n.head || String.starts_with ~prefix:"material/" n.head)
             then None else Some (if active_camera then "active" else
               Option.value ~default:"cached" (List.assoc_opt "cook" readouts)) in
           let buttons =
             (if List.mem n.ty Flow.Ty.[ geometry; drawing; image ] then
                [ { Pxui.Ui.caption = "View"; keycap = "V"; active = displayed = Some n.path; usable = true }, `View ]
              else [])
             @ (if P.bypassable n then
                  [ { Pxui.Ui.caption = "Bypass"; keycap = "B"; active = n.bypass; usable = true }, `Bypass ] else [])
             @ (match root_node with
                | Some root when n.head = "scene/camera" ->
                    [ { Pxui.Ui.caption = "Active"; keycap = ""; active = active_camera; usable = not active_camera },
                      `Activate root.path ]
                | _ -> [])
             @ (if follow <> None then
                  [ { Pxui.Ui.caption = "Enter"; keycap = "I"; active = false; usable = true }, `Enter ] else []) in
           let head = Pxui.Ui.inspector_header ui ~key:"ws-header" ~kind:n.head
             ?badge:(if displayed = Some n.path then Some "view" else None)
             ?index:(if narrow || window then None else Option.map (Printf.sprintf "NO. %04d") node_id)
             ?rename:(if n.synthetic then None else Some Flow.Symbol.valid_name)
             ~actions:(List.map fst buttons)
             ~title:(Flow_graph.Projection.title n)
             ~detail:(match zone_text with
               | Some (kind, _) -> Printf.sprintf "%s · %s" kind (Flow.Ty.to_string n.ty)
               | None ->
                   (match geo with
                    | Some g ->
                        (* what the cook knows of the node: its points and primitives *)
                        let count k word = Printf.sprintf "%s %s" (group_digits k) word in
                        String.concat " · " ([ count g.points (if g.points = 1 then "point" else "points");
                          count g.prims (if g.prims = 1 then "prim" else "prims") ]
                          @ Option.to_list (Option.map (Printf.sprintf "cooked %.3f s") g.seconds))
                    | None ->
                   (match scene_state with
                    | Some state -> n.head ^ " · " ^ state
                    | None ->
                   (match String.concat " · " (List.map snd readouts) with
                    | "" -> Flow.Ty.to_string n.ty | text -> text)))) ~reset:"Reset all" ~reset_enabled:(List.exists (fun (parameter : Flow_sop.Port.parameter) -> authored parameter <> None) parameters) () in
           let pressed = Option.map (fun i -> snd (List.nth buttons i)) head.chosen in
           (* Reset all: every written argument of the node removed, one undoable entry *)
           let reset_all = if not head.reset_pressed then [] else
             match List.filter_map (fun (parameter : Flow_sop.Port.parameter) ->
               Option.map (fun _ -> Flow_graph.Flow_edit.Disconnect { node = n.path;
                 key = Flow_graph.Flow_edit.Kw parameter.path; fallback = None }) (authored parameter)) parameters with
             | [] -> []
             | [ op ] -> [ Syntax_edit op ]
             | ops -> [ Syntax_batch ("Reset all", ops) ] in
           (* the name: editing it is the pane's Rename (the text's binding name); a nested node
              has no name yet, giving it one binds it *)
           let rename = if n.synthetic || head.renamed = Flow_graph.Projection.title n then []
             else [ Syntax_edit (Flow_graph.Flow_edit.Rename { node = n.path; to_ = head.renamed }) ] in
           let flags = match pressed with
             | Some `Bypass -> [ Syntax_edit (Flow_graph.Flow_edit.Toggle_bypass { node = n.path }) ], []
             | Some `View -> [], [ Pxui_graph.Scope.Display_set n.path ]
             | Some `Enter -> [], [ Pxui_graph.Scope.Activated n.path ]
             | Some (`Activate root) ->
                 [ Syntax_edit (Flow_graph.Flow_edit.Set_arg { node = root; key = Flow_graph.Flow_edit.Kw "camera"; sub = [];
                     value = S.make (S.Sym (List.nth n.path (List.length n.path - 1))) }) ], []
             | None -> [], [] in
           let on_card = ref 0 in
           let requests, picks = Pxui.Ui.inspector_body ui (fun () ->
           zone_says ~n ~zone_text ~width ui;
           readout_rows ~zone_text ~width ~readouts ~footer ~value ~n ui;
           let hoist = if footer.invariant && Pxui.Ui.inspector_button ui ~key:"ws-hoist" "Move out of the loop"
             then [ Syntax_edit (Flow_graph.Flow_edit.Hoist { node = n.path }) ] else [] in
           (* a macro call: what it is, and the request the pane's lens button makes *)
           let macro = match n.macro with
             | Some name ->
                 Pxui.Ui.inspector_message ui ~key:"ws-macro"
                   ("Macro " ^ name ^ ": its rows are the holes.");
                 if Pxui.Ui.inspector_button ui ~key:"ws-inline" "Replace call with expansion"
                 then [ Syntax_edit (Flow_graph.Flow_edit.Inline_macro { node = n.path }) ] else []
             | None -> [] in
           (* the items of a list or a string: each can move up a place *)
           let movers = if n.synthetic || not (Flow_graph.Projection.reorderable n) then [] else
             List.concat_map (fun (r : P.row) -> match r.key with
               | Flow_graph.Flow_edit.Pos k when k >= 1 && k < 24 ->
                   if Pxui.Ui.inspector_button ui ~key:(Printf.sprintf "ws-move-%d" k)
                       (Printf.sprintf "Move item %d up" (k + 1))
                   then [ Syntax_edit (Flow_graph.Flow_edit.Move_item { node = n.path; pos = k }) ] else []
               | _ -> []) n.rows in
           if n.head = "settings/config" then Pxui.Ui.inspector_message ui ~key:"ws-startup-settings"
             "Title, size, frame rate and seed apply on restart.";
           let ref_rows = reference_rows ~n ~value () in
           let layout_names, layout_row, size_names, size_row = layout_and_size_rows ~n ~value () in
           let op_rows = operator_rows ~parameters ~ref_rows ~layout_row ~size_row ~pin_of ~n () in
           let entries = List.concat_map (fun ((r : P.row), (ir : Pxui_shell.Inspector.flow_row)) ->
             (ir.path, r, [], ir) :: sub_entries ~shown:ir.shown r) op_rows in
           (* the operator rows' Unfold: the nested call becomes its own binding and card *)
           let unfold_nested = List.concat_map (fun ((r : P.row), _) ->
             match r.expr with
             | Some e when operator_call e ->
                 if Pxui.Ui.inspector_button ui ~key:("ws-unfold-" ^ r.label) ("Unfold " ^ r.label)
                 then [ Syntax_edit (Flow_graph.Flow_edit.Unfold { node = n.path; key = r.key; sub = [] }) ] else []
             | _ -> []) op_rows in
           let arg_row = arg_row_in entries in
           let rows = Option.to_list layout_row @ Option.to_list size_row @ List.map snd ref_rows @ List.map (fun (_, _, _, ir) -> ir) entries @ List.map (fun (parameter : Flow_sop.Port.parameter) ->
             let wired = match authored parameter with Some e -> not (literal e) | None -> false in
             { Pxui_shell.Inspector.path = parameter.path; fields = parameter.fields; shown = on_card_row parameter.path; locked = false;
               (* a computed argument reads as its expression, "=" first: typing another one, or
                  a number, replaces it; the cross removes it *)
               (* a plain name is a link, read as "<- name"; anything else is an expression *)
               drive = (match authored parameter with
                 | Some { S.node = S.Sym name; _ } when wired -> Some name
                 | Some e when wired -> Some ("=" ^ Flow.Lisp.flat e)
                 | _ -> None);
               (* what the drive gives now: the node's value at the probe *)
               live = (match parameter.fields with
                 | [ { Param.current = Param.Int_value i; _ } ] -> Some (string_of_int i)
                 | [ { current = Param.Float_value f; _ } ] -> Some (Editor_core.Number.show f)
                 | [ { current = Param.Bool_value b; _ } ] -> Some (string_of_bool b)
                 | _ -> None) }) parameters in
           let expanded = List.filter_map (fun (f : Param.field_view) ->
             match f.folder with [] -> None | first :: _ -> Some first) fields |> List.sort_uniq String.compare in
           let edits = field_edits ~rows ~layout_names ~graph ~resized ~n ~size_names ~value ~arg_row ~row_of ~ref_rows ~parameters ~expanded ~width ~on_choice ui in
           output_section ~geo ~width ui;
           let note = note_section ~n ~width ui in
           let picks = iteration_picks ~records ~n ~probes ~chain ~footer ui in
           on_card := List.length (List.filter (fun (r : Pxui_shell.Inspector.flow_row) -> r.shown && r.path.[0] <> '@') rows);
           hoist @ macro @ note @ movers @ unfold_nested @ edits, picks) in
           (* the sheet's bar: what a field takes, and how many rows are on the card *)
           if not (window || narrow) then
             Pxui.Ui.inspector_bar ui ~hints:[ "s", "pin row to card"; "\xe2\x8c\xa5 click", "type a value" ] ~count:!on_card;
           fst flags @ rename @ reset_all @ requests, snd flags @ picks)
  | _ -> [], []

(* The node of the graph pane that a lowered node of the open object was made by, at the iterations
   the selectors probe. *)
let node_path value node_id =
  match value.scope_key, value.doc.Document.workspace with
  | Some { scope; records = Some records; _ }, _ ->
      let chains = Flow_graph.Probe.chains scope in
      let rec search (s : Flow_graph.Projection.scope) =
        List.find_map (fun (n : Flow_graph.Projection.node) ->
          if compiled_at value chains records n.path = Some node_id then Some n.path
          else Option.bind n.zone (fun (z : Flow_graph.Projection.zone) -> search z.scope)) s.nodes in
      search scope
  | _ -> None
