(* The panel tree of a [ui/workspace] graph: its panels, where each was written, the viewports
   and canvases it names, the start state and the layout switch. *)
open Sop
module E = Flow.Eval
module W = Flow.Workspace
module F = Flow_graph.Flow_edit

let ( let* ) = Result.bind
let diag code message = Diagnostic.error ~code message

module Panels = Editor_core.Panels

type editor = {
  tree : Panels.t;
  origins : (Panels.path * Document.origin) list;
  named : (Panels.path * string) list;
  start : Document.start;
  repeated : string list;
  follows : (Panels.path * Panels.path) list;
  viewports : (string * E.value) list;
  canvases : (string * E.value) list;
  preview_sources : (string * Document.preview_source) list;
  switch : Document.switch option;
}

let viewport_key path = "v" ^ String.concat "." (List.map string_of_int path)

(* The tree of a [ui/workspace] value, with the graph a [ui/graph] names and the scene
   of each viewport; the origins come from walking the terms beside the values. *)
let panel_tree root =
  let named = ref [] and viewports = ref [] and canvases = ref [] and switch = ref None in
  let start = ref { Document.focus = None; looking = []; graph_views = []; tabs = [] } in
  let flag name args = List.assoc_opt name args = Some (E.Bool true) in
  let text name args =
    match List.assoc_opt name args with Some (E.Text t) -> Some t | _ -> None
  in
  let leaf path args panel =
    if flag "focus" args && !start.focus = None then
      start := { !start with focus = Some (List.rev path) };
    Ok (Panels.Leaf panel)
  in
  let arg name args =
    match List.assoc_opt name args with
    | Some v -> Ok v
    | None -> Error (diag "E_LOWER" ("A panel is missing its " ^ name ^ "."))
  in
  let axis = function E.Text "vertical" -> `V | _ -> `H in
  let rec go path = function
    | E.Struct ("ui/canvas", _, args) ->
        let key = "c" ^ String.concat "." (List.map string_of_int (List.rev path)) in
        let* drawing = arg "drawing" args in
        canvases := (key, drawing) :: !canvases;
        leaf path args (Panels.Canvas key)
    | E.Struct ("ui/viewport", _, args) ->
        let key = viewport_key (List.rev path) in
        let* scene = arg "scene" args in
        viewports := (key, scene) :: !viewports;
        if flag "look_through" args then start := { !start with looking = key :: !start.looking };
        leaf path args (Panels.View key)
    | E.Struct ("ui/graph", _, args) ->
        let here = List.rev path in
        Option.iter (fun name -> named := (here, name) :: !named) (text "graph" args);
        Option.iter
          (fun view -> start := { !start with graph_views = (here, view) :: !start.graph_views })
          (text "view" args);
        leaf path args Graph
    | E.Struct ("ui/inspector", _, args) -> leaf path args Inspector
    | E.Struct ("ui/spreadsheet", _, args) -> leaf path args Spreadsheet
    | E.Struct ("ui/outline", _, args) -> leaf path args Outline
    | E.Struct ("ui/list", _, args) -> leaf path args List
    | E.Struct ("ui/lisp", _, args) ->
        Option.iter
          (fun tab -> start := { !start with tabs = (List.rev path, tab) :: !start.tabs })
          (text "tab" args);
        leaf path args Lisp
    | E.Struct ("ui/timeline", _, args) -> leaf path args Timeline
    | E.Struct (("ui/split" | "ui/split-at"), _, args) ->
        let* first = arg "first" args in
        let* second = arg "second" args in
        let* a = go (0 :: path) first in
        let* b = go (1 :: path) second in
        let points name =
          match List.assoc_opt name args with
          | Some (E.Int n) -> Some n
          | Some (E.Float f) -> Some (int_of_float f)
          | _ -> None
        in
        let size : Panels.size =
          match (points "first_size", points "second_size", List.assoc_opt "ratio" args) with
          | Some n, _, _ -> `First n
          | None, Some n, _ -> `Second n
          | None, None, Some (E.Float r) -> `Ratio r
          | None, None, Some (E.Int n) -> `Ratio (float_of_int n)
          | _ -> `Ratio 0.5
        in
        Ok
          (Panels.Split
             {
               axis =
                 axis (Option.value (List.assoc_opt "axis" args) ~default:(E.Text "horizontal"));
               size;
               a;
               b;
             })
    | E.Struct ("ui/tile", _, args) ->
        let* cells =
          List.fold_left
            (fun acc (i, (_, v)) ->
              let* acc = acc in
              let* c = go (i :: path) v in
              Ok (c :: acc))
            (Ok [])
            (List.mapi (fun i a -> (i, a)) args)
        in
        Ok (Panels.Tile (List.rev cells))
    | E.Struct ("ui/floating", _, args) ->
        let* p = arg "panel" args in
        let* t = go (0 :: path) p in
        Ok (Panels.Float t)
    (* a switch is transparent: its active layout sits at the switch's own place in the tree, and
       a layout that is not showing registers nothing but its shape *)
    | E.Struct ("ui/switch", _, args) ->
        let active = match List.assoc_opt "active" args with Some (E.Int n) -> n | _ -> 0 in
        let first = !switch = None in
        if first then switch := Some { Document.layouts = []; active };
        let kids = List.filter_map (fun (n, v) -> if n = "active" then None else Some v) args in
        let* trees =
          List.fold_left
            (fun acc (i, kid) ->
              let* acc = acc in
              let saved = (!named, !viewports, !canvases, !start) in
              let* tree = go path kid in
              if i <> active then (
                let g, v, c, s = saved in
                named := g;
                viewports := v;
                canvases := c;
                start := s);
              Ok (tree :: acc))
            (Ok [])
            (List.mapi (fun i k -> (i, k)) kids)
        in
        let trees = List.rev trees in
        if first then switch := Some { Document.layouts = trees; active };
        Ok (List.nth trees active)
    | _ -> Error (diag "E_LOWER" "The editor graph returns a (ui/workspace ...) of panels.")
  in
  let* tree = go [] root in
  let* () = Result.map_error (diag "E_RANGE") (Panels.valid tree) in
  Ok (tree, List.rev !named, !start, List.rev !viewports, List.rev !canvases, !switch)

(* Which panels are named: the terms of the graph beside its value. *)
let origins (graph : W.graph) value =
  let bindings, result =
    match graph.body.node with W.Let (bs, r) -> (bs, r) | _ -> ([], graph.body)
  in
  let env =
    List.filter_map (fun (p, t) -> match p with W.Name n -> Some (n, t) | _ -> None) bindings
  in
  let found = ref [] and ofs = ref [] in
  let add path o = found := (List.rev path, o) :: !found in
  (* a panel that is not a name is written in place: the argument [key] of the call around it *)
  let inline home path key (t : W.term) =
    match t.node with
    | W.Ref_binding (_, []) | W.Loop _ -> ()
    | _ -> add path (Document.Inline (home, key))
  in
  let rec walk place path (t : W.term) (v : E.value) =
    let here () =
      match (t.path, place) with
      | Some p, _ -> Document.Bound_at p
      | None, Some (parent, key) -> Document.Inline_in (parent, key)
      | None, None -> Document.Looped
    in
    let pos i = Flow_graph.Flow_edit.Pos i in
    match (t.node, v) with
    | W.Ref_binding (n, []), _ ->
        add path (Document.Bound n);
        Option.iter (fun t' -> walk None path t' v) (List.assoc_opt n env)
    | W.Op { op = "ui/inspector" | "ui/spreadsheet" | "ui/list" | "ui/lisp"; args; _ }, _ ->
        Option.iter
          (fun (a : W.term) -> ofs := (List.rev path, a) :: !ofs)
          (List.assoc_opt "of" args)
    | W.Op { op = "ui/workspace"; args = (_, r) :: _; _ }, E.Struct (_, _, (_, rv) :: _) ->
        let h = here () in
        inline h path (pos 0) r;
        walk (Some (h, pos 0)) path r rv
    | W.Op { op = "ui/split" | "ui/split-at"; args; _ }, E.Struct (_, _, vargs) ->
        let h = here () in
        List.iteri
          (fun i key ->
            match (List.assoc_opt key args, List.assoc_opt key vargs) with
            | Some a, Some va ->
                let k = pos (Option.get (List.find_index (fun (n, _) -> n = key) args)) in
                inline h (i :: path) k a;
                walk (Some (h, k)) (i :: path) a va
            | _ -> ())
          [ "first"; "second" ]
    | W.Op { op = "ui/switch"; args; _ }, E.Struct (_, _, vargs) -> (
        (* transparent: the active layout is found at the switch's own path *)
        let h = here () in
        let layouts l = List.filter (fun (n, _) -> n <> "active") l in
        let active = match List.assoc_opt "active" vargs with Some (E.Int n) -> n | _ -> 0 in
        match (List.nth_opt (layouts args) active, List.nth_opt (layouts vargs) active) with
        | Some (_, a), Some (_, va) ->
            inline h path (pos active) a;
            walk (Some (h, pos active)) path a va
        | _ -> ())
    | W.Op { op = "ui/floating"; args = (_, a) :: _; _ }, E.Struct (_, _, (_, va) :: _) ->
        let h = here () in
        inline h (0 :: path) (pos 0) a;
        walk (Some (h, pos 0)) (0 :: path) a va
    | W.Op { op = "ui/tile"; args; _ }, E.Struct (_, _, vargs) ->
        let h = here () in
        if
          List.exists
            (fun (_, (a : W.term)) -> match a.node with W.Loop _ -> true | _ -> false)
            args
        then begin
          let key =
            pos
              (Option.get
                 (List.find_index
                    (fun (_, (a : W.term)) -> match a.node with W.Loop _ -> true | _ -> false)
                    args))
          in
          List.iteri (fun i _ -> add (i :: path) (Document.Loop (h, key))) vargs
        end
        else
          List.iteri
            (fun i (_, a) ->
              inline h (i :: path) (pos i) a;
              Option.iter
                (walk (Some (h, pos i)) (i :: path) a)
                (List.nth_opt (List.map snd vargs) i))
            args
    | _ -> ()
  in
  walk None [] result value;
  (!found, List.rev !ofs)

let editor (workspace : Workspace_doc.t) (plan : E.plan) =
  let* value = Workspace_doc.context_value ~force:false workspace plan Flow.Context.editor in
  match value with
  | None -> Ok None
  | Some (E.Struct ("ui/workspace", _, ("root", root) :: _) as whole) ->
      let* tree, named, start, viewports, canvases, switch = panel_tree root in
      let g = Option.get (Workspace_doc.editor_graph workspace) in
      let origins, ofs = origins g whole in
      let all = Panels.leaves tree in
      let bound path =
        match List.assoc_opt path origins with Some (Document.Bound n) -> Some n | _ -> None
      in
      let names = List.filter_map (fun (path, _) -> bound path) all in
      let repeated =
        List.sort_uniq String.compare
          (List.filter (fun n -> List.length (List.filter (( = ) n) names) > 1) names)
      in
      (* [:of name]: the graph leaf that binding makes in the layout shown *)
      let* follows =
        List.fold_left
          (fun acc (path, (term : W.term)) ->
            let* acc = acc in
            match term.node with
            | W.Ref_binding (n, []) -> (
                match
                  List.find_opt (fun (p, panel) -> panel = Panels.Graph && bound p = Some n) all
                with
                | Some (target, _) -> Ok ((path, target) :: acc)
                | None ->
                    Error
                      (diag "E_PANEL_OF"
                         (Printf.sprintf ":of %s is not a graph panel of the layout shown." n)))
            | _ -> Error (diag "E_PANEL_OF" ":of takes the binding of a graph panel."))
          (Ok []) ofs
      in
      let leaves =
        Panels.leaves tree
        |> List.filter_map (function
          | path, (Panels.View key | Canvas key) -> Some (path, key, List.assoc_opt path origins)
          | _ -> None)
      in
      (* ponytail: quadratic uniqueness scan over viewport leaves; use name
         counts if large layouts beyond the 16-renderer limit need it. *)
      let keys =
        List.map
          (fun (_, old, origin) ->
            let key =
              match origin with
              | Some (Document.Bound name)
                when List.length (List.filter (fun (_, _, o) -> o = origin) leaves) = 1 ->
                  Printf.sprintf "%c:%S/%S" old.[0] g.name name
              | _ -> old
            in
            (old, key))
          leaves
      in
      let key old = List.assoc old keys in
      let rec remap = function
        | Panels.Leaf (View old) -> Panels.Leaf (View (key old))
        | Leaf (Canvas old) -> Leaf (Canvas (key old))
        | Leaf p -> Leaf p
        | Split s -> Split { s with a = remap s.a; b = remap s.b }
        | Tile cells -> Tile (List.map remap cells)
        | Float panel -> Float (remap panel)
      in
      let rec authored = function
        | Document.Bound_at path -> F.arg_text workspace.source path F.Whole
        | Inline_in (home, arg) -> Option.bind (authored home) (fun form -> F.arg_of form arg)
        | Copy _ | Looped -> None
      in
      let preview_sources =
        List.map
          (fun (_, old, origin) ->
            let panel_form =
              match origin with
              | Some (Document.Bound name) -> F.arg_text workspace.source [ g.name; name ] F.Whole
              | Some (Inline (home, arg)) ->
                  Option.bind (authored home) (fun form -> F.arg_of form arg)
              | Some (Loop _) | None -> None
            in
            ( key old,
              {
                Document.editor_graph = g.name;
                panel = origin;
                scene_ref = Option.bind panel_form (fun form -> F.arg_of form (F.Pos 0));
                instance = List.assoc old viewports;
              } ))
          (List.filter (fun (_, old, _) -> List.mem_assoc old viewports) leaves)
      in
      let start = { start with looking = List.map key start.looking } in
      Ok
        (Some
           {
             tree = remap tree;
             origins;
             named;
             repeated;
             follows;
             start;
             viewports = List.map (fun (old, scene) -> (key old, scene)) viewports;
             canvases = List.map (fun (old, drawing) -> (key old, drawing)) canvases;
             preview_sources;
             switch;
           })
  | Some _ -> Error (diag "E_LOWER" "The editor graph returns a (ui/workspace ...).")
