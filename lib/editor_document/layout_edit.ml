open Flow_graph.Edit_tree

(* The editor graph's layout edits: panels, splits, tiles, floats and the switch of layouts.
   They rewrite the text like [Flow_graph.Flow_edit] does, with the tree helpers of
   [Flow_graph.Edit_tree], and the document applies them as their own change type. *)

type path = Flow.Workspace.path

type op =
  | Set_layout_size of { node : path; size : [ `Ratio of float | `First of int | `Second of int ] }
  | Split_panel of { node : path; axis : [ `H | `V ]; ratio : float; panel : S.t }
  | Close_panel of { node : path }
  | Dock_panel of { node : path; target : path; side : [ `Left | `Right | `Top | `Bottom ] }
  | Set_panel_kind of { node : path; panel : S.t }
  | Set_layout of { graph : string; index : int }
  | Layout_new of { graph : string }
  | Merge_layouts of { graph : string }
  | Merge_then_new of { graph : string }
  | Layout_remove of { graph : string }
  | Layout_window of { graph : string; panel : S.t }
  | Layout_float of { graph : string; at : int list }

(* a split ratio clamped to [0.1, 0.9] with four decimals, trailing zeros trimmed *)
let ratio_text r =
  let rec trim t =
    if String.ends_with ~suffix:"0" t && not (String.ends_with ~suffix:".0" t) then
      trim (String.sub t 0 (String.length t - 1))
    else t
  in
  trim (Printf.sprintf "%.4f" (Float.max 0.1 (Float.min 0.9 r)))

(* ---- layouts: the editor graph's panels, with the switch at the root ----
   [(ui/workspace (ui/switch a b :active 0))], the switch written in place or bound; a graph
   without one has a single layout, the workspace's own tree. *)

let containers = [ "ui/split"; "ui/split-at"; "ui/tile"; "ui/floating" ]

let layout_binding (e : S.t) =
  match head_sym e with
  | Some h -> List.mem h containers
  | None -> ( match e.node with S.Sym _ -> true | _ -> false)

let binding_of ps n = List.find_map (fun (p, e) -> if pat_key p = n then Some e else None) ps

(* a layout as one expression: the splits, tiles and floats it names are copied in; panels stay shared *)
let rec solid ps (e : S.t) =
  match e.node with
  | S.Sym n -> ( match binding_of ps n with Some b when layout_binding b -> solid ps b | _ -> e)
  | S.List ({ S.node = S.Sym h; _ } :: _) when List.mem h containers -> map_children (solid ps) e
  | _ -> e

(* the layout bindings a layout reads, through the ones it names *)
let rec reach ps e =
  List.concat_map
    (fun n ->
      match binding_of ps n with Some b when layout_binding b -> n :: reach ps b | _ -> [])
    (dedup (sym_list e))

let layout_kids (e : S.t) =
  match (head_sym e, e.node) with
  | Some ("ui/split" | "ui/split-at"), S.List (_ :: args) -> (
      match List.rev (positional args) with b :: a :: _ -> [ a; b ] | _ -> [])
  | Some ("ui/tile" | "ui/floating"), S.List (_ :: args) -> positional args
  | _ -> []

(* where child [i] sits among the positional arguments *)
let kid_pos (e : S.t) i =
  match (head_sym e, e.node) with
  | Some ("ui/split" | "ui/split-at"), S.List (_ :: args) -> List.length (positional args) - 2 + i
  | _ -> i

(* Remove a named panel from every layout wrapper. Docking keeps its binding
   for reinsertion; closing drops it. Empty wrapper bindings disappear too. *)
let strip_panel ~keep sc leaf =
  let removed = ref [ leaf ] in
  let rec strip (e : S.t) =
    match e.node with
    | S.Sym name when List.mem name !removed -> None
    | S.List ({ S.node = S.Sym ("ui/split" | "ui/split-at"); _ } :: _) -> (
        match layout_kids e with
        | [ a; b ] -> (
            match (strip a, strip b) with
            | None, other | other, None -> other
            | Some a, Some b ->
                Some (arg_set (arg_set e (Pos (kid_pos e 0)) (Some a)) (Pos (kid_pos e 1)) (Some b))
            )
        | _ -> Some e)
    | S.List (({ S.node = S.Sym "ui/tile"; _ } as head) :: cells) -> (
        match List.filter_map strip cells with
        | [] -> None
        | cells -> Some { e with node = S.List (head :: cells) })
    | S.List [ ({ S.node = S.Sym ("ui/floating" | "ui/workspace"); _ } as head); child ] ->
        Option.map (fun child -> { e with node = S.List [ head; child ] }) (strip child)
    | S.List (({ S.node = S.Sym "ui/switch"; _ } as head) :: args) ->
        Some
          {
            e with
            node =
              S.List
                (head
                :: List.map
                     (fun child ->
                       match strip child with
                       | Some child -> child
                       | None -> fail "Keep at least one panel in every layout.")
                     args);
          }
    | _ -> Some e
  in
  let rec clean ps =
    let before = List.length !removed in
    let ps =
      List.filter_map
        (fun (p, e) ->
          if pat_key p = leaf then if keep then Some (p, e) else None
          else
            match strip e with
            | Some e -> Some (p, e)
            | None ->
                removed := pat_key p :: !removed;
                None)
        ps
    in
    if before = List.length !removed then ps else clean ps
  in
  let ps = clean sc.ps in
  let res =
    match strip sc.res with Some r -> r | None -> fail "Keep at least one docked panel."
  in
  (ps, res, !removed)

let drop_kid (e : S.t) i =
  match head_sym e with
  | Some ("ui/split" | "ui/split-at") -> Some (List.nth (layout_kids e) (1 - i))
  | Some "ui/tile" -> (
      match layout_kids e with [ _ ] -> None | _ -> Some (arg_set e (Pos i) None))
  | _ -> None

(* the layout without the panel at a tree path, and that panel *)
let rec take path (e : S.t) =
  match path with
  | [] -> fail "A layout keeps its last panel."
  | i :: rest -> (
      let kid =
        match List.nth_opt (layout_kids e) i with
        | Some k -> k
        | None -> fail "That panel is not in this layout."
      in
      if rest = [] then (drop_kid e i, kid)
      else
        match take rest kid with
        | Some k, taken -> (Some (arg_set e (Pos (kid_pos e i)) (Some k)), taken)
        | None, taken -> (drop_kid e i, taken))

let node_at path e =
  List.fold_left
    (fun e i ->
      match List.nth_opt (layout_kids e) i with
      | Some k -> k
      | None -> fail "That panel is not in this layout.")
    e path

let rec has_docked (e : S.t) =
  match head_sym e with
  | Some "ui/floating" -> false
  | Some ("ui/split" | "ui/split-at" | "ui/tile") -> List.exists has_docked (layout_kids e)
  | _ -> true

(* the [(ui/workspace ...)] an editor graph returns: its result, or the binding its result names *)
let workspace_call sc =
  match sc.res.node with
  | S.Sym n -> (
      match binding_of sc.ps n with
      | Some b when head_sym b = Some "ui/workspace" -> b
      | _ -> fail "This editor graph does not return a (ui/workspace ...).")
  | _ when head_sym sc.res = Some "ui/workspace" -> sc.res
  | _ -> fail "This editor graph does not return a (ui/workspace ...)."

let editor_scope s =
  let sc = ensure s in
  ignore (workspace_call sc);
  sc

let workspace_arg sc =
  match arg_get (workspace_call sc) (Pos 0) with
  | Some r -> r
  | None -> fail "The workspace holds no panels."

(* the scope with [root] as the workspace's panel, written where the call is *)
let set_workspace sc ps root =
  let call = arg_set (workspace_call sc) (Pos 0) (Some root) in
  match sc.res.node with
  | S.Sym n ->
      { sc with ps = List.map (fun (p, e) -> if pat_key p = n then (p, call) else (p, e)) ps }
  | _ -> { sc with ps; res = call }

let rebuilt sc = rebuild sc sc.ps sc.res
let is_switch (e : S.t) = head_sym e = Some "ui/switch"

(* the first switch of the layout: the root itself, or one nested in its splits, tiles and floats
   (bound to a name, or written in place); the binding holding it, or none when it is written in place *)
let switch_place sc =
  let rec find ~top (e : S.t) =
    match e.node with
    | S.Sym n -> (
        match binding_of sc.ps n with
        | Some b when is_switch b -> Some (Some n, b)
        | Some b when layout_binding b -> find ~top:false b
        | _ -> None)
    | _ when is_switch e -> if top then Some (None, e) else None
    | _ -> (
        match head_sym e with
        | Some h when List.mem h containers -> List.find_map (find ~top) (layout_kids e)
        | _ -> None)
  in
  find ~top:true (workspace_arg sc)

(* the first switch written in place below the root's splits, tiles and floats *)
let replace_inline_switch sw (root : S.t) =
  let found = ref false in
  let rec go (e : S.t) =
    if !found then e
    else if is_switch e then (
      found := true;
      keep_notes e sw)
    else match head_sym e with Some h when List.mem h containers -> map_children go e | _ -> e
  in
  go root

let set_switch sc ps place sw =
  match place with
  | None -> rebuilt (set_workspace sc ps (replace_inline_switch sw (workspace_arg sc)))
  | Some n ->
      rebuild sc
        (List.map (fun (p, e) -> if pat_key p = n then (p, keep_notes e sw) else (p, e)) ps)
        sc.res

let active_of args =
  match kw_get args "active" with
  | None -> 0
  | Some { S.node = S.Num s; _ } -> Option.value ~default:0 (int_of_string_opt s)
  | Some _ -> fail "The active layout is an expression: change it in the text."

(* the switch's active input; an [:active] outside its layouts is refused *)
let active_layout args =
  let a = active_of args in
  match if a < 0 then None else List.nth_opt (positional args) a with
  | Some o -> o
  | None -> fail "The active layout is not one of the switch's layouts."

(* layout bindings in [cands] that nothing reads any more go *)
let prune body cands =
  match scope_of body with
  | None -> body
  | Some b ->
      let used ps n =
        count_refs n b.res > 0
        || List.exists (fun (p, e) -> pat_key p <> n && count_refs n e > 0) ps
      in
      let rec go ps =
        let dead, kept =
          List.partition
            (fun (p, e) ->
              List.mem (pat_key p) cands && layout_binding e && not (used ps (pat_key p)))
            ps
        in
        if dead = [] then ps else go kept
      in
      collapse b (go b.ps) b.res

(* [f ps layout] rewrites the active layout: the switch's active input, else the workspace's tree *)
let edit_layout src graph f =
  edit_scope src [ graph ] (fun s ->
      let sc = editor_scope s in
      let slot, put =
        match switch_place sc with
        | Some (place, sw) ->
            let args = List.tl (S.children sw) in
            let a = active_of args in
            (active_layout args, fun e -> set_switch sc sc.ps place (arg_set sw (Pos a) (Some e)))
        | None -> (workspace_arg sc, fun e -> rebuilt (set_workspace sc sc.ps e))
      in
      prune (put (f sc.ps slot)) (reach sc.ps slot))

let one f = f

let rec rewrite src op : unit -> S.t list =
  match op with
  | Merge_then_new { graph } ->
      fun () -> rewrite (rewrite src (Merge_layouts { graph }) ()) (Layout_new { graph }) ()
  | Set_layout_size { node; size } ->
      one (fun () ->
          let sp, leaf = split_node node in
          edit_scope src sp (fun s ->
              let e = get_node s leaf in
              let axis, a, b =
                match (head_sym e, e.node) with
                | Some ("ui/split" | "ui/split-at"), S.List (_ :: args) -> (
                    match (positional args, layout_kids e) with
                    | axis :: _, [ a; b ] -> (axis, a, b)
                    | _ -> fail "That panel is not a split.")
                | _ -> fail "That panel is not a split."
              in
              (* one side fixed in whole points, or a ratio with four decimals *)
              let fixed key n = call "ui/split" [ axis; a; b; kwf key; num (max 1 n) ] in
              set_node s leaf
                (keep_notes e
                   (match size with
                   | `First n -> fixed "first_size" n
                   | `Second n -> fixed "second_size" n
                   | `Ratio r -> call "ui/split-at" [ axis; mk (S.Num (ratio_text r)); a; b ]))))
  | Split_panel { node; axis; ratio; panel } ->
      one (fun () ->
          let sp, leaf = split_node node in
          let used = root_used src (List.hd sp) in
          edit_scope src sp (fun s ->
              let sc =
                match scope_of s with
                | Some sc -> sc
                | None -> fail "This editor graph is a single expression. Edit it in Lisp."
              in
              let j = first_pair_index sc leaf in
              let a = fresh used (leaf ^ "_a") in
              let b = fresh used (leaf ^ "_b") in
              let p, orig = List.nth sc.ps j in
              let ratio = ratio_text ratio in
              let split =
                call "ui/split-at"
                  [
                    mk (S.Str (if axis = `H then "horizontal" else "vertical"));
                    mk (S.Num ratio);
                    sym a;
                    sym b;
                  ]
              in
              let before = List.take j sc.ps and after = List.drop (j + 1) sc.ps in
              reorder
                (rebuild sc (before @ [ (sym a, orig); (sym b, panel); (p, split) ] @ after) sc.res)))
  | Close_panel { node } ->
      one (fun () ->
          let sp, leaf = split_node node in
          edit_scope src sp (fun s ->
              let sc =
                match scope_of s with
                | Some sc -> sc
                | None -> fail "This editor graph is a single expression. Edit it in Lisp."
              in
              ignore (get_node s leaf);
              let ps, res, _ = strip_panel ~keep:false sc leaf in
              reorder (rebuild sc ps res)))
  | Dock_panel { node; target; side } ->
      one (fun () ->
          let sp, leaf = split_node node and tp, target = split_node target in
          if sp <> tp || leaf = target then fail "Dock two different panels of the same layout.";
          let used = root_used src (List.hd sp) in
          edit_scope src sp (fun s ->
              let sc = ensure s in
              ignore (get_node s leaf);
              ignore (get_node s target);
              let ps, res, removed = strip_panel ~keep:true sc leaf in
              if List.mem target removed then fail "A panel cannot dock inside its own group.";
              let group = fresh used (target ^ "_dock") in
              let first = side = `Left || side = `Top in
              let split =
                call "ui/split-at"
                  [
                    mk (S.Str (if side = `Top || side = `Bottom then "vertical" else "horizontal"));
                    mk (S.Num "0.5");
                    sym (if first then leaf else target);
                    sym (if first then target else leaf);
                  ]
              in
              let ps =
                List.map
                  (fun (p, e) -> (p, if pat_key p = target then e else rename_place target group e))
                  ps
              in
              reorder (rebuild sc (ps @ [ (sym group, split) ]) (rename_place target group res))))
  | Set_panel_kind { node; panel } ->
      one (fun () ->
          let sp, leaf = split_node node in
          edit_scope src sp (fun s -> set_node s leaf panel))
  | Set_layout { graph; index } ->
      one (fun () ->
          edit_scope src [ graph ] (fun s ->
              let sc = editor_scope s in
              match switch_place sc with
              | None -> fail "This workspace has one layout and no switch."
              | Some (place, sw) ->
                  if index < 0 || index >= List.length (positional (List.tl (S.children sw))) then
                    fail "There is no layout %d." index;
                  set_switch sc sc.ps place (arg_set sw (Kw "active") (Some (num index)))))
  | Layout_new { graph } ->
      one (fun () ->
          let used = root_used src graph in
          edit_scope src [ graph ] (fun s ->
              let sc = editor_scope s in
              (* a workspace without a switch gets one around its tree *)
              let sc, place, sw =
                match switch_place sc with
                | Some (place, sw) -> (sc, place, sw)
                | None ->
                    let tree = workspace_arg sc in
                    let ps, kid =
                      match tree.node with
                      | S.Sym _ -> (sc.ps, tree)
                      | _ ->
                          let n = fresh used "layout" in
                          (sc.ps @ [ (sym n, tree) ], sym n)
                    in
                    let name = fresh used "switch" and sw = call "ui/switch" [ kid ] in
                    (set_workspace sc (ps @ [ (sym name, sw) ]) (sym name), Some name, sw)
              in
              let args = List.tl (S.children sw) in
              let n = List.length (positional args) in
              if n >= 10 then fail "Ten layouts is the limit of the digit keys.";
              let copy = solid sc.ps (active_layout args) in
              let name = fresh used "layout" in
              let sw =
                arg_set (arg_set sw (Pos n) (Some (sym name))) (Kw "active") (Some (num n))
              in
              reorder (set_switch sc (sc.ps @ [ (sym name, copy) ]) place sw)))
  | Merge_layouts { graph } ->
      one (fun () ->
          (* an older file: the other [:context editor] graphs become layouts of a switch in [graph]; each
         brings its bindings, renamed [graph_name] so they cannot clash, and then goes *)
          let editor_graph (i : S.t) =
            match i.node with
            | S.List ({ S.node = S.Sym "graph"; _ } :: { S.node = S.Sym _; _ } :: rest) ->
                let rec has = function
                  | { S.node = S.Kw "context"; _ } :: { S.node = S.Sym "editor"; _ } :: _ -> true
                  | _ :: rest -> has rest
                  | [] -> false
                in
                has rest
            | _ -> false
          in
          let others =
            List.filter_map
              (fun i ->
                match root_name i with
                | Some n when n <> graph && editor_graph i -> Some (n, i)
                | _ -> None)
              (snd (workspace_parts src))
          in
          if others = [] then fail "There is one editor graph and nothing to merge.";
          let used = root_used src graph in
          let merged =
            edit_scope src [ graph ] (fun s ->
                let sc = editor_scope s in
                let sc, place, sw =
                  match switch_place sc with
                  | Some (place, sw) -> (sc, place, sw)
                  | None ->
                      let tree = workspace_arg sc in
                      let ps, kid =
                        match tree.node with
                        | S.Sym _ -> (sc.ps, tree)
                        | _ ->
                            let n = fresh used "layout" in
                            (sc.ps @ [ (sym n, tree) ], sym n)
                      in
                      let name = fresh used "switch" and sw = call "ui/switch" [ kid ] in
                      (set_workspace sc (ps @ [ (sym name, sw) ]) (sym name), Some name, sw)
                in
                let ps, sw =
                  List.fold_left
                    (fun (ps, sw) (name, item) ->
                      let theirs = editor_scope (last_child item) in
                      let names =
                        List.map
                          (fun (p, _) -> (pat_key p, fresh used (name ^ "_" ^ pat_key p)))
                          theirs.ps
                      in
                      let rename e =
                        List.fold_left (fun e (old, nw) -> rename_ref old nw e) e names
                      in
                      let own =
                        List.map
                          (fun (p, e) -> (sym (List.assoc (pat_key p) names), rename e))
                          theirs.ps
                      in
                      let tree = rename (workspace_arg theirs) in
                      let ps, kid =
                        match tree.node with
                        | S.Sym _ -> (ps @ own, tree)
                        | _ ->
                            let n = fresh used name in
                            (ps @ own @ [ (sym n, tree) ], sym n)
                      in
                      if List.length (positional (List.tl (S.children sw))) >= 10 then
                        fail "Ten layouts is the limit of the digit keys.";
                      ( ps,
                        arg_set sw
                          (Pos (List.length (positional (List.tl (S.children sw)))))
                          (Some kid) ))
                    (sc.ps, sw) others
                in
                reorder (set_switch sc ps place sw))
          in
          map_items merged
            (List.filter (fun i ->
                 match root_name i with Some n -> not (List.mem_assoc n others) | None -> true)))
  | Layout_remove { graph } ->
      one (fun () ->
          edit_scope src [ graph ] (fun s ->
              let sc = editor_scope s in
              match switch_place sc with
              | None -> fail "This workspace has one layout and no switch."
              | Some (place, sw) ->
                  let args = List.tl (S.children sw) in
                  let n = List.length (positional args) and a = active_of args in
                  if n < 2 then fail "A switch keeps its last layout.";
                  let gone = active_layout args in
                  let sw =
                    arg_set (arg_set sw (Pos a) None) (Kw "active") (Some (num (min a (n - 2))))
                  in
                  prune (set_switch sc sc.ps place sw) (reach sc.ps gone)))
  | Layout_window { graph; panel } ->
      one (fun () ->
          edit_layout src graph (fun _ layout ->
              call "ui/split-at"
                [ mk (S.Str "horizontal"); mk (S.Num "0.5"); layout; call "ui/floating" [ panel ] ]))
  | Layout_float { graph; at } ->
      one (fun () ->
          edit_layout src graph (fun ps layout ->
              let tree = solid ps layout in
              let up = match List.rev at with _ :: up -> List.rev up | [] -> [] in
              if up <> [] && head_sym (node_at up tree) = Some "ui/floating" then
                (* a window docks beside the rest *)
                match take up tree with
                | Some rest, window ->
                    call "ui/split-at"
                      [
                        mk (S.Str "horizontal");
                        mk (S.Num "0.7");
                        rest;
                        List.hd (layout_kids window);
                      ]
                | None, window -> List.hd (layout_kids window)
              else
                match take at tree with
                | Some rest, panel when has_docked rest ->
                    call "ui/split-at"
                      [
                        mk (S.Str "horizontal");
                        mk (S.Num "0.5");
                        rest;
                        call "ui/floating" [ panel ];
                      ]
                | _ -> fail "A layout keeps one docked panel."))

let label = function
  | Set_layout_size _ -> "Resize panel"
  | Split_panel _ -> "Split panel"
  | Close_panel _ -> "Close panel"
  | Set_panel_kind _ -> "Retype panel"
  | Dock_panel _ -> "Dock panel"
  | Set_layout _ -> "Layout"
  | Merge_layouts _ | Merge_then_new _ -> "Merge layouts"
  | Layout_new _ -> "New layout"
  | Layout_remove _ -> "Remove layout"
  | Layout_window _ -> "New window"
  | Layout_float _ -> "Float panel"

let gesture = function Set_layout _ -> Some "layout" | _ -> None

(* the paths an op reads, for the imported-form check *)
let paths = function
  | Set_layout_size { node; _ }
  | Split_panel { node; _ }
  | Close_panel { node }
  | Set_panel_kind { node; _ } ->
      [ node ]
  | Dock_panel { node; target; _ } -> [ node; target ]
  | Set_layout { graph; _ }
  | Layout_new { graph }
  | Merge_layouts { graph }
  | Merge_then_new { graph }
  | Layout_remove { graph }
  | Layout_window { graph; _ }
  | Layout_float { graph; _ } ->
      [ [ graph ] ]

(* the other editor graphs are gone: the layout keys of the editor graph go with them *)
let merges = function Merge_layouts _ | Merge_then_new _ -> true | _ -> false

(* a closed panel's key goes; every other key stays *)
let remap op p =
  match op with Close_panel { node } when has_prefix ~prefix:node p -> None | _ -> Some p

let apply_checked ?ops ?library ?checker catalog src op =
  apply_rewrite ?ops ?library ?checker catalog (rewrite src op)

(* a resize changes no node: the layout keys need no pruning *)
let changes_nodes = function Set_layout_size _ -> false | _ -> true
