open Edit_base

(* ---- layouts: the editor graph's panels, with the switch at the root ----
   [(ui/workspace (ui/switch a b :active 0))], the switch written in place or bound; a graph
   without one has a single layout, the workspace's own tree. *)

let panel_expr src kind =
  let rec context = function
    | { S.node = S.Kw "context"; _ } :: { S.node = S.Sym c; _ } :: _ -> Some c
    | _ :: rest -> context rest
    | [] -> None in
  let scene = List.find_map (fun (item : S.t) -> match item.node with
    | S.List ({ S.node = S.Sym "graph"; _ } :: { S.node = S.Sym n; _ } :: rest)
      when context rest = Some "scene" -> Some n
    | _ -> None) (snd (workspace_parts src)) in
  match kind with
  | "outline" | "graph" | "list" | "lisp" | "inspector" | "spreadsheet" | "timeline" -> call ("ui/" ^ kind) []
  | "viewport" ->
      (match scene with
       | Some n -> call "ui/viewport" [ call "ref" [ sym n ] ]
       | None -> fail "A viewport needs a scene graph.")
  | "canvas" ->
      (match List.find_map (fun (item : S.t) -> match item.node with
        | S.List ({S.node = S.Sym "graph"; _} :: {S.node = S.Sym n; _} :: rest)
          when context rest = Some "draw" -> Some n
        | _ -> None) (snd (workspace_parts src)) with
       | Some n -> call "ui/canvas" [call "ref" [sym n]]
       | None -> fail "A canvas needs a draw graph.")
  | _ when String.starts_with ~prefix:"graph:" kind ->
      call "ui/graph" [ mk (S.Str (String.sub kind 6 (String.length kind - 6))) ]
  | _ -> fail "Unknown panel type %s." kind

let containers = [ "ui/split"; "ui/split-at"; "ui/tile"; "ui/floating" ]
let layout_binding (e : S.t) = match head_sym e with
  | Some h -> List.mem h containers
  | None -> (match e.node with S.Sym _ -> true | _ -> false)
let binding_of ps n = List.find_map (fun (p, e) -> if pat_key p = n then Some e else None) ps

(* a layout as one expression: the splits, tiles and floats it names are copied in; panels stay shared *)
let rec solid ps (e : S.t) = match e.node with
  | S.Sym n -> (match binding_of ps n with Some b when layout_binding b -> solid ps b | _ -> e)
  | S.List ({ S.node = S.Sym h; _ } :: _) when List.mem h containers -> map_children (solid ps) e
  | _ -> e

(* the layout bindings a layout reads, through the ones it names *)
let rec reach ps e = List.concat_map (fun n -> match binding_of ps n with
  | Some b when layout_binding b -> n :: reach ps b | _ -> []) (dedup (sym_list e))

let layout_kids (e : S.t) = match head_sym e, e.node with
  | Some ("ui/split" | "ui/split-at"), S.List (_ :: args) ->
      (match List.rev (positional args) with b :: a :: _ -> [ a; b ] | _ -> [])
  | Some ("ui/tile" | "ui/floating"), S.List (_ :: args) -> positional args
  | _ -> []

(* where child [i] sits among the positional arguments *)
let kid_pos (e : S.t) i = match head_sym e, e.node with
  | Some ("ui/split" | "ui/split-at"), S.List (_ :: args) -> List.length (positional args) - 2 + i
  | _ -> i

(* Remove a named panel from every layout wrapper. Docking keeps its binding
   for reinsertion; closing drops it. Empty wrapper bindings disappear too. *)
let strip_panel ~keep sc leaf =
  let removed = ref [leaf] in
  let rec strip (e : S.t) = match e.node with
    | S.Sym name when List.mem name !removed -> None
    | S.List ({ S.node = S.Sym ("ui/split" | "ui/split-at"); _ } :: _) ->
        (match layout_kids e with
         | [a; b] -> (match strip a, strip b with
             | None, other | other, None -> other
             | Some a, Some b -> Some (arg_set (arg_set e (Pos (kid_pos e 0)) (Some a))
                 (Pos (kid_pos e 1)) (Some b)))
         | _ -> Some e)
    | S.List (({ S.node = S.Sym "ui/tile"; _ } as head) :: cells) ->
        (match List.filter_map strip cells with [] -> None
         | cells -> Some {e with node = S.List (head :: cells)})
    | S.List [({ S.node = S.Sym ("ui/floating" | "ui/workspace"); _ } as head); child] ->
        Option.map (fun child -> {e with node = S.List [head; child]}) (strip child)
    | S.List (({ S.node = S.Sym "ui/switch"; _ } as head) :: args) ->
        Some {e with node = S.List (head :: List.map (fun child -> match strip child with
          | Some child -> child | None -> fail "Keep at least one panel in every layout.") args)}
    | _ -> Some e in
  let rec clean ps =
    let before = List.length !removed in
    let ps = List.filter_map (fun (p, e) ->
      if pat_key p = leaf then (if keep then Some (p, e) else None)
      else match strip e with
        | Some e -> Some (p, e)
        | None -> removed := pat_key p :: !removed; None) ps in
    if before = List.length !removed then ps else clean ps in
  let ps = clean sc.ps in
  let res = match strip sc.res with Some r -> r | None -> fail "Keep at least one docked panel." in
  ps, res, !removed

let drop_kid (e : S.t) i = match head_sym e with
  | Some ("ui/split" | "ui/split-at") -> Some (List.nth (layout_kids e) (1 - i))
  | Some "ui/tile" -> (match layout_kids e with [ _ ] -> None | _ -> Some (arg_set e (Pos i) None))
  | _ -> None

(* the layout without the panel at a tree path, and that panel *)
let rec take path (e : S.t) = match path with
  | [] -> fail "A layout keeps its last panel."
  | i :: rest ->
      let kid = match List.nth_opt (layout_kids e) i with
        | Some k -> k | None -> fail "That panel is not in this layout." in
      if rest = [] then drop_kid e i, kid
      else match take rest kid with
        | Some k, taken -> Some (arg_set e (Pos (kid_pos e i)) (Some k)), taken
        | None, taken -> drop_kid e i, taken

let node_at path e = List.fold_left (fun e i -> match List.nth_opt (layout_kids e) i with
  | Some k -> k | None -> fail "That panel is not in this layout.") e path

let rec has_docked (e : S.t) = match head_sym e with
  | Some "ui/floating" -> false
  | Some ("ui/split" | "ui/split-at" | "ui/tile") -> List.exists has_docked (layout_kids e)
  | _ -> true

(* the [(ui/workspace ...)] an editor graph returns: its result, or the binding its result names *)
let workspace_call sc = match sc.res.node with
  | S.Sym n -> (match binding_of sc.ps n with
      | Some b when head_sym b = Some "ui/workspace" -> b
      | _ -> fail "This editor graph does not return a (ui/workspace ...).")
  | _ when head_sym sc.res = Some "ui/workspace" -> sc.res
  | _ -> fail "This editor graph does not return a (ui/workspace ...)."
let editor_scope s = let sc = ensure s in ignore (workspace_call sc); sc
let workspace_arg sc = match arg_get (workspace_call sc) (Pos 0) with
  | Some r -> r | None -> fail "The workspace holds no panels."
(* the scope with [root] as the workspace's panel, written where the call is *)
let set_workspace sc ps root =
  let call = arg_set (workspace_call sc) (Pos 0) (Some root) in
  match sc.res.node with
  | S.Sym n -> { sc with ps = List.map (fun (p, e) -> if pat_key p = n then p, call else p, e) ps }
  | _ -> { sc with ps; res = call }
let rebuilt sc = rebuild sc sc.ps sc.res

let is_switch (e : S.t) = head_sym e = Some "ui/switch"
(* the first switch of the layout: the root itself, or one nested in its splits, tiles and floats
   (bound to a name, or written in place); the binding holding it, or none when it is written in place *)
let switch_place sc =
  let rec find ~top (e : S.t) = match e.node with
    | S.Sym n -> (match binding_of sc.ps n with
        | Some b when is_switch b -> Some (Some n, b)
        | Some b when layout_binding b -> find ~top:false b
        | _ -> None)
    | _ when is_switch e -> if top then Some (None, e) else None
    | _ -> (match head_sym e with
        | Some h when List.mem h containers -> List.find_map (find ~top) (layout_kids e)
        | _ -> None) in
  find ~top:true (workspace_arg sc)

(* the first switch written in place below the root's splits, tiles and floats *)
let replace_inline_switch sw (root : S.t) =
  let found = ref false in
  let rec go (e : S.t) =
    if !found then e
    else if is_switch e then (found := true; keep_notes e sw)
    else match head_sym e with
      | Some h when List.mem h containers -> map_children go e
      | _ -> e in
  go root
let set_switch sc ps place sw = match place with
  | None -> rebuilt (set_workspace sc ps (replace_inline_switch sw (workspace_arg sc)))
  | Some n -> rebuild sc (List.map (fun (p, e) -> if pat_key p = n then p, keep_notes e sw else p, e) ps) sc.res

let active_of args = match kw_get args "active" with
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
let prune body cands = match scope_of body with
  | None -> body
  | Some b ->
      let used ps n = count_refs n b.res > 0 || List.exists (fun (p, e) -> pat_key p <> n && count_refs n e > 0) ps in
      let rec go ps =
        let dead, kept = List.partition (fun (p, e) ->
          List.mem (pat_key p) cands && layout_binding e && not (used ps (pat_key p))) ps in
        if dead = [] then ps else go kept in
      collapse b (go b.ps) b.res

(* [f ps layout] rewrites the active layout: the switch's active input, else the workspace's tree *)
let edit_layout src graph f = edit_scope src [ graph ] (fun s ->
  let sc = editor_scope s in
  let slot, put = match switch_place sc with
    | Some (place, sw) ->
        let args = List.tl (S.children sw) in
        let a = active_of args in
        active_layout args,
        (fun e -> set_switch sc sc.ps place (arg_set sw (Pos a) (Some e)))
    | None -> workspace_arg sc, (fun e -> rebuilt (set_workspace sc sc.ps e)) in
  prune (put (f sc.ps slot)) (reach sc.ps slot))

(* the call, loop or scope written in input [key] of [leaf] becomes a binding just before the
   binding that held it; the scope with it, and its name *)
let unfold_in ~used ?name s leaf key sub =
  let e = get_node s leaf in
  let whole = match arg_get e key with Some w -> w | None -> fail "That input is not set." in
  let inner = match get_sub whole sub with Some i -> i | None -> fail "That part does not exist." in
  let h = match head_sym inner with
    | Some h when h <> "ref" -> h
    | _ -> fail "Only a call, loop or scope can be unfolded." in
  let base = fst (split_leaf leaf) in
  let name = match name with
    | Some n -> n
    | None ->
        fresh used
          (if is_zone inner then
             (if base = "@result" then "each" else base ^ "_" ^ (if h = "sum" then "total" else "each"))
           else if h = "let*" then "block" else value_base h) in
  let e' = arg_set e key (Some (set_sub whole sub (sym name))) in
  let sc = ensure (set_node s leaf e') in
  let at = if base = "@result" then List.length sc.ps else first_pair_index sc leaf in
  rebuild sc (insert_at sc.ps at [ sym name, inner ]) sc.res, name

(* a wire never deletes a node: the nested node an input held stays, as a binding nothing reads *)
let keep_nested ?(pick = fun a -> node_call a || value_call a) ~used s leaf key = match key with
  | Pos _ | Kw _ | Arm _ ->
      (match arg_get (get_node s leaf) key with
       | Some a when pick a -> fst (unfold_in ~used s leaf key [])
       | _ -> s)
  | _ -> s
