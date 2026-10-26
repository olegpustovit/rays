include Edit_base
include Edit_layout

(* a split ratio clamped to [0.1, 0.9] with four decimals, trailing zeros trimmed *)
let ratio_text r =
  let rec trim t = if String.ends_with ~suffix:"0" t && not (String.ends_with ~suffix:".0" t)
    then trim (String.sub t 0 (String.length t - 1)) else t in
  trim (Printf.sprintf "%.4f" (Float.max 0.1 (Float.min 0.9 r)))

(* a binding used once is inlined into its use *)
let fold_into src node =
  let sp, leaf = split_node node in
  edit_scope src sp (fun s ->
    let sc = match scope_of s with Some sc -> sc | None -> fail "Fold needs a named node." in
    if leaf = "@result" then fail "A result cannot be folded.";
    if nested leaf then fail "That node is already written in its use.";
    let j = first_pair_index sc leaf in
    let p, e = List.nth sc.ps j in
    let name = match p.node with S.Sym n -> n | _ -> fail "Only a plain name can be folded." in
    let others = List.filteri (fun k _ -> k <> j) sc.ps in
    let uses = List.fold_left (fun n (_, x) -> n + count_refs name x) (count_refs name sc.res) others in
    if uses <> 1 then fail "Fold needs exactly one use of %s inside its scope." name;
    (* the binding's note goes with it *)
    let rep = replace_ref name { e with notes = p.notes @ e.notes } in
    let ps = List.map (fun (q, x) -> q, rep x) others and res = rep sc.res in
    if List.exists (fun (_, x) -> count_refs name x > 0) ps || count_refs name res > 0 then
      fail "%s is read by field; it cannot be folded." name;
    collapse sc ps res)

(* the rule of the expression cards: an unnamed math node with one reader (not the result) is written in it *)
let inline_autos src sp names = List.fold_left (fun src n ->
  let ok = ref false in
  (try ignore (edit_scope src sp (fun s ->
    (match scope_of s with
     | Some sc when not (nested n) && sc.res.node <> S.Sym n ->
         Option.iter (fun j -> let e = snd (List.nth sc.ps j) in ok := value_call e && auto_named n e) (find_pair sc n)
     | _ -> ()); s)) with Fail _ -> ());
  if !ok then (try fold_into src (sp @ [ n ]) with Fail _ -> src) else src) src names

let rewrite ?fallback src op : (unit -> S.t list) list =
  let one f = [ f ] in
  match op with
  | Add_arm {node; after} -> one (fun () ->
      let sp, leaf = split_node node in
      edit_scope src sp (fun scope ->
        let e = get_node scope leaf in
        let h = Option.value ~default:"" (head_sym e) in
        if h <> "cond" && h <> "case" then fail "Only cond and case have editable arms.";
        let args = List.tl (S.children e) in
        let prefix, args = if h = "case" then [List.hd args], List.tl args else [], args in
        let arms = pairs args in
        let count = List.length arms - 1 in
        if after < -1 || after >= count then fail "That arm no longer exists.";
        let test = if h = "cond" then sym "false" else
          match arms with
          | ({S.node = S.Num value; _}, _) :: _ -> mk (S.Num (Flow.Lisp.float (float_of_string value +. 1.)))
          | ({S.node = S.Str value; _}, _) :: _ -> mk (S.Str (value ^ "1"))
          | ({S.node = S.Sym "true"; _}, _) :: _ -> sym "false"
          | _ -> sym "true" in
        let fallback = Option.value ~default:(snd (List.hd (List.rev arms))) fallback in
        set_node scope leaf {e with node = S.List (sym h :: prefix @ flat_pairs (insert_at arms (after + 1) [test, fallback]))}))
  | Delete_arm {node; index} -> one (fun () ->
      let sp, leaf = split_node node in
      edit_scope src sp (fun scope ->
        let e = get_node scope leaf in
        let h = Option.value ~default:"" (head_sym e) in
        if h <> "cond" && h <> "case" then fail "Only cond and case have editable arms.";
        let args = List.tl (S.children e) in
        let prefix, args = if h = "case" then [List.hd args], List.tl args else [], args in
        let arms = pairs args in
        if index < 0 || index >= List.length arms - 1 then fail "The final else arm stays.";
        set_node scope leaf {e with node = S.List (sym h :: prefix @ flat_pairs (List.filteri (fun i _ -> i <> index) arms))}))
  | Set_arg { node; key; sub; value } -> one (fun () ->
      let sp, leaf = split_node node in
      edit_scope src sp (fun s ->
        let e = get_node s leaf in
        let arg = if sub = [] then value else
          (match arg_get e key with
           | Some cur -> set_sub cur sub value
           | None -> fail "That input is not set.") in
        reorder (set_node s leaf (arg_set e key (Some arg)))))
  | Connect { node; key; src = name; iter } -> one (fun () ->
      let sp, leaf = split_node node in
      let used = root_used src (List.hd sp) in
      let prev = ref [] in
      let out = edit_scope src sp (fun s ->
        (* the output of a nested node feeds a second input: it needs a name *)
        let s, name = if nested name then
            (if fst (split_leaf name) <> "@result" && find_pair (ensure s) name = None then
               fail "Name that node first (rename it): it is written inside another scope.";
             let held, at = holder name in unfold_in ~used s held at [])
          else s, name in
        if leaf = "@result" && key = Whole then
          let sc = ensure s in
          let ps = match sc.res.node with
            | S.Sym _ -> sc.ps
            | _ ->
                let base = match head_sym sc.res with
                  | Some h -> (match String.split_on_char '/' h with [ _; k ] -> k | _ -> h)
                  | None -> "result" in
                let fresh = fresh_name src ~root:(List.hd sp) base in
                sc.ps @ [ sym fresh, sc.res ] in
          reorder (rebuild sc ps (sym name))
        else
          let s = keep_nested ~used s leaf key in
          let e = get_node s leaf in
          (match arg_get e key with Some { S.node = S.Sym n; _ } -> prev := [ n ] | _ -> ());
          let step = match iter, arg_get e key with
            | true, Some ({ S.node = S.Num n; _ } as cur) when n <> "0" && n <> "1" && float_of_string_opt n <> Some 0.
                && float_of_string_opt n <> Some 1. -> call "*" [ sym name; cur ]
            | _ -> sym name in
          reorder (set_node s leaf (arg_set e key (Some step)))) in
      inline_autos out sp (name :: !prev))
  | Disconnect { node; key; fallback } -> one (fun () ->
      let sp, leaf = split_node node in
      let prev = ref [] in
      let out = edit_scope src sp (fun s ->
        let s = keep_nested ~pick:value_call ~used:(root_used src (List.hd sp)) s leaf key in
        let e = get_node s leaf in
        (match arg_get e key with Some { S.node = S.Sym n; _ } -> prev := [ n ] | _ -> ());
        let v = match key, fallback with
          | _, Some d -> Some d
          | (Kw _ | Field _ | Pos _), None -> None
          | _, None -> fail "Nothing to fall back to. Drag another output onto it instead." in
        reorder (set_node s leaf (arg_set e key v))) in
      inline_autos out sp !prev)
  | Set_input_default { form; input; value } -> one (fun () ->
      with_root src form (fun r ->
        let hit = ref false in
        let r = map_children (fun (c : S.t) -> match c.node with
          | S.Vec ds -> { c with node = S.Vec (List.map (fun (d : S.t) -> match d.node with
              | S.List (({ S.node = S.Sym n; _ } as name) :: colon :: ty :: _) when n = input ->
                  hit := true; { d with node = S.List [ name; colon; ty; value ] }
              | _ -> d) ds) }
          | _ -> c) r in
        if not !hit then fail "No input %s." input;
        r))
  | Unfold { node; key; sub } -> one (fun () ->
      let sp, leaf = split_node node in
      let used = root_used src (List.hd sp) in
      edit_scope src sp (fun s -> reorder (fst (unfold_in ~used s leaf key sub))))
  | Fold_into { node } -> one (fun () -> fold_into src node)
  | Delete_nodes { nodes } -> one (fun () ->
      (* inner first: deeper scopes, then nodes nested deeper in one binding *)
      let depth n = List.length n, List.length (String.split_on_char '#' (snd (split_node n))) in
      let by_depth = List.sort (fun a b -> compare (depth b) (depth a)) nodes in
      let touched = ref [] in
      let out = List.fold_left (fun src node ->
        let sp, leaf = split_node node in
        let gone = ref None in
        let src = edit_scope src sp (fun s ->
          if nested leaf then
            (* a nested node leaves its chain: what it read first takes its place *)
            let held, key = holder leaf in
            let h = get_node s held in
            let below = match Option.bind (arg_get h key) (fun e -> arg_get e (chain_in e)), key with
              | Some b, _ -> Some b
              | None, Pos _ -> Some (sym "nil")
              | None, _ -> None in
            set_node s held (arg_set h key below)
          else match scope_of s with
          | Some sc when leaf <> "@result" ->
              let j = first_pair_index sc leaf in
              let p, e = List.nth sc.ps j in
              gone := Some e;
              touched := (sp, dedup (sym_list e)) :: !touched;
              let others = List.filteri (fun k _ -> k <> j) sc.ps in
              (* a deleted value dissolves: its readers get its expression; a scene object leaves
                 the result that held it instead, and so does a loop the result reads (below) *)
              let obj = starts_with "scene/" (head_sym e) || starts_with "world/" (head_sym e) in
              let rep = match p.node with S.Sym n when not obj -> replace_ref n e | _ -> Fun.id in
              collapse sc (List.map (fun (q, x) -> q, rep x) others) (if is_zone e then sc.res else rep sc.res)
          | _ -> fail "Only a named node can be deleted.") in
        (* a scene object, World layer or loop of scene objects also leaves the result that held it *)
        match !gone with
        | Some e when starts_with "scene/" (head_sym e) || starts_with "world/" (head_sym e) || is_zone e ->
            let below = if is_zone e then None else arg_get e (Pos 0) in
            with_root src (List.hd sp) (fun root ->
              let root = detach leaf below root in
              if starts_with "world/" (head_sym e) then detach_result leaf below root else root)
        | _ -> src) src by_depth in
      List.iter (fun node ->
        (* read inside its own scope only: a sibling scope may bind the same name (a duplicated loop) *)
        let sp, name = split_node node in
        let used = ref false in
        (try ignore (edit_scope out sp (fun s -> used := List.mem name (sym_list s); s)) with Fail _ -> ());
        if not (nested name) && !used then
          fail "%s is still read (a field read such as %s.x, or a scene object). Disconnect those first." name name) nodes;
      List.fold_left (fun out (sp, names) -> inline_autos out sp names) out !touched)
  | Rename { node; to_ } -> one (fun () ->
      let sp, leaf = split_node node in
      let used = sym_list (root_form src (List.hd sp)) in
      if not (valid_name to_) || List.mem to_ used || W.name_taken to_ then
        fail "Pick a new lowercase name that is not used anywhere in this graph.";
      let out = edit_scope src sp (fun s ->
        (* naming a nested node binds it *)
        if nested leaf then
          let held, key = holder leaf in
          reorder (fst (unfold_in ~used:(ref []) ~name:to_ s held key []))
        else
        let sc = match scope_of s with Some sc -> sc | None -> fail "Rename needs a named node." in
        let j = first_pair_index sc leaf in
        let old = match (fst (List.nth sc.ps j)).node with S.Sym n -> n | _ -> fail "Only a plain name can be renamed." in
        let ps = List.mapi (fun k (p, v) ->
          if k = j then { p with S.node = S.Sym to_ }, v
          else if k > j then p, rename_ref old to_ v else p, v) sc.ps in
        rebuild sc ps (rename_ref old to_ sc.res)) in
      if nested leaf then out else inline_autos out sp [ to_ ])
  | Hoist { node } -> one (fun () ->
      let sp, leaf = split_node node in
      let pp, owner = match sp with
        | [ _ ] | [] -> fail "Only a node inside a loop or scope can move out."
        | _ -> split_node sp in
      edit_scope src pp (fun ps_expr ->
        let out_scope = ensure ps_expr in
        let hoisted = ref None in
        let inner_edit (v : S.t) =
          let entered, back = enter v in
          let sc = match scope_of entered with Some sc -> sc | None -> fail "Nothing to move." in
          let j = first_pair_index sc leaf in
          let p, e = List.nth sc.ps j in
          let local = List.filter (fun x -> List.exists (fun (q, _) -> List.mem x (pat_names q)) sc.ps)
              (dedup (sym_list e)) in
          if local <> [] then
            fail "%s uses %s from inside this scope. Move that out first." leaf (String.concat ", " local);
          hoisted := Some (p, e);
          back (collapse sc (List.filteri (fun k _ -> k <> j) sc.ps) sc.res) in
        let ps, res, at =
          if owner = "@result" then out_scope.ps, inner_edit out_scope.res, List.length out_scope.ps
          else
            let k = first_pair_index out_scope owner in
            List.mapi (fun i (p, v) -> if i = k then p, inner_edit v else p, v) out_scope.ps, out_scope.res, k in
        let p, e = Option.get !hoisted in
        reorder (rebuild out_scope (insert_at ps at [ p, e ]) res)))
  | Wrap { nodes; loop } ->
      let sp = scope_path_of nodes in
      let root = List.hd sp in
      let rootf = root_form src root in
      let attempt ?(nodes = nodes) build = fun () ->
        let used = root_used src root in
        edit_scope src sp (fun s ->
          let x = select s nodes "Repeat" in
          let iv = match List.find_opt (fun c -> not (List.mem c !used)
              && not (List.mem c (sel_declared x))) [ "i"; "j"; "k"; "n"; "idx" ] with
            | Some c -> used := c :: !used; c
            | None -> fresh used "i" in
          let added = build x iv used in
          reorder (rebuild x.sc (insert_at x.keep (min x.first (List.length x.keep)) added) x.sc.res)) in
      let zone_clause iv n = vec [ sym iv; call "range" [ num n ] ] in
      let zone h iv n body = call h [ zone_clause iv n; body ] in
      (match loop with
       | If -> List.map (fun fallback -> attempt (fun x _ _ ->
           [fst x.out, call "if" [sym "true"; body_of x; fallback]]))
           (match fallback with Some fallback -> [fallback] | None ->
             [sym "nil"; num 0; sym "false"; vec [num 0; num 0; num 0]; mk (S.Str ""); call "list" []])
       | Map | Filter ->
           let h = if loop = Map then "map" else "filter" in
           let over x iv coll = call h [ call "fn" [ vec [ sym iv ]; body_of x ]; coll ] in
           (* two nodes: one is the collection, the other the body, read per item.  The checker
              picks the order; the source stays a binding of its own. *)
           let from_source = match nodes with
             | [ n1; n2 ] ->
                 List.map (fun (w, c) -> let c = snd (split_node c) in
                   attempt ~nodes:[ w ] (fun x iv _ ->
                     [ fst x.out, over { x with sel = [ fst x.out, replace_ref c (sym iv) (body_of x) ] } iv (sym c) ]))
                   [ n1, n2; n2, n1 ]
             | _ -> [] in
           (* otherwise a named placeholder [items] beside the wrapped node, one literal to edit *)
           from_source @ [ attempt (fun x iv used ->
               let items = fresh used "items" in
               [ sym items, call "range" [ num 6 ]; fst x.out, over x iv (sym items) ]) ]
       | For ->
           (* ponytail: the checker is the type oracle, so try the geometry and
              drawing shapes (collect and merge) and then the number shape (sum) *)
           List.map (fun merge -> attempt (fun x iv used ->
               let each = fresh used (x.out_name ^ "_each") in
               [ sym each, zone "for" iv 6 (body_of x); fst x.out, call merge [ sym each ] ])) [ "sop/merge"; "draw/merge" ]
           @ [ attempt (fun x iv _ -> [ fst x.out, zone "sum" iv 6 (body_of x) ]) ]
       | Fold ->
           (* every outside name the selection reads is a candidate to feed back *)
           let free = ref [] in
           ignore (edit_scope src sp (fun s ->
             free := outside_names ~root:rootf (select s nodes "Iterate"); s));
           if !free = [] then
             [ fun () -> fail "Iterate feeds the result back into an input of the same type; none of the selected nodes reads one from outside." ]
           else List.map (fun f -> attempt (fun x iv used ->
             let prev = fresh used "prev" in
             [ fst x.out, call "fold" [ vec [ sym prev; sym f ]; zone_clause iv 4;
                                        replace_ref f (sym prev) (body_of x) ] ])) !free)
  | Make_local_fn { nodes } -> one (fun () ->
      let sp = scope_path_of nodes in
      let root = List.hd sp in
      let rootf = root_form src root in
      let used = root_used src root in
      edit_scope src sp (fun s ->
        let x = select s nodes "A function" in
        let fns = fn_names rootf in
        let outer = outside_names ~root:rootf x |> List.filter (fun n -> not (List.mem n fns)) in
        if List.mem (Option.value ~default:"" (head_sym (body_of x))) fns then fail "That already calls a local function; open it with i.";
        if outer = [] then fail "A function needs an input: this reads no other node. Wire one in first.";
        let fname = fresh used (x.out_name ^ "_fn") in
        let params = List.map (fun n -> fresh used ("in_" ^ n)) outer in
        let body = List.fold_left2 (fun b n p -> replace_ref n (sym p) b) (body_of x) outer params in
        let added = [ sym fname, call "fn" [ vec (List.map sym params); body ];
                      fst x.out, call fname (List.map sym outer) ] in
        reorder (rebuild x.sc (insert_at x.keep (min x.first (List.length x.keep)) added) x.sc.res)))
  | Make_macro { nodes; name; holes } -> one (fun () ->
      let sp = scope_path_of nodes in
      let root = List.hd sp in
      let rootf = root_form src root in
      let _, items = workspace_parts src in
      let taken = List.filter_map (fun (i : S.t) -> match i.node with
        | S.List ({ S.node = S.Sym ("graph" | "defn" | "defmacro"); _ } :: { S.node = S.Sym n; _ } :: _) -> Some n
        | _ -> None) items in
      if not (Flow.Macro.valid_name name) || List.mem name taken || W.name_taken name then
        fail "Pick a new lowercase macro name.";
      let names = List.map snd holes in
      if List.exists (fun n -> not (valid_name n)) names || List.length (dedup names) <> List.length names then
        fail "Hole names must be distinct lowercase names.";
      let defn = ref None in
      let out = edit_scope src sp (fun s ->
        let x = select s nodes "A macro" in
        let tmpl = body_of x in
        let free = outside_names ~root:rootf x in
        let hole_vals = List.map (fun (path, _) -> match get_sub tmpl path with
          | Some v -> v | None -> fail "A hole is not in the template.") holes in
        let body = List.fold_left (fun b (path, n) -> set_sub b path (unquote (sym n))) tmpl holes in
        let body = List.fold_left (fun b f -> replace_ref f (unquote (sym f)) b) body free in
        let body = if List.length x.sel > 1
          then List.fold_left (fun b n -> replace_ref n (sym (n ^ "#")) b) body (sel_names x) else body in
        let params = names @ free in
        defn := Some (call "defmacro" [ sym name; vec (List.map sym params); mk (S.Quote (S.Quasi, body)) ]);
        let added = [ fst x.out, call name (hole_vals @ List.map sym free) ] in
        reorder (rebuild x.sc (insert_at x.keep (min x.first (List.length x.keep)) added) x.sc.res)) in
      map_items out (fun items -> Option.get !defn :: items))
  | Make_defn { nodes; name; context; params } -> one (fun () ->
      let sp = scope_path_of nodes in
      let root = List.hd sp in
      let rootf = root_form src root in
      let _, items = workspace_parts src in
      let taken = List.filter_map (fun (i : S.t) -> match i.node with
        | S.List ({ S.node = S.Sym ("graph" | "defn" | "defmacro"); _ } :: { S.node = S.Sym n; _ } :: _) -> Some n
        | _ -> None) items in
      if not (valid_name name) || List.mem name taken || W.name_taken name then
        fail "Pick a new lowercase function name.";
      let defn = ref None in
      let out = edit_scope src sp (fun s ->
        let x = select s nodes "A function" in
        let outer = outside_names ~root:rootf x in
        if List.sort compare (List.map fst params) <> List.sort compare outer then
          fail "The function reads %s from outside; each needs a type."
            (if outer = [] then "nothing" else String.concat ", " outer);
        let typed = List.map (fun n ->
          let ty = List.assoc n params in
          mk (S.List [ sym n; sym ":"; sym ty ])) outer in
        defn := Some (call "defn" [ sym name; kwf "context"; sym context; vec typed; body_of x ]);
        let added = [ fst x.out, call name (List.map sym outer) ] in
        reorder (rebuild x.sc (insert_at x.keep (min x.first (List.length x.keep)) added) x.sc.res)) in
      map_items out (fun items -> Option.get !defn :: items))
  | Inline_macro { node } -> one (fun () ->
      let sp, leaf = split_node node in
      let _, items = workspace_parts src in
      let macros = List.filter (fun i -> head_sym i = Some "defmacro") items in
      edit_scope src sp (fun s ->
        let e = get_node s leaf in
        match Flow.Macro.expand macros e with
        | Ok x -> reorder (set_node s leaf x)
        | Error d -> raise (Fail d)))
  | Inline_defn { node } -> one (fun () ->
      let sp, leaf = split_node node in
      let _, items = workspace_parts src in
      edit_scope src sp (fun s ->
        let e = get_node s leaf in
        let name, args = match e.node with
          | S.List ({ S.node = S.Sym n; _ } :: args) -> n, args
          | _ -> fail "Only a call of a function can be inlined." in
        let defn = match List.find_opt (fun i -> head_sym i = Some "defn"
            && (match S.children i with _ :: { S.node = S.Sym n; _ } :: _ -> n = name | _ -> false)) items with
          | Some d -> d | None -> fail "%s is not a function of this workspace." name in
        let parts = List.tl (List.tl (S.children defn)) in
        let params, body = match List.filter (fun (x : S.t) -> match x.node with S.Vec _ -> true | _ -> false) parts,
            List.rev parts with
          | { S.node = S.Vec ps; _ } :: _, body :: _ -> ps, body
          | _ -> fail "%s has no body to inline." name in
        let names = List.map (fun (p : S.t) -> match p.node with
          | S.List ({ S.node = S.Sym n; _ } :: _) | S.Sym n -> n | _ -> fail "%s has a parameter that is not a name." name) params in
        if List.length names <> List.length args then fail "%s is called with a different number of inputs." name;
        (* two passes, so an argument that is a name of another parameter is not substituted again *)
        let tmp i = "inl#" ^ string_of_int i in
        let body = List.fold_left (fun b (i, n) -> replace_ref n (sym (tmp i)) b) body (List.mapi (fun i n -> i, n) names) in
        let body = List.fold_left (fun b (i, a) -> replace_ref (tmp i) a b) body (List.mapi (fun i a -> i, a) args) in
        reorder (set_node s leaf body)))
  | Move_binding { node; by } -> one (fun () ->
      let sp, leaf = split_node node in
      edit_scope src sp (fun s ->
        let sc = match scope_of s with Some sc when not (nested leaf) && leaf <> "@result" -> sc
          | _ -> fail "Only a named binding can move." in
        let j = first_pair_index sc leaf in
        let k = j + by in
        if k < 0 || k >= List.length sc.ps then fail "%s cannot move further." leaf;
        let a = Array.of_list sc.ps in
        let t = a.(j) in a.(j) <- a.(k); a.(k) <- t;
        let moved = rebuild sc (Array.to_list a) sc.res in
        let keys x = match scope_of x with Some sc -> List.map (fun (p, _) -> pat_key p) sc.ps | None -> [] in
        let r = reorder moved in
        if keys r <> keys moved then fail "%s is read by or reads its neighbour; it cannot pass it." leaf;
        r))
  | Move_step { node; earlier } -> one (fun () ->
      let sp, leaf = split_node node in
      edit_scope src sp (fun s ->
        (* the later call of the two trades places with the call in its chain input *)
        let outer = if earlier then leaf
          else if nested leaf && snd (holder leaf) = chain_in (get_node s (fst (holder leaf))) then fst (holder leaf)
          else fail "That is the last step of its chain." in
        let o = get_node s outer in
        let ko = chain_in o in
        let i = match arg_get o ko with Some i when node_call i -> i | _ -> fail "That is the first step of its chain." in
        let ki = chain_in i in
        if arg_get i ki = None then fail "That is the first step of its chain.";
        (* (o .. (i .. x ..) ..) becomes (i .. (o .. x ..) ..) *)
        set_node s outer (arg_set i ki (Some (arg_set o ko (arg_get i ki))))))
  | Toggle_bypass { node } -> one (fun () ->
      let sp, leaf = split_node node in
      edit_scope src sp (fun s ->
        let e = get_node s leaf in
        (match e.node, head_sym e with
         | S.List _, Some h when not (is_zone e) && h <> "let*" && h <> "fn" -> ()
         | _ -> fail "Only an operator or function call can be bypassed.");
        let meta = if List.mem "bypass" e.meta then List.filter (( <> ) "bypass") e.meta else e.meta @ [ "bypass" ] in
        set_node s leaf { e with S.meta = meta }))
  | Set_note { node; text } -> one (fun () ->
      let notes = note_lines text in
      match node with
      | [ root ] -> with_root src root (fun r -> { r with S.notes = notes })
      | _ ->
          let sp, leaf = split_node node in
          if nested leaf then fail "Name the node first (rename it): notes attach to named bindings.";
          edit_scope src sp (fun s ->
            match scope_of s with
            | Some sc when leaf = "@result" -> rebuild sc sc.ps { sc.res with S.notes = notes }
            | Some sc ->
                let j = first_pair_index sc leaf in
                rebuild sc (List.mapi (fun k (p, v) -> if k = j then { p with S.notes = notes }, v else p, v) sc.ps) sc.res
            | None when leaf = "@result" -> { s with S.notes = notes }
            | None -> fail "Notes attach to named bindings."))
  | Add_item { node } -> one (fun () ->
      let sp, leaf = split_node node in
      edit_scope src sp (fun s ->
        let e = get_node s leaf in
        match e.node with
        | S.List (({ S.node = S.Sym ("list" | "str" as h); _ } as hd) :: args) ->
            let v = if h = "str" then mk (S.Str " ") else
              match List.rev args with
              | ({ S.node = S.Num a; _ } as last) :: { S.node = S.Num b; _ } :: _ ->
                  (match float_of_string_opt a, float_of_string_opt b with
                   | Some fa, Some fb ->
                       let x = Float.round ((2. *. fa -. fb) *. 1000.) /. 1000. in
                       if Float.is_integer fa then mk (S.Num (string_of_int (int_of_float x)))
                       else
                         mk (S.Num (Flow.Lisp.float x))
                   | _ -> { last with notes = [] })
              | last :: _ -> { last with notes = [] }
              | [] -> mk (S.Num "0") in
            set_node s leaf { e with node = S.List (hd :: args @ [ v ]) }
        | _ -> fail "Only a list or str can take another item."))
  | Move_item { node; pos } -> one (fun () ->
      let sp, leaf = split_node node in
      edit_scope src sp (fun s ->
        let e = get_node s leaf in
        match e.node with
        | S.List (h :: args) when pos >= 1 && pos < List.length (positional args) ->
            (* the items are the positional arguments: keyword pairs stay where they are written *)
            let items = Array.of_list (positional args) in
            let rec go k = function
              | a :: b :: r when is_kw a -> a :: b :: go k r
              | _ :: r -> items.(if k = pos then pos - 1 else if k = pos - 1 then pos else k) :: go (k + 1) r
              | [] -> [] in
            let args = go 0 args in
            (* a [scene/merge]'s [:skip] tuples end in an argument position: they follow the items *)
            let args = if head_sym e <> Some "scene/merge" || skip_of_args args = [] then args else
              with_kw args "skip" (Some (skip_value (List.map (fun t -> match List.rev t with
                | p :: outer -> List.rev ((if p = pos then pos - 1 else if p = pos - 1 then pos else p) :: outer)
                | [] -> t) (skip_of_args args)))) in
            set_node s leaf { e with node = S.List (h :: args) }
        | _ -> fail "There is no item %d to move up." pos))
  | Add_field { node; name; value } -> one (fun () ->
      let sp, leaf = split_node node in
      edit_scope src sp (fun s ->
        let e = get_node s leaf in
        let key = match e.node with S.Map _ -> Field name | _ -> Kw name in
        if arg_get e key <> None then fail "There is a field %s already." name;
        reorder (set_node s leaf (arg_set e key (Some value)))))
  | Add_node { scope; name; expr } -> one (fun () ->
      if not (valid_name name) || List.mem name (sym_list (root_form src (List.hd scope)))
         || W.name_taken name then fail "Pick a new lowercase name that is not used in this graph.";
      edit_scope src scope (fun s ->
        let sc = ensure s in
        let ps = sc.ps @ [ sym name, expr ] in
        let body, expr = if List.length scope = 1 then attach sc ps name expr else rebuild sc ps sc.res, expr in
        let body = match scope_of body with
          | Some b -> rebuild b (List.map (fun (p, v) -> if p.S.node = S.Sym name then p, expr else p, v) b.ps) b.res
          | None -> body in
        reorder body))

  | Set_layout_size { node; size } -> one (fun () ->
      let sp, leaf = split_node node in
      edit_scope src sp (fun s ->
        let e = get_node s leaf in
        let axis, a, b = match head_sym e, e.node with
          | Some ("ui/split" | "ui/split-at"), S.List (_ :: args) ->
              (match positional args, layout_kids e with
               | axis :: _, [ a; b ] -> axis, a, b
               | _ -> fail "That panel is not a split.")
          | _ -> fail "That panel is not a split." in
        (* one side fixed in whole points, or a ratio with four decimals *)
        let fixed key n = call "ui/split" [ axis; a; b; kwf key; num (max 1 n) ] in
        set_node s leaf (keep_notes e (match size with
          | `First n -> fixed "first_size" n
          | `Second n -> fixed "second_size" n
          | `Ratio r ->
              call "ui/split-at" [ axis; mk (S.Num (ratio_text r)); a; b ]))))
  | Split_panel { node; axis; ratio; kind } -> one (fun () ->
      let sp, leaf = split_node node in
      let used = root_used src (List.hd sp) in
      edit_scope src sp (fun s ->
        let sc = match scope_of s with
          | Some sc -> sc
          | None -> fail "This editor graph is a single expression. Edit it in Lisp." in
        let j = first_pair_index sc leaf in
        let a = fresh used (leaf ^ "_a") in
        let b = fresh used (leaf ^ "_b") in
        let p, orig = List.nth sc.ps j in
        let other = if head_sym orig = Some "ui/lisp" then "ui/graph" else "ui/lisp" in
        let ratio = ratio_text ratio in
        let split = call "ui/split-at"
          [ mk (S.Str (if axis = `H then "horizontal" else "vertical")); mk (S.Num ratio);
            sym a; sym b ] in
        let before = List.filteri (fun k _ -> k < j) sc.ps
        and after = List.filteri (fun k _ -> k > j) sc.ps in
        reorder (rebuild sc (before @ [ sym a, orig; sym b, (match kind with Some kind -> panel_expr src kind | None -> call other []); p, split ] @ after) sc.res)))
  | Close_panel { node } -> one (fun () ->
      let sp, leaf = split_node node in
      edit_scope src sp (fun s ->
        let sc = match scope_of s with
          | Some sc -> sc
          | None -> fail "This editor graph is a single expression. Edit it in Lisp." in
        ignore (get_node s leaf);
        let ps, res, _ = strip_panel ~keep:false sc leaf in
        reorder (rebuild sc ps res)))
  | Dock_panel { node; target; side } -> one (fun () ->
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
        let split = call "ui/split-at" [mk (S.Str (if side = `Top || side = `Bottom then "vertical" else "horizontal"));
          mk (S.Num "0.5"); sym (if first then leaf else target); sym (if first then target else leaf)] in
        let ps = List.map (fun (p, e) -> p,
          (if pat_key p = target then e else rename_place target group e)) ps in
        reorder (rebuild sc (ps @ [sym group, split]) (rename_place target group res))))
  | Duplicate { nodes } -> one (fun () ->
      let sp, names = duplicate_plan src nodes in
      edit_scope src sp (fun s ->
        let sc = ensure s in
        let selected = List.filter (fun (p, _) -> List.mem_assoc (pat_key p) names) sc.ps in
        if List.length selected <> List.length names then fail "A selected node no longer exists.";
        let rename e = List.fold_left (fun e (old, fresh) -> rename_ref old fresh e) e names in
        let copies = List.map (fun (p, e) -> match p.S.node with
          | S.Sym n -> { p with S.node = S.Sym (List.assoc n names); notes = [] }, rename e
          | _ -> fail "Only a named node can be duplicated.") selected in
        let last = List.fold_left max 0 (List.filter_map (fun (p, _) -> find_pair sc (pat_key p)) selected) in
        reorder (rebuild sc (insert_at sc.ps (last + 1) copies) sc.res)))
  | Group_merge { nodes; name } -> one (fun () ->
      (* the selected scene objects leave their merge for a new merge [name], which takes the
         place of the first of them *)
      let sp = scope_path_of nodes in
      let leaves = dedup (List.map (fun n -> snd (split_node n)) nodes) in
      if List.length leaves < 2 then fail "Select two or more objects to group.";
      if not (valid_name name) || List.mem name (sym_list (root_form src (List.hd sp))) || W.name_taken name then
        fail "Pick a new lowercase name that is not used in this graph.";
      let first = ref true in
      let rec regroup (e : S.t) : S.t =
        let e = map_children regroup e in
        match head_sym e with
        | Some "scene/merge" ->
            let picked = List.filter_map Fun.id (List.mapi (fun i (c : S.t) -> match c.node with
              | S.Sym n when List.mem n leaves -> Some i | _ -> None) (positional (List.tl (S.children e)))) in
          (match picked with
           | [] -> e
           | at :: rest ->
               let e = if !first then arg_set e (Pos at) (Some (sym name)) else arg_set e (Pos at) None in
               first := false;
               List.fold_left (fun e i -> arg_set e (Pos i) None) e (List.rev rest))
        | _ -> e in
      let grouped = with_root src (List.hd sp) regroup in
      if !first then fail "None of those objects is in a merge.";
      edit_scope grouped sp (fun s ->
        let sc = ensure s in
        reorder (rebuild sc (sc.ps @ [ sym name, call "scene/merge" (List.map sym leaves) ]) sc.res)))
  | Remove_graph { name } -> one (fun () ->
      let items = snd (workspace_parts src) in
      if not (List.exists (fun i -> root_name i = Some name) items) then
        fail "No graph or definition %s." name;
      (match graph_readers items name with
       | [] -> ()
       | readers -> fail "%s is still read by %s." name (String.concat ", " readers));
      map_items src (List.filter (fun i -> root_name i <> Some name)))
  | Rename_graph { name; to_ } -> one (fun () ->
      let items = snd (workspace_parts src) in
      if not (List.exists (fun i -> root_name i = Some name) items) then fail "No graph %s." name;
      if List.exists (fun i -> root_name i = Some to_) items then fail "%s is already a graph." to_;
      map_items src (List.map (fun item ->
        let item = rename_graph_ref name to_ item in
        if root_name item <> Some name then item
        else match item.node with
          | S.List (h :: n :: rest) -> { item with node = S.List (h :: { n with node = S.Sym to_ } :: rest) }
          | _ -> item)))
  | Set_graph { name; form } -> one (fun () ->
      (* the whole [(graph name ...)] form: replaced, or appended when the workspace has none *)
      if root_name form <> Some name then fail "That form is not the graph %s." name;
      let exists = List.exists (fun i -> root_name i = Some name) (snd (workspace_parts src)) in
      map_items src (fun items ->
        if exists then List.map (fun i -> if root_name i = Some name then form else i) items
        else items @ [ form ]))
  | Set_panel_kind { node; kind } -> one (fun () ->
      let sp, leaf = split_node node in
      let expr = panel_expr src kind in
      edit_scope src sp (fun s -> set_node s leaf expr))
  | Set_layout { graph; index } -> one (fun () ->
      edit_scope src [ graph ] (fun s ->
        let sc = editor_scope s in
        match switch_place sc with
        | None -> fail "This workspace has one layout and no switch."
        | Some (place, sw) ->
            if index < 0 || index >= List.length (positional (List.tl (S.children sw))) then
              fail "There is no layout %d." index;
            set_switch sc sc.ps place (arg_set sw (Kw "active") (Some (num index)))))
  | Layout_new { graph } -> one (fun () ->
      let used = root_used src graph in
      edit_scope src [ graph ] (fun s ->
        let sc = editor_scope s in
        (* a workspace without a switch gets one around its tree *)
        let sc, place, sw = match switch_place sc with
          | Some (place, sw) -> sc, place, sw
          | None ->
              let tree = workspace_arg sc in
              let ps, kid = match tree.node with
                | S.Sym _ -> sc.ps, tree
                | _ -> let n = fresh used "layout" in sc.ps @ [ sym n, tree ], sym n in
              let name = fresh used "switch" and sw = call "ui/switch" [ kid ] in
              set_workspace sc (ps @ [ sym name, sw ]) (sym name), Some name, sw in
        let args = List.tl (S.children sw) in
        let n = List.length (positional args) in
        if n >= 10 then fail "Ten layouts is the limit of the digit keys.";
        let copy = solid sc.ps (active_layout args) in
        let name = fresh used "layout" in
        let sw = arg_set (arg_set sw (Pos n) (Some (sym name))) (Kw "active") (Some (num n)) in
        reorder (set_switch sc (sc.ps @ [ sym name, copy ]) place sw)))
  | Merge_layouts { graph } -> one (fun () ->
      (* an older file: the other [:context editor] graphs become layouts of a switch in [graph]; each
         brings its bindings, renamed [graph_name] so they cannot clash, and then goes *)
      let editor_graph (i : S.t) = match i.node with
        | S.List ({ S.node = S.Sym "graph"; _ } :: { S.node = S.Sym _; _ } :: rest) ->
            let rec has = function
              | { S.node = S.Kw "context"; _ } :: { S.node = S.Sym "editor"; _ } :: _ -> true
              | _ :: rest -> has rest
              | [] -> false in
            has rest
        | _ -> false in
      let others = List.filter_map (fun i -> match root_name i with
        | Some n when n <> graph && editor_graph i -> Some (n, i) | _ -> None) (snd (workspace_parts src)) in
      if others = [] then fail "There is one editor graph and nothing to merge.";
      let used = root_used src graph in
      let merged = edit_scope src [ graph ] (fun s ->
        let sc = editor_scope s in
        let sc, place, sw = match switch_place sc with
          | Some (place, sw) -> sc, place, sw
          | None ->
              let tree = workspace_arg sc in
              let ps, kid = match tree.node with
                | S.Sym _ -> sc.ps, tree
                | _ -> let n = fresh used "layout" in sc.ps @ [ sym n, tree ], sym n in
              let name = fresh used "switch" and sw = call "ui/switch" [ kid ] in
              set_workspace sc (ps @ [ sym name, sw ]) (sym name), Some name, sw in
        let ps, sw = List.fold_left (fun (ps, sw) (name, item) ->
          let theirs = editor_scope (last_child item) in
          let names = List.map (fun (p, _) -> pat_key p, fresh used (name ^ "_" ^ pat_key p)) theirs.ps in
          let rename e = List.fold_left (fun e (old, nw) -> rename_ref old nw e) e names in
          let own = List.map (fun (p, e) -> sym (List.assoc (pat_key p) names), rename e) theirs.ps in
          let tree = rename (workspace_arg theirs) in
          let ps, kid = match tree.node with
            | S.Sym _ -> ps @ own, tree
            | _ -> let n = fresh used name in ps @ own @ [ sym n, tree ], sym n in
          if List.length (positional (List.tl (S.children sw))) >= 10 then
            fail "Ten layouts is the limit of the digit keys.";
          ps, arg_set sw (Pos (List.length (positional (List.tl (S.children sw))))) (Some kid)) (sc.ps, sw) others in
        reorder (set_switch sc ps place sw)) in
      map_items merged (List.filter (fun i -> match root_name i with
        | Some n -> not (List.mem_assoc n others) | None -> true)))
  | Layout_remove { graph } -> one (fun () ->
      edit_scope src [ graph ] (fun s ->
        let sc = editor_scope s in
        match switch_place sc with
        | None -> fail "This workspace has one layout and no switch."
        | Some (place, sw) ->
            let args = List.tl (S.children sw) in
            let n = List.length (positional args) and a = active_of args in
            if n < 2 then fail "A switch keeps its last layout.";
            let gone = active_layout args in
            let sw = arg_set (arg_set sw (Pos a) None) (Kw "active") (Some (num (min a (n - 2)))) in
            prune (set_switch sc sc.ps place sw) (reach sc.ps gone)))
  | Layout_window { graph; kind } -> one (fun () ->
      let panel = panel_expr src kind in
      edit_layout src graph (fun _ layout ->
        call "ui/split-at" [ mk (S.Str "horizontal"); mk (S.Num "0.5"); layout; call "ui/floating" [ panel ] ]))
  | Layout_float { graph; at } -> one (fun () ->
      edit_layout src graph (fun ps layout ->
        let tree = solid ps layout in
        let up = match List.rev at with _ :: up -> List.rev up | [] -> [] in
        if up <> [] && head_sym (node_at up tree) = Some "ui/floating" then
          (* a window docks beside the rest *)
          match take up tree with
          | Some rest, window ->
              call "ui/split-at" [ mk (S.Str "horizontal"); mk (S.Num "0.7"); rest; List.hd (layout_kids window) ]
          | None, window -> List.hd (layout_kids window)
        else match take at tree with
          | Some rest, panel when has_docked rest ->
              call "ui/split-at" [ mk (S.Str "horizontal"); mk (S.Num "0.5"); rest; call "ui/floating" [ panel ] ]
          | _ -> fail "A layout keeps one docked panel."))

(* ---- the public functions ---- *)

let label = function
  | Set_arg _ -> "Edit value" | Connect _ -> "Connect" | Disconnect _ -> "Disconnect"
  | Set_input_default _ -> "Input default" | Unfold _ -> "Unfold" | Fold_into _ -> "Fold"
  | Wrap { loop = For; _ } -> "Repeat" | Wrap { loop = Fold; _ } -> "Iterate"
  | Wrap { loop = Map; _ } -> "Wrap in map" | Wrap { loop = Filter; _ } -> "Wrap in filter"
  | Inline_defn _ -> "Inline function" | Move_binding _ -> "Move binding" | Move_step _ -> "Move step"
  | Wrap {loop = If; _} -> "Wrap conditional" | Add_arm _ -> "Add arm" | Delete_arm _ -> "Delete arm"
  | Hoist _ -> "Move out" | Rename _ -> "Rename" | Make_local_fn _ -> "Make function"
  | Make_defn _ -> "Make reusable function"
  | Make_macro _ -> "Make macro" | Inline_macro _ -> "Inline macro"
  | Toggle_bypass _ -> "Bypass" | Set_note _ -> "Note" | Add_item _ -> "Add item"
  | Move_item _ -> "Move item" | Add_field _ -> "Add field" | Add_node _ -> "Add node"
  | Delete_nodes _ -> "Delete"
  | Set_layout_size _ -> "Resize panel" | Split_panel _ -> "Split panel"
  | Close_panel _ -> "Close panel" | Set_panel_kind _ -> "Retype panel"
  | Dock_panel _ -> "Dock panel"
  | Set_graph _ -> "Edit graph"
  | Duplicate _ -> "Duplicate"
  | Remove_graph _ -> "Remove graph"
  | Rename_graph _ -> "Rename graph"
  | Group_merge _ -> "Group"
  | Set_layout _ -> "Layout" | Merge_layouts _ -> "Merge layouts" | Layout_new _ -> "New layout" | Layout_remove _ -> "Remove layout"
  | Layout_window _ -> "New window" | Layout_float _ -> "Float panel"

let key_text = function
  | Whole -> "" | Pos i -> string_of_int i | Kw k | Field k -> k | Bv (i, j) -> Printf.sprintf "%d.%d" i j
  | Arm i -> if i < 0 then "else" else Printf.sprintf "then%d" i

let gesture = function
  | Set_arg { node; key; sub; _ } ->
      Some (Printf.sprintf "scrub:%s:%s:%s" (String.concat "/" node) (key_text key)
        (String.concat "." (List.map string_of_int sub)))
  | Set_note { node; _ } -> Some ("note:" ^ String.concat "/" node)
  | Set_layout _ -> Some "layout"
  | Set_input_default { form; input; _ } -> Some (Printf.sprintf "scrub:input:%s:%s" form input)
  | _ -> None

type macro_draft = { literals : (int list * S.t) list; free : string list; name : string }

let macro_draft src nodes =
  try
    let sp = scope_path_of nodes in
    let root = List.hd sp in
    let rootf = root_form src root in
    let draft = ref None in
    ignore (edit_scope src sp (fun s ->
      let x = select s nodes "A macro" in
      draft := Some { literals = literals (body_of x); free = outside_names ~root:rootf x;
                      name = fresh_name src ~root (x.out_name ^ "_tpl") };
      s));
    Ok (Option.get !draft)
  with Fail d -> Error d

type defn_draft = { free : string list; name : string }

let defn_draft src nodes =
  try
    let sp = scope_path_of nodes in
    let root = List.hd sp in
    let rootf = root_form src root in
    let draft = ref None in
    ignore (edit_scope src sp (fun s ->
      let x = select s nodes "A function" in
      draft := Some { free = outside_names ~root:rootf x; name = fresh_name src ~root x.out_name };
      s));
    Ok (Option.get !draft)
  with Fail d -> Error d

let macro_op draft ~nodes ~name choices =
  Make_macro { nodes; name; holes = List.filteri (fun i _ -> i < Array.length choices) draft.literals
    |> List.mapi (fun i (path, _) -> path, choices.(i))
    |> List.filter_map (fun (path, (on, hole)) -> if on then Some (path, hole) else None) }

let check ?ops ?library ?checker catalog forms =
  let text, _ = Flow.Lisp.print forms in
  match S.parse text with
  | Error d -> Error d
  | Ok forms ->
      (match (Option.value checker ~default:(W.check ?ops ?library)) catalog forms with
       | Some ws, _ -> Ok (forms, ws)
       | None, ds ->
           Error (match List.find_opt (fun (d : Flow.Diagnostic.t) -> d.severity = Flow.Diagnostic.Error) ds with
             | Some d -> d
             | None -> Flow.Diagnostic.error ~code:"E_EDIT" "The edit does not check."))

(* the last resort: whatever a rewrite raises ([List.nth], [Option.get], [Failure]) is a refused
   edit, never a dead editor *)
let refusal = function
  | Fail d -> d
  | (Out_of_memory | Sys.Break) as e -> raise e
  | e -> Flow.Diagnostic.error ~code:"E_EDIT" ("The edit failed: " ^ Printexc.to_string e)

let apply_checked ?ops ?library ?checker catalog src op =
  let candidates () =
  let path = match op with
    | Wrap {nodes; loop = If} ->
        let path = ref None and scope = scope_path_of nodes in
        (try ignore (edit_scope src scope (fun s ->
          let selection = select s nodes "Wrap conditional" in
          path := Some (scope @ [selection.out_name]); s)) with Fail _ -> ());
        !path
    | Add_arm {node; _} -> Some node | _ -> None in
  let fallback = Option.bind path (fun path ->
    let rec find (term : W.term) =
      if term.path = Some path then Some term.ty else
      let children = match term.node with
        | Vec xs | List_lit xs | Str xs | List_op (_,xs) | Hof (_,xs) -> xs
        | Call {args;_} | Op {args;_} -> List.map snd args
        | Call_fn {args;_} -> args | Graph_ref {inputs;_} -> List.map snd inputs
        | Let (bs,r) -> List.map snd bs @ [r] | State {init;step;_} -> [init;step]
        | Loop {accs;clauses;body;_} -> List.map snd accs @ List.map snd clauses @ [body]
        | With_error(a,b) -> [a;b]
        | Error_rows{rows=a;mapper=b;_} -> [a;b]
        | If (a,b,c) -> [a;b;c] | Cond (arms,d) -> List.concat_map (fun (a,b) -> [a;b]) arms @ [d]
        | Case (s,arms,d) -> s :: List.map snd arms @ [d] | Fn {body;_} -> [body]
        | Record fs -> List.map snd fs | Assoc (r,fs) -> r :: List.map snd fs
        | Get (r,_) | Bypass r | Expanded {body = r; _} -> [r]
        | _ -> [] in List.find_map find children in
    match (Option.value checker ~default:(W.check ?ops ?library)) catalog src with
    | Some ws, _ -> Option.bind (List.find_map (fun (g : W.graph) -> find g.body) (ws.graphs @ ws.defs))
        branch_default
    | _ -> None) in
  rewrite ?fallback src op in
  match candidates () with
  | exception e -> Error (refusal e)
  | candidates ->
      let rec first err = function
        | [] -> Error (Option.get err)
        | attempt :: rest ->
            (match attempt () with
             | exception e -> first (if err = None then Some (refusal e) else err) rest
             | forms -> (match check ?ops ?library ?checker catalog forms with
                 | Ok _ as ok -> ok
                 | Error d -> first (if err = None then Some d else err) rest)) in
      first None candidates

let apply catalog src op = Result.map fst (apply_checked catalog src op)

let has_prefix ~prefix p =
  let n = List.length prefix in
  List.length p >= n && List.filteri (fun i _ -> i < n) p = prefix

(* the leaf of [p] at the depth of [node]'s, when [p] is [node], a node nested in it, or below either *)
let under node p =
  let k = List.length node - 1 in
  if List.length p > k && has_prefix ~prefix:(List.filteri (fun i _ -> i < k) node) p then
    let leaf = List.nth node k and at = List.nth p k in
    if at = leaf then Some (k, "")
    else if String.starts_with ~prefix:(leaf ^ "#") at then
      Some (k, String.sub at (String.length leaf) (String.length at - String.length leaf))
    else None
  else None
let relabel k leaf p = List.mapi (fun i s -> if i = k then leaf else s) p

let remap op p = match op with
  | Add_arm {node; after} | Delete_arm {node; index = after} ->
      let removed = match op with Delete_arm _ -> true | _ -> false in
      (match under node p with
       | Some (depth, suffix) when String.starts_with ~prefix:"#then" suffix ->
           let tail = String.sub suffix 5 (String.length suffix - 5) in
           let digits, rest = match String.index_opt tail '#' with
             | Some i -> String.sub tail 0 i, String.sub tail i (String.length tail - i)
             | None -> tail, "" in
           let arm = if digits = "" then 0 else int_of_string (String.sub digits 1 (String.length digits - 1)) - 1 in
           if removed && arm = after then None else
           let arm = if removed && arm > after then arm - 1 else if not removed && arm > after then arm + 1 else arm in
           let key = if arm = 0 then "#then" else "#then~" ^ string_of_int (arm + 1) in
           Some (relabel depth (List.nth node depth ^ key ^ rest) p)
       | _ -> Some p)
  | Rename { node; to_ } when under node p <> None ->
      let k, rest = Option.get (under node p) in
      Some (relabel k (to_ ^ rest) p)
  | Delete_nodes { nodes } when List.exists (fun n -> nested (snd (split_node n)) && under n p <> None) nodes ->
      (* the nodes nested in a deleted nested node: the first input moves up to its place *)
      let n = List.find (fun n -> nested (snd (split_node n)) && under n p <> None) nodes in
      (match under n p with
       | Some (k, rest) when String.starts_with ~prefix:"#0" rest ->
           Some (relabel k (List.nth n k ^ String.sub rest 2 (String.length rest - 2)) p)
       | _ -> None)
  | Rename_graph { name; to_ } when has_prefix ~prefix:[ name ] p -> Some (to_ :: List.tl p)
  | Hoist { node } when has_prefix ~prefix:node p && List.length node >= 3 ->
      let k = List.length node - 2 in
      Some (List.filteri (fun i _ -> i <> k) p)
  | Delete_nodes { nodes } when List.exists (fun n -> has_prefix ~prefix:n p) nodes -> None
  | Close_panel { node } when has_prefix ~prefix:node p -> None
  | _ -> Some p

let arg_of = arg_get

let nested_nodes (e : S.t) = match e.node with
  | S.List (_ :: args) when e.meta <> [] || not (is_zone e) ->
      let args = S.attribute_args (Option.value ~default:"" (S.head e)) args in
      List.filter (fun (_, a) -> node_call a)
        (List.mapi (fun i a -> Pos i, a) (positional args) @ List.map (fun (k, v) -> Kw k, v) (keywords args))
  | _ -> []

let leaf_keys leaf = match split_leaf leaf with
  | exception Fail _ -> None
  | parts -> Some parts

let arg_text src node key =
  let sp, leaf = split_node node in
  let found = ref None in
  (try ignore (edit_scope src sp (fun s -> found := arg_get (get_node s leaf) key; s)) with Fail _ -> ());
  !found

let duplicated src nodes =
  match duplicate_plan src nodes with
  | exception Fail _ -> []
  | sp, names -> List.map (fun (_, fresh) -> sp @ [ fresh ]) names

let free_names e = dedup (free e)
let pat_names = pat_names
let pat_key = pat_key
