open Edit_base

(* the call, loop or scope written in input [key] of [leaf] becomes a binding just before the
   binding that held it; the scope with it, and its name *)
let unfold_in ~used ?name s leaf key sub =
  let e = get_node s leaf in
  let whole = match arg_get e key with Some w -> w | None -> fail "That input is not set." in
  let inner =
    match get_sub whole sub with Some i -> i | None -> fail "That part does not exist."
  in
  let h =
    match head_sym inner with
    | Some h when h <> "ref" -> h
    | _ -> fail "Only a call, loop or scope can be unfolded."
  in
  let base = fst (split_leaf leaf) in
  let name =
    match name with
    | Some n -> n
    | None ->
        fresh used
          (if is_zone inner then
             if base = "@result" then "each" else base ^ "_" ^ if h = "sum" then "total" else "each"
           else if h = "let*" then "block"
           else value_base h)
  in
  let e' = arg_set e key (Some (set_sub whole sub (sym name))) in
  let sc = ensure (set_node s leaf e') in
  let at = if base = "@result" then List.length sc.ps else first_pair_index sc leaf in
  (rebuild sc (insert_at sc.ps at [ (sym name, inner) ]) sc.res, name)

(* a wire never deletes a node: the nested node an input held stays, as a binding nothing reads *)
let keep_nested ?(pick = fun a -> node_call a || value_call a) ~used s leaf key =
  match key with
  | Pos _ | Kw _ | Arm _ | Cell _ -> (
      match arg_get (get_node s leaf) key with
      | Some a when pick a -> fst (unfold_in ~used s leaf key [])
      | _ -> s)
  | _ -> s
