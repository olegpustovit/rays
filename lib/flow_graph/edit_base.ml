module S = Flow.Syntax
module W = Flow.Workspace

type path = W.path
type arg_key = Whole | Pos of int | Kw of string | Field of string | Bv of int * int | Arm of int
type loop = For | Fold | If | Map | Filter

type op =
  | Set_arg of { node : path; key : arg_key; sub : int list; value : S.t }
  | Connect of { node : path; key : arg_key; src : string; iter : bool }
  | Disconnect of { node : path; key : arg_key; fallback : S.t option }
  | Set_input_default of { form : string; input : string; value : S.t }
  | Unfold of { node : path; key : arg_key; sub : int list }
  | Fold_into of { node : path }
  | Add_arm of { node : path; after : int }
  | Delete_arm of { node : path; index : int }
  | Wrap of { nodes : path list; loop : loop }
  | Hoist of { node : path }
  | Rename of { node : path; to_ : string }
  | Make_local_fn of { nodes : path list }
  | Make_defn of {
      nodes : path list;
      name : string;
      context : string;
      params : (string * string) list;
    }
  | Make_macro of { nodes : path list; name : string; holes : (int list * string) list }
  | Inline_macro of { node : path }
  | Inline_defn of { node : path }
  | Move_binding of { node : path; by : int }
  | Move_step of { node : path; earlier : bool }
  | Toggle_bypass of { node : path }
  | Set_note of { node : path; text : string }
  | Add_item of { node : path }
  | Move_item of { node : path; pos : int }
  | Add_field of { node : path; name : string; value : S.t }
  | Add_node of { scope : path; name : string; expr : S.t }
  | Delete_nodes of { nodes : path list }
  | Set_graph of { name : string; form : S.t }
  | Duplicate of { nodes : path list }
  | Remove_graph of { name : string }
  | Rename_graph of { name : string; to_ : string }
  | Group_merge of { nodes : path list; name : string }

let fail ?(code = "E_EDIT") fmt =
  Printf.ksprintf (fun message -> raise (Diagnostic.Error (Diagnostic.error ~code message))) fmt

(* ---- syntax helpers ---- *)

let mk ?notes ?meta node = S.make ?notes ?meta node
let sym s = mk (S.Sym s)
let kwf s = mk (S.Kw s)
let vec l = mk (S.Vec l)
let call head args = mk (S.List (sym head :: args))
let num n = mk (S.Num (string_of_int n))
let unquote x = mk (S.Quote (S.Unquote, x))
let pairs = S.pairs
let flat_pairs ps = List.concat_map (fun (a, b) -> [ a; b ]) ps
let head_sym = S.head

let map_children f (e : S.t) : S.t =
  {
    e with
    node =
      (match e.node with
      | S.List l -> S.List (List.map f l)
      | S.Vec l -> S.Vec (List.map f l)
      | S.Map l -> S.Map (List.map f l)
      | S.Quote (k, x) -> S.Quote (k, f x)
      | n -> n);
  }

let root_of s = match String.index_opt s '.' with Some i -> String.sub s 0 i | None -> s

let rec syms (e : S.t) acc =
  match e.node with
  | S.Sym s -> root_of s :: acc
  | S.List ({ S.node = S.Sym "ref"; _ } :: { S.node = S.Sym _; _ } :: rest) ->
      (* [(ref graph ...)] names a graph, never a binding, so a binding of that name is not read *)
      List.fold_left (fun a c -> syms c a) acc rest
  | _ -> List.fold_left (fun a c -> syms c a) acc (S.children e)

let sym_list e = List.rev (syms e [])
let dedup l = List.fold_left (fun acc x -> if List.mem x acc then acc else acc @ [ x ]) [] l
let count_refs name e = List.length (List.filter (( = ) name) (sym_list e))

let rec replace_ref name (by : S.t) (e : S.t) : S.t =
  match e.node with S.Sym s when s = name -> by | _ -> map_children (replace_ref name by) e

let rec rename_ref old nw (e : S.t) : S.t =
  match e.node with
  | S.Sym s when root_of s = old ->
      {
        e with
        node = S.Sym (nw ^ String.sub s (String.length old) (String.length s - String.length old));
      }
  | _ -> map_children (rename_ref old nw) e

(* A panel's place in the layout moves to [new]; an [:of old] still follows the panel itself. *)
let rec rename_place old nw (e : S.t) : S.t =
  match e.node with
  | S.List items ->
      let rec go = function
        | ({ S.node = S.Kw "of"; _ } as k) :: v :: rest -> k :: v :: go rest
        | x :: rest -> rename_place old nw x :: go rest
        | [] -> []
      in
      { e with node = S.List (go items) }
  | _ -> rename_ref old nw e

(* [(ref old ...)] reads [new]: only the reference, never a binding of the same name *)
let rec rename_graph_ref old nw (e : S.t) : S.t =
  match e.node with
  | S.List (({ S.node = S.Sym "ref"; _ } as r) :: ({ S.node = S.Sym n; _ } as s) :: rest)
    when n = old ->
      {
        e with
        node = S.List (r :: { s with node = S.Sym nw } :: List.map (rename_graph_ref old nw) rest);
      }
  | _ -> map_children (rename_graph_ref old nw) e

let rec pat_names (p : S.t) =
  match p.node with
  | S.Sym n -> [ n ]
  | S.Vec l -> List.concat_map pat_names l
  | S.Map l ->
      List.concat_map
        (fun (x : S.t) -> match x.node with S.Vec v -> List.concat_map pat_names v | _ -> [])
        l
  | _ -> []

let pat_key = Flow.Lisp.flat

(* every name a form declares: let and zone bindings, fn parameters, typed inputs *)
let rec declared (e : S.t) acc =
  let bound (vec : S.t) acc =
    match vec.node with
    | S.Vec bs -> List.fold_left (fun a (p, _) -> pat_names p @ a) acc (pairs bs)
    | _ -> acc
  in
  let acc =
    match e.node with
    | S.List ({ S.node = S.Sym "let*"; _ } :: v :: _) -> bound v acc
    | S.List ({ S.node = S.Sym ("for" | "scan" | "sum"); _ } :: v :: _) -> bound v acc
    | S.List ({ S.node = S.Sym "fold"; _ } :: a :: b :: _) -> bound b (bound a acc)
    | S.List ({ S.node = S.Sym "state"; _ } :: a :: _) -> bound a acc
    | S.List ({ S.node = S.Sym "fn"; _ } :: { S.node = S.Vec ps; _ } :: _) ->
        List.fold_left
          (fun a (p : S.t) ->
            match p.node with
            | S.List ({ S.node = S.Sym n; _ } :: _) -> n :: a
            | _ -> pat_names p @ a)
          acc ps
    | S.List ({ S.node = S.Sym n; _ } :: { S.node = S.Sym ":"; _ } :: _) -> n :: acc
    | _ -> acc
  in
  List.fold_left (fun a c -> declared c a) acc (S.children e)

(* names read by a form and not declared inside it *)
let free e =
  let inner = declared e [] in
  List.filter (fun x -> not (List.mem x inner)) (sym_list e)

let nth_child e i = List.nth_opt (S.children e) i

let set_child (e : S.t) i v : S.t =
  let f = List.mapi (fun j c -> if j = i then v else c) in
  {
    e with
    node =
      (match e.node with
      | S.List l when i < List.length l -> S.List (f l)
      | S.Vec l when i < List.length l -> S.Vec (f l)
      | S.Map l when i < List.length l -> S.Map (f l)
      | S.Quote (k, _) when i = 0 -> S.Quote (k, v)
      | _ -> fail "This form has no part %d." i);
  }

let rec get_sub e = function
  | [] -> Some e
  | i :: r -> Option.bind (nth_child e i) (fun c -> get_sub c r)

let rec set_sub e sub v =
  match sub with
  | [] -> v
  | i :: r -> (
      match nth_child e i with
      | Some c -> set_child e i (set_sub c r v)
      | None -> fail "This form has no part %d." i)

let keep_notes (old : S.t) (v : S.t) = if v.notes = [] then { v with notes = old.notes } else v

(* ---- call arguments: positional ones, then :keyword pairs ---- *)

let is_kw (x : S.t) = match x.node with S.Kw _ -> true | _ -> false
let kw_name (k : S.t) = match k.node with S.Kw s -> s | _ -> ""

(* a call's arguments are positional ones and [:keyword value] pairs, in any order *)
let positional args =
  let rec go = function k :: _ :: r when is_kw k -> go r | x :: r -> x :: go r | [] -> [] in
  go args

let keywords args =
  let rec go = function
    | k :: v :: r when is_kw k -> (kw_name k, v) :: go r
    | _ :: r -> go r
    | [] -> []
  in
  go args

let kw_get args k =
  let rec go = function
    | a :: b :: r -> if is_kw a && kw_name a = k then Some b else go (if is_kw a then r else b :: r)
    | _ -> None
  in
  go args

let with_kw args k v =
  let rec go = function
    | a :: b :: r when is_kw a ->
        if kw_name a = k then match v with Some v -> a :: keep_notes b v :: r | None -> r
        else a :: b :: go r
    | x :: r -> x :: go r
    | [] -> ( match v with Some v -> [ kwf k; v ] | None -> [])
  in
  go args

let with_pos args i v =
  let n = List.length (positional args) in
  if i >= n then
    match v with
    | None -> args
    | Some v ->
        (* after the last positional argument, else first *)
        let additions = List.init (i - n) (fun _ -> sym "nil") @ [ v ] in
        if n = 0 then additions @ args
        else
          let rec insert seen = function
            | a :: b :: r when is_kw a -> a :: b :: insert seen r
            | x :: r -> if seen + 1 = n then (x :: additions) @ r else x :: insert (seen + 1) r
            | [] -> []
          in
          insert 0 args
  else
    let rec go k = function
      | a :: b :: r when is_kw a -> a :: b :: go k r
      | x :: r ->
          if k = i then match v with Some v -> keep_notes x v :: r | None -> r
          else x :: go (k + 1) r
      | [] -> []
    in
    go 0 args

(* a [:skip] value (register L16): tuples as written, bare integers when every tuple has one *)
let skip_value tuples =
  if List.for_all (fun t -> List.length t = 1) tuples then
    vec (List.map (fun t -> num (List.hd t)) tuples)
  else vec (List.map (fun t -> vec (List.map num t)) tuples)

let skip_of_args args =
  match kw_get args "skip" with Some v -> Option.value ~default:[] (W.skip_tuples v) | None -> []

(* a [scene/merge]'s [:skip] tuples end in an argument position: when the arguments at [gone]
   (old positions) leave, the ones after them move up and the ones for them go *)
let skip_after_removal args gone =
  let moved =
    List.filter_map
      (fun t ->
        match List.rev t with
        | p :: outer when not (List.mem p gone) ->
            Some (List.rev ((p - List.length (List.filter (fun g -> g < p) gone)) :: outer))
        | _ -> None)
      (skip_of_args args)
  in
  with_kw args "skip" (if moved = [] then None else Some (skip_value moved))

let arm_key e index =
  if index < 0 then if S.head e = Some "if" then Pos 2 else Kw "else"
  else
    Pos
      (match S.head e with
      | Some "case" -> (2 * index) + 2
      | Some "cond" -> (2 * index) + 1
      | _ -> 1)

let arg_get (e : S.t) key =
  let key = match key with Arm i -> arm_key e i | _ -> key in
  match (key, e.node) with
  | Whole, _ -> Some e
  | Bv (i, j), _ -> Option.bind (nth_child e i) (fun c -> nth_child c j)
  | Field k, S.Map l ->
      List.find_map (fun (a, b) -> if kw_name a = k then Some b else None) (pairs l)
  | Pos i, S.List (_ :: args) ->
      List.nth_opt (positional (S.attribute_args (Option.value ~default:"" (S.head e)) args)) i
  | Kw k, S.List (_ :: args) -> kw_get args k
  | _ -> None

let set_pair ps k v =
  if List.exists (fun (a, _) -> kw_name a = k) ps then
    match v with
    | Some v -> List.map (fun (a, b) -> if kw_name a = k then (a, keep_notes b v) else (a, b)) ps
    | None -> List.filter (fun (a, _) -> kw_name a <> k) ps
  else match v with Some v -> ps @ [ (kwf k, v) ] | None -> ps

(* a [scene/root] prints its keywords in a fixed order: the camera, the renderer, the size, the samples *)
let root_order args =
  let rank k =
    match
      List.find_index (( = ) k)
        [ "camera"; "renderer"; "width"; "height"; "max_spp"; "bounces"; "round_samples" ]
    with
    | Some i -> i
    | None -> 99
  in
  let rec split = function
    | k :: v :: r when is_kw k ->
        let kws, pos = split r in
        ((k, v) :: kws, pos)
    | x :: r ->
        let kws, pos = split r in
        (kws, x :: pos)
    | [] -> ([], [])
  in
  let kws, pos = split args in
  pos
  @ flat_pairs
      (List.stable_sort (fun (a, _) (b, _) -> compare (rank (kw_name a)) (rank (kw_name b))) kws)

let arg_set (e : S.t) key (v : S.t option) : S.t =
  let key = match key with Arm i -> arm_key e i | _ -> key in
  let e =
    match e.node with
    | S.List (({ node = S.Sym h; _ } as head) :: args) ->
        { e with node = S.List (head :: S.attribute_args h args) }
    | _ -> e
  in
  match (key, e.node, v) with
  | Whole, _, Some v -> keep_notes e v
  | Whole, _, None -> fail "A whole binding cannot be removed."
  | Bv (i, j), _, Some v -> (
      match nth_child e i with
      | Some c -> set_child e i (set_child c j v)
      | None -> fail "This form has no part %d." i)
  | Bv _, _, None -> fail "A loop clause cannot be removed."
  | Field k, S.Map l, _ -> { e with node = S.Map (flat_pairs (set_pair (pairs l) k v)) }
  | Pos i, S.List (({ S.node = S.Sym "scene/merge"; _ } as h) :: args), None ->
      { e with node = S.List (h :: skip_after_removal (with_pos args i None) [ i ]) }
  | Kw "skip", S.List (({ S.node = S.Sym "for"; _ } as h) :: args), Some v
    when kw_get args "skip" = None ->
      (* the body stays last *)
      let n = List.length args in
      {
        e with
        node = S.List ((h :: List.take (n - 1) args) @ [ kwf "skip"; v; List.nth args (n - 1) ]);
      }
  | Pos i, S.List (h :: args), _ -> { e with node = S.List (h :: with_pos args i v) }
  | Kw k, S.List (({ S.node = S.Sym "scene/root"; _ } as h) :: args), _ ->
      { e with node = S.List (h :: root_order (with_kw args k v)) }
  | Kw k, S.List (h :: args), _ -> { e with node = S.List (h :: with_kw args k v) }
  | _ -> fail "This form has no such input."

(* ---- scopes: a let* (or a bare expression) whose bindings are the nodes ---- *)

type scope = { form : S.t; vec : S.t; ps : (S.t * S.t) list; res : S.t }

let scope_of (e : S.t) =
  match (e.node, e.meta) with
  | S.List [ { S.node = S.Sym "let*"; _ }; ({ S.node = S.Vec bs; _ } as vec); res ], []
    when List.length bs mod 2 = 0 ->
      Some { form = e; vec; ps = pairs bs; res }
  | _ -> None

let ensure (e : S.t) =
  match scope_of e with
  | Some s -> s
  | None ->
      let v = vec [] in
      { form = mk (S.List [ sym "let*"; v; e ]); vec = v; ps = []; res = e }

let rebuild s ps res : S.t =
  match s.form.node with
  | S.List [ h; _; _ ] ->
      { s.form with node = S.List [ h; { s.vec with node = S.Vec (flat_pairs ps) }; res ] }
  | _ -> s.form

let collapse s ps res = if ps = [] then res else rebuild s ps res (* an empty let* is its result *)

(* a call written in an argument is a node of the graph too.  It has no binding: its leaf is the
   leaf of the binding (or [@result]) that holds it, then [#] and each argument on the way down
   ([result#0#:cutters]: the [:cutters] input of the first input of [result]) *)
let node_call (e : S.t) =
  match head_sym e with
  | Some
      ( "fn" | "map" | "filter" | "reduce" | "sort-by" | "iterate" | "if" | "cond" | "case"
      | "with_error" | "error_rows" | "exact" ) ->
      true
  | Some h -> String.length h > 1 && String.contains h '/'
  | None -> false

let nested leaf = String.contains leaf '#'

(* the input a chain runs through: the first input written as a node call ([draw/translate]'s
   drawing, the value of a [->] step), else the first *)
let chain_in (e : S.t) =
  let rec go i =
    match arg_get e (Pos i) with
    | Some a when node_call a -> Pos i
    | Some _ -> go (i + 1)
    | None -> Pos 0
  in
  go 0

let key_segment = function
  | Pos i -> string_of_int i
  | Kw k -> ":" ^ k
  | Arm i -> if i < 0 then "else" else if i = 0 then "then" else "then~" ^ string_of_int (i + 1)
  | Whole | Field _ | Bv _ -> fail "Only an input of a call holds a nested node."

let nested_leaf leaf key = leaf ^ "#" ^ key_segment key

let split_leaf leaf =
  match String.split_on_char '#' leaf with
  | base :: keys ->
      ( base,
        List.map
          (fun k ->
            if k = "else" then Arm (-1)
            else if k = "then" then Arm 0
            else if String.starts_with ~prefix:"then~" k then
              Arm (int_of_string (String.sub k 5 (String.length k - 5)) - 1)
            else if String.length k > 0 && k.[0] = ':' then
              Kw (String.sub k 1 (String.length k - 1))
            else
              match int_of_string_opt k with
              | Some i -> Pos i
              | None -> fail "Node %s no longer exists." leaf)
          keys )
  | [] -> (leaf, [])

(* the node that holds a nested one, and the input it is written in *)
let holder leaf =
  let i = String.rindex leaf '#' in
  (String.sub leaf 0 i, List.hd (snd (split_leaf (String.sub leaf i (String.length leaf - i)))))

let find_pair s leaf =
  let leaf = fst (split_leaf leaf) in
  List.find_index (fun (p, _) -> pat_key p = leaf) s.ps

let is_zone (e : S.t) =
  e.meta = []
  &&
  match head_sym e with
  | Some ("for" | "fold" | "scan" | "sum" | "state" | "fn") -> true
  | _ -> false

let last_child (e : S.t) =
  match List.rev (S.children e) with c :: _ -> c | [] -> fail "Empty form."

let set_last (e : S.t) v : S.t =
  let l = S.children e in
  { e with node = S.List (List.mapi (fun i c -> if i = List.length l - 1 then v else c) l) }

(* the scope inside a binding value: a let* itself, or a zone's body *)
let enter (e : S.t) : S.t * (S.t -> S.t) =
  if scope_of e <> None then (e, Fun.id)
  else if is_zone e then (last_child e, set_last e)
  else fail "That node is not a scope."

let rec descend (cur : S.t) names (f : S.t -> S.t) : S.t =
  match names with
  | [] -> f cur
  | name :: rest ->
      let base, keys = split_leaf name in
      let child, back =
        match scope_of cur with
        | Some s when base = "@result" -> (s.res, fun r -> rebuild s s.ps r)
        | Some s -> (
            match find_pair s name with
            | Some j ->
                let p, v = List.nth s.ps j in
                ( v,
                  fun v' ->
                    rebuild s (List.mapi (fun k q -> if k = j then (p, v') else q) s.ps) s.res )
            | None -> fail "Binding %s no longer exists." name)
        | None when base = "@result" -> (cur, Fun.id)
        | None -> fail "Scope no longer exists."
      in
      let rec inside child = function
        | [] -> (child, Fun.id)
        | key :: keys ->
            let argument =
              match arg_get child key with
              | Some a -> a
              | None -> fail "Node %s no longer exists." name
            in
            let inner, restore = inside argument keys in
            (inner, fun v -> arg_set child key (Some (restore v)))
      in
      let child, restore = inside child keys in
      let entered, out =
        if List.exists (function Arm _ -> true | _ -> false) keys then (child, Fun.id)
        else enter child
      in
      back (restore (out (descend entered rest f)))

let workspace_parts (src : S.t list) =
  match src with
  | ({ S.node = S.List ({ S.node = S.Sym "workspace"; _ } :: _ :: items); _ } as ws) :: _ ->
      (ws, items)
  | _ -> fail "The source is not a (workspace ...)."

let root_name (item : S.t) =
  match item.node with
  | S.List ({ S.node = S.Sym (("graph" | "defn") as h); _ } :: { S.node = S.Sym n; _ } :: _) ->
      Some (if h = "defn" then "def:" ^ n else n)
  | _ -> None

(* the graphs (other than itself) whose text holds [(ref name ...)] *)
let graph_readers items name =
  let rec reads (e : S.t) =
    match e.node with
    | S.List ({ S.node = S.Sym "ref"; _ } :: { S.node = S.Sym n; _ } :: _) when n = name -> true
    | _ -> List.exists reads (S.children e)
  in
  List.filter_map
    (fun item ->
      match root_name item with Some r when r <> name && reads item -> Some r | _ -> None)
    items

let map_items (src : S.t list) f : S.t list =
  match src with
  | ({ S.node = S.List (w :: name :: items); _ } as ws) :: rest ->
      { ws with node = S.List (w :: name :: f items) } :: rest
  | _ -> fail "The source is not a (workspace ...)."

let with_root src seg (f : S.t -> S.t) =
  let hit = ref false in
  let out =
    map_items src
      (List.map (fun item ->
           if root_name item = Some seg then (
             hit := true;
             f item)
           else item))
  in
  if not !hit then fail "No graph or definition %s." seg;
  out

let root_form src seg =
  let _, items = workspace_parts src in
  match List.find_opt (fun i -> root_name i = Some seg) items with
  | Some r -> r
  | None -> fail "No graph or definition %s." seg

let edit_scope src scope_path (g : S.t -> S.t) =
  match scope_path with
  | [] -> fail "No scope."
  | seg :: names -> with_root src seg (fun r -> set_last r (descend (last_child r) names g))

let split_node = function
  | [] | [ _ ] -> fail "Not a node."
  | node ->
      let r = List.rev node in
      (List.rev (List.tl r), List.hd r)

let rec get_node s leaf =
  if nested leaf then
    let base, keys = split_leaf leaf in
    List.fold_left
      (fun e key ->
        match arg_get e key with Some a -> a | None -> fail "Node %s no longer exists." leaf)
      (get_node s base) keys
  else
    match scope_of s with
    | Some sc when leaf = "@result" -> sc.res
    | Some sc -> (
        match find_pair sc leaf with
        | Some j -> snd (List.nth sc.ps j)
        | None -> fail "Binding %s no longer exists." leaf)
    | None when leaf = "@result" -> s
    | None -> fail "Binding %s no longer exists." leaf

let reorder (s : S.t) =
  match scope_of s with
  | None -> s
  | Some sc ->
      (* the first binding whose reads are all bound goes next; by name in two tables, and a scope
         already in order (every scrub) is one pass *)
      let names = Hashtbl.create 64 and seen = Hashtbl.create 64 in
      List.iter (fun (p, _) -> List.iter (fun n -> Hashtbl.replace names n ()) (pat_names p)) sc.ps;
      let ready (_, e) =
        List.for_all (fun x -> (not (Hashtbl.mem names x)) || Hashtbl.mem seen x) (free e)
      in
      let take q = List.iter (fun n -> Hashtbl.replace seen n ()) (pat_names (fst q)) in
      let rec go left out =
        match left with
        | [] -> List.rev out
        | q :: rest when ready q ->
            take q;
            go rest (q :: out)
        | _ -> (
            match List.find_opt ready left with
            | None ->
                fail ~code:"E_GRAPH_CYCLE" "That connection would make a cycle through %s."
                  (String.concat ", " (List.map (fun (p, _) -> pat_key p) left))
            | Some q ->
                take q;
                go (List.filter (fun x -> x != q) left) (q :: out))
      in
      rebuild sc (go sc.ps []) sc.res

let rec set_node s leaf e =
  if nested leaf then
    let base, keys = split_leaf leaf in
    let rec put cur = function
      | [] -> keep_notes cur e
      | key :: rest -> (
          match arg_get cur key with
          | Some a -> arg_set cur key (Some (put a rest))
          | None -> fail "Node %s no longer exists." leaf)
    in
    set_node s base (put (get_node s base) keys)
  else
    match scope_of s with
    | Some sc when leaf = "@result" -> rebuild sc sc.ps (keep_notes sc.res e)
    | Some sc -> (
        match find_pair sc leaf with
        | Some j ->
            rebuild sc
              (List.mapi (fun k (p, v) -> if k = j then (p, keep_notes v e) else (p, v)) sc.ps)
              sc.res
        | None -> fail "Binding %s no longer exists." leaf)
    | None when leaf = "@result" -> e
    | None -> fail "Binding %s no longer exists." leaf

(* ---- names ---- *)

let sanitize base =
  let base =
    match String.rindex_opt base '/' with
    | Some i -> String.sub base (i + 1) (String.length base - i - 1)
    | None -> base
  in
  let b = Buffer.create 16 in
  String.iter
    (fun c ->
      match Char.lowercase_ascii c with
      | ('a' .. 'z' | '0' .. '9' | '_') as c -> Buffer.add_char b c
      | _ -> Buffer.add_char b '_')
    base;
  let s = Buffer.contents b in
  let n = String.length s in
  let i = ref 0 and j = ref n in
  while !i < n && s.[!i] = '_' do
    incr i
  done;
  while !j > !i && s.[!j - 1] = '_' do
    decr j
  done;
  let s = String.sub s !i (!j - !i) in
  if s = "" then "node" else if s.[0] >= 'a' && s.[0] <= 'z' then s else "n" ^ s

let fresh used base =
  let base = sanitize base in
  let rec go n =
    let name = if n = 1 then base else base ^ "_" ^ string_of_int n in
    if List.mem name !used || W.name_taken name then go (n + 1) else name
  in
  let name = go 1 in
  used := name :: !used;
  name

let root_used src seg =
  ref
    (List.concat_map
       (fun (i : S.t) -> if root_name i = Some seg then sym_list i else [])
       (snd (workspace_parts src)))

let fresh_name src ~root base = fresh (root_used src root) base
let fresh_among = fresh

(* the name a value operator's binding starts with *)
let value_base = function
  | "+" -> "sum"
  | "-" -> "difference"
  | "*" -> "product"
  | "/" -> "quotient"
  | "<" | ">" | "<=" | ">=" | "=" -> "test"
  | op -> op

(* a plain operator call: a card of its own only while it is written inside its reader *)
let value_call (e : S.t) =
  (not (node_call e))
  && (not (is_zone e))
  &&
  match head_sym e with
  | Some h -> h <> "list" && h <> "let*" && List.mem h W.value_ops
  | None -> false

(* still called what [fresh] would call it: the base, then maybe [_n] *)
let auto_named name (e : S.t) =
  let b = sanitize (value_base (Option.get (head_sym e))) in
  let n = String.length b in
  name = b
  || String.starts_with ~prefix:(b ^ "_") name
     && String.length name > n + 1
     && String.for_all
          (fun c -> c >= '0' && c <= '9')
          (String.sub name (n + 1) (String.length name - n - 1))

let valid_name n = Flow.Symbol.valid_name n

let rec default_for ?(fresh = Fun.id) (ty : Flow.Ty.t) label =
  match ty with
  | Flow.Ty.Float -> Some (mk (S.Num "0.5"))
  | Int -> Some (mk (S.Num "1"))
  | Bool -> Some (sym "false")
  | Vec3 -> Some (vec [ mk (S.Num "0"); mk (S.Num "0"); mk (S.Num "0") ])
  | Vec2 -> Some (vec [ mk (S.Num "0"); mk (S.Num "0") ])
  | Vec4 -> Some (vec [ mk (S.Num "0"); mk (S.Num "0"); mk (S.Num "0"); mk (S.Num "0") ])
  | Color -> Some (mk (S.Str "#285f77"))
  | Text -> Some (mk (S.Str (if label = "color" then "#285f77" else "text")))
  | Array (Float | Any) -> Some (call "array/float" [ mk (S.Num "4") ])
  | Array Int -> Some (call "array/int" [ mk (S.Num "0") ])
  | Array Text -> Some (call "array/text" [ mk (S.Num "4") ])
  | Array Vec2 ->
      let name = fresh "element" in
      Some
        (call "map"
           [
             call "fn" [ vec [ sym name ]; vec [ sym name; sym name ] ];
             call "array/float" [ mk (S.Num "4") ];
           ])
  | Array Vec3 -> Some (call "array/vec3" [ mk (S.Num "4") ])
  | Array Vec4 -> Some (call "array/vec4" [ mk (S.Num "4") ])
  | Record _ -> branch_default ty
  | Named _ -> Flow.Ty.default ty
  | Fn (Some signature) ->
      Option.map
        (fun body ->
          call "fn"
            [
              vec (List.mapi (fun i _ -> sym (fresh ("arg" ^ string_of_int i))) signature.params);
              body;
            ])
        (branch_default signature.result)
  | _ -> None

and branch_default (ty : Flow.Ty.t) =
  match ty with
  | List _ -> Some (call "list" [])
  | Record fields ->
      let fields =
        List.map
          (fun (name, ty) -> Option.map (fun value -> [ kwf name; value ]) (branch_default ty))
          fields
      in
      if List.exists Option.is_none fields then None
      else Some (mk (S.Map (List.concat_map Option.get fields)))
  | _ -> default_for ty "value"

let rec literals ?(path = []) (e : S.t) =
  match e.node with
  | S.Num _ | S.Str _ -> [ (List.rev path, e) ]
  | S.List _ | S.Vec _ | S.Map _ ->
      List.concat
        (List.mapi
           (fun i (c : S.t) ->
             if
               (i = 0 && match e.node with S.List _ -> true | _ -> false)
               || match c.node with S.Kw _ -> true | _ -> false
             then []
             else literals ~path:(i :: path) c)
           (S.children e))
  | _ -> []

let literals e = literals e

(* ---- selections ---- *)

type selection = {
  sc : scope;
  sel : (S.t * S.t) list;
  keep : (S.t * S.t) list;
  first : int;
  out : S.t * S.t;
  out_name : string;
}

let scope_path_of nodes =
  match nodes with
  | [] -> fail "Select the nodes first."
  | first :: rest ->
      let sp, _ = split_node first in
      List.iter (fun n -> if fst (split_node n) <> sp then fail "Select nodes from one scope.") rest;
      sp

(* the bindings of [nodes] in scope expression [s], the one leaving them *)
let select s nodes what =
  let leaves = List.map (fun n -> snd (split_node n)) nodes in
  if List.mem "@result" leaves then fail "A result cannot be part of %s." what;
  if List.exists nested leaves then
    fail "Name the nodes first (rename them): %s needs named nodes." what;
  let sc = ensure s in
  let sel = List.filter (fun (p, _) -> List.mem (pat_key p) leaves) sc.ps in
  if List.length sel <> List.length (dedup leaves) then fail "A selected node no longer exists.";
  let keep = List.filter (fun (p, _) -> not (List.mem (pat_key p) leaves)) sc.ps in
  let first = Option.get (List.find_index (fun (p, _) -> List.mem (pat_key p) leaves) sc.ps) in
  let mentions n e = List.mem n (free e) in
  let mentioned n = List.exists (fun (_, e) -> mentions n e) keep || mentions n sc.res in
  let outs = List.filter (fun (p, _) -> List.exists mentioned (pat_names p)) sel in
  if List.length outs > 1 then
    fail "%s needs one result leaving the selection; %s are all used outside." what
      (String.concat " and " (List.map (fun (p, _) -> pat_key p) outs));
  let out = match outs with [ o ] -> o | _ -> List.nth sel (List.length sel - 1) in
  let out_name =
    match (fst out).node with S.Sym n -> n | _ -> fail "The result of %s must be a name." what
  in
  { sc; sel; keep; first; out; out_name }

let body_of x =
  match x.sel with
  | [ (_, e) ] -> e
  | sel ->
      let others = List.filter (fun q -> q != x.out) sel in
      call "let*" [ vec (flat_pairs (others @ [ x.out ])); sym x.out_name ]

let insert_at keep i extra =
  let i = max 0 (min i (List.length keep)) in
  List.take i keep @ extra @ List.drop i keep

let sel_names x = List.concat_map (fun (p, _) -> pat_names p) x.sel
let sel_declared x = List.fold_left (fun a (_, e) -> declared e a) [] x.sel

(* names the selection reads from outside, in order of appearance *)
let outside_names ~root x =
  let visible = declared root [] in
  let inner = sel_declared x and own = sel_names x in
  dedup (List.concat_map (fun (_, e) -> sym_list e) x.sel)
  |> List.filter (fun n -> List.mem n visible && (not (List.mem n inner)) && not (List.mem n own))

let fn_names root =
  let rec go (e : S.t) acc =
    let acc =
      match e.node with
      | S.List ({ S.node = S.Sym "let*"; _ } :: { S.node = S.Vec bs; _ } :: _) ->
          List.fold_left
            (fun a ((p : S.t), (v : S.t)) -> if head_sym v = Some "fn" then pat_names p @ a else a)
            acc (pairs bs)
      | _ -> acc
    in
    List.fold_left (fun a c -> go c a) acc (S.children e)
  in
  go root []

(* ---- the rewrites; each returns candidate sources, tried in order ---- *)

let first_pair_index sc leaf =
  match find_pair sc leaf with Some j -> j | None -> fail "Binding %s no longer exists." leaf

let note_lines text =
  let text = String.trim text in
  if text = "" then [] else List.map String.trim (String.split_on_char '\n' text)

(* ---- scene objects and World layers: they join and leave their graph's result ---- *)

let starts_with prefix (h : string option) =
  match h with Some h -> String.starts_with ~prefix h | None -> false

(* where the result of a scope is written: a named binding, or the result itself *)
let result_target sc =
  match sc.res.node with
  | S.Sym r -> (
      match find_pair sc r with
      | Some j -> (`At j, snd (List.nth sc.ps j))
      | None -> (`Result, sc.res))
  | _ -> (`Result, sc.res)

let put_target sc ps = function
  | `At j, e ->
      rebuild sc
        (List.mapi (fun k (p, v) -> if k = j then (p, keep_notes v e) else (p, v)) ps)
        sc.res
  | `Result, e -> rebuild sc ps (keep_notes sc.res e)

(* the merge a new scene object joins: the result's, looking through the root; [ps] holds the new
   binding last *)
let join_merge sc ps name =
  let target, e = result_target sc in
  let add_into (m : S.t) =
    arg_set m (Pos (List.length (positional (List.tl (S.children m))))) (Some (sym name))
  in
  match head_sym e with
  | Some "scene/merge" -> put_target sc ps (target, add_into e)
  | Some "scene/root" -> (
      let wrap inner = put_target sc ps (target, arg_set e (Pos 0) (Some inner)) in
      match arg_get e (Pos 0) with
      | Some { S.node = S.Sym m; _ } -> (
          match find_pair { sc with ps } m with
          | Some j when head_sym (snd (List.nth ps j)) = Some "scene/merge" ->
              rebuild sc
                (List.mapi
                   (fun k (p, v) -> if k = j then (p, keep_notes v (add_into v)) else (p, v))
                   ps)
                sc.res
          | _ -> wrap (call "scene/merge" [ sym m; sym name ]))
      | Some inner when head_sym inner = Some "scene/merge" -> wrap (add_into inner)
      | Some inner -> wrap (call "scene/merge" [ inner; sym name ])
      | None -> wrap (sym name))
  | _ -> rebuild sc ps (call "scene/merge" [ sc.res; sym name ])

(* a new scene object is one more argument of the scene's [scene/merge] (made when the result
   is something else, and looking through the root); a new root takes the result; a new World
   layer goes on top of the stack: it becomes the graph's result over the old one.  [ps] holds the new binding
   last. *)
let attach sc ps name (expr : S.t) =
  let head = head_sym expr in
  let _, e = result_target sc in
  if head = Some "scene/root" then (
    if head_sym e = Some "scene/root" then fail "The scene already has a root.";
    (collapse sc sc.ps (arg_set expr (Pos 0) (Some sc.res)), expr))
  else if starts_with "scene/" head && head <> Some "scene/merge" then (join_merge sc ps name, expr)
  else if starts_with "world/" head then
    match head_sym e with
    | Some "world/none" -> (rebuild sc ps (sym name), expr)
    | Some _ -> (rebuild sc ps (sym name), arg_set expr (Pos 0) (Some sc.res))
    | None -> (rebuild sc ps sc.res, expr)
  else (rebuild sc ps sc.res, expr)

(* a deleted object leaves the merge that held it; a deleted layer leaves the stack, the layer
   above it taking the one below *)
let rec detach name below (e : S.t) : S.t =
  let e = map_children (detach name below) e in
  match head_sym e with
  | Some "scene/merge" ->
      let args = List.tl (S.children e) in
      let gone =
        List.filter_map
          (fun x -> x)
          (List.mapi
             (fun i (c : S.t) -> if c.node = S.Sym name then Some i else None)
             (positional args))
      in
      {
        e with
        node =
          S.List
            (List.hd (S.children e)
            :: skip_after_removal (List.filter (fun (c : S.t) -> c.node <> S.Sym name) args) gone);
      }
  | Some h when String.starts_with ~prefix:"world/" h -> (
      match arg_get e (Pos 0) with
      | Some { S.node = S.Sym n; _ } when n = name -> arg_set e (Pos 0) below
      | _ -> e)
  | _ -> e

(* a deleted World layer that was a world graph's result hands the result to the layer below
   it, or to [(world/none)] *)
let detach_result name below (root : S.t) =
  let top = match below with Some b -> b | None -> call "world/none" [] in
  match root.node with
  | S.List ({ S.node = S.Sym "graph"; _ } :: _) -> (
      let body = last_child root in
      match scope_of body with
      | Some sc when sc.res.node = S.Sym name -> set_last root (rebuild sc sc.ps top)
      | Some _ -> root
      | None -> if body.node = S.Sym name then set_last root top else root)
  | _ -> root

(* the copies a duplicate makes: each selected binding with a fresh name; the copies read each
   other where the originals did *)
let duplicate_plan src nodes =
  let sp = scope_path_of nodes in
  let used = root_used src (List.hd sp) in
  let leaves = dedup (List.map (fun n -> snd (split_node n)) nodes) in
  if List.mem "@result" leaves then fail "The result cannot be duplicated.";
  if List.exists nested leaves then fail "Name the node first (rename it), then duplicate it.";
  (sp, List.map (fun leaf -> (leaf, fresh used leaf)) leaves)

let has_prefix ~prefix p =
  let n = List.length prefix in
  List.length p >= n && List.take n p = prefix
