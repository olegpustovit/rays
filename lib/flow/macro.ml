open Syntax

let max_depth = 32
let max_forms = 5000

let valid_name s =
  s <> ""
  && (match s.[0] with 'a' .. 'z' -> true | _ -> false)
  && String.for_all (function 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true | _ -> false) s

let fail (x : Syntax.t) code message =
  raise (Diagnostic.Error (Diagnostic.error ~span:x.span ~code message))

let flat = Lisp.flat
let base s = match String.index_opt s '.' with Some i -> String.sub s 0 i | None -> s
let fresh_name b = String.ends_with ~suffix:"#" b
let map_seq f xs = List.rev (List.fold_left (fun acc x -> f x :: acc) [] xs)

let parts m =
  match m.node with
  | List [ { node = Sym "defmacro"; _ }; { node = Sym name; _ }; { node = Vec ps; _ }; body ] ->
      Some (name, ps, body)
  | _ -> None

let shape m =
  match parts m with
  | Some p -> p
  | None -> fail m "E_MACRO_SHAPE" "A macro is (defmacro name [params] template)."

let params_exn m =
  let name, ps, _ = shape m in
  let bad =
    Printf.sprintf "Macro %s: & is followed by exactly one rest parameter, at the end." name
  in
  let rec go req = function
    | [] -> (List.rev req, None)
    | { node = Sym "&"; _ } :: rest -> (
        match rest with
        | [ { node = Sym r; _ } ] when valid_name r && not (List.mem r req) -> (List.rev req, Some r)
        | _ -> fail m "E_MACRO_PARAM" bad)
    | { node = Sym p; _ } :: rest when valid_name p && not (List.mem p req) -> go (p :: req) rest
    | p :: _ ->
        fail m "E_MACRO_PARAM"
          (Printf.sprintf "Macro %s: invalid or repeated parameter %s." name (flat p))
  in
  go [] ps

let params m = try Ok (params_exn m) with Diagnostic.Error d -> Error d

let quasi m =
  match parts m with Some (_, _, { node = Quote (Quasi, body); _ }) -> Some body | _ -> None

let check ~known m =
  try
    let name, _, _ = shape m in
    let req, rest = params_exn m in
    let ps = match rest with Some r -> req @ [ r ] | None -> req in
    (match quasi m with
    | None ->
        fail m "E_MACRO_TEMPLATE"
          (Printf.sprintf "Macro %s: the template is quoted: write `(…) with ~parameters." name)
    | Some body ->
        let bad_unquote y =
          fail y "E_MACRO_UNQUOTE"
            (Printf.sprintf "Macro %s: macros unquote only their parameters; %s is not one." name
               (flat y))
        in
        (* a name a caller can bind ([count], [first]) is free only as the head of a call: as an
            argument it would read the caller's binding of it *)
        let rec walk ?(head = false) y =
          match y.node with
          | Sym s ->
              let b = base s in
              if fresh_name b then (
                if not (valid_name (String.sub b 0 (String.length b - 1))) then
                  fail y "E_MACRO_TEMPLATE"
                    (Printf.sprintf "Macro %s: %s is not a valid fresh name." name s))
              else if not (b = ":" || known ~head b) then
                fail y "E_MACRO_CAPTURE"
                  (Printf.sprintf
                     "Macro %s: %s would capture a name from the call site. Pass it as a parameter \
                      (~%s) or write %s# for a fresh name."
                     name s b b)
          | Kw _ | Num _ | Str _ -> ()
          | Quote (Unquote, { node = Sym p; _ }) ->
              if not (List.mem p ps) then bad_unquote y;
              if Some p = rest then
                fail y "E_MACRO_UNQUOTE"
                  (Printf.sprintf "Macro %s: splice the rest parameter with ~@%s." name p)
          | Quote (Splice, { node = Sym p; _ }) when Some p = rest -> ()
          | Quote (Splice, _) ->
              fail y "E_MACRO_UNQUOTE"
                (Printf.sprintf "Macro %s: ~@ splices the rest parameter only%s." name
                   (match rest with Some r -> " (~@" ^ r ^ ")" | None -> ""))
          | Quote (Unquote, _) -> bad_unquote y
          | Quote _ ->
              fail y "E_MACRO_TEMPLATE"
                (Printf.sprintf "Macro %s: nested quoting is not supported." name)
          | List (h :: xs) ->
              walk ~head:true h;
              List.iter walk xs
          | List [] -> ()
          | Vec xs | Map xs -> List.iter walk xs
        in
        walk body);
    []
  with Diagnostic.Error d -> [ d ]

type state = { mutable n : int; mutable size : int }

let state () = { n = 0; size = 0 }
let rec count x = 1 + List.fold_left (fun a y -> a + count y) 0 (children x)

(* One expansion step of the call [x] by macro [m]. *)
let step st m x =
  let name, _, _ = shape m in
  let req, rest = params_exn m in
  let args = match x.node with List (_ :: args) -> args | _ -> [] in
  let nreq = List.length req and nargs = List.length args in
  if if rest <> None then nargs < nreq else nargs <> nreq then
    fail x "E_MACRO_ARITY"
      (Printf.sprintf "%s expects %s%d argument%s; got %d." name
         (if rest <> None then "at least " else "")
         nreq
         (if nreq = 1 then "" else "s")
         nargs);
  let env = List.combine req (List.take nreq args) in
  let more = List.drop nreq args in
  match quasi m with
  | None ->
      fail m "E_MACRO_TEMPLATE"
        (Printf.sprintf "Macro %s: the template is quoted: write `(…) with ~parameters." name)
  | Some body ->
      let fresh = Hashtbl.create 4 in
      let gs s =
        let b = base s in
        if not (fresh_name b) then s
        else begin
          let name =
            match Hashtbl.find_opt fresh b with
            | Some n -> n
            | None ->
                st.n <- st.n + 1;
                let n = String.sub b 0 (String.length b - 1) ^ "__" ^ string_of_int st.n in
                Hashtbl.add fresh b n;
                n
          in
          name ^ String.sub s (String.length b) (String.length s - String.length b)
        end
      in
      let rec q y =
        let at node = { y with node; span = x.span } in
        match y.node with
        | Sym s -> at (Sym (gs s))
        | Kw _ | Num _ | Str _ -> at y.node
        | Quote (Unquote, { node = Sym p; _ }) -> (
            match List.assoc_opt p env with
            | Some a -> a
            | None ->
                fail y "E_MACRO_UNQUOTE"
                  (Printf.sprintf "Macro %s: macros unquote only their parameters; %s is not one."
                     name (flat y)))
        | Quote (Splice, _) ->
            fail y "E_MACRO_UNQUOTE" (Printf.sprintf "Macro %s: ~@ splices into a list." name)
        | Quote _ ->
            fail y "E_MACRO_TEMPLATE"
              (Printf.sprintf "Macro %s: nested quoting is not supported." name)
        | List xs -> at (List (qs xs))
        | Vec xs -> at (Vec (qs xs))
        | Map xs -> at (Map (qs xs))
      and qs xs =
        List.rev
          (List.fold_left
             (fun acc c ->
               match c.node with
               | Quote (Splice, { node = Sym p; _ }) when Some p = rest -> List.rev_append more acc
               | Quote (Splice, _) ->
                   fail c "E_MACRO_UNQUOTE"
                     (Printf.sprintf "Macro %s: ~@ splices the rest parameter only." name)
               | _ -> q c :: acc)
             [] xs)
      in
      q body

let table macros =
  List.filter_map (fun m -> Option.map (fun (name, _, _) -> (name, m)) (parts m)) macros

let inert x =
  match x.node with List ({ node = Sym "defmacro"; _ } :: _) | Quote _ -> true | _ -> false

let call_of tbl x =
  match x.node with
  | List ({ node = Sym h; _ } :: _) -> Option.map (fun m -> (h, m)) (List.assoc_opt h tbl)
  | _ -> None

let map_children f x =
  match x.node with
  | List xs -> { x with node = List (map_seq f xs) }
  | Vec xs -> { x with node = Vec (map_seq f xs) }
  | Map xs -> { x with node = Map (map_seq f xs) }
  | _ -> x

let expand macros x =
  let state = state () in
  let tbl = table macros in
  let rec go depth x =
    if inert x then x
    else
      match call_of tbl x with
      | Some (h, m) ->
          if depth >= max_depth then
            fail x "E_MACRO_DEPTH"
              (Printf.sprintf "Macro expansion exceeds %d nested expansions (at %s)." max_depth h);
          let y = step state m x in
          state.size <- state.size + count y;
          if state.size > max_forms then
            fail x "E_MACRO_SIZE" (Printf.sprintf "Macro expansion of %s exceeds 5,000 forms." h);
          go (depth + 1) y
      | None -> map_children (go depth) x
  in
  try Ok (go 0 x) with Diagnostic.Error d -> Error d

let expand_once ?(state = state ()) macros x =
  let tbl = table macros in
  let finished = ref false in
  let rec go y =
    if !finished || inert y then y
    else
      match call_of tbl y with
      | Some (_, m) ->
          finished := true;
          step state m y
      | None -> map_children go y
  in
  try
    let r = go x in
    Ok (if !finished then r else x)
  with Diagnostic.Error d -> Error d
