(* The pure core of the Lisp text editor: an error-tolerant token stream with matched brackets,
   Lisp indentation, and the reading of the text around a byte (the head of a call, the names
   bound before a point, the context of the enclosing graph).  The editor colours and completes
   over it; nothing here knows a theme, a catalog or the UI. *)

type kind = Comment | Str | Num | Kw | Meta | Head | Sym | Open of int | Close of int | Unmatched
type token = { kind : kind; start : int; stop : int }

let closer_of = function '(' -> ')' | '[' -> ']' | _ -> '}'

let lex text =
  let raw = Syntax.Text_lexer.lex text in
  let tokens = ref [] and count = ref 0 and pairs = ref [] and stack = ref [] in
  let add kind start stop =
    let index = !count in
    tokens := { kind; start; stop } :: !tokens;
    incr count;
    index
  in
  let head_next = ref false and index = ref 0 in
  while !index < Array.length raw do
    let token = raw.(!index) in
    match token.kind with
    | Syntax.Text_lexer.Open c ->
        let at = add (Open (List.length !stack)) token.start token.stop in
        stack := (c, token.start, at) :: !stack;
        head_next := c = '(';
        incr index
    | Syntax.Text_lexer.Close c ->
        (match !stack with
        | (o, at, _) :: rest when closer_of o = c ->
            stack := rest;
            pairs := (at, token.start) :: !pairs;
            ignore (add (Close (List.length rest)) token.start token.stop)
        | _ -> ignore (add Unmatched token.start token.stop));
        head_next := false;
        incr index
    | Syntax.Text_lexer.Comment _ ->
        ignore (add Comment token.start token.stop);
        incr index
    | Syntax.Text_lexer.String _ ->
        ignore (add Str token.start token.stop);
        head_next := false;
        incr index
    | Syntax.Text_lexer.Meta _ ->
        ignore (add (if !head_next then Head else Meta) token.start token.stop);
        head_next := false;
        incr index
    | Syntax.Text_lexer.Word word ->
        let kind =
          if !head_next then Head
          else
            match word with
            | Syntax.Text_lexer.Symbol _ -> Sym
            | Syntax.Text_lexer.Keyword _ -> Kw
            | Syntax.Text_lexer.Number _ -> Num
        in
        ignore (add kind token.start token.stop);
        head_next := false;
        incr index
    | Syntax.Text_lexer.Prefix _ ->
        (* Reader prefixes stay visually attached to a following word. *)
        let stop =
          if !index + 1 < Array.length raw then
            let next = raw.(!index + 1) in
            match next.kind with
            | (Syntax.Text_lexer.Word _ | Syntax.Text_lexer.Meta _ | Syntax.Text_lexer.Prefix _)
              when next.start = token.stop ->
                incr index;
                next.stop
            | _ -> token.stop
          else token.stop
        in
        ignore (add (if !head_next then Head else Sym) token.start stop);
        head_next := false;
        incr index
    | Syntax.Text_lexer.End -> incr index
  done;
  let tokens = Array.of_list (List.rev !tokens) in
  List.iter
    (fun (_, _, index) -> tokens.(index) <- { (tokens.(index)) with kind = Unmatched })
    !stack;
  (tokens, List.rev !pairs, List.map (fun (c, at, _) -> (c, at)) !stack)

(* forms whose body is indented two in from the paren rather than under the first argument *)
let body_forms =
  List.filter
    (fun form -> List.mem form Workspace.special_forms)
    [
      "workspace";
      "graph";
      "defn";
      "defnode";
      "defmacro";
      "let*";
      "for";
      "fold";
      "scan";
      "sum";
      "state";
      "if";
      "fn";
      "cond";
      "case";
    ]

let indent text caret =
  let text = String.sub text 0 (max 0 (min caret (String.length text))) in
  let tokens, _, open_stack = lex text in
  (* characters, not bytes: a multi-byte letter earlier on the line is one column *)
  let column at =
    let start =
      match String.rindex_from_opt text (at - 1) '\n' with Some i -> i + 1 | None -> 0
    in
    let n = ref 0 in
    for i = start to at - 1 do
      if Char.code text.[i] land 0xC0 <> 0x80 then incr n
    done;
    !n
  in
  let width =
    match open_stack with
    | [] -> 0
    | (c, at) :: _ when c <> '(' -> column at + 1
    | (_, at) :: _ -> (
        let line_end =
          match String.index_from_opt text at '\n' with Some j -> j | None -> String.length text
        in
        let after from = List.find_opt (fun t -> t.start > from) (Array.to_list tokens) in
        match after at with
        | Some ({ kind = Head; _ } as head) -> (
            let name = String.sub text head.start (head.stop - head.start) in
            if List.mem name body_forms then column at + 2
            else
              match after head.stop with
              | Some arg when arg.start < line_end -> column arg.start
              | _ -> column at + 1)
        | _ -> column at + 1)
  in
  String.make width ' '

(* ---- reading the text around a byte (over the lexer's tokens) ---- *)

let word text (t : token) = String.sub text t.start (t.stop - t.start)

let index_at tokens at =
  let n = Array.length tokens in
  let rec seek i = if i >= n then -1 else if tokens.(i).start = at then i else seek (i + 1) in
  seek 0

(* the top-level items of the bracket opened by token [i]: the index of each item's first
   token, up to the matching close or the end of the text *)
let items tokens i =
  let n = Array.length tokens in
  let rec go j d acc =
    if j >= n then List.rev acc
    else
      match tokens.(j).kind with
      | Open _ -> go (j + 1) (d + 1) (if d = 0 then j :: acc else acc)
      | Close _ -> if d = 0 then List.rev acc else go (j + 1) (d - 1) acc
      | _ -> go (j + 1) d (if d = 0 then j :: acc else acc)
  in
  go (i + 1) 0 []

(* the byte after item [j] (its matching close for a bracket) *)
let item_stop tokens j =
  let n = Array.length tokens in
  match tokens.(j).kind with
  | Open d ->
      let rec seek k =
        if k >= n then tokens.(n - 1).stop
        else match tokens.(k).kind with Close d' when d' = d -> tokens.(k).stop | _ -> seek (k + 1)
      in
      seek (j + 1)
  | _ -> tokens.(j).stop

(* the head of the call a token sits in: back over balanced brackets to the nearest head *)
let head_before text tokens i =
  let rec back k depth =
    if k < 0 then None
    else
      match tokens.(k).kind with
      | Close _ -> back (k - 1) (depth + 1)
      | Open _ -> if depth > 0 then back (k - 1) (depth - 1) else None
      | Head when depth = 0 -> Some (word text tokens.(k))
      | _ -> back (k - 1) depth
  in
  back (i - 1) 0

let binders = [ "let*"; "for"; "fold"; "scan"; "sum"; "state"; "fn" ]

(* the names bound before [limit] by let*, loops, fn and graph inputs, each with a snippet of
   what it is; the defn names; the graph names (ponytail: a lexical scan, not scoping: every
   name bound earlier in the text is offered) *)
let bound text tokens limit =
  let n = Array.length tokens in
  let names = ref [] and defns = ref [] and graphs = ref [] in
  let snippet j =
    let stop = min (item_stop tokens j) (tokens.(j).start + 48) in
    String.map
      (function '\n' -> ' ' | c -> c)
      (String.sub text tokens.(j).start (stop - tokens.(j).start))
  in
  for i = 0 to n - 1 do
    if tokens.(i).start < limit then
      match tokens.(i).kind with
      | Head
        when i + 1 < n
             && tokens.(i + 1).kind = Sym
             &&
             let h = word text tokens.(i) in
             h = "defn" || h = "defmacro" ->
          defns := word text tokens.(i + 1) :: !defns
      | Head when i + 1 < n && tokens.(i + 1).kind = Sym && word text tokens.(i) = "graph" ->
          graphs := word text tokens.(i + 1) :: !graphs
      | (Open _ | Unmatched) when text.[tokens.(i).start] = '[' -> (
          match head_before text tokens i with
          | Some h when List.mem h binders ->
              let its = Array.of_list (items tokens i) in
              let all = h = "fn" in
              Array.iteri
                (fun pos j ->
                  if tokens.(j).kind = Sym && tokens.(j).start < limit && (all || pos mod 2 = 0)
                  then
                    names :=
                      ( word text tokens.(j),
                        if all || pos + 1 >= Array.length its then "parameter of " ^ h
                        else h ^ " binding · " ^ snippet its.(pos + 1) )
                      :: !names)
                its
          | Some ("graph" | "defn") ->
              (* [(name : type default) ...] *)
              List.iter
                (fun j ->
                  match tokens.(j).kind with
                  | (Open _ | Unmatched) when j + 1 < n && tokens.(j + 1).kind = Head ->
                      names := (word text tokens.(j + 1), "input · " ^ snippet j) :: !names
                  | _ -> ())
                (items tokens i)
          | _ -> ())
      | _ -> ()
  done;
  (List.rev !names, List.rev !defns, List.rev !graphs)

(* the value after keyword [kw] among the items of bracket [i] *)
let kw_value text tokens i kw =
  let rec seek = function
    | j :: k :: rest ->
        if tokens.(j).kind = Kw && word text tokens.(j) = kw then
          if tokens.(k).kind = Sym then Some (word text tokens.(k)) else None
        else seek (k :: rest)
    | _ -> None
  in
  seek (items tokens i)

(* the context of the outermost graph or defn around the caret ([open_stack] innermost first);
   sop otherwise *)
let context_of text tokens open_stack =
  List.fold_left
    (fun found (_, at) ->
      match found with
      | Some _ -> found
      | None ->
          let i = index_at tokens at in
          if
            i >= 0
            && i + 1 < Array.length tokens
            && tokens.(i + 1).kind = Head
            &&
            let h = word text tokens.(i + 1) in
            h = "graph" || h = "defn"
          then Some (Option.value ~default:"sop" (kw_value text tokens i ":context"))
          else None)
    None (List.rev open_stack)
  |> Option.value ~default:"sop"
