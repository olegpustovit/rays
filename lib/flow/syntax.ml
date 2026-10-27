type id = int
type quote_kind = Plain | Quasi | Unquote | Splice
type t = { id : id; node : node; span : Diagnostic.span; notes : string list;
  meta : string list; tail : string list }
and node = Sym of string | Kw of string | Num of string | Str of string
  | List of t list | Vec of t list | Map of t list | Quote of quote_kind * t

type kind =
  | Open of char | Close of char | Prefix of quote_kind | Meta_name of string
  | Comment of string | Word of node | End
type token = { kind : kind; span : Diagnostic.span }
module Text_lexer = Text_lexer

let depth_limit = 256

let nonfinite = function
  | "NaN" -> Some nan | "Infinity" -> Some infinity
  | "-Infinity" -> Some neg_infinity | _ -> None

let number_value text = match nonfinite text with
  | Some _ as value -> value
  | None -> Option.bind (float_of_string_opt text) (fun value ->
      if Float.is_finite value then Some value else None)

let number = Text_lexer.number

let fail source start finish code message = Error (Diagnostic.error
  ~span:{start; finish} ~position:(Diagnostic.position_of_offset source start)
  ~code message)

let separator = Text_lexer.separator

let tokenize source =
  let raw = Text_lexer.lex source in
  (* Control bytes keep their original priority over other lexical errors. *)
  let control_issue = Array.find_map (fun (token : Text_lexer.token) ->
    match token.issue with
    | Some issue when String.starts_with ~prefix:"Control byte " issue.message -> Some issue
    | _ -> None) raw in
  let issue = match control_issue with Some _ -> control_issue
    | None -> Array.find_map (fun (token : Text_lexer.token) -> token.issue) raw in
  match issue with
  | Some issue -> fail source issue.start issue.finish issue.code issue.message
  | None ->
      let kind = function
        | Text_lexer.Open c -> Open c
        | Text_lexer.Close c -> Close c
        | Text_lexer.Prefix '\'' -> Prefix Plain
        | Text_lexer.Prefix '`' -> Prefix Quasi
        | Text_lexer.Prefix '~' -> Prefix Unquote
        | Text_lexer.Prefix '@' -> Prefix Splice
        | Text_lexer.Prefix _ -> assert false
        | Text_lexer.Meta name -> Meta_name name
        | Text_lexer.Comment text -> Comment text
        | Text_lexer.String text -> Word (Str text)
        | Text_lexer.Word (Text_lexer.Symbol text) -> Word (Sym text)
        | Text_lexer.Word (Text_lexer.Keyword text) -> Word (Kw text)
        | Text_lexer.Word (Text_lexer.Number text) -> Word (Num text)
        | Text_lexer.End -> End in
      Ok (Array.to_list (Array.map (fun (token : Text_lexer.token) ->
        {kind=kind token.kind;span={Diagnostic.start=token.start;finish=token.stop}}) raw))

let parse source =
  Phase_timer.measure "parse" (fun () ->
  let ( let* ) = Result.bind in
  let* tokens = tokenize source in
  let tokens = Array.of_list tokens in
  let at = ref 0 and next_id = ref 0 in
  let peek () = tokens.(!at) in
  let take () = let token = peek () in
    (match token.kind with End -> () | _ -> incr at); token in
  let fresh () = let id = !next_id in incr next_id; id in
  let error (token : token) code message =
    fail source token.span.start token.span.finish code message in
  let rec comments () =
    match (peek ()).kind with
    | Comment text -> ignore (take ()); text :: comments ()
    | _ -> [] in
  let close_of = function '(' -> ')' | '[' -> ']' | _ -> '}' in
  let rec form notes depth =
    let token = take () in
    if depth > depth_limit then error token "E_DEPTH"
      (Printf.sprintf "S-expression nesting exceeds %d forms" depth_limit)
    else
      let atom node = Ok {id = fresh (); node; span = token.span; notes;
        meta = []; tail = []} in
      match token.kind with
      | Word node -> atom node
      | Open c -> let id = fresh () in group token c id notes depth []
      | Prefix kind ->
          let id = fresh () in
          (* a comment between ' and its form moves before the quote *)
          let notes = notes @ comments () in
          let* inner = form [] (depth + 1) in
          Ok {id; node = Quote (kind, inner); notes; meta = []; tail = [];
            span = {start = token.span.start; finish = inner.span.finish}}
      | Meta_name name ->
          let notes = notes @ comments () in
          (match (peek ()).kind with
           | End | Close _ -> error token "E_UNEXPECTED" "Metadata needs a form after it"
           | _ ->
               let* inner = form notes (depth + 1) in
               Ok {inner with meta = name :: inner.meta;
                 span = {start = token.span.start; finish = inner.span.finish}})
      | Close c -> error token "E_UNEXPECTED" (Printf.sprintf "Unexpected %C" c)
      | Comment _ | End -> error token "E_UNEXPECTED" "Expected a form"
  and group opening c id notes depth reversed =
    let leading = comments () in
    let token = peek () in
    let finish tail =
      let items = List.rev reversed in
      let span = {Diagnostic.start = opening.span.start; finish = token.span.finish} in
      let node = match c with '(' -> List items | '[' -> Vec items | _ -> Map items in
      let whole = {id; node; notes; meta = []; tail; span} in
      match c, items with
      | '(', {node = Sym "->"; _} :: [] -> error opening "E_THREAD" "-> needs a value to thread"
      | '(', {node = Sym "->"; _} :: x :: steps ->
          (* (-> x (f a) (g b)) is (g (f x a) b); the steps keep their own ids and spans *)
          let step acc (s : t) = match s.node with
            | List (h :: args) when (match h.node with Sym _ -> true | _ -> false) ->
                Ok {s with node = List (h :: acc :: args)}
            | _ -> fail source s.span.start s.span.finish "E_THREAD"
                "A -> step is a call, as in (sop/normals :cusp_angle 0.6)" in
          let rec fold acc = function
            | [] -> Ok acc
            | [last] -> Result.map (fun r -> {r with id; notes = notes @ r.notes; tail = tail @ r.tail; span}) (step acc last)
            | s :: rest -> let* acc = step acc s in fold acc rest in
          if steps = [] then Ok {x with notes = notes @ x.notes}
          else if depth + List.length steps > depth_limit then error opening "E_DEPTH"
            (Printf.sprintf "S-expression nesting exceeds %d forms" depth_limit)
          else fold x steps
      | _ -> Ok whole in
    match token.kind with
    | Close actual when actual = close_of c -> ignore (take ()); finish leading
    | Close actual -> error token "E_UNEXPECTED"
        (Printf.sprintf "Expected %C to close the %C on line %d, found %C"
          (close_of c) c (Diagnostic.position_of_offset source opening.span.start).line actual)
    | End -> error opening "E_UNCLOSED" (Printf.sprintf "This %C is never closed" c)
    | _ ->
        let* item = form leading (depth + 1) in
        group opening c id notes depth (item :: reversed) in
  let rec all reversed =
    let leading = comments () in
    match (peek ()).kind with
    | End ->
        (* ponytail: comments after the last form become the tail of that form
           when it is a container, and are otherwise dropped. *)
        (match reversed, leading with
         | last :: rest, _ :: _ when (match last.node with
             | List _ | Vec _ | Map _ -> true | _ -> false) ->
             Ok (List.rev ({last with tail = last.tail @ leading} :: rest))
         | _ -> Ok (List.rev reversed))
    | _ -> let* item = form leading 0 in all (item :: reversed) in
  all [])

let make ?(notes = []) ?(meta = []) node =
  {id = 0; node; span = {start = 0; finish = 0}; notes; meta; tail = []}

let rec equal a b = a == b || (a.notes = b.notes && a.meta = b.meta && a.tail = b.tail
  && match a.node, b.node with
  | Sym a, Sym b | Kw a, Kw b | Num a, Num b | Str a, Str b -> String.equal a b
  | List a, List b | Vec a, Vec b | Map a, Map b -> List.equal equal a b
  | Quote (a, x), Quote (b, y) -> a = b && equal x y
  | _ -> false)

let head x = match x.node with List ({node = Sym h; _} :: _) -> Some h | _ -> None

let children x = match x.node with
  | Sym _ | Kw _ | Num _ | Str _ -> []
  | List xs | Vec xs | Map xs -> xs
  | Quote (_, y) -> [y]

let renumber first x =
  let next = ref first in
  let rec go x =
    let id = !next in incr next;
    let node = match x.node with
      | (Sym _ | Kw _ | Num _ | Str _) as leaf -> leaf
      | List xs -> List (List.map go xs)
      | Vec xs -> Vec (List.map go xs)
      | Map xs -> Map (List.map go xs)
      | Quote (k, y) -> Quote (k, go y) in
    {x with id; node} in
  let x = go x in (x, !next)
(* Data selectors are positional text, including their keyword spelling. *)
let attribute_args head args =
  let attribute=List.mem head ["sop/attr";"sop/attr_checked";"sop/with_attr";"sop/has_attr";"sop/attr_type";"sop/attr_names";"sop/attr_entries";"attr";"attr_checked";"with_attr";"has_attr"] in
  let group=List.mem head ["sop/group";"sop/group_names";"sop/group_order";"sop/with_group"] in
  let attribute_metadata=List.mem head ["sop/rename_attr";"sop/swap_attr"] in
  let group_metadata=List.mem head ["sop/rename_group";"sop/delete_group"] in
  let positional=attribute || head="sop/count" in
  if not positional && not group && not attribute_metadata && not group_metadata
      && head<>"array/range" && head<>"array/max_length" then args else
  let rec selectors position=function
    |({node=Kw("owner"|"type"|"selection"|"default");_}as key)::value::rest->key::value::selectors position rest
    |({node=Kw name;_}as selector)::rest when position=1->
        {selector with node=Str name}::selectors(position+1)rest
    |first::rest->first::selectors(position+1)rest|[]->[] in
  let args=if positional then selectors 0 args else args in
  let rec enums=function
    | ({node=Kw kind;_}as key)::({node=Sym name;_}as value)::rest
      when (attribute && kind="owner" && List.mem name ["point";"vertex";"primitive";"detail"])
        || (group && kind="owner" && List.mem name ["point";"vertex";"primitive";"edge"])
        || (head="sop/group" && kind="as_owner" && name="point")
        || (attribute_metadata && kind="owner" && List.mem name ["point";"vertex";"primitive";"detail";"any"])
        || (group_metadata && kind="owner" && List.mem name ["point";"vertex";"primitive";"edge";"any"])
        || ((attribute_metadata || group_metadata) && kind="conflict" && List.mem name ["skip";"error";"overwrite";"union"])
        || (head="sop/swap_attr" && kind="method" && List.mem name ["swap";"move";"copy"])
        || (kind="type" && (attribute || head="array/range" || head="array/max_length") && List.mem name (if attribute then ["float";"int";"vec2";"vec3";"vec4";"text";"float_array";"int_array"] else if head="array/max_length" then ["float";"int";"text";"vec2";"vec3";"vec4"] else ["float";"int"])) ->
        key::{value with node=Str name}::enums rest
    | first::rest->first::enums rest|[]->[] in
  enums args
