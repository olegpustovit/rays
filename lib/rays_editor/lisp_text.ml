(* The workspace Lisp as the text area's [Pxui.Ui.language]: an error-tolerant lexer colours
   comments, strings, numbers, keywords, [^:meta] and the head of every form, and brackets by
   depth (an unmatched one in the invalid colour); Enter indents as a Lisp editor does: a body
   form ([let*], [for], [defn], ...) two columns in from its paren, a call under its first
   argument, a vector under its first element.  Over the same tokens and a [vocab] of the
   catalog it completes the token at the caret (kinds, parameters, choices, forms, operators,
   bindings in scope, ranked), describes the token under the pointer and finds the number a
   drag changes.  ponytail: the text is lexed once per frame per need (colours, pairs, indent,
   completions); the workspace texts are a few hundred lines. *)

let separator = Flow.Syntax.separator
let number = Flow.Syntax.number

open Flow.Lisp_scan

(* The lexing of the last four texts asked for, most recent first: a frame asks for the same text
   several times and the next frame asks again, so a text is lexed once per version.  A hit moves
   to the front so the long text outlives the caret prefixes the completions lex.  ponytail: the
   result is shared, never mutated by readers. *)
let lexed = Lru.Identity_list.create ~equal:String.equal 4

let lex text =
  match Lru.Identity_list.find lexed text with
  | Some tokens -> tokens
  | None ->
      let tokens = Flow.Lisp_scan.lex text in
      Lru.Identity_list.add lexed text tokens;
      tokens

let colorize_now (theme : Pxui.Theme.t) text =
  (* kit rev 3: forms in the float blue, a kind's namespace in the vector purple, keywords in the
     bool red, numbers green, strings in the record green; brackets and comments recede to ink-3 *)
  let ports = Pxui.Theme.ports theme and quiet = Pxui.Theme.ink_3 theme in
  let tokens, _, _ = lex text in
  Array.to_list
    (Array.mapi
       (fun i t ->
         let span color = [ (t.start, t.stop, color) ] in
         (* the name of a typed input, [(floors : int 5)], is a binding, not a form: ink *)
         let declares =
           i + 1 < Array.length tokens
           && tokens.(i + 1).kind = Sym
           && String.sub text tokens.(i + 1).start (tokens.(i + 1).stop - tokens.(i + 1).start)
              = ":"
         in
         match t.kind with
         | Comment | Meta -> span quiet
         | Str -> span ports.record
         | Num -> span ports.int
         | Kw -> span ports.bool
         | Head when declares -> []
         | Head -> (
             match String.index_from_opt text t.start '/' with
             | Some slash when slash < t.stop - 1 -> [ (t.start, slash + 1, ports.vec3) ]
             | _ -> span ports.float)
         | Sym -> []
         | Open _ | Close _ -> span quiet
         | Unmatched -> span Pxui.Theme.invalid)
       tokens)
  |> List.concat

(* the colours of the last text, for the theme it was made with *)
let coloured : (Pxui.Theme.t * string * (int * int * Rays_math.Color.t) list) option ref = ref None

let colorize theme text =
  match !coloured with
  | Some (t, s, spans) when t == theme && String.equal s text -> spans
  | _ ->
      let spans = colorize_now theme text in
      coloured := Some (theme, text, spans);
      spans

let brackets text =
  let _, pairs, _ = lex text in
  pairs

(* One kind of the catalog as the text editor sees it: the qualified name a head writes, its
   context (the namespace), its label and category, the slots and the parameters. *)
type entry = {
  qualified : string;
  context : string;
  title : string;
  category : string list;
  slots : (string * bool) list; (* name, required *)
  fields : Param.field_view list;
}

type vocab = { kinds : entry list; by_name : (string, entry) Hashtbl.t; ops : Flow.Op.t list }

let vocab ?(ops = []) (descriptors : Flow_sop.Catalog.descriptor list) =
  let by_name = Hashtbl.create 256 in
  let kinds =
    List.filter_map
      (fun (d : Flow_sop.Catalog.descriptor) ->
        if Hashtbl.mem by_name d.qualified then None
        else begin
          let entry =
            {
              qualified = d.qualified;
              context =
                (match Flow.Context.of_qualified d.qualified with
                | Some context -> Flow.Context.name context
                | None -> (
                    match String.index_opt d.qualified '/' with
                    | Some i -> String.sub d.qualified 0 i
                    | None -> "sop"));
              title = d.label;
              category = d.category;
              slots = List.map (fun (n, r) -> (n, r = Sop.Edit_graph.Required)) d.slots;
              fields = d.fields;
            }
          in
          Hashtbl.add by_name d.qualified entry;
          Some entry
        end)
      descriptors
  in
  { kinds; by_name; ops }

let empty_vocab = vocab []

(* the one-line docs of the special forms; [specials] are the checker's forms ({!Flow.Workspace.special_forms})
   with theirs, so a form the language does not have is never offered *)
let special_docs =
  [
    ("workspace", "(workspace name graphs...) · the document: graphs, defns, macros");
    ( "import",
      "(import \"relative/path.rays\") · shared definitions and graphs beside the workspace" );
    ( "graph",
      "(graph name :context sop [inputs] body) · a network; the body is a let* or an expression" );
    ("defn", "(defn name :context value [(p : type) ...] body) · a function called as (name args)");
    ( "defnode",
      "(defnode sop/name [slots and parameters] body) · a reusable SOP kind with typed inputs and \
       parameter defaults" );
    ("defmacro", "(defmacro name [params] `template) · a syntax template; ~p fills a hole");
    ("let*", "(let* [name expr ...] result) · sequential bindings: the graph's nodes");
    ("for", "(for [i (range n)] body) · repeat over lists; geometry merges, numbers are summed");
    ("fold", "(fold [acc init] [i xs] body) · feed the result back: acc is the previous value");
    ( "iterate",
      "(iterate n init (fn [x] body)) · apply the function n times; n is a value, not a literal" );
    ("state", "(state [s init] step) · fold frames; s is the previous frame's value");
    ("scan", "(scan [acc init] [i xs] body) · like fold, keeping every step as a list");
    ("sum", "(sum [i xs] body) · add the body over the list");
    ("if", "(if test then else) · a branch; both arms have the same type");
    ( "with_error",
      "(with_error protected mapper) · map and rethrow a reached error; success and cancellation \
       bypass the mapper" );
    ( "error_rows",
      "(error_rows flags mapper :policy \"first\") · report an integer 0/1 failure mask; \
       range-last uses native cook grain" );
    ("cond", "(cond test expr ... :else expr) · the first test that holds");
    ("case", "(case expr literal expr ... :else expr) · match a literal");
    ("fn", "(fn [a b] body) · an anonymous function for map, filter, reduce, sort-by");
    ("ref", "(ref graph :input value ...) · another graph's result with input overrides");
    ("values", "(values :a x :b y) · a record of named results");
    ("list", "(list a b c) · a homogeneous list");
    ("concat", "(concat xs ys) · join lists");
    ("str", "(str a b) · text from parts");
    ("get", "(get record :field) · read a field; also record.field");
    ("assoc", "(assoc record :field value) · a record with a field changed");
    ("map", "(map f xs) · apply f to each item");
    ("filter", "(filter f xs) · the items f accepts");
    ("reduce", "(reduce f init xs) · fold a function over a list");
    ("sort-by", "(sort-by f xs) · sort by a key function");
    ("quote", "(quote form) · the form itself, unevaluated");
    ("quasiquote", "`form · a template; ~x fills, ~@xs splices");
    ("unquote", "~x · fill a hole of a template");
    ("unquote-splicing", "~@xs · splice a list into a template");
  ]

let specials =
  List.filter_map
    (fun form -> Option.map (fun doc -> (form, doc)) (List.assoc_opt form special_docs))
    ("import" :: Flow.Workspace.special_forms)

let constants =
  [
    ("t", "time in seconds · a binding reading t recooks every frame");
    ("pi", "3.14159...");
    ("true", "boolean true");
    ("false", "boolean false");
    ("nil", "empty geometry · an unconnected slot");
  ]

let contexts () = List.map Flow.Context.name (Flow.Context.all ())

let special_keywords () =
  [
    (":context", "the graph's context: " ^ String.concat ", " (contexts ()));
    (":skip", ":skip [i ...] · iteration tuples a for or scene/merge leaves out");
    (":else", "the branch when no test holds");
    (":keys", "{:keys [a b]} · destructure a record's fields");
    (":bypass", "^:bypass (call ...) · the call passes its first input through");
  ]

let ws_context name =
  match Flow.Context.of_string name with Ok ctx -> ctx | Error _ -> Flow.Context.sop

let ops_of vocab context =
  List.map
    (fun (o : Flow.Op.t) -> (o.name, o.signature))
    (Flow.Op.of_context ~extra:vocab.ops (ws_context context))

let ty_name = Flow.Ty.to_string

let op_doc name (s : Flow.Workspace.op_signature) =
  let pos = List.map (fun (n, t) -> n ^ ":" ^ ty_name t) s.pos in
  let opt = List.map (fun (n, t) -> "[" ^ n ^ ":" ^ ty_name t ^ "]") s.opt in
  let rest = match s.rest with Some (n, t) -> [ n ^ ":" ^ ty_name t ^ "..." ] | None -> [] in
  let kw = List.map (fun (n, t) -> ":" ^ n ^ " " ^ ty_name t) s.kw in
  "(" ^ String.concat " " ((name :: pos) @ opt @ rest @ kw) ^ ") · built-in operator"

let kind_doc (k : entry) =
  let slots =
    match k.slots with [] -> "" | s -> " · inputs " ^ String.concat ", " (List.map fst s)
  in
  let params =
    match List.length k.fields with
    | 0 -> ""
    | n -> Printf.sprintf " · %d parameter%s" n (if n = 1 then "" else "s")
  in
  k.title
  ^ (if k.category = [] then "" else " · " ^ String.concat " / " k.category)
  ^ slots ^ params

let value_text = function
  | Param.Bool_value b -> string_of_bool b
  | Param.Int_value i -> string_of_int i
  | Param.Float_value f -> Printf.sprintf "%g" f
  | Param.Text_value s | Param.Choice_value s -> "\"" ^ s ^ "\""

let kind_text (f : Param.field_view) =
  match f.kind with
  | Param.Toggle_view -> "bool"
  | Param.Integer_view r -> Printf.sprintf "int %d..%d" r.soft_min r.soft_max
  | Param.Floating_view r -> Printf.sprintf "float %g..%g" r.soft_min r.soft_max
  | Param.Text_view -> "text"
  | Param.Choice_view options -> "one of " ^ String.concat ", " (Array.to_list options)

let field_doc (f : Param.field_view) =
  f.label
  ^ (if f.folder = [] then "" else " · " ^ String.concat " / " f.folder)
  ^ " · " ^ kind_text f ^ " · default " ^ value_text f.default
  ^ match f.description with Some d -> " · " ^ d | None -> ""

(* the keyword arguments a kind takes, as (name, type, doc, primary): a vec3 group once, before
   its components *)
let keywords_of (k : entry) =
  List.map
    (function
      | [] -> assert false
      | (f : Param.field_view) :: _ -> (
          match f.vec3 with
          | Some (group, _) ->
              (* the group is named by its first component's label without the axis *)
              let title =
                if String.length f.label > 2 && f.label.[String.length f.label - 2] = ' ' then
                  String.sub f.label 0 (String.length f.label - 2)
                else f.label
              in
              ( group,
                "vec3",
                (title
                ^ (if f.folder = [] then "" else " · " ^ String.concat " / " f.folder)
                ^ " · [x y z]"
                ^ match f.description with Some d -> " · " ^ d | None -> ""),
                f.primary )
          | None -> (f.name, kind_text f, field_doc f, f.primary)))
    (Result.get_ok (Param.group_vec3 k.fields))
  @ List.filter_map
      (fun (f : Param.field_view) ->
        match f.vec3 with Some _ -> Some (f.name, kind_text f, field_doc f, false) | None -> None)
      k.fields

let usage text tokens name =
  Array.fold_left (fun n t -> if t.kind = Head && word text t = name then n + 1 else n) 0 tokens

(* how well [name] matches the typed [prefix]: 3 a prefix, 2 a prefix of a word after _ or /,
   1 a subsequence, 0 none *)
let quality prefix name =
  let p = String.lowercase_ascii prefix and n = String.lowercase_ascii name in
  if p = "" || String.starts_with ~prefix:p n then 3
  else if
    List.exists (String.starts_with ~prefix:p)
      (String.split_on_char '_' n @ String.split_on_char '/' n)
  then 2
  else if Pxui.Ui.fuzzy_match ~query:p n then 1
  else 0

type candidate = {
  name : string;
  insert : string;
  detail : string;
  doc : string;
  group : int;
  uses : int;
}

(* best match first, then the group (kinds before forms before operators; primary parameters
   first), then the names the text already uses, then the alphabet; at most 48 *)
let rank prefix candidates =
  List.filter_map
    (fun c -> match quality prefix c.name with 0 -> None | q -> Some (q, c))
    candidates
  |> List.stable_sort (fun (qa, a) (qb, b) ->
      if qa <> qb then compare qb qa
      else if a.group <> b.group then compare a.group b.group
      else if a.uses <> b.uses then compare b.uses a.uses
      else compare a.name b.name)
  |> List.take 48 |> List.map snd

(* what the document knows beyond the text shown: every graph, the material graphs, the cameras
   of the scene and the names of the layouts (the values of [:material], [:camera], [:active]) *)
type names = {
  graphs : string list;
  materials : string list;
  cameras : string list;
  layouts : string list;
}

let no_names = { graphs = []; materials = []; cameras = []; layouts = [] }
let value_keywords = [ ":material"; ":camera"; ":active" ]

let complete ?(names = no_names) ?(imports = []) vocab text caret =
  let caret = max 0 (min caret (String.length text)) in
  (* the token being typed ends at the caret: the text before it says what it is *)
  let before, _, open_stack = lex (String.sub text 0 caret) in
  let nb = Array.length before in
  let last = if nb = 0 then None else Some before.(nb - 1) in
  let current =
    match last with
    | Some t
      when t.stop = caret
           && match t.kind with Sym | Kw | Head | Meta | Num | Str -> true | _ -> false ->
        Some t
    | _ -> None
  in
  let head_position =
    match (current, last) with
    | Some { kind = Head; _ }, _ -> true
    | None, Some { kind = Unmatched | Open _; stop; _ } when stop = caret && text.[stop - 1] = '('
      ->
        true
    | _ -> false
  in
  let after_value_keyword =
    match last with
    | Some ({ kind = Kw; _ } as t) -> t.stop < caret && List.mem (word text t) value_keywords
    | _ -> false
  in
  if current = None && (not head_position) && not after_value_keyword then []
  else begin
    (* the whole text, so the brackets around the caret are matched *)
    let tokens, _, _ = lex text in
    let prefix =
      match current with
      | Some ({ kind = Str; _ } as t) ->
          String.sub text (t.start + 1) (caret - t.start - 1) (* past the quote *)
      | Some t -> word text t
      | None -> ""
    in
    let replace =
      match current with
      | Some ({ kind = Str; _ } as t) when caret < String.length text && text.[caret] = '"' ->
          (t.start, caret + 1)
      | Some t -> (t.start, caret)
      | None -> (caret, caret)
    in
    let context = context_of text tokens open_stack in
    let enclosing =
      match open_stack with
      | (_, at) :: _ ->
          let i = index_at tokens at in
          if i >= 0 then Some i else None
      | [] -> None
    in
    let enclosing_head =
      match enclosing with
      | Some i when i + 1 < Array.length tokens && tokens.(i + 1).kind = Head ->
          Some (word text tokens.(i + 1))
      | _ -> None
    in
    let prior =
      match enclosing with
      | Some i -> List.filter (fun j -> tokens.(j).stop <= fst replace) (items tokens i)
      | None -> []
    in
    let present =
      List.filter_map
        (fun j -> if tokens.(j).kind = Kw then Some (word text tokens.(j)) else None)
        prior
    in
    let after_kw =
      match List.rev prior with
      | j :: _ when tokens.(j).kind = Kw -> Some (word text tokens.(j))
      | _ -> None
    in
    let kind_of name =
      match Hashtbl.find_opt vocab.by_name name with
      | Some k -> Some k
      | None -> Hashtbl.find_opt vocab.by_name (context ^ "/" ^ name)
    in
    let kind = Option.bind enclosing_head kind_of in
    let mk ?(group = 5) ?(uses = 0) ?insert name detail doc =
      { name; insert = Option.value ~default:name insert; detail; doc; group; uses }
    in
    (* the choices of the keyword before the caret *)
    let choices =
      match (after_kw, kind) with
      | Some kw, Some k -> (
          let kw = String.sub kw 1 (String.length kw - 1) in
          match List.find_opt (fun (f : Param.field_view) -> f.name = kw) k.fields with
          | Some { kind = Param.Choice_view options; label; _ } ->
              List.map
                (fun o -> mk ~group:0 ~insert:("\"" ^ o ^ "\"") o "choice" (label ^ " = " ^ o))
                (Array.to_list options)
          | _ -> [])
      | _ -> []
    in
    (* an operator's text argument: after its keyword, or at its place among the positional ones *)
    let choices =
      if choices <> [] then choices
      else
        match enclosing_head with
        | None -> []
        | Some h ->
            let argument =
              match after_kw with
              | Some kw -> Some (String.sub kw 1 (String.length kw - 1))
              | None when present = [] && prior <> [] ->
                  Option.bind
                    (Flow.Workspace.op_signature ~ops:vocab.ops (ws_context context) h)
                    (fun s -> Option.map fst (List.nth_opt s.pos (List.length prior - 1)))
              | None -> None
            in
            List.map
              (fun o -> mk ~group:0 ~insert:("\"" ^ o ^ "\"") o "choice" (h ^ " · " ^ o))
              (Option.fold ~none:[] ~some:(Flow.Workspace.op_choices ~ops:vocab.ops h) argument)
    in
    (* a new name in a binder's vector is the writer's to choose *)
    let naming =
      match (open_stack, enclosing) with
      | ('[', _) :: _, Some i -> (
          match head_before text tokens i with
          | Some h when List.mem h binders -> h = "fn" || List.length prior mod 2 = 0
          | _ -> false)
      | _ -> false
    in
    (* the value of :material, :camera or :active, from the document *)
    let valued =
      match (after_kw, current) with
      | _, Some { kind = Kw | Head | Meta | Str; _ } -> []
      | Some ":material", _ ->
          List.map
            (fun m -> mk ~group:0 ~insert:("(ref " ^ m ^ ")") m "material" ("(ref " ^ m ^ ")"))
            names.materials
      | Some ":camera", _ ->
          List.map (fun c -> mk ~group:0 c "camera" "a camera of the scene") names.cameras
      | Some ":active", _ ->
          List.mapi
            (fun i l ->
              mk ~group:0 ~insert:(string_of_int i) (Printf.sprintf "%d %s" i l) "layout"
                ("layout " ^ l))
            names.layouts
      | _ -> []
    in
    let candidates =
      match current with
      | _ when naming -> []
      | _ when valued <> [] -> valued
      | Some { kind = Meta; _ } ->
          [ mk "^:bypass" "meta" (List.assoc ":bypass" (special_keywords ())) ]
      (* a keyword: the kind's parameters and slots, an operator's keywords, a form's keywords *)
      | Some { kind = Kw; _ } ->
          let of_kind =
            match kind with
            | Some k ->
                List.filter_map
                  (fun (name, ty, doc, primary) ->
                    if List.mem (":" ^ name) present then None
                    else Some (mk ~group:(if primary then 0 else 1) (":" ^ name) ty doc))
                  (keywords_of k)
                @ List.map
                    (fun (s, required) ->
                      mk ~group:2 (":" ^ s) "input"
                        (s ^ " · "
                        ^ (if required then "required" else "optional")
                        ^ " input of " ^ k.qualified))
                    k.slots
            | None -> []
          in
          let of_op =
            match enclosing_head with
            | Some h -> (
                match Flow.Workspace.op_signature ~ops:vocab.ops (ws_context context) h with
                | Some s ->
                    List.map (fun (k, t) -> mk ~group:0 (":" ^ k) (ty_name t) (op_doc h s)) s.kw
                | None -> [])
            | None -> []
          in
          let of_form =
            match enclosing_head with
            | Some ("graph" | "defn" | "defmacro") -> [ ":context" ]
            | Some ("for" | "scene/merge") -> [ ":skip" ]
            | Some ("cond" | "case") -> [ ":else" ]
            | _ -> []
          in
          of_kind @ of_op
          @ List.map (fun k -> mk ~group:3 k "form" (List.assoc k (special_keywords ()))) of_form
      | Some { kind = Str; _ } -> choices
      | Some { kind = Num; _ } -> []
      (* the head of a call: this context's kinds, the defns, the special forms, the operators *)
      | _ when head_position ->
          let _, defns, _ = bound text tokens caret in
          List.map
            (fun d ->
              mk ~group:0 ~uses:(usage text tokens d) d "defn"
                (d ^ " · a function of this workspace"))
            defns
          @ List.filter_map
              (fun (name, file) ->
                if List.mem name names.graphs then None
                else
                  Some (mk ~group:0 name ("import · " ^ file) (name ^ " · imported from " ^ file)))
              imports
          @ List.filter_map
              (fun (k : entry) ->
                if k.context = context then
                  Some
                    (mk ~group:0 ~uses:(usage text tokens k.qualified) k.qualified
                       (String.concat " / " k.category)
                       (k.qualified ^ " · " ^ kind_doc k))
                else None)
              vocab.kinds
          @ List.map (fun (s, doc) -> mk ~group:1 ~uses:(usage text tokens s) s "form" doc) specials
          @ List.map
              (fun (o, s) -> mk ~group:2 ~uses:(usage text tokens o) o "operator" (op_doc o s))
              (ops_of vocab context)
      (* an argument: a context after :context, a graph after ref, a choice after its keyword,
         else the bindings in scope, the constants and the defns *)
      | Some { kind = Sym; _ } -> (
          match (after_kw, enclosing_head) with
          | Some ":context", _ ->
              List.map (fun c -> mk ~group:0 c "context" (c ^ " graph")) (contexts ())
          | _, Some "ref" when List.length prior <= 1 ->
              let _, _, graphs = bound text tokens caret in
              List.map
                (fun g ->
                  mk ~group:0 g
                    (match List.assoc_opt g imports with
                    | Some file -> "graph · " ^ file
                    | None -> "graph")
                    ("(ref " ^ g ^ ")"))
                (graphs @ List.filter (fun g -> not (List.mem g graphs)) names.graphs)
          | _ when choices <> [] -> choices
          | _ ->
              let bindings, defns, _ = bound text tokens caret in
              let seen = Hashtbl.create 16 in
              List.filter_map
                (fun (name, doc) ->
                  if Hashtbl.mem seen name then None
                  else (
                    Hashtbl.add seen name ();
                    Some (mk ~group:0 name "binding" doc)))
                (List.rev bindings)
              @ List.map (fun (c, doc) -> mk ~group:1 c "constant" doc) constants
              @ List.map (fun d -> mk ~group:2 d "defn" (d ^ " · a function value")) defns)
      | _ -> []
    in
    List.map
      (fun c ->
        { Pxui.Ui.replace; insert = c.insert; label = c.name; detail = c.detail; doc = c.doc })
      (rank prefix candidates)
  end

(* the token at [byte] and what it is *)
let describe vocab text byte =
  let tokens, _, _ = lex text in
  let n = Array.length tokens in
  let rec find i =
    if i >= n then None
    else if tokens.(i).start <= byte && byte < tokens.(i).stop then Some i
    else find (i + 1)
  in
  match find 0 with
  | None -> None
  | Some i ->
      let t = tokens.(i) in
      let w = word text t in
      let enclosing_head = head_before text tokens i in
      let _, _, open_stack = lex (String.sub text 0 t.start) in
      let context = context_of text tokens open_stack in
      let kind_of name =
        match Hashtbl.find_opt vocab.by_name name with
        | Some k -> Some k
        | None -> Hashtbl.find_opt vocab.by_name (context ^ "/" ^ name)
      in
      let signature = Flow.Workspace.op_signature ~ops:vocab.ops (ws_context context) in
      let bare = if String.length w > 1 then String.sub w 1 (String.length w - 1) else "" in
      let doc =
        match t.kind with
        | Head -> (
            match (List.assoc_opt w specials, kind_of w, signature w) with
            | Some doc, _, _ -> Some doc
            | None, Some k, _ -> Some (k.qualified ^ " · " ^ kind_doc k)
            | None, None, Some s -> Some (op_doc w s)
            | None, None, None ->
                let _, defns, _ = bound text tokens t.start in
                if List.mem w defns then Some (w ^ " · a function of this workspace") else None)
        | Kw -> (
            match (Option.bind enclosing_head kind_of, Option.bind enclosing_head signature) with
            | Some k, _ -> (
                match List.find_opt (fun (name, _, _, _) -> name = bare) (keywords_of k) with
                | Some (_, _, doc, _) -> Some doc
                | None ->
                    if List.mem_assoc bare k.slots then Some (bare ^ " · input of " ^ k.qualified)
                    else List.assoc_opt w (special_keywords ()))
            | None, Some s when List.mem_assoc bare s.kw ->
                Some (w ^ " · " ^ ty_name (List.assoc bare s.kw))
            | _ -> List.assoc_opt w (special_keywords ()))
        | Sym -> (
            match List.assoc_opt w constants with
            | Some doc -> Some doc
            | None -> (
                let bindings, defns, graphs = bound text tokens t.start in
                match List.assoc_opt w (List.rev bindings) with
                | Some doc -> Some (w ^ " · " ^ doc)
                | None ->
                    if List.mem w defns then Some (w ^ " · a function of this workspace")
                    else if List.mem w graphs then Some (w ^ " · a graph of this workspace")
                    else None))
        | Num -> Some (w ^ " · drag sideways to change it, Shift for coarse steps")
        | Meta -> List.assoc_opt bare (special_keywords ())
        | Str -> (
            match Option.bind enclosing_head kind_of with
            | Some k -> (
                let v = String.trim (String.map (function '"' -> ' ' | c -> c) w) in
                match
                  List.find_opt
                    (fun (f : Param.field_view) ->
                      match f.kind with
                      | Param.Choice_view options -> Array.exists (( = ) v) options
                      | _ -> false)
                    k.fields
                with
                | Some f -> Some (v ^ " · a choice of :" ^ f.name ^ " (" ^ f.label ^ ")")
                | None -> None)
            | None -> None)
        | Comment | Open _ | Close _ | Unmatched -> None
      in
      Option.map (fun doc -> (t.start, t.stop, doc)) doc

let number_at text byte =
  let tokens, _, _ = lex text in
  Array.to_list tokens
  |> List.find_map (fun t ->
      if t.kind = Num && t.start <= byte && byte < t.stop then Some (t.start, t.stop) else None)

(* the graph named by the [(ref name)] form under [byte] *)
let ref_at text byte =
  let tokens, _, _ = lex text in
  let found = ref None in
  Array.iteri
    (fun i t ->
      if
        i > 0
        && i + 1 < Array.length tokens
        && t.kind = Head
        && word text t = "ref"
        && tokens.(i + 1).kind = Sym && tokens.(i - 1).start <= byte
        && byte <= tokens.(i + 1).stop + 1
      then found := Some (word text tokens.(i + 1)))
    tokens;
  !found

(* the plain name under [byte] (not a head, keyword or literal) *)

(* the "#rrggbb" literals of the text with their colour *)
(* what a character typed at [caret] lands in: inside a string (the quotes' insides, an open one to
   the end) or after the semicolon of a comment, else code *)
let context_at text caret =
  let tokens, _, _ = lex text in
  match
    Array.find_opt
      (fun t ->
        t.start < caret && caret <= t.stop
        && (t.kind = Comment
           || (t.kind = Str && (caret < t.stop || t.stop - t.start < 2 || text.[t.stop - 1] <> '"'))
           ))
      tokens
  with
  | Some { kind = Comment; _ } -> `Comment
  | Some _ -> `String
  | None -> `Code

let color_chips text =
  let tokens, _, _ = lex text in
  Array.fold_right
    (fun t acc ->
      if t.kind <> Str || t.stop - t.start < 9 || text.[t.stop - 1] <> '"' then acc
      else
        match Rays_math.Color.hex (String.sub text (t.start + 1) (t.stop - t.start - 2)) with
        | Ok color -> (t.start, t.stop, color) :: acc
        | Error _ -> acc)
    tokens []

let language ?(vocab = empty_vocab) ?(names = no_names) ?(imports = []) ?(parinfer = false) theme :
    Pxui.Ui.language =
  let infer = parinfer in
  {
    colorize = colorize theme;
    brackets;
    indent;
    pairs = [ ('(', ')'); ('[', ']'); ('{', '}'); ('"', '"') ];
    complete = complete ~names ~imports vocab;
    describe = describe vocab;
    number_at;
    context_at;
    comment = ";";
    rewrite = (if infer then Some Flow.Parinfer.rewrite else None);
  }
