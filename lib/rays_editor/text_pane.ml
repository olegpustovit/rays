(* The workspace text pane: three tabs over the Lisp text.
   Selection prints the selected binding's top-level ancestor with its upstream
   closure and marks the selection; Graph prints the active graph; Document is
   the whole workspace, an editable draft with an atomic Check and apply.

   Pure state and text here; [view] draws it and returns intents, [Core]
   applies them (the model is never touched inside [Ui.frame]). *)
module S = Flow.Syntax

type tab = Selection | Graph | Document
type path = Flow.Workspace.path

type shown = {
  graph : string;  (* the graph the Selection and Graph tabs read *)
  text : string;
  mark : (int * int) option;  (* byte span of the selected binding in [text] *)
  key : path;  (* what the Selection tab shows: the selected binding's path, else [graph] *)
  applied : string Lazy.t;  (* the whole document's text, for the draft's dirty mark *)
  body : (S.t * (int * Diagnostic.span) list) option;  (* the graph's body and the spans of [text], for the caret *)
  source : S.t list;
  spans : Flow.Lisp.spans;
  applied_spans : Flow.Lisp.spans Lazy.t;
}

(* The find bar: the query, the replacement, whether case counts, whether the replace row is
   open, the byte the current match is looked for from, and whether the query field takes the
   keyboard this frame. *)
type find = { query : string; replace : string; case : bool; replacing : bool; at : int; grab : bool }

(* What the keys ask of the pane: resolved by [view], which knows the text, the caret and the
   selection; [Select] needs none of them. *)
type command = Find | Find_replace | Find_selection | Find_next | Find_previous | Definition | References
  | Select of int * int

(* What the text area is asked to do once: replace ranges (one undo step), then select. *)
type request = { select : int * int; splice : (int * int * string) list }

(* The diagnostics found in the text [at]; a later text shows one only while the text up to the end
   of its line still reads the same ({!visible}), so no mark names a line it has drifted from. *)
type checked = { at : string; found : Diagnostic.t list }

(* One Graph tab's unapplied text, the document it began from and the errors of its last apply:
   every graph keeps its own. *)
type slot = { gtext : string; gbase : Editor_document.Workspace_doc.t option; gerrors : checked }

(* The parse of a draft: [found] is [None] until [seen] has rested for the debounce, and
   belongs to [seen] alone. *)
type live = { seen : string; since : float; found : Diagnostic.t list option }

type state = {
  tab : tab;
  draft : string option;  (* the Document tab's unapplied text *)
  binding_draft : (path * string) option;  (* the Selection tab's unapplied text, and its key *)
  graphs : (string * slot) list;  (* the Graph tab's unapplied texts, by graph *)
  doc_base : Editor_document.Workspace_doc.t option;
  binding_base : Editor_document.Workspace_doc.t option;
  doc_errors : checked;  (* of the last refused apply *)
  binding_errors : checked;
  live : live option;
  wrap : bool;  (* long lines continue on the next row *)
  parinfer : bool;  (* closing brackets follow indentation (Lisp_text.parinfer_text) *)
  menu : (float * float) option;  (* the right-click menu, while open *)
  picker : (int * int * bool) option;  (* the colour literal (byte range with its quotes) being edited, and whether it changed *)
  scrubbing : bool;
  target : path option;  (* a definition revealed in Document, including macros *)
  find : find option;
  pending : command list;
  request : request option;
  nonce : int;  (* raised to scroll the same offset into view again *)
  cache : ((S.t list * Editor_document.Workspace_doc.t option * string * path option * tab) * shown) option;
}

let no_errors = { at = ""; found = [] }

let initial = { tab = Selection; draft = None; binding_draft = None; graphs = [];
  doc_base = None; binding_base = None;
  doc_errors = no_errors; binding_errors = no_errors; live = None; wrap = false; parinfer = true; menu = None; picker = None; scrubbing = false; target = None;
  find = None; pending = []; request = None; nonce = 0; cache = None }

(* ---- reading the source ---- *)

let root_form (source : S.t list) name =
  match source with
  | { S.node = S.List (_ :: _ :: items); _ } :: _ ->
      List.find_opt (fun (item : S.t) -> match item.node with
        | S.List ({ S.node = S.Sym ("graph" | "defn" | "defmacro" as h); _ } :: { S.node = S.Sym n; _ } :: _) ->
            (if h = "defn" then "def:" ^ n else if h = "defmacro" then "macro:" ^ n else n) = name
        | _ -> false) items
  | _ -> None

let last (f : S.t) = List.nth_opt (List.rev (S.children f)) 0

let pairs = S.pairs

let scope (e : S.t) = match e.node with
  | S.List [ { S.node = S.Sym "let*"; _ }; { S.node = S.Vec bs; _ }; res ]
    when List.length bs mod 2 = 0 -> Some (pairs bs, res)
  | _ -> None

(* the scope inside a binding's value: itself, or a loop's body *)
let enter (v : S.t) = match scope v, S.head v with
  | Some _, _ -> v
  | None, Some ("for" | "fold" | "scan" | "sum" | "state" | "fn") -> Option.value ~default:v (last v)
  | None, _ -> v

(* the binding a path names (pattern and expression) below a graph's body *)
let rec find_binding (cur : S.t) = function
  | [] -> None
  | name :: rest ->
      (* a nested node: the binding that holds it, then the inputs down to it *)
      let name, keys = Option.value (Flow_graph.Flow_edit.leaf_keys name) ~default:(name, []) in
      let found = match scope cur with
        | Some (_, res) when name = "@result" -> Some (None, res)
        | Some (ps, _) -> List.find_map (fun ((p : S.t), v) ->
            if Flow_graph.Flow_edit.pat_key p = name then Some (Some p, v) else None) ps
        | None when name = "@result" -> Some (None, cur)
        | None -> None in
      let found = if keys = [] then found else
        Option.bind found (fun (_, v) ->
          List.fold_left (fun e key -> Option.bind e (fun e -> Flow_graph.Flow_edit.arg_of e key)) (Some v) keys
          |> Option.map (fun e -> None, e)) in
      match found with
      | Some (p, v) when rest = [] -> Some (p, v)
      | Some (_, v) -> find_binding (enter v) rest
      | None -> None

let binding source = function
  | graph :: names ->
      Option.bind (root_form source graph) (fun root ->
        Option.bind (last root) (fun body -> find_binding body names))
  | [] -> None

(* Pasted text as bindings of [scope] in [graph]: "name expr" pairs, or bare expressions named by
   their head.  The names are paired with the bindings by position (two forms may share a head),
   each made free of the graph's names and of the ones before it; a pasted binding that reads an
   earlier pasted one reads its new name.  The renames are substituted at once, through
   placeholders, so a new name that is another pasted binding's old name is not renamed again. *)
let paste_ops source ~graph ~scope text =
  let module F = Flow_graph.Flow_edit in
  let rec named = function
    | { S.node = S.Sym n; _ } :: v :: rest -> Option.map (fun r -> (n, v) :: r) (named rest)
    | [] -> Some [] | _ -> None in
  let head (f : S.t) = match S.head f with
    | Some h -> (match String.rindex_opt h '/' with
        | Some i -> String.sub h (i + 1) (String.length h - i - 1) | None -> h)
    | None -> "value" in
  match S.parse text with
  | Error _ | Ok [] -> Error "The clipboard holds no Lisp bindings"
  | Ok forms ->
      let bindings, reading = match named forms with
        | Some ps -> ps, true | None -> List.map (fun f -> head f, f) forms, false in
      let chosen = ref [] in
      let fresh n =
        let rec pick k =
          let name = F.fresh_name source ~root:graph (if k = 0 then n else Printf.sprintf "%s_%d" n k) in
          if List.mem name !chosen then pick (k + 1) else (chosen := name :: !chosen; name) in
        pick 0 in
      let _, ops = List.fold_left (fun (earlier, ops) (old, expr) ->
        let name = fresh old in
        (* what the earlier pasted bindings are called now, the latest of a name first *)
        let seen = ref [] in
        let renames = List.filter (fun (o, n) ->
          reading && o <> n && not (List.mem o !seen) && (seen := o :: !seen; true)) earlier in
        let held = List.mapi (fun i (o, n) -> o, Printf.sprintf "@paste%d" i, n) renames in
        let expr = List.fold_left (fun e (o, hold, _) -> F.rename_ref o hold e) expr held in
        let expr = List.fold_left (fun e (_, hold, n) -> F.rename_ref hold n e) expr held in
        (old, name) :: earlier, F.Add_node { scope; name; expr } :: ops) ([], []) bindings in
      Ok (List.rev ops)

let rec max_id (f : S.t) = List.fold_left (fun m c -> max m (max_id c)) f.id (S.children f)

let span_of spans (f : S.t) = List.assoc_opt f.id spans

(* the marked byte range of a binding: its name through its expression *)
let mark spans (found : (S.t option * S.t) option) = match found with
  | None -> None
  | Some (p, v) ->
      (match Option.bind p (span_of spans), span_of spans v with
       | Some a, Some b -> Some (a.Diagnostic.start, b.finish)
       | None, Some b -> Some (b.start, b.finish)
       | _ -> None)

(* the path of the innermost binding whose text holds [byte] (name through expression) *)
let graph_at (shown : shown) byte =
  match shown.source with
  | {S.node = S.List (_ :: _ :: items); _} :: _ ->
      List.find_map (fun (root : S.t) ->
        match S.children root, span_of shown.spans root with
        | {S.node = S.Sym ("graph" | "defn" | "defmacro" as h); _} :: {node = S.Sym name; _} :: _, Some span
          when span.start <= byte && byte < span.finish ->
            Some ((if h = "defn" then "def:" else if h = "defmacro" then "macro:" else "") ^ name, root)
        | _ -> None) items
  | _ -> None

let binding_at (shown : shown) byte =
  let graph, body = match shown.body with
    | Some body -> shown.graph, Some body
    | None -> (match graph_at shown byte with
        | Some (graph, root) -> graph, Option.map (fun body -> body, shown.spans) (last root)
        | None -> shown.graph, None) in
  match body with
  | None -> None
  | Some (body, spans) ->
      let holds (f : S.t) = match span_of spans f with
        | Some s -> s.Diagnostic.start <= byte && byte <= s.finish | None -> false in
      (* the nested node the byte is in, innermost: a step of a ->, a call inside a call *)
      let rec nested prefix leaf (v : S.t) =
        let body = enter v in
        let inner = if body != v || scope v <> None then within (prefix @ [leaf]) body else None in
        match inner with Some path -> path | None ->
          match List.find_opt (fun (_, a) -> holds a) (Flow_graph.Flow_edit.nested_nodes v) with
          | Some (key, a) -> nested prefix (Flow_graph.Flow_edit.nested_leaf leaf key) a
          | None -> prefix @ [ leaf ]
      and within prefix cur = match scope cur with
        | None -> (match cur.node with
            | S.List _ | S.Map _ when holds cur -> Some (nested prefix "@result" cur)
            | _ -> None)
        | Some (bindings, res) ->
            let inside leaf v = Some (nested prefix leaf v) in
            (match List.find_map (fun ((p : S.t), v) ->
              match span_of spans p, span_of spans v with
              | Some a, Some b when a.Diagnostic.start <= byte && byte <= b.Diagnostic.finish ->
                  inside (Flow_graph.Flow_edit.pat_key p) v
              | _ -> None) bindings with
             | Some _ as found -> found
             | None -> if holds res && (match res.node with S.List _ | S.Map _ -> true | _ -> false)
                 then inside "@result" res else None) in
      let found = match within [] body with
        | Some _ as found -> found
        | None -> if scope body = None && holds body && Flow_graph.Flow_edit.node_call body
            then Some (nested [] "@result" body) else None in
      Option.map (fun names -> graph :: names) found

(* Resolve a wire/read in its lexical scope, from the inner scope outwards.
   A dotted record output still belongs to the binding before its first dot. *)
let reference_target source ~from name =
  let module E = Flow_graph.Flow_edit in
  let name = match String.index_opt name '.' with
    | Some i -> String.sub name 0 i | None -> name in
  let parameter (p : S.t) = match p.node with S.List (p :: _) -> E.pat_names p | _ -> E.pat_names p in
  let rail_names expr = match S.head expr, S.children expr with
    | Some "fn", _ :: {S.node = S.Vec ps; _} :: _ -> List.concat_map parameter ps
    | Some ("for" | "sum" | "fold" | "scan" | "state"), _ :: args ->
        List.concat_map (fun (arg : S.t) -> match arg.node with
          | S.Vec ps -> List.concat_map (fun (p, _) -> E.pat_names p) (pairs ps)
          | _ -> []) (List.take (max 0 (List.length args - 1)) args)
    | _ -> [] in
  let input prefix = match prefix with
    | [graph] -> Option.bind (root_form source graph) (fun root ->
        let params = List.find_map (fun (arg : S.t) -> match arg.node with S.Vec ps -> Some ps | _ -> None)
          (List.drop 2 (S.children root)) in
        if Option.fold ~none:false ~some:(fun ps -> List.exists (fun p -> List.mem name (parameter p)) ps) params
        then Some [graph; ":" ^ name] else None)
    | _ -> None in
  let rec local prefix leaf =
    let expr = match prefix with
      | [graph] -> Option.bind (root_form source graph) last
      | _ -> Option.map snd (binding source prefix) in
    let found = Option.bind expr (fun expr ->
      let body = enter expr in
      let bindings = match scope body with Some (ps, _) -> ps | None -> [] in
      let holder, _ = Option.value ~default:(leaf, []) (E.leaf_keys leaf) in
      let rec before = function
        | [] -> []
        | (p, v) :: _ when E.pat_key p = holder ->
            if S.head v = Some "fn" then [p, v] else []
        | pair :: rest -> pair :: before rest in
      let bindings = if List.exists (fun (p, _) -> E.pat_key p = holder) bindings then before bindings else bindings in
      match List.find_opt (fun (p, _) -> List.mem name (E.pat_names p)) (List.rev bindings) with
      | Some (p, _) -> Some (prefix @ [E.pat_key p])
      | None when E.nested name && binding source (prefix @ [name]) <> None -> Some (prefix @ [name])
      | None when List.mem name (rail_names expr) -> Some prefix
      | None -> input prefix) in
    match found, List.rev prefix with
    | Some _ as found, _ -> found
    | None, child :: (_ :: _ as parent) -> local (List.rev parent) child
    | _ -> None in
  let found = match List.rev from with leaf :: (_ :: _ as prefix) -> local (List.rev prefix) leaf | _ -> None in
  match found with Some _ as found -> found | None ->
    List.find_map (fun key -> if root_form source key <> None then Some [key] else None)
      ["def:" ^ name; "macro:" ^ name; name]

let definition_at (shown : shown) byte =
  let rec symbol (form : S.t) = match form.node, span_of shown.spans form with
    | S.Sym name, Some span when span.start <= byte && byte < span.finish -> Some name
    | _ -> List.find_map symbol (S.children form) in
  Option.bind (List.find_map symbol shown.source) (fun name ->
    let from = Option.value ~default:[shown.graph] (binding_at shown byte) in
    match binding shown.source from with
    | Some (Some p, _) when (match span_of shown.spans p with
        | Some span -> span.start <= byte && byte < span.finish | None -> false) -> Some from
    | _ -> reference_target shown.source ~from name)

let path_mark source spans = function
  | [graph] -> Option.bind (root_form source graph) (fun root ->
      Option.map (fun (span : Diagnostic.span) -> span.start, span.finish) (span_of spans root))
  | [graph; input] when String.starts_with ~prefix:":" input ->
      let name = String.sub input 1 (String.length input - 1) in
      Option.bind (root_form source graph) (fun root ->
        List.find_map (fun (arg : S.t) -> match arg.node with
          | S.Vec ps -> List.find_map (fun (p : S.t) ->
              let pattern = match p.node with S.List (p :: _) -> p | _ -> p in
              if List.mem name (Flow_graph.Flow_edit.pat_names pattern) then
                Option.map (fun (span : Diagnostic.span) -> span.start, span.finish) (span_of spans p)
              else None) ps
          | _ -> None) (List.drop 2 (S.children root)))
  | path -> mark spans (binding source path)

(* the bindings of a scene graph that are cameras (the values of [scene/root :camera]) *)
let cameras source graph =
  match Option.bind (Option.bind (root_form source graph) last) scope with
  | None -> []
  | Some (bindings, _) ->
      List.filter_map (fun ((p : S.t), v) -> match S.head v, p.node with
        | Some "scene/camera", S.Sym n -> Some n
        | _ -> None) bindings

let plural n what = Printf.sprintf "%d %s%s" n what (if n = 1 then "" else "s")

(* the top-level ancestor with what it reads, as a [let*] over the
   root bindings it needs; None when the graph body is not a scope *)
let closure (root : S.t) top =
  match Option.bind (last root) scope with
  | None -> None
  | Some (bindings, _) ->
      let declares = List.concat_map (fun ((p, _) as binding) ->
        List.map (fun n -> n, binding) (Flow_graph.Flow_edit.pat_names p)) bindings in
      let rec need acc name = match List.assoc_opt name declares with
        | Some ((_, v) as binding) when not (List.memq binding acc) ->
            List.fold_left need (binding :: acc) (Flow_graph.Flow_edit.free_names v)
        | _ -> acc in
      let needed = need [] top in
      let sub = List.filter (fun b -> List.memq b needed) bindings in
      if sub = [] then None else begin
        let inputs = match List.find_map (fun (c : S.t) -> match c.node with
            | S.Vec inputs -> Some inputs | _ -> None) (S.children root) with
          | None -> []
          | Some inputs -> List.filter_map (fun (i : S.t) -> match i.node with
              | S.List ({ S.node = S.Sym n; _ } :: _)
                when List.exists (fun (_, v) -> List.mem n (Flow_graph.Flow_edit.free_names v)) sub -> Some n
              | _ -> None) inputs in
        let base = max_id root + 1 in
        let form id node = { (S.make node) with id } in
        let note = Printf.sprintf "%s · %s%s" top
          (if List.length sub > 1 then "with " ^ plural (List.length sub - 1) "upstream binding"
           else "no upstream bindings")
          (if inputs = [] then "" else " and inputs " ^ String.concat ", " inputs) in
        Some { (form base (S.List [ form (base + 1) (S.Sym "let*");
          form (base + 2) (S.Vec (List.concat_map (fun (p, v) -> [ p; v ]) sub));
          form (base + 3) (S.Sym top) ])) with notes = [ note;
            "Selection patches named bindings; omitted bindings stay. Change the result in Graph or Document." ] }
      end

(* Build the complete candidate graph before checking: dependent bindings may
   change type together, so checking individual replacements is incorrect. *)
let selection_form source path form =
  let error message = Error (Diagnostic.error ~code:"E_EDIT" message) in
  match path, scope form with
  | graph :: top :: _, Some (patches, result) ->
      let printed f = fst (Flow.Lisp.print [f]) in
      let names = List.filter_map (fun ((p : S.t), _) -> match p.node with
        | S.Sym n -> Some n | _ -> None) patches in
      let invalid = List.find_opt (fun ((p : S.t), _) -> match p.node with
        | S.Sym n -> binding source [graph; n] = None | _ -> true) patches in
      (match invalid with
       | Some (p, _) -> error (Printf.sprintf "%s is not a binding of %s; add or remove bindings in the Graph tab."
           (String.trim (printed p)) graph)
       | None when List.length names <> List.length (List.sort_uniq String.compare names) ->
           error "Duplicate binding names in Selection."
       | None when printed result <> printed (S.make (S.Sym top)) ->
           error "Selection's result must stay unchanged; edit the result in Graph or Document."
       | None ->
           match Option.bind (root_form source graph) (fun root ->
             Option.map (fun body -> root, body) (last root)) with
           | Some (root, body) ->
               (match scope body with
                | Some (bindings, res) ->
                    (* a closure that changes nothing is the graph as it is, not a refusal: a
                       scrub applies on every frame of its drag, also the ones that did not move *)
                    let bindings = List.map (fun (p, old) ->
                      match List.find_opt (fun (q, _) -> q.S.node = p.S.node) patches with
                      | Some (_, v) when printed old <> printed v ->
                          p, (if v.notes = [] then { v with notes = old.notes } else v)
                      | _ -> p, old) bindings in
                    let body = { body with node = S.List [List.hd (S.children body);
                      { (List.nth (S.children body) 1) with node = S.Vec
                          (List.concat_map (fun (p, v) -> [p; v]) bindings) }; res] } in
                    Ok { root with node = S.List (List.rev (body :: List.tl (List.rev (S.children root)))) }
                | None -> error "Expected the shown Selection closure.")
           | None -> error "Selection graph no longer exists.")
  | _, _ -> error "Expected the shown (let* [...] name) closure, or the graph form."

(* The [Set_graph] the text of the Graph or Selection tab means: the one graph (or function) form
   replaces the graph's; a Selection closure is patched into it ({!selection_form}).  The errors
   that are not the reader's carry no position. *)
let graph_op source ~graph ?selection text =
  let error message = Error (Diagnostic.error ~code:"E_EDIT" message) in
  let set form = Flow_graph.Flow_edit.Set_graph { name = graph; form } in
  match S.parse text, selection with
  | Error d, _ -> Error d
  | Ok [ ({ node = S.List ({ node = S.Sym ("graph" | "defn"); _ } :: _); _ } as form) ], _ -> Ok (set form)
  | Ok [ form ], Some path -> Result.map set (selection_form source path form)
  | Ok [ form ], None -> Ok (set form)
  | Ok _, Some _ -> error "Expected the shown (let* [...] name) closure, or the graph form."
  | Ok _, None -> error "Expected the one graph form"

let make_shown_with ?workspace source graph selected tab =
  let printed = lazy (match workspace with None -> Flow.Lisp.print source
    | Some workspace -> Editor_document.Workspace_doc.print workspace) in
  let applied = lazy (fst (Lazy.force printed))
  and applied_spans = lazy (snd (Lazy.force printed)) in
  let key = match selected with Some path -> path | None -> [ graph ] in
  match tab with
  | Document -> { graph; text = Lazy.force applied; mark = Option.bind selected (path_mark source (Lazy.force applied_spans)); key; applied; body = None;
      source; spans = Lazy.force applied_spans; applied_spans }
  | Selection | Graph ->
      (match root_form source graph with
       | None -> { graph; text = ""; mark = None; key; applied; body = None;
           source; spans = []; applied_spans }
       | Some root ->
           let names = match selected with Some (_ :: names) -> names | _ -> [] in
           let form = match tab, names with
             | Selection, top :: _ -> Option.value ~default:root (closure root top)
             | _ -> root in
           let text, spans = Flow.Lisp.print [ form ] in
           { graph; text; mark = Option.bind selected (path_mark source spans); key; applied;
             body = Some ((if form == root then Option.value ~default:root (last root) else form), spans);
             source; spans; applied_spans })

let make_shown source graph selected tab = make_shown_with source graph selected tab

let patched_shown previous workspace shown =
  let module Doc = Editor_document.Workspace_doc in
  if previous.Doc.layout != workspace.Doc.layout || previous.settings != workspace.settings
    || previous.extra != workspace.extra then None else
  Option.bind (Doc.literal_changes ~previous workspace) (fun changes ->
    let rec atoms (old : S.t) (next : S.t) =
      if S.equal old next then Some [] else
      if old.id <> next.id || old.notes <> next.notes || old.meta <> next.meta || old.tail <> next.tail then None else
      match old.node, next.node with
      | Num _, Num _ | Str _, Str _ | Sym _, Sym _ -> Some [next]
      | Vec old, Vec next when List.length old = List.length next ->
          List.fold_left (fun result (old,next) -> Option.bind result (fun values ->
            Option.map (fun more -> List.rev_append more values) (atoms old next)))
            (Some []) (List.combine old next)
      | _ -> None in
    let values = List.fold_left (fun result (change : Editor_document.Literal_edit.change) ->
      Option.bind result (fun values ->
        Option.bind (Flow_graph.Flow_edit.arg_text previous.source change.path (Kw change.field)) (fun old ->
          Option.map (fun more -> List.rev_append more values) (atoms old change.expr)))) (Some []) changes in
    Option.bind values (fun values ->
      (* Another graph may have changed; only forms printed in this tab are patched. *)
      let present spans = let ids = Hashtbl.create (List.length spans) in
        List.iter (fun (id, _) -> Hashtbl.replace ids id ()) spans;
        List.filter (fun (f : S.t) -> Hashtbl.mem ids f.id) values in
      Option.bind (Flow.Lisp.patch_atoms (shown.text, shown.spans) (present shown.spans)) (fun (text, spans) ->
      let applied_spans = Lazy.force shown.applied_spans in
      Option.map (fun (applied, applied_spans) ->
        let body = Option.bind (root_form workspace.source shown.graph) last in
        {shown with source = workspace.source; text; spans; applied = lazy applied;
          applied_spans = lazy applied_spans;
          body = (match shown.body, body with Some _, Some body -> Some (body, spans) | _ -> None);
          mark = Option.map (fun (a,b) ->
            (* Selection marks are form boundaries in the repaired span map. *)
            let boundary old = List.find_map (fun (id, (span : Diagnostic.span)) ->
              if span.start = old then Option.map (fun s -> s.Diagnostic.start) (List.assoc_opt id spans)
              else if span.finish = old then Option.map (fun s -> s.Diagnostic.finish) (List.assoc_opt id spans)
              else None) shown.spans in
            Option.value ~default:a (boundary a), Option.value ~default:b (boundary b)) shown.mark})
        (Flow.Lisp.patch_atoms (Lazy.force shown.applied, applied_spans) (present applied_spans)))))

(* [shown] recomputed only when the source, the graph, the selection or the
   tab changed *)
let shown ?workspace state ~(source : S.t list) ~graph ~selected =
  let graph = match state.tab, state.binding_draft with
    | Selection, Some (graph :: _, _) -> graph | _ -> graph in
  let selected = match state.tab, state.binding_draft, state.target with
    | Selection, Some (path, _), _ -> Some path
    | Document, _, Some path -> Some path
    | _ -> selected in
  let key = (source, workspace, graph, selected, state.tab) in
  let same a b = match a, b with None, None -> true | Some a, Some b -> a == b | _ -> false in
  match state.cache with
  | Some ((s, w, g, p, t), shown) when s == source && same w workspace
      && g = graph && p = selected && t = state.tab ->
      state, shown
  | _ ->
      let patched = match state.cache, workspace with
        | Some ((_, Some previous, g, p, t), shown), Some workspace
          when state.scrubbing && g = graph && p = selected && t = state.tab ->
            patched_shown previous workspace shown
        | _ -> None in
      let shown = match patched with Some shown -> shown
        | None -> make_shown_with ?workspace source graph selected state.tab in
      { state with cache = Some (key, shown) }, shown

(* The printer's token ID names the source form. Prefer the innermost card,
   then address its argument's child; no text is parsed or searched. *)
let scrub_op (shown : shown) (start, finish) replacement =
  let module F = Flow_graph.Flow_edit in
  let id = List.find_map (fun (id, (span : Diagnostic.span)) ->
    if id <> 0 && span.start = start && span.finish = finish then Some id else None) shown.spans in
  Option.bind id (fun id ->
  if not (S.number replacement) then None else
  let value = S.make (S.Num replacement) in
  let rec sub path (form : S.t) =
    if form.id = id then match form.node with Num _ -> Some (List.rev path) | _ -> None
    else List.find_map Fun.id (List.mapi (fun i child -> sub (i :: path) child) (S.children form)) in
  let rec within path form = match scope form with
    | Some (bindings, result) ->
        (match List.find_map (fun (pattern, expr) -> card (path @ [F.pat_key pattern]) expr) bindings with
         | Some _ as found -> found | None -> card (path @ ["@result"]) result)
    | None -> card (path @ ["@result"]) form
  and card path (form : S.t) =
    let inner = if scope form <> None || List.mem (Option.value ~default:"" (S.head form)) ["for";"fold";"scan";"sum";"state";"fn"]
      then within path (enter form) else None in
    match inner with Some _ as found -> found | None ->
    let nested = List.find_map (fun (key, expr) ->
      let reverse = List.rev path in
      card (List.rev (F.nested_leaf (List.hd reverse) key :: List.tl reverse)) expr) (F.nested_nodes form) in
    match nested with Some _ as found -> found | None ->
    let args = match form.node with
      | List (_ :: args) ->
          let rec keys position = function
            | {S.node = Kw name; _} :: expr :: rest -> (F.Kw name, expr) :: keys position rest
            | expr :: rest -> (F.Pos position, expr) :: keys (position + 1) rest | [] -> [] in
          keys 0 args
      | Map fields -> List.map (fun (key, expr) ->
          F.Field (match key.S.node with Kw name -> name | _ -> ""), expr) (pairs fields)
      | _ -> [F.Whole, form] in
    List.find_map (fun (key, expr) -> if F.node_call expr then None else
      Option.map (fun sub -> F.Set_arg {node = path; key; sub; value}) (sub [] expr)) args in
  match shown.source with
  | {S.node = List (_ :: _ :: roots); _} :: _ ->
      List.find_map (fun (root : S.t) -> match root.node with
        | List ({node = Sym ("graph" | "defn" as h); _} :: {node = Sym name; _} :: _) ->
            Option.bind (last root) (within [if h = "defn" then "def:" ^ name else name])
        | _ -> None) roots
  | _ -> None)

(* ---- a draft against a document that changed under it ---- *)

(* Three texts, merged form by form: [base] is what the draft began from, [mine] the draft,
   [theirs] the text now.  A form only one side changed takes that side's; a form both changed
   merges child by child; graphs of a workspace and bindings of a [let*] pair up by name, so one
   added or removed on either side is kept.  [None]: both changed the same value, or [mine] does
   not read.
   ponytail: a binding or keyword argument [theirs] added goes last in its form; keep [theirs]'
   place for it when the order of bindings bites (the merged text is still checked). *)
let merge3 ~base ~mine ~theirs =
  let text f = fst (Flow.Lisp.print [ f ]) in
  let same a b = text a = text b in
  let rec all = function
    | [] -> Some [] | None :: _ -> None
    | Some x :: rest -> Option.map (fun rest -> x :: rest) (all rest) in
  let key (f : S.t) = match f.node with
    | S.List ({ node = S.Sym h; _ } :: { node = S.Sym n; _ } :: _) -> h ^ " " ^ n
    | S.List ({ node = S.Sym h; _ } :: _) -> h
    | _ -> text f in
  let entries = List.map (fun f -> key f, [ f ]) in
  let rec pairs = function
    | p :: v :: rest -> Option.map (fun rest -> (text p, [ p; v ]) :: rest) (pairs rest)
    | [] -> Some [] | [ _ ] -> None in
  let rec merge (b : S.t) (m : S.t) (t : S.t) =
    if same m b then Some t else if same t b || same m t then Some m
    else
      let node make kids = Option.map (fun kids -> { m with node = make kids }) kids in
      match b.node, m.node, t.node with
      | S.List ({ node = S.Sym "let*"; _ } :: { node = S.Vec bv; _ } :: br),
        S.List (head :: ({ node = S.Vec mv; _ } as vec) :: mr),
        S.List (_ :: { node = S.Vec tv; _ } :: tr) ->
          (match pairs bv, pairs mv, pairs tv with
           | Some bv, Some mv, Some tv ->
               Option.bind (keyed bv mv tv) (fun bindings ->
                 node (fun rest -> S.List (head :: { vec with node = S.Vec bindings } :: rest)) (seq br mr tr))
           | _ -> None)
      | S.List ({ node = S.Sym "workspace"; _ } :: _ as bl), S.List ml, S.List tl ->
          node (fun kids -> S.List kids) (keyed (entries bl) (entries ml) (entries tl))
      | S.List bl, S.List ml, S.List tl ->
          node (fun kids -> S.List kids) (match seq bl ml tl with Some _ as kids -> kids | None -> call bl ml tl)
      | S.Vec bl, S.Vec ml, S.Vec tl -> node (fun kids -> S.Vec kids) (seq bl ml tl)
      | S.Map bl, S.Map ml, S.Map tl -> node (fun kids -> S.Map kids) (seq bl ml tl)
      | _ -> None
  (* a call whose argument count differs: what stands before the first keyword pairs by position,
     the keyword arguments by name *)
  and call bl ml tl =
    let rec split before = function
      | ({ S.node = S.Kw _; _ } :: _) as rest ->
          let rec keywords = function
            | { S.node = S.Kw _; _ } :: _ :: rest -> keywords rest | [] -> true | _ -> false in
          List.rev before, if keywords rest then pairs rest else None
      | f :: rest -> split (f :: before) rest
      | [] -> List.rev before, Some [] in
    match split [] bl, split [] ml, split [] tl with
    | (bp, Some bk), (mp, Some mk), (tp, Some tk) ->
        Option.bind (seq bp mp tp) (fun positional ->
          Option.map (fun keywords -> positional @ keywords) (keyed bk mk tk))
    | _ -> None
  and seq bl ml tl =
    if List.length bl = List.length ml && List.length ml = List.length tl
    then all (List.map2 (fun b (m, t) -> merge b m t) bl (List.combine ml tl)) else None
  and keyed bl ml tl =
    let unique l = let keys = List.map fst l in
      List.length (List.sort_uniq String.compare keys) = List.length keys in
    let sames a b = List.length a = List.length b && List.for_all2 same a b in
    if not (unique bl && unique ml && unique tl) then None else
    let mine = List.map (fun (k, m) -> match List.assoc_opt k bl, List.assoc_opt k tl with
      | Some b, Some t -> seq b m t
      | Some b, None -> if sames m b then Some [] else None  (* they removed what I left alone *)
      | None, Some t -> if sames m t then Some m else None   (* both added it *)
      | None, None -> Some m) ml in
    let theirs = List.map (fun (k, t) -> match List.mem_assoc k ml, List.assoc_opt k bl with
      | true, _ -> Some []
      | false, None -> Some t                                 (* they added it *)
      | false, Some b -> if sames t b then Some [] else None) tl in  (* I removed what they left alone *)
    Option.map List.concat (all (mine @ theirs)) in
  match S.parse base, S.parse mine, S.parse theirs with
  | Ok b, Ok m, Ok t -> Option.map (fun forms -> fst (Flow.Lisp.print forms)) (keyed (entries b) (entries m) (entries t))
  | _ -> None

(* ---- find, symbols, lines ---- *)

(* the (start, stop) of every non-overlapping match of [query], in order; ASCII letters fold
   together unless [case] *)
let find_all ~case query text =
  let n = String.length query in
  if n = 0 then [] else begin
    let fold = if case then Fun.id else String.lowercase_ascii in
    let hay = fold text and needle = fold query in
    let rec same i j = j = n || (hay.[i + j] = needle.[j] && same i (j + 1)) in
    let rec go from acc = match
        (let rec at i = if i + n > String.length hay then None else if same i 0 then Some i else at (i + 1) in at from) with
      | Some i -> go (i + n) ((i, i + n) :: acc)
      | None -> List.rev acc in
    go 0 []
  end

(* the match the bar is on: the first one at or after [at], else the first *)
let find_current (f : find) matches =
  match List.find_opt (fun (a, _) -> a >= f.at) matches with
  | None -> List.nth_opt matches 0
  | m -> m

(* the bar after Next or Previous: [at] moves just past the current match, or onto the one before *)
let find_step (f : find) matches direction =
  match find_current f matches with
  | None -> f
  | Some (a, _) ->
      (match direction with
       | `Next -> { f with at = a + 1 }
       | `Previous ->
           (* the last match before [a], else (wrapping) the last one *)
           let last = List.fold_left (fun _ (b, _) -> b) a matches in
           { f with at = List.fold_left (fun t (b, _) -> if b < a then b else t) last matches })

(* Replace the current match, or all of them: the text area's request and the bar after it *)
let find_replace (f : find) matches ~all =
  let n = String.length f.replace in
  match (if all then matches else Option.to_list (find_current f matches)) with
  | [] -> None
  | (a, _) :: _ as chosen ->
      let splice = List.map (fun (a, b) -> a, b, f.replace) chosen in
      if all then Some ({ select = (a, a + n); splice }, { f with at = a })
      else Some ({ select = (a + n, a + n); splice }, { f with at = a + n })

(* the bindings of a text: graphs, functions, macros and the names of a [let*], by the byte
   their name starts at; empty for text that does not read.  ponytail: the text is parsed per
   request, not cached *)
let symbols text =
  match S.parse text with
  | Error _ -> []
  | Ok forms ->
      let rec walk acc (f : S.t) =
        let acc = match f.node with
          | S.List ({ S.node = S.Sym ("graph" | "defn" | "defmacro" as kind); _ }
                    :: { S.node = S.Sym name; span; _ } :: _) -> (name, kind, span.start) :: acc
          | S.List [ { S.node = S.Sym "let*"; _ }; { S.node = S.Vec bs; _ }; _ ] ->
              List.fold_left (fun acc ((p : S.t), _) -> match p.node with
                | S.Sym name -> (name, "let*", p.span.start) :: acc | _ -> acc) acc (pairs bs)
          | _ -> acc in
        List.fold_left walk acc (S.children f) in
      List.rev (List.fold_left walk [] forms)

(* the byte where 1-based [line] of [text] starts (the last line's for a line past the end), and
   where the line starting at [from] ends *)
let line_start text line =
  let rec go k from = if k <= 1 then from else
      match String.index_from_opt text from '\n' with Some j -> go (k - 1) (j + 1) | None -> from in
  go line 0
let line_end text from = match String.index_from_opt text from '\n' with Some j -> j | None -> String.length text

(* "12" or "12:5": the byte of that 1-based line (and character column), past the end clamped *)
let line_target text input =
  let number s = match int_of_string_opt (String.trim s) with Some n when n >= 1 -> Some n | _ -> None in
  let line, column = match String.index_opt input ':' with
    | Some i -> number (String.sub input 0 i), number (String.sub input (i + 1) (String.length input - i - 1))
    | None -> number input, Some 1 in
  match line, column with
  | Some line, Some column ->
      let from = line_start text line in
      let stop = line_end text from in
      let rec advance i k = if k <= 1 || i >= stop then i
        else advance (i + Uchar.utf_decode_length (String.get_utf_8_uchar text i)) (k - 1) in
      Some (advance from column)
  | _ -> None

(* the Graph tab's slot for a graph *)
let graph_slot state graph = List.assoc_opt graph state.graphs

(* the text the pane shows for its tab: the draft when there is one *)
let current_text state (shown : shown) = match state.tab with
  | Document -> Option.value ~default:(Lazy.force shown.applied) state.draft
  | Graph -> (match graph_slot state shown.graph with Some s -> s.gtext | None -> shown.text)
  | Selection -> (match state.binding_draft with Some (k, t) when k = shown.key -> t | _ -> shown.text)

(* The state after a command: [Select] is a request at once, the rest wait for [view]. *)
let command state = function
  | Select (a, b) -> { state with request = Some { select = (a, b); splice = [] }; nonce = state.nonce + 1 }
  | c -> { state with pending = state.pending @ [ c ] }

(* ---- errors ---- *)

let line_of text (d : Diagnostic.t) = match d.position, d.span with
  | Some p, _ -> Some p.line
  | None, Some s -> Some (Diagnostic.position_of_offset text s.start).line
  | None, None -> None

let real_errors = List.filter (fun (d : Diagnostic.t) -> d.severity = Diagnostic.Error_severity)

(* the byte where 1-based [line] of [text] ends *)
let line_stop text line = line_end text (line_start text line)

(* every symbol of the text with its span, in order; empty for text that does not read *)
let symbol_spans text = match S.parse text with
  | Error _ -> []
  | Ok forms ->
      let rec walk acc (f : S.t) = List.fold_left walk
        (match f.node with S.Sym name -> (name, f.span) :: acc | _ -> acc) (S.children f) in
      List.rev (List.fold_left walk [] forms)

let symbol_at text byte = List.find_map (fun (name, (span : Diagnostic.span)) ->
  if span.start <= byte && byte <= span.finish then Some name else None) (symbol_spans text)

let references text name = List.filter_map (fun (n, (span : Diagnostic.span)) ->
  if n <> name then None else
  let line = (Diagnostic.position_of_offset text span.start).line in
  Some (span.start, String.trim (String.sub text (line_start text line) (line_stop text line - line_start text line)))) (symbol_spans text)


(* the diagnostics of [c] that still hold for [text]: those whose line, and span, lie within the
   bytes [text] still shares with the text they were found in *)
let visible (c : checked) text =
  if c.at = text then c.found else
  let n = min (String.length c.at) (String.length text) in
  let shared = ref 0 in
  while !shared < n && c.at.[!shared] = text.[!shared] do incr shared done;
  List.filter (fun (d : Diagnostic.t) -> match line_of c.at d with
    | Some line -> !shared >= max (line_stop c.at line) (match d.span with Some s -> s.finish | None -> 0)
    | None -> false) c.found

(* what the area marks for [text]: the apply errors that still hold and the rested draft's parse *)
let errors_of state (c : checked) text =
  let held = real_errors (visible c text) in
  held @ (match state.live with
    | Some { seen; found = Some found; _ } when seen = text ->
        List.filter (fun d -> not (List.mem d held)) (real_errors found)
    | _ -> [])

(* the parse errors of a draft some tab holds, once it has rested *)
let live_errors state = match state.live with
  | Some { seen; found = Some found; _ }
    when state.draft = Some seen || (match state.binding_draft with Some (_, t) -> t = seen | None -> false)
         || List.exists (fun (_, s) -> s.gtext = seen) state.graphs -> real_errors found
  | _ -> []

let graph_errors state = List.concat_map (fun (_, s) -> real_errors (visible s.gerrors s.gtext)) state.graphs

(* the apply errors that still hold for the document draft and for the selection's draft *)
let held_errors state =
  real_errors (visible state.doc_errors (Option.value ~default:"" state.draft)),
  real_errors (visible state.binding_errors (match state.binding_draft with Some (_, t) -> t | None -> ""))

let first_error state =
  let doc, binding = held_errors state in
  match doc @ binding @ graph_errors state @ live_errors state with d :: _ -> Some d | [] -> None

(* one line for the crash report and tests: the tab, the draft, the first error, the graph drafts
   and the find bar *)
let summary state =
  let doc, binding = held_errors state in
  Printf.sprintf "%s tab, draft %s%s"
    (match state.tab with Selection -> "selection" | Graph -> "graph" | Document -> "document")
    (if (match state.tab with
         | Document -> state.draft <> None
         | Graph -> state.graphs <> []
         | Selection -> state.binding_draft <> None) then "yes" else "no")
    (match doc, state.draft, binding with
     | d :: _, Some draft, _ ->
         Printf.sprintf ", error%s: %s"
           (match line_of draft d with Some l -> Printf.sprintf " at line %d" l | None -> "")
           (Diagnostic.to_string d)
     | _, _, d :: _ ->
         Printf.sprintf ", error%s: %s"
           (match Option.bind state.binding_draft (fun (_, t) -> line_of t d) with
            | Some l -> Printf.sprintf " at line %d" l | None -> "")
           (Diagnostic.to_string d)
     | d :: _, None, _ -> ", error: " ^ Diagnostic.to_string d
     | [], _, [] ->
         (match graph_errors state @ live_errors state with
          | d :: _ -> ", error: " ^ Diagnostic.to_string d
          | [] -> ""))
  ^ (match List.sort compare (List.map fst state.graphs) with
     | [] -> "" | names -> ", graph drafts: " ^ String.concat " " names)
  ^ (match state.find with
     | Some f -> Printf.sprintf ", find %S at %d%s" f.query f.at (if f.replacing then " replacing" else "")
     | None -> "")

(* ---- the pane ---- *)

type intent =
  | Tab of tab
  | Doc_draft of string
  | Doc_apply of string
  | Doc_discard
  | Binding_draft of path * string
  | Binding_apply of path * string
  | Binding_discard
  | Graph_draft of string * string
  | Graph_apply of string * string
  | Graph_discard of string
  | Live of live
  | Menu of (float * float) option
  | Toggle_wrap
  | Doc_scrub of string * bool
  | Graph_scrub of string * string * bool
  | Binding_scrub of path * string * bool
  | Literal_scrub of { source : S.t list; op : Flow_graph.Flow_edit.op;
      position : Diagnostic.position; fallback : intent }
  | Toggle_parinfer
  | Picker of (int * int * bool) option
  | Open_graph of string
  | Select_binding of path
  | Jump_definition of path
  | Carry_over of int * bool
  | Find_set of find option * bool
  | Request of request option
  | References_of of string
  | Resolved

(* seconds a draft rests before it is parsed *)
let live_delay = 0.3

let dirty state (shown : shown) = match state.draft with
  | Some d -> d <> Lazy.force shown.applied | None -> false

(* The kit's colour control (swatch, hex, r g b) in a popup over the literal [text.[a..b)]:
   the new literal while it is edited. *)
let colour_popup ui ~at text (a, b) =
  let module Ui = Pxui.Ui in
  let module Color = Rays_math.Color in
  let theme = Ui.theme ui in
  let literal = String.sub text (a + 1) (b - a - 2) in
  let colour = Result.value (Rays_math.Color.hex literal) ~default:Rays_math.Color.white in
  let r, g, bl, _ = Rays_math.Color.to_floats colour in
  let alpha = if String.length literal = 9 then String.sub literal 7 2 else "" in
  let hex r g b = Rays_math.Color.to_hex_string (Color.of_unit_floats r g b) ^ alpha in
  let shown = hex r g bl in
  Ui.popup ui ~stroke:(Pxui.Theme.edge theme) ~at ~width:300. ~height:24. "text-colour" (fun () ->
    let box, cx, cy, cw = Ui.inspector_row ui ~width:300. ~key:"text-colour-row" ~label:"colour" () in
    Ui.within ui box (fun () ->
      (* the kit's swatch and hex field; the literal's alpha stays as written *)
      let hex_x = cx +. 28. in
      let typed = Pxui_shell.Kit.colour ui ~key:"text-colour" ~at:(cx, cy) ~w:92. ~swatch:colour ~hex:shown in
      let from_hex = if typed = shown then None else
          Option.map (fun c -> let r, g, b, _ = Rays_math.Color.to_floats c in hex r g b) (Result.to_option (Rays_math.Color.hex typed)) in
      let sliders_x = hex_x +. 72. in
      let field_w = (Float.max 60. (cw -. 100.) -. 16.) /. 3. in
      let from_slider = List.find_map Fun.id (List.mapi (fun index (axis, cur) ->
        let ax_x = sliders_x +. float index *. (field_w +. 8.) in
        Ui.draw ui box (fun paint (x, y, _, _) ->
          Ui.Paint.text paint ~at:(x +. ax_x +. 2., Pxui_shell.Kit.cap_y ui (y +. cy) 20.)
            ~size:(Pxui_shell.Kit.cap_size ui) ~color:(Pxui.Theme.ink_3 theme) axis);
        let display = Printf.sprintf "%.2f" cur in
        let changed, _ = Ui.value_field ui ~at:(ax_x, cy) ~w:field_w ~h:20.
            ~display ~fraction:cur ~slide:(fun f -> Printf.sprintf "%.2f" f) ~edit:false ~left:false
            ~valid:(fun t -> float_of_string_opt t <> None) ("text-colour-" ^ axis) display in
        if changed = display then None else
          Option.map (fun v ->
            let v = Float.max 0. (Float.min 1. v) in
            hex (if index = 0 then v else r) (if index = 1 then v else g) (if index = 2 then v else bl))
            (float_of_string_opt changed)) [ "r", r; "g", g; "b", bl ]) in
      match from_slider with Some _ -> from_slider | None -> from_hex))
  |> Option.map (fun edit -> String.sub text 0 (a + 1) ^ Option.value ~default:literal edit
                             ^ String.sub text (b - 1) (String.length text - b + 1))

(* the tabs' names: the workspace sheet's narrow Lisp panel (under 400 points) says "Doc"; a
   document with a draft wears a star *)
let tab_labels ~width ~dirty =
  let doc = if width < 400 then "Doc" else "Document" in
  [ Selection, "Selection"; Graph, "Graph"; Document, if dirty then doc ^ " *" else doc ]

(* the room the tabs take in the panel's header, 12 apart *)
let tabs_width ui ~width state shown =
  List.fold_left (fun w (_, label) -> w +. 12. +. Pxui.Ui.text_width ui label) (-12.)
    (tab_labels ~width ~dirty:(dirty state shown))

(* a find-bar button right-aligned in a row: it takes its width off [right] and leaves a gap *)
let find_button ui key right ?active name label at_y =
  let w = Pxui_shell.Kit.button_width ui label in
  right := !right -. w;
  let hit = Pxui_shell.Kit.button ui ~key:(key ^ name) ~at:(!right, at_y +. 4.) ~w ?active label in
  right := !right -. 4.; hit

(* the find bar between the text and the footer: one row, two with the replace row *)
let find_bar ~state ~emit ~theme ~find_row ~key ~width ~x ~current ~matches ~rule ~ay ui =
  let module Ui = Pxui.Ui in
  Option.iter (fun (f : find) ->
    let ry = ay +. 1. in
    rule "-find-rule" ay;
    let f' = ref { f with grab = false } and reveal = ref false and closed = ref false in
    let total = List.length matches in
    let index = match current with
      | Some m -> 1 + Option.value ~default:0 (List.find_index (( = ) m) matches) | None -> 0 in
    let count = if f.query = "" then "" else if total = 0 then "No matches"
      else Printf.sprintf "%d / %d" index total in
    let right = ref (x +. width -. 12.) in
    let button = find_button ui key right in
    if button "-find-close" "Close" ry then closed := true;
    if button ~active:f.replacing "-find-replace" "Replace" ry then f' := { !f' with replacing = not f.replacing };
    if button ~active:f.case "-find-case" "Aa" ry then (f' := { !f' with case = not f.case }; reveal := true);
    if button "-find-next" "Next" ry then (f' := find_step !f' matches `Next; reveal := true);
    if button "-find-prev" "Prev" ry then (f' := find_step !f' matches `Previous; reveal := true);
    let count_w = 76. in
    Ui.draw ui (Ui.box ui ~w:(Ui.Px count_w) ~h:(Ui.Px find_row) ~at:(!right -. count_w, ry) (key ^ "-find-count"))
      (fun paint (x, y, w, h) ->
        Ui.Paint.text paint ~at:(x +. w -. Ui.Paint.text_width paint count, Pxui_shell.Kit.text_y ui y h)
          ~color:(if total = 0 && f.query <> "" then Pxui.Theme.invalid else Pxui.Theme.ink_2 theme) count);
    let query, step = Ui.line_field ui ~at:(x +. 12., ry +. 4.) ~w:(Float.max 60. (!right -. count_w -. 8. -. x -. 12.))
        ~h:20. ~grab:f.grab ~placeholder:"Find" (key ^ "-find") f.query in
    if query <> f.query then (f' := { !f' with query }; reveal := true);
    (match step with
     | `Next -> f' := find_step !f' matches `Next; reveal := true
     | `Previous -> f' := find_step !f' matches `Previous; reveal := true
     | `Close -> closed := true
     | `None -> ());
    if f.replacing then begin
      let ry2 = ry +. find_row in
      let right2 = ref (x +. width -. 12.) in
      let button2 name label = find_button ui key right2 name label ry2 in
      let replace_with ~all = match find_replace !f' matches ~all with
        | Some (r, f2) -> emit (Request (Some r)); f' := f2; reveal := true
        | None -> () in
      if button2 "-replace-all" "All" then replace_with ~all:true;
      if button2 "-replace-one" "Replace" then replace_with ~all:false;
      let by, step = Ui.line_field ui ~at:(x +. 12., ry2 +. 4.) ~w:(Float.max 60. (!right2 -. 8. -. x -. 12.))
          ~h:20. ~placeholder:"Replace with" (key ^ "-with") f.replace in
      if by <> f.replace then f' := { !f' with replace = by };
      (match step with
       | `Next -> replace_with ~all:false
       | `Close -> closed := true
       | `Previous | `None -> ())
    end;
    if !closed then begin
      emit (Find_set (None, false));
      emit (Request (Some { select = (match current with Some m -> m | None -> (f.at, f.at)); splice = [] }))
    end else if !f' <> f then emit (Find_set (Some !f', !reveal))) state.find;
  ()

let view ui ~bounds:(x, y, width, height) ~tabs_right ~vocab ~names ~time state (shown : shown) =
  let module Ui = Pxui.Ui in
  let row = float (Ui.row_height ui) in
  let x = float x and y = float y and width = float width and height = float height in
  let theme = Ui.theme ui in
  let intents = ref [] in
  let emit i = intents := i :: !intents in
  (* the printed document is made here, once a frame, never inside a token scrub *)
  ignore (Lazy.force shown.applied);
  let dirty = dirty state shown in
  (* the tabs sit in the panel's header, at its right: the body is all text *)
  (* the workspace sheet's narrow Lisp panel (under 400 points): "Doc", the two buttons alone in
     the bar and no status row *)
  let narrow = width < 400. in
  let tabs = tab_labels ~width:(int_of_float width) ~dirty in
  let active = Option.value ~default:0 (List.find_index (fun (tab, _) -> tab = state.tab) tabs) in
  (* [tabs_right] is where the header has room for them ([Pxui_shell.Chrome.header_slots]); none
     in a header too narrow *)
  Option.iter (fun right ->
    match fst (Pxui_shell.Kit.segments ui ~key:"text-tab" ~right
                 ~y:(y -. float Pxui_shell.Layout.header_height +. 2.) (List.map snd tabs) active) with
    | Some index -> emit (Tab (fst (List.nth tabs index)))
    | None -> ()) tabs_right;
  let bar = 32. in
  let find_row = 28. in
  (* a hairline, the 32-point button bar, a hairline and the 24-point status row *)
  let footer = if narrow then bar +. 1. else bar +. row +. 2. in
  let body_y = y in
  let body_h = Float.max row height in
  let imports = match state.cache with
    | Some ((_, Some workspace, _, _, _), _) ->
        List.concat_map (fun (file, forms) -> List.filter_map (fun form ->
          match S.children form with _ :: {S.node = S.Sym name; _} :: _ -> Some (name, file)
          | _ -> None) forms) workspace.Editor_document.Workspace_doc.imports
    | _ -> [] in
  let language = Lisp_text.language ~vocab ~names ~imports ~parinfer:state.parinfer theme in
  (* the toolbar and the message row under an editable area; a right-click menu offers the same
     buttons and the wrap toggle *)
  let editor key ~at:(ey, eh) ~text ~errors ~spans ?reveal ?(caret_select = fun _ -> None)
      ~apply ~discard ~can_apply ~message ~draft ~scrub () =
    let chips = Lisp_text.color_chips text in
    (* the find bar sits between the text and the footer: one row, two with the replace row *)
    let find_h = match state.find with
      | Some f -> if f.replacing then 2. *. find_row else find_row | None -> 0. in
    let area_h = Float.max row (eh -. footer -. find_h) in
    let matches = match state.find with
      | Some f -> find_all ~case:f.case f.query text | None -> [] in
    let current = Option.bind state.find (fun f -> find_current f matches) in
    (* every match is a band, the current one also a bar under it; the view goes to the current one *)
    let spans = spans @ List.take 2000 matches in
    let shown_chips = chips @ (match current with
      | Some (a, b) -> [ a, b, theme.accent ] | None -> []) in
    let reveal = match current with Some (a, _) -> Some a | None -> reveal in
    (* a dragged number applies live ([scrub], merged into one history entry); typing is a draft *)
    let phase = ref None and caret = ref None and token = ref None and selection = ref None in
    (* Command-click and F12 follow a (ref name) or a definition *)
    let follow byte =
      let target = match Lisp_text.ref_at text byte with
        | Some name when List.mem name names.Lisp_text.graphs -> Some [name]
        | _ -> if text = shown.text then definition_at shown byte else None in
      Option.iter (fun path ->
        if path_mark shown.source shown.spans path = None then emit (Tab Document);
        emit (Jump_definition path)) target in
    let text', submitted = Ui.text_area_submit ui ~at:(x, ey) ~w:width ~h:area_h
        ~wrap:state.wrap ~errors:(List.filter_map (line_of text) errors)
        ~messages:(List.filter_map (fun (d : Diagnostic.t) ->
          Option.map (fun line ->
            line, Option.map (fun (s : Diagnostic.span) -> s.start, s.finish) d.span,
            d.code ^ "  " ^ String.map (function '\n' -> ' ' | c -> c) d.message) (line_of text d)) errors)
        ~spans ?reveal ~reveal_nonce:state.nonce ?select:(Option.map (fun r -> r.select) state.request)
        ?splice:(Option.map (fun r -> r.splice) state.request) ~language
        ~on_context:(fun at -> emit (Menu (Some at))) ~on_scrub:(fun p -> phase := Some p)
        ~on_scrub_edit:(fun range replacement -> token := Some (range, replacement))
        ~on_select:(fun range -> selection := Some range)
        ~chips:shown_chips
        ~on_drop:(fun byte drop -> emit (Carry_over (byte, (match drop with Ui.Dropped _ -> true | Hover _ -> false))))
        (* Command-click follows a (ref name); a click on a colour literal opens the colour control *)
        ~on_click:(fun byte command ->
          if command then follow byte
          else match List.find_opt (fun (a, b, _) -> a <= byte && byte < b) chips with
            | Some (a, b, _) -> emit (Picker (Some (a, b, false)))
            | None -> Option.iter (fun path -> emit (Select_binding path)) (caret_select byte))
        (* the caret in a binding selects its node *)
        ~on_caret:(fun byte -> caret := Some byte)
        ~on_caret_move:(fun byte -> match caret_select byte with
            | Some path when path <> shown.key -> emit (Select_binding path)
            | _ -> ())
        key text in
    (match !phase with
     | Some `Live ->
         let fallback = scrub text' false in
         let literal = Option.bind !token (fun ((start, finish) as range, replacement) ->
           if text <> shown.text || start < 0 || finish > String.length text
             || text' <> String.sub text 0 start ^ replacement
                 ^ String.sub text finish (String.length text - finish) then None
           else Option.map (fun op -> op, Diagnostic.position_of_offset text start) (scrub_op shown range replacement)) in
         emit (match literal with Some (op, position) -> Literal_scrub {source = shown.source; op; position; fallback}
           | None -> fallback)
     | Some `Done -> emit (scrub text' true)
     | None -> if text' <> text then emit (draft text'));
    (* the colour control: each edit applies live as one merged history entry, sealed on close *)
    let text' = match state.picker with
      | Some (a, b, changed) when b <= String.length text' && List.exists (fun (a', b', _) -> a = a' && b = b') chips ->
          (match colour_popup ui ~at:(Float.max x (x +. width -. 310.), ey +. row) text' (a, b) with
           | Some edited when edited <> text' ->
               emit (Picker (Some (a, b, true))); emit (scrub edited false); edited
           | Some _ -> text'
           | None ->
               emit (Picker None); if changed then emit (scrub text' true); text')
      | Some _ -> emit (Picker None); text'
      | None -> text' in
    (* a draft that rests is parsed once: the debounce is the time the text has been the same *)
    (if can_apply then match state.live with
       | Some l when l.seen = text ->
           if l.found = None && time -. l.since >= live_delay then
             emit (Live { l with found = Some (match S.parse text with Error d -> [ d ] | Ok _ -> []) })
       | _ -> emit (Live { seen = text; since = time; found = None }));
    let can_apply = can_apply || text' <> text in
    let ay = ey +. area_h in
    let ty = ay +. find_h in
    let rule name at = Ui.draw ui (Ui.box ui ~w:(Ui.Px width) ~h:(Ui.Px 1.) ~at:(x, at) (key ^ name))
      (fun paint (x, y, w, h) -> Ui.Paint.fill paint ~x ~y ~w ~h (Pxui.Theme.edge theme)) in
    rule "-rule" ty;
    (* what the keys asked this frame, now that the caret and the selection are known *)
    if state.request <> None then emit (Request None);
    List.iter (function
      | (Find | Find_replace | Find_selection) as cmd ->
          let base = Option.value state.find
              ~default:{ query = ""; replace = ""; case = false; replacing = false; at = 0; grab = true } in
          let picked = match !selection with
            | Some (a, b) when a < b && b <= String.length text'
                               && not (String.contains (String.sub text' a (b - a)) '\n') -> Some (a, b)
            | _ -> None in
          emit (Find_set (Some { base with
            query = (match picked with Some (a, b) -> String.sub text' a (b - a) | None -> base.query);
            replacing = base.replacing || cmd = Find_replace;
            at = (match picked, !caret with
                  | Some (a, b), _ -> if cmd = Find_selection then b else a
                  | None, Some c -> c | None, None -> base.at);
            grab = cmd <> Find_selection }, true))
      | (Find_next | Find_previous) as cmd ->
          Option.iter (fun f -> emit (Find_set (Some (find_step f matches
            (if cmd = Find_next then `Next else `Previous)), true))) state.find
      | Definition -> Option.iter follow !caret
      | References -> Option.iter (fun name -> emit (References_of name))
          (Option.bind !caret (symbol_at text'))
      | Select _ -> ()) state.pending;
    if state.pending <> [] then emit Resolved;
    find_bar ~ay ~rule ~matches ~current ~x ~width ~key ~find_row ~theme ~emit ~state ui;
    (* Command-Enter in the area is the button: the one outlined button of the panel *)
    let hint = "\xe2\x8c\x98\xe2\x86\xb5" in
    (* the caret's line and column, 1-based, in the status row's label *)
    let position = Option.map (fun byte ->
      let p = Diagnostic.position_of_offset text' (min byte (String.length text')) in
      Printf.sprintf "%d:%d" p.line p.col) !caret in
    let apply_w = Pxui_shell.Kit.button_width ui ~hint "Check and apply" in
    let by = ty +. 1. +. ((bar -. 20.) /. 2.) in
    if (Pxui_shell.Kit.button ui ~key:(key ^ "-apply") ~at:(x +. 12., by) ~w:apply_w ~primary:true
          ~enabled:can_apply ~hint "Check and apply")
       || (submitted && can_apply)
    then emit (apply text');
    let discard_hint = if narrow then None else Some "esc" in
    if Pxui_shell.Kit.button ui ~key:(key ^ "-discard") ~at:(x +. 12. +. apply_w +. 8., by)
         ~w:(Pxui_shell.Kit.button_width ui ?hint:discard_hint "Discard") ~enabled:can_apply ?hint:discard_hint "Discard"
    then emit discard;
    (* parinfer and its switch, at the bar's end *)
    let switch_x = x +. width -. 12. -. 28. in
    if not narrow then begin
      if Pxui_shell.Kit.switch ui ~key:(key ^ "-parinfer") ~at:(switch_x, ty +. 1. +. ((bar -. 14.) /. 2.)) state.parinfer
      then emit Toggle_parinfer;
      Ui.draw ui (Ui.box ui ~w:(Ui.Px 80.) ~h:(Ui.Px bar) ~at:(switch_x -. 8. -. 80., ty +. 1.) (key ^ "-parinfer-label"))
        (fun paint (x, y, w, h) ->
          let label = "parinfer" in
          Ui.Paint.text paint ~at:(x +. w -. Ui.Paint.text_width paint label, Pxui_shell.Kit.text_y ui y h)
            ~color:(Pxui.Theme.ink_2 theme) label);
      rule "-rule2" (ty +. 1. +. bar)
    end;
    let wrong = errors <> [] in
    let msg = Ui.box ui ~flags:Ui.clip ~w:(Ui.Px width) ~h:(Ui.Px (if narrow then 0. else row)) ~at:(x, ty +. bar +. 2.) (key ^ "-message") in
    if not narrow then Ui.draw ui msg (fun paint (x, y, w, h) ->
      (* the state: a dot in the error colour or the accent for a draft, then what it says; the
         first error is "n errors" in the error colour and its line and message in ink-2 *)
      let tx = if wrong || can_apply then begin
          Ui.Paint.circle paint ~at:(x +. 15., y +. (h /. 2.)) ~radius:3.
            ~fill:(if wrong then Pxui.Theme.invalid else theme.accent) (); x +. 26.
        end else x +. 12. in
      (* the label at the row's end: where the caret is, and whether the text is a draft *)
      let state_label = String.concat " \xc2\xb7 " (Option.to_list position @ (if can_apply then [ "modified" ] else [])) in
      let tail = if state_label <> "" then Pxui_shell.Kit.cap_width ui state_label +. 20. else 12. in
      let ty = Pxui_shell.Kit.text_y ui y h in
      (match errors with
       | (first : Diagnostic.t) :: _ ->
           let count = Printf.sprintf "%d error%s" (List.length errors) (if List.length errors = 1 then "" else "s") in
           Ui.Paint.text paint ~at:(tx, ty) ~color:(Pxui.Theme.invalid) count;
           let dx = tx +. Ui.Paint.text_width paint count +. 8. in
           let detail = (match line_of text first with Some l -> Printf.sprintf "line %d \xc2\xb7 " l | None -> "")
             ^ String.map (function '\n' -> ' ' | c -> c) (Diagnostic.to_string first) in
           Ui.Paint.text paint ~at:(dx, ty) ~color:(Pxui.Theme.ink_2 theme)
             (Ui.ellipsis ~width:(Ui.Paint.text_width paint) ~limit:(x +. w -. tail -. dx) detail)
       | [] ->
           Ui.Paint.text paint ~at:(tx, ty) ~color:(Pxui.Theme.ink_2 theme)
             (Ui.ellipsis ~width:(Ui.Paint.text_width paint) ~limit:(x +. w -. tail -. tx) message));
      if state_label <> "" then
        Ui.Paint.cap paint ~at:(x +. w -. 12. -. Pxui_shell.Kit.cap_width ui state_label, Pxui_shell.Kit.cap_y ui y h)
          state_label);
    (match state.menu with
     | None -> ()
     | Some at ->
         (match Ui.context_menu ui ~at (key ^ "-menu")
                  [ "Check and apply", can_apply; "Discard", can_apply; "", false;
                    (if state.wrap then "Unwrap long lines" else "Wrap long lines"), true;
                    (if state.parinfer then "Parinfer off (keep brackets as typed)"
                     else "Parinfer on (brackets follow indentation)"), true ] with
          | `Open -> ()
          | `Dismiss -> emit (Menu None)
          | `Pick 0 -> emit (Menu None); emit (apply text')
          | `Pick 1 -> emit (Menu None); emit discard
          | `Pick 3 -> emit (Menu None); emit Toggle_wrap
          | `Pick _ -> emit (Menu None); emit Toggle_parinfer)) in
  let message errors ~dirty ~clean = match real_errors errors with
    | d :: _ -> String.map (function '\n' -> ' ' | c -> c) (Diagnostic.to_string d)
    | [] -> if dirty then "Unapplied draft. Every other pane shows the last applied document."
        else clean in
  let caret_select text =
    if text = shown.text then binding_at shown else
    let mapped = lazy (match S.parse text with
    | Error _ -> None
    | Ok forms ->
        let rec spans (form : S.t) = (form.id, form.span) :: List.concat_map spans (S.children form) in
        let spans = List.concat_map spans forms in
        let mapped = match state.tab, forms with
          | Document, _ -> {shown with source = forms; spans; body = None}
          | Graph, [form] -> {shown with spans; body = Option.map (fun body -> body, spans) (last form)}
          | Selection, [form] -> {shown with spans; body = Some
              ((if List.mem (S.head form) [Some "graph"; Some "defn"] then Option.value ~default:form (last form) else form), spans)}
          | _ -> shown in
        Some mapped) in
    fun byte -> Option.bind (Lazy.force mapped) (fun mapped -> binding_at mapped byte) in
  (match state.tab with
   | Document ->
       let text = current_text state shown in
       let errors = errors_of state state.doc_errors text in
       editor "text-document" ~at:(body_y, body_h) ~text ~errors
         ~spans:(if dirty then [] else Option.to_list shown.mark) ?reveal:(if dirty then None else Option.map fst shown.mark)
         ~caret_select:(caret_select text) ~apply:(fun t -> Doc_apply t) ~discard:Doc_discard ~can_apply:dirty
         ~message:(message errors ~dirty ~clean:"Source matches the applied document.")
         ~draft:(fun t -> Doc_draft t) ~scrub:(fun t done_ -> Doc_scrub (t, done_)) ()
   | Graph ->
       let slot = graph_slot state shown.graph in
       let text = current_text state shown in
       let errors = errors_of state (match slot with Some s -> s.gerrors | None -> no_errors) text in
       let dirty = text <> shown.text in
       let spans = if dirty then [] else Option.to_list shown.mark in
       editor "text-graph" ~at:(body_y, body_h) ~text ~errors
         ~spans ?reveal:(if dirty then None else Option.map fst shown.mark)
         ~caret_select:(caret_select text) ~apply:(fun t -> Graph_apply (shown.graph, t)) ~discard:(Graph_discard shown.graph) ~can_apply:dirty
         ~message:(message errors ~dirty
           ~clean:(Printf.sprintf "Edit %s as text; Check and apply checks the whole workspace." shown.graph))
         ~draft:(fun t -> Graph_draft (shown.graph, t))
         ~scrub:(fun t done_ -> Graph_scrub (shown.graph, t, done_)) ()
   | Selection ->
       (* the closure of the selection, editable: its bindings are the graph's root bindings *)
       let text = current_text state shown in
       let dirty = text <> shown.text in
       let spans = if dirty then [] else Option.to_list shown.mark in
       let errors = errors_of state state.binding_errors text in
       editor "text-selection" ~at:(body_y, body_h) ~text ~errors
         ~spans ?reveal:(Option.map fst shown.mark)
         ~caret_select:(caret_select text)
         ~apply:(fun t -> Binding_apply (shown.key, t)) ~discard:Binding_discard ~can_apply:dirty
         ~message:(message errors ~dirty
           ~clean:"The selection with what it reads; Check and apply writes the bindings shown.")
         ~draft:(fun t -> Binding_draft (shown.key, t))
         ~scrub:(fun t done_ -> Binding_scrub (shown.key, t, done_)) ());
  List.rev !intents
