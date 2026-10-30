(* Same-type catalog literals keep the checked graph's topology and liveness.
   Patch their authored form and typed term; callers may reuse the static plan. *)
module S = Flow.Syntax
module W = Flow.Workspace
module E = Flow.Eval
module L = Flow_sop.Lower
module Edit = Sop.Edit_graph

type change = {
  path : W.path;
  field : string;
  kind : string;
  authored : int;
  in_fn : bool;  (** inside a [fn] body: closures keep the old literal *)
  captured : bool;  (** a [fn] body reads a binding on its path: a closure may hold the value *)
  before : E.value;
  after : E.value;
  expr : S.t;
}

let rec literal (form : S.t) =
  if form.meta <> [] then None
  else
    match form.node with
    | Num text when S.number text -> (
        match (int_of_string_opt text, float_of_string_opt text) with
        | Some n, _ -> Some (Flow.Ty.Int, E.Int n, W.Lit (Param.Int_value n))
        | None, Some n when Float.is_finite n ->
            Some (Flow.Ty.Float, E.Float n, W.Lit (Param.Float_value n))
        | _ -> None)
    | Str text -> Some (Flow.Ty.Text, E.Text text, W.Text text)
    | Sym (("true" | "false") as text) ->
        let value = text = "true" in
        Some (Flow.Ty.Bool, E.Bool value, W.Lit (Param.Bool_value value))
    | Vec [ a; b; c ] -> (
        let number form =
          Option.bind (literal form) (function
            | _, E.Int n, _ -> Some (float n)
            | _, E.Float n, _ -> Some n
            | _ -> None)
        in
        match (number a, number b, number c) with
        | Some x, Some y, Some z ->
            let term form =
              let ty, _, node = Option.get (literal form) in
              W.{ path = None; ty; node; form }
            in
            Some (Flow.Ty.Vec3, E.Vec3 (x, y, z), W.Vec (List.map term [ a; b; c ]))
        | _ -> None)
    | _ -> None

(* [f] over every term, bottom up; a term whose children and [f] leave it as it is stays physically,
   so a patched workspace shares what the edit did not touch (the library's terms, a graph before
   the literal) *)
let rec map_term f (term : W.term) =
  let changed = ref false in
  let go t =
    let t' = map_term f t in
    if t' != t then changed := true;
    t'
  in
  let named = List.map (fun (name, t) -> (name, go t)) in
  let node : W.node =
    match term.node with
    | Call call -> Call { call with args = named call.args }
    | Op call -> Op { call with args = named call.args }
    | Call_fn call ->
        Call_fn { call with args = List.map go call.args; body = Option.map go call.body }
    | Graph_ref call -> Graph_ref { call with inputs = named call.inputs }
    | Let (bindings, body) -> Let (List.map (fun (p, t) -> (p, go t)) bindings, go body)
    | State state -> State { state with init = go state.init; step = go state.step }
    | Loop loop ->
        Loop
          {
            loop with
            accs = List.map (fun (p, t) -> (p, go t)) loop.accs;
            clauses = List.map (fun (p, t) -> (p, go t)) loop.clauses;
            body = go loop.body;
          }
    | With_error (a, b) -> With_error (go a, go b)
    | Error_rows scope -> Error_rows { scope with rows = go scope.rows; mapper = go scope.mapper }
    | If (a, b, c) -> If (go a, go b, go c)
    | Cond (arms, last) -> Cond (List.map (fun (a, b) -> (go a, go b)) arms, go last)
    | Case (value, arms, last) -> Case (go value, List.map (fun (a, b) -> (a, go b)) arms, go last)
    | Fn fn -> Fn { fn with body = go fn.body; capture = Option.map go fn.capture }
    | Hof (kind, args) -> Hof (kind, List.map go args)
    | Vec args -> Vec (List.map go args)
    | List_lit args -> List_lit (List.map go args)
    | Record args -> Record (named args)
    | Assoc (value, args) -> Assoc (go value, named args)
    | Get (value, field) -> Get (go value, field)
    | Str args -> Str (List.map go args)
    | List_op (kind, args) -> List_op (kind, List.map go args)
    | Bypass value -> Bypass (go value)
    | Expanded macro -> Expanded { macro with body = go macro.body }
    | (Lit _ | Text _ | Nil | Time | Ref_binding _ | Fn_ref _) as node -> node
  in
  f (if !changed then { term with node } else term)

let rec child form = function
  | [] -> Some form
  | index :: rest ->
      Option.bind (List.nth_opt (S.children form) index) (fun form -> child form rest)

let term_children (node : W.node) =
  match node with
  | Call call -> List.map snd call.args
  | Op call -> List.map snd call.args
  | Call_fn call -> call.args
  | Graph_ref call -> List.map snd call.inputs
  | Let (bindings, body) -> body :: List.map snd bindings
  | State state -> [ state.init; state.step ]
  | Loop loop -> (loop.body :: List.map snd loop.accs) @ List.map snd loop.clauses
  | With_error (a, b) -> [ a; b ]
  | Error_rows { rows = a; mapper = b; _ } -> [ a; b ]
  | If (a, b, c) -> [ a; b; c ]
  | Cond (arms, last) -> last :: List.concat_map (fun (a, b) -> [ a; b ]) arms
  | Case (value, arms, last) -> value :: last :: List.map snd arms
  | Fn fn -> [ fn.body ]
  | Hof (_, args) | Vec args | List_lit args | Str args | List_op (_, args) -> args
  | Record args -> List.map snd args
  | Assoc (value, args) -> value :: List.map snd args
  | Get (value, _) | Bypass value -> [ value ]
  | Expanded macro -> [ macro.body ]
  | Lit _ | Text _ | Nil | Time | Ref_binding _ | Fn_ref _ -> []

let rec find_argument ?(in_fn = false) field id (term : W.term) =
  match term.node with
  | Call { kind; ctx; args }
    when Option.fold ~none:false
           ~some:(fun (arg : W.term) -> arg.form.id = id)
           (List.assoc_opt field args) ->
      Some (kind, ctx, term.form.id, List.assoc field args, in_fn)
  | node ->
      let in_fn = in_fn || match node with Fn _ -> true | _ -> false in
      List.find_map (find_argument ~in_fn field id) (term_children node)

(* names a [fn] body reads: a closure may hold the value bound to one of them *)
let fn_names graphs =
  let names = Hashtbl.create 16 in
  let rec go inside (term : W.term) =
    (match term.node with
    | Ref_binding (name, _) when inside -> Hashtbl.replace names name ()
    | _ -> ());
    let inside = inside || match term.node with Fn _ -> true | _ -> false in
    List.iter (go inside) (term_children term.node)
  in
  List.iter (fun (g : W.graph) -> go false g.body) graphs;
  names

let rec plain (form : S.t) =
  form.notes = [] && form.tail = [] && List.for_all plain (S.children form)

let patch_source source (before : S.t) (after : S.t) =
  let delta = String.length (Flow.Lisp.flat after) - (before.span.finish - before.span.start) in
  let forms = Hashtbl.create 256 in
  let shift (span : Diagnostic.span) =
    if span.start >= before.span.finish then
      { Diagnostic.start = span.start + delta; finish = span.finish + delta }
    else if span.finish >= before.span.finish then { span with finish = span.finish + delta }
    else span
  in
  let rec replacement ~keep_notes start (old : S.t) (value : S.t) : S.t =
    let finish = start + String.length (Flow.Lisp.flat value) in
    let node =
      match (old.node, value.node) with
      | Vec old, Vec values ->
          let at = ref (start + 1) in
          S.Vec
            (List.map2
               (fun old value ->
                 let form = replacement ~keep_notes:false !at old value in
                 at := form.span.finish + 1;
                 form)
               old values)
      | _, node -> node
    in
    let value =
      {
        value with
        id = old.id;
        node;
        notes = (if value.notes = [] && keep_notes then old.notes else value.notes);
        span = { start; finish };
      }
    in
    Hashtbl.replace forms value.id value;
    value
  in
  let rec go (form : S.t) =
    let form =
      if form.id = before.id then replacement ~keep_notes:true before.span.start form after
      else
        let node =
          match form.node with
          | List items -> S.List (List.map go items)
          | Vec items -> Vec (List.map go items)
          | Map items -> Map (List.map go items)
          | Quote (kind, item) -> Quote (kind, go item)
          | node -> node
        in
        { form with node; span = shift form.span }
    in
    Hashtbl.replace forms form.id form;
    form
  in
  (List.map go source, forms)

let patch catalog (workspace : W.t) op =
  match op with
  | Flow_graph.Flow_edit.Set_arg { node = path; key = Kw field; sub; value } ->
      Option.bind (Flow_graph.Flow_edit.arg_text workspace.source path (Kw field)) (fun expr ->
          Option.bind (child expr sub) (fun before ->
              if
                before.id = 0
                || literal expr = None
                || (sub <> [] && before.notes <> [])
                || not (plain value)
              then None
              else
                match (literal before, literal value) with
                | Some (ty, _, _), Some (next_ty, _, _)
                  when ty = next_ty
                       || sub <> []
                          && List.mem ty [ Flow.Ty.Int; Float ]
                          && List.mem next_ty [ Flow.Ty.Int; Float ] ->
                    let call =
                      List.find_map
                        (fun (graph : W.graph) -> find_argument field expr.id graph.body)
                        (workspace.graphs @ workspace.defs)
                    in
                    Option.bind call (fun (kind, ctx, authored, argument, in_fn) ->
                        let head =
                          Option.bind
                            (Flow_graph.Flow_edit.arg_text workspace.source path Whole)
                            S.head
                        in
                        match Option.map (Flow.Catalog.resolve_kind catalog ctx) head with
                        | Some (Ok descriptor) when descriptor.qualified = kind ->
                            Option.bind
                              (List.find_opt
                                 (fun (p : Flow.Catalog.parameter) -> p.name = field)
                                 descriptor.parameters)
                              (fun parameter ->
                                let source, forms = patch_source workspace.source before value in
                                let remap_form (form : S.t) =
                                  if form.span.finish > form.span.start then
                                    Option.value ~default:form (Hashtbl.find_opt forms form.id)
                                  else form
                                in
                                let update (term : W.term) =
                                  let form = remap_form term.form in
                                  if term.form.id = before.id then
                                    let ty, _, node = Option.get (literal form) in
                                    { term with ty; node; form }
                                  else if form == term.form then term
                                  else { term with form }
                                in
                                let argument = map_term update argument in
                                let error = ref None in
                                let report severity code message =
                                  if severity = Diagnostic.Error_severity && !error = None then
                                    error :=
                                      Some (Diagnostic.error ~span:argument.form.span ~code message)
                                in
                                W.validate_parameter report parameter argument;
                                Some
                                  (match !error with
                                  | Some error -> Error error
                                  | None ->
                                      let graph (graph : W.graph) =
                                        {
                                          graph with
                                          body = map_term update graph.body;
                                          inputs =
                                            List.map
                                              (fun (name, ty, value) ->
                                                (name, ty, Option.map (map_term update) value))
                                              graph.inputs;
                                          form =
                                            Option.value ~default:graph.form
                                              (Hashtbl.find_opt forms graph.form.id);
                                        }
                                      in
                                      let _, old_value, _ = Option.get (literal expr)
                                      and _, new_value, _ = Option.get (literal argument.form) in
                                      let workspace =
                                        {
                                          workspace with
                                          source;
                                          graphs = List.map graph workspace.graphs;
                                          defs = List.map graph workspace.defs;
                                          packed_roots =
                                            List.map
                                              (fun ((form : S.t), path) -> (remap_form form, path))
                                              workspace.packed_roots;
                                          macros =
                                            List.map
                                              (fun (form : S.t) ->
                                                Option.value ~default:form
                                                  (Hashtbl.find_opt forms form.id))
                                              workspace.macros;
                                        }
                                      in
                                      Ok
                                        ( workspace,
                                          {
                                            path;
                                            field;
                                            kind;
                                            authored;
                                            in_fn;
                                            captured =
                                              (let names =
                                                 fn_names (workspace.graphs @ workspace.defs)
                                               in
                                               List.exists (Hashtbl.mem names) path);
                                            before = old_value;
                                            after = new_value;
                                            expr = argument.form;
                                          } )))
                        | _ -> None)
                | _ -> None))
  | _ -> None

let lower_nodes (lowered : L.t) change =
  let targets =
    Array.to_list lowered.plan.nodes
    |> List.filter (fun (node : E.node) ->
        lowered.evaluated.authored.(node.id) = change.authored && node.kind = change.kind)
  in
  if
    change.authored = 0 || change.in_fn || lowered.states <> [] || targets = []
    || List.exists
         (fun (node : E.node) ->
           List.assoc_opt change.field node.args <> Some change.before
           || not (Flow_sop.Network.Int_map.mem node.id lowered.compiled))
         targets
  then None
  else
    let values =
      List.map
        (fun (node : E.node) -> (node.id, Flow_sop.Network.Int_map.find node.id lowered.compiled))
        targets
    in
    let patch network =
      List.fold_left
        (fun result (_, id) ->
          Result.bind result (fun (network : Flow_sop.Network.t) ->
              if Edit.find network.geometry ~node_id:id = None then Ok network
              else
                Result.bind
                  (Flow_sop.Network.parameter network
                     Flow_sop.Port.{ node = id; path = change.field })
                  (fun parameter ->
                    Result.bind (L.changes parameter change.after) (fun changes ->
                        (* a cook-level parameter rule refuses here as the full lowering does *)
                        Result.map_error
                          (fun (d : Diagnostic.t) -> { d with code = "E_LOWER" })
                          (Flow_sop.Network.apply_parameters ~node_id:id changes network)))))
        (Ok network) values
    in
    let result =
      List.fold_right
        (fun (graph : L.graph) result ->
          Result.bind result (fun graphs ->
              Result.map (fun network -> { graph with network } :: graphs) (patch graph.network)))
        lowered.graphs (Ok [])
    in
    Some
      (Result.map
         (fun graphs ->
           let nodes =
             Array.map
               (fun (node : E.node) ->
                 if List.mem_assoc node.id values then
                   {
                     node with
                     args =
                       List.map
                         (fun (field, value) ->
                           (field, if field = change.field then change.after else value))
                         node.args;
                   }
                 else node)
               lowered.plan.nodes
           in
           let plan = { lowered.plan with nodes } in
           { lowered with graphs; plan; evaluated = { lowered.evaluated with plan } })
         result)

(* A scene, world or settings object is a [Source]/[Struct] value, not a plan node: swap its field
   in the values the evaluation recorded at the path, keeping every other value physically. *)
let lower_value (lowered : L.t) change =
  if change.in_fn || change.captured || lowered.states <> [] then None
  else
    let targets = Option.value ~default:[] (List.assoc_opt change.path lowered.evaluated.records) in
    let hit = ref false and swapped = ref [] in
    let is_target (v : E.value) =
      List.exists (fun (_, t) -> t == v) targets
      &&
      match v with
      | Source (kind, _, fields) | Struct (kind, _, fields) -> (
          kind = change.kind
          &&
          match List.assoc_opt change.field fields with
          | Some old -> ( try old = change.before with Invalid_argument _ -> false)
          | None -> false)
      | _ -> false
    in
    let same_list a b = List.for_all2 (fun (_, x) (_, y) -> x == y) a b in
    let rec rw (v : E.value) : E.value =
      match List.assq_opt v !swapped with
      | Some next -> next
      | None ->
          let next =
            if is_target v then (
              hit := true;
              match v with
              | Source (k, ty, fs) -> E.Source (k, ty, set fs)
              | Struct (k, ty, fs) -> E.Struct (k, ty, set fs)
              | v -> v)
            else
              match v with
              | List a ->
                  let b = Array.map rw a in
                  if Array.for_all2 ( == ) a b then v else E.List b
              | Record fs -> fields v fs (fun fs -> E.Record fs)
              | Struct (k, ty, fs) -> fields v fs (fun fs -> E.Struct (k, ty, fs))
              | Source (k, ty, fs) -> fields v fs (fun fs -> E.Source (k, ty, fs))
              | v -> v
          in
          (* Keep changed ancestors shared too: a later edit targets their recorded identity. *)
          if next != v then swapped := (v, next) :: !swapped;
          next
    and set fs = List.map (fun (f, x) -> if f = change.field then (f, change.after) else (f, x)) fs
    and fields v fs build =
      let fs' = List.map (fun (f, x) -> (f, rw x)) fs in
      if same_list fs fs' then v else build fs'
    in
    let named l = List.map (fun (n, v) -> (n, rw v)) l in
    let results = named lowered.evaluated.results in
    let records =
      List.map
        (fun (path, vs) -> (path, List.map (fun (i, v) -> (i, rw v)) vs))
        lowered.evaluated.records
    in
    let instances =
      Array.map
        (fun (i : E.instance) -> { i with inputs = named i.inputs; result = rw i.result })
        lowered.plan.instances
    in
    if not !hit then None
    else
      let plan = { lowered.plan with instances } in
      Some (Ok { lowered with plan; evaluated = { lowered.evaluated with plan; results; records } })

let lower lowered change =
  match lower_nodes lowered change with Some r -> Some r | None -> lower_value lowered change
