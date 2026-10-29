module W = Flow.Workspace

(* The form id spreads terms; hashing the term itself collides on the many alike subtrees and degrades into long bucket scans. *)
module Memo = Hashtbl.Make (struct
  type t = W.term

  let equal = ( == )
  let hash (t : W.term) = t.form.Flow.Syntax.id land max_int
end)

(* Checked terms share subterms; rewrite each once (keeping the sharing), or a shared chain is
   walked exponentially often. [transform] is pure per term, so the memo changes nothing else. *)
let map ?(select = Fun.id) ?(helpers = true) transform (graph : W.graph) =
  let memo = Memo.create 256 in
  let rec term (t : W.term) =
    match t.node with
    | Lit _ | Text _ | Nil | Time | Ref_binding _ | Fn_ref _ ->
        term_once t (* cheaper to rebuild than to look up *)
    | _ -> (
        match Memo.find_opt memo t with
        | Some mapped -> mapped
        | None ->
            let mapped = term_once t in
            Memo.add memo t mapped;
            mapped)
  and term_once (t : W.term) =
    let t = select t in
    let fields xs = List.map (fun (name, value) -> (name, term value)) xs in
    let patterns xs = List.map (fun (pattern, value) -> (pattern, term value)) xs in
    let node =
      match t.node with
      | W.Call c -> W.Call { c with args = fields c.args }
      | Op c -> Op { c with args = fields c.args }
      | Call_fn c ->
          Call_fn
            {
              c with
              args = List.map term c.args;
              body = (if helpers then Option.map term c.body else c.body);
            }
      | Graph_ref g -> Graph_ref { g with inputs = fields g.inputs }
      | Let (bindings, body) -> Let (patterns bindings, term body)
      | State s -> State { s with init = term s.init; step = term s.step }
      | Loop l ->
          Loop { l with accs = patterns l.accs; clauses = patterns l.clauses; body = term l.body }
      | With_error (a, b) -> With_error (term a, term b)
      | Error_rows scope ->
          Error_rows { scope with rows = term scope.rows; mapper = term scope.mapper }
      | If (a, b, c) -> If (term a, term b, term c)
      | Cond (arms, default) -> Cond (List.map (fun (a, b) -> (term a, term b)) arms, term default)
      | Case (value, arms, default) -> Case (term value, patterns arms, term default)
      | Fn f -> Fn { f with body = term f.body; capture = Option.map term f.capture }
      | Hof (kind, args) -> Hof (kind, List.map term args)
      | Vec args -> Vec (List.map term args)
      | List_lit args -> List_lit (List.map term args)
      | Record args -> Record (fields args)
      | Get (value, key) -> Get (term value, key)
      | Assoc (value, args) -> Assoc (term value, fields args)
      | Str args -> Str (List.map term args)
      | List_op (kind, args) -> List_op (kind, List.map term args)
      | Bypass value -> Bypass (term value)
      | Expanded e -> Expanded { e with body = term e.body }
      | Lit _ | Text _ | Nil | Time | Ref_binding _ | Fn_ref _ -> t.node
    in
    transform { t with node }
  in
  {
    graph with
    W.body = term graph.W.body;
    inputs = List.map (fun (n, ty, df) -> (n, ty, Option.map term df)) graph.inputs;
  }

let context_ops = Flow.Workspace.body_context_ops

let context_ops_reached ?(inputs = []) ?(defs = []) (graph : W.graph) =
  let defaults =
    List.filter_map
      (fun (name, _, df) ->
        Option.bind df (fun (t : W.term) ->
            match t.node with
            | W.Lit (Param.Bool_value b) -> Some (name, Flow.Eval.Bool b)
            | Lit (Int_value n) -> Some (name, Flow.Eval.Int n)
            | Lit (Float_value f) -> Some (name, Flow.Eval.Float f)
            | Text text -> Some (name, Flow.Eval.Text text)
            | _ -> None))
      graph.inputs
  in
  let found = ref [] and fuel = ref 4096 in
  let rec names = function
    | W.Name name -> [ name ]
    | Seq patterns -> List.concat_map names patterns
    | Keys names -> names
  in
  let remove patterns bindings =
    let bound = List.concat_map names patterns in
    List.filter (fun (name, _) -> not (List.mem name bound)) bindings
  in
  let rec known bindings (t : W.term) =
    match t.node with
    | W.Lit (Param.Bool_value b) -> Some (Flow.Eval.Bool b)
    | Lit (Int_value n) -> Some (Flow.Eval.Int n)
    | Lit (Float_value f) -> Some (Flow.Eval.Float f)
    | Text text -> Some (Flow.Eval.Text text)
    | Ref_binding (name, []) -> List.assoc_opt name bindings
    | Call_fn call when !fuel > 0 ->
        (* a helper over known arguments (a text predicate, say) decides its own branches *)
        decr fuel;
        Option.bind
          (List.find_opt (fun (d : W.graph) -> d.name = call.fn) defs)
          (fun helper ->
            let local =
              List.mapi
                (fun i (name, _, default) ->
                  let value =
                    match List.nth_opt call.args i with
                    | Some argument -> known bindings argument
                    | None -> Option.bind default (known [])
                  in
                  Option.map (fun value -> (name, value)) value)
                helper.inputs
            in
            if List.mem None local then None
            else known (List.filter_map Fun.id local) (Option.value ~default:helper.body call.body))
    | Op { op; args; _ } when not (List.mem op context_ops) ->
        let rec values = function
          | [] -> Some []
          | (key, value) :: rest ->
              Option.bind (known bindings value) (fun value ->
                  Option.map (fun rest -> (key, value) :: rest) (values rest))
        in
        Option.bind (values args) (fun args ->
            Option.bind (Flow.Op.find ~extra:Operators.all op Flow.Context.value) (fun op ->
                try
                  op.check args;
                  Some (op.body ~live:(Input.at_time 0.) ~node:(fun _ _ -> raise Exit) args)
                with Diagnostic.Error _ | Exit -> None))
    | _ -> None
  and scan bindings (body : W.term) =
    let select (t : W.term) =
      match t.node with
      | W.If (test, yes, no) -> (
          match known bindings test with
          | Some (Flow.Eval.Bool true) -> yes
          | Some (Bool false) -> no
          | _ -> t)
      | Cond (arms, default) -> (
          (* An arm whose test is known false never runs; the first known-true arm is the result. *)
          let rec live = function
            | [] -> ([], None)
            | ((test : W.term), yes) :: rest -> (
                match known bindings test with
                | Some (Flow.Eval.Bool false) -> live rest
                | Some (Bool true) -> ([], Some yes)
                | _ ->
                    let arms, taken = live rest in
                    ((test, yes) :: arms, taken))
          in
          match live arms with
          | [], Some yes -> yes
          | [], None -> default
          | arms, Some yes -> { t with node = Cond (arms, yes) }
          | arms, None -> { t with node = Cond (arms, default) })
      | Call_fn call ->
          (match List.find_opt (fun (d : W.graph) -> d.name = call.fn) defs with
          | Some helper ->
              let local =
                List.mapi
                  (fun i (name, _, default) ->
                    let value =
                      match List.nth_opt call.args i with
                      | Some argument -> known bindings argument
                      | None -> Option.bind default (known [])
                    in
                    Option.map (fun value -> (name, value)) value)
                  helper.inputs
                |> List.filter_map Fun.id
              in
              scan local (Option.value ~default:helper.body call.body)
          | None -> Option.iter (scan []) call.body);
          { t with node = Call_fn { call with body = None } }
      | Fn fn ->
          scan (remove (List.map fst fn.params) bindings) fn.body;
          { t with node = Fn { fn with body = { fn.body with node = Nil } } }
      | Fn_ref name ->
          Option.iter
            (fun (helper : W.graph) -> scan [] helper.body)
            (List.find_opt (fun (d : W.graph) -> d.name = name) defs);
          t
      | Let (locals, body) ->
          let bindings =
            List.fold_left
              (fun bindings (pattern, value) ->
                scan bindings value;
                let local =
                  match (pattern, known bindings value) with
                  | W.Name name, Some value -> [ (name, value) ]
                  | _ -> []
                in
                local @ remove [ pattern ] bindings)
              bindings locals
          in
          scan bindings body;
          { t with node = Nil }
      | State state ->
          scan bindings state.init;
          scan (remove [ state.binder ] bindings) state.step;
          { t with node = Nil }
      | Loop loop ->
          List.iter (fun (_, value) -> scan bindings value) loop.accs;
          let bindings = remove (List.map fst loop.accs) bindings in
          let bindings =
            List.fold_left
              (fun bindings (pattern, value) ->
                scan bindings value;
                remove [ pattern ] bindings)
              bindings loop.clauses
          in
          scan bindings loop.body;
          { t with node = Nil }
      | _ -> t
    in
    ignore
      (map ~select
         (fun t ->
           (match t.W.node with
           | W.Op { op = name; _ } when List.mem name context_ops && not (List.mem name !found) ->
               found := name :: !found
           | _ -> ());
           t)
         { graph with body; inputs = [] })
  in
  let bindings = inputs @ defaults in
  scan bindings graph.body;
  List.iter (fun (_, _, default) -> Option.iter (scan bindings) default) graph.inputs;
  !found

let depends ?(op = "sop/context_seed") ?inputs ?defs graph =
  List.mem op (context_ops_reached ?inputs ?defs graph)

let specialize ?(provenance = -1) ~context ~label graph =
  map
    (fun t ->
      match t.W.node with
      | W.Op { op = "sop/provenance_base"; _ } ->
          { t with node = W.Lit (Param.Int_value provenance) }
      | W.Op { op = "sop/context_frame"; _ } ->
          { t with node = W.Lit (Param.Float_value (Int64.to_float (Sop.Context.frame context))) }
      | W.Op { op = "sop/context_time"; _ } ->
          { t with node = W.Lit (Param.Float_value (Sop.Context.time context)) }
      | W.Op
          { op = "sop/context_seed"; args = [ ("namespace", { node = W.Text namespace; _ }) ]; _ }
        ->
          let seed = Operators.seed_for ~seed:(Sop.Context.seed context) ~namespace ~label () in
          { t with node = W.Lit (Param.Int_value seed) }
      | W.Op
          {
            op = "sop/context_seed";
            args =
              [
                ("namespace", { node = W.Text namespace; _ });
                ("salt", { node = W.Lit (Param.Int_value salt); _ });
              ];
            _;
          } ->
          let seed =
            Operators.seed_for ~salt ~seed:(Sop.Context.seed context) ~namespace ~label ()
          in
          { t with node = W.Lit (Param.Int_value seed) }
      | W.Op { op = "sop/context_seed"; _ } ->
          invalid_arg "E_SEED_NAMESPACE: Context seed requires a literal namespace."
      | _ -> t)
    graph

(* Forms reachable from [start]: [leading], then other reached node bodies, helpers, graphs and
   macros, then dispatched macro expansions. [skip] names a node whose body is already in [leading].
   A library definition is not walked: what it reaches is fixed with it (its id, {!forms_digest}). *)
let scoped_forms (checked : W.t) ~leading ~skip ~start =
  let library (form : Flow.Syntax.t) = form.id >= Flow.Syntax.library_base in
  let nodes = Hashtbl.create 8
  and defs = Hashtbl.create 8
  and graphs = Hashtbl.create 4
  and macros = Hashtbl.create 4 in
  let expanded = ref [] in
  let rec node (definition : W.node_definition) =
    let name = definition.kind.qualified in
    if not (Hashtbl.mem nodes name) then begin
      Hashtbl.add nodes name ();
      if not (library definition.body.form) then begin
        graph definition.body;
        List.iter (fun (t, _, _) -> term definition t) definition.validations;
        List.iter (fun (_, t) -> term definition t) definition.normalizers
      end
    end
  and definition table (g : W.graph) =
    if not (Hashtbl.mem table g.name) then begin
      Hashtbl.add table g.name ();
      if not (library g.form) then graph g
    end
  and named name =
    Option.iter (definition defs) (List.find_opt (fun (g : W.graph) -> g.name = name) checked.defs);
    Option.iter node
      (List.find_opt (fun (d : W.node_definition) -> d.kind.qualified = name) checked.nodes)
  and macro name =
    if not (Hashtbl.mem macros name) then
      Option.iter
        (fun form ->
          Hashtbl.add macros name ();
          if not (library form) then syntax form)
        (List.find_opt
           (fun form ->
             match Flow.Syntax.children form with
             | _ :: { node = Flow.Syntax.Sym selected; _ } :: _ -> selected = name
             | _ -> false)
           checked.macros)
  and syntax form =
    (* Macro.expand erases inner Expanded names. Follow their template heads. *)
    Option.iter macro (Flow.Syntax.head form);
    List.iter syntax (Flow.Syntax.children form)
  and transform (t : W.term) =
    (match t.node with
    | W.Call c -> named c.kind
    | Call_fn c -> named c.fn
    | Fn_ref name ->
        named name;
        Option.iter
          (fun (qualified, _, _) -> named qualified)
          (List.assoc_opt name checked.kind_fns)
    | Graph_ref g ->
        Option.iter (definition graphs)
          (List.find_opt (fun (d : W.graph) -> d.name = g.graph) checked.graphs);
        named g.graph
    | Expanded e ->
        macro e.macro;
        (* Parameterized macro heads disappear during recursive expansion.
            Hygiene counters restart per Macro.expand, not per Syntax.id. *)
        expanded := e.body.form :: !expanded
    | _ -> ());
    t
  (* a helper call's body is its definition's, walked once by name *)
  and graph (g : W.graph) = ignore (map ~helpers:false transform g)
  and term (definition : W.node_definition) t =
    graph { definition.body with body = t; inputs = [] }
  in
  start ~node ~graph;
  let forms =
    leading
    @ List.filter_map
        (fun (d : W.node_definition) ->
          if d.kind.qualified <> skip && Hashtbl.mem nodes d.kind.qualified then Some d.body.form
          else None)
        checked.nodes
    @ List.filter_map
        (fun (g : W.graph) -> if Hashtbl.mem defs g.name then Some g.form else None)
        checked.defs
    @ List.filter_map
        (fun (g : W.graph) -> if Hashtbl.mem graphs g.name then Some g.form else None)
        checked.graphs
    @ List.filter
        (fun form ->
          match Flow.Syntax.children form with
          | _ :: { node = Flow.Syntax.Sym name; _ } :: _ -> Hashtbl.mem macros name
          | _ -> false)
        checked.macros
  in
  forms @ List.rev !expanded

(* cache key: length-prefixed dump of spelling, comments and flags, no ids or spans; a library form is
   its id, fixed with the binary *)
let forms_digest forms =
  let buffer = Buffer.create 4096 in
  let str s =
    Buffer.add_string buffer (string_of_int (String.length s));
    Buffer.add_char buffer ':';
    Buffer.add_string buffer s
  in
  let strs l =
    Buffer.add_char buffer '[';
    List.iter str l;
    Buffer.add_char buffer ']'
  in
  let atom c s =
    Buffer.add_char buffer c;
    str s
  in
  let rec form (x : Flow.Syntax.t) =
    if x.id >= Flow.Syntax.library_base then atom 'L' (string_of_int x.id) else library_free x
  and library_free (x : Flow.Syntax.t) =
    if x.notes <> [] then strs x.notes;
    if x.meta <> [] then strs x.meta;
    (match x.node with
    | Sym s -> atom 's' s
    | Kw s -> atom 'k' s
    | Num s -> atom 'n' s
    | Str s -> atom 't' s
    | List l ->
        Buffer.add_char buffer '(';
        List.iter form l
    | Vec l ->
        Buffer.add_char buffer '<';
        List.iter form l
    | Map l ->
        Buffer.add_char buffer '{';
        List.iter form l
    | Quote (kind, x) ->
        Buffer.add_char buffer 'q';
        Buffer.add_char buffer
          (match kind with Plain -> 'p' | Quasi -> 'q' | Unquote -> 'u' | Splice -> 's');
        form x);
    if x.tail <> [] then strs x.tail;
    Buffer.add_char buffer ')'
  in
  List.iter form forms;
  Digest.string (Buffer.contents buffer)

let declaration_source (checked : W.t) (root : W.node_definition) =
  forms_digest
    (scoped_forms checked ~leading:[ root.body.form ] ~skip:root.kind.qualified
       ~start:(fun ~node ~graph:_ -> node root))

(* The kernel cache key: the authored source plus what the graphs being lowered reach, not the whole library. *)
let kernel_source (checked : W.t) =
  forms_digest
    (checked.source
    @ scoped_forms checked
        ~leading:(List.map (fun (g : W.graph) -> g.form) checked.graphs)
        ~skip:""
        ~start:(fun ~node:_ ~graph -> List.iter graph checked.graphs))

let uses_context graph =
  let found = ref false in
  ignore
    (map
       (fun t ->
         (match t.W.node with
         | W.Op { op; _ } when List.mem op context_ops -> found := true
         | _ -> ());
         t)
       graph);
  !found

(* The helpers [roots] reach through [Call_fn], [Fn_ref] and, through the node kinds they call, other Lisp nodes' bodies:
   what a nested lowering must carry (qualifying a library of 1000 helpers per node body was the cost). *)
let reachable_defs (workspace : W.t) roots =
  let seen = Hashtbl.create 16 in
  let rec visit graph =
    let found = ref [] in
    ignore
      (map
         (fun (t : W.term) ->
           (match t.node with
           | W.Call_fn c -> found := c.fn :: !found
           | Call c -> found := c.kind :: !found
           | Fn_ref name -> found := name :: !found
           | Graph_ref { graph; _ } when String.starts_with ~prefix:"def:" graph ->
               found := String.sub graph 4 (String.length graph - 4) :: !found
           | _ -> ());
           t)
         graph);
    List.iter
      (fun name ->
        if not (Hashtbl.mem seen name) then begin
          Hashtbl.add seen name ();
          List.iter (fun (d : W.graph) -> if d.name = name then visit d) workspace.defs;
          List.iter
            (fun (n : W.node_definition) ->
              if n.kind.qualified = name then
                List.iter visit
                  (n.body
                  :: List.map
                       (fun term -> { n.body with W.body = term; inputs = [] })
                       (List.map (fun (t, _, _) -> t) n.validations @ List.map snd n.normalizers)))
            workspace.nodes
        end)
      !found
  in
  List.iter visit roots;
  List.filter (fun (d : W.graph) -> Hashtbl.mem seen d.name) workspace.defs

(* An inlined body reads its cook context through inputs: each distinct context operator, with
   the card's cook label, is the default of a reserved input the body refers to by name. It evaluates
   to a cook-time source, which residuals capture as a binding, never as a term, so a kernel
   specializes it per context by its captures alone. *)
let label_context ~label graph =
  let inputs = ref [] in
  let key (args : (string * W.term) list) =
    List.map
      (fun (name, (a : W.term)) ->
        match a.node with
        | W.Text s -> name ^ "=" ^ s
        | Lit (Param.Int_value n) -> name ^ "=" ^ string_of_int n
        | _ -> name ^ "=?")
      args
  in
  (* a helper's body has its own scope: its operators read the card's label from the evaluation *)
  let graph =
    map ~helpers:false
      (fun t ->
        match t.W.node with
        | W.Op ({ op; args; _ } as o) when List.mem op context_ops ->
            let k = String.concat ";" (op :: key args) in
            let name =
              match List.assoc_opt k !inputs with
              | Some (name, _) -> name
              | None ->
                  let name = "$context" ^ string_of_int (List.length !inputs) in
                  let text = { t with node = W.Text label; path = None; ty = Flow.Ty.Text } in
                  let call =
                    {
                      t with
                      node = W.Op { o with args = args @ [ (Operators.context_label, text) ] };
                    }
                  in
                  inputs := !inputs @ [ (k, (name, call)) ];
                  name
            in
            { t with node = W.Ref_binding (name, []) }
        | _ -> t)
      graph
  in
  ( {
      graph with
      inputs =
        graph.inputs
        @ List.map (fun (_, (name, (call : W.term))) -> (name, call.ty, Some call)) !inputs;
    },
    List.map
      (fun (_, (name, (call : W.term))) ->
        (name, match call.node with W.Op { op; _ } -> op | _ -> assert false))
      !inputs )
