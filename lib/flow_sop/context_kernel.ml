module W = Flow.Workspace

(* The form id spreads terms; hashing the term itself collides on the many alike subtrees and degrades into long bucket scans. *)
module Memo = Hashtbl.Make (struct type t = W.term let equal = ( == ) let hash (t : W.term) = t.form.Flow.Syntax.id land max_int end)

(* Checked terms share subterms; rewrite each once (keeping the sharing), or a shared chain is
   walked exponentially often. [transform] is pure per term, so the memo changes nothing else. *)
let map ?(select=Fun.id) transform (graph:W.graph) =
  let memo=Memo.create 256 in
  let rec term (t:W.term) = match t.node with
    | Lit _ | Text _ | Nil | Time | Ref_binding _ | Fn_ref _ -> term_once t  (* cheaper to rebuild than to look up *)
    | _ -> match Memo.find_opt memo t with
      | Some mapped -> mapped
      | None -> let mapped=term_once t in Memo.add memo t mapped; mapped
  and term_once (t:W.term) =
    let t=select t in
    let fields xs=List.map (fun (name,value) -> name,term value) xs in
    let patterns xs=List.map (fun (pattern,value) -> pattern,term value) xs in
    let node=match t.node with
      | W.Call c -> W.Call {c with args=fields c.args}
      | Op c -> Op {c with args=fields c.args}
      | Call_fn c -> Call_fn {c with args=List.map term c.args;body=Option.map term c.body}
      | Graph_ref g -> Graph_ref {g with inputs=fields g.inputs}
      | Let (bindings,body) -> Let (patterns bindings,term body)
      | State s -> State {s with init=term s.init;step=term s.step}
      | Loop l -> Loop {l with accs=patterns l.accs;clauses=patterns l.clauses;body=term l.body}
      | With_error(a,b) -> With_error(term a,term b)
      | Error_rows scope -> Error_rows{scope with rows=term scope.rows;mapper=term scope.mapper}
      | If (a,b,c) -> If (term a,term b,term c)
      | Cond (arms,default) -> Cond (List.map (fun (a,b)->term a,term b) arms,term default)
      | Case (value,arms,default) -> Case (term value,patterns arms,term default)
      | Fn f -> Fn {f with body=term f.body;capture=Option.map term f.capture}
      | Hof (kind,args) -> Hof (kind,List.map term args)
      | Vec args -> Vec (List.map term args)
      | List_lit args -> List_lit (List.map term args)
      | Record args -> Record (fields args)
      | Get (value,key) -> Get (term value,key)
      | Assoc (value,args) -> Assoc (term value,fields args)
      | Str args -> Str (List.map term args)
      | List_op (kind,args) -> List_op (kind,List.map term args)
      | Bypass value -> Bypass (term value)
      | Expanded e -> Expanded {e with body=term e.body}
      | Lit _ | Text _ | Nil | Time | Ref_binding _ | Fn_ref _ -> t.node in
    transform {t with node} in
  {graph with W.body=term graph.W.body;inputs=List.map (fun (n,ty,df)->n,ty,Option.map term df) graph.inputs}

let context_ops=Flow.Workspace.body_context_ops

let depends ?(op="sop/context_seed") ?(inputs=[]) ?(defs=[]) (graph:W.graph) =
  let defaults=List.filter_map (fun (name,_,df)->Option.bind df (fun (t:W.term)->match t.node with
    | W.Lit (Param.Bool_value b)->Some (name,Flow.Eval.Bool b)
    | Lit (Int_value n)->Some (name,Flow.Eval.Int n)
    | Lit (Float_value f)->Some (name,Flow.Eval.Float f)
    | Text text->Some (name,Flow.Eval.Text text)|_->None)) graph.inputs in
  let found=ref false and fuel=ref 4096 in
  let rec names = function
    | W.Name name -> [name]
    | Seq patterns -> List.concat_map names patterns
    | Keys names -> names in
  let remove patterns bindings =
    let bound=List.concat_map names patterns in
    List.filter (fun (name,_) -> not (List.mem name bound)) bindings in
  let rec known bindings (t:W.term) = match t.node with
    | W.Lit (Param.Bool_value b)->Some (Flow.Eval.Bool b)
    | Lit (Int_value n)->Some (Flow.Eval.Int n)
    | Lit (Float_value f)->Some (Flow.Eval.Float f)
    | Text text->Some (Flow.Eval.Text text)
    | Ref_binding (name,[])->List.assoc_opt name bindings
    | Call_fn call when !fuel > 0 ->
        (* a helper over known arguments (a text predicate, say) decides its own branches *)
        decr fuel;
        Option.bind (List.find_opt (fun (d:W.graph) -> d.name=call.fn) defs) (fun helper->
          let local=List.mapi (fun i (name,_,default) ->
            let value=match List.nth_opt call.args i with
              | Some argument -> known bindings argument
              | None -> Option.bind default (known []) in
            Option.map (fun value -> name,value) value) helper.inputs in
          if List.mem None local then None
          else known (List.filter_map Fun.id local) (Option.value ~default:helper.body call.body))
    | Op {op;args;_} when not (List.mem op context_ops) ->
        let rec values = function
          |[]->Some []
          |(key,value)::rest->Option.bind (known bindings value) (fun value->Option.map (fun rest->(key,value)::rest) (values rest)) in
        Option.bind (values args) (fun args->Option.bind (Flow.Op.find ~extra:Operators.all op Flow.Context.value) (fun op->
          try op.check args; Some (op.body ~live:(Frame_input.at_time 0.) ~node:(fun _ _->raise Exit) args)
          with Flow.Value.Fail _ | Flow.Value.Diagnostic_fail _ | Exit -> None))
    |_->None
  and scan bindings (body:W.term) =
    let select (t:W.term)=match t.node with
      | W.If (test,yes,no)->(match known bindings test with
          | Some (Flow.Eval.Bool true)->yes|Some (Bool false)->no|_->t)
      | Cond (arms,default) ->
          (* An arm whose test is known false never runs; the first known-true arm is the result. *)
          let rec live = function
            | [] -> [], None
            | ((test:W.term),yes)::rest ->
                (match known bindings test with
                 | Some (Flow.Eval.Bool false) -> live rest
                 | Some (Bool true) -> [], Some yes
                 | _ -> let arms,taken = live rest in (test,yes)::arms, taken) in
          (match live arms with
           | [], Some yes -> yes
           | [], None -> default
           | arms, Some yes -> {t with node=Cond (arms,yes)}
           | arms, None -> {t with node=Cond (arms,default)})
      | Call_fn call ->
          (match List.find_opt (fun (d:W.graph) -> d.name=call.fn) defs with
           | Some helper ->
               let local=List.mapi (fun i (name,_,default) ->
                 let value=match List.nth_opt call.args i with
                   | Some argument -> known bindings argument
                   | None -> Option.bind default (known []) in
                 Option.map (fun value -> name,value) value) helper.inputs |> List.filter_map Fun.id in
               scan local (Option.value ~default:helper.body call.body)
           | None -> Option.iter (scan []) call.body);
          {t with node=Call_fn {call with body=None}}
      | Fn fn ->
          scan (remove (List.map fst fn.params) bindings) fn.body;
          {t with node=Fn {fn with body={fn.body with node=Nil}}}
      | Fn_ref name ->
          Option.iter (fun (helper:W.graph) -> scan [] helper.body)
            (List.find_opt (fun (d:W.graph) -> d.name=name) defs);
          t
      | Let (locals,body) ->
          let bindings=List.fold_left (fun bindings (pattern,value) ->
            scan bindings value;
            let local=match pattern,known bindings value with
              | W.Name name,Some value -> [name,value] | _ -> [] in
            local @ remove [pattern] bindings) bindings locals in
          scan bindings body;
          {t with node=Nil}
      | State state ->
          scan bindings state.init;
          scan (remove [state.binder] bindings) state.step;
          {t with node=Nil}
      | Loop loop ->
          List.iter (fun (_,value) -> scan bindings value) loop.accs;
          let bindings=remove (List.map fst loop.accs) bindings in
          let bindings=List.fold_left (fun bindings (pattern,value) ->
            scan bindings value; remove [pattern] bindings) bindings loop.clauses in
          scan bindings loop.body;
          {t with node=Nil}
      |_->t in
    ignore (map ~select (fun t -> (match t.W.node with
      | W.Op {op=name;_} when name=op -> found:=true | _->()); t)
      {graph with body;inputs=[]}) in
  let bindings=inputs@defaults in
  scan bindings graph.body;
  List.iter (fun (_,_,default) -> Option.iter (scan bindings) default) graph.inputs;
  !found

let specialize ?(provenance= -1) ~context ~label graph = map (fun t -> match t.W.node with
  | W.Op {op="sop/provenance_base";_} -> {t with node=W.Lit (Param.Int_value provenance)}
  | W.Op {op="sop/context_frame";_} ->
      {t with node=W.Lit (Param.Float_value (Int64.to_float (Sop.Context.frame context)))}
  | W.Op {op="sop/context_time";_} ->
      {t with node=W.Lit (Param.Float_value (Sop.Context.time context))}
  | W.Op {op="sop/context_seed";args=["namespace",{node=W.Text namespace;_}];_} ->
      let seed=Operators.seed_for ~seed:(Sop.Context.seed context) ~namespace ~label () in
      {t with node=W.Lit (Param.Int_value seed)}
  | W.Op {op="sop/context_seed";args=["namespace",{node=W.Text namespace;_};"salt",{node=W.Lit (Param.Int_value salt);_}];_} ->
      let seed=Operators.seed_for ~salt ~seed:(Sop.Context.seed context) ~namespace ~label () in
      {t with node=W.Lit (Param.Int_value seed)}
  | W.Op {op="sop/context_seed";_} -> invalid_arg "E_SEED_NAMESPACE: Context seed requires a literal namespace."
  | _ -> t) graph

(* Forms reachable from [start]: [leading], then other reached node bodies, helpers, graphs and
   macros, then dispatched macro expansions. [skip] names a node whose body is already in [leading]. *)
let scoped_forms (checked:W.t) ~leading ~skip ~start =
  let nodes=Hashtbl.create 8 and defs=Hashtbl.create 8
  and graphs=Hashtbl.create 4 and macros=Hashtbl.create 4 in
  let expanded=ref [] in
  let rec node (definition:W.node_definition) =
    let name=definition.kind.qualified in
    if not(Hashtbl.mem nodes name) then begin
      Hashtbl.add nodes name ();
      graph definition.body;
      List.iter(fun(t,_,_)->term definition t)definition.validations;
      List.iter(fun(_,t)->term definition t)definition.normalizers
    end
  and definition table (g:W.graph) =
    if not(Hashtbl.mem table g.name) then begin
      Hashtbl.add table g.name ();graph g
    end
  and named name =
    Option.iter (definition defs)
      (List.find_opt(fun(g:W.graph)->g.name=name)checked.defs);
    Option.iter node (List.find_opt(fun(d:W.node_definition)->
      d.kind.qualified=name || List.mem name d.kind.aliases)checked.nodes)
  and macro name =
    if not(Hashtbl.mem macros name) then
      Option.iter(fun form->Hashtbl.add macros name ();syntax form)
        (List.find_opt(fun form->match Flow.Syntax.children form with
          | _::{node=Flow.Syntax.Sym selected;_}::_ -> selected=name
          | _ -> false)checked.macros)
  and syntax form =
    (* Macro.expand erases inner Expanded names. Follow their template heads. *)
    Option.iter macro (Flow.Syntax.head form);
    List.iter syntax (Flow.Syntax.children form)
  and transform (t:W.term) =
    (match t.node with
     | W.Call c -> named c.kind
     | Call_fn c -> named c.fn
     | Fn_ref name -> named name;
         Option.iter(fun(qualified,_,_)->named qualified)
           (List.assoc_opt name checked.kind_fns)
     | Graph_ref g ->
         Option.iter (definition graphs)
           (List.find_opt(fun(d:W.graph)->d.name=g.graph)checked.graphs);
         named g.graph
     | Expanded e ->
         macro e.macro;
         (* Parameterized macro heads disappear during recursive expansion.
            Hygiene counters restart per Macro.expand, not per Syntax.id. *)
         expanded:=e.body.form::!expanded
     | _ -> ());t
  and graph (g:W.graph) = ignore(map transform g)
  and term (definition:W.node_definition) t = graph {definition.body with body=t;inputs=[]} in
  start ~node ~graph;
  let forms=leading@List.filter_map(fun(d:W.node_definition)->
      if d.kind.qualified<>skip && Hashtbl.mem nodes d.kind.qualified
      then Some d.body.form else None)checked.nodes
    @ List.filter_map(fun(g:W.graph)->if Hashtbl.mem defs g.name then Some g.form else None)checked.defs
    @ List.filter_map(fun(g:W.graph)->if Hashtbl.mem graphs g.name then Some g.form else None)checked.graphs
    @ List.filter(fun form->match Flow.Syntax.children form with
      | _::{node=Flow.Syntax.Sym name;_}::_ -> Hashtbl.mem macros name
      | _ -> false)checked.macros in
  forms @ List.rev !expanded

(* A cache key for forms: a compact, length-prefixed serialisation of everything the printer shows
   (spelling, comments, flags) without ids or spans. Pretty-printing them cost a third of a workspace
   lowering; the key only has to change when the authored content changes. *)
let forms_digest forms =
  let buffer = Buffer.create 4096 in
  let str s = Buffer.add_string buffer (string_of_int (String.length s)); Buffer.add_char buffer ':'; Buffer.add_string buffer s in
  let strs l = Buffer.add_char buffer '['; List.iter str l; Buffer.add_char buffer ']' in
  let rec form (x : Flow.Syntax.t) =
    if x.notes <> [] then strs x.notes;
    if x.meta <> [] then strs x.meta;
    (match x.node with
     | Sym s -> Buffer.add_char buffer 's'; str s
     | Kw s -> Buffer.add_char buffer 'k'; str s
     | Num s -> Buffer.add_char buffer 'n'; str s
     | Str s -> Buffer.add_char buffer 't'; str s
     | List l -> Buffer.add_char buffer '('; List.iter form l
     | Vec l -> Buffer.add_char buffer '<'; List.iter form l
     | Map l -> Buffer.add_char buffer '{'; List.iter form l
     | Quote (kind, x) ->
         Buffer.add_char buffer 'q';
         Buffer.add_char buffer (match kind with Plain -> 'p' | Quasi -> 'q' | Unquote -> 'u' | Splice -> 's');
         form x);
    if x.tail <> [] then strs x.tail;
    Buffer.add_char buffer ')' in
  List.iter form forms;
  Digest.string (Buffer.contents buffer)

let declaration_source (checked:W.t) (root:W.node_definition) =
  forms_digest
    (scoped_forms checked ~leading:[root.body.form] ~skip:root.kind.qualified
       ~start:(fun ~node ~graph:_ -> node root))

(* The kernel cache key: the authored source plus what the graphs being lowered reach, not the whole library. *)
let kernel_source (checked:W.t) =
  forms_digest (checked.source @
    scoped_forms checked ~leading:(List.map (fun (g:W.graph)->g.form) checked.graphs) ~skip:""
      ~start:(fun ~node:_ ~graph -> List.iter graph checked.graphs))

let uses_context graph =
  let found = ref false in
  ignore (map (fun t -> (match t.W.node with
    | W.Op {op;_} when List.mem op context_ops -> found := true
    | _ -> ()); t) graph);
  !found

(* Fusing adjacent Lisp point deformers (plan B.F). A node is fusable when its body is one
   [let*] with a single [sop/with_attr in0 :P V] binding, no reductions, folds, errors or
   branches on cooked data. [fuse x u] makes [x]'s body read [u]'s [P] values directly and
   skip [u]'s write, so the two maps share one kernel and [u]'s cook disappears. *)
let reducers = ["array/max";"array/min";"array/sum";"array/any";"array/all";"array/argmax";"array/argmin"]
let scalar = function Flow.Ty.Float | Int | Bool | Text -> true | _ -> false
let is_in0 (t:W.term) = match t.node with W.Ref_binding("in0",[]) -> true | _ -> false
let is_p_read (t:W.term) = match t.node with
  | W.Op {op="sop/attr";args;_} ->
      List.for_all (fun (n,_) -> n="geometry"||n="attribute") args
      && (match List.assoc_opt "geometry" args, List.assoc_opt "attribute" args with
         | Some g, Some {W.node=Text "P";_} -> is_in0 g | _ -> false)
  | _ -> false
let with_p (t:W.term) = match t.node with
  | W.Op {op="sop/with_attr";args;_} when List.for_all (fun (n,_) -> n="geometry"||n="attribute"||n="values") args ->
      (match List.assoc_opt "geometry" args, List.assoc_opt "attribute" args, List.assoc_opt "values" args with
       | Some g, Some {W.node=Text "P";_}, Some v when is_in0 g -> Some v | _ -> None)
  | _ -> None
let fusable_binding (d:W.node_definition) = match d.body.body.node with
  | W.Let (bindings, tail) ->
      let found = List.filter_map (fun (i, (pattern, rhs)) -> match pattern, with_p rhs with
        | W.Name n, Some values -> Some (i, n, values) | _ -> None)
        (List.mapi (fun i b -> i, b) bindings) in
      (match found with [i, n, values] -> Some (bindings, tail, i, n, values) | _ -> None)
  | _ -> None
let fusable (d:W.node_definition) =
  let params = List.filter_map (fun (n,_,_) -> if n="in0" then None else Some n) d.body.inputs in
  let pure condition =
    let ok = ref true in
    ignore (map (fun (t:W.term) -> (match t.node with
      | W.Lit _ | Text _ | Op _ | Str _ | Vec _ -> ()
      | Ref_binding (n,_) when List.mem n params -> ()
      | _ -> ok := false); t) {d.body with body=condition;inputs=[]});
    !ok in
  let ok = ref (d.validations=[] && d.normalizers=[]
    && (match d.slots with [slot] -> not slot.rest | _ -> false)
    && List.for_all (fun (n,ty,_) -> n="in0" || scalar ty) d.body.inputs
    && fusable_binding d <> None) in
  let attrs = ref 0 in
  ignore (map (fun (t:W.term) -> (match t.node with
    | W.Loop _ | State _ | With_error _ | Error_rows _ | Graph_ref _ -> ok := false
    | Hof (kind,_) when kind <> `Map -> ok := false
    | Op {op="sop/with_attr";_} -> incr attrs
    | Op {op="sop/attr"|"sop/delete_attr"|"sop/context_seed"|"sop/context_frame"|"sop/context_time";_} -> ()
    | Call _ -> ok := false
    | Op {op;_} when List.mem op reducers || String.starts_with ~prefix:"sop/" op -> ok := false
    | If (c,_,_) -> if not (pure c) then ok := false
    | Cond (arms,_) -> if not (List.for_all (fun (c,_) -> pure c) arms) then ok := false
    | Case (c,_,_) -> if not (pure c) then ok := false
    | _ -> ()); t) d.body);
  !ok && !attrs = 1

let literal (value:Flow.Eval.value) : W.term option =
  let make ty node = Some W.{path=None;ty;node;form=Flow.Syntax.make (List [])} in
  match value with
  | Float f -> make Flow.Ty.Float (Lit (Param.Float_value f))
  | Int n -> make Int (Lit (Param.Int_value n))
  | Bool b -> make Bool (Lit (Param.Bool_value b))
  | Text s -> make Text (Text s)
  | _ -> None

let fuse (x:W.node_definition) (u:W.node_definition) ~(baked:(string*Flow.Eval.value) list) =
  match fusable_binding x, fusable_binding u with
  | Some (xbs, xtail, _, _, _), Some (ubs, utail, k, cname, values) ->
      let constants = List.filter_map (fun (name,_,default) -> if name="in0" then None else
        Option.map (fun term -> W.Name name, term)
          (match Option.bind (List.assoc_opt name baked) literal with
           | Some term -> Some term | None -> default)) u.body.inputs in
      let uses_name = ref false in
      ignore (map (fun (t:W.term) -> (match t.node with
        | W.Ref_binding (n,_) when n=cname -> uses_name := true | _ -> ()); t)
        {u.body with body=values;inputs=[]});
      if List.length constants <> List.length u.body.inputs - 1 || !uses_name then None else begin
        let term ty node = W.{path=None;ty;node;form=Flow.Syntax.make (List [])} in
        let geometry = Flow.Ty.geometry and array = Flow.Ty.Array Flow.Ty.Vec3 in
        let ref_ ty name = term ty (W.Ref_binding (name,[])) in
        let retag tag term = (map (fun (t:W.term) -> {t with path=Option.map (fun path -> tag :: path) t.path})
          {u.body with body=term;inputs=[]}).body in
        let prefix_bindings = List.filteri (fun i _ -> i < k) ubs in
        let moved = List.mapi (fun i (pattern, rhs) -> if i = k then pattern, ref_ geometry "in0" else pattern, rhs) ubs in
        let region_g = retag "lisp_fuse_u" (term geometry (W.Let (constants @ moved, utail))) in
        (* [exact] on the producer's values would materialize them; the consumer's own [exact] remains. *)
        let values = match values.node with W.Op {op="exact";args=[_,inner];_} -> inner | _ -> values in
        let region_p = retag "lisp_fuse_v" (term array (W.Let (constants @ prefix_bindings, values))) in
        let replace = map (fun (t:W.term) -> if is_p_read t then ref_ t.ty "lisp_fuse_p" else t)
          {x.body with body=term geometry (W.Let (xbs, xtail));inputs=[]} in
        let xbs, xtail = match replace.body.node with W.Let (b,t) -> b,t | _ -> xbs, xtail in
        let prefix = [W.Name "lisp_fuse_p", region_p; W.Name "in0", region_g] in
        let identity = Digest.to_hex (Digest.string (Marshal.to_string baked [Marshal.No_sharing])) in
        let form = Flow.Syntax.make (List [x.body.form; u.body.form;
          Flow.Syntax.make (Sym ("fused-" ^ identity))]) in
        Some {x with body={x.body with body={x.body.body with node=W.Let (prefix @ xbs, xtail)};form}}
      end
  | _ -> None
