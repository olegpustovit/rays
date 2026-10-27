module W = Flow.Workspace
module E = Flow.Eval

let for_node checked (definition : W.node_definition) =
  List.map(fun(name,(normalizer:W.term))->
    let form=normalizer.W.form in
    let term ty node=W.{path=None;ty;node;form} in
    let argument=term Flow.Ty.Text (Ref_binding("raw_value",[])) in
    let body=term Flow.Ty.Text (Let([W.Name "normalizer",normalizer],
      term Flow.Ty.Text (Call_fn{fn="normalizer";args=[argument];body=None}))) in
    let graph=W.{name="lisp_normalize_"^name;context=Flow.Context.value;
      inputs=["raw_value",Flow.Ty.Text,None];body;form} in
    let workspace={checked with W.graphs=[graph]} in
    let evaluate raw=Result.bind(E.Residual.handle (fun () -> E.Private.static_with_requests ~budget:max_int
      ~inputs:[graph.name,["raw_value",E.Text raw]] workspace))(fun result->
      match List.assoc graph.name result.results with
      |E.Text canonical when Array.length result.plan.nodes=0->Ok canonical
      |_->Error(Diagnostic.error ~span:form.span ~code:"E_PARAM_NORMALIZE"
          "An encoded normalizer must return concrete text without cooked effects.")) in
    let normalize raw=Flow.Phase_timer.without(fun()->Result.bind(evaluate raw)(fun canonical->
      Result.bind(evaluate canonical)(fun again->if again=canonical then Ok canonical else
        Error(Diagnostic.error ~span:form.span ~code:"E_PARAM_NORMALIZE"
          "An encoded normalizer must preserve its canonical output.")))) in
    name,normalize)definition.normalizers

let normalize_defaults workspace =
  let normalize_node (node : W.node_definition) =
    let normalizers=for_node workspace node in
    let fields=List.fold_left(fun result (field:Param.field_view)->Result.bind result(fun fields->
      match List.assoc_opt field.name normalizers,field.default with
      |Some normalize,Param.Text_value raw->Result.map(fun canonical->
          let value=Param.Text_value canonical in fields@[{field with default=value;current=value}])(normalize raw)
      |_->Ok(fields@[field]))) (Ok[])node.fields in
    Result.bind fields (fun fields ->
      Result.map (fun parameters ->
        {node with fields;kind={node.kind with parameters=parameters @
          List.filter(fun(p:Flow.Check.parameter)->List.mem p.name node.keyword_inputs)node.kind.parameters}})
        (Result.map_error (Diagnostic.error ~code:"E_VEC3")
          (Flow.Check.parameters_of_fields fields))) in
  Result.map(fun nodes->{workspace with W.nodes})
    (List.fold_left(fun result node->Result.bind result(fun nodes->
      Result.map(fun node->nodes@[node])(normalize_node node))) (Ok[])workspace.W.nodes)

let list f values=Result.map List.rev(List.fold_left(fun result value->
  Result.bind result(fun values->Result.map(fun value->value::values)(f value))) (Ok[])values)
let pairs f values=list(fun(name,value)->Result.map(fun value->name,value)(f value))values
let optional f=function None->Ok None|Some value->Result.map Option.some(f value)

let checked workspace =
  if List.for_all(fun(node:W.node_definition)->node.normalizers=[])workspace.W.nodes then Ok workspace else
  Result.bind(normalize_defaults workspace)(fun workspace->
  let normalizers=List.map(fun(node:W.node_definition)->node.kind.qualified,for_node workspace node)workspace.W.nodes in
  let rec walk (term:W.term) =
    let finish node=Ok{term with W.node} in
    let one f child=Result.bind(walk child)(fun child->finish(f child)) in
    let two f a b=Result.bind(walk a)(fun a->Result.bind(walk b)(fun b->finish(f a b))) in
    let many f values=Result.bind(list walk values)(fun values->finish(f values)) in
    let fields f values=Result.bind(pairs walk values)(fun values->finish(f values)) in
    match term.node with
    | W.Call call->Result.bind(pairs walk call.args)(fun args->
        let hooks=Option.value ~default:[](List.assoc_opt call.kind normalizers) in
        Result.bind(list(fun(name,(arg:W.term))->match List.assoc_opt name hooks,arg.node with
            |Some normalize,W.Text raw->Result.map_error(fun(d:Diagnostic.t)->{d with span=Some arg.form.span})
                (Result.map(fun canonical->name,{arg with node=W.Text canonical})(normalize raw))
            |_->Ok(name,arg))args)(fun args->finish(W.Call{call with args})))
    | Op call->fields(fun args->W.Op{call with args})call.args
    | Graph_ref call->fields(fun inputs->W.Graph_ref{call with inputs})call.inputs
    | Call_fn call->Result.bind(list walk call.args)(fun args->Result.bind(optional walk call.body)(fun body->finish(W.Call_fn{call with args;body})))
    | Vec values->many(fun values->W.Vec values)values
    | List_lit values->many(fun values->W.List_lit values)values
    | Str values->many(fun values->W.Str values)values
    | Hof(kind,values)->many(fun values->W.Hof(kind,values))values
    | List_op(kind,values)->many(fun values->W.List_op(kind,values))values
    | Record values->fields(fun values->W.Record values)values
    | Let(bindings,body)->Result.bind(pairs walk bindings)(fun bindings->one(fun body->W.Let(bindings,body))body)
    | Loop loop->Result.bind(pairs walk loop.accs)(fun accs->Result.bind(pairs walk loop.clauses)(fun clauses->one(fun body->W.Loop{loop with accs;clauses;body})loop.body))
    | State state->two(fun init step->W.State{state with init;step})state.init state.step
    | With_error(a,b)->two(fun a b->W.With_error(a,b))a b
    | Error_rows scope->two(fun rows mapper->W.Error_rows{scope with rows;mapper})scope.rows scope.mapper
    | If(condition,yes,no)->Result.bind(walk condition)(fun condition->two(fun yes no->W.If(condition,yes,no))yes no)
    | Cond(arms,default)->Result.bind(list(fun(a,b)->Result.bind(walk a)(fun a->Result.map(fun b->a,b)(walk b)))arms)
        (fun arms->one(fun default->W.Cond(arms,default))default)
    | Case(value,arms,default)->Result.bind(walk value)(fun value->Result.bind(pairs walk arms)(fun arms->one(fun default->W.Case(value,arms,default))default))
    | Fn fn->Result.bind(optional walk fn.capture)(fun capture->one(fun body->W.Fn{fn with capture;body})fn.body)
    | Get(value,field)->one(fun value->W.Get(value,field))value
    | Bypass value->one(fun value->W.Bypass value)value
    | Expanded expansion->one(fun body->W.Expanded{expansion with body})expansion.body
    | Assoc(value,updates)->Result.bind(walk value)(fun value->fields(fun updates->W.Assoc(value,updates))updates)
    | Lit _ | Text _ | Nil | Time | Ref_binding _ | Fn_ref _->Ok term in
  let graph (graph:W.graph)=Result.bind(walk graph.body)(fun body->Result.map(fun inputs->{graph with body;inputs})
    (list(fun(name,ty,default)->Result.map(fun default->name,ty,default)(optional walk default))graph.inputs)) in
  Result.bind(list graph workspace.graphs)(fun graphs->Result.bind(list graph workspace.defs)(fun defs->
    Result.map(fun nodes->{workspace with W.graphs;defs;nodes})
      (list(fun(node:W.node_definition)->Result.map(fun body->{node with body})(graph node.body))workspace.nodes))))
