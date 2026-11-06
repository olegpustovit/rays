//! Port of lib/flow_sop/context_kernel.ml(i): cook-context constants available to Lisp node bodies
//! (`sop/context_seed` and friends), and the source digests that key prepared kernels.
use crate::operators;
use flow_check::op;
use flow_check::workspace::{self as w, Graph, Node, NodeDefinition, Term, Workspace};
use flow_check::term_arena::{Draft, Literal, PatternId, PatternNode, TermBuilder, TermId};
use flow_syntax::context;
use flow_syntax::syntax::{self, Node as SNode, QuoteKind, Syntax};
use flow_syntax::ty::Ty;
use flow_syntax::value::Value;
use rays_diagnostic::Diagnostic;
use std::cell::{Cell, RefCell};
use std::collections::HashSet;

fn is_context_op(name: &str) -> bool {
    w::BODY_CONTEXT_OPS.contains(&name)
}

type Bindings = Vec<(String, Value)>;

fn remove_pattern(term: &Term, pattern: PatternId, bindings: &[(String, Value)]) -> Bindings {
    fn binds(term: &Term, pattern: PatternId, name: &str) -> bool {
        match term.arena().pattern(pattern) {
            PatternNode::Name(id) => term.name(id) == name,
            PatternNode::Keys(ids) => term.arena().name_ids(ids).iter().any(|&id| term.name(id) == name),
            PatternNode::Seq(ids) => term.arena().pattern_ids(ids).iter().any(|&id| binds(term, id, name)),
        }
    }
    bindings.iter().filter(|(name, _)| !binds(term, pattern, name)).cloned().collect()
}
struct Scan {

    found: RefCell<Vec<String>>,
    fuel: Cell<i64>,
}

fn literal(t: &Term) -> Option<Value> {
    match t.node {
        Node::Lit(id) => match t.arena().literal(id) {
            Literal::Bool(b) => Some(Value::Bool(b)),
            Literal::Int(n) => Some(Value::Int(n)),
            Literal::Float(f) => Some(Value::Float(f)),
            _ => None,
        },
        Node::Text(id) => Some(Value::Text(t.arena().bytes(id).to_vec())),
        _ => None,
    }
}

/// The values a helper's inputs take over known arguments (`None` entries for unknown ones).
fn local_inputs(scan: &Scan, defs: &[Graph], helper: &Graph, owner: &Term, args: &[TermId], bindings: &[(String, Value)]) -> Vec<Option<(String, Value)>> {
    helper.inputs.iter().enumerate().map(|(i, (name, _, default))| {
        let value = match args.get(i) {
            Some(&argument) => scan.known(defs, bindings, &owner.child(argument)),
            None => default.as_ref().and_then(|d| scan.known(defs, &[], d)),
        };
        value.map(|value| (name.clone(), value))
    }).collect()
}

impl Scan {
    fn known(&self, defs: &[Graph], bindings: &[(String, Value)], t: &Term) -> Option<Value> {
        if let Some(value) = literal(t) {
            return Some(value);
        }
        match t.node {
            Node::RefBinding(id) if t.arena().name_ids(t.arena().ref_binding(id).fields).is_empty() => {
                let name = t.name(t.arena().ref_binding(id).name);
                bindings.iter().find(|(n, _)| n == name).map(|(_, v)| v.clone())
            }
            Node::CallFn(id) if self.fuel.get() > 0 => {
                self.fuel.set(self.fuel.get() - 1);
                let call = t.arena().call_fn(id);
                let helper = defs.iter().find(|d| d.name == t.name(call.name))?;
                let local = local_inputs(self, defs, helper, t, t.arena().term_ids(call.args), bindings);
                if local.iter().any(Option::is_none) { return None; }
                let local: Bindings = local.into_iter().flatten().collect();
                match call.body {
                    Some(body) => self.known(defs, &local, &t.child(body)),
                    None => self.known(defs, &local, &helper.body),
                }
            }
            Node::Op(id) if !is_context_op(t.name(t.arena().op(id).op)) => {
                let call = t.arena().op(id);
                let name = t.name(call.op);
                let mut values = Vec::with_capacity(t.arena().fields(call.args).len());
                for field in t.arena().fields(call.args) {
                    values.push((t.name(field.name).to_owned(), self.known(defs, bindings, &t.child(field.term))?));
                }
                let found = op::find(operators::all(), name, &context::value())?;
                (found.check)(&values).ok()?;
                let frame = input::at_time(0.0);
                let exit = |_: &str, _: &op::Args| -> Result<Value, Diagnostic> { Err(flow_syntax::value::fail("E_EXIT", "exit")) };
                (found.body)(&frame, &exit, &values).ok()
            }
            _ => None,
        }
    }

    fn scan(&self, defs: &[Graph], bindings: &[(String, Value)], body: &Term, graph: &Graph) {
        match body.node {
            Node::If(id) => {
                let branch = body.arena().if_node(id);
                match self.known(defs, bindings, &body.child(branch.test)) {
                    Some(Value::Bool(true)) => self.scan(defs, bindings, &body.child(branch.yes), graph),
                    Some(Value::Bool(false)) => self.scan(defs, bindings, &body.child(branch.no), graph),
                    _ => for &id in body.children() { self.scan(defs, bindings, &body.child(id), graph); },
                }
                return;
            }
            Node::Cond(id) => {
                let cond = body.arena().cond(id);
                for arm in body.arena().arms(cond.arms) {
                    match self.known(defs, bindings, &body.child(arm.test)) {
                        Some(Value::Bool(false)) => {}
                        Some(Value::Bool(true)) => { self.scan(defs, bindings, &body.child(arm.body), graph); return; }
                        _ => {
                            self.scan(defs, bindings, &body.child(arm.test), graph);
                            self.scan(defs, bindings, &body.child(arm.body), graph);
                        }
                    }
                }
                self.scan(defs, bindings, &body.child(cond.default), graph);
                return;
            }
            Node::CallFn(id) => {
                let call = body.arena().call_fn(id);
                if let Some(helper) = defs.iter().find(|d| d.name == body.name(call.name)) {
                    let local: Bindings = local_inputs(self, defs, helper, body, body.arena().term_ids(call.args), bindings).into_iter().flatten().collect();
                    match call.body {
                        Some(id) => self.scan(defs, &local, &body.child(id), graph),
                        None => self.scan(defs, &local, &helper.body, graph),
                    }
                } else if let Some(id) = call.body { self.scan(defs, &[], &body.child(id), graph); }
                for &id in body.arena().term_ids(call.args) { self.scan(defs, bindings, &body.child(id), graph); }
                return;
            }
            Node::Fn(id) => {
                let function = body.arena().function(id);
                let mut local = bindings.to_vec();
                for parameter in body.arena().params(function.params) {
                    local = remove_pattern(body, parameter.pattern, &local);
                }
                self.scan(defs, &local, &body.child(function.body), graph);
                if let Some(id) = function.capture { self.scan(defs, bindings, &body.child(id), graph); }
                return;
            }
            Node::FnRef(name) => {
                if let Some(helper) = defs.iter().find(|d| d.name == body.name(name)) { self.scan(defs, &[], &helper.body, graph); }
            }
            Node::Let(id) => {
                let let_node = body.arena().let_node(id);
                let mut local = bindings.to_vec();
                for binding in body.arena().bindings(let_node.bindings) {
                    let value = body.child(binding.term);
                    self.scan(defs, &local, &value, graph);
                    let known = self.known(defs, &local, &value);
                    local = remove_pattern(body, binding.pattern, &local);
                    if let (PatternNode::Name(name), Some(value)) = (body.arena().pattern(binding.pattern), known) {
                        local.insert(0, (body.name(name).to_owned(), value));
                    }
                }
                self.scan(defs, &local, &body.child(let_node.body), graph);
                return;
            }
            Node::State(id) => {
                let state = body.arena().state(id);
                self.scan(defs, bindings, &body.child(state.init), graph);
                self.scan(defs, &remove_pattern(body, state.binder, bindings), &body.child(state.step), graph);
                return;
            }
            Node::Loop(id) => {
                let node = body.arena().loop_node(id);
                let mut local = bindings.to_vec();
                for binding in body.arena().bindings(node.accs) { self.scan(defs, bindings, &body.child(binding.term), graph); }
                for binding in body.arena().bindings(node.accs) { local = remove_pattern(body, binding.pattern, &local); }
                for binding in body.arena().bindings(node.clauses) {
                    self.scan(defs, &local, &body.child(binding.term), graph);
                    local = remove_pattern(body, binding.pattern, &local);
                }
                self.scan(defs, &local, &body.child(node.body), graph);
                return;
            }
            _ => {}
        }
        for &id in body.children() { self.scan(defs, bindings, &body.child(id), graph); }
        if let Node::Op(id) = body.node {
            let name = body.name(body.arena().op(id).op);
            if is_context_op(name) && !self.found.borrow().iter().any(|found| found == name) {
                self.found.borrow_mut().push(name.to_owned());
            }
        }
    }
}

/// The context operators a reachable branch names; `depends` is membership.
pub fn context_ops_reached(inputs: &[(String, Value)], defs: &[Graph], graph: &Graph) -> Vec<String> {
    let defaults: Bindings = graph
        .inputs
        .iter()
        .filter_map(|(name, _, default)| default.as_ref().and_then(literal).map(|v| (name.clone(), v)))
        .collect();
    let scan = Scan { found: RefCell::new(Vec::new()), fuel: Cell::new(4096) };
    let bindings: Bindings = inputs.iter().cloned().chain(defaults).collect();
    scan.scan(defs, &bindings, &graph.body, graph);
    for (_, _, default) in &graph.inputs {
        if let Some(default) = default {
            scan.scan(defs, &bindings, default, graph);
        }
    }
    // OCaml conses: the last name found comes first
    let mut found = scan.found.into_inner();
    found.reverse();
    found
}

/// Whether a reachable, unpruned call of the zero-state operator `op` (default `sop/context_seed`)
/// remains.
pub fn depends(op: Option<&str>, inputs: &[(String, Value)], defs: &[Graph], graph: &Graph) -> bool {
    let op = op.unwrap_or("sop/context_seed");
    context_ops_reached(inputs, defs, graph).iter().any(|name| name == op)
}

/// The graph with its context operators replaced by the context's values. A seed without a literal
/// namespace is OCaml's `Invalid_argument "E_SEED_NAMESPACE: ..."`, here its message as `Err`.
pub fn specialize(provenance: Option<i64>, context: &sop::context::T, label: &str, graph: &Graph) -> Result<Graph, String> {
    let provenance = provenance.unwrap_or(-1);
    let refused: RefCell<Option<String>> = RefCell::new(None);
    let seed = sop::context::seed(context);
    let mut builder = TermBuilder::new();
    let mut transform = |builder: &mut TermBuilder, _: &Term, draft: Draft| -> Draft {
        let Draft::Op { op: name, args, skip } = draft else { return draft };
        let value = match name.as_str() {
            "sop/provenance_base" => Some(param::Value::Int(provenance)),
            "sop/context_frame" => Some(param::Value::Float(sop::context::frame(context) as f64)),
            "sop/context_time" => Some(param::Value::Float(sop::context::time(context))),
            "sop/context_seed" => {
                let namespace = |id: TermId| match builder.node(id) {
                    Node::Text(bytes) => Some(String::from_utf8_lossy(builder.raw_bytes(bytes)).into_owned()),
                    _ => None,
                };
                let salted = match args.as_slice() {
                    [(a, ns)] if a == "namespace" => namespace(*ns).map(|ns| (ns, 0)),
                    [(a, ns), (b, salt)] if a == "namespace" && b == "salt" => match (namespace(*ns), builder.node(*salt)) {
                        (Some(ns), Node::Lit(id)) => match builder.literal(id) {
                            Literal::Int(salt) => Some((ns, salt)),
                            _ => None,
                        },
                        _ => None,
                    },
                    _ => None,
                };
                match salted {
                    Some((namespace, salt)) => Some(param::Value::Int(operators::seed_for(salt, seed, &namespace, label))),
                    None => {
                        *refused.borrow_mut() = Some("E_SEED_NAMESPACE: Context seed requires a literal namespace.".to_owned());
                        None
                    }
                }
            }
            _ => None,
        };
        match value { Some(value) => Draft::Lit(value), None => Draft::Op { op: name, args, skip } }
    };
    let roots: Vec<_> = std::iter::once(graph.body.clone()).chain(graph.inputs.iter().filter_map(|(_, _, default)| default.clone())).collect();
    let ids = builder.rewrite_roots(&roots, &mut transform);
    let form = builder.import_form(&graph.form);
    let arena = builder.freeze();
    let mut defaults = ids[1..].iter();
    let mapped = Graph {
        body: arena.handle(ids[0]),
        inputs: graph.inputs.iter().map(|(name, ty, default)| (name.clone(), ty.clone(), default.as_ref().map(|_| arena.handle(*defaults.next().unwrap())))).collect(),
        form: arena.syntax().handle(form),
        ..graph.clone()
    };
    match refused.into_inner() {
        Some(message) => Err(message),
        None => Ok(mapped),
    }
}

// ---------------------------------------------------------------------------------------------
// source digests

struct Walk {
    nodes: HashSet<String>,
    defs: HashSet<String>,
    graphs: HashSet<String>,
    macros: HashSet<String>,
    expanded: Vec<Syntax>,
}

fn library(form: &Syntax) -> bool {
    form.id >= syntax::LIBRARY_BASE
}

fn walk_node(ws: &Workspace, st: &RefCell<Walk>, definition: &NodeDefinition) {
    let name = definition.kind.qualified.clone();
    if !st.borrow_mut().nodes.insert(name) {
        return;
    }
    if !library(&definition.body.form) {
        walk_graph(ws, st, &definition.body);
        for (t, _, _) in &definition.validations {
            walk_term(ws, st, definition, t);
        }
        for (_, t) in &definition.normalizers {
            walk_term(ws, st, definition, t);
        }
    }
}

fn walk_definition(ws: &Workspace, st: &RefCell<Walk>, graphs: bool, g: &Graph) {
    let added = {
        let mut st = st.borrow_mut();
        if graphs { st.graphs.insert(g.name.clone()) } else { st.defs.insert(g.name.clone()) }
    };
    if added && !library(&g.form) {
        walk_graph(ws, st, g);
    }
}

fn walk_named(ws: &Workspace, st: &RefCell<Walk>, name: &str) {
    if let Some(def) = ws.defs.iter().find(|g| g.name == name) {
        walk_definition(ws, st, false, def);
    }
    if let Some(node) = ws.nodes.iter().find(|d| d.kind.qualified == name) {
        walk_node(ws, st, node);
    }
}

fn macro_name(form: &Syntax) -> Option<&str> {
    form.children().get(1).and_then(|&id| match form.arena().row(id).node {
        SNode::Sym(selected) => Some(form.name(selected)),
        _ => None,
    })
}

fn walk_macro(ws: &Workspace, st: &RefCell<Walk>, name: &str) {
    if st.borrow().macros.contains(name) {
        return;
    }
    if let Some(form) = ws.macros.iter().find(|form| macro_name(form) == Some(name)) {
        st.borrow_mut().macros.insert(name.to_owned());
        if !library(form) {
            walk_syntax(ws, st, form);
        }
    }
}

fn walk_syntax(ws: &Workspace, st: &RefCell<Walk>, form: &Syntax) {
    // Macro.expand erases inner Expanded names. Follow their template heads.
    if let Some(head) = syntax::head(form) {
        walk_macro(ws, st, head);
    }
    for &child in form.children() {
        walk_syntax(ws, st, &form.child(child));
    }
}

fn walk_graph(ws: &Workspace, st: &RefCell<Walk>, g: &Graph) {
    fn visit(ws: &Workspace, st: &RefCell<Walk>, t: &Term) {
        // a helper call's body is its definition's, walked once by name
        if let Node::CallFn(id) = t.node {
            for &child in t.arena().term_ids(t.arena().call_fn(id).args) { visit(ws, st, &t.child(child)); }
        } else {
            for &child in t.children() { visit(ws, st, &t.child(child)); }
        }
        match t.node {
            Node::Call(id) => walk_named(ws, st, t.name(t.arena().call(id).kind)),
            Node::CallFn(id) => walk_named(ws, st, t.name(t.arena().call_fn(id).name)),
            Node::FnRef(id) => {
                let name = t.name(id);
                walk_named(ws, st, name);
                if let Some((_, (qualified, _, _))) = ws.kind_fns.iter().find(|(n, _)| n == name) { walk_named(ws, st, qualified); }
            }
            Node::GraphRef(id) => {
                let name = t.name(t.arena().graph_ref(id).graph);
                if let Some(def) = ws.graphs.iter().find(|d| d.name == name) { walk_definition(ws, st, true, def); }
                walk_named(ws, st, name);
            }
            Node::Expanded(id) => {
                let expanded = t.arena().expanded(id);
                walk_macro(ws, st, t.name(expanded.name));
                st.borrow_mut().expanded.push(t.child(expanded.body).form());
            }
            _ => {}
        }
    }
    visit(ws, st, &g.body);
    for default in g.inputs.iter().filter_map(|(_, _, default)| default.as_ref()) { visit(ws, st, default); }
}

fn walk_term(ws: &Workspace, st: &RefCell<Walk>, definition: &NodeDefinition, t: &Term) {
    walk_graph(ws, st, &Graph { body: t.clone(), inputs: Vec::new(), ..definition.body.clone() });
}

/// Forms reachable from the start: `leading`, then other reached node bodies, helpers, graphs and
/// macros, then dispatched macro expansions. `skip` names a node whose body is already in `leading`.
/// A library definition is not walked: what it reaches is fixed with it.
fn scoped_forms(ws: &Workspace, leading: Vec<Syntax>, skip: &str, root: Option<&NodeDefinition>) -> Vec<Syntax> {
    let st = RefCell::new(Walk { nodes: HashSet::new(), defs: HashSet::new(), graphs: HashSet::new(), macros: HashSet::new(), expanded: Vec::new() });
    match root {
        Some(definition) => walk_node(ws, &st, definition),
        None => {
            for g in ws.graphs.iter() {
                walk_graph(ws, &st, g);
            }
        }
    }
    let st = st.into_inner();
    let mut forms = leading;
    forms.extend(ws.nodes.iter().filter(|d| d.kind.qualified != skip && st.nodes.contains(&d.kind.qualified)).map(|d| d.body.form.clone()));
    forms.extend(ws.defs.iter().filter(|g| st.defs.contains(&g.name)).map(|g| g.form.clone()));
    forms.extend(ws.graphs.iter().filter(|g| st.graphs.contains(&g.name)).map(|g| g.form.clone()));
    forms.extend(ws.macros.iter().filter(|form| macro_name(form).is_some_and(|name| st.macros.contains(name))).cloned());
    forms.extend(st.expanded);
    forms
}

fn put(buffer: &mut Vec<u8>, s: &[u8]) {
    buffer.extend_from_slice(s.len().to_string().as_bytes());
    buffer.push(b':');
    buffer.extend_from_slice(s);
}
fn put_all(buffer: &mut Vec<u8>, l: &[String]) {
    buffer.push(b'[');
    for s in l {
        put(buffer, s.as_bytes());
    }
    buffer.push(b']');
}
fn put_atom(buffer: &mut Vec<u8>, c: u8, s: &[u8]) {
    buffer.push(c);
    put(buffer, s);
}
fn put_form(buffer: &mut Vec<u8>, x: &Syntax) {
    if library(x) {
        put_atom(buffer, b'L', x.id.to_string().as_bytes());
    } else {
        put_library_free(buffer, x);
    }
}
fn put_library_free(buffer: &mut Vec<u8>, x: &Syntax) {
    if !x.notes().is_empty() {
        put_all(buffer, &x.notes());
    }
    if !x.meta().is_empty() {
        put_all(buffer, &x.meta());
    }
    match &x.node {
        SNode::Sym(s) => put_atom(buffer, b's', x.name(*s).as_bytes()),
        SNode::Kw(s) => put_atom(buffer, b'k', x.name(*s).as_bytes()),
        SNode::Num(s) => put_atom(buffer, b'n', x.name(*s).as_bytes()),
        SNode::Str(s) => put_atom(buffer, b't', x.bytes(*s)),
        SNode::List(l) => {
            buffer.push(b'(');
            x.range(*l).iter().for_each(|&id| put_form(buffer, &x.child(id)));
        }
        SNode::Vec(l) => {
            buffer.push(b'<');
            x.range(*l).iter().for_each(|&id| put_form(buffer, &x.child(id)));
        }
        SNode::Map(l) => {
            buffer.push(b'{');
            x.range(*l).iter().for_each(|&id| put_form(buffer, &x.child(id)));
        }
        SNode::Quote(kind, inner) => {
            buffer.push(b'q');
            buffer.push(match kind {
                QuoteKind::Plain => b'p',
                QuoteKind::Quasi => b'q',
                QuoteKind::Unquote => b'u',
                QuoteKind::Splice => b's',
            });
            put_form(buffer, &x.child(*inner));
        }
    }
    if !x.tail().is_empty() {
        put_all(buffer, &x.tail());
    }
    buffer.push(b')');
}

/// Cache key: length-prefixed dump of spelling, comments and flags, no ids or spans; a library form
/// is its id, fixed with the binary. The MD5 is returned as lowercase hex (OCaml: the raw 16 bytes).
fn forms_digest(forms: &[Syntax]) -> String {
    let mut buffer: Vec<u8> = Vec::with_capacity(4096);
    for f in forms {
        put_form(&mut buffer, f);
    }
    md5_hex(&buffer)
}

/// Own declaration and transitively referenced checked declarations/helpers.
pub fn declaration_source(checked: &Workspace, root: &NodeDefinition) -> String {
    forms_digest(&scoped_forms(checked, vec![root.body.form.clone()], &root.kind.qualified, Some(root)))
}

/// Authored source plus the declarations the checked workspace's graphs reach: the key of prepared
/// kernels, instead of the whole library.
pub fn kernel_source(checked: &Workspace) -> String {
    let leading: Vec<Syntax> = checked.graphs.iter().map(|g| g.form.clone()).collect();
    let mut forms = (*checked.source).clone();
    forms.extend(scoped_forms(checked, leading, "", None));
    forms_digest(&forms)
}

/// Whether `specialize` can change the graph: it names a context or provenance operator.
pub fn uses_context(graph: &Graph) -> bool {
    fn term(t: &Term) -> bool {
        matches!(t.node, Node::Op(id) if is_context_op(t.name(t.arena().op(id).op)))
            || t.children().iter().any(|&id| term(&t.child(id)))
    }
    term(&graph.body) || graph.inputs.iter().filter_map(|(_, _, t)| t.as_ref()).any(term)
}

fn visit_reachable(workspace: &Workspace, seen: &RefCell<HashSet<String>>, body: &Term, inputs: &[(String, Ty, Option<Term>)]) {
    fn collect<'a>(arena: &'a flow_check::term_arena::TermArena, id: TermId, names: &mut Vec<&'a str>) {
        for &child in arena.children(id) { collect(arena, child, names); }
        match arena.row(id).node {
            Node::CallFn(id) => names.push(arena.name(arena.call_fn(id).name)),
            Node::Call(id) => names.push(arena.name(arena.call(id).kind)),
            Node::FnRef(name) => names.push(arena.name(name)),
            Node::GraphRef(id) => {
                if let Some(name) = arena.name(arena.graph_ref(id).graph).strip_prefix("def:") { names.push(name); }
            }
            _ => {}
        }
    }
    let mut found = Vec::new();
    collect(body.arena(), body.row_id(), &mut found);
    for default in inputs.iter().filter_map(|(_, _, t)| t.as_ref()) { collect(default.arena(), default.row_id(), &mut found); }
    for name in found.into_iter().rev() {
        if !seen.borrow_mut().insert(name.to_owned()) {
            continue;
        }
        for d in workspace.defs.iter().filter(|d| d.name == name) {
            visit_reachable(workspace, seen, &d.body, &d.inputs);
        }
        for n in workspace.nodes.iter().filter(|n| n.kind.qualified == name) {
            visit_reachable(workspace, seen, &n.body.body, &n.body.inputs);
            let terms = n.validations.iter().map(|(t, _, _)| t).chain(n.normalizers.iter().map(|(_, t)| t));
            for term in terms {
                visit_reachable(workspace, seen, term, &[]);
            }
        }
    }
}

/// The checked helpers the graphs reach, directly or through the Lisp nodes they call, in
/// declaration order.
pub fn reachable_defs<'a, 'g>(workspace: &'a Workspace, roots: impl IntoIterator<Item = &'g Graph>) -> Vec<&'a Graph> {
    let seen: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
    for root in roots {
        visit_reachable(workspace, &seen, &root.body, &root.inputs);
    }
    let seen = seen.into_inner();
    workspace.defs.iter().filter(|d| seen.contains(&d.name)).collect()
}

/// An inlined body reads its cook context through inputs: each distinct context operator, with the
/// card's cook label, is the default of a reserved input the body refers to by name. It evaluates
/// to a cook-time source, which residuals capture as a binding, never as a term, so a kernel
/// specializes it per context by its captures alone. Returns the graph and each input's operator.
pub fn label_context(label: &str, graph: &Graph) -> (Graph, Vec<(String, String)>) {
    fn eligible(t: &Term, into: &mut HashSet<(u64, u32)>) -> bool {
        if !into.insert(t.identity()) { return false; }
        let mut context = matches!(t.node, Node::Op(id) if is_context_op(t.name(t.arena().op(id).op)));
        if let Node::CallFn(id) = t.node {
            for &id in t.arena().term_ids(t.arena().call_fn(id).args) { context |= eligible(&t.child(id), into); }
        } else {
            for &id in t.children() { context |= eligible(&t.child(id), into); }
        }
        context
    }
    let roots: Vec<_> = std::iter::once(graph.body.clone()).chain(graph.inputs.iter().filter_map(|(_, _, default)| default.clone())).collect();
    let mut selected = HashSet::new();
    let mut context = false;
    for root in &roots { context |= eligible(root, &mut selected); }
    if !context { return (graph.clone(), Vec::new()); }
    let mut inputs: Vec<(String, String, String, TermId)> = Vec::new();
    let mut builder = TermBuilder::new();
    let ids = builder.rewrite_roots(&roots, &mut |builder, source, draft| {
        if !selected.contains(&source.identity()) { return draft; }
        let Draft::Op { op: name, args, skip } = draft else { return draft };
        if !is_context_op(&name) { return Draft::Op { op: name, args, skip }; }
        let mut parts = vec![name.clone()];
        parts.extend(args.iter().map(|(name, id)| match builder.node(*id) {
            Node::Text(bytes) => format!("{name}={}", String::from_utf8_lossy(builder.raw_bytes(bytes))),
            Node::Lit(id) => match builder.literal(id) {
                Literal::Int(n) => format!("{name}={n}"),
                _ => format!("{name}=?"),
            },
            _ => format!("{name}=?"),
        }));
        let key = parts.join(";");
        let input = match inputs.iter().find(|(k, ..)| k == &key) {
            Some((_, input, ..)) => input.clone(),
            None => {
                let input = format!("$context{}", inputs.len());
                let form = builder.import_form(&source.form());
                let text = builder.push(None, &Ty::Text, form, Draft::Text(label.as_bytes().to_vec()));
                let mut args = args;
                args.push((operators::context_label().to_owned(), text));
                let path: Option<Vec<_>> = source.path().map(|ids| ids.iter().map(|&id| source.name(id).to_owned()).collect());
                let call = builder.push(path.as_deref(), source.ty(), form, Draft::Op { op: name.clone(), args, skip });
                inputs.push((key, input.clone(), name, call));
                input
            }
        };
        Draft::RefBinding(input, Vec::new())
    });
    let form = builder.import_form(&graph.form);
    let arena = builder.freeze();
    let mut defaults = ids[1..].iter();
    let mut graph = Graph {
        body: arena.handle(ids[0]),
        inputs: graph.inputs.iter().map(|(name, ty, default)| (name.clone(), ty.clone(), default.as_ref().map(|_| arena.handle(*defaults.next().unwrap())))).collect(),
        form: arena.syntax().handle(form),
        ..graph.clone()
    };
    graph.inputs.extend(inputs.iter().map(|(_, name, _, call)| (name.clone(), arena.handle(*call).ty().clone(), Some(arena.handle(*call)))));
    let ops = inputs.into_iter().map(|(_, name, op, _)| (name, op)).collect();
    (graph, ops)
}

fn md5_hex(input: &[u8]) -> String {
    const S: [u32; 64] = [7,12,17,22,7,12,17,22,7,12,17,22,7,12,17,22,5,9,14,20,5,9,14,20,5,9,14,20,5,9,14,20,4,11,16,23,4,11,16,23,4,11,16,23,4,11,16,23,6,10,15,21,6,10,15,21,6,10,15,21,6,10,15,21];
    const K: [u32; 64] = [0xd76aa478,0xe8c7b756,0x242070db,0xc1bdceee,0xf57c0faf,0x4787c62a,0xa8304613,0xfd469501,0x698098d8,0x8b44f7af,0xffff5bb1,0x895cd7be,0x6b901122,0xfd987193,0xa679438e,0x49b40821,0xf61e2562,0xc040b340,0x265e5a51,0xe9b6c7aa,0xd62f105d,0x02441453,0xd8a1e681,0xe7d3fbc8,0x21e1cde6,0xc33707d6,0xf4d50d87,0x455a14ed,0xa9e3e905,0xfcefa3f8,0x676f02d9,0x8d2a4c8a,0xfffa3942,0x8771f681,0x6d9d6122,0xfde5380c,0xa4beea44,0x4bdecfa9,0xf6bb4b60,0xbebfbc70,0x289b7ec6,0xeaa127fa,0xd4ef3085,0x04881d05,0xd9d4d039,0xe6db99e5,0x1fa27cf8,0xc4ac5665,0xf4292244,0x432aff97,0xab9423a7,0xfc93a039,0x655b59c3,0x8f0ccc92,0xffeff47d,0x85845dd1,0x6fa87e4f,0xfe2ce6e0,0xa3014314,0x4e0811a1,0xf7537e82,0xbd3af235,0x2ad7d2bb,0xeb86d391];
    let mut data = input.to_vec(); let bit_len = (data.len() as u64).wrapping_mul(8); data.push(0x80);
    while data.len() % 64 != 56 { data.push(0); }
    data.extend_from_slice(&bit_len.to_le_bytes());
    let (mut a0, mut b0, mut c0, mut d0) = (0x67452301u32, 0xefcdab89u32, 0x98badcfeu32, 0x10325476u32);
    for chunk in data.chunks_exact(64) {
        let mut words = [0u32; 16];
        for (index, bytes) in chunk.chunks_exact(4).enumerate() { words[index] = u32::from_le_bytes([bytes[0],bytes[1],bytes[2],bytes[3]]); }
        let (mut a, mut b, mut c, mut d) = (a0,b0,c0,d0);
        for i in 0..64 {
            let (f, g) = if i < 16 { ((b & c) | (!b & d), i) } else if i < 32 { ((d & b) | (!d & c), (5*i+1)%16) } else if i < 48 { (b ^ c ^ d, (3*i+5)%16) } else { (c ^ (b | !d), (7*i)%16) };
            let next = b.wrapping_add(a.wrapping_add(f).wrapping_add(K[i]).wrapping_add(words[g]).rotate_left(S[i]));
            (a,b,c,d) = (d,next,b,c);
        }
        a0=a0.wrapping_add(a); b0=b0.wrapping_add(b); c0=c0.wrapping_add(c); d0=d0.wrapping_add(d);
    }
    let mut output = String::with_capacity(32);
    const HEX: &[u8; 16] = b"0123456789abcdef";
    for word in [a0, b0, c0, d0] {
        for byte in word.to_le_bytes() {
            output.push(HEX[(byte >> 4) as usize] as char);
            output.push(HEX[(byte & 0xf) as usize] as char);
        }
    }
    output
}

