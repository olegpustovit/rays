//! Port of lib/flow_sop/context_kernel.ml(i): cook-context constants available to Lisp node bodies
//! (`sop/context_seed` and friends), and the source digests that key prepared kernels.
use crate::operators;
use flow_check::op;
use flow_check::workspace::{self as w, Graph, Node, NodeDefinition, Pattern, Term, Workspace};
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

/// Rewrite every term bottom-up: `select` may replace a term first (its replacement is then
/// walked), `transform` sees each rebuilt term. Checked terms are trees here, so no sharing memo
/// is needed. `helpers` false leaves a helper call's inlined body alone.
fn term_once(select: &dyn Fn(&Term) -> Option<Term>, transform: &dyn Fn(Term) -> Term, helpers: bool, t: &Term) -> Term {
    let owned;
    let t = match select(t) { Some(replaced) => { owned = replaced; &owned } None => t };
    let walk = |child: &Term| term_once(select, transform, helpers, child);
    let rebuilt = match &t.node {
        Node::CallFn { fn_name, args, body } if !helpers => Term {
            node: Node::CallFn { fn_name: fn_name.clone(), args: args.iter().map(&walk).collect(), body: body.clone() },
            ..t.clone()
        },
        _ => w::map_children(&walk, t),
    };
    transform(rebuilt)
}

fn map(select: &dyn Fn(&Term) -> Option<Term>, helpers: bool, transform: &dyn Fn(Term) -> Term, graph: &Graph) -> Graph {
    let body = term_once(select, transform, helpers, &graph.body);
    let inputs = graph
        .inputs
        .iter()
        .map(|(name, ty, default)| (name.clone(), ty.clone(), default.as_ref().map(|d| term_once(select, transform, helpers, d))))
        .collect();
    Graph { body, inputs, ..graph.clone() }
}

fn keep(_: &Term) -> Option<Term> {
    None
}

fn pattern_names(pattern: &Pattern) -> Vec<String> {
    match pattern {
        Pattern::Name(name) => vec![name.clone()],
        Pattern::Seq(patterns) => patterns.iter().flat_map(pattern_names).collect(),
        Pattern::Keys(names) => names.clone(),
    }
}

type Bindings = Vec<(String, Value)>;

fn remove(patterns: &[Pattern], bindings: &[(String, Value)]) -> Bindings {
    let bound: Vec<String> = patterns.iter().flat_map(pattern_names).collect();
    bindings.iter().filter(|(name, _)| !bound.contains(name)).cloned().collect()
}

struct Scan {
    defs: Vec<Graph>,
    found: RefCell<Vec<String>>,
    fuel: Cell<i64>,
}

fn literal(t: &Term) -> Option<Value> {
    match &t.node {
        Node::Lit(param::Value::Bool(b)) => Some(Value::Bool(*b)),
        Node::Lit(param::Value::Int(n)) => Some(Value::Int(*n)),
        Node::Lit(param::Value::Float(f)) => Some(Value::Float(*f)),
        Node::Text(text) => Some(Value::Text(text.clone())),
        _ => None,
    }
}

/// The values a helper's inputs take over known arguments (`None` entries for unknown ones).
fn local_inputs(scan: &Scan, helper: &Graph, args: &[Term], bindings: &[(String, Value)]) -> Vec<Option<(String, Value)>> {
    helper
        .inputs
        .iter()
        .enumerate()
        .map(|(i, (name, _, default))| {
            let value = match args.get(i) {
                Some(argument) => scan.known(bindings, argument),
                None => default.as_ref().and_then(|d| scan.known(&[], d)),
            };
            value.map(|value| (name.clone(), value))
        })
        .collect()
}

impl Scan {
    fn known(&self, bindings: &[(String, Value)], t: &Term) -> Option<Value> {
        if let Some(value) = literal(t) {
            return Some(value);
        }
        match &t.node {
            Node::RefBinding(name, fields) if fields.is_empty() => bindings.iter().find(|(n, _)| n == name).map(|(_, v)| v.clone()),
            Node::CallFn { fn_name, args, body } if self.fuel.get() > 0 => {
                // a helper over known arguments (a text predicate, say) decides its own branches
                self.fuel.set(self.fuel.get() - 1);
                let helper = self.defs.iter().find(|d| &d.name == fn_name)?;
                let local = local_inputs(self, helper, args, bindings);
                if local.iter().any(Option::is_none) {
                    return None;
                }
                let local: Bindings = local.into_iter().flatten().collect();
                self.known(&local, body.as_deref().unwrap_or(&helper.body))
            }
            Node::Op { op: name, args, .. } if !is_context_op(name) => {
                let mut values = Vec::with_capacity(args.len());
                for (key, value) in args {
                    values.push((key.clone(), self.known(bindings, value)?));
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

    fn scan(&self, bindings: &[(String, Value)], body: &Term, graph: &Graph) {
        let select_full = |t: &Term| -> Term {
            match &t.node {
                Node::If(test, yes, no) => match self.known(bindings, test) {
                    Some(Value::Bool(true)) => (**yes).clone(),
                    Some(Value::Bool(false)) => (**no).clone(),
                    _ => t.clone(),
                },
                Node::Cond(arms, default) => {
                    // An arm whose test is known false never runs; the first known-true arm is the result.
                    let mut live: Vec<(Term, Term)> = Vec::new();
                    let mut taken: Option<Term> = None;
                    for (test, yes) in arms {
                        match self.known(bindings, test) {
                            Some(Value::Bool(false)) => {}
                            Some(Value::Bool(true)) => {
                                taken = Some(yes.clone());
                                break;
                            }
                            _ => live.push((test.clone(), yes.clone())),
                        }
                    }
                    match (live.is_empty(), taken) {
                        (true, Some(yes)) => yes,
                        (true, None) => (**default).clone(),
                        (false, Some(yes)) => Term { node: Node::Cond(live, Box::new(yes)), ..t.clone() },
                        (false, None) => Term { node: Node::Cond(live, default.clone()), ..t.clone() },
                    }
                }
                Node::CallFn { fn_name, args, body } => {
                    match self.defs.iter().find(|d| &d.name == fn_name) {
                        Some(helper) => {
                            let local: Bindings = local_inputs(self, helper, args, bindings).into_iter().flatten().collect();
                            self.scan(&local, body.as_deref().unwrap_or(&helper.body), graph);
                        }
                        None => {
                            if let Some(inlined) = body {
                                self.scan(&[], inlined, graph);
                            }
                        }
                    }
                    Term { node: Node::CallFn { fn_name: fn_name.clone(), args: args.clone(), body: None }, ..t.clone() }
                }
                Node::Fn { params, body, zone, capture } => {
                    let names: Vec<Pattern> = params.iter().map(|(p, _)| p.clone()).collect();
                    self.scan(&remove(&names, bindings), body, graph);
                    Term {
                        node: Node::Fn {
                            params: params.clone(),
                            body: Box::new(Term { node: Node::Nil, ..(**body).clone() }),
                            zone: zone.clone(),
                            capture: capture.clone(),
                        },
                        ..t.clone()
                    }
                }
                Node::FnRef(name) => {
                    if let Some(helper) = self.defs.iter().find(|d| &d.name == name) {
                        self.scan(&[], &helper.body, graph);
                    }
                    t.clone()
                }
                Node::Let(locals, inner) => {
                    let mut bindings: Bindings = bindings.to_vec();
                    for (pattern, value) in locals {
                        self.scan(&bindings, value, graph);
                        let local = match (pattern, self.known(&bindings, value)) {
                            (Pattern::Name(name), Some(value)) => vec![(name.clone(), value)],
                            _ => Vec::new(),
                        };
                        bindings = local.into_iter().chain(remove(std::slice::from_ref(pattern), &bindings)).collect();
                    }
                    self.scan(&bindings, inner, graph);
                    Term { node: Node::Nil, ..t.clone() }
                }
                Node::State { binder, init, step, .. } => {
                    self.scan(bindings, init, graph);
                    self.scan(&remove(std::slice::from_ref(binder), bindings), step, graph);
                    Term { node: Node::Nil, ..t.clone() }
                }
                Node::Loop { accs, clauses, body, .. } => {
                    for (_, value) in accs {
                        self.scan(bindings, value, graph);
                    }
                    let acc_patterns: Vec<Pattern> = accs.iter().map(|(p, _)| p.clone()).collect();
                    let mut bindings = remove(&acc_patterns, bindings);
                    for (pattern, value) in clauses {
                        self.scan(&bindings, value, graph);
                        bindings = remove(std::slice::from_ref(pattern), &bindings);
                    }
                    self.scan(&bindings, body, graph);
                    Term { node: Node::Nil, ..t.clone() }
                }
                _ => t.clone(),
            }
        };
        let record = |t: Term| -> Term {
            if let Node::Op { op: name, .. } = &t.node {
                if is_context_op(name) && !self.found.borrow().contains(name) {
                    self.found.borrow_mut().push(name.clone());
                }
            }
            t
        };
        let select = |t: &Term| Some(select_full(t));
        map(&select, true, &record, &Graph { body: body.clone(), inputs: Vec::new(), ..graph.clone() });
    }
}

/// The context operators a reachable branch names; `depends` is membership.
pub fn context_ops_reached(inputs: &[(String, Value)], defs: &[Graph], graph: &Graph) -> Vec<String> {
    let defaults: Bindings = graph
        .inputs
        .iter()
        .filter_map(|(name, _, default)| default.as_ref().and_then(literal).map(|v| (name.clone(), v)))
        .collect();
    let scan = Scan { defs: defs.to_vec(), found: RefCell::new(Vec::new()), fuel: Cell::new(4096) };
    let bindings: Bindings = inputs.iter().cloned().chain(defaults).collect();
    scan.scan(&bindings, &graph.body, graph);
    for (_, _, default) in &graph.inputs {
        if let Some(default) = default {
            scan.scan(&bindings, default, graph);
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
    let transform = |t: Term| -> Term {
        let Node::Op { op: name, args, .. } = &t.node else { return t };
        let lit = |node: param::Value| Term { node: Node::Lit(node), ..t.clone() };
        match name.as_str() {
            "sop/provenance_base" => lit(param::Value::Int(provenance)),
            "sop/context_frame" => lit(param::Value::Float(sop::context::frame(context) as f64)),
            "sop/context_time" => lit(param::Value::Float(sop::context::time(context))),
            "sop/context_seed" => {
                let namespace = |term: &Term| match &term.node {
                    Node::Text(bytes) => Some(String::from_utf8_lossy(bytes).into_owned()),
                    _ => None,
                };
                let salted = match args.as_slice() {
                    [(a, ns)] if a == "namespace" => namespace(ns).map(|ns| (ns, 0)),
                    [(a, ns), (b, salt)] if a == "namespace" && b == "salt" => match (namespace(ns), &salt.node) {
                        (Some(ns), Node::Lit(param::Value::Int(salt))) => Some((ns, *salt)),
                        _ => None,
                    },
                    _ => None,
                };
                match salted {
                    Some((namespace, salt)) => lit(param::Value::Int(operators::seed_for(salt, seed, &namespace, label))),
                    None => {
                        *refused.borrow_mut() = Some("E_SEED_NAMESPACE: Context seed requires a literal namespace.".to_owned());
                        t.clone()
                    }
                }
            }
            _ => t.clone(),
        }
    };
    let mapped = map(&keep, true, &transform, graph);
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
    match syntax::children(form).get(1).map(|c| &c.node) {
        Some(SNode::Sym(selected)) => Some(selected.as_str()),
        _ => None,
    }
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
    for child in syntax::children(form) {
        walk_syntax(ws, st, child);
    }
}

fn walk_graph(ws: &Workspace, st: &RefCell<Walk>, g: &Graph) {
    let transform = |t: Term| -> Term {
        match &t.node {
            Node::Call { kind, .. } => walk_named(ws, st, kind),
            Node::CallFn { fn_name, .. } => walk_named(ws, st, fn_name),
            Node::FnRef(name) => {
                walk_named(ws, st, name);
                if let Some((_, (qualified, _, _))) = ws.kind_fns.iter().find(|(n, _)| n == name) {
                    walk_named(ws, st, qualified);
                }
            }
            Node::GraphRef { graph, .. } => {
                if let Some(def) = ws.graphs.iter().find(|d| &d.name == graph) {
                    walk_definition(ws, st, true, def);
                }
                walk_named(ws, st, graph);
            }
            Node::Expanded { macro_name, body } => {
                walk_macro(ws, st, macro_name);
                // Parameterized macro heads disappear during recursive expansion. Hygiene counters
                // restart per Macro.expand, not per Syntax.id.
                st.borrow_mut().expanded.push((*body.form).clone());
            }
            _ => {}
        }
        t
    };
    // a helper call's body is its definition's, walked once by name
    map(&keep, false, &transform, g);
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
    if !x.notes.is_empty() {
        put_all(buffer, &x.notes);
    }
    if !x.meta.is_empty() {
        put_all(buffer, &x.meta);
    }
    match &x.node {
        SNode::Sym(s) => put_atom(buffer, b's', s.as_bytes()),
        SNode::Kw(s) => put_atom(buffer, b'k', s.as_bytes()),
        SNode::Num(s) => put_atom(buffer, b'n', s.as_bytes()),
        SNode::Str(s) => put_atom(buffer, b't', s),
        SNode::List(l) => {
            buffer.push(b'(');
            l.iter().for_each(|f| put_form(buffer, f));
        }
        SNode::Vec(l) => {
            buffer.push(b'<');
            l.iter().for_each(|f| put_form(buffer, f));
        }
        SNode::Map(l) => {
            buffer.push(b'{');
            l.iter().for_each(|f| put_form(buffer, f));
        }
        SNode::Quote(kind, inner) => {
            buffer.push(b'q');
            buffer.push(match kind {
                QuoteKind::Plain => b'p',
                QuoteKind::Quasi => b'q',
                QuoteKind::Unquote => b'u',
                QuoteKind::Splice => b's',
            });
            put_form(buffer, inner);
        }
    }
    if !x.tail.is_empty() {
        put_all(buffer, &x.tail);
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
    let found = Cell::new(false);
    let transform = |t: Term| -> Term {
        if let Node::Op { op: name, .. } = &t.node {
            if is_context_op(name) {
                found.set(true);
            }
        }
        t
    };
    map(&keep, true, &transform, graph);
    found.get()
}

fn visit_reachable(workspace: &Workspace, seen: &RefCell<HashSet<String>>, graph: &Graph) {
    let found: RefCell<Vec<String>> = RefCell::new(Vec::new());
    let transform = |t: Term| -> Term {
        match &t.node {
            Node::CallFn { fn_name, .. } => found.borrow_mut().push(fn_name.clone()),
            Node::Call { kind, .. } => found.borrow_mut().push(kind.clone()),
            Node::FnRef(name) => found.borrow_mut().push(name.clone()),
            Node::GraphRef { graph, .. } if graph.starts_with("def:") => found.borrow_mut().push(graph[4..].to_owned()),
            _ => {}
        }
        t
    };
    map(&keep, true, &transform, graph);
    let mut names = found.into_inner();
    names.reverse();
    for name in names {
        if !seen.borrow_mut().insert(name.clone()) {
            continue;
        }
        for d in workspace.defs.iter().filter(|d| d.name == name) {
            visit_reachable(workspace, seen, d);
        }
        for n in workspace.nodes.iter().filter(|n| n.kind.qualified == name) {
            visit_reachable(workspace, seen, &n.body);
            let terms = n.validations.iter().map(|(t, _, _)| t).chain(n.normalizers.iter().map(|(_, t)| t));
            for term in terms {
                visit_reachable(workspace, seen, &Graph { body: term.clone(), inputs: Vec::new(), ..n.body.clone() });
            }
        }
    }
}

/// The checked helpers the graphs reach, directly or through the Lisp nodes they call, in
/// declaration order.
pub fn reachable_defs(workspace: &Workspace, roots: &[Graph]) -> Vec<Graph> {
    let seen: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
    for root in roots {
        visit_reachable(workspace, &seen, root);
    }
    let seen = seen.into_inner();
    workspace.defs.iter().filter(|d| seen.contains(&d.name)).cloned().collect()
}

/// An inlined body reads its cook context through inputs: each distinct context operator, with the
/// card's cook label, is the default of a reserved input the body refers to by name. It evaluates
/// to a cook-time source, which residuals capture as a binding, never as a term, so a kernel
/// specializes it per context by its captures alone. Returns the graph and each input's operator.
pub fn label_context(label: &str, graph: &Graph) -> (Graph, Vec<(String, String)>) {
    let inputs: RefCell<Vec<(String, (String, Term))>> = RefCell::new(Vec::new());
    let key = |args: &[(String, Term)]| -> Vec<String> {
        args.iter()
            .map(|(name, a)| match &a.node {
                Node::Text(s) => format!("{name}={}", String::from_utf8_lossy(s)),
                Node::Lit(param::Value::Int(n)) => format!("{name}={n}"),
                _ => format!("{name}=?"),
            })
            .collect()
    };
    // a helper's body has its own scope: its operators read the card's label from the evaluation
    let transform = |t: Term| -> Term {
        let Node::Op { op: name, args, skip } = &t.node else { return t };
        if !is_context_op(name) {
            return t;
        }
        let mut parts = vec![name.clone()];
        parts.extend(key(args));
        let k = parts.join(";");
        let existing = inputs.borrow().iter().find(|(key, _)| *key == k).map(|(_, (n, _))| n.clone());
        let input = match existing {
            Some(input) => input,
            None => {
                let input = format!("$context{}", inputs.borrow().len());
                let text = Term { node: Node::Text(label.as_bytes().to_vec()), path: None, ty: Ty::Text, ..t.clone() };
                let mut with_label = args.clone();
                with_label.push((operators::context_label().to_owned(), text));
                let call = Term { node: Node::Op { op: name.clone(), args: with_label, skip: skip.clone() }, ..t.clone() };
                inputs.borrow_mut().push((k, (input.clone(), call)));
                input
            }
        };
        Term { node: Node::RefBinding(input, Vec::new()), ..t.clone() }
    };
    let mut graph = map(&keep, false, &transform, graph);
    let inputs = inputs.into_inner();
    graph.inputs.extend(inputs.iter().map(|(_, (name, call))| (name.clone(), call.ty.clone(), Some(call.clone()))));
    let ops = inputs
        .iter()
        .map(|(_, (name, call))| match &call.node {
            Node::Op { op, .. } => (name.clone(), op.clone()),
            _ => unreachable!("label_context stores only operator calls"),
        })
        .collect();
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
