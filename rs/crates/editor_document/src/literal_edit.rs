//! Port of lib/editor_document/literal_edit.ml: a literal edited in place. Same-type catalog
//! literals keep the checked graph's topology and liveness; the edit re-checks the workspace and
//! patches the lowering instead of lowering again.
//!
//! Gap: OCaml tells the recorded values apart by physical identity (`==`); Rust values have none,
//! so `lower_value` matches the recorded `Source`/`Struct` values structurally.
use crate::layout_by_path::Path;
use flow_check::catalog::{self as fc, Catalog};
use flow_check::workspace::{self as w, Graph, Node as TNode, Term, Workspace};
use flow_graph::flow_edit::{arg_text, ArgKey, Op};
use flow_sop::lower as l;
use flow_sop::network as net;
use flow_sop::port::Port;
use flow_syntax::lisp;
use flow_syntax::syntax::{self as s, Node, Syntax};
use flow_syntax::ty::Ty;
use flow_syntax::value::Value as E;
use rays_diagnostic::{Diagnostic, Severity, Span};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

type S = Syntax;
type R<T> = Result<T, Diagnostic>;

#[derive(Clone, Debug)]
pub struct Change {
    pub path: Path,
    pub field: String,
    pub kind: String,
    pub authored: i64,
    /// Inside a `fn` body: closures keep the old literal.
    pub in_fn: bool,
    /// A `fn` body reads a binding on its path: a closure may hold the value.
    pub captured: bool,
    pub before: E,
    pub after: E,
    pub expr: S,
}

/// OCaml `int_of_string_opt` for the forms a literal can take (63-bit range).
pub fn int_of_string_opt(text: &str) -> Option<i64> {
    let (neg, body) = match text.strip_prefix('-') {
        Some(rest) => (true, rest),
        None => (false, text.strip_prefix('+').unwrap_or(text)),
    };
    let (radix, digits) = if let Some(r) = body.strip_prefix("0x").or_else(|| body.strip_prefix("0X")) {
        (16, r)
    } else if let Some(r) = body.strip_prefix("0o").or_else(|| body.strip_prefix("0O")) {
        (8, r)
    } else if let Some(r) = body.strip_prefix("0b").or_else(|| body.strip_prefix("0B")) {
        (2, r)
    } else {
        (10, body)
    };
    if digits.is_empty() || digits.starts_with('_') {
        return None;
    }
    let clean: String = digits.chars().filter(|c| *c != '_').collect();
    let magnitude = i128::from_str_radix(&clean, radix).ok()?;
    let value = if neg { -magnitude } else { magnitude };
    if (-(1i128 << 62)..(1i128 << 62)).contains(&value) { Some(value as i64) } else { None }
}

fn float_of_string_opt(text: &str) -> Option<f64> {
    cfmt::parse_ocaml_float(text)
}

type Lit = (Ty, E, TNode);

fn literal(form: &S) -> Option<Lit> {
    if !form.meta.is_empty() {
        return None;
    }
    match &form.node {
        Node::Num(text) if s::number(text) => match (int_of_string_opt(text), float_of_string_opt(text)) {
            (Some(n), _) => Some((Ty::Int, E::Int(n), TNode::Lit(param::Value::Int(n)))),
            (None, Some(n)) if n.is_finite() => Some((Ty::Float, E::Float(n), TNode::Lit(param::Value::Float(n)))),
            _ => None,
        },
        Node::Str(text) => Some((Ty::Text, E::Text(text.clone()), TNode::Text(text.clone()))),
        Node::Sym(text) if text == "true" || text == "false" => {
            let value = text == "true";
            Some((Ty::Bool, E::Bool(value), TNode::Lit(param::Value::Bool(value))))
        }
        Node::Vec(items) if items.len() == 3 => {
            let number = |form: &S| match literal(form) {
                Some((_, E::Int(n), _)) => Some(n as f64),
                Some((_, E::Float(n), _)) => Some(n),
                _ => None,
            };
            let (x, y, z) = (number(&items[0])?, number(&items[1])?, number(&items[2])?);
            let term = |form: &S| {
                let (ty, _, node) = literal(form).expect("checked above");
                Term { path: None, ty, node, form: std::sync::Arc::new(form.clone()) }
            };
            Some((Ty::Vec3, E::Vec3(x, y, z), TNode::Vec(items.iter().map(term).collect())))
        }
        _ => None,
    }
}

/// `f` over every term, bottom up.
fn map_term(f: &dyn Fn(&Term) -> Term, term: &Term) -> Term {
    f(&w::map_children(&|t| map_term(f, t), term))
}

fn child(form: &S, sub: &[i64]) -> Option<S> {
    let mut form = form.clone();
    for &index in sub {
        let next = s::children(&form).get(usize::try_from(index).ok()?).map(|c| (*c).clone())?;
        form = next;
    }
    Some(form)
}

type Found = (String, w::Context, i64, Term, bool);

fn find_argument(in_fn: bool, field: &str, id: i64, term: &Term) -> Option<Found> {
    if let TNode::Call { kind, ctx, args } = &term.node {
        if let Some((_, arg)) = args.iter().find(|(n, _)| n == field) {
            if arg.form.id == id {
                return Some((kind.clone(), *ctx, term.form.id, arg.clone(), in_fn));
            }
        }
    }
    let in_fn = in_fn || matches!(term.node, TNode::Fn { .. });
    w::children(term).iter().find_map(|c| find_argument(in_fn, field, id, c))
}

/// Names a `fn` body reads: a closure may hold the value bound to one of them.
fn fn_names(graphs: &[&Graph]) -> HashSet<String> {
    fn go(names: &mut HashSet<String>, inside: bool, term: &Term) {
        if let (TNode::RefBinding(name, _), true) = (&term.node, inside) {
            names.insert(name.clone());
        }
        let inside = inside || matches!(term.node, TNode::Fn { .. });
        for c in w::children(term) {
            go(names, inside, &c);
        }
    }
    let mut names = HashSet::new();
    for g in graphs {
        go(&mut names, false, &g.body);
    }
    names
}

fn plain(form: &S) -> bool {
    form.notes.is_empty() && form.tail.is_empty() && s::children(form).iter().all(|c| plain(c))
}

fn patch_source(source: &[S], before: &S, after: &S) -> (Vec<S>, HashMap<i64, S>) {
    let delta = lisp::flat(after).len() as i64 - (before.span.finish - before.span.start);
    let mut forms: HashMap<i64, S> = HashMap::new();
    let shift = |span: &Span| {
        if span.start >= before.span.finish {
            Span { start: span.start + delta, finish: span.finish + delta }
        } else if span.finish >= before.span.finish {
            Span { start: span.start, finish: span.finish + delta }
        } else {
            span.clone()
        }
    };
    fn replacement(forms: &mut HashMap<i64, S>, keep_notes: bool, start: i64, old: &S, value: &S) -> S {
        let finish = start + lisp::flat(value).len() as i64;
        let node = match (&old.node, &value.node) {
            (Node::Vec(olds), Node::Vec(values)) => {
                let mut at = start + 1;
                let mut out = Vec::new();
                for (o, v) in olds.iter().zip(values) {
                    let form = replacement(forms, false, at, o, v);
                    at = form.span.finish + 1;
                    out.push(form);
                }
                Node::Vec(out)
            }
            (_, node) => node.clone(),
        };
        let value = S {
            id: old.id,
            node,
            notes: if value.notes.is_empty() && keep_notes { old.notes.clone() } else { value.notes.clone() },
            span: Span { start, finish },
            ..value.clone()
        };
        forms.insert(value.id, value.clone());
        value
    }
    fn go(forms: &mut HashMap<i64, S>, shift: &dyn Fn(&Span) -> Span, before: &S, after: &S, form: &S) -> S {
        let out = if form.id == before.id {
            replacement(forms, true, before.span.start, form, after)
        } else {
            let node = match &form.node {
                Node::List(items) => Node::List(items.iter().map(|i| go(forms, shift, before, after, i)).collect()),
                Node::Vec(items) => Node::Vec(items.iter().map(|i| go(forms, shift, before, after, i)).collect()),
                Node::Map(items) => Node::Map(items.iter().map(|i| go(forms, shift, before, after, i)).collect()),
                Node::Quote(kind, item) => Node::Quote(*kind, Box::new(go(forms, shift, before, after, item))),
                node => node.clone(),
            };
            S { node, span: shift(&form.span), ..form.clone() }
        };
        forms.insert(out.id, out.clone());
        out
    }
    let out = source.iter().map(|f| go(&mut forms, &shift, before, after, f)).collect();
    (out, forms)
}

/// `None` when the op is not a single literal change.
pub fn patch(catalog: &Catalog, workspace: &Workspace, op: &Op) -> Option<R<(Workspace, Change)>> {
    let Op::SetArg { node: path, key: ArgKey::Kw(field), sub, value } = op else { return None };
    let expr = arg_text(&workspace.source, path, &ArgKey::Kw(field.clone()))?;
    let before = child(&expr, sub)?;
    if before.id == 0 || literal(&expr).is_none() || (!sub.is_empty() && !before.notes.is_empty()) || !plain(value) {
        return None;
    }
    let (ty, _, _) = literal(&before)?;
    let (next_ty, _, _) = literal(value)?;
    let numeric = |t: &Ty| matches!(t, Ty::Int | Ty::Float);
    if !(ty == next_ty || (!sub.is_empty() && numeric(&ty) && numeric(&next_ty))) {
        return None;
    }
    let (kind, ctx, authored, argument, in_fn) = workspace
        .graphs
        .iter()
        .chain(workspace.defs.iter())
        .find_map(|graph| find_argument(false, field, expr.id, &graph.body))?;
    let head = arg_text(&workspace.source, path, &ArgKey::Whole).and_then(|f| s::head(&f).map(str::to_owned))?;
    let descriptor = fc::resolve_kind(catalog, &ctx, &head).ok()?;
    if descriptor.qualified != kind {
        return None;
    }
    let parameter = descriptor.parameters.iter().find(|p| p.name == *field)?;
    let (source, forms) = patch_source(&workspace.source, &before, value);
    let remap_form = |form: &S| -> (S, bool) {
        if form.span.finish > form.span.start {
            match forms.get(&form.id) {
                Some(f) => (f.clone(), true),
                None => (form.clone(), false),
            }
        } else {
            (form.clone(), false)
        }
    };
    let update = |term: &Term| -> Term {
        let (form, changed) = remap_form(&term.form);
        if term.form.id == before.id {
            let (ty, _, node) = literal(&form).expect("patched literal");
            Term { ty, node, form: std::sync::Arc::new(form), ..term.clone() }
        } else if !changed {
            term.clone()
        } else {
            Term { form: std::sync::Arc::new(form), ..term.clone() }
        }
    };
    let argument = map_term(&update, &argument);
    let mut error: Option<Diagnostic> = None;
    {
        let mut report = |severity: Severity, code: &str, message: &str| {
            if severity == Severity::ErrorSeverity && error.is_none() {
                error = Some(rays_diagnostic::error(code.to_owned(), message.to_owned(), Some(argument.form.span.clone()), None, None, Vec::new(), Vec::new()));
            }
        };
        w::validate_parameter(None, &mut report, &fc_param(parameter), &argument);
    }
    if let Some(error) = error {
        return Some(Err(error));
    }
    let graph = |g: &Graph| Graph {
        body: map_term(&update, &g.body),
        inputs: g.inputs.iter().map(|(name, ty, value)| (name.clone(), ty.clone(), value.as_ref().map(|v| map_term(&update, v)))).collect(),
        form: forms.get(&g.form.id).cloned().unwrap_or_else(|| g.form.clone()),
        ..g.clone()
    };
    let old_value = literal(&expr).map(|l| l.1)?;
    let new_value = literal(&argument.form).map(|l| l.1)?;
    let workspace = Workspace {
        source,
        graphs: workspace.graphs.iter().map(graph).collect(),
        defs: workspace.defs.iter().map(graph).collect(),
        packed_roots: workspace.packed_roots.iter().map(|(form, path)| (remap_form(form).0, path.clone())).collect(),
        macros: workspace.macros.iter().map(|f| forms.get(&f.id).cloned().unwrap_or_else(|| f.clone())).collect(),
        ..workspace.clone()
    };
    let names = fn_names(&workspace.graphs.iter().chain(workspace.defs.iter()).collect::<Vec<_>>());
    let change = Change {
        path: path.clone(),
        field: field.clone(),
        kind,
        authored,
        in_fn,
        captured: path.iter().any(|p| names.contains(p)),
        before: old_value,
        after: new_value,
        expr: (*argument.form).clone(),
    };
    Some(Ok((workspace, change)))
}

fn fc_param(p: &fc::Parameter) -> fc::Parameter {
    p.clone()
}

fn lower_nodes(lowered: &l::T, change: &Change) -> Option<R<l::T>> {
    let targets: Vec<&flow_eval::Node> = lowered
        .plan
        .nodes
        .iter()
        .filter(|node| lowered.evaluated.authored.get(node.id as usize) == Some(&change.authored) && node.kind == change.kind)
        .collect();
    if change.authored == 0
        || change.in_fn
        || !lowered.states.is_empty()
        || targets.is_empty()
        || targets.iter().any(|node| {
            node.args.iter().find(|(f, _)| *f == change.field).map(|(_, v)| v) != Some(&change.before) || !lowered.compiled.contains_key(&node.id)
        })
    {
        return None;
    }
    let values: Vec<(i64, i64)> = targets.iter().map(|n| (n.id, lowered.compiled[&n.id])).collect();
    let patch = |network: &net::Network| -> R<net::Network> {
        let mut network = network.clone();
        for (_, id) in &values {
            if sop::edit_graph::find(&network.geometry, *id).is_none() {
                continue;
            }
            let parameter = net::parameter(&network, &Port { node: *id, path: change.field.clone() })?;
            let changes = l::changes(&parameter, &change.after)?;
            // a cook-level parameter rule refuses here as the full lowering does
            network = net::apply_parameters(*id, &changes, &network).map_err(|d| Diagnostic { code: "E_LOWER".to_owned(), ..d })?;
        }
        Ok(network)
    };
    let mut graphs: Vec<l::Graph> = Vec::new();
    for graph in lowered.graphs.iter().rev() {
        match patch(&graph.network) {
            Ok(network) => graphs.push(l::Graph { network, ..graph.clone() }),
            Err(e) => return Some(Err(e)),
        }
    }
    graphs.reverse();
    let nodes: Vec<flow_eval::Node> = lowered
        .plan
        .nodes
        .iter()
        .map(|node| {
            if values.iter().any(|(id, _)| *id == node.id) {
                flow_eval::Node {
                    args: node
                        .args
                        .iter()
                        .map(|(f, v)| (f.clone(), if *f == change.field { change.after.clone() } else { v.clone() }))
                        .collect(),
                    ..node.clone()
                }
            } else {
                node.clone()
            }
        })
        .collect();
    let plan = flow_eval::Plan { nodes, ..lowered.plan.clone() };
    let mut evaluated = (*lowered.evaluated).clone();
    evaluated.plan = plan.clone();
    Some(Ok(l::T { graphs, plan, evaluated: Arc::new(evaluated), ..lowered.clone() }))
}

/// A scene, world or settings object is a `Source`/`Struct` value, not a plan node: swap its field
/// in the values the evaluation recorded at the path, keeping every other value.
fn lower_value(lowered: &l::T, change: &Change) -> Option<R<l::T>> {
    if change.in_fn || change.captured || !lowered.states.is_empty() {
        return None;
    }
    let targets: Vec<E> = lowered
        .evaluated
        .records
        .iter()
        .find(|(p, _)| *p == change.path)
        .map(|(_, vs)| vs.iter().map(|(_, v)| v.clone()).collect())
        .unwrap_or_default();
    let mut hit = false;
    let set = |fs: &[(String, E)]| -> Vec<(String, E)> {
        fs.iter().map(|(f, x)| if *f == change.field { (f.clone(), change.after.clone()) } else { (f.clone(), x.clone()) }).collect()
    };
    fn is_target(targets: &[E], change: &Change, v: &E) -> bool {
        targets.iter().any(|t| t == v)
            && match v {
                E::Source(kind, _, fields) | E::Struct(kind, _, fields) => {
                    *kind == change.kind && fields.iter().find(|(f, _)| *f == change.field).map(|(_, old)| old == &change.before).unwrap_or(false)
                }
                _ => false,
            }
    }
    fn rw(v: &E, targets: &[E], change: &Change, hit: &mut bool, set: &dyn Fn(&[(String, E)]) -> Vec<(String, E)>) -> E {
        if is_target(targets, change, v) {
            *hit = true;
            return match v {
                E::Source(k, ty, fs) => E::Source(k.clone(), ty.clone(), set(fs)),
                E::Struct(k, ty, fs) => E::Struct(k.clone(), ty.clone(), set(fs)),
                v => v.clone(),
            };
        }
        let fields = |fs: &[(String, E)], hit: &mut bool| fs.iter().map(|(f, x)| (f.clone(), rw(x, targets, change, hit, set))).collect::<Vec<_>>();
        match v {
            E::List(a) => E::List(a.iter().map(|x| rw(x, targets, change, hit, set)).collect()),
            E::Record(fs) => E::Record(fields(fs, hit)),
            E::Struct(k, ty, fs) => E::Struct(k.clone(), ty.clone(), fields(fs, hit)),
            E::Source(k, ty, fs) => E::Source(k.clone(), ty.clone(), fields(fs, hit)),
            v => v.clone(),
        }
    }
    let mut rwv = |v: &E, hit: &mut bool| rw(v, &targets, change, hit, &set);
    let results: Vec<(String, E)> = lowered.evaluated.results.iter().map(|(n, v)| (n.clone(), rwv(v, &mut hit))).collect();
    let records: w::Paths = Default::default();
    let _ = records;
    let records: flow_eval::Records = lowered
        .evaluated
        .records
        .iter()
        .map(|(path, vs)| (path.clone(), vs.iter().map(|(i, v)| (i.clone(), rwv(v, &mut hit))).collect()))
        .collect();
    let instances: Vec<flow_eval::Instance> = lowered
        .plan
        .instances
        .iter()
        .map(|i| flow_eval::Instance {
            inputs: i.inputs.iter().map(|(n, v)| (n.clone(), rwv(v, &mut hit))).collect(),
            result: rwv(&i.result, &mut hit),
            ..i.clone()
        })
        .collect();
    if !hit {
        return None;
    }
    let plan = flow_eval::Plan { instances, ..lowered.plan.clone() };
    let mut evaluated = (*lowered.evaluated).clone();
    evaluated.plan = plan.clone();
    evaluated.results = results;
    evaluated.records = records;
    Some(Ok(l::T { plan, evaluated: Arc::new(evaluated), ..lowered.clone() }))
}

pub fn lower(lowered: &l::T, change: &Change) -> Option<R<l::T>> {
    lower_nodes(lowered, change).or_else(|| lower_value(lowered, change))
}
