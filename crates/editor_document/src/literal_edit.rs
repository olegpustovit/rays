//! Port of lib/editor_document/literal_edit.ml: a literal edited in place. Same-type catalog
//! literals keep the checked graph's topology and liveness; the edit re-checks the workspace and
//! patches the lowering instead of lowering again.
//!
//! Gap: OCaml tells the recorded values apart by physical identity (`==`); Rust values have none,
//! so `lower_value` matches the recorded `Source`/`Struct` values structurally.
use crate::layout_by_path::Path;
use flow_check::catalog::{self as fc, Catalog};
use flow_check::workspace::{self as w, Graph, Node as TNode, Term, Workspace};
use flow_check::term_arena::{Draft as TermDraft, TermArena, TermBuilder, TermId};
use flow_syntax::arena::{SyntaxBuilder, SyntaxId};
use flow_graph::flow_edit::{arg_text, ArgKey, Op};
use flow_sop::lower as l;
use flow_sop_catalog::network as net;
use flow_sop_catalog::port::Port;
use flow_syntax::lisp;
use flow_syntax::syntax::{self as s, Node, Syntax};
use flow_syntax::ty::Ty;
use flow_syntax::value::{Value as E, struct_value, source_value};
use rays_diagnostic::{Diagnostic, Severity, Span};
use std::collections::HashMap;
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

fn float_of_string_opt(text: &str) -> Option<f64> {
    cfmt::parse_ocaml_float(text)
}

type Lit = (Ty, E);

fn literal(form: &S) -> Option<Lit> {
    if !form.meta().is_empty() {
        return None;
    }
    match &form.node {
        Node::Num(text) if s::number(form.name(*text)) => match (form.name(*text).parse::<i64>().ok(), float_of_string_opt(form.name(*text))) {
            (Some(n), _) => Some((Ty::Int, E::Int(n))),
            (None, Some(n)) if n.is_finite() => Some((Ty::Float, E::Float(n))),
            _ => None,
        },
        Node::Str(text) => Some((Ty::Text, E::Text(form.shared_bytes(*text)))),
        Node::Sym(text) if form.name(*text) == "true" || form.name(*text) == "false" => {
            Some((Ty::Bool, E::Bool(form.name(*text) == "true")))
        }
        Node::Vec(range) if form.range(*range).len() == 3 => {
            let items = form.range(*range);
            let number = |id| match literal(&form.child(id)) {
                Some((_, E::Int(n))) => Some(n as f64),
                Some((_, E::Float(n))) => Some(n),
                _ => None,
            };
            let (x, y, z) = (number(items[0])?, number(items[1])?, number(items[2])?);
            Some((Ty::Vec3, E::Vec3(x, y, z)))
        }
        _ => None,
    }
}


fn child(form: &S, sub: &[i64]) -> Option<S> {
    let mut form = form.clone();
    for &index in sub {
        let next = form.child(*form.children().get(usize::try_from(index).ok()?)?);
        form = next;
    }
    Some(form)
}

type Found = (String, w::Context, i64, Term, bool);

fn find_argument(in_fn: bool, field: &str, id: i64, term: &Term) -> Option<Found> {
    let arena = term.arena();
    if let TNode::Call(call) = term.node {
        let call = arena.call(call);
        if let Some(field) = arena.fields(call.args).iter().find(|arg| term.name(arg.name) == field) {
            let arg = term.child(field.term);
            if arg.form().id == id {
                return Some((term.name(call.kind).to_owned(), call.ctx, term.form().id, arg, in_fn));
            }
        }
    }
    let in_fn = in_fn || matches!(term.node, TNode::Fn(_));
    term.children().iter().find_map(|child| find_argument(in_fn, field, id, &term.child(*child)))
}

/// Names a `fn` body reads: a closure may hold the value bound to one of them.
/// Whether a function body refers to a binding named on `path`: a walk of arena rows that stops
/// at the first hit and allocates nothing (it runs on every follow-viewport orbit frame).
fn captures(graphs: &[&Graph], path: &[String]) -> bool {
    fn go(arena: &TermArena, path: &[String], inside: bool, row: TermId) -> bool {
        let node = arena.row(row).node;
        if let (TNode::RefBinding(id), true) = (node, inside) {
            let name = arena.name(arena.ref_binding(id).name);
            if path.iter().any(|p| p == name) {
                return true;
            }
        }
        let inside = inside || matches!(node, TNode::Fn(_));
        arena.children(row).iter().any(|child| go(arena, path, inside, *child))
    }
    graphs.iter().any(|g| go(g.body.arena(), path, false, g.body.row_id()))
}

fn plain(form: &S) -> bool {
    form.notes().is_empty() && form.tail().is_empty() && form.children().iter().all(|id| plain(&form.child(*id)))
}

fn patch_source(source: &[S], before: &S, after: &S) -> R<(Vec<S>, HashMap<i64, S>)> {
    let delta = lisp::flat(after).len() as i64 - (i64::from(before.span.finish) - i64::from(before.span.start));
    fn offset(position: i64) -> R<u32> {
        let bytes = usize::try_from(position).map_err(|_| rays_diagnostic::error("E_SOURCE_SIZE".into(), "Source offset is negative.".into(), None, None, None, Vec::new(), Vec::new()))?;
        rays_diagnostic::check_source_len(bytes)?;
        Ok(u32::try_from(bytes).expect("checked source length"))
    }
    let mut forms: HashMap<i64, SyntaxId> = HashMap::new();
    let shift = |span: &Span| -> R<Span> {
        Ok(if span.start >= before.span.finish {
            Span { start: offset(i64::from(span.start) + delta)?, finish: offset(i64::from(span.finish) + delta)? }
        } else if span.finish >= before.span.finish {
            Span { start: span.start, finish: offset(i64::from(span.finish) + delta)? }
        } else {
            span.clone()
        })
    };
    fn replacement(builder: &mut SyntaxBuilder, forms: &mut HashMap<i64, SyntaxId>, keep_notes: bool, start: u32, old: &S, value: &S) -> R<SyntaxId> {
        let finish = offset(i64::from(start) + lisp::flat(value).len() as i64)?;
        let node = match (old.node, value.node) {
            (Node::Vec(olds), Node::Vec(values)) => {
                let mut at = offset(i64::from(start) + 1)?;
                let mut out = Vec::new();
                for (old_id, value_id) in old.range(olds).iter().zip(value.range(values)) {
                    let id = replacement(builder, forms, false, at, &old.child(*old_id), &value.child(*value_id))?;
                    at = offset(i64::from(builder.row(id).span.finish) + 1)?;
                    out.push(id);
                }
                Node::Vec(builder.range(&out))
            }
            _ => builder.row(builder.imported(value).expect("replacement imported once")).node,
        };
        let trivia = s::Trivia {
            notes: if value.notes().is_empty() && keep_notes { old.notes().to_vec() } else { value.notes().to_vec() },
            meta: value.meta().to_vec(),
            tail: value.tail().to_vec(),
        };
        let id = builder.push(old.id, node, Span { start, finish }, Some(trivia));
        forms.insert(old.id, id);
        Ok(id)
    }
    fn go(builder: &mut SyntaxBuilder, forms: &mut HashMap<i64, SyntaxId>, shift: &dyn Fn(&Span) -> R<Span>, before: &S, after: &S, form: &S) -> R<SyntaxId> {
        let out = if form.id == before.id {
            replacement(builder, forms, true, before.span.start, form, after)?
        } else {
            let imported = builder.imported(form).expect("source imported once");
            let node = match form.node {
                Node::List(range) | Node::Vec(range) | Node::Map(range) => {
                    let children = form.range(range).iter().map(|id| go(builder, forms, shift, before, after, &form.child(*id))).collect::<R<Vec<_>>>()?;
                    let range = builder.range(&children);
                    match form.node { Node::List(_) => Node::List(range), Node::Vec(_) => Node::Vec(range), _ => Node::Map(range) }
                }
                Node::Quote(kind, id) => Node::Quote(kind, go(builder, forms, shift, before, after, &form.child(id))?),
                _ => builder.row(imported).node,
            };
            builder.push(form.id, node, shift(&form.span)?, form.trivia().cloned())
        };
        forms.insert(form.id, out);
        Ok(out)
    }
    let mut builder = SyntaxBuilder::new();
    for root in source { builder.import(root); }
    builder.import(after);
    let roots = source.iter().map(|form| go(&mut builder, &mut forms, &shift, before, after, form)).collect::<R<Vec<_>>>()?;
    let arena = builder.freeze();
    Ok((roots.into_iter().map(|id| arena.handle(id)).collect(), forms.into_iter().map(|(authored, id)| (authored, arena.handle(id))).collect()))
}

/// `None` when the op is not a single literal change.
pub fn patch(catalog: &Catalog, workspace: &Workspace, op: &Op) -> Option<R<(Workspace, Change)>> {
    let Op::SetArg { node: path, key: ArgKey::Kw(field), sub, value } = op else { return None };
    let expr = arg_text(&workspace.source, path, &ArgKey::Kw(field.clone()))?;
    let before = child(&expr, sub)?;
    if before.id == 0 || literal(&expr).is_none() || (!sub.is_empty() && !before.notes().is_empty()) || !plain(value) {
        return None;
    }
    let (ty, _) = literal(&before)?;
    let (next_ty, _) = literal(value)?;
    let numeric = |t: &Ty| matches!(t, Ty::Int | Ty::Float);
    if !(ty == next_ty || (!sub.is_empty() && numeric(&ty) && numeric(&next_ty))) {
        return None;
    }
    let (kind, ctx, authored, _, in_fn) = workspace
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
    let (source, forms) = match patch_source(&workspace.source, &before, value) { Ok(patched) => patched, Err(error) => return Some(Err(error)) };
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
    let graph = |g: &Graph| {
        let mut builder = TermBuilder::new();
        let graph_form = forms.get(&g.form.id).unwrap_or(&g.form);
        let graph_form = builder.import_form(graph_form);
        let mut roots = vec![g.body.clone()];
        roots.extend(g.inputs.iter().filter_map(|(_, _, default)| default.clone()));
        let roots = builder.rewrite_roots_with(&roots, &mut |builder, term, draft| {
            let source_form = term.form();
            let (form, changed) = remap_form(&source_form);
            let (draft, ty) = if source_form.id == before.id {
                let (ty, value) = literal(&form).expect("patched literal");
                let draft = match value {
                    E::Int(n) => TermDraft::Lit(param::Value::Int(n)),
                    E::Float(n) => TermDraft::Lit(param::Value::Float(n)),
                    E::Bool(n) => TermDraft::Lit(param::Value::Bool(n)),
                    E::Text(bytes) => TermDraft::Text(bytes),
                    E::Vec3(..) => TermDraft::Vec(form.children().iter().map(|id| {
                        let component = form.child(*id);
                        let (ty, value) = literal(&component).expect("numeric vector component");
                        let value = match value {
                            E::Int(n) => param::Value::Int(n),
                            E::Float(n) => param::Value::Float(n),
                            _ => unreachable!("numeric vector component"),
                        };
                        let form = builder.import_form(&component);
                        builder.push(None, &ty, form, TermDraft::Lit(value))
                    }).collect()),
                    _ => unreachable!("literal value"),
                };
                (draft, Some(ty))
            } else {
                (draft, None)
            };
            let mut id = builder.rebuild(term, draft);
            if let Some(ty) = ty { id = builder.with_type(id, &ty); }
            if changed { id = builder.with_form(id, &form); }
            id
        });
        let (arena, roots, graph_forms) = builder.finish(&roots, &[graph_form]);
        let mut defaults = roots[1..].iter();
        Graph {
            body: arena.handle(roots[0]),
            inputs: g.inputs.iter().map(|(name, ty, default)| (name.clone(), ty.clone(), default.as_ref().map(|_| arena.handle(*defaults.next().expect("default root"))))).collect(),
            form: arena.syntax().handle(graph_forms[0]),
            ..g.clone()
        }
    };
    // A graph none of whose forms `remap_form` would replace (library bodies from other files, or
    // forms ending before the patched literal) rebuilds to itself: reuse it. A follow-viewport orbit
    // patches the camera every frame, and rebuilding every graph made that frame cost the workspace.
    // ponytail: graphs after the literal still rebuild when its text length changes; shift spans in place if that shows.
    let before_edit = |arena: &flow_syntax::arena::SyntaxArena| {
        arena.rows().iter().all(|row| row.id != before.id && (row.span.finish <= before.span.start || row.span.finish <= row.span.start || !forms.contains_key(&row.id)))
    };
    let untouched = |g: &Graph| {
        before_edit(g.form.arena()) && before_edit(g.body.arena().syntax()) && g.inputs.iter().all(|(_, _, default)| default.as_ref().is_none_or(|d| before_edit(d.arena().syntax())))
    };
    let reuse = |g: &Graph| if untouched(g) { g.clone() } else { graph(g) };
    let graphs: Vec<_> = workspace.graphs.iter().map(&reuse).collect();
    let defs: Vec<_> = workspace.defs.iter().map(&reuse).collect();
    let (_, _, _, argument, _) = graphs.iter().chain(&defs).find_map(|graph| find_argument(false, field, expr.id, &graph.body))?;
    let mut error: Option<Diagnostic> = None;
    {
        let mut report = |severity: Severity, code: &str, message: &str| {
            if severity == Severity::ErrorSeverity && error.is_none() {
                error = Some(rays_diagnostic::error(code.to_owned(), message.to_owned(), Some(argument.form().span.clone()), None, None, Vec::new(), Vec::new()));
            }
        };
        w::validate_parameter(None, &mut report, &fc_param(parameter), &argument);
    }
    if let Some(error) = error {
        return Some(Err(error));
    }
    let old_value = literal(&expr).map(|l| l.1)?;
    let new_value = literal(&argument.form()).map(|l| l.1)?;
    let workspace = Workspace {
        source: source.into(),
        graphs: graphs.into(),
        defs: defs.into(),
        packed_roots: workspace.packed_roots.iter().map(|(form, path)| (remap_form(form).0, path.clone())).collect(),
        macros: workspace.macros.iter().map(|f| forms.get(&f.id).cloned().unwrap_or_else(|| f.clone())).collect::<Vec<_>>().into(),
        ..workspace.clone()
    };
    let change = Change {
        path: path.clone(),
        field: field.clone(),
        kind,
        authored,
        in_fn,
        captured: captures(&workspace.graphs.iter().chain(workspace.defs.iter()).collect::<Vec<_>>(), path),
        before: old_value,
        after: new_value,
        expr: argument.form(),
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
    let plan = flow_eval::Plan { id: flow_eval::fresh_plan_id(), nodes, ..lowered.plan.clone() };
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
                E::Source(g) | E::Struct(g) => {
                    g.name == change.kind && g.fields.iter().find(|(f, _)| *f == change.field).map(|(_, old)| old == &change.before).unwrap_or(false)
                }
                _ => false,
            }
    }
    fn rw(v: &E, targets: &[E], change: &Change, hit: &mut bool, set: &dyn Fn(&[(String, E)]) -> Vec<(String, E)>) -> E {
        if is_target(targets, change, v) {
            *hit = true;
            return match v {
                E::Source(g) => source_value(g.name.clone(), g.ty.clone(), set(&g.fields)),
                E::Struct(g) => struct_value(g.name.clone(), g.ty.clone(), set(&g.fields)),
                v => v.clone(),
            };
        }
        let fields = |fs: &[(String, E)], hit: &mut bool| fs.iter().map(|(f, x)| (f.clone(), rw(x, targets, change, hit, set))).collect::<Vec<_>>();
        match v {
            E::List(a) => E::List(Arc::new(a.iter().map(|x| rw(x, targets, change, hit, set)).collect())),
            E::Record(fs) => E::Record(fields(fs, hit)),
            E::Struct(g) => struct_value(g.name.clone(), g.ty.clone(), fields(&g.fields, hit)),
            E::Source(g) => source_value(g.name.clone(), g.ty.clone(), fields(&g.fields, hit)),
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
    let plan = flow_eval::Plan { id: flow_eval::fresh_plan_id(), instances, ..lowered.plan.clone() };
    let mut evaluated = (*lowered.evaluated).clone();
    evaluated.plan = plan.clone();
    evaluated.results = results;
    evaluated.records = records;
    Some(Ok(l::T { plan, evaluated: Arc::new(evaluated), ..lowered.clone() }))
}

pub fn lower(lowered: &l::T, change: &Change) -> Option<R<l::T>> {
    lower_nodes(lowered, change).or_else(|| lower_value(lowered, change))
}

#[cfg(test)]
mod compact_span_tests {
    use super::*;

    #[test]
    fn whole_vector_edit_replaces_checked_components() {
        let catalog = crate::kinds::catalog(1, &sop::nodes::factories()).unwrap();
        let doc = crate::workspace_doc::of_text(
            &Default::default(), &catalog,
            "(workspace w (graph scene :context scene (scene/camera :eye [0 0 6])))",
        ).unwrap();
        let op = Op::SetArg {
            node: vec!["scene".into(), "@result".into()], key: ArgKey::Kw("eye".into()),
            sub: vec![], value: s::parse("[9 2.5 -3]").unwrap().remove(0),
        };
        let (checked, change) = patch(&catalog, &doc.checked, &op).unwrap().unwrap();
        let (_, _, _, argument, _) = find_argument(false, "eye", change.expr.id, &checked.graphs[0].body).unwrap();
        let values: Vec<_> = argument.children().iter().map(|id| {
            let term = argument.child(*id);
            let TNode::Lit(literal) = term.node else { panic!("numeric component") };
            term.arena().literal(literal).clone()
        }).collect();
        use flow_check::term_arena::Literal;
        assert!(matches!(values.as_slice(), [Literal::Int(9), Literal::Float(f), Literal::Int(-3)] if *f == 2.5), "numeric components");
        let (full, diagnostics) = flow_sop_catalog::catalog::check_workspace(&doc.checked.ops, false, None, &catalog, &checked.source);
        let full = full.unwrap_or_else(|| panic!("{diagnostics:?}"));
        assert_eq!(checked.graphs[0].body, full.graphs[0].body);
        let patched = flow_eval::static_with(&checked, true, None, None).unwrap();
        let reference = flow_eval::static_with(&full, true, None, None).unwrap();
        assert_eq!(patched.results, reference.results);
        assert_eq!(patched.records, reference.records);
    }

    #[test]
    fn orbit_patch_reuses_graphs_before_the_literal() {
        let catalog = crate::kinds::catalog(1, &sop::nodes::factories()).unwrap();
        let doc = crate::workspace_doc::of_text(
            &Default::default(), &catalog,
            "(workspace w (defn twice :context value [(x : float)] (* x 2.0)) (graph early :context sop (sop/box :size [1 1 1])) (graph scene :context scene (scene/camera :eye [0 0 6])))",
        ).unwrap();
        let eye = |text: &str| Op::SetArg {
            node: vec!["scene".into(), "@result".into()], key: ArgKey::Kw("eye".into()),
            sub: vec![], value: s::parse(text).unwrap().remove(0),
        };
        let (once, _) = patch(&catalog, &doc.checked, &eye("[9 2.5 -3]")).unwrap().unwrap();
        let (twice, change) = patch(&catalog, &once, &eye("[10.125 2 -3.5]")).unwrap().unwrap();
        assert!(!change.captured);
        let same = |a: &Graph, b: &Graph| Arc::ptr_eq(a.body.arena(), b.body.arena());
        assert!(same(&once.graphs[0], &twice.graphs[0]), "a graph before the literal was rebuilt");
        assert!(once.defs.iter().zip(twice.defs.iter()).all(|(a, b)| same(a, b)), "a defn or library body was rebuilt");
        assert!(!same(&once.graphs[1], &twice.graphs[1]));
        let (full, diagnostics) = flow_sop_catalog::catalog::check_workspace(&doc.checked.ops, false, None, &catalog, &twice.source);
        let full = full.unwrap_or_else(|| panic!("{diagnostics:?}"));
        for (patched, checked) in twice.graphs.iter().zip(full.graphs.iter()).chain(twice.defs.iter().zip(full.defs.iter())) {
            assert_eq!(patched.body, checked.body, "{}", patched.name);
        }
    }

    #[test]
    fn literal_growth_rejects_offsets_beyond_the_compact_source_limit() {
        let parsed = s::parse("[1 2]").unwrap().remove(0);
        let mut builder = s::SyntaxBuilder::new();
        let root = builder.import(&parsed);
        let root = builder.with_location(root, parsed.id, rays_diagnostic::Span { start: parsed.span.start, finish: rays_diagnostic::MAX_SOURCE_BYTES as u32 });
        let source = vec![builder.freeze().handle(root)];
        let Node::Vec(items) = &source[0].node else { panic!("expected vector") };
        let before = source[0].child(source[0].range(*items)[0]);
        let mut builder = s::SyntaxBuilder::new();
        let after = builder.with_node(&before, s::Draft::Num("1234".into()));
        let after = builder.freeze().handle(after);
        assert_eq!(patch_source(&source, &before, &after).unwrap_err().code, "E_SOURCE_SIZE");
    }
}
