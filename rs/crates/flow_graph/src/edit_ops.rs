// Port of lib/flow_graph/edit_ops.ml: every graph gesture as a pure rewrite of the workspace source.
use crate::edit_base::*;
use crate::edit_unfold::*;
use flow_check::{catalog::Catalog, op::Op as FlowOp, workspace as w};
use flow_syntax::syntax::{self as s, Node, QuoteKind};
use flow_syntax::{lisp, macros, ty::Ty};
use rays_diagnostic::{Diagnostic, Severity};

fn hd(sp: &[String]) -> R<&String> {
    sp.first().ok_or_else(|| fail("No scope."))
}

fn float_of(t: &str) -> R<f64> {
    cfmt::parse_ocaml_float(t).ok_or_else(|| fail("The edit failed: Failure(\"float_of_string\")"))
}

// a binding used once is inlined into its use
pub fn fold_into(src: &[S], node: &[String]) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    edit_scope(src, &sp, &mut |sx| {
        let sc = scope_of(sx).ok_or_else(|| fail("Fold needs a named node."))?;
        if leaf == "@result" {
            return Err(fail("A result cannot be folded."));
        }
        if nested(&leaf) {
            return Err(fail("That node is already written in its use."));
        }
        let j = first_pair_index(&sc, &leaf)?;
        let (p, e) = sc.ps[j].clone();
        let name = match &p.node {
            Node::Sym(n) => n.clone(),
            _ => return Err(fail("Only a plain name can be folded.")),
        };
        let others: Vec<(S, S)> = sc.ps.iter().enumerate().filter(|(k, _)| *k != j).map(|(_, q)| q.clone()).collect();
        let uses = others.iter().fold(count_refs(&name, &sc.res), |n, (_, x)| n + count_refs(&name, x));
        if uses != 1 {
            return Err(fail(&format!("Fold needs exactly one use of {} inside its scope.", name)));
        }
        // the binding's note goes with it
        let mut moved = e.clone();
        *moved.notes_mut() = p.notes().iter().chain(e.notes().iter()).cloned().collect();
        let rep = |x: &S| replace_ref(&name, &moved, x);
        let ps: Vec<(S, S)> = others.iter().map(|(q, x)| (q.clone(), rep(x))).collect();
        let res = rep(&sc.res);
        if ps.iter().any(|(_, x)| count_refs(&name, x) > 0) || count_refs(&name, &res) > 0 {
            return Err(fail(&format!("{} is read by field; it cannot be folded.", name)));
        }
        Ok(collapse(&sc, &ps, res))
    })
}

// the rule of the expression cards: an unnamed math node with one reader (not the result) is written in it
pub fn inline_autos(src: Vec<S>, sp: &[String], names: &[String]) -> Vec<S> {
    let mut src = src;
    for n in names {
        let mut ok = false;
        let _ = edit_scope(&src, sp, &mut |sx| {
            if let Some(sc) = scope_of(sx) {
                if !nested(n) && sc.res.node != Node::Sym(n.clone()) {
                    if let Some(j) = find_pair(&sc, n) {
                        let e = &sc.ps[j].1;
                        ok = value_call(e) && auto_named(n, e);
                    }
                }
            }
            Ok(sx.clone())
        });
        if ok {
            let mut node = sp.to_vec();
            node.push(n.clone());
            if let Ok(folded) = fold_into(&src, &node) {
                src = folded;
            }
        }
    }
    src
}

/// How a `Wrap` writes its result: one candidate rewrite.
#[derive(Clone, Debug)]
pub enum Build {
    If(S),
    MapSource { h: String, c: String },
    Items { h: String },
    ForMerge(String),
    ForSum,
    Fold(String),
}

/// One candidate rewrite of the source.
#[derive(Clone, Debug)]
pub enum Attempt {
    Plain(Op, Option<S>),
    Wrap { sp: Vec<String>, nodes: Vec<Path>, build: Build },
    Refuse(String),
}

fn zone_clause(iv: &str, n: i64) -> S {
    vec(vec![sym(iv), call("range", vec![num(n)])])
}
fn zone(h: &str, iv: &str, n: i64, body: S) -> S {
    call(h, vec![zone_clause(iv, n), body])
}

fn build_wrap(b: &Build, x: &Selection, iv: &str, used: &mut Vec<String>) -> Vec<(S, S)> {
    let out = x.out().0.clone();
    match b {
        Build::If(fallback) => vec![(out, call("if", vec![sym("true"), body_of(x), fallback.clone()]))],
        Build::MapSource { h, c } => {
            let body = replace_ref(c, &sym(iv), &body_of(x));
            vec![(out, call(h, vec![call("fn", vec![vec(vec![sym(iv)]), body]), sym(c)]))]
        }
        Build::Items { h } => {
            let items = fresh(used, "items");
            vec![
                (sym(&items), call("range", vec![num(6)])),
                (out, call(h, vec![call("fn", vec![vec(vec![sym(iv)]), body_of(x)]), sym(&items)])),
            ]
        }
        Build::ForMerge(merge) => {
            let each = fresh(used, &format!("{}_each", x.out_name));
            vec![(sym(&each), zone("for", iv, 6, body_of(x))), (out, call(merge, vec![sym(&each)]))]
        }
        Build::ForSum => vec![(out, zone("sum", iv, 6, body_of(x)))],
        Build::Fold(f) => {
            let prev = fresh(used, "prev");
            vec![(out, call("fold", vec![vec(vec![sym(&prev), sym(f)]), zone_clause(iv, 4), replace_ref(f, &sym(&prev), &body_of(x))]))]
        }
    }
}

fn wrap_attempt(src: &[S], sp: &[String], nodes: &[Path], build: &Build) -> R<Vec<S>> {
    let root = hd(sp)?;
    let mut used = root_used(src, root);
    edit_scope(src, sp, &mut |sx| {
        let x = select(sx, nodes, "Repeat")?;
        let declared = sel_declared(&x);
        let iv = match ["i", "j", "k", "n", "idx"].iter().find(|c| !used.iter().any(|u| u == *c) && !declared.iter().any(|d| d == *c)) {
            Some(c) => {
                used.insert(0, (*c).to_owned());
                (*c).to_owned()
            }
            None => fresh(&mut used, "i"),
        };
        let added = build_wrap(build, &x, &iv, &mut used);
        reorder(&rebuild(&x.sc, &insert_at(&x.keep, x.first.min(x.keep.len()) as i64, &added), x.sc.res.clone()))
    })
}

fn arms_of(sx: &S, leaf: &str) -> R<(S, String, Vec<S>, Vec<(S, S)>)> {
    let e = get_node(sx, leaf)?;
    let h = head_sym(&e).unwrap_or("").to_owned();
    if h != "cond" && h != "case" {
        return Err(fail("Only cond and case have editable arms."));
    }
    let kids = children(&e);
    let args = kids[1..].to_vec();
    let (prefix, args) = if h == "case" {
        let first = args.first().ok_or_else(|| fail("The edit failed: Failure(\"hd\")"))?.clone();
        (vec![first], args[1..].to_vec())
    } else {
        (Vec::new(), args)
    };
    let arms = pairs(&args);
    Ok((e, h, prefix, arms))
}

fn add_arm(src: &[S], node: &[String], after: i64, fallback: &Option<S>) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    edit_scope(src, &sp, &mut |sx| {
        let (e, h, prefix, arms) = arms_of(sx, &leaf)?;
        let count = arms.len() as i64 - 1;
        if after < -1 || after >= count {
            return Err(fail("That arm no longer exists."));
        }
        let test = if h == "cond" {
            sym("false")
        } else {
            match arms.first() {
                Some((a, _)) => match &a.node {
                    Node::Num(value) => mk(Node::Num(lisp::float(float_of(value)? + 1.0))),
                    Node::Str(value) => {
                        let mut v = value.clone();
                        v.push(b'1');
                        mk(Node::Str(v))
                    }
                    Node::Sym(n) if n == "true" => sym("false"),
                    _ => sym("true"),
                },
                None => sym("true"),
            }
        };
        let fb = match fallback {
            Some(f) => f.clone(),
            None => arms.last().ok_or_else(|| fail("The edit failed: Failure(\"hd\")"))?.1.clone(),
        };
        let mut items = vec![sym(&h)];
        items.extend(prefix.iter().cloned());
        items.extend(flat_pairs(&insert_at(&arms, after + 1, &[(test, fb)])));
        set_node(sx, &leaf, with_node(&e, Node::List(items.into())))
    })
}

fn delete_arm(src: &[S], node: &[String], index: i64) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    edit_scope(src, &sp, &mut |sx| {
        let (e, h, prefix, arms) = arms_of(sx, &leaf)?;
        if index < 0 || index >= arms.len() as i64 - 1 {
            return Err(fail("The final else arm stays."));
        }
        let kept: Vec<(S, S)> = arms.iter().enumerate().filter(|(i, _)| *i as i64 != index).map(|(_, a)| a.clone()).collect();
        let mut items = vec![sym(&h)];
        items.extend(prefix.iter().cloned());
        items.extend(flat_pairs(&kept));
        set_node(sx, &leaf, with_node(&e, Node::List(items.into())))
    })
}

fn set_arg(src: &[S], node: &[String], key: &ArgKey, sub: &[i64], value: &S) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    edit_scope(src, &sp, &mut |sx| {
        let e = get_node(sx, &leaf)?;
        let arg = if sub.is_empty() {
            value.clone()
        } else {
            match arg_get(&e, key) {
                Some(cur) => set_sub(&cur, sub, value.clone())?,
                None => return Err(fail("That input is not set.")),
            }
        };
        reorder(&set_node(sx, &leaf, arg_set(&e, key, Some(arg))?)?)
    })
}

fn connect(src: &[S], node: &[String], key: &ArgKey, name: &str, iter: bool) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    let root = hd(&sp)?.clone();
    let mut used = root_used(src, &root);
    let mut prev: Vec<String> = Vec::new();
    let out = edit_scope(src, &sp, &mut |sx| {
        // the output of a nested node feeds a second input: it needs a name
        let (sx, name) = if nested(name) {
            if split_leaf(name)?.0 != "@result" && find_pair(&ensure(sx), name).is_none() {
                return Err(fail("Name that node first (rename it): it is written inside another scope."));
            }
            let (held, at) = holder(name)?;
            unfold_in(&mut used, None, sx, &held, &at, &[])?
        } else {
            (sx.clone(), name.to_owned())
        };
        if leaf == "@result" && *key == ArgKey::Whole {
            let sc = ensure(&sx);
            let ps = if matches!(sc.res.node, Node::Sym(_)) {
                sc.ps.clone()
            } else {
                let base = match head_sym(&sc.res) {
                    Some(h) => {
                        let parts: Vec<&str> = h.split('/').collect();
                        if parts.len() == 2 { parts[1].to_owned() } else { h.to_owned() }
                    }
                    None => "result".to_owned(),
                };
                let fr = fresh_name(src, &root, &base);
                let mut ps = sc.ps.clone();
                ps.push((sym(&fr), sc.res.clone()));
                ps
            };
            reorder(&rebuild(&sc, &ps, sym(&name)))
        } else {
            let sx = keep_nested(&default_pick, &mut used, &sx, &leaf, key)?;
            let e = get_node(&sx, &leaf)?;
            if let Some(Node::Sym(n)) = arg_get(&e, key).map(|a| a.node) {
                prev = vec![n];
            }
            let step = match (iter, arg_get(&e, key)) {
                (true, Some(cur)) => match &cur.node {
                    Node::Num(n) if n != "0" && n != "1" && cfmt::parse_ocaml_float(n) != Some(0.0) && cfmt::parse_ocaml_float(n) != Some(1.0) => {
                        call("*", vec![sym(&name), cur.clone()])
                    }
                    _ => sym(&name),
                },
                _ => sym(&name),
            };
            reorder(&set_node(&sx, &leaf, arg_set(&e, key, Some(step))?)?)
        }
    })?;
    let mut names = vec![name.to_owned()];
    names.extend(prev);
    Ok(inline_autos(out, &sp, &names))
}

fn disconnect(src: &[S], node: &[String], key: &ArgKey, fallback: &Option<S>) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    let root = hd(&sp)?.clone();
    let mut prev: Vec<String> = Vec::new();
    let out = edit_scope(src, &sp, &mut |sx| {
        let mut used = root_used(src, &root);
        let sx = keep_nested(&value_call, &mut used, sx, &leaf, key)?;
        let e = get_node(&sx, &leaf)?;
        if let Some(Node::Sym(n)) = arg_get(&e, key).map(|a| a.node) {
            prev = vec![n];
        }
        let v = match (key, fallback) {
            (_, Some(d)) => Some(d.clone()),
            (ArgKey::Kw(_) | ArgKey::Field(_) | ArgKey::Pos(_), None) => None,
            (_, None) => return Err(fail("Nothing to fall back to. Drag another output onto it instead.")),
        };
        reorder(&set_node(&sx, &leaf, arg_set(&e, key, v)?)?)
    })?;
    Ok(inline_autos(out, &sp, &prev))
}

fn set_input_default(src: &[S], form: &str, input: &str, value: &S) -> R<Vec<S>> {
    with_root(src, form, &mut |r| {
        let mut hit = false;
        let r2 = map_children(
            &mut |c| match &c.node {
                Node::Vec(ds) => with_node(
                    c,
                    Node::Vec(
                        ds.iter()
                            .map(|d| match &d.node {
                                Node::List(l) if l.len() >= 3 && sym_of(&l[0]) == Some(input) => {
                                    hit = true;
                                    with_node(d, Node::List(vec![l[0].clone(), l[1].clone(), l[2].clone(), value.clone()].into()))
                                }
                                _ => d.clone(),
                            })
                            .collect(),
                    ),
                ),
                _ => c.clone(),
            },
            r,
        );
        if !hit {
            return Err(fail(&format!("No input {}.", input)));
        }
        Ok(r2)
    })
}

fn unfold(src: &[S], node: &[String], key: &ArgKey, sub: &[i64]) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    let mut used = root_used(src, hd(&sp)?);
    edit_scope(src, &sp, &mut |sx| reorder(&unfold_in(&mut used, None, sx, &leaf, key, sub)?.0))
}

fn delete_nodes(src: &[S], nodes: &[Path]) -> R<Vec<S>> {
    // inner first: deeper scopes, then nodes nested deeper in one binding
    let mut depths: Vec<((usize, usize), &Path)> = Vec::new();
    for n in nodes {
        let leaf = split_node(n)?.1;
        depths.push(((n.len(), leaf.split('#').count()), n));
    }
    depths.sort_by(|a, b| b.0.cmp(&a.0));
    let mut touched: Vec<(Vec<String>, Vec<String>)> = Vec::new();
    let mut cur: Vec<S> = src.to_vec();
    for (_, node) in &depths {
        let (sp, leaf) = split_node(node)?;
        let mut gone: Option<S> = None;
        let edited = edit_scope(&cur, &sp, &mut |sx| {
            if nested(&leaf) {
                // a nested node leaves its chain: what it read first takes its place
                let (held, key) = holder(&leaf)?;
                let h = get_node(sx, &held)?;
                let below = match (arg_get(&h, &key).and_then(|e| arg_get(&e, &chain_in(&e))), &key) {
                    (Some(b), _) => Some(b),
                    (None, ArgKey::Pos(_)) => Some(sym("nil")),
                    (None, _) => None,
                };
                set_node(sx, &held, arg_set(&h, &key, below)?)
            } else {
                match scope_of(sx) {
                    Some(sc) if leaf != "@result" => {
                        let j = first_pair_index(&sc, &leaf)?;
                        let (p, e) = sc.ps[j].clone();
                        gone = Some(e.clone());
                        touched.push((sp.clone(), dedup(&sym_list(&e))));
                        let others: Vec<(S, S)> = sc.ps.iter().enumerate().filter(|(k, _)| *k != j).map(|(_, q)| q.clone()).collect();
                        // a deleted value dissolves: its readers get its expression; a scene object leaves
                        // the result that held it instead, and so does a loop the result reads (below)
                        let obj = starts_with("scene/", head_sym(&e)) || starts_with("world/", head_sym(&e));
                        let rep = |x: &S| match &p.node {
                            Node::Sym(n) if !obj => replace_ref(n, &e, x),
                            _ => x.clone(),
                        };
                        let ps: Vec<(S, S)> = others.iter().map(|(q, x)| (q.clone(), rep(x))).collect();
                        let res = if is_zone(&e) { sc.res.clone() } else { rep(&sc.res) };
                        Ok(collapse(&sc, &ps, res))
                    }
                    _ => Err(fail("Only a named node can be deleted.")),
                }
            }
        })?;
        // a scene object, World layer or loop of scene objects also leaves the result that held it
        cur = match gone {
            Some(e) if starts_with("scene/", head_sym(&e)) || starts_with("world/", head_sym(&e)) || is_zone(&e) => {
                let below = if is_zone(&e) { None } else { arg_get(&e, &ArgKey::Pos(0)) };
                with_root(&edited, hd(&sp)?, &mut |root| {
                    let root = detach(&leaf, &below, root)?;
                    if starts_with("world/", head_sym(&e)) { detach_result(&leaf, &below, &root) } else { Ok(root) }
                })?
            }
            _ => edited,
        };
    }
    let out = cur;
    for node in nodes {
        // read inside its own scope only: a sibling scope may bind the same name (a duplicated loop)
        let (sp, name) = split_node(node)?;
        let mut used = false;
        let _ = edit_scope(&out, &sp, &mut |sx| {
            used = sym_list(sx).contains(&name);
            Ok(sx.clone())
        });
        if !nested(&name) && used {
            return Err(fail(&format!(
                "{} is still read (a field read such as {}.x, or a scene object). Disconnect those first.",
                name, name
            )));
        }
    }
    let mut out = out;
    for (sp, names) in touched.iter().rev() {
        out = inline_autos(out, sp, names);
    }
    Ok(out)
}

fn rename(src: &[S], node: &[String], to: &str) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    let used = sym_list(&root_form(src, hd(&sp)?)?);
    if !valid_name(to) || used.iter().any(|u| u == to) || w::name_taken(to) {
        return Err(fail("Pick a new lowercase name that is not used anywhere in this graph."));
    }
    let out = edit_scope(src, &sp, &mut |sx| {
        // naming a nested node binds it
        if nested(&leaf) {
            let (held, key) = holder(&leaf)?;
            reorder(&unfold_in(&mut Vec::new(), Some(to), sx, &held, &key, &[])?.0)
        } else {
            let sc = scope_of(sx).ok_or_else(|| fail("Rename needs a named node."))?;
            let j = first_pair_index(&sc, &leaf)?;
            let old = match &sc.ps[j].0.node {
                Node::Sym(n) => n.clone(),
                _ => return Err(fail("Only a plain name can be renamed.")),
            };
            let ps: Vec<(S, S)> = sc
                .ps
                .iter()
                .enumerate()
                .map(|(k, (p, v))| {
                    if k == j {
                        (with_node(p, Node::Sym(to.to_owned())), v.clone())
                    } else if k > j {
                        (p.clone(), rename_ref(&old, to, v))
                    } else {
                        (p.clone(), v.clone())
                    }
                })
                .collect();
            Ok(rebuild(&sc, &ps, rename_ref(&old, to, &sc.res)))
        }
    })?;
    Ok(if nested(&leaf) { out } else { inline_autos(out, &sp, &[to.to_owned()]) })
}

fn hoist_inner(v: &S, leaf: &str, hoisted: &mut Option<(S, S)>) -> R<S> {
    let entered = enter(v)?;
    let sc = scope_of(&entered).ok_or_else(|| fail("Nothing to move."))?;
    let j = first_pair_index(&sc, leaf)?;
    let (p, e) = sc.ps[j].clone();
    let local: Vec<String> = dedup(&sym_list(&e))
        .into_iter()
        .filter(|x| sc.ps.iter().any(|(q, _)| pat_names(q).contains(x)))
        .collect();
    if !local.is_empty() {
        return Err(fail(&format!("{} uses {} from inside this scope. Move that out first.", leaf, local.join(", "))));
    }
    *hoisted = Some((p, e));
    let rest: Vec<(S, S)> = sc.ps.iter().enumerate().filter(|(k, _)| *k != j).map(|(_, q)| q.clone()).collect();
    Ok(leave(v, collapse(&sc, &rest, sc.res.clone())))
}

fn hoist(src: &[S], node: &[String]) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    let (pp, owner) = match sp.len() {
        0 | 1 => return Err(fail("Only a node inside a loop or scope can move out.")),
        _ => split_node(&sp)?,
    };
    edit_scope(src, &pp, &mut |ps_expr| {
        let out_scope = ensure(ps_expr);
        let mut hoisted: Option<(S, S)> = None;
        let (ps, res, at) = if owner == "@result" {
            let res = hoist_inner(&out_scope.res, &leaf, &mut hoisted)?;
            (out_scope.ps.clone(), res, out_scope.ps.len())
        } else {
            let k = first_pair_index(&out_scope, &owner)?;
            let mut ps = out_scope.ps.clone();
            ps[k].1 = hoist_inner(&ps[k].1.clone(), &leaf, &mut hoisted)?;
            (ps, out_scope.res.clone(), k)
        };
        let pe = hoisted.ok_or_else(|| fail("Nothing to move."))?;
        reorder(&rebuild(&out_scope, &insert_at(&ps, at as i64, &[pe]), res))
    })
}

fn wrap_attempts(src: &[S], nodes: &[Path], loop_: Loop, fallback: &Option<S>) -> R<Vec<Attempt>> {
    let sp = scope_path_of(nodes)?;
    let root = hd(&sp)?.clone();
    let rootf = root_form(src, &root)?;
    let mk_attempt = |nodes: Vec<Path>, build: Build| Attempt::Wrap { sp: sp.clone(), nodes, build };
    match loop_ {
        Loop::If => {
            let fallbacks = match fallback {
                Some(f) => vec![f.clone()],
                None => vec![
                    sym("nil"),
                    num(0),
                    sym("false"),
                    vec(vec![num(0), num(0), num(0)]),
                    mk(Node::Str(Vec::new())),
                    call("list", Vec::new()),
                ],
            };
            Ok(fallbacks.into_iter().map(|f| mk_attempt(nodes.to_vec(), Build::If(f))).collect())
        }
        Loop::Map | Loop::Filter => {
            let h = if loop_ == Loop::Map { "map" } else { "filter" };
            // two nodes: one is the collection, the other the body, read per item.  The checker
            // picks the order; the source stays a binding of its own.
            let mut out = Vec::new();
            if let [n1, n2] = nodes {
                for (wn, c) in [(n1, n2), (n2, n1)] {
                    let c = split_node(c)?.1;
                    out.push(mk_attempt(vec![wn.clone()], Build::MapSource { h: h.to_owned(), c }));
                }
            }
            // otherwise a named placeholder `items` beside the wrapped node, one literal to edit
            out.push(mk_attempt(nodes.to_vec(), Build::Items { h: h.to_owned() }));
            Ok(out)
        }
        Loop::For => {
            // ponytail: the checker is the type oracle, so try the geometry and
            // drawing shapes (collect and merge) and then the number shape (sum)
            let mut out: Vec<Attempt> = ["sop/merge", "draw/merge"].iter().map(|m| mk_attempt(nodes.to_vec(), Build::ForMerge((*m).to_owned()))).collect();
            out.push(mk_attempt(nodes.to_vec(), Build::ForSum));
            Ok(out)
        }
        Loop::Fold => {
            // every outside name the selection reads is a candidate to feed back
            let mut free_names: Vec<String> = Vec::new();
            edit_scope(src, &sp, &mut |sx| {
                free_names = outside_names(&rootf, &select(sx, nodes, "Iterate")?);
                Ok(sx.clone())
            })?;
            if free_names.is_empty() {
                Ok(vec![Attempt::Refuse(
                    "Iterate feeds the result back into an input of the same type; none of the selected nodes reads one from outside.".to_owned(),
                )])
            } else {
                Ok(free_names.into_iter().map(|f| mk_attempt(nodes.to_vec(), Build::Fold(f))).collect())
            }
        }
    }
}

fn make_local_fn(src: &[S], nodes: &[Path]) -> R<Vec<S>> {
    let sp = scope_path_of(nodes)?;
    let root = hd(&sp)?.clone();
    let rootf = root_form(src, &root)?;
    let mut used = root_used(src, &root);
    edit_scope(src, &sp, &mut |sx| {
        let x = select(sx, nodes, "A function")?;
        let fns = fn_names(&rootf);
        let outer: Vec<String> = outside_names(&rootf, &x).into_iter().filter(|n| !fns.contains(n)).collect();
        if fns.iter().any(|f| f == head_sym(&body_of(&x)).unwrap_or("")) {
            return Err(fail("That already calls a local function; open it with i."));
        }
        if outer.is_empty() {
            return Err(fail("A function needs an input: this reads no other node. Wire one in first."));
        }
        let fname = fresh(&mut used, &format!("{}_fn", x.out_name));
        let params: Vec<String> = outer.iter().map(|n| fresh(&mut used, &format!("in_{}", n))).collect();
        let mut body = body_of(&x);
        for (n, p) in outer.iter().zip(params.iter()) {
            body = replace_ref(n, &sym(p), &body);
        }
        let added = vec![
            (sym(&fname), call("fn", vec![vec(params.iter().map(|p| sym(p)).collect()), body])),
            (x.out().0.clone(), call(&fname, outer.iter().map(|n| sym(n)).collect())),
        ];
        reorder(&rebuild(&x.sc, &insert_at(&x.keep, x.first.min(x.keep.len()) as i64, &added), x.sc.res.clone()))
    })
}

fn taken_names(items: &[S]) -> Vec<String> {
    items
        .iter()
        .filter_map(|i| match &i.node {
            Node::List(l) if l.len() >= 2 && matches!(sym_of(&l[0]), Some("graph" | "defn" | "defmacro")) => sym_of(&l[1]).map(|n| n.to_owned()),
            _ => None,
        })
        .collect()
}

fn make_macro(src: &[S], nodes: &[Path], name: &str, holes: &[(Vec<i64>, String)]) -> R<Vec<S>> {
    let sp = scope_path_of(nodes)?;
    let rootf = root_form(src, hd(&sp)?)?;
    let (_, items) = workspace_parts(src)?;
    let taken = taken_names(&items);
    if !macros::valid_name(name) || taken.iter().any(|t| t == name) || w::name_taken(name) {
        return Err(fail("Pick a new lowercase macro name."));
    }
    let names: Vec<String> = holes.iter().map(|h| h.1.clone()).collect();
    if names.iter().any(|n| !valid_name(n)) || dedup(&names).len() != names.len() {
        return Err(fail("Hole names must be distinct lowercase names."));
    }
    let mut defn: Option<S> = None;
    let out = edit_scope(src, &sp, &mut |sx| {
        let x = select(sx, nodes, "A macro")?;
        let tmpl = body_of(&x);
        let free_n = outside_names(&rootf, &x);
        let mut hole_vals = Vec::new();
        for (path, _) in holes {
            hole_vals.push(get_sub(&tmpl, path).ok_or_else(|| fail("A hole is not in the template."))?);
        }
        let mut body = tmpl.clone();
        for (path, n) in holes {
            body = set_sub(&body, path, unquote(sym(n)))?;
        }
        for f in &free_n {
            body = replace_ref(f, &unquote(sym(f)), &body);
        }
        if x.sel.len() > 1 {
            for n in sel_names(&x) {
                body = replace_ref(&n, &sym(&format!("{}#", n)), &body);
            }
        }
        let mut params = names.clone();
        params.extend(free_n.iter().cloned());
        defn = Some(call("defmacro", vec![sym(name), vec(params.iter().map(|p| sym(p)).collect()), mk(Node::Quote(QuoteKind::Quasi, std::sync::Arc::new(body)))]));
        let mut args = hole_vals.clone();
        args.extend(free_n.iter().map(|f| sym(f)));
        let added = vec![(x.out().0.clone(), call(name, args))];
        reorder(&rebuild(&x.sc, &insert_at(&x.keep, x.first.min(x.keep.len()) as i64, &added), x.sc.res.clone()))
    })?;
    let defn = defn.ok_or_else(|| fail("The edit failed: Invalid_argument(\"option is None\")"))?;
    map_items(&out, &mut |items| {
        let mut r = vec![defn.clone()];
        r.extend(items);
        Ok(r)
    })
}

fn make_defn(src: &[S], nodes: &[Path], name: &str, context: &str, params: &[(String, String)]) -> R<Vec<S>> {
    let sp = scope_path_of(nodes)?;
    let rootf = root_form(src, hd(&sp)?)?;
    let (_, items) = workspace_parts(src)?;
    let taken = taken_names(&items);
    if !valid_name(name) || taken.iter().any(|t| t == name) || w::name_taken(name) {
        return Err(fail("Pick a new lowercase function name."));
    }
    let mut defn: Option<S> = None;
    let out = edit_scope(src, &sp, &mut |sx| {
        let x = select(sx, nodes, "A function")?;
        let outer = outside_names(&rootf, &x);
        let mut a: Vec<String> = params.iter().map(|p| p.0.clone()).collect();
        a.sort();
        let mut b = outer.clone();
        b.sort();
        if a != b {
            return Err(fail(&format!(
                "The function reads {} from outside; each needs a type.",
                if outer.is_empty() { "nothing".to_owned() } else { outer.join(", ") }
            )));
        }
        let mut typed = Vec::new();
        for n in &outer {
            let ty = params.iter().find(|p| &p.0 == n).map(|p| p.1.clone()).ok_or_else(|| fail("The edit failed: Not_found"))?;
            typed.push(mk(Node::List(vec![sym(n), sym(":"), sym(&ty)].into())));
        }
        defn = Some(call("defn", vec![sym(name), kwf("context"), sym(context), vec(typed), body_of(&x)]));
        let added = vec![(x.out().0.clone(), call(name, outer.iter().map(|n| sym(n)).collect()))];
        reorder(&rebuild(&x.sc, &insert_at(&x.keep, x.first.min(x.keep.len()) as i64, &added), x.sc.res.clone()))
    })?;
    let defn = defn.ok_or_else(|| fail("The edit failed: Invalid_argument(\"option is None\")"))?;
    map_items(&out, &mut |items| {
        let mut r = vec![defn.clone()];
        r.extend(items);
        Ok(r)
    })
}

fn inline_macro(src: &[S], node: &[String]) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    let (_, items) = workspace_parts(src)?;
    let defs: Vec<S> = items.into_iter().filter(|i| head_sym(i) == Some("defmacro")).collect();
    edit_scope(src, &sp, &mut |sx| {
        let e = get_node(sx, &leaf)?;
        let x = macros::expand(&defs, &e)?;
        reorder(&set_node(sx, &leaf, x)?)
    })
}

fn inline_defn(src: &[S], node: &[String]) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    let (_, items) = workspace_parts(src)?;
    edit_scope(src, &sp, &mut |sx| {
        let e = get_node(sx, &leaf)?;
        let (name, args) = match &e.node {
            Node::List(l) if !l.is_empty() && sym_of(&l[0]).is_some() => (sym_of(&l[0]).unwrap().to_owned(), l[1..].to_vec()),
            _ => return Err(fail("Only a call of a function can be inlined.")),
        };
        let defn = items
            .iter()
            .find(|i| head_sym(i) == Some("defn") && matches!(children(i).get(1).and_then(|c| sym_of(c).map(|n| n == name)), Some(true)))
            .ok_or_else(|| fail(&format!("{} is not a function of this workspace.", name)))?;
        let kids = children(defn);
        let parts: Vec<S> = kids[2..].to_vec();
        let pvec = parts.iter().find(|x| matches!(x.node, Node::Vec(_)));
        let (params, body) = match (pvec, parts.last()) {
            (Some(S { node: Node::Vec(ps), .. }), Some(body)) => (ps.clone(), body.clone()),
            _ => return Err(fail(&format!("{} has no body to inline.", name))),
        };
        let mut names = Vec::new();
        for p in &params {
            match &p.node {
                Node::List(l) if !l.is_empty() && sym_of(&l[0]).is_some() => names.push(sym_of(&l[0]).unwrap().to_owned()),
                Node::Sym(n) => names.push(n.clone()),
                _ => return Err(fail(&format!("{} has a parameter that is not a name.", name))),
            }
        }
        if names.len() != args.len() {
            return Err(fail(&format!("{} is called with a different number of inputs.", name)));
        }
        // two passes, so an argument that is a name of another parameter is not substituted again
        let tmp = |i: usize| format!("inl#{}", i);
        let mut body = body;
        for (i, n) in names.iter().enumerate() {
            body = replace_ref(n, &sym(&tmp(i)), &body);
        }
        for (i, a) in args.iter().enumerate() {
            body = replace_ref(&tmp(i), a, &body);
        }
        reorder(&set_node(sx, &leaf, body)?)
    })
}

fn move_binding(src: &[S], node: &[String], by: i64) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    edit_scope(src, &sp, &mut |sx| {
        let sc = match scope_of(sx) {
            Some(sc) if !nested(&leaf) && leaf != "@result" => sc,
            _ => return Err(fail("Only a named binding can move.")),
        };
        let j = first_pair_index(&sc, &leaf)? as i64;
        let k = j + by;
        if k < 0 || k >= sc.ps.len() as i64 {
            return Err(fail(&format!("{} cannot move further.", leaf)));
        }
        let mut a = sc.ps.clone();
        a.swap(j as usize, k as usize);
        let moved = rebuild(&sc, &a, sc.res.clone());
        let keys = |x: &S| -> Vec<String> {
            match scope_of(x) {
                Some(sc) => sc.ps.iter().map(|(p, _)| pat_key(p)).collect(),
                None => Vec::new(),
            }
        };
        let r = reorder(&moved)?;
        if keys(&r) != keys(&moved) {
            return Err(fail(&format!("{} is read by or reads its neighbour; it cannot pass it.", leaf)));
        }
        Ok(r)
    })
}

fn move_step(src: &[S], node: &[String], earlier: bool) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    edit_scope(src, &sp, &mut |sx| {
        // the later call of the two trades places with the call in its chain input
        let outer = if earlier {
            leaf.clone()
        } else if nested(&leaf) && {
            let (held, key) = holder(&leaf)?;
            key == chain_in(&get_node(sx, &held)?)
        } {
            holder(&leaf)?.0
        } else {
            return Err(fail("That is the last step of its chain."));
        };
        let o = get_node(sx, &outer)?;
        let ko = chain_in(&o);
        let i = match arg_get(&o, &ko) {
            Some(i) if node_call(&i) => i,
            _ => return Err(fail("That is the first step of its chain.")),
        };
        let ki = chain_in(&i);
        let inner = arg_get(&i, &ki).ok_or_else(|| fail("That is the first step of its chain."))?;
        // (o .. (i .. x ..) ..) becomes (i .. (o .. x ..) ..)
        set_node(sx, &outer, arg_set(&i, &ki, Some(arg_set(&o, &ko, Some(inner))?))?)
    })
}

fn toggle_bypass(src: &[S], node: &[String]) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    edit_scope(src, &sp, &mut |sx| {
        let e = get_node(sx, &leaf)?;
        match (&e.node, head_sym(&e)) {
            (Node::List(_), Some(h)) if !is_zone(&e) && h != "let*" && h != "fn" => {}
            _ => return Err(fail("Only an operator or function call can be bypassed.")),
        }
        let mut e2 = e.clone();
        if e.meta().iter().any(|m| m == "bypass") {
            *e2.meta_mut() = e.meta().iter().filter(|m| *m != "bypass").cloned().collect();
        } else {
            e2.meta_mut().push("bypass".to_owned());
        }
        set_node(sx, &leaf, e2)
    })
}

fn set_note(src: &[S], node: &[String], text: &str) -> R<Vec<S>> {
    let notes = note_lines(text);
    if let [root] = node {
        return with_root(src, root, &mut |r| {
            let mut r = r.clone();
            *r.notes_mut() = notes.clone();
            Ok(r)
        });
    }
    let (sp, leaf) = split_node(node)?;
    if nested(&leaf) {
        return Err(fail("Name the node first (rename it): notes attach to named bindings."));
    }
    edit_scope(src, &sp, &mut |sx| match scope_of(sx) {
        Some(sc) if leaf == "@result" => {
            let mut res = sc.res.clone();
            *res.notes_mut() = notes.clone();
            Ok(rebuild(&sc, &sc.ps, res))
        }
        Some(sc) => {
            let j = first_pair_index(&sc, &leaf)?;
            let mut ps = sc.ps.clone();
            *ps[j].0.notes_mut() = notes.clone();
            Ok(rebuild(&sc, &ps, sc.res.clone()))
        }
        None if leaf == "@result" => {
            let mut r = sx.clone();
            *r.notes_mut() = notes.clone();
            Ok(r)
        }
        None => Err(fail("Notes attach to named bindings.")),
    })
}

fn add_item(src: &[S], node: &[String]) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    edit_scope(src, &sp, &mut |sx| {
        let e = get_node(sx, &leaf)?;
        match &e.node {
            Node::List(l) if !l.is_empty() && matches!(sym_of(&l[0]), Some("list" | "str")) => {
                let h = sym_of(&l[0]).unwrap();
                let args = &l[1..];
                let strip = |x: &S| {
                    let mut x = x.clone();
                    *x.notes_mut() = Vec::new();
                    x
                };
                let v = if h == "str" {
                    mk(Node::Str(b" ".to_vec()))
                } else {
                    let n = args.len();
                    if n >= 2 {
                        let last = &args[n - 1];
                        match (&last.node, &args[n - 2].node) {
                            (Node::Num(a), Node::Num(b)) => match (cfmt::parse_ocaml_float(a), cfmt::parse_ocaml_float(b)) {
                                (Some(fa), Some(fb)) => {
                                    let x = ((2.0 * fa - fb) * 1000.0).round() / 1000.0;
                                    if fa.is_finite() && fa.fract() == 0.0 {
                                        mk(Node::Num((x as i64).to_string()))
                                    } else {
                                        mk(Node::Num(lisp::float(x)))
                                    }
                                }
                                _ => strip(last),
                            },
                            _ => strip(last),
                        }
                    } else if n == 1 {
                        strip(&args[0])
                    } else {
                        mk(Node::Num("0".into()))
                    }
                };
                let mut items = l.clone();
                items.push(v);
                set_node(sx, &leaf, with_node(&e, Node::List(items)))
            }
            _ => Err(fail("Only a list or str can take another item.")),
        }
    })
}

fn move_item(src: &[S], node: &[String], pos: i64) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    edit_scope(src, &sp, &mut |sx| {
        let e = get_node(sx, &leaf)?;
        match &e.node {
            Node::List(l) if !l.is_empty() && pos >= 1 && pos < positional(&l[1..]).len() as i64 => {
                let args = &l[1..];
                // the items are the positional arguments: keyword pairs stay where they are written
                let items = positional(args);
                let mut out = Vec::new();
                let mut k: i64 = 0;
                let mut i = 0;
                while i < args.len() {
                    if i + 1 < args.len() && is_kw(&args[i]) {
                        out.push(args[i].clone());
                        out.push(args[i + 1].clone());
                        i += 2;
                    } else {
                        let at = if k == pos { pos - 1 } else if k == pos - 1 { pos } else { k };
                        out.push(items[at as usize].clone());
                        k += 1;
                        i += 1;
                    }
                }
                // a `scene/merge`'s `:skip` tuples end in an argument position: they follow the items
                let out = if head_sym(&e) != Some("scene/merge") || skip_of_args(&out).is_empty() {
                    out
                } else {
                    let tuples: Vec<Vec<i64>> = skip_of_args(&out)
                        .into_iter()
                        .map(|t| match t.split_last() {
                            Some((p, outer)) => {
                                let mut r = outer.to_vec();
                                r.push(if *p == pos { pos - 1 } else if *p == pos - 1 { pos } else { *p });
                                r
                            }
                            None => t,
                        })
                        .collect();
                    with_kw(&out, "skip", Some(skip_value(&tuples)))
                };
                let mut items = vec![l[0].clone()];
                items.extend(out);
                set_node(sx, &leaf, with_node(&e, Node::List(items.into())))
            }
            _ => Err(fail(&format!("There is no item {} to move up.", pos))),
        }
    })
}

fn add_field(src: &[S], node: &[String], name: &str, value: &S) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    edit_scope(src, &sp, &mut |sx| {
        let e = get_node(sx, &leaf)?;
        let key = if matches!(e.node, Node::Map(_)) { ArgKey::Field(name.to_owned()) } else { ArgKey::Kw(name.to_owned()) };
        if arg_get(&e, &key).is_some() {
            return Err(fail(&format!("There is a field {} already.", name)));
        }
        reorder(&set_node(sx, &leaf, arg_set(&e, &key, Some(value.clone()))?)?)
    })
}

fn add_node(src: &[S], scope: &[String], name: &str, expr: &S) -> R<Vec<S>> {
    let first = hd(scope)?;
    if !valid_name(name) || sym_list(&root_form(src, first)?).iter().any(|n| n == name) || w::name_taken(name) {
        return Err(fail("Pick a new lowercase name that is not used in this graph."));
    }
    edit_scope(src, scope, &mut |sx| {
        let sc = ensure(sx);
        let mut ps = sc.ps.clone();
        ps.push((sym(name), expr.clone()));
        let (body, expr) = if scope.len() == 1 { attach(&sc, &ps, name, expr)? } else { (rebuild(&sc, &ps, sc.res.clone()), expr.clone()) };
        let body = match scope_of(&body) {
            Some(b) => {
                let ps2: Vec<(S, S)> = b.ps.iter().map(|(p, v)| if p.node == Node::Sym(name.to_owned()) { (p.clone(), expr.clone()) } else { (p.clone(), v.clone()) }).collect();
                rebuild(&b, &ps2, b.res.clone())
            }
            None => body,
        };
        reorder(&body)
    })
}

fn duplicate(src: &[S], nodes: &[Path]) -> R<Vec<S>> {
    let (sp, names) = duplicate_plan(src, nodes)?;
    edit_scope(src, &sp, &mut |sx| {
        let sc = ensure(sx);
        let selected: Vec<(S, S)> = sc.ps.iter().filter(|(p, _)| names.iter().any(|(k, _)| *k == pat_key(p))).cloned().collect();
        if selected.len() != names.len() {
            return Err(fail("A selected node no longer exists."));
        }
        let rename_all = |e: &S| names.iter().fold(e.clone(), |e, (old, fr)| rename_ref(old, fr, &e));
        let mut copies = Vec::new();
        for (p, e) in &selected {
            match &p.node {
                Node::Sym(n) => {
                    let fr = names.iter().find(|(k, _)| k == n).map(|x| x.1.clone()).ok_or_else(|| fail("The edit failed: Not_found"))?;
                    let mut np = with_node(p, Node::Sym(fr));
                    *np.notes_mut() = Vec::new();
                    copies.push((np, rename_all(e)));
                }
                _ => return Err(fail("Only a named node can be duplicated.")),
            }
        }
        let last = selected.iter().filter_map(|(p, _)| find_pair(&sc, &pat_key(p))).fold(0usize, |a, b| a.max(b));
        reorder(&rebuild(&sc, &insert_at(&sc.ps, last as i64 + 1, &copies), sc.res.clone()))
    })
}

fn regroup(e: &S, leaves: &[String], name: &str, first: &mut bool) -> R<S> {
    let e = try_map_children(&mut |c| regroup(c, leaves, name, &mut *first), e)?;
    if head_sym(&e) != Some("scene/merge") {
        return Ok(e);
    }
    let kids = children(&e);
    let picked: Vec<i64> = positional(&kids[1..])
        .iter()
        .enumerate()
        .filter(|(_, c)| matches!(&c.node, Node::Sym(n) if leaves.contains(n)))
        .map(|(i, _)| i as i64)
        .collect();
    match picked.split_first() {
        None => Ok(e),
        Some((at, rest)) => {
            let mut e = if *first { arg_set(&e, &ArgKey::Pos(*at), Some(sym(name)))? } else { arg_set(&e, &ArgKey::Pos(*at), None)? };
            *first = false;
            for i in rest.iter().rev() {
                e = arg_set(&e, &ArgKey::Pos(*i), None)?;
            }
            Ok(e)
        }
    }
}

// the selected scene objects leave their merge for a new merge `name`, which takes the
// place of the first of them
fn group_merge(src: &[S], nodes: &[Path], name: &str) -> R<Vec<S>> {
    let sp = scope_path_of(nodes)?;
    let leaves = dedup(&nodes.iter().map(|n| split_node(n).map(|x| x.1)).collect::<R<Vec<_>>>()?);
    if leaves.len() < 2 {
        return Err(fail("Select two or more objects to group."));
    }
    if !valid_name(name) || sym_list(&root_form(src, hd(&sp)?)?).iter().any(|n| n == name) || w::name_taken(name) {
        return Err(fail("Pick a new lowercase name that is not used in this graph."));
    }
    let mut first = true;
    let grouped = with_root(src, hd(&sp)?, &mut |r| regroup(r, &leaves, name, &mut first))?;
    if first {
        return Err(fail("None of those objects is in a merge."));
    }
    edit_scope(&grouped, &sp, &mut |sx| {
        let sc = ensure(sx);
        let mut ps = sc.ps.clone();
        ps.push((sym(name), call("scene/merge", leaves.iter().map(|l| sym(l)).collect())));
        reorder(&rebuild(&sc, &ps, sc.res.clone()))
    })
}

fn remove_graph(src: &[S], name: &str) -> R<Vec<S>> {
    let items = workspace_parts(src)?.1;
    if !items.iter().any(|i| root_name(i).as_deref() == Some(name)) {
        return Err(fail(&format!("No graph or definition {}.", name)));
    }
    let readers = graph_readers(&items, name);
    if !readers.is_empty() {
        return Err(fail(&format!("{} is still read by {}.", name, readers.join(", "))));
    }
    map_items(src, &mut |items| Ok(items.into_iter().filter(|i| root_name(i).as_deref() != Some(name)).collect()))
}

fn rename_graph(src: &[S], name: &str, to: &str) -> R<Vec<S>> {
    let items = workspace_parts(src)?.1;
    if !items.iter().any(|i| root_name(i).as_deref() == Some(name)) {
        return Err(fail(&format!("No graph {}.", name)));
    }
    if items.iter().any(|i| root_name(i).as_deref() == Some(to)) {
        return Err(fail(&format!("{} is already a graph.", to)));
    }
    map_items(src, &mut |items| {
        Ok(items
            .into_iter()
            .map(|item| {
                let item = rename_graph_ref(name, to, &item);
                if root_name(&item).as_deref() != Some(name) {
                    return item;
                }
                match &item.node {
                    Node::List(l) if l.len() >= 2 => {
                        let mut out = vec![l[0].clone(), with_node(&l[1], Node::Sym(to.to_owned()))];
                        out.extend(l[2..].iter().cloned());
                        with_node(&item, Node::List(out.into()))
                    }
                    _ => item,
                }
            })
            .collect())
    })
}

fn set_graph(src: &[S], name: &str, form: &S) -> R<Vec<S>> {
    // the whole `(graph name ...)` form: replaced, or appended when the workspace has none
    if root_name(form).as_deref() != Some(name) {
        return Err(fail(&format!("That form is not the graph {}.", name)));
    }
    let exists = workspace_parts(src)?.1.iter().any(|i| root_name(i).as_deref() == Some(name));
    map_items(src, &mut |items| {
        if exists {
            Ok(items.into_iter().map(|i| if root_name(&i).as_deref() == Some(name) { form.clone() } else { i }).collect())
        } else {
            let mut items = items;
            items.push(form.clone());
            Ok(items)
        }
    })
}

/// The candidate rewrites of an op, tried in order.
pub fn rewrite(src: &[S], op: &Op, fallback: &Option<S>) -> R<Vec<Attempt>> {
    match op {
        Op::Wrap { nodes, loop_ } => wrap_attempts(src, nodes, *loop_, fallback),
        _ => Ok(vec![Attempt::Plain(op.clone(), fallback.clone())]),
    }
}

/// Run one candidate.
pub fn run_attempt(src: &[S], a: &Attempt) -> R<Vec<S>> {
    match a {
        Attempt::Refuse(m) => Err(fail(m)),
        Attempt::Wrap { sp, nodes, build } => wrap_attempt(src, sp, nodes, build),
        Attempt::Plain(op, fallback) => match op {
            Op::AddArm { node, after } => add_arm(src, node, *after, fallback),
            Op::DeleteArm { node, index } => delete_arm(src, node, *index),
            Op::SetArg { node, key, sub, value } => set_arg(src, node, key, sub, value),
            Op::Connect { node, key, src: name, iter } => connect(src, node, key, name, *iter),
            Op::Disconnect { node, key, fallback } => disconnect(src, node, key, fallback),
            Op::SetInputDefault { form, input, value } => set_input_default(src, form, input, value),
            Op::Unfold { node, key, sub } => unfold(src, node, key, sub),
            Op::FoldInto { node } => fold_into(src, node),
            Op::DeleteNodes { nodes } => delete_nodes(src, nodes),
            Op::Rename { node, to_ } => rename(src, node, to_),
            Op::Hoist { node } => hoist(src, node),
            Op::Wrap { .. } => Err(fail("Wrap has its own candidates.")),
            Op::MakeLocalFn { nodes } => make_local_fn(src, nodes),
            Op::MakeMacro { nodes, name, holes } => make_macro(src, nodes, name, holes),
            Op::MakeDefn { nodes, name, context, params } => make_defn(src, nodes, name, context, params),
            Op::InlineMacro { node } => inline_macro(src, node),
            Op::InlineDefn { node } => inline_defn(src, node),
            Op::MoveBinding { node, by } => move_binding(src, node, *by),
            Op::MoveStep { node, earlier } => move_step(src, node, *earlier),
            Op::ToggleBypass { node } => toggle_bypass(src, node),
            Op::SetNote { node, text } => set_note(src, node, text),
            Op::AddItem { node } => add_item(src, node),
            Op::MoveItem { node, pos } => move_item(src, node, *pos),
            Op::AddField { node, name, value } => add_field(src, node, name, value),
            Op::AddNode { scope, name, expr } => add_node(src, scope, name, expr),
            Op::Duplicate { nodes } => duplicate(src, nodes),
            Op::GroupMerge { nodes, name } => group_merge(src, nodes, name),
            Op::RemoveGraph { name } => remove_graph(src, name),
            Op::RenameGraph { name, to_ } => rename_graph(src, name, to_),
            Op::SetGraph { name, form } => set_graph(src, name, form),
        },
    }
}

// ---- the public functions ----

/// The history label: "Repeat", "Iterate", "Unfold", "Make macro", ...
pub fn label(op: &Op) -> &'static str {
    match op {
        Op::SetArg { .. } => "Edit value",
        Op::Connect { .. } => "Connect",
        Op::Disconnect { .. } => "Disconnect",
        Op::SetInputDefault { .. } => "Input default",
        Op::Unfold { .. } => "Unfold",
        Op::FoldInto { .. } => "Fold",
        Op::Wrap { loop_: Loop::For, .. } => "Repeat",
        Op::Wrap { loop_: Loop::Fold, .. } => "Iterate",
        Op::Wrap { loop_: Loop::Map, .. } => "Wrap in map",
        Op::Wrap { loop_: Loop::Filter, .. } => "Wrap in filter",
        Op::InlineDefn { .. } => "Inline function",
        Op::MoveBinding { .. } => "Move binding",
        Op::MoveStep { .. } => "Move step",
        Op::Wrap { loop_: Loop::If, .. } => "Wrap conditional",
        Op::AddArm { .. } => "Add arm",
        Op::DeleteArm { .. } => "Delete arm",
        Op::Hoist { .. } => "Move out",
        Op::Rename { .. } => "Rename",
        Op::MakeLocalFn { .. } => "Make function",
        Op::MakeDefn { .. } => "Make reusable function",
        Op::MakeMacro { .. } => "Make macro",
        Op::InlineMacro { .. } => "Inline macro",
        Op::ToggleBypass { .. } => "Bypass",
        Op::SetNote { .. } => "Note",
        Op::AddItem { .. } => "Add item",
        Op::MoveItem { .. } => "Move item",
        Op::AddField { .. } => "Add field",
        Op::AddNode { .. } => "Add node",
        Op::DeleteNodes { .. } => "Delete",
        Op::SetGraph { .. } => "Edit graph",
        Op::Duplicate { .. } => "Duplicate",
        Op::RemoveGraph { .. } => "Remove graph",
        Op::RenameGraph { .. } => "Rename graph",
        Op::GroupMerge { .. } => "Group",
    }
}

fn key_text(k: &ArgKey) -> String {
    match k {
        ArgKey::Whole => String::new(),
        ArgKey::Pos(i) => i.to_string(),
        ArgKey::Kw(k) | ArgKey::Field(k) => k.clone(),
        ArgKey::Bv(i, j) => format!("{}.{}", i, j),
        ArgKey::Arm(i) => if *i < 0 { "else".to_owned() } else { format!("then{}", i) },
        ArgKey::Cell(i) => format!("cell{}", i),
    }
}

/// A history merge key for a gesture that repeats while dragged (scrubbing an argument), `None` for everything else.
pub fn gesture(op: &Op) -> Option<String> {
    match op {
        Op::SetArg { node, key, sub, .. } => Some(format!(
            "scrub:{}:{}:{}",
            node.join("/"),
            key_text(key),
            sub.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(".")
        )),
        Op::SetNote { node, .. } => Some(format!("note:{}", node.join("/"))),
        Op::SetInputDefault { form, input, .. } => Some(format!("scrub:input:{}:{}", form, input)),
        _ => None,
    }
}

#[derive(Clone, Debug)]
pub struct MacroDraft {
    pub literals: Vec<(Vec<i64>, S)>,
    pub free: Vec<String>,
    pub name: String,
}

/// What `MakeMacro` over these nodes would template, for the dialog that picks its holes.
pub fn macro_draft(src: &[S], nodes: &[Path]) -> Result<MacroDraft, Diagnostic> {
    let sp = scope_path_of(nodes)?;
    let root = hd(&sp)?.clone();
    let rootf = root_form(src, &root)?;
    let mut draft = None;
    edit_scope(src, &sp, &mut |sx| {
        let x = select(sx, nodes, "A macro")?;
        draft = Some(MacroDraft {
            literals: literals(&body_of(&x)),
            free: outside_names(&rootf, &x),
            name: fresh_name(src, &root, &format!("{}_tpl", x.out_name)),
        });
        Ok(sx.clone())
    })?;
    draft.ok_or_else(|| fail("The edit failed: Invalid_argument(\"option is None\")"))
}

#[derive(Clone, Debug)]
pub struct DefnDraft {
    pub free: Vec<String>,
    pub name: String,
}

/// What `MakeDefn` over these nodes needs typed: the outside names.
pub fn defn_draft(src: &[S], nodes: &[Path]) -> Result<DefnDraft, Diagnostic> {
    let sp = scope_path_of(nodes)?;
    let root = hd(&sp)?.clone();
    let rootf = root_form(src, &root)?;
    let mut draft = None;
    edit_scope(src, &sp, &mut |sx| {
        let x = select(sx, nodes, "A function")?;
        draft = Some(DefnDraft { free: outside_names(&rootf, &x), name: fresh_name(src, &root, &x.out_name) });
        Ok(sx.clone())
    })?;
    draft.ok_or_else(|| fail("The edit failed: Invalid_argument(\"option is None\")"))
}

/// The `MakeMacro` a dialog's answer means: entry `i` says whether literal `i` is a hole and how it is named.
pub fn macro_op(draft: &MacroDraft, nodes: &[Path], name: &str, choices: &[(bool, String)]) -> Op {
    let holes = draft
        .literals
        .iter()
        .take(choices.len())
        .enumerate()
        .filter_map(|(i, (path, _))| if choices[i].0 { Some((path.clone(), choices[i].1.clone())) } else { None })
        .collect();
    Op::MakeMacro { nodes: nodes.to_vec(), name: name.to_owned(), holes }
}

pub type Checker = dyn Fn(&Catalog, &[S]) -> (Option<w::Workspace>, Vec<Diagnostic>);

fn check(ops: &[FlowOp], library: bool, checker: Option<&Checker>, catalog: &Catalog, forms: &[S]) -> R<(Vec<S>, w::Workspace)> {
    let (text, _) = lisp::print(forms);
    let forms = s::parse(&text)?;
    let (ws, ds) = match checker {
        Some(c) => c(catalog, &forms),
        None => w::check(ops, library, false, None, catalog, &forms),
    };
    match ws {
        Some(ws) => Ok((forms, ws)),
        None => Err(ds
            .iter()
            .find(|d| d.severity == Severity::ErrorSeverity)
            .cloned()
            .unwrap_or_else(|| fail("The edit does not check."))),
    }
}

fn find_term(term: &w::Term, path: &[String]) -> Option<Ty> {
    if term.path.as_deref() == Some(path) {
        return Some(term.ty.clone());
    }
    crate::projection::subterms(term).iter().find_map(|c| find_term(c, path))
}

/// The rewritten source (printed and parsed again) and its check. An op that has alternatives
/// (`Wrap`) tries them in order and reports the first diagnostic when none checks.
pub fn apply_checked_with(ops: &[FlowOp], library: bool, checker: Option<&Checker>, catalog: &Catalog, src: &[S], op: &Op) -> R<(Vec<S>, w::Workspace)> {
    let path: Option<Vec<String>> = match op {
        Op::Wrap { nodes, loop_: Loop::If } => {
            let mut path = None;
            if let Ok(scope) = scope_path_of(nodes) {
                let _ = edit_scope(src, &scope, &mut |sx| {
                    let selection = select(sx, nodes, "Wrap conditional")?;
                    let mut p = scope.clone();
                    p.push(selection.out_name.clone());
                    path = Some(p);
                    Ok(sx.clone())
                });
            }
            path
        }
        Op::AddArm { node, .. } => Some(node.clone()),
        _ => None,
    };
    let fallback = path.and_then(|path| {
        let (ws, _) = match checker {
            Some(c) => c(catalog, src),
            None => w::check(ops, library, false, None, catalog, src),
        };
        let ws = ws?;
        let ty = ws.graphs.iter().chain(ws.defs.iter()).find_map(|g| find_term(&g.body, &path))?;
        branch_default(&ty)
    });
    let candidates = rewrite(src, op, &fallback)?;
    let mut err: Option<Diagnostic> = None;
    for a in &candidates {
        match run_attempt(src, a) {
            Err(d) => {
                if err.is_none() {
                    err = Some(d);
                }
            }
            Ok(forms) => match check(ops, library, checker, catalog, &forms) {
                Ok(ok) => return Ok(ok),
                Err(d) => {
                    if err.is_none() {
                        err = Some(d);
                    }
                }
            },
        }
    }
    Err(err.unwrap_or_else(|| fail("The edit failed: Invalid_argument(\"option is None\")")))
}

pub fn apply_checked(catalog: &Catalog, src: &[S], op: &Op) -> R<(Vec<S>, w::Workspace)> {
    apply_checked_with(&[], false, None, catalog, src, op)
}

/// One rewrite of the source, checked.
pub fn apply_rewrite(ops: &[FlowOp], library: bool, checker: Option<&Checker>, catalog: &Catalog, attempt: &mut dyn FnMut() -> R<Vec<S>>) -> R<(Vec<S>, w::Workspace)> {
    let forms = attempt()?;
    check(ops, library, checker, catalog, &forms)
}

pub fn apply(catalog: &Catalog, src: &[S], op: &Op) -> R<Vec<S>> {
    apply_checked(catalog, src, op).map(|x| x.0)
}

// the leaf of `p` at the depth of `node`'s, when `p` is `node`, a node nested in it, or below either
fn under(node: &[String], p: &[String]) -> Option<(usize, String)> {
    if node.is_empty() {
        return None;
    }
    let k = node.len() - 1;
    if p.len() > k && has_prefix(&node[..k], p) {
        let leaf = &node[k];
        let at = &p[k];
        if at == leaf {
            Some((k, String::new()))
        } else if at.starts_with(&format!("{}#", leaf)) {
            Some((k, at[leaf.len()..].to_owned()))
        } else {
            None
        }
    } else {
        None
    }
}

fn relabel(k: usize, leaf: &str, p: &[String]) -> Path {
    p.iter().enumerate().map(|(i, s)| if i == k { leaf.to_owned() } else { s.clone() }).collect()
}

fn nested_node(n: &[String]) -> bool {
    split_node(n).map(|x| nested(&x.1)).unwrap_or(false)
}

/// Where a layout key goes after the op: `Rename` rewrites the prefix, `Hoist` drops the owner
/// segment, `DeleteNodes` drops the deleted nodes' keys (`None`), everything else keeps it.
pub fn remap(op: &Op, p: &[String]) -> Option<Path> {
    match op {
        Op::AddArm { node, after: index } | Op::DeleteArm { node, index } => {
            let removed = matches!(op, Op::DeleteArm { .. });
            match under(node, p) {
                Some((depth, suffix)) if suffix.starts_with("#then") => {
                    let tail = &suffix[5..];
                    let (digits, rest) = match tail.find('#') {
                        Some(i) => (&tail[..i], &tail[i..]),
                        None => (tail, ""),
                    };
                    let arm: i64 = if digits.is_empty() { 0 } else { digits[1..].parse::<i64>().unwrap_or(1) - 1 };
                    if removed && arm == *index {
                        None
                    } else {
                        let arm = if removed && arm > *index {
                            arm - 1
                        } else if !removed && arm > *index {
                            arm + 1
                        } else {
                            arm
                        };
                        let key = if arm == 0 { "#then".to_owned() } else { format!("#then~{}", arm + 1) };
                        Some(relabel(depth, &format!("{}{}{}", node[depth], key, rest), p))
                    }
                }
                _ => Some(p.to_vec()),
            }
        }
        Op::Rename { node, to_ } if under(node, p).is_some() => {
            let (k, rest) = under(node, p).unwrap();
            Some(relabel(k, &format!("{}{}", to_, rest), p))
        }
        Op::DeleteNodes { nodes } if nodes.iter().any(|n| nested_node(n) && under(n, p).is_some()) => {
            // the nodes nested in a deleted nested node: the first input moves up to its place
            let n = nodes.iter().find(|n| nested_node(n) && under(n, p).is_some()).unwrap();
            match under(n, p) {
                Some((k, rest)) if rest.starts_with("#0") => Some(relabel(k, &format!("{}{}", n[k], &rest[2..]), p)),
                _ => None,
            }
        }
        Op::RenameGraph { name, to_ } if has_prefix(&[name.clone()], p) => {
            let mut r = vec![to_.clone()];
            r.extend(p[1..].iter().cloned());
            Some(r)
        }
        Op::Hoist { node } if has_prefix(node, p) && node.len() >= 3 => {
            let k = node.len() - 2;
            Some(p.iter().enumerate().filter(|(i, _)| *i != k).map(|(_, s)| s.clone()).collect())
        }
        Op::DeleteNodes { nodes } if nodes.iter().any(|n| has_prefix(n, p)) => None,
        _ => Some(p.to_vec()),
    }
}

pub fn arg_of(e: &S, key: &ArgKey) -> Option<S> {
    arg_get(e, key)
}

/// The node calls written in the inputs of a call, with the input each is written in.
pub fn nested_nodes(e: &S) -> Vec<(ArgKey, S)> {
    match &e.node {
        Node::List(l) if !l.is_empty() && (!e.meta().is_empty() || !is_zone(e)) => {
            let args = s::attribute_args(head_sym(e).unwrap_or(""), &l[1..]);
            let mut out: Vec<(ArgKey, S)> = positional(&args).into_iter().enumerate().map(|(i, a)| (ArgKey::Pos(i as i64), a)).collect();
            out.extend(keywords(&args).into_iter().map(|(k, v)| (ArgKey::Kw(k), v)));
            out.into_iter().filter(|(_, a)| node_call(a)).collect()
        }
        _ => Vec::new(),
    }
}

/// A leaf as the binding (or `@result`) and the inputs down to the nested node.
pub fn leaf_keys(leaf: &str) -> Option<(String, Vec<ArgKey>)> {
    split_leaf(leaf).ok()
}

/// The argument `key` of the binding `node`, as written.
pub fn arg_text(src: &[S], node: &[String], key: &ArgKey) -> Option<S> {
    let (sp, leaf) = split_node(node).ok()?;
    let mut found = None;
    let _ = edit_scope(src, &sp, &mut |sx| {
        found = arg_get(&get_node(sx, &leaf)?, key);
        Ok(sx.clone())
    });
    found
}

/// The paths the copies of `Duplicate { nodes }` get, in order; empty when it cannot apply.
pub fn duplicated(src: &[S], nodes: &[Path]) -> Vec<Path> {
    match duplicate_plan(src, nodes) {
        Err(_) => Vec::new(),
        Ok((sp, names)) => names
            .into_iter()
            .map(|(_, fr)| {
                let mut p = sp.clone();
                p.push(fr);
                p
            })
            .collect(),
    }
}

