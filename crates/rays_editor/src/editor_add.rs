//! Port of lib/rays_editor/editor_add.ml: what the add menu adds. A kind picked in the node menu
//! becomes one `AddNode` at the selected zone (else the graph body), wired to the selected node
//! when the kind takes a geometry input; the language's forms and "Value" entries are cards with a
//! small working expression to edit in place. Bundle C.
#![allow(non_camel_case_types)]
use flow_syntax::symbol::Symbol;
use editor_document::document::Level;
use editor_document::scene_sync;
use flow_check::op;
use flow_check::workspace as w;
use flow_graph::flow_edit as fe;
use flow_graph::projection as p;
use flow_syntax::context::{self, Context};
use flow_syntax::syntax::{self as s, Node as SNode, Syntax};
use flow_syntax::ty::{self, Ty};
use pxui_graph::node_menu::Entry;

use pxui_graph::scope_state as scope;
use sop::edit_graph::InputRequirement;

use crate::editor_inspect::{kind, kinds, new_graph, new_material};
use crate::editor_model::*;
use crate::editor_panes::graph_name;

type S = Syntax;

fn sym(name: &str) -> S {
    s::make(s::Draft::Sym(name.to_owned()))
}
fn num(n: &str) -> S {
    s::make(s::Draft::Num(n.to_owned()))
}
fn parse_one(text: &str) -> S {
    s::parse(text).expect("a form of the add menu parses").remove(0)
}

/// The binding a selected path ends in, none for a keyword input, a result or a nested call.
pub fn binding_name(path: &[Symbol]) -> Option<String> {
    let last = path.last()?;
    if last.is_empty() || last.starts_with(':') || last.starts_with('@') || last.contains('#') {
        None
    } else {
        Some(last.to_string())
    }
}

/// Does `e` read the name.
pub fn reads(name: &str, e: &S) -> bool {
    match &e.node {
        SNode::Sym(n) => e.name(*n) == name,
        _ => e.children().iter().any(|&id| reads(name, &e.child(id))),
    }
}

/// Where a node is added from the list or the pane: the graph the level shows, else the scene or
/// World graph that adding an object or a layer creates.
pub fn add_target<'a>(value: &ModelView<'a>) -> Option<(String, Context)> {
    let ws = &value.doc.workspace.ws;
    match graph_name(value) {
        Some(name) => {
            let ctx = ws.checked.graphs.iter().find(|g| g.name == name).map(|g| g.context).unwrap_or_else(context::sop);
            Some((name, ctx))
        }
        None => match pane_level(value) {
            Level::Scene => Some(("scene".to_owned(), context::scene())),
            Level::Inside(id) if kind(value, id).as_deref() == Some("world") => Some(("world".to_owned(), context::world())),
            _ => None,
        },
    }
}

/// The language's forms, which are not operators: the menu adds each as a card with a small
/// working expression to edit in place. Section of the Value category, name in the menu, the
/// expression it starts as, the binding's name.
pub const FORMS: [(&str, &str, &str, &str); 17] = [
    ("Frame", "state", "(state [previous 0.0] (+ previous (frame/dt)))", "state"),
    ("Branch", "if", "(if true 1.0 0.0)", "choice"),
    ("Branch", "cond", "(cond false 1.0 :else 0.0)", "choice"),
    ("Branch", "case", "(case 0 0 1.0 :else 0.0)", "choice"),
    ("Loop", "for", "(for [i (range 4)] i)", "each"),
    ("Loop", "fold", "(fold [total 0.0] [i (range 4)] (+ total i))", "folded"),
    ("Loop", "iterate", "(iterate 3 0.0 (fn [x] (+ x 1.0)))", "iterated"),
    ("Loop", "scan", "(scan [total 0.0] [i (range 4)] (+ total i))", "steps"),
    ("Loop", "sum", "(sum [i (range 4)] i)", "total"),
    ("Function", "fn", "(fn [x] x)", "function"),
    ("Function", "map", "(map (fn [x] x) (range 4))", "mapped"),
    ("Function", "filter", "(filter (fn [x] (> x 1)) (range 4))", "kept"),
    ("Function", "reduce", "(reduce (fn [total x] (+ total x)) 0 (range 4))", "reduced"),
    ("Function", "sort-by", "(sort-by (fn [x] x) (range 4))", "sorted"),
    ("Data", "list", "(list 1.0 2.0 3.0)", "items"),
    ("Data", "record", "{:a 1.0 :b 2.0}", "record"),
    ("Data", "let*", "(let* [a 1.0] a)", "scope"),
];

fn value_entry(output: Option<Ty>, sub: &str, key: &str, label: &str) -> Entry {
    let output = match (output, sub) {
        (Some(ty), _) => ty,
        (None, "Compare") => Ty::Bool,
        (None, "Text") => Ty::Text,
        (None, "Convert") if key == "=int" => Ty::Int,
        (None, _) => Ty::Float,
    };
    Entry { key: key.to_owned(), label: label.to_owned(), category: vec!["Value".to_owned(), sub.to_owned()], arity: 0, context: "value".to_owned(), output, off: None }
}

/// The menu's "Value" entries: a value is a binding with an expression (a number, the time, an
/// operator call), which the other nodes read by name ("=number", "=t", "=+", ...).
pub fn value_entries() -> Vec<Entry> {
    let value_ops = op::of_context(&[], &context::value());
    let categorize = |name: &str| -> String { value_ops.iter().find(|o| o.name == name).map(|o| o.category.clone()).expect("a value op has a category") };
    let mut out = vec![
        value_entry(None, "Math", "=number", "Number"),
        value_entry(None, "Math", "=t", "Time (t)"),
        value_entry(Some(Ty::Vec3), "Math", "=vec3", "Vector"),
        value_entry(None, "Text", "=text", "Text"),
        value_entry(None, "Text", "=str", "str"),
    ];
    for (sub, form, _, _) in FORMS.iter() {
        let output = match *sub {
            "Loop" | "Data" | "Function" => Ty::List(Box::new(Ty::Float)),
            _ => Ty::Float,
        };
        out.push(value_entry(Some(output), sub, &format!("={form}"), form));
    }
    for o in w::value_ops() {
        if FORMS.iter().any(|(_, form, _, _)| *form == o) {
            continue;
        }
        let output = if o == "array/max_length" { Some(Ty::Int) } else { None };
        out.push(value_entry(output, &categorize(&o), &format!("={o}"), &o));
    }
    out
}

/// The expression and the name a "Value" entry makes.
pub fn value_expression(ops: &[op::Op], wire: Option<(String, Ty)>, context: &Context, key: &str) -> (S, String) {
    let o = &key[1..];
    match o {
        "number" => (num("1.0"), "value".to_owned()),
        "t" => (sym("t"), "time".to_owned()),
        "vec3" => {
            let mut builder = s::SyntaxBuilder::new();
            let items = (0..3).map(|_| builder.push_draft(0, s::Draft::Num("0.0".into()), Default::default(), None)).collect();
            let id = builder.push_draft(0, s::Draft::Vec(items), Default::default(), None);
            (builder.freeze().handle(id), "vector".to_owned())
        },
        "text" => (s::make(s::Draft::Str(b"text".to_vec())), "text".to_owned()),
        "str" => {
            let mut builder = s::SyntaxBuilder::new();
            let items = [s::Draft::Sym("str".into()), s::Draft::Str(b"text".to_vec())].into_iter()
                .map(|draft| builder.push_draft(0, draft, Default::default(), None)).collect();
            let id = builder.push_draft(0, s::Draft::List(items), Default::default(), None);
            (builder.freeze().handle(id), "text".to_owned())
        },
        "array/index_of" => (parse_one("(array/index_of (array/int 0) (array/int 0))"), "matched".to_owned()),
        "array/scatter_add" => (parse_one("(array/scatter_add (array/int 0) (array/float 0) 0)"), "added".to_owned()),
        "sop/with_attr" => (
            parse_one(
                "(sop/with_attr (sop/mesh :points (array/vec3 0) :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0)) \"attribute\" (array/vec3 0))",
            ),
            "attribute".to_owned(),
        ),
        "sop/mesh" | "sop/remap" | "sop/concat" => {
            let mesh = "(sop/mesh :points (array/vec3 0) :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0))";
            let (text, name) = if o == "sop/mesh" {
                (mesh.to_owned(), "mesh")
            } else if o == "sop/concat" {
                (format!("(sop/concat (list {mesh} {mesh}))"), "concatenated")
            } else {
                (format!("(sop/remap {mesh} {mesh} :points (array/int 0) :vertices (array/int 0) :primitives (array/int 0))"), "remap")
            };
            (parse_one(&text), name.to_owned())
        }
        "sop/mesh_planes" => (
            parse_one(
                "(sop/mesh_planes :points {:x (array/float 0) :y (array/float 0) :z (array/float 0)} :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0))",
            ),
            "mesh".to_owned(),
        ),
        form if FORMS.iter().any(|(_, f, _, _)| *f == form) => {
            let (_, _, text, name) = FORMS.iter().find(|(_, f, _, _)| *f == form).unwrap();
            (parse_one(text), (*name).to_owned())
        }
        o => {
            let signature = w::op_signature(ops, context, o);
            let mut builder = s::SyntaxBuilder::new();
            // the selected node feeds the first input of its type
            let mut wired = false;
            let mut args = vec![builder.push_draft(0, s::Draft::Sym(o.into()), Default::default(), None)];
            if let Some(sig) = &signature {
                for (label, ty) in &sig.pos {
                    let arg = match (&wire, fe::default_for(ty, label, None)) {
                        (Some((name, wty)), _) if !wired && ty::fits(wty, ty) => {
                            wired = true;
                            builder.push_draft(0, s::Draft::Sym(name.clone()), Default::default(), None)
                        }
                        (_, Some(d)) => builder.import(&d),
                        (_, None) => match ty {
                            Ty::List(_) => {
                                let items = [s::Draft::Sym("range".into()), s::Draft::Num("4".into())].into_iter()
                                    .map(|draft| builder.push_draft(0, draft, Default::default(), None)).collect();
                                builder.push_draft(0, s::Draft::List(items), Default::default(), None)
                            },
                            _ => builder.push_draft(0, s::Draft::Num("0.5".into()), Default::default(), None),
                        },
                    };
                    args.push(arg);
                }
            }
            let id = builder.push_draft(0, s::Draft::List(args), Default::default(), None);
            (builder.freeze().handle(id), fe::value_base(o))
        }
    }
}

fn last_of(path: &[Symbol]) -> &str {
    path.last().map(|x| x.as_str()).unwrap_or("")
}

/// A kind picked in the node menu, as one `AddNode` at the selected zone (else the graph body),
/// wired to the selected node when the kind takes a geometry input.
pub fn scope_add<'a>(value: &ModelView<'a>, key: &str) -> Vec<change> {
    let target = add_target(value);
    if let Some(context) = key.strip_prefix("newgraph:") {
        // "New graph: <context>" of the add menu: a whole graph, opened once made
        return vec![change::Syntax_batch("New graph".to_owned(), vec![new_graph(value, context).1])];
    }
    let Some((graph, context)) = target else { return vec![change::Declined("Open a graph to add a node to it".to_owned())] };
    if context == context::scene() && (key == "geometry" || key == "world" || key.starts_with("of:")) {
        // composition: geometry brings its SOP graph, World its world graph, one gesture each
        let geometry = |existing: Option<&str>, label: String| vec![change::Syntax_batch(label, scene_sync::add_geometry(&value.doc, existing))];
        return if key == "geometry" {
            geometry(None, "Add geometry".to_owned())
        } else if key == "world" {
            match scene_sync::add_world(&value.doc) {
                Ok(ops) => vec![change::Syntax_batch("Add World".to_owned(), ops)],
                Err(message) => vec![change::Declined(message)],
            }
        } else {
            let g = &key[3..];
            geometry(Some(g), format!("Add geometry of {g}"))
        };
    }
    if context == context::scene() && key == "material" {
        return vec![change::Syntax_batch("New material".to_owned(), vec![new_material(value).1])];
    }
    let ws = &value.doc.workspace.ws;
    let selected = scope::selected(pane_view(value));
    if context == context::scene() && key == "merge" {
        // several selected objects move into a new merge; with none selected, an empty merge
        let name = fe::fresh_name(&ws.source, &graph, "group");
        return if selected.len() >= 2 {
            vec![change::Syntax_edit(fe::Op::GroupMerge { nodes: selected, name })]
        } else {
            let mut builder = s::SyntaxBuilder::new();
            let head = builder.push_draft(0, s::Draft::Sym("scene/merge".into()), Default::default(), None);
            let id = builder.push_draft(0, s::Draft::List(vec![head]), Default::default(), None);
            vec![change::Syntax_edit(fe::Op::AddNode { scope: vec![graph.into()], name, expr: builder.freeze().handle(id) })]
        };
    }
    let is_value = key.starts_with('=');
    let material_of = key.starts_with("of-material:");
    let kind_d = kinds(value, &context).into_iter().find(|d| d.key == key);
    let arity: i64 = match &kind_d {
        Some(d) => d.slots.len() as i64 - d.keyword_inputs.len() as i64,
        None => {
            if material_of {
                1
            } else {
                0
            }
        }
    };
    let laid = crate::editor_model::pane_laid(value);
    // a nested node (a call written in another's input) has no name to read: it gets one first
    // (`Rename` binds it), in the same undo step as the node added after it
    let hoisted: Option<(flow_check::workspace::Path, String)> = match (selected.as_slice(), laid) {
        ([path], Some(k)) if path.len() >= 2 && last_of(path).contains('#') => p::find(&k.scope, path).map(|n| {
            let base = match n.head.rfind('/') {
                Some(i) => n.head[i + 1..].to_owned(),
                None => n.head.clone(),
            };
            (path.clone(), fe::fresh_name(&ws.source, &graph, &base))
        }),
        _ => None,
    };
    let binding = |path: &[Symbol]| -> Option<String> {
        match &hoisted {
            Some((p_, name)) if p_.as_slice() == path => Some(name.clone()),
            _ => binding_name(path),
        }
    };
    let (scope_path, input): (flow_check::workspace::Path, Option<String>) = if (context == context::scene() || context == context::world()) && !is_value {
        // an object joins the scene's merge and a layer goes on top of the stack: the edit itself does that
        (vec![graph.clone().into()], None)
    } else if let [path] = selected.as_slice() {
        if path.len() >= 2 {
            (path[..path.len() - 1].to_vec(), if arity > 0 { binding(path) } else { None })
        } else {
            (vec![graph.clone().into()], None)
        }
    } else {
        // nothing selected: a kind with an input reads the graph's result, so the text still
        // checks (a required input is never left open)
        let result = match laid {
            Some(k) => match &k.scope.result {
                p::ScopeResult::Link(name) => Some(name.clone()),
                _ => None,
            },
            None => None,
        };
        (vec![graph.clone().into()], if arity > 0 { result } else { None })
    };
    let (expr, base): (S, String) = if is_value {
        let wire: Option<(String, Ty)> = match (selected.as_slice(), laid) {
            ([path], Some(k)) if path.len() >= 2 => binding(path).and_then(|name| p::find(&k.scope, path).map(|n| (name, n.ty.clone()))),
            _ => None,
        };
        value_expression(&ws.checked.ops, wire, &context, key)
    } else {
        let mut builder = s::SyntaxBuilder::new();
        let head = builder.push_draft(0, s::Draft::Sym(format!("{}/{}", w::context_name(&context), if material_of { "material" } else { key })), Default::default(), None);
        let geometry = match (&context, key, ws.checked.graphs.iter().find(|g| g.context == context::sop())) {
            (c, "geometry", Some(g)) if *c == context::scene() => {
                let items = [s::Draft::Sym("ref".into()), s::Draft::Sym(g.name.clone())].into_iter()
                    .map(|draft| builder.push_draft(0, draft, Default::default(), None)).collect();
                vec![builder.push_draft(0, s::Draft::List(items), Default::default(), None)]
            },
            _ => Vec::new(),
        };
        let mut positional = geometry.clone();
        match &input {
            Some(n) => positional.push(builder.push_draft(0, s::Draft::Sym(n.clone()), Default::default(), None)),
            None => {
                if arity > 0 && geometry.is_empty() {
                    positional.push(builder.push_draft(0, s::Draft::Sym("nil".into()), Default::default(), None));
                }
            }
        }
        let mut defaults = Vec::new();
        if let Some(d) = &kind_d {
            let mut next_position = 0usize;
            let types: Vec<String> = if d.slot_types.is_empty() { d.slots.iter().map(|_| "geometry".to_owned()).collect() } else { d.slot_types.clone() };
            for ((name, required), ty) in d.slots.iter().zip(types.iter()) {
                let supplied = if d.keyword_inputs.contains(name) {
                    false
                } else {
                    let i = next_position;
                    next_position += 1;
                    i < positional.len()
                };
                if supplied || ty == "geometry" || *required != InputRequirement::Required {
                    continue;
                }
                let fresh = |n: &str| fe::fresh_name(&ws.source, &graph, n);
                if let Some(form) = ty::of_string(ty).and_then(|t_| fe::default_for(&t_, name, Some(&fresh))) {
                    defaults.push(builder.push_draft(0, s::Draft::Kw(name.clone()), Default::default(), None));
                    defaults.push(builder.import(&form));
                }
            }
        }
        let mut items = vec![head];
        items.extend(positional);
        items.extend(defaults);
        if material_of {
            items.push(builder.push_draft(0, s::Draft::Kw("material".into()), Default::default(), None));
            let reference = [s::Draft::Sym("ref".into()), s::Draft::Sym(key[12..].into())].into_iter()
                .map(|draft| builder.push_draft(0, draft, Default::default(), None)).collect();
            items.push(builder.push_draft(0, s::Draft::List(reference), Default::default(), None));
        }
        let id = builder.push_draft(0, s::Draft::List(items), Default::default(), None);
        (builder.freeze().handle(id), if material_of { "material".to_owned() } else { key.to_owned() })
    };
    let mut name = fe::fresh_name(&ws.source, &graph, &base);
    if let Some((_, h)) = &hoisted {
        if *h == name {
            name = fe::fresh_name(&ws.source, &graph, &format!("{name}_2"));
        }
    }
    let add = fe::Op::AddNode { scope: scope_path, name: name.clone(), expr: expr.clone() };
    let rename: Vec<fe::Op> = match &hoisted {
        Some((node, to)) => vec![fe::Op::Rename { node: node.clone(), to_: to.clone() }],
        None => vec![],
    };
    // a wire pulled off an existing wire: the new node goes between its source and its sink
    let insert: Vec<fe::Op> = match (selected.as_slice(), &input, pxui_graph::scope_pane::selected_wire(pane_view(value)), laid) {
        ([path], Some(from), Some((target, wkey)), Some(k)) if reads(from, &expr) => {
            let ok = match p::find(&k.scope, &target) {
                Some(n) => n.rows.iter().any(|r| r.key == wkey && r.chip == p::Chip::Name(last_of(path).to_owned())),
                None => false,
            };
            if ok { vec![fe::Op::Connect { node: target.to_vec(), key: wkey, src: name.clone(), iter: false }] } else { vec![] }
        }
        _ => vec![],
    };
    if rename.is_empty() && insert.is_empty() {
        vec![change::Syntax_edit(add)]
    } else {
        let label = if insert.is_empty() { "Add node" } else { "Insert node" };
        let mut ops = rename;
        ops.push(add);
        ops.extend(insert);
        vec![change::Syntax_batch(label.to_owned(), ops)]
    }
}
