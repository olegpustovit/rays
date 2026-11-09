//! Shared s-expression forms for settings and preset viewport state.
use flow_syntax::syntax::{make as make_node, Draft, Node, Syntax, SyntaxBuilder, SyntaxId};
use flow_syntax::lisp;

pub fn kw(name: &str) -> Syntax {
    make_node(Draft::Kw(name.to_string()))
}

pub fn number(x: f64) -> Syntax {
    make_node(Draft::Num(lisp::float(x)))
}

pub fn vec_of_numbers(xs: &[f64]) -> Syntax {
    container(Draft::Vec, xs.iter().map(|x| number(*x)).collect())
}

pub fn container(kind: fn(Vec<SyntaxId>) -> Draft, items: Vec<Syntax>) -> Syntax {
    let mut builder = SyntaxBuilder::new();
    let children = items.iter().map(|form| builder.import(form)).collect();
    let root = builder.push_draft(0, kind(children), rays_diagnostic::Span { start: 0, finish: 0 }, None);
    builder.freeze().handle(root)
}

pub fn map(pairs: Vec<(&str, Syntax)>) -> Syntax {
    let mut items = Vec::with_capacity(pairs.len() * 2);
    for (k, v) in pairs {
        items.push(kw(k));
        items.push(v);
    }
    container(Draft::Map, items)
}

pub fn print(form: &Syntax) -> Result<String, String> {
    lisp::print(std::slice::from_ref(form)).map(|(text, _)| text).map_err(|error| rays_diagnostic::to_string(&error))
}

/// OCaml's `float_of_string_opt` on a number form, finite only.
pub fn num_of(form: &Syntax) -> Option<f64> {
    match &form.node {
        Node::Num(n) => cfmt::parse_ocaml_float(form.name(*n)).filter(|v| v.is_finite()),
        _ => None,
    }
}

pub fn vec_values(form: &Syntax) -> Option<Vec<f64>> {
    match &form.node {
        Node::Vec(xs) => form.range(*xs).iter().map(|id| num_of(&form.child(*id))).collect(),
        _ => None,
    }
}

/// The value after keyword `name` in a map form.
pub fn field(form: &Syntax, name: &str) -> Option<Syntax> {
    match &form.node {
        Node::Map(items) => {
            let items = form.range(*items);
            let mut i = 0;
            while i + 1 < items.len() {
                if let Node::Kw(k) = form.arena().row(items[i]).node {
                    if form.name(k) == name {
                        return Some(form.child(items[i + 1]));
                    }
                }
                i += 2;
            }
            None
        }
        _ => None,
    }
}
