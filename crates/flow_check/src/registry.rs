//! Public authoring metadata shared by lookup, diagnostics and generated documentation.
use crate::{catalog, op};
use flow_syntax::{context::{self, Context}, ty::{self, Ty}};

#[derive(Clone, Debug)]
pub struct Entry {
    pub name: String,
    pub context: Option<Context>,
    pub signature: String,
    pub doc: String,
    pub example: String,
    pub hidden: bool,
}

pub const FORMS: &[(&str, &str, &str, &str)] = &[
    ("workspace", "name declarations...", "Group graphs and reusable definitions into a workspace.", "(workspace demo (graph value :context value 1))"),
    ("graph", "name [:context context] [inputs] body", "Declare a named, reactive graph.", "(graph picture :context draw (draw/background \"#182838\"))"),
    ("defn", "name [:context context] [inputs] body", "Define a reusable function with typed inputs.", "(defn twice :context value [(x : float)] (* x 2))"),
    ("defnode", "name metadata... [inputs] body", "Define a reusable node with editable parameter metadata.", "(defnode sop/identity :label \"Identity\" [(geo : geometry)] geo)"),
    ("defmacro", "name [parameters] body", "Expand syntax before checking the workspace.", "(defmacro twice [x] `(+ ~x ~x))"),
    ("let", "[name expression ...] body", "Bind names sequentially; later bindings can use earlier ones.", "(let [x 2 y (+ x 1)] (* x y))"),
    ("let*", "[name expression ...] body", "Bind names sequentially; equivalent to let.", "(let* [x 2] (+ x 1))"),
    ("ref", "graph :input value ...", "Read another graph with optional input overrides.", "(ref picture)"),
    ("for", "[name collection ...] body", "Collect a result for each item in a bounded collection.", "(for [x (range 3)] (* x 2))"),
    ("fold", "[acc initial name collection] body", "Accumulate a result over a bounded collection.", "(fold [acc 0 x (range 3)] (+ acc x))"),
    ("scan", "[acc initial name collection] body", "Collect intermediate accumulator values.", "(scan [acc 0 x (range 3)] (+ acc x))"),
    ("sum", "[name collection] body", "Add the numeric results of a bounded iteration.", "(sum [x (range 3)] x)"),
    ("state", "[name initial] body", "Retain a value between frames and compute its next value.", "(state [n 0] (+ n 1))"),
    ("if", "condition then else", "Choose one of two values using a boolean condition.", "(if true 1 2)"),
    ("with_error", "protected mapper", "Map a runtime diagnostic to a recovery value.", "(with_error 1 (fn [error] 0))"),
    ("error_rows", "value", "Expose deferred diagnostics as data rows.", "(error_rows 1)"),
    ("values", "value ...", "Return multiple named outputs from a node.", "(values :geometry nil)"),
    ("fn", "[parameters] body", "Create a pure anonymous function.", "(fn [x] (+ x 1))"),
    ("cond", "test result ... :else result", "Select the result of the first true condition.", "(cond true 1 :else 2)"),
    ("case", "value literal result ... :else result", "Match a value against literal alternatives.", "(case 1 1 2 :else 3)"),
    ("list", "value ...", "Construct a homogeneous list.", "(list 1 2 3)"),
    ("concat", "collection ...", "Join collections with a common element type.", "(concat (list 1) (list 2))"),
    ("str", "value ...", "Format and concatenate values as text.", "(str \"frame \" 1)"),
    ("get", "record :field", "Read a field from a record.", "(get {:x 1} :x)"),
    ("assoc", "record :field value ...", "Return a record with updated fields.", "(assoc {:x 1} :x 2)"),
    ("map", "function collection ...", "Apply a pure function to corresponding collection elements.", "(map (fn [x] (* x 2)) (range 3))"),
    ("filter", "predicate collection", "Keep elements for which a predicate returns true.", "(filter (fn [x] (> x 0)) (range 3))"),
    ("reduce", "function initial collection", "Reduce a collection into one accumulator value.", "(reduce (fn [a x] (+ a x)) 0 (range 3))"),
    ("sort_by", "function collection", "Sort a collection by a numeric key.", "(sort_by (fn [x] (- x)) (range 3))"),
    ("iterate", "count initial function [:until predicate]", "Apply a pure function repeatedly with a fixed maximum count.", "(iterate 3 1 (fn [x] (* x 2)))"),
    ("quote", "syntax", "Return syntax without evaluating it.", "(quote x)"),
    ("quasiquote", "syntax", "Build macro syntax with unquoted substitutions.", "(quasiquote (+ 1 2))"),
    ("unquote", "expression", "Insert evaluated syntax inside quasiquote.", "(defmacro identity [x] `(unquote x))"),
    ("unquote-splicing", "expression", "Insert a sequence of syntax forms inside quasiquote.", "(defmacro call [f args] `(~f ~@args))"),
];

pub fn op_signature(operator: &op::Op) -> String {
    let sig = &operator.signature;
    let mut parts = vec![operator.name.clone()];
    parts.extend(sig.pos.iter().map(|(name, _)| name.to_string()));
    parts.extend(sig.opt.iter().map(|(name, _)| format!("[{name}]")));
    if let Some((name, _)) = &sig.rest { parts.push(format!("{name}...")); }
    parts.extend(sig.kw.iter().map(|(name, _)| format!(":{name}")));
    format!("({})", parts.join(" "))
}

pub fn kind_signature(kind: &catalog::Kind) -> String {
    let mut parts = vec![kind.qualified.clone()];
    parts.extend(kind.slots.iter().map(|slot| {
        if slot.rest { format!("{}...", slot.name) }
        else if slot.required { slot.name.clone() } else { format!("[{}]", slot.name) }
    }));
    parts.extend(kind.parameters.iter().map(|parameter| format!(":{}", parameter.name)));
    format!("({})", parts.join(" "))
}

pub fn signature(name: &str) -> Option<String> {
    op::all().iter().find(|operator| operator.name == name).map(op_signature)
        .or_else(|| FORMS.iter().find(|entry| entry.0 == name).map(|entry| format!("({name} {})", entry.1)))
}

fn sample(ty: &Ty) -> String {
    match ty {
        Ty::Float => "1.0".into(), Ty::Int | Ty::Any => "1".into(), Ty::Bool => "true".into(),
        Ty::Text => "\"example\"".into(), Ty::Color => "\"#5aaaf0\"".into(),
        Ty::Vec2 => "[1 1]".into(), Ty::Vec3 => "[1 1 0]".into(), Ty::Vec4 => "[1 1 1 1]".into(),
        Ty::List(element) => format!("(list {})", sample(element)),
        Ty::Array(element) => format!("(array/{} 1 {})", ty::to_string(element), sample(element)),
        Ty::Record(fields) => format!("{{{}}}", fields.iter().map(|(name, ty)| format!(":{name} {}", sample(ty))).collect::<Vec<_>>().join(" ")),
        Ty::Fn(_) => "(fn [x] x)".into(),
        Ty::Named(name) => match name.as_str() {
            "geometry" => "nil".into(), "drawing" => "(draw/merge)".into(),
            "image" => "(image/noise :width 1 :height 1)".into(),
            "panel" => "(ui/canvas (draw/merge))".into(),
            _ => "nil".into(),
        },
    }
}

pub fn example(name: &str, sig: &op::Signature) -> String {
    if name == "draw/circle" { return "(draw/circle [320 180] 42.5 :fill \"#5aaaf0\")".into(); }
    let mut parts = vec![name.to_owned()];
    parts.extend(sig.pos.iter().map(|(_, ty)| sample(ty)));
    format!("({})", parts.join(" "))
}

pub fn description(name: &str, sig: &op::Signature) -> String {
    match name {
        "draw/circle" => "Draw a filled or stroked circle in logical points.".into(),
        "draw/line" => "Draw a line between two positions in logical points.".into(),
        "draw/background" => "Fill the entire canvas with a color.".into(),
        "frame/width" => "Read the canvas width in logical points.".into(),
        "frame/height" => "Read the canvas height in logical points.".into(),
        "frame/dt" => "Read elapsed seconds since the previous frame.".into(),
        "frame/index" => "Read the zero-based frame index.".into(),
        "rand/at" => "Read a deterministic random value at an index.".into(),
        "noise3" => "Sample seeded three-dimensional coherent noise.".into(),
        _ => {
            let action = name.replace(['/', '_', '-'], " ");
            let inputs = sig.pos.iter().map(|(name, ty)| format!("{name}: {}", ty::to_string(ty))).collect::<Vec<_>>();
            if inputs.is_empty() { format!("{action}; optional parameters are listed in the signature.") }
            else { format!("{action}; inputs {}.", inputs.join(", ")) }
        }
    }
}

pub fn entries(catalog: &catalog::Catalog) -> Vec<Entry> {
    let mut entries: Vec<_> = op::all().iter().map(|operator| Entry {
        name: operator.name.clone(), context: Some(operator.ctx), signature: op_signature(operator),
        doc: operator.doc.clone(), example: operator.example.clone(),
        hidden: operator.name.starts_with("int/") || operator.name.starts_with("array/") && ["array/nth", "array/count"].contains(&operator.name.as_str()),
    }).collect();
    entries.extend(FORMS.iter().map(|&(name, args, doc, example)| Entry {
        name: name.into(), context: None, signature: format!("({name} {args})"),
        doc: doc.into(), example: example.into(), hidden: name == "let*",
    }));
    entries.extend(catalog.kinds.iter().map(|kind| Entry {
        name: kind.qualified.clone(), context: Some(kind.context), signature: kind_signature(kind),
        doc: kind.description.clone(),
        example: format!("({}{})", kind.qualified, kind.slots.iter().filter(|slot| slot.required && !slot.rest).map(|slot| format!(" {}", sample(&crate::workspace::slot_ty(Some(slot), kind)))).collect::<String>()),
        hidden: false,
    }));
    entries.sort_by(|a, b| a.name.cmp(&b.name));
    entries.dedup_by(|a, b| a.name == b.name);
    entries
}

pub fn names(catalog: &catalog::Catalog) -> Vec<String> {
    let mut names = op::all().iter().map(|operator| operator.name.clone())
        .chain(FORMS.iter().map(|entry| entry.0.to_owned()))
        .chain(catalog.kinds.iter().map(|kind| kind.qualified.clone())).collect::<Vec<_>>();
    names.sort(); names.dedup(); names
}

pub fn known_prefix(catalog: &catalog::Catalog, prefix: &str) -> bool {
    prefix == "user" || context::all().iter().any(|&ctx| context::name(ctx) == prefix)
        || names(catalog).iter().any(|name| name.split_once('/').is_some_and(|(ns, _)| ns == prefix))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn authoring_diagnostics_search_all_names_and_validate_constants() {
        let catalog = catalog::Catalog { version: 0, kinds: vec![] };
        for (name, expected) in [("frame/widht", "frame/width"), ("draw/backgroud", "draw/background"), ("sqr", "sqrt")] {
            assert_eq!(catalog::suggestions(name, &catalog).first().map(String::as_str), Some(expected));
        }
        for (expression, code) in [("(draw/circle [0 0 0] 3 :fill \"notacolor\")", "E_TYPE"), ("(draw/circle [0 0 0] 3 4)", "E_ARITY"), ("(frame/widht)", "E_UNKNOWN_KIND")] {
            let source = format!("(workspace probe (graph picture :context draw {expression}))");
            let forms = flow_syntax::syntax::parse(&source).unwrap();
            let (_, diagnostics) = crate::workspace::check(&[], false, false, None, &catalog, &forms);
            let diagnostic = diagnostics.iter().find(|diagnostic| diagnostic.code == code).unwrap_or_else(|| panic!("{expression}: {diagnostics:?}"));
            assert!(diagnostic.signature.is_some(), "{diagnostic:?}");
            if code == "E_UNKNOWN_KIND" { assert_eq!(diagnostic.suggest.first().map(String::as_str), Some("frame/width")); }
        }
        let entries = entries(&catalog);
        assert!(entries.iter().all(|entry| !entry.doc.is_empty() && !entry.example.is_empty()));
    }
}
