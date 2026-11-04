//! The plan's "ppx_rays SOP metadata -> plain const tables, checked against flow_manifest.sexp by a
//! test": every registered factory is rendered in the manifest's own format and must equal its
//! `(kind "sop/<key>" ...)` block of lib/flow_sop/flow_manifest.sexp.
use edit_graph::InputRequirement;
use param::{KindView, Value};

use crate::edit_graph::{self, Factory};
use crate::node::{CookMode, Elementwise, Topology};
use crate::nodes;

const MANIFEST: &str = include_str!("../../../../lib/flow_sop/flow_manifest.sexp");

fn quoted(text: &str) -> String {
    format!("\"{}\"", text.replace('\\', "\\\\").replace('"', "\\\""))
}

/// `Manifest.number`: the shortest of %.15g and %.17g that round-trips (what `Flow.Lisp.float` prints,
/// without a trailing `.0`).
fn number(v: f64) -> String {
    let short = cfmt::g(v, 15);
    if short.parse::<f64>() == Ok(v) { short } else { cfmt::g(v, 17) }
}

fn render_value(value: &Value) -> String {
    match value {
        Value::Bool(b) => format!("(bool {b})"),
        Value::Int(i) => format!("(int {i})"),
        Value::Float(f) => format!("(float {})", number(*f)),
        Value::Text(t) => format!("(text {})", quoted(&String::from_utf8_lossy(t))),
        Value::Choice(c) => format!("(choice {})", quoted(c)),
    }
}

fn render_kind(kind: &KindView) -> String {
    let opt_i = |v: Option<i64>| v.map_or("nil".to_owned(), |v| v.to_string());
    let opt_f = |v: Option<f64>| v.map_or("nil".to_owned(), number);
    match kind {
        KindView::Toggle => "(bool)".to_owned(),
        KindView::Integer(r) => {
            format!("(int (soft {} {}) (hard {} {}))", r.soft_min, r.soft_max, opt_i(r.hard_min), opt_i(r.hard_max))
        }
        KindView::Floating(r) => format!(
            "(float (soft {} {}) (hard {} {}))",
            number(r.soft_min),
            number(r.soft_max),
            opt_f(r.hard_min),
            opt_f(r.hard_max)
        ),
        KindView::Text => "(text)".to_owned(),
        KindView::Choice(labels) => format!("(choice {})", labels.iter().map(|l| quoted(l)).collect::<Vec<_>>().join(" ")),
    }
}

fn render_facts(f: &crate::node::Facts) -> String {
    let mode = match f.cook_mode {
        CookMode::Generator => "generator".to_owned(),
        CookMode::DuplicateInput(i) => format!("duplicate-input {i}"),
        CookMode::InPlace(i) => format!("in-place {i}"),
        CookMode::InstanceInput(i) => format!("instance-input {i}"),
        CookMode::Passthrough(i) => format!("passthrough {i}"),
        CookMode::Generic => "generic".to_owned(),
    };
    let list = |names: &[String]| names.iter().map(|n| quoted(n)).collect::<Vec<_>>().join(" ");
    format!(
        "(facts (cook-mode {mode}) (elementwise {}) (reads {}) (writes {}) (topology {}) (exact {}))",
        match f.elementwise {
            Elementwise::Points => "points",
            Elementwise::Primitives => "primitives",
            Elementwise::None => "none",
        },
        list(&f.reads),
        list(&f.writes),
        match f.topology {
            Topology::Preserved => "preserved",
            Topology::Changed => "changed",
        },
        f.exact
    )
}

fn render(factory: &Factory) -> String {
    let key = edit_graph::factory_key(factory);
    let slots: Vec<String> = edit_graph::factory_slot_names(factory)
        .iter()
        .zip(edit_graph::factory_inputs(factory))
        .zip(edit_graph::factory_input_types(factory))
        .map(|((name, requirement), ty)| {
            let word = match requirement {
                InputRequirement::Required => "required",
                InputRequirement::Optional => "optional",
                InputRequirement::Rest => "rest",
                InputRequirement::OptionalRest => "optional-rest",
            };
            let ty = if ty == "geometry" { String::new() } else { format!(" {ty}") };
            format!("(slot {} {word}{ty})", quoted(name))
        })
        .collect();
    let fields: Vec<String> = edit_graph::factory_fields(factory)
        .iter()
        .map(|f| {
            assert!(f.unit.is_none(), "{key}: unit rendering is not supported by this check yet");
            let folder: String = f.folder.iter().map(|p| format!(" {}", quoted(p))).collect();
            let vec3 = f.vec3.as_ref().map_or("(vec3)".to_owned(), |(g, i)| format!("(vec3 {} {i})", quoted(g)));
            let description = f.description.as_ref().map_or(String::new(), |d| format!(" (description {})", quoted(d)));
            format!(
                "(field {} {} (folder{folder}) {} {} (primary {}) {vec3}{description})",
                quoted(&f.name),
                quoted(&f.label),
                render_kind(&f.kind),
                render_value(&f.default),
                f.primary
            )
        })
        .collect();
    format!(
        "(kind {} (key {}) (operation {}) (label {}) (category {}) (slots {}) (fields {}) (outputs (output \"geo\" geometry)) {})",
        quoted(&format!("sop/{key}")),
        quoted(key),
        quoted(edit_graph::factory_operation(factory)),
        quoted(edit_graph::factory_label(factory)),
        edit_graph::factory_category(factory).iter().map(|c| quoted(c)).collect::<Vec<_>>().join(" "),
        slots.join(" "),
        fields.join(" "),
        render_facts(edit_graph::factory_facts(factory)),
    )
}

/// Whitespace-insensitive form: single spaces, none after `(` or before `)`.
fn normalize(text: &str) -> String {
    let collapsed = text.split_whitespace().collect::<Vec<_>>().join(" ");
    collapsed.replace("( ", "(").replace(" )", ")")
}

/// The balanced `(kind "sop/<key>" ...)` form of the manifest.
fn manifest_block(key: &str) -> String {
    let start = MANIFEST.find(&format!("(kind \"sop/{key}\"")).unwrap_or_else(|| panic!("manifest has no kind sop/{key}"));
    let (mut depth, mut in_string, mut escaped) = (0_i32, false, false);
    for (offset, c) in MANIFEST[start..].char_indices() {
        if in_string {
            match (escaped, c) {
                (true, _) => escaped = false,
                (false, '\\') => escaped = true,
                (false, '"') => in_string = false,
                _ => {}
            }
            continue;
        }
        match c {
            '"' => in_string = true,
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    return MANIFEST[start..start + offset + 1].to_owned();
                }
            }
            _ => {}
        }
    }
    panic!("unbalanced manifest block for sop/{key}");
}

#[test]
fn every_registered_factory_matches_the_manifest() {
    let factories = nodes::factories();
    assert!(!factories.is_empty());
    for factory in &factories {
        let key = edit_graph::factory_key(factory);
        assert_eq!(normalize(&render(factory)), normalize(&manifest_block(key)), "manifest entry of sop/{key}");
    }
}
