//! Port of lib/flow_sop/manifest.ml: the deterministic catalog snapshot (`flow_manifest.sexp`)
//! shared by the generator and the runtime check.
use flow_check::catalog as flow_catalog;
use flow_check::defnode::Defnode;
use flow_check::op::Op;
use flow_check::port_type::{self, PortType};
use flow_check::workspace;
use flow_syntax::symbol;
use flow_syntax::syntax;
use flow_syntax::ty;
use param::{FieldView, Impact, KindView, Value};
use rays_diagnostic::Diagnostic;
use sop::edit_graph::{self, Factory, InputRequirement};
use sop::node::{CookMode, Elementwise, Facts, Topology};

use crate::catalog::{self, Descriptor};

/// Deterministic catalog snapshot shared by the generator and runtime check.
pub const VERSION: i64 = 202609;

fn quote_bytes(value: &[u8]) -> String {
    let mut out: Vec<u8> = Vec::with_capacity(value.len() + 2);
    out.push(b'"');
    for &byte in value {
        match byte {
            b'"' => out.extend_from_slice(b"\\\""),
            b'\\' => out.extend_from_slice(b"\\\\"),
            b'\n' => out.extend_from_slice(b"\\n"),
            other => out.push(other),
        }
    }
    out.push(b'"');
    String::from_utf8_lossy(&out).into_owned()
}

fn quote(value: &str) -> String {
    quote_bytes(value.as_bytes())
}

fn group(name: &str, values: &[String]) -> String {
    if values.is_empty() {
        format!("({name})")
    } else {
        format!("({name} {})", values.join(" "))
    }
}

fn strings(name: &str, values: &[String]) -> String {
    let quoted: Vec<String> = values.iter().map(|v| quote(v)).collect();
    group(name, &quoted)
}

/// `Manifest.number`: `Flow.Lisp.float` without a trailing `.0`. Non-finite is a caller bug.
fn number(number: f64) -> String {
    assert!(number.is_finite(), "Manifest.number: non-finite");
    let text = flow_syntax::lisp::float(number);
    match text.strip_suffix(".0") {
        Some(short) => short.to_owned(),
        None => text,
    }
}

fn optional_int(value: Option<i64>) -> String {
    value.map_or("nil".to_owned(), |v| v.to_string())
}

fn optional_float(value: Option<f64>) -> String {
    value.map_or("nil".to_owned(), number)
}

fn literal(value: &Value) -> String {
    match value {
        Value::Bool(v) => group("bool", &[v.to_string()]),
        Value::Int(v) => group("int", &[v.to_string()]),
        Value::Float(v) => group("float", &[number(*v)]),
        Value::Text(v) => group("text", &[quote_bytes(v)]),
        Value::Choice(v) => group("choice", &[quote(v)]),
    }
}

fn field(field: &FieldView) -> String {
    let kind = match &field.kind {
        KindView::Toggle => group("bool", &[]),
        KindView::Integer(range) => group(
            "int",
            &[
                group("soft", &[range.soft_min.to_string(), range.soft_max.to_string()]),
                group("hard", &[optional_int(range.hard_min), optional_int(range.hard_max)]),
            ],
        ),
        KindView::Floating(range) => group(
            "float",
            &[
                group("soft", &[number(range.soft_min), number(range.soft_max)]),
                group("hard", &[optional_float(range.hard_min), optional_float(range.hard_max)]),
            ],
        ),
        KindView::Text => group("text", &[]),
        KindView::Choice(labels) => strings("choice", labels),
    };
    let mut parts = vec![
        quote(&field.name),
        quote(&field.label),
        strings("folder", &field.folder),
        kind,
        literal(&field.default),
        group("primary", &[field.primary.to_string()]),
        match &field.vec3 {
            None => group("vec3", &[]),
            Some((name, index)) => group("vec3", &[quote(name), index.to_string()]),
        },
    ];
    if let Some(unit) = &field.unit {
        parts.push(group("unit", &[quote(unit)]));
    }
    if let Some(text) = &field.description {
        parts.push(group("description", &[quote(text)]));
    }
    match field.impact {
        Impact::Cook => {}
        Impact::View => parts.push(group("impact", &["view".to_owned()])),
        Impact::Export => parts.push(group("impact", &["export".to_owned()])),
    }
    group("field", &parts)
}

fn outputs(ports: &[(String, PortType)]) -> String {
    let rendered: Vec<String> = ports
        .iter()
        .map(|(name, ty)| {
            let name_ty = port_type::name(ty).to_ascii_lowercase();
            let shown = if symbol::valid_name(&name_ty) { name_ty } else { quote(&name_ty) };
            group("output", &[quote(name), shown])
        })
        .collect();
    group("outputs", &rendered)
}

fn facts(facts: &Facts) -> String {
    let mode = match facts.cook_mode {
        CookMode::Generator => vec!["generator".to_owned()],
        CookMode::DuplicateInput(i) => vec!["duplicate-input".to_owned(), i.to_string()],
        CookMode::InPlace(i) => vec!["in-place".to_owned(), i.to_string()],
        CookMode::InstanceInput(i) => vec!["instance-input".to_owned(), i.to_string()],
        CookMode::Passthrough(i) => vec!["passthrough".to_owned(), i.to_string()],
        CookMode::Generic => vec!["generic".to_owned()],
    };
    group(
        "facts",
        &[
            group("cook-mode", &mode),
            group(
                "elementwise",
                &[match facts.elementwise {
                    Elementwise::Points => "points",
                    Elementwise::Primitives => "primitives",
                    Elementwise::None => "none",
                }
                .to_owned()],
            ),
            strings("reads", &facts.reads),
            strings("writes", &facts.writes),
            group(
                "topology",
                &[match facts.topology {
                    Topology::Preserved => "preserved",
                    Topology::Changed => "changed",
                }
                .to_owned()],
            ),
            group("exact", &[facts.exact.to_string()]),
        ],
    )
}

struct Entry {
    declaration: Option<Facts>,
    keyword_inputs: Vec<String>,
    properties: Vec<String>,
    qualified: String,
    key: String,
    operation: String,
    label: String,
    category: Vec<String>,
    slots: Vec<String>,
    fields: Vec<FieldView>,
    result: Vec<(String, PortType)>,
}

fn entry(e: Entry) -> String {
    let fields = if e.fields.is_empty() {
        group("fields", &[])
    } else {
        let rendered: Vec<String> = e.fields.iter().map(field).collect();
        format!("(fields\n        {})", rendered.join("\n        "))
    };
    let mut properties = vec![
        group("key", &[quote(&e.key)]),
        group("operation", &[quote(&e.operation)]),
        group("label", &[quote(&e.label)]),
        strings("category", &e.category),
        group("slots", &e.slots),
        fields,
        outputs(&e.result),
    ];
    if !e.keyword_inputs.is_empty() {
        properties.push(strings("keyword-inputs", &e.keyword_inputs));
    }
    if let Some(declaration) = &e.declaration {
        properties.push(facts(declaration));
    }
    properties.extend(e.properties);
    format!("(kind {}\n      {})", quote(&e.qualified), properties.join("\n      "))
}

fn type_word(ty: &str) -> String {
    if symbol::valid_name(ty) { ty.to_owned() } else { quote(ty) }
}

fn descriptor_entry(declaration: Option<Facts>, d: &Descriptor) -> String {
    let types: Vec<String> = if d.slot_types.is_empty() {
        d.slots.iter().map(|_| "geometry".to_owned()).collect()
    } else {
        d.slot_types.clone()
    };
    let slots: Vec<String> = d
        .slots
        .iter()
        .zip(&types)
        .map(|((name, requirement), ty)| {
            let word = match requirement {
                InputRequirement::Required => "required",
                InputRequirement::Optional => "optional",
                InputRequirement::Rest => "rest",
                InputRequirement::OptionalRest => "optional-rest",
            };
            let slot = group("slot", &[quote(name), word.to_owned()]);
            if ty == "geometry" {
                slot
            } else {
                format!("{} {})", &slot[..slot.len() - 1], type_word(ty))
            }
        })
        .collect();
    entry(Entry {
        declaration,
        keyword_inputs: d.keyword_inputs.clone(),
        properties: Vec::new(),
        qualified: d.qualified.clone(),
        key: d.key.clone(),
        operation: d.operation.clone(),
        label: d.label.clone(),
        category: d.category.clone(),
        slots,
        fields: d.fields.clone(),
        result: if d.qualified.starts_with("sop/") { vec![("geo".to_owned(), PortType::Geometry)] } else { Vec::new() },
    })
}

fn lisp_node(calls: &[String], node: &Defnode) -> String {
    let key = flow_catalog::short(&node.kind.qualified);
    let slots: Vec<String> = node
        .slots
        .iter()
        .map(|slot| {
            let ty = ty::to_string(&workspace::slot_ty(Some(slot), &node.kind));
            let word = match (slot.required, slot.rest) {
                (true, false) => "required",
                (false, false) => "optional",
                (true, true) => "rest",
                (false, true) => "optional-rest",
            };
            let mut parts = vec![quote(&slot.name), word.to_owned()];
            if ty != "geometry" {
                parts.push(type_word(&ty));
            }
            group("slot", &parts)
        })
        .collect();
    let lookup = |name: &str| -> Option<String> {
        node.fields.iter().find_map(|field| match &field.default {
            Value::Text(text) if field.name == name => Some(String::from_utf8_lossy(text).into_owned()),
            _ => None,
        })
    };
    let declaration = node.kind.facts.as_ref().map(|f| Facts {
        cook_mode: if node.slots.is_empty() { CookMode::Generator } else { CookMode::Generic },
        elementwise: match f.elementwise {
            flow_catalog::KernelElements::Points => Elementwise::Points,
            flow_catalog::KernelElements::Primitives => Elementwise::Primitives,
            flow_catalog::KernelElements::Irregular => Elementwise::None,
        },
        reads: flow_catalog::resolve_component_names(&lookup, &f.reads),
        writes: flow_catalog::resolve_component_names(&lookup, &f.writes),
        topology: if f.preserves_topology { Topology::Preserved } else { Topology::Changed },
        exact: f.exact,
    });
    entry(Entry {
        declaration,
        keyword_inputs: node.keyword_inputs.clone(),
        properties: vec![group("body", &["lisp".to_owned()]), strings("calls", calls)],
        qualified: node.kind.qualified.clone(),
        key: key,
        operation: node.operation.clone(),
        label: node.label.clone(),
        category: node.category.clone(),
        slots,
        fields: node.fields.clone(),
        result: node.kind.outputs.clone(),
    })
}

fn md5_hex(input: &[u8]) -> String {
    const S: [u32; 64] = [
        7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14,
        20, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6,
        10, 15, 21,
    ];
    const K: [u32; 64] = [0xd76aa478,0xe8c7b756,0x242070db,0xc1bdceee,0xf57c0faf,0x4787c62a,0xa8304613,0xfd469501,0x698098d8,0x8b44f7af,0xffff5bb1,0x895cd7be,0x6b901122,0xfd987193,0xa679438e,0x49b40821,0xf61e2562,0xc040b340,0x265e5a51,0xe9b6c7aa,0xd62f105d,0x02441453,0xd8a1e681,0xe7d3fbc8,0x21e1cde6,0xc33707d6,0xf4d50d87,0x455a14ed,0xa9e3e905,0xfcefa3f8,0x676f02d9,0x8d2a4c8a,0xfffa3942,0x8771f681,0x6d9d6122,0xfde5380c,0xa4beea44,0x4bdecfa9,0xf6bb4b60,0xbebfbc70,0x289b7ec6,0xeaa127fa,0xd4ef3085,0x04881d05,0xd9d4d039,0xe6db99e5,0x1fa27cf8,0xc4ac5665,0xf4292244,0x432aff97,0xab9423a7,0xfc93a039,0x655b59c3,0x8f0ccc92,0xffeff47d,0x85845dd1,0x6fa87e4f,0xfe2ce6e0,0xa3014314,0x4e0811a1,0xf7537e82,0xbd3af235,0x2ad7d2bb,0xeb86d391];

    let mut data = input.to_vec();
    let bit_len = (data.len() as u64).wrapping_mul(8);
    data.push(0x80);
    while data.len() % 64 != 56 {
        data.push(0);
    }
    data.extend_from_slice(&bit_len.to_le_bytes());
    let (mut a0, mut b0, mut c0, mut d0) = (0x67452301u32, 0xefcdab89u32, 0x98badcfeu32, 0x10325476u32);
    for chunk in data.chunks_exact(64) {
        let mut words = [0u32; 16];
        for (index, bytes) in chunk.chunks_exact(4).enumerate() {
            words[index] = u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
        }
        let (mut a, mut b, mut c, mut d) = (a0, b0, c0, d0);
        for i in 0..64 {
            let (f, g) = if i < 16 {
                ((b & c) | (!b & d), i)
            } else if i < 32 {
                ((d & b) | (!d & c), (5 * i + 1) % 16)
            } else if i < 48 {
                (b ^ c ^ d, (3 * i + 5) % 16)
            } else {
                (c ^ (b | !d), (7 * i) % 16)
            };
            let next = b.wrapping_add(a.wrapping_add(f).wrapping_add(K[i]).wrapping_add(words[g]).rotate_left(S[i]));
            (a, b, c, d) = (d, next, b, c);
        }
        a0 = a0.wrapping_add(a);
        b0 = b0.wrapping_add(b);
        c0 = c0.wrapping_add(c);
        d0 = d0.wrapping_add(d);
    }
    let mut output = String::with_capacity(32);
    for word in [a0, b0, c0, d0] {
        for byte in word.to_le_bytes() {
            output.push_str(&format!("{byte:02x}"));
        }
    }
    output
}

/// The checked-in S-expression and its digest, checking with `ops` (OCaml `Operators.all`).
/// The digest covers the sop kinds, the catalog the PPX links against; the generated scene, world
/// and settings kinds (`extra`) follow them and are outside it.
pub fn generate_with(
    ops: &[Op],
    extra: &[Descriptor],
    nodes: Option<&[Defnode]>,
    factories: &[Factory],
) -> Result<(String, String), Diagnostic> {
    let supplied = nodes.is_some();
    let nodes: Vec<Defnode> = match nodes {
        Some(nodes) => nodes.to_vec(),
        None => catalog::node_definitions()?.to_vec(),
    };
    let catalog = catalog::of_factories(VERSION, extra, Some(&nodes), factories)?;
    let owned_sources: Vec<syntax::Syntax>;
    let sources: &[syntax::Syntax] = if supplied {
        owned_sources = nodes.iter().map(|node| node.form.clone()).collect();
        &owned_sources
    } else {
        catalog::sources()?
    };
    let workspace_form = syntax::parse("(workspace sop_nodes)").expect("literal parses");
    let (checked, diagnostics) = catalog::check_workspace(ops, true, Some(sources), &catalog, &workspace_form);
    let checked = match checked {
        Some(checked) => checked,
        None => return Err(diagnostics.into_iter().next().expect("a refused check reports")),
    };
    let mut kinds: Vec<String> = factories
        .iter()
        .map(|f| descriptor_entry(Some(edit_graph::factory_facts(f).clone()), &catalog::descriptor(f)))
        .collect();
    for node in &nodes {
        let definition = checked
            .nodes
            .iter()
            .find(|definition| definition.kind.qualified == node.kind.qualified)
            .expect("every library node is checked");
        let mut shown = node.clone();
        shown.fields = definition.fields.clone();
        shown.kind = definition.kind.clone();
        kinds.push(lisp_node(&workspace::kind_calls(&definition.body.body), &shown));
    }
    let payload = format!("{}\n{}", group("version", &[VERSION.to_string()]), group("kinds", &kinds));
    let digest = md5_hex(payload.as_bytes());
    let mut all = kinds;
    all.extend(extra.iter().map(|d| descriptor_entry(None, d)));
    let text = format!(
        "(flow_manifest\n  {}\n  {}\n  (kinds\n    {}))\n",
        group("version", &[VERSION.to_string()]),
        group("digest", &[quote(&digest)]),
        all.join("\n    ")
    );
    Ok((text, digest))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn md5_known_answers() {
        assert_eq!(md5_hex(b""), "d41d8cd98f00b204e9800998ecf8427e");
        assert_eq!(md5_hex(b"abc"), "900150983cd24fb0d6963f7d28e17f72");
    }

    #[test]
    fn quoting_and_numbers() {
        assert_eq!(quote("a\"b\\c\nd"), "\"a\\\"b\\\\c\\nd\"");
        assert_eq!(number(1.0), "1");
        assert_eq!(number(0.5), "0.5");
        assert_eq!(number(-10.0), "-10");
    }
}

/// The checked-in S-expression and its digest (checked with `Operators.all`).
pub fn generate(extra: &[Descriptor], nodes: Option<&[Defnode]>, factories: &[Factory]) -> Result<(String, String), Diagnostic> {
    generate_with(crate::operators::all(), extra, nodes, factories)
}

#[cfg(test)]
mod golden {
    use super::*;

    const GOLDEN: &str = include_str!("../../../../lib/flow_sop/flow_manifest.sexp");

    /// The checked-in manifest, byte for byte up to the generated scene/world/settings kinds
    /// (`Editor_document.Kinds.descriptors`, outside the digest and not ported to this crate).
    #[test]
    fn manifest_reproduces_the_checked_in_file() {
        // ponytail: checks every library node (minutes at -O0); run with RAYS_SLOW=1 (cargo t gives it 60 s).
        if std::env::var_os("RAYS_SLOW").is_none() {
            eprintln!("skipped: set RAYS_SLOW=1 to regenerate and compare flow_manifest.sexp");
            return;
        }
        // the checker recurses on Lisp depth: the app runs it on a 256 MB stack
        std::thread::Builder::new()
            .stack_size(256 << 20)
            .spawn(reproduces)
            .expect("spawns")
            .join()
            .unwrap_or_else(|panic| std::panic::resume_unwind(panic));
    }

    fn reproduces() {
        let (text, digest) = generate(&[], None, &sop::nodes::factories()).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
        let cut = ["scene/", "world/", "settings/"]
            .iter()
            .filter_map(|prefix| GOLDEN.find(&format!("\n    (kind \"{prefix}")))
            .min()
            .expect("the golden holds editor kinds");
        let expected = format!("{}))\n", &GOLDEN[..cut]);
        // ponytail: compare the sop and Lisp kinds only; add the editor descriptors when editor_document exists.
        assert_eq!(text.len(), expected.len(), "generated length");
        if text != expected {
            let at = text.bytes().zip(expected.bytes()).position(|(a, b)| a != b).unwrap_or(0);
            panic!("first difference at byte {at}: {:?} vs {:?}", &text[at.saturating_sub(60)..(at + 60).min(text.len())], &expected[at.saturating_sub(60)..(at + 60).min(expected.len())]);
        }
        assert!(GOLDEN.contains(&format!("(digest \"{digest}\")")));
    }
}
