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
use rays_math::hash::md5_hex;
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

    const GOLDEN: &str = include_str!("../../../crates/flow_sop/flow_manifest.sexp");
    fn sop_golden() -> &'static str {
        let cut = ["scene/", "world/", "settings/"]
            .iter()
            .filter_map(|prefix| GOLDEN.find(&format!("\n    (kind \"{prefix}")))
            .min()
            .expect("the golden holds editor kinds");
        &GOLDEN[..cut]
    }

    fn kinds() -> Vec<String> {
        sop_golden().split("\n    (kind ").skip(1).map(|body| format!("(kind {body}")).collect()
    }

    #[test]
    fn manifest_native_kinds_order_and_digest_match_the_checked_in_file() {
        let factories = sop::nodes::factories();
        let nodes = catalog::node_definitions().unwrap();
        let expected = kinds();
        assert_eq!(expected.len(), factories.len() + nodes.len());
        assert_eq!(NODES, nodes.iter().map(|node| node.kind.qualified.as_str()).collect::<Vec<_>>());
        for (factory, expected) in factories.iter().zip(&expected) {
            assert_eq!(&descriptor_entry(Some(edit_graph::factory_facts(factory).clone()), &catalog::descriptor(factory)), expected);
        }
        for (node, expected) in nodes.iter().zip(&expected[factories.len()..]) {
            assert!(expected.starts_with(&format!("(kind {}\n", quote(&node.kind.qualified))));
        }
        let payload = format!("{}\n{}", group("version", &[VERSION.to_string()]), group("kinds", &expected));
        assert_eq!(sop_golden(), format!("(flow_manifest\n  (version {VERSION})\n  (digest \"{}\")\n  (kinds\n    {}", md5_hex(payload.as_bytes()), expected.join("\n    ")));
        // Exercise the public generator without rechecking the entire node library.
        let (native, digest) = generate(&[], Some(&[]), &factories).unwrap();
        let native_kinds = &expected[..factories.len()];
        let payload = format!("{}\n{}", group("version", &[VERSION.to_string()]), group("kinds", native_kinds));
        assert_eq!(digest, md5_hex(payload.as_bytes()));
        assert_eq!(native, format!("(flow_manifest\n  (version {VERSION})\n  (digest \"{digest}\")\n  (kinds\n    {}))\n", native_kinds.join("\n    ")));
    }

    fn checked_node_matches(name: &'static str) {
        std::thread::Builder::new().stack_size(256 << 20).spawn(move || {
            let node = catalog::node_definitions().unwrap().iter().find(|node| node.kind.qualified == name).expect("shipped node");
            let factories = sop::nodes::factories();
            let catalog = catalog::of_factories(VERSION, &[], None, &factories).unwrap();
            // Check the real definition and its transitive helpers, not the whole unrelated library.
            let sources = catalog::private::reachable_sources(catalog::sources().unwrap(), std::slice::from_ref(&node.form));
            let forms = syntax::parse("(workspace sop_nodes)").unwrap();
            let (checked, diagnostics) = catalog::check_workspace(crate::operators::all(), true, Some(&sources), &catalog, &forms);
            let checked = checked.unwrap_or_else(|| panic!("{name}: {}", diagnostics.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")));
            let definition = checked.nodes.iter().find(|definition| definition.kind.qualified == name).expect("checked node");
            let mut shown = node.clone();
            shown.fields = definition.fields.clone();
            shown.kind = definition.kind.clone();
            let actual = lisp_node(&workspace::kind_calls(&definition.body.body), &shown);
            let prefix = format!("(kind {}\n", quote(name));
            let expected = kinds().into_iter().find(|kind| kind.starts_with(&prefix)).expect("golden entry");
            assert_eq!(actual, expected, "{name}");
        }).unwrap().join().unwrap();
    }

    macro_rules! node_goldens {
        ($($test:ident $name:literal),* $(,)?) => {
            const NODES: &[&str] = &[$($name),*];
            $(#[test] fn $test() { checked_node_matches($name) })*
        };
    }
    node_goldens!(
        node_attr_from_image "sop/attr_from_image",
        node_attribute_blur "sop/attribute_blur",
        node_attribute_composite "sop/attribute_composite",
        node_attribute_copy "sop/attribute_copy",
        node_attribute_fade "sop/attribute_fade",
        node_attribute_interpolate "sop/attribute_interpolate",
        node_attribute_laplacian "sop/attribute_laplacian",
        node_attribute_noise "sop/attribute_noise",
        node_attribute_noise_quaternion "sop/attribute_noise_quaternion",
        node_attribute_randomize "sop/attribute_randomize",
        node_attribute_remap "sop/attribute_remap",
        node_bend "sop/bend",
        node_blast "sop/blast",
        node_blast_by_attribute "sop/blast_by_attribute",
        node_blend_shapes "sop/blend_shapes",
        node_bound "sop/bound",
        node_box "sop/box",
        node_carve "sop/carve",
        node_circle "sop/circle",
        node_circle_from_edges "sop/circle_from_edges",
        node_color_by_height "sop/color_by_height",
        node_compact_points "sop/compact_points",
        node_connectivity "sop/connectivity",
        node_convert_line "sop/convert_line",
        node_copy_to_points "sop/copy_to_points",
        node_crease "sop/crease",
        node_delete_attributes "sop/delete_attributes",
        node_delete_edge_group "sop/delete_edge_group",
        node_distance_along_geometry "sop/distance_along_geometry",
        node_distance_from_target "sop/distance_from_target",
        node_duplicate "sop/duplicate",
        node_edge_cusp "sop/edge_cusp",
        node_edge_divide "sop/edge_divide",
        node_edge_equalize "sop/edge_equalize",
        node_edge_flip "sop/edge_flip",
        node_edge_relax "sop/edge_relax",
        node_edge_straighten "sop/edge_straighten",
        node_edge_transport "sop/edge_transport",
        node_edge_transport_curves "sop/edge_transport_curves",
        node_edge_transport_parent "sop/edge_transport_parent",
        node_ends "sop/ends",
        node_enumerate "sop/enumerate",
        node_extract_centroid "sop/extract_centroid",
        node_extract_point_from_curve "sop/extract_point_from_curve",
        node_graph_color "sop/graph_color",
        node_grid "sop/grid",
        node_group_backface "sop/group_backface",
        node_group_boundary_components "sop/group_boundary_components",
        node_group_bounds "sop/group_bounds",
        node_group_combine "sop/group_combine",
        node_group_copy "sop/group_copy",
        node_group_delete "sop/group_delete",
        node_group_edge_depth "sop/group_edge_depth",
        node_group_edges "sop/group_edges",
        node_group_expand "sop/group_expand",
        node_group_find_path "sop/group_find_path",
        node_group_from_attribute_boundary "sop/group_from_attribute_boundary",
        node_group_invert "sop/group_invert",
        node_group_non_planar "sop/group_non_planar",
        node_group_normal "sop/group_normal",
        node_group_promote_boundary "sop/group_promote_boundary",
        node_group_promotions "sop/group_promotions",
        node_group_random "sop/group_random",
        node_group_range "sop/group_range",
        node_group_ranges "sop/group_ranges",
        node_group_rename "sop/group_rename",
        node_group_unshared "sop/group_unshared",
        node_groups_from_name "sop/groups_from_name",
        node_iso_surface "sop/iso_surface",
        node_line "sop/line",
        node_match_axis "sop/match_axis",
        node_match_size "sop/match_size",
        node_material "sop/material",
        node_measure "sop/measure",
        node_measure_curvature "sop/measure_curvature",
        node_merge "sop/merge",
        node_mirror "sop/mirror",
        node_mountain "sop/mountain",
        node_name_from_groups "sop/name_from_groups",
        node_noise_displace "sop/noise_displace",
        node_normals "sop/normals",
        node_ordered_group "sop/ordered_group",
        node_peak "sop/peak",
        node_platonic "sop/platonic",
        node_point_generate "sop/point_generate",
        node_point_jitter "sop/point_jitter",
        node_point_replicate "sop/point_replicate",
        node_point_split "sop/point_split",
        node_point_velocity "sop/point_velocity",
        node_points "sop/points",
        node_poly_bridge "sop/poly_bridge",
        node_poly_loft "sop/poly_loft",
        node_poly_path "sop/poly_path",
        node_polyframe "sop/polyframe",
        node_polywire "sop/polywire",
        node_promote_attributes "sop/promote_attributes",
        node_rename_attributes "sop/rename_attributes",
        node_rename_edge_group "sop/rename_edge_group",
        node_resample "sop/resample",
        node_rest_position "sop/rest_position",
        node_reverse "sop/reverse",
        node_revolve "sop/revolve",
        node_rewire_vertices "sop/rewire_vertices",
        node_scatter "sop/scatter",
        node_separate_pieces "sop/separate_pieces",
        node_set_color "sop/set_color",
        node_set_float "sop/set_float",
        node_set_int "sop/set_int",
        node_set_orient "sop/set_orient",
        node_set_transform "sop/set_transform",
        node_set_vector "sop/set_vector",
        node_skin "sop/skin",
        node_smooth "sop/smooth",
        node_snap_to_grid "sop/snap_to_grid",
        node_sort "sop/sort",
        node_spiral "sop/spiral",
        node_subdivide "sop/subdivide",
        node_swap_attributes "sop/swap_attributes",
        node_sweep "sop/sweep",
        node_sweep_circle "sop/sweep_circle",
        node_torus "sop/torus",
        node_transform "sop/transform",
        node_tube "sop/tube",
        node_uv_auto_seam "sop/uv_auto_seam",
        node_uv_project "sop/uv_project",
        node_uv_sphere "sop/uv_sphere",
        node_uv_transform "sop/uv_transform",
        node_uv_unitize "sop/uv_unitize",
    );


    /// The checked-in manifest, byte for byte up to the generated scene/world/settings kinds
    /// (`Editor_document.Kinds.descriptors`, outside the digest and not ported to this crate).
    #[test]
    #[ignore = "slow: exhaustive full-library regeneration; default node goldens check every entry independently"]
    fn manifest_reproduces_the_checked_in_file() {
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
