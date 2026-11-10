//! Catalog membership, metadata, cache keys and shipped modeling nodes.
#![allow(dead_code, unused_imports)]
use std::cell::RefCell;
use std::sync::Arc;

use flow_sop::{catalog, lisp_sop, lower, manifest, operators};
use flow_syntax::syntax::{self, Node};
use param::{Effects, FieldView, Impact, KindView, Value};
use rdk_core::attribute::{self as attr, Owner, Storage};
use rdk_core::geometry as geo;
use rdk_core::group;
use rdk_core::packed::{float3, float4};
use rdk_core::topology::{self, PrimitiveKind};
use sop::context::{self as sctx, Options as CtxOptions};
use sop::edit_graph::{self as edit, Factory, InputRequirement};
use sop::node;
use sop::payload;
use sop::session;

fn check(condition: bool, message: &str) {
    assert!(condition, "{message}");
}

fn diag(d: &rays_diagnostic::Diagnostic) -> String {
    rays_diagnostic::to_string(d)
}

fn text(s: &str) -> Value {
    Value::Text(s.as_bytes().into())
}

fn choice(s: &str) -> Value {
    Value::Choice(s.to_owned())
}

fn pv(name: &str, value: Value) -> (String, Value) {
    (name.to_owned(), value)
}

thread_local! {
    static REGISTERED: RefCell<Option<Vec<Factory>>> = const { RefCell::new(None) };
}

/// The native factories plus the ones the library's Lisp definitions lower to.
fn registered_factories() -> Vec<Factory> {
    REGISTERED.with(|cell| {
        cell.borrow_mut()
            .get_or_insert_with(|| {
                let started = std::time::Instant::now();
                let native = sop::nodes::factories();
                let cat = catalog::of_factories(manifest::VERSION, &[], None, &native).unwrap_or_else(|d| panic!("{}", diag(&d)));
                let forms = syntax::parse("(workspace registered_nodes)").unwrap();
                // An explicit library keeps every definition; the default prunes to what the forms reach.
                let sources = catalog::sources().unwrap_or_else(|d| panic!("{}", diag(&d)));
                let workspace = match catalog::check_workspace(operators::all(), true, Some(sources), &cat, &forms) {
                    (Some(w), d) if d.is_empty() => w,
                    (_, ds) => panic!("{}", ds.iter().map(diag).collect::<Vec<_>>().join("\n")),
                };
                eprintln!("registered: check {:?}", started.elapsed());
                let lowered = lower::of_checked(Some(native.clone()), lower::Options::default(), &workspace).unwrap_or_else(|d| panic!("{}", diag(&d)));
                eprintln!("registered: lowered {:?}", started.elapsed());
                let mut all = native;
                all.extend(lowered.factories.iter().cloned());
                all
            })
            .clone()
    })
}

fn factory_named(key: &str) -> Factory {
    registered_factories().into_iter().find(|f| edit::factory_key(f) == key).unwrap_or_else(|| panic!("no factory {key}"))
}

fn catalog_node(key: &str, inputs: &[Option<node::T>], changes: &[(String, Value)]) -> node::T {
    let factory = factory_named(key);
    let n = edit::instantiate_optional(&factory, inputs).unwrap_or_else(|m| panic!("{m}"));
    node::apply_parameters(&n, changes).unwrap_or_else(|m| panic!("{m}")).0
}

fn edit_parameters(graph: &node::T, node_id: i64, changes: &[(String, Value)]) -> Result<(node::T, Effects), String> {
    let (document, effects) = edit::apply_parameters(&edit::of_graph(graph), node_id, changes)?;
    Ok((edit::compile(&document)?, effects))
}

fn cook_with(session: &mut session::T, context: &sctx::T, n: &node::T) -> geo::T {
    match session::cook(session, None, context, n) {
        Ok(output) => payload::geometry(&output.payload).unwrap_or_else(|d| panic!("{}", diag(&d))),
        Err(error) => panic!("{}", rays_diagnostic::error_to_string(&error)),
    }
}

fn cook(session: &mut session::T, n: &node::T) -> geo::T {
    cook_with(session, &sctx::create(CtxOptions::default()).unwrap(), n)
}

fn cook_at(domains: i64, grain: i64, entries: i64, bytes: i64, n: &node::T) -> geo::T {
    let mut s = session::create(entries, bytes).unwrap();
    let context = sctx::create(CtxOptions { domains: Some(domains), grain: Some(grain), ..Default::default() }).unwrap();
    let g = cook_with(&mut s, &context, n);
    session::close(&mut s);
    g
}

type Planes = (Vec<f64>, Vec<f64>, Vec<f64>);

fn positions(g: &geo::T) -> Planes {
    let v = float3::view::view(geo::positions(g)).unwrap();
    (v.x.to_vec(), v.y.to_vec(), v.z.to_vec())
}

fn topo(g: &geo::T) -> (i64, Vec<i64>, Vec<i64>, Vec<u8>) {
    let v = topology::view::view(geo::topology(g)).unwrap();
    (v.point_count, v.vertex_points.iter().map(|&p| i64::from(p)).collect(), v.primitive_offsets.iter().map(|&p| i64::from(p)).collect(), v.primitive_kinds.to_vec())
}

fn float3(name: &str, g: &geo::T) -> Planes {
    let a = geo::find_attribute(Owner::Point, name, g).unwrap_or_else(|| panic!("missing {name}"));
    match attr::storage(&a) {
        Ok(Storage::Float3(v)) => {
            let v = float3::view::view(&v).unwrap();
            (v.x.to_vec(), v.y.to_vec(), v.z.to_vec())
        }
        _ => panic!("{name} has wrong storage"),
    }
}

/// Positions, topology and every attribute and group as comparable bytes.
fn bytes(g: &geo::T) -> String {
    let mut out = String::new();
    let (x, y, z) = positions(g);
    for i in 0..x.len() {
        out.push_str(&format!("{:x},{:x},{:x};", x[i].to_bits(), y[i].to_bits(), z[i].to_bits()));
    }
    out.push_str(&format!("{:?}", topo(g)));
    for a in geo::attributes(g) {
        out.push_str(&format!("A{}:{}:", attr::name(&a), attr::kind_name(&a)));
        match attr::storage(&a) {
            Ok(Storage::Float(v)) => out.push_str(&format!("{:?}", v.iter().map(|x| x.to_bits()).collect::<Vec<_>>())),
            Ok(Storage::Int(v)) => out.push_str(&format!("{v:?}")),
            Ok(Storage::Text(v)) => out.push_str(&format!("{v:?}")),
            Ok(Storage::Float2(v)) => out.push_str(&format!("{:?}", (0..rdk_core::packed::float2::length(&v)).map(|i| rdk_core::packed::float2::get(&v, i).map(|(a, b)| (a.to_bits(), b.to_bits()))).collect::<Vec<_>>())),
            Ok(Storage::Float3(v)) => out.push_str(&format!("{:?}", (0..float3::length(&v)).map(|i| float3::get(&v, i).map(|(a, b, c)| (a.to_bits(), b.to_bits(), c.to_bits()))).collect::<Vec<_>>())),
            Ok(Storage::Float4(v)) => out.push_str(&format!("{:?}", (0..float4::length(&v)).map(|i| float4::get(&v, i).map(|(a, b, c, d)| (a.to_bits(), b.to_bits(), c.to_bits(), d.to_bits()))).collect::<Vec<_>>())),
            Ok(_) => out.push_str("array"),
            Err(e) => out.push_str(&format!("{e:?}")),
        }
        out.push(';');
    }
    out
}

fn field(n: &node::T, name: &str) -> FieldView {
    node::parameter_fields(n).into_iter().find(|f| f.name == name).unwrap_or_else(|| panic!("no field {name}"))
}

fn labelled(label: &str, with_: &[(&str, node::T)], source: &str) -> node::T {
    let with_: Vec<(String, node::T)> = with_.iter().map(|(k, v)| ((*k).to_owned(), v.clone())).collect();
    node::relabel(label, &lisp_sop::node(&with_, source))
}

fn lisp(source: &str) -> node::T {
    lisp_sop::node(&[], source)
}

fn is_floating(f: &FieldView) -> bool {
    matches!(f.kind, KindView::Floating(_))
}

fn check_metadata(factory: &Factory, fields: &[FieldView]) -> i64 {
    let key = edit::factory_key(factory);
    let names = edit::factory_slot_names(factory);
    let mut sorted = names.to_vec();
    sorted.sort();
    sorted.dedup();
    check(names.len() == edit::factory_arity(factory) && sorted.len() == names.len(), &format!("{key} has invalid geometry port names"));
    for name in names {
        check(!fields.iter().any(|f| f.name == *name || f.vec3.as_ref().is_some_and(|(g, _)| g == name)), &format!("{key} has a geometry port shadowing a parameter port"));
    }
    if names.len() > 1 {
        let unnamed: Vec<String> = (0..names.len()).map(|i| format!("in{i}")).collect();
        check(names != unnamed.as_slice(), &format!("{key} still has unnamed multiple inputs"));
    }
    let mut count = 0;
    for x in fields {
        if !is_floating(x) || !x.name.ends_with("_x") {
            continue;
        }
        let prefix = &x.name[..x.name.len() - 2];
        let find = |suffix: &str| fields.iter().find(|f| f.name == format!("{prefix}{suffix}") && is_floating(f));
        if let (Some(y), Some(z)) = (find("_y"), find("_z")) {
            let group = if fields.iter().any(|f| f.name == prefix) { format!("{prefix}_vector") } else { prefix.to_owned() };
            for (component, f) in [x, y, z].into_iter().enumerate() {
                check(f.vec3 == Some((group.clone(), component as i64)) && f.folder == x.folder, &format!("{key}.{} lost its vector port metadata", f.name));
            }
            count += 1;
        }
    }
    count
}

fn read_sketch(name: &str) -> String {
    super::support::read_repo(&format!("sketches/{name}"))
}

fn gallery_grid_warning() {
    let catalog = rays_editor::workspace_catalog(None).unwrap();
    let source = read_sketch("sop_gallery.rays");
    let (_, warnings) = editor_document::workspace_doc::check_text(&Default::default(), &catalog, &source).unwrap_or_else(|ds| panic!("{}", super::support::diag_text(&ds)));
    check(
        warnings.iter().map(|d| (d.code.as_str(), d.severity)).collect::<Vec<_>>() == vec![("W_SOFT_RANGE", rays_diagnostic::Severity::Warning); 2],
        "gallery soft-range warnings changed or became errors",
    );
    let n = lisp("(sop/grid :columns 100 :rows 100 :size 10 :width 10 :height 10)");
    for name in ["columns", "rows"] {
        check(field(&n, name).current == Value::Int(100), "a soft grid range clamped the gallery");
    }
    let mut s = session::create(8, 16_777_216).unwrap();
    let g = cook(&mut s, &n);
    check(geo::point_count(&g) == 10_201 && geo::primitive_count(&g) == 20_000, "the gallery's 100x100 grid did not cook at its authored resolution");
    session::close(&mut s);
}

fn motion() {
    let mut s = session::create(32, 16_000_000).unwrap();
    let previous = lisp_sop::snapshot(&rdk_ops::line_geometry::points(&[(0., 0., 0.), (1., 1., 1.)]).unwrap());
    let current = lisp_sop::snapshot(&rdk_ops::line_geometry::points(&[(1., 2., 3.), (3., 5., 7.)]).unwrap());
    let rested = catalog_node("rest_position", &[Some(current.clone()), None], &[]);
    let graph = catalog_node("point_velocity", &[Some(rested), Some(previous.clone()), None], &[pv("dt", Value::Float(0.5)), pv("add_x", Value::Float(1.)), pv("add_z", Value::Float(-1.))]);
    let output = cook(&mut s, &graph);
    let rest = float3("rest", &output);
    let velocity = float3("v", &output);
    check(rest.0 == [1., 3.] && rest.1 == [2., 5.] && rest.2 == [3., 7.], "Rest Position SOP output");
    check(velocity.0 == [3., 5.] && velocity.1 == [4., 8.] && velocity.2 == [5., 11.], "Point Velocity SOP output");
    let invalid = catalog_node("point_velocity", &[Some(current), Some(previous), None], &[pv("group", text("missing"))]);
    match session::cook(&mut s, None, &sctx::create(CtxOptions::default()).unwrap(), &invalid) {
        Err(error) => check(error.code == "missing_group", &format!("unexpected diagnostic {}", error.code)),
        Ok(_) => panic!("Point Velocity SOP accepted a missing group"),
    }
    session::close(&mut s);
}

fn set_color() {
    let mut s = session::create(8, 1_000_000).unwrap();
    let b = catalog_node("box", &[], &[]);
    let colors = |s: &mut session::T, n: &node::T| -> float4::view::View {
        match geo::find_attribute(Owner::Point, "Cd", &cook(s, n)) {
            Some(a) => match attr::storage(&a) {
                Ok(Storage::Float4(v)) => float4::view::view(&v),
                _ => panic!("set_color did not write a float4 Cd"),
            },
            None => panic!("set_color wrote no Cd"),
        }
    };
    let plain = colors(&mut s, &catalog_node("set_color", &[Some(b.clone())], &[]));
    check(plain.x.iter().all(|v| *v == 1.) && plain.w.iter().all(|v| *v == 1.), "set_color defaults are not opaque white");
    let grouped = lisp_sop::node(&[("box".to_owned(), b)], r#"(sop/group_range (sop/ext_box) :name "g" :end_ 0)"#);
    let tinted = colors(&mut s, &catalog_node("set_color", &[Some(grouped)], &[pv("group", text("g")), pv("color_r", Value::Float(0.25)), pv("alpha", Value::Float(0.5))]));
    check(tinted.x[0] == 0.25 && tinted.w[0] == 0.5 && tinted.x[1] == 1. && tinted.w[1] == 1., "set_color group did not restrict the write");
    session::close(&mut s);
}

fn membership() {
    let keys: Vec<String> = registered_factories().iter().map(|f| edit::factory_key(f).to_owned()).collect();
    // 162 SOP nodes and the three native carriers of ported ones (image_texels, pack_instances, subdivide_cracks)
    let mut unique = keys.clone();
    unique.sort();
    unique.dedup();
    check(keys.len() == 165 && unique.len() == 165, &format!("combined SOP registry has a missing or duplicate factory key ({} / {})", keys.len(), unique.len()));
    check(keys.iter().any(|k| k == "material"), "SOP editor catalog is missing material");
    for gone in ["delete_attribute", "bounding_box", "rename_group", "rename_attribute", "group_promote", "promote_attribute"] {
        check(!keys.iter().any(|k| k == gone), &format!("duplicate {gone} factory remains registered"));
    }
    let lists: &[(&str, &[&str])] = &[
        ("SOP editor catalog", &["box", "grid", "platonic", "points", "switch", "copy_to_points", "boolean_fracture", "boolean", "boolean_seam", "boolean_detect", "intersection_analysis", "attribute_noise_quaternion", "point_jitter", "null", "normals", "transform", "triangulate", "mountain", "line", "circle", "spiral", "uv_sphere", "torus", "tube", "match_size", "mirror", "clip", "exploded_view"]),
        ("SOP editor topology catalog", &["crease", "subdivide", "edge_divide", "edge_collapse", "dissolve", "poly_bevel"]),
        ("SOP editor deformation catalog", &["peak", "bend", "smooth", "reverse", "clean", "facet"]),
        ("SOP editor edge/piece catalog", &["separate_pieces", "edge_flip", "edge_cusp", "edge_straighten", "circle_from_edges", "edge_equalize"]),
        ("SOP editor modeling catalog", &["snap_to_grid", "remesh", "poly_extrude", "poly_fill", "convert_line", "resample", "carve", "ends", "join_curves", "poly_path", "poly_reduce", "measure_curvature", "attribute_laplacian", "polyframe", "duplicate", "match_axis", "convex_hull", "extract_centroid", "bound"]),
        ("SOP editor catalog", &["fuse", "ray"]),
        ("SOP editor distance catalog", &["distance_along_geometry", "distance_from_geometry", "distance_from_target"]),
        ("SOP editor topology/attribute catalog", &["point_split", "poly_bridge", "graph_color", "edge_relax", "poly_loft", "revolve", "sweep", "polywire"]),
        ("SOP editor attribute catalog", &["measure", "connectivity", "set_float", "set_int", "set_vector", "set_orient", "set_transform", "set_color", "rest_position", "enumerate", "attribute_blur", "attr_from_image"]),
        ("SOP editor UV catalog", &["uv_project", "uv_transform", "uv_auto_seam", "uv_unitize", "uv_flatten", "uv_relax"]),
        ("SOP editor group catalog", &["group_edges", "group_random", "group_bounds", "group_normal", "group_non_planar", "group_backface", "group_edge_depth", "group_unshared", "group_boundary_components", "group_from_attribute_boundary", "group_promote_boundary", "group_promotions", "group_invert", "group_delete", "group_rename", "group_find_path", "group_copy", "group_transfer", "groups_from_name", "group_combine", "group_expand", "group_range", "group_ranges", "name_from_groups"]),
        ("SOP editor general catalog", &["poly_cut", "sort", "noise_displace", "color_by_height", "scatter"]),
        ("SOP editor attribute generator", &["attribute_noise", "attribute_remap", "attribute_randomize", "attribute_mirror"]),
        ("SOP editor transport catalog", &["rewire_vertices", "edge_transport", "edge_transport_curves", "edge_transport_parent"]),
        ("SOP editor metadata utility", &["compact_points", "bound", "group_rename", "delete_edge_group", "rename_edge_group", "delete_attributes", "rename_attributes", "swap_attributes", "triangulate_2d", "extract_point_from_curve", "soft_transform", "point_generate", "point_replicate", "attribute_fade", "blast_by_attribute", "blast"]),
        ("SOP editor catalog", &["point_velocity"]),
        ("SOP editor transfer catalog", &["attribute_transfer", "attribute_transfer_all", "attribute_transfer_surface", "attribute_copy", "attribute_interpolate", "promote_attributes"]),
    ];
    for (what, list) in lists {
        for key in *list {
            check(keys.iter().any(|k| k == key), &format!("{what} is missing {key}"));
        }
    }
}

fn manifest_checks(exhaustive: bool) {
    let factories = registered_factories();
    let keys: Vec<String> = factories.iter().map(|f| edit::factory_key(f).to_owned()).collect();
    let native = sop::nodes::factories();
    let flow_catalog = catalog::of_factories(1, &[], None, &native).unwrap_or_else(|d| panic!("{}", diag(&d)));
    let (manifest_text, digest) = manifest::generate(&[], None, &native).unwrap_or_else(|d| panic!("{}", diag(&d)));
    let (from_manifest, read_digest) = flow_check::catalog::catalog_of_manifest(&manifest_text).unwrap_or_else(|d| panic!("{}", diag(&d)));
    check(read_digest == digest && from_manifest.kinds == flow_catalog.kinds, "Flow manifest checker descriptor differs from the live catalog");
    let transform = from_manifest.kinds.iter().find(|k| k.qualified == "sop/transform").unwrap();
    check(
        transform.facts.as_ref().is_some_and(|f| {
            f.elementwise == flow_check::catalog::KernelElements::Points
                && f.reads == ["P", "N"]
                && f.writes == ["P", "N"]
                && f.preserves_topology
                && !f.exact
        }),
        "Flow manifest omitted declared transform facts",
    );
    let at = manifest_text.find('2').unwrap();
    let mut stale = manifest_text.clone();
    stale.replace_range(at..at + 1, "3");
    match flow_check::catalog::catalog_of_manifest(&stale) {
        Err(d) => check(d.code == "E_CATALOG" && d.message == "Flow manifest digest does not match its contents", "Flow manifest reader reported the wrong digest error"),
        Ok(_) => panic!("Flow manifest reader accepted changed contents"),
    }
    check(manifest::generate(&[], None, &native) == Ok((manifest_text.clone(), digest.clone())), "Flow catalog manifest is not deterministic");
    let forms = syntax::parse(&manifest_text).unwrap();
    assert!(forms.len() == 1, "Flow manifest needs one top-level form");
    let owner = &forms[0];
    let children = |name: &str, id: syntax::SyntaxId| {
        let items = owner.arena().children(id);
        match owner.arena().row(id).node {
            Node::List(_) if items.first().is_some_and(|&id| matches!(owner.arena().row(id).node, Node::Sym(h) if owner.name(h) == name)) => &items[1..],
            _ => panic!("Malformed Flow manifest {name}"),
        }
    };
    let body = children("flow_manifest", owner.row_id());
    assert!(body.len() == 3, "Malformed Flow manifest body");
    match (children("version", body[0]), children("digest", body[1])) {
        ([v], [h]) => match (owner.arena().row(*v).node, owner.arena().row(*h).node) {
            (Node::Num(n), Node::Str(s)) => check(owner.name(n).parse::<i64>().unwrap() == manifest::VERSION && owner.bytes(s) == digest.as_bytes(), "Flow manifest header changed"),
            _ => panic!("Malformed Flow manifest header"),
        },
        _ => panic!("Malformed Flow manifest header"),
    }
    let names: Vec<Vec<u8>> = children("kinds", body[2])
        .iter()
        .map(|&kind| match children("kind", kind).first().map(|&id| owner.arena().row(id).node) {
            Some(Node::Str(name)) => owner.bytes(name).to_vec(),
            _ => panic!("Malformed Flow manifest kind"),
        })
        .collect();
    // the manifest lists the native factories and the library's Lisp nodes: every registered kind
    check(names.len() == keys.len(), "Flow manifest omitted kinds");
    for key in &keys {
        let want = format!("sop/{key}");
        check(names.iter().any(|n| n.as_slice() == want.as_bytes()), &format!("Flow manifest omitted SOP {key}"));
    }
    // The schema sweep reads the factory's fields; building all default nodes is the exhaustive run's.
    let mut vector_groups = 0;
    for factory in &factories {
        vector_groups += check_metadata(factory, edit::factory_fields(factory));
        let slots: Vec<Option<node::T>> = vec![None; edit::factory_arity(factory)];
        let key = edit::factory_key(factory);
        if exhaustive || key == "box" {
            match edit::instantiate_optional(factory, &slots) {
                Ok(n) => {
                    check(edit::factory_facts(factory) == node::facts(&n), &format!("default factory facts differ from the instantiated node: {key}"));
                    if key == "box" {
                        let primary: Vec<String> = node::parameter_fields(&n).into_iter().filter(|f| f.primary).map(|f| f.name).collect();
                        check(primary == ["size_x", "size_y", "size_z"], "Box primary rows still expose topology instead of size");
                    }
                    check(node::operation(&n) == edit::factory_operation(factory), &format!("registered SOP {key} advertises operation {} but builds {}", edit::factory_operation(factory), node::operation(&n)));
                }
                Err(m) => panic!("registered SOP {key} could not be constructed: {m}"),
            }
        }
    }
    check(vector_groups == 104, &format!("catalog vector inventory changed ({vector_groups})"));
}

fn perturbed(view: &FieldView) -> Option<Value> {
    match (&view.kind, &view.current) {
        (KindView::Toggle, Value::Bool(v)) => Some(Value::Bool(!v)),
        (KindView::Integer(r), Value::Int(v)) => {
            let ok = |c: i64| r.hard_min.is_none_or(|l| c >= l) && r.hard_max.is_none_or(|h| c <= h);
            [v + 1, v - 1, r.soft_min, r.soft_max].into_iter().find(|c| c != v && ok(*c)).map(Value::Int)
        }
        (KindView::Floating(r), Value::Float(v)) => {
            let ok = |c: f64| r.hard_min.is_none_or(|l| c >= l) && r.hard_max.is_none_or(|h| c <= h);
            [v + 0.25, v - 0.25, (r.soft_min + r.soft_max) / 2., r.soft_min, r.soft_max].into_iter().find(|c| c != v && ok(*c)).map(Value::Float)
        }
        (KindView::Text, Value::Text(v)) => {
            let mut t = v.to_vec();
            if t.is_empty() {
                t = b"p".to_vec();
            } else {
                t.push(b'p');
            }
            Some(Value::Text(t.into()))
        }
        (KindView::Choice(options), Value::Choice(v)) => options.iter().find(|o| *o != v).map(|o| Value::Choice(o.clone())),
        (KindView::Choice(options), Value::Int(v)) => [v + 1, v - 1, 0].into_iter().find(|c| c != v && *c >= 0 && (*c as usize) < options.len()).map(Value::Int),
        _ => None,
    }
}

/// Every cook-impact schema field of every registered SOP participates in the cache identity.
fn perturbation() {
    let (mut changed, mut rejected) = (0, 0);
    for factory in registered_factories() {
        let key = edit::factory_key(&factory).to_owned();
        let slots: Vec<Option<node::T>> = vec![None; edit::factory_arity(&factory)];
        let n = edit::instantiate_optional(&factory, &slots).unwrap();
        check(!node::has_parameters(&n) || !node::parameter_key(&n).is_empty(), &format!("{key} has no schema-derived cache identity"));
        for view in node::parameter_fields(&n) {
            if view.impact != Impact::Cook {
                continue;
            }
            let Some(value) = perturbed(&view) else { panic!("{key}.{} has no perturbable value", view.name) };
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| node::apply_parameters(&n, &[(view.name.clone(), value)])));
            match result {
                Err(_) | Ok(Err(_)) => rejected += 1,
                Ok(Ok((_, effects))) if !effects.cook => rejected += 1,
                Ok(Ok((edited, _))) => {
                    changed += 1;
                    check(node::id(&edited) == node::id(&n) && node::parameter_key(&edited) != node::parameter_key(&n), &format!("changing {key}.{} did not change its cache key", view.name));
                }
            }
        }
    }
    println!("catalog cache-key perturbation: {changed} fields changed, {rejected} rejected");
    check(changed > 10 * rejected, &format!("only {changed} catalog fields were perturbable ({rejected} rejected)"));
}

fn manifest_default() {
    manifest_checks(false);
}

fn manifest_exhaustive() {
    manifest_checks(true);
}

fn run_body() {
    let source = labelled("box", &[], r#"(sop/box :size [2.0 2.0 2.0] :connectivity "Quads" :consolidate_points true)"#);
    let ordinary_chain = labelled("normal", &[("source", source.clone())], "(sop/normals (sop/ext_source))");
    for n in [ordinary_chain.clone(), node::inputs(&ordinary_chain)[0].clone()] {
        check(node::has_parameters(&n), &format!("catalog node has no parameters: {}", node::label(&n)));
    }
    let grid = labelled("grid", &[], "(sop/grid :width 3.0 :height 3.0 :columns 2 :rows 2 :size 3.0)");
    let plane = labelled("mountain", &[("grid", grid)], "(sop/mountain (sop/ext_grid) :seed 3 :height 0.2 :frequency [0.2 1.0 0.2] :octaves 2 :lacunarity 2.0 :roughness 0.5)");
    let origin = labelled("points", &[], "(sop/points :points 4)");
    let orient_node = labelled("orient", &[("origin", origin)], r#"(sop/attribute_noise_quaternion (sop/ext_origin) :seed 4 :owner "Point" :name "orient" :location "element-number" :frequency [0.2 0.2 0.2] :octaves 2)"#);
    let targets = labelled("jitter", &[("orient", orient_node)], "(sop/point_jitter (sop/ext_orient) :seed 5 :scale 0.1)");
    let cutters = labelled("copy", &[("plane", plane.clone()), ("targets", targets.clone())], "(sop/copy_to_points (sop/ext_plane) (sop/ext_targets))");
    let fracture_node = labelled("fracture", &[("source", source), ("cutters", cutters)], "(sop/boolean_fracture (sop/ext_source) (sop/ext_cutters))");
    let graph = labelled("exploded-view", &[("fracture", fracture_node)], "(sop/exploded_view (sop/ext_fracture))");
    let fracture = node::inputs(&graph)[0].clone();
    let orient = node::inputs(&targets)[0].clone();
    let custom_noise = lisp_sop::node(
        &[("origin".to_owned(), node::inputs(&orient)[0].clone())],
        r#"(sop/attribute_noise_quaternion (sop/ext_origin) :owner "Point" :name "orient" :seed 4 :frequency [0.2 0.2 0.2] :octaves 2 :location "attribute:rest position" :range "min-max;vec4,0.0,0.1,0.2,0.3;vec4,0.7,0.8,0.9,1.0")"#,
    );
    let box_node = lisp("(sop/box)");
    let box_names: Vec<String> = node::parameter_fields(&box_node).into_iter().map(|f| f.name).collect();
    let box_refs: Vec<&str> = box_names.iter().map(String::as_str).collect();
    let defaults: Vec<(&node::T, Vec<&str>)> = vec![
        (&plane, vec!["group", "direction_attribute", "mask_attribute", "height_attribute", "recompute_normals"]),
        (&orient, vec!["group", "owner", "name", "location", "range"]),
        (&targets, vec!["group", "mask_attribute", "id_attribute", "axis_x", "axis_y", "axis_z"]),
        (&fracture, vec!["resolve_cutter_self_intersections", "detriangulation", "require_closed", "piece_attribute"]),
        (&box_node, box_refs),
    ];
    for (n, names) in defaults {
        for name in names {
            let v = field(n, name);
            check(v.current == v.default, &format!("catalog create/menu default differs: {name}"));
        }
    }
    let (changed_graph, effects) = edit_parameters(&targets, node::id(&orient), &[pv("location", field(&custom_noise, "location").current), pv("range", field(&custom_noise, "range").current)]).unwrap();
    let changed = node::find(&changed_graph, node::id(&orient)).unwrap();
    check(
        effects.cook && node::id(&changed) == node::id(&orient) && field(&changed, "location").current == field(&custom_noise, "location").current && field(&changed, "range").current == field(&custom_noise, "range").current,
        "quaternion noise location/range were not editable schema fields",
    );
    for (n, name, expected) in [(&plane, "height_attribute", ""), (&targets, "id_attribute", ""), (&orient, "name", "orient"), (&fracture, "piece_attribute", "piece")] {
        check(field(n, name).current == text(expected), &format!("catalog hidden parameter has wrong default: {name}"));
        let (edited, effects) = node::apply_parameters(n, &[pv(name, text("edited"))]).unwrap();
        check(effects.cook && node::id(&edited) == node::id(n) && field(&edited, name).current == text("edited"), &format!("catalog field did not rebuild: {name}"));
    }
    let parameterized: Vec<String> = node::inspect(&graph).into_iter().filter(|i| i.has_parameters).map(|i| i.label).collect();
    for label in ["box", "grid", "mountain", "points", "orient", "jitter", "copy", "fracture", "exploded-view"] {
        check(parameterized.iter().any(|l| l == label), &format!("catalog node has no PPX inspector metadata: {label}"));
    }
    let point_node = node::inspect(&graph).into_iter().find(|i| i.label == "points").unwrap();
    let (edited, effects) = edit_parameters(&graph, point_node.id, &[pv("points", Value::Int(999))]).unwrap();
    let edited_points = node::find(&edited, point_node.id).unwrap();
    check(effects.cook && field(&edited_points, "points").current == Value::Int(50), "catalog point count did not enforce its PPX hard maximum");
    let cube = labelled("cube", &[], "(sop/box)");
    let dodecahedron = labelled("dodecahedron", &[], r#"(sop/platonic :kind "Dodecahedron" :radius 1.0 :normals "None")"#);
    let switched = labelled("source-switch", &[("cube", cube.clone()), ("dodecahedron", dodecahedron.clone())], "(sop/switch (sop/ext_cube) (sop/ext_dodecahedron))");
    check(node::operation(&switched) == "switch" && node::has_parameters(&switched), "catalog Switch is not the standard parameterized SOP switch");
    let (switched, switch_effects) = edit_parameters(&switched, node::id(&switched), &[pv("input", Value::Int(1))]).unwrap();
    check(switch_effects.cook && field(&switched, "input").current == Value::Int(1), "catalog Switch did not retain its selected labeled input");
    let mut s = session::create(8, 1_000_000).unwrap();
    let cube_topology = geo::topology(&cook(&mut s, &cube)).clone();
    check(topology::primitive_count(&cube_topology) == 6 && topology::primitive_size(&cube_topology, 0).unwrap() == 4, "catalog Box default must retain six quad faces");
    check(geo::primitive_count(&cook(&mut s, &switched)) == geo::primitive_count(&cook(&mut s, &dodecahedron)), "catalog Switch did not cook the selected dodecahedron branch");
    let replacement = labelled("replacement-grid", &[], "(sop/grid :width 1.0 :height 1.0 :columns 2 :rows 2 :size 1.0)");
    let document = edit::of_graph(&switched);
    let document = edit::add_node(None, None, &replacement, &document).unwrap();
    let document = edit::connect(node::id(&replacement), node::id(&switched), 1, &document).unwrap();
    let rebuilt = edit::compile(&document).unwrap();
    let switch_field = field(&rebuilt, "input");
    check(matches!(&switch_field.kind, KindView::Choice(options) if options.iter().any(|o| o == "1 · replacement-grid")), "rewiring a Switch did not rebuild its input-dependent parameter labels");
    check(switch_field.current == Value::Int(1) && node::parameter_key(&switched) == node::parameter_key(&rebuilt), "rewiring Switch labels must retain its integer selection and parameter key");
    session::close(&mut s);

    let mut key_session = session::create(8, 1_000_000).unwrap();
    let exploded = labelled("key-view", &[("key_box", labelled("key-box", &[], "(sop/box)"))], "(sop/exploded_view (sop/ext_key_box))");
    cook(&mut key_session, &exploded);
    let misses = session::stats(&key_session).misses;
    let (exploded, _) = node::apply_parameters(&exploded, &[pv("piece_attribute", text("shard"))]).unwrap();
    cook(&mut key_session, &exploded);
    check(session::stats(&key_session).misses == misses + 1, "a schema field ignored by the operator key did not miss the session cache");
    session::close(&mut key_session);

    use InputRequirement::{Optional, Required};
    let optional_second = |key: &str, message: &str| {
        let f = factory_named(key);
        check(edit::factory_inputs(&f) == vec![Required, Optional] && edit::instantiate_optional(&f, &[None, None]).is_ok(), message);
    };
    optional_second("match_size", "PPX descriptor lost Match Size's optional target input signature");
    for key in ["boolean_detect", "intersection_analysis", "fuse", "poly_loft", "rest_position"] {
        optional_second(key, &format!("{key} lost its optional collision input signature"));
    }
    optional_second("delete_attributes", "Delete Attributes lost its optional reference input signature");
    optional_second("point_replicate", "Point Replicate lost its optional custom-shape input signature");
    let fade_factory = factory_named("attribute_fade");
    check(edit::factory_inputs(&fade_factory) == vec![Required, Optional, Optional] && edit::instantiate_optional(&fade_factory, &[None, None, None]).is_ok(), "Attribute Fade lost its two optional source input signatures");
    let fade_source = labelled("fade-source", &[], "(sop/box)");
    let fade_hold = labelled("fade-hold", &[], "(sop/box)");
    let fade = edit::instantiate_optional(&fade_factory, &[Some(fade_source.clone()), None, Some(fade_hold.clone())]).unwrap();
    let (fade, _) = node::apply_parameters(&fade, &[pv("fade_in", Value::Float(3.))]).unwrap();
    check(node::inputs(&fade).iter().map(node::id).collect::<Vec<_>>() == [node::id(&fade_source), node::id(&fade_hold)], "Attribute Fade parameter edit reassigned its sparse hold-source port");
    let velocity_factory = factory_named("point_velocity");
    check(edit::factory_inputs(&velocity_factory) == vec![Required, Optional, Optional] && edit::instantiate_optional(&velocity_factory, &[None, None, None]).is_ok(), "Point Velocity lost its previous/next optional input signatures");
    let velocity = edit::instantiate_optional(&velocity_factory, &[Some(fade_source.clone()), None, Some(fade_hold.clone())]).unwrap();
    let (velocity, _) = node::apply_parameters(&velocity, &[pv("dt", Value::Float(0.1))]).unwrap();
    check(node::inputs(&velocity).iter().map(node::id).collect::<Vec<_>>() == [node::id(&fade_source), node::id(&fade_hold)], "Point Velocity parameter edit reassigned its sparse next-sample port");
    let points_factory = factory_named("points");
    let points = edit::instantiate(&points_factory, &[]).unwrap();
    check(node::operation(&points) == "points" && edit::factory_arity(&points_factory) == 0, "point-generation editor factory has the wrong node or arity");
    let mut point_session = session::create(2, 1_000_000).unwrap();
    let point_mesh = rdk_rays::rays_mesh::to_mesh(&cook(&mut point_session, &points)).unwrap_or_else(|e| panic!("{}", rdk_core::error::to_string(&e)));
    check(matches!(rays::mesh::mode(&point_mesh), rays::mesh::mode::Points) && rays::mesh::vertex_count(&point_mesh) == 50, "point-only SOP output did not retain point rendering mode");
    session::close(&mut point_session);
    let explosion = rdk_rays::packed_pieces::explosion(node::operation(&graph), &node::parameter_fields(&graph)).unwrap();
    check(node::operation(&graph) == "exploded_view" && explosion.amount == 0.32 && explosion.piece_attribute == "piece", "standard Exploded View node lost its operation or PPX defaults");
}

/// Blend Shapes, Attribute Composite, Skin and Group Ordered as nodes: the sketches/ws_morph workspace
/// lowers and each graph cooks to the geometry its node promises, the same at one and four domains.
fn morph_sketch() {
    let forms = syntax::parse(&read_sketch("ws_morph.rays")).unwrap_or_else(|d| panic!("{}", diag(&d)));
    let lowered = lower::workspace(None, editor_document::kinds::descriptors(), lower::Options::default(), &forms).unwrap_or_else(|d| panic!("{}", diag(&d)));
    let cook_graph = |domains: i64, name: &str| {
        let graph = lowered.graphs.iter().find(|g| g.name == name).unwrap();
        let n = edit::compile_node(&graph.network.geometry, graph.root.unwrap()).unwrap();
        cook_at(domains, 97, 64, 64_000_000, &n)
    };
    let both = |name: &str| {
        let one = cook_graph(1, name);
        let four = cook_graph(4, name);
        check(positions(&one) == positions(&four) && topo(&one) == topo(&four), &format!("{name} differs between one and four domains"));
        one
    };
    // sheet: 28x28 quads keep their topology; the blend lifts the flat sheet part of the way to its two shapes.
    let sheet = both("sheet");
    let p = positions(&sheet);
    let low = p.1.iter().cloned().fold(f64::INFINITY, f64::min);
    let high = p.1.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    check(geo::point_count(&sheet) == 29 * 29 && geo::primitive_count(&sheet) == 28 * 28, "Blend Shapes changed the sheet's topology");
    check(p.0.iter().all(|v| v.is_finite()) && p.1.iter().all(|v| v.is_finite()) && high - low > 0.2 && high - low < 3., &format!("Blend Shapes left the sheet flat or blew it up (y {low}..{high})"));
    // tinted: the mean of #2f6fb5 at weight 1 and #f2a03d at weight 0.35.
    let tinted = both("tinted");
    let cd = match geo::find_attribute(Owner::Point, "Cd", &tinted).map(|a| attr::storage(&a)) {
        Some(Ok(Storage::Float4(v))) => float4::view::view(&v),
        _ => panic!("Attribute Composite dropped or retyped Cd"),
    };
    let cool = rays_math::color::hex_exn("#2f6fb5").unwrap();
    let warm = rays_math::color::hex_exn("#f2a03d").unwrap();
    let mean = |a: i64, b: i64| (a as f64 + 0.35 * b as f64) / 255. / 1.35;
    check(
        cd.x.len() == 29 * 29
            && cd.x.iter().all(|r| (r - mean(cool.r, warm.r)).abs() < 1e-6)
            && cd.y.iter().all(|g| (g - mean(cool.g, warm.g)).abs() < 1e-6)
            && cd.z.iter().all(|b| (b - mean(cool.b, warm.b)).abs() < 1e-6),
        &format!("Attribute Composite Cd is {} {} {}, expected {} {} {}", cd.x[0], cd.y[0], cd.z[0], mean(cool.r, warm.r), mean(cool.g, warm.g), mean(cool.b, warm.b)),
    );
    check(positions(&tinted) == positions(&sheet), "Attribute Composite moved points although P was not allowed");
    // vase: six 32-point rings skin into five bands of 32 quads.
    let vase = both("vase");
    let skin = geo::find_group(group::Owner::Primitive, "skin", &vase);
    let members = |g: &group::T| (0..group::length(g)).filter(|i| group::mem(*i, g)).count() as i64;
    check(
        geo::point_count(&vase) == 6 * 32 && geo::primitive_count(&vase) == 5 * 32 && skin.as_ref().map(members) == Some(5 * 32),
        &format!("Skin made {} primitives over {} points", geo::primitive_count(&vase), geo::point_count(&vase)),
    );
    let index = rdk_core::topology_index::create(None, geo::topology(&vase)).unwrap();
    let non_manifold = (0..rdk_core::topology_index::edge_count(&index)).filter(|e| rdk_core::topology_index::edge_incidence_count(&index, *e).unwrap() > 2).count();
    check(non_manifold == 0, "Skin wall has non-manifold edges");
    check(geo::find_group(group::Owner::Point, "rim", &vase).and_then(|g| group::ordered_elements(&g)) == Some(vec![0, 8, 16, 24]), "Group Ordered lost its authored order");
    // an unwired Blend Shapes passes its input through
    let mut s = session::create(8, 1_000_000).unwrap();
    let grid = lisp(r#"(sop/grid :width_mode "Auto" :height_mode "Auto" :rows 2 :columns 2)"#);
    let idle = catalog_node("blend_shapes", &[Some(grid.clone()), None, None, None, None], &[]);
    check(node::operation(&idle) == "blend_shapes" && positions(&cook(&mut s, &idle)) == positions(&cook(&mut s, &grid)), "unwired Blend Shapes is not a passthrough");
    session::close(&mut s);
}

/// The inputs of the Blend Shapes, Attribute Composite and Skin nodes that the ws_morph sketch leaves
/// alone: masks, point IDs, alpha and the rest guide, the same at one and four domains.
fn node_inputs() {
    let add = |owner: Owner, name: &str, storage: Storage, g: geo::T| {
        let a = attr::create_owned(name, owner, storage).unwrap();
        geo::with_attribute(a, &g).unwrap()
    };
    let line = |xs: &[f64]| rdk_ops::line_geometry::points(&xs.iter().map(|x| (*x, 0., 0.)).collect::<Vec<_>>()).unwrap();
    let point = Owner::Point;
    let xs = |g: &geo::T| positions(g).0;
    let both = |message: &str, n: &node::T| {
        let one = cook_at(1, 1, 16, 8_000_000, n);
        let four = cook_at(4, 1, 16, 8_000_000, n);
        check(bytes(&one) == bytes(&four), &format!("{message} differs between one and four domains"));
        one
    };
    let near = |l: &[f64], r: &[f64]| l.len() == r.len() && l.iter().zip(r).all(|(a, b)| (a - b).abs() < 1e-12);
    // Blend Shapes masks: the first input's mask, then the shape's own
    let source = add(point, "mask", Storage::Float(std::sync::Arc::new(vec![0., 0.5, 1.])), line(&[0., 1., 2.]));
    let target = add(point, "mask", Storage::Float(std::sync::Arc::new(vec![1., 0.5, 0.])), line(&[10., 11., 12.]));
    let default_inputs = vec![Some(lisp_sop::snapshot(&source)), Some(lisp_sop::snapshot(&target)), None, None, None];
    let blend = |inputs: &[Option<node::T>], changes: &[(String, Value)]| catalog_node("blend_shapes", inputs, changes);
    let scaled = |source_name: &str| {
        both("masked Blend Shapes", &blend(&default_inputs, &[pv("weight1", Value::Float(0.5)), pv("masking", choice("Scale from attribute")), pv("mask_source", choice(source_name))]))
    };
    check(near(&xs(&scaled("First input")), &[0., 3.5, 7.]), "Blend Shapes did not scale by the first input's mask");
    check(near(&xs(&scaled("Shape")), &[5., 3.5, 2.]), "Blend Shapes did not scale by the shape's mask");
    let set = both("set-mask Blend Shapes", &blend(&default_inputs, &[pv("weight1", Value::Float(0.125)), pv("masking", choice("Set from attribute"))]));
    check(near(&xs(&set), &[0., 6., 12.]), "Blend Shapes did not take its weight from the mask");
    // Blend Shapes point IDs: the shape lists the same points in another order
    let ordered = add(point, "id", Storage::Int(std::sync::Arc::new(vec![10, 20, 30])), line(&[0., 1., 2.]));
    let shuffled = add(point, "id", Storage::Int(std::sync::Arc::new(vec![30, 10, 20])), line(&[12., 10., 11.]));
    let inputs = vec![Some(lisp_sop::snapshot(&ordered)), Some(lisp_sop::snapshot(&shuffled)), None, None, None];
    let by_id = both("ID-matched Blend Shapes", &blend(&inputs, &[pv("point_id_attribute", text("id")), pv("attributes", text("P"))]));
    let by_index = both("index-matched Blend Shapes", &blend(&inputs, &[pv("attributes", text("P"))]));
    check(near(&xs(&by_id), &[10., 11., 12.]) && near(&xs(&by_index), &[12., 10., 11.]), "Blend Shapes point ID attribute did not match points by ID");
    // Attribute Composite alpha: each input's alpha weighs its values
    let layer = |ps: &[f64], values: &[f64], alpha: &[f64]| {
        let g = add(point, "value", Storage::Float(std::sync::Arc::new(values.to_vec())), line(ps));
        add(point, "alpha", Storage::Float(std::sync::Arc::new(alpha.to_vec())), g)
    };
    let composite = both(
        "alpha Attribute Composite",
        &catalog_node(
            "attribute_composite",
            &[Some(lisp_sop::snapshot(&layer(&[0., 1., 2.], &[2., 4., 6.], &[1., 1., 0.]))), Some(lisp_sop::snapshot(&layer(&[10., 11., 12.], &[10., 20., 30.], &[1., 0., 1.]))), None, None, None],
            &[pv("point_attributes", text("P value")), pv("allow_position", Value::Bool(true)), pv("alpha_attribute", text("alpha"))],
        ),
    );
    let values = match geo::find_attribute(point, "value", &composite).map(|a| attr::storage(&a)) {
        Some(Ok(Storage::Float(v))) => v,
        _ => panic!("Attribute Composite dropped or retyped value"),
    };
    check(near(&xs(&composite), &[5., 1., 12.]) && near(&values, &[6., 4., 30.]), "Attribute Composite did not weigh its inputs by alpha");
    // Skin rest guide: the second ring's points are turned a quarter; pairing on the rest positions gives
    // the wall of the unturned rings
    let rings = |turn: i64| {
        let pts: Vec<(f64, f64, f64)> = (0..8)
            .map(|index| {
                let (section, local) = (index / 4, index % 4);
                let angle = std::f64::consts::PI * 0.5 * (local + if section == 1 { turn } else { 0 }) as f64;
                (angle.cos(), section as f64, angle.sin())
            })
            .collect();
        let t = topology::create_owned(8, (0..8).collect(), vec![0, 4, 8], vec![PrimitiveKind::ClosedPolyline; 2]).unwrap();
        let p = float3::view::of_owned_exn(pts.iter().map(|p| p.0).collect(), pts.iter().map(|p| p.1).collect(), pts.iter().map(|p| p.2).collect()).unwrap();
        geo::create(p, t, None, None, None).unwrap()
    };
    let skin = |inputs: &[Option<node::T>]| both("Skin", &catalog_node("skin", inputs, &[]));
    let corners = |g: &geo::T| topo(g).1;
    let straight = skin(&[Some(lisp_sop::snapshot(&rings(0))), None]);
    let turned = skin(&[Some(lisp_sop::snapshot(&rings(1))), None]);
    let guided = skin(&[Some(lisp_sop::snapshot(&rings(1))), Some(lisp_sop::snapshot(&rings(0)))]);
    check(
        geo::primitive_count(&guided) == 4 && corners(&guided) == corners(&straight) && corners(&guided) != corners(&turned) && xs(&guided) == xs(&turned),
        "Skin did not pair its sections on the rest input's positions",
    );
}

pub fn cases() -> Vec<(&'static str, fn())> {
    let mut cases: Vec<(&'static str, fn())> = vec![
        ("sopcat_gallery_grid_warning", gallery_grid_warning),
        ("sopcat_motion", motion),
        ("sopcat_set_color", set_color),
        ("sopcat_membership", membership),
        ("sopcat_manifest", manifest_default),
        ("sopcat_graph_edits", run_body),
        ("sopcat_morph_sketch", morph_sketch),
        ("sopcat_node_inputs", node_inputs),
    ];
    // Exhaustive qualification checks every field of every registered SOP.
    if std::env::var("RAYS_SLOW").as_deref() == Ok("1") {
        cases.push(("sopcat_manifest_exhaustive", manifest_exhaustive));
        cases.push(("sopcat_perturbation", perturbation));
    }
    cases
}
fn trace(line: &str) {
    if let Ok(path) = std::env::var("RAYS_T_TRACE") {
        use std::io::Write;
        if let Ok(mut f) = std::fs::OpenOptions::new().create(true).append(true).open(path) {
            let _ = writeln!(f, "{line}");
        }
    }
}
