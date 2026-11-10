//! Port of lib/flow_sop/test_metadata_kernel.ml: authored metadata operations preserve native owner,
//! storage and ordering semantics.
use std::sync::Arc;
use super::support::*;
use super::test_group_kernel::{evaluated, flow_ok, ok};
use crate::{attribute_metadata, catalog, lisp_sop, manifest, operators};
use flow_graph::flow_edit::{apply_checked_with, ArgKey, Op};
use flow_graph::projection as proj;
use flow_ir::flow_ir::executor;
use flow_syntax::syntax::{self, Node};
use flow_syntax::ty::{self, Ty};
use flow_syntax::value::Value;
use rays_diagnostic::Diagnostic;
use rdk_core::attribute::{self, Owner, Storage};
use rdk_core::geometry as geo;
use rdk_core::packed::{float2, float3, float4, float_array, int_array};
use rdk_core::{group, topology};
use sop::context::Options as ContextOptions;
use sop::{context as sctx, payload, session};

const MAX_INT: i64 = i64::MAX;
const MIN_INT: i64 = i64::MIN;

fn quoted(value: &str) -> String {
    flow_syntax::lisp::quote_string(value.as_bytes())
}

fn cook(input: &geo::T, expression: &str) -> Result<geo::T, String> {
    let node = lisp_sop::node(&[("source".to_owned(), lisp_sop::snapshot(input))], expression);
    let mut s = session::create(32, 16_000_000).unwrap();
    let run = |s: &mut session::T, domains: i64, grain: i64| -> Result<geo::T, String> {
        let context = sctx::create(ContextOptions { domains: Some(domains), grain: Some(grain), ..Default::default() }).unwrap();
        match session::cook(s, None, &context, &node) {
            Ok(output) => payload::geometry(&output.payload).map_err(|d| rays_diagnostic::error_to_string(&d)),
            Err(error) => Err(rays_diagnostic::error_to_string(&error)),
        }
    };
    let expected = run(&mut s, 1, 97);
    for domains in [1i64, 8] {
        for grain in [97i64, 4096] {
            session::clear(&mut s);
            let actual = run(&mut s, domains, grain);
            let same = match (&expected, &actual) {
                (Ok(a), Ok(b)) => geometry_bytes(a) == geometry_bytes(b),
                (Err(a), Err(b)) => a == b,
                _ => false,
            };
            assert!(same, "Metadata mutation domain/grain results differ");
        }
    }
    session::close(&mut s);
    expected
}

fn rename_attr(owner: &str, pattern: &str, replacement: &str, conflict: &str, input: &geo::T) -> Result<geo::T, String> {
    cook(input, &format!("(sop/rename_attr (sop/ext_source) {} {} :owner {} :conflict {})", quoted(pattern), quoted(replacement), quoted(owner), quoted(conflict)))
}

fn rename_group(owner: &str, pattern: &str, replacement: &str, conflict: &str, input: &geo::T) -> Result<geo::T, String> {
    cook(input, &format!("(sop/rename_group (sop/ext_source) {} {} :owner {} :conflict {})", quoted(pattern), quoted(replacement), quoted(owner), quoted(conflict)))
}

fn delete_group(owner: &str, pattern: &str, unused: bool, input: &geo::T) -> Result<geo::T, String> {
    cook(input, &format!("(sop/delete_group (sop/ext_source) {} :owner {} :unused {})", quoted(pattern), quoted(owner), unused))
}

fn rename_edge(from: &str, into: &str, input: &geo::T) -> Result<geo::T, String> {
    cook(input, &format!("(sop/rename_edge (sop/ext_source) {} {})", quoted(from), quoted(into)))
}

fn swap(owner: &str, source: &str, destination: &str, method: &str, input: &geo::T) -> Result<geo::T, String> {
    cook(input, &format!("(sop/swap_attr (sop/ext_source) {} {} :owner {} :method {})", quoted(source), quoted(destination), quoted(owner), quoted(method)))
}

fn query(owner: Owner, name: &str, pattern: &str, input: &geo::T) -> Result<Value, Diagnostic> {
    let owner = match owner {
        Owner::Point => "point",
        Owner::Vertex => "vertex",
        Owner::Primitive => "primitive",
        Owner::Detail => "detail",
    };
    let (value, resolve) = evaluated(input, &format!("({} geo {} :owner {})", name, quoted(pattern), quoted(owner)));
    let program = flow_ok(executor::compile(&value, &executor::CompileOptions { strict: true, ..Default::default() }));
    let live = input::at_time(0.0);
    let expected = flow_eval::host::force_reference(&value, &live, None, None, Some(resolve.clone()));
    for domains in [1i64, 8] {
        parallel::run_unit(Some(domains), &|| {
            let options = executor::ForceOptions { strict: true, resolve: Some(resolve.clone()), ..Default::default() };
            let actual = executor::force(&program, &options, &live);
            assert!(actual == expected, "Metadata query strict/reference/domain results differ");
            Ok(())
        })
        .unwrap();
    }
    expected
}

fn attr_type(owner: Owner, name: &str, input: &geo::T) -> Result<Value, Diagnostic> {
    query(owner, "sop/attr_type", name, input)
}

fn attr_names(owner: Owner, pattern: &str, input: &geo::T) -> Result<Value, Diagnostic> {
    query(owner, "sop/attr_names", pattern, input)
}

fn text(s: &str) -> Value {
    Value::Text(s.as_bytes().into())
}

fn add(owner: Owner, name: &str, value: f64, geometry: &geo::T) -> geo::T {
    ok(geo::with_attribute(ok(attribute::create_owned(name, owner, Storage::Float(Arc::new(vec![value])))), geometry))
}

fn fixture() -> geo::T {
    let geometry = ok(geo::generate_point_ranges(1, &mut |_, _, x, y, z| {
        x[0] = 0.0;
        y[0] = 0.0;
        z[0] = 0.0;
    }));
    let geometry = add(Owner::Point, "source", 2.0, &geometry);
    let geometry = add(Owner::Point, "target", 7.0, &geometry);
    add(Owner::Detail, "source", 11.0, &geometry)
}

fn names(geometry: &geo::T) -> Vec<(Owner, String)> {
    geo::attributes(geometry).iter().map(|a| (attribute::owner(a), attribute::name(a).to_owned())).collect()
}

fn value(owner: Owner, name: &str, geometry: &geo::T) -> f64 {
    match geo::find_attribute(owner, name, geometry) {
        Some(a) => match attribute::storage(&a) {
            Ok(Storage::Float(values)) => values[0],
            _ => panic!("not a float attribute"),
        },
        None => panic!("missing attribute {name}"),
    }
}

fn same(a: &geo::T, b: &geo::T) -> bool {
    geo::data_id(a) == geo::data_id(b)
}

#[test]
fn rename_conflicts_canonical_p_and_type_queries() {
    let input = fixture();
    let original = geometry_bytes(&input);
    let skip = rename_attr("point", "source", "target", "skip", &input).unwrap();
    assert!(same(&skip, &input), "skip conflict must retain exact unchanged geometry");
    assert!(rename_attr("point", "source", "target", "error", &input).is_err(), "error conflict must fail");
    assert!(original == geometry_bytes(&input), "error conflict mutated input");
    let overwrite = rename_attr("point", "source", "target", "overwrite", &input).unwrap();
    assert!(value(Owner::Point, "target", &overwrite) == 2.0 && value(Owner::Detail, "source", &overwrite) == 11.0, "overwrite crossed owner or retained destination payload");
    assert!(names(&overwrite) == vec![(Owner::Point, "target".to_owned()), (Owner::Detail, "source".to_owned())], "overwrite changed stable source slot");
    assert!(rename_attr("point", "source", "P", "overwrite", &input).is_err(), "rename must reserve canonical P");
    let missed = rename_attr("any", "not_found", "renamed", "error", &input).unwrap();
    assert!(same(&missed, &input), "pattern no-match must be a no-op");
    assert!(attr_type(Owner::Point, "P", &input) == Ok(text("vec3")), "canonical P type missing");
    assert!(matches!(attr_type(Owner::Point, "absent", &input), Err(d) if d.code == "E_ATTR_TYPE"), "type query fabricated missing storage");
    let grouped = ok(geo::generate_point_ranges(4, &mut |first, last, x, y, z| {
        for i in first..last {
            x[i as usize] = i as f64;
            y[i as usize] = 0.0;
            z[i as usize] = 0.0;
        }
    }));
    let ordered = |name: &str, elements: &[i64]| ok(group::ordered(group::Owner::Point, name, 4, elements));
    let grouped = ok(geo::with_group(ordered("source", &[1, 2]), &grouped));
    let grouped = ok(geo::with_group(ordered("target", &[3, 1]), &grouped));
    let grouped = ok(geo::with_group(ordered("unused", &[]), &grouped));
    let united = rename_group("point", "source", "target", "union", &grouped).unwrap();
    assert!(geo::find_group(group::Owner::Point, "source", &united).is_none(), "union did not remove source");
    let target = geo::find_group(group::Owner::Point, "target", &united).unwrap();
    assert!(group::ordered_elements(&target) == Some(vec![3, 1, 2]), "union lost destination-first preferred order");
    let deleted = delete_group("point", "", true, &grouped).unwrap();
    assert!(
        geo::find_group(group::Owner::Point, "source", &deleted).is_some() && geo::find_group(group::Owner::Point, "target", &deleted).is_some() && geo::find_group(group::Owner::Point, "unused", &deleted).is_none(),
        "blank rules/delete_unused semantics changed"
    );
    assert!(rename_edge("missing", "missing", &grouped).is_err(), "equal missing exact edge name must fail");
}

#[test]
fn all_owners_and_storages_rename_without_copying() {
    let positions = ok(float3::view::of_owned_exn(vec![0.0, 1.0, 0.0, 2.0], vec![0.0, 0.0, 1.0, 2.0], vec![0.0; 4]));
    let t = ok(topology::polygons_owned(4, vec![0, 1, 2], vec![0, 3]));
    let geometry = ok(geo::create(positions, t, None, None, None));
    let owners = [(Owner::Point, 4usize), (Owner::Vertex, 3), (Owner::Primitive, 1), (Owner::Detail, 1)];
    let storages = |count: usize| -> Vec<(&'static str, Storage)> {
        let plane = |v: f64| vec![v; count];
        let offsets: Vec<i64> = (0..=count as i64).map(|i| i * 16).collect();
        vec![
            ("float", Storage::Float(Arc::new(plane(-0.0)))),
            ("int", Storage::Int(Arc::new(vec![MAX_INT; count]))),
            ("vec2", Storage::Float2(ok(float2::of_owned(plane(-0.0), plane(2.0))))),
            ("vec3", Storage::Float3(ok(float3::view::of_owned_exn(plane(1.0), plane(2.0), plane(3.0))))),
            ("vec4", Storage::Float4(ok(float4::of_owned(plane(1.0), plane(2.0), plane(3.0), plane(4.0))))),
            ("text", Storage::Text(Arc::new(vec!["preserved \u{2713}\n".as_bytes().to_vec(); count].into()))),
            ("float_array", Storage::FloatArray(ok(float_array::create_owned(offsets.clone(), vec![0.25; count * 16])))),
            ("int_array", Storage::IntArray(ok(int_array::create_owned(offsets, vec![MIN_INT; count * 16])))),
        ]
    };
    let mut geometry = geometry;
    for (owner, count) in owners {
        for (kind, storage) in storages(count) {
            geometry = ok(geo::with_attribute(ok(attribute::create_owned(&format!("a_{kind}"), owner, storage)), &geometry));
        }
    }
    let before = geometry_bytes(&geometry);
    let renamed = rename_attr("any", "a_*", "b_*", "error", &geometry).unwrap();
    for (owner, _) in owners {
        for kind in ["float", "int", "vec2", "vec3", "vec4", "text", "float_array", "int_array"] {
            let source = geo::find_attribute(owner, &format!("a_{kind}"), &geometry).unwrap();
            let result = geo::find_attribute(owner, &format!("b_{kind}"), &renamed).unwrap();
            assert!(attribute::storage_id(&source) == attribute::storage_id(&result), "rename copied or changed attribute storage");
            assert!(attr_type(owner, &format!("b_{kind}"), &renamed) == Ok(text(kind)), "renamed type query differs");
        }
    }
    assert!(geometry_bytes(&geometry) == before, "all-storage rename mutated source");
    match attr_names(Owner::Point, "b_*", &renamed) {
        Ok(Value::Text_array(names)) => {
            // the query's array is a copy: changing it leaves the geometry alone
            let mut copy = names.to_rows();
            copy[0] = b"mutated copy".to_vec();
            assert!(geo::find_attribute(Owner::Point, "b_float", &renamed).is_some(), "name query shared mutable geometry state");
        }
        other => panic!("attr_names is not a text array: {}", other.is_ok()),
    }
}

#[test]
fn copy_move_swap_preserve_owner_payloads_and_canonical_p() {
    let geometry = fixture();
    for (method, source_exists, source_value, target_value) in [("copy", true, Some(2.0), 2.0), ("move", false, None, 2.0), ("swap", true, Some(7.0), 2.0)] {
        let result = swap("point", "source", "target", method, &geometry).unwrap();
        assert!(geo::find_attribute(Owner::Point, "source", &result).is_some() == source_exists, "{method}: source presence differs");
        if let Some(expected) = source_value {
            assert!(value(Owner::Point, "source", &result) == expected, "{method}: source payload differs");
        }
        assert!(value(Owner::Point, "target", &result) == target_value, "{method}: destination payload differs");
        assert!(value(Owner::Detail, "source", &result) == 11.0, "{method}: crossed attribute owner");
    }
    let copied = swap("point", "P", "rest", "move", &geometry).unwrap();
    assert!(float3::data_id(geo::positions(&copied)) == float3::data_id(geo::positions(&geometry)), "moving canonical P changed positions");
    assert!(geo::find_attribute(Owner::Point, "rest", &copied).is_some(), "moving P must copy it");
    assert!(swap("point", "source", "P", "copy", &geometry).is_err(), "scalar destination P must fail");
    assert!(geometry_bytes(&geometry) == geometry_bytes(&fixture()), "metadata mutated original input");
}

#[test]
fn attr_entries_columns_keep_native_order_and_owner_filter() {
    let input = fixture();
    let texts = |owner: Option<Owner>, component: &str| -> Vec<String> {
        match attribute_metadata::entries(owner, component, &input) {
            Ok(Value::Text_array(values)) => values.iter().map(|v| String::from_utf8(v.to_vec()).unwrap()).collect(),
            _ => panic!("attr_entries column is not text"),
        }
    };
    assert!(texts(None, "name") == ["source", "target", "source"], "attr_entries names must keep native attribute order");
    assert!(texts(None, "owner") == ["point", "point", "detail"], "attr_entries owners must follow the names");
    assert!(texts(None, "type") == ["float", "float", "float"], "attr_entries types must follow the names");
    assert!(texts(Some(Owner::Point), "name") == ["source", "target"], "attr_entries owner filter lost an attribute");
    assert!(texts(Some(Owner::Primitive), "name").is_empty(), "attr_entries must be empty for an absent owner");
    assert!(attribute_metadata::entries(None, "bogus", &input).is_err(), "attr_entries accepted a bad column");
}

#[test]
fn metadata_graph_projection_and_enum_gestures_remain_checked() {
    let cat = flow_ok(catalog::of_factories(manifest::VERSION, &[], None, &sop::nodes::factories()));
    let checker = |c: &flow_check::catalog::Catalog, forms: &[syntax::Syntax]| catalog::check_workspace(operators::all(), false, None, c, forms);
    let forms = parse(
        r#"(workspace metadata_projection (graph g :context sop
    (let* [geo (sop/line) renamed (sop/rename_attr geo "N" "normal" :owner point :conflict skip)
      swapped (sop/swap_attr renamed "normal" "backup" :owner vertex :method copy)
      names (sop/attr_names geo "*" :owner primitive) kind (sop/attr_type geo :P)
      entries (sop/attr_entries geo :owner primitive)
      grouped (sop/rename_group swapped "old*" "new*" :owner edge :conflict union)] grouped)))"#,
    );
    let checked = match checker(&cat, &forms) {
        (Some(checked), d) if d.is_empty() => checked,
        (_, d) => panic!("{}", d.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    };
    let pane = proj::of_graph(&cat, &checked, "g").unwrap();
    let path = |binding: &str| vec!["g".to_owned(), binding.to_owned()];
    for (binding, ty) in [
        ("renamed", ty::geometry()),
        ("swapped", ty::geometry()),
        ("names", Ty::Array(Box::new(Ty::Text))),
        ("entries", Ty::Record(vec![("name".into(), Ty::Array(Box::new(Ty::Text))), ("type".into(), Ty::Array(Box::new(Ty::Text))), ("owner".into(), Ty::Array(Box::new(Ty::Text)))])),
        ("kind", Ty::Text),
        ("grouped", ty::geometry()),
    ] {
        let node = proj::find(&pane, &path(binding)).unwrap();
        assert!(node.ty == ty, "Metadata graph output type differs");
        assert!(node.rows.iter().any(|row| row.label == "owner"), "Metadata owner row missing");
    }
    for (binding, key, value) in [("renamed", "conflict", "overwrite"), ("swapped", "method", "move"), ("grouped", "owner", "point")] {
        let mut b = flow_syntax::arena::SyntaxBuilder::new();
        let name = b.intern(value);
        let id = b.push(0, Node::Sym(name), rays_diagnostic::Span { start: 0, finish: 0 }, None);
        let op = Op::SetArg { node: path(binding), key: ArgKey::Kw(key.to_owned()), sub: vec![], value: b.freeze().handle(id) };
        let (_, checked) = apply_checked_with(&[], false, Some(&checker), &cat, &forms, &op).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
        let pane = proj::of_graph(&cat, &checked, "g").unwrap();
        let node = proj::find(&pane, &path(binding)).unwrap();
        let row = node.rows.iter().find(|row| row.label == key).unwrap();
        assert!(flow_syntax::lisp::flat(row.expr.as_ref().unwrap()) == format!("{value:?}"), "Metadata gesture lost enum normalization");
    }
}
