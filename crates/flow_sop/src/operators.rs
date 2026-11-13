//! the SOP workspace host's operator declarations.
use flow_syntax::symbol::{self, Symbol};
use crate::{attribute_kernel, attribute_metadata};
use flow_check::op::{Args, Body, Check, Op, Out, OutArgs, Shape, Signature};
use flow_syntax::context;
use flow_syntax::ty::{self, Ty};
use flow_syntax::value::{self, Value};
use rays_diagnostic::Diagnostic;
use std::sync::{Arc, LazyLock};

type Named = [(Symbol, Ty, Option<param::Value>)];

fn fail<R>(code: &str, message: &str) -> Result<R, Diagnostic> {
    Err(value::fail(code, message))
}
fn ar(element: Ty) -> Ty {
    Ty::Array(Box::new(element))
}
fn rec(fields: &[(&str, Ty)]) -> Ty {
    Ty::Record(pairs(fields))
}
fn pairs(fields: &[(&str, Ty)]) -> Vec<(Symbol, Ty)> {
    fields.iter().map(|(n, t)| (symbol::intern(n), t.clone())).collect()
}
fn sig(pos: &[(&str, Ty)], kw: &[(&str, Ty)]) -> Signature {
    Signature { pos: pairs(pos), opt: Vec::new(), rest: None, kw: pairs(kw) }
}
fn constant(ty: Ty) -> Out {
    Arc::new(move |_| Ok(ty.clone()))
}
fn text(s: &str) -> Value {
    Value::Text(s.as_bytes().into())
}
fn arg<'a>(args: &'a Args, name: &str) -> Option<&'a Value> {
    let name = symbol::find(name)?;
    args.iter().find(|(key, _)| *key == name).map(|(_, v)| v)
}
fn text_arg(args: &Args, name: &str) -> Option<String> {
    match arg(args, name) {
        Some(Value::Text(bytes)) => Some(String::from_utf8_lossy(bytes).into_owned()),
        _ => None,
    }
}
fn component_args(args: &Args, component: &str) -> Vec<(Symbol, Value)> {
    let mut all = args.to_vec();
    all.push(("component".into(), text(component)));
    all
}
fn named(args: &Args) -> Vec<(Symbol, Ty, Option<param::Value>)> {
    args.iter()
        .map(|(name, v)| (name.clone(), value::ty_of(v), if let Value::Text(t) = v { Some(param::Value::Text(t.clone())) } else { None }))
        .collect()
}
fn find_named<'a>(args: &'a Named, name: &str) -> Option<&'a (Symbol, Ty, Option<param::Value>)> {
    let name = symbol::find(name)?;
    args.iter().find(|(n, _, _)| *n == name)
}

/// A body returning one typed source.
fn source_body(name: &'static str, ty: Ty) -> Body {
    Arc::new(move |_, _, args| Ok(value::source_value(name.to_owned(), ty.clone(), args.to_vec().into())))
}
/// A body returning a record of sources, one per component.
fn record_body(name: &'static str, fields: Vec<(Symbol, Ty)>) -> Body {
    Arc::new(move |_, _, args| {
        Ok(Value::Record(
            fields.iter().map(|(component, ty)| (component.clone(), value::source_value(name.to_owned(), ty.clone(), component_args(args, component).into()))).collect::<Vec<_>>().into(),
        ))
    })
}
fn node_body(name: &'static str) -> Body {
    Arc::new(move |_, node, args| node(name, args))
}
fn noop() -> Check {
    Arc::new(|_| Ok(()))
}

// ---------------------------------------------------------------------------------------------
// attributes

fn type_argument(args: &Named) -> Result<String, Diagnostic> {
    match find_named(args, "type") {
        None => Ok("vec3".to_owned()),
        Some((_, _, Some(param::Value::Text(kind)))) => {
            let kind = std::str::from_utf8(kind).map_err(|_| value::fail("E_ATTR_TYPE", "Attribute type must be UTF-8 text."))?.to_owned();
            attribute_kernel::value_type(&kind)?;
            Ok(kind)
        }
        Some(_) => fail("E_ATTR_TYPE", "Attribute type must be a literal storage type."),
    }
}

fn attribute_type(args: &Named) -> Result<Ty, Diagnostic> {
    attribute_kernel::value_type(&type_argument(args)?)
}

fn planar_values(kind: &str, ty: &Ty) -> bool {
    let Ty::Record(fields) = ty else { return false };
    let mut names: Vec<&str> = match kind {
        "vec2" => vec!["x", "y"],
        "vec3" => vec!["x", "y", "z"],
        "vec4" => vec!["x", "y", "z", "w"],
        _ => Vec::new(),
    };
    if names.is_empty() {
        return false;
    }
    names.sort();
    let mut have: Vec<&str> = fields.iter().map(|(n, _)| n.as_str()).collect();
    have.sort();
    have == names && fields.iter().all(|(_, t)| ty::fits(t, &ar(Ty::Float)))
}

fn validate_args(args: &Args) -> Result<(), Diagnostic> {
    match arg(args, "attribute") {
        Some(Value::Text(name)) => {
            let name = std::str::from_utf8(name).map_err(|_| value::fail("E_ATTR_NAME", "Attribute name must be UTF-8 text."))?;
            if (name).trim().is_empty() {
                return fail("E_ATTR_NAME", "An attribute needs a nonblank name.");
            }
        }
        _ => return fail("E_ATTR_NAME", "An attribute needs a nonblank name."),
    }
    if let Some(owner) = arg(args, "owner") {
        match owner {
            Value::Text(owner) => {
                attribute_kernel::owner(std::str::from_utf8(owner).map_err(|_| value::fail("E_ATTR_OWNER", "Attribute owner must be UTF-8 text."))?)?;
            }
            _ => return fail("E_ATTR_OWNER", "Attribute owner must be text."),
        }
    }
    if let Some(kind) = arg(args, "type") {
        match kind {
            Value::Text(kind) => {
                attribute_kernel::value_type(std::str::from_utf8(kind).map_err(|_| value::fail("E_ATTR_TYPE", "Attribute type must be UTF-8 text."))?)?;
            }
            _ => return fail("E_ATTR_TYPE", "Attribute type must be text."),
        }
    }
    Ok(())
}

fn attr() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/attr".into(),
        ctx: context::sop(),
        signature: sig(&[("geometry", ty::geometry()), ("attribute", Ty::Text)], &[("owner", Ty::Text), ("type", Ty::Text)]),
        out: constant(ar(Ty::Vec3)),
        out_args: Some(Arc::new(|args| attribute_type(args))),
        any_num: false,
        choices: Vec::new(),
        shape: Shape::Struct { splice: false },
        live: false,
        category: "Attributes".into(),
        arithmetic: None,
        packed_extension: None,
        check: Arc::new(validate_args),
        body: Arc::new(|_, _, args| {
            let ty = attribute_type(&named(args))?;
            match ty {
                Ty::Record(fields) => Ok(Value::Record(
                    fields
                        .into_iter()
                        .map(|(name, ty)| {
                            let component = value::struct_value("sop/attr", ty, component_args(args, &name).into());
                            (name, component)
                        })
                        .collect::<Vec<_>>().into(),
                )),
                ty => Ok(value::struct_value("sop/attr", ty, args.to_vec().into())),
            }
        }),
    }
}

fn checked_attribute_type(args: &Named) -> Result<Ty, Diagnostic> {
    let kind = type_argument(args)?;
    if !["float", "vec2", "vec3", "vec4"].contains(&kind.as_str()) {
        return fail("E_ATTR_TYPE", "Checked attributes require float, vec2, vec3 or vec4 storage.");
    }
    Ok(rec(&[("values", attribute_type(args)?), ("valid", ar(Ty::Int))]))
}

fn attr_checked() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/attr_checked".into(),
        out: constant(rec(&[("values", ar(Ty::Vec3)), ("valid", ar(Ty::Int))])),
        out_args: Some(Arc::new(|args| checked_attribute_type(args))),
        check: Arc::new(|args| {
            validate_args(args)?;
            let kind = text_arg(args, "type").unwrap_or_else(|| "vec3".to_owned());
            if !["float", "vec2", "vec3", "vec4"].contains(&kind.as_str()) {
                return fail("E_ATTR_TYPE", "Checked attributes require float, vec2, vec3 or vec4 storage.");
            }
            Ok(())
        }),
        body: Arc::new(|_, _, args| match checked_attribute_type(&named(args))? {
            Ty::Record(fields) => Ok(Value::Record(
                fields
                    .into_iter()
                    .map(|(name, ty)| {
                        let component = value::source_value("sop/attr_checked", ty, component_args(args, &name).into());
                        (name, component)
                    })
                    .collect::<Vec<_>>().into(),
            )),
            _ => unreachable!("checked_attribute_type is a record"),
        }),
        ..attr()
    }
}

/// Float-family attribute as planes with a cook-time width: x, y, z, w (planes past `width` are zero).
fn planes_type() -> Ty {
    rec(&[("width", Ty::Int), ("x", ar(Ty::Float)), ("y", ar(Ty::Float)), ("z", ar(Ty::Float)), ("w", ar(Ty::Float))])
}

fn attr_planes() -> Op {
    let Ty::Record(fields) = planes_type() else { unreachable!("planes_type is a record") };
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/attr_planes".into(),
        signature: sig(&[("geometry", ty::geometry()), ("attribute", Ty::Text)], &[("owner", Ty::Text)]),
        out: constant(planes_type()),
        out_args: None,
        check: Arc::new(validate_args),
        body: record_body("sop/attr_planes", fields),
        ..attr()
    }
}

fn with_attr() -> Op {
    let base = attr();
    let mut signature = base.signature.clone();
    signature.pos.push(("values".into(), Ty::Any));
    signature.kw.extend(pairs(&[("selection", ar(Ty::Int)), ("default", Ty::Any), ("when", Ty::Bool)]));
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/with_attr".into(),
        signature,
        out: constant(ty::geometry()),
        out_args: Some(Arc::new(|args| {
            let ty = attribute_type(args)?;
            let present = |name: &str| args.iter().any(|(key, _, _)| key == name);
            if present("selection") != present("default") {
                return fail("E_ATTR_SELECTION", "Selected writes require both selection and default.");
            }
            if present("selection") {
                checked_attribute_type(args)?;
                let expected = ty::elem(&ty).expect("attribute types are arrays");
                if let Some((_, actual, _)) = find_named(args, "default") {
                    if !ty::fits(actual, &expected) {
                        return fail("E_ATTR_TYPE", &format!("Attribute default requires {}.", ty::to_string(&expected)));
                    }
                }
            }
            if let Some((_, actual, _)) = find_named(args, "values") {
                if !(ty::fits(actual, &ty) || (!present("selection") && planar_values(&type_argument(args)?, actual))) {
                    return fail("E_ATTR_TYPE", &format!("Attribute values require {}.", ty::to_string(&ty)));
                }
            }
            Ok(ty::geometry())
        })),
        shape: Shape::Scalar,
        check: Arc::new(|args| {
            validate_args(args)?;
            if arg(args, "selection").is_some() != arg(args, "default").is_some() {
                return fail("E_ATTR_SELECTION", "Selected writes require both selection and default.");
            }
            Ok(())
        }),
        body: guarded_body("sop/with_attr"),
        ..base
    }
}

/// `:when` guards the write: a static guard decides it here, a cooked one when it cooks.
fn guarded_body(name: &'static str) -> Body {
    Arc::new(move |_, node, args| match arg(args, "when") {
        Some(Value::Bool(false)) => arg(args, "geometry").cloned().ok_or_else(|| value::fail("E_ARITY", "Missing operator input geometry.")),
        Some(Value::Bool(true)) => {
            let mut rest = args.to_vec();
            if let Some(at) = rest.iter().position(|(k, _)| k == "when") {
                rest.remove(at);
            }
            node(name, &rest)
        }
        _ => node(name, args),
    })
}

fn has_attr() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/has_attr".into(),
        out: constant(Ty::Bool),
        out_args: Some(Arc::new(|args| {
            if find_named(args, "type").is_some() {
                attribute_type(args)?;
            }
            Ok(Ty::Bool)
        })),
        body: source_body("sop/has_attr", Ty::Bool),
        ..attr()
    }
}

fn delete_attr() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/delete_attr".into(),
        signature: sig(&[("geometry", ty::geometry()), ("attribute", Ty::Text)], &[("owner", Ty::Text), ("when", Ty::Bool)]),
        out: constant(ty::geometry()),
        out_args: None,
        shape: Shape::Scalar,
        choices: vec![("owner".into(), strings(&["point", "vertex", "primitive", "detail"]))],
        check: Arc::new(|args| {
            match text_arg(args, "attribute") {
                Some(pattern) if !(&pattern).trim().is_empty() => {
                    rdk_core::attribute_pattern::compile(&pattern).map_err(|e| value::fail("E_ATTR_PATTERN", rdk_core::error::message(&e)))?;
                }
                _ => return fail("E_ATTR_NAME", "An attribute pattern must be nonblank."),
            }
            if let Some(owner) = arg(args, "owner") {
                match owner {
                    Value::Text(owner) if ["point", "vertex", "primitive", "detail"].contains(&String::from_utf8_lossy(owner).as_ref()) => {}
                    _ => return fail("E_ATTR_OWNER", "Attribute owner is point, vertex, primitive or detail."),
                }
            }
            Ok(())
        }),
        body: guarded_body("sop/delete_attr"),
        ..attr()
    }
}

fn strings(items: &[&str]) -> Vec<String> {
    items.iter().map(|s| (*s).to_owned()).collect()
}

// ---------------------------------------------------------------------------------------------
// topology

fn count() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/count".into(),
        category: "Topology".into(),
        signature: sig(&[("geometry", ty::geometry()), ("element", Ty::Text)], &[]),
        out: constant(Ty::Int),
        out_args: None,
        choices: vec![("element".into(), strings(&["points", "vertices", "primitives", "edges"]))],
        check: Arc::new(|args| match text_arg(args, "element") {
            Some(element) if ["points", "vertices", "primitives", "edges"].contains(&element.as_str()) => Ok(()),
            _ => fail("E_TOPOLOGY_COUNT", "Count element is points, vertices, primitives or edges."),
        }),
        body: source_body("sop/count", Ty::Int),
        ..attr()
    }
}

fn int_fields(names: &[&str]) -> Vec<(Symbol, Ty)> {
    names.iter().map(|n| (symbol::intern(n), ar(Ty::Int))).collect()
}

/// `{edges with name; out; body}`: a record of integer arrays, one source per component.
fn edges_like(name: &'static str, components: &[&str]) -> Op {
    let fields = int_fields(components);
    Op {
        doc: "".into(), example: "".into(),
        name: name.into(),
        signature: sig(&[("geometry", ty::geometry())], &[]),
        out: constant(Ty::Record(fields.clone())),
        choices: Vec::new(),
        check: noop(),
        body: record_body(name, fields),
        ..count()
    }
}

fn edges() -> Op {
    edges_like("sop/edges", &["a", "b"])
}
fn topology() -> Op {
    edges_like("sop/topology", &["vertex_points", "primitive_offsets", "kinds"])
}

const ADJACENCY_FIELDS: [&str; 11] = [
    "primitive_of_vertex",
    "next_vertex",
    "previous_vertex",
    "edge_of_vertex",
    "opposite_vertex",
    "edge_a",
    "edge_b",
    "point_edge_offsets",
    "point_edges",
    "point_vertex_offsets",
    "point_vertices",
];

fn adjacency() -> Op {
    edges_like("sop/adjacency", &ADJACENCY_FIELDS)
}
/// Per incident corner (the slots of sop/adjacency's point_vertices): the first vertex of its face
/// and the corners the face's fan covers, 0 unless the face is a polygon of three corners or more.
fn fans() -> Op {
    edges_like("sop/fans", &["first", "corners"])
}
fn triangles() -> Op {
    edges_like("sop/triangles", &["offsets", "ok", "primitive", "a", "b", "c"])
}

fn components() -> Op {
    let fields = vec![("id".into(), ar(Ty::Int)), ("count".into(), Ty::Int)];
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/components".into(),
        signature: sig(&[("geometry", ty::geometry())], &[("over", Ty::Text), ("mask", ar(Ty::Int)), ("seams", ar(Ty::Int))]),
        out: constant(Ty::Record(fields.clone())),
        choices: vec![("over".into(), strings(&["edges", "points"]))],
        check: Arc::new(|args| match arg(args, "over") {
            None => Ok(()),
            Some(Value::Text(over)) if over.as_ref() == b"edges" || over.as_ref() == b"points" => Ok(()),
            Some(_) => fail("E_COMPONENTS_OVER", "Components :over is edges or points."),
        }),
        body: record_body("sop/components", fields),
        ..edges()
    }
}

// graph/ operators: pure array functions over the adjacency record.
fn adjacency_type() -> Ty {
    Ty::Record(int_fields(&ADJACENCY_FIELDS))
}
fn rows_type() -> Ty {
    rec(&[("offsets", ar(Ty::Int)), ("values", ar(Ty::Int))])
}

fn graph_op(name: &'static str, pos: &[(&str, Ty)], kw: &[(&str, Ty)], fields: &[(&str, Ty)]) -> Op {
    let fields = pairs(fields);
    Op {
        doc: "".into(), example: "".into(),
        name: name.into(),
        category: "Topology".into(),
        signature: sig(pos, kw),
        out: constant(Ty::Record(fields.clone())),
        choices: Vec::new(),
        check: noop(),
        body: record_body(name, fields),
        ..count()
    }
}

fn shortest_paths() -> Op {
    graph_op(
        "graph/shortest_paths",
        &[("adjacency", adjacency_type()), ("lengths", ar(Ty::Float)), ("roots", ar(Ty::Int))],
        &[
            ("radius", Ty::Float),
            ("hops", Ty::Bool),
            ("blocked_points", ar(Ty::Int)),
            ("blocked_edges", ar(Ty::Int)),
            ("target", Ty::Int),
            ("ties", Ty::Text),
        ],
        &[("distance", ar(Ty::Float)), ("parent", ar(Ty::Int)), ("path", ar(Ty::Int)), ("order", ar(Ty::Int))],
    )
}
fn forest() -> Op {
    graph_op(
        "graph/forest",
        &[("parents", ar(Ty::Int))],
        &[("mask", ar(Ty::Int))],
        &[("order", ar(Ty::Int)), ("depth", ar(Ty::Int)), ("root", ar(Ty::Int)), ("offsets", ar(Ty::Int)), ("children", ar(Ty::Int))],
    )
}
fn color() -> Op {
    graph_op("graph/color", &[("adjacency", rows_type()), ("order", ar(Ty::Int))], &[], &[("color", ar(Ty::Int)), ("count", Ty::Int)])
}
fn chains() -> Op {
    graph_op(
        "graph/chains",
        &[("adjacency", adjacency_type())],
        &[("mask", ar(Ty::Int))],
        &[("offsets", ar(Ty::Int)), ("values", ar(Ty::Int)), ("closed", ar(Ty::Int))],
    )
}

fn mesh_fields() -> Vec<(&'static str, Ty)> {
    vec![
        ("points", ar(Ty::Vec3)),
        ("vertex_points", ar(Ty::Int)),
        ("primitive_offsets", ar(Ty::Int)),
        ("kinds", ar(Ty::Int)),
    ]
}

fn needs_all(op: &'static str, fields: Vec<Symbol>) -> OutArgs {
    Arc::new(move |args| {
        for name in &fields {
            if !args.iter().any(|(key, _, _)| key == name) {
                return fail("E_ARGS", &format!("{op} needs :{name}."));
            }
        }
        Ok(ty::geometry())
    })
}
fn requires_all(op: &'static str, fields: Vec<Symbol>) -> Check {
    Arc::new(move |args| {
        for name in &fields {
            if !args.iter().any(|(key, _)| key == name) {
                return fail("E_ARGS", &format!("{op} needs :{name}."));
            }
        }
        Ok(())
    })
}

fn mesh() -> Op {
    let fields = mesh_fields();
    let names: Vec<Symbol> = fields.iter().map(|(n, _)| symbol::intern(n)).collect();
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/mesh".into(),
        signature: sig(&[], &fields),
        out: constant(ty::geometry()),
        out_args: Some(needs_all("sop/mesh", names.clone())),
        shape: Shape::Scalar,
        choices: Vec::new(),
        check: requires_all("sop/mesh", names),
        body: node_body("sop/mesh"),
        ..count()
    }
}

/// A separate typed constructor keeps existing mesh/error polymorphism intact. Both representations
/// use the same owned native topology boundary.
fn mesh_planes() -> Op {
    let planes = rec(&[("x", ar(Ty::Float)), ("y", ar(Ty::Float)), ("z", ar(Ty::Float))]);
    let mut fields = mesh_fields();
    fields[0] = ("points", planes);
    let names: Vec<Symbol> = fields.iter().map(|(n, _)| symbol::intern(n)).collect();
    let needs = needs_all("sop/mesh_planes", names.clone());
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/mesh_planes".into(),
        signature: sig(&[], &fields),
        out_args: Some(Arc::new(move |args| {
            needs(args)?;
            if let Some((_, Ty::Record(fields), _)) = find_named(args, "points") {
                if fields.len() != 3 {
                    return fail("E_TYPE", "Mesh planes require exactly x, y and z float-array fields.");
                }
            }
            Ok(ty::geometry())
        })),
        check: requires_all("sop/mesh_planes", names),
        body: node_body("sop/mesh_planes"),
        ..mesh()
    }
}

// ---------------------------------------------------------------------------------------------
// groups

fn group_check(args: &Args) -> Result<(), Diagnostic> {
    match text_arg(args, "name") {
        Some(name) if !(&name).trim().is_empty() => {}
        _ => return fail("E_GROUP_NAME", "Group name must not be blank."),
    }
    if let Some(owner) = arg(args, "owner") {
        match owner {
            Value::Text(owner) => {
                crate::group_kernel::owner(&String::from_utf8_lossy(owner))?;
            }
            _ => return fail("E_GROUP_OWNER", "Group owner is point, vertex, primitive or edge."),
        }
    }
    if let Some(target) = arg(args, "as_owner") {
        match target {
            Value::Text(t) if t.as_ref() == b"point" => {}
            _ => return fail("E_GROUP_TARGET", "Group promotion target is point."),
        }
    }
    Ok(())
}

fn group() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/group".into(),
        category: "Groups".into(),
        signature: sig(&[("geometry", ty::geometry())], &[("name", Ty::Text), ("owner", Ty::Text), ("as_owner", Ty::Text)]),
        out: constant(ar(Ty::Int)),
        out_args: None,
        choices: vec![
            ("owner".into(), strings(&["point", "vertex", "primitive", "edge"])),
            ("as_owner".into(), strings(&["point"])),
        ],
        check: Arc::new(group_check),
        body: source_body("sop/group", ar(Ty::Int)),
        ..attr()
    }
}

fn without_as_owner(choices: &[(Symbol, Vec<String>)]) -> Vec<(Symbol, Vec<String>)> {
    choices.iter().filter(|(n, _)| n != "as_owner").cloned().collect()
}

fn group_order_type() -> Ty {
    rec(&[("ordered", Ty::Bool), ("indices", ar(Ty::Int))])
}

fn group_order() -> Op {
    let base = group();
    let mut signature = base.signature.clone();
    signature.kw.retain(|(n, _)| n != "as_owner");
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/group_order".into(),
        out: constant(group_order_type()),
        signature,
        choices: without_as_owner(&base.choices),
        body: record_body("sop/group_order", pairs(&[("ordered", Ty::Bool), ("indices", ar(Ty::Int))])),
        ..base
    }
}

fn group_names() -> Op {
    let base = group();
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/group_names".into(),
        choices: without_as_owner(&base.choices),
        signature: sig(&[("geometry", ty::geometry()), ("pattern", Ty::Text)], &[("owner", Ty::Text)]),
        out: constant(ar(Ty::Text)),
        check: Arc::new(|args| {
            if let Some(owner) = arg(args, "owner") {
                match owner {
                    Value::Text(owner) => {
                        crate::group_kernel::owner(&String::from_utf8_lossy(owner))?;
                    }
                    _ => return fail("E_GROUP_OWNER", "Group owner is point, vertex, primitive or edge."),
                }
            }
            match arg(args, "pattern") {
                Some(Value::Text(pattern)) => rdk_core::attribute_pattern::compile(&String::from_utf8_lossy(pattern))
                    .map(|_| ())
                    .map_err(|e| value::fail("E_GROUP_PATTERN", rdk_core::error::message(&e))),
                _ => fail("E_GROUP_PATTERN", "Group pattern must be text."),
            }
        }),
        body: source_body("sop/group_names", ar(Ty::Text)),
        ..base
    }
}

fn group_entries() -> Op {
    let fields = vec![("owners".into(), ar(Ty::Text)), ("names".into(), ar(Ty::Text))];
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/group_entries".into(),
        signature: sig(&[("geometry", ty::geometry()), ("pattern", Ty::Text)], &[("owner", Ty::Text)]),
        out: constant(Ty::Record(fields.clone())),
        choices: vec![("owner".into(), strings(&["point", "vertex", "primitive", "edge", "any"]))],
        check: noop(),
        body: record_body("sop/group_entries", fields),
        ..group()
    }
}

fn with_group() -> Op {
    let base = group();
    let mut signature = base.signature.clone();
    signature.pos = pairs(&[("geometry", ty::geometry()), ("values", ar(Ty::Int))]);
    signature.kw.push(("order".into(), Ty::Any));
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/with_group".into(),
        signature,
        out: constant(ty::geometry()),
        shape: Shape::Scalar,
        out_args: Some(Arc::new(|args| {
            if let Some((_, actual, _)) = find_named(args, "order") {
                if !(ty::fits(actual, &ar(Ty::Int)) || ty::fits(actual, &group_order_type())) {
                    return fail("E_GROUP_ORDER", "Group order requires an integer array or an ordered/indices record.");
                }
            }
            Ok(ty::geometry())
        })),
        body: node_body("sop/with_group"),
        ..base
    }
}

// ---------------------------------------------------------------------------------------------
// context

/// The reserved argument naming the card whose inlined body reads its cook context.
pub fn context_label() -> &'static str {
    flow_eval::CONTEXT_LABEL
}

/// Inside an inlined defnode body (the host adds the card's cook label as `$label`) a context
/// operator is a cook-time source the kernel resolves from its cook context; elsewhere it is refused.
fn context_source(what: &'static str) -> Body {
    Arc::new(move |_, _, args| match arg(args, context_label()) {
        Some(Value::Text(_)) => {
            let (name, ty) = match what {
                "Context seed" => ("sop/context_seed", Ty::Int),
                "Provenance base" => ("sop/provenance_base", Ty::Int),
                "Context frame" => ("sop/context_frame", Ty::Float),
                _ => ("sop/context_time", Ty::Float),
            };
            Ok(value::source_value(name.to_owned(), ty, args.to_vec().into()))
        }
        _ => fail("E_NODE_CONTEXT", &format!("{what} is available inside a defnode body.")),
    })
}

fn context_seed() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/context_seed".into(),
        ctx: context::value(),
        category: "Random".into(),
        signature: sig(&[("namespace", Ty::Text)], &[("salt", Ty::Int)]),
        out: constant(Ty::Int),
        out_args: None,
        shape: Shape::Scalar,
        check: Arc::new(|args| {
            match text_arg(args, "namespace") {
                Some(namespace) if !(&namespace).trim().is_empty() => {}
                _ => return fail("E_SEED_NAMESPACE", "A context seed namespace must be nonblank."),
            }
            match arg(args, "salt") {
                None | Some(Value::Int(_)) => Ok(()),
                Some(_) => fail("E_SEED_SALT", "A context seed salt must be an integer."),
            }
        }),
        body: context_source("Context seed"),
        ..attr()
    }
}

fn value_context(name: &'static str, category: &str, out: Ty, what: &'static str) -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: name.into(),
        ctx: context::value(),
        category: category.into(),
        signature: sig(&[], &[]),
        out: constant(out),
        out_args: None,
        shape: Shape::Scalar,
        check: noop(),
        body: context_source(what),
        ..attr()
    }
}

/// Exact context-seed mixing used by `sop/context_seed`, shared with host primitive fixtures.
/// Every seed/salt bit participates; namespace and label are separately framed UTF-8 parts.
pub fn seed_for(salt: i64, seed: i64, namespace: &str, label: &str) -> i64 {
    attribute_kernel::seed_for(salt, seed, namespace, label)
}

// ---------------------------------------------------------------------------------------------
// metadata

fn metadata_text(args: &Args, name: &str) -> Result<String, Diagnostic> {
    text_arg(args, name).map_or_else(|| fail("E_METADATA_TEXT", "Metadata names and patterns must be text."), Ok)
}

fn metadata_owner(args: &Args, parse: &dyn Fn(&str) -> Result<(), Diagnostic>) -> Result<(), Diagnostic> {
    match arg(args, "owner") {
        None => Ok(()),
        Some(Value::Text(owner)) => parse(&String::from_utf8_lossy(owner)),
        Some(_) => fail("E_METADATA_OWNER", "Metadata owner must be text."),
    }
}

fn attribute_owner_ok(owner: &str) -> Result<(), Diagnostic> {
    attribute_metadata::attribute_owner(owner).map(|_| ())
}
fn group_owner_ok(owner: &str) -> Result<(), Diagnostic> {
    attribute_metadata::group_owner(owner).map(|_| ())
}

fn rewrite_check(args: &Args, parse: &dyn Fn(&str) -> Result<(), Diagnostic>, conflict: &dyn Fn(&str) -> Result<(), Diagnostic>) -> Result<(), Diagnostic> {
    metadata_owner(args, parse)?;
    match arg(args, "conflict") {
        None => {}
        Some(Value::Text(policy)) => conflict(&String::from_utf8_lossy(policy))?,
        Some(_) => return fail("E_METADATA_CONFLICT", "Conflict policy must be text."),
    }
    rdk_core::attribute_pattern::compile_rewrite(&metadata_text(args, "pattern")?, &metadata_text(args, "replacement")?)
        .map(|_| ())
        .map_err(|e| value::fail("E_METADATA_PATTERN", rdk_core::error::message(&e)))
}

fn rename_attr() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/rename_attr".into(),
        category: "Metadata".into(),
        signature: sig(
            &[("geometry", ty::geometry()), ("pattern", Ty::Text), ("replacement", Ty::Text)],
            &[("owner", Ty::Text), ("conflict", Ty::Text)],
        ),
        choices: vec![
            ("owner".into(), strings(&["point", "vertex", "primitive", "detail", "any"])),
            ("conflict".into(), strings(&["skip", "error", "overwrite"])),
        ],
        check: Arc::new(|args| {
            rewrite_check(args, &attribute_owner_ok, &|policy| attribute_metadata::attr_conflict(policy).map(|_| ()))
        }),
        body: node_body("sop/rename_attr"),
        ..delete_attr()
    }
}

fn rename_group() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/rename_group".into(),
        choices: vec![
            ("owner".into(), strings(&["point", "vertex", "primitive", "edge", "any"])),
            ("conflict".into(), strings(&["skip", "error", "overwrite", "union"])),
        ],
        check: Arc::new(|args| {
            metadata_owner(args, &group_owner_ok)?;
            match arg(args, "conflict") {
                None => Ok(()),
                Some(Value::Text(policy)) => attribute_metadata::group_conflict(&String::from_utf8_lossy(policy)).map(|_| ()),
                Some(_) => fail("E_METADATA_CONFLICT", "Conflict policy must be text."),
            }
        }),
        body: node_body("sop/rename_group"),
        ..rename_attr()
    }
}

fn swap_attr() -> Op {
    let base = rename_attr();
    let mut signature = base.signature.clone();
    signature.pos = pairs(&[("geometry", ty::geometry()), ("source", Ty::Text), ("destination", Ty::Text)]);
    signature.kw = pairs(&[("owner", Ty::Text), ("method", Ty::Text)]);
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/swap_attr".into(),
        signature,
        choices: vec![
            ("owner".into(), strings(&["point", "vertex", "primitive", "detail"])),
            ("method".into(), strings(&["swap", "move", "copy"])),
        ],
        check: Arc::new(|args| {
            metadata_owner(args, &attribute_owner_ok)?;
            match arg(args, "method") {
                None => Ok(()),
                Some(Value::Text(method)) => {
                    let method = String::from_utf8_lossy(method).trim().to_ascii_lowercase();
                    if ["swap", "move", "copy"].contains(&method.as_str()) {
                        Ok(())
                    } else {
                        fail("E_METADATA_METHOD", "Attribute method is swap, move or copy.")
                    }
                }
                Some(_) => fail("E_METADATA_METHOD", "Attribute method is swap, move or copy."),
            }
        }),
        body: node_body("sop/swap_attr"),
        ..base
    }
}

fn delete_attrs() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/delete_attrs".into(),
        category: "Metadata".into(),
        signature: sig(
            &[("geometry", ty::geometry())],
            &[
                ("reference", ty::geometry()),
                ("inverse", Ty::Bool),
                ("point", Ty::Text),
                ("vertex", Ty::Text),
                ("primitive", Ty::Text),
                ("detail", Ty::Text),
            ],
        ),
        choices: Vec::new(),
        check: Arc::new(|args| {
            for name in ["point", "vertex", "primitive", "detail"] {
                match arg(args, name) {
                    None => {}
                    Some(Value::Text(pattern)) => {
                        let pattern = String::from_utf8_lossy(pattern);
                        if !(&pattern).trim().is_empty() {
                            rdk_core::attribute_pattern::compile(&pattern).map_err(|e| value::fail("E_METADATA_PATTERN", rdk_core::error::message(&e)))?;
                        }
                    }
                    Some(_) => return fail("E_METADATA_PATTERN", "Delete patterns must be text."),
                }
            }
            Ok(())
        }),
        body: node_body("sop/delete_attrs"),
        ..delete_attr()
    }
}

fn delete_group() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/delete_group".into(),
        signature: sig(&[("geometry", ty::geometry()), ("pattern", Ty::Text)], &[("owner", Ty::Text), ("unused", Ty::Bool)]),
        choices: vec![("owner".into(), strings(&["point", "vertex", "primitive", "edge", "any"]))],
        check: Arc::new(|args| metadata_owner(args, &group_owner_ok)),
        body: node_body("sop/delete_group"),
        ..delete_attrs()
    }
}

fn rename_edge() -> Op {
    let base = rename_attr();
    let mut signature = base.signature.clone();
    signature.kw = Vec::new();
    Op { name: "sop/rename_edge".into(), signature, choices: Vec::new(), check: noop(), body: node_body("sop/rename_edge"), ..base }
}

fn delete_edge() -> Op {
    let base = rename_edge();
    let mut signature = base.signature.clone();
    signature.pos = pairs(&[("geometry", ty::geometry()), ("name", Ty::Text)]);
    Op { name: "sop/delete_edge".into(), signature, body: node_body("sop/delete_edge"), ..base }
}

fn attr_type() -> Op {
    let base = attr();
    let mut signature = base.signature.clone();
    signature.kw = pairs(&[("owner", Ty::Text)]);
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/attr_type".into(),
        category: "Metadata".into(),
        signature,
        out: constant(Ty::Text),
        out_args: None,
        body: source_body("sop/attr_type", Ty::Text),
        ..base
    }
}

fn attr_names() -> Op {
    let base = attr_type();
    let mut signature = base.signature.clone();
    signature.pos = pairs(&[("geometry", ty::geometry()), ("pattern", Ty::Text)]);
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/attr_names".into(),
        signature,
        out: constant(ar(Ty::Text)),
        check: Arc::new(|args| {
            metadata_owner(args, &attribute_owner_ok)?;
            rdk_core::attribute_pattern::compile(&metadata_text(args, "pattern")?)
                .map(|_| ())
                .map_err(|e| value::fail("E_ATTR_PATTERN", rdk_core::error::message(&e)))
        }),
        body: source_body("sop/attr_names", ar(Ty::Text)),
        ..base
    }
}

fn attr_entries() -> Op {
    let base = attr_type();
    let mut signature = base.signature.clone();
    signature.pos = pairs(&[("geometry", ty::geometry())]);
    let fields = vec![("name".into(), ar(Ty::Text)), ("type".into(), ar(Ty::Text)), ("owner".into(), ar(Ty::Text))];
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/attr_entries".into(),
        signature,
        out: constant(Ty::Record(fields.clone())),
        check: Arc::new(|args| metadata_owner(args, &attribute_owner_ok)),
        body: record_body("sop/attr_entries", fields),
        ..base
    }
}

fn glob_error() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "text/glob_error".into(),
        ctx: context::value(),
        category: "Text".into(),
        signature: sig(&[("pattern", Ty::Text)], &[]),
        choices: Vec::new(),
        check: noop(),
        body: Arc::new(|_, _, args| {
            let message = match rdk_core::attribute_pattern::compile(&metadata_text(args, "pattern")?) {
                Ok(_) => String::new(),
                Err(e) => rdk_core::error::message(&e).to_owned(),
            };
            Ok(text(&message))
        }),
        ..attr_type()
    }
}

fn pattern_replacement() -> Signature {
    sig(&[("pattern", Ty::Text), ("replacement", Ty::Text)], &[])
}
fn pattern_replacement_text() -> Signature {
    sig(&[("pattern", Ty::Text), ("replacement", Ty::Text), ("text", Ty::Text)], &[])
}

fn glob_rewrite_error() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "text/glob_rewrite_error".into(),
        signature: pattern_replacement(),
        body: Arc::new(|_, _, args| {
            let message = match rdk_core::attribute_pattern::compile_rewrite(&metadata_text(args, "pattern")?, &metadata_text(args, "replacement")?) {
                Ok(_) => String::new(),
                Err(e) => rdk_core::error::message(&e).to_owned(),
            };
            Ok(text(&message))
        }),
        ..glob_error()
    }
}

fn glob_rewrite_set_error() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "text/glob_rewrite_set_error".into(),
        signature: pattern_replacement(),
        body: Arc::new(|_, _, args| {
            let message = match rdk_core::attribute_pattern::compile_rewrite_set(&metadata_text(args, "pattern")?, &metadata_text(args, "replacement")?) {
                Ok(_) => String::new(),
                Err(e) => rdk_core::error::message(&e).to_owned(),
            };
            Ok(text(&message))
        }),
        ..glob_error()
    }
}

/// Symbolic prototype arguments retain the ordinary typed Source boundary, so these pure text
/// operations also run inside structured cooked loops.
fn pure_source(ty: Ty, name: &str, args: &Args, run: &dyn Fn() -> Result<Value, Diagnostic>) -> Result<Value, Diagnostic> {
    if args.iter().any(|(_, v)| matches!(v, Value::Source(..) | Value::Residual(_))) {
        Ok(value::source_value(name.to_owned(), ty, args.to_vec().into()))
    } else {
        run()
    }
}

fn glob_rewrite_op(name: &'static str, rewrite: fn(&str, &str, &str) -> Result<String, Diagnostic>) -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: name.into(),
        signature: pattern_replacement_text(),
        body: Arc::new(move |_, _, args| {
            pure_source(Ty::Text, name, args, &|| {
                let (pattern, replacement, value) = (metadata_text(args, "pattern")?, metadata_text(args, "replacement")?, metadata_text(args, "text")?);
                rewrite(&pattern, &replacement, &value).map(|s| text(&s))
            })
        }),
        ..glob_error()
    }
}

fn glob_rewrite() -> Op {
    glob_rewrite_op("text/glob_rewrite", attribute_metadata::glob_rewrite)
}
fn glob_rewrite_set() -> Op {
    glob_rewrite_op("text/glob_rewrite_set", attribute_metadata::glob_rewrite_set)
}

fn glob_rewrite_set_matches() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "text/glob_rewrite_set_matches".into(),
        signature: pattern_replacement_text(),
        out: constant(Ty::Bool),
        body: Arc::new(|_, _, args| {
            pure_source(Ty::Bool, "text/glob_rewrite_set_matches", args, &|| {
                let (pattern, replacement, value) = (metadata_text(args, "pattern")?, metadata_text(args, "replacement")?, metadata_text(args, "text")?);
                attribute_metadata::glob_rewrite_set_matches(&pattern, &replacement, &value).map(Value::Bool)
            })
        }),
        ..glob_error()
    }
}

fn glob_literal() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "text/glob_literal".into(),
        signature: sig(&[("text", Ty::Text)], &[]),
        body: Arc::new(|_, _, args| {
            pure_source(Ty::Text, "text/glob_literal", args, &|| attribute_metadata::glob_literal(&metadata_text(args, "text")?).map(|s| text(&s)))
        }),
        ..glob_error()
    }
}

fn group_name_valid() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "text/group_name_valid".into(),
        signature: sig(&[("text", Ty::Text)], &[]),
        out: constant(Ty::Bool),
        body: Arc::new(|_, _, args| {
            pure_source(Ty::Bool, "text/group_name_valid", args, &|| {
                Ok(Value::Bool(rdk_ops::group_ops::valid_group_name(&metadata_text(args, "text")?)))
            })
        }),
        ..glob_error()
    }
}

fn group_name_forced() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "text/group_name_forced".into(),
        signature: sig(&[("text", Ty::Text)], &[]),
        body: Arc::new(|_, _, args| {
            pure_source(Ty::Text, "text/group_name_forced", args, &|| {
                Ok(text(&rdk_ops::group_ops::force_valid_group_name(&metadata_text(args, "text")?)))
            })
        }),
        ..glob_error()
    }
}

fn with_planes() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/with_planes".into(),
        signature: sig(&[("geometry", ty::geometry()), ("attribute", Ty::Text), ("planes", planes_type())], &[("owner", Ty::Text)]),
        out_args: None,
        check: Arc::new(validate_args),
        body: node_body("sop/with_planes"),
        ..mesh()
    }
}

/// Weighted carry for one owner: new row i mixes source rows a.(i) and b.(i) at t.(i).
fn mix_type() -> Ty {
    rec(&[("a", ar(Ty::Int)), ("b", ar(Ty::Int)), ("t", ar(Ty::Float))])
}

fn remap() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/remap".into(),
        out_args: None,
        signature: sig(
            &[("source", ty::geometry()), ("target", ty::geometry())],
            &[
                ("points", ar(Ty::Int)),
                ("vertices", ar(Ty::Int)),
                ("primitives", ar(Ty::Int)),
                ("point_mix", mix_type()),
                ("vertex_mix", mix_type()),
                ("primitive_mix", mix_type()),
                ("positions", Ty::Text),
                ("edges", rec(&[("offsets", ar(Ty::Int)), ("values", ar(Ty::Int))])),
                ("lead", Ty::Bool),
                ("lead_groups", Ty::Bool),
            ],
        ),
        choices: vec![("positions".into(), strings(&["target", "mapped"]))],
        check: Arc::new(|args| {
            for (name, mix) in [("points", "point_mix"), ("vertices", "vertex_mix"), ("primitives", "primitive_mix")] {
                match (arg(args, name).is_some(), arg(args, mix).is_some()) {
                    (true, false) | (false, true) => {}
                    (false, false) => return fail("E_ARGS", &format!("sop/remap needs :{name} or :{mix}.")),
                    (true, true) => return fail("E_ARGS", &format!("sop/remap takes :{name} or :{mix}, not both.")),
                }
            }
            Ok(())
        }),
        body: node_body("sop/remap"),
        ..mesh()
    }
}

fn concat() -> Op {
    Op {
        doc: "".into(), example: "".into(),
        name: "sop/concat".into(),
        signature: sig(
            &[("sources", Ty::List(Box::new(ty::geometry())))],
            &[("source_attribute", Ty::Text), ("source_base", Ty::Int), ("tag_base", Ty::Int)],
        ),
        out_args: None,
        check: noop(),
        body: node_body("sop/concat"),
        ..mesh()
    }
}

static ALL: LazyLock<Vec<Op>> = LazyLock::new(|| {
    let mut ops = vec![
        flow_check::table::operator(std::path::PathBuf::new()),
        attr(),
        attr_checked(),
        with_attr(),
        has_attr(),
        delete_attr(),
        count(),
        edges(),
        topology(),
        adjacency(),
        fans(),
        triangles(),
        components(),
        shortest_paths(),
        forest(),
        color(),
        chains(),
        mesh(),
        mesh_planes(),
        remap(),
        concat(),
        attr_planes(),
        with_planes(),
        group(),
        group_order(),
        group_names(),
        group_entries(),
        with_group(),
        context_seed(),
        value_context("sop/provenance_base", "Copy", Ty::Int, "Provenance base"),
        value_context("sop/context_frame", "Random", Ty::Float, "Context frame"),
        value_context("sop/context_time", "Random", Ty::Float, "Context time"),
        rename_attr(),
        swap_attr(),
        delete_attrs(),
        rename_group(),
        delete_group(),
        rename_edge(),
        delete_edge(),
        attr_type(),
        attr_names(),
        attr_entries(),
        glob_error(),
        glob_rewrite_error(),
        glob_rewrite(),
        glob_rewrite_set_error(),
        glob_rewrite_set(),
        glob_rewrite_set_matches(),
        glob_literal(),
        group_name_valid(),
        group_name_forced(),
    ];
    for operator in &mut ops {
        operator.doc = flow_check::registry::description(&operator.name, &operator.signature).into();
        operator.example = flow_check::registry::example(&operator.name, &operator.signature).into();
    }
    ops
});

/// Default immutable operator declarations owned by the SOP workspace host.
pub fn all() -> &'static [Op] {
    ALL.as_slice()
}

/// How the lowerer builds a node of this kind; `None` is an ordinary catalog or Lisp node.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Lowering {
    GeometryArrays,
    Metadata,
    AttributeWrite,
    GroupWrite,
    AttributeDelete,
    Curve,
    Zone,
    Resource,
}

pub fn lowering(name: &str) -> Option<Lowering> {
    match name {
        "sop/mesh" | "sop/mesh_planes" | "sop/remap" | "sop/concat" | "sop/with_planes" => Some(Lowering::GeometryArrays),
        "sop/rename_attr" | "sop/swap_attr" | "sop/delete_attrs" | "sop/rename_group" | "sop/delete_group" | "sop/rename_edge" | "sop/delete_edge" => {
            Some(Lowering::Metadata)
        }
        "sop/with_attr" => Some(Lowering::AttributeWrite),
        "sop/with_group" => Some(Lowering::GroupWrite),
        "sop/delete_attr" => Some(Lowering::AttributeDelete),
        "sop/curve" => Some(Lowering::Curve),
        "zone/points" | "zone/pieces" => Some(Lowering::Zone),
        "image/load" | "image/render" | "image/map" | "exact" => Some(Lowering::Resource),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn find(name: &str) -> &'static Op {
        all().iter().find(|op| op.name == name).unwrap()
    }

    #[test]
    fn declares_the_authored_operator_list_in_order() {
        let names: Vec<&str> = all().iter().map(|op| op.name.as_str()).collect();
        assert_eq!(names.len(), 51);
        assert_eq!(names[0], "table/load");
        assert_eq!(names[1], "sop/attr");
        assert_eq!(names[29], "sop/provenance_base");
        assert_eq!(names[50], "text/group_name_forced");
    }

    #[test]
    fn inherits_parent_factory_fields() {
        assert_eq!(find("sop/mesh").category, "Topology");
        assert_eq!(find("sop/edges").shape, Shape::Struct { splice: false });
        assert_eq!(find("sop/with_attr").shape, Shape::Scalar);
        assert_eq!(find("text/glob_literal").ctx, context::value());
        assert_eq!(find("sop/group_order").signature.kw.len(), 2);
        assert_eq!(find("sop/rename_edge").signature.kw.len(), 0);
        assert_eq!(find("sop/with_group").signature.kw.len(), 4);
    }

    #[test]
    fn lowering_table() {
        assert_eq!(lowering("sop/mesh"), Some(Lowering::GeometryArrays));
        assert_eq!(lowering("exact"), Some(Lowering::Resource));
        assert_eq!(lowering("sop/attr"), None);
    }

    #[test]
    fn counts_and_checks_refuse_bad_arguments() {
        let bad = vec![("geometry".into(), Value::NoGeo), ("element".into(), text("faces"))];
        assert_eq!((find("sop/count").check)(&bad).err().unwrap().code, "E_TOPOLOGY_COUNT");
        let remap = vec![("points".into(), Value::NoGeo)];
        assert_eq!((find("sop/remap").check)(&remap).err().unwrap().message, "sop/remap needs :vertices or :vertex_mix.");
        let mesh = (find("sop/mesh").check)(&[]).err().unwrap();
        assert_eq!(mesh.message, "sop/mesh needs :points.");
    }

    #[test]
    fn context_operators_need_a_defnode_body() {
        let frame = input::at_time(0.0);
        let node: &dyn Fn(&str, &Args) -> Result<Value, Diagnostic> = &|_, _| unreachable!();
        let op = find("sop/context_frame");
        assert_eq!((op.body)(&frame, node, &[]).err().unwrap().code, "E_NODE_CONTEXT");
        let labelled = vec![(context_label().into(), text("card"))];
        assert!(matches!((op.body)(&frame, node, &labelled).unwrap(), Value::Source(g) if g.ty == Ty::Float && g.name == "sop/context_frame"));
        let provenance = find("sop/provenance_base");
        assert!(matches!((provenance.body)(&frame, node, &labelled).unwrap(), Value::Source(g) if g.ty == Ty::Int && g.name == "sop/provenance_base"));
    }
}

/// Ports of  and  (the packed-versus-reference parts
/// that need no JavaScript or benchmark): `Operators::all()` as the checker's operator set, the
/// prelude helpers as library sources, native reference bytes against strict packed bytes.
#[cfg(test)]
mod parity {
    use super::*;
    use crate::catalog;
    use flow_check::catalog::Catalog;
    use flow_eval as e;
    use flow_ir::packed;
    use flow_syntax::syntax::{self, Node as SNode, Syntax};
    use std::sync::LazyLock;

    // Full signed 64-bit seed and index bounds.
    const MIN_INT: i64 = i64::MIN;
    const MAX_INT: i64 = i64::MAX;
    /// Lanes per parity fixture: over the 4,096-lane parallel floor, 9 blocks of at most 1,024
    /// lanes, so 5 chunks of 2+ blocks at 8 domains with a partial last block and chunk. The reference
    /// walker is linear in lanes and dominates these cases. `random_three_outputs_match_native`
    /// keeps 100,000 lanes (no walker) for the 16-blocks-a-chunk cap of `packed::block_layout`.
    const LANES: usize = 8193;

    /// The prelude's helpers, less the ones in the sop context (they call Lisp-registered nodes,
    /// absent from this bare catalog).
    static SOURCES: LazyLock<Vec<Syntax>> = LazyLock::new(|| {
        let text = include_str!("../../../crates/flow_sop/nodes/prelude.rays");
        let forms = syntax::parse(text).unwrap();
        forms[0].children().iter().map(|&id| forms[0].child(id))
            .filter(|form| {
                syntax::head(form) == Some("defn")
                    && !matches!(
                        form.children(),
                        [_, _, k, s, ..] if matches!(form.arena().row(*k).node, SNode::Kw(name) if form.name(name) == "context") && matches!(form.arena().row(*s).node, SNode::Sym(name) if form.name(name) == "sop")
                    )
            })
            .collect()
    });

    fn checked(context: &str, bindings: &str, body: &str) -> flow_check::workspace::Workspace {
        let source = format!("(workspace w (graph g :context {context} (let* [{bindings}tested {body}] 0.0)))");
        let forms = syntax::parse(&source).unwrap();
        match catalog::check_workspace(operators_for_tests(), false, Some(SOURCES.as_slice()), &Catalog { version: 1, kinds: Vec::new() }, &forms) {
            (Some(w), d) if d.is_empty() => w,
            (_, d) => panic!("{}", d.iter().map(|d| d.message.clone()).collect::<Vec<_>>().join("\n")),
        }
    }
    fn operators_for_tests() -> &'static [Op] {
        all()
    }
    fn value_in(context: &str, seed: Option<i64>, body: &str) -> Result<Value, Diagnostic> {
        let bindings = seed.map_or(String::new(), |s| format!("seed {s} "));
        let evaluation = e::static_with(&checked(context, &bindings, body), true, None, None)?;
        let (_, values) = evaluation.records.iter().find(|(path, _)| path == &vec!["g".to_owned(), "tested".to_owned()]).expect("tested record");
        Ok(values[0].1.clone())
    }
    fn value(seed: Option<i64>, body: &str) -> Value {
        value_in("host", seed, body).unwrap()
    }

    fn compile(mapped: &Value) -> packed::T {
        let Value::Residual(r) = mapped else { panic!("mapped live value must be residual") };
        let term = e::residual::residual_view(*r).unwrap().term;
        match packed::compile_result(&packed::CompileOptions::default(), *r, &term) {
            Ok(p) => p,
            Err(ds) => panic!("{}", ds.iter().map(|d| d.message.clone()).collect::<Vec<_>>().join("\n")),
        }
    }
    fn map(seed: Option<i64>, ty: Ty, input: Value, body: &str) -> (Value, packed::T) {
        let source = format!("(fn [(i : {})] {body})", flow_syntax::ty::to_string(&ty));
        let Value::Fn(f) = value(seed, &source) else { panic!("fn expected") };
        let signature = flow_syntax::ty::FnSignature { params: vec![ty], result: Ty::Float };
        let mapped = e::host::map_function(None, None, &signature, f, &[input]).unwrap();
        let program = compile(&mapped);
        (mapped, program)
    }
    fn force(program: &packed::T, live: &input::Frame) -> Result<Value, Diagnostic> {
        packed::force(program, live, &packed::ForceOptions { strict: true, ..Default::default() })
    }
    fn reference(mapped: &Value, live: &input::Frame) -> Result<Value, Diagnostic> {
        e::host::force_reference(mapped, live, None, None, None)
    }
    fn same(expected: &[f64], actual: &Value) {
        let Value::Floats(1, actual) = actual else { panic!("float array expected") };
        assert_eq!(actual.len(), expected.len());
        for (i, (x, y)) in expected.iter().zip(actual.iter()).enumerate() {
            assert_eq!(x.to_bits(), y.to_bits(), "differs at index {i}: {x:?} <> {y:?}");
        }
    }
    fn domains(work: &dyn Fn()) {
        for count in [1, 8] {
            parallel::run_unit(Some(count), &|| {
                work();
                Ok(())
            })
            .unwrap();
        }
    }
    fn at_frame(frame: i64) -> input::Frame {
        let mut live = input::at_time(0.0);
        live.frame = frame;
        live
    }
    fn native(seed: i64, index: i64) -> f64 {
        rays_math::rand::float_at(rays_math::rand::seed(seed), index)
    }
    fn bases() -> [i64; 6] {
        [MIN_INT, MIN_INT + 1, -(1 << 53) - 1, 0, (1 << 53) + 1, MAX_INT]
    }
    fn indices(count: usize) -> Vec<i64> {
        let b = bases();
        (0..count).map(|i| b[i % b.len()].wrapping_add((i / b.len()) as i64)).collect()
    }

    fn random_proof(mapped:&Value,program:&packed::T,live:&input::Frame,expected:&[f64]){
        same(expected,&reference(mapped,live).unwrap());
        domains(&||same(expected,&force(program,live).unwrap()));
        js_exact(program,live,expected);
    }
    fn rand_range(seed:i64){
        let count=LANES as i64;
        let mapped=value(None,&format!("(map (fn [i] (rand_at (frame/index) i)) (array/range {count} :type int))"));
        let program=compile(&mapped);let live=at_frame(seed);
        let expected=(0..count).map(|i|native(seed,i)).collect::<Vec<_>>();
        random_proof(&mapped,&program,&live,&expected);
    }
    fn rand_indices(seed:i64,primitive:bool){
        let indices=indices(LANES);let input=Value::IntArray(Arc::new(indices.clone()));
        let(mapped,program,live)=if primitive{
            let(m,p)=map(Some(seed),Ty::Int,input,"(rand/at (int/add seed (frame/index)) i)");(m,p,input::at_time(0.))
        }else if seed<0{
            let(m,p)=map(Some(seed),Ty::Int,input,"(rand_at seed i)");(m,p,input::at_time(0.))
        }else{
            let(m,p)=map(None,Ty::Int,input,"(rand_at (frame/index) i)");(m,p,at_frame(seed))
        };
        let expected=indices.iter().map(|i|native(seed,*i)).collect::<Vec<_>>();
        random_proof(&mapped,&program,&live,&expected);
    }
    macro_rules! random_seed {
        ($module:ident,$seed:expr)=>{
            mod $module {use super::*;
                #[test]fn helper_indices(){rand_indices($seed,false)}
                #[test]fn primitive_indices(){rand_indices($seed,true)}
            }
        };
    }
    random_seed!(random_minimum,MIN_INT);random_seed!(random_zero,0);random_seed!(random_42,42);random_seed!(random_maximum,MAX_INT);
    #[test]fn random_range_zero(){rand_range(0)}
    #[test]fn random_range_one(){rand_range(1)}
    #[test]fn random_range_42(){rand_range(42)}
    #[test]fn random_range_maximum(){rand_range(MAX_INT)}
    #[test]fn random_scalar_signed_boundaries(){
        for seed in [MIN_INT,MIN_INT+1,0,1,42,MAX_INT]{for index in bases(){for op in ["rand_at","rand/at"]{
            let Value::Float(actual)=value(None,&format!("({op} {seed} {index})"))else{panic!("float expected")};
            assert_eq!(actual.to_bits(),native(seed,index).to_bits(),"{op} seed{seed}/index{index}");
        }}}
    }
    #[test]
    fn random_three_outputs_match_native(){
        let count=100000;
        let mapped = value(
            None,
            "(map (fn [(i : int)] [(rand/at (int/add 42 (frame/index)) i) (rand/at (int/add 43 (frame/index)) i) (rand/at (int/add 44 (frame/index)) i)]) (array/range 100000 :type int))",
        );
        let program = compile(&mapped);
        let live = input::at_time(0.0);
        let expected: Vec<f64> = (0..count * 3).map(|i| native(42 + (i % 3) as i64, (i / 3) as i64)).collect();
        domains(&|| match force(&program, &live).unwrap() {
            Value::Floats(3, actual) => same(&expected, &Value::Floats(1, actual)),
            other => panic!("vec3 array expected, got {other:?}"),
        });
    }
    #[test]
    fn random_exact_seed_and_index_columns(){
        let indices=indices(LANES);
        let live=input::at_time(0.0);
        let b = bases();
        let seeds: Vec<i64> = (0..indices.len()).map(|i| b[i / 7 % b.len()].wrapping_add((i / b.len()) as i64)).collect();
        let Value::Fn(f) = value(None, "(fn [(s : int) (i : int)] (if (int/lt s 0) (rand/at s (int/add i (frame/index))) (rand/at s (int/sub i (frame/index)))))") else {
            panic!("fn expected")
        };
        let signature = flow_syntax::ty::FnSignature { params: vec![Ty::Int, Ty::Int], result: Ty::Float };
        let mapped = e::host::map_function(None, None, &signature, f, &[Value::IntArray(Arc::new(seeds.clone())), Value::IntArray(Arc::new(indices.clone()))]).unwrap();
        let program = compile(&mapped);
        let expected: Vec<f64> = seeds.iter().zip(&indices).map(|(s, i)| native(*s, *i)).collect();
        random_proof(&mapped,&program,&live,&expected);
    }

    fn points(count: usize) -> Vec<(f64, f64, f64)> {
        let tiny = f64::from_bits(1);
        let special = [(0.0, 0.0, 0.0), (-0.0, -0.0, -0.0), (tiny, -tiny, tiny), (1e-300, -1e-300, 1e-300), (-0.37, 0.73, -1.25), (-1.0, -2.0, -3.0)];
        (0..count).map(|i| if i < special.len() { special[i] } else { (i as f64 * 0.137 - 256.2, (i as f64 * 0.37).sin(), (i % 19) as f64 * -0.31) }).collect()
    }
    fn vec3_input(points: &[(f64, f64, f64)]) -> Value {
        Value::Floats(3, Arc::new(points.iter().flat_map(|(x, y, z)| [*x, *y, *z]).collect()))
    }

    fn noise3_strict_packed_matches_the_reference(seed:i64,octaves:i64,lacunarity:f64,gain:f64,special_only:bool) {
        let all = points(if special_only {6}else{LANES});
        let check = |pts: &[(f64, f64, f64)], seed: i64, octaves: i64, lacunarity: f64, gain: f64| {
            let (mapped, program) = map(
                None,
                Ty::Vec3,
                vec3_input(pts),
                &format!("(noise3 (+ i [t 0.0 0.0]) :seed {seed} :octaves {octaves} :lacunarity {lacunarity:?} :gain {gain:?})"),
            );
            let live = input::at_time(0.0);
            let Value::Floats(1, expected) = reference(&mapped, &live).unwrap() else { panic!("floats expected") };
            domains(&|| same(&expected, &force(&program, &live).unwrap()));
            // the native noise, within 4 ULP of the Lisp-evaluated value
            let noise = rays_math::noise::create(seed);
            for (i, (x, y, z)) in pts.iter().enumerate() {
                let native = if lacunarity == 2.0 && gain == 0.5 {
                    rays_math::noise::fbm3(Some(octaves), &noise, *x, *y, *z).unwrap()
                } else {
                    rays_math::noise::fbm3_with_scratch(&mut rays_math::noise::create_fbm3_scratch(), &noise, octaves, lacunarity, gain, *x, *y, *z)
                };
                let (_, exponent) = frexp(native.abs().max(expected[i].abs()));
                let ulp = 2f64.powi((exponent - 53).max(-1074));
                assert!((native - expected[i]).abs() / ulp <= 4.0, "noise3 seed{seed}/octaves{octaves}/index{i}: native {native:.17} vs Lisp {:.17}", expected[i]);
            }
        };
        check(&all,seed,octaves,lacunarity,gain);
    }
    // One process per parameter family; a failure names its octave count.
    macro_rules! noise_case {
        ($name:ident,$seed:expr,$octaves:expr,$lacunarity:expr,$gain:expr,$special:expr)=>{
            #[test]fn $name(){for octaves in $octaves {
                if let Err(panic)=std::panic::catch_unwind(||noise3_strict_packed_matches_the_reference($seed,octaves,$lacunarity,$gain,$special)) { eprintln!("{} octaves {octaves} failed",stringify!($name)); std::panic::resume_unwind(panic); }
            }}
        };
    }
    noise_case!(noise_default,42,[1, 4, 32],2.,0.5,false);
    noise_case!(noise_special_seed0,0,[1, 4, 32],2.,0.5,true);
    noise_case!(noise_special_seed1,1,[1, 4, 32],2.,0.5,true);
    noise_case!(noise_special_seed42,42,[1, 4, 32],2.,0.5,true);
    noise_case!(noise_lacunarity17,7,[2, 5, 32],1.7,0.62,false);
    noise_case!(noise_zero_gain,7,[2, 5, 32],3.,0.,false);
    noise_case!(noise_lacunarity05,7,[2, 5, 32],0.5,1.,false);
    noise_case!(noise_unit_gain,7,[2, 5, 32],2.,1.,false);
    noise_case!(noise_lacunarity231,7,[2, 5, 32],2.31,0.25,false);

    /// `Float.frexp`: mantissa in [0.5, 1) and exponent, for finite nonzero values.
    fn frexp(x: f64) -> (f64, i32) {
        if x == 0.0 || !x.is_finite() {
            return (x, 0);
        }
        let bits = x.to_bits();
        let exponent = ((bits >> 52) & 0x7ff) as i32;
        if exponent == 0 {
            let (m, e) = frexp(x * 2f64.powi(64));
            return (m, e - 64);
        }
        (f64::from_bits((bits & !(0x7ff << 52)) | (1022 << 52)), exponent - 1022)
    }

    #[test]
    fn noise3_range_and_live_parameters() {
        for source in [
            "(noise3 [0.1 -0.2 0.3] :octaves 3 :lacunarity 0.0)",
            "(noise3 [0.1 -0.2 0.3] :octaves 3 :gain 1.5)",
            "(noise3 [0.1 -0.2 0.3] :octaves 3 :gain -0.5)",
            "(noise3 [0.1 -0.2 0.3] :octaves 0)",
            "(noise3 [0.1 -0.2 0.3] :octaves 65)",
        ] {
            let refused = value_in("host", None, source).err().unwrap_or_else(|| panic!("out-of-range noise shape accepted: {source}"));
            assert_eq!(refused.code, "E_NOISE_RANGE", "{source}");
        }
        // a live lacunarity and gain are the region's registers: one program whose bytes at each
        // time, and whose refusal, are the walker's
        let all = points(LANES);
        let input = vec3_input(&all);
        let (mapped, program) = map(None, Ty::Vec3, input.clone(), "(noise3 i :seed 7 :octaves 5 :lacunarity (+ 1.7 t) :gain (- 0.62 t))");
        for time in [0.0, 0.25] {
            let live = input::at_time(time);
            let Value::Floats(1, expected) = reference(&mapped, &live).unwrap() else { panic!("floats expected") };
            same(&expected, &force(&program, &live).unwrap());
        }
        let live = input::at_time(0.75);
        let (a, b) = (reference(&mapped, &live).err().unwrap(), force(&program, &live).err().unwrap());
        assert!(a.code == "E_NOISE_RANGE" && b.code == a.code);
        // a live octave count is an Int register
        let (mapped, program) = map(None, Ty::Vec3, input, "(noise3 i :seed 7 :octaves (int (+ 1.0 (* t 8.0))) :lacunarity 1.9)");
        for time in [0.0, 0.25, 3.875] {
            let live = input::at_time(time);
            let Value::Floats(1, expected) = reference(&mapped, &live).unwrap() else { panic!("floats expected") };
            same(&expected, &force(&program, &live).unwrap());
        }
        let live = input::at_time(8.0);
        let (a, b) = (reference(&mapped, &live).err().unwrap(), force(&program, &live).err().unwrap());
        assert!(a.code == "E_NOISE_RANGE" && b.code == a.code);
    }

    fn math_function(input_ty: Ty, body: &str) -> flow_syntax::value::FnId {
        let source = format!("(workspace math (graph result :context value (let* [tested (fn [(x : {})] {body})] 0.0)))", flow_syntax::ty::to_string(&input_ty));
        let forms = syntax::parse(&source).unwrap();
        let (w, d) = catalog::check_workspace(all(), false, Some(SOURCES.as_slice()), &Catalog { version: 1, kinds: Vec::new() }, &forms);
        let w = w.unwrap_or_else(|| panic!("{}", d.iter().map(|d| d.message.clone()).collect::<Vec<_>>().join("\n")));
        let evaluation = e::static_with(&w, true, None, None).unwrap();
        let (_, values) = evaluation.records.iter().find(|(p, _)| p == &vec!["result".to_owned(), "tested".to_owned()]).unwrap();
        match &values[0].1 {
            Value::Fn(f) => *f,
            _ => panic!("fn expected"),
        }
    }
    fn math_compile(input_ty: Ty, output_ty: Ty, input: Value, body: &str) -> (Value, packed::T) {
        let f = math_function(input_ty.clone(), body);
        let signature = flow_syntax::ty::FnSignature { params: vec![input_ty], result: output_ty };
        let mapped = e::host::map_function(None, None, &signature, f, &[input]).unwrap();
        let program = compile(&mapped);
        (mapped, program)
    }
    fn bytes(value: &Value) -> Vec<u64> {
        match value {
            Value::Floats(1, xs) | Value::Floats(3, xs) | Value::Floats(4, xs) => xs.iter().map(|x| x.to_bits()).collect(),
            other => panic!("float arrays expected, got {other:?}"),
        }
    }

    fn packed_math_case(body: &str, input_ty: Ty, output_ty: Ty, time: f64) {
        let count = LANES;
        let input = if input_ty == Ty::Float {
            Value::Floats(1, Arc::new((0..count).map(|i| if i == 0 { -0.0 } else { (i as f64 * 0.31).sin() * 0.8 }).collect()))
        } else {
            Value::Floats(3, Arc::new((0..count * 3).map(|i| if i < 3 { -0.0 } else { (i as f64 * 0.73).sin() * 2f64.powi((i % 40) as i32 - 20) }).collect()))
        };
        let (mapped, program) = math_compile(input_ty, output_ty, input, body);
        let live = input::at_time(time);
        let expected = bytes(&reference(&mapped, &live).unwrap());
        domains(&|| {
            let actual = bytes(&force(&program, &live).unwrap_or_else(|d| panic!("{body}: {}: {}", d.code, d.message)));
            assert_eq!(actual.len(), expected.len());
            for (lane, (actual, expected)) in actual.iter().zip(&expected).enumerate() {
                assert_eq!(actual, expected, "{body} at time bits {:016x}, lane {lane}: actual {:016x}, reference {:016x}", time.to_bits(), actual, expected);
            }
        });
    }
    macro_rules! math_cases {
        ($module:ident, $body:literal, $input:expr, $output:expr) => {
            // One process per operator: failures name the body and time bits.
            #[test] fn $module() { for time in [-0.0, 0.0, 0.37, 1.0] { packed_math_case($body, $input, $output, time); } }
        };
    }
    math_cases!(packed_tan, "(tan (+ x t))", Ty::Float, Ty::Float);
    math_cases!(packed_atan2, "(atan2 x t)", Ty::Float, Ty::Float);
    math_cases!(packed_asin, "(asin (* (+ x t) 0.25))", Ty::Float, Ty::Float);
    math_cases!(packed_acos, "(acos (* (+ x t) 0.25))", Ty::Float, Ty::Float);
    math_cases!(packed_exp, "(exp (+ x t))", Ty::Float, Ty::Float);
    math_cases!(packed_log, "(log (+ (+ x t) 4.0))", Ty::Float, Ty::Float);
    math_cases!(packed_sign, "(sign (+ x t))", Ty::Float, Ty::Float);
    math_cases!(packed_hypot, "(hypot x t)", Ty::Float, Ty::Float);
    math_cases!(packed_dot, "(dot x [t 0.3 -0.2])", Ty::Vec3, Ty::Float);
    math_cases!(packed_cross, "(cross x [t 0.3 -0.2])", Ty::Vec3, Ty::Vec3);
    math_cases!(packed_normalize, "(normalize (+ x [t 0.0 0.0]))", Ty::Vec3, Ty::Vec3);
    math_cases!(packed_normalize_frame, "(normalize (+ x [(float (frame/index)) t 0.0]))", Ty::Vec3, Ty::Vec3);

    #[test]
    fn packed_hypot_extremes_and_invalid_domains_refuse() {
        let (mapped, program) = math_compile(Ty::Float, Ty::Float, Value::Floats(1, Arc::new(vec![1e308, 1e-300, f64::from_bits(1), -0.0])), "(hypot x (+ x t))");
        let live = input::at_time(0.0);
        let expected = bytes(&reference(&mapped, &live).unwrap());
        domains(&|| assert_eq!(bytes(&force(&program, &live).unwrap()), expected));
        for body in ["(asin (+ x t))", "(acos (+ x t))", "(log (+ x t))", "(exp (+ (* x 1000.0) t))"] {
            let (mapped, program) = math_compile(Ty::Float, Ty::Float, Value::Floats(1, Arc::new(vec![2.0, -2.0])), body);
            for result in [reference(&mapped, &live), force(&program, &live)] {
                assert_eq!(result.err().unwrap_or_else(|| panic!("{body}: expected nonfinite error")).code, "E_NONFINITE", "{body}");
            }
        }
    }

    #[test]
    fn quaternion_finite_overflow_guards_match_the_strict_reference() {
        let limit = f64::MAX.sqrt();
        let cases = [
            (-0.0, 0.0, 0.0, 0.0),
            (1e-12, 0.0, 0.0, 0.0),
            (1e308, -1e308, 0.0, -0.0),
            (limit, 0.0, 0.0, -0.0),
            (f64::from_bits(limit.to_bits() + 1), 0.0, 0.0, -0.0),
            (limit * 0.5, limit * 0.25, 0.0, -limit * 0.25),
            (limit, limit, -limit, limit),
        ];
        let input = Value::Floats(4, Arc::new(
            (0..LANES * 4)
                .map(|index| {
                    let (x, y, z, w) = cases[index / 4 % cases.len()];
                    [x, y, z, w][index % 4]
                })
                .collect(),
        ));
        let (mapped, program) = math_compile(Ty::Vec4, Ty::Vec4, input, "(quat_normalize [x.x x.y x.z (+ x.w t)])");
        let live = input::at_time(0.0);
        let expected = bytes(&reference(&mapped, &live).unwrap());
        domains(&|| assert_eq!(bytes(&force(&program, &live).unwrap()), expected));
    }
    fn fma_proof(input_ty:Ty,output_ty:Ty,input:Value,body:&str,expected:Vec<f64>){
        use flow_ir::flow_ir::executor;
        let(value,program)=math_compile(input_ty,output_ty,input,body);
        let compiled=executor::compile(&value,&executor::CompileOptions{strict:true,..Default::default()}).unwrap();
        let live=input::at_time(0.0);
        let check=|actual:Value|{
            let actual=bytes(&actual);assert_eq!(actual.len(),expected.len());
            for(i,(a,e))in actual.iter().zip(&expected).enumerate(){assert_eq!(*a,e.to_bits(),"{body} lane{i}");}
        };
        check(reference(&value,&live).unwrap());
        for domains in [1,4,8]{parallel::run_unit(Some(domains),&||{
            check(force(&program,&live).unwrap());
            check(executor::force(&compiled,&executor::ForceOptions{strict:true,..Default::default()},&live).unwrap());Ok(())
        }).unwrap();}
        assert!(!packed::gpu_refusals(false,false,&program).is_empty(),"binary64 FMA must refuse fast GPU export");
        js_exact(&program,&live,&expected);
    }
    fn js_exact(program:&packed::T,live:&input::Frame,expected:&[f64]){
        let inputs=packed::host::prepare_values(program,&Default::default(),live).unwrap();
        let js_value=|v:&Value|match v{
            Value::Int(n)=>format!("{n}n"),Value::Float(x)=>flow_syntax::lisp::float(*x),
            Value::Vec2(x,y)=>format!("[{},{}]",flow_syntax::lisp::float(*x),flow_syntax::lisp::float(*y)),
            Value::Vec3(x,y,z)=>format!("[{},{},{}]",flow_syntax::lisp::float(*x),flow_syntax::lisp::float(*y),flow_syntax::lisp::float(*z)),
            Value::Vec4(x,y,z,w)=>format!("[{},{},{},{}]",flow_syntax::lisp::float(*x),flow_syntax::lisp::float(*y),flow_syntax::lisp::float(*z),flow_syntax::lisp::float(*w)),
            _=>panic!("numeric uniform expected")
        };
        let arrays=inputs.arrays.iter().map(|a|match a{
            Value::Floats(1, xs)|Value::Floats(3, xs)|Value::Floats(4, xs)=>format!("Float64Array.from([{}])",xs.iter().map(|x|flow_syntax::lisp::float(*x)).collect::<Vec<_>>().join(",")),
            Value::IntArray(xs)=>format!("BigInt64Array.from([{}])",xs.iter().map(|x|format!("{x}n")).collect::<Vec<_>>().join(",")),
            _=>panic!("numeric input array expected")
        }).collect::<Vec<_>>().join(",");
        let mut uniforms=inputs.uniforms.iter().map(|v|{let s=js_value(v);if matches!(v,Value::Vec2(..)|Value::Vec3(..)|Value::Vec4(..)){s}else{format!("[{s}]")}}).collect::<Vec<_>>();
        uniforms.push(format!("[{}]",inputs.frame.iter().map(js_value).collect::<Vec<_>>().join(",")));
        let script=format!("const b=new DataView(new ArrayBuffer(8));const bits=x=>{{b.setFloat64(0,x,true);return b.getBigUint64(0,true)}};\n{}\nconst out=new Float64Array({}),ins=[{}],uni=[{}];if(region(ins,uni,[out],0,{})!==0||region(ins,uni,[out],{},{})!==0)throw Error('region refused');const expected=[{}];for(let i=0;i<out.length;i++)if(bits(out[i])!==expected[i])throw Error('region bits '+i);",
            flow_web::js::region(false,program).unwrap(),expected.len(),arrays,uniforms.join(","),inputs.count/2,inputs.count/2,inputs.count,expected.iter().map(|x|format!("0x{:x}n",x.to_bits())).collect::<Vec<_>>().join(","));
        use std::io::Write;
        let mut child=std::process::Command::new("node").stdin(std::process::Stdio::piped()).stdout(std::process::Stdio::piped()).stderr(std::process::Stdio::piped()).spawn().expect("Node is required for actual JavaScript parity");
        child.stdin.take().unwrap().write_all(script.as_bytes()).unwrap();
        let out=child.wait_with_output().unwrap();assert!(out.status.success(),"{}",String::from_utf8_lossy(&out.stderr));
    }
    fn cross_fma(body:&str){
        let cases:[[f64;3];4]=[[0.,0.,0.],[-0.,0.,-0.],[0.3,-0.7,1.2],[1e-150,-2e-150,3e-150]];
        let input=(0..5001*3).map(|i|cases[i/3%4][i%3]).collect::<Vec<_>>();
        let expected=(0..5001*3).map(|i|{let[x,y,z]=cases[i/3%4];[(-z).mul_add(y,y*z),(-x).mul_add(z,z*x),(-y).mul_add(x,x*y)][i%3]}).collect();
        fma_proof(Ty::Vec3,Ty::Vec3,Value::Floats(3, Arc::new(input)),body,expected);
    }
    #[test]fn cross_intrinsic_native_fma_all_tiers_js(){cross_fma("(cross x x)")}
    #[test]fn cross_helper_native_fma_all_tiers_js(){cross_fma("(vec3_cross x x)")}
    #[test]fn matrix_native_fma_all_tiers_js(){
        let rows:[[f64;4];3]=[[-0.,0.,-0.,0.],[0.3,-0.7,1.2,-0.5],[1e-150,-2e-150,3e-150,-4e-150]];
        let right=[[2.,3.,-1.,4.],[5.,-2.,6.,-3.],[1.,4.,3.,2.],[0.2,-0.1,0.4,2.]];
        let input=(0..5001*4).map(|i|rows[i/4%3][i%4]).collect::<Vec<_>>();
        let expected=(0..5001*4).map(|i|{let mut out=0.;for k in 0..4{out=rows[i/4%3][k].mul_add(right[k][i%4],out);}out}).collect();
        fma_proof(Ty::Vec4,Ty::Vec4,Value::Floats(4, Arc::new(input)),"(get (mat4_mul {:r0 x :r1 [0.0 0.0 0.0 0.0] :r2 [0.0 0.0 0.0 0.0] :r3 [0.0 0.0 0.0 0.0]} {:r0 [2.0 3.0 -1.0 4.0] :r1 [5.0 -2.0 6.0 -3.0] :r2 [1.0 4.0 3.0 2.0] :r3 [0.2 -0.1 0.4 2.0]}) :r0)",expected);
    }
    #[test]fn cross_overflow_refuses_both_execution_tiers(){
        let(value,program)=math_compile(Ty::Vec3,Ty::Vec3,Value::Floats(3, Arc::new(vec![0.,f64::MAX,0.])),"(cross x [0.0 0.0 2.0])");
        let live=input::at_time(0.0);for result in [reference(&value,&live),force(&program,&live)]{assert_eq!(result.unwrap_err().code,"E_NONFINITE");}
    }
}
