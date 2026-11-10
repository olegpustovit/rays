// Lives in flow_eval because it drives Workspace.check and Eval together.
use flow_syntax::symbol::Symbol;
use crate::*;
use flow_check::{catalog::{Catalog, Kind, Slot}, op::{self, Op, Shape}, ray_host, workspace as W};
use flow_syntax::value;
use rays_diagnostic::{self, Diagnostic, Severity};
use std::sync::{Arc, Mutex};
use flow_syntax::{context, syntax, ty};
use std::cell::RefCell;

fn t(s: &str) -> Value { Value::Text(s.as_bytes().into()) }
fn ia(v: &[i64]) -> Value { Value::IntArray(Arc::new(v.to_vec())) }
fn fa(v: &[f64]) -> Value { Value::Floats(1, Arc::new(v.to_vec())) }
fn na(n: &str, v: Value) -> (Symbol, Value) { (n.into(), v) }
const MAX_INT: i64 = i64::MAX;
const MIN_INT: i64 = i64::MIN;

fn nominal(ty: &Ty) -> Option<Value> {
    let n = match ty { Ty::Named(n) => n.as_str(), _ => return None };
    Some(match n {
        "panel" => value::struct_value("ui/outline", ty::panel(), vec![].into()),
        "scene" => value::struct_value("scene/merge", ty::scene(), vec![].into()),
        "world" => value::struct_value("world/none", ty::world(), vec![].into()),
        "settings" => value::struct_value("settings/config", ty::settings(), vec![].into()),
        "editor" => value::struct_value("ui/workspace", ty::editor(), vec![].into()),
        "material" => value::struct_value("material/standard", ty::material(), vec![].into()),
        "geometry" => Value::NoGeo,
        _ => return None,
    })
}
fn sample(name: &str, ty: &Ty) -> Value {
    match ty {
        Ty::Int => Value::Int(if name == "index" || name == "active" || name == "first" { 0 } else { 2 }),
        Ty::Float => Value::Float(if name == "ratio" || name == "gain" { 0.5 } else { 1.5 }),
        Ty::Bool => Value::Bool(true),
        Ty::Vec3 => Value::Vec3(1., 2., 3.),
        Ty::Vec2 => Value::Vec2(1., 2.),
        Ty::Vec4 => Value::Vec4(1., 2., 3., 4.),
        Ty::Text => t(if name == "axis" { "horizontal" } else if name == "button" { "left" } else { "a" }),
        Ty::Color => t("#336699"),
        Ty::List(e) => Value::List(Arc::new(vec![sample("", e), sample("", e)])),
        Ty::Array(e) if **e == Ty::Int && name == "offsets" => ia(&[0, 2]),
        Ty::Array(e) => value::array_init(e, 2, &|_| sample("", e)).unwrap(),
        Ty::Named(_) => nominal(ty).unwrap_or_else(|| Value::Deferred(ty.clone(), 0)),
        Ty::Any => Value::Float(1.5),
        Ty::Record(fs) => Value::Record(fs.iter().map(|(n, t)| (n.clone(), sample(n, t))).collect::<Vec<_>>().into()),
        Ty::Fn(_) => Value::Fn(FnId(0, 0)),
    }
}
fn literal_of(v: &Value) -> Option<param::Value> {
    match v {
        Value::Int(n) => Some(param::Value::Int(*n)),
        Value::Float(n) => Some(param::Value::Float(*n)),
        Value::Bool(b) => Some(param::Value::Bool(*b)),
        Value::Text(s) => Some(param::Value::Text(s.clone())),
        _ => None,
    }
}
fn no_node(_: &str, _: &[(Symbol, Value)]) -> Result<Value, Diagnostic> { panic!("node callback must not run") }
fn result_type(o: &Op, args: &[(Symbol, Value)]) -> Result<Ty, Diagnostic> {
    let typed = args.iter().map(|(n, v)| (n.clone(), value::ty_of(v), literal_of(v))).collect::<Vec<_>>();
    op::result_type(o, &typed)
}
fn check(o: &Op, args: &[(Symbol, Value)]) -> Value {
    if o.name == "error" {
        match (o.body)(&input::at_time(0.), &no_node, args) {
            Ok(_) => panic!("error returned"),
            Err(d) => { assert!(d.code == "a" && d.message == "a"); return Value::NoGeo; }
        }
    }
    let made = RefCell::new(0i64);
    let node = |_: &str, a: &[(Symbol, Value)]| -> Result<Value, Diagnostic> {
        let ty = result_type(o, a)?;
        let mut m = made.borrow_mut();
        let id = *m; *m += 1;
        Ok(Value::Deferred(ty, id))
    };
    let result = match (o.body)(&input::at_time(0.), &node, args) {
        Ok(v) => v,
        Err(d) => panic!("{}: {}: {}", o.name, d.code, d.message),
    };
    let rt = result_type(o, args).unwrap_or_else(|d| panic!("{}: {}: {}", o.name, d.code, d.message));
    assert!(ty::fits(&value::ty_of(&result), &rt), "{}", o.name);
    if let Shape::Struct { .. } = o.shape {
        assert!(match &result { Value::Struct(..) => true, Value::Deferred(ty, _) => ty::is_cooked(ty), _ => false }, "{}", o.name);
    }
    result
}
fn find(name: &str, c: context::Context) -> Op { op::find(&[], name, &c).unwrap_or_else(|| panic!("no op {name}")).clone() }
fn apply(name: &str, args: &[(Symbol, Value)]) -> Value { check(&find(name, context::value()), args) }
fn rejects_code(name: &str, args: &[(Symbol, Value)], code: &str) {
    let o = find(name, context::value());
    match (o.body)(&input::at_time(0.), &no_node, args) { Err(d) => assert_eq!(d.code, code, "{name}"), Ok(_) => panic!("accepted {name}") }
}
fn parse(s: &str) -> Vec<syntax::Syntax> { syntax::parse(s).unwrap() }
fn empty() -> Catalog { Catalog { version: 1, kinds: vec![] } }
fn wcheck(ops: &[Op], catalog: &Catalog, src: &str) -> (Option<W::Workspace>, Vec<Diagnostic>) { W::check(ops, false, false, None, catalog, &parse(src)) }
fn run0(ws: &W::Workspace) -> Eval { run(ws, false, None, None, None, 0.).unwrap() }
fn result_of(e: &Eval, g: &str) -> Value { e.results.iter().find(|(n, _)| n == g).unwrap().1.clone() }

#[test]
fn every_operator_shape_and_arithmetic() {
    assert_eq!(op::all().len(), 199);
    for id in 0..10 { ray_host::register(id, Arc::new(|_| ray_host::MISS)); }
    let repr = find("text/repr", context::value());
    let mut every = Vec::new(); for c in 0..=255u8 { every.push(c); }
    for (text, expected) in [every, vec![], "é日本😀\0\u{8}\t\n\r\\\"".as_bytes().to_vec()].into_iter().zip(include_str!("../golden/text_repr_bytes.txt").lines()) {
        assert_eq!(check(&repr, &[na("text", Value::Text(text.into()))]), Value::Text(expected.as_bytes().into()));
    }
    for o in op::all().iter().filter(|o| !o.name.starts_with("gpu/")) {
        assert_eq!(op::find(&[], &o.name, &o.ctx).unwrap().name, o.name);
        assert_eq!(op::derive_packed_kind(o).is_some(), o.ctx == context::value() && o.shape == Shape::Scalar, "{}", o.name);
        let s = &o.signature;
        let base: Vec<(Symbol, Value)> = s.pos.iter().map(|(n, ty)| (n.clone(), match (o.name.as_str(), n.as_str()) {
            ("array/scatter_add", "indices") | ("array/compact", "mask") => ia(&[0, 1]),
            ("array/index_of", "keys") => ia(&[1, 2]),
            ("array/index_of", "queries") => ia(&[2, 3]),
            ("asin" | "acos", _) => Value::Float(0.5),
            _ => sample(n, ty),
        })).collect();
        let mut with_opt = base.clone(); with_opt.extend(s.opt.iter().map(|(n, ty)| (n.clone(), sample(n, ty))));
        for args in [base.clone(), with_opt] {
            check(o, &args);
            if let Some((n, ty)) = &s.rest { let mut a = args.clone(); a.push((n.clone(), sample(n, ty))); a.push((n.clone(), sample(n, ty))); check(o, &a); }
            for (n, ty) in &s.kw {
                let v = match o.choices.iter().find(|(c, _)| c == n) { Some((_, c)) if !c.is_empty() => t(&c[0]), _ => sample(n, ty) };
                let mut a = args.clone(); a.push((n.clone(), v)); check(o, &a);
            }
        }
        let mut valid = base.clone();
        if let Some((n, ty)) = &s.rest { valid.push((n.clone(), sample(n, ty))); }
        (o.check)(&valid).unwrap();
        for (n, ty) in &s.kw {
            let v = match o.choices.iter().find(|(c, _)| c == n) { Some((_, c)) if !c.is_empty() => t(&c[0]), _ => sample(n, ty) };
            let mut a = valid.clone(); a.push((n.clone(), v)); (o.check)(&a).unwrap();
        }
        for (n, choices) in &o.choices {
            assert!(s.pos.iter().chain(&s.opt).chain(&s.kw).any(|(k, _)| k == n));
            assert!(!choices.is_empty());
        }
        if let Some(binary) = op::arith(&o.name) {
            for (a, b) in [(Value::Int(2), Value::Int(3)), (Value::Float(1.5), Value::Int(2)), (Value::Vec3(1., 2., 3.), Value::Float(2.)), (Value::Float(2.), Value::Vec3(1., 2., 3.))] {
                assert_eq!((binary.apply)(&a, &b).unwrap(), check(o, &[na("a", a.clone()), na("b", b.clone())]), "{}", o.name);
            }
        }
    }
}

#[test]
fn raster_stage_intrinsics_refuse_cpu_values() {
    for name in ["gpu/fwidth", "gpu/float_bits", "gpu/sample", "gpu/sample_level", "gpu/read", "gpu/texture_size", "gpu/instance_id", "gpu/front_facing", "gpu/depth", "gpu/fragment_x", "gpu/fragment_y", "gpu/trace_triangles", "gpu/trace_any", "gpu/trace_curves", "gpu/trace_curves_any", "gpu/scene_vertex", "gpu/scene_vertex_end", "gpu/scene_instance", "gpu/scene_sphere", "gpu/scene_strand", "gpu/scene_spheres", "gpu/scene_strands", "gpu/group_scan", "gpu/store", "gpu/store_int"] {
        let o = find(name, context::value());
        let args = o.signature.pos.iter().map(|(n, ty)| (n.clone(), sample(n, ty))).collect::<Vec<_>>();
        match (o.body)(&input::at_time(0.), &no_node, &args) { Err(d) => assert_eq!(d.code, "E_GPU_STAGE", "{name}"), Ok(_) => panic!("{name}") }
    }
}

#[test]
fn contexts_checks_and_custom_kinds() {
    for ctx in context::all() { for o in op::of_context(&[], &ctx) { assert!(o.ctx == ctx || o.ctx == context::value()); } }
    assert_eq!(find("rand", context::scene()).name, "value/rand");
    let mut sine = find("sin", context::value()); sine.category = "Copied".into();
    assert!(op::derive_packed_kind(&sine).is_none());
    let rejects = |name: &str, args: Vec<(Symbol, Value)>| match (find(name, context::editor()).check)(&args) {
        Err(d) if d.code == "E_RANGE" => (), other => panic!("{name}: {other:?}"),
    };
    rejects("ui/graph", vec![na("view", t("invalid"))]);
    rejects("ui/lisp", vec![na("tab", t("invalid"))]);
    rejects("ui/split", vec![na("axis", t("invalid"))]);
    rejects("ui/split_at", vec![na("ratio", Value::Float(1.))]);
    rejects("ui/tile", vec![]);
    rejects("ui/switch", vec![na("panel", sample("", &ty::panel())), na("active", Value::Int(1))]);
    (find("ui/graph", context::editor()).check)(&[na("view", Value::Residual(ResidualId(0, 0)))]).unwrap();
    let e = empty();
    let (ws, ds) = wcheck(&[], &e, "(workspace w (graph g :context material (material/standard)))");
    assert!(ws.is_some() && !ds.iter().any(|d| d.severity == Severity::ErrorSeverity));
    let (reserved, ds) = wcheck(&[], &e, "(workspace w (defmacro m [] `(standard)) (graph g :context material (m)))");
    assert!(reserved.is_some() && ds.is_empty(), "{:?}", ds);
    let (known, ds) = wcheck(&[], &e, "(workspace w (graph g :context value (material/standard)))");
    assert!(known.is_none() && ds.iter().any(|d| d.code == "E_WRONG_CONTEXT"));
    let mut k = Kind { qualified: "custom/root".into(), description: "Test node.".into(), context: context::scene(), slots: vec![], parameters: vec![], outputs: vec![], facts: None };
    let c1 = Catalog { version: 1, kinds: vec![k.clone()] };
    let (ws, ds) = wcheck(&[], &c1, "(workspace w (graph g :context scene (root)))");
    assert!(ds.is_empty());
    let evaluated = run0(&ws.unwrap());
    assert_eq!(result_of(&evaluated, "g"), value::struct_value("custom/root", ty::scene(), vec![].into()));
    assert_eq!(evaluated.plan.nodes.len(), 0);
    k.slots = vec![Slot { name: "child".into(), required: true, rest: false, ty: None }];
    let c2 = Catalog { version: 1, kinds: vec![k] };
    let (ws, ds) = wcheck(&[], &c2, "(workspace w (graph g :context scene (first (map root (list nil)))))");
    assert!(ds.is_empty());
    let evaluated = run0(&ws.unwrap());
    assert_eq!(result_of(&evaluated, "g"), value::struct_value("custom/root", ty::scene(), vec![na("child", Value::NoGeo)].into()));
    assert_eq!(evaluated.plan.nodes.len(), 0);
}

#[test]
fn exact_integer_arithmetic_and_arrays() {
    let product = 131071i64 * 131071 * 19349663;
    assert_eq!(apply("int/mul", &[na("a", Value::Int(131071 * 131071)), na("b", Value::Int(19349663))]), Value::Int(product));
    assert_eq!(apply("int/mod", &[na("a", Value::Int(product)), na("b", Value::Int(97))]), Value::Int(product % 97));
    assert_eq!(apply("int/and", &[na("a", Value::Int(product)), na("b", Value::Int(MAX_INT))]), Value::Int(product));
    let values = Value::Floats(3, Arc::new(vec![1., 2., 3., 4., 5., 6.]));
    let tail = apply("array/slice", &[na("array", values), na("first", Value::Int(1)), na("count", Value::Int(1))]);
    assert_eq!(tail, Value::Floats(3, Arc::new(vec![4., 5., 6.])));
    assert_eq!(apply("array/concat", &[na("first", tail.clone()), na("second", tail)]), Value::Floats(3, Arc::new(vec![4., 5., 6., 4., 5., 6.])));
    assert_eq!(apply("equal?", &[na("a", t("key-pressed")), na("b", t("key-pressed"))]), Value::Bool(true));
}

fn host_forms(body: &str) -> String { format!("(workspace w (graph g :context host {body}))") }
fn checked(ops: &[Op], body: &str) -> W::Workspace {
    match W::check(ops, false, false, None, &empty(), &parse(&host_forms(body))) {
        (Some(w), d) if d.is_empty() => w,
        (_, ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    }
}
fn value_of(body: &str) -> Value { result_of(&run0(&checked(&[], body)), "g") }

#[test]
fn large_array_constructors_defer_through_compiled_scalar_calls() {
    for body in [
        "(array/range 5000 :type int)",
        "(array/float 5000 -0.)",
        "(array/vec3 5000 [1. -0. 3.])",
        "(array/vec4 5000 [1. 2. 3. 4.])",
        "(let* [make (fn [(n : int)] (array/int n 7))] (make 5000))",
    ] {
        let workspace = checked(&[], body);
        let handlers = ResidualHandlers { defer: Some(Arc::new(|_| true)), ..Default::default() };
        let deferred = residual::handle(handlers, &mut || host::static_with_requests(None, false, None, &workspace)).unwrap();
        let value = result_of(&deferred, "g");
        assert!(matches!(value, Value::Residual(_)), "eager constant array: {body}");
        assert_eq!(host::force_reference(&value, &input::at_time(0.), None, None, None).unwrap(), value_of(body));
    }
    let invalid = checked(&[], "(array/int 1152921504606846976 0)");
    let expected = host::static_with_requests(None, false, None, &invalid).err().unwrap();
    let handlers = ResidualHandlers { defer: Some(Arc::new(|_| true)), ..Default::default() };
    let deferred = residual::handle(handlers, &mut || host::static_with_requests(None, false, None, &invalid)).unwrap();
    let value = result_of(&deferred, "g");
    assert!(matches!(value, Value::Residual(_)), "a virtual array must not reserve its materialized storage");
    let actual = host::force_reference(&value, &input::at_time(0.), None, None, None).unwrap_err();
    assert_eq!(actual.code, "E_ARRAY_MEMORY");
    assert_eq!((actual.code, actual.message), (expected.code, expected.message));
}

#[test]
fn high_integers_stay_exact_end_to_end() {
    let high = (1i64 << 53) + 1;
    let text = high.to_string();
    assert!(value::int_of(&Value::Int(high)).unwrap() == high && value::int_of(&Value::Int(MAX_INT)).unwrap() == MAX_INT);
    assert_eq!(value_of(&format!("(range {} {})", high, high + 3)), Value::List(Arc::new(vec![Value::Int(high), Value::Int(high + 1), Value::Int(high + 2)])));
    assert_eq!(value_of(&format!("(array/int 3 {text})")), ia(&[high, high, high]));
    assert_eq!(value_of("(array/range 3 :type int)"), ia(&[0, 1, 2]));
    assert_eq!(value_of("(array/range 3)"), fa(&[0., 1., 2.]));
    assert_eq!(value_of(&format!("(map (fn [x] (int/add x 1)) (array/int 3 {text}))")), ia(&[high + 1; 3]));
    assert_eq!(value_of(&format!("(for [x (array/int 65 {text})] x)")), ia(&[high; 65]));
    assert_eq!(value_of("(scan [a 0] [x (array/int 3 2)] (int/add a x))"), ia(&[2, 4, 6]));
    assert_eq!(value_of("(scan [a 7] [x (array/int 0)] (int/add a x))"), ia(&[]));
    assert_eq!(value_of("(fold [a 7] [x (array/int 0)] (int/add a x))"), Value::Int(7));
    assert_eq!(value_of("(reduce int/add 7 (array/int 0))"), Value::Int(7));
    assert_eq!(value_of("(array/sum (array/int 0))"), Value::Int(0));
    assert_eq!(value_of(&format!("(array/sum (array/int 2 {MAX_INT}))")), Value::Int(-2));
    assert_eq!(value_of(&format!("(array/nth (array/concat (array/slice (array/int 3 {text}) 1 1) (array/int 1 0)) 0)")), Value::Int(high));
    let workspace = checked(&[], &format!("(map (fn [x] (int/add x 1)) (array/int 4097 {text}))"));
    let expected = ia(&vec![high + 1; 4097]);
    let run_bulk = |bulk: Value| {
        let handlers = ResidualHandlers { bulk: Some(Arc::new(move |_| Some(bulk.clone()))), ..Default::default() };
        let e = residual::handle(handlers, &mut || host::static_with_requests(None, false, None, &workspace)).unwrap();
        result_of(&e, "g")
    };
    assert_eq!(run_bulk(expected.clone()), expected);
    // A rounded prefix or wrong kind cannot replace exact integer evaluation.
    assert_eq!(run_bulk(ia(&vec![high; 4097])), expected);
    assert_eq!(run_bulk(fa(&vec![(high + 1) as f64; 4097])), expected);
}

fn typed_op() -> Op {
    let base = find("array/float", context::value());
    let mut o = base.clone();
    o.name = "test/typed".into();
    o.signature = op::Signature { pos: vec![], opt: vec![], rest: None, kw: vec![("type".into(), Ty::Text)] };
    o.choices = vec![];
    o.out = Arc::new(|_| Ok(Ty::Array(Box::new(Ty::Float))));
    o.out_args = Some(Arc::new(|args| match args.iter().find(|(n, _, _)| n == "type") {
        None => Ok(Ty::Array(Box::new(Ty::Float))),
        Some((_, _, Some(param::Value::Text(s)))) if s.as_ref() == b"float" => Ok(Ty::Array(Box::new(Ty::Float))),
        Some((_, _, Some(param::Value::Text(s)))) if s.as_ref() == b"vec3" => Ok(Ty::Array(Box::new(Ty::Vec3))),
        _ => Err(value::fail("E_TEST_TYPE", "The type must be literal.")),
    }));
    o.body = Arc::new(|_, _, args| match args.iter().find(|(n, _)| n == "type") {
        Some((_, Value::Text(s))) if s.as_ref() == b"vec3" => Ok(Value::Floats(3, Arc::new(vec![1., 2., 3.]))),
        _ => Ok(Value::Floats(1, Arc::new(vec![1.]))),
    });
    o
}

#[test]
fn typed_and_source_operators() {
    let operator = typed_op();
    for (body, expected, ty) in [
        ("(test/typed)", fa(&[1.]), Ty::Array(Box::new(Ty::Float))),
        ("(test/typed :type \"vec3\")", Value::Floats(3, Arc::new(vec![1., 2., 3.])), Ty::Array(Box::new(Ty::Vec3))),
    ] {
        let workspace = checked(std::slice::from_ref(&operator), body);
        assert_eq!(workspace.graphs[0].body.ty(), &ty);
        assert_eq!(result_of(&r#static(&workspace, false, None).unwrap(), "g"), expected);
    }
    let (workspace, diagnostics) = W::check(std::slice::from_ref(&operator), false, false, None, &empty(), &parse(&host_forms("(let* [kind \"vec3\"] (test/typed :type kind))")));
    assert!(workspace.is_none() && diagnostics.iter().any(|d| d.code == "E_TEST_TYPE" && d.span.is_some()));
    let mut source = find("array/float", context::value());
    source.name = "test/present".into();
    source.signature = op::Signature { pos: vec![], opt: vec![], rest: None, kw: vec![] };
    source.choices = vec![];
    source.out_args = None;
    source.out = Arc::new(|_| Ok(Ty::Bool));
    source.body = Arc::new(|_, _, _| Ok(value::source_value("test/present", Ty::Bool, vec![].into())));
    let sources = vec![source];
    let static_of = |body: &str| result_of(&r#static(&checked(&sources, body), false, None).unwrap(), "g");
    let resolve: Option<Arc<dyn Fn(&Value) -> Result<Value, Diagnostic> + Send + Sync>> = Some(Arc::new(|v| match v {
        Value::Source(g) if g.name == "test/present" && g.fields.is_empty() => Ok(Value::Bool(false)),
        _ => panic!("unexpected source"),
    }));
    let marker = static_of("(test/present)");
    assert!(is_live(&marker) && !frame_dependent(&marker));
    let live = input::at_time(0.);
    assert_eq!(host::force_reference(&marker, &live, None, None, resolve.clone()), Ok(Value::Bool(false)));
    assert_eq!(host::force_reference(&static_of("(if (test/present) (sqrt -1) 7)"), &live, None, None, resolve.clone()), Ok(Value::Int(7)));
    // the arena keeps no resolver after its call: it captures the cook's geometries and their device buffers
    assert_eq!(Arc::strong_count(resolve.as_ref().unwrap()), 1);
    match host::force_reference(&marker, &live, None, None, None) { Err(d) => assert_eq!(d.code, "E_DATA_SOURCE"), Ok(_) => panic!() }
    for resolved in [marker.clone(), Value::Float(1.)] {
        let r: Option<Arc<dyn Fn(&Value) -> Result<Value, Diagnostic> + Send + Sync>> = Some(Arc::new(move |_| Ok(resolved.clone())));
        match host::force_reference(&marker, &live, None, None, r) { Err(d) => assert_eq!(d.code, "E_DATA_SOURCE"), Ok(_) => panic!() }
    }
}

type Resolve = Arc<dyn Fn(&Value) -> Result<Value, Diagnostic> + Send + Sync>;
fn host_error(cause: &str, code: &str, message: &str) -> Diagnostic {
    rays_diagnostic::error(code.into(), message.into(), None, None, Some(cause.into()), vec![], vec![])
}

#[test]
fn host_forcing_agrees_with_supplied_state() {
    let live = input::at_time(2.);
    let state = create_state();
    let marker = value::source_value("test/array", Ty::Array(Box::new(Ty::Float)), vec![na("count", Value::Int(2))].into());
    let legacy = value::struct_value("test/array", Ty::Array(Box::new(Ty::Float)), vec![na("count", Value::Int(2))].into());
    let data = fa(&[0., -0.]);
    for marker in [marker.clone(), legacy] {
        for supplied in [None, Some(&state)] {
            let before = state_stamp(&state);
            let calls = Arc::new(Mutex::new(0));
            let (c, d) = (calls.clone(), data.clone());
            let resolve: Resolve = Arc::new(move |_| { *c.lock().unwrap() += 1; Ok(d.clone()) });
            let actual = host::force_reference(&marker, &live, supplied, None, Some(resolve.clone())).unwrap();
            assert!(actual == data && *calls.lock().unwrap() == 1 && state_stamp(&state) == before);
            for (resolved, code) in [(marker.clone(), "E_DATA_SOURCE"), (ia(&[0, 1]), "E_DATA_SOURCE"), (fa(&[0., f64::NAN]), "E_NONFINITE")] {
                let r: Resolve = Arc::new(move |_| Ok(resolved.clone()));
                match host::force_reference(&marker, &live, supplied, None, Some(r)) { Err(d) => assert_eq!(d.code, code), Ok(_) => panic!() }
            }
            let reason = host_error("host cause", "E_HOST", "host failure");
            let rr = reason.clone();
            let r: Resolve = Arc::new(move |_| Err(rr.clone()));
            assert_eq!(host::force_reference(&marker, &live, supplied, None, Some(r)), Err(reason));
            *calls.lock().unwrap() = 0;
            let mut bad = live.clone(); bad.dt = f64::NAN;
            match host::force_reference(&marker, &bad, supplied, None, Some(resolve)) { Err(d) => assert!(d.code == "E_FRAME" && *calls.lock().unwrap() == 0), Ok(_) => panic!() }
        }
    }
    let (ws, ds) = wcheck(&[], &empty(), "(workspace nested_source (graph g :context value (+ t 1.)))");
    assert!(ds.is_empty());
    let dynamic = result_of(&r#static(&ws.unwrap(), false, None).unwrap(), "g");
    let fields = vec![na("value", dynamic)];
    let marker = value::source_value("test/nested", Ty::Any, vec![].into());
    let calls = Arc::new(Mutex::new(0));
    let (c, f) = (calls.clone(), fields.clone());
    let resolve: Resolve = Arc::new(move |_| { *c.lock().unwrap() += 1; Ok(Value::Record(f.clone().into())) });
    assert_eq!(host::force_reference(&marker, &live, None, None, Some(resolve)), Ok(Value::Record(vec![na("value", Value::Float(3.))].into())));
    assert_eq!(*calls.lock().unwrap(), 1);
    let marker = value::source_value("test/live_argument", Ty::Float, fields.into());
    let m2 = marker.clone();
    let resolve: Resolve = Arc::new(move |actual| { assert_eq!(*actual, m2); Ok(Value::Float(7.)) });
    assert_eq!(host::force_reference(&marker, &live, None, None, Some(resolve)), Ok(Value::Float(7.)));
    let descriptor = value::source_value("test/borrowed", Ty::Array(Box::new(Ty::Float)), vec![na("geometry", Value::Deferred(ty::geometry(), 17)), na("owner", t("point"))].into());
    let legacy = value::struct_value("test/borrowed", Ty::Array(Box::new(Ty::Int)), vec![na("count", Value::Int(2))].into());
    let nested = value::source_value("test/write", Ty::Array(Box::new(Ty::Float)), vec![na("values", Value::Record(vec![na("offsets", legacy), na("values", descriptor)].into()))].into());
    for supplied in [None, Some(&state)] {
        let calls = Arc::new(Mutex::new(0));
        let (c, d, n) = (calls.clone(), data.clone(), nested.clone());
        let resolve: Resolve = Arc::new(move |actual| { assert_eq!(*actual, n); *c.lock().unwrap() += 1; Ok(d.clone()) });
        assert_eq!(host::force_reference(&nested, &live, supplied, None, Some(resolve.clone())).unwrap(), data);
        assert_eq!(*calls.lock().unwrap(), 1);
        let n = nested.clone();
        let r: Resolve = Arc::new(move |actual| { assert_eq!(*actual, n); Ok(Value::Int(0)) });
        match host::force_reference(&nested, &live, supplied, None, Some(r)) { Err(d) => assert_eq!(d.code, "E_DATA_SOURCE"), Ok(_) => panic!() }
        let reason = host_error("nested host cause", "E_HOST", "nested host failure");
        let (n, rr) = (nested.clone(), reason.clone());
        let r: Resolve = Arc::new(move |actual| { assert_eq!(*actual, n); Err(rr.clone()) });
        assert_eq!(host::force_reference(&nested, &live, supplied, None, Some(r)), Err(reason));
        *calls.lock().unwrap() = 0;
        let mut bad = live.clone(); bad.dt = f64::NAN;
        match host::force_reference(&nested, &bad, supplied, None, Some(resolve)) { Err(d) => assert!(d.code == "E_FRAME" && *calls.lock().unwrap() == 0), Ok(_) => panic!() }
    }
    let (ws, ds) = wcheck(&[], &empty(), "(workspace source_state (graph g :context value (state [previous 0.0] (+ previous 1.0))))");
    assert!(ds.is_empty());
    let fold = result_of(&r#static(&ws.unwrap(), false, None).unwrap(), "g");
    let before = state_stamp(&state);
    let reason = host_error("rollback cause", "E_HOST", "stateful host failure");
    let (st, rr, f2, l2, b2) = (state.clone(), reason.clone(), fold.clone(), live.clone(), before.clone());
    let resolve: Resolve = Arc::new(move |_| {
        force(&f2, &l2, Some(&st)).unwrap();
        assert_ne!(state_stamp(&st), b2);
        Err(rr.clone())
    });
    assert_eq!(host::force_reference(&marker, &live, Some(&state), None, Some(resolve)), Err(reason));
    assert_eq!(state_stamp(&state), before);
}

#[test]
fn constant_vector_constructors_match_element_builder_bits() {
    for (name, ty, fill) in [("array/vec3", Ty::Vec3, Value::Vec3(-0., 1., -2.)), ("array/vec4", Ty::Vec4, Value::Vec4(-0., 1., -2., f64::from_bits(1)))] {
        let o = find(name, context::value());
        for count in [0i64, 1, 2, 3, 5, 17, 4097, 32769] {
            let actual = (o.body)(&input::at_time(0.), &no_node, &[na("count", Value::Int(count)), na("value", fill.clone())]).unwrap();
            let expected = value::array_init(&ty, count, &|_| fill.clone()).unwrap();
            let data = |v: &Value| match v { Value::Floats(3, d) | Value::Floats(4, d) => d.clone(), _ => panic!() };
            let (a, b) = (data(&actual), data(&expected));
            assert_eq!(a.len(), b.len());
            assert!(a.iter().zip(b.iter()).all(|(x, y)| x.to_bits() == y.to_bits()), "{name} {count}");
        }
    }
}

fn capture(f: &dyn Fn() -> Result<Value, Diagnostic>) -> Result<Value, (String, String)> { f().map_err(|d| (d.code, d.message)) }

#[test]
fn constant_array_descriptors_match_dense_constructor_errors() {
    for (name, fill) in [("array/float", Value::Float(-0.)), ("array/vec3", Value::Vec3(-0., 1., 2.)), ("array/vec4", Value::Vec4(-0., 1., 2., 3.))] {
        let o = find(name, context::value());
        for count in [0i64, 1, 7, -1] {
            let args = vec![na("count", Value::Int(count)), na("value", fill.clone())];
            let actual = capture(&|| op::constant_array_descriptor(name, &args));
            let expected = capture(&|| (o.body)(&input::at_time(0.), &no_node, &args));
            match (actual, expected) {
                (Err(a), Err(b)) => assert_eq!(a, b),
                (Ok(Value::Source(g)), Ok(data)) => {
                    assert_eq!(g.fields.len(), 2);
                    assert!(matches!((&g.fields[0], &g.fields[1]), ((c, Value::Int(n)), (v, _)) if c == "count" && v == "value" && *n == count));
                    assert_eq!(g.ty, value::ty_of(&data));
                    if count > 0 { let row = &g.fields[1].1; assert_eq!(*row, value::array_get(&data, 0).unwrap()); }
                }
                other => panic!("{name} {count}: {other:?}"),
            }
        }
        for fill in [t("wrong"), Value::Float(f64::NAN)] {
            let args = vec![na("count", Value::Int(1)), na("value", fill)];
            let actual = capture(&|| op::constant_array_descriptor(name, &args));
            let expected = capture(&|| (o.body)(&input::at_time(0.), &no_node, &args));
            assert!(expected.is_err());
            assert_eq!(actual, expected);
        }
    }
    let huge = i64::MAX;
    let args = vec![na("count", Value::Int(huge)), na("value", Value::Vec4(0., 0., 0., 0.))];
    let Value::Source(source) = op::constant_array_descriptor("array/vec4", &args).unwrap() else { panic!("virtual array descriptor") };
    assert_eq!(source.fields.get_str("count"), Some(&Value::Int(huge)));
    assert_eq!(capture(&|| (find("array/vec4", context::value()).body)(&input::at_time(0.), &no_node, &args)).unwrap_err().0, "E_ARRAY_MEMORY");
    assert_eq!(capture(&|| op::constant_array_descriptor("array/vec4", &[na("count", Value::Int(huge)), na("value", t("unused"))])).unwrap_err().0, "E_ARRAY_TYPE");
    match op::constant_array_descriptor("array/vec4", &[na("count", Value::Int(0)), na("value", t("unused"))]) {
        Ok(Value::Source(g)) => assert_eq!(g.fields, vec![na("count", Value::Int(0)), na("value", Value::Vec4(0., 0., 0., 0.))].into()),
        other => panic!("{other:?}"),
    }
    for args in [vec![], vec![na("count", Value::Int(1)), na("count", Value::Int(1))], vec![na("count", Value::Int(1)), na("extra", Value::Int(0))]] {
        match capture(&|| op::constant_array_descriptor("array/vec4", &args)) { Err((c, _)) => assert_eq!(c, "E_ARRAY_TYPE"), other => panic!("{other:?}") }
    }
}

#[test]
fn whole_array_primitives() {
    for name in ["array/scatter_add", "array/compact", "array/scan_sum", "array/grade", "array/index_of"] {
        let o = find(name, context::value());
        assert!(o.shape == Shape::Scalar && o.ctx == context::value());
    }
    let grade = find("array/grade", context::value());
    assert!(grade.signature.pos == vec![("keys".into(), Ty::Array(Box::new(Ty::Any)))] && grade.signature.kw == vec![("descending".into(), Ty::Bool)]);
    let keys = ia(&[2, 1, 2]);
    assert_eq!(apply("array/grade", &[na("keys", keys.clone())]), ia(&[1, 0, 2]));
    assert_eq!(apply("array/grade", &[na("keys", keys.clone()), na("descending", Value::Bool(false))]), ia(&[1, 0, 2]));
    assert_eq!(apply("array/grade", &[na("keys", keys), na("descending", Value::Bool(true))]), ia(&[0, 2, 1]));
    assert_eq!(apply("array/index_of", &[na("keys", ia(&[9007199254740993, 2, 2])), na("queries", ia(&[2, 7, 9007199254740993, 2, 7]))]), ia(&[1, -1, 0, 1, -1]));
    let ta = |v: &[&[u8]]| Value::TextArray(Arc::new(v.iter().map(|s| s.to_vec()).collect()));
    assert_eq!(apply("array/index_of", &[na("keys", ta(&[b"\xff\x00", "é".as_bytes(), "é".as_bytes()])), na("queries", ta(&["é".as_bytes(), b"missing", b"\xff\x00"]))]), ia(&[1, -1, 0]));
    assert_eq!(apply("array/compact", &[na("mask", ia(&[1, 0, 1]))]), ia(&[0, 2]));
    assert_eq!(apply("float/next_after", &[na("x", Value::Float(1.)), na("y", Value::Float(2.))]), Value::Float(f64::from_bits(1f64.to_bits() + 1)));
    assert_eq!(apply("float/next_after", &[na("x", Value::Float(1.)), na("y", Value::Float(0.))]), Value::Float(f64::from_bits(1f64.to_bits() - 1)));
    assert_eq!(apply("float/next_after", &[na("x", Value::Float(3.)), na("y", Value::Float(3.))]), Value::Float(3.));
    assert_eq!(apply("float/next_after", &[na("x", Value::Float(0.)), na("y", Value::Float(1.))]), Value::Float(f64::from_bits(1)));
    assert_eq!(apply("array/segment_argmax", &[na("values", ia(&[2, 5, 5, 1, 5])), na("offsets", ia(&[0, 3, 3, 5]))]), ia(&[1, -1, 4]));
    assert_eq!(apply("array/segment_argmax", &[na("values", fa(&[0., -0., 0.])), na("offsets", ia(&[0, 3]))]), ia(&[0]));
    assert_eq!(apply("array/segment_argmax", &[na("values", fa(&[])), na("offsets", ia(&[0]))]), ia(&[]));
    assert_eq!(apply("array/scan_sum", &[na("array", ia(&[MAX_INT, 1, -1]))]), ia(&[0, MAX_INT, MIN_INT, MAX_INT]));
    assert_eq!(apply("array/scatter_add", &[na("indices", ia(&[0, 0, 1])), na("values", fa(&[1., 2., 3.])), na("size", Value::Int(3))]), fa(&[3., 3., 0.]));
    assert_eq!(apply("array/scatter_add", &[na("indices", ia(&[0, 0, 1])), na("values", ia(&[MAX_INT, 1, 2])), na("size", Value::Int(3))]), ia(&[MIN_INT, 2, 0]));
    assert_eq!(apply("array/scatter_add", &[na("indices", ia(&[0, 0, 1])), na("values", Value::Floats(3, Arc::new(vec![1., 2., 3., 4., 5., 6., 1., 1., 1.]))), na("size", Value::Int(2))]), Value::Floats(3, Arc::new(vec![5., 7., 9., 1., 1., 1.])));
    for (name, args, expected) in [
        ("array/grade", vec![na("keys", fa(&[]))], ia(&[])),
        ("array/index_of", vec![na("keys", ia(&[])), na("queries", ia(&[1, 2]))], ia(&[-1, -1])),
        ("array/index_of", vec![na("keys", ta(&[b"a"])), na("queries", ta(&[]))], ia(&[])),
        ("array/compact", vec![na("mask", ia(&[]))], ia(&[])),
        ("array/scan_sum", vec![na("array", ia(&[]))], ia(&[0])),
        ("array/scatter_add", vec![na("indices", ia(&[])), na("values", fa(&[])), na("size", Value::Int(2))], fa(&[0., 0.])),
    ] { assert_eq!(apply(name, &args), expected, "{name}"); }
    // results never alias their arguments
    for (name, args) in [
        ("array/grade", vec![na("keys", ia(&[1, 0, 1]))]),
        ("array/index_of", vec![na("keys", ia(&[1, 2])), na("queries", ia(&[2, 1]))]),
        ("array/compact", vec![na("mask", ia(&[1, 0, 1]))]),
        ("array/scan_sum", vec![na("array", ia(&[1, 2]))]),
        ("array/scatter_add", vec![na("indices", ia(&[0, 0])), na("values", fa(&[1., 2.])), na("size", Value::Int(1))]),
    ] {
        let before = format!("{args:?}");
        let result = apply(name, &args);
        match result {
            Value::IntArray(mut a) => { if !a.is_empty() { Arc::make_mut(&mut a)[0] = 999; } }
            Value::Floats(1, mut a) => { if !a.is_empty() { Arc::make_mut(&mut a)[0] = 999.; } }
            _ => panic!(),
        }
        assert_eq!(format!("{args:?}"), before);
    }
    for (code, name, args) in [
        ("E_ARRAY_MASK", "array/compact", vec![na("mask", ia(&[2]))]),
        ("E_ARRAY_OFFSETS", "array/segment_argmax", vec![na("values", ia(&[1])), na("offsets", ia(&[0, 2]))]),
        ("E_ARRAY_TYPE", "array/segment_argmax", vec![na("values", Value::Floats(3, Arc::new(vec![0., 0., 0.]))), na("offsets", ia(&[0, 1]))]),
        ("E_NONFINITE", "array/segment_argmax", vec![na("values", fa(&[f64::NAN])), na("offsets", ia(&[0, 1]))]),
        ("E_NONFINITE", "float/next_after", vec![na("x", Value::Float(f64::NAN)), na("y", Value::Float(1.))]),
        ("E_ARRAY_TYPE", "array/scan_sum", vec![na("array", fa(&[1.]))]),
        ("E_NONFINITE", "array/grade", vec![na("keys", fa(&[f64::NAN]))]),
        ("E_ARRAY_TYPE", "array/index_of", vec![na("keys", ia(&[])), na("queries", ta(&[]))]),
        ("E_ARRAY_RANGE", "array/scatter_add", vec![na("indices", ia(&[1])), na("values", ia(&[2])), na("size", Value::Int(1))]),
        ("E_ARRAY_COUNT", "array/scatter_add", vec![na("indices", ia(&[])), na("values", fa(&[1.])), na("size", Value::Int(1))]),
    ] { rejects_code(name, &args, code); }
}
